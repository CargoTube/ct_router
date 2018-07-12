-module(ctr_broker).


-include_lib("ct_msg/include/ct_msg.hrl").
-include("ct_router.hrl").

-export([
         handle_message/3,
         unsubscribe_all/1,

         send_session_meta_event/2,

         init/0
        ]).

handle_message(subscribe, Message, Session) ->
    do_subscribe(Message, Session);
handle_message(unsubscribe, Message, Session) ->
    do_unsubscribe(Message, Session);
handle_message(publish, Message, Session) ->
    do_publish(Message, Session).


send_session_meta_event(join, Session) ->
    Keys = [session, authid, authrole, authmethod,
           authprovider, transport],
    Info = maps:with(Keys, cta_session:to_map(Session)),
    do_publish(?PUBLISH(-1, #{}, <<"wamp.session.on_join">>, [Info]), Session),
    ok;
send_session_meta_event(leave, Session) ->
    Id = cta_session:get_id(Session),
    do_publish(?PUBLISH(-1, #{}, <<"wamp.session.on_leave">>, [Id]), Session),
    ok.



unsubscribe_all(Session) ->
    Subs = cta_session:get_subscriptions(Session),
    SessionId = cta_session:get_id(Session),

    Delete = fun(SubId, ok) ->
                     delete_subscription(SubId, SessionId),
                     ok
             end,
    lists:foldl(Delete, ok, Subs),
    ok.

init() ->
    create_table().


do_subscribe({subscribe, _RequestId, _Options, Uri} = Msg, Session) ->
    SessionId = cta_session:get_id(Session),
    Realm = cta_session:get_realm(Session),
    NewSub = #ctr_subscription{
                uri = Uri,
                realm = Realm,
                created = calendar:universal_time(),
                subscribers = [SessionId]
               },
    Result = store_subscription(NewSub),
    handle_subscribe_result(Result, Msg, Session).


do_unsubscribe({unsubscribe, _ReqId, SubId} = Msg , Session) ->
    SessionId = cta_session:get_id(Session),
    Result = delete_subscription(SubId, SessionId),
    handle_unsubscribe_result(Result, Msg, Session).



do_publish({publish, ReqId, Options, Topic, Arguments, ArgumentsKw} = Msg,
           Session) ->
    Realm = cta_session:get_realm(Session),
    SessionId = cta_session:get_id(Session),

    NewPub = #ctr_publication{
                realm = Realm,
                topic = Topic,
                options = Options,
                pub_sess_id = SessionId,
                ts = calendar:universal_time(),
                arguments = Arguments,
                argumentskw = ArgumentsKw},

    {ok, Publication} = store_publication(NewPub),
    #ctr_publication{
       id = PubId,
       sub_id = SubId,
       subs = AllSubs
      } = Publication,
    Subs = ctrb_blackwhite_pubex:filter_subscriber(AllSubs, Options, SessionId),
    send_event(Msg, SubId, PubId, Subs),

    WantAcknowledge = maps:get(acknowledge, Options, false),
    maybe_send_published(WantAcknowledge, ReqId, PubId, Session).


handle_subscribe_result({added, Subscription}, Msg, Session) ->
    #ctr_subscription{ id = SubId } = Subscription,
    send_subscribed(Msg, SubId, Session);
handle_subscribe_result({created, Subscription}, Msg, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    %% TODO: meta events
    send_subscribed(Msg, SubId, Session).



handle_unsubscribe_result({removed, Subscription}, Msg, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    send_unsubscribed(Msg, SubId, Session);
handle_unsubscribe_result({deleted, Subscription}, Msg, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    send_unsubscribed(Msg, SubId, Session),
    %% TODO: meta events
    ok;
handle_unsubscribe_result({error, not_found}, Msg, Session) ->
    {unsubscribe, _, SubId} = Msg,
    HasSubscription = cta_session:has_subscription(SubId, Session),
    maybe_send_unsubscribe(HasSubscription, Msg, SubId, Session).



send_subscribed(Msg, SubId, Session) ->
    %% TODO: meta events
    {ok, NewSession} = cta_session:add_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?SUBSCRIBED(RequestId, SubId)),
    ok.

send_unsubscribed(Msg, SubId, Session) ->
    %% TODO: meta events
    {ok, NewSession} = cta_session:remove_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?UNSUBSCRIBED(RequestId)),
    ok.

maybe_send_unsubscribe(true, Msg, SubId, Session) ->
    send_unsubscribed(Msg, SubId, Session);
maybe_send_unsubscribe(false, Msg, _SubId, Session ) ->
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(unsubscribe, RequestId, #{}, no_such_subscription),
    ok = ct_router:to_session(Session, Error),
    ok.

send_event({publish, _, _, _, Arguments, ArgumentsKw}, SubId, PubId, Subs) ->
    Event = ?EVENT(SubId, PubId, #{}, Arguments, ArgumentsKw),
    Send =
        fun(SessId, _) ->
                case cta_session:lookup(SessId) of
                    {ok, Subscriber} ->
                        ct_router:to_session(Subscriber, Event);
                    _ ->
                        ok
                end
        end,
    lists:foldl(Send, ok, Subs),
    ok.

maybe_send_published(true, RequestId, PubId, Session) ->
    ok = ct_router:to_session(Session, ?PUBLISHED(RequestId, PubId));
maybe_send_published(false, _, _, _) ->
    ok.

store_subscription(Sub0) ->
    #ctr_subscription{
       uri = Uri,
       realm = Realm,
       subscribers = [SessionId]
      } = Sub0,
    NewId = ctr_utils:gen_global_id(),
    NewSub = Sub0#ctr_subscription{id = NewId},

    MatchHead = #ctr_subscription{uri=Uri, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    WriteIfNew =
        fun(Id, Sub) ->
                case mnesia:wread({ctr_subscription, Id}) of
                    [] ->
                        ok = mnesia:write(Sub),
                        {created, Sub};
                    _ ->
                        {error, id_exists}
                end
        end,

    Store =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubs = [ SessionId |
                                    lists:delete(SessionId, Subs)],
                        NewSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubs
                                           },
                        ok = mnesia:write(NewSubscription),
                        {added, NewSubscription};
                    [] ->
                        WriteIfNew(NewId, NewSub)
                end
        end,
    Result = mnesia:transaction(Store),
    handle_store_result(Result, Sub0).

handle_store_result({atomic, {created, Subscription}}, _) ->
    {created, Subscription};
handle_store_result({atomic, {added, Subscription}}, _) ->
    {added, Subscription};
handle_store_result({atomic, {error, id_exists}}, Subscription) ->
    store_subscription(Subscription).


store_publication(Pub0) ->
    #ctr_publication{
       realm = Realm,
       topic = Topic,
       pub_sess_id = SessId
      } = Pub0,
    {ok, Session} = cta_session:lookup(SessId),
    Realm = cta_session:get_realm(Session),
    NewPubId = ctr_utils:gen_global_id(),

    NewPub = Pub0#ctr_publication{id = NewPubId},

    MatchHead = #ctr_subscription{uri=Topic, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatSpec = [{MatchHead, Guard, GiveObject}],


    UpdateOrWriteNew =
        fun([#ctr_subscription{id = SubId, subscribers = Subs}]) ->
                UpdatedPub = NewPub#ctr_publication{sub_id=SubId, subs = Subs},
                ok = mnesia:write(UpdatedPub),
                {ok, UpdatedPub};
           ([]) ->
                ok = mnesia:write(NewPub),
                {ok, NewPub}
        end,

    LookupAndStore =
        fun() ->
                case mnesia:wread({ctr_publication, NewPubId}) of
                    [] ->
                        Found = mnesia:select(ctr_subscription, MatSpec, write),
                        UpdateOrWriteNew(Found);
                    _ ->
                        {error, pub_id_exists}
                end
        end,
    Result = mnesia:transaction(LookupAndStore),
    handle_publication_store_result(Result, Pub0).


handle_publication_store_result({atomic, {ok, Publication}}, _Pub0) ->
    {ok, Publication};
handle_publication_store_result({atomic, {error, pub_id_exists}}, Pub0) ->
    store_publication(Pub0).


delete_subscription(SubId, SessionId) ->
    DeleteOrUpdateSubscription =
        fun([], Subscription) ->
                mnesia:delete({ctr_subscription, SubId}),
                {deleted, Subscription};
           (_, Subscription) ->
                ok = mnesia:write(Subscription),
                {removed, Subscription}
        end,

    Delete =
        fun() ->
                case mnesia:wread({ctr_subscription, SubId}) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubscriber = lists:delete(SessionId, Subs),
                        UpdatedSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubscriber
                                           },
                        DeleteOrUpdateSubscription(NewSubscriber,
                                                   UpdatedSubscription);
                    [] ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(Delete),
    handle_delete_result(Result).


handle_delete_result({atomic, {deleted, Subscription}}) ->
    {deleted, Subscription};
handle_delete_result({atomic, {removed, Subscription}}) ->
    {removed, Subscription};
handle_delete_result({atomic, {error, not_found}}) ->
    {error, not_found}.



create_table() ->
    mnesia:delete_table(ctr_subscription),
    mnesia:delete_table(ctr_publication),
    SubDef = [{attributes, record_info(fields, ctr_subscription)},
              {ram_copies, [node()]},
              {index, [realm, uri, match]}
             ],
    PubDef = [{attributes, record_info(fields, ctr_publication)},
              {ram_copies, [node()]},
              {index, [realm, topic]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_subscription, SubDef),
    {atomic, ok} = mnesia:create_table(ctr_publication, PubDef),
    ok.
