-module(ctr_broker).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3,
         unsubscribe_all/1,

         init/0
        ]).

-record(ctr_subscription, {
          id = undefined,
          realm = undefined,
          uri = undefined,
          match = exact,
          created = undefined,
          subscribers = []
         }).

-record(ctr_publication, {
          id = undefined,
          pub_sess_id = undefined,
          subs = [],
          realm = undefined,
          topic = undefined,
          ts = undefined,
          sub_id = undefined,
          arguments = undefined,
          argumentskw = undefined
         }).

handle_message(subscribe, Message, Session) ->
    do_subscribe(Message, Session);
handle_message(unsubscribe, Message, Session) ->
    do_unsubscribe(Message, Session);
handle_message(publish, Message, Session) ->
    do_publish(Message, Session).


unsubscribe_all(Session) ->
    Subs = ctr_session:get_subscriptions(Session),
    SessionId = ctr_session:get_id(Session),

    Delete = fun(SubId, ok) ->
                     delete_subscription(SubId, SessionId),
                     ok
             end,
    lists:foldl(Delete, ok, Subs),
    ok.

init() ->
    create_table().


do_subscribe({subscribe, _RequestId, Options, Uri} = Msg, Session) ->
    lager:debug("broker: subscribe ~p ~p", [Uri, Options]),
    SessionId = ctr_session:get_id(Session),
    Realm = ctr_session:get_realm(Session),
    NewSub = #ctr_subscription{
                uri = Uri,
                realm = Realm,
                created = calendar:universal_time(),
                subscribers = [SessionId]
               },
    Result = store_subscription(NewSub),
    handle_subscribe_result(Result, Msg, Session).


do_unsubscribe({unsubscribe, ReqId, SubId} = Msg , Session) ->
    lager:debug("broker: unsubscribe ~p ~p", [ReqId, SubId]),
    SessionId = ctr_session:get_id(Session),
    Result = delete_subscription(SubId, SessionId),
    handle_unsubscribe_result(Result, Msg, Session).



do_publish(Msg, Session) ->
    lager:debug("broker: publish ~p", [Msg]),
    Realm = ctr_session:get_realm(Session),
    Topic = get_publish_topic(Msg),
    Arguments = get_publish_arguments(Msg),
    ArgumentsKw = get_publish_argumentskw(Msg),
    NewPubId = ctr_utils:gen_global_id(),

    NewPub = #ctr_publication{
                realm = Realm,
                topic = Topic,
                id = NewPubId,
                ts = calendar:universal_time(),
                arguments = Arguments,
                argumentskw = ArgumentsKw},

    {ok, Publication} = store_publication(NewPub),
    #ctr_publication{
       id = PubId,
       sub_id = SubId,
       subs = Subs
      } = Publication,
    send_event(Msg, SubId, PubId, Subs, Session),
    WantAcknowledge = wants_acknowledge(Msg),
    maybe_send_published(WantAcknowledge, Msg, PubId, Session).


handle_subscribe_result({added, Subscription}, Msg, Session) ->
    #ctr_subscription{ id = SubId } = Subscription,
    send_subscribed(Msg, SubId, Session);
handle_subscribe_result({created, Subscription}, Msg, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    %% TODO: meta events
    send_subscribed(Msg,SubId, Session).



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
    HasSubscription = ctr_session:has_subscription(SubId, Session),
    maybe_send_unsubscribe(HasSubscription, Msg, SubId, Session).



send_subscribed(Msg, SubId, Session) ->
    %% TODO: meta events
    {ok, NewSession} = ctr_session:add_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?SUBSCRIBED(RequestId, SubId)),
    ok.

send_unsubscribed(Msg, SubId, Session) ->
    %% TODO: meta events
    {ok, NewSession} = ctr_session:remove_subscription(SubId, Session),
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

send_event(Msg, SubId, PubId, Subs0, Session) ->
    SessionId = ctr_session:get_id(Session),
    Subs = lists:delete(SessionId, Subs0),
    Arguments = get_publish_arguments(Msg),
    ArgumentsKw = get_publish_argumentskw(Msg),
    Event = ?EVENT(SubId, PubId, #{}, Arguments, ArgumentsKw),
    lager:debug("broker: sending event to ~p",[Subs]),
    Send =
        fun(SessId, _) ->
                case ctr_session:lookup(SessId) of
                    {ok, Session} ->
                        ct_router:to_session(Session, Event);
                    _ ->
                        ok
                end
        end,
    lists:foldl(Send, ok, Subs),
    ok.

maybe_send_published(true, Msg, PubId, Session) ->
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(Session, ?PUBLISHED(RequestId, PubId));
maybe_send_published(false, _, _, _) ->
    ok.

get_publish_topic(PublishMsg) ->
    erlang:element(4, PublishMsg).

get_publish_arguments({publish, _, _, _, Arguments}) ->
    Arguments;
get_publish_arguments({publish, _, _, _, Arguments, _}) ->
    Arguments;
get_publish_arguments(_) ->
    undefined.

get_publish_argumentskw({publish, _, _, _, _, ArgumentsKw}) ->
    ArgumentsKw;
get_publish_argumentskw(_) ->
    undefined.

wants_acknowledge(Msg) ->
    Options = erlang:element(3, Msg),
    maps:get(acknowledge, Options, false).


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
                        case mnesia:wread({ctr_subscription, NewId}) of
                            [] ->
                                ok = mnesia:write(NewSub),
                                {created, NewSub};
                            _ ->
                                {error, id_exists}
                        end
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
    {ok, Session} = ctr_session:lookup(SessId),
    Realm = ctr_session:get_realm(Session),
    NewPubId = ctr_utils:gen_global_id(),

    NewPub = Pub0#ctr_publication{id = NewPubId},

    MatchHead = #ctr_subscription{uri=Topic, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatSpec = [{MatchHead, Guard, GiveObject}],

    Lookup =
        fun() ->
                case mnesia:wread({ctr_publication, NewPubId}) of
                    [] ->
                        case mnesia:select(ctr_subscription, MatSpec, write) of
                            [#ctr_subscription{id = SubId,
                                               subscribers = Subs}] ->
                                UpdatedPub =
                                    NewPub #ctr_publication{sub_id=SubId,
                                                            subs = Subs
                                                           },
                                ok = mnesia:write(UpdatedPub),
                                {ok, UpdatedPub};

                            [] ->
                                ok = mnesia:write(NewPub),
                                {ok, NewPub}
                        end;
                    _ ->
                        {error, pub_id_exists}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_publication_store_result(Result, Pub0).


handle_publication_store_result({atomic, {ok, Publication}}, _Pub0) ->
    {ok, Publication};
handle_publication_store_result({atomic, {error, pub_id_exists}}, Pub0) ->
    store_publication(Pub0).


delete_subscription(SubId, SessionId) ->
    Delete =
        fun() ->
                case mnesia:wread({ctr_subscription, SubId}) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubs = lists:delete(SessionId, Subs),
                        NewSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubs
                                           },
                        case NewSubs of
                            [] ->
                                mnesia:delete({ctr_subscription, SubId}),
                                {deleted, NewSubscription};

                            _ ->
                                ok = mnesia:write(NewSubscription),
                                {removed, NewSubscription}
                        end;
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
