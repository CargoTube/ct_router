-module(ctr_broker).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3,

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

init() ->
    create_table().


do_subscribe({subscribe, _RequestId, Options, Uri} = Msg, Session) ->
    lager:debug("broker: subscribe ~p ~p", [Uri, Options]),
    PeerAtGate = ctr_session:get_peer(Session),
    Realm = ctr_session:get_realm(Session),
    NewId = ctr_utils:gen_global_id(),

    MatchHead = #ctr_subscription{uri=Uri, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    NewSub = #ctr_subscription{
                id = NewId,
                uri = Uri,
                realm = Realm,
                created = calendar:universal_time(),
                subscribers = [PeerAtGate]
               },

    Subscribe =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec) of
                    [#ctr_subscription{id = Id,
                                   subscribers = Subs } = Subscription] ->
                        NewSubs = [ PeerAtGate |
                                    lists:delete(PeerAtGate, Subs)],
                        ok = mnesia:write(
                               Subscription#ctr_subscription{
                                 subscribers = NewSubs
                                }
                              ),
                        {added, Id};
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
    Result = mnesia:transaction(Subscribe),
    handle_subscribe_result(Result, Msg, Session).


do_unsubscribe({unsubscribe, ReqId, SubId} = Msg , Session) ->
    lager:debug("broker: unsubscribe ~p ~p", [ReqId, SubId]),
    PeerAtGate = ctr_session:get_peer(Session),
    Unsubscribe =
        fun() ->
                case mnesia:wread({ctr_subscription, ReqId}) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubs = lists:delete(PeerAtGate, Subs),
                        NewSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubs
                                           },
                        lager:debug("broker: new subscription ~p",
                                    [NewSubscription]),
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
    Result = mnesia:transaction(Unsubscribe),
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

    MatchHead = #ctr_subscription{uri=Topic, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    Lookup =
        fun() ->
                case mnesia:wread({ctr_publication, NewPubId}) of
                    [] ->
                        case mnesia:select(ctr_subscription, MatchSpec) of
                            [#ctr_subscription{id = SubId,
                                               subscribers = Subs}] ->
                                ok = mnesia:write(
                                       NewPub#ctr_publication{sub_id=SubId}
                                      ),
                                {ok, NewPubId, SubId, Subs};

                            [] ->
                                ok = mnesia:write(NewPub),
                                {ok, NewPubId, -1, []}
                        end;
                    _ ->
                        {error, pub_id_exists}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_publish_result(Result, Msg, Session).


handle_subscribe_result({atomic, {added, SubId}}, Msg, Session) ->
    send_subscribed(Msg, SubId, Session);
handle_subscribe_result({atomic, {created, Subscription}}, Msg, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    %% TODO: meta events
    send_subscribed(Msg,SubId, Session);
handle_subscribe_result({atomic, {error, id_exists}}, Msg, Session) ->
    do_subscribe(Msg, Session).



handle_unsubscribe_result({atomic, {removed, Subscription}}, Msg, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    send_unsubscribed(Msg, SubId, Session);
handle_unsubscribe_result({atomic, {deleted, Subscription}}, Msg, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    send_unsubscribed(Msg, SubId, Session),
    %% TODO: meta events
    ok;
handle_unsubscribe_result({atomic, {error, not_found}}, Msg, Session) ->
    {unsubscribe, _, SubId} = Msg,
    HasSubscription = ctr_session:has_subscription(SubId, Session),
    maybe_send_unsubscribe(HasSubscription, Msg, SubId, Session).


handle_publish_result({atomic, {ok, PubId, SubId, Subs}}, Msg, Session) ->
    lager:debug("broker: publication ~p created", [PubId]),
    send_event(Msg, SubId, PubId, Subs, Session),
    WantAcknowledge = wants_acknowledge(Msg),
    maybe_send_published(WantAcknowledge, Msg, PubId, Session),
    ok;
handle_publish_result({atomic, {error, pub_id_exists}}, Msg, Session) ->
    do_publish(Msg, Session).


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
    Peer = ctr_session:get_peer(Session),
    Subs = lists:delete(Peer, Subs0),
    Arguments = get_publish_arguments(Msg),
    ArgumentsKw = get_publish_argumentskw(Msg),
    Event = ?EVENT(SubId, PubId, #{}, Arguments, ArgumentsKw),
    lager:debug("broker: sending event to ~p",[Subs]),
    ct_router:to_peer(Subs, {to_peer, Event}),
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
