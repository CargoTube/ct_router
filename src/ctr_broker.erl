-module(ctr_broker).

-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3,
         unsubscribe_all/1,

         send_session_meta_event/2,
         send_subscription_meta_event/3,
         send_registration_meta_event/3,

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
    Uri = <<"wamp.session.on_join">>,
    do_publish(?PUBLISH(-1, #{}, Uri, [Info]), Session, true),
    ok;
send_session_meta_event(leave, Session) ->
    Id = cta_session:get_id(Session),
    Uri = <<"wamp.session.on_leave">>,
    do_publish(?PUBLISH(-1, #{}, Uri, [Id]), Session, true),
    ok.


send_subscription_meta_event(Event, Session, Subscription)
  when Event == create; Event == subscribe; Event == unsubscribe;
       Event == delete ->
    Mapping = [
               {create, <<"wamp.subscription.on_create">>},
               {subscribe, <<"wamp.subscription.on_subscribe">>},
               {unsubscribe, <<"wamp.subscription.on_unsubscribe">>},
               {delete, <<"wamp.subscription.on_delete">>}
              ],
    {Event, Uri} = lists:keyfind(Event, 1, Mapping),

    SubUri = ctr_subscription:get_uri(Subscription),
    IsMeta = is_tuple(lists:keyfind(SubUri, 2, Mapping)),
    maybe_suppress_subscription_meta_event(IsMeta, Uri, Subscription, Session).

maybe_suppress_subscription_meta_event(false, Uri, Subscription, Session) ->
    SessId = cta_session:get_id(Session),
    Keys = [id, created, match, uri],
    SubscriptionMap = ctr_subscription:to_map(Subscription),
    Details = maps:with(Keys, SubscriptionMap),

    do_publish(?PUBLISH(-1, #{exclude_me => false}, Uri, [SessId, Details]),
               Session, true),
    ok;
maybe_suppress_subscription_meta_event(true, _, _, _) ->
    ok.


send_registration_meta_event(Event, Session, Registration)
  when Event == create; Event == register; Event == unregister;
       Event == delete ->
    Mapping = [
               {create, <<"wamp.registration.on_create">>},
               {register, <<"wamp.registration.on_register">>},
               {unregister, <<"wamp.registration.on_unregister">>},
               {delete, <<"wamp.registration.on_delete">>}
              ],
    {Event, Uri} = lists:keyfind(Event, 1, Mapping),
    SessId = cta_session:get_id(Session),
    Keys = [id, created, uri, match, invoke],
    RegistrationMap = ctr_registration:to_map(Registration),
    Details = maps:with(Keys, RegistrationMap),
    do_publish(?PUBLISH(-1, #{exclude_me => false}, Uri, [SessId, Details]),
               Session),
    ok.


unsubscribe_all(Session) ->
    Subs = cta_session:get_subscriptions(Session),
    SessionId = cta_session:get_id(Session),

    Delete = fun(SubId, ok) ->
                     ctr_subscription:delete(SubId, SessionId),
                     ok
             end,
    lists:foldl(Delete, ok, Subs),
    ok.


init() ->
    ctr_subscription:init().


do_subscribe({subscribe, _RequestId, _Options, Uri} = Msg, Session) ->
    Realm = cta_session:get_realm(Session),
    Result = ctr_subscription:new(Uri, Realm, Session),
    handle_subscribe_result(Result, Msg, Session).


do_unsubscribe({unsubscribe, _ReqId, SubId} = Msg , Session) ->
    SessionId = cta_session:get_id(Session),
    Result = ctr_subscription:delete(SubId, SessionId),
    handle_unsubscribe_result(Result, Msg, Session).


do_publish(Msg, Session) ->
    do_publish(Msg, Session, false).

do_publish({publish, ReqId, Options, Topic, Arguments, ArgumentsKw}, Session,
           MetaEvent) ->
    Realm = cta_session:get_realm(Session),
    SessionId = cta_session:get_id(Session),


    DiscloseOption = maps:get(disclose_me, Options, false),
    DiscloseSession = cta_session:is_disclose_publisher(Session),
    Disclose = (DiscloseOption or DiscloseSession) and (not MetaEvent),


    EventOpts = maybe_set_publisher(Disclose , SessionId, #{}),

    {ok, Publication} = ctr_publication:new(Realm, Topic, Options, Arguments,
                                            ArgumentsKw, SessionId),
    PubId = ctr_publication:get_id(Publication),
    SubId = ctr_publication:get_subscription_id(Publication),
    AllSubs = ctr_publication:get_subscribers(Publication),
    Subs = ctrb_blackwhite_pubex:filter_subscriber(AllSubs, Options, SessionId),

    Event = ?EVENT(SubId, PubId, EventOpts, Arguments, ArgumentsKw),
    send_event(Event, Subs),

    WantAcknowledge = maps:get(acknowledge, Options, false),
    maybe_send_published(WantAcknowledge, ReqId, PubId, Session).

maybe_set_publisher(true, SessionId, Options) ->
    maps:put(publisher, SessionId, Options);
maybe_set_publisher(false, _, Options) ->
    maps:remove(publisher, Options).



handle_subscribe_result({added, Subscription}, Msg, Session) ->
    send_subscribed(Msg, Subscription, Session);
handle_subscribe_result({created, Subscription}, Msg, Session) ->
    send_subscription_meta_event(create, Session, Subscription),
    send_subscribed(Msg, Subscription, Session).



handle_unsubscribe_result({removed, Subscription}, Msg, Session) ->
    send_unsubscribed(Msg, Subscription, Session);
handle_unsubscribe_result({deleted, Subscription}, Msg, Session) ->
    send_unsubscribed(Msg, Subscription, Session),
    send_subscription_meta_event(delete, Session, Subscription),
    ok;
handle_unsubscribe_result({error, not_found}, Msg, Session) ->
    {unsubscribe, _, SubId} = Msg,
    false = cta_session:has_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(unsubscribe, RequestId, #{}, no_such_subscription),
    ok = ct_router:to_session(Session, Error),
    ok.


send_subscribed(Msg, Subscription, Session) ->
    SubId = ctr_subscription:get_id(Subscription),
    ok = send_subscription_meta_event(subscribe, Session, Subscription),
    {ok, NewSession} = cta_session:add_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?SUBSCRIBED(RequestId, SubId)),
    ok.

send_unsubscribed(Msg, Subscription, Session) ->
    SubId = ctr_subscription:get_id(Subscription),
    ok = send_subscription_meta_event(unsubscribe, Session, Subscription),
    {ok, NewSession} = cta_session:remove_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?UNSUBSCRIBED(RequestId)),
    ok.


send_event( Event,  Subs) ->
    %% Event = ?EVENT(SubId, PubId, EventOpts, Arguments, ArgumentsKw),
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
