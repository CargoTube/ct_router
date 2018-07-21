-module(ctr_broker).


-include_lib("ct_msg/include/ct_msg.hrl").
-include("ct_router.hrl").

-export([
         handle_message/3,
         unsubscribe_all/1,

         send_session_meta_event/2,
         send_subscription_meta_event/3,

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

    SessId = cta_session:get_id(Session),
    Keys = [id, created, match, uri],
    SubscriptionMap = ctr_subscription:to_map(Subscription),
    Details = maps:with(Keys, SubscriptionMap),

    do_publish(?PUBLISH(-1, #{exclude_me => false}, Uri, [SessId, Details]),
               Session),
    ok.


unsubscribe_all(Session) ->
    Subs = cta_session:get_subscriptions(Session),
    SessionId = cta_session:get_id(Session),

    Delete = fun(SubId, ok) ->
                     ctr_broker_data:delete_subscription(SubId, SessionId),
                     ok
             end,
    lists:foldl(Delete, ok, Subs),
    ok.


init() ->
    ctr_broker_data:create_table().


do_subscribe({subscribe, _RequestId, _Options, Uri} = Msg, Session) ->
    SessionId = cta_session:get_id(Session),
    Realm = cta_session:get_realm(Session),
    Result = ctr_broker_data:add_subscription(Uri, Realm, SessionId),
    handle_subscribe_result(Result, Msg, Session).


do_unsubscribe({unsubscribe, _ReqId, SubId} = Msg , Session) ->
    SessionId = cta_session:get_id(Session),
    Result = ctr_broker_data:delete_subscription(SubId, SessionId),
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

    {ok, Publication} = ctr_broker_data:store_publication(NewPub),
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
    #ctr_subscription{ id = SubId } = Subscription,
    ok = send_subscription_meta_event(subscribe, Session, Subscription),
    {ok, NewSession} = cta_session:add_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?SUBSCRIBED(RequestId, SubId)),
    ok.

send_unsubscribed(Msg, Subscription, Session) ->
    #ctr_subscription{id = SubId} = Subscription,
    ok = send_subscription_meta_event(unsubscribe, Session, Subscription),
    {ok, NewSession} = cta_session:remove_subscription(SubId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?UNSUBSCRIBED(RequestId)),
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
