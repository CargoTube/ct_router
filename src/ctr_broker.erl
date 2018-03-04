-module(ctr_broker).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3,

         init/0
        ]).

-record(subscription, {
          id = undefined,
          uri = undefined,
          realm = undefined,
          match = exact,
          created = undefined,
          subscribers = []
         }).

handle_message(subscribe, Message, Session) ->
    do_subscribe(Message, Session);
handle_message(unsubscribe, Message, Session) ->
    do_unsubscribe(Message, Session);
handle_message(publish, Message, Session) ->
    do_publish(Message, Session).

init() ->
    create_table().


do_subscribe({subscribe, _RequestId, _Options, Uri} = Msg, Session) ->
    PeerAtGate = ctr_session:get_peer(Session),
    Realm = ctr_session:get_realm(Session),
    NewId = ctr_utils:gen_global_id(),

    MatchHead = #subscription{uri=Uri, realm=Realm, _='_'},
    Guard = [],
    Result = ['$_'],
    MatchSpec = {MatchHead, Guard, Result},

    NewSub = #subscription{
                id = NewId,
                uri = Uri,
                realm = Realm,
                created = calendar:universal_time(),
                subscribers = [PeerAtGate]
               },

    Subscribe =
        fun() ->
                case mnesia:select(subscription, MatchSpec) of
                    [#subscription{id = Id,
                                   subscribers = Subs } = Subscription] ->
                        NewSubs = [ PeerAtGate |
                                    lists:delete(PeerAtGate, Subs)],
                        ok = mnesia:write(
                               Subscription#subscription{subscribers = NewSubs}
                              ),
                        {added, Id};
                    [] ->
                        case mnesia:wread({subscription, NewId}) of
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

do_unsubscribe(_, _) ->
    ok.

do_publish(_, _) ->
    ok.


handle_subscribe_result({atomic, {added, SubId}}, Msg, Session) ->
    RequestId = ct_msg:get_request_id(Msg),
    ct_router:to_session(Session, ?SUBSCRIBED(RequestId, SubId)),
    ok;
handle_subscribe_result({atomic, {created, Subscription}}, Msg, Session) ->
    #subscription{id = SubId} = Subscription,
    %% TODO: meta events
    RequestId = ct_msg:get_request_id(Msg),
    ct_router:to_session(Session, ?SUBSCRIBED(RequestId, SubId)),
    ok;
handle_subscribe_result({atomic, {error, id_exists}}, Msg, Session) ->
    do_subscribe(Msg, Session).



create_table() ->
    {atomic, ok} = mnesia:delete_table(subscription),
    TabDef = [{attributes, record_info(fields, subscription)},
              {ram_copies, [node()]},
              {index, [realm, uri, match]}
             ],
    {atomic, ok} = mnesia:create_table(registration, TabDef),
    ok.
