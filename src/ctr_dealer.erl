-module(ctr_dealer).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3,
         unregister_all/1,

         init/0
        ]).

-record(ctr_registration, {
          id = undefined,
          realm = undefined,
          topic = undefined,
          match = exact,
          created = undefined,
          callees = []
         }).


handle_message(register, Message, Session) ->
    do_register(Message, Session);
handle_message(unregister, Message, Session) ->
    do_unregister(Message, Session);
handle_message(call, Message, Session) ->
    do_call(Message, Session);
handle_message(yield, Message, Session) ->
    do_yield(Message, Session).



unregister_all(Session) ->
    Regs = ctr_session:get_registrations(Session),
    PeerAtGate = ctr_session:get_peer(Session),

    Delete = fun(RegId) ->
                     delete_registration(RegId, PeerAtGate),
                     ok
             end,
    lists:foldl(Delete, ok, Regs),
    ok.


do_register({register, ReqId, Options, Procedure}, Session) ->
    ok.

do_unregister({unregister, ReqId, ReqId}, Session) ->
    ok.


do_call(Msg, Session) ->
    ReqId = erlang:element(2, Msg),
    Options = erlang:element(3, Msg),
    Procedure = erlang:element(4, Msg),

    ok.

do_yield(Msg, Session) ->
    ReqId = erlang:element(2, Msg),
    Options = erlang:element(3, Msg),
    ok.


delete_registration(RegId, PeerAtGate) ->
    ok.


init() ->
    create_table().




create_table() ->
    mnesia:delete_table(ctr_subscription),
    mnesia:delete_table(ctr_publication),
    RegDef = [{attributes, record_info(fields, ctr_registration)},
              {ram_copies, [node()]},
              {index, [realm, uri, match]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_subscription, RegDef),
    ok.
