-module(ctr_dealer).

-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3,
         unregister_all/1,

         init/0
        ]).

init() ->
    ctrd_invocation:init(),
    ctr_dealer_data:init().


handle_message(register, Message, Session) ->
    do_register(Message, Session);
handle_message(unregister, Message, Session) ->
    do_unregister(Message, Session);
handle_message(call, Message, Session) ->
    do_call(Message, Session);
handle_message(error, Message, Session) ->
    ctrd_invocation:invocation_error(Message, Session);
handle_message(yield, Message, Session) ->
    ctrd_invocation:yield(Message, Session).


unregister_all(Session) ->
    Regs = cta_session:get_registrations(Session),
    SessId = cta_session:get_id(Session),

    Delete = fun(RegId, ok) ->
                     ctr_dealer_data:delete_registration(RegId, SessId),
                     ok
             end,
    lists:foldl(Delete, ok, Regs),
    ok.


do_register({register, _ReqId, _Options, Procedure} = Msg, Session) ->
    SessId = cta_session:get_id(Session),
    Realm = cta_session:get_realm(Session),

    NewReg = ctr_registration:new(Procedure, Realm, SessId),
    Result = ctr_dealer_data:store_registration(NewReg),
    handle_register_result(Result, Msg, Session).


handle_register_result({created, Registration}, Msg, Session) ->
    ctr_broker:send_registration_meta_event(create, Session, Registration),
    send_registered(Msg, Registration, Session);
handle_register_result({error, procedure_exists}, Msg, Session) ->
    ReqId = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(Session, ?ERROR(register, ReqId, #{},
                                              procedure_already_exists)),
    ok.

do_unregister({unregister, _ReqId, RegId} = Msg, Session) ->
    SessId = cta_session:get_id(Session),
    Result = ctr_dealer_data:delete_registration(RegId, SessId),
    handle_unregister_result(Result, Msg, Session).



do_call({call, _ReqId, _Options, Procedure, _Arguments, _ArgumentsKw} = Msg,
        Session) ->
    Internal = ctr_callee:is_procedure(Procedure),
    Result = find_registration(Internal, Procedure, Session),
    handle_call_registration(Result, Msg, Session).

find_registration(true, _, _) ->
    {ok, system};
find_registration(false, Procedure, Session) ->
    ctr_dealer_data:find_registration(Procedure, Session).



handle_unregister_result({atomic, {removed, Registration}}, Msg, Session) ->
    RegId = ctr_registration:get_id(Registration),
    send_unregistered(Msg, RegId, Session);
handle_unregister_result({atomic, {deleted, Registration}}, Msg, Session) ->
    RegId = ctr_registration:get_id(Registration),
    send_unregistered(Msg, RegId, Session),
    ctr_broker:send_registration_meta_event(delete, Session, Registration),
    ok;
handle_unregister_result({atomic, {error, not_found}}, Msg, Session) ->
    {unregister, _, RegId} = Msg,
    false = cta_session:has_registration(RegId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(unregister, RequestId, #{}, no_such_registration),
    ok = ct_router:to_session(Session, Error),
    ok.

handle_call_registration({ok, system}, Msg, Session) ->
    Response = ctr_callee:handle_call(Msg, Session),
    ok = ct_router:to_session(Session, Response),
    ok;
handle_call_registration({ok, Registration}, Msg, Session) ->
    RegId = ctr_registration:get_id(Registration),
    Callees = ctr_registration:get_callees(Registration),
    ctrd_invocation:new(RegId, Callees, Msg, Session),
    ok;
handle_call_registration({error, not_found}, Msg, Session) ->
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(call, RequestId, #{}, no_such_procedure),
    ok = ct_router:to_session(Session, Error),
    ok.


send_registered(Msg, Registration, Session) ->
    ctr_broker:send_registration_meta_event(register, Session, Registration),
    RegId = ctr_registration:get_id(Registration),
    {ok, NewSession} = cta_session:add_registration(RegId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?REGISTERED(RequestId, RegId)),
    ok.

send_unregistered(Msg, Registration, Session) ->
    ctr_broker:send_registration_meta_event(unregister, Session, Registration),
    RegId = ctr_registration:get_id(Registration),
    {ok, NewSession} = cta_session:remove_registration(RegId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?UNREGISTERED(RequestId)),
    ok.
