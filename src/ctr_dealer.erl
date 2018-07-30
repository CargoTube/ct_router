-module(ctr_dealer).

-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3,
         unregister_all/1,

         init/0
        ]).

init() ->
    ctrd_invocation:init(),
    ctr_registration:init().


handle_message(register, Message, Session) ->
    do_register(Message, Session);
handle_message(unregister, Message, Session) ->
    do_unregister(Message, Session);
handle_message(call, Message, Session) ->
    do_call(Message, Session);
handle_message(error, Message, Session) ->
    do_invocation_error(Message, Session);
    %% ctrd_invocation:invocation_error(Message, Session);
handle_message(yield, Message, Session) ->
    do_yield(Message, Session).
    %% ctrd_invocation:yield(Message, Session).


unregister_all(Session) ->
    Regs = cta_session:get_registrations(Session),
    SessId = cta_session:get_id(Session),

    Delete = fun(RegId, ok) ->
                     ctr_registration:delete(RegId, SessId),
                     ok
             end,
    lists:foldl(Delete, ok, Regs),
    ok.

do_register({register, _ReqId, _Options, Procedure} = Msg, Session) ->
    SessId = cta_session:get_id(Session),
    Realm = cta_session:get_realm(Session),

    Result = ctr_registration:new(Procedure, Realm, SessId),
    handle_register_result(Result, Msg, Session).


handle_register_result({created, Registration}, Msg, Session) ->
    ctr_broker:send_registration_meta_event(create, Session, Registration),
    send_registered(Msg, Registration, Session);
handle_register_result({error, procedure_exists}, Msg, Session) ->
    {ok, ReqId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(Session, ?ERROR(register, ReqId, #{},
                                              procedure_already_exists)),
    ok.

do_unregister({unregister, _ReqId, RegId} = Msg, Session) ->
    SessId = cta_session:get_id(Session),
    Result = ctr_registration:delete(RegId, SessId),
    handle_unregister_result(Result, Msg, Session).



do_call({call, _ReqId, _Options, Procedure, _Arguments, _ArgumentsKw} = Msg,
        Session) ->
    Internal = ctr_callee:is_procedure(Procedure),
    Result = match_registration(Internal, Procedure, Session),
    handle_call_registration(Result, Msg, Session).

match_registration(true, _, _) ->
    {ok, system};
match_registration(false, Procedure, Session) ->
    ctr_registration:match(Procedure, Session).


handle_unregister_result({atomic, {removed, Registration}}, Msg, Session) ->
    send_unregistered(Msg, Registration, Session);
handle_unregister_result({atomic, {deleted, Registration}}, Msg, Session) ->
    send_unregistered(Msg, Registration, Session),
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
handle_call_registration({ok, Registration},
                         {call, _, Options, _, Arguments, ArgumentsKw} = Msg,
                         CallerSession) ->
    CallerSessId = cta_session:get_id(CallerSession),
    RegId = ctr_registration:get_id(Registration),
    Callees = ctr_registration:get_callees(Registration),
    {ok, Invoc} = ctrd_invocation:new(RegId, Callees, Msg, CallerSession),
    DiscloseSession = cta_session:is_disclose_caller(CallerSession),
    DiscloseOption = maps:get(disclose_me, Options, false),
    Disclose = DiscloseSession or DiscloseOption,
    Details = maybe_set_caller(Disclose, CallerSessId),
    send_invocation(Invoc, RegId, Details, Arguments, ArgumentsKw),
    ok;
handle_call_registration({error, not_found}, Msg, Session) ->
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(call, RequestId, #{}, no_such_procedure),
    ok = ct_router:to_session(Session, Error),
    ok.

maybe_set_caller(true, SessionId) ->
    #{caller => SessionId};
maybe_set_caller(_, _) ->
    #{}.


do_yield({yield, InvocId, _Options, Arguments, ArgumentsKw}, CalleeSession) ->
    CalleeSessId = cta_session:get_id(CalleeSession),

    Result = ctrd_invocation:find_invocation(InvocId, CalleeSessId),
    maybe_send_result(Result, #{}, Arguments, ArgumentsKw).

maybe_send_result({ok, Invoc}, Details, Arguments, ArgumentsKw) ->
    CallerSessId = ctrd_invocation:get_caller_sess_id(Invoc),
    CallerReqId = ctrd_invocation:get_caller_req_id(Invoc),
    ok = ctrd_invocation:delete_invocation_if_configured(Invoc),
    ResultMsg = ?RESULT(CallerReqId, Details, Arguments, ArgumentsKw),
    send_message([CallerSessId], ResultMsg),
    ok;
maybe_send_result(_, _, _, _) ->
    ok.

do_invocation_error({error, invocation, InvocId, ErrorUri, Arguments,
                     ArgumentsKw}, CalleeSession) ->
    CalleeSessId = cta_session:get_id(CalleeSession),
    Result = ctrd_invocation:find_invocation(InvocId, CalleeSessId),
    maybe_send_error(Result, #{}, ErrorUri, Arguments, ArgumentsKw).


maybe_send_error({ok, Invoc}, Details, Uri, Arguments, ArgumentsKw) ->
    CallerSessId = ctrd_invocation:get_caller_sess_id(Invoc),
    CallerReqId = ctrd_invocation:get_caller_req_id(Invoc),
    ok = ctrd_invocation:delete_invocation_if_configured(Invoc),
    ResultMsg = ?ERROR(call, CallerReqId, Details, Uri, Arguments, ArgumentsKw),
    send_message([CallerSessId], ResultMsg),
    ok;
maybe_send_error(_, _, _, _, _) ->
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

send_invocation(Invocation, RegistrationId, Options, Args, ArgsKw) ->
    InvocId = ctrd_invocation:get_id(Invocation),
    CalleeIds = ctrd_invocation:get_callees(Invocation),
    InvocMsg = ?INVOCATION(InvocId, RegistrationId, Options, Args, ArgsKw),
    send_message(CalleeIds, InvocMsg),
    ok.


send_message([], _) ->
    ok;
send_message([SessionId | Tail], Msg) ->
    maybe_send(cta_session:lookup(SessionId), Msg),
    send_message(Tail, Msg).

maybe_send({ok, Session}, Msg) ->
    ct_router:to_session(Session, Msg);
maybe_send(_, _) ->
    ok.
