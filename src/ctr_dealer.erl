-module(ctr_dealer).


-include_lib("ct_msg/include/ct_msg.hrl").
-include("ct_router.hrl").

-export([
         handle_message/3,
         unregister_all/1,
         get_registration_lists/1,
         get_registration/2,

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


get_registration_lists(Realm) ->
    {ok, Registrations} = ctr_dealer_data:get_registrations_of_realm(Realm),

    Separator = fun(#ctr_registration{ id = Id, match = exact },
                    {ExactList, PrefixList, WildcardList}) ->
                        { [ Id | ExactList ], PrefixList, WildcardList };
                   (#ctr_registration{ id = Id, match = prefix },
                    {ExactList, PrefixList, WildcardList}) ->
                        { ExactList, [ Id | PrefixList], WildcardList };
                   (#ctr_registration{ id = Id, match = wildcard },
                    {ExactList, PrefixList, WildcardList}) ->
                        { ExactList, PrefixList, [ Id | WildcardList ] }
                end,
    {E, P, W} = lists:foldl(Separator, {[], [], []}, Registrations),
    {ok, #{exact => E, prefix => P, wildcard => W}}.


get_registration(Id, Realm) ->
    Result = ctr_dealer_data:lookup_regisration(Id, Realm),
    maybe_convert_to_map(Result).

maybe_convert_to_map({ok, #ctr_registration{id = Id, created = Created,
                                            procedure = Uri, match = Match,
                                            invoke = Invoke,
                                            callee_sess_ids = Callees }}) ->

    {ok, #{id => Id, created => iso8601:format(Created), uri => Uri,
           match => Match, invoke => Invoke, callees => Callees}};
maybe_convert_to_map(Other) ->
    Other.




do_register({register, _ReqId, _Options, Procedure} = Msg, Session) ->
    SessId = cta_session:get_id(Session),
    Realm = cta_session:get_realm(Session),
    NewId = ctr_utils:gen_global_id(),


    NewReg = #ctr_registration{
                id = NewId,
                procedure = Procedure,
                realm = Realm,
                created = calendar:universal_time(),
                callee_sess_ids = [SessId]
               },
    Result = ctr_dealer_data:store_registration(NewReg),
    handle_register_result(Result, Msg, Session).


handle_register_result({created, #ctr_registration{id = RegId}},
                       Msg, Session) ->
    %% TODO: meta events
    send_registered(Msg, RegId, Session);
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
    #ctr_registration{id = RegId} = Registration,
    send_unregistered(Msg, RegId, Session);
handle_unregister_result({atomic, {deleted, Registration}}, Msg, Session) ->
    #ctr_registration{id = RegId} = Registration,
    send_unregistered(Msg, RegId, Session),
    %% TODO: meta events
    ok;
handle_unregister_result({atomic, {error, not_found}}, Msg, Session) ->
    {unregister, _, RegId} = Msg,
    HasRegistration = cta_session:has_registration(RegId, Session),
    maybe_send_unregistered(HasRegistration, Msg, RegId, Session).

handle_call_registration({ok, system}, Msg, Session) ->
    Response = ctr_callee:handle_call(Msg, Session),
    ok = ct_router:to_session(Session, Response),
    ok;
handle_call_registration({ok, Registration}, Msg, Session) ->
    #ctr_registration{
       id = RegistrationId,
       callee_sess_ids = CalleeIds
      } = Registration,

    ctrd_invocation:new(RegistrationId, CalleeIds, Msg, Session),
    ok;
handle_call_registration({error, not_found}, Msg, Session) ->
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(call, RequestId, #{}, no_such_procedure),
    ok = ct_router:to_session(Session, Error),
    ok.


send_registered(Msg, RegId, Session) ->
    %% TODO: meta events
    {ok, NewSession} = cta_session:add_registration(RegId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?REGISTERED(RequestId, RegId)),
    ok.

maybe_send_unregistered(true, Msg, RegId, Session) ->
    send_unregistered(Msg, RegId, Session);
maybe_send_unregistered(false, Msg, _RegId, Session) ->
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(unregister, RequestId, #{}, no_such_registration),
    ok = ct_router:to_session(Session, Error),
    ok.

send_unregistered(Msg, RegId, Session) ->
    %% TODO: meta events
    {ok, NewSession} = cta_session:remove_registration(RegId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?UNREGISTERED(RequestId)),
    ok.
