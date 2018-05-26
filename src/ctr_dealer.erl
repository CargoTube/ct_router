-module(ctr_dealer).


-include_lib("ct_msg/include/ct_msg.hrl").
-include("ct_router.hrl").

-export([
         handle_message/3,
         unregister_all/1,

         init/0
        ]).

init() ->
    ctrd_invocation:init(),
    create_table().


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
                     delete_registration(RegId, SessId),
                     ok
             end,
    lists:foldl(Delete, ok, Regs),
    ok.


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
    Result = store_registration(NewReg),
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
    Result = delete_registration(RegId, SessId),
    handle_unregister_result(Result, Msg, Session).



do_call({call, _ReqId, _Options, Procedure, _Arguments, _ArgumentsKw} = Msg,
        Session) ->
    Internal = ctr_callee:is_procedure(Procedure),
    Result = find_registration(Internal, Procedure, Session),
    handle_call_registration(Result, Msg, Session).



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
    ctrd_callee:handle_call(Msg, Session),
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


store_registration(Registration) ->
    #ctr_registration{
       id = NewId,
       procedure = Procedure,
       realm = Realm
      } = Registration,

    MatchHead = #ctr_registration{procedure=Procedure, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Register =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, write) of
                    [] ->
                        case mnesia:wread({ctr_registration, NewId}) of
                            [] ->
                                ok = mnesia:write(Registration),
                                {created, Registration};
                            _ ->
                                {error, id_exists}
                        end;
                    _ ->
                        {error, procedure_exists}
                end
        end,
    Result = mnesia:transaction(Register),
    handle_store_result(Result, Registration).

handle_store_result({atomic, {created, Registration}}, _) ->
    {created, Registration};
handle_store_result({atomic, {error, procedure_exists}}, _Registration) ->
    {error, procedure_exists};
handle_store_result({atomic, {error, id_exists}}, Registration) ->
    store_registration(Registration).


find_registration(true, _Procedure, _Session) ->
    {ok, system};
find_registration(false, Procedure, Session) ->
    Realm = cta_session:get_realm(Session),

    MatchHead = #ctr_registration{procedure=Procedure, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    FindCallee =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, write) of
                    [Registration] ->
                        {ok, Registration};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(FindCallee),
    handle_find_result(Result).

handle_find_result({atomic, {ok, Registration}}) ->
    {ok, Registration};
handle_find_result({atomic, {error, not_found}}) ->
    {error, not_found}.




delete_registration(RegId, SessId) ->
    Unregister =
        fun() ->
                case mnesia:wread({ctr_registration, RegId}) of
                    [#ctr_registration{callee_sess_ids = Callees} = Reg] ->
                        NewCallees = lists:delete(SessId, Callees),
                        NewReg = Reg#ctr_registration{
                                   callee_sess_ids = NewCallees},
                        case NewCallees of
                            [] ->
                                mnesia:delete({ctr_registration, RegId}),
                                {deleted, NewReg};

                            _ ->
                                ok = mnesia:write(NewReg),
                                {removed, NewReg}
                        end;
                    [] ->
                        {error, not_found}
                end
        end,
    mnesia:transaction(Unregister).



create_table() ->
    mnesia:delete_table(ctr_registration),
    RegDef = [{attributes, record_info(fields, ctr_registration)},
              {ram_copies, [node()]},
              {index, [realm, procedure, match]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_registration, RegDef),
    ok.
