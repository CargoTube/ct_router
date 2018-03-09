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
          procedure = undefined,
          match = exact,
          created = undefined,
          callee_sess_ids = []
         }).

init() ->
    ctrd_invocation:init(),
    create_table().


handle_message(register, Message, Session) ->
    do_register(Message, Session);
handle_message(unregister, Message, Session) ->
    do_unregister(Message, Session);
handle_message(call, Message, Session) ->
    do_call(Message, Session);
handle_message(yield, Message, Session) ->
    ctrd_invocation:yield(Message, Session).


unregister_all(Session) ->
    Regs = ctr_session:get_registrations(Session),
    SessId = ctr_session:get_id(Session),

    Delete = fun(RegId, ok) ->
                     delete_registration(RegId, SessId),
                     ok
             end,
    lists:foldl(Delete, ok, Regs),
    ok.


do_register({register, _ReqId, Options, Procedure} = Msg, Session) ->
    lager:debug("dealer: register ~p ~p", [Procedure, Options]),
    SessId = ctr_session:get_id(Session),
    Realm = ctr_session:get_realm(Session),
    NewId = ctr_utils:gen_global_id(),

    MatchHead = #ctr_registration{procedure=Procedure, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    NewReg = #ctr_registration{
                id = NewId,
                procedure = Procedure,
                realm = Realm,
                created = calendar:universal_time(),
                callee_sess_ids = [SessId]
               },

    Register =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, write) of
                    [] ->
                        case mnesia:wread({ctr_registration, NewId}) of
                            [] ->
                                ok = mnesia:write(NewReg),
                                {created, NewReg};
                            _ ->
                                {error, id_exists}
                        end;
                    _ ->
                        {error, procedure_exists}
                end
        end,
    Result = mnesia:transaction(Register),
    handle_register_result(Result, Msg, Session).

do_unregister({unregister, ReqId, RegId} = Msg, Session) ->
    lager:debug("dealer: unregister ~p ~p", [ReqId, RegId]),
    SessId = ctr_session:get_id(Session),
    Result = delete_registration(RegId, SessId),
    handle_unregister_result(Result, Msg, Session).



do_call(Msg, Session) ->
    call = erlang:element(1, Msg),
    Procedure = erlang:element(4, Msg),
    Realm = ctr_session:get_realm(Session),

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
    handle_call_callee(Result, Msg, Session).


handle_register_result({atomic, {created, Registration}}, Msg, Session) ->
    #ctr_registration{id = RegId} = Registration,
    %% TODO: meta events
    send_registered(Msg, RegId, Session);
handle_register_result({atomic, {error, procedure_exists}}, Msg, Session) ->
    ReqId = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(Session, ?ERROR(register, ReqId, #{},
                                              procedure_already_exists)),
    ok;
handle_register_result({atomic, {error, id_exists}}, Msg, Session) ->
    do_register(Msg, Session).



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
    HasRegistration = ctr_session:has_registration(RegId, Session),
    maybe_send_unregistered(HasRegistration, Msg, RegId, Session).

handle_call_callee({atomic, {ok, Registration}}, Msg, Session) ->
    #ctr_registration{
       id = RegistrationId,
       callee_sess_ids = CalleeIds
      } = Registration,
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Options = erlang:element(3, Msg),
    Arguments = get_arguments(Msg),
    ArgumentsKw = get_argumentskw(Msg),
    Realm = ctr_session:get_realm(Session),
    CallerId = ctr_session:get_id(Session),

    ctrd_invocation:new(Realm, CallerId, RequestId, Options, Arguments,
                        ArgumentsKw, RegistrationId, CalleeIds),
    ok;
handle_call_callee({atomic, {error, not_found}}, Msg, Session) ->
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    Error = ?ERROR(call, RequestId, #{}, no_such_procedure),
    ok = ct_router:to_session(Session, Error),
    ok.


send_registered(Msg, RegId, Session) ->
    %% TODO: meta events
    {ok, NewSession} = ctr_session:add_registration(RegId, Session),
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
    {ok, NewSession} = ctr_session:remove_registration(RegId, Session),
    {ok, RequestId} = ct_msg:get_request_id(Msg),
    ok = ct_router:to_session(NewSession, ?UNREGISTERED(RequestId)),
    ok.

delete_registration(RegId, SessId) ->
    lager:debug("dealer: delete registration ~p",[RegId]),
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

get_arguments({call, _, _, _, Arguments}) ->
    Arguments;
get_arguments({call, _, _, _, Arguments, _}) ->
    Arguments;
get_arguments(_) ->
    undefined.

get_argumentskw({call, _, _, _, _, ArgumentsKw}) ->
    ArgumentsKw;
get_argumentskw(_) ->
    undefined.




create_table() ->
    mnesia:delete_table(ctr_registration),
    RegDef = [{attributes, record_info(fields, ctr_registration)},
              {ram_copies, [node()]},
              {index, [realm, procedure, match]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_registration, RegDef),
    ok.
