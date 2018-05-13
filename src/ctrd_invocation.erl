-module(ctrd_invocation).


-include_lib("ct_msg/include/ct_msg.hrl").
-include("ct_router.hrl").

-export([
         new/4,
         invocation_error/2,
         yield/2,

         init/0
        ]).

init() ->
    create_table().

new(RegistrationId, CalleeIds, {call, CallerReqId, _Options, _Procedure,
                                Arguments, ArgumentsKw} , CallerSession) ->
    Realm = cta_session:get_realm(CallerSession),
    CallerSessId = cta_session:get_id(CallerSession),

    Invoc0 = #ctrd_invocation{
                caller_sess_id = CallerSessId,
                caller_req_id = CallerReqId,
                callees = CalleeIds,
                realm = Realm
               },
    {ok, Invoc} = store_invocation(Invoc0),
    send_invocation(Invoc, RegistrationId, #{}, Arguments, ArgumentsKw),
    ok.

invocation_error({error, invocation, InvocId, ErrorUri, Arguments, ArgumentsKw},
                 CalleeSession) ->
    CalleeSessId = cta_session:get_id(CalleeSession),
    Result = find_invocation(InvocId, CalleeSessId),
    maybe_send_error(Result, #{}, ErrorUri, Arguments, ArgumentsKw).


yield({yield, InvocId, _Options, Arguments, ArgumentsKw}, CalleeSession) ->
    CalleeSessId = cta_session:get_id(CalleeSession),

    Result = find_invocation(InvocId, CalleeSessId),
    maybe_send_result(Result, #{}, Arguments, ArgumentsKw).

maybe_send_result({ok, Invoc}, Details, Arguments, ArgumentsKw) ->
    #ctrd_invocation{
       caller_sess_id = CallerSessId,
       caller_req_id = CallerReqId
      } = Invoc,
    ResultMsg = ?RESULT(CallerReqId, Details, Arguments, ArgumentsKw),
    send_message([CallerSessId] ,ResultMsg),
    ok;
maybe_send_result(_, _, _, _) ->
    ok.

maybe_send_error({ok, Invoc}, Details, Uri, Arguments, ArgumentsKw) ->
    #ctrd_invocation{
       caller_sess_id = CallerSessId,
       caller_req_id = CallerReqId
      } = Invoc,
    ResultMsg = ?ERROR(call, CallerReqId, Details, Uri, Arguments, ArgumentsKw),
    send_message([CallerSessId] ,ResultMsg),
    ok;
maybe_send_error(_, _, _, _, _) ->
    ok.

send_invocation(Invocation, RegistrationId, Options, Args, ArgsKw) ->
    #ctrd_invocation{
       id = InvocId,
       callees = CalleeIds } = Invocation,
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



store_invocation(Invoc) ->
    NewId = ctr_utils:gen_global_id(),
    NewInvoc = Invoc#ctrd_invocation{id = NewId},
    StoreInvocation =
        fun() ->
                case mnesia:wread({ctrd_invocation, NewId}) of
                    [] ->
                        ok = mnesia:write(NewInvoc),
                        {ok, NewInvoc};
                    _ ->
                        {error, id_exists}
                end
        end,
    Result = mnesia:transaction(StoreInvocation),
    handle_invocation_store_result(Result, NewInvoc).


handle_invocation_store_result({atomic, {ok, Invoc}}, _) ->
    {ok, Invoc};
handle_invocation_store_result({atomic, {error, id_exists}}, Invoc) ->
    store_invocation(Invoc).


find_invocation(InvocId, _CalleeSessId) ->
    FindInvocation =
        fun() ->
                case mnesia:read({ctrd_invocation, InvocId}) of
                    [Invoc] -> {ok, Invoc};
                    _ -> {error, not_found}
                end
        end,
    Result = mnesia:transaction(FindInvocation),
    handle_invocation_find_result(Result).

handle_invocation_find_result({atomic, {ok, Invocation}}) ->
    {ok, Invocation};
handle_invocation_find_result(_) ->
    {error, not_found}.

create_table() ->
    mnesia:delete_table(ctrd_invocation),
    InvDef = [{attributes, record_info(fields, ctrd_invocation)},
              {ram_copies, [node()]},
              {index, [realm]}
             ],
    {atomic, ok} = mnesia:create_table(ctrd_invocation, InvDef),
    ok.
