-module(ctrd_invocation).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         new/8,
         yield/4,

         init/0
        ]).

-record(ctrd_invocation, {
          id = undefined,
          caller_sess_id = undefined,
          caller_req_id = undefined,
          callees = [],
          results = [],
          realm = undefined
         }).

init() ->
    create_table().

new(Realm, CallerSessId, CallerReqId, _Options, Arguments,
    ArgumentsKw, RegistrationId, CalleeIds) ->
    Invoc = #ctrd_invocation{
                caller_sess_id = CallerSessId,
                caller_req_id = CallerReqId,
                callees = CalleeIds,
                realm = Realm
               },
    {ok, Invoc} = store_invocation(Invoc),
    send_invocation(Invoc, RegistrationId, #{}, Arguments, ArgumentsKw),
    ok.

%% invocation_error() ->
%%     ok.

yield(InvocId, _Options, Arguments, ArgumentsKw) ->
    Result = find_invocation(InvocId),
    maybe_send_result(Result, #{}, Arguments, ArgumentsKw).

maybe_send_result({ok, Invoc}, Details, Arguments, ArgumentsKw) ->
    #ctrd_invocation{
       caller_sess_id = CallerSessId,
       caller_req_id = CallerReqId
      } = Invoc,
    ResultMsg = ?RESULT(CallerReqId, Details, Arguments, ArgumentsKw),
    send_message(CallerSessId ,ResultMsg),
    ok;
maybe_send_result(_, _, _, _) ->
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
    maybe_send(ctr_session:lookup(SessionId), Msg),
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


find_invocation(InvocId) ->
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
