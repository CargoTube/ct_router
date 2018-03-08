-module(ctrd_invocation).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         new/8,

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

new(Realm, CallerSessId, CallerReqId,
    _Options, Arguments, ArgumentsKw,
    RegistrationId, CalleeIds) ->
    Invoc = #ctrd_invocation{
                caller_sess_id = CallerSessId,
                caller_req_id = CallerReqId,
                callees = CalleeIds,
                realm = Realm
               },
    {ok, InvocId} = store_invocation(Invoc),
    InvocMsg = ?INVOCATION(InvocId, RegistrationId, #{}, Arguments,
                           ArgumentsKw),
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
                        {ok, NewId};
                    _ ->
                        {error, id_exists}
                end
        end,
    Result = mnesia:transaction(StoreInvocation),
    handle_invocation_store_result(Result, NewInvoc).


handle_invocation_store_result({atomic, {ok, Id}}, _) ->
    {ok, Id};
handle_invocation_store_result({atomic, {error, id_exists}}, Invoc) ->
    store_invocation(Invoc).



create_table() ->
    mnesia:delete_table(ctrd_invocation),
    InvDef = [{attributes, record_info(fields, ctrd_invocation)},
              {ram_copies, [node()]},
              {index, [realm, procedure, match]}
             ],
    {atomic, ok} = mnesia:create_table(ctrd_invocation, InvDef),
    ok.
