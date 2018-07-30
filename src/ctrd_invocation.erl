-module(ctrd_invocation).


-include_lib("ct_msg/include/ct_msg.hrl").
-include("ct_router.hrl").

-export([
         new/4,

         get_id/1,
         get_callees/1,
         get_caller_req_id/1,
         get_caller_sess_id/1,

         find_invocation/2,
         delete_invocation_if_configured/1,

         init/0
        ]).

init() ->
    create_table().

new(RegistrationId, CalleeIds, {call, CallerReqId, _, Procedure, _, _},
    CallerSession) ->
    Realm = cta_session:get_realm(CallerSession),
    CallerSessId = cta_session:get_id(CallerSession),
    Invoc0 = #ctrd_invocation{
                caller_sess_id = CallerSessId,
                caller_req_id = CallerReqId,
                procedure = Procedure,
                reg_id = RegistrationId,
                callees = CalleeIds,
                realm = Realm
               },
    store_invocation(Invoc0).


get_id(#ctrd_invocation{id = Id}) ->
    Id.

get_callees(#ctrd_invocation{callees = Callees}) ->
    Callees.

get_caller_sess_id(#ctrd_invocation{caller_sess_id = CallerSessId}) ->
    CallerSessId.

get_caller_req_id(#ctrd_invocation{caller_req_id = CallerReqId}) ->
    CallerReqId.

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


delete_invocation_if_configured(Invocation) ->
    DoDelete = application:get_env(ct_router, delete_invocation, false),
    maybe_delete_invocation(DoDelete, Invocation).

maybe_delete_invocation(true, #ctrd_invocation{id=Id}) ->
    DeleteInvocation =
        fun() ->
                mnesia:delete({ctrd_invocation, Id})
        end,
    Result = mnesia:transaction(DeleteInvocation),
    handle_invocation_delete_result(Result);
maybe_delete_invocation(false, _) ->
    ok.


handle_invocation_delete_result({atomic, ok}) ->
    ok;
handle_invocation_delete_result(Error) ->
    {error, Error}.


create_table() ->
    mnesia:delete_table(ctrd_invocation),
    InvDef = [{attributes, record_info(fields, ctrd_invocation)},
              {disc_copies, [node()]},
              {index, [realm]}
             ],
    {atomic, ok} = mnesia:create_table(ctrd_invocation, InvDef),
    ok.
