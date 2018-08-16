-module(ctrd_invocation).


-include_lib("ct_msg/include/ct_msg.hrl").
-include_lib("ctr_data/include/ctr_data.hrl").

-export([
         new/4,

         get_id/1,
         get_callees/1,
         get_caller_req_id/1,
         get_caller_sess_id/1,
         add_result/2,

         get_invocation/2,
         delete_invocation_if_configured/1

        ]).


new(RegistrationId, CalleeIds, {call, CallerReqId, Options, Procedure,
                                Arguments, ArgumentsKw}, CallerSession) ->
    Realm = cta_session:get_realm(CallerSession),
    CallerSessId = cta_session:get_id(CallerSession),
    Invoc = #ctrd_invocation{
               caller_sess_id = CallerSessId,
               caller_req_id = CallerReqId,
               procedure = Procedure,
               options = Options,
               arguments = Arguments,
               argumentskw = ArgumentsKw,
               reg_id = RegistrationId,
               callees = CalleeIds,
               ts = calendar:universal_time(),
               realm = Realm
              },
    store_invocation(Invoc).


get_id(#ctrd_invocation{id = Id}) ->
    Id.

get_callees(#ctrd_invocation{callees = Callees}) ->
    Callees.

get_caller_sess_id(#ctrd_invocation{caller_sess_id = CallerSessId}) ->
    CallerSessId.

get_caller_req_id(#ctrd_invocation{caller_req_id = CallerReqId}) ->
    CallerReqId.

add_result(Result, #ctrd_invocation{id = Id, realm=Realm}) ->
    ctr_data:invocation_add_result(Result, Id, Realm).


store_invocation(Invoc) ->
    ctr_data:add_invocation(Invoc).


get_invocation(InvocId, CalleeSession) ->
    Realm = cta_session:get_realm(CalleeSession),
    ctr_data:get_invocation(InvocId, Realm).



delete_invocation_if_configured(Invocation) ->
    DoDelete = application:get_env(ct_router, delete_invocation, false),
    maybe_delete_invocation(DoDelete, Invocation).

maybe_delete_invocation(true, Invocation) ->
    ctr_data:remove_invocation(Invocation);
maybe_delete_invocation(false, _) ->
    ok.
