-module(ctr_registration).

-include("ct_router.hrl").

-export([
         new/3,
         get_id/1,
         to_map/1,
         get_callees/1
        ]).

new(Procedure, Realm, SessId) ->
    NewId = ctr_utils:gen_global_id(),
    #ctr_registration{
       id = NewId,
       procedure = Procedure,
       realm = Realm,
       created = calendar:universal_time(),
       callee_sess_ids = [SessId]
      }.

get_id(#ctr_registration{id = Id}) ->
    Id.

to_map(#ctr_registration{id = Id, created = Created, procedure = Uri,
                         match = Match, invoke = Invoke,
                         callee_sess_ids = Callees }) ->
    #{id => Id,
      created => iso8601:format(Created),
      uri => Uri,
      match => Match,
      invoke => Invoke,
      callees => Callees}.

get_callees(#ctr_registration{ callee_sess_ids = Callees } ) ->
    Callees.
