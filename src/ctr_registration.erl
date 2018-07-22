-module(ctr_registration).

-include("ct_router.hrl").

-export([
         init/0,

         new/3,
         get_id/1,
         to_map/1,
         get_callees/1,

         match/2,
         lookup/2,

         list_of_realm/1,
         separated_list_of_realm/1,
         delete/2
        ]).

new(Procedure, Realm, SessId) ->
    NewId = ctr_utils:gen_global_id(),
    NewReg = #ctr_registration{
       id = NewId,
       procedure = Procedure,
       realm = Realm,
       created = calendar:universal_time(),
       callee_sess_ids = [SessId]
      },
    ctr_dealer_data:store_registration(NewReg).

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

lookup(RegistrationId, Realm) ->
    ctr_dealer_data:lookup_regisration(RegistrationId, Realm).

match(Procedure, Session) ->
    ctr_dealer_data:match_registration(Procedure, Session).

list_of_realm(Realm) ->
    ctr_dealer_data:get_registrations_of_realm(Realm).

separated_list_of_realm(Realm) ->
    {ok, Registrations} = list_of_realm(Realm),
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
    #{exact => E, prefix => P, wildcard => W}.


delete(RegistrationId, SessionId) ->
  ctr_dealer_data:delete_registration(RegistrationId, SessionId).

init() ->
    ctr_dealer_data:init().
