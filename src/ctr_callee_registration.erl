-module(ctr_callee_registration).

-export([list/3,
         get/3,
         callees/3,
         callee_count/3
        ]).

list(_, _, Realm) ->
    {ok, Map} = ctr_dealer:get_registration_lists(Realm),
    {[Map], undefined}.

get([Id], _, Realm) ->
    Result = ctr_dealer:lookup_registration(Id, Realm),
    handle_registration_result(Result).

callees([Id], _, Realm) ->
    Result = ctr_dealer:lookup_registration(Id, Realm),
    handle_callee_list_result(Result).


callee_count([Id], _, Realm) ->
    Result = ctr_dealer:lookup_registration(Id, Realm),
    handle_callee_count_result(Result).

handle_registration_result({ok, RegistrationMap}) ->
    Keys = [id, created, uri, match, invoke],
    {[maps:with(Keys, RegistrationMap)], undefined};
handle_registration_result({error, not_found}) ->
    throw(no_such_registration).

handle_callee_list_result({ok, #{callees := Callees}}) ->
    {[Callees], undefined};
handle_callee_list_result({error, not_found}) ->
    throw(no_such_registration).

handle_callee_count_result({ok, #{callees := Callees}}) ->
    {[length(Callees)], undefined};
handle_callee_count_result({error, not_found}) ->
    throw(no_such_registration).
