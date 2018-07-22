-module(ctr_callee_registration).

-export([list/3,
         get/3,
         callees/3,
         callee_count/3
        ]).

list(_, _, Realm) ->
    RegMap = ctr_registration:separated_list_of_realm(Realm),
    { [ RegMap ], undefined}.

get([Id], _, Realm) ->
    Result = ctr_registration:lookup(Id, Realm),
    maybe_convert_to_map(Result).

maybe_convert_to_map({ok, Registration}) ->
    Keys = [id, created, uri, match, invoke],
    Map = ctr_registration:to_map(Registration),
    {[maps:with(Keys, Map)], undefined};
maybe_convert_to_map(_Other) ->
    throw(no_such_registration).

callees([Id], _, Realm) ->
    Result = ctr_registration:lookup(Id, Realm),
    handle_callee_list_result(Result).


callee_count([Id], _, Realm) ->
    Result = ctr_registration:lookup(Id, Realm),
    handle_callee_count_result(Result).

handle_callee_list_result({ok, Registration}) ->
    Callees = ctr_registration:get_callees(Registration),
    {[Callees], undefined};
handle_callee_list_result({error, not_found}) ->
    throw(no_such_registration).

handle_callee_count_result({ok, Registration}) ->
    Callees = ctr_registration:get_callees(Registration),
    {[length(Callees)], undefined};
handle_callee_count_result({error, not_found}) ->
    throw(no_such_registration).
