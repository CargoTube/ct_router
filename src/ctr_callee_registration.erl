-module(ctr_callee_registration).

-export([list/3,
         get/3,
         lookup/3,
         match/3,
         list_callees/3,
         count_callees/3
        ]).

list(_, _, Realm) ->
    RegMap = ctr_registration:separated_list_of_realm(Realm),
    { [ RegMap ], undefined}.

get([Id], _, Realm) ->
    Result = ctr_registration:get(Id, Realm),
    maybe_convert_to_map(Result).

maybe_convert_to_map({ok, Registration}) ->
    Keys = [id, created, uri, match, invoke],
    Map = ctr_registration:to_map(Registration),
    {[maps:with(Keys, Map)], undefined};
maybe_convert_to_map(_Other) ->
    throw(no_such_registration).

match([Procedure], _ArgsKw, Realm) ->
    Result = ctr_registration:match(Procedure, Realm),
    handle_match_result(Result).

handle_match_result({ok, Registration}) ->
    Id = ctr_registration:get_id(Registration),
    {[Id], undefined};
handle_match_result(_) ->
    {[null], undefined}.



lookup([Procedure, OptionsIn], _ArgsKw, Realm) ->
    Options = ct_msg_conversion:value_to_internal(OptionsIn),
    Result = ctr_registration:lookup(Procedure, Options, Realm),
    handle_lookup_result(Result);
lookup([Procedure], ArgsKw, Realm) ->
    lookup([Procedure, #{}], ArgsKw, Realm).

handle_lookup_result({ok, Registration}) ->
    Id = ctr_registration:get_id(Registration),
    {[Id], undefined};
handle_lookup_result(_) ->
    {[null], undefined}.



list_callees([Id], _, Realm) ->
    Result = ctr_registration:get(Id, Realm),
    handle_callee_list_result(Result).

handle_callee_list_result({ok, Registration}) ->
    Callees = ctr_registration:get_callees(Registration),
    {[Callees], undefined};
handle_callee_list_result({error, not_found}) ->
    throw(no_such_registration).

count_callees([Id], _, Realm) ->
    Result = ctr_registration:get(Id, Realm),
    handle_callee_count_result(Result).


handle_callee_count_result({ok, Registration}) ->
    Callees = ctr_registration:get_callees(Registration),
    {[length(Callees)], undefined};
handle_callee_count_result({error, not_found}) ->
    throw(no_such_registration).
