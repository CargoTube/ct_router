-module(ctr_callee_registration).

-include("ct_router.hrl").
-export([list/3,
         get/3,
         callees/3,
         callee_count/3
        ]).

list(_, _, Realm) ->
    {ok, Registrations} = ctr_dealer_data:get_registrations_of_realm(Realm),

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
    { [ #{exact => E, prefix => P, wildcard => W} ], undefined}.

get([Id], _, Realm) ->
    Result = ctr_dealer_data:lookup_regisration(Id, Realm),
    maybe_convert_to_map(Result).

maybe_convert_to_map({ok, Registration}) ->
    Keys = [id, created, uri, match, invoke],
    Map = ctr_registration:to_map(Registration),
    {[maps:with(Keys, Map)], undefined};
maybe_convert_to_map(_Other) ->
    throw(no_such_registration).

callees([Id], _, Realm) ->
    Result = ctr_dealer_data:lookup_regisration(Id, Realm),
    handle_callee_list_result(Result).


callee_count([Id], _, Realm) ->
    Result = ctr_dealer_data:lookup_regisration(Id, Realm),
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
