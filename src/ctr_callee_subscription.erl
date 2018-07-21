-module(ctr_callee_subscription).

-include("ct_router.hrl").

-export([ list/3,
          get/3,
          subscriber/3,
          subscriber_count/3
        ]).


list(_Args, _Kw, Realm) ->
    {ok, Subscriptions} = ctr_broker_data:get_subscription_list(Realm),

    Separator = fun(#ctr_subscription{ id = Id, match = exact },
                    {ExactList, PrefixList, WildcardList}) ->
                        { [ Id | ExactList ], PrefixList, WildcardList };
                   (#ctr_subscription{ id = Id, match = prefix },
                    {ExactList, PrefixList, WildcardList}) ->
                        { ExactList, [ Id | PrefixList], WildcardList };
                   (#ctr_subscription{ id = Id, match = wildcard },
                    {ExactList, PrefixList, WildcardList}) ->
                        { ExactList, PrefixList, [ Id | WildcardList ] }
                end,
    {E, P, W} = lists:foldl(Separator, {[], [], []}, Subscriptions),
    { [#{exact => E, prefix => P, wildcard => W}], undefined}.

get([Id], _Kw, Realm) ->
    Result = ctr_broker_data:get_subscription(Id, Realm),
    handle_get_result(Result).

handle_get_result({ok, #ctr_subscription{id = Id, created = Created,
                                             uri = Uri, match = Match }}) ->

    { [ #{id => Id, created => iso8601:format(Created), match => Match,
           uri => Uri}], undefined};
handle_get_result(_) ->
    throw(no_such_registration).

subscriber([Id], _Kw, Realm) ->
    Result = ctr_broker:get_map(Id, Realm),
    {[to_subscriber_list(Result)], undefined}.

subscriber_count([Id], _Kw, Realm) ->
    Result = ctr_broker:get_map(Id, Realm),
    {[length(to_subscriber_list(Result))], undefined}.

to_subscriber_list({ok, #{subs := Subs}}) ->
    Subs;
to_subscriber_list(_) ->
    throw(no_such_registration).
