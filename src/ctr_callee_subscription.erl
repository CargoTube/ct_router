-module(ctr_callee_subscription).


-export([ list/3,
          get/3,
          lookup/3,
          match/3,
          subscriber/3,
          subscriber_count/3
        ]).


list(undefined, undefined, Realm) ->
    SubsMap = ctr_subscription:separated_list_of_realm(Realm),
    { [SubsMap], undefined}.

get([Id], undefined, Realm) ->
    Result = ctr_subscription:get(Id, Realm),
    handle_get_result(Result).

handle_get_result({ok, Subscription}) ->
    Keys = [id, created, match, uri],
    SubscriptionMap = ctr_subscription:to_map(Subscription),
    { [maps:with(Keys, SubscriptionMap) ], undefined};
handle_get_result(_) ->
    throw(no_such_subscription).

lookup([Uri], undefined, Realm) ->
    lookup([Uri, #{}], undefined, Realm);
lookup([Uri, Options], undefined, Realm) ->
    Result = ctr_subscription:lookup(Uri, Options, Realm),
    handle_lookup_result(Result).

handle_lookup_result({ok, Subsciption}) ->
    Id = ctr_subscription:get_id(Subsciption),
    {[Id], undefined};
handle_lookup_result(_) ->
    {[null], undefined}.


match([Uri], undefined, Realm) ->
    Result = ctr_subscription:match(Uri, Realm),
    handle_match_result(Result).


handle_match_result({ok, []})  ->
    {[null], undefined};
handle_match_result({ok, SubscriptionList})  ->
    Convert = fun(Subscription, List) ->
                      Id = ctr_subscription:get_id(Subscription),
                      [ Id | List]
              end,
    IdList = lists:foldl(Convert, [], SubscriptionList),
    {[lists:reverse(IdList)], undefined};
handle_match_result(_) ->
    {[null], undefined}.


subscriber([Id], undefined, Realm) ->
    Result = get_subscription_subscribers(Id, Realm),
    {[to_subscriber_list(Result)], undefined}.

subscriber_count([Id], _Kw, Realm) ->
    Result = get_subscription_subscribers(Id, Realm),
    {[length(to_subscriber_list(Result))], undefined}.


get_subscription_subscribers(Id, Realm) ->
    Result = ctr_subscription:get(Id, Realm),
    maybe_get_subscribers(Result).

maybe_get_subscribers({ok, Subscription}) ->
    {ok, ctr_subscription:get_subscribers(Subscription)};
maybe_get_subscribers(Error) ->
    Error.


to_subscriber_list({ok, Subs}) ->
    Subs;
to_subscriber_list(_) ->
    throw(no_such_subscription).
