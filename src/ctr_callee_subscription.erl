-module(ctr_callee_subscription).


-export([ list/3,
          get/3,
          subscriber/3,
          subscriber_count/3
        ]).


list(_Args, _Kw, Realm) ->
    SubsMap = ctr_subscription:separated_list_of_realm(Realm),
    { [SubsMap], undefined}.

get([Id], _Kw, Realm) ->
    Result = ctr_subscription:lookup(Id, Realm),
    handle_get_result(Result).

handle_get_result({ok, Subscription}) ->
    Keys = [id, created, match, uri],
    SubscriptionMap = ctr_subscription:to_map(Subscription),
    { [maps:with(Keys, SubscriptionMap) ], undefined};
handle_get_result(_) ->
    throw(no_such_subscription).

subscriber([Id], _Kw, Realm) ->
    Result = get_subscription_subscribers(Id, Realm),
    {[to_subscriber_list(Result)], undefined}.

subscriber_count([Id], _Kw, Realm) ->
    Result = get_subscription_subscribers(Id, Realm),
    {[length(to_subscriber_list(Result))], undefined}.


get_subscription_subscribers(Id, Realm) ->
    Result = ctr_subscription:lookup(Id, Realm),
    maybe_get_subscribers(Result).

maybe_get_subscribers({ok, Subscription}) ->
    {ok, ctr_subscription:get_subscribers(Subscription)};
maybe_get_subscribers(Error) ->
    Error.


to_subscriber_list({ok, Subs}) ->
    Subs;
to_subscriber_list(_) ->
    throw(no_such_subscription).
