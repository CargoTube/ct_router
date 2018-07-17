-module(ctr_callee_subscription).

-export([ list/3,
          get/3,
          subscriber/3,
          subscriber_count/3
        ]).


list(_Args, _Kw, Realm) ->
    {ok, Map} = ctr_broker:get_list_map(Realm),
    {[Map], undefined}.

get([Id], _Kw, Realm) ->
    Result = ctr_broker:get_map(Id, Realm),
    handle_get_result(Result).

handle_get_result({ok, Map}) ->
    Keys = [id, match, created, uri],
    {[maps:with(Keys, Map)], undefined};
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
