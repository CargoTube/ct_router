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
    {[Map], undefined};
handle_get_result(_) ->
    throw(no_such_registration).

subscriber(_Args, _Kw, _Realm) ->
    throw(no_such_procedure).

subscriber_count(_Args, _Kw, _Realm) ->
    throw(no_such_procedure).
