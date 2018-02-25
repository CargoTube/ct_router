-module(ctr_realm).

-export([new/3,
         get_name/1,
         get_role/2,
         get_auth_methods/1
        ]).


-record(realm, {name = undefined,
                authmethods = [],
                authmapping = []
               }).


new(Name, AuthMethods, AuthMapping) when is_binary(Name) ->
    #realm{name = Name,
           authmethods = AuthMethods,
           authmapping = AuthMapping
          }.


get_role(AuthId, #realm{authmapping = Mapping}) ->
    Result = lists:keyfind(AuthId, 1, Mapping),
    return_role(Result).

return_role([{_, Role}]) ->
    {ok, Role};
return_role(_) ->
    {error, not_found}.


get_name(#realm{name = Name}) ->
    Name.



get_auth_methods(#realm{authmethods = Methods}) ->
    Methods.
