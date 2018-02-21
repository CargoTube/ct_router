-module(ctr_realm).

-export([create/1,
         get_name/1
        ]).


-record(realm, {name = undefined}).


create(Name) when is_binary(Name) ->
    #realm{name = Name}.

get_name(#realm{name = Name}) ->
    Name.
