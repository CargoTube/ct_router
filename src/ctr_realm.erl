-module(ctr_realm).

-export([new/3,
         close/1,

         get_name/1,
         get_role/2,
         get_auth_methods/1,

         lookup/1,
         init/0
        ]).


-record(realm, {name = undefined,
                authmethods = [],
                authmapping = []
               }).

init() ->
    create_table().

new(Name, AuthMethods, AuthMapping) when is_binary(Name) ->
    Realm = #realm{name = Name,
                   authmethods = AuthMethods,
                   authmapping = AuthMapping
                  },
    try_saving_realm(Realm, true).

close(RealmName) ->
    delete_by_name(RealmName).

get_role(AuthId, #realm{authmapping = Mapping}) ->
    Result = lists:keyfind(AuthId, 1, Mapping),
    return_role(Result).

return_role({_, Role}) ->
    {ok, Role};
return_role(_) ->
    {error, not_found}.


get_name(#realm{name = Name}) ->
    Name.


get_auth_methods(#realm{authmethods = Methods}) ->
    Methods.

lookup(Name) ->
    lookup_by_name(Name).

lookup_by_name(Name) ->
    Lookup = fun() ->
                     case mnesia:read({realm, Name}) of
                         [Realm] ->
                             {ok, Realm};
                         [] ->
                             {error, not_found};
                         _ ->
                             {error, bad_state}
                     end
             end,
    Result = mnesia:transaction(Lookup),
    unify_result(Result).

try_saving_realm(#realm{name = Name} = Realm, New) ->
    Store = fun(true) ->
                    case mnesia:wread({realm, Name}) of
                        [] ->
                            mnesia:write(Realm),
                            {ok, Realm};
                        _ ->
                            {error, exists}
                    end;
               (false) ->
                    ok = mnesia:write(Realm),
                    {ok, Realm}
            end,
    Result = mnesia:transaction(Store, [New]),
    unify_result(Result).


delete_by_name(Name) ->
    Delete = fun() ->
                     case mnesia:wread({realm, Name}) of
                         [_] ->
                             mnesia:delete({realm, Name});
                         [] ->
                             {error, not_found}
                     end
             end,
    Result = mnesia:transaction(Delete),
    unify_result(Result).


create_table() ->
    {atomic, ok} = mnesia:delete_table(realm),
    TabDef = [{attributes, record_info(fields, realm)},
              {ram_copies, [node()]},
              {index, []}
             ],
    {atomic, ok} = mnesia:create_table(realm, TabDef),
    ok.

unify_result({atomic, Result}) ->
    Result;
unify_result(Other) ->
    Other.
