-module(ctr_callee_session).

-export([count/3,
         list/3,
         get/3]).

count(Args, _Kw, Realm) ->
    {[length(sessions_get(Args, Realm))], undefined}.

list(Args, _Kw, Realm) ->
    ToId = fun(Session, Ids) ->
                   [ cta_session:get_id(Session) | Ids ]
           end,
    Ids = lists:foldl(ToId, [], sessions_get(Args, Realm)),
    {[Ids], undefined}.

get([Id], _Kw, Realm) ->
    Result = cta_session:lookup(Id),
    handle_session_result(Result, Realm).


handle_session_result({ok, Session}, Realm) ->
    SameRealm = (cta_session:get_realm(Session) == Realm),
    maybe_return_session_info(SameRealm, Session);
handle_session_result(_Error, _Realm) ->
    throw(no_such_session).


maybe_return_session_info(true, Session) ->
    SessMap = cta_session:to_map(Session),
    Keys = [session, authid, authrole, authmethod, authprovider, transport],
    {[maps:with(Keys, SessMap)], undefined};
maybe_return_session_info(false, _Session) ->
    throw(no_such_session).


sessions_get(undefined, Realm) ->
    sessions_get([no_filter], Realm);
sessions_get([AuthRoles], Realm) when is_list(AuthRoles);
                                      AuthRoles == no_filter ->
    Filter = fun(Session) ->
                     Role = cta_session:get_authrole(Session),
                     AuthRoles == no_filter orelse lists:member(Role, AuthRoles)
             end,
    {ok, List} = cta_session:lookup_by_realm(Realm),
    lists:filter(Filter, List);
sessions_get(_Args, _Realm) ->
    throw(invalid_argument).
