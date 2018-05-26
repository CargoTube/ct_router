-module(ctr_callee).

-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         is_procedure/1,

         handle_call/2
        ]).

-define(PROCEDURES, [ { <<"wamp.session.count">>, fun session_count/3 },
                      { <<"wamp.session.list">>, fun session_list/3 },
                      { <<"wamp.session.get">>, fun session_get/3 }
                    ]).

is_procedure(Procedure) ->
    Result = lists:keyfind(Procedure, 1, ?PROCEDURES),
    is_tuple(Result).


handle_call({call, ReqId, _Options, Procedure, Args, ArgsKw}, Session) ->
    Realm = cta_session:get_realm(Session),
    {_, Fun} = lists:keyfind(Procedure, 1, ?PROCEDURES),
    Result = Fun(Args, ArgsKw, Realm),
    handle_call_fun_result(Result, ReqId).

handle_call_fun_result({error, Error}, ReqId) ->
    ?ERROR(call, ReqId, #{}, Error);
handle_call_fun_result({ResArgs, ResArgsKw}, ReqId) ->
    ?RESULT(ReqId, #{}, ResArgs, ResArgsKw).



session_count(Args, _Kw, Realm) ->
    {[length(sessions_get(Args, Realm))], undefined}.

session_list(Args, _Kw, Realm) ->
    ToId = fun(Session, Ids) ->
                   [ cta_session:get_id(Session) | Ids ]
           end,
    Ids = lists:foldl(ToId, [], sessions_get(Args, Realm)),
    {[Ids], undefined}.

session_get([Id], _Kw, Realm) ->
    Result = cta_session:lookup(Id),
    handle_session_result(Result, Realm).


handle_session_result({ok, Session}, Realm) ->
    SameRealm = (cta_session:get_realm(Session) == Realm),
    maybe_return_session_info(SameRealm, Session);
handle_session_result(_Error, _Realm) ->
    {error, no_such_session}.


maybe_return_session_info(true, Session) ->
    SessMap = cta_session:to_map(Session),
    Keys = [session, authid, authrole, authmethod, authprovider, transport],
    {[maps:with(Keys, SessMap)], undefined};
maybe_return_session_info(false, _Session) ->
    {error, no_such_session}.



sessions_get(undefined, Realm) ->
    sessions_get([[]], Realm);
sessions_get([AuthRoles], Realm) ->
    Filter = fun(Session) ->
                     Role = cta_session:get_authrole(Session),
                     lists:member(Role, AuthRoles)
             end,
    {ok, List} = cta_session:lookup_by_realm(Realm),
    lists:filter(Filter, List).
