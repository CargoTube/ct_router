-module(ctr_session).

-export([new/3,
         close/1,

         set_auth_details/4,
         authenticate/2,

         get_peer/1,
         get_id/1,
         get_realm/1,

         is_authenticated/1,
         get_authrole/1,

         lookup/1,

         init/0
        ]).


-record(session, {id = undefined,
                  realm = undefined,
                  details = #{},
                  authid = undefined,
                  authrole = undefined,
                  authprovider = undefined,
                  authmethod = undefined,
                  authenticated = false,
                  peer_at_gate = undefined
                 }).

init() ->
    create_table().

lookup(SessionId) ->
    lookup_by_id(SessionId).

new(RealmName, Details, PeerAtGate)  ->
    Id = ctr_utils:gen_global_id(),
    Session = #session{ id = Id,
                        realm = RealmName,
                        details = Details,
                        peer_at_gate = PeerAtGate },
    try_saving_session(Session, true).

close(SessionId) when is_integer(SessionId) ->
    delete_by_id(SessionId);
close(#session{id = SessionId}) ->
    delete_by_id(SessionId).


set_auth_details(AuthId, AuthMethod, AuthProvider, Session) ->
    NewSession = Session#session{ authid = AuthId,
                                  authmethod = AuthMethod,
                                  authprovider = AuthProvider},
    try_saving_session(NewSession, false).


authenticate(AuthRole, Session) ->
    NewSession = Session#session{
                   authrole = AuthRole,
                   authenticated = true
                  },

    try_saving_session(NewSession, false).

is_authenticated(#session{authenticated = IsAuth}) ->
    IsAuth.

get_peer(#session{peer_at_gate = PeerAtGate}) ->
    PeerAtGate.

get_id(#session{id = Id}) ->
    Id.

get_realm(#session{realm = Realm}) ->
    Realm.

get_authrole(#session{authrole = Role}) ->
    Role.

lookup_by_id(Id) ->
    Lookup = fun() ->
                     case mnesia:read({session, Id}) of
                         [Session] ->
                             {ok, Session};
                         [] ->
                             {error, not_found};
                         _ ->
                             {error, bad_state}
                     end
             end,
    Result = mnesia:transaction(Lookup),
    unify_result(Result).

try_saving_session(#session{id = Id} = Session, New) ->
    Store = fun(true) ->
                    case mnesia:wread({session, Id}) of
                        [] ->
                            ok = mnesia:write(Session),
                            {ok, Session};
                        _ ->
                            {error, exists}
                    end;
               (false) ->
                    ok = mnesia:write(Session),
                    {ok, Session}
            end,
    Result = mnesia:transaction(Store, [New]),
    maybe_retry_saving_session(Result, Session, New).

maybe_retry_saving_session({atomic, {ok, Session}}, _, _) ->
    {ok, Session};
maybe_retry_saving_session({atomic, {error, exists}}, Session, true) ->
    Id = ctr_utils:gen_global_id(),
    NewSession = Session#session{id = Id},
    try_saving_session(NewSession, true);
maybe_retry_saving_session(Other, Session, New) ->
    lager:debug("session: saving error [new=~p], ~p ~p", [New, Other, Session]),
    {error, saving}.

delete_by_id(Id) ->
    Delete = fun() ->
                     case mnesia:wread({session, Id}) of
                         [_] ->
                             mnesia:delete({session, Id});
                         [] ->
                             {error, not_found}
                     end
             end,
    Result = mnesia:transaction(Delete),
    unify_result(Result).


create_table() ->
    {atomic, ok} = mnesia:delete_table(session),
    TabDef = [{attributes, record_info(fields, session)},
              {ram_copies, [node()]},
              {index, [realm, peer_at_gate]}
             ],
    {atomic, ok} = mnesia:create_table(session, TabDef),
    ok.

unify_result({atomic, Result}) ->
    Result;
unify_result(Other) ->
    Other.
