-module(ctr_session).

-export([new/3,
         set_auth_details/4,
         authenticate/2,

         get_peer/1,
         get_id/1,
         get_realm/1,

         is_authenticated/1,
         get_authrole/1
        ]).


-record(session, {id = undefined,
                  realm = undefined,
                  authid = undefined,
                  authrole = undefined,
                  authprovider = undefined,
                  authmethod = undefined,
                  authenticated = false,
                  peer_at_gate = undefined
                 }).

new(Id, RealmName, PeerAtGate)  ->
    #session{ id = Id, realm = RealmName, peer_at_gate = PeerAtGate }.

set_auth_details(AuthId, AuthMethod, AuthProvider, Session) ->
    NewSession = Session#session{ authid = AuthId,
                                  authmethod = AuthMethod,
                                  authprovider = AuthProvider},
    {ok, Result} = ctr_sessions:update_session(NewSession),
    Result.


authenticate(AuthRole, Session) ->
    NewSession = Session#session{
                   authrole = AuthRole,
                   authenticated = true
                  },
    {ok, Result} = ctr_sessions:update_session(NewSession),
    Result.

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
