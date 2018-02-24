-module(ctr_session).

-export([new/3,
         authenticate/1,
         to_map/1,

         is_authenticated/1,
         get_peer/1,
         get_id/1,
         get_realm/1
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

authenticate(Session) ->
   Session#session{authenticated = true}.

is_authenticated(#session{authenticated = IsAuth}) ->
    IsAuth.

get_peer(#session{peer_at_gate = PeerAtGate}) ->
    PeerAtGate.

get_id(#session{id = Id}) ->
    Id.

get_realm(#session{realm = Realm}) ->
    Realm.

to_map(#session{id = Id, realm = Realm, authid = AuthId, authrole = Role,
               authenticated = Authenticated, peer_at_gate = PeerAtGate}) ->
    #{id => Id,
      realm => Realm,
      authid => AuthId,
      role => Role,
      is_auth => Authenticated,
      peer => PeerAtGate
     }.
