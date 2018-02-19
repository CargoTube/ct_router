-module(ctr_session).

-export([new/2,
         authenticate/1,
         to_map/1
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

new(Id, PeerAtGate)  ->
    #session{ id = Id, peer_at_gate = PeerAtGate }.

authenticate(Session) ->
   Session#session{authenticated = true}.

to_map(#session{id = Id, realm = Realm, authid = AuthId, authrole = Role,
               authenticated = Authenticated, peer_at_gate = PeerAtGate}) ->
    #{id => Id,
      realm => Realm,
      authid => AuthId,
      role => Role,
      is_auth => Authenticated,
      peer => PeerAtGate
     }.
