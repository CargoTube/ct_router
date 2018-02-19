-module(ctr_auth).

-export([handle_hello/2,
        handle_authenticate/2]).


-include_lib("ct_msg/include/ct_msg.hrl").

handle_hello({hello, Realm, Details}, Peer) ->
    RealmExists = does_realm_exist(Realm),
    SessionResult = maybe_create_session(RealmExists, Realm, Details, Peer),
    send_welcome_challenge_or_abort(SessionResult, Realm, Peer).


handle_authenticate(_Authenticate, PeerAtGate) ->
    PeerAtGate ! {to_peer, ?ABORT(#{}, canceled)},
    ok.


does_realm_exist(_RealmName) ->
    true.


maybe_create_session(true, Realm, Details, Peer) ->
    {ok, Session} = ctr_sessions:create_new_session(Realm, Peer),
    AuthType = get_auth_type(Realm, Details),
    {ok, Session, AuthType};
maybe_create_session(false, _Realm, _Details, _Peer) ->
    {error, no_such_realm}.


get_auth_type(_, _) ->
    anonymous.

send_welcome_challenge_or_abort({ok, Session, anonymous}, _, Peer) ->
    #{id := SessionId} = ctr_session:to_map(Session),
    Peer ! {session, SessionId},
    Peer ! {to_peer, ?WELCOME( SessionId, #{})},
    ok;
send_welcome_challenge_or_abort({ok, _Session, wampcra}, _, _Peer) ->
    %% TODO: implement
    ok;
send_welcome_challenge_or_abort({error, no_such_realm}, _, PeerAtGate) ->
    PeerAtGate ! {to_peer, ?ABORT(#{}, no_such_realm)},
    ok;
send_welcome_challenge_or_abort(_, _, PeerAtGate) ->
    %% by default cancel the session
    PeerAtGate ! {to_peer, ?ABORT(#{}, canceled)},
    ok.
