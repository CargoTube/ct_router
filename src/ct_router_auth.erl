-module(ct_router_auth).

-export([handle_hello/2,
        handle_authenticate/2]).


-include_lib("ct_msg/include/ct_msg.hrl").

handle_hello({hello, Realm, Details}, Peer) ->
    %% TODO:
    %% - check if realm does exist or is allowed to be created
    %% - set up session if allowed and return welcom
    %% - else return abort
    RealmExists = does_realm_exist(Realm),
    SessionResult = maybe_create_session(RealmExists, Realm, Details, Peer),
    send_welcome_challenge_or_abort(SessionResult, Realm, Peer).


handle_authenticate(_Authenticate, PeerAtGate) ->
    PeerAtGate ! {to_peer, ?ABORT(#{}, canceled)},
    ok.



does_realm_exist(_RealmName) ->
    true.


maybe_create_session(true, _Realm, _Details, _Peer) ->
   {session, 1, #{}}.


send_welcome_challenge_or_abort({session, SessionId, SessionData}, _, Peer) ->
    Peer ! {session, SessionData},
    Peer ! {to_peer, ?WELCOME( SessionId, #{})},
    ok;
send_welcome_challenge_or_abort(_, _, PeerAtGate) ->
    %% by default cancel the session
    PeerAtGate ! {to_peer, ?ABORT(#{}, canceled)},
    ok.
