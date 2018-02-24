-module(ctr_auth).

-export([handle_hello/2,
         handle_authenticate/2,
         is_message_allowed/2
        ]).

-include_lib("ct_msg/include/ct_msg.hrl").

handle_hello({hello, RealmName, Details}, Peer) ->
    Result = ctr_realms:lookup_realm(RealmName),
    SessionResult = maybe_create_session(Result, RealmName, Details, Peer),
    send_welcome_challenge_or_abort(SessionResult, RealmName, Peer).


handle_authenticate(_Authenticate, PeerAtGate) ->
    ct_router:to_peer(PeerAtGate ,{to_peer, ?ABORT(#{}, canceled)}),
    ok.

is_message_allowed(_Message, _Session) ->
    %% TODO: implement
    true.

maybe_create_session({ok, Realm}, RealmName, Details, Peer) ->
    {ok, Session} = ctr_sessions:new_session(RealmName, Peer),
    AuthMethod = get_auth_method(Realm, Details),
    {ok, Session, AuthMethod};
maybe_create_session(_Result, _RealmName, _Details, _Peer) ->
    {error, no_such_realm}.


get_auth_method(_, _) ->
    none.


send_welcome_challenge_or_abort({ok, Session, none}, _, Peer) ->
    #{id := SessionId} = ctr_session:to_map(Session),
    send_to_peer(Peer, ?WELCOME( SessionId, #{})),
    ok;
send_welcome_challenge_or_abort({ok, _Session, wampcra}, _, _Peer) ->
    %% TODO: implement
    ok;
send_welcome_challenge_or_abort({error, no_such_realm}, _, PeerAtGate) ->
    send_to_peer(PeerAtGate, ?ABORT(#{}, no_such_realm)),
    ok;
send_welcome_challenge_or_abort(_, _, PeerAtGate) ->
    %% by default cancel the session
    send_to_peer(PeerAtGate, ?ABORT(#{}, canceled)),
    ok.

send_to_peer(Peer, Message) ->
    ct_router:to_peer(Peer, {to_peer, Message}).
