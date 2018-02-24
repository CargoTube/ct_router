-module(ct_router).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_hello/2,
         handle_authenticate/3,
         handle_established/4,
         handle_session_closed/2,

         to_peer/2
        ]).


handle_hello(Hello, PeerAtGate) ->
    ctr_auth:handle_hello(Hello, PeerAtGate).

handle_authenticate(Authenticate, SessionId, PeerAtGate) ->
    Session = get_session(SessionId, PeerAtGate),
    ctr_auth:handle_authenticate(Authenticate, Session, PeerAtGate).

handle_established(Type, Message, SessionId, PeerAtGate) ->
    Session = get_session(SessionId, PeerAtGate),
    ctr_routing:handle_established(Type, Message, Session).

handle_session_closed(SessionId, PeerAtGate) ->
    Session = get_session(SessionId, PeerAtGate),
    ctr_sessions:close_session(Session),
    ok.

get_session(SessionId, PeerAtGate) ->
    {ok, Session} = ctr_sessions:lookup(SessionId),
    PeerAtGate = ctr_session:get_peer(Session),
    Session.


to_peer(PeerAtGate, Message) ->
    lager:debug("[~p] ~p ! ~p", [self(), PeerAtGate, Message]),
    PeerAtGate  ! Message.
