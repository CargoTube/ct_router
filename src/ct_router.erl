-module(ct_router).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_hello/2,
         handle_authenticate/3,
         handle_established/4,
         handle_session_closed/2,

         agent_identification/0,
         agent_roles/0,

         to_session/2,
         to_peer/2
        ]).

agent_identification() ->
    Version = get_agent_version(),
    Agent = get_agent_name(),
    Dash = <<"-">>,
    << Agent/binary, Dash/binary, Version/binary>>.

agent_roles() ->
    #{
       broker => #{},
       dealer => #{}
     }.


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
    close_session(Session),
    ok.

get_session(SessionId, PeerAtGate) ->
    {ok, Session} = ctr_session:lookup(SessionId),
    PeerAtGate = ctr_session:get_peer(Session),
    Session.

close_session(Session) ->
    ctr_session:close(Session).


to_session(Session, Message) ->
    PeerAtGate = ctr_session:get_peer(Session),
    to_peer([PeerAtGate], {to_peer, Message}).

to_peer([], _Message) ->
    ok;
to_peer([PeerAtGate | Tail], Message) ->
    PeerAtGate ! Message,
    to_peer(Tail, Message);
to_peer(PeerAtGate, Message) ->
    to_peer([PeerAtGate], Message).


get_agent_version() ->
    {ok, Version} = application:get_key(vsn),
    list_to_binary(application:get_env(cargotube, version, Version)).

get_agent_name() ->
    list_to_binary(application:get_env(cargotube, name, "CargoTube.org")).
