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
       broker => #{
         features => #{
           subscriber_blackwhite_listing => true,
           publisher_exclusion => true,
           publisher_identification => false,
           publication_trustlevels => false,
           subscription_meta_api => false,
           pattern_based_subscription => false,
           sharded_subscription => false,
           event_history => false,
           session_meta_apit => false
          }
        },
       dealer => #{
         features => #{
           progressive_call_results => false,
           call_timeout => false,
           call_canceling => false,
           caller_identification => false,
           call_trustlevel => false,
           registration_meta_api => false,
           pattern_based_registration => false,
           shared_registration => false,
           sharded_registration => false,
           testament_meta_api => false,
           session_meta_apit => false
           }
        }
     }.


handle_hello(Hello, PeerAtGate) ->
    AuthResult = ct_auth:handle_hello(Hello, PeerAtGate),
    handle_auth_result(AuthResult, PeerAtGate).

handle_authenticate(Authenticate, SessionId, PeerAtGate) ->
    Session = get_session(SessionId, PeerAtGate),
    AuthResult = ct_auth:handle_authenticate(Authenticate, Session, PeerAtGate),
    handle_auth_result(AuthResult, PeerAtGate).

handle_established(Type, Message, SessionId, PeerAtGate) ->
    Session = get_session(SessionId, PeerAtGate),
    ctr_routing:handle_established(Type, Message, Session).

handle_session_closed(SessionId, PeerAtGate) ->
    Session = get_session(SessionId, PeerAtGate),
    close_session(Session),
    ok.

get_session(SessionId, PeerAtGate) ->
    {ok, Session} = cta_session:lookup(SessionId),
    PeerAtGate = cta_session:get_peer(Session),
    Session.

close_session(Session) ->
    ctr_broker:unsubscribe_all(Session),
    ctr_dealer:unregister_all(Session),
    cta_session:close(Session).


handle_auth_result({ok, Session}, _PeerAtGate) ->
    SessionId = cta_session:get_id(Session),
    Details = #{
      agent => ct_router:agent_identification(),
      roles => ct_router:agent_roles()
     },
    to_session(Session,?WELCOME( SessionId, Details));
handle_auth_result({abort, Reason}, PeerAtGate) ->
    message_to_peer(PeerAtGate, ?ABORT(#{}, Reason)).




to_session(Session, Message) ->
    PeerAtGate = cta_session:get_peer(Session),
    to_peer([PeerAtGate], {to_peer, Message}).

message_to_peer(Peer, Message) ->
    to_peer([Peer], {to_peer, Message}).

to_peer([], _Message) ->
    ok;
to_peer([PeerAtGate | Tail], Message) ->
    PeerAtGate ! Message,
    to_peer(Tail, Message);
to_peer(PeerAtGate, Message) ->
    to_peer([PeerAtGate], Message).


get_agent_version() ->
    {ok, Version} = application:get_key(vsn),
    ensure_binary(application:get_env(cargotube, version, Version)).

get_agent_name() ->
    ensure_binary(application:get_env(cargotube, name, "CargoTube.org")).


ensure_binary(Binary) when is_binary(Binary) ->
    Binary;
ensure_binary(List) when is_list(List) ->
    list_to_binary(List).
