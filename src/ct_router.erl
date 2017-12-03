-module(ct_router).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_hello/2,
         handle_authenticate/2,
         handle_established/4,
         handle_session_closed/2
        ]).

handle_hello(Hello, PeerAtGate) ->
    MaybeJobId = get_maybe_jobid(hello_queue),
    maybe_handle_hello(MaybeJobId, Hello, PeerAtGate).

handle_authenticate(Authenticate, PeerAtGate) ->
    MaybeJobId = get_maybe_jobid(authenticate_queue),
    maybe_handle_authenticate(MaybeJobId, Authenticate, PeerAtGate).

handle_established(Type, Message, Session, PeerAtGate) ->
    MaybeJobId = get_maybe_jobid_for_session(Session),
    maybe_handle_established(MaybeJobId, Type, Message, Session, PeerAtGate).

handle_session_closed(Session, _PeerAtGate) ->
    ct_router_sessions:close_session(Session),
    ok.


get_maybe_jobid_for_session(Session) ->
    QueueName = ct_router_session:get_queue(Session),
    jobs:ask(QueueName).


get_maybe_jobid(QueueType) ->
    {ok, Queue} = application:gen_env(QueueType),
    jobs:ask(Queue).



maybe_handle_hello({ok, JobId}, Hello, PeerAtGate) ->
    try
        ct_router_auth:handle_hello(Hello, PeerAtGate)
    after
        jobs:done(JobId)
    end,
    ok;
maybe_handle_hello({error, rejected}, _Hello, PeerAtGate) ->
    PeerAtGate ! {to_peer, ?ABORT(#{}, canceled)},
    ok.

maybe_handle_authenticate({ok, JobId}, Authenticate, PeerAtGate) ->
    try
        ct_router_auth:handle_authenticate(Authenticate, PeerAtGate)
    after
        jobs:done(JobId)
    end,
    ok;
maybe_handle_authenticate({error, rejected}, _Authenticate, PeerAtGate) ->
    PeerAtGate ! {to_peer, ?ABORT(#{}, canceled)},
    ok.


maybe_handle_established({ok, JobId}, Type, Message, Session, Peer) ->
    try
        ct_router_routing:handle_established(Type, Message, Session, Peer)
    after
        jobs:done(JobId)
    end,
    ok;
maybe_handle_established({error, rejected}, Type, Message, Session, Peer) ->
    ct_router_routing:reject_for_message(Type, Message, Session, Peer).
