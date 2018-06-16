-module(ct_router_test).

-include_lib("eunit/include/eunit.hrl").

agent_identification_test() ->
    Result = ct_router:agent_identification(),
    ?assertEqual(true, is_binary(Result)).

agent_roles_test() ->
    Result = ct_router:agent_roles(),
    ?assertEqual(true, is_map(Result)).


handle_hello_test() ->
    {ok, Mock} = start_mock(),
    try
        Peer = self(),
        HelloOne = {hello, <<"one">>, #{}},
        ResultOne = ct_router:handle_hello(HelloOne, Peer),
        ok = receive
                 {to_peer, {welcome, _, _}} ->
                     ok;
                 _ ->
                     error
             end,
        ?assertEqual(ok, ResultOne),
        HelloTwo = {hello, <<"two">>, #{}},
        ResultTwo = ct_router:handle_hello(HelloTwo, Peer),
        ok = receive
                 {to_peer, {abort, _, _}} ->
                     ok;
                 _ ->
                     error
             end,
        ?assertEqual(ok, ResultTwo)
    after
        stop_mock(Mock)
    end,
    ok.

%% handle_authenticate_test() ->
%%     {ok, Mock} = start_mock(),
%%     try
%%         Result = ct_router:handle_authenticate(Authenticate, SessionId,
%%                                                   PeerAtGate),
%%         ?assertEqual(ok, Result)
%%     after
%%         stop_mock(Mock)
%%     end,
%%     ok.

handle_established_test() ->
    {ok, Mock} = start_mock(),
    try
        Message = {publish, 1, #{}, <<"test.topic">>, [1,2]},
        Type = element(1, Message),
        SessionId = establish_session,
        PeerAtGate = self(),
        Result = ct_router:handle_established(Type, Message, SessionId,
                                              PeerAtGate),
        ?assertEqual(ok, Result)
    after
        stop_mock(Mock)
    end,
    ok.

handle_session_closed_test() ->
    {ok, Mock} = start_mock(),
    try
        SessionId = establish_session,
        PeerAtGate = self(),
        Result = ct_router:handle_session_closed(SessionId, PeerAtGate),
        ?assertEqual(ok, Result)
    after
        stop_mock(Mock)
    end,
    ok.

start_mock() ->
    Modules = [ct_auth, cta_session, ctr_broker, ctr_dealer, ctr_routing],
    HandleHello = fun({hello, RealmName, _Details}, _Peer) ->
                          case RealmName of
                              <<"one">> -> {ok, session};
                              <<"two">> -> {abort, reaons}
                          end

                  end,
    SessGetId = fun(_Session) ->
                        123
                end,
    SessGetPeer = fun(_Session) ->
                        self()
                end,
    SessLookup = fun(SessionId) ->
                         {ok, SessionId}
                 end,
    SessClose = fun(_Session) ->
                        ok
                end,
    SessMetaEvent = fun(_Event, _Session) ->
                            ok
                end,
    BrokerUnsubscribeAll = fun(_Session) ->
                                  ok
                           end,
    DealerUnregisterAll = fun(_Session) ->
                                  ok
                          end,
    RoutingEstablished = fun(_Type, _Message, _Session) ->
                                 ok
                         end,
    test_utils:meck_new(Modules),
    ok = meck:expect(ct_auth, handle_hello, HandleHello),
    ok = meck:expect(cta_session, get_id, SessGetId),
    ok = meck:expect(cta_session, get_peer, SessGetPeer),
    ok = meck:expect(cta_session, lookup, SessLookup),
    ok = meck:expect(cta_session, close, SessClose),
    ok = meck:expect(ctr_broker, send_session_meta_event, SessMetaEvent),
    ok = meck:expect(ctr_broker, unsubscribe_all, BrokerUnsubscribeAll),
    ok = meck:expect(ctr_dealer, unregister_all, DealerUnregisterAll),
    ok = meck:expect(ctr_routing, handle_established, RoutingEstablished),
    {ok, Modules}.




stop_mock(Modules) ->
    test_utils:meck_done(Modules),
    ok.
