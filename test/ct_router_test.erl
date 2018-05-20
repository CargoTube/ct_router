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
        ?assertEqual(ok, ResultOne)
    after
        stop_mock(Mock)
    end,
    ok.


start_mock() ->
    Modules = [ct_auth, cta_session],
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
    test_utils:meck_new(Modules),
    ok = meck:expect(ct_auth, handle_hello, HandleHello),
    ok = meck:expect(cta_session, get_id, SessGetId),
    ok = meck:expect(cta_session, get_peer, SessGetPeer),
    {ok, Modules}.



stop_mock(Modules) ->
    test_utils:meck_done(Modules),
    ok.
