-module(ct_router_app_test).

-include_lib("eunit/include/eunit.hrl").

start_test() ->
    {ok, Mock} = start_mock(),
    try
        StartType = normal,
        StartArgs = unused,
        Result = ct_router_app:start(StartType, StartArgs),
        ?assertEqual(sup_result, Result)
    after
        stop_mock(Mock)
    end,
    ok.

stop_test() ->
    {ok, Mock} = start_mock(),
    try
        State = unused,
        Result = ct_router_app:stop(State),
        ?assertEqual(ok, Result)
    after
        stop_mock(Mock)
    end,
    ok.

start_mock() ->
    Modules = [ctr_db, ct_router_sup],
    DbInit = fun() -> ok end,
    SubStart = fun() -> sup_result end,
    test_utils:meck_new(Modules),
    ok = meck:expect(ctr_db, init, DbInit),
    ok = meck:expect(ct_router_sup, start_link, SubStart),
    {ok, Modules}.



stop_mock(Modules) ->
    test_utils:meck_done(Modules),
    ok.
