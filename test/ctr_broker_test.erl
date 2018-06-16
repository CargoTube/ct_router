-module(ctr_broker_test).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    {ok, Mock} = start_mock(),
    try
        Result = ctr_broker:init(),
        ?assertEqual(ok, Result)
    after
        stop_mock(Mock)
    end,
    ok.

send_session_meta_event_test() ->
    {ok, Mock} = start_mock(),
    try
        Result = ctr_broker:init(),
        ?assertEqual(ok, Result)
    after
        stop_mock(Mock)
    end,
    ok.

start_mock() ->
    Modules = [mnesia, cta_session],
    MnesiaCreate = fun(_Name, _TableDef) -> {atomic, ok} end,
    MnesiaDelete = fun(_Name) -> {atomic, ok} end,
    test_utils:meck_new(Modules),
    ok = meck:expect(mnesia, create_table, MnesiaCreate),
    ok = meck:expect(mnesia, delete_table, MnesiaDelete),
    {ok, Modules}.



stop_mock(Modules) ->
    test_utils:meck_done(Modules),
    ok.
