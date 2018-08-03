-module(ctr_db).

-export([init/0]).

-define(DBDIR, "db").

init() ->
    Dir = application:get_env(ct_router, mnesia_dir, ?DBDIR),
    DirExists = filelib:is_dir(Dir),
    ok = stop_mnesia(),
    ok = create_schema_if_needed(DirExists),
    ok = start_mnesia(),
    ok = init_tables(),
    ok.

init_tables() ->
    cta_session:init(),
    cta_realm:init(),
    ok.


stop_mnesia() ->
    application:stop(mnesia),
    MnesiaDir = application:get_env(ct_router, mnesia_dir, ?DBDIR),
    application:set_env(mnesia, dir, MnesiaDir),
    ok.


create_schema_if_needed(true) ->
    ok;
create_schema_if_needed(false) ->
    ok = mnesia:create_schema([node()]),
    ok = start_mnesia(),
    ok.


start_mnesia() ->
    {ok, _} = application:ensure_all_started(mnesia),
    ok.
