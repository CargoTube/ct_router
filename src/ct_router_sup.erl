-module(ct_router_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-spec start_link() -> {ok, pid()}.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, noparams).

init(noparams) ->
    Procs = [
             realms_worker(),
             sessions_worker()
            ],
    Flags = #{},
    {ok, {Flags, Procs}}.

sessions_worker() ->
    #{ id => sessions,
       start => {ctr_sessions, start_link, []}
     }.

realms_worker() ->
    #{ id => realms,
       start => {ctr_realms, start_link, []}
     }.
