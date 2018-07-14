-module(ctr_stats).

-behaviour(gen_server).

-export([add/2,
         update/0,

         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


-record(state, {
          entries = [],
          oldest = undefined,
          slowest = undefined,
          fastest = undefined,
          percentile50 = 0,
          percentile99 = 0,
          avg_msg_sec = 0,
          median = undefined
         }).


add(Type, Duration) ->
    gen_server:cast(?MODULE, {add, Type, Duration/1000.0}).

update() ->
    gen_server:cast(?MODULE, update).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_params, []).

init(no_params) ->
    TimeMs = application:get_env(ct_router, stat_update, 300000),
    timer:apply_interval(TimeMs, ?MODULE, update, []),
    {ok, #state{}}.

handle_call(_From, _Message, State) ->
    {reply, unsupported, State}.

handle_cast({add, Type, Duration}, #state{entries = Entries} = State) ->
    Now = erlang:system_time(second),
    NewEntries = [ { Duration, Type, Now} | Entries ],
    {noreply, State#state{entries = NewEntries} };
handle_cast(update, State) ->
    {noreply, update_state(State)};
handle_cast(_Message, State) ->
    {noreply, State}.

handle_info(_Message, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

update_state(#state{entries = []} = State) ->
    lager:info("message stats: nothing sent."),
    State;
update_state(#state{entries = Entries}) ->
    Now = erlang:system_time(second),
    MaxKeepSec = application:get_env(ct_router, stat_keep, 86400),
    StillOkayTime = Now - MaxKeepSec,

    FilterOld =
        fun({_Duration, _Type, Time}) ->
               Time >= StillOkayTime
        end,
    ActiveEntries = lists:filter(FilterOld, Entries),

    Sort =
        fun({DurationA, _, _}, {DurationB, _, _}) ->
                DurationA =< DurationB
         end,
    Sorted = lists:sort(Sort, ActiveEntries),
    Length = length(Sorted),
    Percentile50 = percentile( 0.5,  Sorted, Length),
    Percentile99 = percentile( 0.99,  Sorted, Length),
    {Fastest, _, _} = lists:nth(1, Sorted),
    {Slowest, _, _} = lists:nth(Length, Sorted),

    lager:info("message stats: [ ~p / ~p / * ~p * / ~p ] ms",
               [Slowest, Percentile50, Percentile99, Fastest]),

    #state{entries = Sorted,
           percentile50 = Percentile50,
           percentile99 = Percentile99,
           fastest = Fastest, slowest = Slowest
          }.


percentile(Percentil, Entries, Length) ->
    Lower = 1.0/(Length + 1.0),
    Upper = Length/(Length + 1.0),
    Position = percentile_pos(Percentil, Length, Lower, Upper),
    FPos = math:floor(Position),
    calc_percentile(FPos, Position - FPos, Entries).

percentile_pos(Percentil, _, Lower, _) when Percentil =< Lower ->
    1;
percentile_pos(Percentil, Length, _, Upper) when Percentil >= Upper ->
    Length;
percentile_pos(Percentil, Length, _ ,_) ->
    Percentil * Length.

calc_percentile(Pos, 0.0, Entries) ->
    {Result, _, _} = lists:nth(Pos, Entries),
    Result;
calc_percentile(Pos, Partial, Entries) ->
    {A, _, _} = lists:nth(Pos, Entries),
    {B, _, _} = lists:nth(Pos + 1, Entries),
    A + (B - A) * Partial.
