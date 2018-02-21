-module(ctr_sessions).
-behaviour(gen_server).

-export([
         new_session/1,
         update_session/1,
         close_session/1,
         lookup_session/1,

         list_sessions/0,

         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).

new_session(PeerAtGate) ->
    gen_server:call(?MODULE, {new_session, PeerAtGate}).

update_session(Session) ->
    gen_server:call(?MODULE, {update_session, Session}).

close_session(SessionId) when is_integer(SessionId) ->
    gen_server:call(?MODULE, {close_session, SessionId});
close_session(#{id := SessionId}) ->
    close_session(SessionId).

lookup_session(IdOrPeer) ->
    to_tagged_result(ets:lookup(?MODULE, IdOrPeer)).

list_sessions() ->
    gen_server:call(?MODULE, list_sessions).


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_parameter, []).

init(no_parameter) ->
    create_table(),
    {ok, #state{}}.

handle_call({close_session, SessionId}, _From, State) ->
    Result = do_close_session(SessionId),
    {reply, Result, State};
handle_call({update_session, Session}, _From, State) ->
    Result = do_update_session(Session),
    {reply, Result, State};
handle_call({new_session, PeerAtGate}, _From, State) ->
    Result = create_new_session(PeerAtGate),
    {reply, Result, State};
handle_call(list_sessions, _From, State) ->
    do_list_sessions(),
    {reply, ok, State};
handle_call(_Msg, _From, State) ->
    {reply, ignored, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Msg, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

create_new_session(PeerAtGate) ->
    Id = ctr_utils:gen_global_id(),
    Session = ctr_session:new(Id, PeerAtGate),
    Result = ets:insert_new(?MODULE, {Id, Session}),
    maybe_rerun_session_creation(Result, Session, Id, PeerAtGate).

maybe_rerun_session_creation(true, Session, Id, PeerAtGate) ->
    true = ets:insert_new(?MODULE, {PeerAtGate, Id}),
    {ok, Session};
maybe_rerun_session_creation(false, _Session, _Id, PeerAtGate) ->
    create_new_session(PeerAtGate).

do_update_session(Session) ->
    #{id := Id, peer := PeerAtGate} = ctr_session:to_map(Session),
    true = ets:insert(?MODULE, {Id, Session}),
    true = ets:insert(?MODULE, {PeerAtGate, Session}),
    ok.

to_tagged_result([]) ->
    {error, not_found};
to_tagged_result([{_, Session}]) ->
    {ok, Session}.

do_close_session(SessionId) ->
    Session = ets:lookup(?MODULE, SessionId),
    delete_if_exists(Session).

delete_if_exists([]) ->
    {error, not_found};
delete_if_exists([{Id, Session}]) ->
    #{peer := Peer} = ctr_session:to_map(Session),
    true = ets:delete(?MODULE, Id),
    true = ets:delete(?MODULE, Peer),
    ok.

do_list_sessions() ->
    true = ets:safe_fixtable(?MODULE, true),
    First = ets:first(?MODULE),
    print_session_or_exit(First).

print_session_or_exit('$end_of_table') ->
    true = ets:safe_fixtable(?MODULE, false),
    ok;
print_session_or_exit(Entry) ->
    print_session(Entry),
    Next = ets:next(?MODULE, Entry),
    print_session_or_exit(Next).

print_session({Id, Session}) when is_integer(Id) ->
    lager:debug(io_lib:format("~p", Session));
print_session(_) ->
    ok.



create_table() ->
    ets:new(?MODULE, [named_table, {keypos, 1}, set, protected, {heir, none}]),
    ok.
