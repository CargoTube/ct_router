-module(ctr_realms).
-behaviour(gen_server).

-export([
         new_realm/1,
         update_realm/1,
         close_realm/1,
         lookup_realm/1,

         list_realms/0,

         start_link/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {}).

new_realm(Name) when is_binary(Name) ->
    gen_server:call(?MODULE, {new_realm, Name}).

update_realm(Realm) ->
    gen_server:call(?MODULE, {update_realm, Realm}).

close_realm(Realm) ->
    gen_server:call(?MODULE, {close_realm, Realm}).

lookup_realm(Name) ->
    to_tagged_result(ets:lookup(?MODULE, Name)).

list_realms() ->
    gen_server:call(?MODULE, list_realms).


-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, no_parameter, []).

init(no_parameter) ->
    create_table(),
    {ok, #state{}}.

handle_call({close_session, SessionId}, _From, State) ->
    Result = do_close_realm(SessionId),
    {reply, Result, State};
handle_call({update_session, Session}, _From, State) ->
    Result = do_update_realm(Session),
    {reply, Result, State};
handle_call({new_session, PeerAtGate}, _From, State) ->
    Result = create_new_realm(PeerAtGate),
    {reply, Result, State};
handle_call(list_realms, _From, State) ->
    do_list_realms(),
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

create_new_realm(Name) ->
    Realm = ctr_realm:new(Name),
    Result = ets:insert_new(?MODULE, {Name, Realm}),
    to_creation_result(Result).


do_update_realm(Realm) ->
    Name = ctr_realm:get_name(Realm),
    true = ets:insert(?MODULE, {Name, Realm}),
    ok.

to_tagged_result([]) ->
    {error, not_found};
to_tagged_result([{_, Session}]) ->
    {ok, Session}.

to_creation_result(true) ->
    ok;
to_creation_result(false) ->
    {error, exists}.

do_close_realm(Realm) ->
    Name = ctr_realm:get_name(Realm),
    FoundRealm = ets:lookup(?MODULE, Name),
    delete_if_exists(FoundRealm).

delete_if_exists([]) ->
    {error, not_found};
delete_if_exists([{Id, _Realm}]) ->
    true = ets:delete(?MODULE, Id),
    %% TODO: close all related sessions
    ok.

do_list_realms() ->
    true = ets:safe_fixtable(?MODULE, true),
    First = ets:first(?MODULE),
    print_realm_or_exit(First).

print_realm_or_exit('$end_of_table') ->
    true = ets:safe_fixtable(?MODULE, false),
    ok;
print_realm_or_exit({_, Realm} = Entry) ->
    lager:debug(io_lib:format("~p", Realm)),
    Next = ets:next(?MODULE, Entry),
    print_realm_or_exit(Next).


create_table() ->
    ets:new(?MODULE, [named_table, {keypos, 1}, set, protected, {heir, none}]),
    ok.