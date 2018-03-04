-module(ctr_auth).

-export([handle_hello/2,
         handle_authenticate/2,
         is_message_allowed/2
        ]).

-include_lib("ct_msg/include/ct_msg.hrl").

handle_hello({hello, RealmName, Details}, Peer) ->
    lager:debug("auth: ~p hello - ~p ~p",[Peer, RealmName, Details]),
    Result = ctr_realm:lookup(RealmName),
    SessionResult = maybe_create_session(Result, Details, Peer),
    send_welcome_challenge_or_abort(SessionResult, Peer).


handle_authenticate(Authenticate, PeerAtGate) ->
    lager:debug("auth: ~p authenticate - ~p",[PeerAtGate, Authenticate]),
    ct_router:to_peer(PeerAtGate ,{to_peer, ?ABORT(#{}, canceled)}),
    ok.

is_message_allowed(_Message, _Session) ->
    %% TODO: implement
    true.

maybe_create_session({ok, Realm}, Details, Peer) ->
    lager:debug("auth: ~p realm ok",[Peer]),
    RealmName = ctr_realm:get_name(Realm),
    {ok, Session} = ctr_session:new(RealmName, Peer),
    AuthMethod = get_auth_method(Realm, Details),
    lager:debug("auth: ~p authmethod ~p",[Peer, AuthMethod]),
    {ok, Session, AuthMethod, Realm};
maybe_create_session(_Result, _Details, Peer) ->
    lager:debug("auth: ~p no realm",[Peer]),
    {error, no_such_realm}.


get_auth_method(Realm, Details) ->
    AuthId = maps:get(<<"authid">>, Details, undefined),
    AuthMethods = get_client_authmethods(Details, AuthId),
    RealmMethods = ctr_realm:get_auth_methods(Realm),

    ToAtom =
        fun(Method, List) ->
                try
                    [ binary_to_existing_atom(Method, utf8) | List ]
                catch _:_ ->
                        List
                end
        end,
    ClientSupported = lists:foldl(ToAtom, [], AuthMethods),

    FindBestMethod =
        fun(Method, Current) ->
                case lists:member(Method, ClientSupported) of
                    true ->
                        Method;
                    false ->
                        Current
                end
        end,
    lists:foldr(FindBestMethod, none, RealmMethods).


get_client_authmethods(Details, undefined) ->
    maps:get(<<"authmethods">>, Details, [<<"anonymous">>]);
get_client_authmethods(Details, _) ->
    maps:get(<<"authmethods">>, Details, []).


send_welcome_challenge_or_abort({ok, Session, anonymous, Realm}, Peer) ->
    lager:debug("auth: ~p anonymous login",[Peer]),
    {ok, NewSession} = ctr_session:set_auth_details(anonymous, anonymous,
                                                    anonymous, Session),
    RoleResult = ctr_realm:get_role(anonymous, Realm),
    maybe_authenticate_session(RoleResult, NewSession);
send_welcome_challenge_or_abort({error, no_such_realm}, PeerAtGate) ->
    send_abort(PeerAtGate, no_such_realm);
send_welcome_challenge_or_abort({ok, Session, _, _}, _PeerAtGate) ->
    abort_session(Session);
send_welcome_challenge_or_abort( _, PeerAtGate) ->
    send_abort(PeerAtGate, canceled).

maybe_authenticate_session({ok, Role}, Session) ->
    {ok, NewSession} = ctr_session:authenticate(Role, Session),
    SessionId = ctr_session:get_id(NewSession),
    lager:debug("auth: ~p anonymous as role ~p ",
                [ctr_session:get_peer(NewSession), Role]),
    ct_router:to_session(Session,?WELCOME( SessionId, #{})),
    ok;
maybe_authenticate_session(_, Session) ->
    lager:debug("auth: ~p no role",[ctr_session:get_peer(Session)]),
    abort_session(Session).


abort_session(Session) ->
    Peer = ctr_session:get_peer(Session),
    lager:debug("auth: ~p  close session",[Peer]),
    ok = ctr_session:close(Session),
    send_abort(Peer, canceled).

send_abort(Peer, Reason) ->
    lager:debug("auth: ~p  sending abort",[Peer]),
    ct_router:to_peer(Peer, {to_peer, ?ABORT(#{}, Reason)}),
    ok.
