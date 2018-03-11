-module(ctr_session).

-export([new/3,
         close/1,

         set_auth_details/4,
         authenticate/2,
         add_subscription/2,
         has_subscription/2,
         remove_subscription/2,

         add_registration/2,
         has_registration/2,
         remove_registration/2,

         get_peer/1,
         get_id/1,
         get_realm/1,
         get_subscriptions/1,
         get_registrations/1,

         is_authenticated/1,
         get_authrole/1,

         lookup/1,

         init/0
        ]).


-record(ctr_session, {id = undefined,
                      realm = undefined,
                      details = #{},
                      authid = undefined,
                      authrole = undefined,
                      authprovider = undefined,
                      authmethod = undefined,
                      authenticated = false,
                      subscriptions = [],
                      registrations = [],
                      peer_at_gate = undefined
                     }).

init() ->
    create_table().

lookup(SessionId) ->
    lookup_by_id(SessionId).

new(RealmName, Details, PeerAtGate)  ->
    Id = ctr_utils:gen_global_id(),
    Session = #ctr_session{ id = Id,
                        realm = RealmName,
                        details = Details,
                        peer_at_gate = PeerAtGate },
    try_saving_session(Session, true).


close(#ctr_session{id = SessionId} = Session) ->
    ctr_broker:unsubscribe_all(Session),
    ctr_dealer:unregister_all(Session),
    %% ct_router:perform_testatment(Session),
    delete_by_id(SessionId).


set_auth_details(AuthId, AuthMethod, AuthProvider, Session) ->
    NewSession = Session#ctr_session{ authid = AuthId,
                                  authmethod = AuthMethod,
                                  authprovider = AuthProvider},
    try_saving_session(NewSession, false).


authenticate(AuthRole, Session) ->
    NewSession = Session#ctr_session{
                   authrole = AuthRole,
                   authenticated = true
                  },

    try_saving_session(NewSession, false).


add_subscription(SubId, #ctr_session{subscriptions = Subs} = Session) ->
    NewSubs = [ SubId | lists:delete(SubId, Subs) ],
    NewSession = Session#ctr_session{ subscriptions = NewSubs },
    try_saving_session(NewSession, false).

has_subscription(SubId, #ctr_session{subscriptions = Subs}) ->
    lists:member(SubId, Subs).

get_subscriptions(#ctr_session{subscriptions = Subs}) ->
    Subs.

remove_subscription(SubId, #ctr_session{subscriptions = Subs} = Session) ->
    NewSubs = lists:delete(SubId, Subs),
    NewSession = Session#ctr_session{ subscriptions = NewSubs },
    try_saving_session(NewSession, false).

add_registration(RegId, #ctr_session{registrations = Regs} = Session) ->
    NewRegs = [ RegId | lists:delete(RegId, Regs) ],
    NewSession = Session#ctr_session{ registrations = NewRegs },
    try_saving_session(NewSession, false).

has_registration(RegId, #ctr_session{registrations = Regs}) ->
    lists:member(RegId, Regs).

get_registrations(#ctr_session{registrations = Regs}) ->
    Regs.

remove_registration(SubId, #ctr_session{subscriptions = Subs} = Session) ->
    NewSubs = lists:delete(SubId, Subs),
    NewSession = Session#ctr_session{ subscriptions = NewSubs },
    try_saving_session(NewSession, false).

is_authenticated(#ctr_session{authenticated = IsAuth}) ->
    IsAuth.

get_peer(#ctr_session{peer_at_gate = PeerAtGate}) ->
    PeerAtGate.

get_id(#ctr_session{id = Id}) ->
    Id.

get_realm(#ctr_session{realm = Realm}) ->
    Realm.

get_authrole(#ctr_session{authrole = Role}) ->
    Role.

lookup_by_id(Id) ->
    Lookup = fun() ->
                     case mnesia:read({ctr_session, Id}) of
                         [Session] ->
                             {ok, Session};
                         [] ->
                             {error, not_found};
                         _ ->
                             {error, bad_state}
                     end
             end,
    Result = mnesia:transaction(Lookup),
    unify_result(Result).

try_saving_session(#ctr_session{id = Id} = Session, New) ->
    Store = fun(true) ->
                    case mnesia:wread({ctr_session, Id}) of
                        [] ->
                            ok = mnesia:write(Session),
                            {ok, Session};
                        _ ->
                            {error, exists}
                    end;
               (false) ->
                    ok = mnesia:write(Session),
                    {ok, Session}
            end,
    Result = mnesia:transaction(Store, [New]),
    maybe_retry_saving_session(Result, Session, New).

maybe_retry_saving_session({atomic, {ok, Session}}, _, _) ->
    {ok, Session};
maybe_retry_saving_session({atomic, {error, exists}}, Session, true) ->
    Id = ctr_utils:gen_global_id(),
    NewSession = Session#ctr_session{id = Id},
    try_saving_session(NewSession, true);
maybe_retry_saving_session(Other, Session, New) ->
    {error, saving}.

delete_by_id(Id) ->
    Delete = fun() ->
                     case mnesia:wread({ctr_session, Id}) of
                         [_] ->
                             mnesia:delete({ctr_session, Id});
                         [] ->
                             {error, not_found}
                     end
             end,
    Result = mnesia:transaction(Delete),
    unify_result(Result).


create_table() ->
    mnesia:delete_table(ctr_session),
    TabDef = [{attributes, record_info(fields, ctr_session)},
              {ram_copies, [node()]},
              {index, [realm, peer_at_gate]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_session, TabDef),
    ok.

unify_result({atomic, Result}) ->
    Result;
unify_result(Other) ->
    Other.
