-module(ctr_gen_data_ram_dealer).

-include("ct_router.hrl").

-export([
         list_registrations/1,
         lookup_registration/3,
         match_registration/2,
         get_registration/2,

         store_registration/1,
         delete_registration/2,

         store_invocation/1,
         get_invocation/2,
         delete_invocation/1,

         init/0
        ]).


init() ->
    create_table().

store_registration(Registration) ->
    #ctr_registration{
       id = NewId,
       procedure = Procedure,
       realm = Realm
      } = Registration,

    MatchHead = #ctr_registration{procedure=Procedure, realm=Realm,  _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    CreateIfNew =
        fun([]) ->
                ok = mnesia:write(Registration),
                {created, Registration};
           (_) ->

                {error, id_exists}
        end,
    Register =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, write) of
                    [] ->
                        Found = mnesia:wread({ctr_registration, NewId}),
                        CreateIfNew(Found);
                    _ ->
                        {error, procedure_exists}
                end
        end,
    Result = mnesia:transaction(Register),
    handle_store_result(Result, Registration).

handle_store_result({atomic, {created, Registration}}, _) ->
    {created, Registration};
handle_store_result({atomic, {error, procedure_exists}}, _Registration) ->
    {error, procedure_exists};
handle_store_result({atomic, {error, id_exists}}, Registration) ->
    store_registration(Registration).


match_registration(Procedure, Realm) ->
    MatchHead = #ctr_registration{procedure=Procedure, realm=Realm,
                                  match=exact, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    FindCallee =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, read) of
                    [Registration] ->
                        {ok, Registration};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(FindCallee),
    handle_find_result(Result).

list_registrations(Realm) ->
    MatchHead = #ctr_registration{realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    GetRegistrations =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, read) of
                    ListOfRegs when is_list(ListOfRegs) ->
                        {ok, ListOfRegs};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(GetRegistrations),
    handle_registration_result(Result).

handle_registration_result({atomic, {ok, ListOfRegs}}) ->
    {ok, ListOfRegs};
handle_registration_result(Other) ->
    lager:error("registration lookup error: ~p", Other),
    {ok, []}.


lookup_registration(Procedure, Options, Realm) ->
    Match = maps:get(match, Options, exact),
    MatchHead = #ctr_registration{realm=Realm, procedure=Procedure,
                                  match=Match, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    GetRegistrations =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, read) of
                    [Registration] ->
                        {ok, Registration};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(GetRegistrations),
    handle_find_result(Result).



get_registration(Id, Realm) ->
    MatchHead = #ctr_registration{realm=Realm, id=Id, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    GetRegistrations =
        fun() ->
                case mnesia:select(ctr_registration, MatchSpec, read) of
                    [Registration] ->
                        {ok, Registration};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(GetRegistrations),
    handle_find_result(Result).


handle_find_result({atomic, {ok, Registration}}) ->
    {ok, Registration};
handle_find_result({atomic, {error, not_found}}) ->
    {error, not_found}.


delete_registration(RegId, SessId) ->
    DeleteIfEmpty =
        fun([], RegistrationId, Registration) ->
                mnesia:delete({ctr_registration, RegistrationId}),
                {deleted, Registration};
           (_, _RegistrationId, Registration) ->
                ok = mnesia:write(Registration),
                {removed, Registration}
        end,

    Unregister =
        fun() ->
                case mnesia:wread({ctr_registration, RegId}) of
                    [#ctr_registration{callee_sess_ids = Callees} = Reg] ->
                        NewCallees = lists:delete(SessId, Callees),
                        NewReg = Reg#ctr_registration{
                                   callee_sess_ids = NewCallees},
                        DeleteIfEmpty(NewCallees, RegId, NewReg);
                    [] ->
                        {error, not_found}
                end
        end,
    mnesia:transaction(Unregister).



store_invocation(Invoc) ->
    NewId = ctr_utils:gen_global_id(),
    NewInvoc = Invoc#ctrd_invocation{id = NewId},
    StoreInvocation =
        fun() ->
                case mnesia:wread({ctrd_invocation, NewId}) of
                    [] ->
                        ok = mnesia:write(NewInvoc),
                        {ok, NewInvoc};
                    _ ->
                        {error, id_exists}
                end
        end,
    Result = mnesia:transaction(StoreInvocation),
    handle_invocation_store_result(Result, NewInvoc).


handle_invocation_store_result({atomic, {ok, Invoc}}, _) ->
    {ok, Invoc};
handle_invocation_store_result({atomic, {error, id_exists}}, Invoc) ->
    store_invocation(Invoc).

get_invocation(InvocationId, _Realm) ->
    FindInvocation =
        fun() ->
                case mnesia:read({ctrd_invocation, InvocationId}) of
                    [Invoc] -> {ok, Invoc};
                    _ -> {error, not_found}
                end
        end,
    Result = mnesia:transaction(FindInvocation),
    handle_invocation_find_result(Result).


handle_invocation_find_result({atomic, {ok, Invocation}}) ->
    {ok, Invocation};
handle_invocation_find_result(_) ->
    {error, not_found}.



delete_invocation(#ctrd_invocation{id=Id}) ->
    DeleteInvocation =
        fun() ->
                mnesia:delete({ctrd_invocation, Id})
        end,
    Result = mnesia:transaction(DeleteInvocation),
    handle_invocation_delete_result(Result).

handle_invocation_delete_result({atomic, ok}) ->
    ok;
handle_invocation_delete_result(Error) ->
    {error, Error}.


create_table() ->
    mnesia:delete_table(ctr_registration),
    RegDef = [{attributes, record_info(fields, ctr_registration)},
              {ram_copies, [node()]},
              {index, [realm, procedure, match]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_registration, RegDef),
    mnesia:delete_table(ctrd_invocation),
    InvDef = [{attributes, record_info(fields, ctrd_invocation)},
              {disc_copies, [node()]},
              {index, [realm]}
             ],
    {atomic, ok} = mnesia:create_table(ctrd_invocation, InvDef),
    ok.
