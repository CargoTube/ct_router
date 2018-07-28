-module(ctr_dealer_data).

-include("ct_router.hrl").

-export([init/0,

         store_registration/1,
         match_registration/2,

         get_registrations_of_realm/1,
         lookup_regisration/2,
         delete_registration/2
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


match_registration(Procedure, Session) ->
    Realm = cta_session:get_realm(Session),

    MatchHead = #ctr_registration{procedure=Procedure, realm=Realm, _='_'},
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

handle_find_result({atomic, {ok, Registration}}) ->
    {ok, Registration};
handle_find_result({atomic, {error, not_found}}) ->
    {error, not_found}.

get_registrations_of_realm(Realm) ->
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



lookup_regisration(Id, Realm) ->
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



delete_registration(RegId, SessId) ->

    DeleteIfEmpty =
        fun([], RegistrationId, Registration) ->
                mnesia:delete({ctr_registration, RegistrationId}),
                {deleted, Registration};
           (_, _RegistrationId, Registration) ->
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



create_table() ->
    mnesia:delete_table(ctr_registration),
    RegDef = [{attributes, record_info(fields, ctr_registration)},
              {ram_copies, [node()]},
              {index, [realm, procedure, match]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_registration, RegDef),
    ok.
