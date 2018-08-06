-module(ctr_gen_data_ram).

-behaviour(ctr_gen_data).

-include("ct_router.hrl").

-export([
         list_subscriptions/1,
         lookup_subscription/3,
         match_subscription/2,
         get_subscription/2,
         add_subscription/4,
         remove_subscription/3,
         store_publication/1,

         list_registrations/1,
         lookup_registration/3,
         match_registration/2,
         get_registration/2,
         add_registration/4,
         remove_registration/3,

         add_invocation/1,
         get_invocation/2,
         remove_invocation/2,

         init/0
        ]).

list_subscriptions(Realm) ->
    ctr_gen_data_ram_broker:list_subscriptions(Realm).

lookup_subscription(Procedure, Options, Realm) ->
    ctr_gen_data_ram_broker:lookup_subscription(Procedure, Options, Realm).


match_subscription(Procedure, Realm) ->
    ctr_gen_data_ram_broker:match_subscription(Procedure, Realm).


get_subscription(ProcedureId, Realm) ->
    ctr_gen_data_ram_broker:get_subscription(ProcedureId, Realm).


add_subscription(Uri, _Match, SessionId, Realm) ->
    ctr_gen_data_ram_broker:add_subscription(Uri, Realm, SessionId).


remove_subscription(SubscriptionId, SessionId, _Realm) ->
    ctr_gen_data_ram_broker:delete_subscription(SubscriptionId, SessionId).


store_publication(Publication) ->
    ctr_gen_data_ram_broker:store_publication(Publication).


list_registrations(Realm) ->
    ctr_gen_data_ram_dealer:list_registrations(Realm).


lookup_registration(Procedure, Options, Realm) ->
    ctr_gen_data_ram_dealer:lookup_registration(Procedure, Options, Realm).


match_registration(Procedure, Realm) ->
    ctr_gen_data_ram_dealer:match_registration(Procedure, Realm).

get_registration(ProcedureId, Realm) ->
    ctr_gen_data_ram_dealer:get_registration(ProcedureId, Realm).


add_registration(Procedure, _Match, SessionId, Realm) ->
    NewId = ctr_utils:gen_global_id(),
    NewReg = #ctr_registration{
                id = NewId,
                procedure = Procedure,
                realm = Realm,
                created = calendar:universal_time(),
                callee_sess_ids = [SessionId]
               },
    ctr_gen_data_ram_dealer:store_registration(NewReg).

remove_registration(RegistrationId, SessionId, _Realm) ->
    ctr_gen_data_ram_dealer:delete_registration(RegistrationId, SessionId).


add_invocation(Invocation) ->
    ctr_gen_data_ram_dealer:store_invocation(Invocation).

get_invocation(InvocationId, Realm) ->
    ctr_gen_data_ram_dealer:get_invocation(InvocationId, Realm).

remove_invocation(InvocationId, Realm) ->
    ctr_gen_data_ram_dealer:delete_invocation(InvocationId, Realm).

init() ->
    ctr_gen_data_ram_broker:init(),
    ctr_gen_data_ram_dealer:init(),
    ok.
