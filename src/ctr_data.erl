-module(ctr_data).

-behaviour(ctr_data_if).

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
         remove_invocation/2
        ]).

list_subscriptions(Realm) ->
    Module = get_module(),
    Module:list_subscriptions(Realm).

lookup_subscription(Procedure, Options, Realm) ->
    Module = get_module(),
    Module:lookup_subscription(Procedure, Options, Realm).


match_subscription(Procedure, Realm) ->
    Module = get_module(),
    Module:match_subscription(Procedure, Realm).


get_subscription(ProcedureId, Realm) ->
    Module = get_module(),
    Module:get_subscription(ProcedureId, Realm).


add_subscription(Uri, Match, SessionId, Realm) ->
    Module = get_module(),
    Module:add_subscription(Uri, Match, SessionId, Realm).


remove_subscription(SubscriptionId, SessionId, Realm) ->
    Module = get_module(),
    Module:remove_subscription(SubscriptionId, SessionId, Realm).


store_publication(Publication) ->
    Module = get_module(),
    Module:store_publication(Publication).



list_registrations(Realm) ->
    Module = get_module(),
    Module:list_registrations(Realm).


lookup_registration(Procdure, Options, Realm) ->
    Module = get_module(),
    Module:lookup_registration(Procdure, Options, Realm).


match_registration(Procdure, Realm) ->
    Module = get_module(),
    Module:match_registration(Procdure, Realm).

get_registration(ProcdureId, Realm) ->
    Module = get_module(),
    Module:get_registration(ProcdureId, Realm).


add_registration(Procedure, Match, SessionId, Realm) ->
    Module = get_module(),
    Module:add_registration(Procedure, Match, SessionId, Realm).

remove_registration(RegistrationId, SessionId, Realm) ->
    Module = get_module(),
    Module:remove_registration(RegistrationId, SessionId, Realm).


%% invocation for keeping track of running calls

add_invocation(Invocation) ->
    Module = get_module(),
    Module:add_invocation(Invocation).

get_invocation(InvocationId, Realm) ->
    Module = get_module(),
    Module:get_invocation(InvocationId, Realm).

remove_invocation(InvocationId, Realm) ->
    Module = get_module(),
    Module:remove_invocation(InvocationId, Realm).


get_module() ->
    application:get_env(ct_router, data_if, ctr_data_off).
