-module(ctr_gen_data).


-include("ct_router.hrl").

-export([
         initialize/0,

         do_list_subscriptions/1,
         do_lookup_subscription/3,
         do_match_subscription/2,
         do_get_subscription/2,
         do_add_subscription/4,
         do_remove_subscription/3,
         do_store_publication/1,

         do_list_registrations/1,
         do_lookup_registration/3,
         do_match_registration/2,
         do_get_registration/2,
         do_add_registration/4,
         do_remove_registration/3,

         do_add_invocation/1,
         do_get_invocation/2,
         do_remove_invocation/2
        ]).


-callback init() -> ok.


-type uri() :: binary().
-type error_reason() :: any().
-type options() :: map().
-type id() :: non_neg_integer().
-type match() :: exact | prefix | wildcard.
-type subscription() :: #ctr_subscription{}.
-type publication() :: #ctr_publication{}.
-type registration() :: #ctr_registration{}.
-type invocation() :: #ctrd_invocation{}.


%% for broker with subscriptions

-callback list_subscriptions(Realm :: uri()) ->
    {ok, [ subscription() ]} | {error, Reason :: error_reason()}.


-callback lookup_subscription(Procedure :: uri(), Options :: options(),
                               Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback match_subscription(Procedure :: uri(), Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback get_subscription(ProcedureId :: id(), Realm :: uri()) ->
    {ok, subscription()} | {error, Reason :: error_reason()}.


-callback add_subscription(Uri :: uri(), Match :: match(), SessionId :: id(),
                           Realm :: uri()) ->
    { created | added, subscription()} | {error, Reason :: error_reason()}.


-callback  remove_subscription(SubscriptionId :: id(), SessionId :: id(),
                               Realm :: uri() ) ->
    { removed | deleted, subscription() } | {error, Reason :: error_reason()}.


-callback store_publication(Publication :: publication()) ->
    { ok, Publication :: publication() } | { error, Reason :: error_reason() }.


%% for dealer with registrations and invocations

-callback list_registrations(Realm :: uri()) ->
    {ok, RegistratiionList :: [registration()]}.


-callback lookup_registration(Procdure :: uri(), Options :: options(),
                              Realm :: uri()) ->
    {ok, Registration :: registration()} | {error, Reason :: error_reason()}.


-callback match_registration(Procdure :: uri(), Realm :: uri()) ->
    {ok, Registration :: registration()} | {error, Reason :: error_reason()}.

-callback get_registration(ProcdureId :: id(), Realm :: uri()) ->
    {ok, Registration :: registration()} | {error, Reason :: error_reason()}.


-callback add_registration(Procedure :: uri(), Match :: match(),
                           SessionId :: id(), Realm :: uri()) ->
    {created | added, Registration :: registration()} |
    {error, Reason :: error_reason()}.

-callback remove_registration(RegistrationId :: id(), SessionId :: id(),
                              Realm :: uri() ) ->
    {removed | deleted, Registration :: registration()} |
    {error, Reason :: error_reason()}.


%% invocation for keeping track of running calls

-callback add_invocation( Invocation :: invocation() ) ->
    {ok, UpdatedInvocation :: invocation()}.

-callback get_invocation(InvocationId :: id(), Realm :: uri()) ->
    {ok, Invocation :: invocation()} | {error, Reason :: error_reason()}.

-callback remove_invocation(InvocationId :: id(), Realm :: uri()) ->
    ok | {error, Reason :: error_reason()}.

do_list_subscriptions(Realm) ->
    Module = get_module(),
    Module:list_subscriptions(Realm).

do_lookup_subscription(Procedure, Options, Realm) ->
    Module = get_module(),
    Module:lookup_subscription(Procedure, Options, Realm).


do_match_subscription(Procedure, Realm) ->
    Module = get_module(),
    Module:match_subscription(Procedure, Realm).


do_get_subscription(ProcedureId, Realm) ->
    Module = get_module(),
    Module:get_subscription(ProcedureId, Realm).


do_add_subscription(Uri, Match, SessionId, Realm) ->
    Module = get_module(),
    Module:add_subscription(Uri, Match, SessionId, Realm).


do_remove_subscription(SubscriptionId, SessionId, Realm) ->
    Module = get_module(),
    Module:remove_subscription(SubscriptionId, SessionId, Realm).


do_store_publication(Publication) ->
    Module = get_module(),
    Module:store_publication(Publication).



do_list_registrations(Realm) ->
    Module = get_module(),
    Module:list_registrations(Realm).


do_lookup_registration(Procdure, Options, Realm) ->
    Module = get_module(),
    Module:lookup_registration(Procdure, Options, Realm).


do_match_registration(Procdure, Realm) ->
    Module = get_module(),
    Module:match_registration(Procdure, Realm).

do_get_registration(ProcdureId, Realm) ->
    Module = get_module(),
    Module:get_registration(ProcdureId, Realm).


do_add_registration(Procedure, Match, SessionId, Realm) ->
    Module = get_module(),
    Module:add_registration(Procedure, Match, SessionId, Realm).

do_remove_registration(RegistrationId, SessionId, Realm) ->
    Module = get_module(),
    Module:remove_registration(RegistrationId, SessionId, Realm).


%% invocation for keeping track of running calls

do_add_invocation(Invocation) ->
    Module = get_module(),
    Module:add_invocation(Invocation).

do_get_invocation(InvocationId, Realm) ->
    Module = get_module(),
    Module:get_invocation(InvocationId, Realm).

do_remove_invocation(InvocationId, Realm) ->
    Module = get_module(),
    Module:remove_invocation(InvocationId, Realm).


initialize() ->
    Module = get_module(),
    Module:init().


get_module() ->
    application:get_env(ct_router, data_if, ctr_gen_data_off).
