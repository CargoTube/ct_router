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


-define(DATA_MODULE(),
        application:get_env(ct_router, data_if, ctr_gen_data_off)).
-define(RUN(Func, Args),
        Module = ?DATA_MODULE(),
        apply(Module, Func, Args)).

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
    ?RUN(list_subscriptions, [Realm]).

do_lookup_subscription(Procedure, Options, Realm) ->
    ?RUN(lookup_subscription, [Procedure, Options, Realm]).

do_match_subscription(Procedure, Realm) ->
    ?RUN(match_subscription, [Procedure, Realm]).


do_get_subscription(ProcedureId, Realm) ->
    ?RUN(get_subscription, [ProcedureId, Realm]).

do_add_subscription(Uri, Match, SessionId, Realm) ->
    ?RUN(add_subscription, [Uri, Match, SessionId, Realm]).

do_remove_subscription(SubscriptionId, SessionId, Realm) ->
    ?RUN(remove_subscription, [SubscriptionId, SessionId, Realm]).


do_store_publication(Publication) ->
    ?RUN(store_publication, [Publication]).

do_list_registrations(Realm) ->
    ?RUN(list_registrations, [Realm]).

do_lookup_registration(Procedure, Options, Realm) ->
    ?RUN(lookup_registration, [Procedure, Options, Realm]).


do_match_registration(Procedure, Realm) ->
    ?RUN(match_registration, [Procedure, Realm]).

do_get_registration(ProcedureId, Realm) ->
    ?RUN(get_registration, [ProcedureId, Realm]).


do_add_registration(Procedure, Match, SessionId, Realm) ->
    ?RUN(add_registration, [Procedure, Match, SessionId, Realm]).

do_remove_registration(RegistrationId, SessionId, Realm) ->
    ?RUN(remove_registration, [RegistrationId, SessionId, Realm]).


%% invocation for keeping track of running calls

do_add_invocation(Invocation) ->
    ?RUN(add_invocation, [Invocation]).

do_get_invocation(InvocationId, Realm) ->
    ?RUN(get_invocation, [InvocationId, Realm]).

do_remove_invocation(InvocationId, Realm) ->
    ?RUN(remove_invocation, [InvocationId, Realm]).


initialize() ->
    Module = ?DATA_MODULE(),
    lager:debug("data interface is ~p",[Module]),
    Module:init().
