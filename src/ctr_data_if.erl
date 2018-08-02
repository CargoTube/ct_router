-module(ctr_data_if).


-include("ct_router.hrl").


-callback init() -> ok.



%% for broker with subscriptions

-callback list_subscriptions(Realm :: binary()) ->
    {ok, [ #ctr_subscription{} ]} | {error, Reason :: any()}.


-callback lookup_subscriptions(Procedure :: binary(), Options :: map(),
                               Realm :: binary()) ->
    {ok, #ctr_subscription{}} | {error, Reason :: any()}.


-callback match_subscription(Procedure :: binary(), Realm :: binary()) ->
    {ok, #ctr_subscription{}} | {error, Reason :: any()}.


-callback get_subscription(ProcedureId :: non_neg_integer(),
                           Realm :: binary()) ->
    {ok, #ctr_subscription{}} | {error, Reason :: any()}.


-callback add_subscription(Uri :: binary(), Match :: atom(),
                           SessionId :: non_neg_integer(), Realm :: binary()) ->
    { created | added, #ctr_subscription{}}.


-callback  remove_subscription(SubscriptionId :: non_neg_integer(),
                               SessionId :: non_neg_integer(),
                               Realm :: binary() ) ->
    { removed | deleted, #ctr_subscription{} } | {error, not_found}.


-callback store_publication(Publication :: #ctr_publication{}) ->
    { ok, #ctr_publication{} }.



%% for dealer with registrations and invocations

-callback list_registrations(Realm :: binary()) ->
    {ok, [#ctr_registration{}]}.


-callback lookup_registration(Procdure :: binary(), Options :: map(),
                              Realm :: binary()) ->
    {ok, #ctr_registration{}} | {error, Reason :: any()}.


-callback match_registration(Procdure :: binary(), Realm :: binary()) ->
    {ok, #ctr_registration{}} | {error, Reason :: any()}.

-callback get_registration(ProcdureId :: non_neg_integer(),
                           Realm :: binary()) ->
    {ok, #ctr_registration{}} | {error, Reason :: any()}.


-callback add_registration(Procedure :: binary(), Match :: atom(),
                           SessionId :: non_neg_integer(), Realm :: binary()) ->
    {created, #ctr_registration{}} | {error, Reason :: any()}.

-callback remove_registration(RegistrationId :: non_neg_integer(),
                              SessionId :: non_neg_integer(),
                              Realm :: binary() ) ->
    {removed | deleted, #ctr_registration{}} | {error, Reason :: any()}.


%% invocation for keeping track of running calls

-callback add_invocation(#ctrd_invocation{}) ->
    {ok, #ctrd_invocation{}}.

-callback get_invocation(InvocationId :: non_neg_integer(),
                         Realm :: binary()) ->
    {ok, #ctrd_invocation{}} | {error, Reason :: any()}.

-callback remove_invocation(InvocationId :: non_neg_integer(),
                            Realm :: binary()) ->
    ok | {error, Reason :: any()}.
