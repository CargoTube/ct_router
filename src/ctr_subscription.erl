-module(ctr_subscription).

-include("ct_router.hrl").

-export([to_map/1,

         get_id/1,
         get_uri/1,
         get_subscribers/1,

         new/2,
         delete/2,

         lookup/2,

         list_of_realm/1,
         separated_list_of_realm/1,

         init/0
        ]).

init() ->
    ctr_broker_data:create_table(),
    ok.

to_map(#ctr_subscription{id = Id, created = Created, uri = Uri,
                         match = Match, subscribers = Subs }) ->
    #{id => Id,
      created => iso8601:format(Created),
      match => Match,
      uri => Uri,
      subs => Subs}.

get_id(#ctr_subscription{id = Id}) ->
    Id.

get_uri(#ctr_subscription{uri = Uri}) ->
    Uri.

get_subscribers(#ctr_subscription{subscribers = Subs}) ->
    Subs.


new(Uri, Session) ->
    Realm = cta_session:get_realm(Session),
    SessionId = cta_session:get_id(Session),
    ctr_broker_data:add_subscription(Uri, Realm, SessionId).

delete(SubscriptionId, SessionId) ->
    ctr_broker_data:delete_subscription(SubscriptionId, SessionId).

lookup(SubscriptionId, Realm) ->
    ctr_broker_data:get_subscription(SubscriptionId, Realm).


list_of_realm(Realm) ->
    ctr_broker_data:get_subscription_list(Realm).

separated_list_of_realm(Realm) ->
    {ok, Subscriptions} = list_of_realm(Realm),

    Separator = fun(#ctr_subscription{ id = Id, match = exact },
                    {ExactList, PrefixList, WildcardList}) ->
                        { [ Id | ExactList ], PrefixList, WildcardList };
                   (#ctr_subscription{ id = Id, match = prefix },
                    {ExactList, PrefixList, WildcardList}) ->
                        { ExactList, [ Id | PrefixList], WildcardList };
                   (#ctr_subscription{ id = Id, match = wildcard },
                    {ExactList, PrefixList, WildcardList}) ->
                        { ExactList, PrefixList, [ Id | WildcardList ] }
                end,
    {E, P, W} = lists:foldl(Separator, {[], [], []}, Subscriptions),
    #{exact => E, prefix => P, wildcard => W}.
