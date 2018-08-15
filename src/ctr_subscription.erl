-module(ctr_subscription).

-include_lib("ctr_data/include/ctr_data.hrl").

-export([
         to_map/1,

         add/3,
         remove/2,

         get_id/1,
         get_uri/1,
         get_subscribers/1,


         list_of_realm/1,
         lookup/3,
         match/2,
         get/2,

         separated_list_of_realm/1
        ]).


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

add(Uri, Match, Session) ->
    Realm = cta_session:get_realm(Session),
    SessionId = cta_session:get_id(Session),
    ctr_data:do_add_subscription(Uri, Match, Realm, SessionId).

remove(SubscriptionId, Session) ->
    Realm = cta_session:get_realm(Session),
    SessionId = cta_session:get_id(Session),
    ctr_data:do_remove_subscription(SubscriptionId, SessionId, Realm).

get(SubscriptionId, Realm) ->
    ctr_data:do_get_subscription(SubscriptionId, Realm).

lookup(Topic, Options, Realm) ->
    ctr_data:do_lookup_subscription(Topic, Options, Realm).

match(Topic, Realm) ->
    ctr_data:do_match_subscription(Topic, Realm).

list_of_realm(Realm) ->
    ctr_data:do_list_subscriptions(Realm).

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
