-module(ctr_subscription).

-include("ct_router.hrl").

-export([to_map/1,

         get_id/1,
         get_uri/1,
         get_subscribers/1
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
