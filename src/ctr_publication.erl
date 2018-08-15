-module(ctr_publication).

-include_lib("ctr_data/include/ctr_data.hrl").

-export([new/6,
         get_id/1,
         get_subscription_id/1,
         get_subscribers/1,
         get_details/1,

         store/1
        ]).


new(Realm, Topic, Options, Arguments, ArgumentsKw, SessionId) ->
    Publication = #ctr_publication{realm = Realm,
                                   topic = Topic,
                                   options = Options,
                                   pub_sess_id = SessionId,
                                   ts = calendar:universal_time(),
                                   arguments = Arguments,
                                   argumentskw = ArgumentsKw},
    store(Publication).

get_id(#ctr_publication{id = Id}) ->
    Id.

get_subscription_id(#ctr_publication{sub_id = SubId}) ->
    SubId.

get_subscribers(#ctr_publication{subs = Subs}) ->
    Subs.

get_details(#ctr_publication{details = Details}) ->
    Details.

store(Publication) ->
    ctr_data:store_publication(Publication).
