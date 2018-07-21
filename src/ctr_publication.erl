-module(ctr_publication).

-include("ct_router.hrl").

-export([new/6,
         get_id/1,
         get_subscription_id/1,
         get_subscribers/1
        ]).


new(Realm, Topic, Options, Arguments, ArgumentsKw, SessionId) ->
    #ctr_publication{realm = Realm,
                     topic = Topic,
                     options = Options,
                     pub_sess_id = SessionId,
                     ts = calendar:universal_time(),
                     arguments = Arguments,
                     argumentskw = ArgumentsKw}.

get_id(#ctr_publication{id = Id}) ->
    Id.

get_subscription_id(#ctr_publication{sub_id = SubId}) ->
    SubId.

get_subscribers(#ctr_publication{subs = Subs}) ->
    Subs.
