-module(ctr_broker_data).

-include("ct_router.hrl").

-export([
         get_subscription_list/1,
         lookup_subscription/3,
         match_subscription/2,
         get_subscription/2,

         add_subscription/3,
         delete_subscription/2,

         store_publication/1,
         init/0
         ]).

init() ->
    create_table().

get_subscription(Id, Realm) ->
    MatchHead = #ctr_subscription{id=Id, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    [Subscription] ->
                        {ok, Subscription};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_get_result(Result).


get_subscription_list(Realm) ->
    MatchHead = #ctr_subscription{realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    List when is_list(List) ->
                        {ok, List};
                    Other ->
                        {error, Other}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_list_result(Result).


match_subscription(Topic, Realm) ->
    MatchHead = #ctr_subscription{realm=Realm, uri=Topic, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    List when is_list(List) ->
                        {ok, List};
                    Other ->
                        {error, Other}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_list_result(Result).

lookup_subscription(Topic, Options , Realm) ->
    Match = maps:get(match, Options, exact),
    MatchHead = #ctr_subscription{realm=Realm, match=Match, uri=Topic, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],
    Lookup =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    [Subscription] ->
                        {ok, Subscription};
                    _ ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(Lookup),
    handle_subscription_get_result(Result).


handle_subscription_get_result({atomic, Result}) ->
    Result;
handle_subscription_get_result(Other) ->
    lager:error("subscription get/lookup error: ~p", Other),
    {error, not_found}.

handle_subscription_list_result({atomic, {ok, List}}) ->
    {ok, List};
handle_subscription_list_result(Other) ->
    lager:error("subscription get list/match error: ~p", Other),
    {ok, []}.


add_subscription(Uri, Realm, SessionId) ->
    NewId = ctr_utils:gen_global_id(),
    NewSub = #ctr_subscription{id = NewId,
                               uri = Uri,
                               realm = Realm,
                               subscribers = [SessionId],
                               created = calendar:universal_time()
                              },

    MatchHead = #ctr_subscription{uri=Uri, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatchSpec = [{MatchHead, Guard, GiveObject}],

    WriteIfNew =
        fun(Id, Sub) ->
                case mnesia:wread({ctr_subscription, Id}) of
                    [] ->
                        ok = mnesia:write(Sub),
                        {created, Sub};
                    _ ->
                        {error, id_exists}
                end
        end,

    Store =
        fun() ->
                case mnesia:select(ctr_subscription, MatchSpec, write) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubs = [ SessionId |
                                    lists:delete(SessionId, Subs)],
                        NewSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubs
                                           },
                        ok = mnesia:write(NewSubscription),
                        {added, NewSubscription};
                    [] ->
                        WriteIfNew(NewId, NewSub)
                end
        end,
    Result = mnesia:transaction(Store),
    handle_store_result(Result, NewSub).

handle_store_result({atomic, {created, Subscription}}, _) ->
    {created, Subscription};
handle_store_result({atomic, {added, Subscription}}, _) ->
    {added, Subscription};
handle_store_result({atomic, {error, id_exists}}, Subscription) ->
    #ctr_subscription{ realm = Realm, subscribers = [SessionId],
                       uri = Uri} = Subscription,
    add_subscription(Uri, Realm, SessionId).

delete_subscription(SubId, SessionId) ->
    DeleteOrUpdateSubscription =
        fun([], Subscription) ->
                mnesia:delete({ctr_subscription, SubId}),
                {deleted, Subscription};
           (_, Subscription) ->
                ok = mnesia:write(Subscription),
                {removed, Subscription}
        end,

    Delete =
        fun() ->
                case mnesia:wread({ctr_subscription, SubId}) of
                    [#ctr_subscription{subscribers = Subs } = Subscription] ->
                        NewSubscriber = lists:delete(SessionId, Subs),
                        UpdatedSubscription = Subscription#ctr_subscription{
                                            subscribers = NewSubscriber
                                           },
                        DeleteOrUpdateSubscription(NewSubscriber,
                                                   UpdatedSubscription);
                    [] ->
                        {error, not_found}
                end
        end,
    Result = mnesia:transaction(Delete),
    handle_delete_result(Result).


handle_delete_result({atomic, {deleted, Subscription}}) ->
    {deleted, Subscription};
handle_delete_result({atomic, {removed, Subscription}}) ->
    {removed, Subscription};
handle_delete_result({atomic, {error, not_found}}) ->
    {error, not_found}.


store_publication(Pub0) ->
    #ctr_publication{
       realm = Realm,
       topic = Topic,
       pub_sess_id = SessId
      } = Pub0,
    {ok, Session} = cta_session:lookup(SessId),
    Realm = cta_session:get_realm(Session),
    NewPubId = ctr_utils:gen_global_id(),

    NewPub = Pub0#ctr_publication{id = NewPubId},

    MatchHead = #ctr_subscription{uri=Topic, realm=Realm, _='_'},
    Guard = [],
    GiveObject = ['$_'],
    MatSpec = [{MatchHead, Guard, GiveObject}],


    UpdateOrWriteNew =
        fun([#ctr_subscription{id = SubId, subscribers = Subs}]) ->
                UpdatedPub = NewPub#ctr_publication{sub_id=SubId, subs = Subs},
                ok = mnesia:write(UpdatedPub),
                {ok, UpdatedPub};
           ([]) ->
                ok = mnesia:write(NewPub),
                {ok, NewPub}
        end,

    LookupAndStore =
        fun() ->
                case mnesia:wread({ctr_publication, NewPubId}) of
                    [] ->
                        Found = mnesia:select(ctr_subscription, MatSpec, write),
                        UpdateOrWriteNew(Found);
                    _ ->
                        {error, pub_id_exists}
                end
        end,
    Result = mnesia:transaction(LookupAndStore),
    handle_publication_store_result(Result, Pub0).


handle_publication_store_result({atomic, {ok, Publication}}, _Pub0) ->
    {ok, Publication};
handle_publication_store_result({atomic, {error, pub_id_exists}}, Pub0) ->
    store_publication(Pub0).

create_table() ->
    mnesia:delete_table(ctr_subscription),
    mnesia:delete_table(ctr_publication),
    SubDef = [{attributes, record_info(fields, ctr_subscription)},
              {ram_copies, [node()]},
              {index, [realm, uri, match]}
             ],
    PubDef = [{attributes, record_info(fields, ctr_publication)},
              {ram_copies, [node()]},
              {index, [realm, topic]}
             ],
    {atomic, ok} = mnesia:create_table(ctr_subscription, SubDef),
    {atomic, ok} = mnesia:create_table(ctr_publication, PubDef),
    ok.
