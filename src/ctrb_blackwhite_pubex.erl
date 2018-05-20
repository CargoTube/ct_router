-module(ctrb_blackwhite_pubex).

-export([filter_subscriber/3]).


filter_subscriber(Subs0, Options, Session) ->
    SessionId = cta_session:get_id(Session),
    Exclude = maps:get(exclude_me, Options, true),
    Subs = maybe_exclude_publisher(Exclude, SessionId, Subs0),
    Exclude = maps:get(exclude, Options, []),
    ExcludeAuthId = maps:get(exclude_authid, Options, []),
    ExcludeAuthRole = maps:get(exclude_authrole, Options, []),
    Eligible = maps:get(eligible, Options, all),
    EligibleAuthId = maps:get(eligible_authid, Options, all),
    EligibleAuthRole = maps:get(eligible_authrole, Options, all),
    FilterList = [{exclude, Exclude}, {exclude_authid, ExcludeAuthId},
                  {exclude_authrole, ExcludeAuthRole}, {eligible, Eligible},
                  {eligible_authid, EligibleAuthId},
                  {eligible_authrole, EligibleAuthRole}],
    do_filter_subscribers(Subs, FilterList).

maybe_exclude_publisher(true, Id, Subs) ->
    lists:delete(Id, Subs);
maybe_exclude_publisher(_, _, Subs) ->
    Subs.

do_filter_subscribers(Subs, []) ->
    Subs;
do_filter_subscribers(Subs, [{exclude, []} | Tail]) ->
    do_filter_subscribers(Subs, Tail);
do_filter_subscribers(Subs, [{exclude_authid, []} | Tail]) ->
    do_filter_subscribers(Subs, Tail);
do_filter_subscribers(Subs, [{exclude_authrole, []} | Tail]) ->
    do_filter_subscribers(Subs, Tail);
do_filter_subscribers(Subs, [{eligible, all} | Tail]) ->
    do_filter_subscribers(Subs, Tail);
do_filter_subscribers(Subs, [{eligible_authid, all} | Tail]) ->
    do_filter_subscribers(Subs, Tail);
do_filter_subscribers(Subs, [{eligible_authrole, all} | Tail]) ->
    do_filter_subscribers(Subs, Tail);
do_filter_subscribers(Subs, [{exclude, Exclude} | Tail]) ->
    Filter = fun(Id) ->
                     case lists:member(Id, Exclude) of
                         true -> false;
                         false -> true
                     end
             end,
    run_filter(Filter, Subs, Tail);
do_filter_subscribers(Subs, [{exclude_authid, Exclude} | Tail]) ->
    Filter = fun(Id) ->
                     case cta_session:lookup(Id) of
                         {ok, Session} ->
                             AuthId = cta_session:get_authid(Session),
                             case lists:member(AuthId, Exclude) of
                                 true -> false;
                                 false -> true
                             end;
                         _ ->
                             false
                     end
             end,
    run_filter(Filter, Subs, Tail);
do_filter_subscribers(Subs, [{exclude_authrole, Exclude} | Tail]) ->
    Filter = fun(Id) ->
                     case cta_session:lookup(Id) of
                         {ok, Session} ->
                             AuthRole = cta_session:get_authrole(Session),
                             case lists:member(AuthRole, Exclude) of
                                 true -> false;
                                 false -> true
                             end;
                         _ ->
                             false
                     end
             end,
    run_filter(Filter, Subs, Tail);
do_filter_subscribers(Subs, [{eligible, Eligible} | Tail]) ->
    Filter = fun(Id) ->
                     lists:member(Id, Eligible)
             end,
    run_filter(Filter, Subs, Tail);
do_filter_subscribers(Subs, [{eligible_authid, Eligible} | Tail]) ->
    Filter = fun(Id) ->
                     case cta_session:lookup(Id) of
                         {ok, Session} ->
                             AuthId = cta_session:get_authid(Session),
                             lists:member(AuthId, Eligible);
                         _ ->
                             false
                     end
             end,
    run_filter(Filter, Subs, Tail);
do_filter_subscribers(Subs, [{eligible_authrole, Eligible} | Tail]) ->
    Filter = fun(Id) ->
                     case cta_session:lookup(Id) of
                         {ok, Session} ->
                             AuthRole = cta_session:get_authrole(Session),
                             lists:member(AuthRole, Eligible);
                         _ ->
                             false
                     end
             end,
    run_filter(Filter, Subs, Tail).

run_filter(Filter, Subs, Tail) ->
    NewSubs = lists:filter(Filter, Subs),
    do_filter_subscribers(NewSubs, Tail).
