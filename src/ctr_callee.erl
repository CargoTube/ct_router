-module(ctr_callee).

-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         is_procedure/1,

         handle_call/2
        ]).

-define(PROCEDURES,
        [{ <<"wamp.session.count">>, fun session_count/3 },
         { <<"wamp.session.list">>, fun session_list/3 },
         { <<"wamp.session.get">>, fun session_get/3 },

         { <<"wamp.registration.list">>, fun reg_list/3 },
         { <<"wamp.registration.lookup">>, fun reg_lookup/3 },
         { <<"wamp.registration.match">>, fun reg_match/3 },
         { <<"wamp.registration.get">>, fun reg_get/3 },
         { <<"wamp.registration.list_callees">>, fun reg_callees/3 },
         { <<"wamp.registration.count_callees">>, fun reg_callee_count/3 },

         { <<"wamp.subscription.list">>, fun subs_list/3 },
         { <<"wamp.subscription.lookup">>, fun subs_lookup/3 },
         { <<"wamp.subscription.match">>, fun subs_match/3 },
         { <<"wamp.subscription.get">>, fun subs_get/3 },
         { <<"wamp.subscription.list_subscribers">>, fun subs_sub_list/3 },
         { <<"wamp.subscription.count_subscribers">>, fun subs_sub_count/3 },

         undefined
        ]).

is_procedure(Procedure) ->
    Result = lists:keyfind(Procedure, 1, ?PROCEDURES),
    is_tuple(Result).


handle_call({call, ReqId, _Options, Procedure, Args, ArgsKw}, Session) ->
    try
        Realm = cta_session:get_realm(Session),
        {_, Fun} = lists:keyfind(Procedure, 1, ?PROCEDURES),
        {ResArgs, ResArgsKw} = Fun(Args, ArgsKw, Realm),
        ?RESULT(ReqId, #{}, ResArgs, ResArgsKw)
    catch Error ->
            ?ERROR(call, ReqId, #{}, Error);
          error:function_clause ->
            ?ERROR(call, ReqId, #{}, invalid_argument)
    end.


session_count(Args, Kw, Realm) ->
    ctr_callee_session:count(Args, Kw, Realm).

session_list(Args, Kw, Realm) ->
    ctr_callee_session:list(Args, Kw, Realm).

session_get(Args, Kw, Realm) ->
    ctr_callee_session:get(Args, Kw, Realm).



reg_list(Args, Kw, Realm) ->
    ctr_callee_registration:list(Args, Kw, Realm).

reg_lookup(_Args, _Kw, _Realm) ->
    throw(no_such_procedure).

reg_match(_Args, _Kw, _Realm) ->
    throw(no_such_procedure).

reg_get(Args, Kw, Realm) ->
    ctr_callee_registration:get(Args, Kw, Realm).

reg_callees(Args, Kw, Realm) ->
    ctr_callee_registration:callees(Args, Kw, Realm).

reg_callee_count(Args, Kw, Realm) ->
    ctr_callee_registration:callee_count(Args, Kw, Realm).



subs_list(Args, Kw, Realm) ->
    ctr_callee_subscription:list(Args, Kw, Realm).

subs_lookup(_Args, _Kw, _Realm) ->
    throw(no_such_procedure).

subs_match(_Args, _Kw, _Realm) ->
    throw(no_such_procedure).

subs_get(Args, Kw, Realm) ->
    ctr_callee_subscription:get(Args, Kw, Realm).

subs_sub_list(Args, Kw, Realm) ->
    ctr_callee_subscription:subscriber(Args, Kw, Realm).

subs_sub_count(Args, Kw, Realm) ->
    ctr_callee_subscription:subscriber_count(Args, Kw, Realm).
