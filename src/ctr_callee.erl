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
         { <<"wamp.registration.list_callees">>, fun reg_list_callees/3 },
         { <<"wamp.registration.count_callees">>, fun reg_count_callees/3 },

         { <<"wamp.subscription.list">>, fun subs_list/3 },
         { <<"wamp.subscription.lookup">>, fun subs_lookup/3 },
         { <<"wamp.subscription.match">>, fun subs_match/3 },
         { <<"wamp.subscription.get">>, fun subs_get/3 },
         { <<"wamp.subscription.list_subscribers">>, fun subs_list_subs/3 },
         { <<"wamp.subscription.count_subscribers">>, fun subs_count_subs/3 },

         undefined
        ]).

is_procedure(<< Prefix:5/binary, _/binary >>) ->
    Prefix == <<"wamp.">>;
is_procedure(_) ->
    false.

handle_call({call, ReqId, Options, Procedure, Args, ArgsKw}, Session) ->
    try
        Result = lists:keyfind(Procedure, 1, ?PROCEDURES),
        maybe_call_fun(Result, ReqId, Options, Args, ArgsKw, Session)
    catch Error ->
            ?ERROR(call, ReqId, #{}, Error);
          error:function_clause ->
            ?ERROR(call, ReqId, #{}, invalid_argument)
    end.


maybe_call_fun({_, Fun}, RequestId, _Options, Args, ArgsKw, Session) ->
    Realm = cta_session:get_realm(Session),
    {ResArgs, ResArgsKw} = Fun(Args, ArgsKw, Realm),
    ?RESULT(RequestId, #{}, ResArgs, ResArgsKw);
maybe_call_fun(_, RequesId, _Options, _Args, _ArgsKw, _Session) ->
    ?ERROR(call, RequesId, #{}, no_such_procedure).




session_count(Args, Kw, Realm) ->
    ctr_callee_session:count(Args, Kw, Realm).

session_list(Args, Kw, Realm) ->
    ctr_callee_session:list(Args, Kw, Realm).

session_get(Args, Kw, Realm) ->
    ctr_callee_session:get(Args, Kw, Realm).



reg_list(Args, Kw, Realm) ->
    ctr_callee_registration:list(Args, Kw, Realm).

reg_lookup(Args, Kw, Realm) ->
    ctr_callee_registration:lookup(Args, Kw, Realm).

reg_match(Args, Kw, Realm) ->
    ctr_callee_registration:match(Args, Kw, Realm).

reg_get(Args, Kw, Realm) ->
    ctr_callee_registration:get(Args, Kw, Realm).

reg_list_callees(Args, Kw, Realm) ->
    ctr_callee_registration:list_callees(Args, Kw, Realm).

reg_count_callees(Args, Kw, Realm) ->
    ctr_callee_registration:count_callees(Args, Kw, Realm).



subs_list(Args, Kw, Realm) ->
    ctr_callee_subscription:list(Args, Kw, Realm).

subs_lookup(Args, Kw, Realm) ->
    ctr_callee_subscription:lookup(Args, Kw, Realm).

subs_match(Args, Kw, Realm) ->
    ctr_callee_subscription:match(Args, Kw, Realm).

subs_get(Args, Kw, Realm) ->
    ctr_callee_subscription:get(Args, Kw, Realm).

subs_list_subs(Args, Kw, Realm) ->
    ctr_callee_subscription:list_subscribers(Args, Kw, Realm).

subs_count_subs(Args, Kw, Realm) ->
    ctr_callee_subscription:count_subscribers(Args, Kw, Realm).
