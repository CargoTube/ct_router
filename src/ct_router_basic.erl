-module(ct_router_basic).

-behaviour(ct_router_if).

-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_hello_message/2,
         handle_established_message/4,
         handle_session_closed_message/2
        ]).

handle_hello_message(_Hello, Gate) ->
    Gate ! {to_peer, ?ABORT(#{}, no_such_realm)},
    ok.

handle_established_message(_Type, _Message, _Session, _Gate) ->
    ok.

handle_session_closed_message(_Session, _Gate) ->
    ok.
