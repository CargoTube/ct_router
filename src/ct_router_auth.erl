-module(ct_router_auth).

-export([handle_hello/2,
        handle_authenticate/2]).


-include_lib("ct_msg/include/ct_msg.hrl").

handle_hello({hello, _Realm, _Details}, PeerAtGate) ->
    %% TODO: implement
    PeerAtGate ! {to_peer, ?WELCOME( 1, #{})},
    ok.

handle_authenticate(_Authenticate, PeerAtGate) ->
    PeerAtGate ! {to_peer, ?ABORT(#{}, canceled)},
    ok.
