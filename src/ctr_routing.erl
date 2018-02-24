-module(ctr_routing).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_established/4
        ]).


handle_established(Type, Message, Session, PeerAtGate) ->
    %% TODO: check auth
    lager:debug(" --> ~p : ~p <~p> [~p]", [Type, Message, Session, PeerAtGate]),
    ok.
