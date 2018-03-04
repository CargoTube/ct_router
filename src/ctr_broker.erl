-module(ctr_broker).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_message/3
        ]).


handle_message(register, Message, Session) ->
    do_register(Message, Session);
handle_message(unregister, Message, Session) ->
    do_unregister(Message, Session);
handle_message(publish, Message, Session) ->
    do_publish(Message, Session).


do_register(_, _) ->
    ok.

do_unregister(_, _) ->
    ok.

do_publish(_, _) ->
    ok.
