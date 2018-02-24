-module(ctr_routing).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_established/3
        ]).


handle_established(Type, Message, Session) ->
    IsAuth = ctr_session:is_authenticated(Session),
    IsAllowed = ctr_auth:is_message_allowed(Message, Session),
    maybe_handle_message(IsAuth, IsAllowed, Type, Message, Session).

maybe_handle_message(true, true, Type, Message, Session) ->
    lager:debug("routing: handle message ~p [~p]", [Message, Session]),
    handle_message(Type, Message, Session);
maybe_handle_message(true, _, Type, Message, Session) ->
    lager:debug("routing: message forbidden ~p [~p]", [Message, Session]),
    send_auth_error(Type, Message, Session);
maybe_handle_message(_, _, _Type, Message, Session) ->
    lager:debug("routing: NOT AUTHED! ~p [~p]", [Message, Session]),
    ct_router:to_peer(ctr_session:get_peer(Session), ?GOODBYE(#{}, canceled)).


handle_message(Type, Message, Session) ->
    send_auth_error(Type, Message, Session).

send_auth_error(Type, Message, Session) ->
    lager:debug("routing: auth error ~p [~p]", [Message, Session]),
    {ok, RequestId} = ct_msg:get_request_id(Message),
    Msg = ?ERROR(Type, RequestId, #{}, not_authorized),
    send_to_peer(Session, Msg).

send_to_peer(Session, Msg) ->
    ct_router:to_peer(ctr_session:get_peer(Session), Msg).
