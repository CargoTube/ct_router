-module(ctr_routing).


-include_lib("ct_msg/include/ct_msg.hrl").

-export([
         handle_established/3
        ]).


handle_established(Type, Message, Session) ->
    IsAuth = cta_session:is_authenticated(Session),
    IsAllowed = ct_auth:is_message_allowed(Message, Session),
    maybe_handle_message(IsAuth, IsAllowed, Type, Message, Session).

maybe_handle_message(true, true, Type, Message, Session) ->
    handle_message(Type, Message, Session);
maybe_handle_message(true, _, Type, Message, Session) ->
    send_auth_error(Type, Message, Session);
maybe_handle_message(_, _, _Type, _Message, Session) ->
    ct_router:to_session(Session, ?GOODBYE(#{}, canceled)).


handle_message(Type, Message, Session)
  when Type == subscribe; Type == unsubscribe; Type == publish  ->
    ctr_broker:handle_message(Type, Message, Session);
handle_message(Type, Message, Session)
  when Type == call; Type == register; Type == unregister;
       Type == yield; Type == error ->
    ctr_dealer:handle_message(Type, Message, Session);
handle_message(Type, Message, Session) ->
    send_auth_error(Type, Message, Session).

send_auth_error(Type, Message, Session) ->
    {ok, ReqId} = ct_msg:get_request_id(Message),
    ct_router:to_session(Session, ?ERROR(Type, ReqId, #{}, not_authorized)).
