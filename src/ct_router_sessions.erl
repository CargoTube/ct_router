-module(ct_router_sessions).

-export([close_session/1,
         get_queue/1
        ]).


close_session(_Session) ->
    ok.

get_queue(_Session) ->
    ct_queue_established.
