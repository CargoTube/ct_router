-module(ct_router_sup_test).

-include_lib("eunit/include/eunit.hrl").

init_test() ->
    Result = ct_router_sup:init(noparams),
    ?assertEqual({ok, {#{}, []}}, Result).
