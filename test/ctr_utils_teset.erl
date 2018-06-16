-module(ctr_utils_teset).

-include_lib("eunit/include/eunit.hrl").

gen_global_id_test() ->
    Id = ctr_utils:gen_global_id(),
    ?assertEqual(true, Id >= 0),
    ?assertEqual(true, Id =< 9007199254740992).
