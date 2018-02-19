-module(ctr_utils).

-export([
         init/0,
         gen_global_id/0
        ]).

init() ->
    ok.

gen_global_id() ->
    rand:uniform(9007199254740993) - 1.
