-module(ctr_utils).

-export([
         gen_global_id/0
        ]).

gen_global_id() ->
    rand:uniform(9007199254740993) - 1.
