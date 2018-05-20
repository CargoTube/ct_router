-module(test_utils).

-export([meck_new/1]).
-export([meck_done/1]).


meck_new([]) ->
    ok;
meck_new([H | T]) ->
    meck:new(H),
    meck_new(T).


meck_done([]) ->
    ok;
meck_done([H | T]) ->
    true = case meck:validate(H) of
               true -> true;
               _ -> io:format("invalid call to mock of ~p~n", [H]),
                    false
           end,
    ok = meck:unload(H),
    meck_done(T).
