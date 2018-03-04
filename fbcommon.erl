-module(fbcommon).

-export([test/3]).

test(Input, Expect, Result) ->
    case Result of
        Expect ->
            io:format("Passed for ~p~n", [Input]);
        _ ->
            io:format("~n"
                      "Unexpected result for ~p~n"
                      "       got ~p~n"
                      "  expected ~p~n"
                      "~n", [Input, Result, Expect])
    end.
