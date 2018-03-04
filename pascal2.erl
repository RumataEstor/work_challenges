%% https://codelab.interviewbit.com/problems/pascal2/
-module(pascal2).

-export([start/0]).

start() ->
    true and
        test(0, [1]) and
        test(1, [1, 1]) and
        test(2, [1, 2, 1]) and
        test(3, [1, 3, 3, 1]) and
        test(4, [1, 4, 6, 4, 1]) and
        test(5, [1, 5, 10, 10, 5, 1]) and
        test(6, [1, 6, 15, 20, 15, 6, 1]) and
        true.

test(Input, Expect) ->
    fbcommon:test(Input, Expect, calc_row(Input)).

calc_row(N) when N >= 0 ->
    calc_row(N, [1]).

calc_row(0, Result) ->
    Result;
calc_row(N, Prev) ->
    calc_row(N - 1, calc_next(Prev)).


calc_next([A | Rest]) ->
    calc_next(A, Rest, [1]).

calc_next(A, [B | Rest], Result) ->
    calc_next(B, Rest, [A + B | Result]);
calc_next(1, [], Result) ->
    [1 | Result].
