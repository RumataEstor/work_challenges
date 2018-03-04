%% https://codelab.interviewbit.com/problems/excel1/
-module(excel1).

-export([start/0]).

start() ->
    true and
        test("A", 1) and
        test("B", 2) and
        test("C", 3) and
        test("Z", 26) and
        test("AA", 27) and
        test("AB", 28) and
        test("ZUB", 18124) and
        true.

test(Col, Expected) ->
    fbcommon:test(Col, Expected, num(Col)).


num(Col) ->
    num(Col, 0).

num([Ch | Rest], N) when Ch >= $A, Ch =< $Z ->
    num(Rest, N * 26 + Ch - $A + 1);
num([], N) ->
    N.
