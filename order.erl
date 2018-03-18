% https://codelab.interviewbit.com/problems/order/
-module(order).

-export([start/0]).

start() ->
    true and
        test(lists:zip([5, 3, 2, 6, 1, 4],
                       [0, 1, 2, 0, 3, 2]),
             [5, 3, 2, 1, 6, 4]) and
        test([{76, 1}, {87, 0}], [87, 76]) and
        true.

test(Heights, Expect) ->
    fbcommon:test(Heights, Expect, run(Heights)).

run(Heights) ->
    Sorted = lists:sort(fun({H1, _}, {H2, _}) -> H1 >= H2 end, Heights),
    lists:foldl(fun put_next/2, [], Sorted).

put_next({H, InFronts}, Result) ->
    {Head, Tail} = lists:split(InFronts, Result),
    Head ++ [H] ++ Tail.
