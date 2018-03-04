%% https://codelab.interviewbit.com/problems/nthend/
-module(nthend).

-export([start/0]).

start() ->
    true and
        test([1, 2, 3, 4, 5], 2, [1, 2, 3, 5]) and
        test([1, 2, 3, 4, 5], 5, [2, 3, 4, 5]) and
        test([1, 2, 3, 4, 5], 15, [2, 3, 4, 5]) and
        test([1, 2, 3, 4, 5], 0, [1, 2, 3, 4]) and
        test([1, 2, 3, 4, 5], 1, [1, 2, 3, 4]) and
        true.

test(List, N, Expected) ->
    fbcommon:test({List, N}, Expected, nthend(List, N)).


nthend(List, N) ->
    Length = length(List),
    remove(Length - N, List).

remove(_, [_]) ->
    [];
remove(I, [_ | List]) when I =< 0 ->
    List;
remove(N, [V | List]) ->
    [V | remove(N - 1, List)].
