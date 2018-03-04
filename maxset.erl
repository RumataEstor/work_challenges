https://codelab.interviewbit.com/problems/maxset/
-module(maxset).

-export([start/0]).

start() ->
    test([1, 2, 3, -1, 4, 5], [4, 5]),
    test([-1, -1, -1], []),
    test([1, -1, 2, -2, 3, 0, 1, -6, 4], [3, 0, 1]),
    test([1, -1, 4, -3, 2, 0, 2, -2, 3, 0, 1, 0, -6, 4], [3, 0, 1, 0]),
    ok.

test(List, Expect) ->
    case maxset(List) of
        Expect ->
            io:format("Passed for ~p~n", [List]);
        Result ->
            io:fomrat("~n"
                      "Failed for ~p~n"
                      "       got ~p~n"
                      "  expected ~p~n",
                      [List, Result, Expect])
    end.


maxset(List) ->
    maxset(List, [], 0, 0).


maxset([], BestSet, _, _) ->
    lists:reverse(BestSet);
maxset(List, BestSet, BestSum, BestLength) ->
    {NextSet, NextSum, NextLength, Rest} = find_pos_set(List),
    if
        NextSum > BestSum orelse
        (NextSum == BestSum andalso
         NextLength > BestLength) ->
            maxset(Rest, NextSet, NextSum, NextLength);
        true ->
            maxset(Rest, BestSet, BestSum, BestLength)
    end.


find_pos_set(List) ->
    find_pos_set(List, [], 0, 0).


find_pos_set([Num | Rest], Set, Sum, Length) ->
    if Num >= 0 ->
            find_pos_set(Rest, [Num | Set], Sum + Num, Length + 1);
       true ->
            {Set, Sum, Length, Rest}
    end;
find_pos_set([], Set, Sum, Length) ->
    {Set, Sum, Length, []}.
