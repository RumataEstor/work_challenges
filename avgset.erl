%% https://codelab.interviewbit.com/problems/avgset/
-module(avgset).

-export([start/0]).

start() ->
    true and
        test([1, 7, 15, 29, 11, 9],
             {[9, 15], [1, 7, 11, 29]}) and
        test([33, 2, 12, 25, 38, 11, 46, 17, 19, 47, 13, 0, 39, 42, 8, 4],
             {[0,4,38,47], [2,8,11,12,13,17,19,25,33,39,42,46]}) and
        test([28, 14, 34, 18, 20, 34, 17, 24, 7, 22, 17, 49, 49,
              20, 46, 22, 18, 46, 45, 39, 50, 40, 17, 48, 23],
             []) and
        test([28, 14, 34, 18, 20, 34, 17, 24, 7, 22, 17, 49, 49,
              20, 46, 22, 18, 46, 45, 39, 50, 40, 17, 48, 23,
              0, 0, 0, 18, 46, 45, 39, 50, 40, 17, 48, 23],
             {[18,40], [0,0,0,7,14,17,17,17,17,18,18,20,20,22,22,23,23,24,
                        28,34,34,39,39,40,45,45,46,46,46,48,48,49,49,50,50]}) and
        true.

test(Col, Expected) ->
    fbcommon:test(Col, Expected, find(Col)).


find(List) ->
    Sorted = lists:sort(List),
    N = length(Sorted),
    S = lists:foldl(fun erlang:'+'/2, 0, Sorted),
    case find_by_len(Sorted, 1, S, N) of
        [] ->
            [];
        Result ->
            {Result, Sorted -- Result}
    end.


find_by_len(_, N1, _, N) when N1 + N1 > N ->
    [];
find_by_len(Sorted, N1, S, N) ->
    %% io:format("find_by_len(~p, ~p, ~p, ~p)~n", [Sorted, N1, S, N]),
    S1B = S * N1,
    if
        S1B rem N =/= 0 ->
            find_by_len(Sorted, N1 + 1, S, N);
        true ->
            S1 = S1B div N,
            case find_by_sum(Sorted, N1, S1) of
                [] ->
                    find_by_len(Sorted, N1 + 1, S, N);
                Result ->
                    Result
            end
    end.

find_by_sum([], _, _) ->
    [];
find_by_sum(_, 0, _) ->
    [];
find_by_sum([Item | _], 1, Item) ->
    [Item];
find_by_sum([Item | _], _, S) when Item > S ->
    [];
find_by_sum([Item | Sorted], N, S) ->
    case find_by_sum(Sorted, N - 1, S - Item) of
        [] ->
            find_by_sum(Sorted, N, S);
        Result ->
            [Item | Result]
    end.
