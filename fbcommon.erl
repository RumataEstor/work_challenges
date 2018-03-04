-module(fbcommon).

-export([test/3]).
-export([tree_from_list/1]).


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


tree_from_list(List) ->
    [Result] = tree_from_list(1, List),
    Result.


tree_from_list(Amount, List) when Amount =/= 0 ->
    case partition_for_top(Amount, List, 0) of
        {0, []} ->
            build_leafs(Amount, List);
        {NewAmount, Rest} ->
            Bottom = tree_from_list(NewAmount, Rest),
            build_leafs(Amount, List, Bottom)
    end.


partition_for_top(0, Rest, Amount) ->
    {Amount, Rest};
partition_for_top(N, [-1 | Rest], Amount) ->
    partition_for_top(N - 1, Rest, Amount);
partition_for_top(N, [_ | Rest], Amount) ->
    partition_for_top(N - 1, Rest, Amount + 2).


build_leafs(0, _) -> [];
build_leafs(N, [-1 | Rest]) ->
    [nil | build_leafs(N - 1, Rest)];
build_leafs(N, [Val | Rest]) ->
    [{Val, nil, nil} | build_leafs(N - 1, Rest)].


build_leafs(0, _, []) -> [];
build_leafs(N, [-1 | Rest], Trees) ->
    [nil | build_leafs(N - 1, Rest, Trees)];
build_leafs(N, [Val | Rest], [Left, Right | Trees]) ->
    [{Val, Left, Right} | build_leafs(N - 1, Rest, Trees)].
