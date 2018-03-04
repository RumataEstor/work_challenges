%% https://codelab.interviewbit.com/problems/arrange2/
-module(arrange2).

-export([start/0]).

start() ->
    true and
        test(<<"WWWB">>, 2, 0) and
        test(<<"BBBBBBBBBBBBBBBBBBBBBBBBBWWWWWWWWWWWWWWWWWWW">>, 2, 0) and
        test(<<"BBBBBBBBBBBBBBBBBBBBBBWWWWWWWWWWWWWWWWWWWWWW">>, 19, 0) and
        test(<<"BWBWWWWBWBBWBWBWBBWBBBWWWBWBWBWWWBWBWBWBBWBW">>, 19, 13) and
        test(<<"W">>, 2, -1) and
        true.

test(Horses, Stables, Expected) ->
    fbcommon:test({Horses, Stables}, Expected, arrange(Horses, Stables)).


arrange(Horses, Stables) ->
    {_, Value} = find_best(Horses, 0, Stables, #{}),
    Value.


find_best(Horses, Current, Stables, Cache) ->
    Key = {Current, Stables},
    case Cache of
        #{Key := Value} ->
            {Cache, Value};
        _ when Stables == 1 ->
            Value = calc_rest(Horses, 0, 0),
            {Cache#{Key => Value}, Value};
        _ ->
            {NewCache, Value} = find_best(Horses, Current, Stables, Cache, 0, 0, -1),
            {NewCache#{Key => Value}, Value}
    end.


calc_rest(<<$W, Rest/binary>>, White, Black) ->
    calc_rest(Rest, White + 1, Black);
calc_rest(<<$B, Rest/binary>>, White, Black) ->
    calc_rest(Rest, White, Black + 1);
calc_rest(<<>>, White, Black) ->
    White * Black.


find_best(Horses, _Current, Stables, Cache, _, _, Best)
  when Stables > byte_size(Horses) ->
    {Cache, Best};
find_best(<<Horse, Rest/binary>> = Horses, Current, Stables, Cache, White, Black, Best) ->
    {NewCache, RestValue} = find_best(Rest, Current + 1, Stables - 1, Cache),
    {NewWhite, NewBlack} = case Horse of
                               $W -> {White + 1, Black};
                               $B -> {White, Black + 1}
                           end,
    case NewWhite * NewBlack + RestValue of
        NewBest when NewBest < Best; Best =:= -1 ->
            find_best(Rest, Current + 1, Stables, NewCache, NewWhite, NewBlack, NewBest);
        _ ->
            find_best(Rest, Current + 1, Stables, NewCache, NewWhite, NewBlack, Best)
    end.
