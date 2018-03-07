%% https://codelab.interviewbit.com/problems/maxcoin/
-module(maxcoin).

-export([start/0]).

start() ->
    true and
        test({1, 2}, 2) and
        test({1, 2, 3, 4}, 6) and
        test({28,14,34,18,20,34,17,24,7,22,17,49,49,20,46,22,18,46,45,39,50,
              40,17,48,23,0,0,0,18,46,45,39,50,40,17,48,23}, 524) and
        true.


test(Coins, Expected) ->
    fbcommon:test(Coins, Expected, maxcoin(Coins)).


maxcoin(Coins) ->
    {_, {Value, _}} = maxcoin(Coins, 1, tuple_size(Coins), #{}),
    Value.


maxcoin(Coins, Start, Start, Cache) ->
    {Cache, {element(Start, Coins), 0}};
maxcoin(Coins, Start, End, Cache) ->
    Key = {Start, End},
    case Cache of
        #{Key := Value} ->
            {Cache, Value};
        _ ->
            {NewCache, Value} = find_best(Coins, Start, End, Cache),
            {NewCache#{Key => Value}, Value}
    end.


find_best(Coins, Start, End, Cache0) ->
    {Cache1, {OtherStart, ThisStart}} = maxcoin(Coins, Start + 1, End, Cache0),
    {Cache2, {OtherEnd, ThisEnd}} = maxcoin(Coins, Start, End - 1, Cache1),
    NewThisStart = ThisStart + element(Start, Coins),
    NewThisEnd = ThisEnd + element(End, Coins),
    Value = if NewThisStart - OtherStart >= NewThisEnd - OtherEnd ->
                    {NewThisStart, OtherStart};
               true ->
                    {NewThisEnd, OtherEnd}
            end,
    {Cache2, Value}.
