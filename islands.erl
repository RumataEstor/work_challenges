%% https://www.careercup.com/question?id=4593205218639872
-module(islands).

-export([start/0]).

start() ->
    true and
        test(["01010",
              "01001",
              "01101"], 3) and
        test(["10101",
              "10111"], 2) and
        test(["10101",
              "10111",
              "11100"], 1) and
        true.


test(Input, Expected) ->
    fbcommon:test(Input, Expected, count(Input)).


count([Row | _] = Rows) ->
    count(lists:duplicate(length(Row), 0), Rows, #{next => 1});
count([]) ->
    0.

count(TopRow, [Row | Rest], MaybeIslands) ->
    {NewRow, NewIslands} = count_row(Row, TopRow, MaybeIslands),
    count(NewRow, Rest, NewIslands);
count(_, [], MaybeIslands) ->
    length(maps:values(maps:remove(next, MaybeIslands))).

count_row(Row, TopRow, MaybeIslands) ->
    count_row(Row, TopRow, MaybeIslands, 0, []).

count_row([], [], MaybeIslands, _, Result) ->
    {lists:reverse(Result), MaybeIslands};
count_row([$0 | Row], [_ | TopRow], MaybeIslands, _, Result) ->
    count_row(Row, TopRow, MaybeIslands, 0, [0 | Result]);
count_row([$1 | Row], [0 | TopRow], MaybeIslands, 0, Result) ->
    #{next := Next} = MaybeIslands,
    NextIslands = MaybeIslands#{next => Next + 1, Next => Next},
    count_row(Row, TopRow, NextIslands, Next, [Next | Result]);
count_row([$1 | Row], [0 | TopRow], MaybeIslands, Last, Result) ->
    count_row(Row, TopRow, MaybeIslands, Last, [Last | Result]);
count_row([$1 | Row], [Top | TopRow], MaybeIslands, 0, Result) ->
    count_row(Row, TopRow, MaybeIslands, Top, [Top | Result]);
count_row([$1 | Row], [Top | TopRow], MaybeIslands, Left, Result) ->
    NewIslands = maps:remove(Top, MaybeIslands),
    count_row(Row, TopRow, NewIslands, Left, [Left | Result]).
