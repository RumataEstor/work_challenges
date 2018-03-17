% https://codelab.interviewbit.com/problems/searchrange/
-module(searchrange).

-export([start/0]).

start() ->
    true and
        test({5, 7, 7, 8, 8, 10}, 8, {3, 4}) and
        test({1}, 1, {0, 0}) and
        test({4,7,7,7,8,10,10}, 3, {-1, -1}) and
        test({1,1,1,1,1,1,1,1,1,1,1,1,1,1,2,2,2,2,2,2,2,2,2,2,2,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,3,4,4,4,4,4,4,4,4,4,4,4,5,5,5,5,5,5,5,5,5,5,5,5,6,6,6,6,6,6,6,6,6,6,6,6,6,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,7,8,8,8,8,8,8,8,8,9,9,9,9,9,9,9,9,9,9,9,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10}, 10, {118, 133}) and
        true.

test(Array, Value, Expect) ->
    fbcommon:test({Array, Value}, Expect, run(Array, Value)).

run(Array, Value) ->
    run(Array, Value, 0, tuple_size(Array) + 1).

run(Array, Value, Start, End) when End - Start >= 2 ->
    Middle = (Start + End) div 2,
    case element(Middle, Array) of
        Smaller when Smaller < Value ->
            run(Array, Value, Middle, End);
        Bigger when Bigger > Value ->
            run(Array, Value, Start, Middle);
        _ ->
            {find_start(Array, Value, Start, Middle),
             find_end(Array, Value, Middle, End)}
    end;
run(_, _, _, _) ->
    {-1, -1}.


find_start(Array, Value, Start, End) when End - Start > 1 ->
    Middle = (Start + End) div 2,
    case element(Middle, Array) of
        Smaller when Smaller < Value ->
            find_start(Array, Value, Middle, End);
        _ ->
            find_start(Array, Value, Start, Middle)
    end;
find_start(_, _, _, End) ->
    End - 1.


find_end(Array, Value, Start, End) when End - Start > 1 ->
    Middle = (Start + End) div 2,
    case element(Middle, Array) of
        Bigger when Bigger > Value ->
            find_end(Array, Value, Start, Middle);
        _ ->
            find_end(Array, Value, Middle, End)
    end;
find_end(_, _, Start, _) ->
    Start - 1.
