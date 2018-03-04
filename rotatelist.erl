%% https://codelab.interviewbit.com/problems/rotatelist/
-module(rotatelist).

-export([start/0]).

start() ->
    true and
        test([1, 2, 3, 4, 5], 2, [4, 5, 1, 2, 3]) and
        test([1, 2, 3, 4, 5], 5, [1, 2, 3, 4, 5]) and
        test([1, 2, 3, 4, 5], 11, [5, 1, 2, 3, 4]) and
        test([23,57,65,90,56,69,77,52,71,74,15,25,41,17,76,95,58,38,29,68,4,89,55,99,13,92,98,62,36,59,54,48,53,12,11,6,2,35,46,39,20,27,44,78,82,67,91,64,97,43,84,83,70,73,79,88,16,1,96,66,80,72,10,19,100,33,75,3,81,24,22,87,63,9,7,40,8,34,101,60,28], 20,
             [72,10,19,100,33,75,3,81,24,22,87,63,9,7,40,8,34,101,60,28,23,57,65,90,56,69,77,52,71,74,15,25,41,17,76,95,58,38,29,68,4,89,55,99,13,92,98,62,36,59,54,48,53,12,11,6,2,35,46,39,20,27,44,78,82,67,91,64,97,43,84,83,70,73,79,88,16,1,96,66,80]) and
        true.

test(List, N, Expected) ->
    fbcommon:test({List, N}, Expected, rotate(List, N)).


rotate(List, 0) ->
    List;
rotate(List, N) ->
    Length = length(List),
    rotate(List, (Length - (N rem Length)) rem Length, []).

rotate(Head, 0, Tail) ->
    Head ++ lists:reverse(Tail);
rotate([I | Head], N, Tail) ->
    rotate(Head, N - 1, [I | Tail]).
