%% Build a function that given a list of Integers pairs in the range
%% [-65000,65000], that represents {X, Y} Coordinates in a Plane. The function
%% returns the two closest pairs.
%% Example:
%% [{0,0}, {1,20}, {5, 2}] returns [{0,0}, {5, 2}]
%% [{-10,10}, {1,5}, {4, 3}] returns [{1,5}, {4, 3}]
-module(points).
-export([find_closest/1, find_closest_simple/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Optimized solution
%%
%% Base complexity is O(N2)
%% Sorting list is O(N log N) and it allows to check only those points that have
%% X-distance not more that current minimal distance.
%%
%% Not using square root allows use integer arithmetics which is much quicker
%% than floats and is exact in precision.
find_closest(Points) ->
    [{X0, Y0} = P0 | [P1 | Rest1] = Rest0] = lists:sort(Points),
    D2 = distance(P0, P1),
    find_closest(Rest1, X0, Y0, D2, Rest0, [P0, P1]).


find_closest([{X1, Y1} = P | Rest], X0, Y0, D2, List, R) ->
    X = X0 - X1,
    X2 = X * X,
    if X2 >= D2 ->
            get_next(List, D2, R);
       true ->
            Y = Y0 - Y1,
            N2 = Y * Y + X2,
            if N2 >= D2 ->
                    find_closest(Rest, X0, Y0, D2, List, R);
               true ->
                    find_closest(Rest, X0, Y0, N2, List, [{X0, Y0}, P])
            end
    end;
find_closest(_, _, _, D2, List, R) ->
    get_next(List, D2, R).


-compile({inline, [get_next/3]}).

get_next([{X, Y} | Rest], D2, R) ->
    find_closest(Rest, X, Y, D2, Rest, R);
get_next(_, _, R) ->
    R.


distance({X0, Y0}, {X1, Y1}) ->
    X = X0 - X1,
    Y = Y0 - Y1,
    X * X + Y * Y.


find_closest_simple([P0 | [P1 | Points1] = Points0]) ->
    find_closest_simple(Points1, P0, distance(P0, P1), Points0, [P0, P1]).

find_closest_simple([P1 | Rest], P0, D2, Points, R) ->
    case distance(P0, P1) of
        N2 when N2 < D2 ->
            find_closest_simple(Rest, P0, N2, Points, [P0, P1]);
        _ ->
            find_closest_simple(Rest, P0, D2, Points, R)
    end;
find_closest_simple(_, _, D2, [P | Points], R) ->
    find_closest_simple(Points, P, D2, Points, R);
find_closest_simple(_, _, _, _, R) ->
    R.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

find_closest_test() ->
    ?assertEqual([{0,0}, {5, 2}],
                 find_closest([{0,0}, {1,20}, {5, 2}])),
    ?assertEqual([{1,5}, {4, 3}],
                 find_closest([{-10,10}, {1,5}, {4, 3}])),
    ok.

find_closest_simple_test() ->
    ?assertEqual([{0,0}, {5, 2}],
                 find_closest_simple([{0,0}, {1,20}, {5, 2}])),
    ?assertEqual([{1,5}, {4, 3}],
                 find_closest_simple([{-10,10}, {1,5}, {4, 3}])),
    ok.

performance_test_() ->
    [{timeout, 600, ?_test(performance_check())}
     || _ <- lists:seq(1, 10)].

performance_check() ->
    List = random_points(1000, -65000, 65000),
    {T0, R0} = timer:tc(?MODULE, find_closest, [List]),
    ?debugFmt("find_closest        ~p", [T0]),
    {T1, R1} = timer:tc(?MODULE, find_closest_simple, [List]),
    ?debugFmt("find_closest_simple ~p", [T1]),
    case dist_equal(R0, R1) of
        true -> ?debugMsg("---------------------------equal");
        _ -> ?debugFmt("************************** Not equal:\n~p\n~p", [R0, R1])
    end,
    ok.

dist_equal(R, R) ->
    true;
dist_equal([P1, P2], [P3, P4]) ->
    distance(P1, P2) == distance(P3, P4).

random_points(N, Min, Max) ->
    random:seed(erlang:now()),
    M = Max - Min + 1,
    lists:usort([{random:uniform(M) + Min - 1, random:uniform(M) + Min - 1}
                 || _ <- lists:seq(1, N)]).

-endif.
