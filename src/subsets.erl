%% Build a function that given a list of Integers in the range [-65000,65000],
%% the function returns true if any subset of the list summed is equal to zero.
%% False otherwise.
%% Example:
%% - [0, 1, 2, -3] returns true. As 1+2+(-3)==0.
%% - [1, 2, 3, -8] returns false. As no subset summed is equal 0.
%% - [1, 4, 5, 2, -3] returns true.
-module(subsets).
-export([find_zero_simple/1, find_zero/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Naive solution
%%
%% complexity = sum[i = 1, N](i!)
%% The only optimization is to sort numbers and not to look after we already
%% have positive sum.
%%
find_zero_simple(List) ->
    try lists:sort(fun less/2, List) of
        Sorted ->
            find_num(0, Sorted)
    catch
        true -> true
    end.

less(A, B) ->
    if A =:= 0; B =:= 0 -> throw(true);
       true -> A < B
    end.


find_num(Num, [Num | _]) ->
    true;
find_num(Num, [I | Rest]) when Num > I ->
    case find_num(Num - I, Rest) of
        true ->
            true;
        _ ->
            find_num(Num, Rest)
    end;
find_num(_, _) ->
    false.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Optimized solution
%%
%% The idea is to split positive and negative items in the start list and find
%% equal subsums in them.
%% Next, it's better to have subsums lists in decreasing order to easier
%% maintain balance between in growing subsum lists. That's why it's better to
%% have input lists sorted. While generating subsums new subsums are checked
%% for membership in another subsum list. Knowing it is sorted helps to optimize
%% there too.
%% When one of the lists is exhausted, another subsum list continue to grow
%% using appropriate list. At the same time maximum value ontop the stopped
%% growing subsum list caps growing another.
%% 
%% I used throw(true) to quickly exit to simplify functions. Performance loss on
%% catch is insignificant with overall algorithm complexity.
%%
%% 
%% This solution uses only 1 core.
%% I could use small optimizations with separate processes:
%%   - to check new items added to subsum list to be members of another
%%     (in find_sum/2).
%%   - to sort different lists in parallel (in find_zero).
%%
%% But it would make code even more unreadable and performance gain would be
%% insignificant.
%%
%% Hope, you'll like it =)
find_zero(List) ->
    case split_fold(List, [], []) of
        true ->
            true;
        {List1, List2} when List1 =:= []; List2 =:= [] ->
            false;
        {List1, List2} ->
            catch find_equals(lists:sort(List1), lists:sort(List2))
    end.


split_fold([N | Rest], NegList, PosList) ->
    if N =:= 0 ->
            true;
       N < 0 ->
            split_fold(Rest, [-N | NegList], PosList);
       true ->
            split_fold(Rest, NegList, [N | PosList])
    end;
split_fold([], NegList, PosList) ->
    {NegList, PosList}.


find_equals([H1 | Tail1], [H2 | Tail2]) ->
    if H1 == H2 ->
            true;
       true ->
            find_equals([H1, 0], Tail1, [H2, 0], Tail2)
    end.


find_equals(Sums1, [H1 | Tail1], [Max2 | _] = Sums2, List2) ->
    [NewMax1 | _] = NewSums1 = subsums_r(Sums1, H1, fun(I) -> find_sum(I, Sums2) end),
    if NewMax1 > Max2 ->
            find_equals(Sums2, List2, NewSums1, Tail1);
       true ->
            find_equals(NewSums1, Tail1, Sums2, List2)
    end;
find_equals([Max1 | _] = Sums1, _, Sums2, List2) ->
    find_equals(Sums1, lists:dropwhile(fun(I) -> I >= Max1 end, Sums2), List2).


find_equals([Max1 | _] = Sums1, Sums2, [H2 | Tail2]) ->
    NewSums2 = lists:dropwhile(fun(I) -> I >= Max1 end,
                               subsums_r(Sums2, H2, fun(I) -> find_sum(I, Sums1) end)),
    find_equals(Sums1, NewSums2, Tail2);
find_equals(_, _, _) ->
    false.


find_sum(I, [Max | Tail] = Sums) ->
    if I > Max ->
            fun(N) -> find_sum(N, Sums) end;
       I < Max ->
            find_sum(I, Tail);
       true ->
            throw(true)
    end.


subsums_r([S0, S1 | STail], N, Fun) ->
    Current = S0 + N,
    [Current | subsums_r(STail, N, S1 + N, [S1], [S0], Fun(Current))];
subsums_r([S0], N, Fun) ->
    Current = S0 + N,
    Fun(Current),
    [Current, S0];
subsums_r([], _N, _Fun) ->
    [].

-compile({inline, [subsums_get_current/5]}).

subsums_get_current([S0 | STail], N, R, F, Fun) ->
    subsums_r(STail, N, S0 + N, [S0 | R], F, Fun);
subsums_get_current([], _, R, F, _) ->
    F ++ lists:reverse(R).


subsums_r(Subsums, N, Current, R, [F0 | FTail] = F, Fun) ->
    if Current > F0 ->
            [Current | subsums_get_current(Subsums, N, R, F, Fun(Current))];
       Current < F0 ->
            [F0 | subsums_r(Subsums, N, Current, R, FTail, Fun)];
       true ->
            [F0 | subsums_get_current(Subsums, N, R, FTail, Fun)]
    end;
subsums_r(Subsums, N, Current, R, _, Fun) ->
    subsums_r(Subsums, N, Current, [], lists:reverse(R), Fun).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Tests
%%
-include_lib("eunit/include/eunit.hrl").
-ifdef(TEST).

find_zero_simple_test() ->
    ?assertEqual(true, find_zero_simple([0, 1, 2, -3])),
    ?assertEqual(false, find_zero_simple([1, 2, 3, -8])),
    ?assertEqual(true, find_zero_simple([1, 4, 5, 2, -3])),
    ok.

subsums_4_test() ->
    ?assertEqual(lists:usort([2, 3, 5, 7] ++ [I + 1 || I <- [2, 3, 5, 7]] ++ [1]),
                 subsums([2, 3, 5, 7], 1, [1], [], undefined)),
    ?assertEqual(lists:usort([2, 5, 8, 100] ++ [I + 2 || I <- [2, 5, 8, 100]] ++ [1]),
                 subsums([2, 5, 8, 100], 2, [1], [], undefined)),
    ok.

subsums_test() ->
    ?assertEqual(
       [1, 2, 3, 4, 5, 6],
       subsums([1, 2, 3], 6)),
    ?assertEqual(
       [2, 4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24],
       subsums([2, 4, 8, 16], 25)),
    ok.

do_nothing(_) ->
    fun do_nothing/1.

subsums_r_test() ->
    ?assertEqual(true, find_zero([0, 1, 2, -3])),
    ?assertEqual(false, find_zero([1, 2, 3, -8])),
    ?assertEqual(true, find_zero([1, 4, 5, 2, -3])),
    L = lists:seq(30, 1, -1),
    ?assertEqual([31 | L], subsums_r(L, 1, fun do_nothing/1)),
    ok.

subsums_r_test_() ->
    {timeout, 600,
     [begin
          L = lists:usort(random_list(100, 0, 100)),
          N = random:uniform(100),
          ?_test(?assertEqual(
                    lists:reverse(subsums(L, N, [], [], undefined)),
                    subsums_r(lists:reverse(L), N, fun do_nothing/1)))
      end || _ <- lists:seq(1, 100)]}.

find_zero_test() ->
    ?assertEqual(true, catch find_zero([0, 1, 2, -3])),
    ?assertEqual(false, catch find_zero([1, 2, 3, -8])),
    ?assertEqual(true, catch find_zero([1, 4, 5, 2, -3])),
    ok.

find_zero_test_() ->
    [begin
         L = random_list(20, -100, 100),
         {timeout, 600,
          ?_test(?assertEqual(find_zero_simple(L),
                              find_zero(L)))}
     end || _ <- lists:seq(1, 10)].

random_list(Amount, Min, Max) ->
    random:seed(erlang:now()),
    M = Max - Min + 1,
    [random:uniform(M) + Min - 1 || _ <- lists:seq(1, Amount)].

performance_test_() ->
    List = random_list(320, -65000, 65000),
    L = [-65000, 64999] ++ [2 || _ <- lists:seq(1, 20)],
    [
     {inparallel, [{timeout, 60, ?_test(performance_check(L, find_zero))},
                   {timeout, 60, ?_test(performance_check(L, find_zero_simple))}
                  ]},
     {inparallel, [{timeout, 600, ?_test(performance_check(List, find_zero))},
                   {timeout, 600, ?_test(performance_check(List, find_zero_simple))}
                  ]}].

performance_check(List, Fun) ->
    ?debugFmt("starting ~p", [Fun]),
    {T0, R0} = timer:tc(?MODULE, Fun, [List]),
    ?debugFmt("~p ~p ~p", [Fun, T0, R0]),
    ok.

subsums(List, Max) ->
    subsums([], List, Max).

subsums(Subsums, [N | Next], Max) ->
    subsums(subsums(Subsums, N, [N], [], Max), Next, Max);
subsums(Subsums, [], _) ->
    Subsums.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% subsums(SortedSubsums, NewValue, [NewValue], [], MaxValue)
%% Simple but ineffective function to produce subsums of given SortedSubsums and
%% parameter NewValue. SortedSubsums are sorted in increasing order, so do the
%% result.
%% 
%% Items in SortedSubsums MUST be not more than MaxValue, and the result will
%% have no values more than MaxValue.
%%
%% Function is equal to
%% lists:usort([I || I <-- SortedSubsums ++ [I + NewValue || I <- SortedSubsums]
%%                         ++ [NewValue]], I <- MaxValue])
%% but is of O(N) complexity.
subsums([], _, F, R, _) ->
    F ++ lists:reverse(R);
subsums([Min | Next] = Subsums, N, [F0 | FTail] = F, R, Max) ->
    if F0 == Min ->
            subsums(Subsums, N, FTail, R, Max);
       F0 < Min ->
            [F0 | subsums(Subsums, N, FTail, R, Max)];
       true ->
            case N + Min of
                V when V =< Max ->
                    [Min | subsums(Next, N, F, [V | R], Max)];
                _ ->
                    [Min | subsums(Next, N, F, R, Max)]
            end
    end;
subsums([Min | Next], N, [], [], Max) ->
    case N + Min of
        V when V =< Max ->
            [Min | subsums(Next, N, [N + Min], [], Max)];
        _ ->
            [Min]
    end;
subsums(Subsums, N, [], R, Max) ->
    subsums(Subsums, N, lists:reverse(R), [], Max).

-endif.
