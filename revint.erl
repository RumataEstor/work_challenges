%% https://codelab.interviewbit.com/problems/revint/
-module(revint).

-export([start/0]).

start() ->
    true and
        test(123, 321) and
        test(-123, -321) and
        test(2147483647, 0) and
        test(2147483412, 2143847412) and
        test(2147447412, 2147447412) and
        test(2147457412, 0) and
        true.

test(Num, Expected) ->
    fbcommon:test(Num, Expected, rev(Num)).


rev(Num) ->
    {Pos, Max, Sign} = if Num >= 0 -> {Num, 16#7fffffff, 1};
                          Num < 0 -> {-Num, 16#80000000, -1}
                       end,
    rev(Pos, Sign, 0, Max, Max div 10).

rev(0, Sign, Result, _Max, _Max10) ->
    Result * Sign;
rev(_N, _Sign, Result, _Max, Max10) when Result > Max10 ->
    0;
rev(Num, Sign, Result, Max, Max10) ->
    Digit = Num rem 10,
    case Result * 10 of
        Result10 when Result10 > Max - Digit ->
            0;
        Result10 ->
            rev(Num div 10, Sign, Result10 + Digit, Max, Max10)
    end.
