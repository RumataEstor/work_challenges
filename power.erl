%% https://codelab.interviewbit.com/problems/power/
-module(power).

-export([start/0]).

start() ->
    test("1", 0),
    test("2", 1),
    test("3", 0),
    test("4", 1),
    test("6", 0),
    test("1208925819614629174706176", 1),
    test("1208925819614629174706178", 0),
    test("120892581961462917470617812089258196146291747061781208925819614629174706178120892581961462917470617812089258196146291747061781208925819614629174706178", 0),
    test("10715086071862673209484250490600018105614048117055336074437503883703510511249361224931983788156958581275946729175531468251871452856923140435984577574698574803934567774824230985421074605062371141877954182153046474983581941267398767559165543946077062914571196477686542167660429831652624386837205668069376", 1),
    ok.

test(String, Expect) ->
    fbcommon:test(String, Expect, is_power(String)).


is_power([]) -> 0;
is_power([Ch]) ->
    if Ch == $2; Ch == $4; Ch == $8 -> 1;
       true -> 0
    end;
is_power(String) ->
    catch is_power(divide(String, 2, 0, true)).

divide([Ch | Rest], Divisor, Remainder, Starting)
  when Ch >= $0, Ch =< $9 ->
    Number = Remainder * 10 + Ch - $0,
    Divided = Number div Divisor,
    NextRemainder = Number rem Divisor,
    if
        Starting andalso Divided == 0 ->
            divide(Rest, Divisor, NextRemainder, Starting);
        true ->
            [Divided + $0
             | divide(Rest, Divisor, NextRemainder, false)]
    end;
divide([], _, 1, _) ->
    throw(0);
divide([], _, 0, _) ->
    [].
