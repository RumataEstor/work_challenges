% https://codelab.interviewbit.com/problems/ipaddress/
-module(ipaddress).

-export([start/0]).
-export([get_value/4]).

start() ->
    true and
        test(<<"25525511135">>, [<<"255.255.11.135">>, <<"255.255.111.35">>]) and
        test(<<"0100100">>, [<<"0.10.0.100">>, <<"0.100.10.0">>]) and
        test(<<"99999999">>, [<<"99.99.99.99">>]) and
        test(<<"999999999">>, []) and
        true.

test(String, Expect) ->
    fbcommon:test(String, Expect, run(String)).

run(String) ->
    run(String, byte_size(String), 3, <<>>).


get_suffix(RestParts) ->
    case RestParts of
        0 -> <<>>;
        _ -> <<".">>
    end.


run(String, Size, RestParts, Prefix) ->
    MinLen = max(1, Size - RestParts * 3),
    case String of
        <<"0">> when RestParts == 0 ->
            [<<Prefix/binary, "0">>];
        <<"0", Rest/binary>> when MinLen == 1 ->
            run(Rest, Size - 1, RestParts - 1, <<Prefix/binary, "0.">>);
        <<"0", _/binary>> ->
            [];
        _ ->
            MaxLen = min(Size - RestParts, 3),
            run(String, Size, RestParts, Prefix, MinLen, MaxLen)
    end.

get_value(Rest, 0, V, S) ->
    if V =< 255 ->
            {S, Rest};
       true ->
            false
    end;
get_value(<<C, Rest/binary>>, Len, V, S) ->
    get_value(Rest, Len - 1, V * 10 + C - $0, <<S/binary, C>>).


run(String, Size, RestParts, Prefix, Len, MaxLen) when Len =< MaxLen ->
    case get_value(String, Len, 0, Prefix) of
        false ->
            [];
        {NewPrefix, Rest} when RestParts > 0 ->
            Suffix = get_suffix(RestParts),
            run(Rest, Size - Len, RestParts - 1, <<NewPrefix/binary, Suffix/binary>>);
        {NewPrefix, <<>>} when RestParts == 0 ->
            [NewPrefix];
        _ ->
            []
    end
        ++ run(String, Size, RestParts, Prefix, Len + 1, MaxLen);
run(_, _, _, _, _, _) ->
    [].
