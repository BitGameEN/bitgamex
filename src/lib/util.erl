%%%-----------------------------------
%%% @Module  : util
%%% @Description: 公共函数
%%%-----------------------------------

-include("common.hrl").

-module(util).
-export([
        launch_log/2,
        zero_if_negative/1,
        now_datetime/0,
        now_datetime_str/0,
        unixtime/0,
        longunixtime/0,
        md5/1,
        rand/2,
        clamp/3,
        ceil/1,
        floor/1,
        binary_to_float/1,
        f2s/1,
        implode/2,
        implode/3,
        explode/2,
        explode/3,
        string_to_term/1,
        bitstring_to_term/1,
        term_to_string/1,
        term_to_bitstring/1,
        term_to_bitstring_p/1,
        lookup_one/2,
        lookup_one/3,
        lookup_all/2,
        lookup_all/3,
        match_one/2,
        match_all/2,
        url_encode/1,
        url_decode/1,
        html_unescape/1,
        is_process_alive/1,
        upper_1st_char/1,
        esc/1,
        contains/2,
        trim/1,
        device_model/1,
        get_country_code/1
	  ]
).

-include("egeoip.hrl").


launch_log(FormatStr, Args) ->
    ?INFO(FormatStr, Args),
    Logs = lib_global_data:read_mem_only(<<"launch_logs">>, ""),
    Now = util:unixtime(),
    StartTime = lib_global_data:read_mem_only(<<"launch_start_time">>, Now),
    ElapsedSeconds = Now - StartTime,
    NewLogs = Logs ++ "(" ++ integer_to_list(ElapsedSeconds) ++ ")" ++ lists:flatten(io_lib:format(FormatStr, Args)),
    lib_global_data:write_mem_only(<<"launch_logs">>, NewLogs),
    ok.

%% 如果为负，则取0
zero_if_negative(Num) ->
    if
        Num < 0 ->
            0;
        true ->
            Num
    end.

%% UTC-0点的datetime
now_datetime() ->
    erlang:localtime_to_universaltime(erlang:localtime()).

%% "2016-01-01 00:00:00"
now_datetime_str() ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:localtime_to_universaltime(erlang:localtime()),
    Res = io_lib:format("~4.4.0w-~2.2.0w-~2.2.0w ~2.2.0w:~2.2.0w:~2.2.0w", [Year, Month, Day, Hour, Minute, Second]),
    lists:flatten(Res).

%% 取得当前的unix时间戳（单位为秒）
unixtime() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.

%% 取得当前的unix时间戳（单位为毫秒）
longunixtime() ->
    {M, S, Ms} = os:timestamp(),
    M * 1000000000 + S*1000 + Ms div 1000.

%% 转换成HEX格式的md5
md5(S) ->
    lists:flatten([io_lib:format("~2.16.0b", [N]) || N <- binary_to_list(erlang:md5(S))]).

%% 产生一个介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(Min, Max) when Min > Max -> 0;
rand(Min, Max) ->
    %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
    case get(random_seed) of
        undefined ->
            RandSeed = mod_rand:get_seed(),
            rand:seed(exsplus, RandSeed),
            put(random_seed, RandSeed);
        _ -> skip
    end,
    M = Min - 1,
    rand:uniform(Max - M) + M.

% 将val钳制在min和max之间
clamp(Min, Max, Val) ->
    case Val < Min of
        true -> Min;
        false ->
            case Val > Max of
                true -> Max;
                false -> Val
            end
    end.

%%向上取整
ceil(N) ->
    T = trunc(N),
    case N == T of
        true  -> T;
        false -> 1 + T
    end.

%%向下取整
floor(X) ->
    T = trunc(X),
    case (X < T) of
        true -> T - 1;
        _ -> T
    end.

binary_to_float(Bin) ->
    case catch erlang:binary_to_float(Bin) of
        {'EXIT',{badarg,_}} ->
            binary_to_integer(Bin);
        Float -> Float
    end.

%% convert float to string
f2s(N) when is_integer(N) ->
    list_to_binary(integer_to_list(N) ++ ".00000000");
f2s(F) when is_float(F) ->
    [A] = io_lib:format("~.8f", [F]),
    list_to_binary(A).

%% 在List中的每两个元素之间插入一个分隔符
implode(_S, []) ->
    [<<>>];
implode(S, L) when is_list(L) ->
    implode(S, L, []).
implode(_S, [H], NList) ->
    lists:reverse([thing_to_list(H) | NList]);
implode(S, [H | T], NList) ->
    L = [thing_to_list(H) | NList],
    implode(S, T, [S | L]).

thing_to_list(X) when is_integer(X) -> integer_to_list(X);
thing_to_list(X) when is_float(X)   -> float_to_list(X);
thing_to_list(X) when is_atom(X)    -> atom_to_list(X);
thing_to_list(X) when is_binary(X)  -> binary_to_list(X);
thing_to_list(X) when is_list(X)    -> X.

%% 以分隔符拆分字符串
explode(S, B) ->
    re:split(B, S, [{return, list}]).
explode(S, B, int) ->
    [list_to_integer(Str) || Str <- explode(S, B), length(Str) > 0].

%% term反序列化，string转换为term，e.g., "[{a},1]"  => [{a},1]
string_to_term(String) ->
    %%{ok, S} = file:open("string_to_term.txt", [append]),
    %%io:format(S, "~p~n", [String]),
    %%file:close(S),
    case erl_scan:string(String++".") of
        {ok, Tokens, _} ->
            case erl_parse:parse_term(Tokens) of
                {ok, Term} -> Term;
                _Err ->
                    io:format("~nerl_parse wrong:==>~n~ts~n~p~n<==~n", [String, erlang:process_info(self(), current_stacktrace)]),
                    undefined
            end;
        _Error ->
            io:format("~nerl_scan wrong:==>~n~ts~n~p~n<==~n", [String, erlang:process_info(self(), current_stacktrace)]),
            undefined
    end.

%% term反序列化，bitstring转换为term，e.g., <<"[{a},1]">>  => [{a},1]
bitstring_to_term(<<>>) -> undefined;
bitstring_to_term(undefined) -> undefined;
bitstring_to_term(<<$<, Char:8, _/binary>>) when Char =/= $< -> undefined; % 使pid无效
bitstring_to_term(BitString) ->
    case binary:match(BitString, [<<"#Ref<">>], []) of
        nomatch -> string_to_term(binary_to_list(BitString));
        _ -> undefined
    end.

%% term序列化，term转换为string格式，e.g., [{a},1] => "[{a},1]"
term_to_string(Term) ->
    binary_to_list(list_to_binary(io_lib:format("~p", [Term]))).

%% term序列化，term转换为bitstring格式，e.g., [{a},1] => <<"[{a},1]">>
%%当列表中的值全小于255时，~p会显示字母，所以用~w，同时有个好处，对于字符串中含有单引号等字符时，不会导致sql语法错误
term_to_bitstring(Term) ->
    erlang:list_to_bitstring(io_lib:format("~w", [Term])).

term_to_bitstring_p(Term) ->
    erlang:list_to_bitstring(io_lib:format("~p", [Term])).

lookup_one(Table, Key) ->
    lookup_one(Table, Key, false).

lookup_one(Table, Key, false) ->
    case ets:lookup(Table, Key) of
        [] -> [];
        [R] -> R
    end;
lookup_one(Table, Key, true) ->
    case mnesia:dirty_read(Table, Key) of
        [] -> [];
        [R] -> R
    end.

lookup_all(Table, Key) ->
    lookup_all(Table, Key, false).

lookup_all(Table, Key, false) ->
    ets:lookup(Table, Key);
lookup_all(Table, Key, true) ->
    mnesia:dirty_read(Table, Key).

match_one(Table, Pattern) ->
    case ets:match_object(Table, Pattern) of
        [] -> [];
        [R] -> R
    end.

match_all(Table, Pattern) ->
    ets:match_object(Table, Pattern).

%% url编码
url_encode(B) when is_list(B) ->
    binary_to_list(url_encode(list_to_binary(B), <<>>));
url_encode(B) when is_binary(B) ->
    url_encode(B, <<>>).

url_encode(<< $\s, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $+ >>);
url_encode(<< $-, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $- >>);
url_encode(<< $., Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $. >>);
url_encode(<< $0, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $0 >>);
url_encode(<< $1, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $1 >>);
url_encode(<< $2, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $2 >>);
url_encode(<< $3, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $3 >>);
url_encode(<< $4, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $4 >>);
url_encode(<< $5, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $5 >>);
url_encode(<< $6, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $6 >>);
url_encode(<< $7, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $7 >>);
url_encode(<< $8, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $8 >>);
url_encode(<< $9, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $9 >>);
url_encode(<< $A, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $A >>);
url_encode(<< $B, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $B >>);
url_encode(<< $C, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $C >>);
url_encode(<< $D, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $D >>);
url_encode(<< $E, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $E >>);
url_encode(<< $F, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $F >>);
url_encode(<< $G, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $G >>);
url_encode(<< $H, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $H >>);
url_encode(<< $I, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $I >>);
url_encode(<< $J, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $J >>);
url_encode(<< $K, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $K >>);
url_encode(<< $L, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $L >>);
url_encode(<< $M, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $M >>);
url_encode(<< $N, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $N >>);
url_encode(<< $O, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $O >>);
url_encode(<< $P, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $P >>);
url_encode(<< $Q, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $Q >>);
url_encode(<< $R, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $R >>);
url_encode(<< $S, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $S >>);
url_encode(<< $T, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $T >>);
url_encode(<< $U, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $U >>);
url_encode(<< $V, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $V >>);
url_encode(<< $W, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $W >>);
url_encode(<< $X, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $X >>);
url_encode(<< $Y, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $Y >>);
url_encode(<< $Z, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $Z >>);
url_encode(<< $_, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $_ >>);
url_encode(<< $a, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $a >>);
url_encode(<< $b, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $b >>);
url_encode(<< $c, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $c >>);
url_encode(<< $d, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $d >>);
url_encode(<< $e, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $e >>);
url_encode(<< $f, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $f >>);
url_encode(<< $g, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $g >>);
url_encode(<< $h, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $h >>);
url_encode(<< $i, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $i >>);
url_encode(<< $j, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $j >>);
url_encode(<< $k, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $k >>);
url_encode(<< $l, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $l >>);
url_encode(<< $m, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $m >>);
url_encode(<< $n, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $n >>);
url_encode(<< $o, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $o >>);
url_encode(<< $p, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $p >>);
url_encode(<< $q, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $q >>);
url_encode(<< $r, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $r >>);
url_encode(<< $s, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $s >>);
url_encode(<< $t, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $t >>);
url_encode(<< $u, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $u >>);
url_encode(<< $v, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $v >>);
url_encode(<< $w, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $w >>);
url_encode(<< $x, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $x >>);
url_encode(<< $y, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $y >>);
url_encode(<< $z, Rest/bits >>, Acc) -> url_encode(Rest, << Acc/bits, $z >>);
url_encode(<< C, Rest/bits >>, Acc) ->
    H = hex(C bsr 4),
    L = hex(C band 16#0f),
    url_encode(Rest, << Acc/bits, $%, H, L >>);
url_encode(<<>>, Acc) ->
    Acc.

hex( 0) -> $0;
hex( 1) -> $1;
hex( 2) -> $2;
hex( 3) -> $3;
hex( 4) -> $4;
hex( 5) -> $5;
hex( 6) -> $6;
hex( 7) -> $7;
hex( 8) -> $8;
hex( 9) -> $9;
hex(10) -> $A;
hex(11) -> $B;
hex(12) -> $C;
hex(13) -> $D;
hex(14) -> $E;
hex(15) -> $F.

%% url解码
url_decode(B) when is_list(B) ->
    binary_to_list(url_decode(list_to_binary(B), <<>>));
url_decode(B) when is_binary(B) ->
    url_decode(B, <<>>).

url_decode(<< $%, H, L, Rest/bits >>, Acc) ->
    C = (unhex(H) bsl 4 bor unhex(L)),
    url_decode(Rest, << Acc/bits, C >>);
url_decode(<< $+, Rest/bits >>, Acc) ->
    url_decode(Rest, << Acc/bits, " " >>);
url_decode(<< C, Rest/bits >>, Acc) when C =/= $% ->
    url_decode(Rest, << Acc/bits, C >>);
url_decode(<<>>, Acc) ->
    Acc.

unhex($0) ->  0;
unhex($1) ->  1;
unhex($2) ->  2;
unhex($3) ->  3;
unhex($4) ->  4;
unhex($5) ->  5;
unhex($6) ->  6;
unhex($7) ->  7;
unhex($8) ->  8;
unhex($9) ->  9;
unhex($A) -> 10;
unhex($B) -> 11;
unhex($C) -> 12;
unhex($D) -> 13;
unhex($E) -> 14;
unhex($F) -> 15;
unhex($a) -> 10;
unhex($b) -> 11;
unhex($c) -> 12;
unhex($d) -> 13;
unhex($e) -> 14;
unhex($f) -> 15.

%% html反转义
html_unescape(L) -> html_unescape(L, []).
html_unescape([], Acc) -> lists:reverse(Acc);
html_unescape([$&,$l,$t,$;|T], Acc) -> html_unescape(T, [$<|Acc]);
html_unescape([$&,$g,$t,$;|T], Acc) -> html_unescape(T, [$>|Acc]);
html_unescape([$&,$q,$u,$o,$t,$;|T], Acc) -> html_unescape(T, [$"|Acc]);
html_unescape([H|T], Acc) -> html_unescape(T, [H|Acc]).

is_process_alive(Pid) ->
    try
        case is_pid(Pid) of
            true ->
                case rpc:call(node(Pid), erlang, is_process_alive, [Pid]) of
                    {badrpc, Reason} ->
                        ?ERR("is_process_alive: pid=~p, from_node=~p, to_node=~p, reason=~p~nstack=~p~n", [Pid, node(), node(Pid), Reason, erlang:get_stacktrace()]),
                        false;
                    Res -> Res
                end;
            false -> false
        end
    catch
        _:_ -> false
    end.

upper_1st_char(<<First:8, Rest/binary>>) when First >= $a, First =< $z ->
    <<(First+$A-$a):8, Rest/binary>>;
upper_1st_char(Str) ->
    Str.

esc(BinString) ->
    BinString2 = binary:replace(BinString, <<"'">>, <<"''">>, [global]),
    binary:replace(BinString2, <<"\\">>, <<"\\\\">>, [global]).

contains(String, SearchList) ->
    F = fun(SearchStr) ->
            case re:run(String, SearchStr, [caseless]) of
                {match, _} -> true;
                _ -> false
            end
        end,
    lists:any(F, SearchList).

trim(Bin) when is_binary(Bin) ->
    list_to_binary(trim(binary_to_list(Bin)));
trim(Str) when is_list(Str) ->
    Str1 = string:strip(Str, both, 32), % 空格
    Str2 = string:strip(Str1, both, 9),  % tab
    Str3 = string:strip(Str2, both, 13), % 回车
    Str4 = string:strip(Str3, both, 10),  % 换行
    Str4;
trim(undefined) ->
    <<>>;
trim(Data) ->
    Data.

% 获取设备类型字串
% https://support.hockeyapp.net/kb/client-integration-ios-mac-os-x-tvos/ios-device-types
device_model(<<"iPhone1,1">>) ->   <<"iPhone">>;
device_model(<<"iPhone1,2">>) ->   <<"iPhone 3G">>;
device_model(<<"iPhone2,1">>) ->   <<"iPhone 3GS">>;
device_model(<<"iPhone3,1">>) ->   <<"iPhone 4 (GSM)">>;
device_model(<<"iPhone3,3">>) ->   <<"iPhone 4 (CDMA)">>;
device_model(<<"iPhone4,1">>) ->   <<"iPhone 4S">>;
device_model(<<"iPhone5,1">>) ->   <<"iPhone 5 (A1428)">>;
device_model(<<"iPhone5,2">>) ->   <<"iPhone 5 (A1429)">>;
device_model(<<"iPhone5,3">>) ->   <<"iPhone 5c (A1456/A1532)">>;
device_model(<<"iPhone5,4">>) ->   <<"iPhone 5c (A1507/A1516/A1529)">>;
device_model(<<"iPhone6,1">>) ->   <<"iPhone 5s (A1433/A1453)">>;
device_model(<<"iPhone6,2">>) ->   <<"iPhone 5s (A1457/A1518/A1530)">>;
device_model(<<"iPhone7,1">>) ->   <<"iPhone 6 Plus">>;
device_model(<<"iPhone7,2">>) ->   <<"iPhone 6">>;
device_model(<<"iPhone8,1">>) ->   <<"iPhone 6s">>;
device_model(<<"iPhone8,2">>) ->   <<"iPhone 6s Plus">>;
device_model(<<"iPhone8,4">>) ->   <<"iPhone SE">>;
device_model(<<"iPhone9,1">>) ->   <<"iPhone 7 (A1660/A1779/A1780)">>;
device_model(<<"iPhone9,2">>) ->   <<"iPhone 7 Plus (A1661/A1785/A1786)">>;
device_model(<<"iPhone9,3">>) ->   <<"iPhone 7 (A1778)">>;
device_model(<<"iPhone9,4">>) ->   <<"iPhone 7 Plus (A1784)">>;
device_model(<<"iPad1,1">>) ->     <<"iPad">>;
device_model(<<"iPad2,1">>) ->     <<"iPad 2 (Wi-Fi)">>;
device_model(<<"iPad2,2">>) ->     <<"iPad 2 (GSM)">>;
device_model(<<"iPad2,3">>) ->     <<"iPad 2 (CDMA)">>;
device_model(<<"iPad2,4">>) ->     <<"iPad 2 (Wi-Fi, revised)">>;
device_model(<<"iPad2,5">>) ->     <<"iPad mini (Wi-Fi)">>;
device_model(<<"iPad2,6">>) ->     <<"iPad mini (A1454)">>;
device_model(<<"iPad2,7">>) ->     <<"iPad mini (A1455)">>;
device_model(<<"iPad3,1">>) ->     <<"iPad (3rd gen, Wi-Fi)">>;
device_model(<<"iPad3,2">>) ->     <<"iPad (3rd gen, Wi-Fi+LTE Verizon)">>;
device_model(<<"iPad3,3">>) ->     <<"iPad (3rd gen, Wi-Fi+LTE AT&T)">>;
device_model(<<"iPad3,4">>) ->     <<"iPad (4th gen, Wi-Fi)">>;
device_model(<<"iPad3,5">>) ->     <<"iPad (4th gen, A1459)">>;
device_model(<<"iPad3,6">>) ->     <<"iPad (4th gen, A1460)">>;
device_model(<<"iPad4,1">>) ->     <<"iPad Air (Wi-Fi)">>;
device_model(<<"iPad4,2">>) ->     <<"iPad Air (Wi-Fi+LTE)">>;
device_model(<<"iPad4,3">>) ->     <<"iPad Air (Rev)">>;
device_model(<<"iPad4,4">>) ->     <<"iPad mini 2 (Wi-Fi)">>;
device_model(<<"iPad4,5">>) ->     <<"iPad mini 2 (Wi-Fi+LTE)">>;
device_model(<<"iPad4,6">>) ->     <<"iPad mini 2 (Rev)">>;
device_model(<<"iPad4,7">>) ->     <<"iPad mini 3 (Wi-Fi)">>;
device_model(<<"iPad4,8">>) ->     <<"iPad mini 3 (A1600)">>;
device_model(<<"iPad4,9">>) ->     <<"iPad mini 3 (A1601)">>;
device_model(<<"iPad5,1">>) ->     <<"iPad mini 4 (Wi-Fi)">>;
device_model(<<"iPad5,2">>) ->     <<"iPad mini 4 (Wi-Fi+LTE)">>;
device_model(<<"iPad5,3">>) ->     <<"iPad Air 2 (Wi-Fi)">>;
device_model(<<"iPad5,4">>) ->     <<"iPad Air 2 (Wi-Fi+LTE)">>;
device_model(<<"iPad6,3">>) ->     <<"iPad Pro (9.7 inch) (Wi-Fi)">>;
device_model(<<"iPad6,4">>) ->     <<"iPad Pro (9.7 inch) (Wi-Fi+LTE)">>;
device_model(<<"iPad6,7">>) ->     <<"iPad Pro (12.9 inch, Wi-Fi)">>;
device_model(<<"iPad6,8">>) ->     <<"iPad Pro (12.9 inch, Wi-Fi+LTE)">>;
device_model(<<"iPod1,1">>) ->     <<"iPod touch">>;
device_model(<<"iPod2,1">>) ->     <<"iPod touch (2nd gen)">>;
device_model(<<"iPod3,1">>) ->     <<"iPod touch (3rd gen)">>;
device_model(<<"iPod4,1">>) ->     <<"iPod touch (4th gen)">>;
device_model(<<"iPod5,1">>) ->     <<"iPod touch (5th gen)">>;
device_model(<<"iPod7,1">>) ->     <<"iPod touch (6th gen)">>;
device_model(Model) -> Model.

get_country_code(undefined) ->
    <<>>;
get_country_code(Ip) ->
  try
    case egeoip:ip2long(Ip) of
        {ok, IpAddressLong} ->
            case egeoip:lookup(IpAddressLong) of
                {ok, #geoip{country_code = Code}} -> Code;
                _ -> <<>>
            end;
        _ -> <<>>
    end
  catch
    _:_ -> <<>>
  end.

