%%%--------------------------------------
%%% @Module  : lib_ipv4
%%% @Description:ip检查相关
%%%--------------------------------------
-module(lib_ipv4).
-export([verify_ip/2]).

%%检查ip在指定列表中
verify_ip(CheckIp, IPList) ->
    case IPList of
        [] ->
            false;
        _ ->
            F = fun(Ip) ->
                Checks = string:tokens(Ip, "."),
                case Checks =:= CheckIp of
                    true -> true;
                    false -> skip
                end
            end,
            Result = [F(One) || One <- IPList],
            lists:member(true, Result)
    end.

