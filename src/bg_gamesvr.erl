%%%-----------------------------------
%%% @Module  : bg_gamesvr
%%% @Description: gamesvr启动
%%%-----------------------------------
-module(bg_gamesvr).
-export([start/1]).

-include("common.hrl").

start([Ip, Sid]) ->
    ok = start_disperse([Ip, Sid]),
    ok = start_kernel(),
    ok = start_rand(),
    util:launch_log("<==== game server ready.~n~n~n", []),
    ok.

%%开启服务器自动发现
start_disperse([Ip, Sid]) ->
    util:launch_log("====> start_disperse...~n", []),
    {ok,_} = supervisor:start_child(
                bg_gamesvr_sup,
                {mod_disperse,
                {mod_disperse, start_link, [Ip, 0, Sid, ?SVRTYPE_GAME]},
                permanent, 10000, supervisor, [mod_disperse]}),
    ok.

%%开启核心服务
start_kernel() ->
    util:launch_log("====> start_kernel...~n", []),
    {ok,_} = supervisor:start_child(
                bg_gamesvr_sup,
                {mod_kernel,
                {mod_kernel, start_link, []},
                permanent, 10000, supervisor, [mod_kernel]}),
    ok.

%%随机种子
start_rand() ->
    util:launch_log("====> start_rand...~n", []),
    {ok,_} = supervisor:start_child(
                bg_gamesvr_sup,
                {mod_rand,
                {mod_rand, start_link,[]},
                permanent, 10000, supervisor, [mod_rand]}),
    ok.

