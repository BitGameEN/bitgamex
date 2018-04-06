%%%--------------------------------------
%%% @Module  : bg
%%% @Description:  bitgame服务器开启
%%%--------------------------------------
-module(bg).
-export([gatesvr_start/0, gatesvr_stop/0]).
-export([gamesvr_start/0, gamesvr_stop/0]).
-export([xchgsvr_start/0, xchgsvr_stop/0]).
-export([info/0, topN_bin/1]).
-export([env/0]).

-define(GATESVR_APPS, [sasl, emysql, gatesvr]).
-define(GAMESVR_APPS, [sasl, emysql, gamesvr]).
-define(XCHGSVR_APPS, [sasl, emysql, xchgsvr, ibrowse]).

-include("common.hrl").
-include("gameConfigPlatform.hrl").


%% 获取环境（注意：目前只支持gatesvr和gamesvr）
%% 据此区分不同的逻辑分支，如下：
%% case bg:env() of
%%     ?ENV_DEV -> ...
%%     ?ENV_BETA -> ...
%%     ?ENV_STAGE -> ...
%%     ?ENV_PROD -> ...
%%     _ -> ...
%% end
env() ->
    case application:get_env(gamesvr, env) of
        {ok, Env} -> Env;
        undefined ->
            case application:get_env(gatesvr, env) of
                {ok, Env} -> Env;
                undefined -> ?ENV_PROD
            end
    end.

%%启动网关服
gatesvr_start() ->
    try
        ok = application:start(crypto),
        % ssl
        ok = application:start(asn1),
        ok = application:start(public_key),
        ok = application:start(ssl),
        % ssl-end
        ok = lager:start(),
        ok = egeoip:start(),
        ok = application:start(ranch),
        ok = application:start(cowlib),
        ok = application:start(cowboy),
        ok = start_applications(?GATESVR_APPS),
        {ok, _} = recon_web:start()
    after
        timer:sleep(100)
    end.

%%停止网关服
gatesvr_stop() ->
    try
        cowboy:stop_listener(https_listener), % 停止监听
        wait(?SVRTYPE_GATE),
        ok = stop_applications(?GATESVR_APPS)
    after
        init:stop() % 能停掉heart
    end.

%%启动游戏服
gamesvr_start() ->
    try
        ok = application:start(crypto),
        ok = lager:start(),
        ok = timer2:start(),
        ok = start_applications(?GAMESVR_APPS),
        {ok, _} = recon_web:start()
        %ok = erlcloud:start()
    after
        timer:sleep(100)
    end.

%%停止游戏服
gamesvr_stop() ->
    try
        wait(?SVRTYPE_GAME),
        ok = stop_applications(?GAMESVR_APPS),
        ok = timer2:stop()
    after
        init:stop() % 能停掉heart
    end.

%%启动转账服
xchgsvr_start() ->
    try
        ok = application:start(crypto),
        % ssl
        ok = application:start(asn1),
        ok = application:start(public_key),
        ok = application:start(ssl),
        % ssl-end
        ok = lager:start(),
        ok = application:start(ranch),
        ok = application:start(cowlib),
        ok = application:start(cowboy),
        ok = start_applications(?XCHGSVR_APPS),
        {ok, _} = recon_web:start()
    after
        timer:sleep(100)
    end.

%%停止转账服
xchgsvr_stop() ->
    try
        cowboy:stop_listener(https_listener), % 停止监听
        wait(?SVRTYPE_XCHG),
        ok = stop_applications(?XCHGSVR_APPS)
    after
        init:stop() % 能停掉heart
    end.

wait(SvrType) ->
    wait(0, 2, SvrType).

% Count：空闲状态（没有数据库操作）的计数；CountMax：连续CountMax次空闲状态，这时候就可以结束了
wait(Count, CountMax, SvrType) ->
    timer:sleep(5000),
    DBBusyNum = get_db_busy_num(SvrType),
    util:launch_log("db busy num = ~w~n", [DBBusyNum]),
    case DBBusyNum > 0 of
        true ->
            wait(0, CountMax, SvrType);
        false ->
            NewCount = Count + 1,
            case NewCount < CountMax of
                true ->
                    wait(NewCount, CountMax, SvrType);
                false ->
                    util:launch_log("~nyou can close safely now~n~n", []),
                    void
            end
    end.

get_db_busy_num(SvrType) ->
    case SvrType of
        ?SVRTYPE_GAME ->
            emysql_conn_mgr:get_wait_num(?DB_RUN) + emysql_conn_mgr:get_lock_num(?DB_RUN);
        _ -> 0
    end + emysql_conn_mgr:get_wait_num(?DB_LOG) + emysql_conn_mgr:get_lock_num(?DB_LOG) +
        emysql_conn_mgr:get_wait_num(?DB_USR) + emysql_conn_mgr:get_lock_num(?DB_USR) +
        emysql_conn_mgr:get_overflow_num().

info() ->
    SchedId      = erlang:system_info(scheduler_id),
    SchedNum     = erlang:system_info(schedulers),
    ProcCount    = erlang:system_info(process_count),
    ProcLimit    = erlang:system_info(process_limit),
    ProcMemUsed  = erlang:memory(processes_used),
    ProcMemAlloc = erlang:memory(processes),
    MemTotal     = erlang:memory(total),
    ?INFO( "erlang node sys info:
                       ~n   Scheduler id:                         ~p
                       ~n   Num scheduler:                        ~p
                       ~n   Process count:                        ~p
                       ~n   Process limit:                        ~p
                       ~n   Memory used by erlang processes:      ~p
                       ~n   Memory allocated by erlang processes: ~p
                       ~n   The total amount of memory allocated: ~p
                       ~n",
                            [SchedId, SchedNum, ProcCount, ProcLimit,
                             ProcMemUsed, ProcMemAlloc, MemTotal]),
      ok.

%% 查找前N个binary内存占用大户
topN_bin(N)->
        [{M, P, process_info(P, [registered_name, initial_call, current_function, dictionary]), B} ||
        {P, M, B} <- lists:sublist(lists:reverse(lists:keysort(2,processes_sorted_by_binary())),N)].
 
processes_sorted_by_binary()->
     [case process_info(P, binary) of
              {_, Bins} ->
                 SortedBins = lists:usort(Bins),
                 {_, Sizes, _} = lists:unzip3(SortedBins),
                 {P, lists:sum(Sizes), []};
              _ ->
                {P, 0, []}
         end ||P <- processes()].

%%############辅助调用函数##############
manage_applications(Iterate, Do, Undo, SkipError, ErrorTag, Apps) ->
    Iterate(fun (App, Acc) ->
                    case Do(App) of
                        ok -> [App | Acc];%合拢
                        {error, {SkipError, _}} -> Acc;
                        {error, Reason} ->
                            [Undo(One) || One <- Acc],
                            throw({error, {ErrorTag, App, Reason}})
                    end
            end, [], Apps),
    ok.

start_applications(Apps) ->
    manage_applications(fun lists:foldl/3,
                        fun application:start/1,
                        fun application:stop/1,
                        already_started,
                        cannot_start_application,
                        Apps).

stop_applications(Apps) ->
    manage_applications(fun lists:foldr/3,
                        fun application:stop/1,
                        fun application:start/1,
                        not_started,
                        cannot_stop_application,
                        Apps).
