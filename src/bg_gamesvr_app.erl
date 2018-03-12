%%%-----------------------------------
%%% @Module  : bg_gamesvr_app
%%% @Description: 游戏服
%%%-----------------------------------
-module(bg_gamesvr_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("common.hrl").
-include("record.hrl").

start(normal, []) ->
    lib_global_data:init(),
    lib_global_data:write_mem_only(<<"launch_start_time">>, util:unixtime()),
    util:launch_log("====> game server launching...Please wait...~n", []),
    %%从启动参数-extra获取参数（节点，端口，服务器id）
    [Ip, ServerId] = init:get_plain_arguments(),
    util:launch_log("====> init db connection pool...~n", []),
    ok = init_db(),
    {ok, SupPid} = bg_gamesvr_sup:start_link(),
    ok = init_cache(),
    bg_gamesvr:start([list_to_binary(Ip), list_to_integer(ServerId)]),
    {ok, SupPid}.

stop(_State) ->
    void.

%% 数据库连接初始化
init_db() ->
    CfgDBName = case application:get_env(db_cfg_name) of
        {ok, CName} -> CName;
        undefined -> ?DB_CFG_NAME
    end,
    CfgDBHost = case application:get_env(db_cfg_host) of
        {ok, CHost} -> CHost;
        undefined -> ?DB_CFG_HOST
    end,
    CfgDBPass = case application:get_env(db_cfg_pass) of
        {ok, CPass} -> CPass;
        undefined -> ?DB_CFG_PASS
    end,
    CfgDBPoolSize = case application:get_env(db_cfg_poolsize) of
        {ok, CPoolSize} -> CPoolSize;
        undefined -> 1
    end,
    UsrDBName = case application:get_env(db_usr_name) of
        {ok, UName} -> UName;
        undefined -> ?DB_USR_NAME
    end,
    UsrDBHost = case application:get_env(db_usr_host) of
        {ok, UHost} -> UHost;
        undefined -> ?DB_USR_HOST
    end,
    UsrDBPass = case application:get_env(db_usr_pass) of
        {ok, UPass} -> UPass;
        undefined -> ?DB_USR_PASS
    end,
    UsrDBPoolSize = case application:get_env(db_usr_poolsize) of
        {ok, UPoolSize} -> UPoolSize;
        undefined -> 10
    end,
    RunDBName = case application:get_env(db_run_name) of
        {ok, RName} -> RName;
        undefined -> ?DB_RUN_NAME
    end,
    RunDBHost = case application:get_env(db_run_host) of
        {ok, RHost} -> RHost;
        undefined -> ?DB_RUN_HOST
    end,
    RunDBPass = case application:get_env(db_run_pass) of
        {ok, RPass} -> RPass;
        undefined -> ?DB_RUN_PASS
    end,
    RunDBPoolSize = case application:get_env(db_run_poolsize) of
        {ok, RPoolSize} -> RPoolSize;
        undefined -> 10
    end,
    LogDBName = case application:get_env(db_log_name) of
        {ok, LName} -> LName;
        undefined -> ?DB_LOG_NAME
    end,
    LogDBHost = case application:get_env(db_log_host) of
        {ok, LHost} -> LHost;
        undefined -> ?DB_LOG_HOST
    end,
    LogDBPass = case application:get_env(db_log_pass) of
        {ok, LPass} -> LPass;
        undefined -> ?DB_LOG_PASS
    end,
    LogDBPoolSize = case application:get_env(db_log_poolsize) of
        {ok, LPoolSize} -> LPoolSize;
        undefined -> 10
    end,

    util:launch_log("db_cfg: name=~p, host=~p, pass=~p, poolsize=~p~n", [CfgDBName, CfgDBHost, CfgDBPass, CfgDBPoolSize]),
    util:launch_log("db_usr: name=~p, host=~p, pass=~p, poolsize=~p~n", [UsrDBName, UsrDBHost, UsrDBPass, UsrDBPoolSize]),
    util:launch_log("db_run: name=~p, host=~p, pass=~p, poolsize=~p~n", [RunDBName, RunDBHost, RunDBPass, RunDBPoolSize]),
    util:launch_log("db_log: name=~p, host=~p, pass=~p, poolsize=~p~n", [LogDBName, LogDBHost, LogDBPass, LogDBPoolSize]),
    emysql:add_pool(?DB_CFG, CfgDBPoolSize, ?DB_CFG_USER, CfgDBPass, CfgDBHost, ?DB_CFG_PORT, CfgDBName, ?DB_ENCODE),
    emysql:add_pool(?DB_USR, UsrDBPoolSize, ?DB_USR_USER, UsrDBPass, UsrDBHost, ?DB_USR_PORT, UsrDBName, ?DB_ENCODE),
    emysql:add_pool(?DB_RUN, RunDBPoolSize, ?DB_RUN_USER, RunDBPass, RunDBHost, ?DB_RUN_PORT, RunDBName, ?DB_ENCODE),
    emysql:add_pool(?DB_LOG, LogDBPoolSize, ?DB_LOG_USER, LogDBPass, LogDBHost, ?DB_LOG_PORT, LogDBName, ?DB_ENCODE),
    ok.

%% 缓存连接初始化
init_cache() ->
    ConnPoolSize = case application:get_env(cache_conn_pool_size) of
        {ok, CacheConnPoolSize} -> CacheConnPoolSize;
        undefined -> 10
    end,
    Host = case application:get_env(cache_host) of
        {ok, CacheHost} -> CacheHost;
        undefined -> ?CACHE_HOST
    end,
    util:launch_log("couchbase(as cache): host=~p, poolsize=~p~n", [Host, ConnPoolSize]),
    {ok, _} = supervisor:start_child(
                bg_gamesvr_sup,
                {cache,
                {cache, start_link,[ConnPoolSize, Host]},
                permanent, 10000, supervisor, [cache]}),
    ok.

