%%%-----------------------------------
%%% @Module  : gk_xchgsvr_app
%%% @Description: 打包程序
%%%-----------------------------------
-module(bg_xchgsvr_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("common.hrl").

start(_Type, _Args) ->
    lib_global_data:init(),
    [Ip, ServerId] = init:get_plain_arguments(),
    ok = init_db(),
    {ok, SupPid} = bg_xchgsvr_sup:start_link([Ip, list_to_integer(ServerId)]),
    ok = init_cache(),
    {ok, SupPid}.

stop(_State) ->
    void.

%% 数据库连接初始化
init_db() ->
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

    ?INFO("~ndb_usr: name=~p, host=~p, pass=~p, poolsize=~p~n", [UsrDBName, UsrDBHost, UsrDBPass, UsrDBPoolSize]),
    ?INFO("~ndb_run: name=~p, host=~p, pass=~p, poolsize=~p~n", [RunDBName, RunDBHost, RunDBPass, RunDBPoolSize]),
    ?INFO("~ndb_log: name=~p, host=~p, pass=~p, poolsize=~p~n", [LogDBName, LogDBHost, LogDBPass, LogDBPoolSize]),
	emysql:add_pool(?DB_USR, UsrDBPoolSize, ?DB_USR_USER, UsrDBPass, UsrDBHost, ?DB_USR_PORT, UsrDBName, ?DB_ENCODE),
    emysql:add_pool(?DB_RUN, RunDBPoolSize, ?DB_RUN_USER, RunDBPass, RunDBHost, ?DB_RUN_PORT, RunDBName, ?DB_ENCODE),
    emysql:add_pool(?DB_LOG, LogDBPoolSize, ?DB_LOG_USER, LogDBPass, LogDBHost, ?DB_LOG_PORT, LogDBName, ?DB_ENCODE),
    ok.

%% 缓存连接初始化
init_cache() ->
    ConnPoolSize = case application:get_env(cache_conn_pool_size) of
        {ok, CacheConnPoolSize} -> CacheConnPoolSize;
        undefined -> 5
    end,
    Host = case application:get_env(cache_host) of
        {ok, CacheHost} -> CacheHost;
        undefined -> ?CACHE_HOST
    end,
    ?INFO("couchbase(as cache): host=~p, poolsize=~p~n", [Host, ConnPoolSize]),
    {ok, _} = supervisor:start_child(
                bg_xchgsvr_sup,
                {cache,
                {cache, start_link, [ConnPoolSize, Host]},
                permanent, 10000, supervisor, [cache]}),
    ok.

