%%%-----------------------------------
%%% @Module  : bg_gatesvr_app
%%% @Description: 打包程序
%%%-----------------------------------
-module(bg_gatesvr_app).
-behaviour(application).

-export([start/2, stop/1]).

-include("common.hrl").

start(_Type, _Args) ->
    lib_global_data:init(),
    [Ip, Port, ServerId] = init:get_plain_arguments(),
    ok = init_db(),
    {ok, SupPid} = bg_gatesvr_sup:start_link([Ip, list_to_integer(Port), list_to_integer(ServerId)]),
    ok = init_cache(),
    ok = init_idgen(),
    {ok, SupPid}.

stop(_State) ->
    void.

%% 数据库连接初始化
init_db() ->
    UsrDBName = case application:get_env(db_usr_name) of
        {ok, Name} -> Name;
        undefined -> ?DB_USR_NAME
    end,
    UsrDBHost = case application:get_env(db_usr_host) of
        {ok, Host} -> Host;
        undefined -> ?DB_USR_HOST
    end,
    UsrDBPass = case application:get_env(db_usr_pass) of
        {ok, Pass} -> Pass;
        undefined -> ?DB_USR_PASS
    end,
    UsrDBPoolSize = case application:get_env(db_usr_poolsize) of
        {ok, PoolSize} -> PoolSize;
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
    ?INFO("~ndb_log: name=~p, host=~p, pass=~p, poolsize=~p~n", [LogDBName, LogDBHost, LogDBPass, LogDBPoolSize]),
    emysql:add_pool(?DB_USR, UsrDBPoolSize, ?DB_USR_USER, UsrDBPass, UsrDBHost, ?DB_USR_PORT, UsrDBName, ?DB_ENCODE),
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
                bg_gatesvr_sup,
                {cache,
                {cache, start_link, [ConnPoolSize, Host]},
                permanent, 10000, supervisor, [cache]}),
    ok.

%% 唯一id生成
init_idgen() ->
    ConnPoolSize = case application:get_env(cache_conn_pool_size) of
        {ok, CacheConnPoolSize} -> CacheConnPoolSize;
        undefined -> 5
    end,
    Host = case application:get_env(cache_host) of
        {ok, CacheHost} -> CacheHost;
        undefined -> ?CACHE_HOST
    end,
    ?INFO("couchbase(as id_gen): host=~p, poolsize=~p~n", [Host, ConnPoolSize]),
    {ok, _} = supervisor:start_child(
                bg_gatesvr_sup,
                {id_gen,
                {id_gen, start_link, [ConnPoolSize, Host]},
                permanent, 10000, supervisor, [id_gen]}),
    ok.

