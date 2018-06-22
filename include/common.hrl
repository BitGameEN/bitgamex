%%%------------------------------------------------
%%% File    : common.hrl
%%% Description: 公共定义
%%%------------------------------------------------

% use lager log system: debug, info, notice, warning, error, critical, alert, emergency
-ifdef(debug).
    -define(DEBUG(F, A), lager:debug(F, A)).
    -define(DBG(Str, Args), lager:info(Str, Args)).
    -define(DBGS(Str), lager:info(Str)).
-else.
    -define(DEBUG(F, A), ok).
    -define(DBG(Str, Args), ok).
    -define(DBGS(Str), ok).
-endif.
-define(INFO(F, A), lager:info(F, A)).
-define(ERR(F, A), lager:error(F, A)).

%%数据库连接（分别有：配置库、运行时库、日志库、用户库）
-define(DB_CFG, bg_mysql_cfg).
-define(DB_RUN, bg_mysql_run).
-define(DB_LOG, bg_mysql_log).
-define(DB_USR, bg_mysql_usr).

-define(DB_CFG_NAME, "bitgame_cfg").
-define(DB_CFG_HOST, "127.0.0.1").
-define(DB_CFG_PORT, 3306).
-define(DB_CFG_USER, "bitgame").
-define(DB_CFG_PASS, "bitgame123").

-define(DB_RUN_NAME, "bitgame_run").
-define(DB_RUN_HOST, "127.0.0.1").
-define(DB_RUN_PORT, 3306).
-define(DB_RUN_USER, "bitgame").
-define(DB_RUN_PASS, "bitgame123").

-define(DB_LOG_NAME, "bitgame_log").
-define(DB_LOG_HOST, "127.0.0.1").
-define(DB_LOG_PORT, 3306).
-define(DB_LOG_USER, "bitgame").
-define(DB_LOG_PASS, "bitgame123").

-define(DB_USR_NAME, "bitgame_usr").
-define(DB_USR_HOST, "127.0.0.1").
-define(DB_USR_PORT, 3306).
-define(DB_USR_USER, "bitgame").
-define(DB_USR_PASS, "bitgame123").

-define(DB_ENCODE, utf8).

%%缓存连接
-define(CACHE, bg_cache).
-define(IDGEN, bg_idgen).
-define(CACHE_HOST, "127.0.0.1").
%% CACHE_HOST:
%% A list of hosts:port separated by ';' to the
%% administration port of the couchbase cluster. (ex:
%% "host1;host2:9000;host3" would try to connect to
%% host1 on port 8091, if that fails it'll connect to
%% host2 on port 9000 etc).

-define(SVRTYPE_GATE, 1).
-define(SVRTYPE_GAME, 2).
-define(SVRTYPE_XCHG, 3).

%%ETS
-define(ETS_GAMESVR, ets_gamesvr).
-define(ETS_XCHGSVR, ets_xchgsvr).

%%DETS（注意，dets有2GB限制！）
-define(DETS_GLOBAL_RUN_DATA, dets_global_run_data).

-define(ETSRC, {read_concurrency, true}).
-define(ETSWC, {write_concurrency, true}).

-define(B2T(B), util:bitstring_to_term(B)).
-define(T2B(T), util:term_to_bitstring(T)).
-define(T2B_P(T), util:term_to_bitstring_p(T)).

% 用正则式过滤掉进程id，否则无法通过erl_parse
-define(S2T(S), util:string_to_term(re:replace(S, "<[0-9]+\.[0-9]+\.[0-9]+>", "undefined", [global, {return,list}]))).
-define(T2S(T), util:term_to_string(T)).

% 金币的操作宏
-define(G(RawGold, GoldType), lib_gold:gold(RawGold, GoldType)).
-define(G(RawGold, GoldType, NewGoldValue), lib_gold:gold(RawGold, GoldType, NewGoldValue)).

% 默认币种
-define(DEFAULT_GOLD_TYPE, <<"BGX">>).

% 特定的几个错误码定义
-define(ERRNO_IP_BLOCKED, -999). % ip受限
-define(ERRNO_MISSING_PARAM, -998). % 参数不全
-define(ERRNO_WRONG_PARAM, -997). % 参数错误
-define(ERRNO_VERIFY_FAILED, -996). % 校验失败
-define(ERRNO_ACTION_NOT_SUPPORT, -995). % 协议不支持
-define(ERRNO_EXCEPTION, -994). % 异常抛出
-define(ERRNO_GAME_NOT_OPEN, -993). % 游戏未开放
-define(ERRNO_READ_CACHE, -992). % 读缓存失败
-define(ERRNO_NO_RPC_SERVER, -991). % 不存在rpc服务器
-define(ERRNO_GOLD_NOT_ENOUGH, -990). % 金币不足
-define(ERRNO_HTTP_REQ_FAILED, -989). % 请求失败
-define(ERRNO_HTTP_REQ_TIMEOUT, -988). % 请求超时
-define(ERRNO_UNIDENTIFIED, -987). % 未确定的错误
-define(ERRNO_INVALID_WALLET_ADDR, -986). % 无效的钱包地址
-define(ERRNO_INVALID_EXCHANGE_ID, -985). % 无效的交易所账号

