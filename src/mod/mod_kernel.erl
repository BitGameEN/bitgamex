%%%------------------------------------
%%% @Module  : mod_kernel
%%% @Description: 核心服务
%%%------------------------------------
-module(mod_kernel).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("record.hrl").

start_link() ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [], []).

init([]) ->
    process_flag(trap_exit, true),
    %%初始ets表
    util:launch_log("====> \tinit_ets...~n", []),
    ok = init_ets(),
    %%初始dets表
    util:launch_log("====> \tinit_dets...~n", []),
    ok = init_dets(),
    {ok, 1}.

handle_cast(_R, Status) ->
    {noreply, Status}.

handle_call(_R, _FROM, Status) ->
    {reply, ok, Status}.

handle_info(_Reason, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    close_dets(),
    {ok, Status}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

%% ================== 私有函数 =================

%%初始ETS表
init_ets() ->
    run_role:init(),
    ok.

%%初始DETS表
init_dets() ->
    %%开辟此表完全是为了简单方便，脱离数据库存取的繁琐，主要用于存储全局key-value形式的存盘数据，既然是存盘的，就不能清空
    SvrId = mod_disperse:server_id(),
    GlobalRunFile = "global_run_data_"++integer_to_list(SvrId),
    dets:open_file(?DETS_GLOBAL_RUN_DATA, [{type, set}, {file, GlobalRunFile}]),
    ok.

%%关闭DETS表
close_dets() ->
    SvrId = mod_disperse:server_id(),
    dets:close(?DETS_GLOBAL_RUN_DATA),
    ok.
