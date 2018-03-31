%%%------------------------------------
%%% @Module  : mod_distributor
%%% @Description: 玩游戏挖矿分配器进程
%%%------------------------------------
-module(mod_distributor).
-behaviour(gen_server).

-export([start_link/0, req_add_balance/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-define(TO_DISTRIBUTE_INTERVAL, 5 * 1000).

-record(status, {
    requests = []
}).

start_link() ->
    gen_server:start_link(?MODULE, [], []).

req_add_balance(RequestInfo) ->
    gen_server:cast(?MODULE, {req_add_balance, RequestInfo}).

init([]) ->
    process_flag(trap_exit, true),
    erlang:send_after(?TO_DISTRIBUTE_INTERVAL, self(), to_distribute),
    {ok, #status{}}.

handle_cast({req_add_balance, RequestInfo}, #status{requests = Requests} = Status) ->
    % 放入队列
    {noreply, Status#status{requests = [RequestInfo | Requests]}};

handle_cast(_R, Status) ->
    {noreply, Status}.

handle_call(_R, _FROM, Status) ->
    {reply, ok, Status}.

handle_info(to_distribute, Status) ->
    erlang:send_after(?TO_DISTRIBUTE_INTERVAL, self(), to_distribute),
    {noreply, distribute(Status)};

handle_info(_Reason, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    {ok, distribute(Status)}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

% 返回NewStatus
distribute(#status{requests = Requests} = Status) ->
    try
        lib_mining:distribute_game_delta_balances(Requests, ?TO_DISTRIBUTE_INTERVAL)
    catch
        _:ErrMsg ->
            ?ERR("distributor failed:~n~p~n", [ErrMsg])
    end,
    Status#status{requests = []}.

