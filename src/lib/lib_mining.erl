%%%--------------------------------------
%%% @Module  : lib_mining
%%% @Description: 挖矿相关处理
%%%--------------------------------------
-module(lib_mining).
-export([distribute_game_delta_balances/2, distribute_login_delta_balances/1]).

-include("common.hrl").
-include("gameConfigGlobalKey.hrl").
-include("record.hrl").
-include("record_run_role_gold_to_draw.hrl").

% 挖矿逻辑
% 游戏内挖矿的总量为15亿代币，第一年3.75亿，每天产量固定，约102.74万个，每2年产出减半
-define(MINING_START_TIME, 1525017600). % 2018/4/30 0:0:0 UTC-0
-define(MINING_OUTPUT_PER_SECOND, 375000000 / (365 * 24 * 3600)).

get_output_quota(Type, DurationSeconds) ->
    Proportion = case Type of
                     game -> 1 - lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_LOGIN);
                     login -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_LOGIN)
                 end,
    Years = util:get_diff_days(util:unixtime(), ?MINING_START_TIME) div 365,
    ?MINING_OUTPUT_PER_SECOND * DurationSeconds * Proportion * math:pow(0.5, Years div 2) / lib_game:gamesvr_num().

distribute_game_delta_balances([], _) ->
    void;
distribute_game_delta_balances(Requests, DurationSeconds) ->
    Quota = get_output_quota(game, DurationSeconds),
    DistributeL0 = [{{Uid, Gid}, DeltaBalance * lib_game:game_hard_coef(Gid), Time} ||
                    #add_balane_req{uid = Uid, game_id = Gid, delta_balance = DeltaBalance, time = Time} <- Requests],
    Total = lists:sum([V || {_, V, _} <- DistributeL0]),
    DistributeL = case Total =< Quota of
                      true -> DistributeL0;
                      false -> [{K, Quota * V / Total, T} || {K, V, T} <- DistributeL0]
                  end,
    F = fun({UserId, GameId}, AddBalance, Time) ->
            lib_role_gold_to_draw:add_gold_to_draw(UserId, GameId, [{Time, AddBalance}])
        end,
    [F(K, V, T) || {K, V, T} <- DistributeL],
    ok.

distribute_login_delta_balances(PlayerId) ->
    ok.