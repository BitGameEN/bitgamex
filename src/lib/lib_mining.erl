%%%--------------------------------------
%%% @Module  : lib_mining
%%% @Description: 挖矿相关处理
%%%--------------------------------------
-module(lib_mining).
-export([distribute_game_delta_golds/2, distribute_login_delta_golds/1]).

-include("common.hrl").
-include("gameConfigGlobalKey.hrl").
-include("record.hrl").
-include("record_cfg_gold_type.hrl").
-include("record_run_role_gold_to_draw.hrl").

% 挖矿逻辑
% 对于BGX，游戏内挖矿的总量为15亿代币，第一年3.75亿，每天产量固定，约102.74万个，每2年产出减半
%-define(BGX_MINING_START_TIME, 1525017600). % 2018/4/30 0:0:0 UTC-0
%-define(BGX_MINING_OUTPUT_PER_SECOND, 375000000 / (365 * 24 * 3600)).

get_output_quota(LogicType, GoldType, DurationSeconds) ->
    Proportion = case LogicType of
                     game -> 1 - lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_LOGIN);
                     login -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_LOGIN)
                 end,
    case cfg_gold_type:get(GoldType) of
        #gold_type{mining_start_time = MiningStartTime, mining_output_first_day = MiningOutputFirstDay, half_life_days = HalfLifeDays} ->
            NumOfHalfLifeCycles = util:get_diff_days(util:unixtime(), MiningStartTime) div HalfLifeDays,
            (MiningOutputFirstDay / 86400) * DurationSeconds * Proportion * math:pow(0.5, NumOfHalfLifeCycles) / lib_game:gamesvr_num();
        _ -> 0
    end.

distribute_game_delta_golds([], _) ->
    void;
distribute_game_delta_golds(Requests, DurationSeconds) ->
    lib_role_gold_to_draw:put_gold_drain_type_and_drain_id(distribute_game_delta_golds, 0, 0),
    GTs = lists:usort([GT || #add_gold_req{gold_type = GT} <- Requests]),
    DistributeFunSameGT =
        fun(TheGT) ->
            RequestsTheGT = [One || (#add_gold_req{gold_type = GT} = One) <- Requests, GT =:= TheGT],
            Quota = get_output_quota(game, TheGT, DurationSeconds),
            DistributeL0 = [{{Uid, Gid}, DeltaGold * lib_game:game_hard_coef(Gid), Time} ||
                            #add_gold_req{uid = Uid, game_id = Gid, delta_gold = DeltaGold, time = Time} <- RequestsTheGT],
            Total = lists:sum([V || {_, V, _} <- DistributeL0]),
            DistributeL = case Total =< Quota of
                              true -> DistributeL0;
                              false -> [{K, Quota * V / Total, T} || {K, V, T} <- DistributeL0]
                          end,
            [lib_role_gold_to_draw:add_gold_to_draw(UserId, GameId, TheGT, [{Time, AddGold}]) ||
               {{UserId, GameId}, AddGold, Time} <- DistributeL]
        end,
    [DistributeFunSameGT(GT) || GT <- GTs],
    ok.

distribute_login_delta_golds(PlayerId) ->
    ok.