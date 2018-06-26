%%%--------------------------------------
%%% @Module  : lib_mining
%%% @Description: 挖矿相关处理
%%%--------------------------------------
-module(lib_mining).
-export([distribute_game_delta_golds/2, distribute_login_delta_golds/3]).
-export([clear_cache_power_list/0]).
-export([get_rand_gold_type/1, get_gold_types/1]).

-include("common.hrl").
-include("gameConfig.hrl").
-include("gameConfigGlobalKey.hrl").
-include("record.hrl").
-include("record_cfg_gold_type.hrl").
-include("record_run_role.hrl").
-include("record_run_role_gold_to_draw.hrl").
-include("record_usr_game.hrl").

% 挖矿逻辑
% 对于BGX，游戏内挖矿的总量为15亿代币，第一年3.75亿，每天产量固定，约102.74万个，每2年产出减半
%-define(BGX_MINING_START_TIME, 1525017600). % 2018/4/30 0:0:0 UTC-0
%-define(BGX_MINING_OUTPUT_PER_SECOND, 375000000 / (365 * 24 * 3600)).

get_output_quota(LogicType, GoldType, DurationSeconds) ->
    Proportion = case LogicType of
                     login -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_LOGIN);
                     random -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_RANDOM);
                     game -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_GAME);
                     _ -> 0
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
                              false -> [{K, util:round8d(Quota * V / Total), T} || {K, V, T} <- DistributeL0]
                          end,
            [lib_role_gold_to_draw:add_gold_to_draw(UserId, GameId, TheGT, [{Time, AddGold}], ?MINING_DRAIN_TYPE_SAVEGAME) ||
               {{UserId, GameId}, AddGold, Time} <- DistributeL],
            % 幸运随机挖矿
            UidGidPairs = [{Uid, Gid} || #add_gold_req{uid = Uid, game_id = Gid} <- RequestsTheGT],
            Uids = lists:usort([Uid || {Uid, _} <- UidGidPairs]), % 去重
            LuckyUserId = util:rand_one(Uids),
            Gids = [Gid || {Uid, Gid} <- UidGidPairs, Uid =:= LuckyUserId],
            LuckyGameId = util:rand_one(Gids),
            RandomQuota = get_output_quota(random, TheGT, DurationSeconds),
            lib_role_gold_to_draw:add_gold_to_draw(LuckyUserId, LuckyGameId, TheGT, [{util:unixtime(), RandomQuota}], ?MINING_DRAIN_TYPE_RANDOM)
        end,
    [DistributeFunSameGT(GT) || GT <- GTs],
    ok.

-define(TWO_DAY_SECONDS, 2 * 24 * 3600).
distribute_login_delta_golds(PlayerId, GameId, DurationSeconds0) ->
    DurationSeconds = util:clamp(1, ?TWO_DAY_SECONDS, DurationSeconds0),
    TheGT = get_rand_gold_type(GameId),
    % 因为power总值在持续增长，使用当前的power总值，一定不会超发
    PowerSum = lists:sum([P || {_, P} <- get_power_list()]),
    Role = run_role:get_one({GameId, PlayerId}),
    AddGold0 = get_output_quota(login, TheGT, DurationSeconds) * Role#run_role.power / PowerSum,
    AddGold = util:round8d(AddGold0),
    case AddGold > 0 of
        true ->
            lib_role_gold_to_draw:put_gold_drain_type_and_drain_id(distribute_login_delta_golds, 0, 0),
            N = DurationSeconds div 7200,
            AddGoldList = case N of
                              0 ->
                                  [AddGold];
                              _ ->
                                  Average = util:round8d(AddGold / (DurationSeconds / 7200)),
                                  L = lists:duplicate(N, Average),
                                  Remainder = AddGold - Average * N,
                                  [Remainder | L]
                          end,
            Now = util:unixtime(),
            TimeList = [Now - (I - 1) * 7200 || I <- lists:seq(1, length(AddGoldList))],
            TimeToAddGoldPairs = lists:zip(TimeList, AddGoldList),
            [lib_role_gold_to_draw:add_gold_to_draw(PlayerId, GameId, TheGT, [{T, G}], ?MINING_DRAIN_TYPE_LOGIN) || {T, G} <- TimeToAddGoldPairs];
        false -> void
    end,
    ok.

-define(CACHE_KEY_POWERSUM_LIST, <<"cache_key_powersum_list">>).
% 获取各游戏的power分布
get_power_list() ->
    case cache:get(?CACHE_KEY_POWERSUM_LIST) of
        {true, _Cas, Val} -> Val;
        _ ->
            GameIds = usr_game:get_game_gids_by_game_type_and_open_status({?GAME_TYPE_MINING, 1}),
            Sql = <<"select sum(power) from role_~p">>,
            PowerList = [{GameId, case db_esql:get_one(?DB_RUN, io_lib:format(Sql, [GameId])) of
                                      undefined -> 0;
                                      Sum -> Sum
                                  end} || GameId <- GameIds],
            cache:set(?CACHE_KEY_POWERSUM_LIST, PowerList, 300), % 缓存5分钟，时间不能太长，否则容易超发
            PowerList
    end.

% 当增加新游戏，或者下架旧游戏时，需要立即清除power_list
clear_cache_power_list() ->
    case cache:get(?CACHE_KEY_POWERSUM_LIST) of
        {true, _Cas, CacheKey} -> cache:del(CacheKey);
        _ -> void
    end,
    ok.

get_rand_gold_type(GameId) ->
    case usr_game:get_one(GameId) of
        #usr_game{mining_rule = MiningRule} ->
            % rule格式：[{'BGX', 30}, {'BTC', 10}, {'ETH', 10}, {'ELA', 50}]
            {GoldTypeList, WeightList} = lists:unzip(MiningRule),
            I = util:rand_list_index(WeightList),
            list_to_binary(atom_to_list(lists:nth(I, GoldTypeList)));
        _ -> ?DEFAULT_GOLD_TYPE
    end.

get_gold_types(GameId) ->
    case usr_game:get_one(GameId) of
        #usr_game{mining_rule = MiningRule} ->
            % rule格式：[{'BGX', 30}, {'BTC', 10}, {'ETH', 10}, {'ELA', 50}]
            {GoldTypeList, _} = lists:unzip(MiningRule),
            GoldTypeList;
        _ -> []
    end.

