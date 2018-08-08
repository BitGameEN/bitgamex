%%%--------------------------------------
%%% @Module  : lib_mining
%%% @Description: 挖矿相关处理
%%%--------------------------------------
-module(lib_mining).
-export([distribute_game_delta_golds/2, distribute_login_delta_golds/4]).
-export([get_rand_gold_type/2, get_gold_types/2]).

-include("common.hrl").
-include("gameConfig.hrl").
-include("gameConfigGlobalKey.hrl").
-include("record.hrl").
-include("record_cfg_gold_type.hrl").
-include("record_run_role.hrl").
-include("record_run_role_gold_to_draw.hrl").
-include("record_usr_game.hrl").
-include("record_usr_game_package.hrl").

% 挖矿逻辑
% 对于BGX，游戏内挖矿的总量为15亿代币，第一年3.75亿，每天产量固定，约102.74万个，每2年产出减半
%-define(BGX_MINING_START_TIME, 1525017600). % 2018/4/30 0:0:0 UTC-0
%-define(BGX_MINING_OUTPUT_PER_SECOND, 375000000 / (365 * 24 * 3600)).

get_output_quota(LogicType, Pool, DurationSeconds) ->
    Proportion = case LogicType of
                     login -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_LOGIN);
                     random -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_RANDOM);
                     game -> lib_global_config:get(?GLOBAL_CONFIG_KEY_GOLD_PROPORTION_FOR_GAME);
                     _ -> 0
                 end,
    case get_pool(Pool) of
        #gold_type{mining_start_time = MiningStartTime, mining_output_first_day = MiningOutputFirstDay, half_life_days = HalfLifeDays} ->
            NumOfHalfLifeCycles = util:get_diff_days(util:unixtime(), MiningStartTime) div HalfLifeDays,
            (MiningOutputFirstDay / 86400) * DurationSeconds * Proportion * math:pow(0.5, NumOfHalfLifeCycles) / lib_game:gamesvr_num();
        _ -> 0
    end.

get_pool({GameId, PkgId, GoldType}) ->
    Pkgs = get(pkgs),
    {_, #usr_game_package{mining_pools = Pools}} = lists:keyfind({GameId, PkgId}, 1, Pkgs),
    case lists:keyfind(binary_to_atom(GoldType, utf8), 1, Pools) of
        {_, MiningStartTime, MiningOutputFirstDay, HalfLifeDays, ChainType, Amount} ->
            #gold_type{gold_type = GoldType, mining_start_time = MiningStartTime, mining_output_first_day = MiningOutputFirstDay,
                       half_life_days = HalfLifeDays, chain_type = ChainType, amount = Amount};
        _ -> mining_pool_not_found
    end;
get_pool(GoldType) ->
    cfg_gold_type:get(GoldType).

distribute_game_delta_golds([], _) ->
    void;
distribute_game_delta_golds(Requests0, DurationSeconds) ->
    lib_role_gold_to_draw:put_gold_drain_type_and_drain_id(distribute_game_delta_golds, 0, 0),
    Pkgs0 = lists:usort([{GameId, PkgId} || #add_gold_req{game_id = GameId, game_pkg_id = PkgId} <- Requests0]),
    Pkgs = [{{GameId, PkgId}, Pkg} || {GameId, PkgId} <- Pkgs0, (Pkg = usr_game_package:get_one({GameId, PkgId})) =/= []], % 有自己独立矿池的包
    put(pkgs, Pkgs),
    Requests = [case lists:keymember({GameId, PkgId}, 1, Pkgs) of
                    true -> {{GameId, PkgId, GT}, Req};
                    false -> {GT, Req}
                end || (#add_gold_req{game_id = GameId, game_pkg_id = PkgId, gold_type = GT} = Req) <- Requests0],
    Pools = lists:usort([Pool || {Pool, _} <- Requests]),
    DistributeFunSamePool =
        fun(ThePool) ->
            RequestsThePool = [Req || {Pool, Req} <- Requests, Pool =:= ThePool],
            Quota = get_output_quota(game, ThePool, DurationSeconds),
            DistributeL0 = [{{Uid, Gid}, DeltaGold * lib_game:game_hard_coef(Gid), Time} ||
                            #add_gold_req{uid = Uid, game_id = Gid, delta_gold = DeltaGold, time = Time} <- RequestsThePool],
            Total = lists:sum([V || {_, V, _} <- DistributeL0]),
            DistributeL = case Total =< Quota of
                              true -> DistributeL0;
                              false -> [{K, util:round8d(Quota * V / Total), T} || {K, V, T} <- DistributeL0]
                          end,
            {ThePkgId, TheGT} =
                case ThePool of
                    {_GameId, PkgId, GT} -> {PkgId, GT};
                    GT -> {0, GT}
                end,
            [lib_role_gold_to_draw:add_gold_to_draw(UserId, GameId, ThePkgId, TheGT, [{Time, AddGold}], ?MINING_DRAIN_TYPE_SAVEGAME) ||
               {{UserId, GameId}, AddGold, Time} <- DistributeL],
            % 幸运随机挖矿
            UidGidPairs = [{Uid, Gid} || #add_gold_req{uid = Uid, game_id = Gid} <- RequestsThePool],
            Uids = lists:usort([Uid || {Uid, _} <- UidGidPairs]), % 去重
            LuckyUserId = util:rand_one(Uids),
            Gids = [Gid || {Uid, Gid} <- UidGidPairs, Uid =:= LuckyUserId],
            LuckyGameId = util:rand_one(Gids),
            RandomQuota = get_output_quota(random, ThePool, DurationSeconds),
            LuckyAddGold = util:round8d(RandomQuota),
            lib_role_gold_to_draw:add_gold_to_draw(LuckyUserId, LuckyGameId, ThePkgId, TheGT, [{util:unixtime(), LuckyAddGold}], ?MINING_DRAIN_TYPE_RANDOM)
        end,
    [DistributeFunSamePool(Pool) || Pool <- Pools],
    ok.

-define(TWO_DAY_SECONDS, 2 * 24 * 3600).
distribute_login_delta_golds(PlayerId, GameId, PkgId, DurationSeconds0) ->
    DurationSeconds = util:clamp(1, ?TWO_DAY_SECONDS, DurationSeconds0),
    TheGT = get_rand_gold_type(GameId, PkgId),
    {Pool, Pkgs} =
        case usr_game_package:get_one({GameId, PkgId}) of
            [] -> {TheGT, []};
            Pkg -> {{GameId, PkgId, TheGT}, [{{GameId, PkgId}, Pkg}]}
        end,
    put(pkgs, Pkgs),
    % 因为power总值在持续增长，使用当前的power总值，不会超发
    PowerSum = get_power_sum(GameId, PkgId, TheGT),
    Role = run_role:get_one({GameId, PlayerId}),
    AddGold0 = get_output_quota(login, Pool, DurationSeconds) * Role#run_role.power / PowerSum,
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
                                  Remainder = util:round8d(AddGold - Average * N),
                                  [Remainder | L]
                          end,
            Now = util:unixtime(),
            TimeList = [Now - (I - 1) * 7200 || I <- lists:seq(1, length(AddGoldList))],
            TimeToAddGoldPairs = lists:zip(TimeList, AddGoldList),
            [lib_role_gold_to_draw:add_gold_to_draw(PlayerId, GameId, PkgId, TheGT, [{T, G}], ?MINING_DRAIN_TYPE_LOGIN) || {T, G} <- TimeToAddGoldPairs];
        false -> void
    end,
    ok.

cache_key_power_sum(GameId, PkgId, GoldType) ->
    GameIdBin = integer_to_binary(GameId),
    PkgIdBin = integer_to_binary(PkgId),
    <<"cache_key_powersum_", GameIdBin/binary, "_", PkgIdBin/binary, "_", GoldType/binary>>.

get_power_sum(GameId, PkgId, GoldType) ->
    GoldTypeAtom = binary_to_atom(GoldType, utf8),
    CacheKey = cache_key_power_sum(GameId, PkgId, GoldType),
    case cache:get(CacheKey) of
        {true, _Cas, Val} -> Val;
        _ ->
            GameIds = usr_game:get_game_gids_by_game_type_and_open_status({?GAME_TYPE_MINING, 1}),
            {GameIdsPublicPool, GameIdsPrivatePool} = lists:partition(fun(Gid) -> usr_game_package:get_one({Gid, 0}) =:= [] end, GameIds),
            case usr_game_package:get_one({GameId, PkgId}) of
                [] ->
                    RacingGameIds = [Gid || Gid <- GameIdsPublicPool, (Game = usr_game:get_one(Gid)) =/= [], lists:keymember(GoldTypeAtom, 1, Game#usr_game.mining_rule)],
                    Sql = <<"select sum(power) from role_~p">>,
                    PowerList = [case db_esql:get_one(?DB_RUN, io_lib:format(Sql, [Gid])) of
                                     undefined -> 0;
                                     Sum -> Sum
                                 end || Gid <- RacingGameIds],
                    PowerSum = lists:sum(PowerList),
                    cache:set(CacheKey, PowerSum, 300), % 缓存5分钟，时间不能太长，否则容易超发
                    PowerSum;
                _ ->
                    % 出于性能考虑，只计算人头数；同时出于降低实现复杂度，只考虑上次登录的是该游戏的玩家
                    Sql = <<"select count(*) from user where current_game_id=~p and current_game_package_id=~p">>,
                    PowerSum = case db_esql:get_one(?DB_USR, io_lib:format(Sql, [GameId, PkgId])) of
                                   undefined -> 0;
                                   Cnt -> Cnt
                               end,
                    cache:set(CacheKey, PowerSum, 300), % 缓存5分钟，时间不能太长，否则容易超发
                    PowerSum
            end
    end.

get_rand_gold_type(GameId, PkgId) ->
    MiningRule =
        case usr_game_package:get_one({GameId, PkgId}) of
            #usr_game_package{mining_rule = MR} -> MR;
            _ ->
                case usr_game:get_one(GameId) of
                    #usr_game{mining_rule = MR} -> MR;
                    _ -> []
                end
        end,
    case MiningRule of
        [] -> ?DEFAULT_GOLD_TYPE;
        _ -> % rule格式：[{'BGX', 30}, {'BTC', 10}, {'ETH', 10}, {'ELA', 50}]
            {GoldTypeList, WeightList} = lists:unzip(MiningRule),
            I = util:rand_list_index(WeightList),
            list_to_binary(atom_to_list(lists:nth(I, GoldTypeList)))
    end.

get_gold_types(GameId, PkgId) ->
    MiningRule =
        case usr_game_package:get_one({GameId, PkgId}) of
            #usr_game_package{mining_rule = MR} -> MR;
            _ ->
                case usr_game:get_one(GameId) of
                    #usr_game{mining_rule = MR} -> MR;
                    _ -> []
                end
        end,
    case MiningRule of
        [] -> [];
        _ -> % rule格式：[{'BGX', 30}, {'BTC', 10}, {'ETH', 10}, {'ELA', 50}]
            {GoldTypeList, _} = lists:unzip(MiningRule),
            GoldTypeList
    end.

