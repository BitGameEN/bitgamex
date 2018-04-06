%%%--------------------------------------
%%% @Module  : lib_game
%%% @Description: 游戏相关处理
%%%--------------------------------------
-module(lib_game).
-export([set_gamesvr_num/1, gamesvr_num/0, game_hard_coef/1, add_reclaimed_gold/3, put_gold_drain_type_and_drain_id/3]).

-include("common.hrl").
-include("record_usr_game.hrl").
-include("record_usr_game_reclaimed_gold.hrl").
-include("record_log_gold_reclaimed.hrl").


% game服务器数量的缓存key
-define(GAMESVR_NUM_CACHE_KEY, <<"gamesvr_num_cache_key">>).


set_gamesvr_num(Num) ->
    cache:set(?GAMESVR_NUM_CACHE_KEY, Num).

gamesvr_num() ->
    case catch cache:get(?GAMESVR_NUM_CACHE_KEY) of
        {true, Cas, Val} -> Val;
        _ -> 1
    end.

game_hard_coef(GameId) ->
    case usr_game:get_one(GameId) of
        #usr_game{hard_coef = Coef} -> Coef;
        _ -> 1.0
    end.

add_reclaimed_gold(GameId, GoldType, 0) ->
    ok;
add_reclaimed_gold(GameId, GoldType, DeltaGold) ->
    {true, Cas} = lock(GameId),
    try
        GameReclaimedGold =
            case usr_game_reclaimed_gold:get_one(GameId) of
                [] ->
                    R = #usr_game_reclaimed_gold{game_id = GameId, gold = <<"{}">>, time = util:unixtime()},
                    usr_game_reclaimed_gold:set_one(R),
                    R;
                R_ -> R_
            end,
        RawGold = GameReclaimedGold#usr_game_reclaimed_gold.gold,
        OldGold = ?G(RawGold, GoldType),
        NewGold = OldGold + DeltaGold,
        case NewGold < 0 of
            true -> throw({?ERRNO_GOLD_NOT_ENOUGH, <<"reclaimed gold not enough">>});
            false -> void
        end,
        usr_game_reclaimed_gold:set_one(GameReclaimedGold#usr_game_reclaimed_gold{gold = ?G(RawGold, GoldType, NewGold), time = util:unixtime()}),
        LogR = #log_gold_reclaimed{
                game_id = GameId,
                gold_type = GoldType,
                delta = DeltaGold,
                old_value = OldGold,
                new_value = NewGold,
                drain_type = case get(game_gold_drain_type) of undefined -> <<>>; V -> V end,
                drain_id = case get(game_gold_drain_id) of undefined -> 0; V -> V end,
                drain_count = case get(game_gold_drain_count) of undefined -> 0; V -> V end,
                time = util:unixtime(),
                call_flow = get_call_flow(get(game_gold_drain_type))
            },
        spawn(fun() -> log_gold_reclaimed:set_one(LogR) end),
        unlock(GameId, Cas),
        ok
    catch
        throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
            unlock(GameId, Cas),
            throw({ErrNo, ErrMsg});
        _:ExceptionErr ->
            unlock(GameId, Cas),
            ?ERR("add_reclaimed_gold exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
            throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

% 锁定成功，返回{true, Cas}
lock(GameId) ->
    LockKey = cache_lock_key(GameId),
    case cache:get_and_lock(LockKey) of
        false ->
            cache:set(LockKey, <<>>),
            lock(GameId);
        {true, Cas, _} ->
            {true, Cas}
    end.

unlock(GameId, Cas) ->
    cache:unlock(cache_lock_key(GameId), Cas).

cache_lock_key(GameId) ->
    list_to_binary(io_lib:format(<<"lock_game_reclaimed_gold_~p">>, [GameId])).

get_call_flow(undefined) ->
    {current_stacktrace, Stack} = erlang:process_info(self(), current_stacktrace),
    [_, _ | RestStack] = Stack,
    Calls = [{Module, Function} || {Module, Function, Arity, Location} <- RestStack, Module =/= proc_lib],
    Res = ?T2B(Calls),
    binary:replace(Res, <<"'">>, <<"">>, [global]);
get_call_flow(DrainType) ->
    <<>>.

put_gold_drain_type_and_drain_id(DrainType, DrainId, DrainCount) ->
    put(game_gold_drain_type, DrainType),
    put(game_gold_drain_id, DrainId),
    put(game_gold_drain_count, DrainCount),
    ok.

clear_gold_drain_type_and_drain_id() ->
    erase(game_gold_drain_type),
    erase(game_gold_drain_id),
    erase(game_gold_drain_count),
    ok.

