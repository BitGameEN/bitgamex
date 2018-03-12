%%%--------------------------------------
%%% @Module  : lib_user_gold
%%% @Description: 用户金币相关处理
%%%--------------------------------------
-module(lib_user_gold).
-export([lock/1, unlock/2]).
-export([save/1, put_gold_drain_type_and_drain_id/3]).

-include("common.hrl").
-include("record_usr_user.hrl").
-include("record_usr_user_gold.hrl").
-include("record_log_gold.hrl").


% 锁定成功，返回{true, Cas, Val}
lock(PlayerId) ->
    case usr_user_gold:get_one(PlayerId) of
        [] -> {false, not_exist};
        _ -> cache:get_and_lock(usr_user_gold:cache_key(PlayerId))
    end.

unlock(PlayerId, Cas) ->
    cache:unlock(usr_user_gold:cache_key(PlayerId), Cas).
    
save(#usr_user_gold{player_id = PlayerId} = UserGold) ->
    OldUserGold = usr_user_gold:get_one(PlayerId),
    GoldDelta = trunc(UserGold#usr_user_gold.gold - OldUserGold#usr_user_gold.gold),
    case GoldDelta of
        0 -> void;
        _ ->
            User = usr_user:get_one(PlayerId),
            R = #log_gold{
                    player_id = PlayerId,
                    game_id = User#usr_user.current_game_id,
                    delta = GoldDelta,
                    old_value = OldUserGold#usr_user_gold.gold,
                    new_value = UserGold#usr_user_gold.gold,
                    drain_type = case get(gold_drain_type) of undefined -> <<>>; V -> V end,
                    drain_id = case get(gold_drain_id) of undefined -> 0; V -> V end,
                    drain_count = case get(gold_drain_count) of undefined -> 0; V -> V end,
                    time = util:unixtime(),
                    call_flow = get_call_flow(get(gold_drain_type))
                },
            spawn(fun() -> log_gold:set_one(R) end)
    end,
    usr_user_gold:set_one(UserGold#usr_user_gold{time = util:unixtime()}).

get_call_flow(undefined) ->
    {current_stacktrace, Stack} = erlang:process_info(self(), current_stacktrace),
    [_, _ | RestStack] = Stack,
    Calls = [{Module, Function} || {Module, Function, Arity, Location} <- RestStack, Module =/= proc_lib],
    Res = ?T2B(Calls),
    binary:replace(Res, <<"'">>, <<"">>, [global]);
get_call_flow(DrainType) ->
    <<>>.

put_gold_drain_type_and_drain_id(DrainType, DrainId, DrainCount) ->
    put(gold_drain_type, DrainType),
    put(gold_drain_id, DrainId),
    put(gold_drain_count, DrainCount),
    ok.

clear_gold_drain_type_and_drain_id() ->
    erase(gold_drain_type),
    erase(gold_drain_id),
    erase(gold_drain_count),
    ok.

