%%%--------------------------------------
%%% @Module  : lib_role_gold
%%% @Description: 角色金币相关处理
%%%--------------------------------------
-module(lib_role_gold).
-export([add_gold/3, put_gold_drain_type_and_drain_id/3]).

-include("common.hrl").
-include("record_run_role_gold.hrl").
-include("record_log_gold.hrl").


add_gold(PlayerId, GameId, DeltaGold) ->
    {true, Cas} = lock(PlayerId),
    try
        RoleGold = run_role_gold:get_one({GameId, PlayerId}),
        OldGold = RoleGold#run_role_gold.gold,
        NewGold = OldGold + DeltaGold,
        case NewGold < 0 of
            true -> throw({?ERRNO_GOLD_NOT_ENOUGH, <<"gold not enough">>});
            false -> void
        end,
        save(RoleGold#run_role_gold{gold = NewGold}),
        unlock(PlayerId, Cas),
        ok
    catch
        throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
            unlock(PlayerId, Cas),
            throw({ErrNo, ErrMsg});
        _:ExceptionErr ->
            unlock(PlayerId, Cas),
            ?ERR("add_gold exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
            throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

% 锁定成功，返回{true, Cas}
lock(PlayerId) ->
    LockKey = cache_lock_key(PlayerId),
    case cache:get_and_lock(LockKey) of
        false ->
            cache:set(LockKey, <<>>),
            lock(PlayerId);
        {true, Cas, _} ->
            {true, Cas}
    end.

unlock(PlayerId, Cas) ->
    cache:unlock(cache_lock_key(PlayerId), Cas).

cache_lock_key(PlayerId) ->
    % 仍旧用lock_user_gold
    list_to_binary(io_lib:format(<<"lock_user_gold_~p">>, [PlayerId])).

save(#run_role_gold{key_id = KeyId, player_id = PlayerId} = RoleGold) ->
    OldRoleGold = run_role_gold:get_one(KeyId),
    GoldDelta = RoleGold#run_role_gold.gold - OldRoleGold#run_role_gold.gold,
    case GoldDelta of
        0 -> void;
        _ ->
            R = #log_gold{
                    player_id = PlayerId,
                    game_id = RoleGold#run_role_gold.game_id,
                    delta = GoldDelta,
                    old_value = OldRoleGold#run_role_gold.gold,
                    new_value = RoleGold#run_role_gold.gold,
                    drain_type = case get(role_gold_drain_type) of undefined -> <<>>; V -> V end,
                    drain_id = case get(role_gold_drain_id) of undefined -> 0; V -> V end,
                    drain_count = case get(role_gold_drain_count) of undefined -> 0; V -> V end,
                    time = util:unixtime(),
                    call_flow = get_call_flow(get(role_gold_drain_type))
                },
            spawn(fun() -> log_gold:set_one(R) end)
    end,
    run_role_gold:set_one(RoleGold#run_role_gold{time = util:unixtime()}).

get_call_flow(undefined) ->
    {current_stacktrace, Stack} = erlang:process_info(self(), current_stacktrace),
    [_, _ | RestStack] = Stack,
    Calls = [{Module, Function} || {Module, Function, Arity, Location} <- RestStack, Module =/= proc_lib],
    Res = ?T2B(Calls),
    binary:replace(Res, <<"'">>, <<"">>, [global]);
get_call_flow(DrainType) ->
    <<>>.

put_gold_drain_type_and_drain_id(DrainType, DrainId, DrainCount) ->
    put(role_gold_drain_type, DrainType),
    put(role_gold_drain_id, DrainId),
    put(role_gold_drain_count, DrainCount),
    ok.

clear_gold_drain_type_and_drain_id() ->
    erase(role_gold_drain_type),
    erase(role_gold_drain_id),
    erase(role_gold_drain_count),
    ok.

