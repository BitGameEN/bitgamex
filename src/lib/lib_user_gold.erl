%%%--------------------------------------
%%% @Module  : lib_user_gold
%%% @Description: 用户金币相关处理
%%%--------------------------------------
-module(lib_user_gold).
-export([add_gold/3, put_gold_drain_type_and_drain_id/3]).

-include("common.hrl").
-include("record_usr_user.hrl").
-include("record_usr_user_gold.hrl").
-include("record_log_gold_on_user.hrl").

add_gold(PlayerId, GoldType, 0) ->
    ok;
add_gold(PlayerId, GoldType, DeltaGold) ->
    {true, Cas} = lock(PlayerId),
    try
        UserGold = usr_user_gold:get_one(PlayerId),
        RawGold = UserGold#usr_user_gold.gold,
        OldGold = ?G(RawGold, GoldType),
        NewGold = OldGold + DeltaGold,
        case NewGold < 0 of
            true -> throw({?ERRNO_GOLD_NOT_ENOUGH, <<"gold not enough">>});
            false -> void
        end,
        usr_user_gold:set_one(UserGold#usr_user_gold{gold = ?G(RawGold, GoldType, NewGold), time = util:unixtime()}),
        User = usr_user:get_one(PlayerId),
        R = #log_gold_on_user{
                player_id = PlayerId,
                game_id = User#usr_user.current_game_id,
                gold_type = GoldType,
                delta = DeltaGold,
                old_value = OldGold,
                new_value = NewGold,
                drain_type = case get(user_gold_drain_type) of undefined -> <<>>; V -> V end,
                drain_id = case get(user_gold_drain_id) of undefined -> 0; V -> V end,
                drain_count = case get(user_gold_drain_count) of undefined -> 0; V -> V end,
                time = util:unixtime(),
                call_flow = get_call_flow(get(user_gold_drain_type))
            },
        spawn(fun() -> log_gold_on_user:set_one(R) end),
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
    list_to_binary(io_lib:format(<<"lock_user_gold_~p">>, [PlayerId])).

get_call_flow(undefined) ->
    {current_stacktrace, Stack} = erlang:process_info(self(), current_stacktrace),
    [_, _ | RestStack] = Stack,
    Calls = [{Module, Function} || {Module, Function, Arity, Location} <- RestStack, Module =/= proc_lib],
    Res = ?T2B(Calls),
    binary:replace(Res, <<"'">>, <<"">>, [global]);
get_call_flow(DrainType) ->
    <<>>.

put_gold_drain_type_and_drain_id(DrainType, DrainId, DrainCount) ->
    put(user_gold_drain_type, DrainType),
    put(user_gold_drain_id, DrainId),
    put(user_gold_drain_count, DrainCount),
    ok.

clear_gold_drain_type_and_drain_id() ->
    erase(user_gold_drain_type),
    erase(user_gold_drain_id),
    erase(user_gold_drain_count),
    ok.

