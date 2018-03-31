%%%--------------------------------------
%%% @Module  : lib_user_gold_to_draw
%%% @Description: 用户待领金币相关处理
%%%--------------------------------------
-module(lib_user_gold_to_draw).
-export([add_gold_to_draw/2, put_gold_drain_type_and_drain_id/3]).

-include("common.hrl").
-include("record_usr_user.hrl").
-include("record_usr_user_gold_to_draw.hrl").
-include("record_log_gold_to_draw.hrl").


add_gold_to_draw(PlayerId, GoldListToDraw) when is_list(GoldListToDraw) ->
    {true, Cas} = lock(PlayerId),
    try
        UserGoldToDraw = usr_user_gold_to_draw:get_one(PlayerId),
        OldGoldList = UserGoldToDraw#usr_user_gold_to_draw.gold_list,
        NewGoldList = GoldListToDraw ++ OldGoldList,
        save(UserGoldToDraw#usr_user_gold_to_draw{gold_list = NewGoldList}),
        unlock(PlayerId, Cas),
        ok
    catch
        throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
            unlock(PlayerId, Cas),
            throw({ErrNo, ErrMsg});
        _:ExceptionErr ->
            unlock(PlayerId, Cas),
            ?ERR("add_gold_to_draw exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
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
    list_to_binary(io_lib:format(<<"lock_usr_user_gold_to_draw_~p">>, [PlayerId])).

save(#usr_user_gold_to_draw{player_id = PlayerId, gold_list = NewGoldList} = UserGoldToDraw) ->
    #usr_user_gold_to_draw{gold_list = OldGoldList} = usr_user_gold_to_draw:get_one(PlayerId),
    OldGoldToDraw = lists:sum([V || {_, V} <- OldGoldList]),
    NewGoldToDraw = lists:sum([V || {_, V} <- NewGoldList]),
    GoldDelta = NewGoldToDraw - OldGoldToDraw,
    case GoldDelta of
        0 -> void;
        _ ->
            User = usr_user:get_one(PlayerId),
            R = #log_gold_to_draw{
                    player_id = PlayerId,
                    game_id = User#usr_user.current_game_id,
                    delta = GoldDelta,
                    old_value = OldGoldToDraw,
                    new_value = NewGoldToDraw,
                    drain_type = case get(gold_drain_type) of undefined -> <<>>; V -> V end,
                    drain_id = case get(gold_drain_id) of undefined -> 0; V -> V end,
                    drain_count = case get(gold_drain_count) of undefined -> 0; V -> V end,
                    time = util:unixtime(),
                    call_flow = get_call_flow(get(gold_drain_type))
                },
            spawn(fun() -> log_gold_to_draw:set_one(R) end)
    end,
    usr_user_gold_to_draw:set_one(UserGoldToDraw#usr_user_gold_to_draw{time = util:unixtime()}).

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

