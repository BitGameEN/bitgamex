%%%--------------------------------------
%%% @Module  : lib_role_gold_to_draw
%%% @Description: 角色待领金币相关处理
%%%--------------------------------------
-module(lib_role_gold_to_draw).
-export([add_gold_to_draw/4, delete_gold_to_draw/3, put_gold_drain_type_and_drain_id/3]).

-include("common.hrl").
-include("record_run_role_gold_to_draw.hrl").
-include("record_log_gold_to_draw.hrl").

-define(MAX_SIZE_GOLD_LIST, 20).

% 调用者保证T不一样
add_gold_to_draw(PlayerId, GameId, GoldType, GoldListToDraw0) when is_list(GoldListToDraw0) ->
    {true, Cas} = lock(PlayerId),
    try
        GoldListToDraw = [{T, GoldType, V} || {T, V} <- GoldListToDraw0],
        RoleGoldToDraw = run_role_gold_to_draw:get_one({GameId, PlayerId}),
        OldGoldList = RoleGoldToDraw#run_role_gold_to_draw.gold_list,
        OldGoldListOfTheType = [{T, V} || {T, GT, V} <- OldGoldList, GT =:= GoldType],
        NewGoldList = case length(OldGoldList) < ?MAX_SIZE_GOLD_LIST of
                          true -> GoldListToDraw ++ OldGoldList;
                          false ->
                              case OldGoldListOfTheType of
                                  [{HeadT, HeadV} | _] ->
                                      RestGoldList = lists:keydelete(HeadT, 1, OldGoldList),
                                      SumGold = lists:sum([V || {_, V} <- [{HeadT, HeadV} | GoldListToDraw0]]),
                                      MaxTime = lists:max([T || {T, _} <- GoldListToDraw0]),
                                      [{MaxTime, GoldType, SumGold} | RestGoldList];
                                  _ ->
                                      GoldListToDraw ++ OldGoldList
                              end
                      end,
        run_role_gold_to_draw:set_one(RoleGoldToDraw#run_role_gold_to_draw{gold_list = NewGoldList, time = util:unixtime()}),
        OldGoldToDraw = lists:sum([V || {_, V} <- OldGoldListOfTheType]),
        DeltaGoldToDraw = lists:sum([V || {_, V} <- GoldListToDraw0]),
        NewGoldToDraw = OldGoldToDraw + DeltaGoldToDraw,
        R = #log_gold_to_draw{
                player_id = PlayerId,
                game_id = RoleGoldToDraw#run_role_gold_to_draw.game_id,
                gold_type = GoldType,
                delta = DeltaGoldToDraw,
                old_value = OldGoldToDraw,
                new_value = NewGoldToDraw,
                drain_type = case get(role_gold_to_draw_drain_type) of undefined -> <<>>; V -> V end,
                drain_id = case get(role_gold_to_draw_drain_id) of undefined -> 0; V -> V end,
                drain_count = case get(role_gold_to_draw_drain_count) of undefined -> 0; V -> V end,
                time = util:unixtime(),
                call_flow = get_call_flow(get(role_gold_to_draw_drain_type))
            },
        spawn(fun() -> log_gold_to_draw:set_one(R) end),
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

delete_gold_to_draw(PlayerId, GameId, CoinId) ->
    {true, Cas} = lock(PlayerId),
    try
        RoleGoldToDraw = run_role_gold_to_draw:get_one({GameId, PlayerId}),
        GoldList = RoleGoldToDraw#run_role_gold_to_draw.gold_list,
        case lists:keyfind(CoinId, 1, GoldList) of
            false -> void;
            {_, GoldType, Amount} ->
                NewGoldList = lists:keydelete(CoinId, 1, GoldList),
                run_role_gold_to_draw:set_one(RoleGoldToDraw#run_role_gold_to_draw{gold_list = NewGoldList, time = util:unixtime()}),
                OldGoldToDraw = lists:sum([V || {_, GT, V} <- GoldList, GT =:= GoldType]),
                DeltaGoldToDraw = -Amount,
                NewGoldToDraw = lists:sum([V || {_, GT, V} <- NewGoldList, GT =:= GoldType]),
                R = #log_gold_to_draw{
                        player_id = PlayerId,
                        game_id = GameId,
                        gold_type = GoldType,
                        delta = DeltaGoldToDraw,
                        old_value = OldGoldToDraw,
                        new_value = NewGoldToDraw,
                        drain_type = case get(role_gold_to_draw_drain_type) of undefined -> <<>>; V -> V end,
                        drain_id = case get(role_gold_to_draw_drain_id) of undefined -> 0; V -> V end,
                        drain_count = case get(role_gold_to_draw_drain_count) of undefined -> 0; V -> V end,
                        time = util:unixtime(),
                        call_flow = get_call_flow(get(role_gold_to_draw_drain_type))
                    },
                spawn(fun() -> log_gold_to_draw:set_one(R) end)
        end,
        unlock(PlayerId, Cas),
        ok
    catch
        throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
            unlock(PlayerId, Cas),
            throw({ErrNo, ErrMsg});
        _:ExceptionErr ->
            unlock(PlayerId, Cas),
            ?ERR("delete_gold_to_draw exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
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

get_call_flow(undefined) ->
    {current_stacktrace, Stack} = erlang:process_info(self(), current_stacktrace),
    [_, _ | RestStack] = Stack,
    Calls = [{Module, Function} || {Module, Function, Arity, Location} <- RestStack, Module =/= proc_lib],
    Res = ?T2B(Calls),
    binary:replace(Res, <<"'">>, <<"">>, [global]);
get_call_flow(DrainType) ->
    <<>>.

put_gold_drain_type_and_drain_id(DrainType, DrainId, DrainCount) ->
    put(role_gold_to_draw_drain_type, DrainType),
    put(role_gold_to_draw_drain_id, DrainId),
    put(role_gold_to_draw_drain_count, DrainCount),
    ok.

clear_gold_drain_type_and_drain_id() ->
    erase(role_gold_to_draw_drain_type),
    erase(role_gold_to_draw_drain_id),
    erase(role_gold_to_draw_drain_count),
    ok.

