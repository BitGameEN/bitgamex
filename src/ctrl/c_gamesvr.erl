%%%--------------------------------------
%%% @Module  : c_gamesvr
%%% @Description: gamesvr的逻辑处理模块
%%%--------------------------------------
-module(c_gamesvr).
-export([get_game_data/4,
         save_game_data/3]).

-include("common.hrl").
-include("record_run_role.hrl").
-include("record_usr_game.hrl").
-include("record_usr_user_gold.hrl").

%% 获取玩家的游戏数据
get_game_data(GameId, UserId, DoCreate, CreateArgs) ->
    GameData =
        case run_role:get_one({GameId, UserId}) of
            [] -> % 在该游戏中尚无角色
                case DoCreate of
                    true ->
                        [Now, PeerIp] = CreateArgs,
                        run_role:set_one(
                          #run_role{player_id = UserId,
                                    game_id = GameId,
                                    create_time = Now,
                                    last_login_time = Now,
                                    last_login_ip = ?T2B(PeerIp),
                                    time = Now});
                    false -> void
                end,
                <<>>;
            Role ->
                Role#run_role.game_data
        end,
    UserGold = usr_user_gold:get_one(UserId),
    Balance = UserGold#usr_user_gold.gold,
    {ok, GameData, Balance}.

save_game_data(GameId, UserId, GameData) ->
  try
    run_data:trans_begin(),

    Now = util:unixtime(),
    #usr_game{balance_lua_f = BalanceLuaF} = usr_game:get_one(GameId),
    #run_role{game_data = OldGameData} = Role = run_role:get_one({GameId, UserId}),
    DeltaBalance0 =
        case BalanceLuaF of
            <<>> -> 0;
            _ ->
                % 结算脚本函数，比如：
                % function f(s0, s)
                %   return 0.5
                % end
                ?DBG("balance_lua_f:~n~s~n", [BalanceLuaF]),
                LuaState0 = luerl:init(),
                {_, LuaState} = luerl:do(BalanceLuaF, LuaState0),
                {[V], _} = luerl:call_function([f], [OldGameData, GameData], LuaState),
                V
        end,
    % 对基础增量进行额外的规则调整
    DeltaBalance = DeltaBalance0,
    lib_user_gold:put_gold_drain_type_and_drain_id(save_game_data, GameId, DeltaBalance),
    lib_user_gold:add_gold(UserId, DeltaBalance),
    run_role:set_one(Role#run_role{game_data = GameData, old_game_data = OldGameData, time = Now}),

    run_data:trans_commit(),
    UserGold = usr_user_gold:get_one(UserId),
    {ok, GameData, UserGold#usr_user_gold.gold, DeltaBalance0}

  catch
    throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
        run_data:trans_rollback(),
        throw({?ERRNO_EXCEPTION, ErrMsg});
    _:ExceptionErr ->
        run_data:trans_rollback(),
        ?ERR("save_game_data exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
        throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

