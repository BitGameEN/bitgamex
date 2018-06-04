%%%--------------------------------------
%%% @Module  : c_gamesvr
%%% @Description: gamesvr的逻辑处理模块
%%%--------------------------------------
-module(c_gamesvr).
-export([get_game_data/4,
         save_game_data/3,
         transfer_coin_in_game/6,
         get_coin_list_to_draw/2,
         draw_coin/3,
         consume_coin/4]).

-include("common.hrl").
-include("gameConfig.hrl").
-include("gameConfigGlobalKey.hrl").
-include("record.hrl").
-include("record_run_role.hrl").
-include("record_run_role_gold.hrl").
-include("record_run_role_gold_to_draw.hrl").
-include("record_usr_game.hrl").
-include("record_usr_user.hrl").
-include("record_usr_user_gold.hrl").
-include("record_usr_gold_transfer.hrl").

%% 获取玩家的游戏数据
get_game_data(GameId, UserId, DoLogin, LoginArgs) ->
  try
    run_data:trans_begin(),

    GameData =
        case run_role:get_one({GameId, UserId}) of
            [] -> % 在该游戏中尚无角色
                case DoLogin of
                    true ->
                        [Now, PeerIp] = LoginArgs,
                        run_role:set_one(
                          #run_role{player_id = UserId,
                                    game_id = GameId,
                                    create_time = Now,
                                    last_login_time = Now,
                                    last_login_ip = ?T2B(PeerIp),
                                    time = Now}),
                        run_role_gold:set_one(#run_role_gold{player_id = UserId, game_id = GameId, gold = <<"{}">>, time = Now}),
                        run_role_gold_to_draw:set_one(#run_role_gold_to_draw{player_id = UserId, game_id = GameId,
                                                                             gold_list = [{Now, lib_mining:get_gold_type(GameId), 0.01}], time = Now});
                    false -> void
                end,
                <<>>;
            Role ->
                case DoLogin of
                    true ->
                        [Now, PeerIp] = LoginArgs,
                        run_role:set_one(
                          Role#run_role{last_login_time = Now,
                                        last_login_ip = ?T2B(PeerIp),
                                        time = Now}),
                        % 登录给金币
                        lib_mining:distribute_login_delta_golds(UserId, GameId, Now - Role#run_role.last_login_time);
                    false -> void
                end,
                Role#run_role.game_data
        end,

    UserGold = usr_user_gold:get_one(UserId),
    UserBalance = UserGold#usr_user_gold.gold,
    RoleGold = run_role_gold:get_one({GameId, UserId}),
    RoleBalance = RoleGold#run_role_gold.gold,

    run_data:trans_commit(),
    {ok, GameData, UserBalance, RoleBalance}

  catch
    throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
        run_data:trans_rollback(),
        throw({ErrNo, ErrMsg});
    _:ExceptionErr ->
        run_data:trans_rollback(),
        ?ERR("get_game_data exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
        throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

save_game_data(GameId, UserId, GameData) ->
  try
    run_data:trans_begin(),

    Now = util:unixtime(),
    #usr_game{balance_lua_f = BalanceLuaF} = usr_game:get_one(GameId),
    #run_role{game_data = OldGameData} = Role = run_role:get_one({GameId, UserId}),
    % todo：先假设游戏每次只给一种币，后面再扩展
    {GoldType, DeltaGold} =
        case BalanceLuaF of
            <<>> -> {?DEFAULT_GOLD_TYPE, 0};
            _ ->
                % 结算脚本函数，比如：
                %package.path = package.path .. ";../priv/?.lua;"
                %json = require "json"
                %
                %function f(s0, s)
                %  t = json.decode(s)
                %  return t["score"] * 0.1
                %end
                ?DBG("balance_lua_f:~n~s~n", [BalanceLuaF]),
                LuaState0 = luerl:init(),
                {_, LuaState} = luerl:do(BalanceLuaF, LuaState0),
                {[V], _} = luerl:call_function([f], [OldGameData, GameData], LuaState),
                {lib_mining:get_gold_type(GameId), V}
        end,

    case DeltaGold > 0 of
        true ->
            mod_distributor:req_add_balance(#add_gold_req{uid = UserId, game_id = GameId, gold_type = GoldType, delta_gold = util:round5d(DeltaGold), time = Now});
        false ->
            lib_role_gold:put_gold_drain_type_and_drain_id(save_game_data, GameId, DeltaGold),
            lib_role_gold:add_gold(UserId, GameId, GoldType, DeltaGold),
            run_role:set_one(Role#run_role{game_data = GameData, old_game_data = OldGameData, time = Now})
    end,

    RoleGold = run_role_gold:get_one({GameId, UserId}),

    run_data:trans_commit(),
    {ok, GameData, RoleGold#run_role_gold.gold, DeltaGold}

  catch
    throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
        run_data:trans_rollback(),
        throw({ErrNo, ErrMsg});
    _:ExceptionErr ->
        run_data:trans_rollback(),
        ?ERR("save_game_data exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
        throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

transfer_coin_in_game(GameId, UserId, DstUserId, GoldType, Amount, ReceiptData) ->
  try
    run_data:trans_begin(),

    DstRole = run_role:get_one({GameId, DstUserId}),
    case is_record(DstRole, run_role) of
        true -> void;
        false -> throw({-2, <<"dst role does not exist">>})
    end,

    TransferDiscountInGame = lib_global_config:get(?GLOBAL_CONFIG_KEY_TRANSFER_DISCOUNT_IN_GAME),
    User = usr_user:get_one(UserId),
    lib_role_gold:put_gold_drain_type_and_drain_id(gold_transfer, ?GOLD_TRANSFER_TYPE_IN_GAME, Amount),
    lib_role_gold:add_gold(UserId, GameId, GoldType, -Amount),
    lib_role_gold:add_gold(DstUserId, GameId, GoldType, Amount * (1 - TransferDiscountInGame)),
    lib_game:put_gold_drain_type_and_drain_id(gold_transfer, ?GOLD_TRANSFER_TYPE_IN_GAME, Amount),
    lib_game:add_reclaimed_gold(GameId, GoldType, Amount * TransferDiscountInGame),
    NowDateTime = util:now_datetime_str(),
    TransactionId = integer_to_list(UserId) ++ "_" ++ integer_to_list(DstUserId) ++ "_" ++ integer_to_list(util:longunixtime()),
    TransferR = #usr_gold_transfer{
                    type = ?GOLD_TRANSFER_TYPE_IN_GAME,
                    transaction_type = ?GOLD_TRANSFER_TX_TYPE_IN_GAME,
                    transaction_id = TransactionId,
                    receipt = ReceiptData,
                    player_id = UserId,
                    device_id = User#usr_user.device_id,
                    wallet_addr = <<>>,
                    gold_type = GoldType,
                    gold = Amount,
                    status = 1,
                    error_tag = <<>>,
                    receive_game_id = GameId,
                    receive_time = NowDateTime,
                    update_time = NowDateTime},
    usr_gold_transfer:set_one(TransferR),
    RoleGold = run_role_gold:get_one({GameId, UserId}),

    run_data:trans_commit(),
    {ok, RoleGold#run_role_gold.gold}

  catch
    throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
        run_data:trans_rollback(),
        throw({ErrNo, ErrMsg});
    _:ExceptionErr ->
        run_data:trans_rollback(),
        ?ERR("transfer_coin_in_game exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
        throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

get_coin_list_to_draw(GameId, UserId) ->
    #run_role_gold_to_draw{gold_list = GoldList0} = run_role_gold_to_draw:get_one({GameId, UserId}),
    {ok, {[{integer_to_binary(TimeKey), {[{coin_type, GoldType}, {amount, Amount}]}} || {TimeKey, GoldType, Amount} <- GoldList0]}}.

draw_coin(GameId, UserId, CoinId) ->
  try
    run_data:trans_begin(),

    #run_role_gold_to_draw{gold_list = GoldListToDraw} = run_role_gold_to_draw:get_one({GameId, UserId}),
    case lists:keyfind(CoinId, 1, GoldListToDraw) of
        false ->
            throw({-1, <<"the specified coin id was not found">>});
        {_, GoldType, Amount} ->
            lib_role_gold:put_gold_drain_type_and_drain_id(draw_coin, GoldType, Amount),
            lib_role_gold:add_gold(UserId, GameId, GoldType, Amount),
            lib_role_gold_to_draw:put_gold_drain_type_and_drain_id(draw_coin, GoldType, Amount),
            lib_role_gold_to_draw:delete_gold_to_draw(UserId, GameId, CoinId),
            RoleGold = run_role_gold:get_one({GameId, UserId}),
            run_data:trans_commit(),
            {ok, RoleGold#run_role_gold.gold}
    end

  catch
    throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
        run_data:trans_rollback(),
        throw({ErrNo, ErrMsg});
    _:ExceptionErr ->
        run_data:trans_rollback(),
        ?ERR("draw_coin exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
        throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

consume_coin(GameId, UserId, GoldType, Amount) ->
  try
    run_data:trans_begin(),

    lib_role_gold:put_gold_drain_type_and_drain_id(consume_coin, GoldType, Amount),
    lib_role_gold:add_gold(UserId, GameId, GoldType, -Amount),
    lib_game:put_gold_drain_type_and_drain_id(consume_coin, GoldType, Amount),
    lib_game:add_reclaimed_gold(GameId, GoldType, Amount),
    RoleGold = run_role_gold:get_one({GameId, UserId}),
    run_data:trans_commit(),
    {ok, RoleGold#run_role_gold.gold}

  catch
    throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
        run_data:trans_rollback(),
        throw({ErrNo, ErrMsg});
    _:ExceptionErr ->
        run_data:trans_rollback(),
        ?ERR("consume_coin exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
        throw({?ERRNO_EXCEPTION, ?T2B(ExceptionErr)})
    end.

