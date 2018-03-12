%%%--------------------------------------
%%% @Module  : c_gamesvr
%%% @Description: gamesvr的逻辑处理模块
%%%--------------------------------------
-module(c_gamesvr).
-export([get_game_data/4]).

-include("common.hrl").
-include("record_run_role.hrl").
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
                                    last_login_ip = PeerIp,
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

