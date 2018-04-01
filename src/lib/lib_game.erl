%%%--------------------------------------
%%% @Module  : lib_game
%%% @Description: 游戏相关处理
%%%--------------------------------------
-module(lib_game).
-export([set_gamesvr_num/1, gamesvr_num/0, game_hard_coef/1]).

-include("common.hrl").
-include("record_usr_game.hrl").


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

