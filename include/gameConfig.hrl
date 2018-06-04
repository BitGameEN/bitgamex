%%%------------------------------------------------
%%% File    : gameConfig.hrl
%%% Description: 游戏逻辑定义
%%%------------------------------------------------

-define(GAME_TYPE_MINING, 0).
-define(GAME_TYPE_NON_MINING, 1).

-define(GOLD_TRANSFER_TYPE_IN_GAME, 0).
-define(GOLD_TRANSFER_TYPE_GAME_TO_XCHG, 1).
-define(GOLD_TRANSFER_TYPE_GAME_TO_WALLET, 2).
-define(GOLD_TRANSFER_TYPE_XCHG_TO_GAME, 3).

-define(GOLD_TRANSFER_TX_TYPE_IN_GAME, 0).
-define(GOLD_TRANSFER_TX_TYPE_GAME_TO_XCHG, 1).
-define(GOLD_TRANSFER_TX_TYPE_XCHG_TO_GAME, 2).

-define(SUPPORT_GOLD_TYPES, [<<"BGX">>, <<"BTC">>, <<"ETH">>, <<"ELA">>, <<"PLY">>]).