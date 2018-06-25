%%%------------------------------------------------
%%% File    : gameConfig.hrl
%%% Description: 游戏逻辑定义
%%%------------------------------------------------

% 游戏类型（挖矿、非挖矿）
-define(GAME_TYPE_MINING, 0).
-define(GAME_TYPE_NON_MINING, 1).

% 游戏币转账类型
-define(GOLD_TRANSFER_TYPE_IN_GAME, 0).
-define(GOLD_TRANSFER_TYPE_GAME_TO_XCHG, 1).
-define(GOLD_TRANSFER_TYPE_GAME_TO_WALLET, 2).
-define(GOLD_TRANSFER_TYPE_XCHG_TO_GAME, 3).

% 游戏币转账事务类型
-define(GOLD_TRANSFER_TX_TYPE_IN_GAME, 0).
-define(GOLD_TRANSFER_TX_TYPE_GAME_TO_XCHG, 1).
-define(GOLD_TRANSFER_TX_TYPE_XCHG_TO_GAME, 2).

% 支持的游戏币类型
-define(SUPPORT_GOLD_TYPES, cfg_gold_type:get_ids()).

% 游戏币挖矿来源
-define(MINING_DRAIN_TYPE_LOGIN, 0).
-define(MINING_DRAIN_TYPE_SAVEGAME, 1).
-define(MINING_DRAIN_TYPE_RANDOM, 2).

