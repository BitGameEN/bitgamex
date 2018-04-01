%%%--------------------------------------------------------
%%% @Module: run_role_gold
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(run_role_gold, {
		key_id,
		player_id = 0, % 用户id（玩家id）
		ver = 0, % 数据结构版本
		game_id = 0, % 游戏id
		gold = 0, % 金币
		time = 0 % 更新时间戳
}).
