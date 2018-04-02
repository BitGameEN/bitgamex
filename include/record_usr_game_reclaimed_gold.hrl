%%%--------------------------------------------------------
%%% @Module: usr_game_reclaimed_gold
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_game_reclaimed_gold, {
		key_id,
		game_id = 0, % 游戏id
		gold = 0, % 游戏回收的总金币数
		time = 0 % 时间戳
}).
