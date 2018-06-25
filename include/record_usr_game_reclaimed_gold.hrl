%%%--------------------------------------------------------
%%% @Module: usr_game_reclaimed_gold
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_game_reclaimed_gold, {
		key_id,
		game_id = 0, % 游戏id
		gold = <<"">>, % 游戏回收的总金币数，json格式：{"BGX":数量, "BTC":数量, "ETH":数量, ...}
		time = 0 % 时间戳
}).
