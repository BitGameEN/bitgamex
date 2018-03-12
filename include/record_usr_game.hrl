%%%--------------------------------------------------------
%%% @Module: usr_game
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_game, {
		key_id,
		game_id = 0, % 游戏id
		game_name = <<"">>, % 游戏名
		open_status = 0, % 游戏状态，0-close, 1-open
		game_key = <<"">> % 游戏固定key，用于登录校验
}).
