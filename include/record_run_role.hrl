%%%--------------------------------------------------------
%%% @Module: run_role
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(run_role, {
		key_id,
		player_id = 0, % 玩家id（用户id）
		ver = 0, % 数据结构版本
		game_id = 0, % 游戏id
		create_time = 0, % 创建时间
		last_login_time = 0, % 最后登陆时间
		last_login_ip = <<"">>, % 最后登陆IP
		game_data = <<"">>, % 游戏数据
		old_game_data = <<"">>, % 老的游戏数据（出错回档用）
		power = 1, % 原力值
		time = 0 % 更新时间戳
}).
