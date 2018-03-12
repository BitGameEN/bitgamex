%%%--------------------------------------------------------
%%% @Module: log_player_login
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(log_player_login, {
		key_id,
		id = 0, % 自增id
		game_id = 0, % 游戏id
		player_id = 0, % 玩家id
		device_id = <<"">>, % 设备id
		device_model = <<"">>, % 设备型号
		os_type = <<"">>, % 操作系统类型
		os_ver = <<"">>, % 操作系统版本
		ip = <<"">>, % ip地址
		lang = <<"">>, % 语言
		time = 0 % 时间戳
}).
