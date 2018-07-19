%%%--------------------------------------------------------
%%% @Module: usr_user
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_user, {
		key_id,
		id = 0, % 用户id（玩家id）
		hash_id = <<"">>, % 用户哈希id
		user_name = <<"">>, % 用户名
		password = <<"">>, % 登录密码
		player_name = <<"">>, % 玩家名
		avatar = 0, % 玩家头像
		device_id = <<"">>, % 设备id
		org_device_id = <<"">>, % 最初的设备id
		is_bind = 0, % 是否绑定
		ios_gamecenter_id = <<"">>, % 绑定苹果id
		google_id = <<"">>, % 绑定谷歌id
		facebook_id = <<"">>, % 绑定脸书id
		current_game_id = 0, % 当前所在游戏
		current_game_package_id = 0, % 当前所在游戏包id
		current_game_uid = <<"">>, % 当前所在游戏用户标识
		session_token = <<"">>, % 会话令牌
		lang = <<"">>, % 语言
		os_type = <<"">>, % 操作系统类型
		country_code = <<"">>, % 国家编码
		create_time = 0, % 创建时间
		last_login_time = 0, % 上次登录时间
		status = 0, % 玩家状态（0-正常，1-封禁）
		forbid_login_endtime = 0, % 封号截止时间
		bind_xchg_accid = <<"">>, % 绑定的交易所账号id
		bind_xchg_uid = 0, % 绑定的交易所唯一id（bind_xchg_accid可能会变，但该id不变）
		bind_wallet_addr = <<"">>, % 绑定的钱包地址
		time = 0 % 更新时间戳
}).
