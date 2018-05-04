%%%--------------------------------------------------------
%%% @Module: usr_game
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_game, {
		key_id,
		game_id = 0, % 游戏id
		game_name = <<"">>, % 游戏名
		open_status = 0, % 游戏状态，0-close, 1-open
		game_key = <<"">>, % 游戏固定key，用于登录校验
		balance_lua_f = <<"">>, % 结算lua脚本函数代码
		hard_coef = 1, % 难度系数，难度高给分紧的：> 1，难度低给分松的：< 1，其余：= 1
		trusteeship_exuserid = 0, % 游戏信用金托管账户游戏方无权使用，给用户提取使用
		cp_name = <<"">>, % 开发商名称
		cp_exuserid = 0, % 开发商支取账号，用户消耗代币行为收取利润账户
		ip_list = <<"">>, % IP列表
		token_symbol_list = <<"">>, % 游戏可交易代币列表
		game_type = 0 % 游戏类型，0:挖矿,1:不挖矿
}).
