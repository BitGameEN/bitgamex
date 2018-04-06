%%%--------------------------------------------------------
%%% @Module: usr_gold_transfer
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_gold_transfer, {
		key_id,
		id = 0, % 唯一id
		type = 0, % 类型：0 - in game, 1 - game to exchange, 2 - game to wallet, 3 - exchange to game
		transaction_type = 0, % 类型：0 - in game, 1 - game to exchange, 2 - exchange to game
		transaction_id = <<"">>, % 转账事务id
		receipt = <<"">>, % 收据
		player_id = 0, % 玩家id
		device_id = <<"">>, % device id
		xchg_accid = <<"">>, % 交易所账号id
		wallet_addr = <<"">>, % 钱包地址
		gold_type = <<"">>, % 币种：bgx, btc, eth, ...
		gold = 0, % 金币
		status = 0, % 回调状态，0 - 未回调，1 - 已成功回调，-1 - 回调返回失败结果
		error_tag = <<"0">>, % 回调未成功时的错误号
		receive_game_id = 0, % 收到时的游戏id
		receive_time = 0, % 收到时间
		update_time = 0 % 更新时间
}).
