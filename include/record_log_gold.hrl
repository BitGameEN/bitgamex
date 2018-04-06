%%%--------------------------------------------------------
%%% @Module: log_gold
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(log_gold, {
		key_id,
		id = 0, % 自增id
		game_id = 0, % 游戏id
		player_id = 0, % 玩家id
		gold_type = <<"">>, % 币种：bgx, btc, eth, ...
		delta = 0, % 变化量，负数表示消耗
		old_value = 0, % 旧值
		new_value = 0, % 新值
		drain_type = <<"">>, % 来源类型
		drain_id = 0, % 来源相关id，比如买道具，则为道具id
		drain_count = 0, % 来源相关id对应的数量，比如买道具，买了多少个
		time = 0, % 时间戳
		call_flow = <<"">> % 调用上下文
}).
