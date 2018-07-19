%%%--------------------------------------------------------
%%% @Module: usr_game_package
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_game_package, {
		key_id,
		game_id = 0, % 游戏id
		package_id = 0, % 包id
		mining_rule = [], % erlang, 格式例子：[{'BGX', 30}, {'BTC', 10}, {'ETH', 10}, {'ELA', 50}]
		mining_pools = [] % erlang, 格式：[{gold_type, mining_start_time, mining_output_first_day, half_life_days, chain_type, amount}]
}).
