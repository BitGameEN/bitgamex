%%%--------------------------------------------------------
%%% @Module: gold_type
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(gold_type, {
		gold_type = <<""/utf8>>, % 币种：BGX, BTC, ETH, ...
		mining_start_time = 0, % 挖矿起始时间戳
		mining_output_first_day = 0.00, % 挖矿第一天产出量
		half_life_days = 0, % 半衰期，多少天产出减半
		chain_type = <<""/utf8>>, % 链类型
		amount = 0 % 总量（如果需要调整产出速率时，产出开始时间也需要修改为新规则生效时间，并结算之前的产出，剩余的量为新的总量）
}).
