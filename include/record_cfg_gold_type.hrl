%%%--------------------------------------------------------
%%% @Module: gold_type
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(gold_type, {
		gold_type = <<""/utf8>>, % 币种：BGX, BTC, ETH, ...
		mining_start_time = 0, % 挖矿起始时间戳
		mining_output_first_day = 0.00, % 挖矿第一天产出量
		half_life_days = 0 % 半衰期，多少天产出减半
}).
