%%%--------------------------------------------------------
%%% @Module: cfg_gold_type
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(cfg_gold_type).
-export([get/1, get_ids/0]).
-include("common.hrl").
-include("gameConfig.hrl").
-include("record_cfg_gold_type.hrl").

get_ids() ->
	[<<"ACT">>,<<"BGX">>,<<"BTC">>,<<"ELA">>,<<"ETH">>,<<"MAN">>,<<"PLY">>].

get(<<"ACT">>) ->
	#gold_type{
		gold_type = <<"ACT"/utf8>>,
		mining_start_time = 1529668215,
		mining_output_first_day = 100.0,
		half_life_days = 730,
		chain_type = <<"act"/utf8>>,
		amount = 1000000
	};

get(<<"BGX">>) ->
	#gold_type{
		gold_type = <<"BGX"/utf8>>,
		mining_start_time = 1529668215,
		mining_output_first_day = 1.0e4,
		half_life_days = 730,
		chain_type = <<"eth"/utf8>>,
		amount = 1500000000
	};

get(<<"BTC">>) ->
	#gold_type{
		gold_type = <<"BTC"/utf8>>,
		mining_start_time = 1529668215,
		mining_output_first_day = 0.01,
		half_life_days = 730,
		chain_type = <<"btc"/utf8>>,
		amount = 1000
	};

get(<<"ELA">>) ->
	#gold_type{
		gold_type = <<"ELA"/utf8>>,
		mining_start_time = 1529668215,
		mining_output_first_day = 1.0,
		half_life_days = 730,
		chain_type = <<"ela"/utf8>>,
		amount = 100000
	};

get(<<"ETH">>) ->
	#gold_type{
		gold_type = <<"ETH"/utf8>>,
		mining_start_time = 1529668215,
		mining_output_first_day = 0.2,
		half_life_days = 730,
		chain_type = <<"eth"/utf8>>,
		amount = 10000
	};

get(<<"MAN">>) ->
	#gold_type{
		gold_type = <<"MAN"/utf8>>,
		mining_start_time = 1529668215,
		mining_output_first_day = 100.0,
		half_life_days = 730,
		chain_type = <<"eth"/utf8>>,
		amount = 1000000
	};

get(<<"PLY">>) ->
	#gold_type{
		gold_type = <<"PLY"/utf8>>,
		mining_start_time = 1529668215,
		mining_output_first_day = 1.0e4,
		half_life_days = 730,
		chain_type = <<"eth"/utf8>>,
		amount = 21000000
	};

get(_) ->
	null.

