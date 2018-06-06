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
		mining_start_time = 1525017600,
		mining_output_first_day = 100.0,
		half_life_days = 730
	};

get(<<"BGX">>) ->
	#gold_type{
		gold_type = <<"BGX"/utf8>>,
		mining_start_time = 1525017600,
		mining_output_first_day = 1027397.26,
		half_life_days = 730
	};

get(<<"BTC">>) ->
	#gold_type{
		gold_type = <<"BTC"/utf8>>,
		mining_start_time = 1525017600,
		mining_output_first_day = 100.0,
		half_life_days = 730
	};

get(<<"ELA">>) ->
	#gold_type{
		gold_type = <<"ELA"/utf8>>,
		mining_start_time = 1525017600,
		mining_output_first_day = 100.0,
		half_life_days = 730
	};

get(<<"ETH">>) ->
	#gold_type{
		gold_type = <<"ETH"/utf8>>,
		mining_start_time = 1525017600,
		mining_output_first_day = 100.0,
		half_life_days = 730
	};

get(<<"MAN">>) ->
	#gold_type{
		gold_type = <<"MAN"/utf8>>,
		mining_start_time = 1525017600,
		mining_output_first_day = 100.0,
		half_life_days = 730
	};

get(<<"PLY">>) ->
	#gold_type{
		gold_type = <<"PLY"/utf8>>,
		mining_start_time = 1528092516,
		mining_output_first_day = 1.0e5,
		half_life_days = 730
	};

get(_) ->
	null.

