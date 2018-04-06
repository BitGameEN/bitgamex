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
	[<<"BGX">>].

get(<<"BGX">>) ->
	#gold_type{
		gold_type = <<"BGX"/utf8>>,
		mining_start_time = 1525017600,
		mining_output_first_day = 1027397.26,
		half_life_days = 730
	};

get(_) ->
	null.

