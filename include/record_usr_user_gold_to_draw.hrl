%%%--------------------------------------------------------
%%% @Module: usr_user_gold_to_draw
%%% @Description: 自动生成
%%%--------------------------------------------------------

-record(usr_user_gold_to_draw, {
		key_id,
		player_id = 0, % 用户id（玩家id）
		gold_list = [], % erlang，待领金币列表，格式：[{时间戳, 数量}, ...]
		time = 0 % 更新时间戳
}).
