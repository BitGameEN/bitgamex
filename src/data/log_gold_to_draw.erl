%%%--------------------------------------------------------
%%% @Module: log_gold_to_draw
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(log_gold_to_draw).
-export([get_one/1, set_one/1, build_record_from_row/1]).
-include("common.hrl").
-include("record_log_gold_to_draw.hrl").

get_one(Id) ->
	case db_esql:get_row(?DB_LOG, <<"select id,game_id,player_id,gold_type,delta,old_value,new_value,drain_type,drain_id,drain_count,time,call_flow from gold_to_draw where id=?">>, [Id]) of
		[] -> [];
		Row -> build_record_from_row(Row)
	end.

set_one(R0) when is_record(R0, log_gold_to_draw) ->
	case R0#log_gold_to_draw.key_id =:= undefined of
		false ->
			syncdb(R0),
			R0#log_gold_to_draw.key_id;
		true ->
			#log_gold_to_draw{
				id = Id,
				game_id = Game_id,
				player_id = Player_id,
				gold_type = Gold_type,
				delta = Delta,
				old_value = Old_value,
				new_value = New_value,
				drain_type = Drain_type,
				drain_id = Drain_id,
				drain_count = Drain_count,
				time = Time,
				call_flow = Call_flow
			} = R0,
			spawn(fun() -> {ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_LOG, io_lib:format(<<"insert into gold_to_draw(id,game_id,player_id,gold_type,delta,old_value,new_value,drain_type,drain_id,drain_count,time,call_flow) values(~p,~p,~p,'~s',~p,~p,~p,'~s','~s',~p,~p,'~s'); select last_insert_id()">>,
				[Id, Game_id, Player_id, Gold_type, Delta, Old_value, New_value, Drain_type, Drain_id, Drain_count, Time, Call_flow])) end)
	end.

syncdb(R) when is_record(R, log_gold_to_draw) ->
	#log_gold_to_draw{
		id = Id,
		game_id = Game_id,
		player_id = Player_id,
		gold_type = Gold_type,
		delta = Delta,
		old_value = Old_value,
		new_value = New_value,
		drain_type = Drain_type,
		drain_id = Drain_id,
		drain_count = Drain_count,
		time = Time,
		call_flow = Call_flow
	} = R,
	spawn(fun() -> db_esql:execute(?DB_LOG, <<"replace into gold_to_draw(id,game_id,player_id,gold_type,delta,old_value,new_value,drain_type,drain_id,drain_count,time,call_flow) values(?,?,?,?,?,?,?,?,?,?,?,?)">>,
		[Id, Game_id, Player_id, Gold_type, Delta, Old_value, New_value, Drain_type, Drain_id, Drain_count, Time, Call_flow]) end).

build_record_from_row([Id, Game_id, Player_id, Gold_type, Delta, Old_value, New_value, Drain_type, Drain_id, Drain_count, Time, Call_flow]) ->
	#log_gold_to_draw{
		key_id = Id,
		id = Id,
		game_id = Game_id,
		player_id = Player_id,
		gold_type = Gold_type,
		delta = Delta,
		old_value = Old_value,
		new_value = New_value,
		drain_type = Drain_type,
		drain_id = Drain_id,
		drain_count = Drain_count,
		time = Time,
		call_flow = Call_flow
	}.

