%%%--------------------------------------------------------
%%% @Module: log_gold_on_user
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(log_gold_on_user).
-export([get_one/1, set_one/1, build_record_from_row/1]).
-include("common.hrl").
-include("record_log_gold_on_user.hrl").

get_one(Id) ->
	case db_esql:get_row(?DB_LOG, <<"select id,game_id,player_id,delta,old_value,new_value,drain_type,drain_id,drain_count,time,call_flow from gold_on_user where id=?">>, [Id]) of
		[] -> [];
		Row -> build_record_from_row(Row)
	end.

set_one(R0) when is_record(R0, log_gold_on_user) ->
	case R0#log_gold_on_user.key_id =:= undefined of
		false ->
			syncdb(R0),
			R0#log_gold_on_user.key_id;
		true ->
			#log_gold_on_user{
				id = Id,
				game_id = Game_id,
				player_id = Player_id,
				delta = Delta,
				old_value = Old_value,
				new_value = New_value,
				drain_type = Drain_type,
				drain_id = Drain_id,
				drain_count = Drain_count,
				time = Time,
				call_flow = Call_flow
			} = R0,
			{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_LOG, io_lib:format(<<"insert into gold_on_user(id,game_id,player_id,delta,old_value,new_value,drain_type,drain_id,drain_count,time,call_flow) values(~p,~p,~p,~p,~p,~p,'~s',~p,~p,~p,'~s'); select last_insert_id()">>,
				[Id, Game_id, Player_id, Delta, Old_value, New_value, Drain_type, Drain_id, Drain_count, Time, Call_flow])),
			R = R0#log_gold_on_user{key_id = Insert_id, id = Insert_id},
			R#log_gold_on_user.key_id
	end.

syncdb(R) when is_record(R, log_gold_on_user) ->
	#log_gold_on_user{
		id = Id,
		game_id = Game_id,
		player_id = Player_id,
		delta = Delta,
		old_value = Old_value,
		new_value = New_value,
		drain_type = Drain_type,
		drain_id = Drain_id,
		drain_count = Drain_count,
		time = Time,
		call_flow = Call_flow
	} = R,
	db_esql:execute(?DB_LOG, <<"replace into gold_on_user(id,game_id,player_id,delta,old_value,new_value,drain_type,drain_id,drain_count,time,call_flow) values(?,?,?,?,?,?,?,?,?,?,?)">>,
		[Id, Game_id, Player_id, Delta, Old_value, New_value, Drain_type, Drain_id, Drain_count, Time, Call_flow]).

build_record_from_row([Id, Game_id, Player_id, Delta, Old_value, New_value, Drain_type, Drain_id, Drain_count, Time, Call_flow]) ->
	#log_gold_on_user{
		key_id = Id,
		id = Id,
		game_id = Game_id,
		player_id = Player_id,
		delta = Delta,
		old_value = Old_value,
		new_value = New_value,
		drain_type = Drain_type,
		drain_id = Drain_id,
		drain_count = Drain_count,
		time = Time,
		call_flow = Call_flow
	}.

