%%%--------------------------------------------------------
%%% @Module: usr_user_gold_to_draw
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_user_gold_to_draw).
-export([get_one/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_user_gold_to_draw.hrl").

get_one(Player_id = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select player_id,gold_list,time from user_gold_to_draw where player_id=?">>, [Player_id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_user_gold_to_draw.key_id), R),
					R
			end
	end.

set_field(Player_id = Id, Field, Val) ->
	Fields = record_info(fields, usr_user_gold_to_draw),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_user_gold_to_draw) ->
	case R0#usr_user_gold_to_draw.key_id =:= undefined of
		false ->
			syncdb(R0),
			cache:set(cache_key(R0#usr_user_gold_to_draw.key_id), R0),
			R0#usr_user_gold_to_draw.key_id;
		true ->
			#usr_user_gold_to_draw{
				player_id = Player_id,
				gold_list = Gold_list,
				time = Time
			} = R0,
			{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_USR, io_lib:format(<<"insert into user_gold_to_draw(player_id,gold_list,time) values(~p,'~s',~p); select last_insert_id()">>,
				[Player_id, ?T2B(Gold_list), Time])),
			R = R0#usr_user_gold_to_draw{key_id = Insert_id, player_id = Insert_id},
			cache:set(cache_key(R#usr_user_gold_to_draw.key_id), R),
			R#usr_user_gold_to_draw.key_id
	end.

del_one(R) when is_record(R, usr_user_gold_to_draw) ->
	Player_id = R#usr_user_gold_to_draw.key_id,
	db_esql:execute(?DB_USR, <<"delete from user_gold_to_draw where player_id=?">>, [Player_id]),
	cache:del(cache_key(R#usr_user_gold_to_draw.key_id)),
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select player_id from user_gold_to_draw limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_user_gold_to_draw) ->
	#usr_user_gold_to_draw{
		player_id = Player_id,
		gold_list = Gold_list,
		time = Time
	} = R,
	db_esql:execute(?DB_USR, <<"replace into user_gold_to_draw(player_id,gold_list,time) values(?,?,?)">>,
		[Player_id, ?T2B(Gold_list), Time]).

build_record_from_row([Player_id, Gold_list, Time]) ->
	#usr_user_gold_to_draw{
		key_id = Player_id,
		player_id = Player_id,
		gold_list = ?B2T(Gold_list),
		time = Time
	}.

cache_key(Player_id = Id) ->
	list_to_binary(io_lib:format(<<"usr_user_gold_to_draw_~p">>, [Player_id])).

