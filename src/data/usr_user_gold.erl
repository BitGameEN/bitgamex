%%%--------------------------------------------------------
%%% @Module: usr_user_gold
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_user_gold).
-export([get_one/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_user_gold.hrl").

get_one(Player_id = Id) ->
	case run_data:in_trans() of
		true ->
			case run_data:trans_get(usr_user_gold, Id) of
				[] ->
					get_one_(Id);
				trans_deleted -> [];
				R -> R
			end;
		false ->
			get_one_(Id)
	end.

get_one_(Player_id = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select player_id,gold,time from user_gold where player_id=?">>, [Player_id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_user_gold.key_id), R),
					R
			end
	end.

set_field(Player_id = Id, Field, Val) ->
	Fields = record_info(fields, usr_user_gold),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_user_gold) ->
	case R0#usr_user_gold.key_id =:= undefined of
		false ->
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_user_gold, R0#usr_user_gold.key_id, R0,
						void,
						void);
				false ->
					syncdb(R0),
					cache:set(cache_key(R0#usr_user_gold.key_id), R0)
			end,
			R0#usr_user_gold.key_id;
		true ->
			#usr_user_gold{
				player_id = Player_id,
				gold = Gold,
				time = Time
			} = R0,
			R = R0#usr_user_gold{key_id = Player_id},
			F = fun() ->
					run_data:db_write(add, R, fun() -> 1 = db_esql:execute(?DB_USR, io_lib:format(<<"insert into user_gold(player_id,gold,time) values(~p,'~s',~p)">>,
						[Player_id, Gold, Time])) end),
					cache:set(cache_key(R#usr_user_gold.key_id), R)
				end,
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_user_gold, R#usr_user_gold.key_id, {trans_inserted, R}, F, fun() -> usr_user_gold:del_one(R, true) end);
				false ->
					F()
			end,
			R#usr_user_gold.key_id
	end.

del_one(R) when is_record(R, usr_user_gold) ->
	case run_data:in_trans() of
		true ->
			run_data:trans_del(usr_user_gold, R#usr_user_gold.key_id,
				fun() -> usr_user_gold:del_one(R) end,
				void);
		false ->
			Player_id = R#usr_user_gold.key_id,
			run_data:db_write(del, R, fun() -> db_esql:execute(?DB_USR, <<"delete from user_gold where player_id=?">>, [Player_id]) end),
			cache:del(cache_key(R#usr_user_gold.key_id))
	end,
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select player_id from user_gold limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_user_gold) ->
	#usr_user_gold{
		player_id = Player_id,
		gold = Gold,
		time = Time
	} = R,
	run_data:db_write(upd, R, fun() -> db_esql:execute(?DB_USR, io_lib:format(<<"insert into user_gold(player_id,gold,time) values(?,?,?) on duplicate key update "
		"gold = ?, time = ?">>, []),
		[Player_id, Gold, Time, Gold, Time]) end).

build_record_from_row([Player_id, Gold, Time]) ->
	#usr_user_gold{
		key_id = Player_id,
		player_id = Player_id,
		gold = Gold,
		time = Time
	}.

cache_key(Player_id = Id) ->
	list_to_binary(io_lib:format(<<"usr_user_gold_~p">>, [Player_id])).

