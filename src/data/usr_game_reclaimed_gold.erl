%%%--------------------------------------------------------
%%% @Module: usr_game_reclaimed_gold
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_game_reclaimed_gold).
-export([get_one/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_game_reclaimed_gold.hrl").

get_one(Game_id = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select game_id,gold,time from game_reclaimed_gold where game_id=?">>, [Game_id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_game_reclaimed_gold.key_id), R),
					R
			end
	end.

set_field(Game_id = Id, Field, Val) ->
	Fields = record_info(fields, usr_game_reclaimed_gold),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_game_reclaimed_gold) ->
	case R0#usr_game_reclaimed_gold.key_id =:= undefined of
		false ->
			syncdb(R0),
			cache:set(cache_key(R0#usr_game_reclaimed_gold.key_id), R0),
			R0#usr_game_reclaimed_gold.key_id;
		true ->
			#usr_game_reclaimed_gold{
				game_id = Game_id,
				gold = Gold,
				time = Time
			} = R0,
			{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_USR, io_lib:format(<<"insert into game_reclaimed_gold(game_id,gold,time) values(~p,'~s',~p); select last_insert_id()">>,
				[Game_id, Gold, Time])),
			R = R0#usr_game_reclaimed_gold{key_id = Insert_id, game_id = Insert_id},
			cache:set(cache_key(R#usr_game_reclaimed_gold.key_id), R),
			R#usr_game_reclaimed_gold.key_id
	end.

del_one(R) when is_record(R, usr_game_reclaimed_gold) ->
	Game_id = R#usr_game_reclaimed_gold.key_id,
	db_esql:execute(?DB_USR, <<"delete from game_reclaimed_gold where game_id=?">>, [Game_id]),
	cache:del(cache_key(R#usr_game_reclaimed_gold.key_id)),
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select game_id from game_reclaimed_gold limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_game_reclaimed_gold) ->
	#usr_game_reclaimed_gold{
		game_id = Game_id,
		gold = Gold,
		time = Time
	} = R,
	db_esql:execute(?DB_USR, <<"replace into game_reclaimed_gold(game_id,gold,time) values(?,?,?)">>,
		[Game_id, Gold, Time]).

build_record_from_row([Game_id, Gold, Time]) ->
	#usr_game_reclaimed_gold{
		key_id = Game_id,
		game_id = Game_id,
		gold = Gold,
		time = Time
	}.

cache_key(Game_id = Id) ->
	list_to_binary(io_lib:format(<<"usr_game_reclaimed_gold_~p">>, [Game_id])).

