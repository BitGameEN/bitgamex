%%%--------------------------------------------------------
%%% @Module: usr_game
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_game).
-export([get_one/1, get_game_gids_by_open_status/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_game.hrl").

get_one(Game_id = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select game_id,game_name,open_status,game_key,balance_lua_f,hard_coef from game where game_id=?">>, [Game_id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_game.key_id), R),
					R
			end
	end.

get_game_gids_by_open_status(Open_status = Id) ->
	case db_esql:get_all(?DB_USR, <<"select game_id from game where open_status=?">>, [Open_status]) of
		[] -> [];
		Rows ->
			[Game_id_ || [Game_id_ | _] <- Rows]
	end.

set_field(Game_id = Id, Field, Val) ->
	Fields = record_info(fields, usr_game),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_game) ->
	case R0#usr_game.key_id =:= undefined of
		false ->
			syncdb(R0),
			cache:set(cache_key(R0#usr_game.key_id), R0),
			R0#usr_game.key_id;
		true ->
			#usr_game{
				game_id = Game_id,
				game_name = Game_name,
				open_status = Open_status,
				game_key = Game_key,
				balance_lua_f = Balance_lua_f,
				hard_coef = Hard_coef
			} = R0,
			{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_USR, io_lib:format(<<"insert into game(game_id,game_name,open_status,game_key,balance_lua_f,hard_coef) values(~p,'~s',~p,'~s','~s',~p); select last_insert_id()">>,
				[Game_id, util:esc(Game_name), Open_status, Game_key, Balance_lua_f, Hard_coef])),
			R = R0#usr_game{key_id = Insert_id, game_id = Insert_id},
			cache:set(cache_key(R#usr_game.key_id), R),
			R#usr_game.key_id
	end.

del_one(R) when is_record(R, usr_game) ->
	Game_id = R#usr_game.key_id,
	db_esql:execute(?DB_USR, <<"delete from game where game_id=?">>, [Game_id]),
	cache:del(cache_key(R#usr_game.key_id)),
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select game_id from game limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_game) ->
	#usr_game{
		game_id = Game_id,
		game_name = Game_name,
		open_status = Open_status,
		game_key = Game_key,
		balance_lua_f = Balance_lua_f,
		hard_coef = Hard_coef
	} = R,
	db_esql:execute(?DB_USR, <<"replace into game(game_id,game_name,open_status,game_key,balance_lua_f,hard_coef) values(?,?,?,?,?,?)">>,
		[Game_id, util:esc(Game_name), Open_status, Game_key, Balance_lua_f, Hard_coef]).

build_record_from_row([Game_id, Game_name, Open_status, Game_key, Balance_lua_f, Hard_coef]) ->
	#usr_game{
		key_id = Game_id,
		game_id = Game_id,
		game_name = Game_name,
		open_status = Open_status,
		game_key = Game_key,
		balance_lua_f = Balance_lua_f,
		hard_coef = Hard_coef
	}.

cache_key(Game_id = Id) ->
	list_to_binary(io_lib:format(<<"usr_game_~p">>, [Game_id])).

