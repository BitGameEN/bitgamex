%%%--------------------------------------------------------
%%% @Module: usr_game_package
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_game_package).
-export([get_one/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_game_package.hrl").

get_one({Game_id, Package_id} = Id) ->
	case run_data:in_trans() of
		true ->
			case run_data:trans_get(usr_game_package, Id) of
				[] ->
					get_one_(Id);
				trans_deleted -> [];
				R -> R
			end;
		false ->
			get_one_(Id)
	end.

get_one_({Game_id, Package_id} = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select game_id,package_id,mining_rule,mining_pools from game_package where game_id=? and package_id=?">>, [Game_id, Package_id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_game_package.key_id), R),
					R
			end
	end.

set_field({Game_id, Package_id} = Id, Field, Val) ->
	Fields = record_info(fields, usr_game_package),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_game_package) ->
	case R0#usr_game_package.key_id =:= undefined of
		false ->
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_game_package, R0#usr_game_package.key_id, R0,
						void,
						void);
				false ->
					syncdb(R0),
					cache:set(cache_key(R0#usr_game_package.key_id), R0)
			end,
			R0#usr_game_package.key_id;
		true ->
			#usr_game_package{
				game_id = Game_id,
				package_id = Package_id,
				mining_rule = Mining_rule,
				mining_pools = Mining_pools
			} = R0,
			R = R0#usr_game_package{key_id = {Game_id, Package_id}},
			F = fun() ->
					run_data:db_write(add, R, fun() -> 1 = db_esql:execute(?DB_USR, io_lib:format(<<"insert into game_package(game_id,package_id,mining_rule,mining_pools) values(~p,~p,'~s','~s')">>,
						[Game_id, Package_id, ?T2B(Mining_rule), ?T2B(Mining_pools)])) end),
					cache:set(cache_key(R#usr_game_package.key_id), R)
				end,
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_game_package, R#usr_game_package.key_id, {trans_inserted, R}, F, fun() -> usr_game_package:del_one(R) end);
				false ->
					F()
			end,
			R#usr_game_package.key_id
	end.

del_one(R) when is_record(R, usr_game_package) ->
	case run_data:in_trans() of
		true ->
			run_data:trans_del(usr_game_package, R#usr_game_package.key_id,
				fun() -> usr_game_package:del_one(R) end,
				void);
		false ->
			{Game_id, Package_id} = R#usr_game_package.key_id,
			run_data:db_write(del, R, fun() -> db_esql:execute(?DB_USR, <<"delete from game_package where game_id=? and package_id=?">>, [Game_id, Package_id]) end),
			cache:del(cache_key(R#usr_game_package.key_id))
	end,
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select game_id,package_id from game_package limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_game_package) ->
	#usr_game_package{
		game_id = Game_id,
		package_id = Package_id,
		mining_rule = Mining_rule,
		mining_pools = Mining_pools
	} = R,
	run_data:db_write(upd, R, fun() -> db_esql:execute(?DB_USR, io_lib:format(<<"insert into game_package(game_id,package_id,mining_rule,mining_pools) values(?,?,?,?) on duplicate key update "
		"package_id = ?, mining_rule = ?, mining_pools = ?">>, []),
		[Game_id, Package_id, ?T2B(Mining_rule), ?T2B(Mining_pools), Package_id, ?T2B(Mining_rule), ?T2B(Mining_pools)]) end).

build_record_from_row([Game_id, Package_id, Mining_rule, Mining_pools]) ->
	#usr_game_package{
		key_id = {Game_id, Package_id},
		game_id = Game_id,
		package_id = Package_id,
		mining_rule = ?B2T(Mining_rule),
		mining_pools = ?B2T(Mining_pools)
	}.

cache_key({Game_id, Package_id} = Id) ->
	list_to_binary(io_lib:format(<<"usr_game_package_~p_~p">>, [Game_id, Package_id])).

