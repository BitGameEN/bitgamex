%%%--------------------------------------------------------
%%% @Module: run_role_gold_to_draw
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(run_role_gold_to_draw).
-export([init/0, load_one/1, get_one_locally/1, get_one/1, set_one/1, set_field/3, del_one/1, del_one/2, syncdb/1, reload_one/1, clean_all_cache/1, cache_key/1]).
-include("common.hrl").
-include("record_run_role_gold_to_draw.hrl").

init() ->
	ets:new(run_role_gold_to_draw, [{keypos, #run_role_gold_to_draw.key_id}, named_table, public, set, ?ETSRC, ?ETSWC]),
	ok.

load_one({Game_id, Player_id} = Id) ->
	get_one(Id, direct_from_db).

get_one_locally({Game_id, Player_id} = Id) ->
	get_one(Id, just_from_ets).

get_one({Game_id, Player_id} = Id) ->
	case run_data:in_trans() of
		true ->
			case run_data:trans_get(run_role_gold_to_draw, Id) of
				[] ->
					get_one(Id, anyway);
				trans_deleted -> [];
				R -> run_data_ver:upgrade_if_need(R)
			end;
		false ->
			get_one(Id, anyway)
	end.

get_one({Game_id, Player_id} = Id, just_from_ets) ->
	run_data:lookup_one(run_role_gold_to_draw, Id, false);
get_one({Game_id, Player_id} = Id, direct_from_db) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val0} ->
			Val = run_data_ver:upgrade_if_need(Val0),
			insert_ets(Val),
			Val;
		_ ->
			case db_esql:get_row(?DB_RUN, io_lib:format(<<"select player_id,ver,game_id,gold_list,time from role_gold_to_draw_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) of
				[] -> [];
				Row ->
					insert_ets_from_db(Row),
					run_data:lookup_one(run_role_gold_to_draw, Id, false)
			end
	end;
get_one({Game_id, Player_id} = Id, anyway) ->
	case run_data:lookup_one(run_role_gold_to_draw, Id, false) of
		[] ->
			case cache:get(cache_key(Id)) of
				{true, _Cas, Val0} ->
					Val = run_data_ver:upgrade_if_need(Val0),
					Val;
				_ ->
					case db_esql:get_row(?DB_RUN, io_lib:format(<<"select player_id,ver,game_id,gold_list,time from role_gold_to_draw_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) of
						[] -> [];
						Row -> % do not insert ets
							R = build_record_from_row(Row),
							cache:set(cache_key(R#run_role_gold_to_draw.key_id), R),
							R
					end
			end;
		R -> R
	end;
get_one(_Id, _GetPolicy) -> throw(get_policy_not_supported).

set_field({Game_id, Player_id} = Id, Field, Val) ->
	Fields = record_info(fields, run_role_gold_to_draw),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, run_role_gold_to_draw) ->
	case R0#run_role_gold_to_draw.key_id =:= undefined of
		false ->
			case run_data:in_trans() of
				true ->
					run_data:trans_set(run_role_gold_to_draw, R0#run_role_gold_to_draw.key_id, R0,
						void,
						void);
				false ->
					case util:lookup_one(run_role_gold_to_draw, R0#run_role_gold_to_draw.key_id, false) of
						[] ->
							Old = get_one(R0#run_role_gold_to_draw.key_id, anyway),
							update_keymap_if_need(Old, R0);
						Old ->
							ets:insert(run_role_gold_to_draw, R0),
							update_keymap_if_need(Old, R0)
					end,
					syncdb(R0),
					cache:set(cache_key(R0#run_role_gold_to_draw.key_id), R0)
			end,
			R0#run_role_gold_to_draw.key_id;
		true ->
			#run_role_gold_to_draw{
				player_id = Player_id,
				ver = Ver,
				game_id = Game_id,
				gold_list = Gold_list,
				time = Time
			} = R0,
			R = R0#run_role_gold_to_draw{key_id = {Game_id, Player_id}, ver = run_data_ver:newest_ver(run_role_gold_to_draw)},
			F = fun() ->
					run_data:db_write(add, R, fun() -> 1 = db_esql:execute(?DB_RUN, io_lib:format(<<"insert into role_gold_to_draw_~p(player_id,ver,game_id,gold_list,time) values(~p,~p,~p,'~s',~p)">>,
						[Game_id, Player_id, Ver, Game_id, ?T2B(Gold_list), Time])) end),
					cache:set(cache_key(R#run_role_gold_to_draw.key_id), R),
					insert_ets(R)
				end,
			case run_data:in_trans() of
				true ->
					run_data:trans_set(run_role_gold_to_draw, R#run_role_gold_to_draw.key_id, {trans_inserted, R}, F, fun() -> run_role_gold_to_draw:del_one(R, true) end);
				false ->
					F()
			end,
			R#run_role_gold_to_draw.key_id
	end.

del_one(R) when is_record(R, run_role_gold_to_draw) ->
	del_one(R, false);
del_one({Game_id, Player_id} = Id) ->
	del_one(Id, false).

del_one(R, DelDbAlso) when is_record(R, run_role_gold_to_draw) ->
	case run_data:in_trans() of
		true ->
			run_data:trans_del(run_role_gold_to_draw, R#run_role_gold_to_draw.key_id,
				fun() -> run_role_gold_to_draw:del_one(R, DelDbAlso) end,
				void);
		false ->
			{Game_id, Player_id} = R#run_role_gold_to_draw.key_id,
			case DelDbAlso of
				true ->
					run_data:db_write(del, R, fun() -> db_esql:execute(?DB_RUN, io_lib:format(<<"delete from role_gold_to_draw_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) end),
					cache:del(cache_key(R#run_role_gold_to_draw.key_id));
				false -> void
			end,
			ets:delete(run_role_gold_to_draw, R#run_role_gold_to_draw.key_id)
	end,
	ok;
del_one({Game_id, Player_id} = Id, DelDbAlso) ->
	case util:lookup_one(run_role_gold_to_draw, Id, false) of
		[] ->
			case DelDbAlso of
				true ->
					case get_one(Id, anyway) of
						[] -> void;
						R -> del_one(R, DelDbAlso)
					end;
				false -> void
			end;
		R -> del_one(R, DelDbAlso)
	end.

reload_one({Game_id, Player_id} = Id) ->
	cache:del(cache_key(Id)),
	case util:lookup_one(run_role_gold_to_draw, Id, false) of
		[] -> true;
		R ->
			case db_esql:get_row(?DB_RUN, io_lib:format(<<"select player_id,ver,game_id,gold_list,time from role_gold_to_draw_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) of
				[] -> false;
				Row ->
					insert_ets_from_db(Row),
					true
			end
	end.

clean_all_cache(Game_id) ->
	clean_all_cache(0, Game_id),
	ok.

clean_all_cache(N, Game_id) ->
	case db_esql:get_all(?DB_RUN, io_lib:format(<<"select game_id, player_id from role_gold_to_draw_~p limit ?, 1000">>, [Game_id]), [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F({Id1, Id2}) || [Id1, Id2 | _] <- Rows],
			clean_all_cache(N + 1, Game_id)
	end.

syncdb(R) when is_record(R, run_role_gold_to_draw) ->
	#run_role_gold_to_draw{
		player_id = Player_id,
		ver = Ver,
		game_id = Game_id,
		gold_list = Gold_list,
		time = Time
	} = R,
	run_data:db_write(upd, R, fun() -> db_esql:execute(?DB_RUN, io_lib:format(<<"insert into role_gold_to_draw_~p(player_id,ver,game_id,gold_list,time) values(?,?,?,?,?) on duplicate key update "
		"ver = ?, game_id = ?, gold_list = ?, time = ?">>, [Game_id]),
		[Player_id, Ver, Game_id, ?T2B(Gold_list), Time,Ver, Game_id, ?T2B(Gold_list), Time]) end);
syncdb(Gid) ->
	case get_one_locally(Gid) of
		[] -> void;
		R -> syncdb(R)
	end.

insert_ets_from_db([Game_id, Player_id|_] = Row) ->
	R = build_record_from_row(Row),
	cache:set(cache_key(R#run_role_gold_to_draw.key_id), R),
	insert_ets(R).

insert_ets(R) ->
	ets:insert(run_role_gold_to_draw, R),
	ok.

build_record_from_row([Player_id, Ver, Game_id, Gold_list, Time]) ->
	R0 = #run_role_gold_to_draw{
		key_id = {Game_id, Player_id},
		player_id = Player_id,
		ver = Ver,
		game_id = Game_id,
		gold_list = ?B2T(Gold_list),
		time = Time
	},
	run_data_ver:upgrade_if_need(R0).

update_keymap_if_need(Old, R) ->
	ok.

cache_key({Game_id, Player_id} = Id) ->
	list_to_binary(io_lib:format(<<"run_role_gold_to_draw_~p_~p">>, [Game_id, Player_id])).

