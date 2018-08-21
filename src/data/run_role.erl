%%%--------------------------------------------------------
%%% @Module: run_role
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(run_role).
-export([init/0, load_one/1, get_one_locally/1, get_one/1, set_one/1, set_field/3, del_one/1, del_one/2, syncdb/1, reload_one/1, clean_all_cache/1, cache_key/1]).
-include("common.hrl").
-include("record_run_role.hrl").

init() ->
	ets:new(run_role, [{keypos, #run_role.key_id}, named_table, public, set, ?ETSRC, ?ETSWC]),
	ok.

load_one({Game_id, Player_id} = Id) ->
	get_one(Id, direct_from_db).

get_one_locally({Game_id, Player_id} = Id) ->
	get_one(Id, just_from_ets).

get_one({Game_id, Player_id} = Id) ->
	case run_data:in_trans() of
		true ->
			case run_data:trans_get(run_role, Id) of
				[] ->
					get_one(Id, anyway);
				trans_deleted -> [];
				R -> run_data_ver:upgrade_if_need(R)
			end;
		false ->
			get_one(Id, anyway)
	end.

get_one({Game_id, Player_id} = Id, just_from_ets) ->
	run_data:lookup_one(run_role, Id, false);
get_one({Game_id, Player_id} = Id, direct_from_db) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val0} ->
			Val = run_data_ver:upgrade_if_need(Val0),
			insert_ets(Val),
			Val;
		_ ->
			case db_esql:get_row(?DB_RUN, io_lib:format(<<"select player_id,ver,game_id,create_time,last_login_time,last_login_ip,game_data,old_game_data,power,time from role_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) of
				[] -> [];
				Row ->
					insert_ets_from_db(Row),
					run_data:lookup_one(run_role, Id, false)
			end
	end;
get_one({Game_id, Player_id} = Id, anyway) ->
	case run_data:lookup_one(run_role, Id, false) of
		[] ->
			case cache:get(cache_key(Id)) of
				{true, _Cas, Val0} ->
					Val = run_data_ver:upgrade_if_need(Val0),
					Val;
				_ ->
					case db_esql:get_row(?DB_RUN, io_lib:format(<<"select player_id,ver,game_id,create_time,last_login_time,last_login_ip,game_data,old_game_data,power,time from role_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) of
						[] -> [];
						Row -> % do not insert ets
							R = build_record_from_row(Row),
							cache:set(cache_key(R#run_role.key_id), R),
							R
					end
			end;
		R -> R
	end;
get_one(_Id, _GetPolicy) -> throw(get_policy_not_supported).

set_field({Game_id, Player_id} = Id, Field, Val) ->
	Fields = record_info(fields, run_role),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, run_role) ->
	case R0#run_role.key_id =:= undefined of
		false ->
			case run_data:in_trans() of
				true ->
					run_data:trans_set(run_role, R0#run_role.key_id, R0,
						void,
						void);
				false ->
					case util:lookup_one(run_role, R0#run_role.key_id, false) of
						[] ->
							Old = get_one(R0#run_role.key_id, anyway),
							update_keymap_if_need(Old, R0);
						Old ->
							update_keymap_if_need(Old, R0)
					end,
					syncdb(R0),
					cache:set(cache_key(R0#run_role.key_id), R0)
			end,
			R0#run_role.key_id;
		true ->
			#run_role{
				player_id = Player_id,
				ver = Ver,
				game_id = Game_id,
				create_time = Create_time,
				last_login_time = Last_login_time,
				last_login_ip = Last_login_ip,
				game_data = Game_data,
				old_game_data = Old_game_data,
				power = Power,
				time = Time
			} = R0,
			R = R0#run_role{key_id = {Game_id, Player_id}, ver = run_data_ver:newest_ver(run_role)},
			F = fun() ->
					run_data:db_write(add, R, fun() -> 1 = db_esql:execute(?DB_RUN, io_lib:format(<<"insert into role_~p(player_id,ver,game_id,create_time,last_login_time,last_login_ip,game_data,old_game_data,power,time) values(~p,~p,~p,~p,~p,'~s','~s','~s',~p,~p)">>,
						[Game_id, Player_id, Ver, Game_id, Create_time, Last_login_time, Last_login_ip, Game_data, Old_game_data, Power, Time])) end),
					cache:set(cache_key(R#run_role.key_id), R),
					insert_ets(R)
				end,
			case run_data:in_trans() of
				true ->
					run_data:trans_set(run_role, R#run_role.key_id, {trans_inserted, R}, F, fun() -> run_role:del_one(R, true) end);
				false ->
					F()
			end,
			R#run_role.key_id
	end.

del_one(R) when is_record(R, run_role) ->
	del_one(R, false);
del_one({Game_id, Player_id} = Id) ->
	del_one(Id, false).

del_one(R, DelDbAlso) when is_record(R, run_role) ->
	case run_data:in_trans() of
		true ->
			run_data:trans_del(run_role, R#run_role.key_id,
				fun() -> run_role:del_one(R, DelDbAlso) end,
				void);
		false ->
			{Game_id, Player_id} = R#run_role.key_id,
			case DelDbAlso of
				true ->
					run_data:db_write(del, R, fun() -> db_esql:execute(?DB_RUN, io_lib:format(<<"delete from role_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) end),
					cache:del(cache_key(R#run_role.key_id));
				false -> void
			end,
			ets:delete(run_role, R#run_role.key_id)
	end,
	ok;
del_one({Game_id, Player_id} = Id, DelDbAlso) ->
	case util:lookup_one(run_role, Id, false) of
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
	case util:lookup_one(run_role, Id, false) of
		[] -> true;
		R ->
			case db_esql:get_row(?DB_RUN, io_lib:format(<<"select player_id,ver,game_id,create_time,last_login_time,last_login_ip,game_data,old_game_data,power,time from role_~p where game_id=? and player_id=?">>, [Game_id]), [Game_id, Player_id]) of
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
	case db_esql:get_all(?DB_RUN, io_lib:format(<<"select game_id, player_id from role_~p limit ?, 1000">>, [Game_id]), [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F({Id1, Id2}) || [Id1, Id2 | _] <- Rows],
			clean_all_cache(N + 1, Game_id)
	end.

syncdb(R) when is_record(R, run_role) ->
	#run_role{
		player_id = Player_id,
		ver = Ver,
		game_id = Game_id,
		create_time = Create_time,
		last_login_time = Last_login_time,
		last_login_ip = Last_login_ip,
		game_data = Game_data,
		old_game_data = Old_game_data,
		power = Power,
		time = Time
	} = R,
	run_data:db_write(upd, R, fun() -> db_esql:execute(?DB_RUN, io_lib:format(<<"insert into role_~p(player_id,ver,game_id,create_time,last_login_time,last_login_ip,game_data,old_game_data,power,time) values(?,?,?,?,?,?,?,?,?,?) on duplicate key update "
		"ver = ?, game_id = ?, create_time = ?, last_login_time = ?, last_login_ip = ?, game_data = ?, old_game_data = ?, power = ?, time = ?">>, [Game_id]),
		[Player_id, Ver, Game_id, Create_time, Last_login_time, Last_login_ip, Game_data, Old_game_data, Power, Time, Ver, Game_id, Create_time, Last_login_time, Last_login_ip, Game_data, Old_game_data, Power, Time]) end);
syncdb(Gid) ->
	case get_one_locally(Gid) of
		[] -> void;
		R -> syncdb(R)
	end.

insert_ets_from_db([Game_id, Player_id|_] = Row) ->
	R = build_record_from_row(Row),
	cache:set(cache_key(R#run_role.key_id), R),
	insert_ets(R).

insert_ets(R) ->
	ok.

build_record_from_row([Player_id, Ver, Game_id, Create_time, Last_login_time, Last_login_ip, Game_data, Old_game_data, Power, Time]) ->
	R0 = #run_role{
		key_id = {Game_id, Player_id},
		player_id = Player_id,
		ver = Ver,
		game_id = Game_id,
		create_time = Create_time,
		last_login_time = Last_login_time,
		last_login_ip = Last_login_ip,
		game_data = Game_data,
		old_game_data = Old_game_data,
		power = Power,
		time = Time
	},
	run_data_ver:upgrade_if_need(R0).

update_keymap_if_need(Old, R) ->
	ok.

cache_key({Game_id, Player_id} = Id) ->
	list_to_binary(io_lib:format(<<"run_role_~p_~p">>, [Game_id, Player_id])).

