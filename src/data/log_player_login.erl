%%%--------------------------------------------------------
%%% @Module: log_player_login
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(log_player_login).
-export([get_one/1, set_one/1, build_record_from_row/1]).
-include("common.hrl").
-include("record_log_player_login.hrl").

get_one(Id) ->
	case db_esql:get_row(?DB_LOG, <<"select id,game_id,player_id,device_id,device_model,os_type,os_ver,ip,lang,time from player_login where id=?">>, [Id]) of
		[] -> [];
		Row -> build_record_from_row(Row)
	end.

set_one(R0) when is_record(R0, log_player_login) ->
	case R0#log_player_login.key_id =:= undefined of
		false ->
			syncdb(R0),
			R0#log_player_login.key_id;
		true ->
			#log_player_login{
				id = Id,
				game_id = Game_id,
				player_id = Player_id,
				device_id = Device_id,
				device_model = Device_model,
				os_type = Os_type,
				os_ver = Os_ver,
				ip = Ip,
				lang = Lang,
				time = Time
			} = R0,
			spawn(fun() -> {ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_LOG, io_lib:format(<<"insert into player_login(id,game_id,player_id,device_id,device_model,os_type,os_ver,ip,lang,time) values(~p,~p,~p,'~s','~s','~s','~s','~s','~s',~p); select last_insert_id()">>,
				[Id, Game_id, Player_id, Device_id, Device_model, Os_type, Os_ver, Ip, Lang, Time])) end)
	end.

syncdb(R) when is_record(R, log_player_login) ->
	#log_player_login{
		id = Id,
		game_id = Game_id,
		player_id = Player_id,
		device_id = Device_id,
		device_model = Device_model,
		os_type = Os_type,
		os_ver = Os_ver,
		ip = Ip,
		lang = Lang,
		time = Time
	} = R,
	spawn(fun() -> db_esql:execute(?DB_LOG, <<"replace into player_login(id,game_id,player_id,device_id,device_model,os_type,os_ver,ip,lang,time) values(?,?,?,?,?,?,?,?,?,?)">>,
		[Id, Game_id, Player_id, Device_id, Device_model, Os_type, Os_ver, Ip, Lang, Time]) end).

build_record_from_row([Id, Game_id, Player_id, Device_id, Device_model, Os_type, Os_ver, Ip, Lang, Time]) ->
	#log_player_login{
		key_id = Id,
		id = Id,
		game_id = Game_id,
		player_id = Player_id,
		device_id = Device_id,
		device_model = Device_model,
		os_type = Os_type,
		os_ver = Os_ver,
		ip = Ip,
		lang = Lang,
		time = Time
	}.

