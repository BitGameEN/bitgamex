%%%--------------------------------------------------------
%%% @Module: usr_user
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_user).
-export([get_one/1, get_user_gids_by_user_name/1, get_user_gids_by_status/1, get_user_gids_by_org_device_id/1, get_user_gids_by_device_id_and_is_bind/1, get_user_gids_by_ios_gamecenter_id/1, get_user_gids_by_hash_id/1, get_user_gids_by_google_id/1, get_user_gids_by_facebook_id/1, get_user_gids_by_current_game_id/1, get_user_gids_by_create_time/1, get_user_gids_by_country_code/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_user.hrl").

get_one(Id = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select id,hash_id,user_name,password,player_name,avatar,device_id,org_device_id,is_bind,ios_gamecenter_id,google_id,facebook_id,current_game_id,current_game_uid,session_token,lang,os_type,country_code,create_time,last_login_time,status,forbid_login_endtime,bind_xchg_accid,bind_wallet_addr,time from user where id=?">>, [Id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_user.key_id), R),
					R
			end
	end.

get_user_gids_by_user_name(User_name = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where user_name=?">>, [User_name]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_status(Status = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where status=?">>, [Status]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_org_device_id(Org_device_id = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where org_device_id=?">>, [Org_device_id]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_device_id_and_is_bind({Device_id, Is_bind} = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where device_id=? and is_bind=?">>, [Device_id, Is_bind]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_ios_gamecenter_id(Ios_gamecenter_id = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where ios_gamecenter_id=?">>, [Ios_gamecenter_id]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_hash_id(Hash_id = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where hash_id=?">>, [Hash_id]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_google_id(Google_id = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where google_id=?">>, [Google_id]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_facebook_id(Facebook_id = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where facebook_id=?">>, [Facebook_id]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_current_game_id(Current_game_id = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where current_game_id=?">>, [Current_game_id]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_create_time(Create_time = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where create_time=?">>, [Create_time]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_user_gids_by_country_code(Country_code = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from user where country_code=?">>, [Country_code]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

set_field(Id = Id, Field, Val) ->
	Fields = record_info(fields, usr_user),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_user) ->
	case R0#usr_user.key_id =:= undefined of
		false ->
			spawn(fun() -> syncdb(R0) end),
			cache:set(cache_key(R0#usr_user.key_id), R0),
			R0#usr_user.key_id;
		true ->
			#usr_user{
				id = Id,
				hash_id = Hash_id,
				user_name = User_name,
				password = Password,
				player_name = Player_name,
				avatar = Avatar,
				device_id = Device_id,
				org_device_id = Org_device_id,
				is_bind = Is_bind,
				ios_gamecenter_id = Ios_gamecenter_id,
				google_id = Google_id,
				facebook_id = Facebook_id,
				current_game_id = Current_game_id,
				current_game_uid = Current_game_uid,
				session_token = Session_token,
				lang = Lang,
				os_type = Os_type,
				country_code = Country_code,
				create_time = Create_time,
				last_login_time = Last_login_time,
				status = Status,
				forbid_login_endtime = Forbid_login_endtime,
				bind_xchg_accid = Bind_xchg_accid,
				bind_wallet_addr = Bind_wallet_addr,
				time = Time
			} = R0,
			{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_USR, io_lib:format(<<"insert into user(id,hash_id,user_name,password,player_name,avatar,device_id,org_device_id,is_bind,ios_gamecenter_id,google_id,facebook_id,current_game_id,current_game_uid,session_token,lang,os_type,country_code,create_time,last_login_time,status,forbid_login_endtime,bind_xchg_accid,bind_wallet_addr,time) values(~p,'~s','~s','~s','~s',~p,'~s','~s',~p,'~s','~s','~s',~p,'~s','~s','~s','~s','~s',~p,~p,~p,~p,'~s','~s',~p); select last_insert_id()">>,
				[Id, Hash_id, util:esc(User_name), Password, util:esc(Player_name), Avatar, Device_id, Org_device_id, Is_bind, Ios_gamecenter_id, Google_id, Facebook_id, Current_game_id, Current_game_uid, Session_token, Lang, Os_type, Country_code, Create_time, Last_login_time, Status, Forbid_login_endtime, Bind_xchg_accid, Bind_wallet_addr, Time])),
			R = R0#usr_user{key_id = Insert_id, id = Insert_id},
			cache:set(cache_key(R#usr_user.key_id), R),
			R#usr_user.key_id
	end.

del_one(R) when is_record(R, usr_user) ->
	Id = R#usr_user.key_id,
	db_esql:execute(?DB_USR, <<"delete from user where id=?">>, [Id]),
	cache:del(cache_key(R#usr_user.key_id)),
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select id from user limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_user) ->
	#usr_user{
		id = Id,
		hash_id = Hash_id,
		user_name = User_name,
		password = Password,
		player_name = Player_name,
		avatar = Avatar,
		device_id = Device_id,
		org_device_id = Org_device_id,
		is_bind = Is_bind,
		ios_gamecenter_id = Ios_gamecenter_id,
		google_id = Google_id,
		facebook_id = Facebook_id,
		current_game_id = Current_game_id,
		current_game_uid = Current_game_uid,
		session_token = Session_token,
		lang = Lang,
		os_type = Os_type,
		country_code = Country_code,
		create_time = Create_time,
		last_login_time = Last_login_time,
		status = Status,
		forbid_login_endtime = Forbid_login_endtime,
		bind_xchg_accid = Bind_xchg_accid,
		bind_wallet_addr = Bind_wallet_addr,
		time = Time
	} = R,
	db_esql:execute(?DB_USR, <<"replace into user(id,hash_id,user_name,password,player_name,avatar,device_id,org_device_id,is_bind,ios_gamecenter_id,google_id,facebook_id,current_game_id,current_game_uid,session_token,lang,os_type,country_code,create_time,last_login_time,status,forbid_login_endtime,bind_xchg_accid,bind_wallet_addr,time) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)">>,
		[Id, Hash_id, util:esc(User_name), Password, util:esc(Player_name), Avatar, Device_id, Org_device_id, Is_bind, Ios_gamecenter_id, Google_id, Facebook_id, Current_game_id, Current_game_uid, Session_token, Lang, Os_type, Country_code, Create_time, Last_login_time, Status, Forbid_login_endtime, Bind_xchg_accid, Bind_wallet_addr, Time]).

build_record_from_row([Id, Hash_id, User_name, Password, Player_name, Avatar, Device_id, Org_device_id, Is_bind, Ios_gamecenter_id, Google_id, Facebook_id, Current_game_id, Current_game_uid, Session_token, Lang, Os_type, Country_code, Create_time, Last_login_time, Status, Forbid_login_endtime, Bind_xchg_accid, Bind_wallet_addr, Time]) ->
	#usr_user{
		key_id = Id,
		id = Id,
		hash_id = Hash_id,
		user_name = User_name,
		password = Password,
		player_name = Player_name,
		avatar = Avatar,
		device_id = Device_id,
		org_device_id = Org_device_id,
		is_bind = Is_bind,
		ios_gamecenter_id = Ios_gamecenter_id,
		google_id = Google_id,
		facebook_id = Facebook_id,
		current_game_id = Current_game_id,
		current_game_uid = Current_game_uid,
		session_token = Session_token,
		lang = Lang,
		os_type = Os_type,
		country_code = Country_code,
		create_time = Create_time,
		last_login_time = Last_login_time,
		status = Status,
		forbid_login_endtime = Forbid_login_endtime,
		bind_xchg_accid = Bind_xchg_accid,
		bind_wallet_addr = Bind_wallet_addr,
		time = Time
	}.

cache_key(Id = Id) ->
	list_to_binary(io_lib:format(<<"usr_user_~p">>, [Id])).

