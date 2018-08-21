%%%--------------------------------------------------------
%%% @Module: usr_useractivation
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_useractivation).
-export([get_one/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_useractivation.hrl").

get_one(Id = Id) ->
	case run_data:in_trans() of
		true ->
			case run_data:trans_get(usr_useractivation, Id) of
				[] ->
					get_one_(Id);
				trans_deleted -> [];
				R -> R
			end;
		false ->
			get_one_(Id)
	end.

get_one_(Id = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select id,uid,gameid,gameuid,createdate from useractivation where id=?">>, [Id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_useractivation.key_id), R),
					R
			end
	end.

set_field(Id = Id, Field, Val) ->
	Fields = record_info(fields, usr_useractivation),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_useractivation) ->
	case R0#usr_useractivation.key_id =:= undefined of
		false ->
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_useractivation, R0#usr_useractivation.key_id, R0,
						void,
						void);
				false ->
					syncdb(R0),
					cache:set(cache_key(R0#usr_useractivation.key_id), R0)
			end,
			R0#usr_useractivation.key_id;
		true ->
			#usr_useractivation{
				id = Id,
				uid = Uid,
				gameid = Gameid,
				gameuid = Gameuid,
				createdate = Createdate
			} = R0,
			{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_USR, io_lib:format(<<"insert into useractivation(id,uid,gameid,gameuid,createdate) values(~p,~p,~p,'~s',~p); select last_insert_id()">>,
				[Id, Uid, Gameid, Gameuid, Createdate])),
			R = R0#usr_useractivation{key_id = Insert_id, id = Insert_id},
			F = fun() ->
					cache:set(cache_key(R#usr_useractivation.key_id), R)
				end,
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_useractivation, R#usr_useractivation.key_id, {trans_inserted, R}, F, fun() -> usr_useractivation:del_one(R) end);
				false ->
					F()
			end,
			R#usr_useractivation.key_id
	end.

del_one(R) when is_record(R, usr_useractivation) ->
	case run_data:in_trans() of
		true ->
			run_data:trans_del(usr_useractivation, R#usr_useractivation.key_id,
				fun() -> usr_useractivation:del_one(R) end,
				void);
		false ->
			Id = R#usr_useractivation.key_id,
			run_data:db_write(del, R, fun() -> db_esql:execute(?DB_USR, <<"delete from useractivation where id=?">>, [Id]) end),
			cache:del(cache_key(R#usr_useractivation.key_id))
	end,
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select id from useractivation limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_useractivation) ->
	#usr_useractivation{
		id = Id,
		uid = Uid,
		gameid = Gameid,
		gameuid = Gameuid,
		createdate = Createdate
	} = R,
	run_data:db_write(upd, R, fun() -> db_esql:execute(?DB_USR, io_lib:format(<<"insert into useractivation(id,uid,gameid,gameuid,createdate) values(?,?,?,?,?) on duplicate key update "
		"uid = ?, gameid = ?, gameuid = ?, createdate = ?">>, []),
		[Id, Uid, Gameid, Gameuid, Createdate, Uid, Gameid, Gameuid, Createdate]) end).

build_record_from_row([Id, Uid, Gameid, Gameuid, Createdate]) ->
	#usr_useractivation{
		key_id = Id,
		id = Id,
		uid = Uid,
		gameid = Gameid,
		gameuid = Gameuid,
		createdate = Createdate
	}.

cache_key(Id = Id) ->
	list_to_binary(io_lib:format(<<"usr_useractivation_~p">>, [Id])).

