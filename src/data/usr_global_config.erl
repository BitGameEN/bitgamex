%%%--------------------------------------------------------
%%% @Module: usr_global_config
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_global_config).
-export([get_one/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_global_config.hrl").

get_one(Id = Id) ->
	case run_data:in_trans() of
		true ->
			case run_data:trans_get(usr_global_config, Id) of
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
			case db_esql:get_row(?DB_USR, <<"select id,global_key,content from global_config where id=?">>, [Id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_global_config.key_id), R),
					R
			end
	end.

set_field(Id = Id, Field, Val) ->
	Fields = record_info(fields, usr_global_config),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_global_config) ->
	case R0#usr_global_config.key_id =:= undefined of
		false ->
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_global_config, R0#usr_global_config.key_id, R0,
						void,
						void);
				false ->
					syncdb(R0),
					cache:set(cache_key(R0#usr_global_config.key_id), R0)
			end,
			R0#usr_global_config.key_id;
		true ->
			#usr_global_config{
				id = Id,
				global_key = Global_key,
				content = Content
			} = R0,
			R = R0#usr_global_config{key_id = Id},
			F = fun() ->
					run_data:db_write(add, R, fun() -> 1 = db_esql:execute(?DB_USR, io_lib:format(<<"insert into global_config(id,global_key,content) values(~p,'~s','~s')">>,
						[Id, Global_key, Content])) end),
					cache:set(cache_key(R#usr_global_config.key_id), R)
				end,
			case run_data:in_trans() of
				true ->
					run_data:trans_set(usr_global_config, R#usr_global_config.key_id, {trans_inserted, R}, F, fun() -> usr_global_config:del_one(R) end);
				false ->
					F()
			end,
			R#usr_global_config.key_id
	end.

del_one(R) when is_record(R, usr_global_config) ->
	case run_data:in_trans() of
		true ->
			run_data:trans_del(usr_global_config, R#usr_global_config.key_id,
				fun() -> usr_global_config:del_one(R) end,
				void);
		false ->
			Id = R#usr_global_config.key_id,
			run_data:db_write(del, R, fun() -> db_esql:execute(?DB_USR, <<"delete from global_config where id=?">>, [Id]) end),
			cache:del(cache_key(R#usr_global_config.key_id))
	end,
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select id from global_config limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_global_config) ->
	#usr_global_config{
		id = Id,
		global_key = Global_key,
		content = Content
	} = R,
	run_data:db_write(upd, R, fun() -> db_esql:execute(?DB_USR, io_lib:format(<<"insert into global_config(id,global_key,content) values(?,?,?) on duplicate key update "
		"global_key = ?, content = ?">>, []),
		[Id, Global_key, Content, Global_key, Content]) end).

build_record_from_row([Id, Global_key, Content]) ->
	#usr_global_config{
		key_id = Id,
		id = Id,
		global_key = Global_key,
		content = Content
	}.

cache_key(Id = Id) ->
	list_to_binary(io_lib:format(<<"usr_global_config_~p">>, [Id])).

