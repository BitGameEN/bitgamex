%%%--------------------------------------------------------
%%% @Module: usr_gold_transfer
%%% @Description: 自动生成
%%%--------------------------------------------------------
-module(usr_gold_transfer).
-export([get_one/1, get_gold_transfer_gids_by_wallet_addr/1, get_gold_transfer_gids_by_update_time/1, get_gold_transfer_gids_by_type/1, get_gold_transfer_gids_by_transaction_id_and_transaction_type/1, get_gold_transfer_gids_by_status/1, get_gold_transfer_gids_by_player_id/1, set_one/1, set_field/3, del_one/1, syncdb/1, clean_all_cache/0, cache_key/1]).
-include("common.hrl").
-include("record_usr_gold_transfer.hrl").

get_one(Id = Id) ->
	case cache:get(cache_key(Id)) of
		{true, _Cas, Val} ->
			Val;
		_ ->
			case db_esql:get_row(?DB_USR, <<"select id,type,transaction_type,transaction_id,receipt,player_id,device_id,xchg_accid,wallet_addr,gold,status,error_tag,receive_game_id,receive_time,update_time from gold_transfer where id=?">>, [Id]) of
				[] -> [];
				Row ->
					R = build_record_from_row(Row),
					cache:set(cache_key(R#usr_gold_transfer.key_id), R),
					R
			end
	end.

get_gold_transfer_gids_by_wallet_addr(Wallet_addr = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from gold_transfer where wallet_addr=?">>, [Wallet_addr]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_gold_transfer_gids_by_update_time(Update_time = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from gold_transfer where update_time=?">>, [Update_time]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_gold_transfer_gids_by_type(Type = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from gold_transfer where type=?">>, [Type]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_gold_transfer_gids_by_transaction_id_and_transaction_type({Transaction_id, Transaction_type} = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from gold_transfer where transaction_id=? and transaction_type=?">>, [Transaction_id, Transaction_type]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_gold_transfer_gids_by_status(Status = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from gold_transfer where status=?">>, [Status]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

get_gold_transfer_gids_by_player_id(Player_id = Id) ->
	case db_esql:get_all(?DB_USR, <<"select id from gold_transfer where player_id=?">>, [Player_id]) of
		[] -> [];
		Rows ->
			[Id_ || [Id_ | _] <- Rows]
	end.

set_field(Id = Id, Field, Val) ->
	Fields = record_info(fields, usr_gold_transfer),
	L = lists:zip(Fields, lists:seq(1, length(Fields))),
	{_, N} = lists:keyfind(Field, 1, L),
	R0 = get_one(Id),
	R = setelement(N+1, R0, Val),
	set_one(R),
	R.

set_one(R0) when is_record(R0, usr_gold_transfer) ->
	case R0#usr_gold_transfer.key_id =:= undefined of
		false ->
			syncdb(R0),
			cache:set(cache_key(R0#usr_gold_transfer.key_id), R0),
			R0#usr_gold_transfer.key_id;
		true ->
			#usr_gold_transfer{
				id = Id,
				type = Type,
				transaction_type = Transaction_type,
				transaction_id = Transaction_id,
				receipt = Receipt,
				player_id = Player_id,
				device_id = Device_id,
				xchg_accid = Xchg_accid,
				wallet_addr = Wallet_addr,
				gold = Gold,
				status = Status,
				error_tag = Error_tag,
				receive_game_id = Receive_game_id,
				receive_time = Receive_time,
				update_time = Update_time
			} = R0,
			{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_USR, io_lib:format(<<"insert into gold_transfer(id,type,transaction_type,transaction_id,receipt,player_id,device_id,xchg_accid,wallet_addr,gold,status,error_tag,receive_game_id,receive_time,update_time) values(~p,~p,~p,'~s','~s',~p,'~s','~s','~s',~p,~p,'~s',~p,~p,~p); select last_insert_id()">>,
				[Id, Type, Transaction_type, Transaction_id, Receipt, Player_id, Device_id, Xchg_accid, Wallet_addr, Gold, Status, Error_tag, Receive_game_id, Receive_time, Update_time])),
			R = R0#usr_gold_transfer{key_id = Insert_id, id = Insert_id},
			cache:set(cache_key(R#usr_gold_transfer.key_id), R),
			R#usr_gold_transfer.key_id
	end.

del_one(R) when is_record(R, usr_gold_transfer) ->
	Id = R#usr_gold_transfer.key_id,
	db_esql:execute(?DB_USR, <<"delete from gold_transfer where id=?">>, [Id]),
	cache:del(cache_key(R#usr_gold_transfer.key_id)),
	ok.

clean_all_cache() ->
	clean_all_cache(0),
	ok.

clean_all_cache(N) ->
	case db_esql:get_all(?DB_USR, <<"select id from gold_transfer limit ?, 1000">>, [N * 1000]) of
		[] -> ok;
		Rows ->
			F = fun(Id) -> cache:del(cache_key(Id)) end,
			[F(Id) || [Id | _] <- Rows],
			clean_all_cache(N + 1)
	end.

syncdb(R) when is_record(R, usr_gold_transfer) ->
	#usr_gold_transfer{
		id = Id,
		type = Type,
		transaction_type = Transaction_type,
		transaction_id = Transaction_id,
		receipt = Receipt,
		player_id = Player_id,
		device_id = Device_id,
		xchg_accid = Xchg_accid,
		wallet_addr = Wallet_addr,
		gold = Gold,
		status = Status,
		error_tag = Error_tag,
		receive_game_id = Receive_game_id,
		receive_time = Receive_time,
		update_time = Update_time
	} = R,
	db_esql:execute(?DB_USR, <<"replace into gold_transfer(id,type,transaction_type,transaction_id,receipt,player_id,device_id,xchg_accid,wallet_addr,gold,status,error_tag,receive_game_id,receive_time,update_time) values(?,?,?,?,?,?,?,?,?,?,?,?,?,?,?)">>,
		[Id, Type, Transaction_type, Transaction_id, Receipt, Player_id, Device_id, Xchg_accid, Wallet_addr, Gold, Status, Error_tag, Receive_game_id, Receive_time, Update_time]).

build_record_from_row([Id, Type, Transaction_type, Transaction_id, Receipt, Player_id, Device_id, Xchg_accid, Wallet_addr, Gold, Status, Error_tag, Receive_game_id, Receive_time, Update_time]) ->
	#usr_gold_transfer{
		key_id = Id,
		id = Id,
		type = Type,
		transaction_type = Transaction_type,
		transaction_id = Transaction_id,
		receipt = Receipt,
		player_id = Player_id,
		device_id = Device_id,
		xchg_accid = Xchg_accid,
		wallet_addr = Wallet_addr,
		gold = Gold,
		status = Status,
		error_tag = Error_tag,
		receive_game_id = Receive_game_id,
		receive_time = Receive_time,
		update_time = Update_time
	}.

cache_key(Id = Id) ->
	list_to_binary(io_lib:format(<<"usr_gold_transfer_~p">>, [Id])).

