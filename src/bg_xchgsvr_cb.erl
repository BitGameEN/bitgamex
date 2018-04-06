%%%--------------------------------------
%%% @Module  : bg_xchgsvr_cb
%%% @Description: 交易所回调服务模块
%%%--------------------------------------
-module(bg_xchgsvr_cb).

-export([init/2]).

-include("common.hrl").
-include("gameConfig.hrl").
-include("gameConfig3rdParty.hrl").
-include("record_usr_user.hrl").
-include("record_usr_user_gold.hrl").
-include("record_usr_gold_transfer.hrl").

-define(ASSERT(EXPRESSION), (true = (EXPRESSION))).

-define(print_result, true).

-ifdef(print_result).
-define(PRINT_BEGIN(), {ok, S} = file:open("./exchange.txt", [write]), put(result_file_s, S)).
-define(PRINT(FormatStr, Args), io:format(get(result_file_s), FormatStr, Args)).
-define(PRINT_END(), file:close(get(result_file_s))).
-else.
-define(PRINT_BEGIN(), ok).
-define(PRINT(FormatStr, Args), ok).
-define(PRINT_END(), ok).
-endif.

init(Req, Opts) ->
    {PeerIp0, _} = maps:get(peer, Req),
    {Ip0, Ip1, Ip2, Ip3} = PeerIp0,
    PeerIp = [integer_to_list(Ip0), integer_to_list(Ip1), integer_to_list(Ip2), integer_to_list(Ip3)],
    AllowedIPs = case application:get_env(xchgsvr, xchg_server_ips) of
                     {ok, IPS} -> IPS;
                     undefined -> []
                 end,
    Req2 = case AllowedIPs =:= [] orelse lib_ipv4:verify_ip(PeerIp, AllowedIPs) of
        true ->
            Method = cowboy_req:method(Req),
            #{a := Action} = cowboy_req:match_qs([a], Req),
            try
                action(Method, Action, Req)
            catch
                throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
                    cowboy_req:reply(200, #{}, lib_http:reply_body_fail(Action, ErrNo, ErrMsg), Req);
                _:ExceptionErr ->
                    ?ERR("bg_xchgsvr_cb exception:~nerr_msg=~p~nstack=~p~n", [ExceptionErr, erlang:get_stacktrace()]),
                    cowboy_req:reply(200, #{}, lib_http:reply_body_fail(Action, ?ERRNO_EXCEPTION, ?T2B(ExceptionErr)), Req)
            end;
        false ->
            ?ERR("exchange cb exception: ip rejected, ~p~n", [PeerIp0]),
            #{action := Action} = cowboy_req:match_qs([a], Req),
            cowboy_req:reply(200, #{}, lib_http:reply_body_fail(Action, ?ERRNO_IP_BLOCKED, <<"ip blocked">>), Req)
    end,
    {ok, Req2, Opts}.

action(<<"GET">>, undefined, Req) ->
    cowboy_req:reply(200, #{}, lib_http:reply_body_fail(undefined, ?ERRNO_MISSING_PARAM, <<"Missing parameter">>), Req);

action(<<"GET">>, <<"transfer_coin_to_game">> = Action, Req) ->
    ?PRINT_BEGIN(),
    TransferType = ?GOLD_TRANSFER_TYPE_XCHG_TO_GAME,
    TransactionType = ?GOLD_TRANSFER_TX_TYPE_XCHG_TO_GAME,
    try
        ?PRINT("url=~s~n", [iolist_to_binary(cowboy_req:uri(Req, #{}))]),
        #{param_data := ParamData, sign := Sign0} = cowboy_req:match_qs([param_data, sign], Req),

        ParamByte = base64:decode(ParamData),
        Sign = base64:decode(Sign0),

        [Entry1] = public_key:pem_decode(?SELF_PRIVATE_KEY),
        RSAPriKey = public_key:pem_entry_decode(Entry1),
        Param = decrypt_private(ParamByte, RSAPriKey),
        
        ?PRINT("param=~s~n", [Param]),

        % 签名校验是交易所的回调
        [Entry2] = public_key:pem_decode(?EXCHANGE_PUBLIC_KEY),
        RSAPubKey = public_key:pem_entry_decode(Entry2),
        case public_key:verify(Param, sha, Sign, RSAPubKey) of
            true -> void;
            false -> throw({?ERRNO_VERIFY_FAILED, <<"sign unmatched">>})
        end,
    
        % transaction_id=xx&exchange_accid=xx&game_uid=xx&token_symbol=xx&amount=xx&time=xx
        ReceiptData = util:url_decode(Param),
        ParamList = string:tokens(binary_to_list(ReceiptData), [$&]),
        ParamPairs = [begin
                          case string:tokens(One, [$=]) of
                              [K, V] -> {K, V};
                              [K] -> {K, ""}
                          end
                      end || One <- ParamList],
        TransactionId = proplists:get_value("transaction_id", ParamPairs), ?ASSERT(TransactionId =/= undefined),
        ExchangeAccid = proplists:get_value("exchange_accid", ParamPairs), ?ASSERT(ExchangeAccid =/= undefined),
        GameUidStr = proplists:get_value("game_uid", ParamPairs), ?ASSERT(GameUidStr =/= undefined),
        GoldType = proplists:get_value("token_symbol", ParamPairs), ?ASSERT(GoldType =/= undefined),
        AmountStr = proplists:get_value("amount", ParamPairs), ?ASSERT(AmountStr =/= undefined),
        TimeStr = proplists:get_value("time", ParamPairs), ?ASSERT(TimeStr =/= undefined),
    
        PlayerId = list_to_integer(GameUidStr), ?ASSERT(PlayerId > 0),
        Amount = list_to_float(AmountStr), ?ASSERT(Amount > 0),
        Time = list_to_integer(TimeStr), ?ASSERT(Time > 0),

        put(transaction_id, TransactionId),
        Usr = usr_user:get_one(PlayerId),

        {true, Cas} = lib_user_gold_transfer:lock(TransactionType, PlayerId),
        put(user_gold_transfer_lock, {TransactionType, PlayerId, Cas}),

        TransferAmount = case usr_gold_transfer:get_gold_transfer_gids_by_transaction_id_and_transaction_type({TransactionId, TransactionType}) of
            [] ->
                NowDateTime = util:now_datetime_str(),
                TransferR = #usr_gold_transfer{
                                type = TransferType,
                                transaction_type = TransactionType,
                                transaction_id = TransactionId,
                                receipt = ReceiptData,
                                player_id = PlayerId,
                                device_id = Usr#usr_user.device_id,
                                wallet_addr = <<>>,
                                gold_type = GoldType,
                                gold = Amount,
                                status = 0,
                                error_tag = <<>>,
                                receive_game_id = Usr#usr_user.current_game_id,
                                receive_time = NowDateTime,
                                update_time = NowDateTime},
                usr_gold_transfer:set_one(TransferR),
                Amount;
            [TransferGid] ->
                % 参数中的PlayerId和之前记录的应该匹配，不匹配会有异常抛出
                #usr_gold_transfer{status = Status, player_id = OldPlayerId, gold_type = OldGoldType, gold = OldAmount} = usr_gold_transfer:get_one(TransferGid),
                case Status of
                    0 -> void;
                    1 -> % 成功回调过
                        throw({-1, <<"delivery has been done already">>})
                end,
                case OldPlayerId =:= PlayerId of
                    true -> void;
                    false ->
                        ?ERR("exchange cb player_id unmatch: player_id=~p, old_player_id=~p, transfer_gid=~p~n", [PlayerId, OldPlayerId, TransferGid]),
                        throw({-2, <<"unmatched player_id">>})
                end,
                case OldGoldType =:= GoldType of
                    true -> void;
                    false ->
                        ?ERR("exchange cb gold type unmatch: player_id=~p, transfer_gid=~p, old_gold_type=~p, now_gold_type=~p~n", [PlayerId, TransferGid, OldGoldType, GoldType]),
                        throw({-3, <<"unmatched token_symbol">>})
                end,
                case OldAmount =:= Amount of
                    true -> void;
                    false ->
                        ?ERR("exchange cb amount unmatch: player_id=~p, transfer_gid=~p, old_amount=~p, now_amount=~p~n", [PlayerId, TransferGid, OldAmount, Amount])
                end,
                OldAmount
        end,

        run_data:trans_begin(),
        % 加金币
        lib_user_gold:put_gold_drain_type_and_drain_id(gold_transfer, TransferType, TransferAmount),
        lib_user_gold:add_gold(PlayerId, GoldType, TransferAmount),
        % 更新transfer记录
        lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {ok, GoldType, TransferAmount}),
        run_data:trans_commit(),
        unlock(),

        UserGold = usr_user_gold:get_one(PlayerId),
        cowboy_req:reply(200, #{}, lib_http:reply_body_succ(#{balance => ?G(UserGold#usr_user_gold.gold, GoldType)}), Req)

    catch
        throw:{ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
            run_data:trans_rollback(),
            ?ERR("exchange cb exception:~nerr_msg=~p,url=~p~n", [ErrMsg, iolist_to_binary(cowboy_req:uri(Req, #{}))]),
            case ErrNo of
                -1 -> void;
                _ -> lib_user_gold_transfer:update_transfer_log(TransactionType, get(transaction_id), {error, ErrNo, ErrMsg})
            end,
            unlock(),
            ?PRINT_END(),
            throw({ErrNo, ErrMsg});
        _:ExceptionErr ->
            run_data:trans_rollback(),
            ?ERR("exchange cb exception:~nerr_msg=~p,url=~p~nstack=~p~n", [ExceptionErr, iolist_to_binary(cowboy_req:uri(Req, #{})), erlang:get_stacktrace()]),
            ErrMsg = ?T2B(ExceptionErr),
            lib_user_gold_transfer:update_transfer_log(TransactionType, get(transaction_id), {error, ?ERRNO_EXCEPTION, ErrMsg}),
            unlock(),
            ?PRINT_END(),
            throw({?ERRNO_EXCEPTION, ErrMsg})
    end;

action(_, Action, Req) ->
    cowboy_req:reply(200, #{}, lib_http:reply_body_fail(Action, ?ERRNO_ACTION_NOT_SUPPORT, <<"Action not supported">>), Req).

% 解锁
unlock() ->
    case get(user_gold_transfer_lock) of
        {TransType, PlyrId, CasVal} ->
            lib_user_gold_transfer:unlock(TransType, PlyrId, CasVal);
        _ -> void
    end.

decrypt_private(ParamByte, RSAPriKey) ->
    decrypt_private(ParamByte, RSAPriKey, <<>>).

decrypt_private(ParamByte, RSAPriKey, Acc) ->
    ByteSize = byte_size(ParamByte),
    ParamBytePart = binary:part(ParamByte, 0, min(128, ByteSize)),
    ParamPart = public_key:decrypt_private(ParamBytePart, RSAPriKey),
    case ByteSize > 128 of
        true ->
            ParamByteRest = binary:part(ParamByte, 128, ByteSize - 128),
            decrypt_private(ParamByteRest, RSAPriKey, <<Acc/binary, ParamPart/binary>>);
        false ->
            <<Acc/binary, ParamPart/binary>>
    end.

