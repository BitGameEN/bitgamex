%%%--------------------------------------
%%% @Module  : c_xchgsvr
%%% @Description: xchgsvr的逻辑处理模块
%%%--------------------------------------
-module(c_xchgsvr).

-export([check_account/3]).
-export([transfer_gold_to_exchange/5, transfer_gold_to_wallet/6]).

-include("common.hrl").
-include("gameConfig.hrl").
-include("gameConfigGlobalKey.hrl").
-include("gameConfig3rdParty.hrl").
-include("record_usr_user.hrl").
-include("record_run_role_gold.hrl").
-include("record_usr_gold_transfer.hrl").

%-define(URL_PREFIX, "https://open.bitgamex.org/api/").
-define(URL_PREFIX, "http://127.0.0.1:8081/").
-define(CHECK_ACCOUNT_URL, ?URL_PREFIX ++ "checkaccount").

-define(TRANSFER_TO_XCHG_URL, "https://exchange_ip/?a=transfer_coin_to_exchange").
-define(TRANSFER_TO_WALLET_URL, "https://exchange_ip/?a=transfer_coin_to_wallet").

-define(JSON_CONTENT, {"Content-Type", "application/json; charset=utf8"}).
-define(HTTP_CLIENT_TIMEOUT, 10000).
-define(HTTP_CLIENT_OPTIONS, [{max_sessions, 100}, {max_pipeline_size, 10}]).

check_account(GameId, GameKey, ExchangeAccId) ->
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"appid=", (integer_to_binary(GameId))/binary, "&bitaccount=", ExchangeAccId/binary, "&key=", GameKey/binary, "&timestamp=", (integer_to_binary(NowMilliSecs))/binary>>,
    MD5Val = list_to_binary(util:md5(MD5Bin)),
    Params = [{appid, GameId}, {bitaccount, ExchangeAccId}, {sign, MD5Val}, {timestamp, NowMilliSecs}],
    JsonParams = jsx:encode(Params),
    ?DBG("JsonParams: ~p~n", [JsonParams]),
    case ibrowse:send_req(?CHECK_ACCOUNT_URL, [?JSON_CONTENT], post, JsonParams) of
        {ok, Status, Head, Body} ->
            case Status of
                "200" ->
                    JsonObject = jsx:decode(list_to_binary(Body)),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    case lists:keyfind(<<"code">>, 1, JsonObject) of
                        {_, 0} -> % 成功
                            {ok, ExchangeAccId};
                        {_, ErrCode} -> % 失败
                            {_, ErrMsg} = lists:keyfind(<<"message">>, 1, JsonObject),
                            throw({ErrCode, ErrMsg})
                    end;
                _ ->
                    throw({?ERRNO_HTTP_REQ_FAILED, Body})
            end;
        {error, req_timedout} ->
            throw({?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

transfer_gold_to_exchange(GameId, UserId, GoldType, Amount, ReceiptData) ->
    transfer_gold(?GOLD_TRANSFER_TYPE_GAME_TO_XCHG, GameId, UserId, GoldType, Amount, <<>>, ReceiptData).

transfer_gold_to_wallet(GameId, UserId, GoldType, Amount, WalletAddr, ReceiptData) ->
    transfer_gold(?GOLD_TRANSFER_TYPE_GAME_TO_WALLET, GameId, UserId, GoldType, Amount, WalletAddr, ReceiptData).

transfer_gold(TransferType, GameId, UserId, GoldType, Amount0, WalletAddr, ReceiptData) ->
    #usr_user{id = UserId, bind_xchg_accid = BindXchgAccId, device_id = DeviceId} = usr_user:get_one(UserId),
    TransactionType = ?GOLD_TRANSFER_TX_TYPE_GAME_TO_XCHG,
    lib_role_gold:put_gold_drain_type_and_drain_id(gold_transfer, TransferType, Amount0),
    lib_role_gold:add_gold(UserId, GameId, GoldType, -Amount0),
    TransactionId = lib_user_gold_transfer:gen_uuid(),
    NowDateTime = util:now_datetime_str(),
    TransferR = #usr_gold_transfer{
                    type = TransferType,
                    transaction_type = TransactionType,
                    transaction_id = TransactionId,
                    receipt = ReceiptData,
                    player_id = UserId,
                    device_id = DeviceId,
                    xchg_accid = BindXchgAccId,
                    wallet_addr = WalletAddr,
                    gold_type = GoldType,
                    gold = Amount0,
                    status = 0,
                    error_tag = <<>>,
                    receive_game_id = GameId,
                    receive_time = NowDateTime,
                    update_time = NowDateTime},
    usr_gold_transfer:set_one(TransferR),
    TransferDiscountToXchg = lib_global_config:get(?GLOBAL_CONFIG_KEY_TRANSFER_DISCOUNT_TO_XCHG),
    %% todo：临时为了客户端调试注释掉，以后改回来
    %Amount = Amount0 * (1 - TransferDiscountToXchg),
    %% 参数串：
    %% 发送到交易所：transaction_id=xx&game_uid=xx&exchange_accid=xx&token_symbol=xx&amount=xx&time=xx
    %% 发送到钱包：transaction_id=xx&game_uid=xx&exchange_accid=xx&wallet_address=xx&token_symbol=xx&amount=xx&time=xx
    %UserIdBin = integer_to_binary(UserId),
    %AmountBin = util:f2s(Amount),
    %TimeBin = integer_to_binary(util:unixtime()),
    %Params0 =
    %    case TransferType of
    %        ?GOLD_TRANSFER_TYPE_GAME_TO_XCHG ->
    %            <<"transaction_id=", TransactionId/binary, "&game_uid=", UserIdBin/binary, "&exchange_accid=", BindXchgAccId/binary,
    %              "&token_symbol=", GoldType/binary, "&amount=", AmountBin/binary, "&time=", TimeBin/binary>>;
    %        ?GOLD_TRANSFER_TYPE_GAME_TO_WALLET ->
    %            <<"transaction_id=", TransactionId/binary, "&game_uid=", UserIdBin/binary, "&exchange_accid=", BindXchgAccId/binary,
    %              "&wallet_address=", WalletAddr/binary, "&token_symbol=", GoldType/binary, "&amount=", AmountBin/binary, "&time=", TimeBin/binary>>
    %    end,
    %% 用自己的私钥签名
    %[Entry1] = public_key:pem_decode(?SELF_PRIVATE_KEY),
    %RSAPriKey = public_key:pem_entry_decode(Entry1),
    %Sign0 = public_key:sign(Params0, 'sha', RSAPriKey),
    %% 用交易所的公钥加密
    %[Entry2] = public_key:pem_decode(?EXCHANGE_PUBLIC_KEY),
    %RSAPubKey = public_key:pem_entry_decode(Entry2),
    %Params1 = public_key:encrypt_public(Params0, RSAPubKey),
    %% 然后，base64、url编码
    %Sign = util:url_encode(base64:encode(Sign0)),
    %ParamData = util:url_encode(base64:encode(Params1)),
    %Params = [{<<"param_data">>, ParamData}, {<<"sign">>, Sign}],
    Params = [],

    % 发送，并处理结果
    Callback =
        fun(JsonObject) ->
            case lists:keyfind(<<"succ">>, 1, JsonObject) of
                {_, 0} -> % 失败
                    lib_role_gold:add_gold(UserId, GameId, GoldType, Amount0), % 返回游戏币
                    ErrNo = case lists:keyfind(<<"errno">>, 1, JsonObject) of
                                {_, ErrNo_} ->  ErrNo_;
                                false -> ?ERRNO_UNIDENTIFIED
                            end,
                    ErrMsg = case lists:keyfind(<<"errmsg">>, 1, JsonObject) of
                                 {_, ErrMsg_} -> ErrMsg_;
                                 false -> <<>>
                             end,
                    lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {error, ErrNo, ErrMsg}),
                    throw({ErrNo, ErrMsg});
                _ -> % 其余均视为成功
                    Balance =
                        case lists:keyfind(<<"balance">>, 1, JsonObject) of
                            {_, Balance_} -> Balance_;
                            false -> -1
                        end,
                    lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {ok, GoldType, Amount0}),
                    lib_game:put_gold_drain_type_and_drain_id(gold_transfer, TransferType, Amount0),
                    lib_game:add_reclaimed_gold(GameId, GoldType, Amount0 * TransferDiscountToXchg),
                    RoleGold = run_role_gold:get_one({GameId, UserId}),
                    {ok, RoleGold#run_role_gold.gold, Balance}
            end
        end,
    Url = case TransferType of
              ?GOLD_TRANSFER_TYPE_GAME_TO_XCHG -> ?TRANSFER_TO_XCHG_URL;
              ?GOLD_TRANSFER_TYPE_GAME_TO_WALLET -> ?TRANSFER_TO_WALLET_URL
          end,
    case do_transfer_gold_to_exchange(Url, Params) of
        {error, ErrNo, ErrMsg} = Rs ->
            case ErrNo of
                ?ERRNO_HTTP_REQ_TIMEOUT ->
                    % 超时情况下不能确认是否已经发到对端并处理完成，所以不能返回游戏币
                    httpc_proxy:queue_request(Url, get, Params, Callback);
                _ -> lib_role_gold:add_gold(UserId, GameId, GoldType, Amount0) % 返回游戏币
            end,
            lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, Rs),
            throw({ErrNo, ErrMsg});
        JsonObject ->
            Callback(JsonObject)
    end.

do_transfer_gold_to_exchange(Url, Params) ->
    %% todo：临时为了客户端调试注释掉，以后改回来
    %case ibrowse:send_req(Url, [?JSON_CONTENT], get, jsx:encode(Params), ?HTTP_CLIENT_OPTIONS, ?HTTP_CLIENT_TIMEOUT) of
    %    {ok, Status, Head, Body} ->
    %        case Status of
    %            "200" ->
    %                JsonObject = jsx:decode(list_to_binary(Body)),
    %                %?INFO("JsonObject: ~p~n", [JsonObject]),
    %                JsonObject;
    %            _ ->
    %                {error, ?ERRNO_HTTP_REQ_FAILED, Body}
    %        end;
    %    {error, req_timedout} ->
    %        {error, ?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>};
    %    {error, Reason} ->
    %        {error, ?ERRNO_HTTP_REQ_FAILED, Reason}
    %end.
    [{<<"succ">>, 1}, {<<"balance">>, 0}].

