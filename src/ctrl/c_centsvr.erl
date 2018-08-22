%%%--------------------------------------
%%% @Module  : c_centsvr
%%% @Description: centsvr的逻辑处理模块
%%%--------------------------------------
-module(c_centsvr).

-export([check_account/3,
         send_verify_code/5,
         bind_exchange_accid/5,
         unbind_exchange_accid/5,
         transfer_gold_to_exchange/7,
         recharge_gold_to_game/7,
         consume_gold/5]).

-include("common.hrl").
-include("gameConfig.hrl").
-include("gameConfigGlobalKey.hrl").
-include("record_cfg_gold_type.hrl").
-include("record_run_role_gold.hrl").
-include("record_usr_user.hrl").
-include("record_usr_gold_transfer.hrl").

%-define(URL_PREFIX, "https://open.bitgamex.org/api/").
-define(URL_PREFIX, "http://127.0.0.1:9006/").
-define(CHECK_ACCOUNT_URL, ?URL_PREFIX ++ "checkaccount").
-define(SEND_VERIFY_CODE_URL, ?URL_PREFIX ++ "sendverifycode").
-define(BIND_ACCOUNT_URL, ?URL_PREFIX ++ "bindaccount").
-define(UNBIND_ACCOUNT_URL, ?URL_PREFIX ++ "unbindaccount").
-define(TRANSFER_TO_XCHG_URL, ?URL_PREFIX ++ "exchange").
-define(TRANSFER_TO_WALLET_URL, "").
-define(CONSUME_GOLD_URL, ?URL_PREFIX ++ "usetoken").
-define(RECHARGE_TO_GAME_URL, ?URL_PREFIX ++ "recharge").

-define(JSON_CONTENT, {"Content-Type", "application/json; charset=utf8"}).

%% https://github.com/BitGameEN/OpenAPI/blob/master/%E6%A3%80%E6%B5%8B%E4%BA%A4%E6%98%93%E6%89%80%E8%B4%A6%E6%88%B7.md
check_account(GameId, GameKey, ExchangeAccId) ->
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"appid=", (integer_to_binary(GameId))/binary,
               "&bitaccount=", ExchangeAccId/binary,
               "&timestamp=", (integer_to_binary(NowMilliSecs))/binary,
               GameKey/binary>>,
    Params = [{appid, GameId},
              {bitaccount, ExchangeAccId},
              {timestamp, NowMilliSecs}],
    case do_request(?CHECK_ACCOUNT_URL, MD5Bin, Params) of
        {ok, _} -> true;
        {-899, _ErrMsg} -> false;
        Error -> throw(Error)
    end.

%% https://github.com/BitGameEN/OpenAPI/blob/master/%E5%8F%91%E9%80%81%E9%AA%8C%E8%AF%81%E7%A0%81.md
send_verify_code(GameId, GameKey, PlayerId, ExchangeAccId, SendType) ->
    #usr_user{lang = Lang} = usr_user:get_one(PlayerId),
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"appid=", (integer_to_binary(GameId))/binary,
               "&appuid=", (integer_to_binary(PlayerId))/binary,
               "&bitaccount=", ExchangeAccId/binary,
               "&language=", Lang/binary,
               "&sendtype=", (integer_to_binary(SendType))/binary,
               "&timestamp=", (integer_to_binary(NowMilliSecs))/binary,
               "&uid=", (integer_to_binary(PlayerId))/binary,
               GameKey/binary>>,
    Params = [{appid, GameId},
              {appuid, PlayerId},
              {bitaccount, ExchangeAccId},
              {language, Lang},
              {sendtype, SendType},
              {timestamp, NowMilliSecs},
              {uid, PlayerId}],
    case do_request(?SEND_VERIFY_CODE_URL, MD5Bin, Params) of
        {ok, _} -> ok;
        Error -> throw(Error)
    end.

%% https://github.com/BitGameEN/OpenAPI/blob/master/%E7%BB%91%E5%AE%9A%E4%BA%A4%E6%98%93%E6%89%80%E8%B4%A6%E6%88%B7.md
bind_exchange_accid(GameId, GameKey, PlayerId, ExchangeAccId, VerifyCode) ->
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"appid=", (integer_to_binary(GameId))/binary,
               "&appuid=", (integer_to_binary(PlayerId))/binary,
               "&bitaccount=", ExchangeAccId/binary,
               "&code=", VerifyCode/binary,
               "&timestamp=", (integer_to_binary(NowMilliSecs))/binary,
               GameKey/binary>>,
    Params = [{appid, GameId},
              {appuid, PlayerId},
              {bitaccount, ExchangeAccId},
              {code, VerifyCode},
              {timestamp, NowMilliSecs}],
    case do_request(?BIND_ACCOUNT_URL, MD5Bin, Params) of
        {ok, _} -> ok;
        Error -> throw(Error)
    end.

%% https://github.com/BitGameEN/OpenAPI/blob/master/Zh/%E8%A7%A3%E9%99%A4%E7%BB%91%E5%AE%9A%E5%85%B3%E7%B3%BB.md
unbind_exchange_accid(GameId, GameKey, PlayerId, ExchangeAccId, VerifyCode) ->
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"appid=", (integer_to_binary(GameId))/binary,
               "&bitaccount=", ExchangeAccId/binary,
               "&code=", VerifyCode/binary,
               "&timestamp=", (integer_to_binary(NowMilliSecs))/binary,
               "&uid=", (integer_to_binary(PlayerId))/binary,
               GameKey/binary>>,
    Params = [{appid, GameId},
              {bitaccount, ExchangeAccId},
              {code, VerifyCode},
              {timestamp, NowMilliSecs},
              {uid, PlayerId}],
    case do_request(?UNBIND_ACCOUNT_URL, MD5Bin, Params) of
        {ok, _} -> ok;
        Error -> throw(Error)
    end.

%% https://github.com/BitGameEN/OpenAPI/blob/master/%E6%8F%90%E5%8F%96%E4%BB%A3%E5%B8%81.md
transfer_gold_to_exchange(GameId, GameKey, UserId, GoldType, Amount, ReceiptData, VerifyCode) ->
    transfer_gold(?GOLD_TRANSFER_TYPE_GAME_TO_XCHG, GameId, GameKey, UserId, GoldType, Amount, <<>>, ReceiptData, VerifyCode).

transfer_gold(TransferType, GameId, GameKey, UserId, GoldType, Amount0, WalletAddr, ReceiptData, VerifyCode) ->
    % 不放在trans里
    #usr_user{id = UserId, bind_xchg_accid = BindXchgAccId, device_id = DeviceId} = usr_user:get_one(UserId),
    TransactionType = ?GOLD_TRANSFER_TX_TYPE_GAME_TO_XCHG,
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
    lib_role_gold:put_gold_drain_type_and_drain_id(gold_transfer, TransferType, Amount0),
    lib_role_gold:add_gold(UserId, GameId, GoldType, -Amount0), % 先扣除
    TransferDiscountToXchg = lib_global_config:get(?GLOBAL_CONFIG_KEY_TRANSFER_DISCOUNT_TO_XCHG),
    Amount = Amount0 * (1 - TransferDiscountToXchg),
    true = Amount > 0, % 相当于断言

    % 参数串：
    % 发送到交易所：amount=xx&appid=xx&apporderno=xx&appuid=xx&bitaccount=xx&chainname=xx&code=xx&paramdata=xx&timestamp=xx&tokensymbol=xx&uid=xx
    Chain = case cfg_gold_type:get(GoldType) of
                null -> <<>>;
                #gold_type{chain_type = CT} -> CT
            end,
    GoldTypeLower = list_to_binary(string:to_lower(binary_to_list(GoldType))),
    UserIdBin = integer_to_binary(UserId),
    AmountBin = util:f2s(Amount),
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"amount=", AmountBin/binary,
               "&appid=", (integer_to_binary(GameId))/binary,
               "&apporderno=", TransactionId/binary,
               "&appuid=", UserIdBin/binary,
               "&bitaccount=", BindXchgAccId/binary,
               "&chainname=", Chain/binary,
               "&code=", VerifyCode/binary,
               "&paramdata=",
               "&timestamp=", (integer_to_binary(NowMilliSecs))/binary,
               "&tokensymbol=", GoldTypeLower/binary,
               "&uid=", UserIdBin/binary,
               GameKey/binary>>,
    Params = [{amount, AmountBin},
              {appid, GameId},
              {apporderno, TransactionId},
              {appuid, UserId},
              {bitaccount, BindXchgAccId},
              {chainname, Chain},
              {code, VerifyCode},
              {paramdata, <<>>},
              {timestamp, NowMilliSecs},
              {tokensymbol, GoldTypeLower},
              {uid, UserId}],

    % 发送，并处理结果
    OkCallback =
        fun(Data) ->
            Balance =
                case is_list(Data) of
                    true ->
                        case lists:keyfind(<<"balance">>, 1, Data) of
                            {_, Balance_} -> Balance_;
                            false -> -1
                        end;
                    false -> -1
                end,
            lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {ok, GoldType, Amount0}),
            lib_game:put_gold_drain_type_and_drain_id(gold_transfer, TransferType, Amount0),
            lib_game:add_reclaimed_gold(GameId, GoldType, Amount0 * TransferDiscountToXchg),
            RoleGold = run_role_gold:get_one({GameId, UserId}),
            {ok, RoleGold#run_role_gold.gold, Balance}
        end,
    Url = case TransferType of
              ?GOLD_TRANSFER_TYPE_GAME_TO_XCHG -> ?TRANSFER_TO_XCHG_URL;
              ?GOLD_TRANSFER_TYPE_GAME_TO_WALLET -> ?TRANSFER_TO_WALLET_URL
          end,
    case catch do_request(Url, MD5Bin, Params) of
        {ok, Data} ->
            OkCallback(Data);
        {ErrNo, ErrMsg} = Error ->
            case ErrNo of
                ?ERRNO_HTTP_REQ_TIMEOUT ->
                    % 超时情况下不能确认是否已经发到对端并处理完成，所以不能返回游戏币
                    httpc_proxy:queue_request(Url, post, Params,
                                              fun(JsonObject) ->
                                                  case lists:keyfind(<<"code">>, 1, JsonObject) of
                                                      {_, 0} -> % 成功
                                                          {_, Data} = lists:keyfind(<<"data">>, 1, JsonObject),
                                                          OkCallback(Data);
                                                      {_, ErrNo} -> % 失败
                                                          {_, ErrMsg} = lists:keyfind(<<"message">>, 1, JsonObject),
                                                          lib_role_gold:add_gold(UserId, GameId, GoldType, Amount0), % 返回游戏币
                                                          lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {error, ErrNo, ErrMsg}),
                                                          {ErrNo, ErrMsg}
                                                  end
                                              end);
                _ ->
                    lib_role_gold:add_gold(UserId, GameId, GoldType, Amount0) % 返回游戏币
            end,
            lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {error, ErrNo, ErrMsg}),
            throw(Error)
    end.

recharge_gold_to_game(GameId, GameKey, UserId, GoldType, Amount, ReceiptData, VerifyCode) ->
    % 不放在trans里
    #usr_user{id = UserId, bind_xchg_accid = BindXchgAccId, device_id = DeviceId} = usr_user:get_one(UserId),
    TransferType = ?GOLD_TRANSFER_TYPE_XCHG_TO_GAME,
    TransactionType = ?GOLD_TRANSFER_TX_TYPE_XCHG_TO_GAME,
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
                    wallet_addr = <<>>,
                    gold_type = GoldType,
                    gold = Amount,
                    status = 0,
                    error_tag = <<>>,
                    receive_game_id = GameId,
                    receive_time = NowDateTime,
                    update_time = NowDateTime},
    usr_gold_transfer:set_one(TransferR),

    % 参数串：
    % 发送到交易所：amount=xx&appid=xx&apporderno=xx&appuid=xx&bitaccount=xx&chainname=xx&code=xx&paramdata=xx&timestamp=xx&tokensymbol=xx&uid=xx
    Chain = case cfg_gold_type:get(GoldType) of
                null -> <<>>;
                #gold_type{chain_type = CT} -> CT
            end,
    GoldTypeLower = list_to_binary(string:to_lower(binary_to_list(GoldType))),
    UserIdBin = integer_to_binary(UserId),
    AmountBin = util:f2s(Amount),
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"amount=", AmountBin/binary,
               "&appid=", (integer_to_binary(GameId))/binary,
               "&apporderno=", TransactionId/binary,
               "&appuid=", UserIdBin/binary,
               "&bitaccount=", BindXchgAccId/binary,
               "&chainname=", Chain/binary,
               "&code=", VerifyCode/binary,
               "&paramdata=",
               "&timestamp=", (integer_to_binary(NowMilliSecs))/binary,
               "&tokensymbol=", GoldTypeLower/binary,
               "&uid=", UserIdBin/binary,
               GameKey/binary>>,
    Params = [{amount, AmountBin},
              {appid, GameId},
              {apporderno, TransactionId},
              {appuid, UserId},
              {bitaccount, BindXchgAccId},
              {chainname, Chain},
              {code, VerifyCode},
              {paramdata, <<>>},
              {timestamp, NowMilliSecs},
              {tokensymbol, GoldTypeLower},
              {uid, UserId}],

    % 发送，并处理结果
    OkCallback =
        fun(Data) ->
            Balance =
                case is_list(Data) of
                    true ->
                        case lists:keyfind(<<"balance">>, 1, Data) of
                            {_, Balance_} -> Balance_;
                            false -> -1
                        end;
                    false -> -1
                end,
            % 加金币
            lib_role_gold:put_gold_drain_type_and_drain_id(gold_transfer, TransferType, Amount),
            lib_role_gold:add_gold(UserId, GameId, GoldType, Amount),
            % 更新transfer记录
            lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {ok, GoldType, Amount}),
            RoleGold = run_role_gold:get_one({GameId, UserId}),
            {ok, RoleGold#run_role_gold.gold, Balance}
        end,
    Url = ?RECHARGE_TO_GAME_URL,
    case catch do_request(Url, MD5Bin, Params) of
        {ok, Data} ->
            OkCallback(Data);
        {ErrNo, ErrMsg} = Error ->
            case ErrNo of
                ?ERRNO_HTTP_REQ_TIMEOUT ->
                    % 超时情况下不能确认是否已经发到对端并处理完成
                    httpc_proxy:queue_request(Url, post, Params,
                                              fun(JsonObject) ->
                                                  case lists:keyfind(<<"code">>, 1, JsonObject) of
                                                      {_, 0} -> % 成功
                                                          {_, Data} = lists:keyfind(<<"data">>, 1, JsonObject),
                                                          OkCallback(Data);
                                                      {_, ErrNo} -> % 失败
                                                          {_, ErrMsg} = lists:keyfind(<<"message">>, 1, JsonObject),
                                                          lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {error, ErrNo, ErrMsg}),
                                                          {ErrNo, ErrMsg}
                                                  end
                                              end);
                _ ->
                    void
            end,
            lib_user_gold_transfer:update_transfer_log(TransactionType, TransactionId, {error, ErrNo, ErrMsg}),
            throw(Error)
    end.

%% https://github.com/BitGameEN/OpenAPI/blob/master/%E7%94%A8%E6%88%B7%E6%B6%88%E8%80%97%E4%BB%A3%E5%B8%81.md
consume_gold(GameId, GameKey, PlayerId, GoldType, Amount) ->
    AmountBin = util:f2s(Amount),
    NowMilliSecs = util:longunixtime(),
    GoldTypeLower = list_to_binary(string:to_lower(binary_to_list(GoldType))),
    UserIdBin = integer_to_binary(PlayerId),
    MD5Bin = <<"amount=", AmountBin/binary,
               "&appid=", (integer_to_binary(GameId))/binary,
               "&appuid=", UserIdBin/binary,
               "&paramdata=",
               "&timestamp=", (integer_to_binary(NowMilliSecs))/binary,
               "&tokensymbol=", GoldTypeLower/binary,
               "&uid=", UserIdBin/binary,
               GameKey/binary>>,
    Params = [{amount, AmountBin},
              {appid, GameId},
              {appuid, PlayerId},
              {paramdata, <<>>},
              {timestamp, NowMilliSecs},
              {tokensymbol, GoldTypeLower},
              {uid, PlayerId}],
    case do_request(?CONSUME_GOLD_URL, MD5Bin, Params) of
        {ok, _} -> ok;
        Error -> throw(Error)
    end.

do_request(Url, BinToSign, Params0) ->
    MD5Val = list_to_binary(util:md5(BinToSign)),
    Params = lists:keysort(1, [{sign, MD5Val} | Params0]),
    JsonParams = jsx:encode(Params),
    ?DBG("JsonParams: ~p~n", [JsonParams]),
    case ibrowse:send_req(Url, [?JSON_CONTENT], post, JsonParams) of
        {ok, Status, Head, Body} ->
            case Status of
                "200" ->
                    JsonObject = jsx:decode(list_to_binary(Body)),
                    ?DBG("JsonObject: ~p~n", [JsonObject]),
                    case lists:keyfind(<<"code">>, 1, JsonObject) of
                        {_, 0} -> % 成功
                            {_, Data} = lists:keyfind(<<"data">>, 1, JsonObject),
                            {ok, Data};
                        {_, ErrCode} -> % 失败
                            {_, ErrMsg} = lists:keyfind(<<"message">>, 1, JsonObject),
                            {ErrCode, ErrMsg}
                    end;
                _ ->
                    throw({?ERRNO_HTTP_REQ_FAILED, list_to_binary(Body)})
            end;
        {error, req_timedout} ->
            throw({?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({?ERRNO_HTTP_REQ_FAILED, ?T2B(Reason)})
    end.

