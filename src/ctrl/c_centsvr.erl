%%%--------------------------------------
%%% @Module  : c_centsvr
%%% @Description: centsvr的逻辑处理模块
%%%--------------------------------------
-module(c_centsvr).

-export([check_account/3,
         send_verify_code/5]).

-include("common.hrl").
-include("record_usr_user.hrl").

%-define(URL_PREFIX, "https://open.bitgamex.org/api/").
-define(URL_PREFIX, "http://127.0.0.1:8081/").
-define(CHECK_ACCOUNT_URL, ?URL_PREFIX ++ "checkaccount").
-define(SEND_VERIFY_CODE_URL, ?URL_PREFIX ++ "sendverifycode").

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
bind_exchange_accid() ->
    ok.

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
                    throw({?ERRNO_HTTP_REQ_FAILED, Body})
            end;
        {error, req_timedout} ->
            throw({?ERRNO_HTTP_REQ_TIMEOUT, <<"request timeout">>});
        {error, Reason} ->
            throw({?ERRNO_HTTP_REQ_FAILED, Reason})
    end.

