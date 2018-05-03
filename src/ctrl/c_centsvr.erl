%%%--------------------------------------
%%% @Module  : c_centsvr
%%% @Description: centsvr的逻辑处理模块
%%%--------------------------------------
-module(c_centsvr).

-export([check_account/3]).

-include("common.hrl").

%-define(URL_PREFIX, "https://open.bitgamex.org/api/").
-define(URL_PREFIX, "http://127.0.0.1:8081/").
-define(CHECK_ACCOUNT_URL, ?URL_PREFIX ++ "checkaccount").

-define(JSON_CONTENT, {"Content-Type", "application/json; charset=utf8"}).

check_account(GameId, GameKey, ExchangeAccId) ->
    NowMilliSecs = util:longunixtime(),
    MD5Bin = <<"appid=", (integer_to_binary(GameId))/binary, "&bitaccount=", ExchangeAccId/binary, "&key=", GameKey/binary, "&timestamp=", (integer_to_binary(NowMilliSecs))/binary>>,
    Params = [{appid, GameId}, {bitaccount, ExchangeAccId}, {timestamp, NowMilliSecs}],
    case catch do_request(?CHECK_ACCOUNT_URL, MD5Bin, Params) of
        {ok, _} -> true;
        {-899, _ErrMsg} -> false
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

