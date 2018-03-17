-module(httpc_proxy).

-behaviour(gen_server).

-include("common.hrl").

-define(JSON_CONTENT, {"Content-Type", "application/json; charset=utf8"}).
-define(HTTP_CLIENT_TIMEOUT, 10000).
%% Connection pool size: 100, Each connection queue size: 100
-define(HTTP_CLIENT_OPTIONS, [{max_sessions, 100}, {max_pipeline_size, 100}]).

-define(SERVER, ?MODULE).
-define(RESEND_AFTER_DELAY, 20000). % 20 secs
-define(MAX_RETRY, 13).

-define(DETS_HTTP_REQUESTS, dets_http_requests).

%% API
-export([start_link/0,
         request/3,
         async_request/3,
         queue_request/4]).

%% gen_server callbacks
-export([init/1, 
         handle_call/3, 
         handle_cast/2, 
         handle_info/2,
         terminate/2, 
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

request(Url, Method, Params) ->
    request(Url, Method, Params, false).

async_request(Url, Method, Params) ->
    request(Url, Method, Params, true).

request(Url, Method, Params, IsAsync) ->
    Options = case IsAsync of
                  true -> ?HTTP_CLIENT_OPTIONS ++ [{stream_to, self()}];
                  false -> ?HTTP_CLIENT_OPTIONS
              end,
    JsonString = case Params of
                     [] -> Params;
                     _  -> jsx:encode(Params)
                 end,
    ibrowse:send_req(Url, [?JSON_CONTENT], Method, JsonString, Options, ?HTTP_CLIENT_TIMEOUT).

queue_request(Url, Method, Params, Callback) ->
    gen_server:cast(?SERVER, {queue_request, Url, Method, Params, Callback}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================
-record(request, {uuid, url, method, params, cb, retry}).

init([]) ->
    SvrId = mod_disperse:server_id(),
    HttpRequestsFile = "http_requests_"++integer_to_list(SvrId),
    dets:open_file(?DETS_HTTP_REQUESTS, [{type, set}, {file, HttpRequestsFile}]),
    erlang:send_after(?RESEND_AFTER_DELAY, self(), resend_all_requests),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({queue_request, Url, Method, Params, Callback}, State) ->
    Uuid = uuid_factory:gen(),
    Request = #request{uuid = Uuid,
                       url = Url,
                       method = Method,
                       params = Params,
                       cb = Callback,
                       retry = 0},
    dets:insert(?DETS_HTTP_REQUESTS, {Uuid, Request}),
    do_send_request(Request),
    {noreply, State};
handle_cast(Msg, State) ->
    ?INFO("handle_cast unknown msg: ~p~n", [Msg]),
    {noreply, State}.

handle_info(resend_all_requests, State) ->
    dets:traverse(?DETS_HTTP_REQUESTS, fun({_, Request}) -> do_send_request(Request), continue end),
    {noreply, State};
handle_info({resend_requst, Uuid}, State) ->
    case dets:lookup(?DETS_HTTP_REQUESTS, Uuid) of
        [] -> ok;
        [{_, Request}] -> do_send_request(Request)
    end,
    {noreply, State};
handle_info({ibrowse_async_headers, ReqId, Code, _Headers}, State) ->
    put({code, ReqId}, Code),
    {noreply, State};
handle_info({ibrowse_async_response, ReqId, Response}, State) ->
    Code = get({code, ReqId}),
    Uuid = get({req_id, ReqId}),
    if
        Code =:= "200" ->
            case dets:lookup(?DETS_HTTP_REQUESTS, Uuid) of
                [] -> ok;
                [{_, Request}] ->
                    Callback = Request#request.cb,
                    JsonObject = jsx:decode(list_to_binary(Response)),
                    catch Callback(JsonObject)
            end,
            dets:delete(?DETS_HTTP_REQUESTS, Uuid),
            erase({req_id, ReqId}),
            erase({code, ReqId});
        true ->
            case dets:lookup(?DETS_HTTP_REQUESTS, Uuid) of
                [] -> ok;
                [{_, Request}] ->
                    NewRequest = Request#request{retry = Request#request.retry + 1},
                    Retry = NewRequest#request.retry,
                    if
                        Retry > ?MAX_RETRY ->
                            dets:delete(?DETS_HTTP_REQUESTS, Uuid),
                            erase({req_id, ReqId}),
                            erase({code, ReqId});
                        true ->
                            dets:insert(?DETS_HTTP_REQUESTS, {Uuid, NewRequest}),
                            Delay = math:pow(2, Retry) * ?RESEND_AFTER_DELAY,
                            ?INFO("Http Resend Dealy: ~p, Uuid: ~p~n", [Delay, Uuid]),
                            erlang:send_after(trunc(Delay), self(), {resend_requst, Uuid})
                    end
            end
    end,
    {noreply, State};
handle_info({ibrowse_async_response_end, _ReqId}, State) ->
    {noreply, State};
handle_info(Msg, State) ->
    ?INFO("handle_info unknown msg: ~p~n", [Msg]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_send_request(Request) ->
    {ibrowse_req_id, ReqId} = async_request(Request#request.url,
                                            Request#request.method,
                                            Request#request.params),
    put({req_id, ReqId}, Request#request.uuid).
