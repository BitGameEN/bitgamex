%%%--------------------------------------
%%% @Module  : bg_xchgsvr
%%% @Description: 游戏转账服
%%%--------------------------------------
-module(bg_xchgsvr).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(DEFAULT_EXCHANGE_PORT, 80).

%%开启转账服
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    Port =
        case application:get_env(xchgsvr, xchg_http_port) of
            {ok, P} -> P;
            undefined -> ?DEFAULT_EXCHANGE_PORT
        end,
    Dispatch = cowboy_router:compile([{'_', [
            {"/", bg_xchgsvr_cb, []}
        ]}
    ]),

    {ok, _} = cowboy:start_tls(https_listener, 100, [
        {port, Port},
        {cacertfile, "../priv/ssl/bitgame-ca.crt"},
        {certfile, "../priv/ssl/server.crt"},
        {keyfile, "../priv/ssl/server.key"}
    ], #{env => #{dispatch => Dispatch}}),

    {ok, true}.

handle_cast(_Rec, Status) ->
    {noreply, Status}.

handle_call(_Rec, _FROM, Status) ->
    {reply, ok, Status}.

handle_info(_Info, Status) ->
    {noreply, Status}.

terminate(normal, Status) ->
    {ok, Status}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.
