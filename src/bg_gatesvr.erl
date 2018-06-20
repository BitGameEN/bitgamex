%%%--------------------------------------
%%% @Module  : bg_gatesvr
%%% @Description: 游戏网关服
%%%--------------------------------------
-module(bg_gatesvr).
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%%开启网关服
%%Port:端口
start_link(Port) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
    Dispatch = cowboy_router:compile([{'_', [
            {"/", bg_gatesvr_cb, []}
        ]}
    ]),

    {ok, _} = cowboy:start_tls(https_listener, 100, [
        {port, 10000 + Port},
        {cacertfile, "../priv/ssl/bitgame-ca.crt"},
        {certfile, "../priv/ssl/server.crt"},
        {keyfile, "../priv/ssl/server.key"}
    ], #{env => #{dispatch => Dispatch}}),
    %{ok, _} = cowboy:start_clear(http_listener, 100, [{port, Port}], #{env => #{dispatch => Dispatch}}),

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
