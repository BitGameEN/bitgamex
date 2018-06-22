%%%-----------------------------------
%%% @Module  : lib_rpc
%%% @Description: rpc库模块
%%%-----------------------------------
-module(lib_rpc).
-export([rpc/4]).

-include("common.hrl").
-include("record.hrl").

rpc(SvrType, M, F, A) ->
    Server = get_svr(SvrType),
    case rpc:call(Server#server.node, M, F, A) of
        {badrpc, Reason} ->
            ?ERR("rpc exception:~nerr_msg=~p~n", [Reason]),
            throw({?ERRNO_EXCEPTION, ?T2B(Reason)});
        {ErrNo, ErrMsg} when is_integer(ErrNo), is_binary(ErrMsg) ->
            throw({ErrNo, ErrMsg});
        Ret -> Ret
    end.

get_svr(?SVRTYPE_GAME) ->
    case mod_disperse:gamesvr_list() of
        [] -> throw({?ERRNO_NO_RPC_SERVER, <<"no game server">>});
        Servers -> get_rand_svr(Servers)
    end;
get_svr(?SVRTYPE_XCHG) ->
    case mod_disperse:xchgsvr_list() of
        [] -> throw({?ERRNO_NO_RPC_SERVER, <<"no xchg server">>});
        Servers -> get_rand_svr(Servers)
    end.

get_rand_svr([Server]) ->
    Server;
get_rand_svr(Servers) ->
    Len = length(Servers),
    N = util:rand(1, Len),
    lists:nth(N, Servers).

