%%%-----------------------------------
%%% @Module  : bg_xchgsvr_sup
%%% @Description: 监控树
%%%-----------------------------------
-module(bg_xchgsvr_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("common.hrl").

start_link([Ip, ServerId]) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [Ip, ServerId]).

init([Ip, ServerId]) ->
    Children =
        [
            {
                bg_xchgsvr,
                {bg_xchgsvr, start_link, []},
                permanent,
                10000,
                supervisor,
                [bg_xchgsvr]
            },
            {
                mod_disperse,
                {mod_disperse, start_link, [Ip, 0, ServerId, ?SVRTYPE_XCHG]},
                permanent,
                10000,
                supervisor,
                [mod_disperse]
            },
            {
                uuid_factory,
                {uuid_factory, start_link, []},
                permanent,
                10000,
                supervisor,
                [uuid_factory]
            },
            {
                httpc_proxy,
                {httpc_proxy, start_link, []},
                permanent,
                10000,
                supervisor,
                [httpc_proxy]
            }
        ],
    {ok,
        {
            {one_for_one, 10, 10},
            Children
        }
    }.

