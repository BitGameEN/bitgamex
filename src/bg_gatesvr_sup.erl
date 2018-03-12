%%%-----------------------------------
%%% @Module  : bg_gatesvr_sup
%%% @Description: 监控树
%%%-----------------------------------
-module(bg_gatesvr_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

-include("common.hrl").

start_link([Ip, Port, ServerId]) ->
    supervisor:start_link({local,?MODULE}, ?MODULE, [Ip, Port, ServerId]).

init([Ip, Port, ServerId]) ->
    Children =
        [
            {
                bg_gatesvr,
                {bg_gatesvr, start_link, [Port]},
                permanent,
                10000,
                supervisor,
                [bg_gatesvr]
            },
            {
                mod_disperse,
                {mod_disperse, start_link, [Ip, Port, ServerId, ?SVRTYPE_GATE]},
                permanent,
                10000,
                supervisor,
                [mod_disperse]
            },
            {
                mod_rand,
                {mod_rand, start_link, []},
                permanent,
                10000,
                supervisor,
                [mod_rand]
            }
        ],

    {ok,
        {
            {one_for_one, 10, 10},
            Children
        }
    }.
