%%%------------------------------------
%%% @Module  : mod_disperse
%%% @Description: 服务器自动发现
%%%------------------------------------
-module(mod_disperse).
-behaviour(gen_server).

-export([start_link/4,
         rpc_server_add/5,
         server_id/0,
         server_type/0,
         port/0,
         gamesvr_list/0,
         xchgsvr_list/0
        ]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").
-include("record.hrl").
-include("gameConfigPlatform.hrl").

-define(WAIT_TIME, 5000).

-record(state, {
        id,
        ip,
        port,
        node,
        type
    }
).

%% 查询当前服务器id号
%% 返回:int()
server_id() ->
    Node = node(),
    {ok, MP} = re:compile("[0-9]+@", []),
    {match, [S]} = re:run(atom_to_binary(Node, utf8), MP, [{capture, first, list}]),
    list_to_integer(lists:sublist(S, 1, length(S) - 1)).

server_type() ->
    server_id() div 10000.

port() ->
    gen_server:call(?MODULE, get_port).

%% 获取所有游戏服务器的列表
%% 返回:[#server{} | ...]
gamesvr_list() ->
    ets:tab2list(?ETS_GAMESVR).

xchgsvr_list() ->
    ets:tab2list(?ETS_XCHGSVR).

%% 接收其它服务器的加入信息
rpc_server_add(Id, Node, Ip, Port, Type) ->
    ?MODULE ! {rpc_server_add, Id, Node, Ip, Port, Type}.

start_link(Ip, Port, ServerId, Type) ->
    gen_server:start_link({local,?MODULE}, ?MODULE, [Ip, Port, ServerId, Type], []).

init([Ip, Port, ServerId, Type]) ->
    process_flag(trap_exit, true),
    net_kernel:monitor_nodes(true),
    ets:new(?ETS_GAMESVR, [{keypos, #server.id}, named_table, public, set, ?ETSRC, ?ETSWC]),
    ets:new(?ETS_XCHGSVR, [{keypos, #server.id}, named_table, public, set, ?ETSRC, ?ETSWC]),
    State = #state{id = ServerId, ip = Ip, port = Port, node = node(), type = Type},
    add_server([State#state.ip, State#state.port, State#state.id, State#state.node, State#state.type]),
    WaitTime = case bg:env() of
                   ?ENV_DEV -> ?WAIT_TIME;
                   ?ENV_BETA -> ?WAIT_TIME;
                   _ -> ?WAIT_TIME * 2
               end,
    erlang:send_after(WaitTime, ?MODULE, get_and_call_server),
    {ok, State}.

handle_cast(_R , State) ->
    {noreply, State}.

%% 获取服务器id
handle_call(get_server_id, _From, State) ->
    {reply, State#state.id, State};

handle_call(get_port, _From, State) ->
    {reply, State#state.port, State};

handle_call(_R , _FROM, State) ->
    {reply, ok, State}.

%% 获取并通知当前所有服务器
handle_info(get_and_call_server, State) ->
    get_and_call_server(State),
    [erlang:monitor_node(Server#server.node, true) || Server <- gamesvr_list()],
    [erlang:monitor_node(Server#server.node, true) || Server <- xchgsvr_list()],
    case State#state.type of
        ?SVRTYPE_GATE ->
            lib_game:set_gamesvr_num(length(gamesvr_list()));
        _ -> void
    end,
    {noreply, State};

%% 新服务器加入
handle_info({rpc_server_add, Id, Node, Ip, Port, Type}, State) ->
    case Type of
        ?SVRTYPE_GATE ->
            skip;
        ?SVRTYPE_GAME ->
            case State#state.type of
                ?SVRTYPE_GATE ->
                    ?INFO("gamesvr added: id=~p, node=~p, ip=~p, port=~p, type=~p", [Id, Node, Ip, Port, Type]),
                    erlang:monitor_node(Node, true),
                    ets:insert(?ETS_GAMESVR, #server{id = Id, node = Node, ip = Ip, port = Port, type = Type}),
                    lib_game:set_gamesvr_num(length(gamesvr_list()));
                ?SVRTYPE_XCHG ->
                    ?INFO("gamesvr added: id=~p, node=~p, ip=~p, port=~p, type=~p", [Id, Node, Ip, Port, Type]),
                    erlang:monitor_node(Node, true),
                    ets:insert(?ETS_GAMESVR, #server{id = Id, node = Node, ip = Ip, port = Port, type = Type});
                _ ->
                    skip
            end;
        ?SVRTYPE_XCHG ->
            case State#state.type of
                ?SVRTYPE_GATE ->
                    ?INFO("xchgsvr added: id=~p, node=~p, ip=~p, port=~p, type=~p", [Id, Node, Ip, Port, Type]),
                    erlang:monitor_node(Node, true),
                    ets:insert(?ETS_XCHGSVR, #server{id = Id, node = Node, ip = Ip, port = Port, type = Type});
                ?SVRTYPE_GAME ->
                    ?INFO("xchgsvr added: id=~p, node=~p, ip=~p, port=~p, type=~p", [Id, Node, Ip, Port, Type]),
                    erlang:monitor_node(Node, true),
                    ets:insert(?ETS_XCHGSVR, #server{id = Id, node = Node, ip = Ip, port = Port, type = Type});
                _ ->
                    skip
            end;
        _ ->
            ?ERR("Unknown server type: id=~p, node=~p, ip=~p, port=~p, type=~p", [Id, Node, Ip, Port, Type]),
            skip
    end,
    {noreply, State};

%% 处理新节点加入事件
handle_info({nodeup, Node}, State) ->
    try
        rpc:cast(Node, mod_disperse, rpc_server_add, [State#state.id, State#state.node, State#state.ip, State#state.port, State#state.type])
    catch
        _:_ -> skip
    end,
    {noreply, State};

%% 处理节点关闭事件
handle_info({nodedown, Node}, State) ->
    case ets:match_object(?ETS_GAMESVR, #server{node = Node, _ = '_'}) of
        [S] ->
            ?INFO("gamesvr nodedown: node=~p", [Node]),
            del_server(S#server.id),
            ets:match_delete(?ETS_GAMESVR, #server{node = Node, _ = '_'});
        _ ->
            case ets:match_object(?ETS_XCHGSVR, #server{node = Node, _ = '_'}) of
                [S] ->
                    ?INFO("xchgsvr nodedown: node=~p", [Node]),
                    del_server(S#server.id),
                    ets:match_delete(?ETS_XCHGSVR, #server{node = Node, _ = '_'});
                _ ->
                    skip
            end
    end,
    {noreply, State};

handle_info(_Reason, State) ->
    {noreply, State}.

terminate(_R, State) ->
    {ok, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ----------------------- 私有函数 ---------------------------------

%%加入服务器集群
add_server([Ip, Port, ServerId, Node, Type]) ->
    db_esql:execute(?DB_USR, <<"replace into `server` (id, ip, port, node, type, state) values(?,?,?,?,?,0)">>, [ServerId, Ip, Port, Node, Type]).

%%退出服务器集群
del_server(ServerId) ->
    db_esql:execute(?DB_USR, io_lib:format(<<"update `server` set state = 1 where id = ~p">>, [ServerId])).

%%获取并通知所有服务器信息
get_and_call_server(#state{type = SelfType} = State) ->
    Sql = case SelfType of
            ?SVRTYPE_GATE ->
                io_lib:format(<<"select id, ip, port, node, type from server where type = ~p or type = ~p">>, [?SVRTYPE_GAME, ?SVRTYPE_XCHG]);
            ?SVRTYPE_GAME ->
                io_lib:format(<<"select id, ip, port, node, type from server where type = ~p or type = ~p">>, [?SVRTYPE_GATE, ?SVRTYPE_XCHG]);
            ?SVRTYPE_XCHG ->
                io_lib:format(<<"select id, ip, port, node, type from server where type = ~p or type = ~p">>, [?SVRTYPE_GATE, ?SVRTYPE_GAME])
        end,
    Servers = case Sql =:= <<>> of
                  true -> [];
                  false -> db_esql:get_all(?DB_USR, Sql)
              end,
    case Servers of
        [] -> [];
        _ ->
            F = fun([Id, Ip, Port, Node, Type]) ->
                    Node1 = list_to_atom(binary_to_list(Node)),
                    Ip1 = binary_to_list(Ip),
                    case Id /= State#state.id of  % 自己不写入和不通知
                        true ->
                            PongF = fun() ->
                                        case Type of
                                            ?SVRTYPE_GAME ->
                                                ?INFO("gamesvr added: id=~p, node=~p, ip=~p, port=~p, type=~p", [Id, Node, Ip, Port, Type]),
                                                ets:insert(?ETS_GAMESVR,
                                                    #server{
                                                        id = Id,
                                                        node = Node1,
                                                        ip = Ip1,
                                                        port = Port,
                                                        type = Type
                                                    }
                                                );
                                            ?SVRTYPE_XCHG ->
                                                ?INFO("xchgsvr added: id=~p, node=~p, ip=~p, port=~p, type=~p", [Id, Node, Ip, Port, Type]),
                                                ets:insert(?ETS_XCHGSVR,
                                                    #server{
                                                        id = Id,
                                                        node = Node1,
                                                        ip = Ip1,
                                                        port = Port,
                                                        type = Type
                                                    }
                                                );
                                            _ ->
                                                ok
                                        end,
                                        %% 通知已有的服务器加入当前服务器的节点
                                        rpc:cast(Node1, mod_disperse, rpc_server_add, [State#state.id, State#state.node, State#state.ip, State#state.port, State#state.type])
                                    end,
                            set_cookie_if_necessary(SelfType, Node1, Type),
                            case net_adm:ping(Node1) of
                                pong ->
                                    PongF();
                                pang ->
                                    ?INFO("pang! try again using self cookie to ping ~p...~n", [Node1]),
                                    erlang:set_cookie(Node1, erlang:get_cookie()),
                                    case net_adm:ping(Node1) of
                                        pong ->
                                            ?INFO("pong! connected.~n", []),
                                            PongF();
                                        pang ->
                                            del_server(Id)
                                    end
                            end;
                        false ->
                            ok
                    end
                end,
            [F(S) || S <- Servers]
    end.

set_cookie_if_necessary(SelfType, DestNode, DestType) ->
    Cookie = list_to_atom((atom_to_list(erlang:get_cookie()) -- integer_to_list(SelfType)) ++ integer_to_list(DestType)),
    erlang:set_cookie(DestNode, Cookie).

