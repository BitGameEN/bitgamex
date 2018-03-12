%% @doc Supervisor for the mb application.

-module(mbcs_sup).
-author('luxiangyu@msn.com').

-behaviour(application).
-behaviour(supervisor).

%% External exports
-export([start/0, start/2, init/1, stop/1, upgrade/0]).


%% @spec start() -> ServerRet
%% @doc API for starting the supervisor.
start(normal, Args) ->
SupName = {local,?MODULE},
case supervisor:start_link(SupName, ?MODULE, [Args]) of
{ok, Pid} ->
{ok, Pid, {normal, Args}};
Error ->
Error
end;
start(_, _) ->
{error, badarg}.

start() ->
SupName = {local,?MODULE},
supervisor:start_link(SupName, ?MODULE, []).

stop(_StartArgs) ->
ok.

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
{ok, {_, Specs}} = init([]),
[supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
ok.

%% @spec init(_) -> SupervisorTree
%% @doc supervisor callback, ensures yaws is in embedded mode and then
%% returns the supervisor tree.
init([]) -> % Supervisor
init();
init([[]]) -> % Application
init();
init(BadArg) ->
{error, {badarg, BadArg}}.
init() ->
Processes = [{mbcs_server, {mbcs_server, start, []},
permanent, brutal_kill, worker, [mbcs_server]}],
{ok, {{one_for_one, 1, 60}, Processes}}.

