%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%    http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(recon_web_handle).
-include("recon_web.hrl").

%%API
-export([
  notify_handler_have_new_message/2]).

%%CALLBACK
-export([init/2,
  terminate/3,
  websocket_init/1,
  websocket_info/2,
  websocket_handle/2]).


-record(http_state, {action, config = #config{},
  session_id, heartbeat_tref, pid = undefined}).

%% @doc session tell handler has new message in session
-spec notify_handler_have_new_message(pid(), pid()) -> ok.
notify_handler_have_new_message(HanderPid, SessionPid) ->
  erlang:send(HanderPid, {message_arrived, SessionPid}).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  handlers callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

init(Req, [Config]) ->
  Method = cowboy_req:method(Req),
  PathInfo = cowboy_req:path_info(Req),
  lager:debug("+++++ PathInfo:~p~n", [PathInfo]),
  init_by_method(Method, PathInfo, Config, Req).

websocket_init(State) ->
    self() ! post_init,
    {ok, State}.

%%1) http://127.0.0.1:8080/socket.io/1/?t=1436608179209
init_by_method(_Method, [] = _PathInfo,
    Config = #config{heartbeat_timeout = HeartbeatTimeout, session_timeout = SessionTimeout, opts = Opts},
    Req) ->
  Sid = recon_web_uuids:new(),
  HeartbeatTimeoutBin = list_to_binary(integer_to_list(HeartbeatTimeout div 1000)),
  SessionTimeoutBin = list_to_binary(integer_to_list(SessionTimeout div 1000)),

  {ok, _SessionPid} = recon_web_session:create(Sid, SessionTimeout, Opts),
  Result = << ":", HeartbeatTimeoutBin/binary, ":", SessionTimeoutBin/binary, ":websocket, xhr-polling">>,
  Req1 = cowboy_req:reply(200, ?TEXT_HEAD, <<Sid/binary, Result/binary>>, Req),
  {ok, Req1, #http_state{action = ?CREATE_SESSION, config = Config}};

%%2) ws://127.0.0.1:8080/socket.io/1/websocket/8080fa8492eb609e79471f1c5e396681 GET
init_by_method(_Method, [<<"websocket">>, Sid], Config, Req) ->
  {cowboy_websocket, Req, {Config, Sid}};

%%3) ws://127.0.0.1:8080/socket.io/xhr-polling/8080fa8492eb609e79471f1c5e396681 GET
init_by_method(<<"GET">>, [<<"xhr-polling">>, Sid], Config = #config{protocol = Protocol}, Req) ->
  case recon_web_session:find(Sid) of
    {ok, Pid} ->
      case recon_web_session:set_caller_and_pull_msg(Pid, self()) of
        ?SESSION_IN_USE ->
          {ok, Req, #http_state{action = ?SESSION_IN_USE, config = Config, session_id = Sid}};
        [] ->
          TRef = erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
          {cowboy_loop, Req, #http_state{action = ?HEARTBEAT, config = Config, session_id = Sid,
            heartbeat_tref = TRef, pid = Pid}, infinity};
        %%{ok, Req, #http_state{action = ?DATA, config = Config, session_id = Sid, pid = Pid}};
        Messages ->
          Req1 = cowboy_req:reply(200, ?TEXT_HEAD, Protocol:encode(Messages), Req),
          {ok, Req1, #http_state{action = ?DATA, config = Config, session_id = Sid, pid = Pid}}
      end;
    {error, not_found} ->
      {ok, Req, #http_state{action =?SESSION_NOT_FIND, session_id = Sid, config = Config}}
  end;

%% @todo support POST sometime client will send post .... bug!!!
init_by_method(<<"POST">>, [<<"xhr-polling">>, Sid], Config=#config{protocol = Protocol}, Req) ->
  case recon_web_session:find(Sid) of
    {ok, Pid} ->
      case cowboy_req:body(Req) of
        {ok, Body, Req1} ->
          Messages = Protocol:decode(Body),
          recon_web_session:deliver_msg(Pid, Messages),
          {ok, Req1, #http_state{action = ?OK, config = Config, session_id = Sid}};
        {error, _} ->
          {stop, Req, #http_state{action =?WRONG_ACTION, config = Config, session_id = Sid}}
      end;
    {error, not_found} ->
      {ok, Req, #http_state{action =?SESSION_NOT_FIND, session_id = Sid, config = Config}}
  end;

init_by_method(_Method, _PathInfo, Config, Req) ->
  {ok, Req, #http_state{config = Config}}.

terminate(_Reason, _Req, _HttpState = #http_state{action = Action}) when
  Action == ?CREATE_SESSION;Action == ?SESSION_IN_USE->
  ok;
terminate(_Reason, _Req, _HttpState = #http_state{heartbeat_tref = HeartbeatTRef, pid = Pid}) ->
  safe_unlink_caller(Pid, self()),
  case HeartbeatTRef of
    undefined -> ok;
    _ -> erlang:cancel_timer(HeartbeatTRef)
  end;
terminate(Reason, _Req, {_Config, Pid}) ->
  lager:info("recon_web_handler1 terminate: Reason=~p~n", [Reason]),
  recon_web_session:disconnect(Pid),
  ok;
terminate(Reason, _Req, State) ->
  lager:info("recon_web_handler2 terminate: Reason=~p~n,State~p~n", [Reason, State]),
  ok.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Websocket handlers callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
websocket_info(post_init, {Config, Sid}) ->
  case recon_web_session:find(Sid) of
    {ok, Pid} ->
      erlang:monitor(process, Pid),
      self() ! {go, Sid},
      erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
      {ok, {Config, Pid}};
    {error, not_found} -> {stop, {Config, undefined}}
  end;

websocket_info({go, Sid}, {Config, Pid}) ->
  case recon_web_session:set_caller_and_pull_msg(Pid, self()) of
    session_in_use -> {ok, {Config, Pid}};
    Messages ->
      notify_when_first_connect(Messages, Pid, Sid),
      reply_ws_messages(Messages, {Config, Pid})
  end;

websocket_info({timeout, _TRef, {?MODULE, Pid}}, {Config = #config{protocol = Protocol}, Pid}) ->
  recon_web_session:refresh(Pid),
  erlang:start_timer(Config#config.heartbeat, self(), {?MODULE, Pid}),
  {reply, {text, Protocol:encode(heartbeat)}, {Config, Pid}};

websocket_info({message_arrived, Pid}, State) ->
  Messages =  recon_web_session:pull_msg_from_session(Pid),
  reply_ws_messages(Messages, State);

%%heartbeat
websocket_info({timeout, _TRef, {?MODULE, Pid}}, State =
  {#config{protocol = Protocol, heartbeat = Heartbeat}, Pid}) ->
  recon_web_session:refresh(Pid),
  erlang:start_timer(Heartbeat, self(), {?MODULE, Pid}),
  Packet = Protocol:encode(heartbeat),
  {reply, {text, Packet}, State};

%% session process DOWN because we monitor before
websocket_info({'DOWN', _Ref, process, Pid, _Reason}, State = {_Config, Pid}) ->
  {stop, State};

websocket_info(Info, State) ->
  lager:info("unknown wesocket_info: ~p~n", [?MODULE, Info]),
  {ok, State}.

%% message from client websocket
websocket_handle({text, Data}, State = {#config{protocol = Protocol}, Pid}) ->
  Messages = Protocol:decode(Data),
  lager:debug("from client text~p~n", [Messages]),
  recon_web_session:deliver_msg(Pid, Messages),
  {ok, State};

websocket_handle(Data, State) ->
  lager:info("unknown data from client: ~p~n", [Data]),
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% INTERNAL FUNCTION
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

safe_unlink_caller(Pid, Caller) ->
  case Pid =/= undefined andalso is_process_alive(Pid) of
    true -> recon_web_session:unsub_caller(Pid, Caller);
    false -> ok
  end.

notify_when_first_connect(Messages, SessionPid, Sid) ->
  case lists:keyfind(connect, 1, Messages) of
    {connect, <<>>} ->  recon_server:request_system_info(SessionPid, Sid);
    _ -> ok
  end.

reply_ws_messages(Messages, State = {#config{protocol = Protocol}, _Pid}) ->
  case Protocol:encode(Messages) of
    <<>> -> {ok, State};
    Packet -> {reply, {text, Packet}, State}
  end.