%%%------------------------------------
%%% @Module  : mod_db_writer
%%% @Description: 数据库写进程
%%% 数据库写入耗时，避免影响消息响应时间需要异步，但相关的写入不能乱序而且要避免mysql innodb死锁
%%%------------------------------------
-module(mod_db_writer).
-behaviour(gen_server).
-export([start/0, start_link/1, wake_up/1, to_write/2, flush/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("common.hrl").

-define(HIBERNATE_DELAY, 300000).
-define(TO_WRITE_INTERVAL, 30 * 1000).

-record(writer, {
        player_id = 0,
        writer_pid = none
    }).

start() ->
    gen_server:start(?MODULE, {0, none}, []).

start_link(InitArgs) ->
    % 注意：Erlang的设计思想是父进程退，必让子进程退，防止孤儿进程的出现，因此，
    % 不用start_link，防止父进程退出时，会强制子进程退出，替代做法是延迟到init时再主动去link，这样不会被认作父子进程关系
    Parent = self(),
    gen_server:start(?MODULE, {InitArgs, Parent}, []).

to_write(Pid, WriteInfo) ->
    gen_server:cast(Pid, {to_write, WriteInfo, erlang:get_stacktrace()}).

flush(Pid) ->
    gen_server:cast(Pid, flush).

init({PlayerId, Parent}) ->
    process_flag(trap_exit, true),
    case Parent of
        none -> void;
        _ ->
            put(parent, Parent),
            link(Parent)
    end,
    erlang:send_after(?HIBERNATE_DELAY, self(), goto_sleep),
    {ok, #writer{player_id = PlayerId, writer_pid = self()}}.

handle_cast({to_write, WriteF, CasterStack}, Status) when is_function(WriteF, 0) ->
    write_one(WriteF, CasterStack),
    {noreply, Status};

handle_cast({to_write, {Op, Table, Key, WriteF}, CasterStack}, Status) ->
    % 暂时不延迟写入
    %case Op of
    %    add ->
    %        write_one(WriteF, CasterStack);
    %    upd ->
    %        put({Table, Key}, {WriteF, CasterStack}),
    %        case get(to_write_keys) of
    %            undefined ->
    %                put(to_write_keys, [{Table, Key}]);
    %            Keys ->
    %                case lists:member({Table, Key}, Keys) of
    %                    true ->
    %                        void;
    %                    false ->
    %                        put(to_write_keys, [{Table, Key} | Keys])
    %                end
    %        end,
    %        case get(to_write_event) of
    %            undefined ->
    %                TRef = erlang:send_after(?TO_WRITE_INTERVAL, self(), write_delay_ones),
    %                put(to_write_event, TRef);
    %            _ ->
    %                void
    %        end;
    %    del ->
    %        erase({Table, Key}),
    %        write_one(WriteF, CasterStack)
    %end,
    write_one(WriteF, CasterStack),
    {noreply, Status};

handle_cast(flush, Status) ->
    N = do_write_delay_ones(),
    ?DBG("mod_db_writer flush to db(~p) right now --- ~p~n", [N, Status#writer.player_id]),
    {noreply, Status};

handle_cast(_R, Status) ->
    {noreply, Status}.

handle_call(_R, _FROM, Status) ->
    {reply, ok, Status}.


handle_info(write_delay_ones, Status) ->
    TRef = erlang:send_after(?TO_WRITE_INTERVAL, self(), write_delay_ones),
    put(to_write_event, TRef),
    N = do_write_delay_ones(),
    ?DBG("mod_db_writer flush to db(~p) --- ~p~n", [N, Status#writer.player_id]),
    {noreply, Status};

handle_info({player_id, PlayerId, Parent}, Status) ->
    put(parent, Parent),
    link(Parent),
    {noreply, Status#writer{player_id = PlayerId}};

handle_info(goto_sleep, Status) ->
    N = do_write_delay_ones(),
    ?DBG("mod_db_writer flush to db(~p) goto_sleep --- ~p~n", [N, Status#writer.player_id]),
    TRef = get(to_write_event),
    catch erlang:cancel_timer(TRef),
    erase(to_write_event),
    ?DBG("mod_db_writer sleep --- ~p~n", [Status#writer.player_id]),
    proc_lib:hibernate(mod_db_writer, wake_up, [Status]);

handle_info({'EXIT', Pid, Reason}, Status) ->
    case Pid =:= get(parent) of
        true -> {stop, normal, Status};
        false -> {noreply, Status}
    end;

handle_info(stop, Status) ->
    {stop, normal, Status};

handle_info(_Reason, Status) ->
    {noreply, Status}.

terminate(_Reason, Status) ->
    N = do_write_delay_ones(),
    ?DBG("mod_db_writer flush to db(~p) when terminating --- ~p~n", [N, Status#writer.player_id]),
    {ok, Status}.

code_change(_OldVsn, Status, _Extra) ->
    {ok, Status}.

wake_up(#writer{player_id = PlayerId, writer_pid = WriterPid} = Status) ->
    ?DBG("mod_db_writer wake up --- ~p~n", [PlayerId]),
    erlang:send_after(?HIBERNATE_DELAY, WriterPid, goto_sleep),
    gen_server:enter_loop(?MODULE, [], Status, 0).

write_one(WriteF, CasterStack) ->
    try
        WriteF()
    catch
        _:ErrMsg ->
            ?ERR("db_writer failed:~n~p~nstack (when requested) = ~p~n", [ErrMsg, CasterStack])
    end.

do_write_delay_ones() ->
    case get(to_write_keys) of
        undefined -> 0;
        Keys ->
            erase(to_write_keys),
            L = [begin
                     case get(Key) of
                         undefined -> void;
                         {WriteF, CasterStack} ->
                             erase(Key),
                             write_one(WriteF, CasterStack),
                             ok
                     end
                 end || Key <- Keys],
            length([O || O <- L, O =:= ok])
    end.

