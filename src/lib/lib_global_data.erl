%%%--------------------------------------
%%% @Module  : lib_global_data
%%% @Description:全局数据存储
%%%--------------------------------------
-module(lib_global_data).
-include("common.hrl").

-export(
    [
        init/0,
        read/2,
        write/2,
        read_mem_only/2,
        write_mem_only/2,
        del/1
    ]
).

-define(ETS_GLOBAL_RUN_DATA, ets_global_run_data).

init() ->
    ets:new(?ETS_GLOBAL_RUN_DATA, [named_table, public, set, ?ETSRC, ?ETSWC]),
    ok.

read(Key, DefaultVal) ->
    case ets:lookup(?ETS_GLOBAL_RUN_DATA, Key) of
        [] ->
            read_dets(Key, DefaultVal);
        [{_K, V}] ->
            V
    end.

read_dets(Key, DefaultVal) ->
    case dets:lookup(?DETS_GLOBAL_RUN_DATA, Key) of
        [] ->
            DefaultVal;
        [{K, V}] ->
            ets:insert(?ETS_GLOBAL_RUN_DATA, {K, V}),
            V
    end.

write(Key, Val) ->
    ets:insert(?ETS_GLOBAL_RUN_DATA, {Key, Val}),
    dets:insert(?DETS_GLOBAL_RUN_DATA, {Key, Val}).

read_mem_only(Key, DefaultVal) ->
    case ets:lookup(?ETS_GLOBAL_RUN_DATA, Key) of
        [] ->
            DefaultVal;
        [{_K, V}] ->
            V
    end.

write_mem_only(Key, Val) ->
    ets:insert(?ETS_GLOBAL_RUN_DATA, {Key, Val}).

del(Key) ->
    ets:delete(?ETS_GLOBAL_RUN_DATA, Key),
    dets:delete(?DETS_GLOBAL_RUN_DATA, Key).
