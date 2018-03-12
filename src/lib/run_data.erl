%%%--------------------------------------
%%% @Module  : run_data
%%% @Description: run数据层公共操作
%%% 事务的覆盖范围是某进程开始调用trans_begin到调用trans_end之间的该进程内部执行的逻辑
%%%--------------------------------------
-module(run_data).

% 逻辑调用接口
-export([trans_begin/0, trans_commit/0, trans_rollback/0, trans_set_commit_function/1, trans_set_pre_commit_function/1, trans_set_post_commit_function/1, trans_set_rollback_function/1]).

% run_xxx模块调用
-export([in_trans/0, trans_get/2, trans_set/5, trans_del/4]).
-export([lookup_one/3]).
-export([db_write/2, db_write/3, set_db_write_over_function/2, get_db_write_over_function/1]).

-include("common.hrl").


trans_begin() ->
    case get(run_data_trans) of
        undefined ->
            put(run_data_trans, 1);
        N ->
            put(run_data_trans, N + 1)
    end,
    ok.

trans_commit() ->
    case get(run_data_trans) of
        N when is_integer(N), N > 1 ->
            put(run_data_trans, N - 1);
        1 ->
            pre_trans_commit(),
            put(run_data_trans, 0),
            do_trans_commit();
        _ ->
            put(run_data_trans, 0)
    end.

pre_trans_commit() ->
    case get(run_data_trans_pre_commit_funs) of
        undefined ->
            void;
        PreCommitFs ->
            [PreCommitF() || PreCommitF <- lists:reverse(PreCommitFs)]
    end,
    erase(run_data_trans_pre_commit_funs),
    ok.

do_trans_commit() ->
    case get(run_data_trans_commit_funs) of
        undefined ->
            void;
        CommitFs ->
            [CommitF() || CommitF <- lists:reverse(CommitFs)]
    end,
    case get(run_data_trans_keys) of
        undefined ->
            void;
        Keys ->
            F = fun({T, K}) ->
                    case get({T, K}) of
                        undefined -> void;
                        trans_deleted -> void;
                        {trans_inserted, V} -> void; % CommitF中已执行
                        V -> T:set_one(V)
                    end;
                   (_) -> void
                end,
            [begin F(Key), erase(Key) end || Key <- Keys]
    end,
    case get(run_data_trans_post_commit_funs) of
        undefined ->
            void;
        PostCommitFs ->
            [PostCommitF() || PostCommitF <- lists:reverse(PostCommitFs)]
    end,
    erase(run_data_trans_keys),
    erase(run_data_trans_commit_funs),
    erase(run_data_trans_post_commit_funs),
    erase(run_data_trans_rollback_funs),
    ok.

trans_rollback() ->
    put(run_data_trans, 0),
    case get(run_data_trans_keys) of
        undefined ->
            void;
        Keys ->
            [erase(Key) || Key <- Keys]
    end,
    case get(run_data_trans_rollback_funs) of
        undefined ->
            void;
        RollbackFs ->
            [RollbackF() || RollbackF <- RollbackFs]
    end,
    erase(run_data_trans_keys),
    erase(run_data_trans_commit_funs),
    erase(run_data_trans_pre_commit_funs),
    erase(run_data_trans_post_commit_funs),
    erase(run_data_trans_rollback_funs),
    ok.

in_trans() ->
    N = get(run_data_trans),
    is_integer(N) andalso N > 0.

trans_get(Table, Key) ->
    case get({Table, Key}) of
        undefined -> [];
        {trans_inserted, R} -> R;
        R -> R
    end.

trans_set(Table, Key, Value, CommitF, RollbackF) ->
    case get({Table, Key}) of
        trans_deleted ->
            void;
        _ ->
            put({Table, Key}, Value),
            case get(run_data_trans_keys) of
                undefined ->
                    put(run_data_trans_keys, [{Table, Key}]);
                Keys ->
                    case lists:member({Table, Key}, Keys) of
                        true ->
                            void;
                        false ->
                            put(run_data_trans_keys, [{Table, Key} | Keys])
                    end
            end,
            case is_function(CommitF, 0) of
                true ->
                    case get(run_data_trans_commit_funs) of
                        undefined ->
                            put(run_data_trans_commit_funs, [CommitF]);
                        CommitFs ->
                            put(run_data_trans_commit_funs, [CommitF | CommitFs])
                    end;
                false ->
                    void
            end,
            case is_function(RollbackF, 0) of
                true ->
                    case get(run_data_trans_rollback_funs) of
                        undefined ->
                            put(run_data_trans_rollback_funs, [RollbackF]);
                        RollbackFs ->
                            put(run_data_trans_rollback_funs, [RollbackF | RollbackFs])
                    end;
                false ->
                    void
            end
    end,
    ok.

trans_set_commit_function(CommitF) ->
    case in_trans() andalso is_function(CommitF, 0) of
        true ->
            case get(run_data_trans_commit_funs) of
                undefined ->
                    put(run_data_trans_commit_funs, [CommitF]);
                CommitFs ->
                    put(run_data_trans_commit_funs, [CommitF | CommitFs])
            end;
        false ->
            case is_function(CommitF, 0) of
                true ->
                    CommitF();
                false ->
                    void
            end
    end,
    ok.

% 该接口设置的函数将在事务提交之前执行，如果产生异常，将会回滚
trans_set_pre_commit_function(CommitF) ->
    case in_trans() andalso is_function(CommitF, 0) of
        true ->
            case get(run_data_trans_pre_commit_funs) of
                undefined ->
                    put(run_data_trans_pre_commit_funs, [CommitF]);
                CommitFs ->
                    put(run_data_trans_pre_commit_funs, [CommitF | CommitFs])
            end;
        false ->
            void
    end,
    ok.

trans_set_post_commit_function(CommitF) ->
    case in_trans() andalso is_function(CommitF, 0) of
        true ->
            case get(run_data_trans_post_commit_funs) of
                undefined ->
                    put(run_data_trans_post_commit_funs, [CommitF]);
                CommitFs ->
                    put(run_data_trans_post_commit_funs, [CommitF | CommitFs])
            end;
        false ->
            void
    end,
    ok.

trans_set_rollback_function(RollbackF) ->
    case in_trans() andalso is_function(RollbackF, 0) of
        true ->
            case get(run_data_trans_rollback_funs) of
                undefined ->
                    put(run_data_trans_rollback_funs, [RollbackF]);
                RollbackFs ->
                    put(run_data_trans_rollback_funs, [RollbackF | RollbackFs])
            end;
        false ->
            void
    end,
    ok.

trans_del(Table, Key, CommitF, RollbackF) ->
    trans_set(Table, Key, trans_deleted, CommitF, RollbackF).

lookup_one(Table, Key, IsMnesia) ->
    run_data_ver:upgrade_if_need(util:lookup_one(Table, Key, IsMnesia)).

db_write(R, WriteF) ->
    case is_function(WriteF, 0) of
        true ->
            F = case run_data:get_db_write_over_function(R) of
                    {true, OverF} when is_function(OverF, 1) ->
                        fun() -> WriteF(), OverF(R) end;
                    _ -> WriteF
                end,
            DBWriterPid = get(db_writer_pid),
            case util:is_process_alive(DBWriterPid) of
                true ->
                    mod_db_writer:to_write(DBWriterPid, F);
                false ->
                    spawn(F)
            end;
        false ->
            void
    end.

db_write(Op, R, WriteF) ->
    case is_function(WriteF, 0) of
        true ->
            F = case run_data:get_db_write_over_function(R) of
                    {true, OverF} when is_function(OverF, 1) ->
                        fun() -> WriteF(), OverF(R) end;
                    _ -> WriteF
                end,
            DBWriterPid = get(db_writer_pid),
            case util:is_process_alive(DBWriterPid) of
                true ->
                    Table = element(1, R),
                    ObjId = element(3, R),
                    mod_db_writer:to_write(DBWriterPid, {Op, Table, ObjId, F});
                false ->
                    spawn(F)
            end;
        false ->
            void
    end.

set_db_write_over_function(PredF, ActF) when is_function(PredF, 1), is_function(ActF, 1) ->
    case get(db_write_over_functions) of
        undefined -> put(db_write_over_functions, [{PredF, ActF}]);
        L -> put(db_write_over_functions, [{PredF, ActF} | L])
    end.

get_db_write_over_function(R) ->
    case get(db_write_over_functions) of
        undefined -> false;
        L ->
            F = fun({PredF, ActF}, {false, Rest}) ->
                    case PredF(R) of
                        true -> {{true, ActF}, Rest};
                        false -> {false, [{PredF, ActF}|Rest]}
                    end;
                   ({PredF, ActF}, {{true, FoundActF}, Rest}) -> {{true, FoundActF}, [{PredF, ActF}|Rest]}
                end,
            {Res, NewL} = lists:foldr(F, {false, []}, L),
            put(db_write_over_functions, lists:reverse(NewL)),
            Res
    end.

