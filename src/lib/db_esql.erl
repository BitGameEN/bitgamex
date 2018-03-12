%%%--------------------------------------
%%% @Module  : db_esql
%%% @Description: MYSQL数据库操作（基于emysql）
%%%--------------------------------------
-module(db_esql).
-export(
    [
        prepare/2,
        multi_execute/2,
        execute/2,
        execute2/2,
        execute/3,
        execute2/3,
        get_one/2,
        get_one/3,
        get_row/2,
        get_row/3,
        get_all/2,
        get_all/3,
        transaction/2,
        reset_one_connection/1
    ]
).
-include("common.hrl").
-include("emysql.hrl").

reset_one_connection(DB) ->
    Conn = emysql_conn_mgr:wait_for_connection(DB),
    case util:unixtime() > Conn#emysql_connection.last_test_time + 3600 of
        false ->
            emysql_conn_mgr:pass_connection(Conn),
            void;
        true ->
            emysql_conn:reset_connection(emysql_conn_mgr:pools(), Conn, pass)
    end.

transaction(DB, Fun) ->
    emysql:transaction(DB, Fun).

prepare(StmtName, Sql) when is_atom(StmtName) ->
    emysql:prepare(StmtName, Sql).

multi_execute(DB, Sql) ->
    case emysql:execute(DB, Sql) of
        #error_packet{msg=Reason} ->
            ?ERR("~n[Database Error]: ~nQuery:   ~ts~nError:   ~ts", [Sql, Reason]),
            error;
        L when is_list(L) ->
            case lists:last(L) of
                #result_packet{rows=Rows} ->
                    {ok, Rows};
                #ok_packet{affected_rows=_R} ->
                    {ok, updated}
            end
    end.

%% 执行一个SQL查询,返回影响的行数
execute(DB, Sql) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql) of
        #ok_packet{affected_rows=R} -> R;
        [#ok_packet{affected_rows=R}|_] -> R; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.
execute(DB, Sql, Args) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql, Args) of
        #ok_packet{affected_rows=R} -> R;
        [#ok_packet{affected_rows=R}|_] -> R; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.

%% 执行一条sql语句
%% @spec execute(Sql) -> {ok, Result} | error
execute2(DB, Sql) ->
    case emysql:execute(DB, Sql) of
        #result_packet{rows=Rows} ->
            {ok, Rows};
        [#result_packet{rows=Rows}|_] -> % 存储过程
            {ok, Rows};
        #ok_packet{affected_rows=_R} ->
            {ok, updated};
        [#ok_packet{affected_rows=_R}|_] -> % 存储过程
            {ok, updated};
        #error_packet{msg=Reason} ->
            ?ERR("~n[Database Error]: ~nQuery:   ~ts~nError:   ~ts", [Sql, Reason]),
            error
    end.
execute2(DB, Sql, Args) ->
    case emysql:execute(DB, Sql, Args) of
        #result_packet{rows=Rows} ->
            {ok, Rows};
        [#result_packet{rows=Rows}|_] -> % 存储过程
            {ok, Rows};
        #ok_packet{affected_rows=_R} ->
            {ok, updated};
        [#ok_packet{affected_rows=_R}|_] -> % 存储过程
            {ok, updated};
        #error_packet{msg=Reason} ->
            ?ERR("~n[Database Error]: ~nQuery:   ~ts~nError:   ~ts", [Sql, Reason]),
            error
    end.

%% 取出查询结果中的第一行第一列
%% 未找到时返回null
get_one(DB, Sql) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql) of
        #result_packet{rows=[]} -> null;
        [#result_packet{rows=[]}|_] -> null; % 存储过程
        #result_packet{rows=[[R]]} -> R;
        [#result_packet{rows=[[R]]}|_] -> R; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.
get_one(DB, Sql, Args) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql, Args) of
        #result_packet{rows=[]} -> null;
        [#result_packet{rows=[]}|_] -> null; % 存储过程
        #result_packet{rows=[[R]]} -> R;
        [#result_packet{rows=[[R]]}|_] -> R; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.

%% 取出查询结果中的第一行
get_row(DB, Sql) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql) of
        #result_packet{rows=[]} -> [];
        [#result_packet{rows=[]}|_] -> []; % 存储过程
        #result_packet{rows=[R]} -> R;
        [#result_packet{rows=[R]}|_] -> R; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.
get_row(DB, Sql, Args) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql, Args) of
        #result_packet{rows=[]} -> [];
        [#result_packet{rows=[]}|_] -> []; % 存储过程
        #result_packet{rows=[R]} -> R;
        [#result_packet{rows=[R]}|_] -> R; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.

%% 取出查询结果中的所有行
get_all(DB, Sql) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql) of
        #result_packet{rows=Rows} -> Rows;
        [#result_packet{rows=Rows}|_] -> Rows; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.
get_all(DB, Sql, Args) ->
    record_sql_to_file(Sql),
    case emysql:execute(DB, Sql, Args) of
        #result_packet{rows=Rows} -> Rows;
        [#result_packet{rows=Rows}|_] -> Rows; % 存储过程
        #error_packet{msg=Reason} -> mysql_halt([Sql, Reason])
    end.

%% @doc 显示人可以看得懂的错误信息
mysql_halt([Sql, Reason]) ->
    erlang:error({db_error, [Sql, Reason]}).

%% 记录sql语句到文件
-ifdef(record_sql).
record_sql_to_file(Sql) when is_list(Sql) ->
    {ok, S} = file:open("sql.txt", [append]),
    io:format(S, "~ts~n~n", [Sql]),
    file:close(S);
record_sql_to_file(Sql) ->
    {ok, S} = file:open("sql.txt", [append]),
    io:format(S, "~p~n~n", [Sql]),
    file:close(S).
-else.
record_sql_to_file(_) -> ok.
-endif.