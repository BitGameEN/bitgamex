%%%--------------------------------------------------------
%%% @Module  : data_autogen_cfg
%%% @Description: 连接cfg库获取配置信息，自动生成cfg模块代码
%%%--------------------------------------------------------
-module(data_autogen_cfg).
-export([run/0, run/2]).

-define(DB, bg_mysql_cfg).
-define(DB_HOST, "localhost").
-define(DB_PORT, 3306).
-define(DB_USER, "bitgame").
-define(DB_PASS, "bitgame123").
-define(DB_NAME, "bitgame_cfg").
-define(DB_ENCODE, utf8).

init_db(DbHost) ->
    mysql:start_link(?DB, DbHost, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, fun(_, _, _, _) -> ok end, ?DB_ENCODE),
    mysql:connect(?DB, DbHost, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, ?DB_ENCODE, true),
    ok.

%% 取出查询结果中的所有行
db_get_all(Sql) ->
    case mysql:fetch(?DB, Sql) of
        {data, {_, _, R, _, _}} -> R;
        {error, {_, _, _, _, Reason}} -> mysql_halt([Sql, Reason])
    end.

%% 显示人可以看得懂的错误信息
mysql_halt([Sql, Reason]) ->
    erlang:error({db_error, [Sql, Reason]}).

run() ->
    case init:get_plain_arguments() of
        [] ->
            run(?DB_HOST);
        [DbHost] ->
            run(DbHost);
        [DbHost, Table] ->
            run(DbHost, Table)
    end.

run(DbHost) ->
    init_db(DbHost),
    gen("gold_type"),
    ok.

run(DbHost, "all") ->
    run(DbHost);
run(DbHost, Tables0) ->
    init_db(DbHost),
    Tables = string:tokens(Tables0, ","),
    [gen(Table) || Table <- Tables],
    ok.

gen(Table) ->
    SqlR = io_lib:format(<<"select * from ~s">>, [Table]),
    SqlNTDCK = io_lib:format(<<"SELECT column_name,data_type,column_default,column_comment,column_key from information_schema.columns WHERE information_schema.columns.TABLE_NAME = '~s' and information_schema.columns.TABLE_SCHEMA = '~s'">>, [Table, ?DB_NAME]),
    FieldInfos = db_get_all(SqlNTDCK),
    FieldNames = [N || [N|_] <- FieldInfos],
    FieldTypes = [T || [_,T|_] <- FieldInfos],
    FieldDefaults = [D || [_,_,D|_] <- FieldInfos],
    FieldComments = [C || [_,_,_,C|_] <- FieldInfos],
    FieldKeyInfos = [K || [_,_,_,_,K|_] <- FieldInfos],
    gen_record(io_lib:format(<<"record_cfg_~s.hrl">>, [Table]), Table, FieldNames, FieldTypes, FieldDefaults, FieldComments),
    gen_erl(io_lib:format(<<"cfg_~s.erl">>, [Table]), io_lib:format(<<"cfg_~s">>, [Table]), SqlR, FieldNames, FieldTypes, FieldKeyInfos, Table).

gen_record(HrlName, RecordName, FieldNames, FieldTypes, FieldDefaults, FieldComments) ->
    {ok, S} = file:open(HrlName, write),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "%%% @Module: ~s~n", [RecordName]),
    io:format(S, <<"%%% @Description: 自动生成~n"/utf8>>, []),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "~n", []),
    io:format(S, "-record(~s, {~n", [RecordName]),

    gen_record_fields(FieldNames, FieldTypes, FieldDefaults, FieldComments, S),
 
    io:format(S, "}).~n", []),
    file:close(S).

gen_record_fields([Name0 | RestNames], [Type | RestTypes], [Default0 | RestDefaults], [Comment | RestComments], S) ->
    Name = string:to_lower(binary_to_list(Name0)),
    Default = case Default0 of
        undefined ->
            case Type of
                <<"char">> -> <<"">>;
                <<"varchar">> -> <<"">>;
                <<"tinytext">> -> <<"[]">>;
                <<"text">> -> <<"[]">>;
                _-> <<"0">>
            end;
        _ ->
            Default0
    end,
    case Type of
        <<"char">> ->
            io:format(S, "\t\t~s = <<\"~s\"/utf8>>", [Name, Default]);
        <<"varchar">> ->
            io:format(S, "\t\t~s = <<\"~s\"/utf8>>", [Name, Default]);
        <<"tinytext">> ->
            io:format(S, "\t\t~s = ~s", [Name, Default]);
        <<"text">> ->
            io:format(S, "\t\t~s = ~s", [Name, Default]);
        _ ->
            io:format(S, "\t\t~s = ~s", [Name, Default])
    end,
    case RestNames of
        [] ->
            io:format(S, " % ~s~n", [Comment]);
        _ ->
            io:format(S, ", % ~s~n", [Comment]),
            gen_record_fields(RestNames, RestTypes, RestDefaults, RestComments, S)
    end.

gen_erl(ErlName, ModuleName, SqlR, FieldNames, FieldTypes, FieldKeyInfos, RecordName) ->
    {ok, S} = file:open(ErlName, write),
    gen_erl_note(RecordName, ModuleName, S),

    Rows = db_get_all(SqlR),

    KeyInfos = lists:zip(lists:seq(1, length(FieldKeyInfos)), FieldKeyInfos),
    KeyIndexes = [I || {I, KeyInfo} <- KeyInfos, KeyInfo =:= <<"PRI">>],

    % 生成get_ids/0
    gen_fun_get_ids(Rows, S, KeyIndexes),

    % 生成get/1
    [gen_fun_get(RecordName, FieldNames, FieldTypes, Row, S, KeyIndexes) || Row <- Rows],

    io:format(S, "get(_) ->~n", []),
    io:format(S, "\tnull.~n~n", []),

    [gen_fun_special(RecordName, FieldNames, FieldTypes, Row, S, get_id(KeyIndexes, Row)) || Row <- Rows],
    gen_fun_special_end(RecordName, S),

    file:close(S).

gen_erl_note(RecordName, ModuleName, S) ->
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "%%% @Module: ~s~n", [ModuleName]),
    io:format(S, <<"%%% @Description: 自动生成~n"/utf8>>, []),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "-module(~s).~n", [ModuleName]),
    io:format(S, "-export([get/1, get_ids/0~s]).~n", [gen_extra_export(RecordName)]),
    io:format(S, "-include(\"common.hrl\").~n", []),
    io:format(S, "-include(\"gameConfig.hrl\").~n", []),
    io:format(S, "-include(\"record_~s.hrl\").~n~n", [ModuleName]).

gen_extra_export(_) -> "".

gen_fun_get_ids(Rows, S, KeyIndexes) ->
    io:format(S, "get_ids() ->~n", []),
    io:format(S, "\t[", []),
    gen_ids(Rows, S, KeyIndexes),
    io:format(S, "].~n~n", []).

gen_fun_get(RecordName, FieldNames, FieldTypes, Row, S, KeyIndexes) ->
    io:format(S, "get(~p) ->~n", [get_id(KeyIndexes, Row)]),
    io:format(S, "\t#~s{~n", [RecordName]),
    gen_fun_get_fields(FieldNames, FieldTypes, Row, S),
    io:format(S, "\t};~n~n", []).

gen_ids(Rows, S, KeyIndexes) ->
    case Rows of
        [] ->
            void;
        _ ->
            [Row | RestRows] = Rows,
            io:format(S, "~p", [get_id(KeyIndexes, Row)]),
            if RestRows /= [] ->
                    io:format(S, ",", []),
                    gen_ids(RestRows, S, KeyIndexes);
                true ->
                    void
            end
    end.

gen_fun_get_fields([Name0|RestNames], [Type|RestTypes], [Value0|RestValues], S) ->
    Name = string:to_lower(binary_to_list(Name0)),
    Value = case Value0 of
        <<"">> ->
            case Type of
                <<"char">> -> <<"">>;
                <<"varchar">> -> <<"">>;
                <<"tinytext">> -> <<"[]">>;
                <<"text">> -> <<"[]">>;
                _-> <<"0">>
            end;
        _ ->
            case Type of
                <<"longblob">> -> <<"">>;
                _ -> Value0
            end
    end,
    case Type of
        <<"char">> ->
            io:format(S, "\t\t~s = <<\"~s\"/utf8>>", [Name, Value]);
        <<"varchar">> ->
            io:format(S, "\t\t~s = <<\"~s\"/utf8>>", [Name, Value]);
        <<"tinytext">> ->
            io:format(S, "\t\t~s = ~s", [Name, Value]);
        <<"text">> ->
            io:format(S, "\t\t~s = ~s", [Name, Value]);
        _ ->
            io:format(S, "\t\t~s = ~p", [Name, Value])
    end,
    case RestNames of
        [] ->
            io:format(S, "~n", []);
        _ ->
            io:format(S, ",~n", []),
            gen_fun_get_fields(RestNames, RestTypes, RestValues, S)
    end.

get_id([], [First | _]) ->
    First;
get_id([Idx], Row) ->
    lists:nth(Idx, Row);
get_id(Idxes, Row) ->
    list_to_tuple([lists:nth(I, Row) || I <- Idxes]).

gen_fun_special(_, _, _, _, _, _) -> ok.

gen_fun_special_end(_, _) -> ok.

