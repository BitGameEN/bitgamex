%%%--------------------------------------------------------
%%% @Module  : data_autogen_log
%%% @Description: 连接log库获取日志表结构，自动生成数据访问代码
%%%--------------------------------------------------------
-module(data_autogen_log).
-export([run/0]).
-define(DB, bg_mysql_log).
-define(DB_HOST, "localhost").
-define(DB_PORT, 3306).
-define(DB_USER, "bitgame").
-define(DB_PASS, "bitgame123").
-define(DB_NAME, "bitgame_log").
-define(DB_ENCODE, utf8).

init_db() ->
    mysql:start_link(?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, fun(_, _, _, _) -> ok end, ?DB_ENCODE),
    mysql:connect(?DB, ?DB_HOST, ?DB_PORT, ?DB_USER, ?DB_PASS, ?DB_NAME, ?DB_ENCODE, true),
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
    init_db(),
    gen("gold"),
    gen("player_login"),
    ok.

gen(Table0) ->
    Table = list_to_binary(Table0),
    SqlNTDC = io_lib:format(<<"SELECT column_name,data_type,column_default,column_comment from information_schema.columns WHERE information_schema.columns.TABLE_NAME = '~s' and information_schema.columns.TABLE_SCHEMA = '~s'">>, [Table, ?DB_NAME]),
    FieldInfos = db_get_all(SqlNTDC),
    FieldNames = [N || [N|_] <- FieldInfos],
    FieldTypes = [T || [_,T|_] <- FieldInfos],
    FieldDefaults = [D || [_,_,D|_] <- FieldInfos],
    FieldComments = [C || [_,_,_,C|_] <- FieldInfos],
    SqlK = io_lib:format(<<"SELECT index_name,column_name from information_schema.statistics WHERE information_schema.statistics.TABLE_NAME = '~s' and information_schema.statistics.TABLE_SCHEMA = '~s' order by index_name">>, [Table, ?DB_NAME]),
    FieldKeyInfos0 = db_get_all(SqlK),
    F = fun(F, [[IndexName, ColumnName] | RestFieldKeyInfos], Acc) ->
                case lists:keyfind(IndexName, 1, Acc) of
                    false ->
                        F(F, RestFieldKeyInfos, [{IndexName, [ColumnName]} | Acc]);
                    {_, L} ->
                        F(F, RestFieldKeyInfos, lists:keyreplace(IndexName, 1, Acc, {IndexName, [ColumnName | L]}))
                end;
            (_, [], Acc) ->
                [{Index, lists:sort(fun compare_column/2, Columns)} || {Index, Columns} <- Acc]
        end,
    FieldKeyInfos = F(F, FieldKeyInfos0, []),
    RecordName = list_to_binary(io_lib:format(<<"log_~s">>, [Table])),
    gen_record(io_lib:format(<<"record_log_~s.hrl">>, [Table]), RecordName, FieldNames, FieldTypes, FieldDefaults, FieldComments),
    gen_erl(Table, io_lib:format(<<"log_~s.erl">>, [Table]), RecordName, FieldNames, FieldTypes, FieldKeyInfos, RecordName).

compare_column(A, B) ->
    A < B.

gen_record(HrlName, RecordName, FieldNames, FieldTypes, FieldDefaults, FieldComments) ->
    {ok, S} = file:open(HrlName, write),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "%%% @Module: ~s~n", [RecordName]),
    io:format(S, <<"%%% @Description: 自动生成~n"/utf8>>, []),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "~n", []),
    io:format(S, "-record(~s, {~n", [RecordName]),
    io:format(S, "\t\tkey_id,~n", []),

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
                <<"tinytext">> -> <<"">>;
                <<"text">> -> <<"">>;
                _-> <<"0">>
            end;
        _ ->
            Default0
    end,
    case Type of
        <<"char">> ->
            io:format(S, "\t\t~s = <<\"~s\">>", [Name, Default]);
        <<"varchar">> ->
            io:format(S, "\t\t~s = <<\"~s\">>", [Name, Default]);
        <<"tinytext">> ->
            io:format(S, "\t\t~s = <<\"~s\">>", [Name, Default]);
        <<"text">> ->
            io:format(S, "\t\t~s = <<\"~s\">>", [Name, Default]);
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

gen_erl(Table, ErlName, ModuleName, FieldNames, FieldTypes, FieldKeyInfos, RecordName) ->
    FieldNames2 = get_names_upper_1st(FieldNames),
    PriNames = get_pri_names(FieldKeyInfos),
    PriNames2 = get_names_upper_1st(PriNames),
    PriId = gen_id(PriNames2),
    PirId2 = gen_id_no_bracket(PriNames2, ", "),

    {ok, S} = file:open(ErlName, write),
    gen_erl_note(ModuleName, S),

    io:format(S, "get_one(~s) ->~n", [PriId]),
    io:format(S, "\tcase db_esql:get_row(?DB_LOG, <<\"select ~s from ~s where ~s\">>, [~s]) of~n",
        [gen_id_no_bracket(FieldNames, ","), Table, gen_id_sql(PriNames), PirId2]),
    io:format(S, "\t\t[] -> [];~n", []),
    io:format(S, "\t\tRow -> build_record_from_row(Row)~n", []),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "set_one(R0) when is_record(R0, ~s) ->~n", [RecordName]),
    io:format(S, "\tcase R0#~s.key_id =:= undefined of~n", [RecordName]),
    io:format(S, "\t\tfalse ->~n", []),
    io:format(S, "\t\t\tsyncdb(R0),~n", []),
    io:format(S, "\t\t\tR0#~s.key_id;~n", [RecordName]),
    io:format(S, "\t\ttrue ->~n", []),
    io:format(S, "\t\t\t#~s{~n", [RecordName]),
    gen_erl_fields(FieldNames, FieldNames2, S),
    io:format(S, "\t\t\t} = R0,~n", []),
    io:format(S, "\t\t\t{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_LOG, io_lib:format(<<\"insert into ~s(~s) values(~s); select last_insert_id()\">>,~n\t\t\t\t[~s])),~n",
        [Table, gen_id_no_bracket(FieldNames, ","), gen_fields_placeholder2(FieldTypes), gen_fields_sql(FieldNames2, FieldTypes)]),
    io:format(S, "\t\t\tR = R0#~s{key_id = Insert_id, ~s = Insert_id},~n", [RecordName, gen_pri_id(PriNames)]),
    io:format(S, "\t\t\tR#~s.key_id~n", [RecordName]),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "syncdb(R) when is_record(R, ~s) ->~n", [RecordName]),
    io:format(S, "\t#~s{~n", [RecordName]),
    gen_erl_fields(FieldNames, FieldNames2, S, <<"\t\t">>),
    io:format(S, "\t} = R,~n", []),
    io:format(S, "\tdb_esql:execute(?DB_LOG, <<\"replace into ~s(~s) values(~s)\">>,~n\t\t[~s]).~n~n",
        [Table, gen_id_no_bracket(FieldNames, ","), gen_fields_placeholder(length(FieldNames)), gen_fields_sql(FieldNames2, FieldTypes)]),

    io:format(S, "build_record_from_row([", []),
    [FieldName | RestFieldNames] = FieldNames2,
    io:format(S, "~s", [FieldName]),
    GenFieldsStrF = fun(FieldName_, S_) -> io:format(S_, ", ~s", [FieldName_]) end,
    [GenFieldsStrF(FName, S) || FName <- RestFieldNames],
    io:format(S, "]) ->~n", []),
    io:format(S, "\t#~s{~n", [RecordName]),
    io:format(S, "\t\tkey_id = ~s,~n", [PriId]),
    gen_erl_fields_string2term(FieldNames, FieldNames2, FieldTypes, S),
    io:format(S, "\t}.~n~n", []),

    file:close(S).

gen_erl_note(ModuleName, S) ->
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "%%% @Module: ~s~n", [ModuleName]),
    io:format(S, <<"%%% @Description: 自动生成~n"/utf8>>, []),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "-module(~s).~n", [ModuleName]),
    io:format(S, "-export([get_one/1, set_one/1, build_record_from_row/1]).~n", []),
    io:format(S, "-include(\"common.hrl\").~n", []),
    io:format(S, "-include(\"record_~s.hrl\").~n~n", [ModuleName]).

gen_erl_fields_string2term([Name|RestNames], [Value|RestValues], [Type|RestTypes], S) ->
    %case Type of
    %    <<"tinytext">> ->
    %        io:format(S, "\t\t~s = ?B2T(~s)", [Name, Value]);
    %    <<"text">> ->
    %        io:format(S, "\t\t~s = ?B2T(~s)", [Name, Value]);
    %    _->
    %        io:format(S, "\t\t~s = ~s", [Name, Value])
    %end,
    io:format(S, "\t\t~s = ~s", [Name, Value]),
	case RestNames of
        [] ->
            io:format(S, "~n", []);
        _ ->
            io:format(S, ",~n", []),
			gen_erl_fields_string2term(RestNames, RestValues, RestTypes, S)
	end.

gen_erl_fields(Names, Values, S) ->
    gen_erl_fields(Names, Values, S, <<"\t\t\t\t">>).

gen_erl_fields([Name|RestNames], [Value|RestValues], S, Indent) ->
    io:format(S, <<Indent/binary, "~s = ~s">>, [Name, Value]),
    case RestNames of
        [] ->
            io:format(S, "~n", []);
        _ ->
            io:format(S, ",~n", []),
            gen_erl_fields(RestNames, RestValues, S, Indent)
    end.

get_pri_names(FieldKeyInfos) ->
    {_, PriNames} = lists:keyfind(<<"PRIMARY">>, 1, FieldKeyInfos),
    PriNames.

% [F1, F2]
get_names_upper_1st(FieldNames) ->
    [util:upper_1st_char(N) || N <- FieldNames].

% <<"{x, y}">>
gen_id([FieldName]) ->
    FieldName;
gen_id(FieldNames) ->
    L = util:implode(", ", FieldNames),
    B = list_to_binary(L),
    <<"{", B/binary, "}">>.

% <<"x, y">>
gen_id_no_bracket([FieldName], _) ->
    FieldName;
gen_id_no_bracket(FieldNames, Imploder) ->
    L = util:implode(Imploder, FieldNames),
    list_to_binary(L).

gen_pri_id([FieldName]) ->
    FieldName.

gen_id_sql([FieldName]) ->
    <<FieldName/binary, "=?">>;
gen_id_sql(FieldNames) ->
    L = [<<FieldName/binary, "=?">> || FieldName <- FieldNames],
    L2 = util:implode(" and ", L),
    list_to_binary(L2).

gen_fields_sql(FieldNames, FieldTypes) ->
    NameF = fun({Name, Type}) ->
            %case Type =:= <<"tinytext">> orelse Type =:= <<"text">> of
            %    true -> <<"?T2B(", Name/binary, ")">>;
            %    false -> Name
            %end
            case Type =:= <<"tinytext">> orelse Type =:= <<"text">> orelse Type =:= <<"varchar">> orelse Type =:= <<"char">> of
                true ->
                    case util:contains(Name, [<<"name">>, <<"msg">>, <<"description">>, <<"notice">>, <<"title">>]) of
                        true -> <<"util:esc(", Name/binary, ")">>;
                        false -> Name
                    end;
                false -> Name
            end
        end,
    Fields = lists:zip(FieldNames, FieldTypes),
    FieldNames2 = [NameF(Field) || Field <- Fields],
    L = util:implode(", ", FieldNames2),
    list_to_binary(L).

gen_fields_placeholder(Len) ->
    L = lists:duplicate(Len, "?"),
    L2 = util:implode(",", L),
    list_to_binary(L2).

gen_fields_placeholder2(FieldTypes) ->
    F = fun(Type) ->

            case Type =:= <<"tinytext">> orelse Type =:= <<"text">> orelse Type =:= <<"varchar">>  orelse Type =:= <<"char">> of
                true -> "'~s'";
                false -> "~p"

            end
        end,
    L = [F(T) || T <- FieldTypes],
    L2 = util:implode(",", L),
    list_to_binary(L2).
