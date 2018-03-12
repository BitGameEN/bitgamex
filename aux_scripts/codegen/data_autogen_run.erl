%%%--------------------------------------------------------
%%% @Module  : data_autogen_run
%%% @Description: 连接run库获取运行时表结构，自动生成数据访问代码
%%%--------------------------------------------------------
-module(data_autogen_run).
-export([run/0]).
-define(DB, bg_mysql_run).
-define(DB_HOST, "localhost").
-define(DB_PORT, 3306).
-define(DB_USER, "bitgame").
-define(DB_PASS, "bitgame123").
-define(DB_NAME, "bitgame_run").
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
    gen("role", false, 2),
    ok.

gen(Table) ->
    gen(Table, false, 1).

gen(Table, UseMnesia) ->
    gen(Table, UseMnesia, 1).

% NeedGid: 0 - 不需要gid，id通过数据库自增键生成
%          1 - 需要gid，id_gen生成
%          2 - 已经是gid，不需要生成
gen(Table0, UseMnesia, NeedGid) ->
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
    RecordName = list_to_binary(io_lib:format(<<"run_~s">>, [Table])),
    gen_record(io_lib:format(<<"record_run_~s.hrl">>, [Table]), RecordName, FieldNames, FieldTypes, FieldDefaults, FieldComments),
    gen_erl(Table, io_lib:format(<<"run_~s.erl">>, [Table]), RecordName, FieldNames, FieldTypes, FieldDefaults, FieldComments, FieldKeyInfos, RecordName, UseMnesia, NeedGid).

compare_column(A, B) ->
    A < B.

gen_record(HrlName, RecordName, FieldNames, FieldTypes, FieldDefaults, FieldComments) ->
    {ok, S} = file:open(HrlName, write),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "%%% @Module: ~s~n", [RecordName]),
    io:format(S, <<"%%% @Description: 自动生成~n"/utf8>>, []),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "~n", []),
    gen_record_extra(S, RecordName),
    io:format(S, "-record(~s, {~n", [RecordName]),
    io:format(S, "\t\tkey_id,~n", []),

    gen_record_fields(FieldNames, FieldTypes, FieldDefaults, FieldComments, S),
 
    io:format(S, "}).~n", []),
    file:close(S).

gen_record_extra(S, _) ->
    ok.

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
    case Comment of
        <<"erlang", _/binary>> ->
            case Default of
                <<"">> ->
                    io:format(S, "\t\t~s", [Name]);
                _ ->
                    io:format(S, "\t\t~s = ~s", [Name, Default])
            end;
        _ ->
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
            end
    end,
    case RestNames of
        [] ->
            io:format(S, " % ~s~n", [Comment]);
        _ ->
            io:format(S, ", % ~s~n", [Comment]),
            gen_record_fields(RestNames, RestTypes, RestDefaults, RestComments, S)
    end.

gen_erl(Table, ErlName, ModuleName, FieldNames, FieldTypes, FieldDefaults, FieldComments, FieldKeyInfos, RecordName, UseMnesia, NeedGid) ->
    FieldNames2 = get_names_upper_1st(FieldNames),
    PriNames = get_pri_names(FieldKeyInfos),
    PriNames2 = get_names_upper_1st(PriNames),
    PriNames3 = get_names_underscore(PriNames2),
    PriId = gen_id(PriNames2),
    PirId2 = gen_id_no_bracket(PriNames2, ", "),
    PirId3 = gen_id_no_bracket(PriNames3, ", "),
    OtherKeys = get_other_names(FieldKeyInfos),
    OtherKeysLower = [get_names_lower(UnionNames) || UnionNames <- OtherKeys],
    OtherKeys2 = lists:zip(OtherKeys, OtherKeysLower),
    DVs = lists:zip(FieldNames, FieldDefaults),
    Types = lists:zip(FieldNames, FieldTypes),

    {ok, S} = file:open(ErlName, write),
    gen_erl_note(Table, ModuleName, OtherKeysLower, S, UseMnesia),

    case UseMnesia of
        false ->
            io:format(S, "init() ->~n", []),
            io:format(S, "\tets:new(~s, [{keypos, #~s.key_id}, named_table, public, set, ?ETSRC, ?ETSWC]),~n", [ModuleName, ModuleName]),
            GenOtherKeyEtsNewF = fun(OtherKeyLower) ->
                    OtherIdAnd = gen_id_no_bracket(OtherKeyLower, "_and_"),
                    io:format(S, "\tets:new(~s_keymap_~s_to_gid, [named_table, public, bag, ?ETSRC, ?ETSWC]),~n", [ModuleName, OtherIdAnd])
                end,
            [GenOtherKeyEtsNewF(KL) || KL <- OtherKeysLower],
            io:format(S, "\tok.~n~n", []);
        true ->
            io:format(S, "table_def(Nodes) ->~n", []),
            io:format(S, "\t[{~s, [{attributes, record_info(fields, ~s)}, {ram_copies, Nodes}, {storage_properties, [{ets, [?ETSRC, ?ETSWC]}]}]}", [ModuleName, ModuleName]),
            GenOtherKeyTabDefF = fun(OtherKeyLower) ->
                    OtherIdAnd = gen_id_no_bracket(OtherKeyLower, "_and_"),
                    io:format(S, ",~n\t {~s_keymap_~s_to_gid, [{type, bag}, {attributes, [k, v]}, {ram_copies, Nodes}, {storage_properties, [{ets, [?ETSRC, ?ETSWC]}]}]}", [ModuleName, OtherIdAnd])
                end,
            [GenOtherKeyTabDefF(KL) || KL <- OtherKeysLower],
            io:format(S, "].~n~n", [])
    end,

    io:format(S, "load_one(~s = Id) ->~n", [PriId]),
    io:format(S, "\tget_one(Id, direct_from_db).~n~n", []),

    io:format(S, "get_one_locally(~s = Id) ->~n", [PriId]),
    io:format(S, "\tget_one(Id, just_from_ets).~n~n", []),

    io:format(S, "get_one(~s = Id) ->~n", [PriId]),
    io:format(S, "\tcase run_data:in_trans() of~n", []),
    io:format(S, "\t\ttrue ->~n", []),
    io:format(S, "\t\t\tcase run_data:trans_get(~s, Id) of~n", [RecordName]),
    io:format(S, "\t\t\t\t[] ->~n", []),
    io:format(S, "\t\t\t\t\tget_one(Id, anyway);~n", []),
    io:format(S, "\t\t\t\ttrans_deleted -> [];~n", []),
    io:format(S, "\t\t\t\tR -> run_data_ver:upgrade_if_need(R)~n", []),
    io:format(S, "\t\t\tend;~n", []),
    io:format(S, "\t\tfalse ->~n", []),
    io:format(S, "\t\t\tget_one(Id, anyway)~n", []),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "get_one(~s = Id, just_from_ets) ->~n", [PriId]),
    io:format(S, "\trun_data:lookup_one(~s, Id, ~p);~n", [ModuleName, UseMnesia]),

    io:format(S, "get_one(~s = Id, direct_from_db) ->~n", [PriId]),
    io:format(S, "\tcase cache:get(cache_key(Id)) of~n", []),
    io:format(S, "\t\t{true, _Cas, Val0} ->~n", []),
    io:format(S, "\t\t\tVal = run_data_ver:upgrade_if_need(Val0),~n", []),
    io:format(S, "\t\t\tinsert_ets(Val),~n", []),
    io:format(S, "\t\t\tVal;~n", []),
    io:format(S, "\t\t_ ->~n", []),
    io:format(S, "\t\t\tcase db_esql:get_row(?DB_RUN, <<\"select ~s from ~s where ~s\">>, [~s]) of~n",
        [gen_id_no_bracket(FieldNames, ","), Table, gen_id_sql(PriNames), PirId2]),
    io:format(S, "\t\t\t\t[] -> [];~n", []),
    io:format(S, "\t\t\t\tRow ->~n", []),
    io:format(S, "\t\t\t\t\tinsert_ets_from_db(Row),~n", []),
    io:format(S, "\t\t\t\t\trun_data:lookup_one(~s, Id, ~p)~n", [ModuleName, UseMnesia]),
    io:format(S, "\t\t\tend~n", []),
    io:format(S, "\tend;~n", []),

    io:format(S, "get_one(~s = Id, anyway) ->~n", [PriId]),
    io:format(S, "\tcase run_data:lookup_one(~s, Id, ~p) of~n", [ModuleName, UseMnesia]),
    io:format(S, "\t\t[] ->~n", []),
    io:format(S, "\t\t\tcase cache:get(cache_key(Id)) of~n", []),
    io:format(S, "\t\t\t\t{true, _Cas, Val0} ->~n", []),
    io:format(S, "\t\t\t\t\tVal = run_data_ver:upgrade_if_need(Val0),~n", []),
    io:format(S, "\t\t\t\t\tVal;~n", []),
    io:format(S, "\t\t\t\t_ ->~n", []),
    io:format(S, "\t\t\t\t\tcase db_esql:get_row(?DB_RUN, <<\"select ~s from ~s where ~s\">>, [~s]) of~n",
        [gen_id_no_bracket(FieldNames, ","), Table, gen_id_sql(PriNames), PirId2]),
    io:format(S, "\t\t\t\t\t\t[] -> [];~n", []),
    io:format(S, "\t\t\t\t\t\tRow -> % do not insert ets~n", []),
    io:format(S, "\t\t\t\t\t\t\tR = build_record_from_row(Row),~n", []),
    io:format(S, "\t\t\t\t\t\t\tcache:set(cache_key(R#~s.key_id), R),~n", [RecordName]),
    io:format(S, "\t\t\t\t\t\t\tR~n", []),
    io:format(S, "\t\t\t\t\tend~n", []),
    io:format(S, "\t\t\tend;~n", []),
    io:format(S, "\t\tR -> R~n", []),
    io:format(S, "\tend;~n", []),

    io:format(S, "get_one(_Id, _GetPolicy) -> throw(get_policy_not_supported).~n~n", []),

    GenOtherKeyGetF = fun(OtherKey, OtherKeyLower) ->
            OtherKey2 = get_names_upper_1st(OtherKeyLower),
            OtherId = gen_id(OtherKey2),
            OtherId2 = gen_id_no_bracket(OtherKey2, ", "),
            OtherIdAnd = gen_id_no_bracket(OtherKeyLower, "_and_"),
            DefalutValue = build_default_value(Types, DVs, OtherKey),
            io:format(S, "get_~s_gids_by_~s(~s = Id) ->~n", [Table, OtherIdAnd, OtherId]),
            io:format(S, "\tcase util:lookup_one(~s_keymap_~s_to_gid, {indexes_loaded, Id}, ~p) of~n", [ModuleName, OtherIdAnd, UseMnesia]),
            io:format(S, "\t\t[] ->~n", []),
            case OtherKey =:= [<<"player_id">>] of
                true ->
                    io:format(S, "\t\t\tcase db_esql:get_all(?DB_RUN, <<\"select ~s from ~s where ~s\">>, [~s]) of~n",
                        [gen_id_no_bracket(FieldNames, ","), Table, gen_id_sql(OtherKey), OtherId2]);
                false ->
                    io:format(S, "\t\t\tcase db_esql:get_all(?DB_RUN, <<\"select ~s from ~s where ~s\">>, [~s]) of~n",
                        [gen_id_no_bracket(PriNames, ", "), Table, gen_id_sql(OtherKey), OtherId2])
            end,
            io:format(S, "\t\t\t\t[] ->~n", []),
            io:format(S, "\t\t\t\t\tcase Id =:= ~s of~n", [DefalutValue]),
            io:format(S, "\t\t\t\t\t\ttrue -> void;~n", []),
            io:format(S, "\t\t\t\t\t\tfalse -> ", []),
            case UseMnesia of
                false ->
                    io:format(S, "ets:insert(~s_keymap_~s_to_gid, {{indexes_loaded, Id}, ok})~n",
                        [ModuleName, OtherIdAnd]);
                true ->
                    io:format(S, "mnesia:dirty_write({~s_keymap_~s_to_gid, {indexes_loaded, Id}, ok})~n",
                        [ModuleName, OtherIdAnd])
            end,
            io:format(S, "\t\t\t\t\tend,~n", []),
            io:format(S, "\t\t\t\t\t[];~n", []),
            io:format(S, "\t\t\t\tRows ->~n", []),
            InnerF = fun(Indent) ->
                    case UseMnesia of
                        false ->
                            io:format(S, "~sets:insert(~s_keymap_~s_to_gid, {{indexes_loaded, Id}, ok}),~n",
                                [Indent, ModuleName, OtherIdAnd]),
                            io:format(S, "~s[ets:insert(~s_keymap_~s_to_gid, {Id, ~s}) || [~s | _] <- Rows],~n",
                                [Indent, ModuleName, OtherIdAnd, PirId3, PirId3]);
                        true ->
                            io:format(S, "~smnesia:dirty_write({~s_keymap_~s_to_gid, {indexes_loaded, Id}, ok}),~n",
                                [Indent, ModuleName, OtherIdAnd]),
                            io:format(S, "~s[mnesia:dirty_write({~s_keymap_~s_to_gid, Id, ~s}) || [~s | _] <- Rows],~n",
                                [Indent, ModuleName, OtherIdAnd, PirId3, PirId3])
                    end,
                    case OtherKey =:= [<<"player_id">>] of
                        true ->
                            io:format(S, "~s[insert_ets_from_db(Row) || Row <- Rows],~n", [Indent]);
                        false ->
                            void
                    end,
                    io:format(S, "~sL0 = util:lookup_all(~s_keymap_~s_to_gid, Id, ~p),~n", [Indent, ModuleName, OtherIdAnd, UseMnesia]),
                    case UseMnesia of
                        false ->
                            io:format(S, "~s[R0 || {_, R0} <- L0]~n", [Indent]);
                        true ->
                            io:format(S, "~s[R0 || {_, _, R0} <- L0]~n", [Indent])
                    end
                end,
            io:format(S, "\t\t\t\t\tcase Id =:= ~s of~n", [DefalutValue]),
            io:format(S, "\t\t\t\t\t\ttrue -> [~s || [~s | _] <- Rows];~n", [PirId3, PirId3]),
            io:format(S, "\t\t\t\t\t\tfalse ->~n", []),
            InnerF("\t\t\t\t\t\t\t"),
            io:format(S, "\t\t\t\t\tend~n", []),
            io:format(S, "\t\t\tend;~n", []),
            io:format(S, "\t\t_ ->~n", []),
            io:format(S, "\t\t\tL = util:lookup_all(~s_keymap_~s_to_gid, Id, ~p),~n", [ModuleName, OtherIdAnd, UseMnesia]),
            case UseMnesia of
                false ->
                    io:format(S, "\t\t\t[R || {_, R} <- L]~n", []);
                true ->
                    io:format(S, "\t\t\t[R || {_, _, R} <- L]~n", [])
            end,
            io:format(S, "\tend.~n~n", []),

            % 生成try_load_indexes函数
            io:format(S, "try_load_indexes_~s_gids_by_~s(~s = Id) ->~n", [Table, OtherIdAnd, OtherId]),
            io:format(S, "\tcase util:lookup_one(~s_keymap_~s_to_gid, {indexes_loaded, Id}, ~p) of~n", [ModuleName, OtherIdAnd, UseMnesia]),
            io:format(S, "\t\t[] ->~n", []),
            io:format(S, "\t\t\tget_~s_gids_by_~s(Id),~n", [Table, OtherIdAnd]),
            io:format(S, "\t\t\terlang:garbage_collect(self());~n", []),
            io:format(S, "\t\t_ -> void~n", []),
            io:format(S, "\tend.~n~n", [])
        end,
    [GenOtherKeyGetF(K, KL) || {K, KL} <- OtherKeys2],

    io:format(S, "set_field(~s = Id, Field, Val) ->~n", [PriId]),
    io:format(S, "\tFields = record_info(fields, ~s),~n", [RecordName]),
    io:format(S, "\tL = lists:zip(Fields, lists:seq(1, length(Fields))),~n", []),
    io:format(S, "\t{_, N} = lists:keyfind(Field, 1, L),~n", []),
    io:format(S, "\tR0 = get_one(Id),~n", []),
    io:format(S, "\tR = setelement(N+1, R0, Val),~n", []),
    io:format(S, "\tset_one(R),~n", []),
    io:format(S, "\tR.~n~n", []),

    io:format(S, "set_one(R0) when is_record(R0, ~s) ->~n", [RecordName]),
    io:format(S, "\tcase R0#~s.key_id =:= undefined of~n", [RecordName]),
    io:format(S, "\t\tfalse ->~n", []),
    io:format(S, "\t\t\tcase run_data:in_trans() of~n", []),
    io:format(S, "\t\t\t\ttrue ->~n", []),
    io:format(S, "\t\t\t\t\trun_data:trans_set(~s, R0#~s.key_id, R0,~n", [RecordName, RecordName]),
    %io:format(S, "\t\t\t\t\t\tfun() -> ~s:set_one(R0) end,~n", [RecordName]),
    io:format(S, "\t\t\t\t\t\tvoid,~n", []),
    io:format(S, "\t\t\t\t\t\tvoid);~n", []),
    io:format(S, "\t\t\t\tfalse ->~n", []),
    io:format(S, "\t\t\t\t\tcase util:lookup_one(~s, R0#~s.key_id, ~p) of~n", [ModuleName, RecordName, UseMnesia]),
    io:format(S, "\t\t\t\t\t\t[] ->~n", []),
    io:format(S, "\t\t\t\t\t\t\tOld = get_one(R0#~s.key_id, anyway),~n", [RecordName]),
    io:format(S, "\t\t\t\t\t\t\tupdate_keymap_if_need(Old, R0);~n", []),
    io:format(S, "\t\t\t\t\t\tOld ->~n", []),
    case UseMnesia of
        false ->
            io:format(S, "\t\t\t\t\t\t\tets:insert(~s, R0),~n", [ModuleName]);
        true ->
            io:format(S, "\t\t\t\t\t\t\tmnesia:dirty_write(~s, R0),~n", [ModuleName])
    end,
    io:format(S, "\t\t\t\t\t\t\tupdate_keymap_if_need(Old, R0)~n", []),
    io:format(S, "\t\t\t\t\tend,~n", []),
    io:format(S, "\t\t\t\t\tsyncdb(R0),~n", []),
    %io:format(S, "\t\t\t\t\tcache:del(cache_key(R0#~s.key_id))~n", [RecordName]),
    io:format(S, "\t\t\t\t\tcache:set(cache_key(R0#~s.key_id), R0)~n", [RecordName]),
    io:format(S, "\t\t\tend,~n", []),
    io:format(S, "\t\t\tR0#~s.key_id;~n", [RecordName]),
    io:format(S, "\t\ttrue ->~n", []),
    io:format(S, "\t\t\t#~s{~n", [RecordName]),
    case NeedGid of
        1 ->
            [PriFieldName | FieldNames_] = FieldNames,
            [PriFieldName2 | FieldNames2_] = FieldNames2,
            io:format(S, "\t\t\t\t~s = ~s_0,~n", [PriFieldName, PriFieldName2]),
            gen_erl_fields(FieldNames_, FieldNames2_, S),
            io:format(S, "\t\t\t} = R0,~n", []),
            io:format(S, "\t\t\t~s = id_gen:gen_id(~s),~n", [PriFieldName2, RecordName]),
            io:format(S, "\t\t\tR = R0#~s{key_id = ~s, ~s = ~s, ver = run_data_ver:newest_ver(~s)},~n", [RecordName, PriFieldName2, gen_pri_id(PriNames), PriFieldName2, RecordName]);
        2 ->
            [PriFieldName2 | _] = FieldNames2,
            gen_erl_fields(FieldNames, FieldNames2, S),
            io:format(S, "\t\t\t} = R0,~n", []),
            io:format(S, "\t\t\tR = R0#~s{key_id = ~s, ~s = ~s, ver = run_data_ver:newest_ver(~s)},~n", [RecordName, PriFieldName2, gen_pri_id(PriNames), PriFieldName2, RecordName]);
        0 ->
            gen_erl_fields(FieldNames, FieldNames2, S),
            io:format(S, "\t\t\t} = R0,~n", []),
            io:format(S, "\t\t\t{ok, [[Insert_id|_]]} = db_esql:multi_execute(?DB_RUN, io_lib:format(<<\"insert into ~s(~s) values(~s); select last_insert_id()\">>,~n\t\t\t\t[~s])),~n",
                [Table, gen_id_no_bracket(FieldNames, ","), gen_fields_placeholder2(FieldTypes), gen_fields_sql(FieldNames2, FieldTypes, FieldComments)]),
            io:format(S, "\t\t\tR = R0#~s{key_id = Insert_id, ~s = Insert_id, ver = run_data_ver:newest_ver(~s)},~n", [RecordName, gen_pri_id(PriNames), RecordName])
    end,
    io:format(S, "\t\t\tF = fun() ->~n", []),
    case NeedGid of
        1 ->
            io:format(S, "\t\t\t\t\trun_data:db_write(add, R, fun() -> 1 = db_esql:execute(?DB_RUN, io_lib:format(<<\"insert into ~s(~s) values(~s)\">>,~n\t\t\t\t\t\t[~s])) end),~n",
                [Table, gen_id_no_bracket(FieldNames, ","), gen_fields_placeholder2(FieldTypes), gen_fields_sql(FieldNames2, FieldTypes, FieldComments)]);
        2 ->
            io:format(S, "\t\t\t\t\trun_data:db_write(add, R, fun() -> 1 = db_esql:execute(?DB_RUN, io_lib:format(<<\"insert into ~s(~s) values(~s)\">>,~n\t\t\t\t\t\t[~s])) end),~n",
                [Table, gen_id_no_bracket(FieldNames, ","), gen_fields_placeholder2(FieldTypes), gen_fields_sql(FieldNames2, FieldTypes, FieldComments)]);
        0 -> void
    end,
    io:format(S, "\t\t\t\t\tcache:set(cache_key(R#~s.key_id), R),~n", [RecordName]),
    io:format(S, "\t\t\t\t\tinsert_ets(R)~n", []),
    io:format(S, "\t\t\t\tend,~n", []),
    io:format(S, "\t\t\tcase run_data:in_trans() of~n", []),
    io:format(S, "\t\t\t\ttrue ->~n", []),
    io:format(S, "\t\t\t\t\trun_data:trans_set(~s, R#~s.key_id, {trans_inserted, R}, F, fun() -> ~s:del_one(R, true) end);~n", [RecordName, RecordName, RecordName]),
    io:format(S, "\t\t\t\tfalse ->~n", []),
    io:format(S, "\t\t\t\t\tF()~n", []),
    io:format(S, "\t\t\tend,~n", []),
    io:format(S, "\t\t\tR#~s.key_id~n", [RecordName]),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "del_one(R) when is_record(R, ~s) ->~n", [RecordName]),
    io:format(S, "\tdel_one(R, false);~n", []),

    io:format(S, "del_one(~s = Id) ->~n", [PriId]),
    io:format(S, "\tdel_one(Id, false).~n~n", []),

    io:format(S, "del_one(R, DelDbAlso) when is_record(R, ~s) ->~n", [RecordName]),
    io:format(S, "\tcase run_data:in_trans() of~n", []),
    io:format(S, "\t\ttrue ->~n", []),
    io:format(S, "\t\t\trun_data:trans_del(~s, R#~s.key_id,~n", [RecordName, RecordName]),
    io:format(S, "\t\t\t\tfun() -> ~s:del_one(R, DelDbAlso) end,~n", [RecordName]),
    io:format(S, "\t\t\t\tvoid);~n", []),
    io:format(S, "\t\tfalse ->~n", []),
    io:format(S, "\t\t\t~s = R#~s.key_id,~n", [PirId2, RecordName]),
    io:format(S, "\t\t\tcase DelDbAlso of~n", []),
    io:format(S, "\t\t\t\ttrue ->~n", []),
    case OtherKeys2 of
        [] -> void;
        _ ->
            io:format(S, "\t\t\t\t\tR0 = case get_one(R#~s.key_id, anyway) of [] -> R; R0_ -> R0_ end,~n", [RecordName])
    end,
    GenKeyMapDelF = fun(OtherKeyLower) ->
            OtherIdAnd = gen_id_no_bracket(OtherKeyLower, "_and_"),
            OtherIdRecord = gen_id_record_format(OtherKeyLower, <<"R0">>, RecordName),
            case UseMnesia of
                false ->
                    io:format(S, "\t\t\t\t\tets:delete_object(~s_keymap_~s_to_gid, {~s, R#~s.key_id}),~n",
                        [ModuleName, OtherIdAnd, OtherIdRecord, RecordName]);
                true ->
                    io:format(S, "\t\t\t\t\tmnesia:dirty_delete_object({~s_keymap_~s_to_gid, ~s, R#~s.key_id}),~n",
                        [ModuleName, OtherIdAnd, OtherIdRecord, RecordName])
            end
        end,
    [GenKeyMapDelF(KL) || {_K, KL} <- OtherKeys2],
    io:format(S, "\t\t\t\t\trun_data:db_write(del, R, fun() -> db_esql:execute(?DB_RUN, <<\"delete from ~s where ~s\">>, [~s]) end),~n", [Table, gen_id_sql(PriNames), PirId2]),
    io:format(S, "\t\t\t\t\tcache:del(cache_key(R#~s.key_id));~n", [RecordName]),
    io:format(S, "\t\t\t\tfalse -> void~n", []),
    io:format(S, "\t\t\tend,~n", []),
    case UseMnesia of
        false ->
            io:format(S, "\t\t\tets:delete(~s, R#~s.key_id)~n", [ModuleName, RecordName]);
        true ->
            io:format(S, "\t\t\tmnesia:dirty_delete(~s, R#~s.key_id)~n", [ModuleName, RecordName])
    end,
    io:format(S, "\tend,~n", []),
    io:format(S, "\tok;~n", []),

    io:format(S, "del_one(~s = Id, DelDbAlso) ->~n", [PriId]),
    io:format(S, "\tcase util:lookup_one(~s, Id, ~p) of~n", [ModuleName, UseMnesia]),
    io:format(S, "\t\t[] ->~n", []),
    io:format(S, "\t\t\tcase DelDbAlso of~n", []),
    io:format(S, "\t\t\t\ttrue ->~n", []),
    io:format(S, "\t\t\t\t\tcase get_one(Id, anyway) of~n", []),
    io:format(S, "\t\t\t\t\t\t[] -> void;~n", []),
    io:format(S, "\t\t\t\t\t\tR -> del_one(R, DelDbAlso)~n", []),
    io:format(S, "\t\t\t\t\tend;~n", []),
    io:format(S, "\t\t\t\tfalse -> void~n", []),
    io:format(S, "\t\t\tend;~n", []),
    io:format(S, "\t\tR -> del_one(R, DelDbAlso)~n", []),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "reload_one(~s = Id) ->~n", [PriId]),
    io:format(S, "\tcache:del(cache_key(Id)),~n", []),
    io:format(S, "\tcase util:lookup_one(~s, Id, ~p) of~n", [ModuleName, UseMnesia]),
    io:format(S, "\t\t[] -> true;~n", []),
    io:format(S, "\t\tR ->~n", []),
    io:format(S, "\t\t\tcase db_esql:get_row(?DB_RUN, <<\"select ~s from ~s where ~s\">>, [~s]) of~n",
        [gen_id_no_bracket(FieldNames, ","), Table, gen_id_sql(PriNames), PirId2]),
    io:format(S, "\t\t\t\t[] -> false;~n", []),
    io:format(S, "\t\t\t\tRow ->~n", []),
    io:format(S, "\t\t\t\t\tinsert_ets_from_db(Row),~n", []),
    io:format(S, "\t\t\t\t\ttrue~n", []),
    io:format(S, "\t\t\tend~n", []),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "clean_all_cache() ->~n", []),
    io:format(S, "\tclean_all_cache(0),~n", []),
    io:format(S, "\tok.~n~n", []),

    io:format(S, "clean_all_cache(N) ->~n", []),
    io:format(S, "\tcase db_esql:get_all(?DB_RUN, <<\"select ~s from ~s limit ?, 1000\">>, [N * 1000]) of~n", [gen_pri_id(PriNames), Table]),
    io:format(S, "\t\t[] -> ok;~n", []),
    io:format(S, "\t\tRows ->~n", []),
    io:format(S, "\t\t\tF = fun(Id) -> cache:del(cache_key(Id)) end,~n", []),
    io:format(S, "\t\t\t[F(Id) || [Id | _] <- Rows],~n", []),
    io:format(S, "\t\t\tclean_all_cache(N + 1)~n", []),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "syncdb(R) when is_record(R, ~s) ->~n", [RecordName]),
    io:format(S, "\t#~s{~n", [RecordName]),
    gen_erl_fields(FieldNames, FieldNames2, S, <<"\t\t">>),
    io:format(S, "\t} = R,~n", []),
    [_ | NonPriFieldNames] = FieldNames,
    [_ | NonPriFieldNames2] = FieldNames2,
    [_ | NonPriFieldTypes] = FieldTypes,
    [_ | NonPriFieldComments] = FieldComments,
    io:format(S, "\trun_data:db_write(upd, R, fun() -> db_esql:execute(?DB_RUN, <<\"insert into ~s(~s) values(~s) on duplicate key update \"~n\t\t\"~s\">>,~n\t\t[~s,~s]) end);~n",
        [Table, gen_id_no_bracket(FieldNames, ","), gen_fields_placeholder(length(FieldNames)), gen_update_fields_sql(NonPriFieldNames),
         gen_fields_sql(FieldNames2, FieldTypes, FieldComments), gen_fields_sql(NonPriFieldNames2, NonPriFieldTypes, NonPriFieldComments)]),

    io:format(S, "syncdb(Gid) ->~n", []),
    io:format(S, "\tcase get_one_locally(Gid) of~n", []),
    io:format(S, "\t\t[] -> void;~n", []),
    io:format(S, "\t\tR -> syncdb(R)~n", []),
    io:format(S, "\tend.~n~n", []),

    io:format(S, "insert_ets_from_db([~s|_] = Row) ->~n", [PirId2]),
    io:format(S, "\tR = build_record_from_row(Row),~n", []),
    io:format(S, "\tcache:set(cache_key(R#~s.key_id), R),~n", [RecordName]),
    io:format(S, "\tinsert_ets(R).~n~n", []),

    io:format(S, "insert_ets(R) ->~n", []),
    case UseMnesia of
        false ->
            io:format(S, "\tets:insert(~s, R),~n", [ModuleName]);
        true ->
            io:format(S, "\tmnesia:dirty_write(~s, R),~n", [ModuleName])
    end,
    GenKeyMapAddF = fun(OtherKey, OtherKeyLower, Indent0) ->
            OtherIdAnd = gen_id_no_bracket(OtherKeyLower, "_and_"),
            OtherIdRecord = gen_id_record_format(OtherKeyLower, <<"R">>, RecordName),
            io:format(S, "~scase ~s =:= ~s of~n", [Indent0, OtherIdRecord, build_default_value(Types, DVs, OtherKey)]),
            io:format(S, "~s\ttrue -> void;~n", [Indent0]),
            io:format(S, "~s\tfalse ->~n", [Indent0]),
            Indent = Indent0 ++ "\t\t",
            io:format(S, "~stry_load_indexes_~s_gids_by_~s(~s),~n", [Indent, Table, OtherIdAnd, OtherIdRecord]),
            case UseMnesia of
                false ->
                    io:format(S, "~sets:insert(~s_keymap_~s_to_gid, {~s, R#~s.key_id})",
                        [Indent, ModuleName, OtherIdAnd, OtherIdRecord, RecordName]);
                true ->
                    io:format(S, "~smnesia:dirty_write({~s_keymap_~s_to_gid, ~s, R#~s.key_id})",
                        [Indent, ModuleName, OtherIdAnd, OtherIdRecord, RecordName])
            end,
            io:format(S, "~n~send,~n", [Indent0])
        end,
    [GenKeyMapAddF(K, KL, "\t") || {K, KL} <- OtherKeys2],
    io:format(S, "\tok.~n~n", []),

    io:format(S, "build_record_from_row([", []),
    [FieldName | RestFieldNames] = FieldNames2,
    io:format(S, "~s", [FieldName]),
    GenFieldsStrF = fun(FieldName_, S_) -> io:format(S_, ", ~s", [FieldName_]) end,
    [GenFieldsStrF(FName, S) || FName <- RestFieldNames],
    io:format(S, "]) ->~n", []),
    io:format(S, "\tR0 = #~s{~n", [RecordName]),
    io:format(S, "\t\tkey_id = ~s,~n", [PriId]),
    gen_erl_fields_string2term(FieldNames, FieldNames2, FieldComments, S),
    io:format(S, "\t},~n", []),
    io:format(S, "\trun_data_ver:upgrade_if_need(R0).~n~n", []),

    io:format(S, "update_keymap_if_need(Old, R) ->~n", []),
    GenKeyMapUpdF = fun(OtherKey, OtherKeyLower, Indent) ->
            OtherIdAnd = gen_id_no_bracket(OtherKeyLower, "_and_"),
            OtherIdRecordOld = gen_id_record_format(OtherKeyLower, <<"Old">>, RecordName),
            OtherIdRecordNew = gen_id_record_format(OtherKeyLower, <<"R">>, RecordName),
            io:format(S, "~scase ~s =:= ~s of~n", [Indent, OtherIdRecordNew, OtherIdRecordOld]),
            io:format(S, "~s\ttrue -> void;~n", [Indent]),
            io:format(S, "~s\tfalse ->~n", [Indent]),
            case UseMnesia of
                false ->
                    io:format(S, "~s\t\tets:delete_object(~s_keymap_~s_to_gid, {~s, Old#~s.key_id}),~n",
                        [Indent, ModuleName, OtherIdAnd, OtherIdRecordOld, RecordName]),
                    io:format(S, "~s\t\tcase ~s =:= ~s of~n", [Indent, OtherIdRecordNew, build_default_value(Types, DVs, OtherKey)]),
                    io:format(S, "~s\t\t\ttrue -> void;~n", [Indent]),
                    io:format(S, "~s\t\t\tfalse ->~n", [Indent]),
                    io:format(S, "~s\t\t\t\ttry_load_indexes_~s_gids_by_~s(~s),~n", [Indent, Table, OtherIdAnd, OtherIdRecordNew]),
                    io:format(S, "~s\t\t\t\tets:insert(~s_keymap_~s_to_gid, {~s, R#~s.key_id})~n",
                        [Indent, ModuleName, OtherIdAnd, OtherIdRecordNew, RecordName]),
                    io:format(S, "~s\t\tend~n", [Indent]);
                true ->
                    io:format(S, "~s\t\tmnesia:dirty_delete_object({~s_keymap_~s_to_gid, ~s, Old#~s.key_id}),~n",
                        [Indent, ModuleName, OtherIdAnd, OtherIdRecordOld, RecordName]),
                    io:format(S, "~s\t\tcase ~s =:= ~s of~n", [Indent, OtherIdRecordNew, build_default_value(Types, DVs, OtherKey)]),
                    io:format(S, "~s\t\t\ttrue -> void;~n", [Indent]),
                    io:format(S, "~s\t\t\tfalse ->~n", [Indent]),
                    io:format(S, "~s\t\t\t\ttry_load_indexes_~s_gids_by_~s(~s),~n", [Indent, Table, OtherIdAnd, OtherIdRecordNew]),
                    io:format(S, "~s\t\t\t\tmnesia:dirty_write({~s_keymap_~s_to_gid, ~s, R#~s.key_id})~n",
                        [Indent, ModuleName, OtherIdAnd, OtherIdRecordNew, RecordName]),
                    io:format(S, "~s\t\tend~n", [Indent])
            end,
            io:format(S, "~send,~n", [Indent])
        end,
    [GenKeyMapUpdF(K, KL, "\t") || {K, KL} <- OtherKeys2],
    io:format(S, "\tok.~n~n", []),

    io:format(S, "cache_key(~s = Id) ->~n", [PriId]),
    case NeedGid of
        0 ->
            io:format(S, "\tlist_to_binary(io_lib:format(<<\"~s_~s_~s\">>, [get(game_id),~s])).~n~n", [RecordName, "~p", "~p", PirId2]);
        _ ->
            io:format(S, "\tlist_to_binary(io_lib:format(<<\"~s_~s\">>, [~s])).~n~n", [RecordName, "~p", PirId2])
    end,

    file:close(S).

gen_erl_note(Table, ModuleName, OtherKeysLower, S, UseMnesia) ->
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "%%% @Module: ~s~n", [ModuleName]),
    io:format(S, <<"%%% @Description: 自动生成~n"/utf8>>, []),
    io:format(S, "%%%--------------------------------------------------------~n", []),
    io:format(S, "-module(~s).~n", [ModuleName]),
    case UseMnesia of
        false ->
            io:format(S, "-export([init/0, load_one/1, get_one_locally/1, get_one/1, ~sset_one/1, set_field/3, del_one/1, del_one/2, syncdb/1, reload_one/1, clean_all_cache/0, cache_key/1]).~n",
                [gen_getonebyotherkey(Table, OtherKeysLower)]);
        true ->
            io:format(S, "-export([table_def/1, load_one/1, get_one_locally/1, get_one/1, ~sset_one/1, set_field/3, del_one/1, del_one/2, syncdb/1, reload_one/1, clean_all_cache/0, cache_key/1]).~n",
                [gen_getonebyotherkey(Table, OtherKeysLower)])
    end,
    io:format(S, "-include(\"common.hrl\").~n", []),
    io:format(S, "-include(\"record_~s.hrl\").~n~n", [ModuleName]).

gen_getonebyotherkey(_, []) ->
    "";
gen_getonebyotherkey(Table, OtherKeysLower) ->
    F = fun(UnionNames) ->
            By = gen_id_no_bracket(UnionNames, "_and_"),
            <<"get_", Table/binary, "_gids_by_", By/binary, "/1">>
        end,
    L = [F(UnionNames) || UnionNames <- OtherKeysLower],
    L2 = util:implode(", ", L),
    L3 = L2 ++ ", ",
    list_to_binary(L3).

gen_erl_fields_string2term([Name|RestNames], [Value|RestValues], [Comment|RestComments], S) ->
    case Comment of
        <<"erlang", _/binary>> ->
            io:format(S, "\t\t~s = ?B2T(~s)", [Name, Value]);
        _->
            io:format(S, "\t\t~s = ~s", [Name, Value])
    end,
    case RestNames of
        [] ->
            io:format(S, "~n", []);
        _ ->
            io:format(S, ",~n", []),
            gen_erl_fields_string2term(RestNames, RestValues, RestComments, S)
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

get_other_names(FieldKeyInfos) ->
    OtherKeyInfos = lists:keydelete(<<"PRIMARY">>, 1, FieldKeyInfos),
    [UnionNames || {_, UnionNames} <- OtherKeyInfos].

% [F1, F2]
get_names_upper_1st(FieldNames) ->
    [util:upper_1st_char(N) || N <- FieldNames].

% [F1_, F2_]
get_names_underscore(FieldNames) ->
    [<<N/binary, "_">> || N <- FieldNames].

% [f1, f2]
get_names_lower(FieldNames) ->
    [list_to_binary(string:to_lower(binary_to_list(N))) || N <- FieldNames].

% <<"{x, y}">>
gen_id([FieldName]) ->
    FieldName;
gen_id(FieldNames) ->
    L = util:implode(", ", FieldNames),
    B = list_to_binary(L),
    <<"{", B/binary, "}">>.

% <<"{R#r.x, R#r.y}">>
gen_id_record_format([FieldName], R, Record) ->
    <<R/binary, "#", Record/binary, ".", FieldName/binary>>;
gen_id_record_format(FieldNames, R, Record) ->
    L = [<<R/binary, "#", Record/binary, ".", FieldName/binary>> || FieldName <- FieldNames],
    L2 = util:implode(", ", L),
    B = list_to_binary(L2),
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

gen_fields_sql(FieldNames, FieldTypes, FieldComments) ->
    NameF = fun({Name, Type, Comment}) ->
            case Comment of
                <<"erlang", _/binary>> -> <<"?T2B(", Name/binary, ")">>;
                _ ->
                    case Type =:= <<"tinytext">> orelse Type =:= <<"text">> orelse Type =:= <<"varchar">> orelse Type =:= <<"char">> of
                        true ->
                            case util:contains(Name, [<<"name">>, <<"msg">>, <<"description">>, <<"notice">>, <<"title">>]) of
                                true -> <<"util:esc(", Name/binary, ")">>;
                                false -> Name
                            end;
                        false -> Name
                    end
            end
        end,
    Fields = lists:zip3(FieldNames, FieldTypes, FieldComments),
    FieldNames2 = [NameF(Field) || Field <- Fields],
    L = util:implode(", ", FieldNames2),
    list_to_binary(L).

gen_update_fields_sql(FieldNames) ->
    FieldNamesN = [<<Name/binary, " = ?">> || Name <- FieldNames],
    L = util:implode(", ", FieldNamesN),
    list_to_binary(L).

gen_fields_placeholder(Len) ->
    L = lists:duplicate(Len, "?"),
    L2 = util:implode(",", L),
    list_to_binary(L2).

gen_fields_placeholder2(FieldTypes) ->
    F = fun(Type) ->
            case Type =:= <<"tinytext">> orelse Type =:= <<"text">> orelse Type =:= <<"varchar">> orelse Type =:= <<"char">> of
                true -> "'~s'";
                false -> "~p"
            end
        end,
    L = [F(T) || T <- FieldTypes],
    L2 = util:implode(",", L),
    list_to_binary(L2).

build_default_value(Types, DVs, [OK]) ->
    {_, DV} = lists:keyfind(OK, 1, DVs),
    {_, Type} = lists:keyfind(OK, 1, Types),
    add_quote_if_need(Type, DV);
build_default_value(Types, DVs, OtherKey) ->
    L = [build_default_value(Types, DVs, [OK]) || OK <- OtherKey],
    L2 = util:implode(", ", L),
    B = list_to_binary(L2),
    <<"{", B/binary, "}">>.

add_quote_if_need(Type, DV) ->
    case Type =:= <<"tinytext">> orelse Type =:= <<"text">> orelse Type =:= <<"varchar">> orelse Type =:= <<"char">> of
        true ->
            case is_binary(DV) of
                true -> <<"<<\"", DV/binary, "\">>">>;
                false -> <<"<<>>">>
            end;
        false ->
            case DV of
                undefined -> <<"0">>;
                _ -> DV
            end
    end.

