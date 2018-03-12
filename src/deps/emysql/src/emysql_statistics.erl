-module(emysql_statistics).

-export([init/0, stat/3, dump/0]).

-record(sample, {
        pool_id,
        sql,
        time_start,
        time_spent,
        wait_num
    }).

init() ->
    ets:new(ets_statistics, [named_table, public, duplicate_bag, {write_concurrency,true}]).

stat(PoolId, Sql, {M, F, A}) ->
    WaitNum = emysql_conn_mgr:get_wait_num(PoolId),
    TimeStart = now_seconds(),
    {TimeSpent, Res} = timer:tc(M, F, A),
    ets:insert(ets_statistics, #sample{pool_id = PoolId, sql = Sql, time_start = TimeStart, time_spent = TimeSpent, wait_num = WaitNum}),
    Res.

dump() ->
    %{ok, S} = file:open("emysql_statistics.txt", write),
    F = fun(E) ->
            case catch io:format("~p\t~p\t~p\t~p\t~ts~n", [E#sample.pool_id, E#sample.time_start, E#sample.time_spent, E#sample.wait_num, E#sample.sql]) of
                ok -> void;
                _ -> io:format("~p\t~p\t~p\t~p\t~p~n", [E#sample.pool_id, E#sample.time_start, E#sample.time_spent, E#sample.wait_num, E#sample.sql])
            end
        end,
    lists:foreach(F, ets:tab2list(ets_statistics)),
    %file:close(S),
    ets:delete_all_objects(ets_statistics).

now_seconds() ->
    {M, S, _} = os:timestamp(),
    M * 1000000 + S.