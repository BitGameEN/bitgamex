%%%--------------------------------------
%%% @Module  : id_gen
%%% @Description: 唯一id生成（基于CouchBase）
%%%--------------------------------------
-module(id_gen).
-export(
    [
        start_link/2,
        set_start_id/2,
        gen_id/1
    ]
).

-include("common.hrl").


start_link(ConnPoolSize, Host) ->
    {ok, Pid} = cberl:start_link(?IDGEN, ConnPoolSize, Host, "", "", "id_gen"),
    init_start_ids(),
    {ok, Pid}.

set_start_id(Type, StartId) when is_atom(Type), is_integer(StartId) ->
    ok = cberl:set(gk_idgen, id_key(Type), 0, integer_to_binary(StartId), none),
    ok.

gen_id(Type) when is_atom(Type) ->
    {ok, _, IdBin} = cberl:incr(gk_idgen, id_key(Type), 1),
    binary_to_integer(IdBin).

id_key(Type) ->
    list_to_binary(atom_to_list(Type)).

init_start_ids() ->
    F = fun(Type) ->
            case cberl:get(?IDGEN, id_key(Type)) of
                {Key, Cas, Val0} ->
                    case (catch binary_to_integer(Val0)) of
                        V when is_integer(V), V >= 0 ->
                            ok;
                        _ ->
                            set_start_id(Type, get_max_id(Type))
                    end;
                {Key, {error, key_enoent}} ->
                    set_start_id(Type, get_max_id(Type));
                {Key, {error, Err}} ->
                    throw({error, Err});
                {error, Err} ->
                    throw({error, Err})
            end
        end,
    Types = [
        usr_user
    ],
    [F(Type) || Type <- Types],
    ok.

get_max_id(usr_user) ->
    case db_esql:get_one(?DB_USR, <<"select max(id) from user">>, []) of
        null -> 0;
        Count -> Count
    end;
get_max_id(_) ->
    0.

