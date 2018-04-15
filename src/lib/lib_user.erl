%%%--------------------------------------
%%% @Module  : lib_user
%%% @Description: 用户相关处理
%%%--------------------------------------
-module(lib_user).
-export([lock/1, unlock/2]).
-export([get_last_login_uid_of_org_device_id/1, get_unbind_uid_of_org_device_id/1, get_bind_uid/2]).

-include("common.hrl").


% 锁定成功，返回{true, Cas}
lock(PlayerId) ->
    LockKey = cache_lock_key(PlayerId),
    case cache:get_and_lock(LockKey) of
        false ->
            cache:set(LockKey, <<>>),
            lock(PlayerId);
        {true, Cas, _} ->
            {true, Cas}
    end.

unlock(PlayerId, Cas) ->
    cache:unlock(cache_lock_key(PlayerId), Cas).

cache_lock_key(PlayerId) ->
    list_to_binary(io_lib:format(<<"lock_usr_~p">>, [PlayerId])).

get_last_login_uid_of_org_device_id(DeviceId) ->
    case db_esql:get_one(?DB_USR, <<"select id from user where org_device_id=? order by last_login_time desc limit 1">>, [DeviceId]) of
        null -> -1;
        Id -> Id
    end.

get_unbind_uid_of_org_device_id(DeviceId) ->
    case db_esql:get_one(?DB_USR, <<"select id from user where org_device_id=? and is_bind=0 limit 1">>, [DeviceId]) of
        null -> -1;
        Id -> Id
    end.

get_bind_uid(<<"gc_id">> = BindType, BindVal) ->
    case usr_user:get_user_gids_by_ios_gamecenter_id(BindVal) of
        [] -> throw({-1, <<"gc_id not bound by any users">>});
        [Id|_] -> Id
    end;
get_bind_uid(<<"gg_id">> = BindType, BindVal) ->
    case usr_user:get_user_gids_by_google_id(BindVal) of
        [] -> throw({-1, <<"gg_id not bound by any users">>});
        [Id|_] -> Id
    end;
get_bind_uid(<<"fb_id">> = BindType, BindVal) ->
    case usr_user:get_user_gids_by_facebook_id(BindVal) of
        [] -> throw({-1, <<"fb_id not bound by any users">>});
        [Id|_] -> Id
    end;
get_bind_uid(BindType, _BindVal) ->
    throw({?ERRNO_WRONG_PARAM, <<"unsupported bind_type: ", BindType/binary>>}).

