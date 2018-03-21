%%%--------------------------------------
%%% @Module  : lib_user
%%% @Description: 用户相关处理
%%%--------------------------------------
-module(lib_user).
-export([lock/1, unlock/2]).

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

