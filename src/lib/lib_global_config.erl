%%%--------------------------------------
%%% @Module  : lib_global_config
%%% @Description: 全局配置参数
%%%--------------------------------------
-module(lib_global_config).
-export([get/1]).

-include("common.hrl").
-include("gameConfigGlobalKey.hrl").
-include("record_usr_global_config.hrl").


get(?GLOBAL_CONFIG_KEY_TRANSFER_DISCOUNT_IN_GAME = Key) ->
    case usr_global_config:get_one(Key) of
        [] -> 0;
        #usr_global_config{content = Val} ->
            util:zero_if_negative(binary_to_float(Val))
    end;
get(?GLOBAL_CONFIG_KEY_TRANSFER_DISCOUNT_TO_XCHG = Key) ->
    case usr_global_config:get_one(Key) of
        [] -> 0;
        #usr_global_config{content = Val} ->
            util:zero_if_negative(binary_to_float(Val))
    end;
get(_) ->
    undefined.

