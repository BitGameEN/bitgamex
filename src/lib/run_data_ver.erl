%%%--------------------------------------
%%% @Module  : run_data_ver
%%% @Description: run数据结构增量更新的版本管理
%%% 注意：
%%% 1. 保证版本号的连续性
%%% 2. 只需修改相应newest_ver和添加upgrade函数分支
%%% 3. 编码时要仔细，多测试
%%%--------------------------------------
-module(run_data_ver).

-export([newest_ver/1, upgrade_if_need/1]).

-include("record.hrl").

-define(VER_POS, 4).


newest_ver(run_role) -> 0;
newest_ver(_) -> 0.

upgrade_if_need(RunData) when is_tuple(RunData) ->
    case get(run_data_in_upgrade) of
        undefined ->
            Tag = element(1, RunData),
            MaxVer = newest_ver(Tag),
            case MaxVer =:= 0 of
                true ->
                    RunData;
                false ->
                    TheVer = element(?VER_POS, RunData),
                    case TheVer >= MaxVer of
                        true ->
                            RunData;
                        false ->
                            upgrade(Tag, RunData, TheVer, MaxVer)
                    end
            end;
        Updated -> Updated
    end;
upgrade_if_need(RunData) ->
    RunData.

% 升至当前版本
upgrade(Tag, RunData, TheVer, MaxVer) when TheVer >= MaxVer ->
    put(run_data_in_upgrade, RunData),
    Tag:set_one(RunData),
    erase(run_data_in_upgrade),
    RunData;

% 未匹配
upgrade(_, RunData, _, _) ->
    RunData.
