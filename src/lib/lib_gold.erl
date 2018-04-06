%%%--------------------------------------
%%% @Module  : lib_gold
%%% @Description: 金币存取的底层处理
%%%--------------------------------------
-module(lib_gold).
-export([gold/2, gold/3]).

% RawGold是个json结构，格式：{"BGX":数量, "BTC":数量, "ETH":数量, ...}
gold(RawGold, GoldType) when is_binary(RawGold) ->
    {L} = jiffy:decode(RawGold),
    case lists:keyfind(GoldType, 1, L) of
        false -> 0;
        {_, V} -> V
    end.

gold(RawGold, GoldType, NewGoldValue) when is_binary(RawGold) ->
    {L0} = jiffy:decode(RawGold),
    L = lists:keystore(GoldType, 1, L0, {GoldType, NewGoldValue}),
    jiffy:encode({L}).

