%%%-----------------------------------
%%% @Module  : lib_address
%%% @Description: 地址相关模块
%%%-----------------------------------
-module(lib_address).
-export([check/1]).
-export([check_ethereum/1, check_bitcoin/1]).

check(Addr0) when is_binary(Addr0) ->
    Addr = binary_to_list(Addr0),
    check_ethereum(Addr) orelse
        check_bitcoin(Addr).

% 简单检查地址，不考虑校验和
check_ethereum(Addr) when is_list(Addr) ->
    case re:run(Addr, "^(0x){1}[0-9a-fA-F]{40}$", []) of
        nomatch -> false;
        {match, _} -> true
    end.

% https://rosettacode.org/wiki/Bitcoin/address_validation
check_bitcoin(Addr) when is_list(Addr) ->
  try
    <<Address:21/binary, Checksum:4/binary>> = base58:base58_to_binary(Addr),
    <<Version:1/binary, _/binary>> = Address,
    <<0>> = Version,
    <<FourBytes:4/binary, _T/binary>> = crypto:hash(sha256, crypto:hash(sha256, Address)),
    Checksum =:= FourBytes
  catch
    _:_ ->
        false
  end.

