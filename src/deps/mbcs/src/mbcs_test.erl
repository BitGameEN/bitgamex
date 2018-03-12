%%
%% @author Xiangyu LU
%% created on 2010-02-20
%%
%% Some simple functional test cases, test the mb implementation
-module(mbcs_test).
-export([encode/3, decode/3]).
-include_lib("eunit/include/eunit.hrl").

encode(Unicode, Encoding, Options) ->
mbcs:encode(Unicode, Encoding, Options).

decode(String, Encoding, Options) ->
mbcs:decode(String, Encoding, Options).

mb_test_() ->
mbcs:start(),
[?_assert(encode("\x{4f60}\x{597d}", cp936, [{return,list}]) =:= "\xc4\xe3\xba\xc3"),
?_assert(encode("\x{4f60}\x{597d}", gbk, [{return,list}]) =:= "\xc4\xe3\xba\xc3"),
?_assert(decode("\xc4\xe3\xba\xc3", gbk, []) =:= "\x{4f60}\x{597d}"),
?_assert(encode("\x{4f60}\x{597d}", utf8, [{return,list}]) =:= "\xe4\xbd\xa0\xe5\xa5\xbd"),
?_assert(encode("\x{4f60}\x{597d}", utf8, [{return,list},{bom,true}]) =:= "\xef\xbb\xbf\xe4\xbd\xa0\xe5\xa5\xbd"),
?_assert(decode("\xe4\xbd\xa0\xe5\xa5\xbd", utf8, []) =:= "\x{4f60}\x{597d}")
].