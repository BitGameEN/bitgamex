%%
%% %CopyrightBegin%
%%
%% Copyright Xiangyu LU(luxiangyu@msn.com) 2009-2010. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%
-module(mbcs).
-export([start/0, stop/0]).
-export([encode/2, encode/3, decode/2, decode/3, from_to/3, from_to/4]).

%%---------------------------------------------------------------------------

-type unicode() :: [non_neg_integer()].
-type encoding() :: atom().
-type option() :: {atom(), term()}.
-type options() :: [option()].

%%---------------------------------------------------------------------------

%% @spec start() -> ok | {error, Reason}
%%
%% @doc Start mbcs server, Return {ok, Pid}.

-spec start() -> ok.

start() ->
case application:start(mbcs) of
{ok, _} ->
ok;
{error,{already_started,_}} ->
ok;
Error ->
Error
end.

%% @spec stop() -> ok
%%
%% @doc Stop the mbcs server.

-spec stop() -> ok.

stop() ->
application:stop(mbcs).

%% ---------------------------------------------------------------------

%% @spec encode(Unicode, Encoding) -> binary() | string() | {error, Reason}
%%
%% @equiv encode(Unicode, Encoding, [])
%%
%% @see encode/3

-spec encode(unicode(), encoding()) -> binary() | string() | {error, tuple()}.

encode(Unicode, Encoding) ->
encode(Unicode, Encoding, []).

%% @spec encode(Unicode, Encoding, Options) -> binary() | string() | {error, Reason}
%%
%% Unicode = unicode()
%% Encoding = 'cp037'
%% | 'cp437'
%% | 'cp500'
%% | 'cp737'
%% | 'cp775'
%% | 'cp850'
%% | 'cp852'
%% | 'cp855'
%% | 'cp857'
%% | 'cp860'
%% | 'cp861'
%% | 'cp862'
%% | 'cp863'
%% | 'cp864'
%% | 'cp865'
%% | 'cp866'
%% | 'cp869'
%% | 'cp874'
%% | 'cp875'
%% | 'cp932'
%% | 'cp936'
%% | 'gbk'
%% | 'cp949'
%% | 'cp950'
%% | 'big5'
%% | 'cp1026'
%% | 'cp1250'
%% | 'cp1251'
%% | 'cp1252'
%% | 'cp1253'
%% | 'cp1254'
%% | 'cp1255'
%% | 'cp1256'
%% | 'cp1257'
%% | 'cp1258'
%% | 'cp10000'
%% | 'cp10006'
%% | 'cp10007'
%% | 'cp10029'
%% | 'cp10079'
%% | 'cp10081'
%% | 'utf8'
%% | 'utf16'
%% | 'utf16le'
%% | 'utf16be'
%% | 'utf32'
%% | 'utf32le'
%% | 'utf32be'
%% Options = [Option]
%% Option = {return, list}
%% | {return, binary}
%% | {error, strict}
%% | {error, ignore}
%% | {error, replace}
%% | {replace, non_neg_integer()}
%% | {bom, true}
%% | {bom, false}
%%
%% @doc Return a Binary or String.
%%
%% @see encode/2

-spec encode(unicode(), encoding(), options()) -> binary() | string() | {error, tuple()}.

encode(Unicode, Encoding, Options) ->
try
gen_server:call(mbcs_server, {encode, Unicode, Encoding, Options})
catch
throw:_ ->
{error, unkonwn_error};
exit:_ ->
{error, unkonwn_error};
error:_ ->
{error, unkonwn_error}
end.


%% ---------------------------------------------------------------------

%% @spec decode(StringOrBinary, Encoding) -> unicode() | {error, Reason}
%%
%% @equiv decode(StringOrBinary, Encoding, [])
%%
%% @see decode/3

-spec decode(string()|binary(), encoding()) -> unicode() | {error, tuple()}.

decode(StringOrBinary, Encoding) ->
decode(StringOrBinary, Encoding, []).

%% @spec decode(StringOrBinary, Encoding, Options) -> unicode() | {error, Reason}
%%
%% StringOrBinary = string()|binary()
%% Encoding = 'cp037'
%% | 'cp437'
%% | 'cp500'
%% | 'cp737'
%% | 'cp775'
%% | 'cp850'
%% | 'cp852'
%% | 'cp855'
%% | 'cp857'
%% | 'cp860'
%% | 'cp861'
%% | 'cp862'
%% | 'cp863'
%% | 'cp864'
%% | 'cp865'
%% | 'cp866'
%% | 'cp869'
%% | 'cp874'
%% | 'cp875'
%% | 'cp932'
%% | 'cp936'
%% | 'gbk'
%% | 'cp949'
%% | 'cp950'
%% | 'big5'
%% | 'cp1026'
%% | 'cp1250'
%% | 'cp1251'
%% | 'cp1252'
%% | 'cp1253'
%% | 'cp1254'
%% | 'cp1255'
%% | 'cp1256'
%% | 'cp1257'
%% | 'cp1258'
%% | 'cp10000'
%% | 'cp10006'
%% | 'cp10007'
%% | 'cp10029'
%% | 'cp10079'
%% | 'cp10081'
%% | 'utf8'
%% | 'utf16'
%% | 'utf16le'
%% | 'utf16be'
%% | 'utf32'
%% | 'utf32le'
%% | 'utf32be'
%% Options = [Option]
%% Option = {return, list}
%% | {return, binary}
%% | {error, strict}
%% | {error, ignore}
%% | {error, replace}
%% | {replace, non_neg_integer()}
%% | {bom, true}
%% | {bom, false}
%%
%% @doc Return a Unicode.
%%
%% @see decode/2

-spec decode(string()|binary(), encoding(), options()) -> unicode() | {error, tuple()}.

decode(StringOrBinary, Encoding, Options) ->
try
gen_server:call(mbcs_server, {decode, StringOrBinary, Encoding, Options})
catch
throw:_ ->
{error, unkonwn_error};
exit:_ ->
{error, unkonwn_error};
error:_ ->
{error, unkonwn_error}
end.

%% ---------------------------------------------------------------------

%% @spec from_to(StringOrBinary, InputEncoding, OutputEncoding) -> string() | binary() | {error, Reason}
%%
%% @equiv from_to(StringOrBinary, InputEncoding, OutputEncoding, [])
%%
%% @see from_to/4

-spec from_to(string()|binary(), encoding(), encoding()) -> string() | binary() | {error, tuple()}.

from_to(StringOrBinary, InputEncoding, OutputEncoding) ->
from_to(StringOrBinary, InputEncoding, OutputEncoding, []).

%% @spec from_to(StringOrBinary, InputEncoding, OutputEncoding, Options) -> string() | binary() | {error, Reason}
%%
%% StringOrBinary = string()|binary()
%% InputEncoding = Encoding
%% OutputEncoding = Encoding
%% Encoding = 'cp037'
%% | 'cp437'
%% | 'cp500'
%% | 'cp737'
%% | 'cp775'
%% | 'cp850'
%% | 'cp852'
%% | 'cp855'
%% | 'cp857'
%% | 'cp860'
%% | 'cp861'
%% | 'cp862'
%% | 'cp863'
%% | 'cp864'
%% | 'cp865'
%% | 'cp866'
%% | 'cp869'
%% | 'cp874'
%% | 'cp875'
%% | 'cp932'
%% | 'cp936'
%% | 'gbk'
%% | 'cp949'
%% | 'cp950'
%% | 'big5'
%% | 'cp1026'
%% | 'cp1250'
%% | 'cp1251'
%% | 'cp1252'
%% | 'cp1253'
%% | 'cp1254'
%% | 'cp1255'
%% | 'cp1256'
%% | 'cp1257'
%% | 'cp1258'
%% | 'cp10000'
%% | 'cp10006'
%% | 'cp10007'
%% | 'cp10029'
%% | 'cp10079'
%% | 'cp10081'
%% | 'utf8'
%% | 'utf16'
%% | 'utf16le'
%% | 'utf16be'
%% | 'utf32'
%% | 'utf32le'
%% | 'utf32be'
%% Options = [Option]
%% Option = {return, list}
%% | {return, binary}
%% | {error, strict}
%% | {error, ignore}
%% | {error, replace}
%% | {replace, non_neg_integer()}
%% | {bom, true}
%% | {bom, false}
%%
%% @doc Return a Unicode.
%%
%% @see decode/2

-spec from_to(string()|binary(), encoding(), encoding(), options()) -> string() | binary() | {error, tuple()}.

from_to(StringOrBinary, InputEncoding, OutputEncoding, Options) ->
try
gen_server:call(mbcs_server, {from_to, StringOrBinary, InputEncoding, OutputEncoding, Options})
catch
throw:_ ->
{error, unkonwn_error};
exit:_ ->
{error, unkonwn_error};
error:_ ->
{error, unkonwn_error}
end.

