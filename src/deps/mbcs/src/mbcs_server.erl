%% @author Xiangyu LU <luxiangyu@msn.com>
%% @copyright 2010 luxiangyu@msn.com.

%% @doc erlang-mbcs server.

-module(mbcs_server).
-author('luxiangyu@msn.com').
-behaviour(gen_server).

-export([start/0, stop/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, code_change/3,
handle_info/2]).

%%--------------------------------------------------------------------------

-define(MBCS_ENCODE_OPTIONS_DEFAULT, [{return, binary},
{error, strict},
{error_replace_char, $?},
{bom, false}]).
-define(MBCS_DECODE_OPTIONS_DEFAULT, [{return, binary},
{error, strict},
{error_replace_char, 16#FFFD}]).

%%--------------------------------------------------------------------------

-record(mbcs_codecs_item, {
undefined :: set:set(), % undefine bytes
leadbytes :: set:set(), % dbcs lead bytes
mbtable :: dict:dict(), % multiple byte to wide character table
wctable :: dict:dict() % wide character to multiple byte table
}).

-record(mbcs_server, {
codecs :: dict:dict(), % codecs name to codecs type 'unicode' | 'mbcs' | 'gb18030'
mbcs_codecs :: dict:dict() % mbcs codecs name to #mbcs_codecs_item{}
}).

-record(mbcs_options, {
return :: atom(), % file name
error :: atom(), % error strategy 'strict' | 'ignore' | 'replace'
error_replace_char :: non_neg_integer(), % error replace char
bom :: boolean() % encode bom
}).

%%--------------------------------------------------------------------------

start() ->
DictPath = code:priv_dir(mbcs),
{ok, CodecsList} = file:consult(DictPath ++ "/codecs.conf"),
{ok, MbcsDictBin} = file:read_file(DictPath ++ "/mbcs.dict"),
State = #mbcs_server{codecs = dict:from_list(CodecsList),
mbcs_codecs = binary_to_term(MbcsDictBin)
},
gen_server:start_link({local, ?MODULE}, ?MODULE, State, []).

stop() ->
gen_server:cast(?MODULE, stop).

init(State=#mbcs_server{}) ->
{ok, State}.

handle_call({encode, Unicode, Encoding, Options}, _From, State)
when is_list(Unicode), is_atom(Encoding), is_list(Options) ->
Res = do_encode(Unicode, Encoding, Options, State),
{reply, Res, State};
handle_call({decode, String, Encoding, Options}, _From, State)
when is_list(String), is_atom(Encoding), is_list(Options) ->
Res = do_decode_string(String, Encoding, Options, State),
{reply, Res, State};
handle_call({decode, Binary, Encoding, Options}, _From, State)
when is_binary(Binary), is_atom(Encoding), is_list(Options) ->
Res = do_decode_binary(Binary, Encoding, Options, State),
{reply, Res, State};
handle_call({from_to, String, InputEncoding, OutputEncoding, Options}, _From, State)
when is_list(String), is_atom(InputEncoding), is_atom(OutputEncoding), is_list(Options) ->
Res = do_from_to_string(String, InputEncoding, OutputEncoding, Options, State),
{reply, Res, State};
handle_call({from_to, Binary, InputEncoding, OutputEncoding, Options}, _From, State)
when is_binary(Binary), is_atom(InputEncoding), is_atom(OutputEncoding), is_list(Options) ->
Res = do_from_to_binary(Binary, InputEncoding, OutputEncoding, Options, State),
{reply, Res, State};
handle_call(_Message, _From, State) ->
Res = {error, illegal_message},
{reply, Res, State}.

handle_cast(stop, State) ->
{stop, normal, State}.

terminate(_Reason, _State) ->
ok.

code_change(_OldVsn, State, _Extra) ->
State.

handle_info({'EXIT', _Pid, normal}, State) ->
% io:format("normal acceptor down~n"),
{noreply, State};
handle_info({'EXIT', _Pid, _Reason}, State) ->
% timer:sleep(100),
{noreply, State};
handle_info(Info, State) ->
error_logger:info_report([{'INFO', Info}, {'State', State}]),
{noreply, State}.

%% @spec parse_options(Options, Default) -> {ok, MbcsOptions} | {error, Reason}
%%
%% @doc Parse Options List to Option Dict,
%% Return {ok, MbcsOptions} | {error, Reason}.

parse_options(Options, OptionsDefault)
when is_list(Options), is_list(OptionsDefault) ->
parse_options1(OptionsDefault ++ Options, #mbcs_options{}).

parse_options1([], MbcsOptions=#mbcs_options{}) ->
{ok, MbcsOptions};
parse_options1([{return, binary} | Tail], MbcsOptions=#mbcs_options{}) ->
parse_options1(Tail, MbcsOptions#mbcs_options{return=binary});
parse_options1([{return, list} | Tail], MbcsOptions=#mbcs_options{}) ->
parse_options1(Tail, MbcsOptions#mbcs_options{return=list});
parse_options1([{error, ignore} | Tail], MbcsOptions=#mbcs_options{}) ->
parse_options1(Tail, MbcsOptions#mbcs_options{error=ignore});
parse_options1([{error, strict} | Tail], MbcsOptions=#mbcs_options{}) ->
parse_options1(Tail, MbcsOptions#mbcs_options{error=strict});
parse_options1([{error, replace} | Tail], MbcsOptions=#mbcs_options{}) ->
parse_options1(Tail, MbcsOptions#mbcs_options{error=replace});
parse_options1([{replace, Char} | Tail], MbcsOptions=#mbcs_options{})
when is_integer(Char)->
parse_options1(Tail, MbcsOptions#mbcs_options{error=replace, error_replace_char=Char});
parse_options1([{error_replace_char, Char} | Tail], MbcsOptions=#mbcs_options{})
when is_integer(Char)->
parse_options1(Tail, MbcsOptions#mbcs_options{error_replace_char=Char});
parse_options1([{bom, true} | Tail], MbcsOptions=#mbcs_options{}) ->
parse_options1(Tail, MbcsOptions#mbcs_options{bom=true});
parse_options1([{bom, false} | Tail], MbcsOptions=#mbcs_options{}) ->
parse_options1(Tail, MbcsOptions#mbcs_options{bom=false});
parse_options1([UnknownOption | _], #mbcs_options{}) ->
{error, {unknown_option, [{option, UnknownOption}]}}.

do_encode(Unicode, Encoding, Options, State=#mbcs_server{codecs=Codecs}) ->
case parse_options(Options, ?MBCS_ENCODE_OPTIONS_DEFAULT) of
{ok, MbcsOptions} ->
case dict:find(Encoding, Codecs) of
{ok, {unicode, MapedEncoding}} ->
do_encode_unicode(Unicode, MapedEncoding, MbcsOptions, State);
{ok, {mbcs, MapedEncoding}} ->
do_encode_mbcs(Unicode, MapedEncoding, MbcsOptions, State);
error ->
{error, {unkonwn_encoding, [{encoding, Encoding}]}}
end;
{error, Reason} ->
{error, Reason}
end.

do_encode_unicode(Unicode, Encoding, #mbcs_options{return=Return, bom=Bom}, _) ->
NewUnicode = case Unicode of
[16#FEFF, RestCodes] ->
case Bom of
true ->
Unicode;
false ->
RestCodes
end;
Unicode ->
case Bom of
true ->
[16#FEFF, Unicode];
false ->
Unicode
end
end,
Binary = unicode:characters_to_binary(NewUnicode, unicode, Encoding),
case Return of
binary ->
Binary;
list ->
erlang:binary_to_list(Binary)
end.

do_encode_mbcs(Unicode, Encoding, MbcsOptions, State) ->
case dict:find(Encoding, State#mbcs_server.mbcs_codecs) of
{ok, #mbcs_codecs_item{wctable=WCTable}} ->
do_encode_mbcs1(Unicode, WCTable, MbcsOptions, 1, []);
error ->
{error, {unkonwn_encoding, [{encoding, Encoding}]}}
end.


do_encode_mbcs1([], _, #mbcs_options{return=Return}, _, String) ->
ReturnString = lists:reverse(String),
case Return of
list -> ReturnString;
binary -> erlang:list_to_binary(ReturnString)
end;
do_encode_mbcs1([Code | RestCodes],
WCTable,
MbcsOptions=#mbcs_options{error=Error,
error_replace_char=ErrorReplaceChar},
Pos,
String) ->
case dict:find(Code, WCTable) of
{ok, Multibyte} ->
case Multibyte > 16#FF of
false ->
do_encode_mbcs1(RestCodes, WCTable, MbcsOptions, Pos+1, [Multibyte | String]);
true ->
do_encode_mbcs1(RestCodes, WCTable, MbcsOptions, Pos+1, [Multibyte band 16#FF, Multibyte bsr 8 | String])
end;
error ->
case Error of
ignore ->
do_encode_mbcs1(RestCodes, WCTable, MbcsOptions, Pos+1, String);
replace ->
do_encode_mbcs1(RestCodes, WCTable, MbcsOptions, Pos+1, [ErrorReplaceChar | String]);
strict ->
{error, {unmapping_unicode, [{unicode, Code}, {pos, Pos}]}}
end
end.

do_decode_string(String, Encoding, Options, State=#mbcs_server{}) ->
try erlang:list_to_binary(String) of
Binary ->
do_decode_binary(Binary, Encoding, Options, State)
catch
error:_ ->
{error, {illegal_list, [{list, String}, {line, ?LINE}]}}
end.

do_decode_binary(Binary, Encoding, Options, State=#mbcs_server{codecs=Codecs}) ->
case parse_options(Options, ?MBCS_DECODE_OPTIONS_DEFAULT) of
{ok, MbcsOptions} ->
case dict:find(Encoding, Codecs) of
{ok, {unicode, MapedEncoding}} ->
do_decode_unicode(Binary, MapedEncoding, MbcsOptions, State);
{ok, {mbcs, MapedEncoding}} ->
do_decode_mbcs(Binary, MapedEncoding, MbcsOptions, State);
error ->
{error, {unkonwn_encoding, [{encoding, Encoding}]}}
end;
{error, Reason} ->
{error, Reason}
end.

do_decode_unicode(Binary, Encoding, #mbcs_options{bom=_Bom}, _) ->
unicode:characters_to_list(Binary, Encoding).

do_decode_mbcs(Binary, Encoding, MbcsOptions, State) ->
case dict:find(Encoding, State#mbcs_server.mbcs_codecs) of
{ok, #mbcs_codecs_item{undefined=Undefined, leadbytes=Leadbytes, mbtable=MBTable}} ->
do_decode_mbcs1(Binary, Undefined, Leadbytes, MBTable, MbcsOptions, 1, []);
error ->
{error, {unkonwn_encoding, [{encoding, Encoding}]}}
end.

do_decode_mbcs1(<<>>, _, _, _, _, _, Unicode) when is_list(Unicode) ->
lists:reverse(Unicode);
do_decode_mbcs1(<<LeadByte:8, Rest/binary>>,
Undefined,
Leadbytes,
MBTable,
MbcsOptions=#mbcs_options{error=Error,
error_replace_char=ErrorReplaceChar},
Pos,
Unicode) ->
case sets:is_element(LeadByte, Undefined) of
true ->
case Error of
ignore ->
do_decode_mbcs1(Rest, Undefined, Leadbytes, MBTable,
MbcsOptions,
Pos+1,
Unicode);
replace ->
do_decode_mbcs1(Rest, Undefined, Leadbytes, MBTable,
MbcsOptions,
Pos+1,
[ErrorReplaceChar | Unicode]);
strict ->
{error, {undefined_character, [{character, LeadByte},
{pos, Pos}]}}
end;
false ->
case sets:size(Leadbytes) =/= 0 andalso sets:is_element(LeadByte, Leadbytes) of
false ->
case dict:find(LeadByte, MBTable) of
{ok, Code} ->
do_decode_mbcs1(Rest, Undefined,
Leadbytes, MBTable,
MbcsOptions,
Pos+1,
[Code | Unicode]);
error ->
case Error of
ignore ->
do_decode_mbcs1(Rest, Undefined,
Leadbytes, MBTable,
MbcsOptions,
Pos+1,
Unicode);
replace ->
do_decode_mbcs1(Rest, Undefined, Leadbytes,
MBTable, MbcsOptions,
Pos+1,
[ErrorReplaceChar | Unicode]);
strict ->
{error, {unmapping_character, [{character, LeadByte}, {pos, Pos}]}}
end
end;
true ->
case erlang:bit_size(Rest) of
0 ->
case Error of
ignore ->
do_decode_mbcs1(Rest, Undefined, Leadbytes, MBTable, MbcsOptions, Pos+1, Unicode);
replace ->
do_decode_mbcs1(Rest, Undefined, Leadbytes, MBTable, MbcsOptions, Pos+1, [ErrorReplaceChar | Unicode]);
strict ->
{error, {incomplete_multibyte_sequence, [{leadbyte, LeadByte}, {pos, Pos}]}}
end;
_Any ->
<<FollowByte:8, Rest1/binary>> = Rest,
MultibyteChar = LeadByte bsl 8 bor FollowByte,
case dict:find(MultibyteChar, MBTable) of
{ok, Code} ->
do_decode_mbcs1(Rest1, Undefined, Leadbytes, MBTable, MbcsOptions, Pos+2, [Code | Unicode]);
error ->
case Error of
ignore ->
do_decode_mbcs1(Rest1, Undefined, Leadbytes, MBTable, MbcsOptions, Pos+2, Unicode);
replace ->
do_decode_mbcs1(Rest1, Undefined, Leadbytes, MBTable, MbcsOptions, Pos+2, [ErrorReplaceChar | Unicode]);
strict ->
{error, {unmapping_multibyte_character, [{multibyte_character, MultibyteChar}, {pos, Pos}]}}
end
end
end
end
end.

do_from_to_string(String, InputEncoding, OutputEncoding, Options, State=#mbcs_server{}) ->
try erlang:list_to_binary(String) of
Binary ->
do_from_to_binary(Binary, InputEncoding, OutputEncoding, Options, State)
catch
error:_ ->
{error, {illegal_list, [{list, String}, {line, ?LINE}]}}
end.

do_from_to_binary(Binary, InputEncoding, OutputEncoding, Options, State=#mbcs_server{}) ->
case do_decode_binary(Binary, InputEncoding, Options, State) of
{error, Reason} ->
{error, Reason};
Unicode ->
case do_encode(Unicode, OutputEncoding, Options, State) of
{error, Reason} ->
{error, Reason};
StringOrBinary ->
StringOrBinary
end
end.