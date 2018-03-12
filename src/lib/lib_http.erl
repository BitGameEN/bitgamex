%%%--------------------------------------
%%% @Module  : lib_http
%%% @Description: http辅助模块
%%%--------------------------------------

-module(lib_http).
-export([reply_body_succ/1, reply_body_fail/3]).


reply_body_succ(ResMap) when is_map(ResMap) ->
    jiffy:encode(ResMap#{succ => 1}).

reply_body_fail(ReqAction, ErrNo, ErrMsg) ->
    jiffy:encode(#{succ => 0, req => ReqAction, errno => ErrNo, errmsg => ErrMsg}).

