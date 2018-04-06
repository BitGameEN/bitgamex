%%%--------------------------------------
%%% @Module  : lib_user_gold_transfer
%%% @Description: 用户金币转账相关处理
%%%--------------------------------------
-module(lib_user_gold_transfer).
-export([update_transfer_log/3, gen_uuid/0]).
-export([lock/2, unlock/3]).

-include("common.hrl").
-include("record_usr_gold_transfer.hrl").


update_transfer_log(TransactionType, TransactionId, Rs) ->
    case usr_gold_transfer:get_gold_transfer_gids_by_transaction_id_and_transaction_type({TransactionId, TransactionType}) of
        [TransferGid] ->
            R0 = usr_gold_transfer:get_one(TransferGid),
            NowDateTime = util:now_datetime_str(),
            R = case Rs of
                    {ok, _, _} ->
                        R0#usr_gold_transfer{
                            status = 1,
                            error_tag = <<>>,
                            update_time = NowDateTime
                        };
                    {error, ?ERRNO_HTTP_REQ_TIMEOUT = ErrNo, ErrMsg} ->
                        R0#usr_gold_transfer{
                            error_tag = ?T2B_P({ErrNo, ErrMsg}),
                            update_time = NowDateTime
                        };
                    {error, ErrNo, ErrMsg} ->
                        R0#usr_gold_transfer{
                            status = -1,
                            error_tag = ?T2B_P({ErrNo, ErrMsg}),
                            update_time = NowDateTime
                        }
                end,
            usr_gold_transfer:set_one(R);
        _ -> void
    end,
    ok.

gen_uuid() ->
    ServerIdBin = integer_to_binary(mod_disperse:server_id()),
    Uuid = uuid_factory:gen(),
    <<ServerIdBin/binary, Uuid/binary>>.

% 锁定成功，返回{true, Cas}
lock(TransactionType, PlayerId) ->
    LockKey = cache_lock_key(TransactionType, PlayerId),
    case cache:get_and_lock(LockKey) of
        false ->
            cache:set(LockKey, <<>>),
            lock(TransactionType, PlayerId);
        {true, Cas, _} ->
            {true, Cas}
    end.

unlock(TransactionType, PlayerId, Cas) ->
    cache:unlock(cache_lock_key(TransactionType, PlayerId), Cas).

cache_lock_key(TransactionType, PlayerId) ->
    list_to_binary(io_lib:format(<<"lock_usr_gold_transfer_~p_~p">>, [TransactionType, PlayerId])).

