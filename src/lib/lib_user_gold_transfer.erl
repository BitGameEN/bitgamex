%%%--------------------------------------
%%% @Module  : lib_user_gold_transfer
%%% @Description: 用户金币转账相关处理
%%%--------------------------------------
-module(lib_user_gold_transfer).
-export([update_transfer_log/3]).

-include("common.hrl").
-include("record_usr_gold_transfer.hrl").


update_transfer_log(TransactionType, TransactionId, Rs) ->
    case usr_gold_transfer:get_gold_transfer_gids_by_transaction_id_and_transaction_type({TransactionId, TransactionType}) of
        [TransferGid] ->
            R0 = usr_gold_transfer:get_one(TransferGid),
            NowDateTime = util:now_datetime_str(),
            R = case Rs of
                    {ok, Gold} ->
                        R0#usr_gold_transfer{
                            status = 1,
                            error_tag = <<>>,
                            gold = Gold,
                            update_time = NowDateTime
                        };
                    {error, ErrTag} ->
                        R0#usr_gold_transfer{
                            status = 0,
                            error_tag = ErrTag,
                            update_time = NowDateTime
                        }
                end,
            usr_gold_transfer:set_one(R);
        _ -> void
    end,
    ok.

