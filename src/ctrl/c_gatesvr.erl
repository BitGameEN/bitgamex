%%%--------------------------------------
%%% @Module  : c_gatesvr
%%% @Description: gatesvr的逻辑处理模块
%%%--------------------------------------
-module(c_gatesvr).
-export([api_login_game/1,
         api_bind_user/1,
         api_get_game/1,
         api_save_game/1,
         api_transfer_coin_in_game/1,
         api_bind_exchange_accid/1,
         api_bind_wallet/1,
         api_transfer_coin_to_exchange/1,
         api_transfer_coin_to_wallet/1]).

-include("common.hrl").
-include("record_usr_user.hrl").
-include("record_usr_user_gold.hrl").
-include("record_log_player_login.hrl").


%% 登录游戏接口
api_login_game([Uid, GameId, DeviceId, Time, DeviceModel, OsType, OsVer, Lang, OrgDeviceId, GCId, GGId, FBId, PeerIp]) ->
    Now = util:unixtime(),
    SessionToken = create_session_token(Uid, GameId, DeviceId),
    UserId =
        case Uid =:= 0 of
            true ->
                case usr_user:get_user_gids_by_org_device_id(DeviceId) of
                    [] ->
                        ToCreateUser = #usr_user{device_id = DeviceId,
                                                 org_device_id = DeviceId,
                                                 lang = Lang,
                                                 os_type = OsType,
                                                 country_code = util:get_country_code(PeerIp),
                                                 create_time = Now,
                                                 time = Now},
                        Id = usr_user:set_one(ToCreateUser),
                        usr_user_gold:set_one(#usr_user_gold{player_id = Id, gold = <<"{}">>, time = Now}),
                        Id;
                    [Id|_] ->
                        Id
                end;
            false ->
                Uid
        end,
    User = usr_user:get_one(UserId),
    case Uid > 0 andalso DeviceId =/= User#usr_user.org_device_id of
        true ->
            throw({?ERRNO_VERIFY_FAILED, <<"device_id verify failed">>});
        false ->
            void
    end,
    case (OrgDeviceId =/= <<>> andalso OrgDeviceId =/= User#usr_user.org_device_id) orelse
             (GCId =/= <<>> andalso GCId =/= User#usr_user.ios_gamecenter_id) orelse
             (GGId =/= <<>> andalso GGId =/= User#usr_user.google_id) orelse
             (FBId =/= <<>> andalso FBId =/= User#usr_user.facebook_id) of
        true ->
            throw({?ERRNO_VERIFY_FAILED, <<"org_device_id or ios_gamecenter_id or google_id or facebook_id verify failed">>});
        false ->
            void
    end,
    ToUpdateUser = User#usr_user{device_id = DeviceId,
                                 current_game_id = GameId,
                                 session_token = SessionToken,
                                 last_login_time = Now,
                                 time = Now},
    usr_user:set_one(ToUpdateUser),
    LogR = #log_player_login{
                game_id = GameId,
                player_id = Uid,
                device_id = DeviceId,
                device_model = util:esc(util:device_model(DeviceModel)),
                os_type = OsType,
                os_ver = OsVer,
                ip = ?T2B(PeerIp),
                lang = Lang,
                time = Now
            },
    log_player_login:set_one(LogR),
    {ok, GameData, UserBalance, RoleBalance} = lib_rpc:rpc(?SVRTYPE_GAME, c_gamesvr, get_game_data, [GameId, UserId, true, [Now, PeerIp]]),
    {ok, #{uid => UserId, token => SessionToken, game_data => GameData, user_balance => UserBalance, role_balance => RoleBalance,
           exchange_accid => User#usr_user.bind_xchg_accid, wallet_addr => User#usr_user.bind_wallet_addr}}.

create_session_token(Uid, GameId, DeviceId) ->
    Time = util:unixtime(),
    Rand = rand:uniform(),
    HardcodeKey = "2A86c02f145425747DD9BCB6c01cdB218d7E4Dba",
    StrCode = util:md5(integer_to_list(Uid) ++ integer_to_list(GameId) ++ binary_to_list(DeviceId) ++
                           integer_to_list(Time) ++ float_to_list(Rand) ++ HardcodeKey),
    list_to_binary(StrCode).

%% 绑定账号接口
api_bind_user([#usr_user{ios_gamecenter_id = CurrBindVal} = User, <<"gc_id">>, BindVal]) ->
    case CurrBindVal of
        <<>> ->
            case usr_user:get_user_gids_by_ios_gamecenter_id(BindVal) of
                [] ->
                    usr_user:set_one(User#usr_user{ios_gamecenter_id = BindVal, is_bind = 1, time = util:unixtime()}),
                    {ok, #{}};
                _ ->
                    throw({-1, <<"gc_id already bound by others">>})
            end;
        _ ->
            case CurrBindVal =:= BindVal of
                true -> {ok, #{}};
                false -> throw({-2, <<"already bound something else">>})
            end
    end;
api_bind_user([#usr_user{google_id = CurrBindVal} = User, <<"gg_id">>, BindVal]) ->
    case CurrBindVal of
        <<>> ->
            case usr_user:get_user_gids_by_google_id(BindVal) of
                [] ->
                    usr_user:set_one(User#usr_user{google_id = BindVal, is_bind = 1, time = util:unixtime()}),
                    {ok, #{}};
                _ ->
                    throw({-1, <<"gg_id already bound by others">>})
            end;
        _ ->
            case CurrBindVal =:= BindVal of
                true -> {ok, #{}};
                false -> throw({-2, <<"already bound something else">>})
            end
    end;
api_bind_user([#usr_user{facebook_id = CurrBindVal} = User, <<"fb_id">>, BindVal]) ->
    case CurrBindVal of
        <<>> ->
            case usr_user:get_user_gids_by_facebook_id(BindVal) of
                [] ->
                    usr_user:set_one(User#usr_user{facebook_id = BindVal, is_bind = 1, time = util:unixtime()}),
                    {ok, #{}};
                _ ->
                    throw({-1, <<"fb_id already bound by others">>})
            end;
        _ ->
            case CurrBindVal =:= BindVal of
                true -> {ok, #{}};
                false -> throw({-2, <<"already bound something else">>})
            end
    end;
api_bind_user([_User, BindType, _BindVal]) ->
    throw({?ERRNO_WRONG_PARAM, <<"unsupported bind_type: ", BindType/binary>>}).

%% 存盘数据获取接口
api_get_game([#usr_user{current_game_id = GameId, id = UserId} = User]) ->
    {ok, GameData, UserBalance, RoleBalance} = lib_rpc:rpc(?SVRTYPE_GAME, c_gamesvr, get_game_data, [GameId, UserId, false, []]),
    {ok, #{game_data => GameData, user_balance => UserBalance, role_balance => RoleBalance}}.

%% 存盘接口
api_save_game([#usr_user{current_game_id = GameId, id = UserId} = User, GameData]) ->
    {ok, GameData, Balance, FRes} = lib_rpc:rpc(?SVRTYPE_GAME, c_gamesvr, save_game_data, [GameId, UserId, GameData]),
    {ok, #{game_data => GameData, role_balance => Balance, f_res => FRes}}.

%% 转账游戏币给其他玩家的接口
api_transfer_coin_in_game([#usr_user{current_game_id = GameId, id = UserId} = User, #usr_user{id = DstUserId} = DstUser, GoldType, Amount, ReceiptData]) ->
    {ok, RoleGold} = lib_rpc:rpc(?SVRTYPE_GAME, c_gamesvr, transfer_coin_in_game, [GameId, UserId, DstUserId, GoldType, Amount, ReceiptData]),
    {ok, #{role_balance => RoleGold}}.

%% 绑定 BIT.GAME 交易所账号的接口
api_bind_exchange_accid([User, ExchangeAccId]) ->
    usr_user:set_one(User#usr_user{bind_xchg_accid = ExchangeAccId, time = util:unixtime()}),
    {ok, #{exchange_accid => ExchangeAccId}}.

%% 绑定以太坊钱包地址的接口
api_bind_wallet([User, WalletAddr]) ->
    usr_user:set_one(User#usr_user{bind_wallet_addr = WalletAddr, time = util:unixtime()}),
    {ok, #{wallet_addr => WalletAddr}}.

%% 转账游戏币给绑定的交易所账号的接口
api_transfer_coin_to_exchange([#usr_user{current_game_id = GameId, id = UserId} = User, GoldType, Amount, ReceiptData]) ->
    {ok, RoleGold, XchgBalance} = lib_rpc:rpc(?SVRTYPE_XCHG, c_xchgsvr, transfer_gold_to_exchange, [GameId, UserId, GoldType, Amount, ReceiptData]),
    {ok, #{role_balance => RoleGold, exchange_balance => XchgBalance}}.

%% 转账游戏币给绑定的钱包的接口
api_transfer_coin_to_wallet([#usr_user{current_game_id = GameId, id = UserId} = User, GoldType, Amount, ReceiptData]) ->
    {ok, RoleGold, XchgBalance} = lib_rpc:rpc(?SVRTYPE_XCHG, c_xchgsvr, transfer_gold_to_wallet, [GameId, UserId, GoldType, Amount, ReceiptData]),
    {ok, #{role_balance => RoleGold, exchange_balance => XchgBalance}}.

