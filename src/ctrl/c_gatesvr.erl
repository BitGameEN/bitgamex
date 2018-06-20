%%%--------------------------------------
%%% @Module  : c_gatesvr
%%% @Description: gatesvr的逻辑处理模块
%%%--------------------------------------
-module(c_gatesvr).
-export([api_login_game/1,
         api_change_password/1,
         api_bind_user/1,
         api_switch_user/1,
         api_get_game/1,
         api_save_game/1,
         api_transfer_coin_in_game/1,
         api_send_verify_code/1,
         api_bind_exchange_accid/1,
         api_bind_wallet/1,
         api_transfer_coin_to_exchange/1,
         api_transfer_coin_to_wallet/1,
         api_get_coin_list_to_draw/1,
         api_draw_coin/1,
         api_consume_coin/1]).

-include("common.hrl").
-include("record_usr_user.hrl").
-include("record_usr_user_gold.hrl").
-include("record_run_role.hrl").
-include("record_log_player_login.hrl").


%% 登录游戏接口
api_login_game([Uid, GameId, DeviceId, <<>> = UserName, Password, Time, NewGuest, DeviceModel, OsType, OsVer, Lang, OrgDeviceId, GCId, GGId, FBId, PeerIp]) ->
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
                    Ids ->
                        case NewGuest =:= 0 of
                            true ->
                                % 找到在该设备上创建的并最后登录的那个账号
                                lib_user:get_last_login_uid_of_org_device_id(DeviceId);
                            false -> % 如果需要新建一个guest
                                case lib_user:get_unbind_uid_of_org_device_id(DeviceId) of
                                    -1 -> % 都绑定过，则允许创建一个新guest
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
                                    UnbindId -> % 还有未绑定的，则用这个未绑定的账号
                                        UnbindId
                                end
                        end
                end;
            false ->
                Uid
        end,
    User = usr_user:get_one(UserId),
    case Uid > 0 andalso User#usr_user.is_bind =:= 0 andalso DeviceId =/= User#usr_user.org_device_id of
        true -> % 没有任何绑定的情况下，设备id必须一致
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
                player_id = UserId,
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
           exchange_accid => User#usr_user.bind_xchg_accid, wallet_addr => User#usr_user.bind_wallet_addr,
           gc_id => User#usr_user.ios_gamecenter_id, gg_id => User#usr_user.google_id, fb_id => User#usr_user.facebook_id}};
api_login_game([Uid, GameId, DeviceId, UserName, Password, Time, NewGuest, DeviceModel, OsType, OsVer, Lang, OrgDeviceId, GCId, GGId, FBId, PeerIp]) when UserName =/= <<>> ->
    Now = util:unixtime(),
    SessionToken = create_session_token(Uid, GameId, DeviceId),
    % todo: 以后得考虑用手机号一起绑定注册（发验证码）或QQ、微信授权登录（而这实际上相当于获取一个device_id），避免无限建小号
    UserId =
        case Uid =:= 0 of
            true ->
                case usr_user:get_user_gids_by_user_name(UserName) of
                    [] ->
                        case NewGuest =:= 0 of
                            true ->
                                throw({?ERRNO_VERIFY_FAILED, <<"new_guest = 0, user name does not exist">>});
                            false ->
                                ToCreateUser = #usr_user{user_name = UserName,
                                                         password = Password,
                                                         device_id = DeviceId,
                                                         org_device_id = DeviceId,
                                                         lang = Lang,
                                                         os_type = OsType,
                                                         country_code = util:get_country_code(PeerIp),
                                                         create_time = Now,
                                                         time = Now},
                                Id = usr_user:set_one(ToCreateUser),
                                usr_user_gold:set_one(#usr_user_gold{player_id = Id, gold = <<"{}">>, time = Now}),
                                Id
                        end;
                    [Id|_] ->
                        case NewGuest =:= 0 of
                            true ->
                                Id;
                            false ->
                                throw({?ERRNO_VERIFY_FAILED, <<"new_guest = 1, user name does exist">>})
                        end
                end;
            false ->
                Uid
        end,
    User = usr_user:get_one(UserId),
    case User#usr_user.user_name =:= UserName andalso User#usr_user.password =:= Password of
        false ->
            throw({?ERRNO_VERIFY_FAILED, <<"user name and password verify failed">>});
        true ->
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
                player_id = UserId,
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
           exchange_accid => User#usr_user.bind_xchg_accid, wallet_addr => User#usr_user.bind_wallet_addr,
           gc_id => User#usr_user.ios_gamecenter_id, gg_id => User#usr_user.google_id, fb_id => User#usr_user.facebook_id}}.

create_session_token(Uid, GameId, DeviceId) ->
    Time = util:unixtime(),
    Rand = rand:uniform(),
    HardcodeKey = "2A86c02f145425747DD9BCB6c01cdB218d7E4Dba",
    StrCode = util:md5(integer_to_list(Uid) ++ integer_to_list(GameId) ++ binary_to_list(DeviceId) ++
                           integer_to_list(Time) ++ float_to_list(Rand) ++ HardcodeKey),
    list_to_binary(StrCode).

%% 修改密码接口
api_change_password([#usr_user{user_name = TargetUserName, password = TargetPassword} = User, UserName, OldPassword, NewPassword]) ->
    case TargetUserName =:= UserName andalso TargetPassword =:= OldPassword of
        false ->
            throw({-1, <<"user name and password verify failed">>});
        true ->
            case OldPassword =:= NewPassword of
                true ->
                    void;
                false ->
                    usr_user:set_one(User#usr_user{password = NewPassword, time = util:unixtime()})
            end,
            {ok, #{}}
    end.

%% 绑定账号接口
api_bind_user([#usr_user{ios_gamecenter_id = CurrBindVal, current_game_id = GameId} = User, <<"gc_id">> = BindType, BindVal]) ->
    case CurrBindVal of
        <<>> ->
            case usr_user:get_user_gids_by_ios_gamecenter_id(BindVal) of
                [] ->
                    usr_user:set_one(User#usr_user{ios_gamecenter_id = BindVal, is_bind = 1, time = util:unixtime()}),
                    {ok, #{bind_type => BindType, bind_val => BindVal}};
                [OtherUserId|_] ->
                    case run_role:get_one({GameId, OtherUserId}) of
                        [] ->
                            throw({-1, <<"gc_id already bound by others">>});
                        #run_role{create_time = CT, last_login_time = LT, game_data = GameData, power = Power} ->
                            FoundRole = {[{bind_type, BindType}, {bind_val, BindVal}, {uid, OtherUserId},
                                          {create_time, CT}, {login_time, LT}, {power, Power}, {game_data, GameData}]},
                            throw({-2, jiffy:encode(FoundRole)})
                    end
            end;
        _ ->
            case CurrBindVal =:= BindVal of
                true -> {ok, #{bind_type => BindType, bind_val => BindVal}};
                false -> throw({-3, <<"already bound something else">>})
            end
    end;
api_bind_user([#usr_user{google_id = CurrBindVal, current_game_id = GameId} = User, <<"gg_id">> = BindType, BindVal]) ->
    case CurrBindVal of
        <<>> ->
            case usr_user:get_user_gids_by_google_id(BindVal) of
                [] ->
                    usr_user:set_one(User#usr_user{google_id = BindVal, is_bind = 1, time = util:unixtime()}),
                    {ok, #{bind_type => BindType, bind_val => BindVal}};
                [OtherUserId|_] ->
                    case run_role:get_one({GameId, OtherUserId}) of
                        [] ->
                            throw({-1, <<"gg_id already bound by others">>});
                        #run_role{create_time = CT, last_login_time = LT, game_data = GameData, power = Power} ->
                            FoundRole = {[{bind_type, BindType}, {bind_val, BindVal}, {uid, OtherUserId},
                                          {create_time, CT}, {login_time, LT}, {power, Power}, {game_data, GameData}]},
                            throw({-2, jiffy:encode(FoundRole)})
                    end
            end;
        _ ->
            case CurrBindVal =:= BindVal of
                true -> {ok, #{bind_type => BindType, bind_val => BindVal}};
                false -> throw({-3, <<"already bound something else">>})
            end
    end;
api_bind_user([#usr_user{facebook_id = CurrBindVal, current_game_id = GameId} = User, <<"fb_id">> = BindType, BindVal]) ->
    case CurrBindVal of
        <<>> ->
            case usr_user:get_user_gids_by_facebook_id(BindVal) of
                [] ->
                    usr_user:set_one(User#usr_user{facebook_id = BindVal, is_bind = 1, time = util:unixtime()}),
                    {ok, #{bind_type => BindType, bind_val => BindVal}};
                [OtherUserId|_] ->
                    case run_role:get_one({GameId, OtherUserId}) of
                        [] ->
                            throw({-1, <<"fb_id already bound by others">>});
                        #run_role{create_time = CT, last_login_time = LT, game_data = GameData, power = Power} ->
                            FoundRole = {[{bind_type, BindType}, {bind_val, BindVal}, {uid, OtherUserId},
                                          {create_time, CT}, {login_time, LT}, {power, Power}, {game_data, GameData}]},
                            throw({-2, jiffy:encode(FoundRole)})
                    end
            end;
        _ ->
            case CurrBindVal =:= BindVal of
                true -> {ok, #{bind_type => BindType, bind_val => BindVal}};
                false -> throw({-3, <<"already bound something else">>})
            end
    end;
api_bind_user([_User, BindType, _BindVal]) ->
    throw({?ERRNO_WRONG_PARAM, <<"unsupported bind_type: ", BindType/binary>>}).

%% 切换账号接口
api_switch_user([#usr_user{id = CurrUserId, current_game_id = GameId, device_id = DeviceId} = User, BindType, BindVal]) ->
    ToUserId = lib_user:get_bind_uid(BindType, BindVal),
    case ToUserId =:= CurrUserId of
        true ->
            throw({-2, <<"can not switch to current user">>});
        false ->
            case run_role:get_one({GameId, ToUserId}) of
                [] ->
                    throw({-3, <<"no role found for switching user">>});
                _ ->
                    ToUser = usr_user:get_one(ToUserId),
                    Time = util:unixtime(),
                    NewGuest = 0,
                    DeviceModel = <<>>,
                    OsType = <<>>,
                    OsVer = <<>>,
                    Lang = <<>>,
                    OrgDeviceId = ToUser#usr_user.org_device_id,
                    GCId = ToUser#usr_user.ios_gamecenter_id,
                    GGId = ToUser#usr_user.google_id,
                    FBId = ToUser#usr_user.facebook_id,
                    PeerIp = <<>>,
                    api_login_game([ToUserId, GameId, DeviceId, Time, NewGuest, DeviceModel, OsType, OsVer, Lang, OrgDeviceId, GCId, GGId, FBId, PeerIp])
            end
    end.

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

%% 发送验证码
api_send_verify_code([GameId, GameKey, Uid, ExchangeAccId, SendType]) ->
    case lib_rpc:rpc(?SVRTYPE_XCHG, c_centsvr, send_verify_code, [GameId, GameKey, Uid, ExchangeAccId, SendType]) of
        ok -> {ok, #{}};
        Err -> throw(Err)
    end.

%% 绑定 BIT.GAME 交易所账号的接口
api_bind_exchange_accid([#usr_user{current_game_id = GameId, id = UserId} = User, GameKey, ExchangeAccId, VerifyCode]) ->
    case lib_rpc:rpc(?SVRTYPE_XCHG, c_centsvr, bind_exchange_accid, [GameId, GameKey, UserId, ExchangeAccId, VerifyCode]) of
        ok ->
            usr_user:set_one(User#usr_user{bind_xchg_accid = ExchangeAccId, time = util:unixtime()}),
            {ok, #{exchange_accid => ExchangeAccId}};
        Err -> throw(Err)
    end.

%% 绑定以太坊钱包地址的接口
api_bind_wallet([User, WalletAddr]) ->
    usr_user:set_one(User#usr_user{bind_wallet_addr = WalletAddr, time = util:unixtime()}),
    {ok, #{wallet_addr => WalletAddr}}.

%% 转账游戏币给绑定的交易所账号的接口
api_transfer_coin_to_exchange([#usr_user{current_game_id = GameId, id = UserId} = User, GameKey, GoldType, Amount, ReceiptData, VerifyCode]) ->
    {ok, RoleGold, XchgBalance} = lib_rpc:rpc(?SVRTYPE_XCHG, c_centsvr, transfer_gold_to_exchange, [GameId, GameKey, UserId, GoldType, Amount, ReceiptData, VerifyCode]),
    {ok, #{role_balance => RoleGold, exchange_balance => XchgBalance}}.

%% 转账游戏币给绑定的钱包的接口
api_transfer_coin_to_wallet([#usr_user{current_game_id = GameId, id = UserId} = User, GoldType, Amount, WalletAddr, ReceiptData]) ->
    {ok, RoleGold, XchgBalance} = lib_rpc:rpc(?SVRTYPE_XCHG, c_xchgsvr, transfer_gold_to_wallet, [GameId, UserId, GoldType, Amount, WalletAddr, ReceiptData]),
    {ok, #{role_balance => RoleGold, exchange_balance => XchgBalance}}.

%% 获取待领游戏币列表的接口
api_get_coin_list_to_draw([#usr_user{current_game_id = GameId, id = UserId} = User]) ->
    {ok, CoinList} = lib_rpc:rpc(?SVRTYPE_GAME, c_gamesvr, get_coin_list_to_draw, [GameId, UserId]),
    {ok, #{coin_list => CoinList}}.

%% 领取游戏币的接口
api_draw_coin([#usr_user{current_game_id = GameId, id = UserId} = User, CoinId]) ->
    {ok, RoleGold} = lib_rpc:rpc(?SVRTYPE_GAME, c_gamesvr, draw_coin, [GameId, UserId, CoinId]),
    {ok, #{role_balance => RoleGold}}.

%% 消耗游戏币的接口
api_consume_coin([#usr_user{current_game_id = GameId, id = UserId} = User, GoldType, Amount]) ->
    {ok, RoleGold} = lib_rpc:rpc(?SVRTYPE_GAME, c_gamesvr, consume_coin, [GameId, UserId, GoldType, Amount]),
    {ok, #{role_balance => RoleGold}}.

