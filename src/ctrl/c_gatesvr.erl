%%%--------------------------------------
%%% @Module  : c_gatesvr
%%% @Description: gatesvr的逻辑处理模块
%%%--------------------------------------
-module(c_gatesvr).
-export([api_login_game/1,
         api_bind_user/1]).

-include("common.hrl").
-include("record_usr_user.hrl").
-include("record_log_player_login.hrl").

% 登录游戏
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
    % todo:
    GameData = <<>>, Balance = 0,
    {ok, #{uid => UserId, token => SessionToken, game_data => GameData, balance => Balance}}.

create_session_token(Uid, GameId, DeviceId) ->
    Time = util:unixtime(),
    Rand = rand:uniform(),
    HardcodeKey = "2A86c02f145425747DD9BCB6c01cdB218d7E4Dba",
    StrCode = util:md5(integer_to_list(Uid) ++ integer_to_list(GameId) ++ binary_to_list(DeviceId) ++
                           integer_to_list(Time) ++ float_to_list(Rand) ++ HardcodeKey),
    list_to_binary(StrCode).

% 绑定账号
api_bind_user([#usr_user{ios_gamecenter_id = CurrBindVal} = User, <<"gc_id">>, BindVal]) ->
    case CurrBindVal of
        <<>> ->
            case usr_user:get_user_gids_by_ios_gamecenter_id(BindVal) of
                [] ->
                    usr_user:set_one(User#usr_user{ios_gamecenter_id = BindVal, is_bind = 1, time = util:unixtime()}),
                    {ok, #{}};
                _ ->
                    throw({-1, <<"gc_id already bound by other">>})
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
                    throw({-1, <<"gg_id already bound by other">>})
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
                    throw({-1, <<"fb_id already bound by other">>})
            end;
        _ ->
            case CurrBindVal =:= BindVal of
                true -> {ok, #{}};
                false -> throw({-2, <<"already bound something else">>})
            end
    end;
api_bind_user([_User, BindType, _BindVal]) ->
    throw({?ERRNO_WRONG_PARAM, <<"unsupported bind_type: ", BindType/binary>>}).

