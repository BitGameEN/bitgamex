%%%------------------------------------------------
%%% File    : record.hrl
%%% Description: 公共结构定义
%%%------------------------------------------------

%% 服务器结构
-record(server, {
    id,
    ip,
    port,
    node,
    type
}).

-record(add_balane_req, {
    uid = 0,
    game_id = 0,
    delta_balance = 0,
    time = 0
}).