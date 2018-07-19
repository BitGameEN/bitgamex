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

-record(add_gold_req, {
    uid = 0,
    game_id = 0,
    game_pkg_id = 0,
    gold_type = <<>>,
    delta_gold = 0,
    time = 0
}).