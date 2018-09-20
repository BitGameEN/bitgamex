-- MySQL dump 10.13  Distrib 5.6.37, for linux-glibc2.12 (x86_64)
--
-- Host: localhost    Database: 
-- ------------------------------------------------------
-- Server version   5.6.37-log

SET NAMES utf8;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
--  Table structure for `game`
-- ----------------------------
DROP TABLE IF EXISTS `game`;
CREATE TABLE `game` (
  `game_id` int(11) NOT NULL AUTO_INCREMENT COMMENT '游戏id',
  `game_hashid` varchar(40) NOT NULL DEFAULT '' COMMENT '游戏的哈希id',
  `game_name` varchar(100) NOT NULL DEFAULT '' COMMENT '游戏名',
  `open_status` tinyint(4) NOT NULL DEFAULT '0' COMMENT '游戏状态，0-close, 1-open',
  `game_key` varchar(50) NOT NULL DEFAULT '' COMMENT '游戏固定key，用于登录校验',
  `balance_lua_f` text NOT NULL COMMENT '结算lua脚本函数代码',
  `hard_coef` float NOT NULL DEFAULT '1' COMMENT '难度系数，难度高给分紧的：> 1，难度低给分松的：< 1，其余：= 1',
  `mining_rule` varchar(255) NOT NULL DEFAULT '[]' COMMENT 'erlang, 格式例子：[{''BGX'', 30}, {''BTC'', 10}, {''ETH'', 10}, {''ELA'', 50}]',
  `trusteeship_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT '游戏信用金托管账户游戏方无权使用，给用户提取使用',
  `cp_name` varchar(50) NOT NULL DEFAULT '' COMMENT '开发商名称',
  `cp_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT '开发商支取账号，用户消耗代币行为收取利润账户',
  `ip_list` varchar(160) NOT NULL DEFAULT '' COMMENT 'IP列表',
  `token_symbol_list` varchar(255) NOT NULL DEFAULT '' COMMENT '游戏可交易代币列表',
  `game_type` int(1) NOT NULL DEFAULT '0' COMMENT '游戏类型，0:挖矿,1:不挖矿',
  PRIMARY KEY (`game_id`),
  KEY `open_status` (`open_status`,`game_type`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏';

-- ----------------------------
--  Table structure for `game_package`
-- ----------------------------
DROP TABLE IF EXISTS `game_package`;
CREATE TABLE `game_package` (
  `game_id` int(11) NOT NULL COMMENT '游戏id',
  `package_id` int(11) NOT NULL COMMENT '包id',
  `mining_rule` varchar(255) NOT NULL DEFAULT '[]' COMMENT 'erlang, 格式例子：[{''BGX'', 30}, {''BTC'', 10}, {''ETH'', 10}, {''ELA'', 50}]',
  `mining_pools` varchar(1024) NOT NULL DEFAULT '[]' COMMENT 'erlang, 格式：[{gold_type, mining_start_time, mining_output_first_day, half_life_days, chain_type, amount}]',
  PRIMARY KEY (`game_id`,`package_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏包';

-- ----------------------------
--  Table structure for `game_reclaimed_gold`
-- ----------------------------
DROP TABLE IF EXISTS `game_reclaimed_gold`;
CREATE TABLE `game_reclaimed_gold` (
  `game_id` int(11) NOT NULL COMMENT '游戏id',
  `gold` text NOT NULL COMMENT '游戏回收的总金币数，json格式：{"BGX":数量, "BTC":数量, "ETH":数量, ...}',
  `time` int(11) NOT NULL DEFAULT '0' COMMENT '时间戳',
  PRIMARY KEY (`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏回收金币';

-- ----------------------------
--  Table structure for `global_config`
-- ----------------------------
DROP TABLE IF EXISTS `global_config`;
CREATE TABLE `global_config` (
  `id` int(11) NOT NULL,
  `global_key` varchar(50) DEFAULT NULL,
  `content` varchar(200) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

-- ----------------------------
--  Table structure for `gold_transfer`
-- ----------------------------
DROP TABLE IF EXISTS `gold_transfer`;
CREATE TABLE `gold_transfer` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '唯一id',
  `type` int(11) NOT NULL COMMENT '类型：0 - in game, 1 - game to exchange, 2 - game to wallet, 3 - exchange to game',
  `transaction_type` int(11) NOT NULL COMMENT '类型：0 - in game, 1 - game to exchange, 2 - exchange to game',
  `transaction_id` varchar(200) NOT NULL DEFAULT '' COMMENT '转账事务id',
  `receipt` varchar(20480) NOT NULL DEFAULT '' COMMENT '收据',
  `player_id` int(11) NOT NULL DEFAULT '0' COMMENT '玩家id',
  `device_id` varchar(50) NOT NULL DEFAULT '' COMMENT 'device id',
  `xchg_accid` varchar(100) NOT NULL DEFAULT '' COMMENT '交易所账号id',
  `wallet_addr` varchar(50) NOT NULL DEFAULT '' COMMENT '钱包地址',
  `gold_type` varchar(20) NOT NULL DEFAULT '' COMMENT '币种：BGX, BTC, ETH, ...',
  `gold` double NOT NULL DEFAULT '0' COMMENT '金币',
  `status` int(11) NOT NULL DEFAULT '0' COMMENT '回调状态，0 - 未回调，1 - 已成功回调，-1 - 回调返回失败结果',
  `error_tag` varchar(512) NOT NULL DEFAULT '0' COMMENT '回调未成功时的错误号',
  `receive_game_id` int(11) NOT NULL DEFAULT '0' COMMENT '收到时的游戏id',
  `receive_time` datetime NOT NULL COMMENT '收到时间',
  `update_time` datetime NOT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`),
  UNIQUE KEY `trans_id` (`transaction_type`,`transaction_id`) USING BTREE,
  KEY `type` (`type`),
  KEY `player_id` (`player_id`),
  KEY `wallet_addr` (`wallet_addr`),
  KEY `status` (`status`),
  KEY `update_time` (`update_time`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='转账';

-- ----------------------------
--  Table structure for `server`
-- ----------------------------
DROP TABLE IF EXISTS `server`;
CREATE TABLE `server` (
  `id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT 'id',
  `ip` varchar(50) NOT NULL DEFAULT '' COMMENT 'ip地址',
  `port` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '端口号',
  `node` varchar(50) NOT NULL DEFAULT '' COMMENT '节点',
  `type` tinyint(4) DEFAULT '1' COMMENT '服务器类型：1-gate, 2-game',
  `state` int(11) DEFAULT '0' COMMENT '状态：0-正常，1-无法访问',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='服务器列表';

-- ----------------------------
--  Table structure for `user`
-- ----------------------------
DROP TABLE IF EXISTS `user`;
CREATE TABLE `user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '用户id（玩家id）',
  `hash_id` varchar(40) NOT NULL DEFAULT '' COMMENT '用户哈希id',
  `user_name` varchar(50) NOT NULL DEFAULT '' COMMENT '用户名',
  `password` varchar(50) NOT NULL DEFAULT '' COMMENT '登录密码',
  `player_name` varchar(50) NOT NULL DEFAULT '' COMMENT '玩家名',
  `avatar` tinyint(2) NOT NULL DEFAULT '0' COMMENT '玩家头像',
  `device_id` varchar(200) NOT NULL DEFAULT '' COMMENT '设备id',
  `org_device_id` varchar(200) NOT NULL DEFAULT '' COMMENT '最初的设备id',
  `is_bind` int(1) NOT NULL DEFAULT '0' COMMENT '是否绑定',
  `ios_gamecenter_id` varchar(40) NOT NULL DEFAULT '' COMMENT '绑定苹果id',
  `google_id` varchar(40) NOT NULL DEFAULT '' COMMENT '绑定谷歌id',
  `facebook_id` varchar(40) NOT NULL DEFAULT '' COMMENT '绑定脸书id',
  `current_game_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '当前所在游戏',
  `current_game_package_id` int(11) NOT NULL DEFAULT '0' COMMENT '当前所在游戏包id',
  `current_game_uid` varchar(50) NOT NULL DEFAULT '' COMMENT '当前所在游戏用户标识',
  `session_token` varchar(50) NOT NULL DEFAULT '' COMMENT '会话令牌',
  `lang` varchar(20) NOT NULL DEFAULT '' COMMENT '语言',
  `os_type` varchar(20) NOT NULL DEFAULT '' COMMENT '操作系统类型',
  `country_code` varchar(32) NOT NULL DEFAULT '' COMMENT '国家编码',
  `create_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '创建时间',
  `last_login_time` int(11) NOT NULL DEFAULT '0' COMMENT '上次登录时间',
  `status` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '玩家状态（0-正常，1-封禁）',
  `forbid_login_endtime` int(11) NOT NULL DEFAULT '0' COMMENT '封号截止时间',
  `bind_xchg_accid` varchar(100) NOT NULL DEFAULT '' COMMENT '绑定的交易所账号id',
  `bind_xchg_uid` int(11) NOT NULL DEFAULT '0' COMMENT '绑定的交易所唯一id（bind_xchg_accid可能会变，但该id不变）',
  `bind_wallet_addr` varchar(50) NOT NULL DEFAULT '' COMMENT '绑定的钱包地址',
  `time` int(11) NOT NULL DEFAULT '0' COMMENT '更新时间戳',
  PRIMARY KEY (`id`),
  KEY `current_game_id` (`current_game_id`),
  KEY `is_bind` (`is_bind`,`device_id`),
  KEY `country_code` (`country_code`),
  KEY `create_time` (`create_time`),
  KEY `status` (`status`),
  KEY `org_device_id` (`org_device_id`) USING BTREE,
  KEY `ios_gamecenter_id` (`ios_gamecenter_id`) USING BTREE,
  KEY `google_id` (`google_id`) USING BTREE,
  KEY `facebook_id` (`facebook_id`) USING BTREE,
  KEY `user_name` (`user_name`) USING BTREE,
  KEY `hash_id` (`hash_id`) USING BTREE,
  KEY `current_game_id_pkg_id` (`current_game_id`,`current_game_package_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏中心账户';

-- ----------------------------
--  Table structure for `user_gold`
-- ----------------------------
DROP TABLE IF EXISTS `user_gold`;
CREATE TABLE `user_gold` (
  `player_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '用户id（玩家id）',
  `gold` text NOT NULL COMMENT '金币，json格式：{"BGX":数量, "BTC":数量, "ETH":数量, ...}',
  `time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '更新时间戳',
  PRIMARY KEY (`player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家金币';


DROP TABLE IF EXISTS `exchangeorderinfo`;
CREATE TABLE `exchangeorderinfo` (
  `ID` int(10) NOT NULL AUTO_INCREMENT,
  `OrderNo` varchar(50) NOT NULL COMMENT '用户中心订单号',
  `Uid` int(11) NOT NULL COMMENT '游戏中心uid',
  `ExchangeOrderNo` varchar(200) DEFAULT NULL COMMENT '交易所流水号',
  `GameOrderNo` varchar(50) NOT NULL COMMENT '游戏订单号',
  `ExUserid` int(11) NOT NULL COMMENT '交易所用户ID',
  `ExAccount` varchar(150) DEFAULT NULL COMMENT '交易所账号名',
  `WalletAdd` varchar(50) DEFAULT NULL COMMENT '钱包地址',
  `GameID` int(8) NOT NULL COMMENT '游戏编号',
  `GameUserID` varchar(50) DEFAULT NULL COMMENT '游戏账号唯一标识',
  `Amount` double(36,18) NOT NULL COMMENT '交易代币数量',
  `TokenSymbol` varchar(20) NOT NULL COMMENT '代币标识',
  `StateType` int(1) NOT NULL COMMENT '1创建订单，2处理中，3处理成功，4处理失败',
  `GameOrderData` varchar(255) DEFAULT NULL COMMENT '游戏订单数据可以json格式，原样返回',
  `CreateTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '订单创建时间',
  `CreateOperator` varchar(20) DEFAULT NULL COMMENT '订单创建人',
  `UpdateTime` timestamp NULL DEFAULT NULL COMMENT '订单完成时间',
  `UpdateOperator` varchar(20) DEFAULT NULL COMMENT '订单状态更新人',
  PRIMARY KEY (`ID`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `gamesymbolinfo`;
CREATE TABLE `gamesymbolinfo` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `GameID` int(11) NOT NULL DEFAULT '0',
  `ChainName` varchar(10) NOT NULL DEFAULT '',
  `TokenSymbol` varchar(10) NOT NULL DEFAULT '',
  `BitSymbol` varchar(20) NOT NULL DEFAULT '',
  `IsUse` int(11) NOT NULL DEFAULT '1',
  `CreateTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `CreateOperator` varchar(20) DEFAULT '' COMMENT '创建人',
  `UpdateTime` timestamp NULL DEFAULT NULL COMMENT '修改时间',
  `UpdateOperator` varchar(20) DEFAULT '' COMMENT '修改人',
  PRIMARY KEY (`ID`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `mining_complement_order`;
CREATE TABLE `mining_complement_order` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `orderno` varchar(50) NOT NULL COMMENT '订单号',
  `game_id` int(11) NOT NULL COMMENT '游戏编号',
  `package_id` int(11) NOT NULL COMMENT '包编号',
  `chain_name` varchar(10) NOT NULL COMMENT '主链名',
  `symbol` varchar(10) NOT NULL COMMENT '币名',
  `amount` double(32,16) NOT NULL COMMENT '金额',
  `status` int(1) NOT NULL DEFAULT '1' COMMENT '状态1:未处理,2:处理中,3:已完成',
  `from_id` int(11) DEFAULT NULL COMMENT '矿池ID(交易所总矿池ID或游戏自己矿池账号ID)',
  `to_id` int(11) DEFAULT NULL COMMENT '游戏托管账号ID',
  `start_time` datetime NOT NULL COMMENT '>=汇总开始时间',
  `end_time` datetime NOT NULL COMMENT '<汇总结束时间',
  `mining_count` int(10) NOT NULL COMMENT '汇总订单数',
  `create_time` datetime NOT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '完成时间',
  `type` int(1) DEFAULT NULL COMMENT '数据来源类型，1:游戏中心汇总数据，2:游戏接口汇总数据',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `miningorderinfo`;
CREATE TABLE `miningorderinfo` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `game_id` int(11) NOT NULL COMMENT '游戏编号',
  `package_id` int(11) NOT NULL COMMENT '包编号',
  `chain_name` varchar(20) NOT NULL COMMENT '链名称',
  `symbol` varchar(20) NOT NULL COMMENT '币种',
  `amount` double(36,18) NOT NULL COMMENT '金额',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `miningpoolinfo`;
CREATE TABLE `miningpoolinfo` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增ID',
  `game_id` int(11) NOT NULL COMMENT '指定游戏编号(0:通用设置)',
  `package_id` int(11) NOT NULL COMMENT '游戏包编号',
  `mining_pool_id` int(11) NOT NULL COMMENT '矿池账号ID 交易所总游戏矿池用于挖矿行为转账到游戏托管账户',
  `chain_name` varchar(10) NOT NULL COMMENT '公链名称',
  `symbol` varchar(10) NOT NULL COMMENT '币名',
  `ratio` int(2) DEFAULT NULL COMMENT '挖矿所占比例，同游戏同包的所有币种sum(ratio)中所占的比例',
  `status` int(1) NOT NULL COMMENT '0:off;1:on',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `rechargeorderinfo`;
CREATE TABLE `rechargeorderinfo` (
  `ID` int(10) NOT NULL AUTO_INCREMENT,
  `OrderNo` varchar(50) NOT NULL COMMENT '用户中心订单号',
  `Uid` int(20) NOT NULL COMMENT '游戏中心userid',
  `ExchangeOrderNo` varchar(200) DEFAULT NULL COMMENT '交易所流水号',
  `GameOrderNo` varchar(50) NOT NULL COMMENT '游戏订单号',
  `ExUserid` int(11) NOT NULL COMMENT '交易所用户ID',
  `ExAccount` varchar(150) DEFAULT NULL COMMENT '交易所账号名',
  `WalletAdd` varchar(50) DEFAULT NULL COMMENT '钱包地址',
  `GameID` int(8) NOT NULL COMMENT '游戏编号',
  `GameUserID` varchar(50) DEFAULT NULL COMMENT '游戏账号唯一标识',
  `Amount` double(36,18) NOT NULL COMMENT '交易代币数量',
  `TokenSymbol` varchar(20) NOT NULL COMMENT '代币标识',
  `StateType` int(1) NOT NULL COMMENT '1创建订单，2处理中，3处理成功，4处理失败',
  `GameOrderData` varchar(255) DEFAULT NULL COMMENT '游戏订单数据可以json格式，原样返回',
  `CreateTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '订单创建时间',
  `CreateOperator` varchar(20) DEFAULT NULL COMMENT '订单创建人',
  `UpdateTime` timestamp NULL DEFAULT NULL COMMENT '订单完成时间',
  `UpdateOperator` varchar(20) DEFAULT NULL COMMENT '订单状态修改人',
  PRIMARY KEY (`ID`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `sendcodeinfo`;
CREATE TABLE `sendcodeinfo` (
  `ID` int(11) NOT NULL AUTO_INCREMENT,
  `ExUid` int(11) NOT NULL,
  `ExAccount` varchar(150) NOT NULL DEFAULT '',
  `VerifyCode` varchar(8) NOT NULL,
  `Appid` int(8) NOT NULL,
  `Appuid` varchar(50) DEFAULT NULL,
  `IsUse` int(1) NOT NULL,
  `CreateDate` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `UpdateDate` timestamp NULL DEFAULT NULL,
  `SendType` int(11) NOT NULL COMMENT '发送类型，1绑定账号，2提取到交易所，3充值到游戏',
  `Uid` int(11) DEFAULT NULL,
  PRIMARY KEY (`ID`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `taskjobinfo`;
CREATE TABLE `taskjobinfo` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `task_name` varchar(255) NOT NULL COMMENT '任务名称',
  `status` int(1) NOT NULL COMMENT '任务状态',
  `describe` varchar(255) DEFAULT NULL COMMENT '任务描述',
  `create_time` datetime DEFAULT NULL COMMENT '创建时间',
  `update_time` datetime DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `transaction_fees`;
CREATE TABLE `transaction_fees` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '手续费比例记录表',
  `game_id` int(10) DEFAULT '0' COMMENT '游戏编号',
  `symbol` varchar(20) DEFAULT '' COMMENT '币种',
  `fees` decimal(10,8) DEFAULT NULL COMMENT '费率',
  `type` int(1) DEFAULT '0' COMMENT '1:out(转出余额到交易所),2:in(转入余额到游戏)',
  `is_use` int(1) DEFAULT '1' COMMENT '0:off,1:on',
  `create_time` timestamp NULL DEFAULT NULL COMMENT '创建时间',
  `update_time` timestamp NULL DEFAULT NULL COMMENT '更新时间',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `transactionrule`;
CREATE TABLE `transactionrule` (
  `ID` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增主键',
  `TokenSymbol` varchar(20) NOT NULL COMMENT '代币标识',
  `MinAmount` double(32,18) NOT NULL DEFAULT '0.000000000000000000' COMMENT '交易最小值，0为不限制',
  `MaxAmount` double(32,18) NOT NULL DEFAULT '0.000000000000000000' COMMENT '交易最大值，0为不限制',
  `CreateTime` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP COMMENT '创建时间',
  `CreateOperator` varchar(20) DEFAULT '' COMMENT '创建操作人',
  `UpdateTime` timestamp NULL DEFAULT NULL COMMENT '更新时间',
  `UpdateOperator` varchar(20) DEFAULT '' COMMENT '更新操作人',
  `IsOpen` int(1) NOT NULL DEFAULT '1' COMMENT '开放状态，0-close, 1-open',
  PRIMARY KEY (`ID`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;

DROP TABLE IF EXISTS `useractivation`;
CREATE TABLE `useractivation` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `uid` int(11) NOT NULL DEFAULT '0',
  `gameid` int(11) NOT NULL DEFAULT '0',
  `gameuid` varchar(50) NOT NULL DEFAULT '0',
  `createdate` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `ExAccount` varchar(100) NOT NULL DEFAULT '',
  `ExUid` int(10) NOT NULL DEFAULT '0',
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;


INSERT INTO `game` VALUES (1,'','test',1,'BIT.GAME.X.8.8.8.8','package.path = package.path .. \";../priv/?.lua;\"\njson = require \"json\"\n\nfunction f(s0, s)\n  t = json.decode(s)\n  return t[\"score\"] * 0.1\nend',1,'[{\'BGX\',50},{\'ELA\',50}]',10067,'',0,'127.0.0.1',',act-act,eth-bgx,btc-btc,eth-eth,ela-ela,eth-man,',0),(2,'','pok',1,'3c579320371c4b12b9492fc75451ec80','',1,'[]',10067,'',0,'',',eth-pok,',1),(3,'','ela_little_game',1,'BIT.GAME.X.8.8.8.8','package.path = package.path .. \";../priv/?.lua;\"\njson = require \"json\"\n\nfunction f(s0, s)\n  t = json.decode(s)\n  return t[\"score\"] * 0.1\nend',1,'[{\'ELA\', 100}]',10067,'',0,'127.0.0.1',',ela-ela,',0),(4,'','ply_little_game',1,'BIT.GAME.X.8.8.8.8','package.path = package.path .. \";../priv/?.lua;\"\njson = require \"json\"\n\nfunction f(s0, s)\n  t = json.decode(s)\n  return t[\"score\"] * 0.1\nend',1,'[{\'PLY\', 100}]',10067,'',0,'127.0.0.1',',eth-ply,',0),(5,'','slg_game',1,'BIT.GAME.X.8.8.8.8','package.path = package.path .. \";../priv/?.lua;\"\njson = require \"json\"\n\nfunction f(s0, s)\n  t = json.decode(s)\n  return t[\"score\"] * 0.1\nend',1,'[{\'ELA\', 100}]',10067,'',0,'127.0.0.1',',ela-ela,eth-bgx,eth-eth,act-act,',0),(6,'','FishChain',1,'BIT.GAME.X.8.8.8.8','',1,'[]',10059,'',0,'',',eth-bgx,ela-ela,',3);
INSERT INTO `game_package` VALUES (5,0,'[{\'BGX\', 100}]','[{\'BGX\', 1529668215, 10000.00, 730, eth, 100000000}]'),(5,1,'[{\'ETH\', 100}]','[{\'ETH\', 1529668215, 0.20, 730, eth, 1000}]'),(5,2,'[{\'ELA\', 100}]','[{\'ELA\', 1529668215, 1.00, 730, ela, 10000}]'),(5,3,'[{\'ACT\', 100}]','[{\'ACT\', 1529668215, 100.00, 730, act, 100000}]');
INSERT INTO `gamesymbolinfo` VALUES (1,1,'eth','bgx','bgx',1,'2018-05-03 06:19:29','System',NULL,''),(2,2,'eth','pok','pok',1,'2018-05-02 12:04:33','System',NULL,''),(3,5,'eth','bgx','bgx',1,'2018-05-03 06:19:29','System',NULL,''),(4,1,'ela','ela','ela',1,'2018-05-03 06:19:29','System',NULL,''),(5,6,'eth','bgx','bgx',1,'2018-09-17 12:38:13','System',NULL,''),(6,6,'ela','ela','ela',1,'2018-09-17 12:38:13','System',NULL,'');
INSERT INTO `global_config` VALUES (1,'transfer_discount_in_game','0.002'),(2,'transfer_discount_to_xchg','0.002'),(3,'gold_proportion_for_login','0.3'),(4,'gold_proportion_for_random','0.1'),(5,'gold_proportion_for_game','0.6');
INSERT INTO `miningpoolinfo` VALUES (1,0,0,10059,'eth','bgx',1,1,NULL,NULL),(2,0,0,10059,'eth','eth',1,1,NULL,NULL),(3,1,0,10001,'eth','eth',1,1,NULL,NULL),(4,0,0,10059,'act','act',1,1,NULL,NULL),(5,0,0,10059,'btc','btc',1,1,NULL,NULL),(6,0,0,10059,'ela','ela',1,1,NULL,NULL),(7,0,0,10059,'eth','man',1,1,NULL,NULL),(8,0,0,10059,'eth','ply',1,1,NULL,NULL);
INSERT INTO `sendcodeinfo` VALUES (1,10009,'','b3hd50s8',2,'testpokaccount',0,'2018-05-16 11:28:07',NULL,1,0),(2,10009,'','5i2fewrm',2,'testpokaccount',0,'2018-05-16 11:29:23',NULL,1,0),(3,10009,'','01fq8hch',2,'testpokaccount',0,'2018-05-16 11:31:13',NULL,1,0),(4,10002,'','x2fcrc6p',2,'testpokaccount',0,'2018-05-17 10:03:48',NULL,1,0),(5,10002,'','85gw2jdh',2,'testpokaccount',0,'2018-05-17 10:04:34',NULL,1,0),(6,10002,'','dfu8bg23',2,'testpokaccount',0,'2018-05-17 10:07:55',NULL,1,0),(7,10002,'','431427',2,'testpokaccount',0,'2018-05-17 10:08:25',NULL,1,0),(8,10002,'','891417',2,'testpokaccount',0,'2018-05-17 10:13:31',NULL,1,0),(9,10009,'','369734',2,'testpokaccount',1,'2018-05-18 04:01:09',NULL,1,0),(10,10003,'','906890',2,'30',0,'2018-05-21 03:41:45',NULL,1,0),(11,10003,'','5qvq0gcu',2,'30',0,'2018-05-21 03:45:37',NULL,1,0),(12,10003,'','454ptyh7',2,'30',0,'2018-05-21 03:46:41',NULL,1,0),(13,10003,'','643730',2,'30',1,'2018-05-21 05:27:01',NULL,1,0),(14,10003,'','761132',2,'30',1,'2018-05-21 08:16:39',NULL,1,0),(15,10003,'','220611',2,'30',1,'2018-05-21 08:41:51',NULL,4,36),(16,10003,'','611569',2,'30',1,'2018-05-21 08:59:21',NULL,1,0),(17,10003,'','826019',2,'30',1,'2018-05-21 09:12:15',NULL,2,36),(18,10003,'','025441',2,'30',1,'2018-05-21 09:31:09',NULL,2,36),(19,10003,'','192403',2,'30',1,'2018-05-21 09:39:57',NULL,3,36),(20,10009,'','635774',2,'testpokaccount',1,'2018-05-22 08:27:56',NULL,1,0),(21,10009,'','577159',2,'testpokaccount',1,'2018-05-23 05:50:25',NULL,1,0),(22,10009,'','730822',2,'testpokaccount',0,'2018-05-23 06:40:09',NULL,1,0),(23,10009,'','250390',2,'13777777777',1,'2018-05-23 06:46:28',NULL,1,0),(24,10009,'','607296',2,'13333333334',1,'2018-05-23 06:55:01',NULL,1,0),(25,10009,'','861476',2,'13333333334',1,'2018-05-23 06:57:16',NULL,1,0),(26,10009,'','979851',2,'13333333334',0,'2018-05-23 07:18:58',NULL,1,0),(27,10009,'','211256',2,'13333333334',1,'2018-05-23 07:21:02',NULL,2,75),(28,180402,'','564563',1,'72',0,'2018-06-20 03:53:07',NULL,1,72),(29,180402,'','867843',1,'72',0,'2018-06-20 09:38:28',NULL,1,72),(30,180402,'','258948',1,'72',0,'2018-06-20 09:43:02',NULL,1,72),(31,180402,'','965085',1,'72',1,'2018-06-20 09:54:19',NULL,2,72);
INSERT INTO `taskjobinfo` VALUES (1,'挖矿转账任务',1,'游戏中心挖矿转账任务，每5分钟执行一次，调用交易所转账接口','2018-07-18 18:40:00','2018-07-18 18:40:05'),(2,'挖矿数据汇总任务',1,'将接收到的挖矿数据每个小时汇总一次等待提交接口调用','2018-07-23 00:00:00','2018-07-23 00:00:00');
INSERT INTO `transaction_fees` VALUES (1,0,'',0.00500000,1,1,'2018-08-26 16:00:00','2018-08-26 16:00:00'),(2,0,'',0.00000000,2,1,'2018-08-26 16:00:00','2018-08-26 16:00:00');
INSERT INTO `transactionrule` VALUES (1,'bgx',0.001000000000000000,10000.000000000000000000,'2018-04-28 12:41:54','System',NULL,'',1),(2,'pok',0.001000000000000000,10000.000000000000000000,'2018-04-28 12:41:54','System',NULL,'',1),(3,'ela',0.001000000000000000,10000.000000000000000000,'2018-09-17 12:38:16','System',NULL,'',1);


SET FOREIGN_KEY_CHECKS = 1;
