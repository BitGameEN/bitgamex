/*
 Navicat Premium Data Transfer

 Source Server         : bitgamex
 Source Server Type    : MySQL
 Source Server Version : 50163
 Source Host           : localhost
 Source Database       : bitgame_usr

 Target Server Type    : MySQL
 Target Server Version : 50163
 File Encoding         : utf-8

 Date: 04/01/2018 09:11:30 AM
*/

SET NAMES utf8;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
--  Table structure for `game`
-- ----------------------------
DROP TABLE IF EXISTS `game`;
CREATE TABLE `game` (
  `game_id` int(11) NOT NULL AUTO_INCREMENT COMMENT '游戏id',
  `game_name` varchar(100) NOT NULL DEFAULT '' COMMENT '游戏名',
  `open_status` tinyint(4) NOT NULL DEFAULT '0' COMMENT '游戏状态，0-close, 1-open',
  `game_key` varchar(50) NOT NULL DEFAULT '' COMMENT '游戏固定key，用于登录校验',
  `balance_lua_f` text NOT NULL COMMENT '结算lua脚本函数代码',
  `hard_coef` float NOT NULL DEFAULT '1' COMMENT '难度系数，难度高给分紧的：> 1，难度低给分松的：< 1，其余：= 1',
  `trusteeship_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT '游戏信用金托管账户游戏方无权使用，给用户提取使用',
  `cp_name` varchar(50) NOT NULL DEFAULT '' COMMENT '开发商名称',
  `cp_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT '开发商支取账号，用户消耗代币行为收取利润账户',
  `ip_list` varchar(160) NOT NULL DEFAULT '' COMMENT 'IP列表',
  PRIMARY KEY (`game_id`),
  KEY `open_status` (`open_status`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏';

-- ----------------------------
--  Table structure for `game_reclaimed_gold`
-- ----------------------------
DROP TABLE IF EXISTS `game_reclaimed_gold`;
CREATE TABLE `game_reclaimed_gold` (
  `game_id` int(11) NOT NULL AUTO_INCREMENT COMMENT '游戏id',
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
  `player_name` varchar(50) NOT NULL DEFAULT '' COMMENT '玩家名',
  `avatar` tinyint(2) NOT NULL DEFAULT '0' COMMENT '玩家头像',
  `device_id` varchar(200) NOT NULL DEFAULT '' COMMENT '设备id',
  `org_device_id` varchar(200) NOT NULL DEFAULT '' COMMENT '最初的设备id',
  `is_bind` int(1) NOT NULL DEFAULT '0' COMMENT '是否绑定',
  `ios_gamecenter_id` varchar(40) NOT NULL DEFAULT '' COMMENT '绑定苹果id',
  `google_id` varchar(40) NOT NULL DEFAULT '' COMMENT '绑定谷歌id',
  `facebook_id` varchar(40) NOT NULL DEFAULT '' COMMENT '绑定脸书id',
  `current_game_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '当前所在游戏',
  `current_game_uid` varchar(50) NOT NULL DEFAULT '' COMMENT '当前所在游戏用户标识',
  `session_token` varchar(50) NOT NULL DEFAULT '' COMMENT '会话令牌',
  `lang` varchar(20) NOT NULL DEFAULT '' COMMENT '语言',
  `os_type` varchar(20) NOT NULL DEFAULT '' COMMENT '操作系统类型',
  `country_code` varchar(32) NOT NULL COMMENT '国家编码',
  `create_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '创建时间',
  `last_login_time` int(11) NOT NULL DEFAULT '0' COMMENT '上次登录时间',
  `status` tinyint(1) unsigned NOT NULL DEFAULT '0' COMMENT '玩家状态（0-正常，1-封禁）',
  `forbid_login_endtime` int(11) NOT NULL DEFAULT '0' COMMENT '封号截止时间',
  `bind_xchg_accid` varchar(100) NOT NULL DEFAULT '' COMMENT '绑定的交易所账号id',
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
  KEY `facebook_id` (`facebook_id`) USING BTREE
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

SET FOREIGN_KEY_CHECKS = 1;
