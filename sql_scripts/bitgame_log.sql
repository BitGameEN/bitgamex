/*
 Navicat Premium Data Transfer

 Source Server         : bitgamex
 Source Server Type    : MySQL
 Source Server Version : 50163
 Source Host           : localhost
 Source Database       : bitgame_log

 Target Server Type    : MySQL
 Target Server Version : 50163
 File Encoding         : utf-8

 Date: 03/11/2018 13:57:36 PM
*/

SET NAMES utf8;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
--  Table structure for `gold`
-- ----------------------------
DROP TABLE IF EXISTS `gold`;
CREATE TABLE `gold` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `game_id` int(11) NOT NULL DEFAULT '0' COMMENT '游戏id',
  `player_id` int(11) NOT NULL DEFAULT '0' COMMENT '玩家id',
  `delta` double(11,0) NOT NULL DEFAULT '0' COMMENT '变化量，负数表示消耗',
  `old_value` double(11,0) NOT NULL DEFAULT '0' COMMENT '旧值',
  `new_value` double(11,0) NOT NULL DEFAULT '0' COMMENT '新值',
  `drain_type` varchar(50) NOT NULL DEFAULT '' COMMENT '来源类型',
  `drain_id` int(11) NOT NULL DEFAULT '0' COMMENT '来源相关id，比如买道具，则为道具id',
  `drain_count` int(11) NOT NULL DEFAULT '0' COMMENT '来源相关id对应的数量，比如买道具，买了多少个',
  `time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '时间戳',
  `call_flow` varchar(256) NOT NULL DEFAULT '' COMMENT '调用上下文',
  PRIMARY KEY (`id`),
  KEY `player_id` (`player_id`),
  KEY `time` (`time`),
  KEY `drain_type` (`drain_type`,`game_id`) USING BTREE,
  KEY `drain_type_and_id` (`drain_type`,`drain_id`,`game_id`) USING BTREE,
  KEY `game_id` (`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='金币记录';

-- ----------------------------
--  Table structure for `player_login`
-- ----------------------------
DROP TABLE IF EXISTS `player_login`;
CREATE TABLE `player_login` (
  `id` int(11) NOT NULL AUTO_INCREMENT COMMENT '自增id',
  `game_id` int(11) DEFAULT '0' COMMENT '游戏id',
  `player_id` int(11) DEFAULT '0' COMMENT '玩家id',
  `device_id` varchar(100) DEFAULT '' COMMENT '设备id',
  `device_model` varchar(32) DEFAULT '' COMMENT '设备型号',
  `os_type` varchar(20) DEFAULT '' COMMENT '操作系统类型',
  `os_ver` varchar(100) DEFAULT '' COMMENT '操作系统版本',
  `ip` varchar(40) DEFAULT '' COMMENT 'ip地址',
  `lang` varchar(50) DEFAULT '' COMMENT '语言',
  `time` int(10) DEFAULT '0' COMMENT '时间戳',
  PRIMARY KEY (`id`),
  KEY `player_id` (`player_id`),
  KEY `time` (`time`),
  KEY `game_id` (`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;

SET FOREIGN_KEY_CHECKS = 1;
