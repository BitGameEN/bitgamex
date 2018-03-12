/*
 Navicat Premium Data Transfer

 Source Server         : bitgamex
 Source Server Type    : MySQL
 Source Server Version : 50163
 Source Host           : localhost
 Source Database       : bitgame_run

 Target Server Type    : MySQL
 Target Server Version : 50163
 File Encoding         : utf-8

 Date: 03/08/2018 20:56:57 PM
*/

SET NAMES utf8;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
--  Table structure for `role`
-- ----------------------------
DROP TABLE IF EXISTS `role`;
CREATE TABLE `role` (
  `player_id` int(11) unsigned NOT NULL COMMENT '玩家id（用户id）',
  `ver` int(11) NOT NULL DEFAULT '0' COMMENT '数据结构版本',
  `create_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '创建时间',
  `last_login_time` int(11) NOT NULL DEFAULT '0' COMMENT '最后登陆时间',
  `last_login_ip` varchar(20) NOT NULL DEFAULT '' COMMENT '最后登陆IP',
  `game_data` varchar(2048) NOT NULL DEFAULT '' COMMENT '游戏数据',
  `old_game_data` varchar(2048) NOT NULL DEFAULT '' COMMENT '老的游戏数据（出错回档用）',
  `time` int(11) NOT NULL DEFAULT '0' COMMENT '更新时间戳',
  PRIMARY KEY (`player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家角色信息';

SET FOREIGN_KEY_CHECKS = 1;
