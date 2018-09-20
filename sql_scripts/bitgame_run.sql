-- MySQL dump 10.13  Distrib 5.6.37, for linux-glibc2.12 (x86_64)
--
-- Host: localhost    Database: 
-- ------------------------------------------------------
-- Server version   5.6.37-log

SET NAMES utf8;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
--  Table structure for `role`
-- ----------------------------
DROP TABLE IF EXISTS `role`;
CREATE TABLE `role` (
  `player_id` int(11) unsigned NOT NULL COMMENT '玩家id（用户id）',
  `ver` int(11) NOT NULL DEFAULT '0' COMMENT '数据结构版本',
  `game_id` int(11) NOT NULL DEFAULT '0' COMMENT '游戏id',
  `create_time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '创建时间',
  `last_login_time` int(11) NOT NULL DEFAULT '0' COMMENT '最后登陆时间',
  `last_login_ip` varchar(20) NOT NULL DEFAULT '' COMMENT '最后登陆IP',
  `game_data` varchar(2048) NOT NULL DEFAULT '' COMMENT '游戏数据',
  `old_game_data` varchar(2048) NOT NULL DEFAULT '' COMMENT '老的游戏数据（出错回档用）',
  `power` int(11) NOT NULL DEFAULT '1' COMMENT '原力值',
  `time` int(11) NOT NULL DEFAULT '0' COMMENT '更新时间戳',
  PRIMARY KEY (`player_id`,`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家角色信息';

-- ----------------------------
--  Table structure for `role_gold`
-- ----------------------------
DROP TABLE IF EXISTS `role_gold`;
CREATE TABLE `role_gold` (
  `player_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '用户id（玩家id）',
  `ver` int(11) NOT NULL DEFAULT '0' COMMENT '数据结构版本',
  `game_id` int(11) NOT NULL DEFAULT '0' COMMENT '游戏id',
  `gold` text NOT NULL COMMENT '金币，json格式：{"BGX":数量, "BTC":数量, "ETH":数量, ...}',
  `time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '更新时间戳',
  PRIMARY KEY (`player_id`,`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家金币';

-- ----------------------------
--  Table structure for `role_gold_to_draw`
-- ----------------------------
DROP TABLE IF EXISTS `role_gold_to_draw`;
CREATE TABLE `role_gold_to_draw` (
  `player_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '用户id（玩家id）',
  `ver` int(11) NOT NULL DEFAULT '0' COMMENT '数据结构版本',
  `game_id` int(11) NOT NULL DEFAULT '0' COMMENT '游戏id',
  `gold_list` varchar(20480) NOT NULL DEFAULT '[]' COMMENT 'erlang，待领金币列表，格式：[{时间戳, 币种, 数量}, ...]',
  `time` int(11) NOT NULL DEFAULT '0' COMMENT '更新时间戳',
  PRIMARY KEY (`player_id`,`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家待领金币';


create table role_1 like role;
create table role_2 like role;
create table role_3 like role;
create table role_4 like role;
create table role_5 like role;

create table role_gold_1 like role_gold;
create table role_gold_2 like role_gold;
create table role_gold_3 like role_gold;
create table role_gold_4 like role_gold;
create table role_gold_5 like role_gold;

create table role_gold_to_draw_1 like role_gold_to_draw;
create table role_gold_to_draw_2 like role_gold_to_draw;
create table role_gold_to_draw_3 like role_gold_to_draw;
create table role_gold_to_draw_4 like role_gold_to_draw;
create table role_gold_to_draw_5 like role_gold_to_draw;

SET FOREIGN_KEY_CHECKS = 1;
