/*
 Navicat Premium Data Transfer

 Source Server         : bitgamex
 Source Server Type    : MySQL
 Source Server Version : 50163
 Source Host           : localhost
 Source Database       : bitgame_cfg

 Target Server Type    : MySQL
 Target Server Version : 50163
 File Encoding         : utf-8

 Date: 04/05/2018 11:41:26 AM
*/

SET NAMES utf8;
SET FOREIGN_KEY_CHECKS = 0;

-- ----------------------------
--  Table structure for `gold_type`
-- ----------------------------
DROP TABLE IF EXISTS `gold_type`;
CREATE TABLE `gold_type` (
  `gold_type` varchar(20) NOT NULL COMMENT '币种：BGX, BTC, ETH, ...',
  `mining_start_time` int(11) NOT NULL DEFAULT '0' COMMENT '挖矿起始时间戳',
  `mining_output_first_day` double(11,2) NOT NULL DEFAULT '0.00' COMMENT '挖矿第一天产出量',
  `half_life_days` int(11) NOT NULL DEFAULT '0' COMMENT '半衰期，多少天产出减半',
  PRIMARY KEY (`gold_type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='币种定义';

-- ----------------------------
--  Records of `gold_type`
-- ----------------------------
BEGIN;
INSERT INTO `gold_type` VALUES ('BGX', '1525017600', '1027397.26', '730');
COMMIT;

SET FOREIGN_KEY_CHECKS = 1;
