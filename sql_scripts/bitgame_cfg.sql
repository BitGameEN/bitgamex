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

 Date: 06/22/2018 20:01:26 PM
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
  `chain_type` varchar(20) NOT NULL DEFAULT '' COMMENT '链类型',
  `amount` double(11,0) NOT NULL DEFAULT '0' COMMENT '总量（如果需要调整产出速率时，产出开始时间也需要修改为新规则生效时间，并结算之前的产出，剩余的量为新的总量）',
  PRIMARY KEY (`gold_type`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='币种定义';

-- ----------------------------
--  Records of `gold_type`
-- ----------------------------
BEGIN;
INSERT INTO `gold_type` VALUES ('ACT', '1529668215', '100.00', '730', 'act', '1000000'), ('BGX', '1529668215', '10000.00', '730', 'eth', '1500000000'), ('BTC', '1529668215', '0.01', '730', 'btc', '1000'), ('ELA', '1529668215', '5.00', '730', 'ela', '100000'), ('ETH', '1529668215', '0.20', '730', 'eth', '10000'), ('MAN', '1529668215', '100.00', '730', 'eth', '1000000'), ('PLY', '1529668215', '10000.00', '730', 'eth', '21000000');
COMMIT;

SET FOREIGN_KEY_CHECKS = 1;
