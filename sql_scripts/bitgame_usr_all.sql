-- MySQL dump 10.13  Distrib 5.1.73, for redhat-linux-gnu (x86_64)
--
-- Host: 127.0.0.1    Database: bitgame_usr
-- ------------------------------------------------------
-- Server version	5.1.73

/*!40101 SET @OLD_CHARACTER_SET_CLIENT=@@CHARACTER_SET_CLIENT */;
/*!40101 SET @OLD_CHARACTER_SET_RESULTS=@@CHARACTER_SET_RESULTS */;
/*!40101 SET @OLD_COLLATION_CONNECTION=@@COLLATION_CONNECTION */;
/*!40101 SET NAMES utf8 */;
/*!40103 SET @OLD_TIME_ZONE=@@TIME_ZONE */;
/*!40103 SET TIME_ZONE='+00:00' */;
/*!40014 SET @OLD_UNIQUE_CHECKS=@@UNIQUE_CHECKS, UNIQUE_CHECKS=0 */;
/*!40014 SET @OLD_FOREIGN_KEY_CHECKS=@@FOREIGN_KEY_CHECKS, FOREIGN_KEY_CHECKS=0 */;
/*!40101 SET @OLD_SQL_MODE=@@SQL_MODE, SQL_MODE='NO_AUTO_VALUE_ON_ZERO' */;
/*!40111 SET @OLD_SQL_NOTES=@@SQL_NOTES, SQL_NOTES=0 */;

--
-- Table structure for table `exchangeorderinfo`
--

DROP TABLE IF EXISTS `exchangeorderinfo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `exchangeorderinfo`
--

LOCK TABLES `exchangeorderinfo` WRITE;
/*!40000 ALTER TABLE `exchangeorderinfo` DISABLE KEYS */;
/*!40000 ALTER TABLE `exchangeorderinfo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `game`
--

DROP TABLE IF EXISTS `game`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game` (
  `game_id` int(11) NOT NULL AUTO_INCREMENT COMMENT '游戏id',
  `game_name` varchar(100) NOT NULL DEFAULT '' COMMENT '游戏名',
  `open_status` tinyint(4) NOT NULL DEFAULT '0' COMMENT '游戏状态，0-close, 1-open',
  `game_key` varchar(50) NOT NULL DEFAULT '' COMMENT '游戏固定key，用于登录校验',
  `balance_lua_f` text NOT NULL COMMENT '结算lua脚本函数代码',
  `hard_coef` float NOT NULL DEFAULT '1' COMMENT '难度系数，难度高给分紧的：> 1，难度低给分松的：< 1，其余：= 1',
<<<<<<< HEAD
  `mining_rule` varchar(255) NOT NULL DEFAULT '[]' COMMENT 'erlang, æ ¼å¼ä¾‹å­ï¼š[{''BGX'', 30}, {''BTC'', 10}, {''ETH'', 10}, {''ELA'', 50}]',
  `trusteeship_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT 'æ¸¸æˆä¿¡ç”¨é‡‘æ‰˜ç®¡è´¦æˆ·æ¸¸æˆæ–¹æ— æƒä½¿ç”¨ï¼Œç»™ç”¨æˆ·æå–ä½¿ç”¨',
  `cp_name` varchar(50) NOT NULL DEFAULT '' COMMENT 'å¼€å‘å•†åç§°',
  `cp_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT 'å¼€å‘å•†æ”¯å–è´¦å·ï¼Œç”¨æˆ·æ¶ˆè€—ä»£å¸è¡Œä¸ºæ”¶å–åˆ©æ¶¦è´¦æˆ·',
  `ip_list` varchar(160) NOT NULL DEFAULT '' COMMENT 'IPåˆ—è¡¨',
  `token_symbol_list` varchar(255) NOT NULL DEFAULT '' COMMENT 'æ¸¸æˆå¯äº¤æ˜“ä»£å¸åˆ—è¡¨',
  `game_type` int(1) NOT NULL DEFAULT '0' COMMENT 'æ¸¸æˆç±»åž‹ï¼Œ0:æŒ–çŸ¿,1:ä¸æŒ–çŸ¿',
=======
  `trusteeship_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT '游戏信用金托管账户游戏方无权使用，给用户提取使用',
  `cp_name` varchar(50) NOT NULL DEFAULT '' COMMENT '开发商名称',
  `cp_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT '开发商支取账号，用户消耗代币行为收取利润账户',
  `ip_list` varchar(160) NOT NULL DEFAULT '' COMMENT 'IP列表',
  `token_symbol_list` varchar(255) NOT NULL DEFAULT '' COMMENT '游戏可交易代币列表',
  `game_type` int(1) NOT NULL DEFAULT '0' COMMENT '游戏类型，0:挖矿,1:不挖矿',
>>>>>>> d8221be68dcebb5a2d32465e9adacc2c2c5e9893
  PRIMARY KEY (`game_id`),
  KEY `open_status` (`open_status`,`game_type`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COMMENT='游戏';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `game`
--

LOCK TABLES `game` WRITE;
/*!40000 ALTER TABLE `game` DISABLE KEYS */;
INSERT INTO `game` (`game_id`, `game_name`, `open_status`, `game_key`, `balance_lua_f`, `hard_coef`, `mining_rule`, `trusteeship_exuserid`, `cp_name`, `cp_exuserid`, `ip_list`, `token_symbol_list`, `game_type`) VALUES (1,'test',1,'BIT.GAME.X.8.8.8.8','package.path = package.path .. \";../priv/?.lua;\"\njson = require \"json\"\n\nfunction f(s0, s)\n  t = json.decode(s)\n  return t[\"score\"] * 0.1\nend',1,'[{\'BGX\', 30}, {\'BTC\', 10}, {\'ETH\', 10}, {\'ELA\', 50}]',0,'',0,'127.0.0.1',',eth-bgx,btc-btc,eth-eth,ela-ela,',0),(2,'pok',1,'3c579320371c4b12b9492fc75451ec80','',1,'[]',10001,'',0,'',',eth-pok,',1);
/*!40000 ALTER TABLE `game` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `game_reclaimed_gold`
--

DROP TABLE IF EXISTS `game_reclaimed_gold`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `game_reclaimed_gold` (
  `game_id` int(11) NOT NULL AUTO_INCREMENT COMMENT '游戏id',
  `gold` text NOT NULL COMMENT '游戏回收的总金币数，json格式：{"BGX":数量, "BTC":数量, "ETH":数量, ...}',
  `time` int(11) NOT NULL DEFAULT '0' COMMENT '时间戳',
  PRIMARY KEY (`game_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏回收金币';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `gamesymbolinfo`
--

DROP TABLE IF EXISTS `gamesymbolinfo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `gamesymbolinfo`
--

LOCK TABLES `gamesymbolinfo` WRITE;
/*!40000 ALTER TABLE `gamesymbolinfo` DISABLE KEYS */;
INSERT INTO `gamesymbolinfo` (`ID`, `GameID`, `ChainName`, `TokenSymbol`, `BitSymbol`, `IsUse`, `CreateTime`, `CreateOperator`, `UpdateTime`, `UpdateOperator`) VALUES (1,1,'eth','bgx','bgx',1,'2018-05-03 06:19:29','System',NULL,''),(2,2,'eth','pok','pok',1,'2018-05-02 12:04:33','System',NULL,'');
/*!40000 ALTER TABLE `gamesymbolinfo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `global_config`
--

DROP TABLE IF EXISTS `global_config`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `global_config` (
  `id` int(11) NOT NULL,
  `global_key` varchar(50) DEFAULT NULL,
  `content` varchar(200) DEFAULT NULL,
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `global_config`
--

LOCK TABLES `global_config` WRITE;
/*!40000 ALTER TABLE `global_config` DISABLE KEYS */;
INSERT INTO `global_config` (`id`, `global_key`, `content`) VALUES (1,'transfer_discount_in_game','0.002'),(2,'transfer_discount_to_xchg','0.002'),(3,'gold_proportion_for_login','0.3');
/*!40000 ALTER TABLE `global_config` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `gold_transfer`
--

DROP TABLE IF EXISTS `gold_transfer`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `rechargeorderinfo`
--

DROP TABLE IF EXISTS `rechargeorderinfo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `rechargeorderinfo`
--

LOCK TABLES `rechargeorderinfo` WRITE;
/*!40000 ALTER TABLE `rechargeorderinfo` DISABLE KEYS */;
/*!40000 ALTER TABLE `rechargeorderinfo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `sendcodeinfo`
--

DROP TABLE IF EXISTS `sendcodeinfo`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `sendcodeinfo`
--

LOCK TABLES `sendcodeinfo` WRITE;
/*!40000 ALTER TABLE `sendcodeinfo` DISABLE KEYS */;
/*!40000 ALTER TABLE `sendcodeinfo` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `server`
--

DROP TABLE IF EXISTS `server`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `server` (
  `id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT 'id',
  `ip` varchar(50) NOT NULL DEFAULT '' COMMENT 'ip地址',
  `port` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '端口号',
  `node` varchar(50) NOT NULL DEFAULT '' COMMENT '节点',
  `type` tinyint(4) DEFAULT '1' COMMENT '服务器类型：1-gate, 2-game',
  `state` int(11) DEFAULT '0' COMMENT '状态：0-正常，1-无法访问',
  PRIMARY KEY (`id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='服务器列表';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `server`
--

LOCK TABLES `server` WRITE;
/*!40000 ALTER TABLE `server` DISABLE KEYS */;
INSERT INTO `server` (`id`, `ip`, `port`, `node`, `type`, `state`) VALUES (10000,'127.0.0.1',8800,'gate10000@127.0.0.1',1,0),(20000,'127.0.0.1',0,'game20000@127.0.0.1',2,0),(30000,'127.0.0.1',0,'xchg30000@127.0.0.1',3,0);
/*!40000 ALTER TABLE `server` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `transactionrule`
--

DROP TABLE IF EXISTS `transactionrule`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
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
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `transactionrule`
--

LOCK TABLES `transactionrule` WRITE;
/*!40000 ALTER TABLE `transactionrule` DISABLE KEYS */;
INSERT INTO `transactionrule` (`ID`, `TokenSymbol`, `MinAmount`, `MaxAmount`, `CreateTime`, `CreateOperator`, `UpdateTime`, `UpdateOperator`, `IsOpen`) VALUES (1,'bgx',1.000000000000000000,20.000000000000000000,'2018-04-28 12:41:54','System',NULL,'',1);
/*!40000 ALTER TABLE `transactionrule` ENABLE KEYS */;
UNLOCK TABLES;

--
-- Table structure for table `user`
--

DROP TABLE IF EXISTS `user`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user` (
  `id` int(11) unsigned NOT NULL AUTO_INCREMENT COMMENT '用户id（玩家id）',
  `user_name` varchar(50) NOT NULL DEFAULT '' COMMENT 'ç”¨æˆ·å',
  `password` varchar(50) NOT NULL DEFAULT '' COMMENT 'ç™»å½•å¯†ç ',
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
  KEY `ios_gamecenter_id` (`ios_gamecenter_id`),
  KEY `google_id` (`google_id`),
  KEY `facebook_id` (`facebook_id`),
  KEY `country_code` (`country_code`),
  KEY `create_time` (`create_time`),
  KEY `status` (`status`),
  KEY `org_device_id` (`org_device_id`) USING BTREE,
  KEY `user_name` (`user_name`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏中心账户';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `user_gold`
--

DROP TABLE IF EXISTS `user_gold`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `user_gold` (
  `player_id` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '用户id（玩家id）',
  `gold` text NOT NULL COMMENT '金币，json格式：{"BGX":数量, "BTC":数量, "ETH":数量, ...}',
  `time` int(11) unsigned NOT NULL DEFAULT '0' COMMENT '更新时间戳',
  PRIMARY KEY (`player_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='玩家金币';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Table structure for table `useractivation`
--

DROP TABLE IF EXISTS `useractivation`;
/*!40101 SET @saved_cs_client     = @@character_set_client */;
/*!40101 SET character_set_client = utf8 */;
CREATE TABLE `useractivation` (
  `id` int(11) NOT NULL AUTO_INCREMENT,
  `uid` int(11) NOT NULL DEFAULT '0',
  `gameid` int(11) NOT NULL DEFAULT '0',
  `gameuid` varchar(50) NOT NULL DEFAULT '0',
  `createdate` timestamp NOT NULL DEFAULT CURRENT_TIMESTAMP,
  PRIMARY KEY (`id`) USING BTREE
) ENGINE=InnoDB DEFAULT CHARSET=utf8 ROW_FORMAT=DYNAMIC;
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `useractivation`
--

LOCK TABLES `useractivation` WRITE;
/*!40000 ALTER TABLE `useractivation` DISABLE KEYS */;
/*!40000 ALTER TABLE `useractivation` ENABLE KEYS */;
UNLOCK TABLES;
/*!40103 SET TIME_ZONE=@OLD_TIME_ZONE */;

/*!40101 SET SQL_MODE=@OLD_SQL_MODE */;
/*!40014 SET FOREIGN_KEY_CHECKS=@OLD_FOREIGN_KEY_CHECKS */;
/*!40014 SET UNIQUE_CHECKS=@OLD_UNIQUE_CHECKS */;
/*!40101 SET CHARACTER_SET_CLIENT=@OLD_CHARACTER_SET_CLIENT */;
/*!40101 SET CHARACTER_SET_RESULTS=@OLD_CHARACTER_SET_RESULTS */;
/*!40101 SET COLLATION_CONNECTION=@OLD_COLLATION_CONNECTION */;
/*!40111 SET SQL_NOTES=@OLD_SQL_NOTES */;

-- Dump completed on 2018-05-04 20:08:54
