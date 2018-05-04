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
  `trusteeship_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT 'æ¸¸æˆä¿¡ç”¨é‡‘æ‰˜ç®¡è´¦æˆ·æ¸¸æˆæ–¹æ— æƒä½¿ç”¨ï¼Œç»™ç”¨æˆ·æå–ä½¿ç”¨',
  `cp_name` varchar(50) NOT NULL DEFAULT '' COMMENT 'å¼€å‘å•†åç§°',
  `cp_exuserid` int(11) NOT NULL DEFAULT '0' COMMENT 'å¼€å‘å•†æ”¯å–è´¦å·ï¼Œç”¨æˆ·æ¶ˆè€—ä»£å¸è¡Œä¸ºæ”¶å–åˆ©æ¶¦è´¦æˆ·',
  `ip_list` varchar(160) NOT NULL DEFAULT '' COMMENT 'IPåˆ—è¡¨',
  `token_symbol_list` varchar(255) NOT NULL DEFAULT '' COMMENT 'æ¸¸æˆå¯äº¤æ˜“ä»£å¸åˆ—è¡¨',
  `game_type` int(1) NOT NULL DEFAULT '0' COMMENT 'æ¸¸æˆç±»åž‹ï¼Œ0:æŒ–çŸ¿,1:ä¸æŒ–çŸ¿',
  PRIMARY KEY (`game_id`),
  KEY `open_status` (`open_status`,`game_type`) USING BTREE
) ENGINE=InnoDB AUTO_INCREMENT=3 DEFAULT CHARSET=utf8 COMMENT='游戏';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `game`
--

LOCK TABLES `game` WRITE;
/*!40000 ALTER TABLE `game` DISABLE KEYS */;
INSERT INTO `game` (`game_id`, `game_name`, `open_status`, `game_key`, `balance_lua_f`, `hard_coef`, `trusteeship_exuserid`, `cp_name`, `cp_exuserid`, `ip_list`, `token_symbol_list`, `game_type`) VALUES (1,'test',1,'BIT.GAME.X.8.8.8.8','package.path = package.path .. \";../priv/?.lua;\"\njson = require \"json\"\n\nfunction f(s0, s)\n  t = json.decode(s)\n  return t[\"score\"] * 0.1\nend',1,0,'',0,'127.0.0.1',',eth-bgx,btc-btc,eth-eth,ela-ela,',0),(2,'pok',1,'3c579320371c4b12b9492fc75451ec80','',1,10001,'',0,'',',eth-pok,',1);
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
) ENGINE=InnoDB AUTO_INCREMENT=2 DEFAULT CHARSET=utf8 COMMENT='游戏回收金币';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `game_reclaimed_gold`
--

LOCK TABLES `game_reclaimed_gold` WRITE;
/*!40000 ALTER TABLE `game_reclaimed_gold` DISABLE KEYS */;
INSERT INTO `game_reclaimed_gold` (`game_id`, `gold`, `time`) VALUES (1,'{\"BGX\":452.56510000000014}',1525403251);
/*!40000 ALTER TABLE `game_reclaimed_gold` ENABLE KEYS */;
UNLOCK TABLES;

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
) ENGINE=InnoDB AUTO_INCREMENT=175 DEFAULT CHARSET=utf8 COMMENT='转账';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `gold_transfer`
--

LOCK TABLES `gold_transfer` WRITE;
/*!40000 ALTER TABLE `gold_transfer` DISABLE KEYS */;
INSERT INTO `gold_transfer` (`id`, `type`, `transaction_type`, `transaction_id`, `receipt`, `player_id`, `device_id`, `xchg_accid`, `wallet_addr`, `gold_type`, `gold`, `status`, `error_tag`, `receive_game_id`, `receive_time`, `update_time`) VALUES (1,0,0,'1_2_1523008496544','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008496&sign=d9f8ebee5bf60c71be81066ef37d2b6d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:54:56','2018-04-06 09:54:56'),(2,0,0,'1_2_1523008515895','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008515&sign=a80129ba280c6a11914f871d93f6042a&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:15','2018-04-06 09:55:15'),(3,0,0,'1_2_1523008517723','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008517&sign=f838f99337f36930d0d5452da2068560&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:17','2018-04-06 09:55:17'),(4,0,0,'1_2_1523008521824','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008521&sign=5813fdbdc7b99ce5cad072929ac2c2c8&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:21','2018-04-06 09:55:21'),(5,0,0,'1_2_1523008523206','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008522&sign=e3b52b19db7c957692efb3961fd7db1f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:23','2018-04-06 09:55:23'),(6,0,0,'1_2_1523008523975','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008523&sign=b9c51b653e9f72a55687898b5e98c78f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:23','2018-04-06 09:55:23'),(7,0,0,'1_2_1523008525482','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008524&sign=826a359762632c3d50a818731035d942&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:25','2018-04-06 09:55:25'),(8,0,0,'1_2_1523008526371','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008525&sign=a88cf56056221a6409bf19aeefcd03f2&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:26','2018-04-06 09:55:26'),(9,0,0,'1_2_1523008527069','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008526&sign=f406bd48e9ba358c8ec3fae1c8bd5a49&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:27','2018-04-06 09:55:27'),(10,0,0,'1_2_1523008527898','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008527&sign=32cd03f3dd002b70e1fedc854b41e33f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:27','2018-04-06 09:55:27'),(11,0,0,'1_2_1523008528850','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008528&sign=eb8976f30876c5924cbb193608125f70&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:28','2018-04-06 09:55:28'),(12,0,0,'1_2_1523008529152','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008528&sign=eb8976f30876c5924cbb193608125f70&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:29','2018-04-06 09:55:29'),(13,0,0,'1_2_1523008530000','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008529&sign=a636cb74b9fb50c675b6dcaba4348c6d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:30','2018-04-06 09:55:30'),(14,0,0,'1_2_1523008530251','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008529&sign=a636cb74b9fb50c675b6dcaba4348c6d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:30','2018-04-06 09:55:30'),(15,0,0,'1_2_1523008531375','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008531&sign=6d3272d4ceabe903f7ba4e23ee884028&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:31','2018-04-06 09:55:31'),(16,0,0,'1_2_1523008532839','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008532&sign=c699132f6e05bf6315e562fed64a1b29&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:32','2018-04-06 09:55:32'),(17,0,0,'1_2_1523008534250','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008533&sign=750e309eab67afdbc75c8d6f648d952c&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:34','2018-04-06 09:55:34'),(18,0,0,'1_2_1523008535845','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008535&sign=6e74b972582945be2dd12b636e1c039d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:35','2018-04-06 09:55:35'),(19,0,0,'1_2_1523008536001','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008535&sign=6e74b972582945be2dd12b636e1c039d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:36','2018-04-06 09:55:36'),(20,0,0,'1_2_1523008536252','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008536&sign=d394a50dedaa4b666ee55df08c38f157&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:36','2018-04-06 09:55:36'),(21,0,0,'1_2_1523008536474','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008536&sign=d394a50dedaa4b666ee55df08c38f157&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:36','2018-04-06 09:55:36'),(22,0,0,'1_2_1523008536660','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008536&sign=d394a50dedaa4b666ee55df08c38f157&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:36','2018-04-06 09:55:36'),(23,0,0,'1_2_1523008536818','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008536&sign=d394a50dedaa4b666ee55df08c38f157&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:36','2018-04-06 09:55:36'),(24,0,0,'1_2_1523008536978','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008536&sign=d394a50dedaa4b666ee55df08c38f157&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:36','2018-04-06 09:55:36'),(25,0,0,'1_2_1523008537173','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008536&sign=d394a50dedaa4b666ee55df08c38f157&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:37','2018-04-06 09:55:37'),(26,0,0,'1_2_1523008537278','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008537&sign=153fd24bd8749e3f16c0eb258954991b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:37','2018-04-06 09:55:37'),(27,0,0,'1_2_1523008537449','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008537&sign=153fd24bd8749e3f16c0eb258954991b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:37','2018-04-06 09:55:37'),(28,0,0,'1_2_1523008537568','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008537&sign=153fd24bd8749e3f16c0eb258954991b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:37','2018-04-06 09:55:37'),(29,0,0,'1_2_1523008538064','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008537&sign=153fd24bd8749e3f16c0eb258954991b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:38','2018-04-06 09:55:38'),(30,0,0,'1_2_1523008538107','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008537&sign=153fd24bd8749e3f16c0eb258954991b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:38','2018-04-06 09:55:38'),(31,0,0,'1_2_1523008538196','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008537&sign=153fd24bd8749e3f16c0eb258954991b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:38','2018-04-06 09:55:38'),(32,0,0,'1_2_1523008538365','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008538&sign=e5c0c2814b41e323fd1e47698493cc9f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:38','2018-04-06 09:55:38'),(33,0,0,'1_2_1523008538550','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008538&sign=e5c0c2814b41e323fd1e47698493cc9f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:38','2018-04-06 09:55:38'),(34,0,0,'1_2_1523008538672','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008538&sign=e5c0c2814b41e323fd1e47698493cc9f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:38','2018-04-06 09:55:38'),(35,0,0,'1_2_1523008538841','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008538&sign=e5c0c2814b41e323fd1e47698493cc9f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:38','2018-04-06 09:55:38'),(36,0,0,'1_2_1523008539026','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008538&sign=e5c0c2814b41e323fd1e47698493cc9f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:39','2018-04-06 09:55:39'),(37,0,0,'1_2_1523008539200','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008538&sign=e5c0c2814b41e323fd1e47698493cc9f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:39','2018-04-06 09:55:39'),(38,0,0,'1_2_1523008539335','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008539&sign=a6dc9eddd05f37366267656f1561905e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:39','2018-04-06 09:55:39'),(39,0,0,'1_2_1523008539481','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008539&sign=a6dc9eddd05f37366267656f1561905e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:39','2018-04-06 09:55:39'),(40,0,0,'1_2_1523008539663','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008539&sign=a6dc9eddd05f37366267656f1561905e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:39','2018-04-06 09:55:39'),(41,0,0,'1_2_1523008539844','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008539&sign=a6dc9eddd05f37366267656f1561905e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:39','2018-04-06 09:55:39'),(42,0,0,'1_2_1523008540027','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008539&sign=a6dc9eddd05f37366267656f1561905e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:40','2018-04-06 09:55:40'),(43,0,0,'1_2_1523008540560','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008540&sign=fdd6eb26dbc0ced2470d51dfc41422aa&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:40','2018-04-06 09:55:40'),(44,0,0,'1_2_1523008540612','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008540&sign=fdd6eb26dbc0ced2470d51dfc41422aa&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:40','2018-04-06 09:55:40'),(45,0,0,'1_2_1523008540701','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008540&sign=fdd6eb26dbc0ced2470d51dfc41422aa&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:40','2018-04-06 09:55:40'),(46,0,0,'1_2_1523008540856','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008540&sign=fdd6eb26dbc0ced2470d51dfc41422aa&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:40','2018-04-06 09:55:40'),(47,0,0,'1_2_1523008541034','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008540&sign=fdd6eb26dbc0ced2470d51dfc41422aa&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:41','2018-04-06 09:55:41'),(48,0,0,'1_2_1523008541200','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008540&sign=fdd6eb26dbc0ced2470d51dfc41422aa&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:41','2018-04-06 09:55:41'),(49,0,0,'1_2_1523008541439','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008541&sign=5e67b03418d2ab3861d6e55242a1da30&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:41','2018-04-06 09:55:41'),(50,0,0,'1_2_1523008541780','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008541&sign=5e67b03418d2ab3861d6e55242a1da30&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:41','2018-04-06 09:55:41'),(51,0,0,'1_2_1523008541981','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008541&sign=5e67b03418d2ab3861d6e55242a1da30&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:41','2018-04-06 09:55:41'),(52,0,0,'1_2_1523008542126','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008541&sign=5e67b03418d2ab3861d6e55242a1da30&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:42','2018-04-06 09:55:42'),(53,0,0,'1_2_1523008542440','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008542&sign=353bd52ba3d7831316e721eb1df6883b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:42','2018-04-06 09:55:42'),(54,0,0,'1_2_1523008542614','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008542&sign=353bd52ba3d7831316e721eb1df6883b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:42','2018-04-06 09:55:42'),(55,0,0,'1_2_1523008542756','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008542&sign=353bd52ba3d7831316e721eb1df6883b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:42','2018-04-06 09:55:42'),(56,0,0,'1_2_1523008543066','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008542&sign=353bd52ba3d7831316e721eb1df6883b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:43','2018-04-06 09:55:43'),(57,0,0,'1_2_1523008543200','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008542&sign=353bd52ba3d7831316e721eb1df6883b&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:43','2018-04-06 09:55:43'),(58,0,0,'1_2_1523008543269','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008543&sign=edab8ad94c4af86175d6c4e4ed1cbced&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:43','2018-04-06 09:55:43'),(59,0,0,'1_2_1523008543425','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008543&sign=edab8ad94c4af86175d6c4e4ed1cbced&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:43','2018-04-06 09:55:43'),(60,0,0,'1_2_1523008543581','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008543&sign=edab8ad94c4af86175d6c4e4ed1cbced&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:43','2018-04-06 09:55:43'),(61,0,0,'1_2_1523008543715','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008543&sign=edab8ad94c4af86175d6c4e4ed1cbced&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:43','2018-04-06 09:55:43'),(62,0,0,'1_2_1523008543886','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008543&sign=edab8ad94c4af86175d6c4e4ed1cbced&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:55:43','2018-04-06 09:55:43'),(63,0,0,'1_2_1523008699593','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008699&sign=5ac5b9be1677121d8d8afd44bd041974&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:19','2018-04-06 09:58:19'),(64,0,0,'1_2_1523008700461','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008700&sign=1cd7e85eabbb5ecab5387999140cebdd&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:20','2018-04-06 09:58:20'),(65,0,0,'1_2_1523008701134','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008700&sign=1cd7e85eabbb5ecab5387999140cebdd&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:21','2018-04-06 09:58:21'),(66,0,0,'1_2_1523008701631','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008701&sign=ffe6c1cd33a7705b36bcb704f285ac5d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:21','2018-04-06 09:58:21'),(67,0,0,'1_2_1523008701776','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008701&sign=ffe6c1cd33a7705b36bcb704f285ac5d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:21','2018-04-06 09:58:21'),(68,0,0,'1_2_1523008701933','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008701&sign=ffe6c1cd33a7705b36bcb704f285ac5d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:21','2018-04-06 09:58:21'),(69,0,0,'1_2_1523008702091','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008701&sign=ffe6c1cd33a7705b36bcb704f285ac5d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:22','2018-04-06 09:58:22'),(70,0,0,'1_2_1523008702416','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008702&sign=9ed3330546f2c2a7e587240b0398573d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:22','2018-04-06 09:58:22'),(71,0,0,'1_2_1523008702560','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008702&sign=9ed3330546f2c2a7e587240b0398573d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:22','2018-04-06 09:58:22'),(72,0,0,'1_2_1523008702727','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008702&sign=9ed3330546f2c2a7e587240b0398573d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:22','2018-04-06 09:58:22'),(73,0,0,'1_2_1523008702890','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008702&sign=9ed3330546f2c2a7e587240b0398573d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:22','2018-04-06 09:58:22'),(74,0,0,'1_2_1523008703045','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=f30cb1498e53d74b5a8fe1ef6c556abf&dst_uid=2&coin_type=BGX&amount=0.5&time=1523008702&sign=9ed3330546f2c2a7e587240b0398573d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-06 09:58:23','2018-04-06 09:58:23'),(75,0,0,'1_2_1523008762580','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=78349f8cf26fcdbf522271f06daff789&dst_uid=2&coin_type=BGX&amount=100.5&time=1523008762&sign=d102d8516ba99df794e5dd2c0f5b6636&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',100.5,1,'',1,'2018-04-06 09:59:22','2018-04-06 09:59:22'),(76,2,1,'300005AC7454923AE6176C5000001','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=78349f8cf26fcdbf522271f06daff789&coin_type=BGX&amount=0.5&time=1523008840&sign=3d1dc5f888ba153f12885b01495411a8&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','12345678','BGX',0.5,0,'',1,'2018-04-06 10:00:41','2018-04-06 10:00:41'),(77,1,1,'300005AC7455F23AE6176C5000002','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=78349f8cf26fcdbf522271f06daff789&coin_type=BGX&amount=1.5&time=1523008863&sign=b19449e279297a9d95ab251afe65fce8&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','','BGX',1.5,0,'',1,'2018-04-06 10:01:03','2018-04-06 10:01:03'),(78,1,1,'300005AC7456D23AE6176C5000003','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=78349f8cf26fcdbf522271f06daff789&coin_type=BGX&amount=1.5&time=1523008877&sign=9eec79060ea375e0eccce3aeba7ef8f3&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','','BGX',1.5,0,'',1,'2018-04-06 10:01:17','2018-04-06 10:01:17'),(79,0,0,'1_2_1523009522593','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=e1f0be0d732aa0e820ba97f98c6850d4&dst_uid=2&coin_type=BGX&amount=100&time=1523009522&sign=64b8c8b80b002bfb39b9e49bd56d1b57&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',100,1,'',1,'2018-04-06 10:12:02','2018-04-06 10:12:02'),(80,0,0,'1_2_1523258120721','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=91fff9cebff8d6b7580e5492843cd06d&dst_uid=2&coin_type=BGX&amount=0.5&time=1523258120&sign=f817f461759b8701d9950e89f2523dc0&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-09 07:15:20','2018-04-09 07:15:20'),(81,0,0,'1_2_1523258123321','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=91fff9cebff8d6b7580e5492843cd06d&dst_uid=2&coin_type=BGX&amount=0.5&time=1523258123&sign=74bf6cf8cee4d63ffca6238b69d4a162&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-09 07:15:23','2018-04-09 07:15:23'),(82,0,0,'1_2_1523258124254','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=91fff9cebff8d6b7580e5492843cd06d&dst_uid=2&coin_type=BGX&amount=0.5&time=1523258124&sign=cda28e1d6218f713197bdab7c984999a&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-09 07:15:24','2018-04-09 07:15:24'),(83,0,0,'1_2_1523258124907','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=91fff9cebff8d6b7580e5492843cd06d&dst_uid=2&coin_type=BGX&amount=0.5&time=1523258124&sign=cda28e1d6218f713197bdab7c984999a&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-09 07:15:24','2018-04-09 07:15:24'),(84,2,1,'300005ACB131523AE612540000001','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=91fff9cebff8d6b7580e5492843cd06d&coin_type=BGX&amount=0.5&time=1523258133&sign=77ff1c5b13fdff2358d03124a4d59d34&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','12345678','BGX',0.5,0,'',1,'2018-04-09 07:15:33','2018-04-09 07:15:33'),(85,1,1,'300005ACB131923AE612540000002','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=91fff9cebff8d6b7580e5492843cd06d&coin_type=BGX&amount=0.5&time=1523258137&sign=61f2639bc444136e4986cbb4189bf91a&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','','BGX',0.5,0,'',1,'2018-04-09 07:15:37','2018-04-09 07:15:37'),(86,0,0,'1_2_1523268296646','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=762169de9f5502035a329cceee33c473&dst_uid=2&coin_type=BGX&amount=0.5&time=1523268296&sign=9c612e4756d62de94f530d9ba8c22d96&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-09 10:04:56','2018-04-09 10:04:56'),(87,2,1,'300005ACF74BF23AE612540000003','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&wallet_addr=&time=1523545278&sign=27d22b442d8d9e974201b964352fa0cf&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','123456','BGX',0.5,1,'',1,'2018-04-12 15:01:19','2018-04-12 15:01:19'),(88,2,1,'300005ACF74C123AE612540000004','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&wallet_addr=&time=1523545280&sign=5bfefd37af712bda63b7ab24d1c3317f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','123456','BGX',0.5,1,'',1,'2018-04-12 15:01:21','2018-04-12 15:01:21'),(89,2,1,'300005ACF74C223AE612540000005','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&wallet_addr=&time=1523545281&sign=4b1cf7c04166065cb738c03ed4b80398&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','123456','BGX',0.5,1,'',1,'2018-04-12 15:01:22','2018-04-12 15:01:22'),(90,1,1,'300005ACF74C723AE612540000006','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&time=1523545286&sign=d65444dce23d9f05d04c3232e2abdf83&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-12 15:01:27','2018-04-12 15:01:27'),(91,1,1,'300005ACF74C723AE612540000007','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&time=1523545286&sign=d65444dce23d9f05d04c3232e2abdf83&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-12 15:01:27','2018-04-12 15:01:27'),(92,1,1,'300005ACF74C823AE612540000008','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&time=1523545287&sign=55ee15e2f232ca3d6ba49ed88280849e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-12 15:01:28','2018-04-12 15:01:28'),(93,1,1,'300005ACF74C923AE612540000009','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&time=1523545288&sign=b1d634ba78af602d1c5b7e1c6b248852&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-12 15:01:29','2018-04-12 15:01:29'),(94,1,1,'300005ACF74CA23AE61254000000A','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&time=1523545289&sign=083ade2df6188d6114f9c6bc66995f9c&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-12 15:01:30','2018-04-12 15:01:30'),(95,1,1,'300005ACF74CB23AE61254000000B','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&time=1523545289&sign=083ade2df6188d6114f9c6bc66995f9c&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-12 15:01:31','2018-04-12 15:01:31'),(96,1,1,'300005ACF74CB23AE61254000000C','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=12ba11aeff006d1ca9993185063b6093&coin_type=BGX&amount=0.5&time=1523545290&sign=269f14f078c8205eb504f0fd779fc97a&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-12 15:01:31','2018-04-12 15:01:31'),(97,2,1,'300005AD0452223AE61254000000D','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=2d7f26e1d6114d8c65655306eff6b77a&coin_type=BGX&amount=0.5&wallet_addr=&time=1523598626&sign=d238809859ac514b5b61ce0a0c6a4de7&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','123456','BGX',0.5,1,'',1,'2018-04-13 05:50:26','2018-04-13 05:50:26'),(98,1,1,'300005AD0452723AE61254000000E','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=2d7f26e1d6114d8c65655306eff6b77a&coin_type=BGX&amount=0.5&time=1523598631&sign=8c5feb3d4ecf5dce414dd784ee528fec&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-13 05:50:31','2018-04-13 05:50:31'),(99,0,0,'1_2_1523778397053','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=8493cbd80dd5e071ec3b438e41860b83&dst_uid=2&coin_type=BGX&amount=0.5&time=1523778397&sign=0803edf14ed89e9d2cb23dbb4761a58e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-15 07:46:37','2018-04-15 07:46:37'),(100,0,0,'1_2_1523802116136','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=64c898a9d10626fb27ae5a00a3bf30f8&dst_uid=2&coin_type=BGX&amount=10.5&time=1523802116&sign=9f8ba86715721f35a39c64a1a2b70cce&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',10.5,1,'',1,'2018-04-15 14:21:56','2018-04-15 14:21:56'),(101,1,1,'300005AD5853823AE61254000000F','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=1&time=1523942712&sign=e2a103f570a06791482e199c55e04041&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',1,1,'',1,'2018-04-17 05:25:12','2018-04-17 05:25:12'),(102,1,1,'300005AD5853C23AE612540000010','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=1&time=1523942716&sign=d91e4190f0734dd85efcd6886845f892&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',1,1,'',1,'2018-04-17 05:25:16','2018-04-17 05:25:16'),(103,1,1,'300005AD5853E23AE612540000011','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=1&time=1523942718&sign=484c30535a8e942a8e27e7d5848cb98e&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',1,1,'',1,'2018-04-17 05:25:18','2018-04-17 05:25:18'),(104,1,1,'300005AD5854123AE612540000012','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=1&time=1523942721&sign=a4eb2f3f3d0b2458bd26ae8296dedda1&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',1,1,'',1,'2018-04-17 05:25:21','2018-04-17 05:25:21'),(105,1,1,'300005AD5867723AE612540000013','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=10.5&time=1523943031&sign=f669f0cc887a754b395e7e86b4fd912d&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10.5,1,'',1,'2018-04-17 05:30:31','2018-04-17 05:30:31'),(106,1,1,'300005AD5867C23AE612540000014','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=10.5&time=1523943036&sign=511151f970470aa1576724791832e16f&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10.5,1,'',1,'2018-04-17 05:30:36','2018-04-17 05:30:36'),(107,2,1,'300005AD5870D23AE612540000015','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=10.5&time=1523943180&sign=a404ebb75ae20c958639592d1058e009&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',10.5,1,'',1,'2018-04-17 05:33:01','2018-04-17 05:33:01'),(108,2,1,'300005AD5871023AE612540000016','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=10.5&time=1523943184&sign=aec5435e52c0c4398d6bbddc33109df4&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',10.5,1,'',1,'2018-04-17 05:33:04','2018-04-17 05:33:04'),(109,2,1,'300005AD5871323AE612540000017','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=10.5&time=1523943186&sign=3b993ce467327fe1922a357964cfad96&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',10.5,1,'',1,'2018-04-17 05:33:07','2018-04-17 05:33:07'),(110,1,1,'300005AD587A023AE612540000018','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=1&time=1523943328&sign=bcaa397a8726b7123fae26e0c4b8c096&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',1,1,'',1,'2018-04-17 05:35:28','2018-04-17 05:35:28'),(111,2,1,'300005AD587A323AE612540000019','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=c85bf7c6472cf09c72057a92b3910832&amount=1&time=1523943331&sign=ba4361f89cca1b547469a4415d5e32ce&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',1,1,'',1,'2018-04-17 05:35:31','2018-04-17 05:35:31'),(112,1,1,'300005AD5937723AE61254000001A','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=31&game_id=1&token=b6bade30369af4d7d64db00faab7cc69&amount=10&time=1523946359&sign=b757a3d18caade9d4e741accd6dba5d1&',31,'5a8irow88gsgb01rownupawmudjcj1nj8fytg6ke','myj4393','','BGX',10,1,'',1,'2018-04-17 06:25:59','2018-04-17 06:25:59'),(113,1,1,'300005AD593BA23AE61254000001B','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=31&game_id=1&token=b6bade30369af4d7d64db00faab7cc69&amount=14&time=1523946425&sign=6e2f06ce168889216b7a8d5cda5651bc&',31,'5a8irow88gsgb01rownupawmudjcj1nj8fytg6ke','myj4393','','BGX',14,1,'',1,'2018-04-17 06:27:06','2018-04-17 06:27:06'),(114,1,1,'300005AD593CC23AE61254000001C','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=31&game_id=1&token=b6bade30369af4d7d64db00faab7cc69&amount=1&time=1523946444&sign=dd8791a11e24d62edebbdad4cedc41a3&',31,'5a8irow88gsgb01rownupawmudjcj1nj8fytg6ke','myj4393','','BGX',1,1,'',1,'2018-04-17 06:27:24','2018-04-17 06:27:24'),(115,1,1,'300005AD5963F23AE61254000001D','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=32&game_id=1&token=72362b42beab18bcb98b34203ada934b&amount=1.9&time=1523947072&sign=5f4c9d3d1dc49873dbf49f84c3f58988&',32,'l9fjdcwxsotporq97wvm2tfe73yxv1pynwvvqhyw','hhhh','','BGX',1.9,1,'',1,'2018-04-17 06:37:51','2018-04-17 06:37:51'),(116,0,0,'2_1_1523947156679','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=83dc40c9c2e9d1efce77a0befdfebc38&dst_uid=1&amount=0.5&time=1523947156&sign=2e26200a0033b8809b4b78d6f8f9812a&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',0.5,1,'',1,'2018-04-17 06:39:16','2018-04-17 06:39:16'),(117,0,0,'1_2_1523961163416','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961163&sign=f71c5bf6303bdca18f1364c6e14bfc9d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:32:43','2018-04-17 10:32:43'),(118,0,0,'1_2_1523961165120','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961164&sign=4563b3f555dbb4d3f18ad1f7f0660856&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:32:45','2018-04-17 10:32:45'),(119,0,0,'1_2_1523961252607','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961252&sign=3248a44f2092ba9d0d1f60463aa23dc2&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:34:12','2018-04-17 10:34:12'),(120,0,0,'1_2_1523961252762','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961252&sign=3248a44f2092ba9d0d1f60463aa23dc2&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:34:12','2018-04-17 10:34:12'),(121,0,0,'1_2_1523961253864','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961253&sign=a1b7140e87f75727a8f0de485fee6c9f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:34:13','2018-04-17 10:34:13'),(122,0,0,'1_2_1523961254263','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961254&sign=fc5c45a22f32c32345640a7eeb86e477&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:34:14','2018-04-17 10:34:14'),(123,0,0,'1_2_1523961257921','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961257&sign=21a025f38372cb5c4311223b4f496b0c&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:34:17','2018-04-17 10:34:17'),(124,0,0,'1_2_1523961265178','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&dst_uid=2&coin_type=BGX&amount=0.5&time=1523961265&sign=ab2cf0f4531acb399243bdae84b32b75&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-17 10:34:25','2018-04-17 10:34:25'),(125,2,1,'300005AD5CDB723AE61254000001E','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&coin_type=BGX&amount=0.5&wallet_addr=&time=1523961271&sign=ccb4484b5c4961065b24591b13bbe3cf&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','123456','BGX',0.5,1,'',1,'2018-04-17 10:34:31','2018-04-17 10:34:31'),(126,1,1,'300005AD5CDBC23AE61254000001F','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=11a72791fac01577294629fbd3a0f463&coin_type=BGX&amount=0.5&time=1523961276&sign=12ff82e23da6e4b62d10e58afc3f9bd0&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcxyz','','BGX',0.5,1,'',1,'2018-04-17 10:34:36','2018-04-17 10:34:36'),(127,0,0,'2_1_1523967995882','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=5cc72b489a29f507444fbf2a35412f3e&dst_uid=1&coin_type=BGX&amount=2.1&time=1523967995&sign=4d07973187036bf5b38f8499b4195509&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',2.1,1,'',1,'2018-04-17 12:26:35','2018-04-17 12:26:35'),(128,0,0,'2_1_1523967997931','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=5cc72b489a29f507444fbf2a35412f3e&dst_uid=1&coin_type=BGX&amount=2.1&time=1523967997&sign=a48c26668adc83874796cec3d900f9b9&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',2.1,1,'',1,'2018-04-17 12:26:37','2018-04-17 12:26:37'),(129,0,0,'2_1_1523970316470','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&dst_uid=1&coin_type=BGX&amount=10.5&time=1523970316&sign=030bf6cd19ab016e42f4e34af36af8b5&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',10.5,1,'',1,'2018-04-17 13:05:16','2018-04-17 13:05:16'),(130,0,0,'2_1_1523970318118','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&dst_uid=1&coin_type=BGX&amount=10.5&time=1523970318&sign=0d3205c96df067a132a5f7bfc963ed7c&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',10.5,1,'',1,'2018-04-17 13:05:18','2018-04-17 13:05:18'),(131,1,1,'300005AD5F11B23AE612540000020','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&coin_type=BGX&amount=0.5&time=1523970331&sign=4b679aca9e78df539b67139e61a189ea&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','','BGX',0.5,1,'',1,'2018-04-17 13:05:31','2018-04-17 13:05:31'),(132,1,1,'300005AD5F11C23AE612540000021','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&coin_type=BGX&amount=0.5&time=1523970332&sign=e72fc3774dc3f7cf117b283c9679c99b&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','','BGX',0.5,1,'',1,'2018-04-17 13:05:32','2018-04-17 13:05:32'),(133,1,1,'300005AD5F11F23AE612540000022','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&coin_type=BGX&amount=0.5&time=1523970334&sign=9be03413d9d15de200d9f47c5ba2fabe&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','','BGX',0.5,1,'',1,'2018-04-17 13:05:35','2018-04-17 13:05:35'),(134,2,1,'300005AD5F14423AE612540000023','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&coin_type=BGX&amount=0.5&wallet_addr=&time=1523970372&sign=70ac5ea1f625d873a83aed0f2640dc3c&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',0.5,1,'',1,'2018-04-17 13:06:12','2018-04-17 13:06:12'),(135,2,1,'300005AD5F14523AE612540000024','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&coin_type=BGX&amount=0.5&wallet_addr=&time=1523970373&sign=3fc2c2a99d6cb2912ee2afe326aec0aa&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',0.5,1,'',1,'2018-04-17 13:06:13','2018-04-17 13:06:13'),(136,0,0,'2_1_1523970375723','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&dst_uid=1&coin_type=BGX&amount=10.5&time=1523970375&sign=bf25e2e61947ecac5fd5c6c9f0c0d9e9&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',10.5,1,'',1,'2018-04-17 13:06:15','2018-04-17 13:06:15'),(137,2,1,'300005AD5F14923AE612540000025','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&coin_type=BGX&amount=0.5&wallet_addr=&time=1523970377&sign=ed96d04b478b92a2999df3f46c962314&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',0.5,1,'',1,'2018-04-17 13:06:17','2018-04-17 13:06:17'),(138,1,1,'300005AD5F14B23AE612540000026','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=2&game_id=1&token=a6d1ce50e17b2f53123050498a30191b&coin_type=BGX&amount=0.5&time=1523970379&sign=e8249e3028abae4737c701ffa8d6107b&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','','BGX',0.5,1,'',1,'2018-04-17 13:06:19','2018-04-17 13:06:19'),(139,1,1,'300005AD5F9B623AE612540000027','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=8badf3d48f1b28810f32f8c96412a48e&amount=10&time=1523972534&sign=fede6164f24f030f0ae6f61b3522ec52&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10,1,'',1,'2018-04-17 13:42:14','2018-04-17 13:42:14'),(140,1,1,'300005AD5F9C823AE612540000028','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=8badf3d48f1b28810f32f8c96412a48e&amount=10&time=1523972551&sign=5be801d1fcd5aadd9380a27f9d455035&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10,1,'',1,'2018-04-17 13:42:32','2018-04-17 13:42:32'),(141,1,1,'300005AD5FA9423AE612540000029','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=c5203d4128968d4cf46accaaecbd1eb1&amount=10&time=1523972756&sign=abb6901d3e59bc8a29b0fc6aafb10992&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10,1,'',1,'2018-04-17 13:45:56','2018-04-17 13:45:56'),(142,1,1,'300005AD6A23C23AE61254000002A','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=5588d192128045b77f3ee0325c3e3b7e&amount=1&time=1524015676&sign=4aa54e0711a8e3822d823e67714c29d2&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',1,1,'',1,'2018-04-18 01:41:16','2018-04-18 01:41:16'),(143,2,1,'300005AD6A24223AE61254000002B','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=5588d192128045b77f3ee0325c3e3b7e&amount=1&time=1524015682&sign=3dbc4d992d95443ccef359811d9776bf&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',1,1,'',1,'2018-04-18 01:41:22','2018-04-18 01:41:22'),(144,0,0,'2_1_1524015816638','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=da4582aa8d0b6b4a05d4a633291a9337&dst_uid=1&coin_type=BGX&amount=0.2&time=1524015816&sign=11a050dca4fede744367caded43c1b8e&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',0.2,1,'',1,'2018-04-18 01:43:36','2018-04-18 01:43:36'),(145,0,0,'2_1_1524015818402','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=2&game_id=1&token=da4582aa8d0b6b4a05d4a633291a9337&dst_uid=1&coin_type=BGX&amount=0.2&time=1524015818&sign=acd144cd763816b279ad5cbe508dfc7c&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','','','BGX',0.2,1,'',1,'2018-04-18 01:43:38','2018-04-18 01:43:38'),(146,2,1,'300005AD6A2CF23AE61254000002C','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=2&game_id=1&token=da4582aa8d0b6b4a05d4a633291a9337&coin_type=BGX&amount=1&wallet_addr=&time=1524015823&sign=676e3d1b5307aaf1af8c2ce1508cf7eb&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',1,1,'',1,'2018-04-18 01:43:43','2018-04-18 01:43:43'),(147,2,1,'300005AD6A2D123AE61254000002D','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=2&game_id=1&token=da4582aa8d0b6b4a05d4a633291a9337&coin_type=BGX&amount=1&wallet_addr=&time=1524015825&sign=ac6f3ed9cef16081eaca92b219fa812e&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',1,1,'',1,'2018-04-18 01:43:45','2018-04-18 01:43:45'),(148,1,1,'300005AD6A2D423AE61254000002E','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=2&game_id=1&token=da4582aa8d0b6b4a05d4a633291a9337&coin_type=BGX&amount=1&time=1524015828&sign=910d5484a155d596cc26290bfcfec628&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','','BGX',1,1,'',1,'2018-04-18 01:43:48','2018-04-18 01:43:48'),(149,1,1,'300005AD6A2D523AE61254000002F','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=2&game_id=1&token=da4582aa8d0b6b4a05d4a633291a9337&coin_type=BGX&amount=1&time=1524015829&sign=f2a78b7e1a7d9cfe9ab3b3797beb6c6a&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','','BGX',1,1,'',1,'2018-04-18 01:43:49','2018-04-18 01:43:49'),(150,1,1,'300005AD6A2D623AE612540000030','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=2&game_id=1&token=da4582aa8d0b6b4a05d4a633291a9337&coin_type=BGX&amount=1&time=1524015830&sign=3752ef28bc84d037bacb7e966ac9824e&',2,'b6da433954ccfbf8f7b69a4f9fdbd3ff','123456','','BGX',1,1,'',1,'2018-04-18 01:43:50','2018-04-18 01:43:50'),(151,1,1,'300005AD6A31C23AE612540000031','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=50479799a23c0482d4de1b3769bbf933&amount=10.5&time=1524015900&sign=0876a283a87ebaea1e5d5341acf766dd&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10.5,1,'',1,'2018-04-18 01:45:00','2018-04-18 01:45:00'),(152,1,1,'300005AD6A31F23AE612540000032','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=50479799a23c0482d4de1b3769bbf933&amount=10.5&time=1524015903&sign=92dd6f72f188e2d7412cfc1b4f836f06&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10.5,1,'',1,'2018-04-18 01:45:03','2018-04-18 01:45:03'),(153,1,1,'300005AD6A32423AE612540000033','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=50479799a23c0482d4de1b3769bbf933&amount=10.5&time=1524015908&sign=37f50ae511dd605f1db1ec1b0c73ec9d&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10.5,1,'',1,'2018-04-18 01:45:08','2018-04-18 01:45:08'),(154,1,1,'300005AD6A32623AE612540000034','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=50479799a23c0482d4de1b3769bbf933&amount=10.5&time=1524015910&sign=acfd8eab8e2730ed76f5958e7a528572&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10.5,1,'',1,'2018-04-18 01:45:10','2018-04-18 01:45:10'),(155,2,1,'300005AD6A32823AE612540000035','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=50479799a23c0482d4de1b3769bbf933&amount=10.5&time=1524015912&sign=be24df8c7a8815c7e0618d4648bdabae&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',10.5,1,'',1,'2018-04-18 01:45:12','2018-04-18 01:45:12'),(156,2,1,'300005AD6A32A23AE612540000036','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=50479799a23c0482d4de1b3769bbf933&amount=10.5&time=1524015914&sign=26ede60a85888e2962fd65bf720f16fb&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',10.5,1,'',1,'2018-04-18 01:45:14','2018-04-18 01:45:14'),(157,1,1,'300005AD72CA723AE612540000037','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=31&game_id=1&token=dd7ebbe08a2066014c41e2ab1ba9b6dd&amount=8&time=1524051110&sign=d78b4d290cb3345b1cdc91cb0bf177fe&',31,'5a8irow88gsgb01rownupawmudjcj1nj8fytg6ke','myj4393','','BGX',8,1,'',1,'2018-04-18 11:31:51','2018-04-18 11:31:51'),(158,1,1,'300005AD72CAE23AE612540000038','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=31&game_id=1&token=dd7ebbe08a2066014c41e2ab1ba9b6dd&amount=8&time=1524051118&sign=606c1d9b21892e7b63976d814a6376b1&',31,'5a8irow88gsgb01rownupawmudjcj1nj8fytg6ke','myj4393','','BGX',8,1,'',1,'2018-04-18 11:31:58','2018-04-18 11:31:58'),(159,0,0,'1_2_1524571308090','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=bafe3a027745dad962eaacc35f9d9650&dst_uid=2&coin_type=BGX&amount=0.5&time=1524571307&sign=3b054cd4c94a457c121f8ebc0743e478&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.5,1,'',1,'2018-04-24 12:01:48','2018-04-24 12:01:48'),(160,2,1,'300005ADF1CB323AE6134F4000001','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=bafe3a027745dad962eaacc35f9d9650&coin_type=BGX&amount=0.2&wallet_addr=&time=1524571315&sign=baf92d007e17bca4a42707ac95be61bd&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','123456','BGX',0.2,1,'',1,'2018-04-24 12:01:55','2018-04-24 12:01:55'),(161,2,1,'300005ADF1CB623AE6134F4000002','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=bafe3a027745dad962eaacc35f9d9650&coin_type=BGX&amount=0.2&wallet_addr=&time=1524571318&sign=62e01364e664b36dbdd7ab6ddb640e0d&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','123456','BGX',0.2,1,'',1,'2018-04-24 12:01:58','2018-04-24 12:01:58'),(162,2,1,'300005ADF1CB723AE6134F4000003','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=bafe3a027745dad962eaacc35f9d9650&coin_type=BGX&amount=0.2&wallet_addr=&time=1524571319&sign=ddad38607ddbc2e358d171fbfcc9377e&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','123456','BGX',0.2,1,'',1,'2018-04-24 12:01:59','2018-04-24 12:01:59'),(163,1,1,'300005ADF1CBB23AE6134F4000004','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=bafe3a027745dad962eaacc35f9d9650&coin_type=BGX&amount=0.15&time=1524571323&sign=49ab6937f4b0afa0dd2cb69d854245e0&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','','BGX',0.15,1,'',1,'2018-04-24 12:02:03','2018-04-24 12:02:03'),(164,1,1,'300005ADF1CBD23AE6134F4000005','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=bafe3a027745dad962eaacc35f9d9650&coin_type=BGX&amount=0.15&time=1524571325&sign=04dca91ccbc6677d55d34f296b86ad93&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','abcdefg','','BGX',0.15,1,'',1,'2018-04-24 12:02:05','2018-04-24 12:02:05'),(165,1,1,'300005AE0442223AE6134F4000006','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=35b07c5a33b99e1235d0a0dd4d7fa19b&amount=1&time=1524646949&sign=e9d43a1e63de65c61b5e8c6cd802d2e5&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',1,1,'',1,'2018-04-25 09:02:26','2018-04-25 09:02:26'),(166,2,1,'300005AE0442723AE6134F4000007','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=35b07c5a33b99e1235d0a0dd4d7fa19b&amount=1&time=1524646954&sign=b0fff34b210882678febed4bb4bcab6a&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',1,1,'',1,'2018-04-25 09:02:31','2018-04-25 09:02:31'),(167,1,1,'300005AE0718A23AE6134F4000008','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=29&game_id=1&token=002f02a3eb3abc354073a4823bf9ffbf&amount=10&time=1524658573&sign=f04a7fd1d05c01bb593206895278e0ed&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','','BGX',10,1,'',1,'2018-04-25 12:16:10','2018-04-25 12:16:10'),(168,2,1,'300005AE0719123AE6134F4000009','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=29&game_id=1&token=002f02a3eb3abc354073a4823bf9ffbf&amount=10&time=1524658580&sign=53acefec13eb44ab5b42ff5163e2d3d9&',29,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583','BGX',10,1,'',1,'2018-04-25 12:16:17','2018-04-25 12:16:17'),(169,1,1,'300005AE2B51E23AE6134F400000A','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=32&game_id=1&token=7d60a8ab835a76cc6fcc8fa55fee676c&amount=1.09&time=1524806941&sign=76927b649dcf4f28fa7da221312e9660&',32,'l9fjdcwxsotporq97wvm2tfe73yxv1pynwvvqhyw','hhhh','','BGX',1.09,1,'',1,'2018-04-27 05:29:02','2018-04-27 05:29:02'),(170,2,1,'300005AE2B52C23AE6134F400000B','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=32&game_id=1&token=7d60a8ab835a76cc6fcc8fa55fee676c&amount=1.09&time=1524806955&sign=eaa8b4014238f9bd98ba45a3d12cfbb3&',32,'l9fjdcwxsotporq97wvm2tfe73yxv1pynwvvqhyw','hhhh','0x777426542a4866242fb2cb740afe264c1fa55ca5','BGX',1.09,1,'',1,'2018-04-27 05:29:16','2018-04-27 05:29:16'),(171,1,1,'300005AE9190223AE6134F400000C','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=32&game_id=1&token=e83daa8bb0fcdcd5015cd7b4672c6745&amount=0.77&time=1525225730&sign=c237ddf24d30183ef2871b39514941df&',32,'l9fjdcwxsotporq97wvm2tfe73yxv1pynwvvqhyw','hhhh','','BGX',0.77,1,'',1,'2018-05-02 01:48:50','2018-05-02 01:48:50'),(172,0,0,'1_2_1525403238864','http://47.74.226.126:8800/?a=transfer_coin_in_game&uid=1&game_id=1&token=c6a0176c039731375bf731e12198da36&dst_uid=2&coin_type=BGX&amount=0.7&time=1525403238&sign=1a9935e0c00904c207e2dbdc1032f5ea&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','','','BGX',0.7,1,'',1,'2018-05-04 03:07:18','2018-05-04 03:07:18'),(173,2,1,'300005AEBCE6923AE6134F400000D','http://47.74.226.126:8800/?a=transfer_coin_to_wallet&uid=1&game_id=1&token=c6a0176c039731375bf731e12198da36&coin_type=BGX&amount=1&wallet_addr=&time=1525403241&sign=6a54d92439bc4e08ae48c6af84d2189f&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','xyz','123456','BGX',1,1,'',1,'2018-05-04 03:07:21','2018-05-04 03:07:21'),(174,1,1,'300005AEBCE6C23AE6134F400000E','http://47.74.226.126:8800/?a=transfer_coin_to_exchange&uid=1&game_id=1&token=c6a0176c039731375bf731e12198da36&coin_type=BGX&amount=1&time=1525403244&sign=d8d671d271be62db30bc5e2aa731c8d8&',1,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','xyz','','BGX',1,1,'',1,'2018-05-04 03:07:24','2018-05-04 03:07:24');
/*!40000 ALTER TABLE `gold_transfer` ENABLE KEYS */;
UNLOCK TABLES;

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
  `current_game_uid` varchar(50) NOT NULL DEFAULT '' COMMENT 'å½“å‰æ‰€åœ¨æ¸¸æˆç”¨æˆ·æ ‡è¯†',
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
) ENGINE=InnoDB AUTO_INCREMENT=36 DEFAULT CHARSET=utf8 COMMENT='游戏中心账户';
/*!40101 SET character_set_client = @saved_cs_client */;

--
-- Dumping data for table `user`
--

LOCK TABLES `user` WRITE;
/*!40000 ALTER TABLE `user` DISABLE KEYS */;
INSERT INTO `user` (`id`, `user_name`, `password`, `player_name`, `avatar`, `device_id`, `org_device_id`, `is_bind`, `ios_gamecenter_id`, `google_id`, `facebook_id`, `current_game_id`, `current_game_uid`, `session_token`, `lang`, `os_type`, `country_code`, `create_time`, `last_login_time`, `status`, `forbid_login_endtime`, `bind_xchg_accid`, `bind_wallet_addr`, `time`) VALUES (1,'','','',0,'24AD6EC8-A2DE-5F98-8A6B-1A43737CC270','24AD6EC8-A2DE-5F98-8A6B-1A43737CC270',0,'','','',1,'','cc0f4bd84316e17d165a3bf99e64f658','English','Mac OS X 10.10.5','CN',1523007666,1525429921,0,0,'xyz','123456',1525429921),(2,'','','',0,'b6da433954ccfbf8f7b69a4f9fdbd3ff','b6da433954ccfbf8f7b69a4f9fdbd3ff',0,'','','',1,'','06a2c1918c5d7d49151cd6fc0fca3451','ChineseSimplified','Android OS 6.0 / API','CN',1523007873,1524658696,0,0,'123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583',1524658696),(3,'','','',0,'161d866aa4e43873ab4586f3cb373a8b15ee9ed2','161d866aa4e43873ab4586f3cb373a8b15ee9ed2',0,'','','',1,'','d04e7af439ef90299bc31e145ae1acd7','Chinese','Windows 10  (10.0.0)','CN',1523175892,1523175892,0,0,'','',1523175892),(4,'','','',0,'05yryk1eeiro3kwpuhmtepx2w23ie8dhst5s5y1r','05yryk1eeiro3kwpuhmtepx2w23ie8dhst5s5y1r',0,'','','',1,'','2dcfe2d39a5b55120e83a949fa04e960','Chinese','Windows 10  (10.0.0)','TW',1523189811,1523190324,0,0,'','',1523190324),(5,'','','',0,'5lakxg03txc40y6vklhac2bdp0672ad0ln8bteqe','5lakxg03txc40y6vklhac2bdp0672ad0ln8bteqe',0,'','','',1,'','fbd5feaa376a19821359bf3ee71f67f2','Chinese','Windows 10  (10.0.0)','TW',1523190343,1523190420,0,0,'','',1523190420),(6,'','','',0,'inr0iuynpyw0igieftjt0mekb7qdz8gyp9sdm0v1','inr0iuynpyw0igieftjt0mekb7qdz8gyp9sdm0v1',0,'','','',1,'','73fb9a3f6e04cfad4de1a6a993a7526b','Chinese','Windows 10  (10.0.0)','TW',1523190434,1523190748,0,0,'','',1523190748),(7,'','','',0,'d4dt7gxkh9jxkzk986c5opv9fdkzwk6e6dlkznw7','d4dt7gxkh9jxkzk986c5opv9fdkzwk6e6dlkznw7',0,'','','',1,'','38482c4e8df27adb4d6c2f72636b807e','Chinese','Windows 10  (10.0.0)','TW',1523190786,1523191328,0,0,'','',1523191328),(8,'','','',0,'9389hhusu4kjvb4gs6ufhc6q551f3n74ck5dxm01','9389hhusu4kjvb4gs6ufhc6q551f3n74ck5dxm01',0,'','','',1,'','1ed6ebfe85846a9f6f5a24290ba74b92','Chinese','Windows 10  (10.0.0)','TW',1523191376,1523242021,0,0,'','',1523242021),(9,'','','',0,'fqr4s422qbe8vuf7bmsx5drr46mmkvcd0dnmili7','fqr4s422qbe8vuf7bmsx5drr46mmkvcd0dnmili7',0,'','','',1,'','133a8b26da3f005e88947501b4904ea9','Chinese','Windows 10  (10.0.0)','CN',1523242039,1523243414,0,0,'','',1523243414),(10,'','','',0,'uuufu1oywq1b2sa0vjuyvypad9q1lecrg9gns9kd','uuufu1oywq1b2sa0vjuyvypad9q1lecrg9gns9kd',0,'','','',1,'','befb73dd823f0c8d959c259077a3c35e','Chinese','Windows 10  (10.0.0)','CN',1523243705,1523244137,0,0,'','',1523244137),(11,'','','',0,'t939mok8fpo9izchdz48q6ugoctgtjqfdkyvlcfj','t939mok8fpo9izchdz48q6ugoctgtjqfdkyvlcfj',0,'','','',1,'','b7a166ec46720d58b19b4c157d1020e1','Chinese','Windows 10  (10.0.0)','CN',1523244162,1523244673,0,0,'','',1523244673),(12,'','','',0,'5x8ak9f844qa2c7k93mth9d186bkuszwrz72658x','5x8ak9f844qa2c7k93mth9d186bkuszwrz72658x',0,'','','',1,'','76f418eea7e6ed957a8e470b7d107810','Chinese','Windows 10  (10.0.0)','CN',1523244689,1523266156,0,0,'xxx@qq.com','testWallxx',1523266156),(13,'','','',0,'eodjivbuh8z8udjny7f8lebouqn9yisoaafko23a','eodjivbuh8z8udjny7f8lebouqn9yisoaafko23a',0,'','','',1,'','b227740102b569ec4a42b0a9928ae624','Chinese','Windows 10  (10.0.0)','GB',1523266174,1523271560,0,0,'','',1523271560),(14,'','','',0,'pglb2yb1wlui19xl4eplbrrbson0l7c7mgp8cq7g','pglb2yb1wlui19xl4eplbrrbson0l7c7mgp8cq7g',0,'','','',1,'','a99b112c54a81ba6f2933e1f5ed401a5','Chinese','Windows 10  (10.0.0)','GB',1523271577,1523274467,0,0,'','',1523274467),(15,'','','',0,'zgaqivhykr35gpjy6eyv3o9xjh13eblqbbgyon6h','zgaqivhykr35gpjy6eyv3o9xjh13eblqbbgyon6h',0,'','','',1,'','0ebdbf278c9c2446db457c34f804ddad','Chinese','Windows 10  (10.0.0)','GB',1523274480,1523344180,0,0,'what the fuck@g.com','sdfsdfsdfsdfdsf',1523344180),(16,'','','',0,'6g9mrnkbvh3gbmeu25e03d3umao297suauc2650y','6g9mrnkbvh3gbmeu25e03d3umao297suauc2650y',0,'','','',1,'','ef3ef5b3762bee454594d0b29cbe2c4d','Chinese','Windows 10  (10.0.0)','US',1523344225,1523345093,0,0,'test12345@qq.com','',1523345093),(17,'','','',0,'67n83z9gkzxwoaqxbwt605lqp6zdsyk78lf2qcpl','67n83z9gkzxwoaqxbwt605lqp6zdsyk78lf2qcpl',0,'','','',1,'','10863b29539f896c70a2b6813f94a093','Chinese','Windows 10  (10.0.0)','JP',1523345353,1523347114,0,0,'123234238479387@qq.com','sljfsjdfjsdf',1523347114),(18,'','','',0,'7hgckp8r8nkbfnolsot2ur917jr41vcxt65o61so','7hgckp8r8nkbfnolsot2ur917jr41vcxt65o61so',0,'','','',1,'','d248e27ae2a74a7e16c2f2c199f489b8','Chinese','Windows 10  (10.0.0)','TW',1523347135,1523347135,0,0,'sdfdsf','sdfsdfsdfsds',1523347154),(19,'','','',0,'r4n6ay7qvvd1cq37cjshf9t27wy57flrg3l8o3ub','r4n6ay7qvvd1cq37cjshf9t27wy57flrg3l8o3ub',0,'','','',1,'','333d69f7495678ce6d2d14152c420e69','Chinese','Windows 10  (10.0.0)','TW',1523347172,1523348965,0,0,'ssss','sdfsdfsdfdsf',1523348965),(20,'','','',0,'fig9g6bw5fmranb5n74cg4yt9fzxxxg03ff4joh0','fig9g6bw5fmranb5n74cg4yt9fzxxxg03ff4joh0',0,'','','',1,'','275ebb4144eb5a2586d16998fc4649f0','Chinese','Windows 10  (10.0.0)','US',1523349708,1523354650,0,0,'Wind_Sword@qq.com','jshfuhkshfkhskf',1523354650),(21,'','','',0,'zpwvdh18vymx9w5f9vtxdxn904bym28iiqfxqlrx','zpwvdh18vymx9w5f9vtxdxn904bym28iiqfxqlrx',0,'','','',1,'','3278d13162e412e53c3dfeaf22b5be0b','ChineseSimplified','Android OS 5.0.2 / A','CN',1523352670,1523353844,0,0,'ggggghv','gdhjbgh',1523353945),(22,'','','',0,'nzg3wn021zkbu3u98iyodb5mojczr7wn9td0p915','nzg3wn021zkbu3u98iyodb5mojczr7wn9td0p915',0,'','','',1,'','4620effe6dbdc227e40de92d7c53d8e1','ChineseSimplified','Android OS 5.0.2 / A','CN',1523354061,1523361264,0,0,'','',1523361264),(23,'','','',0,'p4s96haeguv79j4ah5qisgdhrs3ied3qbtg0q0e3','p4s96haeguv79j4ah5qisgdhrs3ied3qbtg0q0e3',0,'','','',1,'','3774113b97247eefd1f68bfdaf149b47','Chinese','Windows 10  (10.0.0)','US',1523354668,1523358111,0,0,'','',1523358111),(24,'','','',0,'a02rytwfgqrv8rm8eisa28tadjze3184vk8m28p6','a02rytwfgqrv8rm8eisa28tadjze3184vk8m28p6',0,'','','',1,'','aa04e621815dccafab33d40c34dda4a2','Chinese','Windows 10  (10.0.0)','US',1523358123,1523412014,0,0,'','',1523412014),(25,'','','',0,'s325hhb3qdbhdxtfgvdo55pkbbd8n1wbfmkqbb3l','s325hhb3qdbhdxtfgvdo55pkbbd8n1wbfmkqbb3l',0,'','','',1,'','5d6ed35c3dab8418ef024f232d6ea631','ChineseSimplified','Android OS 5.1.1 / A','CN',1523364443,1524290468,0,0,'KORN','22222',1524290468),(26,'','','',0,'tlvrhu8hb6rhga8mlqjzd50mzaukcvtenbq1uvae','tlvrhu8hb6rhga8mlqjzd50mzaukcvtenbq1uvae',0,'','','',1,'','1c9d933b015eaad657971fc77980a3d9','ChineseSimplified','Android OS 5.0.2 / A','CN',1523365209,1523365209,0,0,'','',1523365209),(27,'','','',0,'kcihw62213my8j7qyzdmvuo739s26x9t8xzy4iha','kcihw62213my8j7qyzdmvuo739s26x9t8xzy4iha',0,'','','',0,'','','ChineseSimplified','Android OS 5.0.2 / A','US',1523438393,0,0,0,'','',1523438393),(28,'','','',0,'m5253g29rln9cpy8lxk7n5o5p2jgijppx14i9btw','m5253g29rln9cpy8lxk7n5o5p2jgijppx14i9btw',0,'','','',0,'','','ChineseSimplified','Android OS 5.0.2 / A','US',1523439060,0,0,0,'','',1523439060),(29,'','','',0,'b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5','b2k6orqw0d83ynbqgkrtcavfp4xkexgvz9b77bl5',0,'','','',1,'','002f02a3eb3abc354073a4823bf9ffbf','ChineseSimplified','Android OS 6.0 / API','CN',1523453786,1524658546,0,0,'123456','0x8e53324B2f0d61a6b82bA3BE00eBcCD725E64583',1524658546),(30,'','','',0,'4k6lfsrvdhk290ee6je2olrov95ftjv1wftlbmqk','4k6lfsrvdhk290ee6je2olrov95ftjv1wftlbmqk',0,'','','',1,'','39d1c3919d256fbd9cbeff55fc6c111b','ChineseSimplified','Android OS 7.0 / API','CN',1523500114,1524401470,0,0,'','',1524401470),(31,'','','',0,'5a8irow88gsgb01rownupawmudjcj1nj8fytg6ke','5a8irow88gsgb01rownupawmudjcj1nj8fytg6ke',0,'','','',1,'','2ceeda36938fff0f267b9d779e8d0c72','ChineseSimplified','Android OS 7.1.1 / A','US',1523945900,1524307861,0,0,'myj4393','',1524307861),(32,'','','',0,'l9fjdcwxsotporq97wvm2tfe73yxv1pynwvvqhyw','l9fjdcwxsotporq97wvm2tfe73yxv1pynwvvqhyw',0,'','','',1,'','e83daa8bb0fcdcd5015cd7b4672c6745','ChineseSimplified','Android OS 8.0.0 / A','CN',1523946742,1525225679,0,0,'hhhh','0x777426542a4866242fb2cb740afe264c1fa55ca5',1525225679),(33,'','','',0,'9a1dbdfd0fd51590ce27484595e4941b','9a1dbdfd0fd51590ce27484595e4941b',0,'','','',1,'','7a8b11fc68fc00c2eb821a785a362e71','ChineseSimplified','Android OS 7.1.1 / A','CN',1524014353,1524014353,0,0,'','',1524014353),(34,'','','',0,'7cfd6f75c5c616ddf74bf596653b1017','7cfd6f75c5c616ddf74bf596653b1017',0,'','','',1,'','9cf298b92a31a187926365616b91c7ec','ChineseSimplified','Android OS 8.0.0 / A','CN',1524199537,1524199680,0,0,'','',1524199680),(35,'','','',0,'jo557bwm5qz2i80tnru5b9yrm5knkvigrkn4wsre','jo557bwm5qz2i80tnru5b9yrm5knkvigrkn4wsre',0,'','','',1,'','fc396be76c02455fb09d254fa9a6e302','ChineseSimplified','Android OS 5.1.1 / A','GB',1524297870,1524647537,0,0,'','',1524647537);
/*!40000 ALTER TABLE `user` ENABLE KEYS */;
UNLOCK TABLES;

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
-- Dumping data for table `user_gold`
--

LOCK TABLES `user_gold` WRITE;
/*!40000 ALTER TABLE `user_gold` DISABLE KEYS */;
INSERT INTO `user_gold` (`player_id`, `gold`, `time`) VALUES (1,'{}',1523007666),(2,'{}',1523007873),(3,'{}',1523175892),(4,'{}',1523189811),(5,'{}',1523190343),(6,'{}',1523190434),(7,'{}',1523190786),(8,'{}',1523191376),(9,'{}',1523242039),(10,'{}',1523243705),(11,'{}',1523244162),(12,'{}',1523244689),(13,'{}',1523266174),(14,'{}',1523271577),(15,'{}',1523274480),(16,'{}',1523344225),(17,'{}',1523345353),(18,'{}',1523347135),(19,'{}',1523347172),(20,'{}',1523349708),(21,'{}',1523352670),(22,'{}',1523354061),(23,'{}',1523354668),(24,'{}',1523358123),(25,'{}',1523364443),(26,'{}',1523365209),(27,'{}',1523438393),(28,'{}',1523439060),(29,'{}',1523453786),(30,'{}',1523500114),(31,'{}',1523945900),(32,'{}',1523946742),(33,'{}',1524014353),(34,'{}',1524199537),(35,'{}',1524297870);
/*!40000 ALTER TABLE `user_gold` ENABLE KEYS */;
UNLOCK TABLES;

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

-- Dump completed on 2018-05-04 19:31:54
