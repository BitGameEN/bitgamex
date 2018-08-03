SET NAMES utf8;

ALTER TABLE `player_login` ADD COLUMN `game_package_id` int(11) DEFAULT 0 COMMENT '游戏包id' AFTER `game_id`;

ALTER TABLE `gold_to_draw` ADD COLUMN `package_id` int(11) NOT NULL DEFAULT 0 COMMENT '游戏包id' AFTER `game_id`,
ADD COLUMN `chain_type` varchar(20) NOT NULL DEFAULT '' COMMENT '链' AFTER `gold_type`;

