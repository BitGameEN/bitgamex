SET NAMES utf8;

ALTER TABLE `player_login` ADD COLUMN `game_package_id` int(11) DEFAULT 0 COMMENT '游戏包id' AFTER `game_id`;