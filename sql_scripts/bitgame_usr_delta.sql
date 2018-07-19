SET NAMES utf8;

CREATE TABLE `game_package` (
  `game_id` int(11) NOT NULL COMMENT '游戏id',
  `package_id` int(11) NOT NULL COMMENT '包id',
  `mining_rule` varchar(255) NOT NULL DEFAULT '[]' COMMENT 'erlang, 格式例子：[{''BGX'', 30}, {''BTC'', 10}, {''ETH'', 10}, {''ELA'', 50}]',
  `mining_pools` varchar(1024) NOT NULL DEFAULT '[]' COMMENT 'erlang, 格式：[{gold_type, mining_start_time, mining_output_first_day, half_life_days, chain_type, amount}]',
  PRIMARY KEY (`game_id`,`package_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COMMENT='游戏包';

ALTER TABLE `user` ADD COLUMN `current_game_package_id` int(11) NOT NULL DEFAULT 0 COMMENT '当前所在游戏包id' AFTER `current_game_id`;
ALTER TABLE `user` ADD INDEX `current_game_id_pkg_id`(`current_game_id`, `current_game_package_id`);