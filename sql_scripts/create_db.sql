drop database if exists bitgame_cfg;
drop database if exists bitgame_usr;
create database bitgame_cfg default character set utf8 collate utf8_general_ci;
create database bitgame_usr default character set utf8 collate utf8_general_ci;
grant all on bitgame_cfg.* to bitgame@'localhost' identified by 'bitgame123';
grant all on bitgame_usr.* to bitgame@'localhost' identified by 'bitgame123';

drop database if exists bitgame_run;
create database bitgame_run default character set utf8 collate utf8_general_ci;
grant all on bitgame_run.* to bitgame@'localhost' identified by 'bitgame123';

drop database if exists bitgame_log;
create database bitgame_log default character set utf8 collate utf8_general_ci;
grant all on bitgame_log.* to bitgame@'localhost' identified by 'bitgame123';
