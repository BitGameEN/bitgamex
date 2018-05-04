#!/bin/sh

echo "dumping usr......"
mysqldump --complete-insert -h 127.0.0.1 -u root -p r00t usr > ../sql_scripts/bitgame_usr_all.sql
echo "done!"