#!/bin/bash

# 导入cfg数据库
DB_HOST=127.0.0.1
DB_USR=root
echo "please input mysql password:"
read pwd
DB_PWD=$pwd

/usr/local/mysql/bin/mysql -h${DB_HOST} -u${DB_USR} -p${DB_PWD} << EOF
use bitgame_cfg;
source ../../sql_scripts/bitgame_cfg.sql;
EOF

# 生成cfg文件
erl -pa ../../ebin -make
cd ../../ebin
erl -noshell -s data_autogen_cfg run -s init stop
mv *.hrl ../include
mv *.erl ../src/data
