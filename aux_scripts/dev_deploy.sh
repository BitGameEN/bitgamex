#!/bin/sh

DB_HOST=127.0.0.1
DB_USR=root
DB_PWD=r00t
CB_HOST=127.0.0.1
CB_USR=Administrator
CB_PWD=bitgame


echo "####################################################################"
echo "bitgame server deploying (on CentOS 6.6)......"
echo "####################################################################"

which git > /dev/null 2>&1
if [ $? != 0 ]; then
    echo "####################################################################"
    echo "git does not exist, installing it......"
    echo "####################################################################"
    yum -y install autoconf
    yum -y install perl-ExtUtils-MakeMaker
    yum -y install tk gettext-devel curl-devel
    wget -O git.zip https://github.com/git/git/archive/master.zip
    unzip git.zip
    cd git-master
    autoconf
    ./configure --prefix=/usr/local
    make && make install
    rm /usr/bin/git
    ln -s /usr/local/bin/git /usr/bin/git
    cd ..
fi

echo "####################################################################"
echo "getting source code......"
echo "####################################################################"
if [ ! -d bitgamex ]; then
    git clone https://gitlab.com/gulige/bitgamex.git
else
    cd bitgamex
    git pull
    cd ..
fi

echo "####################################################################"
echo "stopping bitgame servers......"
echo "####################################################################"
cd bitgamex/aux_scripts
./stop.sh

echo "####################################################################"
echo "generating cfg files......"
echo "####################################################################"
cd codegen
./codegen_cfg_all.sh
cd ..

echo "####################################################################"
echo "compiling source code......"
echo "####################################################################"
source /etc/profile
./make.sh

echo "####################################################################"
echo "importing mysql databases......"
echo "####################################################################"
mysql -h${DB_HOST} -u${DB_USR} -p${DB_PWD} << EOF
source ../sql_scripts/create_db.sql;
use bitgame_usr;
source ../sql_scripts/bitgame_usr.sql;
source ../sql_scripts/bitgame_usr_delta.sql;
use bitgame_run;
source ../sql_scripts/bitgame_run.sql;
source ../sql_scripts/bitgame_run_delta.sql;
use bitgame_log;
source ../sql_scripts/bitgame_log.sql;
source ../sql_scripts/bitgame_log_delta.sql;
EOF

echo "####################################################################"
echo "flushing couchbase data bucket......"
echo "####################################################################"
couchbase-cli bucket-flush --force -c ${CB_HOST}:8091 -u ${CB_USR} -p ${CB_PWD} --bucket=default --enable-flush=1

echo "####################################################################"
echo "starting bitgame servers......"
echo "####################################################################"
./start.sh

echo "#############################D-O-N-E################################"
