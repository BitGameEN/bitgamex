#!/bin/sh

OTP_VER=20.2
ERLIN_VER=3.10.1
LIBCB_VER=2.5.2
CB_VER=4.0.0

echo "####################################################################"
echo "bitgame server environment setting up (on CentOS 6)......"
echo "####################################################################"

echo "####################################################################"
echo "1) Installing Erlang......"
echo "####################################################################"

cd /opt/
if [ ! -f otp_src_${OTP_VER}.tar.gz ]; then
  echo "####################################################################"
  echo "downloading otp_src_${OTP_VER}......"
  echo "####################################################################"
  wget http://www.erlang.org/download/otp_src_${OTP_VER}.tar.gz
fi

tar -zxvf otp_src_${OTP_VER}.tar.gz
cd otp_src_${OTP_VER}

echo "####################################################################"
echo "setting up otp build environment......"
echo "####################################################################"
yum -y install kernel-devel ncurses-devel
yum -y install openssl openssl-devel
yum -y install unixODBC unixODBC-devel
yum -y install m4 make gcc gcc-c++

echo "####################################################################"
echo "building and installing otp......"
echo "####################################################################"
./configure --prefix=/usr/local/erlang --enable-hipe --enable-threads --enable-smp-support --enable-kernel-poll --enable-native-libs --with-ssl
make && make install

rm -f /usr/bin/erl_call
ln -s /usr/local/erlang/lib/erlang/lib/erl_interface-${ERLIN_VER}/bin/erl_call /usr/bin/erl_call

echo "####################################################################"
echo "2) Installing mysql......"
echo "####################################################################"
yum -y install mysql-server
/etc/rc.d/init.d/mysqld start

echo "####################################################################"
echo "configuring mysql (permissions, global vars)......"
echo "####################################################################"
mysql -u root << EOF
set password for root@localhost=password('r00t');
delete from mysql.user where user='';

set global slow_query_log = on;
set global table_open_cache=1000;
set global max_connections=800;
set global max_allowed_packet=2097152;
set global key_buffer_size=20971520;
set global query_cache_type=1;
set global query_cache_limit=20971520;
set global query_cache_size=20971520;
set global tmp_table_size=52428800;
set global max_heap_table_size=52428800;
set global read_buffer_size=8388608;
set global low_priority_updates=1;
set global concurrent_insert=2;
EOF

echo "####################################################################"
echo "3) Installing CouchBase......"
echo "####################################################################"
cd /opt/
if [ ! -f libcouchbase-${LIBCB_VER}_centos62_x86_64.tar ]; then
  echo "####################################################################"
  echo "downloading libcouchbase......"
  echo "####################################################################"
  wget http://packages.couchbase.com/clients/c/libcouchbase-${LIBCB_VER}_centos62_x86_64.tar
fi
echo "####################################################################"
echo "installing libcouchbase......"
echo "####################################################################"
tar xvf libcouchbase-${LIBCB_VER}_centos62_x86_64.tar
cd libcouchbase-${LIBCB_VER}_centos62_x86_64
yum -y install libevent
rpm -ivh libcouchbase-devel-${LIBCB_VER}-1.el6.x86_64.rpm libcouchbase2-core-${LIBCB_VER}-1.el6.x86_64.rpm \
libcouchbase2-bin-${LIBCB_VER}-1.el6.x86_64.rpm libcouchbase2-libevent-${LIBCB_VER}-1.el6.x86_64.rpm

cd /opt/
if [ ! -f couchbase-server-enterprise-${CB_VER}-centos6.x86_64.rpm ]; then
  echo "####################################################################"
  echo "downloading couchbase-server......"
  echo "####################################################################"
  wget http://packages.couchbase.com/releases/${CB_VER}/couchbase-server-enterprise-${CB_VER}-centos6.x86_64.rpm
fi
echo "####################################################################"
echo "installing couchbase-server......"
echo "####################################################################"
rpm --install couchbase-server-enterprise-${CB_VER}-centos6.x86_64.rpm

echo "####################################################################"
echo "setting up environment vars......"
echo "####################################################################"
echo "
ERL_HOME=/usr/local/erlang
CB_HOME=/opt/couchbase
PATH=\$ERL_HOME/bin:\$CB_HOME/bin:\$PATH
export ERL_HOME PATH
" >> /etc/profile
. /etc/profile


echo "#############################D-O-N-E################################"