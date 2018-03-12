#!/bin/sh
./safe_make_no_nif.sh
cd ../ebin
echo "starting game server......"
erl -hidden +P 1024000 +K true -name game20000@127.0.0.1 -setcookie bit_gamex_2 -boot start_sasl -config 20000 -s bg gamesvr_start -extra 127.0.0.1 20000
