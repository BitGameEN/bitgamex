#!/bin/sh
./safe_make_no_nif.sh
cd ../ebin
echo "starting gate server......"
erl -hidden +P 1024000 +K true -name gate10000@127.0.0.1 -setcookie bit_gamex_1 -boot start_sasl -config 10000 -s bg gatesvr_start -extra 127.0.0.1 8800 10000

