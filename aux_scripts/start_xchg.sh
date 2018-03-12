#!/bin/sh
./safe_make_no_nif.sh
cd ../ebin
echo "starting xchg server......"
erl -hidden +P 1024000 +K true -name xchg30000@127.0.0.1 -setcookie bit_gamex_3 -boot start_sasl -config 30000 -s bg xchgsvr_start -extra 127.0.0.1 30000
