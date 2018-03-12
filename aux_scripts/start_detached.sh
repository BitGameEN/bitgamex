#!/bin/sh
./safe_make_no_nif.sh
cd ../ebin
echo "starting server......"
erl -hidden +P 1024000 +K true -detached -smp -name gate10000@127.0.0.1 -setcookie bit_gamex_1 -boot start_sasl -config 10000 -s bg gatesvr_start -extra 127.0.0.1 8800 10000
erl -hidden +P 1024000 +K true -detached -smp -name game20000@127.0.0.1 -setcookie bit_gamex_2 -boot start_sasl -config 20000 -s bg gamesvr_start -extra 127.0.0.1 20000
erl -hidden +P 1024000 +K true -detached -smp -name xchg30000@127.0.0.1 -setcookie bit_gamex_3 -boot start_sasl -config 30000 -s bg xchgsvr_start -extra 127.0.0.1 30000
