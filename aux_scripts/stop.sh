echo "stopping servers......"
erl_call -a 'bg gatesvr_stop' -name gate10000@127.0.0.1 -c bit_gamex_1
erl_call -a 'bg xchgsvr_stop' -name xchg30001@127.0.0.1 -c bit_gamex_3
erl_call -a 'bg gamesvr_stop' -name game20000@127.0.0.1 -c bit_gamex_2
erl_call -a 'bg gamesvr_stop' -name game20001@127.0.0.1 -c bit_gamex_2
sleep 2
ps -ef | grep "beam" | grep -v grep
