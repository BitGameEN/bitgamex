#!/bin/sh
cd ./lager_compile
erl -make
cd ..
erl -pa ../ebin -make
cd ../src/deps/luerl
make
cd ../src/deps/jiffy
make
cp priv/jiffy.so ../../../priv
cd ../cberl
#make update-deps
make compile
cp priv/cberl_drv.so ../../../priv
