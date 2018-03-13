#!/bin/sh
cd ./lager_compile
erl -make
cd ..
erl -pa ../ebin -make
cd ../src/deps/luerl
make
