#!/bin/bash
erl -pa ../../ebin -make
cd ../../ebin
erl -noshell -s data_autogen_cfg run -s init stop
erl -noshell -s data_autogen_run run -s init stop
erl -noshell -s data_autogen_log run -s init stop
erl -noshell -s data_autogen_usr run -s init stop
mv *.hrl ../include
mv *.erl ../src/data
