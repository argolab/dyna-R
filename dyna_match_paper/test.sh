#!/bin/sh

set -xe

#python3 rexprs.py

#pypy3 rexprs.py

cython --embed -3 rexprs.py
gcc $(pkg-config --libs --cflags python3-embed) -O3 -fprofile-generate -DPYREX_WITHOUT_ASSERTIONS -march=native -mtune=native rexprs.c
./a.out
gcc $(pkg-config --libs --cflags python3-embed) -O3 -fprofile-use -DPYREX_WITHOUT_ASSERTIONS -march=native -mtune=native rexprs.c
time ./a.out
