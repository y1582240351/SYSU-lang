#!/bin/bash

clang="clang"
mine="mine"

if [ $# -eq 1 ] && [ $1 = "$clang" ]
then
  ( clang -E ../test/2.c |
    clang -cc1 -O0 -S -emit-llvm |
    opt -dot-cfg )
fi


if [ $# -eq 1 ] && [ $1 = "$mine" ]
then 
  ( export PATH=~/sysu/bin:$PATH \
    CPATH=~/sysu/include:$CPATH \
    LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
    LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
    clang -E ../test/2.c |
    clang -cc1 -ast-dump=json |
    sysu-generator )
fi