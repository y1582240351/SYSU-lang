#!/bin/bash

lexer="lexer"
parser="parser"
total="total"

if [ $# -eq 1 ] && [ $1 = "$parser" ]
then
  ( export PATH=~/sysu/bin:$PATH \
  CPATH=~/sysu/include:$CPATH \
  LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
  LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
  sysu-compiler --unittest=parser-1 "**/*.sysu.c" )
fi

if [ $# -eq 1 ] && [ $1 = "$total" ]
then
  CTEST_OUTPUT_ON_FAILURE=1 cmake --build ~/sysu/build -t test
fi