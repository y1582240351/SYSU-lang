#!/bin/bash

clang="clang"
mine="mine"
parser="parser"
generator="generator"
optimizer="optimizer"
total="total"


if [ $# -eq 2 ] && [ $1 = "$parser" ]
then
  chmod +x preprocessor/sysu-preprocessor
  if [ $# -eq 2 ] && [ $2 = "$clang" ]
  then
  ( export PATH=~/sysu/bin:$PATH \
    CPATH=~/sysu/include:$CPATH \
    LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
    LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
    preprocessor/sysu-preprocessor ../test/2.c  | clang -cc1 -ast-dump=json )
  fi

  if [ $# -eq 1 ] && [ $2 = "$mine" ]
  then
  ( export PATH=~/sysu/bin:$PATH \
    CPATH=~/sysu/include:$CPATH \
    LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
    LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
    preprocessor/sysu-preprocessor ../test/2.c | clang -cc1 -dump-tokens 2>&1 | ~/sysu/build/parser/sysu-parser)
  fi
fi


if [ $# -eq 2 ] && [ $1 = "$generator" ]
then
  if [ $# -eq 2 ] && [ $2 = "$clang" ]
  then
    ( export PATH=~/sysu/bin:$PATH \
      CPATH=~/sysu/include:$CPATH \
      LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
      LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
      clang -E ../test/2.c |
      clang -cc1 -S -emit-llvm )
  fi

  if [ $# -eq 2 ] && [ $2 = "$mine" ]
  then
    ( export PATH=~/sysu/bin:$PATH \
      CPATH=~/sysu/include:$CPATH \
      LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
      LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
      sysu-compiler --unittest=benchmark_generator_and_optimizer_1 "tester/h_functional/093_n_queens.sysu.c " )
  fi

  if [ $# -eq 2 ] && [ $2 = "$total" ]
  then
    ( export PATH=~/sysu/bin:$PATH \
      CPATH=~/sysu/include:$CPATH \
      LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
      LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
      sysu-compiler --unittest=benchmark_generator_and_optimizer_1 "./tester/third_party/SYsU-lang-tester-perfermance/performance_test2021-public/*.c" )
  fi
fi

if [ $# -eq 2 ] && [ $1 = "$optimizer" ]
then
  if [ $2 = "$mine" ]
  then
    # ( export PATH=~/sysu/bin:$PATH \
    # CPATH=~/sysu/include:$CPATH \
    # LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
    # LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
    # sysu-compiler --unittest=benchmark_generator_and_optimizer_1 "../test/2.c" )
  #   ( export PATH=~/sysu/bin:$PATH \
  # CPATH=~/sysu/include:$CPATH \
  # LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
  # LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
  # clang -E tester/function_test2020/00_arr_defn2.sysu.c |
  # clang -cc1 -O0 -S -emit-llvm |
  # opt -S --enable-new-pm -load-pass-plugin=libsysuOptimizer.so -passes="sysu-optimizer-pass" )
  ( export PATH=~/sysu/bin:$PATH \
  CPATH=~/sysu/include:$CPATH \
  LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
  LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
  clang -E tester/function_test2020/00_arr_defn2.sysu.c |
  clang -cc1 -O0 -S -emit-llvm | sysu-optimizer )
    # sysu-preprocessor ../test/2.c  | clang -cc1 -ast-dump=json  | sysu-generator | sysu-optimizer )
  fi
  if [ $2 = "$total" ]
  then 
    ( export PATH=~/sysu/bin:$PATH \
    CPATH=~/sysu/include:$CPATH \
    LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
    LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
    sysu-compiler --unittest=benchmark_generator_and_optimizer_1 "tester/third_party/SYsU-lang-tester-perfermance/performance_test2021-private/dead-code-elimination-1.sysu.c" )

  fi
  if [ $2 = "$clang" ]
  then 
    ( export PATH=~/sysu/bin:$PATH \
  CPATH=~/sysu/include:$CPATH \
  LIBRARY_PATH=~/sysu/lib:$LIBRARY_PATH \
  LD_LIBRARY_PATH=~/sysu/lib:$LD_LIBRARY_PATH &&
  clang -E ../test/2.c |
  clang -cc1 -O1 -S -emit-llvm )
  fi
fi
