sudo apt install \
  ninja-build cmake git python3 \
  cpp flex bison zlib1g-dev \
  clang libclang-dev llvm-dev

git clone https://github.com/arcsysu/SYsU-lang
cd SYsU-lang

# 编译安装
rm -rf ~/sysu
cmake -G Ninja \
  -DCMAKE_C_COMPILER=clang \
  -DCMAKE_CXX_COMPILER=clang++ \
  -DCMAKE_INSTALL_PREFIX=~/sysu \
  -DCPACK_SOURCE_IGNORE_FILES=".git/;tester/third_party/" \
  -B ~/sysu/build

cmake --build ~/sysu/build
cmake --build ~/sysu/build -t install