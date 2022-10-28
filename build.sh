# rm -rf ~/sysu
# cmake -G Ninja \
#   -DCMAKE_C_COMPILER=clang \
#   -DCMAKE_CXX_COMPILER=clang++ \
#   -DCMAKE_INSTALL_PREFIX=~/sysu \
#   -DCPACK_SOURCE_IGNORE_FILES=".git/;tester/third_party/" \
#   -B ~/sysu/build

cmake --build ~/sysu/build
cmake --build ~/sysu/build -t install
