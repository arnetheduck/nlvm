#!/bin/bash

set -e

MAJ="33"
MIN="0"
SYSROOT=wasi-sysroot-$MAJ.$MIN+m
SDK=wasi-sdk-$MAJ.$MIN-x86_64-linux

# binaries must match at least the major llvm.version
LLVM_MAJ="$(cat llvm/llvm.version | cut -f1 -d.)"

mkdir -p ext
cd ext

[ -f $SDK/VERSION ] || {
  [ -f $SDK.tar.gz ] || {
    wget https://github.com/WebAssembly/wasi-sdk/releases/download/wasi-sdk-$MAJ/$SDK.tar.gz
  }

  tar xvf $SDK.tar.gz
}
[ -d $SDK/lib/clang/$LLVM_MAJ ] || { echo "LLVM version mismatch"; exit 1 ; }

# The wasi sysroot is different from mingw in that it lives as a subfolder of
# include/lib instead of being a top-level on its own - further investigation
# needed
rm -rf ../{include,lib}/wasm32-wasip1
mkdir -p ../include
mkdir -p ../lib/wasm32-wasip1
cp -a $SDK/share/wasi-sysroot/include/wasm32-wasip1 ../include
cp -a $SDK/share/wasi-sysroot/lib/wasm32-wasip1/*.{a,o} ../lib/wasm32-wasip1/
cp -a $SDK/share/wasi-sysroot/lib/wasm32-wasip1/eh/*.a ../lib/wasm32-wasip1/

rm -r ../include/wasm32-wasip1/noeh

# Use the c++ headers from mingw
rm -r ../include/wasm32-wasip1/eh/c++

# wasi-sdk distributes two versions: eh and noeh - this appears to be a distribution
# choice that is not reflected in the sysroot itself, so we need to copy one of them to the
# sysroot for nlvm to use.
mv ../include/wasm32-wasip1/eh/* ../include/wasm32-wasip1

mkdir -p ../lib/clang/$LLVM_MAJ/lib/
cp -a $SDK/lib/clang/$LLVM_MAJ/lib/wasm32-unknown-wasip1 ../lib/clang/$LLVM_MAJ/lib/
