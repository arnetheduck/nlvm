#!/bin/bash

set -e

mkdir -p ext
cd ext

# llvm-mingw version should preferably match llvm.version
TAG="20251216"
ROOT=llvm-mingw-$TAG-ucrt-ubuntu-22.04-x86_64

[ -f llvm-mingw/bin/clang ] || {
  [ -f $ROOT.tar.xz ] || {
    wget https://github.com/mstorsjo/llvm-mingw/releases/download/$TAG/$ROOT.tar.xz
  }

  tar xvf $ROOT.tar.xz
  mv $ROOT llvm-mingw
}

mkdir -p ../lib/clang/21/lib/
cp -ar llvm-mingw/lib/clang/21/lib/windows ../lib/clang/21/lib/
cp -ar llvm-mingw/x86_64-w64-mingw32 ..
rm ../x86_64-w64-mingw32/include
cp -arH llvm-mingw/x86_64-w64-mingw32/include ../../x86_64-w64-mingw32/
