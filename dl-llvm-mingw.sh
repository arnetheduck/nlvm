#!/bin/bash

set -e

mkdir -p ext
cd ext

# llvm-mingw version should preferably match llvm.version
TAG="20260616"
ROOT=llvm-mingw-$TAG-ucrt-ubuntu-22.04-x86_64

[ -d llvm-mingw/lib/clang/22/lib/windows ] || {
  [ -f $ROOT.tar.xz ] || {
    wget https://github.com/mstorsjo/llvm-mingw/releases/download/$TAG/$ROOT.tar.xz
  }

  tar xvf $ROOT.tar.xz
  mv $ROOT llvm-mingw
}

mkdir -p ../lib/clang/22/lib/
cp -a llvm-mingw/lib/clang/22/lib/windows ../lib/clang/22/lib/
cp -a llvm-mingw/x86_64-w64-mingw32 ..
rm ../x86_64-w64-mingw32/include
cp -aH llvm-mingw/x86_64-w64-mingw32/include ../../x86_64-w64-mingw32/
