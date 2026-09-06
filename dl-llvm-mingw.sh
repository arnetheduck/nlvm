#!/bin/bash

set -e

TAG="$(cat llvm/llvm-mingw.version)"

# binaries must match at least the major llvm.version
LLVM_MAJ="$(cat llvm/llvm.version | cut -f1 -d.)"

mkdir -p ext
cd ext

ROOT=llvm-mingw-$TAG-ucrt-x86_64

[ -d $ROOT ] || {
  [ -f $ROOT.zip ] || {
    wget https://github.com/mstorsjo/llvm-mingw/releases/download/$TAG/$ROOT.zip
  }

  unzip $ROOT.zip
}

[ -d $ROOT/lib/clang/$LLVM_MAJ/lib/windows ] || { echo "LLVM version mismatch"; exit 1 ; }

mkdir -p ../lib/clang/$LLVM_MAJ/lib/
rm -rf ../lib/clang/$LLVM_MAJ/lib/windows
cp -a $ROOT/lib/clang/$LLVM_MAJ/lib/windows ../lib/clang/$LLVM_MAJ/lib/
rm -rf ../x86_64-w64-mingw32
cp -a $ROOT/x86_64-w64-mingw32 ..
cp -a $ROOT/include ../x86_64-w64-mingw32

mkdir -p ../include
rm -rf ../include/c++
mv ../x86_64-w64-mingw32/include/c++ ../include/

# A bit messy - we need to copy things both to the local copy of clang we'll be
# building nim with and nlvm itself - there's probably a better way to do this
mkdir -p ../llvm/sta/lib/clang/$LLVM_MAJ/lib
rm -rf ../llvm/sta/lib/clang/$LLVM_MAJ/lib/windows
cp -a $ROOT/lib/clang/$LLVM_MAJ/lib/windows ../llvm/sta/lib/clang/$LLVM_MAJ/lib
rm -rf ../llvm/sta/x86_64-w64-mingw32
cp -a $ROOT/x86_64-w64-mingw32 ../llvm/sta
cp -a $ROOT/include ../llvm/sta/x86_64-w64-mingw32
