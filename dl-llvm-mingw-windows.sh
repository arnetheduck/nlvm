#!/bin/bash

set -e

# llvm-mingw version should preferably match llvm.version
TAG="$(cat llvm/llvm-mingw.version)"
LLVM_MAJ="$(cat llvm/llvm.version | cut -f1 -d.)"

mkdir -p ext
cd ext

ROOT=llvm-mingw-$TAG-ucrt-x86_64


[ -d llvm-mingw/lib/clang/$LLVM_MAJ/lib/windows ] || {
  [ -f $ROOT.zip ] || {
    wget https://github.com/mstorsjo/llvm-mingw/releases/download/$TAG/$ROOT.zip
  }

  unzip $ROOT.zip
  rm -rf llvm-mingw
  mv $ROOT llvm-mingw
}

[ -d llvm-mingw/lib/clang/$LLVM_MAJ ] || { echo "Wrong version"; quit 1 ; }

# A bit messy - we need to copy things both to the local copy of clang we'll be
# building nim with and nlvm itself - there's probably a better way to do this
mkdir -p ../lib/clang/$LLVM_MAJ/lib/
rm -rf ../lib/clang/$LLVM_MAJ/lib/windows
cp -a llvm-mingw/lib/clang/$LLVM_MAJ/lib/windows ../lib/clang/$LLVM_MAJ/lib/
rm -rf ../x86_64-w64-mingw32
cp -a llvm-mingw/x86_64-w64-mingw32 ..
cp -a llvm-mingw/include ../x86_64-w64-mingw32

mkdir -p ../llvm/sta/lib/clang/$LLVM_MAJ/lib
rm -rf ../llvm/sta/lib/clang/$LLVM_MAJ/lib/windows
cp -a llvm-mingw/lib/clang/$LLVM_MAJ/lib/windows ../llvm/sta/lib/clang/$LLVM_MAJ/lib
rm -rf ../llvm/sta/x86_64-w64-mingw32
cp -a llvm-mingw/x86_64-w64-mingw32 ../llvm/sta
cp -a llvm-mingw/include ../llvm/sta/x86_64-w64-mingw32
