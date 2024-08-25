#!/bin/bash

[ $# -ge 4 ] || {
 echo "$0 major minor patch output_dir"
 exit 1
}

set -e

mkdir -p ext
cd ext

VER="$1.$2"
VER2="$VER.$3"
TGT="$4"
SUFFIX="x86_64-linux-gnu-ubuntu-18.04"
SUFFIX2=""

LLVM_ROOT=llvm-$VER2.src

[ -f clang+llvm-$VER2-$SUFFIX/bin/llvm-config ] || {

[ -f clang+llvm-$VER2-$SUFFIX.tar.xz ] || {
  wget https://github.com/llvm/llvm-project/releases/download/llvmorg-$VER2/clang+llvm-$VER2-$SUFFIX$SUFFIX2.tar.xz
}

  tar xvf clang+llvm-$VER2-$SUFFIX$SUFFIX2.tar.xz
}

mkdir -p $LLVM_ROOT/
rm -rf $LLVM_ROOT/$TGT
cd $LLVM_ROOT
ln -s ../clang+llvm-$VER2-$SUFFIX $TGT
cd ..

