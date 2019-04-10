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

LLVM_ROOT=llvm-$VER2.src

[ -f clang+llvm-$VER2-x86_64-linux-gnu-ubuntu-14.04.tar.xz ] || {
  wget http://releases.llvm.org/$VER2/clang+llvm-$VER2-x86_64-linux-gnu-ubuntu-14.04.tar.xz
}

[ -f clang+llvm-$VER2-x86_64-linux-gnu-ubuntu-14.04/bin/llvm-config ] || {
  tar xvf clang+llvm-$VER2-x86_64-linux-gnu-ubuntu-14.04.tar.xz
}

mkdir -p $LLVM_ROOT/
rm -rf $LLVM_ROOT/$TGT
ln -sr clang+llvm-$VER2-x86_64-linux-gnu-ubuntu-14.04 $LLVM_ROOT/$TGT
