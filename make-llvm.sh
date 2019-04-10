#!/bin/sh

# Build llvm, as used in the Makefile
# A bit broken because it doesn't track cmake options and deps correctly

[ $# -gt 4 ] || {
 echo "$0 major minor patch output_dir cmake_options*"
 exit 1
}

set -e

mkdir -p ext
cd ext

VER="$1.$2"
VER2="$VER.$3"
TGT="$4"

LLVM_ROOT=llvm-$VER2.src
LLD_ROOT=lld-$VER2.src

[ -f $LLVM_ROOT.tar.xz ] || {
  wget http://releases.llvm.org/$VER2/$LLVM_ROOT.tar.xz
}

[ -f $LLVM_ROOT/CMakeLists.txt ] || {
  tar xf $LLVM_ROOT.tar.xz
}

[ -f $LLD_ROOT.tar.xz ] || {
  wget http://releases.llvm.org/$VER2/$LLD_ROOT.tar.xz
}

[ -f $LLD_ROOT/CMakeLists.txt ] || {
  tar xf $LLD_ROOT.tar.xz
}

[ -d $LLVM_ROOT/projects/lld ] || {
  rm -rf $LLVM_ROOT/projects/lld
  ln -sfr $LLD_ROOT $LLVM_ROOT/projects/lld
}

cd $LLVM_ROOT

mkdir -p $TGT
cd $TGT

shift 4
cmake -GNinja -DLLVM_USE_LINKER=gold "$@" ..

ninja
