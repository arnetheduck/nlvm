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
CMAKE_ROOT=cmake-$VER2.src

[ -f $LLVM_ROOT.tar.xz ] || {
  wget https://github.com/llvm/llvm-project/releases/download/llvmorg-$VER2/$LLVM_ROOT.tar.xz
}

[ -f $LLVM_ROOT/CMakeLists.txt ] || {
  tar xf $LLVM_ROOT.tar.xz
}

[ -f $LLD_ROOT.tar.xz ] || {
  wget https://github.com/llvm/llvm-project/releases/download/llvmorg-$VER2/$LLD_ROOT.tar.xz
}

[ -f $LLD_ROOT/CMakeLists.txt ] || {
  tar xf $LLD_ROOT.tar.xz
}

[ -d $LLVM_ROOT/projects/lld ] || {
  rm -rf $LLVM_ROOT/projects/lld
  cd $LLVM_ROOT/projects
  ln -sfr ../../$LLD_ROOT lld
  cd ../..
}

[ -f $CMAKE_ROOT.tar.xz ] || {
  wget https://github.com/llvm/llvm-project/releases/download/llvmorg-$VER2/$CMAKE_ROOT.tar.xz
}

[ -f $CMAKE_ROOT/README.rst ] || {
  tar xf $CMAKE_ROOT.tar.xz
}

[ -f $LLVM_ROOT/cmake/Modules/GNUInstallPackageDir.cmake ] || {
  cp "$CMAKE_ROOT/Modules"/* "$LLVM_ROOT/cmake/modules"
}

[ -f libunwind-$VER2.src.tar.xz ] || {
  wget https://github.com/llvm/llvm-project/releases/download/llvmorg-$VER2/libunwind-$VER2.src.tar.xz
}

[ -f libunwind-$VER2/CMakeLists.txt ] || {
  tar xf libunwind-$VER2.src.tar.xz
  cp -ar libunwind-$VER2.src/include/mach-o $LLD_ROOT/include
}

cd $LLVM_ROOT

mkdir -p $TGT
cd $TGT

shift 4
cmake -GNinja -DLLVM_USE_LINKER=gold LLVM_INCLUDE_BENCHMARKS=OFF "$@" ..

ninja
