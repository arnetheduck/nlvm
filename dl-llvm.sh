#!/bin/bash

OS="`uname`"
ARCH="`uname -m`"

if [ $ARCH == "arm64" ]; then
  echo "LLVM doesn't provide precompiled binaries for arm64 yet :(. Remove STATIC_LLVM=1 and try again. Exiting..."
  exit 1
fi

case $OS in
  'Linux')
    SUFFIX="$ARCH-linux-gnu-ubuntu-18.04"
    ;;
  'Darwin') 
    SUFFIX="$ARCH-apple-darwin"
    ;;
  *) 
    echo "Unsupported OS: $OS" 
    exit 1
  ;;
esac

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

[ -f clang+llvm-$VER2-$SUFFIX/bin/llvm-config ] || {

[ -f clang+llvm-$VER2-$SUFFIX.tar.xz ] || {
  wget https://github.com/llvm/llvm-project/releases/download/llvmorg-$VER2/clang+llvm-$VER2-$SUFFIX.tar.xz
}

  tar xvf clang+llvm-$VER2-$SUFFIX.tar.xz
}

mkdir -p $LLVM_ROOT/
rm -rf $LLVM_ROOT/$TGT
cd $LLVM_ROOT
ln -s ../clang+llvm-$VER2-$SUFFIX $TGT
cd ..

