#!/bin/bash

# Experimental release script

set -e

LLVM_MAJ="$(cat llvm/llvm.version | cut -f1 -d.)"

ROOT=nlvm-linux-$(git rev-parse --short HEAD)

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr
make STATIC_LLVM=1 nlvm/nlvmr

# Copy nlvm and library files
mkdir -p $ROOT/bin $ROOT/lib
cp nlvm/nlvmr $ROOT/bin/nlvm
strip $ROOT/bin/nlvm

cp -a lib/nlvm $ROOT/lib/

# clang headers copied from sta/ that need to be placed relative to `nlvm`
mkdir -p $ROOT/lib/clang/$LLVM_MAJ/
cp -a lib/clang/$LLVM_MAJ/include $ROOT/lib/clang/$LLVM_MAJ/

mkdir -p $ROOT/lib/nim
cd lib/nim
# avoid build junk
git archive --format=tar HEAD lib config | (cd ../../$ROOT/lib/nim && tar xf -)
cd ../..

# Include examples for good measure
git archive --format=tar HEAD examples | (cd $ROOT && tar xf -)

# Native release without cross compiler runtime
rm -rf dist
mkdir -p dist
tar cvfJ dist/$ROOT-native.tar.xz $ROOT/

# Cross compiler
cp -a lib/clang/$LLVM_MAJ/lib $ROOT/lib/clang/$LLVM_MAJ/
cp -a x86_64-w64-mingw32 $ROOT
cp -a include $ROOT
cp -a lib/wasm32-wasip1 $ROOT/lib

tar cvfJ dist/$ROOT.tar.xz $ROOT/
