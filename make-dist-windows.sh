#!/bin/bash

# Experimental release script

set -e

LLVM_MAJ="$(cat llvm/llvm.version | cut -f1 -d.)"

ROOT=nlvm-windows-$(git rev-parse --short HEAD)

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr.exe
make STATIC_LLVM=1 nlvm/nlvmr.exe

# Copy nlvm and library files
mkdir -p $ROOT/bin $ROOT/lib
cp nlvm/nlvmr.exe $ROOT/bin/nlvm.exe
strip $ROOT/bin/nlvm.exe
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

rm -rf dist
mkdir -p dist

cp -a lib/clang/$LLVM_MAJ/lib $ROOT/lib/clang/$LLVM_MAJ/
cp -a x86_64-w64-mingw32 $ROOT
cp -a include $ROOT
cp -a lib/wasm32-wasip1 $ROOT/lib

zip -r -7 dist/$ROOT.zip $ROOT/

rm -rf $ROOT
