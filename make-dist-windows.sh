#!/bin/bash

# Experimental release script

set -e

ROOT=nlvm-windows-$(git rev-parse --short HEAD)

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr.exe
make STATIC_LLVM=1 nlvm/nlvmr.exe

# Copy nlvm and library files
mkdir -p $ROOT $ROOT/lib
cp nlvm/nlvmr.exe $ROOT/nlvm.exe
strip $ROOT/nlvm.exe
cp -r lib/nlvm $ROOT/lib/
cp -r lib/clang $ROOT/lib/

mkdir -p $ROOT/lib/nim
cd lib/nim
# avoid build junk
git archive --format=tar HEAD lib config | (cd ../../$ROOT/lib/nim && tar xf -)
cd ../..

rm -rf dist
mkdir -p dist
zip -r dist/$ROOT.zip $ROOT/

rm -rf $ROOT
