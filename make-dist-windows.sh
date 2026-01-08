#!/bin/bash

# Experimental release script

set -e

ROOT=nlvm-windows-$(git rev-parse --short HEAD)

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr.exe
make STATIC_LLVM=1 nlvm/nlvmr.exe

# Copy nlvm and library files
# TODO these would go in /usr/{bin, share/Nim} normally
mkdir -p $ROOT
cp nlvm/nlvmr.exe $ROOT/nlvm.exe
strip $ROOT/nlvm.exe
cp -r nlvm-lib $ROOT
mkdir -p $ROOT/Nim
cd Nim
# avoid build junk
git archive --format=tar HEAD lib config | (cd ../$ROOT/Nim && tar xf -)
cd ..

rm -rf dist
mkdir -p dist
zip -r dist/$ROOT.zip $ROOT/

rm -rf $ROOT
