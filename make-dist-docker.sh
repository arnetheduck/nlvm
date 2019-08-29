#!/bin/bash

# Experimental release script

set -e

ROOT=nlvm-build-root

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr
make STATIC_LLVM=1 nlvm/nlvmr

# Copy nlvm and library files
# TODO these would go in /usr/{bin, share/Nim} normally
mkdir -p $ROOT
cp nlvm/nlvmr $ROOT/nlvm
cp -r nlvm-lib $ROOT
mkdir -p $ROOT/Nim
cd Nim
# avoid build junk
git archive --format=tar HEAD lib config compiler doc | (cd ../$ROOT/Nim && tar xf -)
cd ..
mkdir -p /usr/lib/nlvm
cp -av $ROOT/* /usr/lib/nlvm
ln -s /usr/lib/nlvm/nlvm /usr/bin/nlvm
