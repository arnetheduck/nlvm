#!/bin/bash

# Experimental release script

set -e

ROOT=nlvm-build-root

rm -rf $ROOT

# Make sure the nlvm binary is fresh
rm -f nlvm/nlvmr
make STATIC_LLVM=1 nlvm/nlvmr

# Copy nlvm and library files
cp -ar nlvm/nlvmr /usr/bin/nlvm
strip /usr/bin/nlvm

cp -ar lib/nlvm /usr/lib/
cp -ar lib/clang /usr/lib/

mkdir -p /usr/lib/nim
cd lib/nim
git archive --format=tar HEAD lib config compiler doc | (cd /usr/lib/nim && tar xf -)
