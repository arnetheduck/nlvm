#!/bin/sh

# Build llvm, as used in the Makefile

set -e

cd llvm

DIR="$1"
TGTS="$2"

shift 2
CC=clang CXX=clang++ cmake -S llvm-project/llvm -B "$DIR" -GNinja \
  -DLLVM_ENABLE_PROJECTS="lld" \
  -DLLVM_USE_SPLIT_DWARF=On \
  -DLLVM_INCLUDE_BENCHMARKS=Off -DLLVM_INCLUDE_EXAMPLES=Off -DLLVM_INCLUDE_TESTS=Off \
  -DLLVM_USE_LINKER=lld \
  "$@"

ninja -C $DIR $TGTS
