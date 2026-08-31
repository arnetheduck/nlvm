#!/bin/sh

# Build llvm, as used in the Makefile

set -e

cd llvm

DIR="$1"
TGTS="$2"

shift 2
CC="${CC:-clang}" CXX="${CXX:-clang++}" cmake -S llvm-project/llvm -B "$DIR" -GNinja \
	-DLLVM_ENABLE_PROJECTS="clang;lld" \
	-DLLVM_TARGETS_TO_BUILD="AArch64;ARM;BPF;RISCV;WebAssembly;X86" \
	-DLLVM_USE_SPLIT_DWARF=On \
	-DLLVM_INCLUDE_BENCHMARKS=Off \
	-DLLVM_INCLUDE_EXAMPLES=Off \
	-DLLVM_INCLUDE_TESTS=Off \
	-DLLVM_INCLUDE_DOCS=Off \
	-DLLVM_USE_LINKER=lld \
	-DLLVM_ENABLE_LIBXML2=Off \
	"$@"

ninja -C $DIR $TGTS
