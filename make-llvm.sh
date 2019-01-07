#!/bin/bash

mkdir -p ext
cd ext

VER="7.0"
VER2="$VER.0"

[[ -f libLLVM-$VER.so ]] && exit 0

[[ -f llvm-$VER2.src.tar.xz ]] || wget http://releases.llvm.org/$VER2/llvm-$VER2.src.tar.xz || exit 1

tar xf llvm-$VER2.src.tar.xz || exit 1
cd llvm-$VER2.src
mkdir -p rel
cd rel
cmake -GNinja -DCMAKE_BUILD_TYPE=RelWithDebInfo -DLLVM_BUILD_LLVM_DYLIB=1 -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_EXPERIMENTAL_TARGETS_TO_BUILD=WebAssembly -DLLVM_ENABLE_ASSERTIONS=1 ..

ninja

cd ..
cd ..

ln -s llvm-$VER2.src/rel/lib/*.so .
