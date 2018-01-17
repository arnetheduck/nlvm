#!/bin/bash

mkdir -p ext
cd ext

VER="5.0"
VER2="$VER.1"

[[ -f libLLVM-$VER.so ]] && exit 0

[[ -f llvm-$VER2.src.tar.xz ]] || wget http://releases.llvm.org/$VER2/llvm-$VER2.src.tar.xz || exit 1

tar xf llvm-$VER2.src.tar.xz || exit 1
cd llvm-$VER2.src
mkdir -p rel
cd rel
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_BUILD_LLVM_DYLIB=1 -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_ENABLE_ASSERTIONS=1 ..

make -j$(nproc)

cd ..
cd ..

ln -s llvm-$VER2.src/rel/lib/*.so .
