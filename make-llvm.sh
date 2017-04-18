#!/bin/sh

pushd ext

[[ -f libLLVM-3.9.so ]] && exit 0

[[ -f llvm-3.9.0.src.tar.xz ]] || wget http://llvm.org/releases/3.9.0/llvm-3.9.0.src.tar.xz || exit 1

tar xvf llvm-3.9.0.src.tar.xz || exit 1
pushd llvm-3.9.0.src
mkdir -p rel
pushd rel
cmake -DCMAKE_BUILD_TYPE=Release -DLLVM_BUILD_LLVM_DYLIB=1 -DLLVM_TARGETS_TO_BUILD=X86 -DLLVM_ENABLE_ASSERTIONS=1 ..

make -j$(nproc) || exit 1

popd
popd

ln -s llvm-3.9.0.src/rel/lib/libLLVM-3.9.so .
