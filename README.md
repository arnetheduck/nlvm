# Introduction

NLVM (the nim-level virtual machine?) is in its present incarnation an llvm 
(http://llvm.org) IR generator for the Nim language (http://nim-lang.org).

In some distant future, it would be nice if (in no particular order):
* it could compile itself
* it generated executables
* it implemented more core Nim features (bounds checking etc)
* it had fewer bugs than the reference nim compiler
* it had a nice automated test suite
* someone found it useful
* earth survived humans

When I started on this little project, I knew neither llvm-ir nor Nim.
Therefore, I'd specially like to thank the friendly folks at the #nim 
channel on freenode that never seemed to tire of my nooby questions. 
Also, thanks to all tutorial writers out there, on llvm, programming 
and other topics for providing such fine sources of copy-pa... er, 
inspiration!

Questions, patches, improvement suggestions and reviews welcome. When 
you find bugs, feel free to fix them as well :)

Fork and enjoy!

Jacek Sieka (arnetheduck on gmail point com), 2016-01-19

# Compile instructions

To do what I do, you will need:
* Linux
* A C/C++ compiler (ironically, I happen to use gcc most of the time)

Start with a clone:

    cd $SRC
    git clone https://github.com/arnetheduck/nlvm.git
    cd nlvm && git submodule update --init

Compile llvm shared library:

    cd $SRC
    wget http://llvm.org/releases/3.7.1/llvm-3.7.1.src.tar.xz
    tar xvf llvm-3.7.1.src.tar.xz
    cd llvm-3.7.1.src
    mkdir build
    cd build
    ../configure --disable-optimized --enable-debug-runtime --enable-targets=x86_64 --enable-shared 
    nice make -j$(nproc)

Compile nim:

    cd $SRC/nlvm/Nim
    ./bootstrap.sh

Compile nlvm:

    cd $SRC/nlvm/nlvm
    ../Nim/bin/nim c nlvm

Run nlvm:

    cd $SRC/nlvm/nlvm
    LD_LIBRARY_PATH=$SRC/llvm-3.7.1.src/build/Debug+Asserts/lib ./nlvm c xxx.nim

See optimized code (assuming llvm is in your `$PATH`):

    opt -O3 nimcache/xxx.bc | llvm-dis

Compile to assymbly (`.s`) (assuming llvm is in your `$PATH`):

    llc nimcache/xxx.bc

Compile and link - can use either of `clang`, `gcc` or `ld`.
* `ld` requires assembly files (generated with `llc`) and lots of flags
  to link correctly to the c library: http://stackoverflow.com/q/3577922
* `gcc` will do the correct linking, but still requires assembly files
* `clang` will link correctly, and works with `.bc` files directly, yay!

With the winner:

    clang nimcache/*.bc -ldl -o xxx


# Random notes

* I have no hopes of keeping up with upstream, so I've pinned it at a particular commit
  with the submodule - patches welcome to update to new upstream versions
