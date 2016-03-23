# Introduction

NLVM (the nim-level virtual machine?) is in its present incarnation an llvm
(http://llvm.org) IR generator for the Nim language (http://nim-lang.org).

In some distant future, it would be nice if (in no particular order):
* it generated executables, and not just bitcode files
* it implemented more core Nim features (bounds checking, GC etc)
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

Jacek Sieka (arnetheduck on gmail point com), 2016-03-23

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

    cd $SRC
    make

Compile with itself and compare:

    cd $SRC
    make compare

# Compiling your code

When compiling, nlvm will generate a single `.bc` file for your whole project,
containing all dependencies in LLVM bytecode format. The following examples
assume you've added LLVM to your `$PATH`.

    cd $SRC/nlvm/nlvm
    LD_LIBRARY_PATH=$SRC/llvm-3.7.1.src/build/Debug+Asserts/lib ./nlvm c nlvm

Convert bitcode to text (`.ll`):

    llvm-dis nimcache/nlvm.bc
    less nimcache/nlvm.ll

See optimized code:

    opt -Os nimcache/nlvm.bc | llvm-dis

Compile to assembly (`.s`):

    llc nimcache/nlvm.bc

Compile and link - can use either of `clang`, `gcc` or `ld`.
* `ld` requires assembly files (generated with `llc`) and lots of flags
  to link correctly to the c library: http://stackoverflow.com/q/3577922
* `gcc` will do the correct linking, but still requires assembly files
* `clang` will link correctly, and works with `.bc` files directly, yay!

Apart from the code of your module, you'll also need to add the workaround
library in `lib/`:

    clang nimcache/nlvm.bc lib/nimbase-linux-amd64.ll -ldl -o xxx

# Random notes

* I have no hopes of keeping up with upstream, so I've pinned it at a
  particular commit with the submodule - patches welcome to update to new
  upstream versions
* The upstream test suite runs `compiler/nim` to compile the test code. To run
  it with `nlvm`, I have a little shell script that simply calls `nlvm` and
  `clang` instead
* nlvm doesn't understand C
* The nim standard library likes to import C headers directly which works
  because the upstream nim compiler uses a C compiler underneath - ergo,
  large parts of the standard library don't work with nlvm.
* nlvm should work on any x86_64 linux, but there is no support for other
  platforms (int size, calling conventions etc) - patches welcome