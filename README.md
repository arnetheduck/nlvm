# Introduction

[NLVM](https://github.com/arnetheduck/nlvm) (the nim-level virtual machine?)
is an [LLVM-based](http://llvm.org) compiler for the [Nim](http://nim-lang.org)
language.

From Nim's point of view, it's a backend just like C or JavaScript - from
LLVM's point of view, it's a language frontend that emits IR.

When I started on this little project, I knew neither llvm-ir nor Nim.
Therefore, I'd specially like to thank the friendly folks at the #nim
channel on freenode that never seemed to tire of my nooby questions.
Also, thanks to all tutorial writers out there, on llvm, programming
and other topics for providing such fine sources of copy-pa... er,
inspiration!

Questions, patches, improvement suggestions and reviews welcome. When
you find bugs, feel free to fix them as well :)

Fork and enjoy!

Jacek Sieka (arnetheduck on gmail point com)

# Status

NLVM can currently:
* compile itself (on linux) and many other Nim apps
* pass ~90% of all upstream test cases

In some distant future, it would be nice if (in no particular order):

* it implemented more core Nim features (threads, stack traces, debug info)
* it had fewer bugs than the reference Nim compiler
* someone found it useful
* earth survived humans

# Compile instructions

To do what I do, you will need:
* Linux
* A C/C++ compiler (ironically, I happen to use gcc most of the time)

Start with a clone:

    cd $SRC
    git clone https://github.com/arnetheduck/nlvm.git
    cd nlvm && git submodule update --init

Compile LLVM shared library - while LLVM is normally linked statically, this
keeps link times of NLVM itself down, and is convenient for development:

    cd $SRC
    wget http://llvm.org/releases/3.9.0/llvm-3.9.0.src.tar.xz
    tar xvf llvm-3.9.0.src.tar.xz
    cd llvm-3.9.0.src
    mkdir build
    cd build
    cmake -G 'Ninja' -DCMAKE_BUILD_TYPE=Debug -DLLVM_BUILD_LLVM_DYLIB=1 -DLLVM_OPTIMIZED_TABLEGEN=1 -DLLVM_TARGETS_TO_BUILD=X86 ..
    nice ninja-build

Compile nim:

    cd $SRC/nlvm/Nim
    sh ./bootstrap.sh

Compile NLVM:

    cd $SRC/nlvm
    make

Compile with itself and compare:

    make compare

Run nim/nlvm test suite (will write test results to `Nim/testresults.html`):

    make test

# Compiling your code

When compiling, NLVM will generate a single `.o` file with all code from your
project and link it using `$(CC)` which helps it pick the right flags for
linking with the C library.

    cd $SRC/nlvm/Nim/examples
    ../../nlvm/nlvm c fizzbuzz

If you want to see the generated LLVM IR, use the `-c` option:

    cd $SRC/nlvm/Nim/examples
    ../../nlvm/nlvm c -c fizzbuzz
    less fizzbuzz.ll

You can then run the LLVM optimizer on it:

    opt -Os fizzbuzz.ll | llvm-dis

... or compile it to assembly (`.s`):

    llc fizzbuzz.ll
    less nlvm.s

Apart from the code of your `.nim` files, the compiler will also mix in the
compatibility found library in `nlvm-lib/`.

# Random notes

* Upstream is pinned using a submodule - nlvm relies heavily on internals
  that keep changing - it's unlikely that it works with any other versions,
  patches welcome to update it
* The upstream test suite runs `compiler/nim` to compile the test code.
  `make test` uses a trick where nlvm is copied to that location, so as to
  fool the test runner
* nlvm doesn't understand C
* The nim standard library likes to import C headers directly which works
  because the upstream nim compiler uses a C compiler underneath - ergo,
  large parts of the standard library don't work with nlvm.
* nlvm should work on any `x86_64` linux, but there is no support for other
  platforms (int size, calling conventions etc) - patches welcome
