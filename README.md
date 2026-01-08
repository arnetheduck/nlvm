<!-- omit in toc -->
# Introduction

[nlvm](https://github.com/arnetheduck/nlvm) (the nim-level virtual machine?)
is an [LLVM-based](http://llvm.org) compiler for the [Nim](http://nim-lang.org)
programming language.

From Nim's point of view, it's a backend just like C or JavaScript - from
LLVM's point of view, it's a language frontend that emits IR.

Questions, patches, improvement suggestions and reviews welcome. When
you find bugs, feel free to fix them as well :)

Fork and enjoy!

Jacek Sieka (arnetheduck on gmail point com)

<!-- omit in toc -->
## Table of contents

- [Features](#features)
- [Installation](#installation)
  - [Binaries](#binaries)
  - [Compiling `nlvm`](#compiling-nlvm)
- [Compiling your code](#compiling-your-code)
  - [Pipeline](#pipeline)
  - [Porting guide](#porting-guide)
    - [dynlib](#dynlib)
    - [{.header.}](#header)
    - [{.emit.}](#emit)
    - [{.asm.}](#asm)
- [wasm32](#wasm32)
- [Cross compiler](#cross-compiler)
  - [Prerequisites](#prerequisites)
  - [Compiling](#compiling)
- [REPL / running your code](#repl--running-your-code)
- [Random notes](#random-notes)

# Features

`nlvm` works as a drop-in replacement for `nim` with the following notable differences:

* Fast compile times - no intermediate `C` compiler step
* DWARF ("zero-cost") exception handling
* High-quality `gdb`/`lldb` debug information with source stepping, type
  information etc
* Smart code generation and optimisation
  * LTO and whole-program optimisation out-of-the-box
  * compiler-intrinsic guided optimisation for overflow checking, memory operations, exception handling
  * heap allocation elision
  * native constant initialization
* Native cross compiler, including `wasm32` support with no extra tooling
* Native integrated fast linker (`lld`)
* Just-in-time execution and REPL (`nlvm r`) using the LLVM [ORCv2 JIT](https://llvm.org/docs/ORCv2.html)
* Built-in [cross-compiler](#cross-compiler)

Most things from `nim` work just fine (see the [porting guide](#porting-guide) below!):

* the same standard library is used
* similar command line options are supported (just change `nim` to `nlvm`!)
* `C` header files are not used - the declaration in the `.nim` file needs to be
  accurate
* If your program has `{.compile.}` dependencies, these work as usual but
  require a corresponding compiler to be installed (ie clang, gcc)

Test coverage is not too bad either:

* bootstrapping and compiling itself
* ~95% of all upstream tests - most failures can be traced to
  the standard library and compiler relying on C implementation details - see
  [skipped-tests.txt](skipped-tests.txt) for an updated list of issues
* compiling most applications
* platforms with tests:
  * Linux/x86_64
  * Windows/x86_64
* majority of the nim standard library (the rest can be fixed easily -
  requires upstream changes however)

How you could contribute:

* work on making [skipped-tests.txt](skipped-tests.txt) smaller
* improve platform support (`osx` should be easy, `arm` would be nice)
* help `nlvm` generate better IR - optimizations, builtins, exception handling..
* help upstream make std library smaller and more `nlvm`-compatible
* send me success stories :)
* leave the computer for a bit and do something real for your fellow earthlings

`nlvm` does _not_:

* understand `C` - as a consequence, `header`, `emit` and similar pragmas
  are ignored - neither will the fancy `importcpp`/`C++` features - see the
  [porting guide](#porting-guide) below!
* support all nim compiler flags and features - do file bugs for anything
  useful that's missing

# Installation

## Binaries

Binaries are available from the [github releases](https://github.com/arnetheduck/nlvm/releases) page.

## Source code

To do what I do, you will need:

* A C/C++ compiler
  * `gcc` on Linux
  * `clang` on Windows
* A cup of tea and a good book
  * Compiling `llvm` takes about an hour the first time (then it's cached)

Start with a clone:

    cd $SRC
    git clone https://github.com/arnetheduck/nlvm.git --recurse-submodules
    cd nlvm

We will need a few development libraries installed, mainly due to how `nlvm`
processes library dependencies (see dynlib section below):

    # Fedora
    sudo dnf install pcre-devel openssl-devel sqlite-devel ninja-build cmake clang libzstd-devel

    # Debian, ubuntu etc
    sudo apt-get install libpcre3-dev libssl-dev libsqlite3-dev ninja-build cmake clang libzstd-dev

    # MSYS2 CLANG64 (note that we need the gcc shim for clang)
    pacboy -S toolchain cmake ninja gcc

Compile `nlvm` (if needed, this will also build `nim` and `llvm`):

    make

Compile with itself and compare:

    make compare

Run test suite:

    make test
    make stats

You can link statically to LLVM to create a stand-alone binary - this will
use a more optimized version of LLVM as well, but takes longer to build:

    make STATIC_LLVM=1

If you want a faster `nlvm`, you can also try the release build - it will be
called `nlvmr`:

    make STATIC_LLVM=1 nlvmr

When you update `nlvm` from `git`, don't forget the submodule:

    git pull && git submodule update

To build a docker image, use:

    make docker

To run built `nlvm` docker image use:

    docker run -v $(pwd):/code/ nlvm c -r /code/test.nim

# Compiling your code

On the command line, `nlvm` is mostly compatible with `nim`.

For compiling pure Nimm code, you do not need a C compiler - `nlvm` will compile
and link the code using the built-in `lld` linker:

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
    less fizzbuzz.s

Apart from the code of your `.nim` files, the compiler will also mix in the
compiler runtime library in `nlvm-lib/`.

## Pipeline

Generally, the `nim` compiler pipeline looks something like this:

    nim --> c files --> IR --> object files --> linker --> executable

In `nlvm`, we remove one step and bunch all the code together:

    nim --> single IR file --> built-in LTO linker --> executable

Going straight to the IR means it's possible to express nim constructs more
clearly, allowing `llvm` to understand the code better and thus do a better
job at optimization. It also helps keep compile times down, because the
`c-to-IR` step can be avoided.

The practical effect of generating a single object file is similar to
`clang -fwhole-program -flto` - it is a bit more expensive in terms of memory,
but results in slightly smaller and faster binaries. Notably, the
`IR-to-machine-code` step, including any optimizations, is repeated in full for
each recompile.

## Porting guide

### dynlib

`nim` uses a runtime dynamic library loading scheme to gain access to shared
libraries. When compiling, no linking is done - instead, when running your
application, `nim` will try to open anything the user has installed.

`nlvm` does not support the `{.dynlib.}` pragma - instead you can use
`{.passL.}` using normal system linking.

```nim
# works with `nim`
proc f() {. importc, dynlib: "mylib" .}

# works with both `nim` and `nlvm`
{.passL: "-lmylib".}
proc f() {. importc .}
```

### {.header.}

When `nim` compiles code, it will generate `c` code which may include other
`c` code, from headers or directly via `emit` statements. This means `nim` has
direct access to symbols declared in the `c` file, which can be both a feature
and a problem.

In `nlvm`, `{.header.}` directives are ignored - `nlvm` looks strictly at
the signature of the declaration, meaning the declaration must _exactly_ match
the `c` header file or subtly ABI issues and crashes ensue!

```nim
# When `nim` encounters this, it will emit `jmp_buf` in the `c` code without
# knowing the true size of the type, letting the `c` compiler determine it
# instead.
type C_JmpBuf {.importc: "jmp_buf", header: "<setjmp.h>".} = object

# nlvm instead ignores the `header` directive completely and will use the
# declaration as written. Failure to correctly declare the type will result
# in crashes and subtle bugs - memory will be overwritten or fields will be
# read from the wrong offsets.
#
# The following works with both `nim` and `nlvm`, but requires you to be
# careful to match the binary size and layout exactly (note how `bycopy`
# sometimes help to further nail down the ABI):

when defined(linux) and defined(amd64):
  type
    C_JmpBuf {.importc: "jmp_buf", bycopy.} = object
      abi: array[200 div sizeof(clong), clong]

# In `nim`, `C` constant defines are often imported using the following trick,
# which makes `nim` emit the right `C` code that the value from the header
# can be read (no writing of course, even though it's a `var`!)
#
# assuming a c header with: `#define RTLD_NOW 2`
# works for nim:
var RTLD_NOW* {.importc: "RTLD_NOW", header: "<dlfcn.h>".}: cint

# both nlvm and nim (note how these values often can be platform-specific):
when defined(linux) and defined(amd64):
  const RTLD_NOW* = cint(2)
```

### {.emit.}

To deal with `emit`, the recommendation is to put the emitted code in a C file
and `{.compile.}` it.

```nim
proc myEmittedFunction() {.importc.}
{.compile: "myemits.c".}
```

```c
void myEmittedFunction() {
  /* ... */
}
```

### {.asm.}

Similar to `{.emit.}`, `{.asm.}` functions must be moved to a separate file and
included in the compilation with `{.compile.}` - this works both with `.S` and
`.c` files.

# Cross compiler

`nlvm` can be used to cross-compile code for a different platform, for example to
create `Windows` executables on a `Linux` machine.

## Prerequisites

For cross-compilation, a `clang`-based environment for that platform must first
be [set up](https://clang.llvm.org/docs/CrossCompilation.html) - the environment
consists of:

* a `sysroot` - the basic libraries needed to create executables for the platform
  * for `Windows`, this is [llvm-mingw](https://github.com/mstorsjo/llvm-mingw/releases)
  * It can be [obtained](https://mcilloni.ovh/2021/02/09/cxx-cross-clang/)
    from the environment you're targeting.
* The compiler runtime for that target (`compiler-rt` or `libgcc`)
  * provided by `llvm-mingw`
* `clang` - for finding libraries in the sysroot and dealing with `{.compile.}`
  * a cross-compilation version of `gcc` may also work, though this hasn't been
    tested

## Compiling

Once the `sysroot` is set up, compiling is as easy as selecting an alternative
OS / CPU using the standard Nim flags, `--os:` and `--cpu:`.

### Windows

A helper script exists to set up `llvm-mingw`:

```sh
./dl-llvm-mingw.sh

# Set up $PATH to include the `clang` compiler that comes with llvm-mingw
. env.sh
```

```sh
# Assuming we're on Linux, compile for Windows and link dependencies statically
# to create a (mostly) stand-alone binary:
nlvm c --os:windows --passl:-static test.nim

# You can also use a target triple
nlvm c --nlvm.triple=x86_64-w64-mingw32 --passl:-static Nim/examples/fizzbuzz.nim

# Run the compiled program via wine
wine test.exe
```

### wasm32

Use `--cpu:wasm32 --os:standalone --gc:none` to compile Nim to (barebones) WASM.

You will need to provide a runtime (ie WASI) and use manual memory allocation as
the garbage collector hasn't yet been ported to WASM and the Nim standard
library lacks WASM / WASI support.

Apart from WASI, an implementation of `panicoverride.nim` also needs to be
provided - here's one that discards all panics:

```nim
# panicoverride.nim
proc rawoutput(s: string) = discard
proc panic(s: string) {.noreturn.} = discard
```

After placing the above code in your project folder, you can compile `.nim`
code to `wasm32`:

```nim
# myfile.nim
proc adder*(v: int): int {.exportc.} =
  v + 4
```

```sh
nlvm c --cpu:wasm32 --os:standalone --gc:none --passl:-Wl,--no-entry myfile.nim
wasm2wat -l myfile.wasm
```

Most WASM-compile code ends up needing WASM [extensions](https://webassembly.org/roadmap/) -
in particular, the bulk memory extension is needed to process data.

Extensions are enabled by passing `--passc:-mattr=+feature,+feature2`, for example:

```sh
nlvm c --cpu:wasm32 --os:standalone --gc:none --passl:-Wl,--no-entry --passc:-mattr=+bulk-memory
```

Passing `--passc:-mattr=help` will print available features (only works while compiling, for now!)

To use functions from the environment (with `importc`), compile with `--passl:-Wl,--allow-undefined`.

# REPL / running your code

`nlvm` supports directly running Nim code using just-in-time compilation:

```sh
# Compile and run `myfile.nim` without creating a binary first
nlvm r myfile.nim
```

This mode can also be used to run code directly from the standard input:

```sh
$ nlvm r
.......................................................
>>> log2(100.0)
stdin(1, 1) Error: undeclared identifier: 'log2'
candidates (edit distance, scope distance); see '--spellSuggest':
 (2, 2): 'low' [proc declared in /home/arnetheduck/src/nlvm/Nim/lib/system.nim(1595, 6)]
...
>>> import math
.....
>>> log2(100.0)
6.643856189774724: float64
```

# Random notes

* Upstream is pinned using a submodule - nlvm relies heavily on internals
  that keep changing - it's unlikely that it works with any other versions,
  patches welcome to update it
* The nim standard library likes to import C headers directly which works
  because the upstream nim compiler uses a C compiler underneath - ergo,
  large parts of the standard library don't work with nlvm.
* Happy to take patches for anything, including better platform support!
* For development, it's convenient to build LLVM with assertions turned on -
  the API is pretty unforgiving
* When I started on this little project, I knew neither llvm nor Nim.
  Therefore, I'd specially like to thank the friendly folks at the #nim
  channel that never seemed to tire of my nooby questions.
  Also, thanks to all tutorial writers out there, on llvm, programming
  and other topics for providing such fine sources of copy-pa... er,
  inspiration!
