# Introduction

[nlvm](https://github.com/arnetheduck/nlvm) (the nim-level virtual machine?)
is an [LLVM-based](http://llvm.org) compiler for the [Nim](http://nim-lang.org)
language.

From Nim's point of view, it's a backend just like C or JavaScript - from
LLVM's point of view, it's a language frontend that emits IR.

When I started on this little project, I knew neither llvm nor Nim.
Therefore, I'd specially like to thank the friendly folks at the #nim
channel that never seemed to tire of my nooby questions.
Also, thanks to all tutorial writers out there, on llvm, programming
and other topics for providing such fine sources of copy-pa... er,
inspiration!

Questions, patches, improvement suggestions and reviews welcome. When
you find bugs, feel free to fix them as well :)

Fork and enjoy!

Jacek Sieka (arnetheduck on gmail point com)

# Status

`nlvm` is generally at par with `nim` in terms of features, with the following
notable differences:

* Fast compile times - no intermediate `C` compiler step
* DWARF ("zero-cost") exception handling
* High-quality `gdb`/`lldb` debug information with source stepping, type
  information etc
* Smart code generation - compiler intrinsics for overflow checking,
  smart constant initialization, etc
* Native `wasm32` support with no extra tooling

Most things from `nim` work just fine (see notes below however!):

* the same standard library is used
* similar command line options are supported (just change `nim` to `nlvm`!)
* `importc` works without needing `C` header files - the declaration in the
  `.nim` file needs to be accurate

Test coverage is not too bad either:

* bootstrapping and compiling itself
* ~95% of all upstream tests - most failures can be traced to
  the standard library and compiler relying on C implementation details - see
  [skipped-tests.txt](skipped-tests.txt) for an updated list of issues
* compiling most applications
* platforms: linux/x86_64, wasm32 (pre-alpha!)
* majority of the nim standard library (the rest can be fixed easily -
  requires upstream changes however)

How you could contribute:

* work on making [skipped-tests.txt](skipped-tests.txt) smaller
* improve platform support (`osx` and `windows` should be easy, `arm` would be
  nice)
* help `nlvm` generate better IR - optimizations, builtins, exception handling..
* help upstream make std library smaller and more `nlvm`-compatible
* send me success stories :)
* leave the computer for a bit and do something real for your fellow earthlings

`nlvm` does _not_:

* understand `C` - as a consequence, `header`, `emit` and similar pragmas
  will not work - neither will the fancy `importcpp`/`C++` features
* support all nim compiler flags and features - do file bugs for anything
  useful that's missing

# Compile instructions

To do what I do, you will need:
* Linux
* A C/C++ compiler (ironically, I happen to use `gcc` most of the time)

Start with a clone:

    cd $SRC
    git clone https://github.com/arnetheduck/nlvm.git
    cd nlvm && git submodule update --init

We will need a few development libraries installed, mainly due to how `nlvm`
processes library dependencies (see dynlib section below):

    # Fedora
    sudo dnf install pcre-devel openssl-devel sqlite-devel ninja-build

    # Debian, ubuntu etc
    sudo apt-get install libpcre3-dev libssl-dev libsqlite3-dev ninja-build

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

When compiling, `nlvm` will generate a single `.o` file with all code from your
project and link it using `$CC` - this helps it pick the right flags for
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
    less fizzbuzz.s

Apart from the code of your `.nim` files, the compiler will also mix in the
compatibility found library in `nlvm-lib/`.

## Pipeline

Generally, the `nim` compiler pipeline looks something like this:

    nim --> c files --> IR --> object files --> executable

In `nlvm`, we remove one step and bunch all the code together:

    nim --> IR --> single object file --> executable

Going straight to the IR means it's possible to express nim constructs more
clearly, allowing `llvm` to understand the code better and thus do a better
job at optimization. It also helps keep compile times down, because the
`c-to-IR` step can be avoided.

The practical effect of generating a single object file is similar to
`gcc -fwhole-program -flto` - it is expensive in terms of memory, but results
in slightly smaller and faster binaries. Notably, the `IR-to-machine-code` step,
including any optimizations, is repeated in full for each recompile.

## Common issues

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

### header and emit

When `nim` compiles code, it will generate `c` code which may include other
`c` code, from headers or directly via `emit` statements. This means `nim` has
direct access do symbols declared in the `c` file, which can be both a feature
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

### wasm32 support

`wasm32` support is still very bare-bones, so you will need to do a bit of
tinkering to get it to work.

Presently, the `wasm32-unknown-unknown` target is mapped to `--os:standalone`
and `--cpu:wasm32` - this choice represents a very raw `wasm` engine with 32-bit
little-endian integers and pointers - in the future, the `nim` standard library
and `system.nim` will need to be updated to support WASM system interfaces like
emscripten or WASI.

To compile wasm files, you will thus need a `panicoverride.nim` - a minimal
example looks like this and discards any errors:

```nim
proc rawoutput(s: string) = discard
proc panic(s: string) {.noreturn.} = discard
```

After placing the above code in your project folder, you can compile `.nim`
code to `wasm32`:

    nim c -c --nlvm.target=wasm32-unknown-unkown myfile.nim
    less myfile.ll

To go from there, follow the steps found
[here](https://aransentin.github.io/cwasm/).

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
