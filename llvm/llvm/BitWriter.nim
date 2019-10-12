## ===-- llvm-c/BitWriter.h - BitWriter Library C Interface ------*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMBitWriter.a, which          *|
## |* implements output of the LLVM bitcode format.                              *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## *
##  @defgroup LLVMCBitWriter Bit Writer
##  @ingroup LLVMC
##
##  @{
##
## ===-- Operations on modules ---------------------------------------------===
## * Writes a module to the specified path. Returns 0 on success.

proc writeBitcodeToFile*(m: ModuleRef; path: cstring): cint {.
    importc: "LLVMWriteBitcodeToFile", dynlib: LLVMLib.}
## * Writes a module to an open file descriptor. Returns 0 on success.

proc writeBitcodeToFD*(m: ModuleRef; fd: cint; shouldClose: cint; unbuffered: cint): cint {.
    importc: "LLVMWriteBitcodeToFD", dynlib: LLVMLib.}
## * Deprecated for LLVMWriteBitcodeToFD. Writes a module to an open file
##     descriptor. Returns 0 on success. Closes the Handle.

proc writeBitcodeToFileHandle*(m: ModuleRef; handle: cint): cint {.
    importc: "LLVMWriteBitcodeToFileHandle", dynlib: LLVMLib.}
## * Writes a module to a new memory buffer and returns it.

proc writeBitcodeToMemoryBuffer*(m: ModuleRef): MemoryBufferRef {.
    importc: "LLVMWriteBitcodeToMemoryBuffer", dynlib: LLVMLib.}
## *
##  @}
##
