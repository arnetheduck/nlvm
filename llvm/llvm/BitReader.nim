## ===-- llvm-c/BitReader.h - BitReader Library C Interface ------*- C++ -*-===*\
## |*                                                                            *|
## |*                     The LLVM Compiler Infrastructure                       *|
## |*                                                                            *|
## |* This file is distributed under the University of Illinois Open Source      *|
## |* License. See LICENSE.TXT for details.                                      *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMBitReader.a, which          *|
## |* implements input of the LLVM bitcode format.                               *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## *
##  @defgroup LLVMCBitReader Bit Reader
##  @ingroup LLVMC
## 
##  @{
## 
##  Builds a module from the bitcode in the specified memory buffer, returning a
##    reference to the module via the OutModule parameter. Returns 0 on success.
##    Optionally returns a human-readable error message via OutMessage.
## 
##    This is deprecated. Use LLVMParseBitcode2.

proc parseBitcode*(memBuf: MemoryBufferRef; outModule: ptr ModuleRef;
                  outMessage: cstringArray): Bool {.importc: "LLVMParseBitcode",
    dynlib: LLVMLib.}
##  Builds a module from the bitcode in the specified memory buffer, returning a
##    reference to the module via the OutModule parameter. Returns 0 on success.

proc parseBitcode2*(memBuf: MemoryBufferRef; outModule: ptr ModuleRef): Bool {.
    importc: "LLVMParseBitcode2", dynlib: LLVMLib.}
##  This is deprecated. Use LLVMParseBitcodeInContext2.

proc parseBitcodeInContext*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                           outModule: ptr ModuleRef; outMessage: cstringArray): Bool {.
    importc: "LLVMParseBitcodeInContext", dynlib: LLVMLib.}
proc parseBitcodeInContext2*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                            outModule: ptr ModuleRef): Bool {.
    importc: "LLVMParseBitcodeInContext2", dynlib: LLVMLib.}
## * Reads a module from the specified path, returning via the OutMP parameter
##     a module provider which performs lazy deserialization. Returns 0 on success.
##     Optionally returns a human-readable error message via OutMessage.
##     This is deprecated. Use LLVMGetBitcodeModuleInContext2.

proc getBitcodeModuleInContext*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                               outM: ptr ModuleRef; outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModuleInContext", dynlib: LLVMLib.}
## * Reads a module from the specified path, returning via the OutMP parameter a
##  module provider which performs lazy deserialization. Returns 0 on success.

proc getBitcodeModuleInContext2*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                                outM: ptr ModuleRef): Bool {.
    importc: "LLVMGetBitcodeModuleInContext2", dynlib: LLVMLib.}
##  This is deprecated. Use LLVMGetBitcodeModule2.

proc getBitcodeModule*(memBuf: MemoryBufferRef; outM: ptr ModuleRef;
                      outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModule", dynlib: LLVMLib.}
proc getBitcodeModule2*(memBuf: MemoryBufferRef; outM: ptr ModuleRef): Bool {.
    importc: "LLVMGetBitcodeModule2", dynlib: LLVMLib.}
## *
##  @}
## 
