#===-- llvm-c/BitReader.h - BitReader Library C Interface ------*- C++ -*-===*\
#|*                                                                            *|
#|*                     The LLVM Compiler Infrastructure                       *|
#|*                                                                            *|
#|* This file is distributed under the University of Illinois Open Source      *|
#|* License. See LICENSE.TXT for details.                                      *|
#|*                                                                            *|
#|*===----------------------------------------------------------------------===*|
#|*                                                                            *|
#|* This header declares the C interface to libLLVMBitReader.a, which          *|
#|* implements input of the LLVM bitcode format.                               *|
#|*                                                                            *|
#|* Many exotic languages can interoperate with C code but have a harder time  *|
#|* with C++ due to name mangling. So in addition to C, this interface enables *|
#|* tools written in such languages.                                           *|
#|*                                                                            *|
#\*===----------------------------------------------------------------------===

#*
#  @defgroup LLVMCBitReader Bit Reader
#  @ingroup LLVMC
# 
#  @{
# 
# Builds a module from the bitcode in the specified memory buffer, returning a
#   reference to the module via the OutModule parameter. Returns 0 on success.
#   Optionally returns a human-readable error message via OutMessage. 

proc parseBitcode*(memBuf: MemoryBufferRef; outModule: ptr ModuleRef; 
                   outMessage: cstringArray): Bool {.
    importc: "LLVMParseBitcode", dynlib: LLVMLib.}
proc parseBitcodeInContext*(contextRef: ContextRef; memBuf: MemoryBufferRef; 
                            outModule: ptr ModuleRef; outMessage: cstringArray): Bool {.
    importc: "LLVMParseBitcodeInContext", dynlib: LLVMLib.}
#* Reads a module from the specified path, returning via the OutMP parameter
#    a module provider which performs lazy deserialization. Returns 0 on success.
#    Optionally returns a human-readable error message via OutMessage. 

proc getBitcodeModuleInContext*(contextRef: ContextRef; memBuf: MemoryBufferRef; 
                                outM: ptr ModuleRef; outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModuleInContext", dynlib: LLVMLib.}
proc getBitcodeModule*(memBuf: MemoryBufferRef; outM: ptr ModuleRef; 
                       outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModule", dynlib: LLVMLib.}
#* Deprecated: Use LLVMGetBitcodeModuleInContext instead. 

proc getBitcodeModuleProviderInContext*(contextRef: ContextRef; 
                                        memBuf: MemoryBufferRef; 
                                        outMP: ptr ModuleProviderRef; 
                                        outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModuleProviderInContext", dynlib: LLVMLib.}
#* Deprecated: Use LLVMGetBitcodeModule instead. 

proc getBitcodeModuleProvider*(memBuf: MemoryBufferRef; 
                               outMP: ptr ModuleProviderRef; 
                               outMessage: cstringArray): Bool {.
    importc: "LLVMGetBitcodeModuleProvider", dynlib: LLVMLib.}
#*
#  @}
# 
