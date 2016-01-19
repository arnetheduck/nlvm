#===-- llvm-c/Support.h - Support C Interface --------------------*- C -*-===*\
#|*                                                                            *|
#|*                     The LLVM Compiler Infrastructure                       *|
#|*                                                                            *|
#|* This file is distributed under the University of Illinois Open Source      *|
#|* License. See LICENSE.TXT for details.                                      *|
#|*                                                                            *|
#|*===----------------------------------------------------------------------===*|
#|*                                                                            *|
#|* This file defines the C interface to the LLVM support library.             *|
#|*                                                                            *|
#\*===----------------------------------------------------------------------===

#*
#  @defgroup LLVMCSupportTypes Types and Enumerations
# 
#  @{
# 

type 
  Bool* = cint

#*
#  Used to pass regions of memory through LLVM interfaces.
# 
#  @see llvm::MemoryBuffer
# 

type 
  MemoryBufferRef* = ptr OpaqueMemoryBuffer

#*
#  @}
# 
#*
#  This function permanently loads the dynamic library at the given path.
#  It is safe to call this function multiple times for the same library.
# 
#  @see sys::DynamicLibrary::LoadLibraryPermanently()
#  

proc loadLibraryPermanently*(filename: cstring): Bool {.
    importc: "LLVMLoadLibraryPermanently", dynlib: LLVMLib.}
#*
#  This function parses the given arguments using the LLVM command line parser.
#  Note that the only stable thing about this function is its signature; you
#  cannot rely on any particular set of command line arguments being interpreted
#  the same way across LLVM versions.
# 
#  @see llvm::cl::ParseCommandLineOptions()
# 

proc parseCommandLineOptions*(argc: cint; argv: cstringArray; overview: cstring) {.
    importc: "LLVMParseCommandLineOptions", dynlib: LLVMLib.}