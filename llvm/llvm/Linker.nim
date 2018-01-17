## ===-- llvm-c/Linker.h - Module Linker C Interface -------------*- C++ -*-===*\
## |*                                                                            *|
## |*                     The LLVM Compiler Infrastructure                       *|
## |*                                                                            *|
## |* This file is distributed under the University of Illinois Open Source      *|
## |* License. See LICENSE.TXT for details.                                      *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This file defines the C interface to the module/file/archive linker.       *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##  This enum is provided for backwards-compatibility only. It has no effect.

type
  LinkerMode* {.size: sizeof(cint).} = enum
    LinkerDestroySource = 0,    ##  This is the default behavior.
    LinkerPreserveSourceRemoved = 1


##  Links the source module into the destination module. The source module is
##  destroyed.
##  The return value is true if an error occurred, false otherwise.
##  Use the diagnostic handler to get any diagnostic message.
## 

proc linkModules2*(dest: ModuleRef; src: ModuleRef): Bool {.
    importc: "LLVMLinkModules2", dynlib: LLVMLib.}