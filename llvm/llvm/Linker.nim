#===-- llvm-c/Linker.h - Module Linker C Interface -------------*- C++ -*-===*\
#|*                                                                            *|
#|*                     The LLVM Compiler Infrastructure                       *|
#|*                                                                            *|
#|* This file is distributed under the University of Illinois Open Source      *|
#|* License. See LICENSE.TXT for details.                                      *|
#|*                                                                            *|
#|*===----------------------------------------------------------------------===*|
#|*                                                                            *|
#|* This file defines the C interface to the module/file/archive linker.       *|
#|*                                                                            *|
#\*===----------------------------------------------------------------------===

# This enum is provided for backwards-compatibility only. It has no effect. 

type 
  LinkerMode* {.size: sizeof(cint).} = enum 
    LinkerDestroySource = 0,  # This is the default behavior. 
    LinkerPreserveSourceRemoved = 1


# Links the source module into the destination module, taking ownership
#  of the source module away from the caller. Optionally returns a
#  human-readable description of any errors that occurred in linking.
#  OutMessage must be disposed with LLVMDisposeMessage. The return value
#  is true if an error occurred, false otherwise.
# 
#  Note that the linker mode parameter \p Unused is no longer used, and has
#  no effect. 

proc linkModules*(dest: ModuleRef; src: ModuleRef; unused: LinkerMode; 
                  outMessage: cstringArray): Bool {.importc: "LLVMLinkModules", 
    dynlib: LLVMLib.}