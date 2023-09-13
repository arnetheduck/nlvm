## ===-- llvm-c/Comdat.h - Module Comdat C Interface -------------*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This file defines the C interface to COMDAT.                               *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  @defgroup LLVMCCoreComdat Comdats
##  @ingroup LLVMCCore
##
##  @{
##

type
  ComdatSelectionKind* {.size: sizeof(cint).} = enum
    AnyComdatSelectionKind, ## < The linker may choose any COMDAT.
    ExactMatchComdatSelectionKind, ## < The data referenced by the COMDAT must
                                    ## < be the same.
    LargestComdatSelectionKind, ## < The linker will choose the largest
                                 ## < COMDAT.
    NoDeduplicateComdatSelectionKind, ## < No deduplication is performed.
    SameSizeComdatSelectionKind ## < The data referenced by the COMDAT must be
                                ## < the same size.


##
##  Return the Comdat in the module with the specified name. It is created
##  if it didn't already exist.
##
##  @see llvm::Module::getOrInsertComdat()
##

proc getOrInsertComdat*(m: ModuleRef; name: cstring): ComdatRef {.
    importc: "LLVMGetOrInsertComdat", dynlib: LLVMLib.}
##
##  Get the Comdat assigned to the given global object.
##
##  @see llvm::GlobalObject::getComdat()
##

proc getComdat*(v: ValueRef): ComdatRef {.importc: "LLVMGetComdat",
    dynlib: LLVMLib.}
##
##  Assign the Comdat to the given global object.
##
##  @see llvm::GlobalObject::setComdat()
##

proc setComdat*(v: ValueRef; c: ComdatRef) {.importc: "LLVMSetComdat",
    dynlib: LLVMLib.}
##
##  Get the conflict resolution selection kind for the Comdat.
##
##  @see llvm::Comdat::getSelectionKind()
##

proc getComdatSelectionKind*(c: ComdatRef): ComdatSelectionKind {.
    importc: "LLVMGetComdatSelectionKind", dynlib: LLVMLib.}
##
##  Set the conflict resolution selection kind for the Comdat.
##
##  @see llvm::Comdat::setSelectionKind()
##

proc setComdatSelectionKind*(c: ComdatRef; kind: ComdatSelectionKind) {.
    importc: "LLVMSetComdatSelectionKind", dynlib: LLVMLib.}
##
##  @}
##
