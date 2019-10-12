## ===-- llvm-c/IRReader.h - IR Reader C Interface -----------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This file defines the C interface to the IR Reader.                        *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## *
##  Read LLVM IR from a memory buffer and convert it into an in-memory Module
##  object. Returns 0 on success.
##  Optionally returns a human-readable description of any errors that
##  occurred during parsing IR. OutMessage must be disposed with
##  LLVMDisposeMessage.
##
##  @see llvm::ParseIR()
##

proc parseIRInContext*(contextRef: ContextRef; memBuf: MemoryBufferRef;
                      outM: ptr ModuleRef; outMessage: cstringArray): Bool {.
    importc: "LLVMParseIRInContext", dynlib: LLVMLib.}