## ===------- llvm-c/Error.h - llvm::Error class C Interface -------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This file defines the C interface to LLVM's Error class.                   *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

const
  ErrorSuccess* = 0

## *
##  Opaque reference to an error instance. Null serves as the 'success' value.
##

type
  ErrorRef* = ptr OpaqueError

## *
##  Error type identifier.
##

type
  ErrorTypeId* = pointer

## *
##  Returns the type id for the given error instance, which must be a failure
##  value (i.e. non-null).
##

proc getErrorTypeId*(err: ErrorRef): ErrorTypeId {.importc: "LLVMGetErrorTypeId",
    dynlib: LLVMLib.}
## *
##  Dispose of the given error without handling it. This operation consumes the
##  error, and the given LLVMErrorRef value is not usable once this call returns.
##  Note: This method *only* needs to be called if the error is not being passed
##  to some other consuming operation, e.g. LLVMGetErrorMessage.
##

proc consumeError*(err: ErrorRef) {.importc: "LLVMConsumeError", dynlib: LLVMLib.}
## *
##  Returns the given string's error message. This operation consumes the error,
##  and the given LLVMErrorRef value is not usable once this call returns.
##  The caller is responsible for disposing of the string by calling
##  LLVMDisposeErrorMessage.
##

proc getErrorMessage*(err: ErrorRef): cstring {.importc: "LLVMGetErrorMessage",
    dynlib: LLVMLib.}
## *
##  Dispose of the given error message.
##

proc disposeErrorMessage*(errMsg: cstring) {.importc: "LLVMDisposeErrorMessage",
    dynlib: LLVMLib.}
## *
##  Returns the type id for llvm StringError.
##

proc getStringErrorTypeId*(): ErrorTypeId {.importc: "LLVMGetStringErrorTypeId",
    dynlib: LLVMLib.}