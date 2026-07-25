## ===-- clang-c/CXErrorCode.h - C Index Error Codes  --------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides the CXErrorCode enumerators.                          *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  Error codes returned by libclang routines.
##
##  Zero (\c CXError_Success) is the only error code indicating success.  Other
##  error codes, including not yet assigned non-zero values, indicate errors.
##

type CXErrorCode* {.size: sizeof(cint).} = enum
  ##
  ##  No error.
  ##
  CXErrorSuccess = 0
    ##
    ##  A generic error code, no further details are available.
    ##
    ##  Errors of this kind can get their own specific error codes in future
    ##  libclang versions.
    ##
  CXErrorFailure = 1
    ##
    ##  libclang crashed while performing the requested operation.
    ##
  CXErrorCrashed = 2
    ##
    ##  The function detected that the arguments violate the function
    ##  contract.
    ##
  CXErrorInvalidArguments = 3
    ##
    ##  An AST deserialization error has occurred.
    ##
  CXErrorASTReadError = 4
