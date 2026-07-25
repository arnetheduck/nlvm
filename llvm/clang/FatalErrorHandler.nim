## ===-- clang-c/FatalErrorHandler.h - Fatal Error Handling --------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  Installs error handler that prints error message to stderr and calls abort().
##  Replaces currently installed error handler (if any).
##

proc installAbortingLlvmFatalErrorHandler*() {.
  importc: "clang_install_aborting_llvm_fatal_error_handler", dynlib: CLangLib
.}

##
##  Removes currently installed error handler (if any).
##  If no error handler is intalled, the default strategy is to print error
##  message to stderr and call exit(1).
##

proc uninstallLlvmFatalErrorHandler*() {.
  importc: "clang_uninstall_llvm_fatal_error_handler", dynlib: CLangLib
.}
