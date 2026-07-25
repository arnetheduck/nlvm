## ===-- clang-c/Rewrite.h - C CXRewriter   --------------------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===

type CXRewriter* = pointer

##
##  Create CXRewriter.
##

proc cXRewriterCreate*(
  tu: CXTranslationUnit
): CXRewriter {.importc: "clang_CXRewriter_create", dynlib: CLangLib.}

##
##  Insert the specified string at the specified location in the original buffer.
##

proc cXRewriterInsertTextBefore*(
  rew: CXRewriter, loc: CXSourceLocation, insert: cstring
) {.importc: "clang_CXRewriter_insertTextBefore", dynlib: CLangLib.}

##
##  Replace the specified range of characters in the input with the specified
##  replacement.
##

proc cXRewriterReplaceText*(
  rew: CXRewriter, toBeReplaced: CXSourceRange, replacement: cstring
) {.importc: "clang_CXRewriter_replaceText", dynlib: CLangLib.}

##
##  Remove the specified range.
##

proc cXRewriterRemoveText*(
  rew: CXRewriter, toBeRemoved: CXSourceRange
) {.importc: "clang_CXRewriter_removeText", dynlib: CLangLib.}

##
##  Save all changed files to disk.
##  Returns 1 if any files were not saved successfully, returns 0 otherwise.
##

proc cXRewriterOverwriteChangedFiles*(
  rew: CXRewriter
): cint {.importc: "clang_CXRewriter_overwriteChangedFiles", dynlib: CLangLib.}

##
##  Write out rewritten version of the main file to stdout.
##

proc cXRewriterWriteMainFileToStdOut*(
  rew: CXRewriter
) {.importc: "clang_CXRewriter_writeMainFileToStdOut", dynlib: CLangLib.}

##
##  Free the given CXRewriter.
##

proc cXRewriterDispose*(
  rew: CXRewriter
) {.importc: "clang_CXRewriter_dispose", dynlib: CLangLib.}
