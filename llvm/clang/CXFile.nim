## ===-- clang-c/CXFile.h - C Index File ---------------------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides the interface to C Index files.                       *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  \defgroup CINDEX_FILES File manipulation routines
##
##  @{
##
##
##  A particular source file that is part of a translation unit.
##

type CXFile* = pointer

##
##  Retrieve the complete file and path name of the given file.
##

proc getFileName*(
  sFile: CXFile
): CXString {.importc: "clang_getFileName", dynlib: CLangLib.}

##
##  Retrieve the last modification time of the given file.
##

proc getFileTime*(
  sFile: CXFile
): TimeT {.importc: "clang_getFileTime", dynlib: CLangLib.}

##
##  Uniquely identifies a CXFile, that refers to the same underlying file,
##  across an indexing session.
##

type CXFileUniqueID* {.bycopy.} = object
  data*: array[3, culonglong]

##
##  Retrieve the unique ID for the given \c file.
##
##  \param file the file to get the ID for.
##  \param outID stores the returned CXFileUniqueID.
##  \returns If there was a failure getting the unique ID, returns non-zero,
##  otherwise returns 0.
##

proc getFileUniqueID*(
  file: CXFile, outID: ptr CXFileUniqueID
): cint {.importc: "clang_getFileUniqueID", dynlib: CLangLib.}

##
##  Returns non-zero if the \c file1 and \c file2 point to the same file,
##  or they are both NULL.
##

proc fileIsEqual*(
  file1: CXFile, file2: CXFile
): cint {.importc: "clang_File_isEqual", dynlib: CLangLib.}

##
##  Returns the real path name of \c file.
##
##  An empty string may be returned. Use \c clang_getFileName() in that case.
##

proc fileTryGetRealPathName*(
  file: CXFile
): CXString {.importc: "clang_File_tryGetRealPathName", dynlib: CLangLib.}

##
##  @}
##
