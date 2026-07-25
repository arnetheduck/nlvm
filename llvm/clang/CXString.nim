## ===-- clang-c/CXString.h - C Index strings  --------------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides the interface to C Index strings.                     *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  \defgroup CINDEX_STRING String manipulation routines
##  \ingroup CINDEX
##
##  @{
##
##
##  A character string.
##
##  The \c CXString type is used to return strings from the interface when
##  the ownership of that string might differ from one call to the next.
##  Use \c clang_getCString() to retrieve the string data and, once finished
##  with the string data, call \c clang_disposeString() to free the string.
##

type
  CXString* {.bycopy.} = object
    data*: pointer
    privateFlags*: cuint

  CXStringSet* {.bycopy.} = object
    strings*: ptr CXString
    count*: cuint

##
##  Retrieve the character data associated with the given string.
##
##  The returned data is a reference and not owned by the user. This data
##  is only valid while the `CXString` is valid. This function is similar
##  to `std::string::c_str()`.
##

proc getCString*(
  string: CXString
): cstring {.importc: "clang_getCString", dynlib: CLangLib.}

##
##  Free the given string.
##

proc disposeString*(
  string: CXString
) {.importc: "clang_disposeString", dynlib: CLangLib.}

##
##  Free the given string set.
##

proc disposeStringSet*(
  set: ptr CXStringSet
) {.importc: "clang_disposeStringSet", dynlib: CLangLib.}

##
##  @}
##
