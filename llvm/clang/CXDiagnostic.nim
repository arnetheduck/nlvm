## ===-- clang-c/CXDiagnostic.h - C Index Diagnostics --------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides the interface to C Index diagnostics.                 *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  \defgroup CINDEX_DIAG Diagnostic reporting
##
##  @{
##
##
##  Describes the severity of a particular diagnostic.
##

type CXDiagnosticSeverity* {.size: sizeof(cint).} = enum
  ##
  ##  A diagnostic that has been suppressed, e.g., by a command-line
  ##  option.
  ##
  CXDiagnosticIgnored = 0
    ##
    ##  This diagnostic is a note that should be attached to the
    ##  previous (non-note) diagnostic.
    ##
  CXDiagnosticNote = 1
    ##
    ##  This diagnostic indicates suspicious code that may not be
    ##  wrong.
    ##
  CXDiagnosticWarning = 2
    ##
    ##  This diagnostic indicates that the code is ill-formed.
    ##
  CXDiagnosticError = 3
    ##
    ##  This diagnostic indicates that the code is ill-formed such
    ##  that future parser recovery is unlikely to produce useful
    ##  results.
    ##
  CXDiagnosticFatal = 4

##
##  A single diagnostic, containing the diagnostic's severity,
##  location, text, source ranges, and fix-it hints.
##

type CXDiagnostic* = pointer

##
##  A group of CXDiagnostics.
##

type CXDiagnosticSet* = pointer

##
##  Determine the number of diagnostics in a CXDiagnosticSet.
##

proc getNumDiagnosticsInSet*(
  diags: CXDiagnosticSet
): cuint {.importc: "clang_getNumDiagnosticsInSet", dynlib: CLangLib.}

##
##  Retrieve a diagnostic associated with the given CXDiagnosticSet.
##
##  \param Diags the CXDiagnosticSet to query.
##  \param Index the zero-based diagnostic number to retrieve.
##
##  \returns the requested diagnostic. This diagnostic must be freed
##  via a call to \c clang_disposeDiagnostic().
##

proc getDiagnosticInSet*(
  diags: CXDiagnosticSet, index: cuint
): CXDiagnostic {.importc: "clang_getDiagnosticInSet", dynlib: CLangLib.}

##
##  Describes the kind of error that occurred (if any) in a call to
##  \c clang_loadDiagnostics.
##

type CXLoadDiagError* {.size: sizeof(cint).} = enum
  ##
  ##  Indicates that no error occurred.
  ##
  CXLoadDiagNone = 0
    ##
    ##  Indicates that an unknown error occurred while attempting to
    ##  deserialize diagnostics.
    ##
  CXLoadDiagUnknown = 1
    ##
    ##  Indicates that the file containing the serialized diagnostics
    ##  could not be opened.
    ##
  CXLoadDiagCannotLoad = 2
    ##
    ##  Indicates that the serialized diagnostics file is invalid or
    ##  corrupt.
    ##
  CXLoadDiagInvalidFile = 3

##
##  Deserialize a set of diagnostics from a Clang diagnostics bitcode
##  file.
##
##  \param file The name of the file to deserialize.
##  \param error A pointer to a enum value recording if there was a problem
##         deserializing the diagnostics.
##  \param errorString A pointer to a CXString for recording the error string
##         if the file was not successfully loaded.
##
##  \returns A loaded CXDiagnosticSet if successful, and NULL otherwise.  These
##  diagnostics should be released using clang_disposeDiagnosticSet().
##

proc loadDiagnostics*(
  file: cstring, error: ptr CXLoadDiagError, errorString: ptr CXString
): CXDiagnosticSet {.importc: "clang_loadDiagnostics", dynlib: CLangLib.}

##
##  Release a CXDiagnosticSet and all of its contained diagnostics.
##

proc disposeDiagnosticSet*(
  diags: CXDiagnosticSet
) {.importc: "clang_disposeDiagnosticSet", dynlib: CLangLib.}

##
##  Retrieve the child diagnostics of a CXDiagnostic.
##
##  This CXDiagnosticSet does not need to be released by
##  clang_disposeDiagnosticSet.
##

proc getChildDiagnostics*(
  d: CXDiagnostic
): CXDiagnosticSet {.importc: "clang_getChildDiagnostics", dynlib: CLangLib.}

##
##  Destroy a diagnostic.
##

proc disposeDiagnostic*(
  diagnostic: CXDiagnostic
) {.importc: "clang_disposeDiagnostic", dynlib: CLangLib.}

##
##  Options to control the display of diagnostics.
##
##  The values in this enum are meant to be combined to customize the
##  behavior of \c clang_formatDiagnostic().
##

type CXDiagnosticDisplayOptions* {.size: sizeof(cint).} = enum
  ##
  ##  Display the source-location information where the
  ##  diagnostic was located.
  ##
  ##  When set, diagnostics will be prefixed by the file, line, and
  ##  (optionally) column to which the diagnostic refers. For example,
  ##
  ##  \code
  ##  test.c:28: warning: extra tokens at end of #endif directive
  ##  \endcode
  ##
  ##  This option corresponds to the clang flag \c -fshow-source-location.
  ##
  CXDiagnosticDisplaySourceLocation = 0x01
    ##
    ##  If displaying the source-location information of the
    ##  diagnostic, also include the column number.
    ##
    ##  This option corresponds to the clang flag \c -fshow-column.
    ##
  CXDiagnosticDisplayColumn = 0x02
    ##
    ##  If displaying the source-location information of the
    ##  diagnostic, also include information about source ranges in a
    ##  machine-parsable format.
    ##
    ##  This option corresponds to the clang flag
    ##  \c -fdiagnostics-print-source-range-info.
    ##
  CXDiagnosticDisplaySourceRanges = 0x04
    ##
    ##  Display the option name associated with this diagnostic, if any.
    ##
    ##  The option name displayed (e.g., -Wconversion) will be placed in brackets
    ##  after the diagnostic text. This option corresponds to the clang flag
    ##  \c -fdiagnostics-show-option.
    ##
  CXDiagnosticDisplayOption = 0x08
    ##
    ##  Display the category number associated with this diagnostic, if any.
    ##
    ##  The category number is displayed within brackets after the diagnostic text.
    ##  This option corresponds to the clang flag
    ##  \c -fdiagnostics-show-category=id.
    ##
  CXDiagnosticDisplayCategoryId = 0x10
    ##
    ##  Display the category name associated with this diagnostic, if any.
    ##
    ##  The category name is displayed within brackets after the diagnostic text.
    ##  This option corresponds to the clang flag
    ##  \c -fdiagnostics-show-category=name.
    ##
  CXDiagnosticDisplayCategoryName = 0x20

##
##  Format the given diagnostic in a manner that is suitable for display.
##
##  This routine will format the given diagnostic to a string, rendering
##  the diagnostic according to the various options given. The
##  \c clang_defaultDiagnosticDisplayOptions() function returns the set of
##  options that most closely mimics the behavior of the clang compiler.
##
##  \param Diagnostic The diagnostic to print.
##
##  \param Options A set of options that control the diagnostic display,
##  created by combining \c CXDiagnosticDisplayOptions values.
##
##  \returns A new string containing for formatted diagnostic.
##

proc formatDiagnostic*(
  diagnostic: CXDiagnostic, options: cuint
): CXString {.importc: "clang_formatDiagnostic", dynlib: CLangLib.}

##
##  Retrieve the set of display options most similar to the
##  default behavior of the clang compiler.
##
##  \returns A set of display options suitable for use with \c
##  clang_formatDiagnostic().
##

proc defaultDiagnosticDisplayOptions*(): cuint {.
  importc: "clang_defaultDiagnosticDisplayOptions", dynlib: CLangLib
.}

##
##  Determine the severity of the given diagnostic.
##

proc getDiagnosticSeverity*(
  a1: CXDiagnostic
): CXDiagnosticSeverity {.importc: "clang_getDiagnosticSeverity", dynlib: CLangLib.}

##
##  Retrieve the source location of the given diagnostic.
##
##  This location is where Clang would print the caret ('^') when
##  displaying the diagnostic on the command line.
##

proc getDiagnosticLocation*(
  a1: CXDiagnostic
): CXSourceLocation {.importc: "clang_getDiagnosticLocation", dynlib: CLangLib.}

##
##  Retrieve the text of the given diagnostic.
##

proc getDiagnosticSpelling*(
  a1: CXDiagnostic
): CXString {.importc: "clang_getDiagnosticSpelling", dynlib: CLangLib.}

##
##  Retrieve the name of the command-line option that enabled this
##  diagnostic.
##
##  \param Diag The diagnostic to be queried.
##
##  \param Disable If non-NULL, will be set to the option that disables this
##  diagnostic (if any).
##
##  \returns A string that contains the command-line option used to enable this
##  warning, such as "-Wconversion" or "-pedantic".
##

proc getDiagnosticOption*(
  diag: CXDiagnostic, disable: ptr CXString
): CXString {.importc: "clang_getDiagnosticOption", dynlib: CLangLib.}

##
##  Retrieve the category number for this diagnostic.
##
##  Diagnostics can be categorized into groups along with other, related
##  diagnostics (e.g., diagnostics under the same warning flag). This routine
##  retrieves the category number for the given diagnostic.
##
##  \returns The number of the category that contains this diagnostic, or zero
##  if this diagnostic is uncategorized.
##

proc getDiagnosticCategory*(
  a1: CXDiagnostic
): cuint {.importc: "clang_getDiagnosticCategory", dynlib: CLangLib.}

##
##  Retrieve the name of a particular diagnostic category.  This
##   is now deprecated.  Use clang_getDiagnosticCategoryText()
##   instead.
##
##  \param Category A diagnostic category number, as returned by
##  \c clang_getDiagnosticCategory().
##
##  \returns The name of the given diagnostic category.
##

proc getDiagnosticCategoryName*(
  category: cuint
): CXString {.importc: "clang_getDiagnosticCategoryName", dynlib: CLangLib.}

##
##  Retrieve the diagnostic category text for a given diagnostic.
##
##  \returns The text of the given diagnostic category.
##

proc getDiagnosticCategoryText*(
  a1: CXDiagnostic
): CXString {.importc: "clang_getDiagnosticCategoryText", dynlib: CLangLib.}

##
##  Determine the number of source ranges associated with the given
##  diagnostic.
##

proc getDiagnosticNumRanges*(
  a1: CXDiagnostic
): cuint {.importc: "clang_getDiagnosticNumRanges", dynlib: CLangLib.}

##
##  Retrieve a source range associated with the diagnostic.
##
##  A diagnostic's source ranges highlight important elements in the source
##  code. On the command line, Clang displays source ranges by
##  underlining them with '~' characters.
##
##  \param Diagnostic the diagnostic whose range is being extracted.
##
##  \param Range the zero-based index specifying which range to
##
##  \returns the requested source range.
##

proc getDiagnosticRange*(
  diagnostic: CXDiagnostic, range: cuint
): CXSourceRange {.importc: "clang_getDiagnosticRange", dynlib: CLangLib.}

##
##  Determine the number of fix-it hints associated with the
##  given diagnostic.
##

proc getDiagnosticNumFixIts*(
  diagnostic: CXDiagnostic
): cuint {.importc: "clang_getDiagnosticNumFixIts", dynlib: CLangLib.}

##
##  Retrieve the replacement information for a given fix-it.
##
##  Fix-its are described in terms of a source range whose contents
##  should be replaced by a string. This approach generalizes over
##  three kinds of operations: removal of source code (the range covers
##  the code to be removed and the replacement string is empty),
##  replacement of source code (the range covers the code to be
##  replaced and the replacement string provides the new code), and
##  insertion (both the start and end of the range point at the
##  insertion location, and the replacement string provides the text to
##  insert).
##
##  \param Diagnostic The diagnostic whose fix-its are being queried.
##
##  \param FixIt The zero-based index of the fix-it.
##
##  \param ReplacementRange The source range whose contents will be
##  replaced with the returned replacement string. Note that source
##  ranges are half-open ranges [a, b), so the source code should be
##  replaced from a and up to (but not including) b.
##
##  \returns A string containing text that should be replace the source
##  code indicated by the \c ReplacementRange.
##

proc getDiagnosticFixIt*(
  diagnostic: CXDiagnostic, fixIt: cuint, replacementRange: ptr CXSourceRange
): CXString {.importc: "clang_getDiagnosticFixIt", dynlib: CLangLib.}

##
##  @}
##
