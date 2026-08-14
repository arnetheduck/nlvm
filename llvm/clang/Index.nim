## ===-- clang-c/Index.h - Indexing Public C Interface -------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides a public interface to a Clang library for extracting  *|
## |* high-level symbol information from source files without exposing the full  *|
## |* Clang C++ API.                                                             *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_CLANG_C_INDEX_H [NewLine] # LLVM_CLANG_C_INDEX_H [NewLine] # clang-c/BuildSystem.h [NewLine] # clang-c/CXDiagnostic.h [NewLine] # clang-c/CXErrorCode.h [NewLine] # clang-c/CXFile.h [NewLine] # clang-c/CXSourceLocation.h [NewLine] # clang-c/CXString.h [NewLine] # clang-c/ExternC.h [NewLine] # clang-c/Platform.h [NewLine]
##  The version constants for the libclang API.
##  CINDEX_VERSION_MINOR should increase when there are API additions.
##  CINDEX_VERSION_MAJOR is intended for "major" source/ABI breaking changes.
##
##  The policy about the libclang API was always to keep it source and ABI
##  compatible, thus CINDEX_VERSION_MAJOR is expected to remain stable.
##  # CINDEX_VERSION_MAJOR 0 [NewLine] # CINDEX_VERSION_MINOR 64 [NewLine] # CINDEX_VERSION_ENCODE ( major , minor ) ( ( ( major ) * 10000 ) + ( ( minor ) * 1 ) ) [NewLine] # CINDEX_VERSION CINDEX_VERSION_ENCODE ( CINDEX_VERSION_MAJOR , CINDEX_VERSION_MINOR ) [NewLine] # CINDEX_VERSION_STRINGIZE_ ( major , minor ) # . # [NewLine] # CINDEX_VERSION_STRINGIZE ( major , minor ) CINDEX_VERSION_STRINGIZE_ ( major , minor ) [NewLine] # CINDEX_VERSION_STRING CINDEX_VERSION_STRINGIZE ( CINDEX_VERSION_MAJOR , CINDEX_VERSION_MINOR ) [NewLine] # __has_feature [NewLine] # __has_feature ( feature ) 0 [NewLine] # [NewLine]  \defgroup CINDEX libclang: C Interface to Clang
##
##  The C Interface to Clang provides a relatively small API that exposes
##  facilities for parsing source code into an abstract syntax tree (AST),
##  loading already-parsed ASTs, traversing the AST, associating
##  physical source locations with elements within the AST, and other
##  facilities that support Clang-based development tools.
##
##  This C interface to Clang will never provide all of the information
##  representation stored in Clang's C++ AST, nor should it: the intent is to
##  maintain an API that is relatively stable from one release to the next,
##  providing only the basic functionality needed to support development tools.
##
##  To avoid namespace pollution, data types are prefixed with "CX" and
##  functions are prefixed with "clang_".
##
##  @{
##
##  An "index" that consists of a set of translation units that would
##  typically be linked together into an executable or library.
##  typedef void * CXIndex ;
## Error: expected ';'!!!

##
##  An opaque type representing target information for a given translation
##  unit.
##

type CXTargetInfo* = ptr cXTargetInfoImpl

##
##  A single translation unit, which resides in an index.
##

type CXTranslationUnit* = ptr cXTranslationUnitImpl

##
##  Opaque pointer representing client data that will be passed through
##  to various callbacks and visitors.
##

type CXClientData* = pointer

##
##  Provides the contents of a file that has not yet been saved to disk.
##
##  Each CXUnsavedFile instance provides the name of a file on the
##  system along with the current contents of that file that have not
##  yet been saved to disk.
##

type CXUnsavedFile* {.bycopy.} = object
  ##
  ##  The file whose contents have not yet been saved.
  ##
  ##  This file must already exist in the file system.
  ##
  filename*: cstring
  ##
  ##  A buffer containing the unsaved contents of this file.
  ##
  contents*: cstring
  ##
  ##  The length of the unsaved contents of this buffer.
  ##
  length*: culong

##
##  Describes the availability of a particular entity, which indicates
##  whether the use of this entity will result in a warning or error due to
##  it being deprecated or unavailable.
##

type CXAvailabilityKind* {.size: sizeof(cint).} = enum
  ##
  ##  The entity is available.
  ##
  CXAvailabilityAvailable
    ##
    ##  The entity is available, but has been deprecated (and its use is
    ##  not recommended).
    ##
  CXAvailabilityDeprecated
    ##
    ##  The entity is not available; any use of it will be an error.
    ##
  CXAvailabilityNotAvailable
    ##
    ##  The entity is available, but not accessible; any use of it will be
    ##  an error.
    ##
  CXAvailabilityNotAccessible

##
##  Describes a version number of the form major.minor.subminor.
##

type CXVersion* {.bycopy.} = object
  ##
  ##  The major version number, e.g., the '10' in '10.7.3'. A negative
  ##  value indicates that there is no version number at all.
  ##
  major*: cint
  ##
  ##  The minor version number, e.g., the '7' in '10.7.3'. This value
  ##  will be negative if no minor version number was provided, e.g., for
  ##  version '10'.
  ##
  minor*: cint
  ##
  ##  The subminor version number, e.g., the '3' in '10.7.3'. This value
  ##  will be negative if no minor or subminor version number was provided,
  ##  e.g., in version '10' or '10.7'.
  ##
  subminor*: cint

##
##  Describes the exception specification of a cursor.
##
##  A negative value indicates that the cursor is not a function declaration.
##

type CXCursorExceptionSpecificationKind* {.size: sizeof(cint).} = enum
  ##
  ##  The cursor has no exception specification.
  ##
  CXCursorExceptionSpecificationKindNone
    ##
    ##  The cursor has exception specification throw()
    ##
  CXCursorExceptionSpecificationKindDynamicNone
    ##
    ##  The cursor has exception specification throw(T1, T2)
    ##
  CXCursorExceptionSpecificationKindDynamic
    ##
    ##  The cursor has exception specification throw(...).
    ##
  CXCursorExceptionSpecificationKindMSAny
    ##
    ##  The cursor has exception specification basic noexcept.
    ##
  CXCursorExceptionSpecificationKindBasicNoexcept
    ##
    ##  The cursor has exception specification computed noexcept.
    ##
  CXCursorExceptionSpecificationKindComputedNoexcept
    ##
    ##  The exception specification has not yet been evaluated.
    ##
  CXCursorExceptionSpecificationKindUnevaluated
    ##
    ##  The exception specification has not yet been instantiated.
    ##
  CXCursorExceptionSpecificationKindUninstantiated
    ##
    ##  The exception specification has not been parsed yet.
    ##
  CXCursorExceptionSpecificationKindUnparsed
    ##
    ##  The cursor has a __declspec(nothrow) exception specification.
    ##
  CXCursorExceptionSpecificationKindNoThrow

##
##  Provides a shared context for creating translation units.
##
##  It provides two options:
##
##  - excludeDeclarationsFromPCH: When non-zero, allows enumeration of "local"
##  declarations (when loading any new translation units). A "local" declaration
##  is one that belongs in the translation unit itself and not in a precompiled
##  header that was used by the translation unit. If zero, all declarations
##  will be enumerated.
##
##  Here is an example:
##
##  \code
##    // excludeDeclsFromPCH = 1, displayDiagnostics=1
##    Idx = clang_createIndex(1, 1);
##
##    // IndexTest.pch was produced with the following command:
##    // "clang -x c IndexTest.h -emit-ast -o IndexTest.pch"
##    TU = clang_createTranslationUnit(Idx, "IndexTest.pch");
##
##    // This will load all the symbols from 'IndexTest.pch'
##    clang_visitChildren(clang_getTranslationUnitCursor(TU),
##                        TranslationUnitVisitor, 0);
##    clang_disposeTranslationUnit(TU);
##
##    // This will load all the symbols from 'IndexTest.c', excluding symbols
##    // from 'IndexTest.pch'.
##    char *args[] = { "-Xclang", "-include-pch=IndexTest.pch" };
##    TU = clang_createTranslationUnitFromSourceFile(Idx, "IndexTest.c", 2, args,
##                                                   0, 0);
##    clang_visitChildren(clang_getTranslationUnitCursor(TU),
##                        TranslationUnitVisitor, 0);
##    clang_disposeTranslationUnit(TU);
##  \endcode
##
##  This process of creating the 'pch', loading it separately, and using it (via
##  -include-pch) allows 'excludeDeclsFromPCH' to remove redundant callbacks
##  (which gives the indexer the same performance benefit as the compiler).
##

proc createIndex*(
  excludeDeclarationsFromPCH: cint, displayDiagnostics: cint
): CXIndex {.importc: "clang_createIndex", dynlib: CLangLib.}

##
##  Destroy the given index.
##
##  The index must not be destroyed until all of the translation units created
##  within that index have been destroyed.
##

proc disposeIndex*(index: CXIndex) {.importc: "clang_disposeIndex", dynlib: CLangLib.}
type
  ##
  ##  Use the default value of an option that may depend on the process
  ##  environment.
  ##
  ##
  ##  Used to indicate that no special CXIndex options are needed.
  ##
  CXChoice* {.size: sizeof(cint).} = enum
    CXChoiceDefault = 0
      ##
      ##  Enable the option.
      ##
    CXChoiceEnabled = 1
      ##
      ##  Disable the option.
      ##
    CXChoiceDisabled = 2

  CXGlobalOptFlags* {.size: sizeof(cint).} = enum
    CXGlobalOptNone = 0x0
      ##
      ##  Used to indicate that threads that libclang creates for indexing
      ##  purposes should use background priority.
      ##
      ##  Affects #clang_indexSourceFile, #clang_indexTranslationUnit,
      ##  #clang_parseTranslationUnit, #clang_saveTranslationUnit.
      ##
    CXGlobalOptThreadBackgroundPriorityForIndexing = 0x1
      ##
      ##  Used to indicate that threads that libclang creates for editing
      ##  purposes should use background priority.
      ##
      ##  Affects #clang_reparseTranslationUnit, #clang_codeCompleteAt,
      ##  #clang_annotateTokens
      ##
    CXGlobalOptThreadBackgroundPriorityForEditing = 0x2
      ##
      ##  Used to indicate that all threads that libclang creates should use
      ##  background priority.
      ##
    # CXGlobalOptThreadBackgroundPriorityForAll =
    #   cXGlobalOptThreadBackgroundPriorityForIndexing or
    #   cXGlobalOptThreadBackgroundPriorityForEditing

##
##  Index initialization options.
##
##  0 is the default value of each member of this struct except for Size.
##  Initialize the struct in one of the following three ways to avoid adapting
##  code each time a new member is added to it:
##  \code
##  CXIndexOptions Opts;
##  memset(&Opts, 0, sizeof(Opts));
##  Opts.Size = sizeof(CXIndexOptions);
##  \endcode
##  or explicitly initialize the first data member and zero-initialize the rest:
##  \code
##  CXIndexOptions Opts = { sizeof(CXIndexOptions) };
##  \endcode
##  or to prevent the -Wmissing-field-initializers warning for the above version:
##  \code
##  CXIndexOptions Opts{};
##  Opts.Size = sizeof(CXIndexOptions);
##  \endcode
##

## !!!Ignored construct:  typedef struct CXIndexOptions {
##  The size of struct CXIndexOptions used for option versioning.
##
##  Always initialize this member to sizeof(CXIndexOptions), or assign
##  sizeof(CXIndexOptions) to it right after creating a CXIndexOptions object.
##  unsigned Size ;
##  A CXChoice enumerator that specifies the indexing priority policy.
##  \sa CXGlobalOpt_ThreadBackgroundPriorityForIndexing
##  unsigned char ThreadBackgroundPriorityForIndexing ;
##  A CXChoice enumerator that specifies the editing priority policy.
##  \sa CXGlobalOpt_ThreadBackgroundPriorityForEditing
##  unsigned char ThreadBackgroundPriorityForEditing ;
##  \see clang_createIndex()
##  unsigned ExcludeDeclarationsFromPCH : 1 ;
##  \see clang_createIndex()
##  unsigned DisplayDiagnostics : 1 ;
##  Store PCH in memory. If zero, PCH are stored in temporary files.
##  unsigned StorePreamblesInMemory : 1 ; unsigned Reserved : 13 ;
##  The path to a directory, in which to store temporary PCH files. If null or
##  empty, the default system temporary directory is used. These PCH files are
##  deleted on clean exit but stay on disk if the program crashes or is killed.
##
##  This option is ignored if \a StorePreamblesInMemory is non-zero.
##
##  Libclang does not create the directory at the specified path in the file
##  system. Therefore it must exist, or storing PCH files will fail.
##  const char * PreambleStoragePath ;
##  Specifies a path which will contain log files for certain libclang
##  invocations. A null value implies that libclang invocations are not logged.
##  const char * InvocationEmissionPath ; } CXIndexOptions ;
## Error: identifier expected, but got: :!!!

##
##  Provides a shared context for creating translation units.
##
##  Call this function instead of clang_createIndex() if you need to configure
##  the additional options in CXIndexOptions.
##
##  \returns The created index or null in case of error, such as an unsupported
##  value of options->Size.
##
##  For example:
##  \code
##  CXIndex createIndex(const char *ApplicationTemporaryPath) {
##    const int ExcludeDeclarationsFromPCH = 1;
##    const int DisplayDiagnostics = 1;
##    CXIndex Idx;
##  #if CINDEX_VERSION_MINOR >= 64
##    CXIndexOptions Opts;
##    memset(&Opts, 0, sizeof(Opts));
##    Opts.Size = sizeof(CXIndexOptions);
##    Opts.ThreadBackgroundPriorityForIndexing = 1;
##    Opts.ExcludeDeclarationsFromPCH = ExcludeDeclarationsFromPCH;
##    Opts.DisplayDiagnostics = DisplayDiagnostics;
##    Opts.PreambleStoragePath = ApplicationTemporaryPath;
##    Idx = clang_createIndexWithOptions(&Opts);
##    if (Idx)
##      return Idx;
##    fprintf(stderr,
##            "clang_createIndexWithOptions() failed. "
##            "CINDEX_VERSION_MINOR = %d, sizeof(CXIndexOptions) = %u\n",
##            CINDEX_VERSION_MINOR, Opts.Size);
##  #else
##    (void)ApplicationTemporaryPath;
##  #endif
##    Idx = clang_createIndex(ExcludeDeclarationsFromPCH, DisplayDiagnostics);
##    clang_CXIndex_setGlobalOptions(
##        Idx, clang_CXIndex_getGlobalOptions(Idx) |
##                 CXGlobalOpt_ThreadBackgroundPriorityForIndexing);
##    return Idx;
##  }
##  \endcode
##
##  \sa clang_createIndex()
##

proc createIndexWithOptions*(
  options: ptr CXIndexOptions
): CXIndex {.importc: "clang_createIndexWithOptions", dynlib: CLangLib.}

##
##  Sets general options associated with a CXIndex.
##
##  This function is DEPRECATED. Set
##  CXIndexOptions::ThreadBackgroundPriorityForIndexing and/or
##  CXIndexOptions::ThreadBackgroundPriorityForEditing and call
##  clang_createIndexWithOptions() instead.
##
##  For example:
##  \code
##  CXIndex idx = ...;
##  clang_CXIndex_setGlobalOptions(idx,
##      clang_CXIndex_getGlobalOptions(idx) |
##      CXGlobalOpt_ThreadBackgroundPriorityForIndexing);
##  \endcode
##
##  \param options A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags.
##

proc cXIndexSetGlobalOptions*(
  a1: CXIndex, options: cuint
) {.importc: "clang_CXIndex_setGlobalOptions", dynlib: CLangLib.}

##
##  Gets the general options associated with a CXIndex.
##
##  This function allows to obtain the final option values used by libclang after
##  specifying the option policies via CXChoice enumerators.
##
##  \returns A bitmask of options, a bitwise OR of CXGlobalOpt_XXX flags that
##  are associated with the given CXIndex object.
##

proc cXIndexGetGlobalOptions*(
  a1: CXIndex
): cuint {.importc: "clang_CXIndex_getGlobalOptions", dynlib: CLangLib.}

##
##  Sets the invocation emission path option in a CXIndex.
##
##  This function is DEPRECATED. Set CXIndexOptions::InvocationEmissionPath and
##  call clang_createIndexWithOptions() instead.
##
##  The invocation emission path specifies a path which will contain log
##  files for certain libclang invocations. A null value (default) implies that
##  libclang invocations are not logged..
##

proc cXIndexSetInvocationEmissionPathOption*(
  a1: CXIndex, path: cstring
) {.importc: "clang_CXIndex_setInvocationEmissionPathOption", dynlib: CLangLib.}

##
##  Determine whether the given header is guarded against
##  multiple inclusions, either with the conventional
##  \#ifndef/\#define/\#endif macro guards or with \#pragma once.
##

proc isFileMultipleIncludeGuarded*(
  tu: CXTranslationUnit, file: CXFile
): cuint {.importc: "clang_isFileMultipleIncludeGuarded", dynlib: CLangLib.}

##
##  Retrieve a file handle within the given translation unit.
##
##  \param tu the translation unit
##
##  \param file_name the name of the file.
##
##  \returns the file handle for the named file in the translation unit \p tu,
##  or a NULL file handle if the file was not a part of this translation unit.
##

proc getFile*(
  tu: CXTranslationUnit, fileName: cstring
): CXFile {.importc: "clang_getFile", dynlib: CLangLib.}

##
##  Retrieve the buffer associated with the given file.
##
##  \param tu the translation unit
##
##  \param file the file for which to retrieve the buffer.
##
##  \param size [out] if non-NULL, will be set to the size of the buffer.
##
##  \returns a pointer to the buffer in memory that holds the contents of
##  \p file, or a NULL pointer when the file is not loaded.
##

proc getFileContents*(
  tu: CXTranslationUnit, file: CXFile, size: ptr csize_t
): cstring {.importc: "clang_getFileContents", dynlib: CLangLib.}

##
##  Retrieves the source location associated with a given file/line/column
##  in a particular translation unit.
##

proc getLocation*(
  tu: CXTranslationUnit, file: CXFile, line: cuint, column: cuint
): CXSourceLocation {.importc: "clang_getLocation", dynlib: CLangLib.}

##
##  Retrieves the source location associated with a given character offset
##  in a particular translation unit.
##

proc getLocationForOffset*(
  tu: CXTranslationUnit, file: CXFile, offset: cuint
): CXSourceLocation {.importc: "clang_getLocationForOffset", dynlib: CLangLib.}

##
##  Retrieve all ranges that were skipped by the preprocessor.
##
##  The preprocessor will skip lines when they are surrounded by an
##  if/ifdef/ifndef directive whose condition does not evaluate to true.
##

proc getSkippedRanges*(
  tu: CXTranslationUnit, file: CXFile
): ptr CXSourceRangeList {.importc: "clang_getSkippedRanges", dynlib: CLangLib.}

##
##  Retrieve all ranges from all files that were skipped by the
##  preprocessor.
##
##  The preprocessor will skip lines when they are surrounded by an
##  if/ifdef/ifndef directive whose condition does not evaluate to true.
##

proc getAllSkippedRanges*(
  tu: CXTranslationUnit
): ptr CXSourceRangeList {.importc: "clang_getAllSkippedRanges", dynlib: CLangLib.}

##
##  Determine the number of diagnostics produced for the given
##  translation unit.
##

proc getNumDiagnostics*(
  unit: CXTranslationUnit
): cuint {.importc: "clang_getNumDiagnostics", dynlib: CLangLib.}

##
##  Retrieve a diagnostic associated with the given translation unit.
##
##  \param Unit the translation unit to query.
##  \param Index the zero-based diagnostic number to retrieve.
##
##  \returns the requested diagnostic. This diagnostic must be freed
##  via a call to \c clang_disposeDiagnostic().
##

proc getDiagnostic*(
  unit: CXTranslationUnit, index: cuint
): CXDiagnostic {.importc: "clang_getDiagnostic", dynlib: CLangLib.}

##
##  Retrieve the complete set of diagnostics associated with a
##         translation unit.
##
##  \param Unit the translation unit to query.
##

proc getDiagnosticSetFromTU*(
  unit: CXTranslationUnit
): CXDiagnosticSet {.importc: "clang_getDiagnosticSetFromTU", dynlib: CLangLib.}

##
##  \defgroup CINDEX_TRANSLATION_UNIT Translation unit manipulation
##
##  The routines in this group provide the ability to create and destroy
##  translation units from files, either by parsing the contents of the files or
##  by reading in a serialized representation of a translation unit.
##
##  @{
##
##
##  Get the original translation unit source file name.
##

proc getTranslationUnitSpelling*(
  cTUnit: CXTranslationUnit
): CXString {.importc: "clang_getTranslationUnitSpelling", dynlib: CLangLib.}

##
##  Return the CXTranslationUnit for a given source file and the provided
##  command line arguments one would pass to the compiler.
##
##  Note: The 'source_filename' argument is optional.  If the caller provides a
##  NULL pointer, the name of the source file is expected to reside in the
##  specified command line arguments.
##
##  Note: When encountered in 'clang_command_line_args', the following options
##  are ignored:
##
##    '-c'
##    '-emit-ast'
##    '-fsyntax-only'
##    '-o \<output file>'  (both '-o' and '\<output file>' are ignored)
##
##  \param CIdx The index object with which the translation unit will be
##  associated.
##
##  \param source_filename The name of the source file to load, or NULL if the
##  source file is included in \p clang_command_line_args.
##
##  \param num_clang_command_line_args The number of command-line arguments in
##  \p clang_command_line_args.
##
##  \param clang_command_line_args The command-line arguments that would be
##  passed to the \c clang executable if it were being invoked out-of-process.
##  These command-line options will be parsed and will affect how the translation
##  unit is parsed. Note that the following options are ignored: '-c',
##  '-emit-ast', '-fsyntax-only' (which is the default), and '-o \<output file>'.
##
##  \param num_unsaved_files the number of unsaved file entries in \p
##  unsaved_files.
##
##  \param unsaved_files the files that have not yet been saved to disk
##  but may be required for code completion, including the contents of
##  those files.  The contents and name of these files (as specified by
##  CXUnsavedFile) are copied when necessary, so the client only needs to
##  guarantee their validity until the call to this function returns.
##

proc createTranslationUnitFromSourceFile*(
  cIdx: CXIndex,
  sourceFilename: cstring,
  numClangCommandLineArgs: cint,
  commandLineArgs: cstringArray,
  numUnsavedFiles: cuint,
  unsavedFiles: ptr CXUnsavedFile,
): CXTranslationUnit {.
  importc: "clang_createTranslationUnitFromSourceFile", dynlib: CLangLib
.}

##
##  Same as \c clang_createTranslationUnit2, but returns
##  the \c CXTranslationUnit instead of an error code.  In case of an error this
##  routine returns a \c NULL \c CXTranslationUnit, without further detailed
##  error codes.
##

proc createTranslationUnit*(
  cIdx: CXIndex, astFilename: cstring
): CXTranslationUnit {.importc: "clang_createTranslationUnit", dynlib: CLangLib.}

##
##  Create a translation unit from an AST file (\c -emit-ast).
##
##  \param[out] out_TU A non-NULL pointer to store the created
##  \c CXTranslationUnit.
##
##  \returns Zero on success, otherwise returns an error code.
##

proc createTranslationUnit2*(
  cIdx: CXIndex, astFilename: cstring, outTU: ptr CXTranslationUnit
): CXIndexOptionsCXErrorCode {.
  importc: "clang_createTranslationUnit2", dynlib: CLangLib
.}

##
##  Flags that control the creation of translation units.
##
##  The enumerators in this enumeration type are meant to be bitwise
##  ORed together to specify which options should be used when
##  constructing the translation unit.
##

type CXIndexOptionsCXTranslationUnitFlags* {.size: sizeof(cint).} = enum
  ##
  ##  Used to indicate that no special translation-unit options are
  ##  needed.
  ##
  CXTranslationUnitNone = 0x0
    ##
    ##  Used to indicate that the parser should construct a "detailed"
    ##  preprocessing record, including all macro definitions and instantiations.
    ##
    ##  Constructing a detailed preprocessing record requires more memory
    ##  and time to parse, since the information contained in the record
    ##  is usually not retained. However, it can be useful for
    ##  applications that require more detailed information about the
    ##  behavior of the preprocessor.
    ##
  CXTranslationUnitDetailedPreprocessingRecord = 0x01
    ##
    ##  Used to indicate that the translation unit is incomplete.
    ##
    ##  When a translation unit is considered "incomplete", semantic
    ##  analysis that is typically performed at the end of the
    ##  translation unit will be suppressed. For example, this suppresses
    ##  the completion of tentative declarations in C and of
    ##  instantiation of implicitly-instantiation function templates in
    ##  C++. This option is typically used when parsing a header with the
    ##  intent of producing a precompiled header.
    ##
  CXTranslationUnitIncomplete = 0x02
    ##
    ##  Used to indicate that the translation unit should be built with an
    ##  implicit precompiled header for the preamble.
    ##
    ##  An implicit precompiled header is used as an optimization when a
    ##  particular translation unit is likely to be reparsed many times
    ##  when the sources aren't changing that often. In this case, an
    ##  implicit precompiled header will be built containing all of the
    ##  initial includes at the top of the main file (what we refer to as
    ##  the "preamble" of the file). In subsequent parses, if the
    ##  preamble or the files in it have not changed, \c
    ##  clang_reparseTranslationUnit() will re-use the implicit
    ##  precompiled header to improve parsing performance.
    ##
  CXTranslationUnitPrecompiledPreamble = 0x04
    ##
    ##  Used to indicate that the translation unit should cache some
    ##  code-completion results with each reparse of the source file.
    ##
    ##  Caching of code-completion results is a performance optimization that
    ##  introduces some overhead to reparsing but improves the performance of
    ##  code-completion operations.
    ##
  CXTranslationUnitCacheCompletionResults = 0x08
    ##
    ##  Used to indicate that the translation unit will be serialized with
    ##  \c clang_saveTranslationUnit.
    ##
    ##  This option is typically used when parsing a header with the intent of
    ##  producing a precompiled header.
    ##
  CXTranslationUnitForSerialization = 0x10
    ##
    ##  DEPRECATED: Enabled chained precompiled preambles in C++.
    ##
    ##  Note: this is a *temporary* option that is available only while
    ##  we are testing C++ precompiled preamble support. It is deprecated.
    ##
  CXTranslationUnitCXXChainedPCH = 0x20
    ##
    ##  Used to indicate that function/method bodies should be skipped while
    ##  parsing.
    ##
    ##  This option can be used to search for declarations/definitions while
    ##  ignoring the usages.
    ##
  CXTranslationUnitSkipFunctionBodies = 0x40
    ##
    ##  Used to indicate that brief documentation comments should be
    ##  included into the set of code completions returned from this translation
    ##  unit.
    ##
  CXTranslationUnitIncludeBriefCommentsInCodeCompletion = 0x80
    ##
    ##  Used to indicate that the precompiled preamble should be created on
    ##  the first parse. Otherwise it will be created on the first reparse. This
    ##  trades runtime on the first parse (serializing the preamble takes time) for
    ##  reduced runtime on the second parse (can now reuse the preamble).
    ##
  CXTranslationUnitCreatePreambleOnFirstParse = 0x100
    ##
    ##  Do not stop processing when fatal errors are encountered.
    ##
    ##  When fatal errors are encountered while parsing a translation unit,
    ##  semantic analysis is typically stopped early when compiling code. A common
    ##  source for fatal errors are unresolvable include files. For the
    ##  purposes of an IDE, this is undesirable behavior and as much information
    ##  as possible should be reported. Use this flag to enable this behavior.
    ##
  CXTranslationUnitKeepGoing = 0x200
    ##
    ##  Sets the preprocessor in a mode for parsing a single file only.
    ##
  CXTranslationUnitSingleFileParse = 0x400
    ##
    ##  Used in combination with CXTranslationUnit_SkipFunctionBodies to
    ##  constrain the skipping of function bodies to the preamble.
    ##
    ##  The function bodies of the main file are not skipped.
    ##
  CXTranslationUnitLimitSkipFunctionBodiesToPreamble = 0x800
    ##
    ##  Used to indicate that attributed types should be included in CXType.
    ##
  CXTranslationUnitIncludeAttributedTypes = 0x1000
    ##
    ##  Used to indicate that implicit attributes should be visited.
    ##
  CXTranslationUnitVisitImplicitAttributes = 0x2000
    ##
    ##  Used to indicate that non-errors from included files should be ignored.
    ##
    ##  If set, clang_getDiagnosticSetFromTU() will not report e.g. warnings from
    ##  included files anymore. This speeds up clang_getDiagnosticSetFromTU() for
    ##  the case where these warnings are not of interest, as for an IDE for
    ##  example, which typically shows only the diagnostics in the main file.
    ##
  CXTranslationUnitIgnoreNonErrorsFromIncludedFiles = 0x4000
    ##
    ##  Tells the preprocessor not to skip excluded conditional blocks.
    ##
  CXTranslationUnitRetainExcludedConditionalBlocks = 0x8000

##
##  Returns the set of flags that is suitable for parsing a translation
##  unit that is being edited.
##
##  The set of flags returned provide options for \c clang_parseTranslationUnit()
##  to indicate that the translation unit is likely to be reparsed many times,
##  either explicitly (via \c clang_reparseTranslationUnit()) or implicitly
##  (e.g., by code completion (\c clang_codeCompletionAt())). The returned flag
##  set contains an unspecified set of optimizations (e.g., the precompiled
##  preamble) geared toward improving the performance of these routines. The
##  set of optimizations enabled may change from one version to the next.
##

proc defaultEditingTranslationUnitOptions*(): cuint {.
  importc: "clang_defaultEditingTranslationUnitOptions", dynlib: CLangLib
.}

##
##  Same as \c clang_parseTranslationUnit2, but returns
##  the \c CXTranslationUnit instead of an error code.  In case of an error this
##  routine returns a \c NULL \c CXTranslationUnit, without further detailed
##  error codes.
##

proc parseTranslationUnit*(
  cIdx: CXIndex,
  sourceFilename: cstring,
  commandLineArgs: cstringArray,
  numCommandLineArgs: cint,
  unsavedFiles: ptr CXUnsavedFile,
  numUnsavedFiles: cuint,
  options: cuint,
): CXTranslationUnit {.importc: "clang_parseTranslationUnit", dynlib: CLangLib.}

##
##  Parse the given source file and the translation unit corresponding
##  to that file.
##
##  This routine is the main entry point for the Clang C API, providing the
##  ability to parse a source file into a translation unit that can then be
##  queried by other functions in the API. This routine accepts a set of
##  command-line arguments so that the compilation can be configured in the same
##  way that the compiler is configured on the command line.
##
##  \param CIdx The index object with which the translation unit will be
##  associated.
##
##  \param source_filename The name of the source file to load, or NULL if the
##  source file is included in \c command_line_args.
##
##  \param command_line_args The command-line arguments that would be
##  passed to the \c clang executable if it were being invoked out-of-process.
##  These command-line options will be parsed and will affect how the translation
##  unit is parsed. Note that the following options are ignored: '-c',
##  '-emit-ast', '-fsyntax-only' (which is the default), and '-o \<output file>'.
##
##  \param num_command_line_args The number of command-line arguments in
##  \c command_line_args.
##
##  \param unsaved_files the files that have not yet been saved to disk
##  but may be required for parsing, including the contents of
##  those files.  The contents and name of these files (as specified by
##  CXUnsavedFile) are copied when necessary, so the client only needs to
##  guarantee their validity until the call to this function returns.
##
##  \param num_unsaved_files the number of unsaved file entries in \p
##  unsaved_files.
##
##  \param options A bitmask of options that affects how the translation unit
##  is managed but not its compilation. This should be a bitwise OR of the
##  CXTranslationUnit_XXX flags.
##
##  \param[out] out_TU A non-NULL pointer to store the created
##  \c CXTranslationUnit, describing the parsed code and containing any
##  diagnostics produced by the compiler.
##
##  \returns Zero on success, otherwise returns an error code.
##

proc parseTranslationUnit2*(
  cIdx: CXIndex,
  sourceFilename: cstring,
  commandLineArgs: cstringArray,
  numCommandLineArgs: cint,
  unsavedFiles: ptr CXUnsavedFile,
  numUnsavedFiles: cuint,
  options: cuint,
  outTU: ptr CXTranslationUnit,
): CXIndexOptionsCXErrorCode {.
  importc: "clang_parseTranslationUnit2", dynlib: CLangLib
.}

##
##  Same as clang_parseTranslationUnit2 but requires a full command line
##  for \c command_line_args including argv[0]. This is useful if the standard
##  library paths are relative to the binary.
##

proc parseTranslationUnit2FullArgv*(
  cIdx: CXIndex,
  sourceFilename: cstring,
  commandLineArgs: cstringArray,
  numCommandLineArgs: cint,
  unsavedFiles: ptr CXUnsavedFile,
  numUnsavedFiles: cuint,
  options: cuint,
  outTU: ptr CXTranslationUnit,
): CXIndexOptionsCXErrorCode {.
  importc: "clang_parseTranslationUnit2FullArgv", dynlib: CLangLib
.}

##
##  Flags that control how translation units are saved.
##
##  The enumerators in this enumeration type are meant to be bitwise
##  ORed together to specify which options should be used when
##  saving the translation unit.
##

type CXIndexOptionsCXSaveTranslationUnitFlags* {.size: sizeof(cint).} = enum
  ##
  ##  Used to indicate that no special saving options are needed.
  ##
  CXSaveTranslationUnitNone = 0x0

##
##  Returns the set of flags that is suitable for saving a translation
##  unit.
##
##  The set of flags returned provide options for
##  \c clang_saveTranslationUnit() by default. The returned flag
##  set contains an unspecified set of options that save translation units with
##  the most commonly-requested data.
##

proc defaultSaveOptions*(
  tu: CXTranslationUnit
): cuint {.importc: "clang_defaultSaveOptions", dynlib: CLangLib.}

##
##  Describes the kind of error that occurred (if any) in a call to
##  \c clang_saveTranslationUnit().
##

type CXIndexOptionsCXSaveError* {.size: sizeof(cint).} = enum
  ##
  ##  Indicates that no error occurred while saving a translation unit.
  ##
  CXSaveErrorNone = 0
    ##
    ##  Indicates that an unknown error occurred while attempting to save
    ##  the file.
    ##
    ##  This error typically indicates that file I/O failed when attempting to
    ##  write the file.
    ##
  CXSaveErrorUnknown = 1
    ##
    ##  Indicates that errors during translation prevented this attempt
    ##  to save the translation unit.
    ##
    ##  Errors that prevent the translation unit from being saved can be
    ##  extracted using \c clang_getNumDiagnostics() and \c clang_getDiagnostic().
    ##
  CXSaveErrorTranslationErrors = 2
    ##
    ##  Indicates that the translation unit to be saved was somehow
    ##  invalid (e.g., NULL).
    ##
  CXSaveErrorInvalidTU = 3

##
##  Saves a translation unit into a serialized representation of
##  that translation unit on disk.
##
##  Any translation unit that was parsed without error can be saved
##  into a file. The translation unit can then be deserialized into a
##  new \c CXTranslationUnit with \c clang_createTranslationUnit() or,
##  if it is an incomplete translation unit that corresponds to a
##  header, used as a precompiled header when parsing other translation
##  units.
##
##  \param TU The translation unit to save.
##
##  \param FileName The file to which the translation unit will be saved.
##
##  \param options A bitmask of options that affects how the translation unit
##  is saved. This should be a bitwise OR of the
##  CXSaveTranslationUnit_XXX flags.
##
##  \returns A value that will match one of the enumerators of the CXSaveError
##  enumeration. Zero (CXSaveError_None) indicates that the translation unit was
##  saved successfully, while a non-zero value indicates that a problem occurred.
##

proc saveTranslationUnit*(
  tu: CXTranslationUnit, fileName: cstring, options: cuint
): cint {.importc: "clang_saveTranslationUnit", dynlib: CLangLib.}

##
##  Suspend a translation unit in order to free memory associated with it.
##
##  A suspended translation unit uses significantly less memory but on the other
##  side does not support any other calls than \c clang_reparseTranslationUnit
##  to resume it or \c clang_disposeTranslationUnit to dispose it completely.
##

proc suspendTranslationUnit*(
  a1: CXTranslationUnit
): cuint {.importc: "clang_suspendTranslationUnit", dynlib: CLangLib.}

##
##  Destroy the specified CXTranslationUnit object.
##

proc disposeTranslationUnit*(
  a1: CXTranslationUnit
) {.importc: "clang_disposeTranslationUnit", dynlib: CLangLib.}

##
##  Flags that control the reparsing of translation units.
##
##  The enumerators in this enumeration type are meant to be bitwise
##  ORed together to specify which options should be used when
##  reparsing the translation unit.
##

type CXIndexOptionsCXReparseFlags* {.size: sizeof(cint).} = enum
  ##
  ##  Used to indicate that no special reparsing options are needed.
  ##
  CXReparseNone = 0x0

##
##  Returns the set of flags that is suitable for reparsing a translation
##  unit.
##
##  The set of flags returned provide options for
##  \c clang_reparseTranslationUnit() by default. The returned flag
##  set contains an unspecified set of optimizations geared toward common uses
##  of reparsing. The set of optimizations enabled may change from one version
##  to the next.
##

proc defaultReparseOptions*(
  tu: CXTranslationUnit
): cuint {.importc: "clang_defaultReparseOptions", dynlib: CLangLib.}

##
##  Reparse the source files that produced this translation unit.
##
##  This routine can be used to re-parse the source files that originally
##  created the given translation unit, for example because those source files
##  have changed (either on disk or as passed via \p unsaved_files). The
##  source code will be reparsed with the same command-line options as it
##  was originally parsed.
##
##  Reparsing a translation unit invalidates all cursors and source locations
##  that refer into that translation unit. This makes reparsing a translation
##  unit semantically equivalent to destroying the translation unit and then
##  creating a new translation unit with the same command-line arguments.
##  However, it may be more efficient to reparse a translation
##  unit using this routine.
##
##  \param TU The translation unit whose contents will be re-parsed. The
##  translation unit must originally have been built with
##  \c clang_createTranslationUnitFromSourceFile().
##
##  \param num_unsaved_files The number of unsaved file entries in \p
##  unsaved_files.
##
##  \param unsaved_files The files that have not yet been saved to disk
##  but may be required for parsing, including the contents of
##  those files.  The contents and name of these files (as specified by
##  CXUnsavedFile) are copied when necessary, so the client only needs to
##  guarantee their validity until the call to this function returns.
##
##  \param options A bitset of options composed of the flags in CXReparse_Flags.
##  The function \c clang_defaultReparseOptions() produces a default set of
##  options recommended for most uses, based on the translation unit.
##
##  \returns 0 if the sources could be reparsed.  A non-zero error code will be
##  returned if reparsing was impossible, such that the translation unit is
##  invalid. In such cases, the only valid call for \c TU is
##  \c clang_disposeTranslationUnit(TU).  The error codes returned by this
##  routine are described by the \c CXErrorCode enum.
##

proc reparseTranslationUnit*(
  tu: CXTranslationUnit,
  numUnsavedFiles: cuint,
  unsavedFiles: ptr CXUnsavedFile,
  options: cuint,
): cint {.importc: "clang_reparseTranslationUnit", dynlib: CLangLib.}

##
##  Categorizes how memory is being used by a translation unit.
##

type CXIndexOptionsCXTUResourceUsageKind* {.size: sizeof(cint).} = enum
  CXTUResourceUsageAST = 1
  CXTUResourceUsageIdentifiers = 2
  CXTUResourceUsageSelectors = 3
  CXTUResourceUsageGlobalCompletionResults = 4
  CXTUResourceUsageSourceManagerContentCache = 5
  CXTUResourceUsageAST_SideTables = 6
  CXTUResourceUsageSourceManagerMembufferMalloc = 7
  CXTUResourceUsageSourceManagerMembufferMMap = 8
  CXTUResourceUsageExternalASTSourceMembufferMalloc = 9
  CXTUResourceUsageExternalASTSourceMembufferMMap = 10
  CXTUResourceUsagePreprocessor = 11
  CXTUResourceUsagePreprocessingRecord = 12
  CXTUResourceUsageSourceManagerDataStructures = 13
  CXTUResourceUsagePreprocessorHeaderSearch = 14
  # CXTUResourceUsageMEMORY_IN_BYTES_BEGIN = cXTUResourceUsageAST
  # CXTUResourceUsageMEMORY_IN_BYTES_END = cXTUResourceUsagePreprocessorHeaderSearch
  # CXTUResourceUsageFirst = cXTUResourceUsageAST
  # CXTUResourceUsageLast = cXTUResourceUsagePreprocessorHeaderSearch

##
##  Returns the human-readable null-terminated C string that represents
##   the name of the memory category.  This string should never be freed.
##

proc getTUResourceUsageName*(
  kind: CXIndexOptionsCXTUResourceUsageKind
): cstring {.importc: "clang_getTUResourceUsageName", dynlib: CLangLib.}

type CXIndexOptionsCXTUResourceUsageEntry* {.bycopy.} = object
  ##  The memory usage category.
  kind*: CXIndexOptionsCXTUResourceUsageKind
  ##  Amount of resources used.
  ##       The units will depend on the resource kind.
  amount*: culong

##
##  The memory usage of a CXTranslationUnit, broken into categories.
##

type CXIndexOptionsCXTUResourceUsage* {.bycopy.} = object
  ##  Private data member, used for queries.
  data*: pointer
  ##  The number of entries in the 'entries' array.
  numEntries*: cuint
  ##  An array of key-value pairs, representing the breakdown of memory
  ##             usage.
  entries*: ptr CXIndexOptionsCXTUResourceUsageEntry

##
##  Return the memory usage of a translation unit.  This object
##   should be released with clang_disposeCXTUResourceUsage().
##

proc getCXTUResourceUsage*(
  tu: CXTranslationUnit
): CXIndexOptionsCXTUResourceUsage {.
  importc: "clang_getCXTUResourceUsage", dynlib: CLangLib
.}

proc disposeCXTUResourceUsage*(
  usage: CXIndexOptionsCXTUResourceUsage
) {.importc: "clang_disposeCXTUResourceUsage", dynlib: CLangLib.}

##
##  Get target information for this translation unit.
##
##  The CXTargetInfo object cannot outlive the CXTranslationUnit object.
##

proc getTranslationUnitTargetInfo*(
  cTUnit: CXTranslationUnit
): CXTargetInfo {.importc: "clang_getTranslationUnitTargetInfo", dynlib: CLangLib.}

##
##  Destroy the CXTargetInfo object.
##

proc targetInfoDispose*(
  info: CXTargetInfo
) {.importc: "clang_TargetInfo_dispose", dynlib: CLangLib.}

##
##  Get the normalized target triple as a string.
##
##  Returns the empty string in case of any error.
##

proc targetInfoGetTriple*(
  info: CXTargetInfo
): CXString {.importc: "clang_TargetInfo_getTriple", dynlib: CLangLib.}

##
##  Get the pointer width of the target in bits.
##
##  Returns -1 in case of error.
##

proc targetInfoGetPointerWidth*(
  info: CXTargetInfo
): cint {.importc: "clang_TargetInfo_getPointerWidth", dynlib: CLangLib.}

##
##  @}
##
##
##  Describes the kind of entity that a cursor refers to.
##

type CXIndexOptionsCXCursorKind* {.size: sizeof(cint).} = enum
  ##  Declarations
  ##
  ##  A declaration whose specific kind is not exposed via this
  ##  interface.
  ##
  ##  Unexposed declarations have the same operations as any other kind
  ##  of declaration; one can extract their location information,
  ##  spelling, find their definitions, etc. However, the specific kind
  ##  of the declaration is not reported.
  ##
  CXCursorUnexposedDecl = 1 ##  A C or C++ struct.
  CXCursorStructDecl = 2 ##  A C or C++ union.
  CXCursorUnionDecl = 3 ##  A C++ class.
  CXCursorClassDecl = 4 ##  An enumeration.
  CXCursorEnumDecl = 5
    ##
    ##  A field (in C) or non-static data member (in C++) in a
    ##  struct, union, or C++ class.
    ##
  CXCursorFieldDecl = 6 ##  An enumerator constant.
  CXCursorEnumConstantDecl = 7 ##  A function.
  CXCursorFunctionDecl = 8 ##  A variable.
  CXCursorVarDecl = 9 ##  A function or method parameter.
  CXCursorParmDecl = 10 ##  An Objective-C \@interface.
  CXCursorObjCInterfaceDecl = 11 ##  An Objective-C \@interface for a category.
  CXCursorObjCCategoryDecl = 12 ##  An Objective-C \@protocol declaration.
  CXCursorObjCProtocolDecl = 13 ##  An Objective-C \@property declaration.
  CXCursorObjCPropertyDecl = 14 ##  An Objective-C instance variable.
  CXCursorObjCIvarDecl = 15 ##  An Objective-C instance method.
  CXCursorObjCInstanceMethodDecl = 16 ##  An Objective-C class method.
  CXCursorObjCClassMethodDecl = 17 ##  An Objective-C \@implementation.
  CXCursorObjCImplementationDecl = 18 ##  An Objective-C \@implementation for a category.
  CXCursorObjCCategoryImplDecl = 19 ##  A typedef.
  CXCursorTypedefDecl = 20 ##  A C++ class method.
  CXCursorCXXMethod = 21 ##  A C++ namespace.
  CXCursorNamespace = 22 ##  A linkage specification, e.g. 'extern "C"'.
  CXCursorLinkageSpec = 23 ##  A C++ constructor.
  CXCursorConstructor = 24 ##  A C++ destructor.
  CXCursorDestructor = 25 ##  A C++ conversion function.
  CXCursorConversionFunction = 26 ##  A C++ template type parameter.
  CXCursorTemplateTypeParameter = 27 ##  A C++ non-type template parameter.
  CXCursorNonTypeTemplateParameter = 28 ##  A C++ template template parameter.
  CXCursorTemplateTemplateParameter = 29 ##  A C++ function template.
  CXCursorFunctionTemplate = 30 ##  A C++ class template.
  CXCursorClassTemplate = 31 ##  A C++ class template partial specialization.
  CXCursorClassTemplatePartialSpecialization = 32 ##  A C++ namespace alias declaration.
  CXCursorNamespaceAlias = 33 ##  A C++ using directive.
  CXCursorUsingDirective = 34 ##  A C++ using declaration.
  CXCursorUsingDeclaration = 35 ##  A C++ alias declaration
  CXCursorTypeAliasDecl = 36 ##  An Objective-C \@synthesize definition.
  CXCursorObjCSynthesizeDecl = 37 ##  An Objective-C \@dynamic definition.
  CXCursorObjCDynamicDecl = 38 ##  An access specifier.
  CXCursorCXXAccessSpecifier = 39
  # CXCursorFirstDecl = cXCursorUnexposedDecl
  # CXCursorLastDecl = cXCursorCXXAccessSpecifier ##  References
  CXCursorFirstRef = 40 ##  Decl references
  CXCursorObjCProtocolRef = 41
  CXCursorObjCClassRef = 42
    ##
    ##  A reference to a type declaration.
    ##
    ##  A type reference occurs anywhere where a type is named but not
    ##  declared. For example, given:
    ##
    ##  \code
    ##  typedef unsigned size_type;
    ##  size_type size;
    ##  \endcode
    ##
    ##  The typedef is a declaration of size_type (CXCursor_TypedefDecl),
    ##  while the type of the variable "size" is referenced. The cursor
    ##  referenced by the type of size is the typedef for size_type.
    ##
  CXCursorTypeRef = 43
  CXCursorCXXBaseSpecifier = 44
    ##
    ##  A reference to a class template, function template, template
    ##  template parameter, or class template partial specialization.
    ##
  CXCursorTemplateRef = 45
    ##
    ##  A reference to a namespace or namespace alias.
    ##
  CXCursorNamespaceRef = 46
    ##
    ##  A reference to a member of a struct, union, or class that occurs in
    ##  some non-expression context, e.g., a designated initializer.
    ##
  CXCursorMemberRef = 47
    ##
    ##  A reference to a labeled statement.
    ##
    ##  This cursor kind is used to describe the jump to "start_over" in the
    ##  goto statement in the following example:
    ##
    ##  \code
    ##    start_over:
    ##      ++counter;
    ##
    ##      goto start_over;
    ##  \endcode
    ##
    ##  A label reference cursor refers to a label statement.
    ##
  CXCursorLabelRef = 48
    ##
    ##  A reference to a set of overloaded functions or function templates
    ##  that has not yet been resolved to a specific function or function template.
    ##
    ##  An overloaded declaration reference cursor occurs in C++ templates where
    ##  a dependent name refers to a function. For example:
    ##
    ##  \code
    ##  template<typename T> void swap(T&, T&);
    ##
    ##  struct X { ... };
    ##  void swap(X&, X&);
    ##
    ##  template<typename T>
    ##  void reverse(T* first, T* last) {
    ##    while (first < last - 1) {
    ##      swap(*first, *--last);
    ##      ++first;
    ##    }
    ##  }
    ##
    ##  struct Y { };
    ##  void swap(Y&, Y&);
    ##  \endcode
    ##
    ##  Here, the identifier "swap" is associated with an overloaded declaration
    ##  reference. In the template definition, "swap" refers to either of the two
    ##  "swap" functions declared above, so both results will be available. At
    ##  instantiation time, "swap" may also refer to other functions found via
    ##  argument-dependent lookup (e.g., the "swap" function at the end of the
    ##  example).
    ##
    ##  The functions \c clang_getNumOverloadedDecls() and
    ##  \c clang_getOverloadedDecl() can be used to retrieve the definitions
    ##  referenced by this cursor.
    ##
  CXCursorOverloadedDeclRef = 49
    ##
    ##  A reference to a variable that occurs in some non-expression
    ##  context, e.g., a C++ lambda capture list.
    ##
  CXCursorVariableRef = 50
  # CXCursorLastRef = cXCursorVariableRef ##  Error conditions
  CXCursorFirstInvalid = 70
  CXCursorNoDeclFound = 71
  CXCursorNotImplemented = 72
  CXCursorInvalidCode = 73
  # CXCursorLastInvalid = cXCursorInvalidCode ##  Expressions
  CXCursorFirstExpr = 100
    ##
    ##  An expression whose specific kind is not exposed via this
    ##  interface.
    ##
    ##  Unexposed expressions have the same operations as any other kind
    ##  of expression; one can extract their location information,
    ##  spelling, children, etc. However, the specific kind of the
    ##  expression is not reported.
    ##
  CXCursorDeclRefExpr = 101
    ##
    ##  An expression that refers to a member of a struct, union,
    ##  class, Objective-C class, etc.
    ##
  CXCursorMemberRefExpr = 102 ##  An expression that calls a function.
  CXCursorCallExpr = 103
    ##  An expression that sends a message to an Objective-C
    ##    object or class.
  CXCursorObjCMessageExpr = 104 ##  An expression that represents a block literal.
  CXCursorBlockExpr = 105
    ##  An integer literal.
    ##
  CXCursorIntegerLiteral = 106
    ##  A floating point number literal.
    ##
  CXCursorFloatingLiteral = 107
    ##  An imaginary number literal.
    ##
  CXCursorImaginaryLiteral = 108
    ##  A string literal.
    ##
  CXCursorStringLiteral = 109
    ##  A character literal.
    ##
  CXCursorCharacterLiteral = 110
    ##  A parenthesized expression, e.g. "(1)".
    ##
    ##  This AST node is only formed if full location information is requested.
    ##
  CXCursorParenExpr = 111
    ##  This represents the unary-expression's (except sizeof and
    ##  alignof).
    ##
  CXCursorUnaryOperator = 112
    ##  [C99 6.5.2.1] Array Subscripting.
    ##
  CXCursorArraySubscriptExpr = 113
    ##  A builtin binary operation expression such as "x + y" or
    ##  "x <= y".
    ##
  CXCursorBinaryOperator = 114
    ##  Compound assignment such as "+=".
    ##
  CXCursorCompoundAssignOperator = 115
    ##  The ?: ternary operator.
    ##
  CXCursorConditionalOperator = 116
    ##  An explicit cast in C (C99 6.5.4) or a C-style cast in C++
    ##  (C++ [expr.cast]), which uses the syntax (Type)expr.
    ##
    ##  For example: (int)f.
    ##
  CXCursorCStyleCastExpr = 117
    ##  [C99 6.5.2.5]
    ##
  CXCursorCompoundLiteralExpr = 118
    ##  Describes an C or C++ initializer list.
    ##
  CXCursorInitListExpr = 119
    ##  The GNU address of label extension, representing &&label.
    ##
  CXCursorAddrLabelExpr = 120
    ##  This is the GNU Statement Expression extension: ({int X=4; X;})
    ##
  CXCursorStmtExpr = 121
    ##  Represents a C11 generic selection.
    ##
  CXCursorGenericSelectionExpr = 122
    ##  Implements the GNU __null extension, which is a name for a null
    ##  pointer constant that has integral type (e.g., int or long) and is the same
    ##  size and alignment as a pointer.
    ##
    ##  The __null extension is typically only used by system headers, which define
    ##  NULL as __null in C++ rather than using 0 (which is an integer that may not
    ##  match the size of a pointer).
    ##
  CXCursorGNUNullExpr = 123
    ##  C++'s static_cast<> expression.
    ##
  CXCursorCXXStaticCastExpr = 124
    ##  C++'s dynamic_cast<> expression.
    ##
  CXCursorCXXDynamicCastExpr = 125
    ##  C++'s reinterpret_cast<> expression.
    ##
  CXCursorCXXReinterpretCastExpr = 126
    ##  C++'s const_cast<> expression.
    ##
  CXCursorCXXConstCastExpr = 127
    ##  Represents an explicit C++ type conversion that uses "functional"
    ##  notion (C++ [expr.type.conv]).
    ##
    ##  Example:
    ##  \code
    ##    x = int(0.5);
    ##  \endcode
    ##
  CXCursorCXXFunctionalCastExpr = 128
    ##  A C++ typeid expression (C++ [expr.typeid]).
    ##
  CXCursorCXXTypeidExpr = 129
    ##  [C++ 2.13.5] C++ Boolean Literal.
    ##
  CXCursorCXXBoolLiteralExpr = 130
    ##  [C++0x 2.14.7] C++ Pointer Literal.
    ##
  CXCursorCXXNullPtrLiteralExpr = 131
    ##  Represents the "this" expression in C++
    ##
  CXCursorCXXThisExpr = 132
    ##  [C++ 15] C++ Throw Expression.
    ##
    ##  This handles 'throw' and 'throw' assignment-expression. When
    ##  assignment-expression isn't present, Op will be null.
    ##
  CXCursorCXXThrowExpr = 133
    ##  A new expression for memory allocation and constructor calls, e.g:
    ##  "new CXXNewExpr(foo)".
    ##
  CXCursorCXXNewExpr = 134
    ##  A delete expression for memory deallocation and destructor calls,
    ##  e.g. "delete[] pArray".
    ##
  CXCursorCXXDeleteExpr = 135
    ##  A unary expression. (noexcept, sizeof, or other traits)
    ##
  CXCursorUnaryExpr = 136
    ##  An Objective-C string literal i.e. @"foo".
    ##
  CXCursorObjCStringLiteral = 137
    ##  An Objective-C \@encode expression.
    ##
  CXCursorObjCEncodeExpr = 138
    ##  An Objective-C \@selector expression.
    ##
  CXCursorObjCSelectorExpr = 139
    ##  An Objective-C \@protocol expression.
    ##
  CXCursorObjCProtocolExpr = 140
    ##  An Objective-C "bridged" cast expression, which casts between
    ##  Objective-C pointers and C pointers, transferring ownership in the process.
    ##
    ##  \code
    ##    NSString *str = (__bridge_transfer NSString *)CFCreateString();
    ##  \endcode
    ##
  CXCursorObjCBridgedCastExpr = 141
    ##  Represents a C++0x pack expansion that produces a sequence of
    ##  expressions.
    ##
    ##  A pack expansion expression contains a pattern (which itself is an
    ##  expression) followed by an ellipsis. For example:
    ##
    ##  \code
    ##  template<typename F, typename ...Types>
    ##  void forward(F f, Types &&...args) {
    ##   f(static_cast<Types&&>(args)...);
    ##  }
    ##  \endcode
    ##
  CXCursorPackExpansionExpr = 142
    ##  Represents an expression that computes the length of a parameter
    ##  pack.
    ##
    ##  \code
    ##  template<typename ...Types>
    ##  struct count {
    ##    static const unsigned value = sizeof...(Types);
    ##  };
    ##  \endcode
    ##
  CXCursorSizeOfPackExpr = 143
    ##  Represents a C++ lambda expression that produces a local function
    ##  object.
    ##
    ##  \code
    ##  void abssort(float *x, unsigned N) {
    ##    std::sort(x, x + N,
    ##              [](float a, float b) {
    ##                return std::abs(a) < std::abs(b);
    ##              });
    ##  }
    ##  \endcode
    ##
  CXCursorLambdaExpr = 144
    ##  Objective-c Boolean Literal.
    ##
  CXCursorObjCBoolLiteralExpr = 145
    ##  Represents the "self" expression in an Objective-C method.
    ##
  CXCursorObjCSelfExpr = 146
    ##  OpenMP 5.0 [2.1.5, Array Section].
    ##  OpenACC 3.3 [2.7.1, Data Specification for Data Clauses (Sub Arrays)]
    ##
  CXCursorArraySectionExpr = 147
    ##  Represents an @available(...) check.
    ##
  CXCursorObjCAvailabilityCheckExpr = 148
    ##
    ##  Fixed point literal
    ##
  CXCursorFixedPointLiteral = 149
    ##  OpenMP 5.0 [2.1.4, Array Shaping].
    ##
  CXCursorOMPArrayShapingExpr = 150
    ##
    ##  OpenMP 5.0 [2.1.6 Iterators]
    ##
  CXCursorOMPIteratorExpr = 151
    ##  OpenCL's addrspace_cast<> expression.
    ##
  CXCursorCXXAddrspaceCastExpr = 152
    ##
    ##  Expression that references a C++20 concept.
    ##
  CXCursorConceptSpecializationExpr = 153
    ##
    ##  Expression that references a C++20 requires expression.
    ##
  CXCursorRequiresExpr = 154
    ##
    ##  Expression that references a C++20 parenthesized list aggregate
    ##  initializer.
    ##
  CXCursorCXXParenListInitExpr = 155
    ##
    ##   Represents a C++26 pack indexing expression.
    ##
  CXCursorPackIndexingExpr = 156
  # CXCursorLastExpr = cXCursorPackIndexingExpr ##  Statements
  CXCursorFirstStmt = 200
    ##
    ##  A statement whose specific kind is not exposed via this
    ##  interface.
    ##
    ##  Unexposed statements have the same operations as any other kind of
    ##  statement; one can extract their location information, spelling,
    ##  children, etc. However, the specific kind of the statement is not
    ##  reported.
    ##
  CXCursorLabelStmt = 201
    ##  A group of statements like { stmt stmt }.
    ##
    ##  This cursor kind is used to describe compound statements, e.g. function
    ##  bodies.
    ##
  CXCursorCompoundStmt = 202
    ##  A case statement.
    ##
  CXCursorCaseStmt = 203
    ##  A default statement.
    ##
  CXCursorDefaultStmt = 204
    ##  An if statement
    ##
  CXCursorIfStmt = 205
    ##  A switch statement.
    ##
  CXCursorSwitchStmt = 206
    ##  A while statement.
    ##
  CXCursorWhileStmt = 207
    ##  A do statement.
    ##
  CXCursorDoStmt = 208
    ##  A for statement.
    ##
  CXCursorForStmt = 209
    ##  A goto statement.
    ##
  CXCursorGotoStmt = 210
    ##  An indirect goto statement.
    ##
  CXCursorIndirectGotoStmt = 211
    ##  A continue statement.
    ##
  CXCursorContinueStmt = 212
    ##  A break statement.
    ##
  CXCursorBreakStmt = 213
    ##  A return statement.
    ##
  CXCursorReturnStmt = 214
    ##  A GCC inline assembly statement extension.
    ##
  CXCursorGCCAsmStmt = 215
  # CXCursorAsmStmt = cXCursorGCCAsmStmt
  ##  Objective-C's overall \@try-\@catch-\@finally statement.
  ##
  CXCursorObjCAtTryStmt = 216
    ##  Objective-C's \@catch statement.
    ##
  CXCursorObjCAtCatchStmt = 217
    ##  Objective-C's \@finally statement.
    ##
  CXCursorObjCAtFinallyStmt = 218
    ##  Objective-C's \@throw statement.
    ##
  CXCursorObjCAtThrowStmt = 219
    ##  Objective-C's \@synchronized statement.
    ##
  CXCursorObjCAtSynchronizedStmt = 220
    ##  Objective-C's autorelease pool statement.
    ##
  CXCursorObjCAutoreleasePoolStmt = 221
    ##  Objective-C's collection statement.
    ##
  CXCursorObjCForCollectionStmt = 222
    ##  C++'s catch statement.
    ##
  CXCursorCXXCatchStmt = 223
    ##  C++'s try statement.
    ##
  CXCursorCXXTryStmt = 224
    ##  C++'s for (* : *) statement.
    ##
  CXCursorCXXForRangeStmt = 225
    ##  Windows Structured Exception Handling's try statement.
    ##
  CXCursorSEHTryStmt = 226
    ##  Windows Structured Exception Handling's except statement.
    ##
  CXCursorSEHExceptStmt = 227
    ##  Windows Structured Exception Handling's finally statement.
    ##
  CXCursorSEHFinallyStmt = 228
    ##  A MS inline assembly statement extension.
    ##
  CXCursorMSAsmStmt = 229
    ##  The null statement ";": C99 6.8.3p3.
    ##
    ##  This cursor kind is used to describe the null statement.
    ##
  CXCursorNullStmt = 230
    ##  Adaptor class for mixing declarations with statements and
    ##  expressions.
    ##
  CXCursorDeclStmt = 231
    ##  OpenMP parallel directive.
    ##
  CXCursorOMPParallelDirective = 232
    ##  OpenMP SIMD directive.
    ##
  CXCursorOMPSimdDirective = 233
    ##  OpenMP for directive.
    ##
  CXCursorOMPForDirective = 234
    ##  OpenMP sections directive.
    ##
  CXCursorOMPSectionsDirective = 235
    ##  OpenMP section directive.
    ##
  CXCursorOMPSectionDirective = 236
    ##  OpenMP single directive.
    ##
  CXCursorOMPSingleDirective = 237
    ##  OpenMP parallel for directive.
    ##
  CXCursorOMPParallelForDirective = 238
    ##  OpenMP parallel sections directive.
    ##
  CXCursorOMPParallelSectionsDirective = 239
    ##  OpenMP task directive.
    ##
  CXCursorOMPTaskDirective = 240
    ##  OpenMP master directive.
    ##
  CXCursorOMPMasterDirective = 241
    ##  OpenMP critical directive.
    ##
  CXCursorOMPCriticalDirective = 242
    ##  OpenMP taskyield directive.
    ##
  CXCursorOMPTaskyieldDirective = 243
    ##  OpenMP barrier directive.
    ##
  CXCursorOMPBarrierDirective = 244
    ##  OpenMP taskwait directive.
    ##
  CXCursorOMPTaskwaitDirective = 245
    ##  OpenMP flush directive.
    ##
  CXCursorOMPFlushDirective = 246
    ##  Windows Structured Exception Handling's leave statement.
    ##
  CXCursorSEHLeaveStmt = 247
    ##  OpenMP ordered directive.
    ##
  CXCursorOMPOrderedDirective = 248
    ##  OpenMP atomic directive.
    ##
  CXCursorOMPAtomicDirective = 249
    ##  OpenMP for SIMD directive.
    ##
  CXCursorOMPForSimdDirective = 250
    ##  OpenMP parallel for SIMD directive.
    ##
  CXCursorOMPParallelForSimdDirective = 251
    ##  OpenMP target directive.
    ##
  CXCursorOMPTargetDirective = 252
    ##  OpenMP teams directive.
    ##
  CXCursorOMPTeamsDirective = 253
    ##  OpenMP taskgroup directive.
    ##
  CXCursorOMPTaskgroupDirective = 254
    ##  OpenMP cancellation point directive.
    ##
  CXCursorOMPCancellationPointDirective = 255
    ##  OpenMP cancel directive.
    ##
  CXCursorOMPCancelDirective = 256
    ##  OpenMP target data directive.
    ##
  CXCursorOMPTargetDataDirective = 257
    ##  OpenMP taskloop directive.
    ##
  CXCursorOMPTaskLoopDirective = 258
    ##  OpenMP taskloop simd directive.
    ##
  CXCursorOMPTaskLoopSimdDirective = 259
    ##  OpenMP distribute directive.
    ##
  CXCursorOMPDistributeDirective = 260
    ##  OpenMP target enter data directive.
    ##
  CXCursorOMPTargetEnterDataDirective = 261
    ##  OpenMP target exit data directive.
    ##
  CXCursorOMPTargetExitDataDirective = 262
    ##  OpenMP target parallel directive.
    ##
  CXCursorOMPTargetParallelDirective = 263
    ##  OpenMP target parallel for directive.
    ##
  CXCursorOMPTargetParallelForDirective = 264
    ##  OpenMP target update directive.
    ##
  CXCursorOMPTargetUpdateDirective = 265
    ##  OpenMP distribute parallel for directive.
    ##
  CXCursorOMPDistributeParallelForDirective = 266
    ##  OpenMP distribute parallel for simd directive.
    ##
  CXCursorOMPDistributeParallelForSimdDirective = 267
    ##  OpenMP distribute simd directive.
    ##
  CXCursorOMPDistributeSimdDirective = 268
    ##  OpenMP target parallel for simd directive.
    ##
  CXCursorOMPTargetParallelForSimdDirective = 269
    ##  OpenMP target simd directive.
    ##
  CXCursorOMPTargetSimdDirective = 270
    ##  OpenMP teams distribute directive.
    ##
  CXCursorOMPTeamsDistributeDirective = 271
    ##  OpenMP teams distribute simd directive.
    ##
  CXCursorOMPTeamsDistributeSimdDirective = 272
    ##  OpenMP teams distribute parallel for simd directive.
    ##
  CXCursorOMPTeamsDistributeParallelForSimdDirective = 273
    ##  OpenMP teams distribute parallel for directive.
    ##
  CXCursorOMPTeamsDistributeParallelForDirective = 274
    ##  OpenMP target teams directive.
    ##
  CXCursorOMPTargetTeamsDirective = 275
    ##  OpenMP target teams distribute directive.
    ##
  CXCursorOMPTargetTeamsDistributeDirective = 276
    ##  OpenMP target teams distribute parallel for directive.
    ##
  CXCursorOMPTargetTeamsDistributeParallelForDirective = 277
    ##  OpenMP target teams distribute parallel for simd directive.
    ##
  CXCursorOMPTargetTeamsDistributeParallelForSimdDirective = 278
    ##  OpenMP target teams distribute simd directive.
    ##
  CXCursorOMPTargetTeamsDistributeSimdDirective = 279
    ##  C++2a std::bit_cast expression.
    ##
  CXCursorBuiltinBitCastExpr = 280
    ##  OpenMP master taskloop directive.
    ##
  CXCursorOMPMasterTaskLoopDirective = 281
    ##  OpenMP parallel master taskloop directive.
    ##
  CXCursorOMPParallelMasterTaskLoopDirective = 282
    ##  OpenMP master taskloop simd directive.
    ##
  CXCursorOMPMasterTaskLoopSimdDirective = 283
    ##  OpenMP parallel master taskloop simd directive.
    ##
  CXCursorOMPParallelMasterTaskLoopSimdDirective = 284
    ##  OpenMP parallel master directive.
    ##
  CXCursorOMPParallelMasterDirective = 285
    ##  OpenMP depobj directive.
    ##
  CXCursorOMPDepobjDirective = 286
    ##  OpenMP scan directive.
    ##
  CXCursorOMPScanDirective = 287
    ##  OpenMP tile directive.
    ##
  CXCursorOMPTileDirective = 288
    ##  OpenMP canonical loop.
    ##
  CXCursorOMPCanonicalLoop = 289
    ##  OpenMP interop directive.
    ##
  CXCursorOMPInteropDirective = 290
    ##  OpenMP dispatch directive.
    ##
  CXCursorOMPDispatchDirective = 291
    ##  OpenMP masked directive.
    ##
  CXCursorOMPMaskedDirective = 292
    ##  OpenMP unroll directive.
    ##
  CXCursorOMPUnrollDirective = 293
    ##  OpenMP metadirective directive.
    ##
  CXCursorOMPMetaDirective = 294
    ##  OpenMP loop directive.
    ##
  CXCursorOMPGenericLoopDirective = 295
    ##  OpenMP teams loop directive.
    ##
  CXCursorOMPTeamsGenericLoopDirective = 296
    ##  OpenMP target teams loop directive.
    ##
  CXCursorOMPTargetTeamsGenericLoopDirective = 297
    ##  OpenMP parallel loop directive.
    ##
  CXCursorOMPParallelGenericLoopDirective = 298
    ##  OpenMP target parallel loop directive.
    ##
  CXCursorOMPTargetParallelGenericLoopDirective = 299
    ##  OpenMP parallel masked directive.
    ##
  CXCursorOMPParallelMaskedDirective = 300
    ##  OpenMP masked taskloop directive.
    ##
  CXCursorOMPMaskedTaskLoopDirective = 301
    ##  OpenMP masked taskloop simd directive.
    ##
  CXCursorOMPMaskedTaskLoopSimdDirective = 302
    ##  OpenMP parallel masked taskloop directive.
    ##
  CXCursorOMPParallelMaskedTaskLoopDirective = 303
    ##  OpenMP parallel masked taskloop simd directive.
    ##
  CXCursorOMPParallelMaskedTaskLoopSimdDirective = 304
    ##  OpenMP error directive.
    ##
  CXCursorOMPErrorDirective = 305
    ##  OpenMP scope directive.
    ##
  CXCursorOMPScopeDirective = 306
    ##  OpenMP reverse directive.
    ##
  CXCursorOMPReverseDirective = 307
    ##  OpenMP interchange directive.
    ##
  CXCursorOMPInterchangeDirective = 308
    ##  OpenMP assume directive.
    ##
  CXCursorOMPAssumeDirective = 309
    ##  OpenMP assume directive.
    ##
  CXCursorOMPStripeDirective = 310
    ##  OpenMP fuse directive
    ##
  CXCursorOMPFuseDirective = 311
    ##  OpenACC Compute Construct.
    ##
  CXCursorOpenACCComputeConstruct = 320
    ##  OpenACC Loop Construct.
    ##
  CXCursorOpenACCLoopConstruct = 321
    ##  OpenACC Combined Constructs.
    ##
  CXCursorOpenACCCombinedConstruct = 322
    ##  OpenACC data Construct.
    ##
  CXCursorOpenACCDataConstruct = 323
    ##  OpenACC enter data Construct.
    ##
  CXCursorOpenACCEnterDataConstruct = 324
    ##  OpenACC exit data Construct.
    ##
  CXCursorOpenACCExitDataConstruct = 325
    ##  OpenACC host_data Construct.
    ##
  CXCursorOpenACCHostDataConstruct = 326
    ##  OpenACC wait Construct.
    ##
  CXCursorOpenACCWaitConstruct = 327
    ##  OpenACC init Construct.
    ##
  CXCursorOpenACCInitConstruct = 328
    ##  OpenACC shutdown Construct.
    ##
  CXCursorOpenACCShutdownConstruct = 329
    ##  OpenACC set Construct.
    ##
  CXCursorOpenACCSetConstruct = 330
    ##  OpenACC update Construct.
    ##
  CXCursorOpenACCUpdateConstruct = 331
    ##  OpenACC atomic Construct.
    ##
  CXCursorOpenACCAtomicConstruct = 332
    ##  OpenACC cache Construct.
    ##
  CXCursorOpenACCCacheConstruct = 333
  # CXCursorLastStmt = cXCursorOpenACCCacheConstruct
  ##
  ##  Cursor that represents the translation unit itself.
  ##
  ##  The translation unit cursor exists primarily to act as the root
  ##  cursor for traversing the contents of a translation unit.
  ##
  CXCursorTranslationUnit = 350 ##  Attributes
  CXCursorFirstAttr = 400
    ##
    ##  An attribute whose specific kind is not exposed via this
    ##  interface.
    ##
  CXCursorIBActionAttr = 401
  CXCursorIBOutletAttr = 402
  CXCursorIBOutletCollectionAttr = 403
  CXCursorCXXFinalAttr = 404
  CXCursorCXXOverrideAttr = 405
  CXCursorAnnotateAttr = 406
  CXCursorAsmLabelAttr = 407
  CXCursorPackedAttr = 408
  CXCursorPureAttr = 409
  CXCursorConstAttr = 410
  CXCursorNoDuplicateAttr = 411
  CXCursorCUDAConstantAttr = 412
  CXCursorCUDADeviceAttr = 413
  CXCursorCUDAGlobalAttr = 414
  CXCursorCUDAHostAttr = 415
  CXCursorCUDASharedAttr = 416
  CXCursorVisibilityAttr = 417
  CXCursorDLLExport = 418
  CXCursorDLLImport = 419
  CXCursorNSReturnsRetained = 420
  CXCursorNSReturnsNotRetained = 421
  CXCursorNSReturnsAutoreleased = 422
  CXCursorNSConsumesSelf = 423
  CXCursorNSConsumed = 424
  CXCursorObjCException = 425
  CXCursorObjCNSObject = 426
  CXCursorObjCIndependentClass = 427
  CXCursorObjCPreciseLifetime = 428
  CXCursorObjCReturnsInnerPointer = 429
  CXCursorObjCRequiresSuper = 430
  CXCursorObjCRootClass = 431
  CXCursorObjCSubclassingRestricted = 432
  CXCursorObjCExplicitProtocolImpl = 433
  CXCursorObjCDesignatedInitializer = 434
  CXCursorObjCRuntimeVisible = 435
  CXCursorObjCBoxable = 436
  CXCursorFlagEnum = 437
  CXCursorConvergentAttr = 438
  CXCursorWarnUnusedAttr = 439
  CXCursorWarnUnusedResultAttr = 440
  CXCursorAlignedAttr = 441
  # CXCursorLastAttr = cXCursorAlignedAttr ##  Preprocessing
  CXCursorPreprocessingDirective = 500
  CXCursorMacroDefinition = 501
  CXCursorMacroExpansion = 502
  # CXCursorMacroInstantiation = cXCursorMacroExpansion
  CXCursorInclusionDirective = 503
  # CXCursorFirstPreprocessing = cXCursorPreprocessingDirective
  # CXCursorLastPreprocessing = cXCursorInclusionDirective
  ##  Extra Declarations
  ##
  ##  A module import declaration.
  ##
  CXCursorModuleImportDecl = 600
  CXCursorTypeAliasTemplateDecl = 601
    ##
    ##  A static_assert or _Static_assert node
    ##
  CXCursorStaticAssert = 602
    ##
    ##  a friend declaration.
    ##
  CXCursorFriendDecl = 603
    ##
    ##  a concept declaration.
    ##
  CXCursorConceptDecl = 604
  # CXCursorFirstExtraDecl = cXCursorModuleImportDecl
  # CXCursorLastExtraDecl = cXCursorConceptDecl
  ##
  ##  A code completion overload candidate.
  ##
  CXCursorOverloadCandidate = 700

const
  CXCursorObjCSuperClassRef = CXCursorFirstRef
  CXCursorInvalidFile = CXCursorFirstInvalid
  CXCursorUnexposedExpr = CXCursorFirstExpr
  CXCursorUnexposedStmt = CXCursorFirstStmt
  CXCursorUnexposedAttr = CXCursorFirstAttr

##
##  A cursor representing some element in the abstract syntax tree for
##  a translation unit.
##
##  The cursor abstraction unifies the different kinds of entities in a
##  program--declaration, statements, expressions, references to declarations,
##  etc.--under a single "cursor" abstraction with a common set of operations.
##  Common operation for a cursor include: getting the physical location in
##  a source file where the cursor points, getting the name associated with a
##  cursor, and retrieving cursors for any child nodes of a particular cursor.
##
##  Cursors can be produced in two specific ways.
##  clang_getTranslationUnitCursor() produces a cursor for a translation unit,
##  from which one can use clang_visitChildren() to explore the rest of the
##  translation unit. clang_getCursor() maps from a physical source location
##  to the entity that resides at that location, allowing one to map from the
##  source code into the AST.
##

type CXIndexOptionsCXCursor* {.bycopy.} = object
  kind*: CXIndexOptionsCXCursorKind
  xdata*: cint
  data*: array[3, pointer]

##
##  \defgroup CINDEX_CURSOR_MANIP Cursor manipulations
##
##  @{
##
##
##  Retrieve the NULL cursor, which represents no entity.
##

proc getNullCursor*(): CXIndexOptionsCXCursor {.
  importc: "clang_getNullCursor", dynlib: CLangLib
.}

##
##  Retrieve the cursor that represents the given translation unit.
##
##  The translation unit cursor can be used to start traversing the
##  various declarations within the given translation unit.
##

proc getTranslationUnitCursor*(
  a1: CXTranslationUnit
): CXIndexOptionsCXCursor {.
  importc: "clang_getTranslationUnitCursor", dynlib: CLangLib
.}

##
##  Determine whether two cursors are equivalent.
##

proc equalCursors*(
  a1: CXIndexOptionsCXCursor, a2: CXIndexOptionsCXCursor
): cuint {.importc: "clang_equalCursors", dynlib: CLangLib.}

##
##  Returns non-zero if \p cursor is null.
##

proc cursorIsNull*(
  cursor: CXIndexOptionsCXCursor
): cint {.importc: "clang_Cursor_isNull", dynlib: CLangLib.}

##
##  Compute a hash value for the given cursor.
##

proc hashCursor*(
  a1: CXIndexOptionsCXCursor
): cuint {.importc: "clang_hashCursor", dynlib: CLangLib.}

##
##  Retrieve the kind of the given cursor.
##

proc getCursorKind*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursorKind {.importc: "clang_getCursorKind", dynlib: CLangLib.}

##
##  Determine whether the given cursor kind represents a declaration.
##

proc isDeclaration*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isDeclaration", dynlib: CLangLib.}

##
##  Determine whether the given declaration is invalid.
##
##  A declaration is invalid if it could not be parsed successfully.
##
##  \returns non-zero if the cursor represents a declaration and it is
##  invalid, otherwise NULL.
##

proc isInvalidDeclaration*(
  a1: CXIndexOptionsCXCursor
): cuint {.importc: "clang_isInvalidDeclaration", dynlib: CLangLib.}

##
##  Determine whether the given cursor kind represents a simple
##  reference.
##
##  Note that other kinds of cursors (such as expressions) can also refer to
##  other cursors. Use clang_getCursorReferenced() to determine whether a
##  particular cursor refers to another entity.
##

proc isReference*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isReference", dynlib: CLangLib.}

##
##  Determine whether the given cursor kind represents an expression.
##

proc isExpression*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isExpression", dynlib: CLangLib.}

##
##  Determine whether the given cursor kind represents a statement.
##

proc isStatement*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isStatement", dynlib: CLangLib.}

##
##  Determine whether the given cursor kind represents an attribute.
##

proc isAttribute*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isAttribute", dynlib: CLangLib.}

##
##  Determine whether the given cursor has any attributes.
##

proc cursorHasAttrs*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_hasAttrs", dynlib: CLangLib.}

##
##  Determine whether the given cursor kind represents an invalid
##  cursor.
##

proc isInvalid*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isInvalid", dynlib: CLangLib.}

##
##  Determine whether the given cursor kind represents a translation
##  unit.
##

proc isTranslationUnit*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isTranslationUnit", dynlib: CLangLib.}

## *
##  Determine whether the given cursor represents a preprocessing
##  element, such as a preprocessor directive or macro instantiation.
##

proc isPreprocessing*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isPreprocessing", dynlib: CLangLib.}

## *
##  Determine whether the given cursor represents a currently
##   unexposed piece of the AST (e.g., CXCursor_UnexposedStmt).
##

proc isUnexposed*(
  a1: CXIndexOptionsCXCursorKind
): cuint {.importc: "clang_isUnexposed", dynlib: CLangLib.}

##
##  Describe the linkage of the entity referred to by a cursor.
##

type CXIndexOptionsCXLinkageKind* {.size: sizeof(cint).} = enum
  ##  This value indicates that no linkage information is available
  ##  for a provided CXCursor.
  CXLinkageInvalid
    ##
    ##  This is the linkage for variables, parameters, and so on that
    ##   have automatic storage.  This covers normal (non-extern) local variables.
    ##
  CXLinkageNoLinkage ##  This is the linkage for static variables and static functions.
  CXLinkageInternal
    ##  This is the linkage for entities with external linkage that live
    ##  in C++ anonymous namespaces.
  CXLinkageUniqueExternal
    ##  This is the linkage for entities with true, external linkage.
  CXLinkageExternal

##
##  Determine the linkage of the entity referred to by a given cursor.
##

proc getCursorLinkage*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXLinkageKind {.importc: "clang_getCursorLinkage", dynlib: CLangLib.}

type CXIndexOptionsCXVisibilityKind* {.size: sizeof(cint).} = enum
  ##  This value indicates that no visibility information is available
  ##  for a provided CXCursor.
  CXVisibilityInvalid ##  Symbol not seen by the linker.
  CXVisibilityHidden
    ##  Symbol seen by the linker but resolves to a symbol inside this object.
  CXVisibilityProtected ##  Symbol seen by the linker and acts like a normal symbol.
  CXVisibilityDefault

##
##  Describe the visibility of the entity referred to by a cursor.
##
##  This returns the default visibility if not explicitly specified by
##  a visibility attribute. The default visibility may be changed by
##  commandline arguments.
##
##  \param cursor The cursor to query.
##
##  \returns The visibility of the cursor.
##

proc getCursorVisibility*(
  cursor: CXIndexOptionsCXCursor
): CXAvailabilityKind {.importc: "clang_getCursorVisibility", dynlib: CLangLib.}

##
##  Determine the availability of the entity that this cursor refers to,
##  taking the current target platform into account.
##
##  \param cursor The cursor to query.
##
##  \returns The availability of the cursor.
##

proc getCursorAvailability*(
  cursor: CXIndexOptionsCXCursor
): CXAvailabilityKind {.importc: "clang_getCursorAvailability", dynlib: CLangLib.}

##
##  Describes the availability of a given entity on a particular platform, e.g.,
##  a particular class might only be available on Mac OS 10.7 or newer.
##

type CXIndexOptionsCXPlatformAvailability* {.bycopy.} = object
  ##
  ##  A string that describes the platform for which this structure
  ##  provides availability information.
  ##
  ##  Possible values are "ios" or "macos".
  ##
  platform*: CXString
  ##
  ##  The version number in which this entity was introduced.
  ##
  introduced*: CXVersion
  ##
  ##  The version number in which this entity was deprecated (but is
  ##  still available).
  ##
  deprecated*: CXVersion
  ##
  ##  The version number in which this entity was obsoleted, and therefore
  ##  is no longer available.
  ##
  obsoleted*: CXVersion
  ##
  ##  Whether the entity is unconditionally unavailable on this platform.
  ##
  unavailable*: cint
  ##
  ##  An optional message to provide to a user of this API, e.g., to
  ##  suggest replacement APIs.
  ##
  message*: CXString

##
##  Determine the availability of the entity that this cursor refers to
##  on any platforms for which availability information is known.
##
##  \param cursor The cursor to query.
##
##  \param always_deprecated If non-NULL, will be set to indicate whether the
##  entity is deprecated on all platforms.
##
##  \param deprecated_message If non-NULL, will be set to the message text
##  provided along with the unconditional deprecation of this entity. The client
##  is responsible for deallocating this string.
##
##  \param always_unavailable If non-NULL, will be set to indicate whether the
##  entity is unavailable on all platforms.
##
##  \param unavailable_message If non-NULL, will be set to the message text
##  provided along with the unconditional unavailability of this entity. The
##  client is responsible for deallocating this string.
##
##  \param availability If non-NULL, an array of CXPlatformAvailability instances
##  that will be populated with platform availability information, up to either
##  the number of platforms for which availability information is available (as
##  returned by this function) or \c availability_size, whichever is smaller.
##
##  \param availability_size The number of elements available in the
##  \c availability array.
##
##  \returns The number of platforms (N) for which availability information is
##  available (which is unrelated to \c availability_size).
##
##  Note that the client is responsible for calling
##  \c clang_disposeCXPlatformAvailability to free each of the
##  platform-availability structures returned. There are
##  \c min(N, availability_size) such structures.
##

proc getCursorPlatformAvailability*(
  cursor: CXIndexOptionsCXCursor,
  alwaysDeprecated: ptr cint,
  deprecatedMessage: ptr CXString,
  alwaysUnavailable: ptr cint,
  unavailableMessage: ptr CXString,
  availability: ptr CXIndexOptionsCXPlatformAvailability,
  availabilitySize: cint,
): cint {.importc: "clang_getCursorPlatformAvailability", dynlib: CLangLib.}

##
##  Free the memory associated with a \c CXPlatformAvailability structure.
##

proc disposeCXPlatformAvailability*(
  availability: ptr CXIndexOptionsCXPlatformAvailability
) {.importc: "clang_disposeCXPlatformAvailability", dynlib: CLangLib.}

##
##  If cursor refers to a variable declaration and it has initializer returns
##  cursor referring to the initializer otherwise return null cursor.
##

proc cursorGetVarDeclInitializer*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursor {.
  importc: "clang_Cursor_getVarDeclInitializer", dynlib: CLangLib
.}

##
##  If cursor refers to a variable declaration that has global storage returns 1.
##  If cursor refers to a variable declaration that doesn't have global storage
##  returns 0. Otherwise returns -1.
##

proc cursorHasVarDeclGlobalStorage*(
  cursor: CXIndexOptionsCXCursor
): cint {.importc: "clang_Cursor_hasVarDeclGlobalStorage", dynlib: CLangLib.}

##
##  If cursor refers to a variable declaration that has external storage
##  returns 1. If cursor refers to a variable declaration that doesn't have
##  external storage returns 0. Otherwise returns -1.
##

proc cursorHasVarDeclExternalStorage*(
  cursor: CXIndexOptionsCXCursor
): cint {.importc: "clang_Cursor_hasVarDeclExternalStorage", dynlib: CLangLib.}

##
##  Describe the "language" of the entity referred to by a cursor.
##

type CXIndexOptionsCXLanguageKind* {.size: sizeof(cint).} = enum
  CXLanguageInvalid = 0
  CXLanguageC
  CXLanguageObjC
  CXLanguageCPlusPlus

##
##  Determine the "language" of the entity referred to by a given cursor.
##

proc getCursorLanguage*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXLanguageKind {.importc: "clang_getCursorLanguage", dynlib: CLangLib.}

##
##  Describe the "thread-local storage (TLS) kind" of the declaration
##  referred to by a cursor.
##

type CXIndexOptionsCXTLSKind* {.size: sizeof(cint).} = enum
  CXTLS_None = 0
  CXTLS_Dynamic
  CXTLS_Static

##
##  Determine the "thread-local storage (TLS) kind" of the declaration
##  referred to by a cursor.
##

proc getCursorTLSKind*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXTLSKind {.importc: "clang_getCursorTLSKind", dynlib: CLangLib.}

##
##  Returns the translation unit that a cursor originated from.
##

proc cursorGetTranslationUnit*(
  a1: CXIndexOptionsCXCursor
): CXTranslationUnit {.importc: "clang_Cursor_getTranslationUnit", dynlib: CLangLib.}

##
##  A fast container representing a set of CXCursors.
##

type CXIndexOptionsCXCursorSet* = ptr cXCursorSetImpl

##
##  Creates an empty CXCursorSet.
##

proc createCXCursorSet*(): CXIndexOptionsCXCursorSet {.
  importc: "clang_createCXCursorSet", dynlib: CLangLib
.}

##
##  Disposes a CXCursorSet and releases its associated memory.
##

proc disposeCXCursorSet*(
  cset: CXIndexOptionsCXCursorSet
) {.importc: "clang_disposeCXCursorSet", dynlib: CLangLib.}

##
##  Queries a CXCursorSet to see if it contains a specific CXCursor.
##
##  \returns non-zero if the set contains the specified cursor.
##

proc cXCursorSetContains*(
  cset: CXIndexOptionsCXCursorSet, cursor: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXCursorSet_contains", dynlib: CLangLib.}

##
##  Inserts a CXCursor into a CXCursorSet.
##
##  \returns zero if the CXCursor was already in the set, and non-zero otherwise.
##

proc cXCursorSetInsert*(
  cset: CXIndexOptionsCXCursorSet, cursor: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXCursorSet_insert", dynlib: CLangLib.}

##
##  Determine the semantic parent of the given cursor.
##
##  The semantic parent of a cursor is the cursor that semantically contains
##  the given \p cursor. For many declarations, the lexical and semantic parents
##  are equivalent (the lexical parent is returned by
##  \c clang_getCursorLexicalParent()). They diverge when declarations or
##  definitions are provided out-of-line. For example:
##
##  \code
##  class C {
##   void f();
##  };
##
##  void C::f() { }
##  \endcode
##
##  In the out-of-line definition of \c C::f, the semantic parent is
##  the class \c C, of which this function is a member. The lexical parent is
##  the place where the declaration actually occurs in the source code; in this
##  case, the definition occurs in the translation unit. In general, the
##  lexical parent for a given entity can change without affecting the semantics
##  of the program, and the lexical parent of different declarations of the
##  same entity may be different. Changing the semantic parent of a declaration,
##  on the other hand, can have a major impact on semantics, and redeclarations
##  of a particular entity should all have the same semantic context.
##
##  In the example above, both declarations of \c C::f have \c C as their
##  semantic context, while the lexical context of the first \c C::f is \c C
##  and the lexical context of the second \c C::f is the translation unit.
##
##  For global declarations, the semantic parent is the translation unit.
##

proc getCursorSemanticParent*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursor {.importc: "clang_getCursorSemanticParent", dynlib: CLangLib.}

##
##  Determine the lexical parent of the given cursor.
##
##  The lexical parent of a cursor is the cursor in which the given \p cursor
##  was actually written. For many declarations, the lexical and semantic parents
##  are equivalent (the semantic parent is returned by
##  \c clang_getCursorSemanticParent()). They diverge when declarations or
##  definitions are provided out-of-line. For example:
##
##  \code
##  class C {
##   void f();
##  };
##
##  void C::f() { }
##  \endcode
##
##  In the out-of-line definition of \c C::f, the semantic parent is
##  the class \c C, of which this function is a member. The lexical parent is
##  the place where the declaration actually occurs in the source code; in this
##  case, the definition occurs in the translation unit. In general, the
##  lexical parent for a given entity can change without affecting the semantics
##  of the program, and the lexical parent of different declarations of the
##  same entity may be different. Changing the semantic parent of a declaration,
##  on the other hand, can have a major impact on semantics, and redeclarations
##  of a particular entity should all have the same semantic context.
##
##  In the example above, both declarations of \c C::f have \c C as their
##  semantic context, while the lexical context of the first \c C::f is \c C
##  and the lexical context of the second \c C::f is the translation unit.
##
##  For declarations written in the global scope, the lexical parent is
##  the translation unit.
##

proc getCursorLexicalParent*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursor {.importc: "clang_getCursorLexicalParent", dynlib: CLangLib.}

##
##  Determine the set of methods that are overridden by the given
##  method.
##
##  In both Objective-C and C++, a method (aka virtual member function,
##  in C++) can override a virtual method in a base class. For
##  Objective-C, a method is said to override any method in the class's
##  base class, its protocols, or its categories' protocols, that has the same
##  selector and is of the same kind (class or instance).
##  If no such method exists, the search continues to the class's superclass,
##  its protocols, and its categories, and so on. A method from an Objective-C
##  implementation is considered to override the same methods as its
##  corresponding method in the interface.
##
##  For C++, a virtual member function overrides any virtual member
##  function with the same signature that occurs in its base
##  classes. With multiple inheritance, a virtual member function can
##  override several virtual member functions coming from different
##  base classes.
##
##  In all cases, this function determines the immediate overridden
##  method, rather than all of the overridden methods. For example, if
##  a method is originally declared in a class A, then overridden in B
##  (which in inherits from A) and also in C (which inherited from B),
##  then the only overridden method returned from this function when
##  invoked on C's method will be B's method. The client may then
##  invoke this function again, given the previously-found overridden
##  methods, to map out the complete method-override set.
##
##  \param cursor A cursor representing an Objective-C or C++
##  method. This routine will compute the set of methods that this
##  method overrides.
##
##  \param overridden A pointer whose pointee will be replaced with a
##  pointer to an array of cursors, representing the set of overridden
##  methods. If there are no overridden methods, the pointee will be
##  set to NULL. The pointee must be freed via a call to
##  \c clang_disposeOverriddenCursors().
##
##  \param num_overridden A pointer to the number of overridden
##  functions, will be set to the number of overridden functions in the
##  array pointed to by \p overridden.
##

proc getOverriddenCursors*(
  cursor: CXIndexOptionsCXCursor,
  overridden: ptr ptr CXIndexOptionsCXCursor,
  numOverridden: ptr cuint,
) {.importc: "clang_getOverriddenCursors", dynlib: CLangLib.}

##
##  Free the set of overridden cursors returned by \c
##  clang_getOverriddenCursors().
##

proc disposeOverriddenCursors*(
  overridden: ptr CXIndexOptionsCXCursor
) {.importc: "clang_disposeOverriddenCursors", dynlib: CLangLib.}

##
##  Retrieve the file that is included by the given inclusion directive
##  cursor.
##

proc getIncludedFile*(
  cursor: CXIndexOptionsCXCursor
): CXFile {.importc: "clang_getIncludedFile", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_CURSOR_SOURCE Mapping between cursors and source code
##
##  Cursors represent a location within the Abstract Syntax Tree (AST). These
##  routines help map between cursors and the physical locations where the
##  described entities occur in the source code. The mapping is provided in
##  both directions, so one can map from source code to the AST and back.
##
##  @{
##
##
##  Map a source location to the cursor that describes the entity at that
##  location in the source code.
##
##  clang_getCursor() maps an arbitrary source location within a translation
##  unit down to the most specific cursor that describes the entity at that
##  location. For example, given an expression \c x + y, invoking
##  clang_getCursor() with a source location pointing to "x" will return the
##  cursor for "x"; similarly for "y". If the cursor points anywhere between
##  "x" or "y" (e.g., on the + or the whitespace around it), clang_getCursor()
##  will return a cursor referring to the "+" expression.
##
##  \returns a cursor representing the entity at the given source location, or
##  a NULL cursor if no such entity can be found.
##

proc getCursor*(
  a1: CXTranslationUnit, a2: CXSourceLocation
): CXIndexOptionsCXCursor {.importc: "clang_getCursor", dynlib: CLangLib.}

##
##  Retrieve the physical location of the source constructor referenced
##  by the given cursor.
##
##  The location of a declaration is typically the location of the name of that
##  declaration, where the name of that declaration would occur if it is
##  unnamed, or some keyword that introduces that particular declaration.
##  The location of a reference is where that reference occurs within the
##  source code.
##

proc getCursorLocation*(
  a1: CXIndexOptionsCXCursor
): CXSourceLocation {.importc: "clang_getCursorLocation", dynlib: CLangLib.}

##
##  Retrieve the physical extent of the source construct referenced by
##  the given cursor.
##
##  The extent of a cursor starts with the file/line/column pointing at the
##  first character within the source construct that the cursor refers to and
##  ends with the last character within that source construct. For a
##  declaration, the extent covers the declaration itself. For a reference,
##  the extent covers the location of the reference (e.g., where the referenced
##  entity was actually used).
##

proc getCursorExtent*(
  a1: CXIndexOptionsCXCursor
): CXSourceRange {.importc: "clang_getCursorExtent", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_TYPES Type information for CXCursors
##
##  @{
##
##
##  Describes the kind of type
##

type CXIndexOptionsCXTypeKind* {.size: sizeof(cint).} = enum
  ##
  ##  Represents an invalid type (e.g., where no type is available).
  ##
  CXTypeInvalid = 0
    ##
    ##  A type whose specific kind is not exposed via this
    ##  interface.
    ##
  CXTypeUnexposed = 1 ##  Builtin types
  CXTypeVoid = 2
  CXTypeBool = 3
  CXTypeCharU = 4
  CXTypeUChar = 5
  CXTypeChar16 = 6
  CXTypeChar32 = 7
  CXTypeUShort = 8
  CXTypeUInt = 9
  CXTypeULong = 10
  CXTypeULongLong = 11
  CXTypeUInt128 = 12
  CXTypeCharS = 13
  CXTypeSChar = 14
  CXTypeWChar = 15
  CXTypeShort = 16
  CXTypeInt = 17
  CXTypeLong = 18
  CXTypeLongLong = 19
  CXTypeInt128 = 20
  CXTypeFloat = 21
  CXTypeDouble = 22
  CXTypeLongDouble = 23
  CXTypeNullPtr = 24
  CXTypeOverload = 25
  CXTypeDependent = 26
  CXTypeObjCId = 27
  CXTypeObjCClass = 28
  CXTypeObjCSel = 29
  CXTypeFloat128 = 30
  CXTypeHalf = 31
  CXTypeFloat16 = 32
  CXTypeShortAccum = 33
  CXTypeAccum = 34
  CXTypeLongAccum = 35
  CXTypeUShortAccum = 36
  CXTypeUAccum = 37
  CXTypeULongAccum = 38
  CXTypeBFloat16 = 39
  CXTypeIbm128 = 40
  # CXTypeFirstBuiltin = cXTypeVoid
  # CXTypeLastBuiltin = cXTypeIbm128
  CXTypeComplex = 100
  CXTypePointer = 101
  CXTypeBlockPointer = 102
  CXTypeLValueReference = 103
  CXTypeRValueReference = 104
  CXTypeRecord = 105
  CXTypeEnum = 106
  CXTypeTypedef = 107
  CXTypeObjCInterface = 108
  CXTypeObjCObjectPointer = 109
  CXTypeFunctionNoProto = 110
  CXTypeFunctionProto = 111
  CXTypeConstantArray = 112
  CXTypeVector = 113
  CXTypeIncompleteArray = 114
  CXTypeVariableArray = 115
  CXTypeDependentSizedArray = 116
  CXTypeMemberPointer = 117
  CXTypeAuto = 118
    ##
    ##  Represents a type that was referred to using an elaborated type keyword.
    ##
    ##  E.g., struct S, or via a qualified name, e.g., N::M::type, or both.
    ##
  CXTypeElaborated = 119 ##  OpenCL PipeType.
  CXTypePipe = 120 ##  OpenCL builtin types.
  CXTypeOCLImage1dRO = 121
  CXTypeOCLImage1dArrayRO = 122
  CXTypeOCLImage1dBufferRO = 123
  CXTypeOCLImage2dRO = 124
  CXTypeOCLImage2dArrayRO = 125
  CXTypeOCLImage2dDepthRO = 126
  CXTypeOCLImage2dArrayDepthRO = 127
  CXTypeOCLImage2dMSAARO = 128
  CXTypeOCLImage2dArrayMSAARO = 129
  CXTypeOCLImage2dMSAADepthRO = 130
  CXTypeOCLImage2dArrayMSAADepthRO = 131
  CXTypeOCLImage3dRO = 132
  CXTypeOCLImage1dWO = 133
  CXTypeOCLImage1dArrayWO = 134
  CXTypeOCLImage1dBufferWO = 135
  CXTypeOCLImage2dWO = 136
  CXTypeOCLImage2dArrayWO = 137
  CXTypeOCLImage2dDepthWO = 138
  CXTypeOCLImage2dArrayDepthWO = 139
  CXTypeOCLImage2dMSAAWO = 140
  CXTypeOCLImage2dArrayMSAAWO = 141
  CXTypeOCLImage2dMSAADepthWO = 142
  CXTypeOCLImage2dArrayMSAADepthWO = 143
  CXTypeOCLImage3dWO = 144
  CXTypeOCLImage1dRW = 145
  CXTypeOCLImage1dArrayRW = 146
  CXTypeOCLImage1dBufferRW = 147
  CXTypeOCLImage2dRW = 148
  CXTypeOCLImage2dArrayRW = 149
  CXTypeOCLImage2dDepthRW = 150
  CXTypeOCLImage2dArrayDepthRW = 151
  CXTypeOCLImage2dMSAARW = 152
  CXTypeOCLImage2dArrayMSAARW = 153
  CXTypeOCLImage2dMSAADepthRW = 154
  CXTypeOCLImage2dArrayMSAADepthRW = 155
  CXTypeOCLImage3dRW = 156
  CXTypeOCLSampler = 157
  CXTypeOCLEvent = 158
  CXTypeOCLQueue = 159
  CXTypeOCLReserveID = 160
  CXTypeObjCObject = 161
  CXTypeObjCTypeParam = 162
  CXTypeAttributed = 163
  CXTypeOCLIntelSubgroupAVCMcePayload = 164
  CXTypeOCLIntelSubgroupAVCImePayload = 165
  CXTypeOCLIntelSubgroupAVCRefPayload = 166
  CXTypeOCLIntelSubgroupAVCSicPayload = 167
  CXTypeOCLIntelSubgroupAVCMceResult = 168
  CXTypeOCLIntelSubgroupAVCImeResult = 169
  CXTypeOCLIntelSubgroupAVCRefResult = 170
  CXTypeOCLIntelSubgroupAVCSicResult = 171
  CXTypeOCLIntelSubgroupAVCImeResultSingleReferenceStreamout = 172
  CXTypeOCLIntelSubgroupAVCImeResultDualReferenceStreamout = 173
  CXTypeOCLIntelSubgroupAVCImeSingleReferenceStreamin = 174
  CXTypeOCLIntelSubgroupAVCImeDualReferenceStreamin = 175
    ##  Old aliases for AVC OpenCL extension types.
  CXTypeExtVector = 176
  CXTypeAtomic = 177
  CXTypeBTFTagAttributed = 178 ##  HLSL Types
  CXTypeHLSLResource = 179
  CXTypeHLSLAttributedResource = 180
  CXTypeHLSLInlineSpirv = 181

const
  CXTypeOCLIntelSubgroupAVCImeResultSingleRefStreamout =
    CXTypeOCLIntelSubgroupAVCImeResultSingleReferenceStreamout
  CXTypeOCLIntelSubgroupAVCImeResultDualRefStreamout =
    CXTypeOCLIntelSubgroupAVCImeResultDualReferenceStreamout
  CXTypeOCLIntelSubgroupAVCImeSingleRefStreamin =
    CXTypeOCLIntelSubgroupAVCImeSingleReferenceStreamin
  CXTypeOCLIntelSubgroupAVCImeDualRefStreamin =
    CXTypeOCLIntelSubgroupAVCImeDualReferenceStreamin

##
##  Describes the calling convention of a function type
##

type CXIndexOptionsCXCallingConv* {.size: sizeof(cint).} = enum
  CXCallingConvDefault = 0
  CXCallingConvC = 1
  CXCallingConvX86StdCall = 2
  CXCallingConvX86FastCall = 3
  CXCallingConvX86ThisCall = 4
  CXCallingConvX86Pascal = 5
  CXCallingConvAAPCS = 6
  CXCallingConvAAPCS_VFP = 7
  CXCallingConvX86RegCall = 8
  CXCallingConvIntelOclBicc = 9
  CXCallingConvWin64 = 10 ##  Alias for compatibility with older versions of API.
  # CXCallingConvX8664Win64 = cXCallingConvWin64
  CXCallingConvX8664SysV = 11
  CXCallingConvX86VectorCall = 12
  CXCallingConvSwift = 13
  CXCallingConvPreserveMost = 14
  CXCallingConvPreserveAll = 15
  CXCallingConvAArch64VectorCall = 16
  CXCallingConvSwiftAsync = 17
  CXCallingConvAArch64SVEPCS = 18
  CXCallingConvM68kRTD = 19
  CXCallingConvPreserveNone = 20
  CXCallingConvRISCVVectorCall = 21
  CXCallingConvRISCVVLSCall32 = 22
  CXCallingConvRISCVVLSCall64 = 23
  CXCallingConvRISCVVLSCall128 = 24
  CXCallingConvRISCVVLSCall256 = 25
  CXCallingConvRISCVVLSCall512 = 26
  CXCallingConvRISCVVLSCall1024 = 27
  CXCallingConvRISCVVLSCall2048 = 28
  CXCallingConvRISCVVLSCall4096 = 29
  CXCallingConvRISCVVLSCall8192 = 30
  CXCallingConvRISCVVLSCall16384 = 31
  CXCallingConvRISCVVLSCall32768 = 32
  CXCallingConvRISCVVLSCall65536 = 33
  CXCallingConvInvalid = 100
  CXCallingConvUnexposed = 200

##
##  The type of an element in the abstract syntax tree.
##
##

type CXIndexOptionsCXType* {.bycopy.} = object
  kind*: CXIndexOptionsCXTypeKind
  data*: array[2, pointer]

##
##  Retrieve the type of a CXCursor (if any).
##

proc getCursorType*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXType {.importc: "clang_getCursorType", dynlib: CLangLib.}

##
##  Pretty-print the underlying type using the rules of the
##  language of the translation unit from which it came.
##
##  If the type is invalid, an empty string is returned.
##

proc getTypeSpelling*(
  ct: CXIndexOptionsCXType
): CXString {.importc: "clang_getTypeSpelling", dynlib: CLangLib.}

##
##  Retrieve the underlying type of a typedef declaration.
##
##  If the cursor does not reference a typedef declaration, an invalid type is
##  returned.
##

proc getTypedefDeclUnderlyingType*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXType {.
  importc: "clang_getTypedefDeclUnderlyingType", dynlib: CLangLib
.}

##
##  Retrieve the integer type of an enum declaration.
##
##  If the cursor does not reference an enum declaration, an invalid type is
##  returned.
##

proc getEnumDeclIntegerType*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXType {.importc: "clang_getEnumDeclIntegerType", dynlib: CLangLib.}

##
##  Retrieve the integer value of an enum constant declaration as a signed
##   long long.
##
##  If the cursor does not reference an enum constant declaration, LLONG_MIN is
##  returned. Since this is also potentially a valid constant value, the kind of
##  the cursor must be verified before calling this function.
##

proc getEnumConstantDeclValue*(
  c: CXIndexOptionsCXCursor
): clonglong {.importc: "clang_getEnumConstantDeclValue", dynlib: CLangLib.}

##
##  Retrieve the integer value of an enum constant declaration as an unsigned
##   long long.
##
##  If the cursor does not reference an enum constant declaration, ULLONG_MAX is
##  returned. Since this is also potentially a valid constant value, the kind of
##  the cursor must be verified before calling this function.
##

proc getEnumConstantDeclUnsignedValue*(
  c: CXIndexOptionsCXCursor
): culonglong {.importc: "clang_getEnumConstantDeclUnsignedValue", dynlib: CLangLib.}

##
##  Returns non-zero if the cursor specifies a Record member that is a bit-field.
##

proc cursorIsBitField*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isBitField", dynlib: CLangLib.}

##
##  Retrieve the bit width of a bit-field declaration as an integer.
##
##  If the cursor does not reference a bit-field, or if the bit-field's width
##  expression cannot be evaluated, -1 is returned.
##
##  For example:
##  \code
##  if (clang_Cursor_isBitField(Cursor)) {
##    int Width = clang_getFieldDeclBitWidth(Cursor);
##    if (Width != -1) {
##      // The bit-field width is not value-dependent.
##    }
##  }
##  \endcode
##

proc getFieldDeclBitWidth*(
  c: CXIndexOptionsCXCursor
): cint {.importc: "clang_getFieldDeclBitWidth", dynlib: CLangLib.}

##
##  Retrieve the number of non-variadic arguments associated with a given
##  cursor.
##
##  The number of arguments can be determined for calls as well as for
##  declarations of functions or methods. For other cursors -1 is returned.
##

proc cursorGetNumArguments*(
  c: CXIndexOptionsCXCursor
): cint {.importc: "clang_Cursor_getNumArguments", dynlib: CLangLib.}

##
##  Retrieve the argument cursor of a function or method.
##
##  The argument cursor can be determined for calls as well as for declarations
##  of functions or methods. For other cursors and for invalid indices, an
##  invalid cursor is returned.
##

proc cursorGetArgument*(
  c: CXIndexOptionsCXCursor, i: cuint
): CXIndexOptionsCXCursor {.importc: "clang_Cursor_getArgument", dynlib: CLangLib.}

##
##  Describes the kind of a template argument.
##
##  See the definition of llvm::clang::TemplateArgument::ArgKind for full
##  element descriptions.
##

type CXIndexOptionsCXTemplateArgumentKind* {.size: sizeof(cint).} = enum
  CXTemplateArgumentKindNull
  CXTemplateArgumentKindType
  CXTemplateArgumentKindDeclaration
  CXTemplateArgumentKindNullPtr
  CXTemplateArgumentKindIntegral
  CXTemplateArgumentKindTemplate
  CXTemplateArgumentKindTemplateExpansion
  CXTemplateArgumentKindExpression
  CXTemplateArgumentKindPack
    ##  Indicates an error case, preventing the kind from being deduced.
  CXTemplateArgumentKindInvalid

##
##  Returns the number of template args of a function, struct, or class decl
##  representing a template specialization.
##
##  If the argument cursor cannot be converted into a template function
##  declaration, -1 is returned.
##
##  For example, for the following declaration and specialization:
##    template <typename T, int kInt, bool kBool>
##    void foo() { ... }
##
##    template <>
##    void foo<float, -7, true>();
##
##  The value 3 would be returned from this call.
##

proc cursorGetNumTemplateArguments*(
  c: CXIndexOptionsCXCursor
): cint {.importc: "clang_Cursor_getNumTemplateArguments", dynlib: CLangLib.}

##
##  Retrieve the kind of the I'th template argument of the CXCursor C.
##
##  If the argument CXCursor does not represent a FunctionDecl, StructDecl, or
##  ClassTemplatePartialSpecialization, an invalid template argument kind is
##  returned.
##
##  For example, for the following declaration and specialization:
##    template <typename T, int kInt, bool kBool>
##    void foo() { ... }
##
##    template <>
##    void foo<float, -7, true>();
##
##  For I = 0, 1, and 2, Type, Integral, and Integral will be returned,
##  respectively.
##

proc cursorGetTemplateArgumentKind*(
  c: CXIndexOptionsCXCursor, i: cuint
): CXIndexOptionsCXTemplateArgumentKind {.
  importc: "clang_Cursor_getTemplateArgumentKind", dynlib: CLangLib
.}

##
##  Retrieve a CXType representing the type of a TemplateArgument of a
##   function decl representing a template specialization.
##
##  If the argument CXCursor does not represent a FunctionDecl, StructDecl,
##  ClassDecl or ClassTemplatePartialSpecialization whose I'th template argument
##  has a kind of CXTemplateArgKind_Integral, an invalid type is returned.
##
##  For example, for the following declaration and specialization:
##    template <typename T, int kInt, bool kBool>
##    void foo() { ... }
##
##    template <>
##    void foo<float, -7, true>();
##
##  If called with I = 0, "float", will be returned.
##  Invalid types will be returned for I == 1 or 2.
##

proc cursorGetTemplateArgumentType*(
  c: CXIndexOptionsCXCursor, i: cuint
): CXIndexOptionsCXType {.
  importc: "clang_Cursor_getTemplateArgumentType", dynlib: CLangLib
.}

##
##  Retrieve the value of an Integral TemplateArgument (of a function
##   decl representing a template specialization) as a signed long long.
##
##  It is undefined to call this function on a CXCursor that does not represent a
##  FunctionDecl, StructDecl, ClassDecl or ClassTemplatePartialSpecialization
##  whose I'th template argument is not an integral value.
##
##  For example, for the following declaration and specialization:
##    template <typename T, int kInt, bool kBool>
##    void foo() { ... }
##
##    template <>
##    void foo<float, -7, true>();
##
##  If called with I = 1 or 2, -7 or true will be returned, respectively.
##  For I == 0, this function's behavior is undefined.
##

proc cursorGetTemplateArgumentValue*(
  c: CXIndexOptionsCXCursor, i: cuint
): clonglong {.importc: "clang_Cursor_getTemplateArgumentValue", dynlib: CLangLib.}

##
##  Retrieve the value of an Integral TemplateArgument (of a function
##   decl representing a template specialization) as an unsigned long long.
##
##  It is undefined to call this function on a CXCursor that does not represent a
##  FunctionDecl, StructDecl, ClassDecl or ClassTemplatePartialSpecialization or
##  whose I'th template argument is not an integral value.
##
##  For example, for the following declaration and specialization:
##    template <typename T, int kInt, bool kBool>
##    void foo() { ... }
##
##    template <>
##    void foo<float, 2147483649, true>();
##
##  If called with I = 1 or 2, 2147483649 or true will be returned, respectively.
##  For I == 0, this function's behavior is undefined.
##

proc cursorGetTemplateArgumentUnsignedValue*(
  c: CXIndexOptionsCXCursor, i: cuint
): culonglong {.
  importc: "clang_Cursor_getTemplateArgumentUnsignedValue", dynlib: CLangLib
.}

##
##  Determine whether two CXTypes represent the same type.
##
##  \returns non-zero if the CXTypes represent the same type and
##           zero otherwise.
##

proc equalTypes*(
  a: CXIndexOptionsCXType, b: CXIndexOptionsCXType
): cuint {.importc: "clang_equalTypes", dynlib: CLangLib.}

##
##  Return the canonical type for a CXType.
##
##  Clang's type system explicitly models typedefs and all the ways
##  a specific type can be represented.  The canonical type is the underlying
##  type with all the "sugar" removed.  For example, if 'T' is a typedef
##  for 'int', the canonical type for 'T' would be 'int'.
##

proc getCanonicalType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_getCanonicalType", dynlib: CLangLib.}

##
##  Determine whether a CXType has the "const" qualifier set,
##  without looking through typedefs that may have added "const" at a
##  different level.
##

proc isConstQualifiedType*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_isConstQualifiedType", dynlib: CLangLib.}

##
##  Determine whether a  CXCursor that is a macro, is
##  function like.
##

proc cursorIsMacroFunctionLike*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isMacroFunctionLike", dynlib: CLangLib.}

##
##  Determine whether a  CXCursor that is a macro, is a
##  builtin one.
##

proc cursorIsMacroBuiltin*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isMacroBuiltin", dynlib: CLangLib.}

##
##  Determine whether a  CXCursor that is a function declaration, is an
##  inline declaration.
##

proc cursorIsFunctionInlined*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isFunctionInlined", dynlib: CLangLib.}

##
##  Determine whether a CXType has the "volatile" qualifier set,
##  without looking through typedefs that may have added "volatile" at
##  a different level.
##

proc isVolatileQualifiedType*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_isVolatileQualifiedType", dynlib: CLangLib.}

##
##  Determine whether a CXType has the "restrict" qualifier set,
##  without looking through typedefs that may have added "restrict" at a
##  different level.
##

proc isRestrictQualifiedType*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_isRestrictQualifiedType", dynlib: CLangLib.}

##
##  Returns the address space of the given type.
##

proc getAddressSpace*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_getAddressSpace", dynlib: CLangLib.}

##
##  Returns the typedef name of the given type.
##

proc getTypedefName*(
  ct: CXIndexOptionsCXType
): CXString {.importc: "clang_getTypedefName", dynlib: CLangLib.}

##
##  For pointer types, returns the type of the pointee.
##

proc getPointeeType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_getPointeeType", dynlib: CLangLib.}

##
##  Retrieve the unqualified variant of the given type, removing as
##  little sugar as possible.
##
##  For example, given the following series of typedefs:
##
##  \code
##  typedef int Integer;
##  typedef const Integer CInteger;
##  typedef CInteger DifferenceType;
##  \endcode
##
##  Executing \c clang_getUnqualifiedType() on a \c CXType that
##  represents \c DifferenceType, will desugar to a type representing
##  \c Integer, that has no qualifiers.
##
##  And, executing \c clang_getUnqualifiedType() on the type of the
##  first argument of the following function declaration:
##
##  \code
##  void foo(const int);
##  \endcode
##
##  Will return a type representing \c int, removing the \c const
##  qualifier.
##
##  Sugar over array types is not desugared.
##
##  A type can be checked for qualifiers with \c
##  clang_isConstQualifiedType(), \c clang_isVolatileQualifiedType()
##  and \c clang_isRestrictQualifiedType().
##
##  A type that resulted from a call to \c clang_getUnqualifiedType
##  will return \c false for all of the above calls.
##

proc getUnqualifiedType*(
  ct: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_getUnqualifiedType", dynlib: CLangLib.}

##
##  For reference types (e.g., "const int&"), returns the type that the
##  reference refers to (e.g "const int").
##
##  Otherwise, returns the type itself.
##
##  A type that has kind \c CXType_LValueReference or
##  \c CXType_RValueReference is a reference type.
##

proc getNonReferenceType*(
  ct: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_getNonReferenceType", dynlib: CLangLib.}

##
##  Return the cursor for the declaration of the given type.
##

proc getTypeDeclaration*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXCursor {.importc: "clang_getTypeDeclaration", dynlib: CLangLib.}

##
##  Returns the Objective-C type encoding for the specified declaration.
##

proc getDeclObjCTypeEncoding*(
  c: CXIndexOptionsCXCursor
): CXString {.importc: "clang_getDeclObjCTypeEncoding", dynlib: CLangLib.}

##
##  Returns the Objective-C type encoding for the specified CXType.
##

proc typeGetObjCEncoding*(
  `type`: CXIndexOptionsCXType
): CXString {.importc: "clang_Type_getObjCEncoding", dynlib: CLangLib.}

##
##  Retrieve the spelling of a given CXTypeKind.
##

proc getTypeKindSpelling*(
  k: CXIndexOptionsCXTypeKind
): CXString {.importc: "clang_getTypeKindSpelling", dynlib: CLangLib.}

##
##  Retrieve the calling convention associated with a function type.
##
##  If a non-function type is passed in, CXCallingConv_Invalid is returned.
##

proc getFunctionTypeCallingConv*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXCallingConv {.
  importc: "clang_getFunctionTypeCallingConv", dynlib: CLangLib
.}

##
##  Retrieve the return type associated with a function type.
##
##  If a non-function type is passed in, an invalid type is returned.
##

proc getResultType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_getResultType", dynlib: CLangLib.}

##
##  Retrieve the exception specification type associated with a function type.
##  This is a value of type CXCursor_ExceptionSpecificationKind.
##
##  If a non-function type is passed in, an error code of -1 is returned.
##

proc getExceptionSpecificationType*(
  t: CXIndexOptionsCXType
): cint {.importc: "clang_getExceptionSpecificationType", dynlib: CLangLib.}

##
##  Retrieve the number of non-variadic parameters associated with a
##  function type.
##
##  If a non-function type is passed in, -1 is returned.
##

proc getNumArgTypes*(
  t: CXIndexOptionsCXType
): cint {.importc: "clang_getNumArgTypes", dynlib: CLangLib.}

##
##  Retrieve the type of a parameter of a function type.
##
##  If a non-function type is passed in or the function does not have enough
##  parameters, an invalid type is returned.
##

proc getArgType*(
  t: CXIndexOptionsCXType, i: cuint
): CXIndexOptionsCXType {.importc: "clang_getArgType", dynlib: CLangLib.}

##
##  Retrieves the base type of the ObjCObjectType.
##
##  If the type is not an ObjC object, an invalid type is returned.
##

proc typeGetObjCObjectBaseType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.
  importc: "clang_Type_getObjCObjectBaseType", dynlib: CLangLib
.}

##
##  Retrieve the number of protocol references associated with an ObjC object/id.
##
##  If the type is not an ObjC object, 0 is returned.
##

proc typeGetNumObjCProtocolRefs*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_Type_getNumObjCProtocolRefs", dynlib: CLangLib.}

##
##  Retrieve the decl for a protocol reference for an ObjC object/id.
##
##  If the type is not an ObjC object or there are not enough protocol
##  references, an invalid cursor is returned.
##

proc typeGetObjCProtocolDecl*(
  t: CXIndexOptionsCXType, i: cuint
): CXIndexOptionsCXCursor {.
  importc: "clang_Type_getObjCProtocolDecl", dynlib: CLangLib
.}

##
##  Retrieve the number of type arguments associated with an ObjC object.
##
##  If the type is not an ObjC object, 0 is returned.
##

proc typeGetNumObjCTypeArgs*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_Type_getNumObjCTypeArgs", dynlib: CLangLib.}

##
##  Retrieve a type argument associated with an ObjC object.
##
##  If the type is not an ObjC or the index is not valid,
##  an invalid type is returned.
##

proc typeGetObjCTypeArg*(
  t: CXIndexOptionsCXType, i: cuint
): CXIndexOptionsCXType {.importc: "clang_Type_getObjCTypeArg", dynlib: CLangLib.}

##
##  Return 1 if the CXType is a variadic function type, and 0 otherwise.
##

proc isFunctionTypeVariadic*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_isFunctionTypeVariadic", dynlib: CLangLib.}

##
##  Retrieve the return type associated with a given cursor.
##
##  This only returns a valid type if the cursor refers to a function or method.
##

proc getCursorResultType*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXType {.importc: "clang_getCursorResultType", dynlib: CLangLib.}

##
##  Retrieve the exception specification type associated with a given cursor.
##  This is a value of type CXCursor_ExceptionSpecificationKind.
##
##  This only returns a valid result if the cursor refers to a function or
##  method.
##

proc getCursorExceptionSpecificationType*(
  c: CXIndexOptionsCXCursor
): cint {.importc: "clang_getCursorExceptionSpecificationType", dynlib: CLangLib.}

##
##  Return 1 if the CXType is a POD (plain old data) type, and 0
##   otherwise.
##

proc isPODType*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_isPODType", dynlib: CLangLib.}

##
##  Return the element type of an array, complex, or vector type.
##
##  If a type is passed in that is not an array, complex, or vector type,
##  an invalid type is returned.
##

proc getElementType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_getElementType", dynlib: CLangLib.}

##
##  Return the number of elements of an array or vector type.
##
##  If a type is passed in that is not an array or vector type,
##  -1 is returned.
##

proc getNumElements*(
  t: CXIndexOptionsCXType
): clonglong {.importc: "clang_getNumElements", dynlib: CLangLib.}

##
##  Return the element type of an array type.
##
##  If a non-array type is passed in, an invalid type is returned.
##

proc getArrayElementType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_getArrayElementType", dynlib: CLangLib.}

##
##  Return the array size of a constant array.
##
##  If a non-array type is passed in, -1 is returned.
##

proc getArraySize*(
  t: CXIndexOptionsCXType
): clonglong {.importc: "clang_getArraySize", dynlib: CLangLib.}

##
##  Retrieve the type named by the qualified-id.
##
##  If a non-elaborated type is passed in, an invalid type is returned.
##

proc typeGetNamedType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_Type_getNamedType", dynlib: CLangLib.}

##
##  Determine if a typedef is 'transparent' tag.
##
##  A typedef is considered 'transparent' if it shares a name and spelling
##  location with its underlying tag type, as is the case with the NS_ENUM macro.
##
##  \returns non-zero if transparent and zero otherwise.
##

proc typeIsTransparentTagTypedef*(
  t: CXIndexOptionsCXType
): cuint {.importc: "clang_Type_isTransparentTagTypedef", dynlib: CLangLib.}

type CXIndexOptionsCXTypeNullabilityKind* {.size: sizeof(cint).} = enum
  ##
  ##  Values of this type can never be null.
  ##
  CXTypeNullabilityNonNull = 0
    ##
    ##  Values of this type can be null.
    ##
  CXTypeNullabilityNullable = 1
    ##
    ##  Whether values of this type can be null is (explicitly)
    ##  unspecified. This captures a (fairly rare) case where we
    ##  can't conclude anything about the nullability of the type even
    ##  though it has been considered.
    ##
  CXTypeNullabilityUnspecified = 2
    ##
    ##  Nullability is not applicable to this type.
    ##
  CXTypeNullabilityInvalid = 3
    ##
    ##  Generally behaves like Nullable, except when used in a block parameter that
    ##  was imported into a swift async method. There, swift will assume that the
    ##  parameter can get null even if no error occurred. _Nullable parameters are
    ##  assumed to only get null on error.
    ##
  CXTypeNullabilityNullableResult = 4

##
##  Retrieve the nullability kind of a pointer type.
##

proc typeGetNullability*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXTypeNullabilityKind {.
  importc: "clang_Type_getNullability", dynlib: CLangLib
.}

##
##  List the possible error codes for \c clang_Type_getSizeOf,
##    \c clang_Type_getAlignOf, \c clang_Type_getOffsetOf,
##    \c clang_Cursor_getOffsetOf, and \c clang_getOffsetOfBase.
##
##  A value of this enumeration type can be returned if the target type is not
##  a valid argument to sizeof, alignof or offsetof.
##

type CXIndexOptionsCXTypeLayoutError* {.size: sizeof(cint).} = enum
  ##
  ##  Type is of kind CXType_Invalid.
  ##
  CXTypeLayoutErrorUndeduced = -6
  CXTypeLayoutErrorInvalidFieldName = -5
    ##
    ##  The type is undeduced.
    ##
  CXTypeLayoutErrorNotConstantSize = -4
    ##
    ##  The Field name is not valid for this record.
    ##
  CXTypeLayoutErrorDependent = -3
    ##
    ##  The type is not a constant size type.
    ##
  CXTypeLayoutErrorIncomplete = -2
    ##
    ##  The type is a dependent Type.
    ##
  CXTypeLayoutErrorInvalid = -1
    ##
    ##  The type is an incomplete Type.
    ##

##
##  Return the alignment of a type in bytes as per C++[expr.alignof]
##    standard.
##
##  If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
##  If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
##    is returned.
##  If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
##    returned.
##  If the type declaration is not a constant size type,
##    CXTypeLayoutError_NotConstantSize is returned.
##

proc typeGetAlignOf*(
  t: CXIndexOptionsCXType
): clonglong {.importc: "clang_Type_getAlignOf", dynlib: CLangLib.}

##
##  Return the class type of an member pointer type.
##
##  If a non-member-pointer type is passed in, an invalid type is returned.
##

proc typeGetClassType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_Type_getClassType", dynlib: CLangLib.}

##
##  Return the size of a type in bytes as per C++[expr.sizeof] standard.
##
##  If the type declaration is invalid, CXTypeLayoutError_Invalid is returned.
##  If the type declaration is an incomplete type, CXTypeLayoutError_Incomplete
##    is returned.
##  If the type declaration is a dependent type, CXTypeLayoutError_Dependent is
##    returned.
##

proc typeGetSizeOf*(
  t: CXIndexOptionsCXType
): clonglong {.importc: "clang_Type_getSizeOf", dynlib: CLangLib.}

##
##  Return the offset of a field named S in a record of type T in bits
##    as it would be returned by __offsetof__ as per C++11[18.2p4]
##
##  If the cursor is not a record field declaration, CXTypeLayoutError_Invalid
##    is returned.
##  If the field's type declaration is an incomplete type,
##    CXTypeLayoutError_Incomplete is returned.
##  If the field's type declaration is a dependent type,
##    CXTypeLayoutError_Dependent is returned.
##  If the field's name S is not found,
##    CXTypeLayoutError_InvalidFieldName is returned.
##

proc typeGetOffsetOf*(
  t: CXIndexOptionsCXType, s: cstring
): clonglong {.importc: "clang_Type_getOffsetOf", dynlib: CLangLib.}

##
##  Return the type that was modified by this attributed type.
##
##  If the type is not an attributed type, an invalid type is returned.
##

proc typeGetModifiedType*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_Type_getModifiedType", dynlib: CLangLib.}

##
##  Gets the type contained by this atomic type.
##
##  If a non-atomic type is passed in, an invalid type is returned.
##

proc typeGetValueType*(
  ct: CXIndexOptionsCXType
): CXIndexOptionsCXType {.importc: "clang_Type_getValueType", dynlib: CLangLib.}

##
##  Return the offset of the field represented by the Cursor.
##
##  If the cursor is not a field declaration, -1 is returned.
##  If the cursor semantic parent is not a record field declaration,
##    CXTypeLayoutError_Invalid is returned.
##  If the field's type declaration is an incomplete type,
##    CXTypeLayoutError_Incomplete is returned.
##  If the field's type declaration is a dependent type,
##    CXTypeLayoutError_Dependent is returned.
##  If the field's name S is not found,
##    CXTypeLayoutError_InvalidFieldName is returned.
##

proc cursorGetOffsetOfField*(
  c: CXIndexOptionsCXCursor
): clonglong {.importc: "clang_Cursor_getOffsetOfField", dynlib: CLangLib.}

##
##  Determine whether the given cursor represents an anonymous
##  tag or namespace
##

proc cursorIsAnonymous*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isAnonymous", dynlib: CLangLib.}

##
##  Determine whether the given cursor represents an anonymous record
##  declaration.
##

proc cursorIsAnonymousRecordDecl*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isAnonymousRecordDecl", dynlib: CLangLib.}

##
##  Determine whether the given cursor represents an inline namespace
##  declaration.
##

proc cursorIsInlineNamespace*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isInlineNamespace", dynlib: CLangLib.}

type CXIndexOptionsCXRefQualifierKind* {.size: sizeof(cint).} = enum
  ##  No ref-qualifier was provided.
  CXRefQualifierNone = 0 ##  An lvalue ref-qualifier was provided (\c &).
  CXRefQualifierLValue ##  An rvalue ref-qualifier was provided (\c &&).
  CXRefQualifierRValue

##
##  Returns the number of template arguments for given template
##  specialization, or -1 if type \c T is not a template specialization.
##

proc typeGetNumTemplateArguments*(
  t: CXIndexOptionsCXType
): cint {.importc: "clang_Type_getNumTemplateArguments", dynlib: CLangLib.}

##
##  Returns the type template argument of a template class specialization
##  at given index.
##
##  This function only returns template type arguments and does not handle
##  template template arguments or variadic packs.
##

proc typeGetTemplateArgumentAsType*(
  t: CXIndexOptionsCXType, i: cuint
): CXIndexOptionsCXType {.
  importc: "clang_Type_getTemplateArgumentAsType", dynlib: CLangLib
.}

##
##  Retrieve the ref-qualifier kind of a function or method.
##
##  The ref-qualifier is returned for C++ functions or methods. For other types
##  or non-C++ declarations, CXRefQualifier_None is returned.
##

proc typeGetCXXRefQualifier*(
  t: CXIndexOptionsCXType
): CXIndexOptionsCXRefQualifierKind {.
  importc: "clang_Type_getCXXRefQualifier", dynlib: CLangLib
.}

##
##  Returns 1 if the base class specified by the cursor with kind
##    CX_CXXBaseSpecifier is virtual.
##

proc isVirtualBase*(
  a1: CXIndexOptionsCXCursor
): cuint {.importc: "clang_isVirtualBase", dynlib: CLangLib.}

##
##  Returns the offset in bits of a CX_CXXBaseSpecifier relative to the parent
##  class.
##
##  Returns a small negative number if the offset cannot be computed. See
##  CXTypeLayoutError for error codes.
##

proc getOffsetOfBase*(
  parent: CXIndexOptionsCXCursor, base: CXIndexOptionsCXCursor
): clonglong {.importc: "clang_getOffsetOfBase", dynlib: CLangLib.}

##
##  Represents the C++ access control level to a base class for a
##  cursor with kind CX_CXXBaseSpecifier.
##

type CXIndexOptionsCX_CXXAccessSpecifier* {.size: sizeof(cint).} = enum
  CX_CXXInvalidAccessSpecifier
  CX_CXXPublic
  CX_CXXProtected
  CX_CXXPrivate

##
##  Returns the access control level for the referenced object.
##
##  If the cursor refers to a C++ declaration, its access control level within
##  its parent scope is returned. Otherwise, if the cursor refers to a base
##  specifier or access specifier, the specifier itself is returned.
##

proc getCXXAccessSpecifier*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCX_CXXAccessSpecifier {.
  importc: "clang_getCXXAccessSpecifier", dynlib: CLangLib
.}

##
##  Represents the storage classes as declared in the source. CX_SC_Invalid
##  was added for the case that the passed cursor in not a declaration.
##

type CXIndexOptionsCX_StorageClass* {.size: sizeof(cint).} = enum
  CX_SC_Invalid
  CX_SC_None
  CX_SC_Extern
  CX_SC_Static
  CX_SC_PrivateExtern
  CX_SC_OpenCLWorkGroupLocal
  CX_SC_Auto
  CX_SC_Register

##
##  Represents a specific kind of binary operator which can appear at a cursor.
##

type CXIndexOptionsCX_BinaryOperatorKindX* {.size: sizeof(cint).} = enum
  CX_BO_Invalid = 0
  CX_BO_PtrMemD = 1
  CX_BO_PtrMemI = 2
  CX_BO_Mul = 3
  CX_BO_Div = 4
  CX_BO_Rem = 5
  CX_BO_Add = 6
  CX_BO_Sub = 7
  CX_BO_Shl = 8
  CX_BO_Shr = 9
  CX_BO_Cmp = 10
  CX_BO_LT = 11
  CX_BO_GT = 12
  CX_BO_LE = 13
  CX_BO_GE = 14
  CX_BO_EQ = 15
  CX_BO_NE = 16
  CX_BO_And = 17
  CX_BO_Xor = 18
  CX_BO_Or = 19
  CX_BO_LAnd = 20
  CX_BO_LOr = 21
  CX_BO_Assign = 22
  CX_BO_MulAssign = 23
  CX_BO_DivAssign = 24
  CX_BO_RemAssign = 25
  CX_BO_AddAssign = 26
  CX_BO_SubAssign = 27
  CX_BO_ShlAssign = 28
  CX_BO_ShrAssign = 29
  CX_BO_AndAssign = 30
  CX_BO_XorAssign = 31
  CX_BO_OrAssign = 32
  CX_BO_Comma = 33 # CX_BO_LAST = cX_BO_Comma

##
##  \brief Returns the operator code for the binary operator.
##
##  @deprecated: use clang_getCursorBinaryOperatorKind instead.
##

proc cursorGetBinaryOpcode*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCX_BinaryOperatorKindX {.
  importc: "clang_Cursor_getBinaryOpcode", dynlib: CLangLib
.}

##
##  \brief Returns a string containing the spelling of the binary operator.
##
##  @deprecated: use clang_getBinaryOperatorKindSpelling instead
##

proc cursorGetBinaryOpcodeStr*(
  op: CXIndexOptionsCX_BinaryOperatorKindX
): CXString {.importc: "clang_Cursor_getBinaryOpcodeStr", dynlib: CLangLib.}

##
##  Returns the storage class for a function or variable declaration.
##
##  If the passed in Cursor is not a function or variable declaration,
##  CX_SC_Invalid is returned else the storage class.
##

proc cursorGetStorageClass*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCX_StorageClass {.
  importc: "clang_Cursor_getStorageClass", dynlib: CLangLib
.}

##
##  Determine the number of overloaded declarations referenced by a
##  \c CXCursor_OverloadedDeclRef cursor.
##
##  \param cursor The cursor whose overloaded declarations are being queried.
##
##  \returns The number of overloaded declarations referenced by \c cursor. If it
##  is not a \c CXCursor_OverloadedDeclRef cursor, returns 0.
##

proc getNumOverloadedDecls*(
  cursor: CXIndexOptionsCXCursor
): cuint {.importc: "clang_getNumOverloadedDecls", dynlib: CLangLib.}

##
##  Retrieve a cursor for one of the overloaded declarations referenced
##  by a \c CXCursor_OverloadedDeclRef cursor.
##
##  \param cursor The cursor whose overloaded declarations are being queried.
##
##  \param index The zero-based index into the set of overloaded declarations in
##  the cursor.
##
##  \returns A cursor representing the declaration referenced by the given
##  \c cursor at the specified \c index. If the cursor does not have an
##  associated set of overloaded declarations, or if the index is out of bounds,
##  returns \c clang_getNullCursor();
##

proc getOverloadedDecl*(
  cursor: CXIndexOptionsCXCursor, index: cuint
): CXIndexOptionsCXCursor {.importc: "clang_getOverloadedDecl", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_ATTRIBUTES Information for attributes
##
##  @{
##
##
##  For cursors representing an iboutletcollection attribute,
##   this function returns the collection element type.
##
##

proc getIBOutletCollectionType*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCXType {.importc: "clang_getIBOutletCollectionType", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_CURSOR_TRAVERSAL Traversing the AST with cursors
##
##  These routines provide the ability to traverse the abstract syntax tree
##  using cursors.
##
##  @{
##
##
##  Describes how the traversal of the children of a particular
##  cursor should proceed after visiting a particular child cursor.
##
##  A value of this enumeration type should be returned by each
##  \c CXCursorVisitor to indicate how clang_visitChildren() proceed.
##

type CXIndexOptionsCXChildVisitResult* {.size: sizeof(cint).} = enum
  ##
  ##  Terminates the cursor traversal.
  ##
  CXChildVisitBreak
    ##
    ##  Continues the cursor traversal with the next sibling of
    ##  the cursor just visited, without visiting its children.
    ##
  CXChildVisitContinue
    ##
    ##  Recursively traverse the children of this cursor, using
    ##  the same visitor and client data.
    ##
  CXChildVisitRecurse

##
##  Visitor invoked for each cursor found by a traversal.
##
##  This visitor function will be invoked for each cursor found by
##  clang_visitCursorChildren(). Its first argument is the cursor being
##  visited, its second argument is the parent visitor for that cursor,
##  and its third argument is the client data provided to
##  clang_visitCursorChildren().
##
##  The visitor should return one of the \c CXChildVisitResult values
##  to direct clang_visitCursorChildren().
##

type CXIndexOptionsCXCursorVisitor* = proc(
  cursor: CXIndexOptionsCXCursor,
  parent: CXIndexOptionsCXCursor,
  clientData: CXClientData,
): CXIndexOptionsCXChildVisitResult

##
##  Visit the children of a particular cursor.
##
##  This function visits all the direct children of the given cursor,
##  invoking the given \p visitor function with the cursors of each
##  visited child. The traversal may be recursive, if the visitor returns
##  \c CXChildVisit_Recurse. The traversal may also be ended prematurely, if
##  the visitor returns \c CXChildVisit_Break.
##
##  \param parent the cursor whose child may be visited. All kinds of
##  cursors can be visited, including invalid cursors (which, by
##  definition, have no children).
##
##  \param visitor the visitor function that will be invoked for each
##  child of \p parent.
##
##  \param client_data pointer data supplied by the client, which will
##  be passed to the visitor each time it is invoked.
##
##  \returns a non-zero value if the traversal was terminated
##  prematurely by the visitor returning \c CXChildVisit_Break.
##

proc visitChildren*(
  parent: CXIndexOptionsCXCursor,
  visitor: CXIndexOptionsCXCursorVisitor,
  clientData: CXClientData,
): cuint {.importc: "clang_visitChildren", dynlib: CLangLib.}

##
##  Visitor invoked for each cursor found by a traversal.
##
##  This visitor block will be invoked for each cursor found by
##  clang_visitChildrenWithBlock(). Its first argument is the cursor being
##  visited, its second argument is the parent visitor for that cursor.
##
##  The visitor should return one of the \c CXChildVisitResult values
##  to direct clang_visitChildrenWithBlock().
##

# when hasFeature(blocks):
#   type CXIndexOptionsCXCursorVisitorBlock* = proc(
#     cursor: CXIndexOptionsCXCursor, parent: CXIndexOptionsCXCursor
#   ): CXIndexOptionsCXChildVisitResult {.cblock.}

# else:
#   type CXIndexOptionsCXCursorVisitorBlock* = ptr cXChildVisitResult
##
##  Visits the children of a cursor using the specified block.  Behaves
##  identically to clang_visitChildren() in all other respects.
##

# proc visitChildrenWithBlock*(
#   parent: CXIndexOptionsCXCursor, `block`: CXIndexOptionsCXCursorVisitorBlock
# ): cuint {.importc: "clang_visitChildrenWithBlock", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_CURSOR_XREF Cross-referencing in the AST
##
##  These routines provide the ability to determine references within and
##  across translation units, by providing the names of the entities referenced
##  by cursors, follow reference cursors to the declarations they reference,
##  and associate declarations with their definitions.
##
##  @{
##
##
##  Retrieve a Unified Symbol Resolution (USR) for the entity referenced
##  by the given cursor.
##
##  A Unified Symbol Resolution (USR) is a string that identifies a particular
##  entity (function, class, variable, etc.) within a program. USRs can be
##  compared across translation units to determine, e.g., when references in
##  one translation refer to an entity defined in another translation unit.
##

proc getCursorUSR*(
  a1: CXIndexOptionsCXCursor
): CXString {.importc: "clang_getCursorUSR", dynlib: CLangLib.}

##
##  Construct a USR for a specified Objective-C class.
##

proc constructUSR_ObjCClass*(
  className: cstring
): CXString {.importc: "clang_constructUSR_ObjCClass", dynlib: CLangLib.}

##
##  Construct a USR for a specified Objective-C category.
##

proc constructUSR_ObjCCategory*(
  className: cstring, categoryName: cstring
): CXString {.importc: "clang_constructUSR_ObjCCategory", dynlib: CLangLib.}

##
##  Construct a USR for a specified Objective-C protocol.
##

proc constructUSR_ObjCProtocol*(
  protocolName: cstring
): CXString {.importc: "clang_constructUSR_ObjCProtocol", dynlib: CLangLib.}

##
##  Construct a USR for a specified Objective-C instance variable and
##    the USR for its containing class.
##

proc constructUSR_ObjCIvar*(
  name: cstring, classUSR: CXString
): CXString {.importc: "clang_constructUSR_ObjCIvar", dynlib: CLangLib.}

##
##  Construct a USR for a specified Objective-C method and
##    the USR for its containing class.
##

proc constructUSR_ObjCMethod*(
  name: cstring, isInstanceMethod: cuint, classUSR: CXString
): CXString {.importc: "clang_constructUSR_ObjCMethod", dynlib: CLangLib.}

##
##  Construct a USR for a specified Objective-C property and the USR
##   for its containing class.
##

proc constructUSR_ObjCProperty*(
  property: cstring, classUSR: CXString
): CXString {.importc: "clang_constructUSR_ObjCProperty", dynlib: CLangLib.}

##
##  Retrieve a name for the entity referenced by this cursor.
##

proc getCursorSpelling*(
  a1: CXIndexOptionsCXCursor
): CXString {.importc: "clang_getCursorSpelling", dynlib: CLangLib.}

##
##  Retrieve a range for a piece that forms the cursors spelling name.
##  Most of the times there is only one range for the complete spelling but for
##  Objective-C methods and Objective-C message expressions, there are multiple
##  pieces for each selector identifier.
##
##  \param pieceIndex the index of the spelling name piece. If this is greater
##  than the actual number of pieces, it will return a NULL (invalid) range.
##
##  \param options Reserved.
##

proc cursorGetSpellingNameRange*(
  a1: CXIndexOptionsCXCursor, pieceIndex: cuint, options: cuint
): CXSourceRange {.importc: "clang_Cursor_getSpellingNameRange", dynlib: CLangLib.}

##
##  Opaque pointer representing a policy that controls pretty printing
##  for \c clang_getCursorPrettyPrinted.
##

type CXIndexOptionsCXPrintingPolicy* = pointer

##
##  Properties for the printing policy.
##
##  See \c clang::PrintingPolicy for more information.
##

type CXIndexOptionsCXPrintingPolicyProperty* {.size: sizeof(cint).} = enum
  CXPrintingPolicyIndentation
  CXPrintingPolicySuppressSpecifiers
  CXPrintingPolicySuppressTagKeyword
  CXPrintingPolicyIncludeTagDefinition
  CXPrintingPolicySuppressScope
  CXPrintingPolicySuppressUnwrittenScope
  CXPrintingPolicySuppressInitializers
  CXPrintingPolicyConstantArraySizeAsWritten
  CXPrintingPolicyAnonymousTagLocations
  CXPrintingPolicySuppressStrongLifetime
  CXPrintingPolicySuppressLifetimeQualifiers
  CXPrintingPolicySuppressTemplateArgsInCXXConstructors
  CXPrintingPolicyBool
  CXPrintingPolicyRestrict
  CXPrintingPolicyAlignof
  CXPrintingPolicyUnderscoreAlignof
  CXPrintingPolicyUseVoidForZeroParams
  CXPrintingPolicyTerseOutput
  CXPrintingPolicyPolishForDeclaration
  CXPrintingPolicyHalf
  CXPrintingPolicyMSWChar
  CXPrintingPolicyIncludeNewlines
  CXPrintingPolicyMSVCFormatting
  CXPrintingPolicyConstantsAsWritten
  CXPrintingPolicySuppressImplicitBase
  CXPrintingPolicyFullyQualifiedName
  # CXPrintingPolicyLastProperty = cXPrintingPolicyFullyQualifiedName

##
##  Get a property value for the given printing policy.
##

proc printingPolicyGetProperty*(
  policy: CXIndexOptionsCXPrintingPolicy,
  property: CXIndexOptionsCXPrintingPolicyProperty,
): cuint {.importc: "clang_PrintingPolicy_getProperty", dynlib: CLangLib.}

##
##  Set a property value for the given printing policy.
##

proc printingPolicySetProperty*(
  policy: CXIndexOptionsCXPrintingPolicy,
  property: CXIndexOptionsCXPrintingPolicyProperty,
  value: cuint,
) {.importc: "clang_PrintingPolicy_setProperty", dynlib: CLangLib.}

##
##  Retrieve the default policy for the cursor.
##
##  The policy should be released after use with \c
##  clang_PrintingPolicy_dispose.
##

proc getCursorPrintingPolicy*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCXPrintingPolicy {.
  importc: "clang_getCursorPrintingPolicy", dynlib: CLangLib
.}

##
##  Release a printing policy.
##

proc printingPolicyDispose*(
  policy: CXIndexOptionsCXPrintingPolicy
) {.importc: "clang_PrintingPolicy_dispose", dynlib: CLangLib.}

##
##  Pretty print declarations.
##
##  \param Cursor The cursor representing a declaration.
##
##  \param Policy The policy to control the entities being printed. If
##  NULL, a default policy is used.
##
##  \returns The pretty printed declaration or the empty string for
##  other cursors.
##

proc getCursorPrettyPrinted*(
  cursor: CXIndexOptionsCXCursor, policy: CXIndexOptionsCXPrintingPolicy
): CXString {.importc: "clang_getCursorPrettyPrinted", dynlib: CLangLib.}

##
##  Pretty-print the underlying type using a custom printing policy.
##
##  If the type is invalid, an empty string is returned.
##

proc getTypePrettyPrinted*(
  ct: CXIndexOptionsCXType, cxPolicy: CXIndexOptionsCXPrintingPolicy
): CXString {.importc: "clang_getTypePrettyPrinted", dynlib: CLangLib.}

##
##  Get the fully qualified name for a type.
##
##  This includes full qualification of all template parameters.
##
##  Policy - Further refine the type formatting
##  WithGlobalNsPrefix - If non-zero, function will prepend a '::' to qualified
##  names
##

proc getFullyQualifiedName*(
  ct: CXIndexOptionsCXType,
  policy: CXIndexOptionsCXPrintingPolicy,
  withGlobalNsPrefix: cuint,
): CXString {.importc: "clang_getFullyQualifiedName", dynlib: CLangLib.}

##
##  Retrieve the display name for the entity referenced by this cursor.
##
##  The display name contains extra information that helps identify the cursor,
##  such as the parameters of a function or template or the arguments of a
##  class template specialization.
##

proc getCursorDisplayName*(
  a1: CXIndexOptionsCXCursor
): CXString {.importc: "clang_getCursorDisplayName", dynlib: CLangLib.}

##  For a cursor that is a reference, retrieve a cursor representing the
##  entity that it references.
##
##  Reference cursors refer to other entities in the AST. For example, an
##  Objective-C superclass reference cursor refers to an Objective-C class.
##  This function produces the cursor for the Objective-C class from the
##  cursor for the superclass reference. If the input cursor is a declaration or
##  definition, it returns that declaration or definition unchanged.
##  Otherwise, returns the NULL cursor.
##

proc getCursorReferenced*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursor {.importc: "clang_getCursorReferenced", dynlib: CLangLib.}

##
##   For a cursor that is either a reference to or a declaration
##   of some entity, retrieve a cursor that describes the definition of
##   that entity.
##
##   Some entities can be declared multiple times within a translation
##   unit, but only one of those declarations can also be a
##   definition. For example, given:
##
##   \code
##   int f(int, int);
##   int g(int x, int y) { return f(x, y); }
##   int f(int a, int b) { return a + b; }
##   int f(int, int);
##   \endcode
##
##   there are three declarations of the function "f", but only the
##   second one is a definition. The clang_getCursorDefinition()
##   function will take any cursor pointing to a declaration of "f"
##   (the first or fourth lines of the example) or a cursor referenced
##   that uses "f" (the call to "f' inside "g") and will return a
##   declaration cursor pointing to the definition (the second "f"
##   declaration).
##
##   If given a cursor for which there is no corresponding definition,
##   e.g., because there is no definition of that entity within this
##   translation unit, returns a NULL cursor.
##

proc getCursorDefinition*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursor {.importc: "clang_getCursorDefinition", dynlib: CLangLib.}

##
##  Determine whether the declaration pointed to by this cursor
##  is also a definition of that entity.
##

proc isCursorDefinition*(
  a1: CXIndexOptionsCXCursor
): cuint {.importc: "clang_isCursorDefinition", dynlib: CLangLib.}

##
##  Retrieve the canonical cursor corresponding to the given cursor.
##
##  In the C family of languages, many kinds of entities can be declared several
##  times within a single translation unit. For example, a structure type can
##  be forward-declared (possibly multiple times) and later defined:
##
##  \code
##  struct X;
##  struct X;
##  struct X {
##    int member;
##  };
##  \endcode
##
##  The declarations and the definition of \c X are represented by three
##  different cursors, all of which are declarations of the same underlying
##  entity. One of these cursor is considered the "canonical" cursor, which
##  is effectively the representative for the underlying entity. One can
##  determine if two cursors are declarations of the same underlying entity by
##  comparing their canonical cursors.
##
##  \returns The canonical cursor for the entity referred to by the given cursor.
##

proc getCanonicalCursor*(
  a1: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursor {.importc: "clang_getCanonicalCursor", dynlib: CLangLib.}

##
##  If the cursor points to a selector identifier in an Objective-C
##  method or message expression, this returns the selector index.
##
##  After getting a cursor with #clang_getCursor, this can be called to
##  determine if the location points to a selector identifier.
##
##  \returns The selector index if the cursor is an Objective-C method or message
##  expression and the cursor is pointing to a selector identifier, or -1
##  otherwise.
##

proc cursorGetObjCSelectorIndex*(
  a1: CXIndexOptionsCXCursor
): cint {.importc: "clang_Cursor_getObjCSelectorIndex", dynlib: CLangLib.}

##
##  Given a cursor pointing to a C++ method call or an Objective-C
##  message, returns non-zero if the method/message is "dynamic", meaning:
##
##  For a C++ method: the call is virtual.
##  For an Objective-C message: the receiver is an object instance, not 'super'
##  or a specific class.
##
##  If the method/message is "static" or the cursor does not point to a
##  method/message, it will return zero.
##

proc cursorIsDynamicCall*(
  c: CXIndexOptionsCXCursor
): cint {.importc: "clang_Cursor_isDynamicCall", dynlib: CLangLib.}

##
##  Given a cursor pointing to an Objective-C message or property
##  reference, or C++ method call, returns the CXType of the receiver.
##

proc cursorGetReceiverType*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXType {.importc: "clang_Cursor_getReceiverType", dynlib: CLangLib.}

##
##  Property attributes for a \c CXCursor_ObjCPropertyDecl.
##

type CXIndexOptionsCXObjCPropertyAttrKind* {.size: sizeof(cint).} = enum
  CXObjCPropertyAttrNoattr = 0x00
  CXObjCPropertyAttrReadonly = 0x01
  CXObjCPropertyAttrGetter = 0x02
  CXObjCPropertyAttrAssign = 0x04
  CXObjCPropertyAttrReadwrite = 0x08
  CXObjCPropertyAttrRetain = 0x10
  CXObjCPropertyAttrCopy = 0x20
  CXObjCPropertyAttrNonatomic = 0x40
  CXObjCPropertyAttrSetter = 0x80
  CXObjCPropertyAttrAtomic = 0x100
  CXObjCPropertyAttrWeak = 0x200
  CXObjCPropertyAttrStrong = 0x400
  CXObjCPropertyAttrUnsafeUnretained = 0x800
  CXObjCPropertyAttrClass = 0x1000

##
##  Given a cursor that represents a property declaration, return the
##  associated property attributes. The bits are formed from
##  \c CXObjCPropertyAttrKind.
##
##  \param reserved Reserved for future use, pass 0.
##

proc cursorGetObjCPropertyAttributes*(
  c: CXIndexOptionsCXCursor, reserved: cuint
): cuint {.importc: "clang_Cursor_getObjCPropertyAttributes", dynlib: CLangLib.}

##
##  Given a cursor that represents a property declaration, return the
##  name of the method that implements the getter.
##

proc cursorGetObjCPropertyGetterName*(
  c: CXIndexOptionsCXCursor
): CXString {.importc: "clang_Cursor_getObjCPropertyGetterName", dynlib: CLangLib.}

##
##  Given a cursor that represents a property declaration, return the
##  name of the method that implements the setter, if any.
##

proc cursorGetObjCPropertySetterName*(
  c: CXIndexOptionsCXCursor
): CXString {.importc: "clang_Cursor_getObjCPropertySetterName", dynlib: CLangLib.}

##
##  'Qualifiers' written next to the return and parameter types in
##  Objective-C method declarations.
##

type CXIndexOptionsCXObjCDeclQualifierKind* {.size: sizeof(cint).} = enum
  CXObjCDeclQualifierNone = 0x0
  CXObjCDeclQualifierIn = 0x1
  CXObjCDeclQualifierInout = 0x2
  CXObjCDeclQualifierOut = 0x4
  CXObjCDeclQualifierBycopy = 0x8
  CXObjCDeclQualifierByref = 0x10
  CXObjCDeclQualifierOneway = 0x20

##
##  Given a cursor that represents an Objective-C method or parameter
##  declaration, return the associated Objective-C qualifiers for the return
##  type or the parameter respectively. The bits are formed from
##  CXObjCDeclQualifierKind.
##

proc cursorGetObjCDeclQualifiers*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_getObjCDeclQualifiers", dynlib: CLangLib.}

##
##  Given a cursor that represents an Objective-C method or property
##  declaration, return non-zero if the declaration was affected by "\@optional".
##  Returns zero if the cursor is not such a declaration or it is "\@required".
##

proc cursorIsObjCOptional*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isObjCOptional", dynlib: CLangLib.}

##
##  Returns non-zero if the given cursor is a variadic function or method.
##

proc cursorIsVariadic*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isVariadic", dynlib: CLangLib.}

##
##  Returns non-zero if the given cursor points to a symbol marked with
##  external_source_symbol attribute.
##
##  \param language If non-NULL, and the attribute is present, will be set to
##  the 'language' string from the attribute.
##
##  \param definedIn If non-NULL, and the attribute is present, will be set to
##  the 'definedIn' string from the attribute.
##
##  \param isGenerated If non-NULL, and the attribute is present, will be set to
##  non-zero if the 'generated_declaration' is set in the attribute.
##

proc cursorIsExternalSymbol*(
  c: CXIndexOptionsCXCursor,
  language: ptr CXString,
  definedIn: ptr CXString,
  isGenerated: ptr cuint,
): cuint {.importc: "clang_Cursor_isExternalSymbol", dynlib: CLangLib.}

##
##  Given a cursor that represents a declaration, return the associated
##  comment's source range.  The range may include multiple consecutive comments
##  with whitespace in between.
##

proc cursorGetCommentRange*(
  c: CXIndexOptionsCXCursor
): CXSourceRange {.importc: "clang_Cursor_getCommentRange", dynlib: CLangLib.}

##
##  Given a cursor that represents a declaration, return the associated
##  comment text, including comment markers.
##

proc cursorGetRawCommentText*(
  c: CXIndexOptionsCXCursor
): CXString {.importc: "clang_Cursor_getRawCommentText", dynlib: CLangLib.}

##
##  Given a cursor that represents a documentable entity (e.g.,
##  declaration), return the associated \paragraph; otherwise return the
##  first paragraph.
##

proc cursorGetBriefCommentText*(
  c: CXIndexOptionsCXCursor
): CXString {.importc: "clang_Cursor_getBriefCommentText", dynlib: CLangLib.}

##
##  @}
##
##  \defgroup CINDEX_MANGLE Name Mangling API Functions
##
##  @{
##
##
##  Retrieve the CXString representing the mangled name of the cursor.
##

proc cursorGetMangling*(
  a1: CXIndexOptionsCXCursor
): CXString {.importc: "clang_Cursor_getMangling", dynlib: CLangLib.}

##
##  Retrieve the CXStrings representing the mangled symbols of the C++
##  constructor or destructor at the cursor.
##

proc cursorGetCXXManglings*(
  a1: CXIndexOptionsCXCursor
): ptr CXStringSet {.importc: "clang_Cursor_getCXXManglings", dynlib: CLangLib.}

##
##  Retrieve the CXStrings representing the mangled symbols of the ObjC
##  class interface or implementation at the cursor.
##

proc cursorGetObjCManglings*(
  a1: CXIndexOptionsCXCursor
): ptr CXStringSet {.importc: "clang_Cursor_getObjCManglings", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_MODULE Inline Assembly introspection
##
##  The functions in this group provide access to information about GCC-style
##  inline assembly statements.
##
##  @{
##
##
##  Given a CXCursor_GCCAsmStmt cursor, return the assembly template string.
##  As per LLVM IR Assembly Template language, template placeholders for
##  inputs and outputs are either of the form $N where N is a decimal number
##  as an index into the input-output specification,
##  or ${N:M} where N is a decimal number also as an index into the
##  input-output specification and M is the template argument modifier.
##  The index N in both cases points into the the total inputs and outputs,
##  or more specifically, into the list of outputs followed by the inputs,
##  starting from index 0 as the first available template argument.
##
##  This function also returns a valid empty string if the cursor does not point
##  at a GCC inline assembly block.
##
##  Users are responsible for releasing the allocation of returned string via
##  \c clang_disposeString.
##

proc cursorGetGCCAssemblyTemplate*(
  a1: CXIndexOptionsCXCursor
): CXString {.importc: "clang_Cursor_getGCCAssemblyTemplate", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, check if the assembly block has goto
##  labels.
##  This function also returns 0 if the cursor does not point at a GCC inline
##  assembly block.
##

proc cursorIsGCCAssemblyHasGoto*(
  a1: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isGCCAssemblyHasGoto", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, count the number of outputs.
##  This function also returns 0 if the cursor does not point at a GCC inline
##  assembly block.
##

proc cursorGetGCCAssemblyNumOutputs*(
  a1: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_getGCCAssemblyNumOutputs", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, count the number of inputs.
##  This function also returns 0 if the cursor does not point at a GCC inline
##  assembly block.
##

proc cursorGetGCCAssemblyNumInputs*(
  a1: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_getGCCAssemblyNumInputs", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, get the constraint and expression cursor
##  to the Index-th input.
##  This function returns 1 when the cursor points at a GCC inline assembly
##  statement, `Index` is within bounds and both the `Constraint` and `Expr` are
##  not NULL.
##  Otherwise, this function returns 0 but leaves `Constraint` and `Expr`
##  intact.
##
##  Users are responsible for releasing the allocation of `Constraint` via
##  \c clang_disposeString.
##

proc cursorGetGCCAssemblyInput*(
  cursor: CXIndexOptionsCXCursor,
  index: cuint,
  constraint: ptr CXString,
  expr: ptr CXIndexOptionsCXCursor,
): cuint {.importc: "clang_Cursor_getGCCAssemblyInput", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, get the constraint and expression cursor
##  to the Index-th output.
##  This function returns 1 when the cursor points at a GCC inline assembly
##  statement, `Index` is within bounds and both the `Constraint` and `Expr` are
##  not NULL.
##  Otherwise, this function returns 0 but leaves `Constraint` and `Expr`
##  intact.
##
##  Users are responsible for releasing the allocation of `Constraint` via
##  \c clang_disposeString.
##

proc cursorGetGCCAssemblyOutput*(
  cursor: CXIndexOptionsCXCursor,
  index: cuint,
  constraint: ptr CXString,
  expr: ptr CXIndexOptionsCXCursor,
): cuint {.importc: "clang_Cursor_getGCCAssemblyOutput", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, count the clobbers in it.
##  This function also returns 0 if the cursor does not point at a GCC inline
##  assembly block.
##

proc cursorGetGCCAssemblyNumClobbers*(
  cursor: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_getGCCAssemblyNumClobbers", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, get the Index-th clobber of it.
##  This function returns a valid empty string if the cursor does not point
##  at a GCC inline assembly block or `Index` is out of bounds.
##
##  Users are responsible for releasing the allocation of returned string via
##  \c clang_disposeString.
##

proc cursorGetGCCAssemblyClobber*(
  cursor: CXIndexOptionsCXCursor, index: cuint
): CXString {.importc: "clang_Cursor_getGCCAssemblyClobber", dynlib: CLangLib.}

##
##  Given a CXCursor_GCCAsmStmt cursor, check if the inline assembly is
##  `volatile`.
##  This function returns 0 if the cursor does not point at a GCC inline
##  assembly block.
##

proc cursorIsGCCAssemblyVolatile*(
  cursor: CXIndexOptionsCXCursor
): cuint {.importc: "clang_Cursor_isGCCAssemblyVolatile", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_MODULE Module introspection
##
##  The functions in this group provide access to information about modules.
##
##  @{
##

type CXIndexOptionsCXModule* = pointer

##
##  Given a CXCursor_ModuleImportDecl cursor, return the associated module.
##

proc cursorGetModule*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXModule {.importc: "clang_Cursor_getModule", dynlib: CLangLib.}

##
##  Given a CXFile header file, return the module that contains it, if one
##  exists.
##

proc getModuleForFile*(
  a1: CXTranslationUnit, a2: CXFile
): CXIndexOptionsCXModule {.importc: "clang_getModuleForFile", dynlib: CLangLib.}

##
##  \param Module a module object.
##
##  \returns the module file where the provided module object came from.
##

proc moduleGetASTFile*(
  module: CXIndexOptionsCXModule
): CXFile {.importc: "clang_Module_getASTFile", dynlib: CLangLib.}

##
##  \param Module a module object.
##
##  \returns the parent of a sub-module or NULL if the given module is top-level,
##  e.g. for 'std.vector' it will return the 'std' module.
##

proc moduleGetParent*(
  module: CXIndexOptionsCXModule
): CXIndexOptionsCXModule {.importc: "clang_Module_getParent", dynlib: CLangLib.}

##
##  \param Module a module object.
##
##  \returns the name of the module, e.g. for the 'std.vector' sub-module it
##  will return "vector".
##

proc moduleGetName*(
  module: CXIndexOptionsCXModule
): CXString {.importc: "clang_Module_getName", dynlib: CLangLib.}

##
##  \param Module a module object.
##
##  \returns the full name of the module, e.g. "std.vector".
##

proc moduleGetFullName*(
  module: CXIndexOptionsCXModule
): CXString {.importc: "clang_Module_getFullName", dynlib: CLangLib.}

##
##  \param Module a module object.
##
##  \returns non-zero if the module is a system one.
##

proc moduleIsSystem*(
  module: CXIndexOptionsCXModule
): cint {.importc: "clang_Module_isSystem", dynlib: CLangLib.}

##
##  \param Module a module object.
##
##  \returns the number of top level headers associated with this module.
##

proc moduleGetNumTopLevelHeaders*(
  a1: CXTranslationUnit, module: CXIndexOptionsCXModule
): cuint {.importc: "clang_Module_getNumTopLevelHeaders", dynlib: CLangLib.}

##
##  \param Module a module object.
##
##  \param Index top level header index (zero-based).
##
##  \returns the specified top level header associated with the module.
##

proc moduleGetTopLevelHeader*(
  a1: CXTranslationUnit, module: CXIndexOptionsCXModule, index: cuint
): CXFile {.importc: "clang_Module_getTopLevelHeader", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_CPP C++ AST introspection
##
##  The routines in this group provide access information in the ASTs specific
##  to C++ language features.
##
##  @{
##
##
##  Determine if a C++ constructor is a converting constructor.
##

proc cXXConstructorIsConvertingConstructor*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXConstructor_isConvertingConstructor", dynlib: CLangLib.}

##
##  Determine if a C++ constructor is a copy constructor.
##

proc cXXConstructorIsCopyConstructor*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXConstructor_isCopyConstructor", dynlib: CLangLib.}

##
##  Determine if a C++ constructor is the default constructor.
##

proc cXXConstructorIsDefaultConstructor*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXConstructor_isDefaultConstructor", dynlib: CLangLib.}

##
##  Determine if a C++ constructor is a move constructor.
##

proc cXXConstructorIsMoveConstructor*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXConstructor_isMoveConstructor", dynlib: CLangLib.}

##
##  Determine if a C++ field is declared 'mutable'.
##

proc cXXFieldIsMutable*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXField_isMutable", dynlib: CLangLib.}

##
##  Determine if a C++ method is declared '= default'.
##

proc cXXMethodIsDefaulted*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isDefaulted", dynlib: CLangLib.}

##
##  Determine if a C++ method is declared '= delete'.
##

proc cXXMethodIsDeleted*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isDeleted", dynlib: CLangLib.}

##
##  Determine if a C++ member function or member function template is
##  pure virtual.
##

proc cXXMethodIsPureVirtual*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isPureVirtual", dynlib: CLangLib.}

##
##  Determine if a C++ member function or member function template is
##  declared 'static'.
##

proc cXXMethodIsStatic*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isStatic", dynlib: CLangLib.}

##
##  Determine if a C++ member function or member function template is
##  explicitly declared 'virtual' or if it overrides a virtual method from
##  one of the base classes.
##

proc cXXMethodIsVirtual*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isVirtual", dynlib: CLangLib.}

##
##  Determine if a C++ member function is a copy-assignment operator,
##  returning 1 if such is the case and 0 otherwise.
##
##  > A copy-assignment operator `X::operator=` is a non-static,
##  > non-template member function of _class_ `X` with exactly one
##  > parameter of type `X`, `X&`, `const X&`, `volatile X&` or `const
##  > volatile X&`.
##
##  That is, for example, the `operator=` in:
##
##     class Foo {
##         bool operator=(const volatile Foo&);
##     };
##
##  Is a copy-assignment operator, while the `operator=` in:
##
##     class Bar {
##         bool operator=(const int&);
##     };
##
##  Is not.
##

proc cXXMethodIsCopyAssignmentOperator*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isCopyAssignmentOperator", dynlib: CLangLib.}

##
##  Determine if a C++ member function is a move-assignment operator,
##  returning 1 if such is the case and 0 otherwise.
##
##  > A move-assignment operator `X::operator=` is a non-static,
##  > non-template member function of _class_ `X` with exactly one
##  > parameter of type `X&&`, `const X&&`, `volatile X&&` or `const
##  > volatile X&&`.
##
##  That is, for example, the `operator=` in:
##
##     class Foo {
##         bool operator=(const volatile Foo&&);
##     };
##
##  Is a move-assignment operator, while the `operator=` in:
##
##     class Bar {
##         bool operator=(const int&&);
##     };
##
##  Is not.
##

proc cXXMethodIsMoveAssignmentOperator*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isMoveAssignmentOperator", dynlib: CLangLib.}

##
##  Determines if a C++ constructor or conversion function was declared
##  explicit, returning 1 if such is the case and 0 otherwise.
##
##  Constructors or conversion functions are declared explicit through
##  the use of the explicit specifier.
##
##  For example, the following constructor and conversion function are
##  not explicit as they lack the explicit specifier:
##
##      class Foo {
##          Foo();
##          operator int();
##      };
##
##  While the following constructor and conversion function are
##  explicit as they are declared with the explicit specifier.
##
##      class Foo {
##          explicit Foo();
##          explicit operator int();
##      };
##
##  This function will return 0 when given a cursor pointing to one of
##  the former declarations and it will return 1 for a cursor pointing
##  to the latter declarations.
##
##  The explicit specifier allows the user to specify a
##  conditional compile-time expression whose value decides
##  whether the marked element is explicit or not.
##
##  For example:
##
##      constexpr bool foo(int i) { return i % 2 == 0; }
##
##      class Foo {
##           explicit(foo(1)) Foo();
##           explicit(foo(2)) operator int();
##      }
##
##  This function will return 0 for the constructor and 1 for
##  the conversion function.
##

proc cXXMethodIsExplicit*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isExplicit", dynlib: CLangLib.}

##
##  Determine if a C++ record is abstract, i.e. whether a class or struct
##  has a pure virtual member function.
##

proc cXXRecordIsAbstract*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXRecord_isAbstract", dynlib: CLangLib.}

##
##  Determine if an enum declaration refers to a scoped enum.
##

proc enumDeclIsScoped*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_EnumDecl_isScoped", dynlib: CLangLib.}

##
##  Determine if a C++ member function or member function template is
##  declared 'const'.
##

proc cXXMethodIsConst*(
  c: CXIndexOptionsCXCursor
): cuint {.importc: "clang_CXXMethod_isConst", dynlib: CLangLib.}

##
##  Given a cursor that represents a template, determine
##  the cursor kind of the specializations would be generated by instantiating
##  the template.
##
##  This routine can be used to determine what flavor of function template,
##  class template, or class template partial specialization is stored in the
##  cursor. For example, it can describe whether a class template cursor is
##  declared with "struct", "class" or "union".
##
##  \param C The cursor to query. This cursor should represent a template
##  declaration.
##
##  \returns The cursor kind of the specializations that would be generated
##  by instantiating the template \p C. If \p C is not a template, returns
##  \c CXCursor_NoDeclFound.
##

proc getTemplateCursorKind*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursorKind {.
  importc: "clang_getTemplateCursorKind", dynlib: CLangLib
.}

##
##  Given a cursor that may represent a specialization or instantiation
##  of a template, retrieve the cursor that represents the template that it
##  specializes or from which it was instantiated.
##
##  This routine determines the template involved both for explicit
##  specializations of templates and for implicit instantiations of the template,
##  both of which are referred to as "specializations". For a class template
##  specialization (e.g., \c std::vector<bool>), this routine will return
##  either the primary template (\c std::vector) or, if the specialization was
##  instantiated from a class template partial specialization, the class template
##  partial specialization. For a class template partial specialization and a
##  function template specialization (including instantiations), this
##  this routine will return the specialized template.
##
##  For members of a class template (e.g., member functions, member classes, or
##  static data members), returns the specialized or instantiated member.
##  Although not strictly "templates" in the C++ language, members of class
##  templates have the same notions of specializations and instantiations that
##  templates do, so this routine treats them similarly.
##
##  \param C A cursor that may be a specialization of a template or a member
##  of a template.
##
##  \returns If the given cursor is a specialization or instantiation of a
##  template or a member thereof, the template or member that it specializes or
##  from which it was instantiated. Otherwise, returns a NULL cursor.
##

proc getSpecializedCursorTemplate*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXCursor {.
  importc: "clang_getSpecializedCursorTemplate", dynlib: CLangLib
.}

##
##  Given a cursor that references something else, return the source range
##  covering that reference.
##
##  \param C A cursor pointing to a member reference, a declaration reference, or
##  an operator call.
##  \param NameFlags A bitset with three independent flags:
##  CXNameRange_WantQualifier, CXNameRange_WantTemplateArgs, and
##  CXNameRange_WantSinglePiece.
##  \param PieceIndex For contiguous names or when passing the flag
##  CXNameRange_WantSinglePiece, only one piece with index 0 is
##  available. When the CXNameRange_WantSinglePiece flag is not passed for a
##  non-contiguous names, this index can be used to retrieve the individual
##  pieces of the name. See also CXNameRange_WantSinglePiece.
##
##  \returns The piece of the name pointed to by the given cursor. If there is no
##  name, or if the PieceIndex is out-of-range, a null-cursor will be returned.
##

proc getCursorReferenceNameRange*(
  c: CXIndexOptionsCXCursor, nameFlags: cuint, pieceIndex: cuint
): CXSourceRange {.importc: "clang_getCursorReferenceNameRange", dynlib: CLangLib.}

type CXIndexOptionsCXNameRefFlags* {.size: sizeof(cint).} = enum
  ##
  ##  Include the nested-name-specifier, e.g. Foo:: in x.Foo::y, in the
  ##  range.
  ##
  CXNameRangeWantQualifier = 0x1
    ##
    ##  Include the explicit template arguments, e.g. \<int> in x.f<int>,
    ##  in the range.
    ##
  CXNameRangeWantTemplateArgs = 0x2
    ##
    ##  If the name is non-contiguous, return the full spanning range.
    ##
    ##  Non-contiguous names occur in Objective-C when a selector with two or more
    ##  parameters is used, or in C++ when using an operator:
    ##  \code
    ##  [object doSomething:here withValue:there]; // Objective-C
    ##  return some_vector[1]; // C++
    ##  \endcode
    ##
  CXNameRangeWantSinglePiece = 0x4

##
##  @}
##
##
##  \defgroup CINDEX_LEX Token extraction and manipulation
##
##  The routines in this group provide access to the tokens within a
##  translation unit, along with a semantic mapping of those tokens to
##  their corresponding cursors.
##
##  @{
##
##
##  Describes a kind of token.
##

type
  ##
  ##  A token that contains some kind of punctuation.
  ##
  CXIndexOptionsCXTokenKind* {.size: sizeof(cint).} = enum
    CXTokenPunctuation
      ##
      ##  A language keyword.
      ##
    CXTokenKeyword
      ##
      ##  An identifier (that is not a keyword).
      ##
    CXTokenIdentifier
      ##
      ##  A numeric, string, or character literal.
      ##
    CXTokenLiteral
      ##
      ##  A comment.
      ##
    CXTokenComment

##
##  Describes a single preprocessing token.
##

type CXIndexOptionsCXToken* {.bycopy.} = object
  intData*: array[4, cuint]
  ptrData*: pointer

##
##  Get the raw lexical token starting with the given location.
##
##  \param TU the translation unit whose text is being tokenized.
##
##  \param Location the source location with which the token starts.
##
##  \returns The token starting with the given location or NULL if no such token
##  exist. The returned pointer must be freed with clang_disposeTokens before the
##  translation unit is destroyed.
##

proc getToken*(
  tu: CXTranslationUnit, location: CXSourceLocation
): ptr CXIndexOptionsCXToken {.importc: "clang_getToken", dynlib: CLangLib.}

##
##  Determine the kind of the given token.
##

proc getTokenKind*(
  a1: CXIndexOptionsCXToken
): CXIndexOptionsCXTokenKind {.importc: "clang_getTokenKind", dynlib: CLangLib.}

##
##  Determine the spelling of the given token.
##
##  The spelling of a token is the textual representation of that token, e.g.,
##  the text of an identifier or keyword.
##

proc getTokenSpelling*(
  a1: CXTranslationUnit, a2: CXIndexOptionsCXToken
): CXString {.importc: "clang_getTokenSpelling", dynlib: CLangLib.}

##
##  Retrieve the source location of the given token.
##

proc getTokenLocation*(
  a1: CXTranslationUnit, a2: CXIndexOptionsCXToken
): CXSourceLocation {.importc: "clang_getTokenLocation", dynlib: CLangLib.}

##
##  Retrieve a source range that covers the given token.
##

proc getTokenExtent*(
  a1: CXTranslationUnit, a2: CXIndexOptionsCXToken
): CXSourceRange {.importc: "clang_getTokenExtent", dynlib: CLangLib.}

##
##  Tokenize the source code described by the given range into raw
##  lexical tokens.
##
##  \param TU the translation unit whose text is being tokenized.
##
##  \param Range the source range in which text should be tokenized. All of the
##  tokens produced by tokenization will fall within this source range,
##
##  \param Tokens this pointer will be set to point to the array of tokens
##  that occur within the given source range. The returned pointer must be
##  freed with clang_disposeTokens() before the translation unit is destroyed.
##
##  \param NumTokens will be set to the number of tokens in the \c *Tokens
##  array.
##
##

proc tokenize*(
  tu: CXTranslationUnit,
  range: CXSourceRange,
  tokens: ptr ptr CXIndexOptionsCXToken,
  numTokens: ptr cuint,
) {.importc: "clang_tokenize", dynlib: CLangLib.}

##
##  Annotate the given set of tokens by providing cursors for each token
##  that can be mapped to a specific entity within the abstract syntax tree.
##
##  This token-annotation routine is equivalent to invoking
##  clang_getCursor() for the source locations of each of the
##  tokens. The cursors provided are filtered, so that only those
##  cursors that have a direct correspondence to the token are
##  accepted. For example, given a function call \c f(x),
##  clang_getCursor() would provide the following cursors:
##
##    * when the cursor is over the 'f', a DeclRefExpr cursor referring to 'f'.
##    * when the cursor is over the '(' or the ')', a CallExpr referring to 'f'.
##    * when the cursor is over the 'x', a DeclRefExpr cursor referring to 'x'.
##
##  Only the first and last of these cursors will occur within the
##  annotate, since the tokens "f" and "x' directly refer to a function
##  and a variable, respectively, but the parentheses are just a small
##  part of the full syntax of the function call expression, which is
##  not provided as an annotation.
##
##  \param TU the translation unit that owns the given tokens.
##
##  \param Tokens the set of tokens to annotate.
##
##  \param NumTokens the number of tokens in \p Tokens.
##
##  \param Cursors an array of \p NumTokens cursors, whose contents will be
##  replaced with the cursors corresponding to each token.
##

proc annotateTokens*(
  tu: CXTranslationUnit,
  tokens: ptr CXIndexOptionsCXToken,
  numTokens: cuint,
  cursors: ptr CXIndexOptionsCXCursor,
) {.importc: "clang_annotateTokens", dynlib: CLangLib.}

##
##  Free the given set of tokens.
##

proc disposeTokens*(
  tu: CXTranslationUnit, tokens: ptr CXIndexOptionsCXToken, numTokens: cuint
) {.importc: "clang_disposeTokens", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_DEBUG Debugging facilities
##
##  These routines are used for testing and debugging, only, and should not
##  be relied upon.
##
##  @{
##
##  for debug/testing

proc getCursorKindSpelling*(
  kind: CXIndexOptionsCXCursorKind
): CXString {.importc: "clang_getCursorKindSpelling", dynlib: CLangLib.}

proc getDefinitionSpellingAndExtent*(
  a1: CXIndexOptionsCXCursor,
  startBuf: cstringArray,
  endBuf: cstringArray,
  startLine: ptr cuint,
  startColumn: ptr cuint,
  endLine: ptr cuint,
  endColumn: ptr cuint,
) {.importc: "clang_getDefinitionSpellingAndExtent", dynlib: CLangLib.}

proc enableStackTraces*() {.importc: "clang_enableStackTraces", dynlib: CLangLib.}
proc executeOnThread*(
  fn: proc(a1: pointer), userData: pointer, stackSize: cuint
) {.importc: "clang_executeOnThread", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_CODE_COMPLET Code completion
##
##  Code completion involves taking an (incomplete) source file, along with
##  knowledge of where the user is actively editing that file, and suggesting
##  syntactically- and semantically-valid constructs that the user might want to
##  use at that particular point in the source code. These data structures and
##  routines provide support for code completion.
##
##  @{
##
##
##  A semantic string that describes a code-completion result.
##
##  A semantic string that describes the formatting of a code-completion
##  result as a single "template" of text that should be inserted into the
##  source buffer when a particular code-completion result is selected.
##  Each semantic string is made up of some number of "chunks", each of which
##  contains some text along with a description of what that text means, e.g.,
##  the name of the entity being referenced, whether the text chunk is part of
##  the template, or whether it is a "placeholder" that the user should replace
##  with actual code,of a specific kind. See \c CXCompletionChunkKind for a
##  description of the different kinds of chunks.
##

type CXIndexOptionsCXCompletionString* = pointer

##
##  A single result of code completion.
##

type CXIndexOptionsCXCompletionResult* {.bycopy.} = object
  ##
  ##  The kind of entity that this completion refers to.
  ##
  ##  The cursor kind will be a macro, keyword, or a declaration (one of the
  ##  *Decl cursor kinds), describing the entity that the completion is
  ##  referring to.
  ##
  ##  \todo In the future, we would like to provide a full cursor, to allow
  ##  the client to extract additional information from declaration.
  ##
  cursorKind*: CXIndexOptionsCXCursorKind
  ##
  ##  The code-completion string that describes how to insert this
  ##  code-completion result into the editing buffer.
  ##
  completionString*: CXIndexOptionsCXCompletionString

##
##  Describes a single piece of text within a code-completion string.
##
##  Each "chunk" within a code-completion string (\c CXCompletionString) is
##  either a piece of text with a specific "kind" that describes how that text
##  should be interpreted by the client or is another completion string.
##

type CXIndexOptionsCXCompletionChunkKind* {.size: sizeof(cint).} = enum
  ##
  ##  A code-completion string that describes "optional" text that
  ##  could be a part of the template (but is not required).
  ##
  ##  The Optional chunk is the only kind of chunk that has a code-completion
  ##  string for its representation, which is accessible via
  ##  \c clang_getCompletionChunkCompletionString(). The code-completion string
  ##  describes an additional part of the template that is completely optional.
  ##  For example, optional chunks can be used to describe the placeholders for
  ##  arguments that match up with defaulted function parameters, e.g. given:
  ##
  ##  \code
  ##  void f(int x, float y = 3.14, double z = 2.71828);
  ##  \endcode
  ##
  ##  The code-completion string for this function would contain:
  ##    - a TypedText chunk for "f".
  ##    - a LeftParen chunk for "(".
  ##    - a Placeholder chunk for "int x"
  ##    - an Optional chunk containing the remaining defaulted arguments, e.g.,
  ##        - a Comma chunk for ","
  ##        - a Placeholder chunk for "float y"
  ##        - an Optional chunk containing the last defaulted argument:
  ##            - a Comma chunk for ","
  ##            - a Placeholder chunk for "double z"
  ##    - a RightParen chunk for ")"
  ##
  ##  There are many ways to handle Optional chunks. Two simple approaches are:
  ##    - Completely ignore optional chunks, in which case the template for the
  ##      function "f" would only include the first parameter ("int x").
  ##    - Fully expand all optional chunks, in which case the template for the
  ##      function "f" would have all of the parameters.
  ##
  CXCompletionChunkOptional
    ##
    ##  Text that a user would be expected to type to get this
    ##  code-completion result.
    ##
    ##  There will be exactly one "typed text" chunk in a semantic string, which
    ##  will typically provide the spelling of a keyword or the name of a
    ##  declaration that could be used at the current code point. Clients are
    ##  expected to filter the code-completion results based on the text in this
    ##  chunk.
    ##
  CXCompletionChunkTypedText
    ##
    ##  Text that should be inserted as part of a code-completion result.
    ##
    ##  A "text" chunk represents text that is part of the template to be
    ##  inserted into user code should this particular code-completion result
    ##  be selected.
    ##
  CXCompletionChunkText
    ##
    ##  Placeholder text that should be replaced by the user.
    ##
    ##  A "placeholder" chunk marks a place where the user should insert text
    ##  into the code-completion template. For example, placeholders might mark
    ##  the function parameters for a function declaration, to indicate that the
    ##  user should provide arguments for each of those parameters. The actual
    ##  text in a placeholder is a suggestion for the text to display before
    ##  the user replaces the placeholder with real code.
    ##
  CXCompletionChunkPlaceholder
    ##
    ##  Informative text that should be displayed but never inserted as
    ##  part of the template.
    ##
    ##  An "informative" chunk contains annotations that can be displayed to
    ##  help the user decide whether a particular code-completion result is the
    ##  right option, but which is not part of the actual template to be inserted
    ##  by code completion.
    ##
  CXCompletionChunkInformative
    ##
    ##  Text that describes the current parameter when code-completion is
    ##  referring to function call, message send, or template specialization.
    ##
    ##  A "current parameter" chunk occurs when code-completion is providing
    ##  information about a parameter corresponding to the argument at the
    ##  code-completion point. For example, given a function
    ##
    ##  \code
    ##  int add(int x, int y);
    ##  \endcode
    ##
    ##  and the source code \c add(, where the code-completion point is after the
    ##  "(", the code-completion string will contain a "current parameter" chunk
    ##  for "int x", indicating that the current argument will initialize that
    ##  parameter. After typing further, to \c add(17, (where the code-completion
    ##  point is after the ","), the code-completion string will contain a
    ##  "current parameter" chunk to "int y".
    ##
  CXCompletionChunkCurrentParameter
    ##
    ##  A left parenthesis ('('), used to initiate a function call or
    ##  signal the beginning of a function parameter list.
    ##
  CXCompletionChunkLeftParen
    ##
    ##  A right parenthesis (')'), used to finish a function call or
    ##  signal the end of a function parameter list.
    ##
  CXCompletionChunkRightParen
    ##
    ##  A left bracket ('[').
    ##
  CXCompletionChunkLeftBracket
    ##
    ##  A right bracket (']').
    ##
  CXCompletionChunkRightBracket
    ##
    ##  A left brace ('{').
    ##
  CXCompletionChunkLeftBrace
    ##
    ##  A right brace ('}').
    ##
  CXCompletionChunkRightBrace
    ##
    ##  A left angle bracket ('<').
    ##
  CXCompletionChunkLeftAngle
    ##
    ##  A right angle bracket ('>').
    ##
  CXCompletionChunkRightAngle
    ##
    ##  A comma separator (',').
    ##
  CXCompletionChunkComma
    ##
    ##  Text that specifies the result type of a given result.
    ##
    ##  This special kind of informative chunk is not meant to be inserted into
    ##  the text buffer. Rather, it is meant to illustrate the type that an
    ##  expression using the given completion string would have.
    ##
  CXCompletionChunkResultType
    ##
    ##  A colon (':').
    ##
  CXCompletionChunkColon
    ##
    ##  A semicolon (';').
    ##
  CXCompletionChunkSemiColon
    ##
    ##  An '=' sign.
    ##
  CXCompletionChunkEqual
    ##
    ##  Horizontal space (' ').
    ##
  CXCompletionChunkHorizontalSpace
    ##
    ##  Vertical space ('\\n'), after which it is generally a good idea to
    ##  perform indentation.
    ##
  CXCompletionChunkVerticalSpace

##
##  Determine the kind of a particular chunk within a completion string.
##
##  \param completion_string the completion string to query.
##
##  \param chunk_number the 0-based index of the chunk in the completion string.
##
##  \returns the kind of the chunk at the index \c chunk_number.
##

proc getCompletionChunkKind*(
  completionString: CXIndexOptionsCXCompletionString, chunkNumber: cuint
): CXIndexOptionsCXCompletionChunkKind {.
  importc: "clang_getCompletionChunkKind", dynlib: CLangLib
.}

##
##  Retrieve the text associated with a particular chunk within a
##  completion string.
##
##  \param completion_string the completion string to query.
##
##  \param chunk_number the 0-based index of the chunk in the completion string.
##
##  \returns the text associated with the chunk at index \c chunk_number.
##

proc getCompletionChunkText*(
  completionString: CXIndexOptionsCXCompletionString, chunkNumber: cuint
): CXString {.importc: "clang_getCompletionChunkText", dynlib: CLangLib.}

##
##  Retrieve the completion string associated with a particular chunk
##  within a completion string.
##
##  \param completion_string the completion string to query.
##
##  \param chunk_number the 0-based index of the chunk in the completion string.
##
##  \returns the completion string associated with the chunk at index
##  \c chunk_number.
##

proc getCompletionChunkCompletionString*(
  completionString: CXIndexOptionsCXCompletionString, chunkNumber: cuint
): CXIndexOptionsCXCompletionString {.
  importc: "clang_getCompletionChunkCompletionString", dynlib: CLangLib
.}

##
##  Retrieve the number of chunks in the given code-completion string.
##

proc getNumCompletionChunks*(
  completionString: CXIndexOptionsCXCompletionString
): cuint {.importc: "clang_getNumCompletionChunks", dynlib: CLangLib.}

##
##  Determine the priority of this code completion.
##
##  The priority of a code completion indicates how likely it is that this
##  particular completion is the completion that the user will select. The
##  priority is selected by various internal heuristics.
##
##  \param completion_string The completion string to query.
##
##  \returns The priority of this completion string. Smaller values indicate
##  higher-priority (more likely) completions.
##

proc getCompletionPriority*(
  completionString: CXIndexOptionsCXCompletionString
): cuint {.importc: "clang_getCompletionPriority", dynlib: CLangLib.}

##
##  Determine the availability of the entity that this code-completion
##  string refers to.
##
##  \param completion_string The completion string to query.
##
##  \returns The availability of the completion string.
##

proc getCompletionAvailability*(
  completionString: CXIndexOptionsCXCompletionString
): CXAvailabilityKind {.importc: "clang_getCompletionAvailability", dynlib: CLangLib.}

##
##  Retrieve the number of annotations associated with the given
##  completion string.
##
##  \param completion_string the completion string to query.
##
##  \returns the number of annotations associated with the given completion
##  string.
##

proc getCompletionNumAnnotations*(
  completionString: CXIndexOptionsCXCompletionString
): cuint {.importc: "clang_getCompletionNumAnnotations", dynlib: CLangLib.}

##
##  Retrieve the annotation associated with the given completion string.
##
##  \param completion_string the completion string to query.
##
##  \param annotation_number the 0-based index of the annotation of the
##  completion string.
##
##  \returns annotation string associated with the completion at index
##  \c annotation_number, or a NULL string if that annotation is not available.
##

proc getCompletionAnnotation*(
  completionString: CXIndexOptionsCXCompletionString, annotationNumber: cuint
): CXString {.importc: "clang_getCompletionAnnotation", dynlib: CLangLib.}

##
##  Retrieve the parent context of the given completion string.
##
##  The parent context of a completion string is the semantic parent of
##  the declaration (if any) that the code completion represents. For example,
##  a code completion for an Objective-C method would have the method's class
##  or protocol as its context.
##
##  \param completion_string The code completion string whose parent is
##  being queried.
##
##  \param kind DEPRECATED: always set to CXCursor_NotImplemented if non-NULL.
##
##  \returns The name of the completion parent, e.g., "NSObject" if
##  the completion string represents a method in the NSObject class.
##

proc getCompletionParent*(
  completionString: CXIndexOptionsCXCompletionString,
  kind: ptr CXIndexOptionsCXCursorKind,
): CXString {.importc: "clang_getCompletionParent", dynlib: CLangLib.}

##
##  Retrieve the brief documentation comment attached to the declaration
##  that corresponds to the given completion string.
##

proc getCompletionBriefComment*(
  completionString: CXIndexOptionsCXCompletionString
): CXString {.importc: "clang_getCompletionBriefComment", dynlib: CLangLib.}

##
##  Retrieve a completion string for an arbitrary declaration or macro
##  definition cursor.
##
##  \param cursor The cursor to query.
##
##  \returns A non-context-sensitive completion string for declaration and macro
##  definition cursors, or NULL for other kinds of cursors.
##

proc getCursorCompletionString*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXCompletionString {.
  importc: "clang_getCursorCompletionString", dynlib: CLangLib
.}

##
##  Contains the results of code-completion.
##
##  This data structure contains the results of code completion, as
##  produced by \c clang_codeCompleteAt(). Its contents must be freed by
##  \c clang_disposeCodeCompleteResults.
##

type CXIndexOptionsCXCodeCompleteResults* {.bycopy.} = object
  ##
  ##  The code-completion results.
  ##
  results*: ptr CXIndexOptionsCXCompletionResult
  ##
  ##  The number of code-completion results stored in the
  ##  \c Results array.
  ##
  numResults*: cuint

##
##  Retrieve the number of fix-its for the given completion index.
##
##  Calling this makes sense only if CXCodeComplete_IncludeCompletionsWithFixIts
##  option was set.
##
##  \param results The structure keeping all completion results
##
##  \param completion_index The index of the completion
##
##  \return The number of fix-its which must be applied before the completion at
##  completion_index can be applied
##

proc getCompletionNumFixIts*(
  results: ptr CXIndexOptionsCXCodeCompleteResults, completionIndex: cuint
): cuint {.importc: "clang_getCompletionNumFixIts", dynlib: CLangLib.}

##
##  Fix-its that *must* be applied before inserting the text for the
##  corresponding completion.
##
##  By default, clang_codeCompleteAt() only returns completions with empty
##  fix-its. Extra completions with non-empty fix-its should be explicitly
##  requested by setting CXCodeComplete_IncludeCompletionsWithFixIts.
##
##  For the clients to be able to compute position of the cursor after applying
##  fix-its, the following conditions are guaranteed to hold for
##  replacement_range of the stored fix-its:
##   - Ranges in the fix-its are guaranteed to never contain the completion
##   point (or identifier under completion point, if any) inside them, except
##   at the start or at the end of the range.
##   - If a fix-it range starts or ends with completion point (or starts or
##   ends after the identifier under completion point), it will contain at
##   least one character. It allows to unambiguously recompute completion
##   point after applying the fix-it.
##
##  The intuition is that provided fix-its change code around the identifier we
##  complete, but are not allowed to touch the identifier itself or the
##  completion point. One example of completions with corrections are the ones
##  replacing '.' with '->' and vice versa:
##
##  std::unique_ptr<std::vector<int>> vec_ptr;
##  In 'vec_ptr.^', one of the completions is 'push_back', it requires
##  replacing '.' with '->'.
##  In 'vec_ptr->^', one of the completions is 'release', it requires
##  replacing '->' with '.'.
##
##  \param results The structure keeping all completion results
##
##  \param completion_index The index of the completion
##
##  \param fixit_index The index of the fix-it for the completion at
##  completion_index
##
##  \param replacement_range The fix-it range that must be replaced before the
##  completion at completion_index can be applied
##
##  \returns The fix-it string that must replace the code at replacement_range
##  before the completion at completion_index can be applied
##

proc getCompletionFixIt*(
  results: ptr CXIndexOptionsCXCodeCompleteResults,
  completionIndex: cuint,
  fixitIndex: cuint,
  replacementRange: ptr CXSourceRange,
): CXString {.importc: "clang_getCompletionFixIt", dynlib: CLangLib.}

##
##  Flags that can be passed to \c clang_codeCompleteAt() to
##  modify its behavior.
##
##  The enumerators in this enumeration can be bitwise-OR'd together to
##  provide multiple options to \c clang_codeCompleteAt().
##

type CXIndexOptionsCXCodeCompleteFlags* {.size: sizeof(cint).} = enum
  ##
  ##  Whether to include macros within the set of code
  ##  completions returned.
  ##
  CXCodeCompleteIncludeMacros = 0x01
    ##
    ##  Whether to include code patterns for language constructs
    ##  within the set of code completions, e.g., for loops.
    ##
  CXCodeCompleteIncludeCodePatterns = 0x02
    ##
    ##  Whether to include brief documentation within the set of code
    ##  completions returned.
    ##
  CXCodeCompleteIncludeBriefComments = 0x04
    ##
    ##  Whether to speed up completion by omitting top- or namespace-level entities
    ##  defined in the preamble. There's no guarantee any particular entity is
    ##  omitted. This may be useful if the headers are indexed externally.
    ##
  CXCodeCompleteSkipPreamble = 0x08
    ##
    ##  Whether to include completions with small
    ##  fix-its, e.g. change '.' to '->' on member access, etc.
    ##
  CXCodeCompleteIncludeCompletionsWithFixIts = 0x10

##
##  Bits that represent the context under which completion is occurring.
##
##  The enumerators in this enumeration may be bitwise-OR'd together if multiple
##  contexts are occurring simultaneously.
##

type CXIndexOptionsCXCompletionContext* {.size: sizeof(cint).} = enum
  ##
  ##  The context for completions is unexposed, as only Clang results
  ##  should be included. (This is equivalent to having no context bits set.)
  ##
  CXCompletionContextUnexposed = 0
    ##
    ##  Completions for any possible type should be included in the results.
    ##
  CXCompletionContextAnyType = 1 shl 0
    ##
    ##  Completions for any possible value (variables, function calls, etc.)
    ##  should be included in the results.
    ##
  CXCompletionContextAnyValue = 1 shl 1
    ##
    ##  Completions for values that resolve to an Objective-C object should
    ##  be included in the results.
    ##
  CXCompletionContextObjCObjectValue = 1 shl 2
    ##
    ##  Completions for values that resolve to an Objective-C selector
    ##  should be included in the results.
    ##
  CXCompletionContextObjCSelectorValue = 1 shl 3
    ##
    ##  Completions for values that resolve to a C++ class type should be
    ##  included in the results.
    ##
  CXCompletionContextCXXClassTypeValue = 1 shl 4
    ##
    ##  Completions for fields of the member being accessed using the dot
    ##  operator should be included in the results.
    ##
  CXCompletionContextDotMemberAccess = 1 shl 5
    ##
    ##  Completions for fields of the member being accessed using the arrow
    ##  operator should be included in the results.
    ##
  CXCompletionContextArrowMemberAccess = 1 shl 6
    ##
    ##  Completions for properties of the Objective-C object being accessed
    ##  using the dot operator should be included in the results.
    ##
  CXCompletionContextObjCPropertyAccess = 1 shl 7
    ##
    ##  Completions for enum tags should be included in the results.
    ##
  CXCompletionContextEnumTag = 1 shl 8
    ##
    ##  Completions for union tags should be included in the results.
    ##
  CXCompletionContextUnionTag = 1 shl 9
    ##
    ##  Completions for struct tags should be included in the results.
    ##
  CXCompletionContextStructTag = 1 shl 10
    ##
    ##  Completions for C++ class names should be included in the results.
    ##
  CXCompletionContextClassTag = 1 shl 11
    ##
    ##  Completions for C++ namespaces and namespace aliases should be
    ##  included in the results.
    ##
  CXCompletionContextNamespace = 1 shl 12
    ##
    ##  Completions for C++ nested name specifiers should be included in
    ##  the results.
    ##
  CXCompletionContextNestedNameSpecifier = 1 shl 13
    ##
    ##  Completions for Objective-C interfaces (classes) should be included
    ##  in the results.
    ##
  CXCompletionContextObjCInterface = 1 shl 14
    ##
    ##  Completions for Objective-C protocols should be included in
    ##  the results.
    ##
  CXCompletionContextObjCProtocol = 1 shl 15
    ##
    ##  Completions for Objective-C categories should be included in
    ##  the results.
    ##
  CXCompletionContextObjCCategory = 1 shl 16
    ##
    ##  Completions for Objective-C instance messages should be included
    ##  in the results.
    ##
  CXCompletionContextObjCInstanceMessage = 1 shl 17
    ##
    ##  Completions for Objective-C class messages should be included in
    ##  the results.
    ##
  CXCompletionContextObjCClassMessage = 1 shl 18
    ##
    ##  Completions for Objective-C selector names should be included in
    ##  the results.
    ##
  CXCompletionContextObjCSelectorName = 1 shl 19
    ##
    ##  Completions for preprocessor macro names should be included in
    ##  the results.
    ##
  CXCompletionContextMacroName = 1 shl 20
    ##
    ##  Natural language completions should be included in the results.
    ##
  CXCompletionContextNaturalLanguage = 1 shl 21
    ##
    ##  #include file completions should be included in the results.
    ##
  CXCompletionContextIncludedFile = 1 shl 22
    ##
    ##  The current context is unknown, so set all contexts.
    ##
  CXCompletionContextUnknown = ((1 shl 23) - 1)

##
##  Returns a default set of code-completion options that can be
##  passed to\c clang_codeCompleteAt().
##

proc defaultCodeCompleteOptions*(): cuint {.
  importc: "clang_defaultCodeCompleteOptions", dynlib: CLangLib
.}

##
##  Perform code completion at a given location in a translation unit.
##
##  This function performs code completion at a particular file, line, and
##  column within source code, providing results that suggest potential
##  code snippets based on the context of the completion. The basic model
##  for code completion is that Clang will parse a complete source file,
##  performing syntax checking up to the location where code-completion has
##  been requested. At that point, a special code-completion token is passed
##  to the parser, which recognizes this token and determines, based on the
##  current location in the C/Objective-C/C++ grammar and the state of
##  semantic analysis, what completions to provide. These completions are
##  returned via a new \c CXCodeCompleteResults structure.
##
##  Code completion itself is meant to be triggered by the client when the
##  user types punctuation characters or whitespace, at which point the
##  code-completion location will coincide with the cursor. For example, if \c p
##  is a pointer, code-completion might be triggered after the "-" and then
##  after the ">" in \c p->. When the code-completion location is after the ">",
##  the completion results will provide, e.g., the members of the struct that
##  "p" points to. The client is responsible for placing the cursor at the
##  beginning of the token currently being typed, then filtering the results
##  based on the contents of the token. For example, when code-completing for
##  the expression \c p->get, the client should provide the location just after
##  the ">" (e.g., pointing at the "g") to this code-completion hook. Then, the
##  client can filter the results based on the current token text ("get"), only
##  showing those results that start with "get". The intent of this interface
##  is to separate the relatively high-latency acquisition of code-completion
##  results from the filtering of results on a per-character basis, which must
##  have a lower latency.
##
##  \param TU The translation unit in which code-completion should
##  occur. The source files for this translation unit need not be
##  completely up-to-date (and the contents of those source files may
##  be overridden via \p unsaved_files). Cursors referring into the
##  translation unit may be invalidated by this invocation.
##
##  \param complete_filename The name of the source file where code
##  completion should be performed. This filename may be any file
##  included in the translation unit.
##
##  \param complete_line The line at which code-completion should occur.
##
##  \param complete_column The column at which code-completion should occur.
##  Note that the column should point just after the syntactic construct that
##  initiated code completion, and not in the middle of a lexical token.
##
##  \param unsaved_files the Files that have not yet been saved to disk
##  but may be required for parsing or code completion, including the
##  contents of those files.  The contents and name of these files (as
##  specified by CXUnsavedFile) are copied when necessary, so the
##  client only needs to guarantee their validity until the call to
##  this function returns.
##
##  \param num_unsaved_files The number of unsaved file entries in \p
##  unsaved_files.
##
##  \param options Extra options that control the behavior of code
##  completion, expressed as a bitwise OR of the enumerators of the
##  CXCodeComplete_Flags enumeration. The
##  \c clang_defaultCodeCompleteOptions() function returns a default set
##  of code-completion options.
##
##  \returns If successful, a new \c CXCodeCompleteResults structure
##  containing code-completion results, which should eventually be
##  freed with \c clang_disposeCodeCompleteResults(). If code
##  completion fails, returns NULL.
##

proc codeCompleteAt*(
  tu: CXTranslationUnit,
  completeFilename: cstring,
  completeLine: cuint,
  completeColumn: cuint,
  unsavedFiles: ptr CXUnsavedFile,
  numUnsavedFiles: cuint,
  options: cuint,
): ptr CXIndexOptionsCXCodeCompleteResults {.
  importc: "clang_codeCompleteAt", dynlib: CLangLib
.}

##
##  Sort the code-completion results in case-insensitive alphabetical
##  order.
##
##  \param Results The set of results to sort.
##  \param NumResults The number of results in \p Results.
##

proc sortCodeCompletionResults*(
  results: ptr CXIndexOptionsCXCompletionResult, numResults: cuint
) {.importc: "clang_sortCodeCompletionResults", dynlib: CLangLib.}

##
##  Free the given set of code-completion results.
##

proc disposeCodeCompleteResults*(
  results: ptr CXIndexOptionsCXCodeCompleteResults
) {.importc: "clang_disposeCodeCompleteResults", dynlib: CLangLib.}

##
##  Determine the number of diagnostics produced prior to the
##  location where code completion was performed.
##

proc codeCompleteGetNumDiagnostics*(
  results: ptr CXIndexOptionsCXCodeCompleteResults
): cuint {.importc: "clang_codeCompleteGetNumDiagnostics", dynlib: CLangLib.}

##
##  Retrieve a diagnostic associated with the given code completion.
##
##  \param Results the code completion results to query.
##  \param Index the zero-based diagnostic number to retrieve.
##
##  \returns the requested diagnostic. This diagnostic must be freed
##  via a call to \c clang_disposeDiagnostic().
##

proc codeCompleteGetDiagnostic*(
  results: ptr CXIndexOptionsCXCodeCompleteResults, index: cuint
): CXDiagnostic {.importc: "clang_codeCompleteGetDiagnostic", dynlib: CLangLib.}

##
##  Determines what completions are appropriate for the context
##  the given code completion.
##
##  \param Results the code completion results to query
##
##  \returns the kinds of completions that are appropriate for use
##  along with the given code completion results.
##

proc codeCompleteGetContexts*(
  results: ptr CXIndexOptionsCXCodeCompleteResults
): culonglong {.importc: "clang_codeCompleteGetContexts", dynlib: CLangLib.}

##
##  Returns the cursor kind for the container for the current code
##  completion context. The container is only guaranteed to be set for
##  contexts where a container exists (i.e. member accesses or Objective-C
##  message sends); if there is not a container, this function will return
##  CXCursor_InvalidCode.
##
##  \param Results the code completion results to query
##
##  \param IsIncomplete on return, this value will be false if Clang has complete
##  information about the container. If Clang does not have complete
##  information, this value will be true.
##
##  \returns the container kind, or CXCursor_InvalidCode if there is not a
##  container
##

proc codeCompleteGetContainerKind*(
  results: ptr CXIndexOptionsCXCodeCompleteResults, isIncomplete: ptr cuint
): CXIndexOptionsCXCursorKind {.
  importc: "clang_codeCompleteGetContainerKind", dynlib: CLangLib
.}

##
##  Returns the USR for the container for the current code completion
##  context. If there is not a container for the current context, this
##  function will return the empty string.
##
##  \param Results the code completion results to query
##
##  \returns the USR for the container
##

proc codeCompleteGetContainerUSR*(
  results: ptr CXIndexOptionsCXCodeCompleteResults
): CXString {.importc: "clang_codeCompleteGetContainerUSR", dynlib: CLangLib.}

##
##  Returns the currently-entered selector for an Objective-C message
##  send, formatted like "initWithFoo:bar:". Only guaranteed to return a
##  non-empty string for CXCompletionContext_ObjCInstanceMessage and
##  CXCompletionContext_ObjCClassMessage.
##
##  \param Results the code completion results to query
##
##  \returns the selector (or partial selector) that has been entered thus far
##  for an Objective-C message send.
##

proc codeCompleteGetObjCSelector*(
  results: ptr CXIndexOptionsCXCodeCompleteResults
): CXString {.importc: "clang_codeCompleteGetObjCSelector", dynlib: CLangLib.}

##
##  @}
##
##
##  \defgroup CINDEX_MISC Miscellaneous utility functions
##
##  @{
##
##
##  Return a version string, suitable for showing to a user, but not
##         intended to be parsed (the format is not guaranteed to be stable).
##

proc getClangVersion*(): CXString {.importc: "clang_getClangVersion", dynlib: CLangLib.}
##
##  Enable/disable crash recovery.
##
##  \param isEnabled Flag to indicate if crash recovery is enabled.  A non-zero
##         value enables crash recovery, while 0 disables it.
##

proc toggleCrashRecovery*(
  isEnabled: cuint
) {.importc: "clang_toggleCrashRecovery", dynlib: CLangLib.}

##
##  Visitor invoked for each file in a translation unit
##         (used with clang_getInclusions()).
##
##  This visitor function will be invoked by clang_getInclusions() for each
##  file included (either at the top-level or by \#include directives) within
##  a translation unit.  The first argument is the file being included, and
##  the second and third arguments provide the inclusion stack.  The
##  array is sorted in order of immediate inclusion.  For example,
##  the first element refers to the location that included 'included_file'.
##

type CXIndexOptionsCXInclusionVisitor* = proc(
  includedFile: CXFile,
  inclusionStack: ptr CXSourceLocation,
  includeLen: cuint,
  clientData: CXClientData,
)

##
##  Visit the set of preprocessor inclusions in a translation unit.
##    The visitor function is called with the provided data for every included
##    file.  This does not include headers included by the PCH file (unless one
##    is inspecting the inclusions in the PCH file itself).
##

proc getInclusions*(
  tu: CXTranslationUnit,
  visitor: CXIndexOptionsCXInclusionVisitor,
  clientData: CXClientData,
) {.importc: "clang_getInclusions", dynlib: CLangLib.}

type CXIndexOptionsCXEvalResultKind* {.size: sizeof(cint).} = enum
  CXEvalUnExposed = 0
  CXEvalInt = 1
  CXEvalFloat = 2
  CXEvalObjCStrLiteral = 3
  CXEvalStrLiteral = 4
  CXEvalCFStr = 5
  CXEvalOther = 6

##
##  Evaluation result of a cursor
##

type CXIndexOptionsCXEvalResult* = pointer

##
##  If cursor is a statement declaration tries to evaluate the
##  statement and if its variable, tries to evaluate its initializer,
##  into its corresponding type.
##  If it's an expression, tries to evaluate the expression.
##

proc cursorEvaluate*(
  c: CXIndexOptionsCXCursor
): CXIndexOptionsCXEvalResult {.importc: "clang_Cursor_Evaluate", dynlib: CLangLib.}

##
##  Returns the kind of the evaluated result.
##

proc evalResultGetKind*(
  e: CXIndexOptionsCXEvalResult
): CXIndexOptionsCXEvalResultKind {.
  importc: "clang_EvalResult_getKind", dynlib: CLangLib
.}

##
##  Returns the evaluation result as integer if the
##  kind is Int.
##

proc evalResultGetAsInt*(
  e: CXIndexOptionsCXEvalResult
): cint {.importc: "clang_EvalResult_getAsInt", dynlib: CLangLib.}

##
##  Returns the evaluation result as a long long integer if the
##  kind is Int. This prevents overflows that may happen if the result is
##  returned with clang_EvalResult_getAsInt.
##

proc evalResultGetAsLongLong*(
  e: CXIndexOptionsCXEvalResult
): clonglong {.importc: "clang_EvalResult_getAsLongLong", dynlib: CLangLib.}

##
##  Returns a non-zero value if the kind is Int and the evaluation
##  result resulted in an unsigned integer.
##

proc evalResultIsUnsignedInt*(
  e: CXIndexOptionsCXEvalResult
): cuint {.importc: "clang_EvalResult_isUnsignedInt", dynlib: CLangLib.}

##
##  Returns the evaluation result as an unsigned integer if
##  the kind is Int and clang_EvalResult_isUnsignedInt is non-zero.
##

proc evalResultGetAsUnsigned*(
  e: CXIndexOptionsCXEvalResult
): culonglong {.importc: "clang_EvalResult_getAsUnsigned", dynlib: CLangLib.}

##
##  Returns the evaluation result as double if the
##  kind is double.
##

proc evalResultGetAsDouble*(
  e: CXIndexOptionsCXEvalResult
): cdouble {.importc: "clang_EvalResult_getAsDouble", dynlib: CLangLib.}

##
##  Returns the evaluation result as a constant string if the
##  kind is other than Int or float. User must not free this pointer,
##  instead call clang_EvalResult_dispose on the CXEvalResult returned
##  by clang_Cursor_Evaluate.
##

proc evalResultGetAsStr*(
  e: CXIndexOptionsCXEvalResult
): cstring {.importc: "clang_EvalResult_getAsStr", dynlib: CLangLib.}

##
##  Disposes the created Eval memory.
##

proc evalResultDispose*(
  e: CXIndexOptionsCXEvalResult
) {.importc: "clang_EvalResult_dispose", dynlib: CLangLib.}

##
##  @}
##
##  \defgroup CINDEX_HIGH Higher level API functions
##
##  @{
##

type CXIndexOptionsCXVisitorResult* {.size: sizeof(cint).} = enum
  CXVisitBreak
  CXVisitContinue

type
  ##
  ##  Function returned successfully.
  ##
  CXIndexOptionsCXCursorAndRangeVisitor* {.bycopy.} = object
    context*: pointer
    visit*: proc(
      context: pointer, a2: CXIndexOptionsCXCursor, a3: CXSourceRange
    ): CXIndexOptionsCXVisitorResult

  CXIndexOptionsCXResult* {.size: sizeof(cint).} = enum
    CXResultSuccess = 0
      ##
      ##  One of the parameters was invalid for the function.
      ##
    CXResultInvalid = 1
      ##
      ##  The function was terminated by a callback (e.g. it returned
      ##  CXVisit_Break)
      ##
    CXResultVisitBreak = 2

##
##  Find references of a declaration in a specific file.
##
##  \param cursor pointing to a declaration or a reference of one.
##
##  \param file to search for references.
##
##  \param visitor callback that will receive pairs of CXCursor/CXSourceRange for
##  each reference found.
##  The CXSourceRange will point inside the file; if the reference is inside
##  a macro (and not a macro argument) the CXSourceRange will be invalid.
##
##  \returns one of the CXResult enumerators.
##

proc findReferencesInFile*(
  cursor: CXIndexOptionsCXCursor,
  file: CXFile,
  visitor: CXIndexOptionsCXCursorAndRangeVisitor,
): CXIndexOptionsCXResult {.importc: "clang_findReferencesInFile", dynlib: CLangLib.}

##
##  Find #import/#include directives in a specific file.
##
##  \param TU translation unit containing the file to query.
##
##  \param file to search for #import/#include directives.
##
##  \param visitor callback that will receive pairs of CXCursor/CXSourceRange for
##  each directive found.
##
##  \returns one of the CXResult enumerators.
##

proc findIncludesInFile*(
  tu: CXTranslationUnit, file: CXFile, visitor: CXIndexOptionsCXCursorAndRangeVisitor
): CXIndexOptionsCXResult {.importc: "clang_findIncludesInFile", dynlib: CLangLib.}

# when hasFeature(blocks):
#   type CXIndexOptionsCXCursorAndRangeVisitorBlock* = proc(
#     a1: CXIndexOptionsCXCursor, a2: CXSourceRange
#   ): CXIndexOptionsCXVisitorResult {.cblock.}

# else:
#   type CXIndexOptionsCXCursorAndRangeVisitorBlock* = ptr cXCursorAndRangeVisitorBlock
# proc findReferencesInFileWithBlock*(
#   a1: CXIndexOptionsCXCursor, a2: CXFile, a3: CXIndexOptionsCXCursorAndRangeVisitorBlock
# ): CXIndexOptionsCXResult {.
#   importc: "clang_findReferencesInFileWithBlock", dynlib: CLangLib
# .}

# proc findIncludesInFileWithBlock*(
#   a1: CXTranslationUnit, a2: CXFile, a3: CXIndexOptionsCXCursorAndRangeVisitorBlock
# ): CXIndexOptionsCXResult {.
#   importc: "clang_findIncludesInFileWithBlock", dynlib: CLangLib
# .}

##
##  The client's data object that is associated with a CXFile.
##

type CXIndexOptionsCXIdxClientFile* = pointer

##
##  The client's data object that is associated with a semantic entity.
##

type CXIndexOptionsCXIdxClientEntity* = pointer

##
##  The client's data object that is associated with a semantic container
##  of entities.
##

type CXIndexOptionsCXIdxClientContainer* = pointer

##
##  The client's data object that is associated with an AST file (PCH
##  or module).
##

type CXIndexOptionsCXIdxClientASTFile* = pointer

##
##  Source location passed to index callbacks.
##

type CXIndexOptionsCXIdxLoc* {.bycopy.} = object
  ptrData*: array[2, pointer]
  intData*: cuint

##
##  Data for ppIncludedFile callback.
##

type CXIndexOptionsCXIdxIncludedFileInfo* {.bycopy.} = object
  ##
  ##  Location of '#' in the \#include/\#import directive.
  ##
  hashLoc*: CXIndexOptionsCXIdxLoc
  ##
  ##  Filename as written in the \#include/\#import directive.
  ##
  filename*: cstring
  ##
  ##  The actual file that the \#include/\#import directive resolved to.
  ##
  file*: CXFile
  isImport*: cint
  isAngled*: cint
  ##
  ##  Non-zero if the directive was automatically turned into a module
  ##  import.
  ##
  isModuleImport*: cint

##
##  Data for IndexerCallbacks#importedASTFile.
##

type
  CXIndexOptionsCXIdxImportedASTFileInfo* {.bycopy.} = object
    ##
    ##  Top level AST file containing the imported PCH, module or submodule.
    ##
    file*: CXFile
    ##
    ##  The imported module or NULL if the AST file is a PCH.
    ##
    module*: CXIndexOptionsCXModule
    ##
    ##  Location where the file is imported. Applicable only for modules.
    ##
    loc*: CXIndexOptionsCXIdxLoc
    ##
    ##  Non-zero if an inclusion directive was automatically turned into
    ##  a module import. Applicable only for modules.
    ##
    isImplicit*: cint

  CXIndexOptionsCXIdxEntityKind* {.size: sizeof(cint).} = enum
    CXIdxEntityUnexposed = 0
    CXIdxEntityTypedef = 1
    CXIdxEntityFunction = 2
    CXIdxEntityVariable = 3
    CXIdxEntityField = 4
    CXIdxEntityEnumConstant = 5
    CXIdxEntityObjCClass = 6
    CXIdxEntityObjCProtocol = 7
    CXIdxEntityObjCCategory = 8
    CXIdxEntityObjCInstanceMethod = 9
    CXIdxEntityObjCClassMethod = 10
    CXIdxEntityObjCProperty = 11
    CXIdxEntityObjCIvar = 12
    CXIdxEntityEnum = 13
    CXIdxEntityStruct = 14
    CXIdxEntityUnion = 15
    CXIdxEntityCXXClass = 16
    CXIdxEntityCXXNamespace = 17
    CXIdxEntityCXXNamespaceAlias = 18
    CXIdxEntityCXXStaticVariable = 19
    CXIdxEntityCXXStaticMethod = 20
    CXIdxEntityCXXInstanceMethod = 21
    CXIdxEntityCXXConstructor = 22
    CXIdxEntityCXXDestructor = 23
    CXIdxEntityCXXConversionFunction = 24
    CXIdxEntityCXXTypeAlias = 25
    CXIdxEntityCXXInterface = 26
    CXIdxEntityCXXConcept = 27

  CXIndexOptionsCXIdxEntityLanguage* {.size: sizeof(cint).} = enum
    CXIdxEntityLangNone = 0
    CXIdxEntityLangC = 1
    CXIdxEntityLangObjC = 2
    CXIdxEntityLangCXX = 3
    CXIdxEntityLangSwift = 4

##
##  Extra C++ template information for an entity. This can apply to:
##  CXIdxEntity_Function
##  CXIdxEntity_CXXClass
##  CXIdxEntity_CXXStaticMethod
##  CXIdxEntity_CXXInstanceMethod
##  CXIdxEntity_CXXConstructor
##  CXIdxEntity_CXXConversionFunction
##  CXIdxEntity_CXXTypeAlias
##

type
  CXIndexOptionsCXIdxEntityCXXTemplateKind* {.size: sizeof(cint).} = enum
    CXIdxEntityNonTemplate = 0
    CXIdxEntityTemplate = 1
    CXIdxEntityTemplatePartialSpecialization = 2
    CXIdxEntityTemplateSpecialization = 3

  CXIndexOptionsCXIdxAttrKind* {.size: sizeof(cint).} = enum
    CXIdxAttrUnexposed = 0
    CXIdxAttrIBAction = 1
    CXIdxAttrIBOutlet = 2
    CXIdxAttrIBOutletCollection = 3

  CXIndexOptionsCXIdxAttrInfo* {.bycopy.} = object
    kind*: CXIndexOptionsCXIdxAttrKind
    cursor*: CXIndexOptionsCXCursor
    loc*: CXIndexOptionsCXIdxLoc

  CXIndexOptionsCXIdxEntityInfo* {.bycopy.} = object
    kind*: CXIndexOptionsCXIdxEntityKind
    templateKind*: CXIndexOptionsCXIdxEntityCXXTemplateKind
    lang*: CXIndexOptionsCXIdxEntityLanguage
    name*: cstring
    usr*: cstring
    cursor*: CXIndexOptionsCXCursor
    attributes*: ptr ptr CXIndexOptionsCXIdxAttrInfo
    numAttributes*: cuint

  CXIndexOptionsCXIdxContainerInfo* {.bycopy.} = object
    cursor*: CXIndexOptionsCXCursor

  CXIndexOptionsCXIdxIBOutletCollectionAttrInfo* {.bycopy.} = object
    attrInfo*: ptr CXIndexOptionsCXIdxAttrInfo
    objcClass*: ptr CXIndexOptionsCXIdxEntityInfo
    classCursor*: CXIndexOptionsCXCursor
    classLoc*: CXIndexOptionsCXIdxLoc

  CXIndexOptionsCXIdxDeclInfoFlags* {.size: sizeof(cint).} = enum
    CXIdxDeclFlagSkipped = 0x1

  CXIndexOptionsCXIdxDeclInfo* {.bycopy.} = object
    entityInfo*: ptr CXIndexOptionsCXIdxEntityInfo
    cursor*: CXIndexOptionsCXCursor
    loc*: CXIndexOptionsCXIdxLoc
    semanticContainer*: ptr CXIndexOptionsCXIdxContainerInfo
    ##
    ##  Generally same as #semanticContainer but can be different in
    ##  cases like out-of-line C++ member functions.
    ##
    lexicalContainer*: ptr CXIndexOptionsCXIdxContainerInfo
    isRedeclaration*: cint
    isDefinition*: cint
    isContainer*: cint
    declAsContainer*: ptr CXIndexOptionsCXIdxContainerInfo
    ##
    ##  Whether the declaration exists in code or was created implicitly
    ##  by the compiler, e.g. implicit Objective-C methods for properties.
    ##
    isImplicit*: cint
    attributes*: ptr ptr CXIndexOptionsCXIdxAttrInfo
    numAttributes*: cuint
    flags*: cuint

  CXIndexOptionsCXIdxObjCContainerKind* {.size: sizeof(cint).} = enum
    CXIdxObjCContainerForwardRef = 0
    CXIdxObjCContainerInterface = 1
    CXIdxObjCContainerImplementation = 2

  CXIndexOptionsCXIdxObjCContainerDeclInfo* {.bycopy.} = object
    declInfo*: ptr CXIndexOptionsCXIdxDeclInfo
    kind*: CXIndexOptionsCXIdxObjCContainerKind

  CXIndexOptionsCXIdxBaseClassInfo* {.bycopy.} = object
    base*: ptr CXIndexOptionsCXIdxEntityInfo
    cursor*: CXIndexOptionsCXCursor
    loc*: CXIndexOptionsCXIdxLoc

  CXIndexOptionsCXIdxObjCProtocolRefInfo* {.bycopy.} = object
    protocol*: ptr CXIndexOptionsCXIdxEntityInfo
    cursor*: CXIndexOptionsCXCursor
    loc*: CXIndexOptionsCXIdxLoc

  CXIndexOptionsCXIdxObjCProtocolRefListInfo* {.bycopy.} = object
    protocols*: ptr ptr CXIndexOptionsCXIdxObjCProtocolRefInfo
    numProtocols*: cuint

  CXIndexOptionsCXIdxObjCInterfaceDeclInfo* {.bycopy.} = object
    containerInfo*: ptr CXIndexOptionsCXIdxObjCContainerDeclInfo
    superInfo*: ptr CXIndexOptionsCXIdxBaseClassInfo
    protocols*: ptr CXIndexOptionsCXIdxObjCProtocolRefListInfo

  CXIndexOptionsCXIdxObjCCategoryDeclInfo* {.bycopy.} = object
    containerInfo*: ptr CXIndexOptionsCXIdxObjCContainerDeclInfo
    objcClass*: ptr CXIndexOptionsCXIdxEntityInfo
    classCursor*: CXIndexOptionsCXCursor
    classLoc*: CXIndexOptionsCXIdxLoc
    protocols*: ptr CXIndexOptionsCXIdxObjCProtocolRefListInfo

  CXIndexOptionsCXIdxObjCPropertyDeclInfo* {.bycopy.} = object
    declInfo*: ptr CXIndexOptionsCXIdxDeclInfo
    getter*: ptr CXIndexOptionsCXIdxEntityInfo
    setter*: ptr CXIndexOptionsCXIdxEntityInfo

  CXIndexOptionsCXIdxCXXClassDeclInfo* {.bycopy.} = object
    declInfo*: ptr CXIndexOptionsCXIdxDeclInfo
    bases*: ptr ptr CXIndexOptionsCXIdxBaseClassInfo
    numBases*: cuint

##
##  Data for IndexerCallbacks#indexEntityReference.
##
##  This may be deprecated in a future version as this duplicates
##  the \c CXSymbolRole_Implicit bit in \c CXSymbolRole.
##

type
  ##
  ##  The entity is referenced directly in user's code.
  ##
  CXIndexOptionsCXIdxEntityRefKind* {.size: sizeof(cint).} = enum
    CXIdxEntityRefDirect = 1
      ##
      ##  An implicit reference, e.g. a reference of an Objective-C method
      ##  via the dot syntax.
      ##
    CXIdxEntityRefImplicit = 2

##
##  Roles that are attributed to symbol occurrences.
##
##  Internal: this currently mirrors low 9 bits of clang::index::SymbolRole with
##  higher bits zeroed. These high bits may be exposed in the future.
##

type CXIndexOptionsCXSymbolRole* {.size: sizeof(cint).} = enum
  CXSymbolRoleNone = 0
  CXSymbolRoleDeclaration = 1 shl 0
  CXSymbolRoleDefinition = 1 shl 1
  CXSymbolRoleReference = 1 shl 2
  CXSymbolRoleRead = 1 shl 3
  CXSymbolRoleWrite = 1 shl 4
  CXSymbolRoleCall = 1 shl 5
  CXSymbolRoleDynamic = 1 shl 6
  CXSymbolRoleAddressOf = 1 shl 7
  CXSymbolRoleImplicit = 1 shl 8

##
##  Data for IndexerCallbacks#indexEntityReference.
##

type CXIndexOptionsCXIdxEntityRefInfo* {.bycopy.} = object
  kind*: CXIndexOptionsCXIdxEntityRefKind
  ##
  ##  Reference cursor.
  ##
  cursor*: CXIndexOptionsCXCursor
  loc*: CXIndexOptionsCXIdxLoc
  ##
  ##  The entity that gets referenced.
  ##
  referencedEntity*: ptr CXIndexOptionsCXIdxEntityInfo
  ##
  ##  Immediate "parent" of the reference. For example:
  ##
  ##  \code
  ##  Foo *var;
  ##  \endcode
  ##
  ##  The parent of reference of type 'Foo' is the variable 'var'.
  ##  For references inside statement bodies of functions/methods,
  ##  the parentEntity will be the function/method.
  ##
  parentEntity*: ptr CXIndexOptionsCXIdxEntityInfo
  ##
  ##  Lexical container context of the reference.
  ##
  container*: ptr CXIndexOptionsCXIdxContainerInfo
  ##
  ##  Sets of symbol roles of the reference.
  ##
  role*: CXIndexOptionsCXSymbolRole

##
##  A group of callbacks used by #clang_indexSourceFile and
##  #clang_indexTranslationUnit.
##

type CXIndexOptionsIndexerCallbacks* {.bycopy.} = object
  ##
  ##  Called periodically to check whether indexing should be aborted.
  ##  Should return 0 to continue, and non-zero to abort.
  ##
  abortQuery*: proc(clientData: CXClientData, reserved: pointer): cint
  ##
  ##  Called at the end of indexing; passes the complete diagnostic set.
  ##
  diagnostic*: proc(clientData: CXClientData, a2: CXDiagnosticSet, reserved: pointer)
  enteredMainFile*: proc(
    clientData: CXClientData, mainFile: CXFile, reserved: pointer
  ): CXIndexOptionsCXIdxClientFile
  ##
  ##  Called when a file gets \#included/\#imported.
  ##
  ppIncludedFile*: proc(
    clientData: CXClientData, a2: ptr CXIndexOptionsCXIdxIncludedFileInfo
  ): CXIndexOptionsCXIdxClientFile
  ##
  ##  Called when a AST file (PCH or module) gets imported.
  ##
  ##  AST files will not get indexed (there will not be callbacks to index all
  ##  the entities in an AST file). The recommended action is that, if the AST
  ##  file is not already indexed, to initiate a new indexing job specific to
  ##  the AST file.
  ##
  importedASTFile*: proc(
    clientData: CXClientData, a2: ptr CXIndexOptionsCXIdxImportedASTFileInfo
  ): CXIndexOptionsCXIdxClientASTFile
  ##
  ##  Called at the beginning of indexing a translation unit.
  ##
  startedTranslationUnit*: proc(
    clientData: CXClientData, reserved: pointer
  ): CXIndexOptionsCXIdxClientContainer
  indexDeclaration*: proc(clientData: CXClientData, a2: ptr CXIndexOptionsCXIdxDeclInfo)
  ##
  ##  Called to index a reference of an entity.
  ##
  indexEntityReference*:
    proc(clientData: CXClientData, a2: ptr CXIndexOptionsCXIdxEntityRefInfo)

proc indexIsEntityObjCContainerKind*(
  a1: CXIndexOptionsCXIdxEntityKind
): cint {.importc: "clang_index_isEntityObjCContainerKind", dynlib: CLangLib.}

proc indexGetObjCContainerDeclInfo*(
  a1: ptr CXIndexOptionsCXIdxDeclInfo
): ptr CXIndexOptionsCXIdxObjCContainerDeclInfo {.
  importc: "clang_index_getObjCContainerDeclInfo", dynlib: CLangLib
.}

proc indexGetObjCInterfaceDeclInfo*(
  a1: ptr CXIndexOptionsCXIdxDeclInfo
): ptr CXIndexOptionsCXIdxObjCInterfaceDeclInfo {.
  importc: "clang_index_getObjCInterfaceDeclInfo", dynlib: CLangLib
.}

proc indexGetObjCCategoryDeclInfo*(
  a1: ptr CXIndexOptionsCXIdxDeclInfo
): ptr CXIndexOptionsCXIdxObjCCategoryDeclInfo {.
  importc: "clang_index_getObjCCategoryDeclInfo", dynlib: CLangLib
.}

proc indexGetObjCProtocolRefListInfo*(
  a1: ptr CXIndexOptionsCXIdxDeclInfo
): ptr CXIndexOptionsCXIdxObjCProtocolRefListInfo {.
  importc: "clang_index_getObjCProtocolRefListInfo", dynlib: CLangLib
.}

proc indexGetObjCPropertyDeclInfo*(
  a1: ptr CXIndexOptionsCXIdxDeclInfo
): ptr CXIndexOptionsCXIdxObjCPropertyDeclInfo {.
  importc: "clang_index_getObjCPropertyDeclInfo", dynlib: CLangLib
.}

proc indexGetIBOutletCollectionAttrInfo*(
  a1: ptr CXIndexOptionsCXIdxAttrInfo
): ptr CXIndexOptionsCXIdxIBOutletCollectionAttrInfo {.
  importc: "clang_index_getIBOutletCollectionAttrInfo", dynlib: CLangLib
.}

proc indexGetCXXClassDeclInfo*(
  a1: ptr CXIndexOptionsCXIdxDeclInfo
): ptr CXIndexOptionsCXIdxCXXClassDeclInfo {.
  importc: "clang_index_getCXXClassDeclInfo", dynlib: CLangLib
.}

##
##  For retrieving a custom CXIdxClientContainer attached to a
##  container.
##

proc indexGetClientContainer*(
  a1: ptr CXIndexOptionsCXIdxContainerInfo
): CXIndexOptionsCXIdxClientContainer {.
  importc: "clang_index_getClientContainer", dynlib: CLangLib
.}

##
##  For setting a custom CXIdxClientContainer attached to a
##  container.
##

proc indexSetClientContainer*(
  a1: ptr CXIndexOptionsCXIdxContainerInfo, a2: CXIndexOptionsCXIdxClientContainer
) {.importc: "clang_index_setClientContainer", dynlib: CLangLib.}

##
##  For retrieving a custom CXIdxClientEntity attached to an entity.
##

proc indexGetClientEntity*(
  a1: ptr CXIndexOptionsCXIdxEntityInfo
): CXIndexOptionsCXIdxClientEntity {.
  importc: "clang_index_getClientEntity", dynlib: CLangLib
.}

##
##  For setting a custom CXIdxClientEntity attached to an entity.
##

proc indexSetClientEntity*(
  a1: ptr CXIndexOptionsCXIdxEntityInfo, a2: CXIndexOptionsCXIdxClientEntity
) {.importc: "clang_index_setClientEntity", dynlib: CLangLib.}

##
##  An indexing action/session, to be applied to one or multiple
##  translation units.
##

type CXIndexOptionsCXIndexAction* = pointer

##
##  An indexing action/session, to be applied to one or multiple
##  translation units.
##
##  \param CIdx The index object with which the index action will be associated.
##

proc indexActionCreate*(
  cIdx: CXIndex
): CXIndexOptionsCXIndexAction {.importc: "clang_IndexAction_create", dynlib: CLangLib.}

##
##  Destroy the given index action.
##
##  The index action must not be destroyed until all of the translation units
##  created within that index action have been destroyed.
##

proc indexActionDispose*(
  a1: CXIndexOptionsCXIndexAction
) {.importc: "clang_IndexAction_dispose", dynlib: CLangLib.}

type
  ##
  ##  Used to indicate that no special indexing options are needed.
  ##
  CXIndexOptionsCXIndexOptFlags* {.size: sizeof(cint).} = enum
    CXIndexOptNone = 0x0
      ##
      ##  Used to indicate that IndexerCallbacks#indexEntityReference should
      ##  be invoked for only one reference of an entity per source file that does
      ##  not also include a declaration/definition of the entity.
      ##
    CXIndexOptSuppressRedundantRefs = 0x1
      ##
      ##  Function-local symbols should be indexed. If this is not set
      ##  function-local symbols will be ignored.
      ##
    CXIndexOptIndexFunctionLocalSymbols = 0x2
      ##
      ##  Implicit function/class template instantiations should be indexed.
      ##  If this is not set, implicit instantiations will be ignored.
      ##
    CXIndexOptIndexImplicitTemplateInstantiations = 0x4
      ##
      ##  Suppress all compiler warnings when parsing for indexing.
      ##
    CXIndexOptSuppressWarnings = 0x8
      ##
      ##  Skip a function/method body that was already parsed during an
      ##  indexing session associated with a \c CXIndexAction object.
      ##  Bodies in system headers are always skipped.
      ##
    CXIndexOptSkipParsedBodiesInSession = 0x10

##
##  Index the given source file and the translation unit corresponding
##  to that file via callbacks implemented through #IndexerCallbacks.
##
##  \param client_data pointer data supplied by the client, which will
##  be passed to the invoked callbacks.
##
##  \param index_callbacks Pointer to indexing callbacks that the client
##  implements.
##
##  \param index_callbacks_size Size of #IndexerCallbacks structure that gets
##  passed in index_callbacks.
##
##  \param index_options A bitmask of options that affects how indexing is
##  performed. This should be a bitwise OR of the CXIndexOpt_XXX flags.
##
##  \param[out] out_TU pointer to store a \c CXTranslationUnit that can be
##  reused after indexing is finished. Set to \c NULL if you do not require it.
##
##  \returns 0 on success or if there were errors from which the compiler could
##  recover.  If there is a failure from which there is no recovery, returns
##  a non-zero \c CXErrorCode.
##
##  The rest of the parameters are the same as #clang_parseTranslationUnit.
##

proc indexSourceFile*(
  a1: CXIndexOptionsCXIndexAction,
  clientData: CXClientData,
  indexCallbacks: ptr CXIndexOptionsIndexerCallbacks,
  indexCallbacksSize: cuint,
  indexOptions: cuint,
  sourceFilename: cstring,
  commandLineArgs: cstringArray,
  numCommandLineArgs: cint,
  unsavedFiles: ptr CXUnsavedFile,
  numUnsavedFiles: cuint,
  outTU: ptr CXTranslationUnit,
  tU_options: cuint,
): cint {.importc: "clang_indexSourceFile", dynlib: CLangLib.}

##
##  Same as clang_indexSourceFile but requires a full command line
##  for \c command_line_args including argv[0]. This is useful if the standard
##  library paths are relative to the binary.
##

proc indexSourceFileFullArgv*(
  a1: CXIndexOptionsCXIndexAction,
  clientData: CXClientData,
  indexCallbacks: ptr CXIndexOptionsIndexerCallbacks,
  indexCallbacksSize: cuint,
  indexOptions: cuint,
  sourceFilename: cstring,
  commandLineArgs: cstringArray,
  numCommandLineArgs: cint,
  unsavedFiles: ptr CXUnsavedFile,
  numUnsavedFiles: cuint,
  outTU: ptr CXTranslationUnit,
  tU_options: cuint,
): cint {.importc: "clang_indexSourceFileFullArgv", dynlib: CLangLib.}

##
##  Index the given translation unit via callbacks implemented through
##  #IndexerCallbacks.
##
##  The order of callback invocations is not guaranteed to be the same as
##  when indexing a source file. The high level order will be:
##
##    -Preprocessor callbacks invocations
##    -Declaration/reference callbacks invocations
##    -Diagnostic callback invocations
##
##  The parameters are the same as #clang_indexSourceFile.
##
##  \returns If there is a failure from which there is no recovery, returns
##  non-zero, otherwise returns 0.
##

proc indexTranslationUnit*(
  a1: CXIndexOptionsCXIndexAction,
  clientData: CXClientData,
  indexCallbacks: ptr CXIndexOptionsIndexerCallbacks,
  indexCallbacksSize: cuint,
  indexOptions: cuint,
  a6: CXTranslationUnit,
): cint {.importc: "clang_indexTranslationUnit", dynlib: CLangLib.}

##
##  Retrieve the CXIdxFile, file, line, column, and offset represented by
##  the given CXIdxLoc.
##
##  If the location refers into a macro expansion, retrieves the
##  location of the macro expansion and if it refers into a macro argument
##  retrieves the location of the argument.
##

proc indexLocGetFileLocation*(
  loc: CXIndexOptionsCXIdxLoc,
  indexFile: ptr CXIndexOptionsCXIdxClientFile,
  file: ptr CXFile,
  line: ptr cuint,
  column: ptr cuint,
  offset: ptr cuint,
) {.importc: "clang_indexLoc_getFileLocation", dynlib: CLangLib.}

##
##  Retrieve the CXSourceLocation represented by the given CXIdxLoc.
##

proc indexLocGetCXSourceLocation*(
  loc: CXIndexOptionsCXIdxLoc
): CXSourceLocation {.importc: "clang_indexLoc_getCXSourceLocation", dynlib: CLangLib.}

##
##  Visitor invoked for each field found by a traversal.
##
##  This visitor function will be invoked for each field found by
##  \c clang_Type_visitFields. Its first argument is the cursor being
##  visited, its second argument is the client data provided to
##  \c clang_Type_visitFields.
##
##  The visitor should return one of the \c CXVisitorResult values
##  to direct \c clang_Type_visitFields.
##

type CXIndexOptionsCXFieldVisitor* = proc(
  c: CXIndexOptionsCXCursor, clientData: CXClientData
): CXIndexOptionsCXVisitorResult

##
##  Visit the fields of a particular type.
##
##  This function visits all the direct fields of the given cursor,
##  invoking the given \p visitor function with the cursors of each
##  visited field. The traversal may be ended prematurely, if
##  the visitor returns \c CXFieldVisit_Break.
##
##  \param T the record type whose field may be visited.
##
##  \param visitor the visitor function that will be invoked for each
##  field of \p T.
##
##  \param client_data pointer data supplied by the client, which will
##  be passed to the visitor each time it is invoked.
##
##  \returns a non-zero value if the traversal was terminated
##  prematurely by the visitor returning \c CXFieldVisit_Break.
##

proc typeVisitFields*(
  t: CXIndexOptionsCXType,
  visitor: CXIndexOptionsCXFieldVisitor,
  clientData: CXClientData,
): cuint {.importc: "clang_Type_visitFields", dynlib: CLangLib.}

##
##  Visit the base classes of a type.
##
##  This function visits all the direct base classes of a the given cursor,
##  invoking the given \p visitor function with the cursors of each
##  visited base. The traversal may be ended prematurely, if
##  the visitor returns \c CXFieldVisit_Break.
##
##  \param T the record type whose field may be visited.
##
##  \param visitor the visitor function that will be invoked for each
##  field of \p T.
##
##  \param client_data pointer data supplied by the client, which will
##  be passed to the visitor each time it is invoked.
##
##  \returns a non-zero value if the traversal was terminated
##  prematurely by the visitor returning \c CXFieldVisit_Break.
##

proc visitCXXBaseClasses*(
  t: CXIndexOptionsCXType,
  visitor: CXIndexOptionsCXFieldVisitor,
  clientData: CXClientData,
): cuint {.importc: "clang_visitCXXBaseClasses", dynlib: CLangLib.}

##
##  Visit the class methods of a type.
##
##  This function visits all the methods of the given cursor,
##  invoking the given \p visitor function with the cursors of each
##  visited method. The traversal may be ended prematurely, if
##  the visitor returns \c CXFieldVisit_Break.
##
##  \param T The record type whose field may be visited.
##
##  \param visitor The visitor function that will be invoked for each
##  field of \p T.
##
##  \param client_data Pointer data supplied by the client, which will
##  be passed to the visitor each time it is invoked.
##
##  \returns A non-zero value if the traversal was terminated
##  prematurely by the visitor returning \c CXFieldVisit_Break.
##

proc visitCXXMethods*(
  t: CXIndexOptionsCXType,
  visitor: CXIndexOptionsCXFieldVisitor,
  clientData: CXClientData,
): cuint {.importc: "clang_visitCXXMethods", dynlib: CLangLib.}

##
##  Describes the kind of binary operators.
##

type CXIndexOptionsCXBinaryOperatorKind* {.size: sizeof(cint).} = enum
  ##  This value describes cursors which are not binary operators.
  CXBinaryOperatorInvalid = 0 ##  C++ Pointer - to - member operator.
  CXBinaryOperatorPtrMemD = 1 ##  C++ Pointer - to - member operator.
  CXBinaryOperatorPtrMemI = 2 ##  Multiplication operator.
  CXBinaryOperatorMul = 3 ##  Division operator.
  CXBinaryOperatorDiv = 4 ##  Remainder operator.
  CXBinaryOperatorRem = 5 ##  Addition operator.
  CXBinaryOperatorAdd = 6 ##  Subtraction operator.
  CXBinaryOperatorSub = 7 ##  Bitwise shift left operator.
  CXBinaryOperatorShl = 8 ##  Bitwise shift right operator.
  CXBinaryOperatorShr = 9 ##  C++ three-way comparison (spaceship) operator.
  CXBinaryOperatorCmp = 10 ##  Less than operator.
  CXBinaryOperatorLT = 11 ##  Greater than operator.
  CXBinaryOperatorGT = 12 ##  Less or equal operator.
  CXBinaryOperatorLE = 13 ##  Greater or equal operator.
  CXBinaryOperatorGE = 14 ##  Equal operator.
  CXBinaryOperatorEQ = 15 ##  Not equal operator.
  CXBinaryOperatorNE = 16 ##  Bitwise AND operator.
  CXBinaryOperatorAnd = 17 ##  Bitwise XOR operator.
  CXBinaryOperatorXor = 18 ##  Bitwise OR operator.
  CXBinaryOperatorOr = 19 ##  Logical AND operator.
  CXBinaryOperatorLAnd = 20 ##  Logical OR operator.
  CXBinaryOperatorLOr = 21 ##  Assignment operator.
  CXBinaryOperatorAssign = 22 ##  Multiplication assignment operator.
  CXBinaryOperatorMulAssign = 23 ##  Division assignment operator.
  CXBinaryOperatorDivAssign = 24 ##  Remainder assignment operator.
  CXBinaryOperatorRemAssign = 25 ##  Addition assignment operator.
  CXBinaryOperatorAddAssign = 26 ##  Subtraction assignment operator.
  CXBinaryOperatorSubAssign = 27 ##  Bitwise shift left assignment operator.
  CXBinaryOperatorShlAssign = 28 ##  Bitwise shift right assignment operator.
  CXBinaryOperatorShrAssign = 29 ##  Bitwise AND assignment operator.
  CXBinaryOperatorAndAssign = 30 ##  Bitwise XOR assignment operator.
  CXBinaryOperatorXorAssign = 31 ##  Bitwise OR assignment operator.
  CXBinaryOperatorOrAssign = 32 ##  Comma operator.
  CXBinaryOperatorComma = 33 # CXBinaryOperatorLast = cXBinaryOperatorComma

##
##  Retrieve the spelling of a given CXBinaryOperatorKind.
##

proc getBinaryOperatorKindSpelling*(
  kind: CXIndexOptionsCXBinaryOperatorKind
): CXString {.importc: "clang_getBinaryOperatorKindSpelling", dynlib: CLangLib.}

##
##  Retrieve the binary operator kind of this cursor.
##
##  If this cursor is not a binary operator then returns Invalid.
##

proc getCursorBinaryOperatorKind*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXBinaryOperatorKind {.
  importc: "clang_getCursorBinaryOperatorKind", dynlib: CLangLib
.}

##
##  Describes the kind of unary operators.
##

type CXIndexOptionsCXUnaryOperatorKind* {.size: sizeof(cint).} = enum
  ##  This value describes cursors which are not unary operators.
  CXUnaryOperatorInvalid ##  Postfix increment operator.
  CXUnaryOperatorPostInc ##  Postfix decrement operator.
  CXUnaryOperatorPostDec ##  Prefix increment operator.
  CXUnaryOperatorPreInc ##  Prefix decrement operator.
  CXUnaryOperatorPreDec ##  Address of operator.
  CXUnaryOperatorAddrOf ##  Dereference operator.
  CXUnaryOperatorDeref ##  Plus operator.
  CXUnaryOperatorPlus ##  Minus operator.
  CXUnaryOperatorMinus ##  Not operator.
  CXUnaryOperatorNot ##  LNot operator.
  CXUnaryOperatorLNot ##  "__real expr" operator.
  CXUnaryOperatorReal ##  "__imag expr" operator.
  CXUnaryOperatorImag ##  __extension__ marker operator.
  CXUnaryOperatorExtension ##  C++ co_await operator.
  CXUnaryOperatorCoawait

##
##  Retrieve the spelling of a given CXUnaryOperatorKind.
##

proc getUnaryOperatorKindSpelling*(
  kind: CXIndexOptionsCXUnaryOperatorKind
): CXString {.importc: "clang_getUnaryOperatorKindSpelling", dynlib: CLangLib.}

##
##  Retrieve the unary operator kind of this cursor.
##
##  If this cursor is not a unary operator then returns Invalid.
##

proc getCursorUnaryOperatorKind*(
  cursor: CXIndexOptionsCXCursor
): CXIndexOptionsCXUnaryOperatorKind {.
  importc: "clang_getCursorUnaryOperatorKind", dynlib: CLangLib
.}

##
##  @}
##
##
##  @}
##
##  CINDEX_DEPRECATED - disabled to silence MSVC deprecation warnings

type CXIndexOptionsCXRemapping* = pointer

proc getRemappings*(
  a1: cstring
): CXIndexOptionsCXRemapping {.importc: "clang_getRemappings", dynlib: CLangLib.}

proc getRemappingsFromFileList*(
  a1: cstringArray, a2: cuint
): CXIndexOptionsCXRemapping {.
  importc: "clang_getRemappingsFromFileList", dynlib: CLangLib
.}

proc remapGetNumFiles*(
  a1: CXIndexOptionsCXRemapping
): cuint {.importc: "clang_remap_getNumFiles", dynlib: CLangLib.}

proc remapGetFilenames*(
  a1: CXIndexOptionsCXRemapping, a2: cuint, a3: ptr CXString, a4: ptr CXString
) {.importc: "clang_remap_getFilenames", dynlib: CLangLib.}

proc remapDispose*(
  a1: CXIndexOptionsCXRemapping
) {.importc: "clang_remap_dispose", dynlib: CLangLib.}
