## ===-- clang-c/CXCompilationDatabase.h - Compilation database  ---*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header provides a public interface to use CompilationDatabase without *|
## |* the full Clang C++ API.                                                    *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##  \defgroup COMPILATIONDB CompilationDatabase functions
##  \ingroup CINDEX
##
##  @{
##
##
##  A compilation database holds all information used to compile files in a
##  project. For each file in the database, it can be queried for the working
##  directory or the command line used for the compiler invocation.
##
##  Must be freed by \c clang_CompilationDatabase_dispose
##

type CXCompilationDatabase* = pointer

##
##  Contains the results of a search in the compilation database
##
##  When searching for the compile command for a file, the compilation db can
##  return several commands, as the file may have been compiled with
##  different options in different places of the project. This choice of compile
##  commands is wrapped in this opaque data structure. It must be freed by
##  \c clang_CompileCommands_dispose.
##

type CXCompileCommands* = pointer

##
##  Represents the command line invocation to compile a specific file.
##

type CXCompileCommand* = pointer

##
##  Error codes for Compilation Database
##

type
  ##
  ##  No error occurred
  ##
  CXCompilationDatabaseError* {.size: sizeof(cint).} = enum
    CXCompilationDatabaseNoError = 0
      ##
      ##  Database can not be loaded
      ##
    CXCompilationDatabaseCanNotLoadDatabase = 1

##
##  Creates a compilation database from the database found in directory
##  buildDir. For example, CMake can output a compile_commands.json which can
##  be used to build the database.
##
##  It must be freed by \c clang_CompilationDatabase_dispose.
##

proc compilationDatabaseFromDirectory*(
  buildDir: cstring, errorCode: ptr CXCompilationDatabaseError
): CXCompilationDatabase {.
  importc: "clang_CompilationDatabase_fromDirectory", dynlib: CLangLib
.}

##
##  Free the given compilation database
##

proc compilationDatabaseDispose*(
  a1: CXCompilationDatabase
) {.importc: "clang_CompilationDatabase_dispose", dynlib: CLangLib.}

##
##  Find the compile commands used for a file. The compile commands
##  must be freed by \c clang_CompileCommands_dispose.
##

proc compilationDatabaseGetCompileCommands*(
  a1: CXCompilationDatabase, completeFileName: cstring
): CXCompileCommands {.
  importc: "clang_CompilationDatabase_getCompileCommands", dynlib: CLangLib
.}

##
##  Get all the compile commands in the given compilation database.
##

proc compilationDatabaseGetAllCompileCommands*(
  a1: CXCompilationDatabase
): CXCompileCommands {.
  importc: "clang_CompilationDatabase_getAllCompileCommands", dynlib: CLangLib
.}

##
##  Free the given CompileCommands
##

proc compileCommandsDispose*(
  a1: CXCompileCommands
) {.importc: "clang_CompileCommands_dispose", dynlib: CLangLib.}

##
##  Get the number of CompileCommand we have for a file
##

proc compileCommandsGetSize*(
  a1: CXCompileCommands
): cuint {.importc: "clang_CompileCommands_getSize", dynlib: CLangLib.}

##
##  Get the I'th CompileCommand for a file
##
##  Note : 0 <= i < clang_CompileCommands_getSize(CXCompileCommands)
##

proc compileCommandsGetCommand*(
  a1: CXCompileCommands, i: cuint
): CXCompileCommand {.importc: "clang_CompileCommands_getCommand", dynlib: CLangLib.}

##
##  Get the working directory where the CompileCommand was executed from
##

proc compileCommandGetDirectory*(
  a1: CXCompileCommand
): CXString {.importc: "clang_CompileCommand_getDirectory", dynlib: CLangLib.}

##
##  Get the filename associated with the CompileCommand.
##

proc compileCommandGetFilename*(
  a1: CXCompileCommand
): CXString {.importc: "clang_CompileCommand_getFilename", dynlib: CLangLib.}

##
##  Get the number of arguments in the compiler invocation.
##
##

proc compileCommandGetNumArgs*(
  a1: CXCompileCommand
): cuint {.importc: "clang_CompileCommand_getNumArgs", dynlib: CLangLib.}

##
##  Get the I'th argument value in the compiler invocations
##
##  Invariant :
##   - argument 0 is the compiler executable
##

proc compileCommandGetArg*(
  a1: CXCompileCommand, i: cuint
): CXString {.importc: "clang_CompileCommand_getArg", dynlib: CLangLib.}

##
##  Get the number of source mappings for the compiler invocation.
##

proc compileCommandGetNumMappedSources*(
  a1: CXCompileCommand
): cuint {.importc: "clang_CompileCommand_getNumMappedSources", dynlib: CLangLib.}

##
##  Get the I'th mapped source path for the compiler invocation.
##

proc compileCommandGetMappedSourcePath*(
  a1: CXCompileCommand, i: cuint
): CXString {.importc: "clang_CompileCommand_getMappedSourcePath", dynlib: CLangLib.}

##
##  Get the I'th mapped source content for the compiler invocation.
##

proc compileCommandGetMappedSourceContent*(
  a1: CXCompileCommand, i: cuint
): CXString {.importc: "clang_CompileCommand_getMappedSourceContent", dynlib: CLangLib.}

##
##  @}
##
