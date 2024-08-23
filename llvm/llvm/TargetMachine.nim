## ===-- llvm-c/TargetMachine.h - Target Machine Library C Interface - C++ -*-=*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to the Target and TargetMachine       *|
## |* classes, which can be used to generate assembly or object files.           *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  @addtogroup LLVMCTarget
##
##  @{
##

type
  TargetMachineOptionsRef* = ptr OpaqueTargetMachineOptions
  TargetMachineRef* = ptr OpaqueTargetMachine
  TargetRef* = ptr target
  CodeGenOptLevel* {.size: sizeof(cint).} = enum
    CodeGenLevelNone
    CodeGenLevelLess
    CodeGenLevelDefault
    CodeGenLevelAggressive

  RelocMode* {.size: sizeof(cint).} = enum
    RelocDefault
    RelocStatic
    RelocPIC
    RelocDynamicNoPic
    RelocROPI
    RelocRWPI
    RelocROPI_RWPI

  CodeModel* {.size: sizeof(cint).} = enum
    CodeModelDefault
    CodeModelJITDefault
    CodeModelTiny
    CodeModelSmall
    CodeModelKernel
    CodeModelMedium
    CodeModelLarge

  CodeGenFileType* {.size: sizeof(cint).} = enum
    AssemblyFile
    ObjectFile

  GlobalISelAbortMode* {.size: sizeof(cint).} = enum
    GlobalISelAbortEnable
    GlobalISelAbortDisable
    GlobalISelAbortDisableWithDiag

##  Returns the first llvm::Target in the registered targets list.

proc getFirstTarget*(): TargetRef {.importc: "LLVMGetFirstTarget", dynlib: LLVMLib.}
##  Returns the next llvm::Target given a previous one (or null if there's none)

proc getNextTarget*(
  t: TargetRef
): TargetRef {.importc: "LLVMGetNextTarget", dynlib: LLVMLib.}

## ===-- Target ------------------------------------------------------------===
##  Finds the target corresponding to the given name and stores it in \p T.
##   Returns 0 on success.

proc getTargetFromName*(
  name: cstring
): TargetRef {.importc: "LLVMGetTargetFromName", dynlib: LLVMLib.}

##  Finds the target corresponding to the given triple and stores it in \p T.
##   Returns 0 on success. Optionally returns any error in ErrorMessage.
##   Use LLVMDisposeMessage to dispose the message.

proc getTargetFromTriple*(
  triple: cstring, t: ptr TargetRef, errorMessage: cstringArray
): Bool {.importc: "LLVMGetTargetFromTriple", dynlib: LLVMLib.}

##  Returns the name of a target. See llvm::Target::getName

proc getTargetName*(
  t: TargetRef
): cstring {.importc: "LLVMGetTargetName", dynlib: LLVMLib.}

##  Returns the description  of a target. See llvm::Target::getDescription

proc getTargetDescription*(
  t: TargetRef
): cstring {.importc: "LLVMGetTargetDescription", dynlib: LLVMLib.}

##  Returns if the target has a JIT

proc targetHasJIT*(t: TargetRef): Bool {.importc: "LLVMTargetHasJIT", dynlib: LLVMLib.}
##  Returns if the target has a TargetMachine associated

proc targetHasTargetMachine*(
  t: TargetRef
): Bool {.importc: "LLVMTargetHasTargetMachine", dynlib: LLVMLib.}

##  Returns if the target as an ASM backend (required for emitting output)

proc targetHasAsmBackend*(
  t: TargetRef
): Bool {.importc: "LLVMTargetHasAsmBackend", dynlib: LLVMLib.}

## ===-- Target Machine ----------------------------------------------------===
##
##  Create a new set of options for an llvm::TargetMachine.
##
##  The returned option structure must be released with
##  LLVMDisposeTargetMachineOptions() after the call to
##  LLVMCreateTargetMachineWithOptions().
##

proc createTargetMachineOptions*(): TargetMachineOptionsRef {.
  importc: "LLVMCreateTargetMachineOptions", dynlib: LLVMLib
.}

##
##  Dispose of an LLVMTargetMachineOptionsRef instance.
##

proc disposeTargetMachineOptions*(
  options: TargetMachineOptionsRef
) {.importc: "LLVMDisposeTargetMachineOptions", dynlib: LLVMLib.}

proc targetMachineOptionsSetCPU*(
  options: TargetMachineOptionsRef, cpu: cstring
) {.importc: "LLVMTargetMachineOptionsSetCPU", dynlib: LLVMLib.}

##
##  Set the list of features for the target machine.
##
##  \param Features a comma-separated list of features.
##

proc targetMachineOptionsSetFeatures*(
  options: TargetMachineOptionsRef, features: cstring
) {.importc: "LLVMTargetMachineOptionsSetFeatures", dynlib: LLVMLib.}

proc targetMachineOptionsSetABI*(
  options: TargetMachineOptionsRef, abi: cstring
) {.importc: "LLVMTargetMachineOptionsSetABI", dynlib: LLVMLib.}

proc targetMachineOptionsSetCodeGenOptLevel*(
  options: TargetMachineOptionsRef, level: CodeGenOptLevel
) {.importc: "LLVMTargetMachineOptionsSetCodeGenOptLevel", dynlib: LLVMLib.}

proc targetMachineOptionsSetRelocMode*(
  options: TargetMachineOptionsRef, reloc: RelocMode
) {.importc: "LLVMTargetMachineOptionsSetRelocMode", dynlib: LLVMLib.}

proc targetMachineOptionsSetCodeModel*(
  options: TargetMachineOptionsRef, codeModel: CodeModel
) {.importc: "LLVMTargetMachineOptionsSetCodeModel", dynlib: LLVMLib.}

##
##  Create a new llvm::TargetMachine.
##
##  \param T the target to create a machine for.
##  \param Triple a triple describing the target machine.
##  \param Options additional configuration (see
##                 LLVMCreateTargetMachineOptions()).
##

proc createTargetMachineWithOptions*(
  t: TargetRef, triple: cstring, options: TargetMachineOptionsRef
): TargetMachineRef {.importc: "LLVMCreateTargetMachineWithOptions", dynlib: LLVMLib.}

##  Creates a new llvm::TargetMachine. See llvm::Target::createTargetMachine

proc createTargetMachine*(
  t: TargetRef,
  triple: cstring,
  cpu: cstring,
  features: cstring,
  level: CodeGenOptLevel,
  reloc: RelocMode,
  codeModel: CodeModel,
): TargetMachineRef {.importc: "LLVMCreateTargetMachine", dynlib: LLVMLib.}

##  Dispose the LLVMTargetMachineRef instance generated by
##   LLVMCreateTargetMachine.

proc disposeTargetMachine*(
  t: TargetMachineRef
) {.importc: "LLVMDisposeTargetMachine", dynlib: LLVMLib.}

##  Returns the Target used in a TargetMachine

proc getTargetMachineTarget*(
  t: TargetMachineRef
): TargetRef {.importc: "LLVMGetTargetMachineTarget", dynlib: LLVMLib.}

##  Returns the triple used creating this target machine. See
##   llvm::TargetMachine::getTriple. The result needs to be disposed with
##   LLVMDisposeMessage.

proc getTargetMachineTriple*(
  t: TargetMachineRef
): cstring {.importc: "LLVMGetTargetMachineTriple", dynlib: LLVMLib.}

##  Returns the cpu used creating this target machine. See
##   llvm::TargetMachine::getCPU. The result needs to be disposed with
##   LLVMDisposeMessage.

proc getTargetMachineCPU*(
  t: TargetMachineRef
): cstring {.importc: "LLVMGetTargetMachineCPU", dynlib: LLVMLib.}

##  Returns the feature string used creating this target machine. See
##   llvm::TargetMachine::getFeatureString. The result needs to be disposed with
##   LLVMDisposeMessage.

proc getTargetMachineFeatureString*(
  t: TargetMachineRef
): cstring {.importc: "LLVMGetTargetMachineFeatureString", dynlib: LLVMLib.}

##  Create a DataLayout based on the targetMachine.

proc createTargetDataLayout*(
  t: TargetMachineRef
): TargetDataRef {.importc: "LLVMCreateTargetDataLayout", dynlib: LLVMLib.}

##  Set the target machine's ASM verbosity.

proc setTargetMachineAsmVerbosity*(
  t: TargetMachineRef, verboseAsm: Bool
) {.importc: "LLVMSetTargetMachineAsmVerbosity", dynlib: LLVMLib.}

##  Enable fast-path instruction selection.

proc setTargetMachineFastISel*(
  t: TargetMachineRef, enable: Bool
) {.importc: "LLVMSetTargetMachineFastISel", dynlib: LLVMLib.}

##  Enable global instruction selection.

proc setTargetMachineGlobalISel*(
  t: TargetMachineRef, enable: Bool
) {.importc: "LLVMSetTargetMachineGlobalISel", dynlib: LLVMLib.}

##  Set abort behaviour when global instruction selection fails to lower/select
##  an instruction.

proc setTargetMachineGlobalISelAbort*(
  t: TargetMachineRef, mode: GlobalISelAbortMode
) {.importc: "LLVMSetTargetMachineGlobalISelAbort", dynlib: LLVMLib.}

##  Enable the MachineOutliner pass.

proc setTargetMachineMachineOutliner*(
  t: TargetMachineRef, enable: Bool
) {.importc: "LLVMSetTargetMachineMachineOutliner", dynlib: LLVMLib.}

##  Emits an asm or object file for the given module to the filename. This
##   wraps several c++ only classes (among them a file stream). Returns any
##   error in ErrorMessage. Use LLVMDisposeMessage to dispose the message.

proc targetMachineEmitToFile*(
  t: TargetMachineRef,
  m: ModuleRef,
  filename: cstring,
  codegen: CodeGenFileType,
  errorMessage: cstringArray,
): Bool {.importc: "LLVMTargetMachineEmitToFile", dynlib: LLVMLib.}

##  Compile the LLVM IR stored in \p M and store the result in \p OutMemBuf.

proc targetMachineEmitToMemoryBuffer*(
  t: TargetMachineRef,
  m: ModuleRef,
  codegen: CodeGenFileType,
  errorMessage: cstringArray,
  outMemBuf: ptr MemoryBufferRef,
): Bool {.importc: "LLVMTargetMachineEmitToMemoryBuffer", dynlib: LLVMLib.}

## ===-- Triple ------------------------------------------------------------===
##  Get a triple for the host machine as a string. The result needs to be
##   disposed with LLVMDisposeMessage.

proc getDefaultTargetTriple*(): cstring {.
  importc: "LLVMGetDefaultTargetTriple", dynlib: LLVMLib
.}

##  Normalize a target triple. The result needs to be disposed with
##   LLVMDisposeMessage.

proc normalizeTargetTriple*(
  triple: cstring
): cstring {.importc: "LLVMNormalizeTargetTriple", dynlib: LLVMLib.}

##  Get the host CPU as a string. The result needs to be disposed with
##   LLVMDisposeMessage.

proc getHostCPUName*(): cstring {.importc: "LLVMGetHostCPUName", dynlib: LLVMLib.}
##  Get the host CPU's features as a string. The result needs to be disposed
##   with LLVMDisposeMessage.

proc getHostCPUFeatures*(): cstring {.
  importc: "LLVMGetHostCPUFeatures", dynlib: LLVMLib
.}

##  Adds the target-specific analysis passes to the pass manager.

proc addAnalysisPasses*(
  t: TargetMachineRef, pm: PassManagerRef
) {.importc: "LLVMAddAnalysisPasses", dynlib: LLVMLib.}

##
##  @}
##
