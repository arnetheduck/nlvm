## ===-- llvm-c/ExecutionEngine.h - ExecutionEngine Lib C Iface --*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMExecutionEngine.o, which    *|
## |* implements various analyses of the LLVM IR.                                *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

#import
#  ExternC, Target, TargetMachine, Types

## LLVM_C_EXTERN_C_BEGIN
## *
##  @defgroup LLVMCExecutionEngine Execution Engine
##  @ingroup LLVMC
##
##  @{
##

proc linkInMCJIT*() {.importc: "LLVMLinkInMCJIT", dynlib: LLVMLib.}
proc linkInInterpreter*() {.importc: "LLVMLinkInInterpreter", dynlib: LLVMLib.}
type
  GenericValueRef* = ptr OpaqueGenericValue
  ExecutionEngineRef* = ptr OpaqueExecutionEngine
  MCJITMemoryManagerRef* = ptr OpaqueMCJITMemoryManager
  MCJITCompilerOptions* {.bycopy.} = object
    OptLevel*: cuint
    CodeModel*: CodeModel
    NoFramePointerElim*: Bool
    EnableFastISel*: Bool
    MCJMM*: MCJITMemoryManagerRef


## ===-- Operations on generic values --------------------------------------===

proc createGenericValueOfInt*(Ty: TypeRef; N: culonglong; IsSigned: Bool): GenericValueRef {.
    importc: "LLVMCreateGenericValueOfInt", dynlib: LLVMLib.}
proc createGenericValueOfPointer*(P: pointer): GenericValueRef {.
    importc: "LLVMCreateGenericValueOfPointer", dynlib: LLVMLib.}
proc createGenericValueOfFloat*(Ty: TypeRef; N: cdouble): GenericValueRef {.
    importc: "LLVMCreateGenericValueOfFloat", dynlib: LLVMLib.}
proc genericValueIntWidth*(GenValRef: GenericValueRef): cuint {.
    importc: "LLVMGenericValueIntWidth", dynlib: LLVMLib.}
proc genericValueToInt*(GenVal: GenericValueRef; IsSigned: Bool): culonglong {.
    importc: "LLVMGenericValueToInt", dynlib: LLVMLib.}
proc genericValueToPointer*(GenVal: GenericValueRef): pointer {.
    importc: "LLVMGenericValueToPointer", dynlib: LLVMLib.}
proc genericValueToFloat*(TyRef: TypeRef; GenVal: GenericValueRef): cdouble {.
    importc: "LLVMGenericValueToFloat", dynlib: LLVMLib.}
proc disposeGenericValue*(GenVal: GenericValueRef) {.
    importc: "LLVMDisposeGenericValue", dynlib: LLVMLib.}
## ===-- Operations on execution engines -----------------------------------===

proc createExecutionEngineForModule*(OutEE: ptr ExecutionEngineRef;
                                        M: ModuleRef; OutError: cstringArray): Bool {.
    importc: "LLVMCreateExecutionEngineForModule", dynlib: LLVMLib.}
proc createInterpreterForModule*(OutInterp: ptr ExecutionEngineRef;
                                    M: ModuleRef; OutError: cstringArray): Bool {.
    importc: "LLVMCreateInterpreterForModule", dynlib: LLVMLib.}
proc createJITCompilerForModule*(OutJIT: ptr ExecutionEngineRef;
                                    M: ModuleRef; OptLevel: cuint;
                                    OutError: cstringArray): Bool {.
    importc: "LLVMCreateJITCompilerForModule", dynlib: LLVMLib.}
proc initializeMCJITCompilerOptions*(Options: ptr MCJITCompilerOptions;
                                        SizeOfOptions: csize_t) {.
    importc: "LLVMInitializeMCJITCompilerOptions", dynlib: LLVMLib.}
## *
##  Create an MCJIT execution engine for a module, with the given options. It is
##  the responsibility of the caller to ensure that all fields in Options up to
##  the given SizeOfOptions are initialized. It is correct to pass a smaller
##  value of SizeOfOptions that omits some fields. The canonical way of using
##  this is:
##
##  MCJITCompilerOptions options;
##  InitializeMCJITCompilerOptions(&options, sizeof(options));
##  ... fill in those options you care about
##  CreateMCJITCompilerForModule(&jit, mod, &options, sizeof(options),
##                                   &error);
##
##  Note that this is also correct, though possibly suboptimal:
##
##  CreateMCJITCompilerForModule(&jit, mod, 0, 0, &error);
##

proc createMCJITCompilerForModule*(OutJIT: ptr ExecutionEngineRef;
                                      M: ModuleRef;
                                      Options: ptr MCJITCompilerOptions;
                                      SizeOfOptions: csize_t;
                                      OutError: cstringArray): Bool {.
    importc: "LLVMCreateMCJITCompilerForModule", dynlib: LLVMLib.}
proc disposeExecutionEngine*(EE: ExecutionEngineRef) {.
    importc: "LLVMDisposeExecutionEngine", dynlib: LLVMLib.}
proc runStaticConstructors*(EE: ExecutionEngineRef) {.
    importc: "LLVMRunStaticConstructors", dynlib: LLVMLib.}
proc runStaticDestructors*(EE: ExecutionEngineRef) {.
    importc: "LLVMRunStaticDestructors", dynlib: LLVMLib.}
proc runFunctionAsMain*(EE: ExecutionEngineRef; F: ValueRef; ArgC: cuint;
                           ArgV: cstringArray; EnvP: cstringArray): cint {.
    importc: "LLVMRunFunctionAsMain", dynlib: LLVMLib.}
proc runFunction*(EE: ExecutionEngineRef; F: ValueRef; NumArgs: cuint;
                     Args: ptr GenericValueRef): GenericValueRef {.
    importc: "LLVMRunFunction", dynlib: LLVMLib.}
proc freeMachineCodeForFunction*(EE: ExecutionEngineRef; F: ValueRef) {.
    importc: "LLVMFreeMachineCodeForFunction", dynlib: LLVMLib.}
proc addModule*(EE: ExecutionEngineRef; M: ModuleRef) {.
    importc: "LLVMAddModule", dynlib: LLVMLib.}
proc removeModule*(EE: ExecutionEngineRef; M: ModuleRef;
                      OutMod: ptr ModuleRef; OutError: cstringArray): Bool {.
    importc: "LLVMRemoveModule", dynlib: LLVMLib.}
proc findFunction*(EE: ExecutionEngineRef; Name: cstring;
                      OutFn: ptr ValueRef): Bool {.
    importc: "LLVMFindFunction", dynlib: LLVMLib.}
proc recompileAndRelinkFunction*(EE: ExecutionEngineRef; Fn: ValueRef): pointer {.
    importc: "LLVMRecompileAndRelinkFunction", dynlib: LLVMLib.}
proc getExecutionEngineTargetData*(EE: ExecutionEngineRef): TargetDataRef {.
    importc: "LLVMGetExecutionEngineTargetData", dynlib: LLVMLib.}
proc getExecutionEngineTargetMachine*(EE: ExecutionEngineRef): TargetMachineRef {.
    importc: "LLVMGetExecutionEngineTargetMachine", dynlib: LLVMLib.}
proc addGlobalMapping*(EE: ExecutionEngineRef; Global: ValueRef;
                          Addr: pointer) {.importc: "LLVMAddGlobalMapping",
    dynlib: LLVMLib.}
proc getPointerToGlobal*(EE: ExecutionEngineRef; Global: ValueRef): pointer {.
    importc: "LLVMGetPointerToGlobal", dynlib: LLVMLib.}
proc getGlobalValueAddress*(EE: ExecutionEngineRef; Name: cstring): uint64_t {.
    importc: "LLVMGetGlobalValueAddress", dynlib: LLVMLib.}
proc getFunctionAddress*(EE: ExecutionEngineRef; Name: cstring): uint64_t {.
    importc: "LLVMGetFunctionAddress", dynlib: LLVMLib.}
## / Returns true on error, false on success. If true is returned then the error
## / message is copied to OutStr and cleared in the ExecutionEngine instance.

proc executionEngineGetErrMsg*(EE: ExecutionEngineRef;
                                  OutError: cstringArray): Bool {.
    importc: "LLVMExecutionEngineGetErrMsg", dynlib: LLVMLib.}
## ===-- Operations on memory managers -------------------------------------===

type
  MemoryManagerAllocateCodeSectionCallback* = proc (Opaque: pointer;
      Size: pointer; Alignment: cuint; SectionID: cuint; SectionName: cstring): ptr uint8_t
  MemoryManagerAllocateDataSectionCallback* = proc (Opaque: pointer;
      Size: pointer; Alignment: cuint; SectionID: cuint; SectionName: cstring;
      IsReadOnly: Bool): ptr uint8_t
  MemoryManagerFinalizeMemoryCallback* = proc (Opaque: pointer;
      ErrMsg: cstringArray): Bool
  MemoryManagerDestroyCallback* = proc (Opaque: pointer)

## *
##  Create a simple custom MCJIT memory manager. This memory manager can
##  intercept allocations in a module-oblivious way. This will return NULL
##  if any of the passed functions are NULL.
##
##  @param Opaque An opaque client object to pass back to the callbacks.
##  @param AllocateCodeSection Allocate a block of memory for executable code.
##  @param AllocateDataSection Allocate a block of memory for data.
##  @param FinalizeMemory Set page permissions and flush cache. Return 0 on
##    success, 1 on error.
##

proc createSimpleMCJITMemoryManager*(Opaque: pointer; AllocateCodeSection: MemoryManagerAllocateCodeSectionCallback;
    AllocateDataSection: MemoryManagerAllocateDataSectionCallback;
    FinalizeMemory: MemoryManagerFinalizeMemoryCallback; Destroy: MemoryManagerDestroyCallback): MCJITMemoryManagerRef {.
    importc: "LLVMCreateSimpleMCJITMemoryManager", dynlib: LLVMLib.}
proc disposeMCJITMemoryManager*(MM: MCJITMemoryManagerRef) {.
    importc: "LLVMDisposeMCJITMemoryManager", dynlib: LLVMLib.}
## ===-- JIT Event Listener functions -------------------------------------===

proc createGDBRegistrationListener*(): JITEventListenerRef {.
    importc: "LLVMCreateGDBRegistrationListener", dynlib: LLVMLib.}
proc createIntelJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreateIntelJITEventListener", dynlib: LLVMLib.}
proc createOProfileJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreateOProfileJITEventListener", dynlib: LLVMLib.}
proc createPerfJITEventListener*(): JITEventListenerRef {.
    importc: "LLVMCreatePerfJITEventListener", dynlib: LLVMLib.}
## *
##  @}
##
## _C_EXTERN_C_END
