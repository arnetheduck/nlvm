## ===----------- llvm-c/LLJIT.h - OrcV2 LLJIT C bindings --------*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to the LLJIT class in                 *|
## |* libLLVMOrcJIT.a, which provides a simple MCJIT-like ORC JIT.               *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## |* Note: This interface is experimental. It is *NOT* stable, and may be       *|
## |*       changed without warning. Only C API usage documentation is           *|
## |*       provided. See the C++ documentation for all higher level ORC API     *|
## |*       details.                                                             *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

#import
#  llvm-c/Error, llvm-c/Orc, llvm-c/TargetMachine, llvm-c/Types

## LLVM_C_EXTERN_C_BEGIN
## *
##  A function for constructing an ObjectLinkingLayer instance to be used
##  by an LLJIT instance.
##
##  Clients can call LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator to
##  set the creator function to use when constructing an LLJIT instance.
##  This can be used to override the default linking layer implementation
##  that would otherwise be chosen by LLJITBuilder.
##
##  Object linking layers returned by this function will become owned by the
##  LLJIT instance. The client is not responsible for managing their lifetimes
##  after the function returns.
##

type
  OrcLLJITBuilderObjectLinkingLayerCreatorFunction* = proc (Ctx: pointer;
      ES: OrcExecutionSessionRef; Triple: cstring): OrcObjectLayerRef

## *
##  A reference to an orc::LLJITBuilder instance.
##

type
  OrcLLJITBuilderRef* = ptr OrcOpaqueLLJITBuilder

## *
##  A reference to an orc::LLJIT instance.
##

type
  OrcLLJITRef* = ptr OrcOpaqueLLJIT

## *
##  Create an OrcLLJITBuilder.
##
##  The client owns the resulting LLJITBuilder and should dispose of it using
##  OrcDisposeLLJITBuilder once they are done with it.
##

proc orcCreateLLJITBuilder*(): OrcLLJITBuilderRef {.
    importc: "LLVMOrcCreateLLJITBuilder", dynlib: LLVMLib.}
## *
##  Dispose of an OrcLLJITBuilderRef. This should only be called if ownership
##  has not been passed to OrcCreateLLJIT (e.g. because some error prevented
##  that function from being called).
##

proc orcDisposeLLJITBuilder*(Builder: OrcLLJITBuilderRef) {.
    importc: "LLVMOrcDisposeLLJITBuilder", dynlib: LLVMLib.}
## *
##  Set the JITTargetMachineBuilder to be used when constructing the LLJIT
##  instance. Calling this function is optional: if it is not called then the
##  LLJITBuilder will use JITTargeTMachineBuilder::detectHost to construct a
##  JITTargetMachineBuilder.
##
##  This function takes ownership of the JTMB argument: clients should not
##  dispose of the JITTargetMachineBuilder after calling this function.
##

proc orcLLJITBuilderSetJITTargetMachineBuilder*(
    Builder: OrcLLJITBuilderRef; JTMB: OrcJITTargetMachineBuilderRef) {.
    importc: "LLVMOrcLLJITBuilderSetJITTargetMachineBuilder", dynlib: LLVMLib.}
## *
##  Set an ObjectLinkingLayer creator function for this LLJIT instance.
##

proc orcLLJITBuilderSetObjectLinkingLayerCreator*(
    Builder: OrcLLJITBuilderRef;
    F: OrcLLJITBuilderObjectLinkingLayerCreatorFunction; Ctx: pointer) {.
    importc: "LLVMOrcLLJITBuilderSetObjectLinkingLayerCreator", dynlib: LLVMLib.}
## *
##  Create an LLJIT instance from an LLJITBuilder.
##
##  This operation takes ownership of the Builder argument: clients should not
##  dispose of the builder after calling this function (even if the function
##  returns an error). If a null Builder argument is provided then a
##  default-constructed LLJITBuilder will be used.
##
##  On success the resulting LLJIT instance is uniquely owned by the client and
##  automatically manages the memory of all JIT'd code and all modules that are
##  transferred to it (e.g. via OrcLLJITAddIRModule). Disposing of the
##  LLJIT instance will free all memory managed by the JIT, including JIT'd code
##  and not-yet compiled modules.
##

proc orcCreateLLJIT*(Result: ptr OrcLLJITRef;
                     Builder: OrcLLJITBuilderRef): ErrorRef {.
    importc: "LLVMOrcCreateLLJIT", dynlib: LLVMLib.}
## *
##  Dispose of an LLJIT instance.
##

proc orcDisposeLLJIT*(J: OrcLLJITRef): ErrorRef {.
    importc: "LLVMOrcDisposeLLJIT", dynlib: LLVMLib.}
## *
##  Get a reference to the ExecutionSession for this LLJIT instance.
##
##  The ExecutionSession is owned by the LLJIT instance. The client is not
##  responsible for managing its memory.
##

proc orcLLJITGetExecutionSession*(J: OrcLLJITRef): OrcExecutionSessionRef {.
    importc: "LLVMOrcLLJITGetExecutionSession", dynlib: LLVMLib.}
## *
##  Return a reference to the Main JITDylib.
##
##  The JITDylib is owned by the LLJIT instance. The client is not responsible
##  for managing its memory.
##

proc orcLLJITGetMainJITDylib*(J: OrcLLJITRef): OrcJITDylibRef {.
    importc: "LLVMOrcLLJITGetMainJITDylib", dynlib: LLVMLib.}
## *
##  Return the target triple for this LLJIT instance. This string is owned by
##  the LLJIT instance and should not be freed by the client.
##

proc orcLLJITGetTripleString*(J: OrcLLJITRef): cstring {.
    importc: "LLVMOrcLLJITGetTripleString", dynlib: LLVMLib.}
## *
##  Returns the global prefix character according to the LLJIT's DataLayout.
##

proc orcLLJITGetGlobalPrefix*(J: OrcLLJITRef): char {.
    importc: "LLVMOrcLLJITGetGlobalPrefix", dynlib: LLVMLib.}
## *
##  Mangles the given string according to the LLJIT instance's DataLayout, then
##  interns the result in the SymbolStringPool and returns a reference to the
##  pool entry. Clients should call OrcReleaseSymbolStringPoolEntry to
##  decrement the ref-count on the pool entry once they are finished with this
##  value.
##

proc orcLLJITMangleAndIntern*(J: OrcLLJITRef; UnmangledName: cstring): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcLLJITMangleAndIntern", dynlib: LLVMLib.}
## *
##  Add a buffer representing an object file to the given JITDylib in the given
##  LLJIT instance. This operation transfers ownership of the buffer to the
##  LLJIT instance. The buffer should not be disposed of or referenced once this
##  function returns.
##
##  Resources associated with the given object will be tracked by the given
##  JITDylib's default resource tracker.
##

proc orcLLJITAddObjectFile*(J: OrcLLJITRef; JD: OrcJITDylibRef;
                               ObjBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddObjectFile", dynlib: LLVMLib.}
## *
##  Add a buffer representing an object file to the given ResourceTracker's
##  JITDylib in the given LLJIT instance. This operation transfers ownership of
##  the buffer to the LLJIT instance. The buffer should not be disposed of or
##  referenced once this function returns.
##
##  Resources associated with the given object will be tracked by ResourceTracker
##  RT.
##

proc orcLLJITAddObjectFileWithRT*(J: OrcLLJITRef;
                                     RT: OrcResourceTrackerRef;
                                     ObjBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddObjectFileWithRT", dynlib: LLVMLib.}
## *
##  Add an IR module to the given JITDylib in the given LLJIT instance. This
##  operation transfers ownership of the TSM argument to the LLJIT instance.
##  The TSM argument should not be disposed of or referenced once this
##  function returns.
##
##  Resources associated with the given Module will be tracked by the given
##  JITDylib's default resource tracker.
##

proc orcLLJITAddLLVMIRModule*(J: OrcLLJITRef; JD: OrcJITDylibRef;
                                 TSM: OrcThreadSafeModuleRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddLLVMIRModule", dynlib: LLVMLib.}
## *
##  Add an IR module to the given ResourceTracker's JITDylib in the given LLJIT
##  instance. This operation transfers ownership of the TSM argument to the LLJIT
##  instance. The TSM argument should not be disposed of or referenced once this
##  function returns.
##
##  Resources associated with the given Module will be tracked by ResourceTracker
##  RT.
##

proc orcLLJITAddLLVMIRModuleWithRT*(J: OrcLLJITRef;
                                       JD: OrcResourceTrackerRef;
                                       TSM: OrcThreadSafeModuleRef): ErrorRef {.
    importc: "LLVMOrcLLJITAddLLVMIRModuleWithRT", dynlib: LLVMLib.}
## *
##  Look up the given symbol in the main JITDylib of the given LLJIT instance.
##
##  This operation does not take ownership of the Name argument.
##

proc orcLLJITLookup*(J: OrcLLJITRef; Result: ptr OrcExecutorAddress;
                        Name: cstring): ErrorRef {.
    importc: "LLVMOrcLLJITLookup", dynlib: LLVMLib.}
## *
##  Returns a non-owning reference to the LLJIT instance's object linking layer.
##

proc orcLLJITGetObjLinkingLayer*(J: OrcLLJITRef): OrcObjectLayerRef {.
    importc: "LLVMOrcLLJITGetObjLinkingLayer", dynlib: LLVMLib.}
## *
##  Returns a non-owning reference to the LLJIT instance's object linking layer.
##

proc orcLLJITGetObjTransformLayer*(J: OrcLLJITRef): OrcObjectTransformLayerRef {.
    importc: "LLVMOrcLLJITGetObjTransformLayer", dynlib: LLVMLib.}
## *
##  Returns a non-owning reference to the LLJIT instance's IR transform layer.
##

proc orcLLJITGetIRTransformLayer*(J: OrcLLJITRef): OrcIRTransformLayerRef {.
    importc: "LLVMOrcLLJITGetIRTransformLayer", dynlib: LLVMLib.}
## *
##  Get the LLJIT instance's default data layout string.
##
##  This string is owned by the LLJIT instance and does not need to be freed
##  by the caller.
##

proc orcLLJITGetDataLayoutStr*(J: OrcLLJITRef): cstring {.
    importc: "LLVMOrcLLJITGetDataLayoutStr", dynlib: LLVMLib.}
## _C_EXTERN_C_END
