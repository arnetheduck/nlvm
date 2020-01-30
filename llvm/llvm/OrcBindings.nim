## ===----------- llvm-c/OrcBindings.h - Orc Lib C Iface ---------*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMOrcJIT.a, which implements  *|
## |* JIT compilation of LLVM IR.                                                *|
## |*                                                                            *|
## |* Many exotic languages can interoperate with C code but have a harder time  *|
## |* with C++ due to name mangling. So in addition to C, this interface enables *|
## |* tools written in such languages.                                           *|
## |*                                                                            *|
## |* Note: This interface is experimental. It is *NOT* stable, and may be       *|
## |*       changed without warning.                                             *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

type
  OrcJITStackRef* = ptr orcOpaqueJITStack
  OrcModuleHandle* = uint64
  OrcTargetAddress* = uint64
  OrcSymbolResolverFn* = proc (name: cstring; lookupCtx: pointer): uint64 {.cdecl.}
  OrcLazyCompileCallbackFn* = proc (jITStack: OrcJITStackRef; callbackCtx: pointer): uint64 {.cdecl.}

## *
##  Create an ORC JIT stack.
##
##  The client owns the resulting stack, and must call OrcDisposeInstance(...)
##  to destroy it and free its memory. The JIT stack will take ownership of the
##  TargetMachine, which will be destroyed when the stack is destroyed. The
##  client should not attempt to dispose of the Target Machine, or it will result
##  in a double-free.
##

proc orcCreateInstance*(tm: TargetMachineRef): OrcJITStackRef {.
    importc: "LLVMOrcCreateInstance", dynlib: LLVMLib.}
## *
##  Get the error message for the most recent error (if any).
##
##  This message is owned by the ORC JIT Stack and will be freed when the stack
##  is disposed of by LLVMOrcDisposeInstance.
##

proc orcGetErrorMsg*(jITStack: OrcJITStackRef): cstring {.
    importc: "LLVMOrcGetErrorMsg", dynlib: LLVMLib.}
## *
##  Mangle the given symbol.
##  Memory will be allocated for MangledSymbol to hold the result. The client
##

proc orcGetMangledSymbol*(jITStack: OrcJITStackRef; mangledSymbol: cstringArray;
                         symbol: cstring) {.importc: "LLVMOrcGetMangledSymbol",
    dynlib: LLVMLib.}
## *
##  Dispose of a mangled symbol.
##

proc orcDisposeMangledSymbol*(mangledSymbol: cstring) {.
    importc: "LLVMOrcDisposeMangledSymbol", dynlib: LLVMLib.}
## *
##  Create a lazy compile callback.
##

proc orcCreateLazyCompileCallback*(jITStack: OrcJITStackRef;
                                  retAddr: ptr OrcTargetAddress;
                                  callback: OrcLazyCompileCallbackFn;
                                  callbackCtx: pointer): ErrorRef {.
    importc: "LLVMOrcCreateLazyCompileCallback", dynlib: LLVMLib.}
## *
##  Create a named indirect call stub.
##

proc orcCreateIndirectStub*(jITStack: OrcJITStackRef; stubName: cstring;
                           initAddr: OrcTargetAddress): ErrorRef {.
    importc: "LLVMOrcCreateIndirectStub", dynlib: LLVMLib.}
## *
##  Set the pointer for the given indirect stub.
##

proc orcSetIndirectStubPointer*(jITStack: OrcJITStackRef; stubName: cstring;
                               newAddr: OrcTargetAddress): ErrorRef {.
    importc: "LLVMOrcSetIndirectStubPointer", dynlib: LLVMLib.}
## *
##  Add module to be eagerly compiled.
##

proc orcAddEagerlyCompiledIR*(jITStack: OrcJITStackRef;
                             retHandle: ptr OrcModuleHandle; `mod`: ModuleRef;
                             symbolResolver: OrcSymbolResolverFn;
                             symbolResolverCtx: pointer): ErrorRef {.
    importc: "LLVMOrcAddEagerlyCompiledIR", dynlib: LLVMLib.}
## *
##  Add module to be lazily compiled one function at a time.
##

proc orcAddLazilyCompiledIR*(jITStack: OrcJITStackRef;
                            retHandle: ptr OrcModuleHandle; `mod`: ModuleRef;
                            symbolResolver: OrcSymbolResolverFn;
                            symbolResolverCtx: pointer): ErrorRef {.
    importc: "LLVMOrcAddLazilyCompiledIR", dynlib: LLVMLib.}
## *
##  Add an object file.
##
##  This method takes ownership of the given memory buffer and attempts to add
##  it to the JIT as an object file.
##  Clients should *not* dispose of the 'Obj' argument: the JIT will manage it
##  from this call onwards.
##

proc orcAddObjectFile*(jITStack: OrcJITStackRef; retHandle: ptr OrcModuleHandle;
                      obj: MemoryBufferRef; symbolResolver: OrcSymbolResolverFn;
                      symbolResolverCtx: pointer): ErrorRef {.
    importc: "LLVMOrcAddObjectFile", dynlib: LLVMLib.}
## *
##  Remove a module set from the JIT.
##
##  This works for all modules that can be added via OrcAdd*, including object
##  files.
##

proc orcRemoveModule*(jITStack: OrcJITStackRef; h: OrcModuleHandle): ErrorRef {.
    importc: "LLVMOrcRemoveModule", dynlib: LLVMLib.}
## *
##  Get symbol address from JIT instance.
##

proc orcGetSymbolAddress*(jITStack: OrcJITStackRef; retAddr: ptr OrcTargetAddress;
                         symbolName: cstring): ErrorRef {.
    importc: "LLVMOrcGetSymbolAddress", dynlib: LLVMLib.}
## *
##  Get symbol address from JIT instance, searching only the specified
##  handle.
##

proc orcGetSymbolAddressIn*(jITStack: OrcJITStackRef;
                           retAddr: ptr OrcTargetAddress; h: OrcModuleHandle;
                           symbolName: cstring): ErrorRef {.
    importc: "LLVMOrcGetSymbolAddressIn", dynlib: LLVMLib.}
## *
##  Dispose of an ORC JIT stack.
##

proc orcDisposeInstance*(jITStack: OrcJITStackRef): ErrorRef {.
    importc: "LLVMOrcDisposeInstance", dynlib: LLVMLib.}
## *
##  Register a JIT Event Listener.
##
##  A NULL listener is ignored.
##

proc orcRegisterJITEventListener*(jITStack: OrcJITStackRef; L: JITEventListenerRef) {.
    importc: "LLVMOrcRegisterJITEventListener", dynlib: LLVMLib.}
## *
##  Unegister a JIT Event Listener.
##
##  A NULL listener is ignored.
##

proc orcUnregisterJITEventListener*(jITStack: OrcJITStackRef;
                                   L: JITEventListenerRef) {.
    importc: "LLVMOrcUnregisterJITEventListener", dynlib: LLVMLib.}