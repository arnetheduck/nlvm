## ===-- llvm-c/OrcEE.h - OrcV2 C bindings ExecutionEngine utils -*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to ExecutionEngine based utils, e.g.  *|
## |* RTDyldObjectLinkingLayer (based on RuntimeDyld) in Orc.                    *|
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

type
  MemoryManagerCreateContextCallback* = proc (ctxCtx: pointer): pointer
  MemoryManagerNotifyTerminatingCallback* = proc (ctxCtx: pointer)

##
##  @defgroup LLVMCExecutionEngineORCEE ExecutionEngine-based ORC Utils
##  @ingroup LLVMCExecutionEngine
##
##  @{
##
##
##  Create a RTDyldObjectLinkingLayer instance using the standard
##  SectionMemoryManager for memory management.
##

proc orcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager*(
    es: OrcExecutionSessionRef): OrcObjectLayerRef {.
    importc: "LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager",
    dynlib: LLVMLib.}
##
##  Create a RTDyldObjectLinkingLayer instance using MCJIT-memory-manager-like
##  callbacks.
##
##  This is intended to simplify transitions for existing MCJIT clients. The
##  callbacks used are similar (but not identical) to the callbacks for
##  LLVMCreateSimpleMCJITMemoryManager: Unlike MCJIT, RTDyldObjectLinkingLayer
##  will create a new memory manager for each object linked by calling the given
##  CreateContext callback. This allows for code removal by destroying each
##  allocator individually. Every allocator will be destroyed (if it has not been
##  already) at RTDyldObjectLinkingLayer destruction time, and the
##  NotifyTerminating callback will be called to indicate that no further
##  allocation contexts will be created.
##
##  To implement MCJIT-like behavior clients can implement CreateContext,
##  NotifyTerminating, and Destroy as:
##
##    void *CreateContext(void *CtxCtx) { return CtxCtx; }
##    void NotifyTerminating(void *CtxCtx) { MyOriginalDestroy(CtxCtx); }
##    void Destroy(void *Ctx) { }
##
##  This scheme simply reuses the CreateContextCtx pointer as the one-and-only
##  allocation context.
##

proc orcCreateRTDyldObjectLinkingLayerWithMCJITMemoryManagerLikeCallbacks*(
    es: OrcExecutionSessionRef; createContextCtx: pointer;
    createContext: MemoryManagerCreateContextCallback;
    notifyTerminating: MemoryManagerNotifyTerminatingCallback;
    allocateCodeSection: MemoryManagerAllocateCodeSectionCallback;
    allocateDataSection: MemoryManagerAllocateDataSectionCallback;
    finalizeMemory: MemoryManagerFinalizeMemoryCallback;
    destroy: MemoryManagerDestroyCallback): OrcObjectLayerRef {.importc: "LLVMOrcCreateRTDyldObjectLinkingLayerWithMCJITMemoryManagerLikeCallbacks",
    dynlib: LLVMLib.}
##
##  Add the given listener to the given RTDyldObjectLinkingLayer.
##
##  Note: Layer must be an RTDyldObjectLinkingLayer instance or
##  behavior is undefined.
##

proc orcRTDyldObjectLinkingLayerRegisterJITEventListener*(
    rTDyldObjLinkingLayer: OrcObjectLayerRef; listener: JITEventListenerRef) {.
    importc: "LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener",
    dynlib: LLVMLib.}
##
##  @}
##
