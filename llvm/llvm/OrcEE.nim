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

## !!!Ignored construct:  # LLVM_C_ORCEE_H [NewLine] # LLVM_C_ORCEE_H [NewLine] # llvm-c/Error.h [NewLine] # llvm-c/ExecutionEngine.h [NewLine] # llvm-c/Orc.h [NewLine] # llvm-c/TargetMachine.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCExecutionEngineORCEE ExecutionEngine-based ORC Utils
##  @ingroup LLVMCExecutionEngine
##
##  @{
##  *
##  Create a RTDyldObjectLinkingLayer instance using the standard
##  SectionMemoryManager for memory management.
##  LLVMOrcObjectLayerRef LLVMOrcCreateRTDyldObjectLinkingLayerWithSectionMemoryManager ( LLVMOrcExecutionSessionRef ES ) ;
## Error: expected ';'!!!

## *
##  Add the given listener to the given RTDyldObjectLinkingLayer.
##
##  Note: Layer must be an RTDyldObjectLinkingLayer instance or
##  behavior is undefined.
##

proc orcRTDyldObjectLinkingLayerRegisterJITEventListener*(
    rTDyldObjLinkingLayer: OrcObjectLayerRef; listener: JITEventListenerRef) {.
    importc: "LLVMOrcRTDyldObjectLinkingLayerRegisterJITEventListener",
    dynlib: LLVMLib.}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END #  LLVM_C_ORCEE_H [NewLine]
## Error: expected ';'!!!
