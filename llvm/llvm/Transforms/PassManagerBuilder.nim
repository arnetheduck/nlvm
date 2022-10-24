## ===-- llvm-c/Transform/PassManagerBuilder.h - PMB C Interface ---*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to the PassManagerBuilder class.      *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## !!!Ignored construct:  # LLVM_C_TRANSFORMS_PASSMANAGERBUILDER_H [NewLine] # LLVM_C_TRANSFORMS_PASSMANAGERBUILDER_H [NewLine] # llvm-c/ExternC.h [NewLine] # llvm-c/Types.h [NewLine] typedef struct LLVMOpaquePassManagerBuilder * LLVMPassManagerBuilderRef ;
## Error: expected ';'!!!

## !!!Ignored construct:  LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCTransformsPassManagerBuilder Pass manager builder
##  @ingroup LLVMCTransforms
##
##  @{
##  * See llvm::PassManagerBuilder. LLVMPassManagerBuilderRef LLVMPassManagerBuilderCreate ( void ) ;
## Error: expected ';'!!!

proc passManagerBuilderDispose*(pmb: PassManagerBuilderRef) {.
    importc: "LLVMPassManagerBuilderDispose", dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::OptLevel.

proc passManagerBuilderSetOptLevel*(pmb: PassManagerBuilderRef; optLevel: cuint) {.
    importc: "LLVMPassManagerBuilderSetOptLevel", dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::SizeLevel.

proc passManagerBuilderSetSizeLevel*(pmb: PassManagerBuilderRef; sizeLevel: cuint) {.
    importc: "LLVMPassManagerBuilderSetSizeLevel", dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::DisableUnitAtATime.

proc passManagerBuilderSetDisableUnitAtATime*(pmb: PassManagerBuilderRef;
    value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableUnitAtATime",
                 dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::DisableUnrollLoops.

proc passManagerBuilderSetDisableUnrollLoops*(pmb: PassManagerBuilderRef;
    value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableUnrollLoops",
                 dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::DisableSimplifyLibCalls

proc passManagerBuilderSetDisableSimplifyLibCalls*(pmb: PassManagerBuilderRef;
    value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableSimplifyLibCalls",
                 dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::Inliner.

proc passManagerBuilderUseInlinerWithThreshold*(pmb: PassManagerBuilderRef;
    threshold: cuint) {.importc: "LLVMPassManagerBuilderUseInlinerWithThreshold",
                      dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::populateFunctionPassManager.

proc passManagerBuilderPopulateFunctionPassManager*(pmb: PassManagerBuilderRef;
    pm: PassManagerRef) {.importc: "LLVMPassManagerBuilderPopulateFunctionPassManager",
                        dynlib: LLVMLib.}
## * See llvm::PassManagerBuilder::populateModulePassManager.

proc passManagerBuilderPopulateModulePassManager*(pmb: PassManagerBuilderRef;
    pm: PassManagerRef) {.importc: "LLVMPassManagerBuilderPopulateModulePassManager",
                        dynlib: LLVMLib.}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END # [NewLine]
## Error: expected ';'!!!
