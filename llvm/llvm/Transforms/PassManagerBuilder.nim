## ===-- llvm-c/Transform/PassManagerBuilder.h - PMB C Interface ---*- C -*-===*\
## |*                                                                            *|
## |*                     The LLVM Compiler Infrastructure                       *|
## |*                                                                            *|
## |* This file is distributed under the University of Illinois Open Source      *|
## |* License. See LICENSE.TXT for details.                                      *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to the PassManagerBuilder class.      *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

type
  PassManagerBuilderRef* = ptr OpaquePassManagerBuilder

## *
##  @defgroup LLVMCTransformsPassManagerBuilder Pass manager builder
##  @ingroup LLVMCTransforms
##
##  @{
##
## * See llvm::PassManagerBuilder.

proc passManagerBuilderCreate*(): PassManagerBuilderRef {.
    importc: "LLVMPassManagerBuilderCreate", llvmImport.}
proc passManagerBuilderDispose*(pmb: PassManagerBuilderRef) {.
    importc: "LLVMPassManagerBuilderDispose", llvmImport.}
## * See llvm::PassManagerBuilder::OptLevel.

proc passManagerBuilderSetOptLevel*(pmb: PassManagerBuilderRef; optLevel: cuint) {.
    importc: "LLVMPassManagerBuilderSetOptLevel", llvmImport.}
## * See llvm::PassManagerBuilder::SizeLevel.

proc passManagerBuilderSetSizeLevel*(pmb: PassManagerBuilderRef; sizeLevel: cuint) {.
    importc: "LLVMPassManagerBuilderSetSizeLevel", llvmImport.}
## * See llvm::PassManagerBuilder::DisableUnitAtATime.

proc passManagerBuilderSetDisableUnitAtATime*(pmb: PassManagerBuilderRef;
    value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableUnitAtATime",
                 llvmImport.}
## * See llvm::PassManagerBuilder::DisableUnrollLoops.

proc passManagerBuilderSetDisableUnrollLoops*(pmb: PassManagerBuilderRef;
    value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableUnrollLoops",
                 llvmImport.}
## * See llvm::PassManagerBuilder::DisableSimplifyLibCalls

proc passManagerBuilderSetDisableSimplifyLibCalls*(pmb: PassManagerBuilderRef;
    value: Bool) {.importc: "LLVMPassManagerBuilderSetDisableSimplifyLibCalls",
                 llvmImport.}
## * See llvm::PassManagerBuilder::Inliner.

proc passManagerBuilderUseInlinerWithThreshold*(pmb: PassManagerBuilderRef;
    threshold: cuint) {.importc: "LLVMPassManagerBuilderUseInlinerWithThreshold",
                      llvmImport.}
## * See llvm::PassManagerBuilder::populateFunctionPassManager.

proc passManagerBuilderPopulateFunctionPassManager*(pmb: PassManagerBuilderRef;
    pm: PassManagerRef) {.importc: "LLVMPassManagerBuilderPopulateFunctionPassManager",
                        llvmImport.}
## * See llvm::PassManagerBuilder::populateModulePassManager.

proc passManagerBuilderPopulateModulePassManager*(pmb: PassManagerBuilderRef;
    pm: PassManagerRef) {.importc: "LLVMPassManagerBuilderPopulateModulePassManager",
                        llvmImport.}
## * See llvm::PassManagerBuilder::populateLTOPassManager.

proc passManagerBuilderPopulateLTOPassManager*(pmb: PassManagerBuilderRef;
    pm: PassManagerRef; internalize: Bool; runInliner: Bool) {.
    importc: "LLVMPassManagerBuilderPopulateLTOPassManager", llvmImport.}
## *
##  @}
##
