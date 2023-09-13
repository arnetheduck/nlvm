## ===-- llvm-c/Transform/PassBuilder.h - PassBuilder for LLVM C ---*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header contains the LLVM-C interface into the new pass manager        *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

##
##  @defgroup LLVMCCoreNewPM New Pass Manager
##  @ingroup LLVMCCore
##
##  @{
##
##
##  A set of options passed which are attached to the Pass Manager upon run.
##
##  This corresponds to an llvm::LLVMPassBuilderOptions instance
##
##  The details for how the different properties of this structure are used can
##  be found in the source for LLVMRunPasses
##

type
  PassBuilderOptionsRef* = ptr OpaquePassBuilderOptions

##
##  Construct and run a set of passes over a module
##
##  This function takes a string with the passes that should be used. The format
##  of this string is the same as opt's -passes argument for the new pass
##  manager. Individual passes may be specified, separated by commas. Full
##  pipelines may also be invoked using `default<O3>` and friends. See opt for
##  full reference of the Passes format.
##

proc runPasses*(m: ModuleRef; passes: cstring; tm: TargetMachineRef;
                options: PassBuilderOptionsRef): ErrorRef {.
    importc: "LLVMRunPasses", dynlib: LLVMLib.}
##
##  Create a new set of options for a PassBuilder
##
##  Ownership of the returned instance is given to the client, and they are
##  responsible for it. The client should call LLVMDisposePassBuilderOptions
##  to free the pass builder options.
##

proc createPassBuilderOptions*(): PassBuilderOptionsRef {.
    importc: "LLVMCreatePassBuilderOptions", dynlib: LLVMLib.}
##
##  Toggle adding the VerifierPass for the PassBuilder, ensuring all functions
##  inside the module is valid.
##

proc passBuilderOptionsSetVerifyEach*(options: PassBuilderOptionsRef;
                                      verifyEach: Bool) {.
    importc: "LLVMPassBuilderOptionsSetVerifyEach", dynlib: LLVMLib.}
##
##  Toggle debug logging when running the PassBuilder
##

proc passBuilderOptionsSetDebugLogging*(options: PassBuilderOptionsRef;
                                        debugLogging: Bool) {.
    importc: "LLVMPassBuilderOptionsSetDebugLogging", dynlib: LLVMLib.}
proc passBuilderOptionsSetLoopInterleaving*(options: PassBuilderOptionsRef;
    loopInterleaving: Bool) {.importc: "LLVMPassBuilderOptionsSetLoopInterleaving",
                              dynlib: LLVMLib.}
proc passBuilderOptionsSetLoopVectorization*(options: PassBuilderOptionsRef;
    loopVectorization: Bool) {.importc: "LLVMPassBuilderOptionsSetLoopVectorization",
                               dynlib: LLVMLib.}
proc passBuilderOptionsSetSLPVectorization*(options: PassBuilderOptionsRef;
    sLPVectorization: Bool) {.importc: "LLVMPassBuilderOptionsSetSLPVectorization",
                              dynlib: LLVMLib.}
proc passBuilderOptionsSetLoopUnrolling*(options: PassBuilderOptionsRef;
    loopUnrolling: Bool) {.importc: "LLVMPassBuilderOptionsSetLoopUnrolling",
                           dynlib: LLVMLib.}
proc passBuilderOptionsSetForgetAllSCEVInLoopUnroll*(
    options: PassBuilderOptionsRef; forgetAllSCEVInLoopUnroll: Bool) {.
    importc: "LLVMPassBuilderOptionsSetForgetAllSCEVInLoopUnroll",
    dynlib: LLVMLib.}
proc passBuilderOptionsSetLicmMssaOptCap*(options: PassBuilderOptionsRef;
    licmMssaOptCap: cuint) {.importc: "LLVMPassBuilderOptionsSetLicmMssaOptCap",
                             dynlib: LLVMLib.}
proc passBuilderOptionsSetLicmMssaNoAccForPromotionCap*(
    options: PassBuilderOptionsRef; licmMssaNoAccForPromotionCap: cuint) {.
    importc: "LLVMPassBuilderOptionsSetLicmMssaNoAccForPromotionCap",
    dynlib: LLVMLib.}
proc passBuilderOptionsSetCallGraphProfile*(options: PassBuilderOptionsRef;
    callGraphProfile: Bool) {.importc: "LLVMPassBuilderOptionsSetCallGraphProfile",
                              dynlib: LLVMLib.}
proc passBuilderOptionsSetMergeFunctions*(options: PassBuilderOptionsRef;
    mergeFunctions: Bool) {.importc: "LLVMPassBuilderOptionsSetMergeFunctions",
                            dynlib: LLVMLib.}
##
##  Dispose of a heap-allocated PassBuilderOptions instance
##

proc disposePassBuilderOptions*(options: PassBuilderOptionsRef) {.
    importc: "LLVMDisposePassBuilderOptions", dynlib: LLVMLib.}
##
##  @}
##
