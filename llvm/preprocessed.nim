# Hand-converted stuff created by preprocessing Core.h/Target.h

proc isAArgument*(val: ValueRef): ValueRef {.importc: "LLVMIsAArgument",
    dynlib: LLVMLib.}
proc isABasicBlock*(val: ValueRef): ValueRef {.importc: "LLVMIsABasicBlock",
    dynlib: LLVMLib.}
proc isAInlineAsm*(val: ValueRef): ValueRef {.importc: "LLVMIsAInlineAsm",
    dynlib: LLVMLib.}
proc isAUser*(val: ValueRef): ValueRef {.importc: "LLVMIsAUser", dynlib: LLVMLib.}
proc isAConstant*(val: ValueRef): ValueRef {.importc: "LLVMIsAConstant",
    dynlib: LLVMLib.}
proc isABlockAddress*(val: ValueRef): ValueRef {.importc: "LLVMIsABlockAddress",
    dynlib: LLVMLib.}
proc isAConstantAggregateZero*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAConstantAggregateZero", dynlib: LLVMLib.}
proc isAConstantArray*(val: ValueRef): ValueRef {.importc: "LLVMIsAConstantArray",
    dynlib: LLVMLib.}
proc isAConstantDataSequential*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAConstantDataSequential", dynlib: LLVMLib.}
proc isAConstantDataArray*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAConstantDataArray", dynlib: LLVMLib.}
proc isAConstantDataVector*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAConstantDataVector", dynlib: LLVMLib.}
proc isAConstantExpr*(val: ValueRef): ValueRef {.importc: "LLVMIsAConstantExpr",
    dynlib: LLVMLib.}
proc isAConstantFP*(val: ValueRef): ValueRef {.importc: "LLVMIsAConstantFP",
    dynlib: LLVMLib.}
proc isAConstantInt*(val: ValueRef): ValueRef {.importc: "LLVMIsAConstantInt",
    dynlib: LLVMLib.}
proc isAConstantPointerNull*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAConstantPointerNull", dynlib: LLVMLib.}
proc isAConstantStruct*(val: ValueRef): ValueRef {.importc: "LLVMIsAConstantStruct",
    dynlib: LLVMLib.}
proc isAConstantTokenNone*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAConstantTokenNone", dynlib: LLVMLib.}
proc isAConstantVector*(val: ValueRef): ValueRef {.importc: "LLVMIsAConstantVector",
    dynlib: LLVMLib.}
proc isAGlobalValue*(val: ValueRef): ValueRef {.importc: "LLVMIsAGlobalValue",
    dynlib: LLVMLib.}
proc isAGlobalAlias*(val: ValueRef): ValueRef {.importc: "LLVMIsAGlobalAlias",
    dynlib: LLVMLib.}
proc isAGlobalIFunc*(val: ValueRef): ValueRef {.importc: "LLVMIsAGlobalIFunc",
    dynlib: LLVMLib.}
proc isAGlobalObject*(val: ValueRef): ValueRef {.importc: "LLVMIsAGlobalObject",
    dynlib: LLVMLib.}
proc isAFunction*(val: ValueRef): ValueRef {.importc: "LLVMIsAFunction",
    dynlib: LLVMLib.}
proc isAGlobalVariable*(val: ValueRef): ValueRef {.importc: "LLVMIsAGlobalVariable",
    dynlib: LLVMLib.}
proc isAUndefValue*(val: ValueRef): ValueRef {.importc: "LLVMIsAUndefValue",
    dynlib: LLVMLib.}
proc isAInstruction*(val: ValueRef): ValueRef {.importc: "LLVMIsAInstruction",
    dynlib: LLVMLib.}
proc isABinaryOperator*(val: ValueRef): ValueRef {.importc: "LLVMIsABinaryOperator",
    dynlib: LLVMLib.}
proc isACallInst*(val: ValueRef): ValueRef {.importc: "LLVMIsACallInst",
    dynlib: LLVMLib.}
proc isAIntrinsicInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAIntrinsicInst",
    dynlib: LLVMLib.}
proc isADbgInfoIntrinsic*(val: ValueRef): ValueRef {.
    importc: "LLVMIsADbgInfoIntrinsic", dynlib: LLVMLib.}
proc isADbgVariableIntrinsic*(val: ValueRef): ValueRef {.
    importc: "LLVMIsADbgVariableIntrinsic", dynlib: LLVMLib.}
proc isADbgDeclareInst*(val: ValueRef): ValueRef {.importc: "LLVMIsADbgDeclareInst",
    dynlib: LLVMLib.}
proc isADbgLabelInst*(val: ValueRef): ValueRef {.importc: "LLVMIsADbgLabelInst",
    dynlib: LLVMLib.}
proc isAMemIntrinsic*(val: ValueRef): ValueRef {.importc: "LLVMIsAMemIntrinsic",
    dynlib: LLVMLib.}
proc isAMemCpyInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAMemCpyInst",
    dynlib: LLVMLib.}
proc isAMemMoveInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAMemMoveInst",
    dynlib: LLVMLib.}
proc isAMemSetInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAMemSetInst",
    dynlib: LLVMLib.}
proc isACmpInst*(val: ValueRef): ValueRef {.importc: "LLVMIsACmpInst", dynlib: LLVMLib.}
proc isAFCmpInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAFCmpInst",
    dynlib: LLVMLib.}
proc isAICmpInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAICmpInst",
    dynlib: LLVMLib.}
proc isAExtractElementInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAExtractElementInst", dynlib: LLVMLib.}
proc isAGetElementPtrInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAGetElementPtrInst", dynlib: LLVMLib.}
proc isAInsertElementInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAInsertElementInst", dynlib: LLVMLib.}
proc isAInsertValueInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAInsertValueInst", dynlib: LLVMLib.}
proc isALandingPadInst*(val: ValueRef): ValueRef {.importc: "LLVMIsALandingPadInst",
    dynlib: LLVMLib.}
proc isAPHINode*(val: ValueRef): ValueRef {.importc: "LLVMIsAPHINode", dynlib: LLVMLib.}
proc isASelectInst*(val: ValueRef): ValueRef {.importc: "LLVMIsASelectInst",
    dynlib: LLVMLib.}
proc isAShuffleVectorInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAShuffleVectorInst", dynlib: LLVMLib.}
proc isAStoreInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAStoreInst",
    dynlib: LLVMLib.}
proc isABranchInst*(val: ValueRef): ValueRef {.importc: "LLVMIsABranchInst",
    dynlib: LLVMLib.}
proc isAIndirectBrInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAIndirectBrInst",
    dynlib: LLVMLib.}
proc isAInvokeInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAInvokeInst",
    dynlib: LLVMLib.}
proc isAReturnInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAReturnInst",
    dynlib: LLVMLib.}
proc isASwitchInst*(val: ValueRef): ValueRef {.importc: "LLVMIsASwitchInst",
    dynlib: LLVMLib.}
proc isAUnreachableInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAUnreachableInst", dynlib: LLVMLib.}
proc isAResumeInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAResumeInst",
    dynlib: LLVMLib.}
proc isACleanupReturnInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsACleanupReturnInst", dynlib: LLVMLib.}
proc isACatchReturnInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsACatchReturnInst", dynlib: LLVMLib.}
proc isAFuncletPadInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAFuncletPadInst",
    dynlib: LLVMLib.}
proc isACatchPadInst*(val: ValueRef): ValueRef {.importc: "LLVMIsACatchPadInst",
    dynlib: LLVMLib.}
proc isACleanupPadInst*(val: ValueRef): ValueRef {.importc: "LLVMIsACleanupPadInst",
    dynlib: LLVMLib.}
proc isAUnaryInstruction*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAUnaryInstruction", dynlib: LLVMLib.}
proc isAAllocaInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAAllocaInst",
    dynlib: LLVMLib.}
proc isACastInst*(val: ValueRef): ValueRef {.importc: "LLVMIsACastInst",
    dynlib: LLVMLib.}
proc isAAddrSpaceCastInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAAddrSpaceCastInst", dynlib: LLVMLib.}
proc isABitCastInst*(val: ValueRef): ValueRef {.importc: "LLVMIsABitCastInst",
    dynlib: LLVMLib.}
proc isAFPExtInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAFPExtInst",
    dynlib: LLVMLib.}
proc isAFPToSIInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAFPToSIInst",
    dynlib: LLVMLib.}
proc isAFPToUIInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAFPToUIInst",
    dynlib: LLVMLib.}
proc isAFPTruncInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAFPTruncInst",
    dynlib: LLVMLib.}
proc isAIntToPtrInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAIntToPtrInst",
    dynlib: LLVMLib.}
proc isAPtrToIntInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAPtrToIntInst",
    dynlib: LLVMLib.}
proc isASExtInst*(val: ValueRef): ValueRef {.importc: "LLVMIsASExtInst",
    dynlib: LLVMLib.}
proc isASIToFPInst*(val: ValueRef): ValueRef {.importc: "LLVMIsASIToFPInst",
    dynlib: LLVMLib.}
proc isATruncInst*(val: ValueRef): ValueRef {.importc: "LLVMIsATruncInst",
    dynlib: LLVMLib.}
proc isAUIToFPInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAUIToFPInst",
    dynlib: LLVMLib.}
proc isAZExtInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAZExtInst",
    dynlib: LLVMLib.}
proc isAExtractValueInst*(val: ValueRef): ValueRef {.
    importc: "LLVMIsAExtractValueInst", dynlib: LLVMLib.}
proc isALoadInst*(val: ValueRef): ValueRef {.importc: "LLVMIsALoadInst",
    dynlib: LLVMLib.}
proc isAVAArgInst*(val: ValueRef): ValueRef {.importc: "LLVMIsAVAArgInst",
    dynlib: LLVMLib.}

# Target.h is quite a mess with lots of site-specific stuff - some of the parts
# that c2nim can't deal with:
proc initializeAArch64TargetInfo*() {.importc: "LLVMInitializeAArch64TargetInfo",
  dynlib: LLVMLib.}
proc initializeAMDGPUTargetInfo*() {.importc: "LLVMInitializeAMDGPUTargetInfo",
 dynlib: LLVMLib.}
proc initializeARMTargetInfo*() {.importc: "LLVMInitializeARMTargetInfo",
dynlib: LLVMLib.}
proc initializeBPFTargetInfo*() {.importc: "LLVMInitializeBPFTargetInfo",
dynlib: LLVMLib.}
proc initializeHexagonTargetInfo*() {.importc: "LLVMInitializeHexagonTargetInfo",
  dynlib: LLVMLib.}
proc initializeLanaiTargetInfo*() {.importc: "LLVMInitializeLanaiTargetInfo",
dynlib: LLVMLib.}
proc initializeMipsTargetInfo*() {.importc: "LLVMInitializeMipsTargetInfo",
dynlib: LLVMLib.}
proc initializeMSP430TargetInfo*() {.importc: "LLVMInitializeMSP430TargetInfo",
 dynlib: LLVMLib.}
proc initializeNVPTXTargetInfo*() {.importc: "LLVMInitializeNVPTXTargetInfo",
dynlib: LLVMLib.}
proc initializePowerPCTargetInfo*() {.importc: "LLVMInitializePowerPCTargetInfo",
  dynlib: LLVMLib.}
proc initializeSparcTargetInfo*() {.importc: "LLVMInitializeSparcTargetInfo",
dynlib: LLVMLib.}
proc initializeSystemZTargetInfo*() {.importc: "LLVMInitializeSystemZTargetInfo",
  dynlib: LLVMLib.}
proc initializeWebAssemblyTargetInfo*() {.importc: "LLVMInitializeWebAssemblyTargetInfo",
      dynlib: LLVMLib.}
proc initializeX86TargetInfo*() {.importc: "LLVMInitializeX86TargetInfo",
dynlib: LLVMLib.}
proc initializeXCoreTargetInfo*() {.importc: "LLVMInitializeXCoreTargetInfo",
dynlib: LLVMLib.}
proc initializeAArch64Target*() {.importc: "LLVMInitializeAArch64Target",
dynlib: LLVMLib.}
proc initializeAMDGPUTarget*() {.importc: "LLVMInitializeAMDGPUTarget",
dynlib: LLVMLib.}
proc initializeARMTarget*() {.importc: "LLVMInitializeARMTarget", dynlib: LLVMLib.}
proc initializeBPFTarget*() {.importc: "LLVMInitializeBPFTarget", dynlib: LLVMLib.}
proc initializeHexagonTarget*() {.importc: "LLVMInitializeHexagonTarget",
dynlib: LLVMLib.}
proc initializeLanaiTarget*() {.importc: "LLVMInitializeLanaiTarget",
dynlib: LLVMLib.}
proc initializeMipsTarget*() {.importc: "LLVMInitializeMipsTarget", dynlib: LLVMLib.}
proc initializeMSP430Target*() {.importc: "LLVMInitializeMSP430Target",
dynlib: LLVMLib.}
proc initializeNVPTXTarget*() {.importc: "LLVMInitializeNVPTXTarget",
dynlib: LLVMLib.}
proc initializePowerPCTarget*() {.importc: "LLVMInitializePowerPCTarget",
dynlib: LLVMLib.}
proc initializeSparcTarget*() {.importc: "LLVMInitializeSparcTarget",
dynlib: LLVMLib.}
proc initializeSystemZTarget*() {.importc: "LLVMInitializeSystemZTarget",
dynlib: LLVMLib.}
proc initializeWebAssemblyTarget*() {.importc: "LLVMInitializeWebAssemblyTarget",
  dynlib: LLVMLib.}
proc initializeX86Target*() {.importc: "LLVMInitializeX86Target", dynlib: LLVMLib.}
proc initializeXCoreTarget*() {.importc: "LLVMInitializeXCoreTarget",
dynlib: LLVMLib.}
proc initializeAArch64TargetMC*() {.importc: "LLVMInitializeAArch64TargetMC",
dynlib: LLVMLib.}
proc initializeAMDGPUTargetMC*() {.importc: "LLVMInitializeAMDGPUTargetMC",
dynlib: LLVMLib.}
proc initializeARMTargetMC*() {.importc: "LLVMInitializeARMTargetMC",
dynlib: LLVMLib.}
proc initializeBPFTargetMC*() {.importc: "LLVMInitializeBPFTargetMC",
dynlib: LLVMLib.}
proc initializeHexagonTargetMC*() {.importc: "LLVMInitializeHexagonTargetMC",
dynlib: LLVMLib.}
proc initializeLanaiTargetMC*() {.importc: "LLVMInitializeLanaiTargetMC",
dynlib: LLVMLib.}
proc initializeMipsTargetMC*() {.importc: "LLVMInitializeMipsTargetMC",
dynlib: LLVMLib.}
proc initializeMSP430TargetMC*() {.importc: "LLVMInitializeMSP430TargetMC",
dynlib: LLVMLib.}
proc initializeNVPTXTargetMC*() {.importc: "LLVMInitializeNVPTXTargetMC",
dynlib: LLVMLib.}
proc initializePowerPCTargetMC*() {.importc: "LLVMInitializePowerPCTargetMC",
dynlib: LLVMLib.}
proc initializeSparcTargetMC*() {.importc: "LLVMInitializeSparcTargetMC",
dynlib: LLVMLib.}
proc initializeSystemZTargetMC*() {.importc: "LLVMInitializeSystemZTargetMC",
dynlib: LLVMLib.}
proc initializeWebAssemblyTargetMC*() {.importc: "LLVMInitializeWebAssemblyTargetMC",
    dynlib: LLVMLib.}
proc initializeX86TargetMC*() {.importc: "LLVMInitializeX86TargetMC",
dynlib: LLVMLib.}
proc initializeXCoreTargetMC*() {.importc: "LLVMInitializeXCoreTargetMC",
dynlib: LLVMLib.}
proc initializeAArch64AsmPrinter*() {.importc: "LLVMInitializeAArch64AsmPrinter",
  dynlib: LLVMLib.}
proc initializeAMDGPUAsmPrinter*() {.importc: "LLVMInitializeAMDGPUAsmPrinter",
 dynlib: LLVMLib.}
proc initializeARMAsmPrinter*() {.importc: "LLVMInitializeARMAsmPrinter",
dynlib: LLVMLib.}
proc initializeBPFAsmPrinter*() {.importc: "LLVMInitializeBPFAsmPrinter",
dynlib: LLVMLib.}
proc initializeHexagonAsmPrinter*() {.importc: "LLVMInitializeHexagonAsmPrinter",
  dynlib: LLVMLib.}
proc initializeLanaiAsmPrinter*() {.importc: "LLVMInitializeLanaiAsmPrinter",
dynlib: LLVMLib.}
proc initializeMipsAsmPrinter*() {.importc: "LLVMInitializeMipsAsmPrinter",
dynlib: LLVMLib.}
proc initializeMSP430AsmPrinter*() {.importc: "LLVMInitializeMSP430AsmPrinter",
 dynlib: LLVMLib.}
proc initializeNVPTXAsmPrinter*() {.importc: "LLVMInitializeNVPTXAsmPrinter",
dynlib: LLVMLib.}
proc initializePowerPCAsmPrinter*() {.importc: "LLVMInitializePowerPCAsmPrinter",
  dynlib: LLVMLib.}
proc initializeSparcAsmPrinter*() {.importc: "LLVMInitializeSparcAsmPrinter",
dynlib: LLVMLib.}
proc initializeSystemZAsmPrinter*() {.importc: "LLVMInitializeSystemZAsmPrinter",
  dynlib: LLVMLib.}
proc initializeWebAssemblyAsmPrinter*() {.importc: "LLVMInitializeWebAssemblyAsmPrinter",
      dynlib: LLVMLib.}
proc initializeX86AsmPrinter*() {.importc: "LLVMInitializeX86AsmPrinter",
dynlib: LLVMLib.}
proc initializeXCoreAsmPrinter*() {.importc: "LLVMInitializeXCoreAsmPrinter",
dynlib: LLVMLib.}
proc initializeAArch64AsmParser*() {.importc: "LLVMInitializeAArch64AsmParser",
 dynlib: LLVMLib.}
proc initializeAMDGPUAsmParser*() {.importc: "LLVMInitializeAMDGPUAsmParser",
dynlib: LLVMLib.}
proc initializeARMAsmParser*() {.importc: "LLVMInitializeARMAsmParser",
dynlib: LLVMLib.}
proc initializeBPFAsmParser*() {.importc: "LLVMInitializeBPFAsmParser",
dynlib: LLVMLib.}
proc initializeHexagonAsmParser*() {.importc: "LLVMInitializeHexagonAsmParser",
 dynlib: LLVMLib.}
proc initializeLanaiAsmParser*() {.importc: "LLVMInitializeLanaiAsmParser",
dynlib: LLVMLib.}
proc initializeMipsAsmParser*() {.importc: "LLVMInitializeMipsAsmParser",
dynlib: LLVMLib.}
proc initializeMSP430AsmParser*() {.importc: "LLVMInitializeMSP430AsmParser",
dynlib: LLVMLib.}
proc initializePowerPCAsmParser*() {.importc: "LLVMInitializePowerPCAsmParser",
 dynlib: LLVMLib.}
proc initializeSparcAsmParser*() {.importc: "LLVMInitializeSparcAsmParser",
dynlib: LLVMLib.}
proc initializeSystemZAsmParser*() {.importc: "LLVMInitializeSystemZAsmParser",
 dynlib: LLVMLib.}
proc initializeWebAssemblyAsmParser*() {.importc: "LLVMInitializeWebAssemblyAsmParser",
     dynlib: LLVMLib.}
proc initializeX86AsmParser*() {.importc: "LLVMInitializeX86AsmParser",
dynlib: LLVMLib.}
proc initializeAArch64Disassembler*() {.importc: "LLVMInitializeAArch64Disassembler",
    dynlib: LLVMLib.}
proc initializeAMDGPUDisassembler*() {.importc: "LLVMInitializeAMDGPUDisassembler",
   dynlib: LLVMLib.}
proc initializeARMDisassembler*() {.importc: "LLVMInitializeARMDisassembler",
dynlib: LLVMLib.}
proc initializeBPFDisassembler*() {.importc: "LLVMInitializeBPFDisassembler",
dynlib: LLVMLib.}
proc initializeHexagonDisassembler*() {.importc: "LLVMInitializeHexagonDisassembler",
    dynlib: LLVMLib.}
proc initializeLanaiDisassembler*() {.importc: "LLVMInitializeLanaiDisassembler",
  dynlib: LLVMLib.}
proc initializeMipsDisassembler*() {.importc: "LLVMInitializeMipsDisassembler",
 dynlib: LLVMLib.}
proc initializeMSP430Disassembler*() {.importc: "LLVMInitializeMSP430Disassembler",
   dynlib: LLVMLib.}
proc initializePowerPCDisassembler*() {.importc: "LLVMInitializePowerPCDisassembler",
    dynlib: LLVMLib.}
proc initializeSparcDisassembler*() {.importc: "LLVMInitializeSparcDisassembler",
  dynlib: LLVMLib.}
proc initializeSystemZDisassembler*() {.importc: "LLVMInitializeSystemZDisassembler",
    dynlib: LLVMLib.}
proc initializeWebAssemblyDisassembler*() {.importc: "LLVMInitializeWebAssemblyDisassembler",
  dynlib: LLVMLib.}
proc initializeX86Disassembler*() {.importc: "LLVMInitializeX86Disassembler",
  dynlib: LLVMLib.}
proc initializeXCoreDisassembler*() {.importc: "LLVMInitializeXCoreDisassembler",
  dynlib: LLVMLib.}
proc initializeAllTargetInfos*() {.inline.} =
  initializeAArch64TargetInfo()
  initializeAMDGPUTargetInfo()
  initializeARMTargetInfo()
  initializeBPFTargetInfo()
  initializeHexagonTargetInfo()
  initializeLanaiTargetInfo()
  initializeMipsTargetInfo()
  initializeMSP430TargetInfo()
  initializeNVPTXTargetInfo()
  initializePowerPCTargetInfo()
  initializeSparcTargetInfo()
  initializeSystemZTargetInfo()
  initializeWebAssemblyTargetInfo()
  initializeX86TargetInfo()
  initializeXCoreTargetInfo()

proc initializeAllTargets*() {.inline.} =
  initializeAArch64Target()
  initializeAMDGPUTarget()
  initializeARMTarget()
  initializeBPFTarget()
  initializeHexagonTarget()
  initializeLanaiTarget()
  initializeMipsTarget()
  initializeMSP430Target()
  initializeNVPTXTarget()
  initializePowerPCTarget()
  initializeSparcTarget()
  initializeSystemZTarget()
  initializeWebAssemblyTarget()
  initializeX86Target()
  initializeXCoreTarget()

proc initializeAllTargetMCs*() {.inline.} =
  initializeAArch64TargetMC()
  initializeAMDGPUTargetMC()
  initializeARMTargetMC()
  initializeBPFTargetMC()
  initializeHexagonTargetMC()
  initializeLanaiTargetMC()
  initializeMipsTargetMC()
  initializeMSP430TargetMC()
  initializeNVPTXTargetMC()
  initializePowerPCTargetMC()
  initializeSparcTargetMC()
  initializeSystemZTargetMC()
  initializeWebAssemblyTargetMC()
  initializeX86TargetMC()
  initializeXCoreTargetMC()

proc initializeAllAsmPrinters*() {.inline.} =
  initializeAArch64AsmPrinter()
  initializeAMDGPUAsmPrinter()
  initializeARMAsmPrinter()
  initializeBPFAsmPrinter()
  initializeHexagonAsmPrinter()
  initializeLanaiAsmPrinter()
  initializeMipsAsmPrinter()
  initializeMSP430AsmPrinter()
  initializeNVPTXAsmPrinter()
  initializePowerPCAsmPrinter()
  initializeSparcAsmPrinter()
  initializeSystemZAsmPrinter()
  initializeWebAssemblyAsmPrinter()
  initializeX86AsmPrinter()
  initializeXCoreAsmPrinter()

proc initializeAllAsmParsers*() {.inline.} =
  initializeAArch64AsmParser()
  initializeAMDGPUAsmParser()
  initializeARMAsmParser()
  initializeBPFAsmParser()
  initializeHexagonAsmParser()
  initializeLanaiAsmParser()
  initializeMipsAsmParser()
  initializeMSP430AsmParser()
  initializePowerPCAsmParser()
  initializeSparcAsmParser()
  initializeSystemZAsmParser()
  initializeWebAssemblyAsmParser()
  initializeX86AsmParser()

proc initializeAllDisassemblers*() {.inline.} =
  initializeAArch64Disassembler()
  initializeAMDGPUDisassembler()
  initializeARMDisassembler()
  initializeBPFDisassembler()
  initializeHexagonDisassembler()
  initializeLanaiDisassembler()
  initializeMipsDisassembler()
  initializeMSP430Disassembler()
  initializePowerPCDisassembler()
  initializeSparcDisassembler()
  initializeSystemZDisassembler()
  initializeWebAssemblyDisassembler()
  initializeX86Disassembler()
  initializeXCoreDisassembler()
