## ===-- llvm-c/Core.h - Core Library C Interface ------------------*- C -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMCore.a, which implements    *|
## |* the LLVM intermediate representation.                                      *|
## |*                                                                            *|
## \*===----------------------------------------------------------------------===

## *
##  @defgroup LLVMC LLVM-C: C interface to LLVM
##
##  This module exposes parts of the LLVM library as a C API.
##
##  @{
##
## *
##  @defgroup LLVMCTransforms Transforms
##
## *
##  @defgroup LLVMCCore Core
##
##  This modules provide an interface to libLLVMCore, which implements
##  the LLVM intermediate representation as well as other related types
##  and utilities.
##
##  Many exotic languages can interoperate with C code but have a harder time
##  with C++ due to name mangling. So in addition to C, this interface enables
##  tools written in such languages.
##
##  @{
##
## *
##  @defgroup LLVMCCoreTypes Types and Enumerations
##
##  @{
##
## / External users depend on the following values being stable. It is not safe
## / to reorder them.

type ##  Terminator Instructions
    ## *
    ##  Emits an error if two values disagree, otherwise the resulting value is
    ##  that of the operands.
    ##
    ##  @see Module::ModFlagBehavior::Error
    ##
  Opcode* {.size: sizeof(cint).} = enum
    Ret = 1, Br = 2, Switch = 3, IndirectBr = 4, Invoke = 5, ##  removed 6 due to API changes
    Unreachable = 7, Add = 8, FAdd = 9, Sub = 10, FSub = 11, Mul = 12, FMul = 13, UDiv = 14, SDiv = 15,
    FDiv = 16, URem = 17, SRem = 18, FRem = 19, ##  Logical Operators
    Shl = 20, LShr = 21, AShr = 22, And = 23, Or = 24, Xor = 25, ##  Memory Operators
    Alloca = 26, Load = 27, Store = 28, GetElementPtr = 29, ##  Cast Operators
    Trunc = 30, ZExt = 31, SExt = 32, FPToUI = 33, FPToSI = 34, UIToFP = 35, SIToFP = 36,
    FPTrunc = 37, FPExt = 38, PtrToInt = 39, IntToPtr = 40, BitCast = 41, ICmp = 42, FCmp = 43,
    PHI = 44, Call = 45, Select = 46, UserOp1 = 47, UserOp2 = 48, VAArg = 49, ExtractElement = 50,
    InsertElement = 51, ShuffleVector = 52, ExtractValue = 53, InsertValue = 54, ##  Atomic operators
    Fence = 55, AtomicCmpXchg = 56, AtomicRMW = 57, ##  Exception Handling Operators
    Resume = 58, LandingPad = 59, AddrSpaceCast = 60, ##  Other Operators
    CleanupRet = 61, CatchRet = 62, CatchPad = 63, CleanupPad = 64, CatchSwitch = 65, FNeg = 66, ##  Standard Binary Operators
    CallBr = 67                 ##  Standard Unary Operators
  TypeKind* {.size: sizeof(cint).} = enum
    VoidTypeKind,             ## *< type with no size
    HalfTypeKind,             ## *< 16 bit floating point type
    FloatTypeKind,            ## *< 32 bit floating point type
    DoubleTypeKind,           ## *< 64 bit floating point type
    X86FP80TypeKind,          ## *< 80 bit floating point type (X87)
    FP128TypeKind,            ## *< 128 bit floating point type (112-bit mantissa)
    PPC_FP128TypeKind,        ## *< 128 bit floating point type (two 64-bits)
    LabelTypeKind,            ## *< Labels
    IntegerTypeKind,          ## *< Arbitrary bit width integers
    FunctionTypeKind,         ## *< Functions
    StructTypeKind,           ## *< Structures
    ArrayTypeKind,            ## *< Arrays
    PointerTypeKind,          ## *< Pointers
    VectorTypeKind,           ## *< SIMD 'packed' format, or other vector type
    MetadataTypeKind,         ## *< Metadata
    X86MMXTypeKind,           ## *< X86 MMX
    TokenTypeKind             ## *< Tokens
  Linkage* {.size: sizeof(cint).} = enum
    ExternalLinkage,          ## *< Externally visible function
    AvailableExternallyLinkage, LinkOnceAnyLinkage, ## *< Keep one copy of function when linking (inline)
    LinkOnceODRLinkage,       ## *< Same, but only replaced by something
                       ##                             equivalent.
    LinkOnceODRAutoHideLinkage, ## *< Obsolete
    WeakAnyLinkage,           ## *< Keep one copy of function when linking (weak)
    WeakODRLinkage,           ## *< Same, but only replaced by something
                   ##                             equivalent.
    AppendingLinkage,         ## *< Special purpose, only applies to global arrays
    InternalLinkage,          ## *< Rename collisions when linking (static
                    ##                                functions)
    PrivateLinkage,           ## *< Like Internal, but omit from symbol table
    DLLImportLinkage,         ## *< Obsolete
    DLLExportLinkage,         ## *< Obsolete
    ExternalWeakLinkage,      ## *< ExternalWeak linkage description
    GhostLinkage,             ## *< Obsolete
    CommonLinkage,            ## *< Tentative definitions
    LinkerPrivateLinkage,     ## *< Like Private, but linker removes.
    LinkerPrivateWeakLinkage  ## *< Like LinkerPrivate, but is weak.
  Visibility* {.size: sizeof(cint).} = enum
    DefaultVisibility,        ## *< The GV is visible
    HiddenVisibility,         ## *< The GV is hidden
    ProtectedVisibility       ## *< The GV is protected
  UnnamedAddr* {.size: sizeof(cint).} = enum
    NoUnnamedAddr,            ## *< Address of the GV is significant.
    LocalUnnamedAddr,         ## *< Address of the GV is locally insignificant.
    GlobalUnnamedAddr         ## *< Address of the GV is globally insignificant.
  DLLStorageClass* {.size: sizeof(cint).} = enum
    DefaultStorageClass = 0, DLLImportStorageClass = 1, ## *< Function to be imported from DLL.
    DLLExportStorageClass = 2
  CallConv* {.size: sizeof(cint).} = enum
    CCallConv = 0, FastCallConv = 8, ColdCallConv = 9, GHCCallConv = 10, HiPECallConv = 11,
    WebKitJSCallConv = 12, AnyRegCallConv = 13, PreserveMostCallConv = 14,
    PreserveAllCallConv = 15, SwiftCallConv = 16, CXXFASTTLSCallConv = 17,
    X86StdcallCallConv = 64, X86FastcallCallConv = 65, ARMAPCSCallConv = 66,
    ARMAAPCSCallConv = 67, ARMAAPCSVFPCallConv = 68, MSP430INTRCallConv = 69,
    X86ThisCallCallConv = 70, PTXKernelCallConv = 71, PTXDeviceCallConv = 72,
    SPIRFUNCCallConv = 75, SPIRKERNELCallConv = 76, IntelOCLBICallConv = 77,
    X8664SysVCallConv = 78, Win64CallConv = 79, X86VectorCallCallConv = 80,
    HHVMCallConv = 81, HHVMCCallConv = 82, X86INTRCallConv = 83, AVRINTRCallConv = 84,
    AVRSIGNALCallConv = 85, AVRBUILTINCallConv = 86, AMDGPUVSCallConv = 87,
    AMDGPUGSCallConv = 88, AMDGPUPSCallConv = 89, AMDGPUCSCallConv = 90,
    AMDGPUKERNELCallConv = 91, X86RegCallCallConv = 92, AMDGPUHSCallConv = 93,
    MSP430BUILTINCallConv = 94, AMDGPULSCallConv = 95, AMDGPUESCallConv = 96
  ValueKind* {.size: sizeof(cint).} = enum
    ArgumentValueKind, BasicBlockValueKind, MemoryUseValueKind, MemoryDefValueKind,
    MemoryPhiValueKind, FunctionValueKind, GlobalAliasValueKind,
    GlobalIFuncValueKind, GlobalVariableValueKind, BlockAddressValueKind,
    ConstantExprValueKind, ConstantArrayValueKind, ConstantStructValueKind,
    ConstantVectorValueKind, UndefValueValueKind, ConstantAggregateZeroValueKind,
    ConstantDataArrayValueKind, ConstantDataVectorValueKind, ConstantIntValueKind,
    ConstantFPValueKind, ConstantPointerNullValueKind, ConstantTokenNoneValueKind,
    MetadataAsValueValueKind, InlineAsmValueKind, InstructionValueKind
  IntPredicate* {.size: sizeof(cint).} = enum
    IntEQ = 32,                 ## *< equal
    IntNE,                    ## *< not equal
    IntUGT,                   ## *< unsigned greater than
    IntUGE,                   ## *< unsigned greater or equal
    IntULT,                   ## *< unsigned less than
    IntULE,                   ## *< unsigned less or equal
    IntSGT,                   ## *< signed greater than
    IntSGE,                   ## *< signed greater or equal
    IntSLT,                   ## *< signed less than
    IntSLE                    ## *< signed less or equal
  RealPredicate* {.size: sizeof(cint).} = enum
    RealPredicateFalse,       ## *< Always false (always folded)
    RealOEQ,                  ## *< True if ordered and equal
    RealOGT,                  ## *< True if ordered and greater than
    RealOGE,                  ## *< True if ordered and greater than or equal
    RealOLT,                  ## *< True if ordered and less than
    RealOLE,                  ## *< True if ordered and less than or equal
    RealONE,                  ## *< True if ordered and operands are unequal
    RealORD,                  ## *< True if ordered (no nans)
    RealUNO,                  ## *< True if unordered: isnan(X) | isnan(Y)
    RealUEQ,                  ## *< True if unordered or equal
    RealUGT,                  ## *< True if unordered or greater than
    RealUGE,                  ## *< True if unordered, greater than, or equal
    RealULT,                  ## *< True if unordered or less than
    RealULE,                  ## *< True if unordered, less than, or equal
    RealUNE,                  ## *< True if unordered or not equal
    RealPredicateTrue         ## *< Always true (always folded)
  LandingPadClauseTy* {.size: sizeof(cint).} = enum
    LandingPadCatch,          ## *< A catch clause
    LandingPadFilter          ## *< A filter clause
  ThreadLocalMode* {.size: sizeof(cint).} = enum
    NotThreadLocal = 0, GeneralDynamicTLSModel, LocalDynamicTLSModel,
    InitialExecTLSModel, LocalExecTLSModel
  AtomicOrdering* {.size: sizeof(cint).} = enum
    AtomicOrderingNotAtomic = 0, ## *< A load or store which is not atomic
    AtomicOrderingUnordered = 1, ## *< Lowest level of atomicity, guarantees
                              ##                                      somewhat sane results, lock free.
    AtomicOrderingMonotonic = 2, ## *< guarantees that if you take all the
                              ##                                      operations affecting a specific address,
                              ##                                      a consistent ordering exists
    AtomicOrderingAcquire = 4, ## *< Acquire provides a barrier of the sort
                            ##                                    necessary to acquire a lock to access other
                            ##                                    memory with normal loads and stores.
    AtomicOrderingRelease = 5, ## *< Release is similar to Acquire, but with
                            ##                                    a barrier of the sort necessary to release
                            ##                                    a lock.
    AtomicOrderingAcquireRelease = 6, ## *< provides both an Acquire and a
                                   ##                                           Release barrier (for fences and
                                   ##                                           operations which both read and write
                                   ##                                            memory).
    AtomicOrderingSequentiallyConsistent = 7
  AtomicRMWBinOp* {.size: sizeof(cint).} = enum
    AtomicRMWBinOpXchg,       ## *< Set the new value and return the one old
    AtomicRMWBinOpAdd,        ## *< Add a value and return the old one
    AtomicRMWBinOpSub,        ## *< Subtract a value and return the old one
    AtomicRMWBinOpAnd,        ## *< And a value and return the old one
    AtomicRMWBinOpNand,       ## *< Not-And a value and return the old one
    AtomicRMWBinOpOr,         ## *< OR a value and return the old one
    AtomicRMWBinOpXor,        ## *< Xor a value and return the old one
    AtomicRMWBinOpMax, ## *< Sets the value if it's greater than the
                      ##                              original using a signed comparison and return
                      ##                              the old one
    AtomicRMWBinOpMin, ## *< Sets the value if it's Smaller than the
                      ##                              original using a signed comparison and return
                      ##                              the old one
    AtomicRMWBinOpUMax, ## *< Sets the value if it's greater than the
                       ##                              original using an unsigned comparison and return
                       ##                              the old one
    AtomicRMWBinOpUMin ## *< Sets the value if it's greater than the
                      ##                              original using an unsigned comparison  and return
                      ##                              the old one
  DiagnosticSeverity* {.size: sizeof(cint).} = enum
    DSError, DSWarning, DSRemark, DSNote
  InlineAsmDialect* {.size: sizeof(cint).} = enum
    InlineAsmDialectATT, InlineAsmDialectIntel
  ModuleFlagBehavior* {.size: sizeof(cint).} = enum
    ModuleFlagBehaviorError, ## *
                            ##  Emits a warning if two values disagree. The result value will be the
                            ##  operand for the flag from the first module being linked.
                            ##
                            ##  @see Module::ModFlagBehavior::Warning
                            ##
    ModuleFlagBehaviorWarning, ## *
                              ##  Adds a requirement that another module flag be present and have a
                              ##  specified value after linking is performed. The value must be a metadata
                              ##  pair, where the first element of the pair is the ID of the module flag
                              ##  to be restricted, and the second element of the pair is the value the
                              ##  module flag should be restricted to. This behavior can be used to
                              ##  restrict the allowable results (via triggering of an error) of linking
                              ##  IDs with the **Override** behavior.
                              ##
                              ##  @see Module::ModFlagBehavior::Require
                              ##
    ModuleFlagBehaviorRequire, ## *
                              ##  Uses the specified value, regardless of the behavior or value of the
                              ##  other module. If both modules specify **Override**, but the values
                              ##  differ, an error will be emitted.
                              ##
                              ##  @see Module::ModFlagBehavior::Override
                              ##
    ModuleFlagBehaviorOverride, ## *
                               ##  Appends the two values, which are required to be metadata nodes.
                               ##
                               ##  @see Module::ModFlagBehavior::Append
                               ##
    ModuleFlagBehaviorAppend, ## *
                             ##  Appends the two values, which are required to be metadata
                             ##  nodes. However, duplicate entries in the second list are dropped
                             ##  during the append operation.
                             ##
                             ##  @see Module::ModFlagBehavior::AppendUnique
                             ##
    ModuleFlagBehaviorAppendUnique


















## *
##  Attribute index are either LLVMAttributeReturnIndex,
##  LLVMAttributeFunctionIndex or a parameter number from 1 to N.
##

const
  AttributeReturnIndex* = 0 ##  ISO C restricts enumerator values to range of 'int'
                         ##  (4294967295 is too large)
                         ##  LLVMAttributeFunctionIndex = ~0U,
  AttributeFunctionIndex* = -1

type
  AttributeIndex* = cuint

## *
##  @}
##

proc initializeCore*(r: PassRegistryRef) {.importc: "LLVMInitializeCore",
                                        dynlib: LLVMLib.}
## * Deallocate and destroy all ManagedStatic variables.
##     @see llvm::llvm_shutdown
##     @see ManagedStatic

proc shutdown*() {.importc: "LLVMShutdown", dynlib: LLVMLib.}
## ===-- Error handling ----------------------------------------------------===

proc createMessage*(message: cstring): cstring {.importc: "LLVMCreateMessage",
    dynlib: LLVMLib.}
proc disposeMessage*(message: cstring) {.importc: "LLVMDisposeMessage",
                                      dynlib: LLVMLib.}
## *
##  @defgroup LLVMCCoreContext Contexts
##
##  Contexts are execution states for the core LLVM IR system.
##
##  Most types are tied to a context instance. Multiple contexts can
##  exist simultaneously. A single context is not thread safe. However,
##  different contexts can execute on different threads simultaneously.
##
##  @{
##

type
  DiagnosticHandler* = proc (a1: DiagnosticInfoRef; a2: pointer)
  YieldCallback* = proc (a1: ContextRef; a2: pointer)

## *
##  Create a new context.
##
##  Every call to this function should be paired with a call to
##  LLVMContextDispose() or the context will leak memory.
##

proc contextCreate*(): ContextRef {.importc: "LLVMContextCreate", dynlib: LLVMLib.}
## *
##  Obtain the global context instance.
##

proc getGlobalContext*(): ContextRef {.importc: "LLVMGetGlobalContext",
                                    dynlib: LLVMLib.}
## *
##  Set the diagnostic handler for this context.
##

proc contextSetDiagnosticHandler*(c: ContextRef; handler: DiagnosticHandler;
                                 diagnosticContext: pointer) {.
    importc: "LLVMContextSetDiagnosticHandler", dynlib: LLVMLib.}
## *
##  Get the diagnostic handler of this context.
##

proc contextGetDiagnosticHandler*(c: ContextRef): DiagnosticHandler {.
    importc: "LLVMContextGetDiagnosticHandler", dynlib: LLVMLib.}
## *
##  Get the diagnostic context of this context.
##

proc contextGetDiagnosticContext*(c: ContextRef): pointer {.
    importc: "LLVMContextGetDiagnosticContext", dynlib: LLVMLib.}
## *
##  Set the yield callback function for this context.
##
##  @see LLVMContext::setYieldCallback()
##

proc contextSetYieldCallback*(c: ContextRef; callback: YieldCallback;
                             opaqueHandle: pointer) {.
    importc: "LLVMContextSetYieldCallback", dynlib: LLVMLib.}
## *
##  Retrieve whether the given context is set to discard all value names.
##
##  @see LLVMContext::shouldDiscardValueNames()
##

proc contextShouldDiscardValueNames*(c: ContextRef): Bool {.
    importc: "LLVMContextShouldDiscardValueNames", dynlib: LLVMLib.}
## *
##  Set whether the given context discards all value names.
##
##  If true, only the names of GlobalValue objects will be available in the IR.
##  This can be used to save memory and runtime, especially in release mode.
##
##  @see LLVMContext::setDiscardValueNames()
##

proc contextSetDiscardValueNames*(c: ContextRef; `discard`: Bool) {.
    importc: "LLVMContextSetDiscardValueNames", dynlib: LLVMLib.}
## *
##  Destroy a context instance.
##
##  This should be called for every call to LLVMContextCreate() or memory
##  will be leaked.
##

proc contextDispose*(c: ContextRef) {.importc: "LLVMContextDispose", dynlib: LLVMLib.}
## *
##  Return a string representation of the DiagnosticInfo. Use
##  LLVMDisposeMessage to free the string.
##
##  @see DiagnosticInfo::print()
##

proc getDiagInfoDescription*(di: DiagnosticInfoRef): cstring {.
    importc: "LLVMGetDiagInfoDescription", dynlib: LLVMLib.}
## *
##  Return an enum LLVMDiagnosticSeverity.
##
##  @see DiagnosticInfo::getSeverity()
##

proc getDiagInfoSeverity*(di: DiagnosticInfoRef): DiagnosticSeverity {.
    importc: "LLVMGetDiagInfoSeverity", dynlib: LLVMLib.}
proc getMDKindIDInContext*(c: ContextRef; name: cstring; sLen: cuint): cuint {.
    importc: "LLVMGetMDKindIDInContext", dynlib: LLVMLib.}
proc getMDKindID*(name: cstring; sLen: cuint): cuint {.importc: "LLVMGetMDKindID",
    dynlib: LLVMLib.}
## *
##  Return an unique id given the name of a enum attribute,
##  or 0 if no attribute by that name exists.
##
##  See http://llvm.org/docs/LangRef.html#parameter-attributes
##  and http://llvm.org/docs/LangRef.html#function-attributes
##  for the list of available attributes.
##
##  NB: Attribute names and/or id are subject to change without
##  going through the C API deprecation cycle.
##

proc getEnumAttributeKindForName*(name: cstring; sLen: csize): cuint {.
    importc: "LLVMGetEnumAttributeKindForName", dynlib: LLVMLib.}
proc getLastEnumAttributeKind*(): cuint {.importc: "LLVMGetLastEnumAttributeKind",
                                       dynlib: LLVMLib.}
## *
##  Create an enum attribute.
##

proc createEnumAttribute*(c: ContextRef; kindID: cuint; val: uint64): AttributeRef {.
    importc: "LLVMCreateEnumAttribute", dynlib: LLVMLib.}
## *
##  Get the unique id corresponding to the enum attribute
##  passed as argument.
##

proc getEnumAttributeKind*(a: AttributeRef): cuint {.
    importc: "LLVMGetEnumAttributeKind", dynlib: LLVMLib.}
## *
##  Get the enum attribute's value. 0 is returned if none exists.
##

proc getEnumAttributeValue*(a: AttributeRef): uint64 {.
    importc: "LLVMGetEnumAttributeValue", dynlib: LLVMLib.}
## *
##  Create a string attribute.
##

proc createStringAttribute*(c: ContextRef; k: cstring; kLength: cuint; v: cstring;
                           vLength: cuint): AttributeRef {.
    importc: "LLVMCreateStringAttribute", dynlib: LLVMLib.}
## *
##  Get the string attribute's kind.
##

proc getStringAttributeKind*(a: AttributeRef; length: ptr cuint): cstring {.
    importc: "LLVMGetStringAttributeKind", dynlib: LLVMLib.}
## *
##  Get the string attribute's value.
##

proc getStringAttributeValue*(a: AttributeRef; length: ptr cuint): cstring {.
    importc: "LLVMGetStringAttributeValue", dynlib: LLVMLib.}
## *
##  Check for the different types of attributes.
##

proc isEnumAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsEnumAttribute",
    dynlib: LLVMLib.}
proc isStringAttribute*(a: AttributeRef): Bool {.importc: "LLVMIsStringAttribute",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreModule Modules
##
##  Modules represent the top-level structure in an LLVM program. An LLVM
##  module is effectively a translation unit or a collection of
##  translation units merged together.
##
##  @{
##
## *
##  Create a new, empty module in the global context.
##
##  This is equivalent to calling LLVMModuleCreateWithNameInContext with
##  LLVMGetGlobalContext() as the context parameter.
##
##  Every invocation should be paired with LLVMDisposeModule() or memory
##  will be leaked.
##

proc moduleCreateWithName*(moduleID: cstring): ModuleRef {.
    importc: "LLVMModuleCreateWithName", dynlib: LLVMLib.}
## *
##  Create a new, empty module in a specific context.
##
##  Every invocation should be paired with LLVMDisposeModule() or memory
##  will be leaked.
##

proc moduleCreateWithNameInContext*(moduleID: cstring; c: ContextRef): ModuleRef {.
    importc: "LLVMModuleCreateWithNameInContext", dynlib: LLVMLib.}
## *
##  Return an exact copy of the specified module.
##

proc cloneModule*(m: ModuleRef): ModuleRef {.importc: "LLVMCloneModule",
    dynlib: LLVMLib.}
## *
##  Destroy a module instance.
##
##  This must be called for every created module or memory will be
##  leaked.
##

proc disposeModule*(m: ModuleRef) {.importc: "LLVMDisposeModule", dynlib: LLVMLib.}
## *
##  Obtain the identifier of a module.
##
##  @param M Module to obtain identifier of
##  @param Len Out parameter which holds the length of the returned string.
##  @return The identifier of M.
##  @see Module::getModuleIdentifier()
##

proc getModuleIdentifier*(m: ModuleRef; len: ptr csize): cstring {.
    importc: "LLVMGetModuleIdentifier", dynlib: LLVMLib.}
## *
##  Set the identifier of a module to a string Ident with length Len.
##
##  @param M The module to set identifier
##  @param Ident The string to set M's identifier to
##  @param Len Length of Ident
##  @see Module::setModuleIdentifier()
##

proc setModuleIdentifier*(m: ModuleRef; ident: cstring; len: csize) {.
    importc: "LLVMSetModuleIdentifier", dynlib: LLVMLib.}
## *
##  Obtain the module's original source file name.
##
##  @param M Module to obtain the name of
##  @param Len Out parameter which holds the length of the returned string
##  @return The original source file name of M
##  @see Module::getSourceFileName()
##

proc getSourceFileName*(m: ModuleRef; len: ptr csize): cstring {.
    importc: "LLVMGetSourceFileName", dynlib: LLVMLib.}
## *
##  Set the original source file name of a module to a string Name with length
##  Len.
##
##  @param M The module to set the source file name of
##  @param Name The string to set M's source file name to
##  @param Len Length of Name
##  @see Module::setSourceFileName()
##

proc setSourceFileName*(m: ModuleRef; name: cstring; len: csize) {.
    importc: "LLVMSetSourceFileName", dynlib: LLVMLib.}
## *
##  Obtain the data layout for a module.
##
##  @see Module::getDataLayoutStr()
##
##  LLVMGetDataLayout is DEPRECATED, as the name is not only incorrect,
##  but match the name of another method on the module. Prefer the use
##  of LLVMGetDataLayoutStr, which is not ambiguous.
##

proc getDataLayoutStr*(m: ModuleRef): cstring {.importc: "LLVMGetDataLayoutStr",
    dynlib: LLVMLib.}
proc getDataLayout*(m: ModuleRef): cstring {.importc: "LLVMGetDataLayout",
    dynlib: LLVMLib.}
## *
##  Set the data layout for a module.
##
##  @see Module::setDataLayout()
##

proc setDataLayout*(m: ModuleRef; dataLayoutStr: cstring) {.
    importc: "LLVMSetDataLayout", dynlib: LLVMLib.}
## *
##  Obtain the target triple for a module.
##
##  @see Module::getTargetTriple()
##

proc getTarget*(m: ModuleRef): cstring {.importc: "LLVMGetTarget", dynlib: LLVMLib.}
## *
##  Set the target triple for a module.
##
##  @see Module::setTargetTriple()
##

proc setTarget*(m: ModuleRef; triple: cstring) {.importc: "LLVMSetTarget",
    dynlib: LLVMLib.}
## *
##  Returns the module flags as an array of flag-key-value triples.  The caller
##  is responsible for freeing this array by calling
##  \c LLVMDisposeModuleFlagsMetadata.
##
##  @see Module::getModuleFlagsMetadata()
##

proc copyModuleFlagsMetadata*(m: ModuleRef; len: ptr csize): ptr ModuleFlagEntry {.
    importc: "LLVMCopyModuleFlagsMetadata", dynlib: LLVMLib.}
## *
##  Destroys module flags metadata entries.
##

proc disposeModuleFlagsMetadata*(entries: ptr ModuleFlagEntry) {.
    importc: "LLVMDisposeModuleFlagsMetadata", dynlib: LLVMLib.}
## *
##  Returns the flag behavior for a module flag entry at a specific index.
##
##  @see Module::ModuleFlagEntry::Behavior
##

proc moduleFlagEntriesGetFlagBehavior*(entries: ptr ModuleFlagEntry; index: cuint): ModuleFlagBehavior {.
    importc: "LLVMModuleFlagEntriesGetFlagBehavior", dynlib: LLVMLib.}
## *
##  Returns the key for a module flag entry at a specific index.
##
##  @see Module::ModuleFlagEntry::Key
##

proc moduleFlagEntriesGetKey*(entries: ptr ModuleFlagEntry; index: cuint;
                             len: ptr csize): cstring {.
    importc: "LLVMModuleFlagEntriesGetKey", dynlib: LLVMLib.}
## *
##  Returns the metadata for a module flag entry at a specific index.
##
##  @see Module::ModuleFlagEntry::Val
##

proc moduleFlagEntriesGetMetadata*(entries: ptr ModuleFlagEntry; index: cuint): MetadataRef {.
    importc: "LLVMModuleFlagEntriesGetMetadata", dynlib: LLVMLib.}
## *
##  Add a module-level flag to the module-level flags metadata if it doesn't
##  already exist.
##
##  @see Module::getModuleFlag()
##

proc getModuleFlag*(m: ModuleRef; key: cstring; keyLen: csize): MetadataRef {.
    importc: "LLVMGetModuleFlag", dynlib: LLVMLib.}
## *
##  Add a module-level flag to the module-level flags metadata if it doesn't
##  already exist.
##
##  @see Module::addModuleFlag()
##

proc addModuleFlag*(m: ModuleRef; behavior: ModuleFlagBehavior; key: cstring;
                   keyLen: csize; val: MetadataRef) {.importc: "LLVMAddModuleFlag",
    dynlib: LLVMLib.}
## *
##  Dump a representation of a module to stderr.
##
##  @see Module::dump()
##

proc dumpModule*(m: ModuleRef) {.importc: "LLVMDumpModule", dynlib: LLVMLib.}
## *
##  Print a representation of a module to a file. The ErrorMessage needs to be
##  disposed with LLVMDisposeMessage. Returns 0 on success, 1 otherwise.
##
##  @see Module::print()
##

proc printModuleToFile*(m: ModuleRef; filename: cstring; errorMessage: cstringArray): Bool {.
    importc: "LLVMPrintModuleToFile", dynlib: LLVMLib.}
## *
##  Return a string representation of the module. Use
##  LLVMDisposeMessage to free the string.
##
##  @see Module::print()
##

proc printModuleToString*(m: ModuleRef): cstring {.
    importc: "LLVMPrintModuleToString", dynlib: LLVMLib.}
## *
##  Get inline assembly for a module.
##
##  @see Module::getModuleInlineAsm()
##

proc getModuleInlineAsm*(m: ModuleRef; len: ptr csize): cstring {.
    importc: "LLVMGetModuleInlineAsm", dynlib: LLVMLib.}
## *
##  Set inline assembly for a module.
##
##  @see Module::setModuleInlineAsm()
##

proc setModuleInlineAsm2*(m: ModuleRef; `asm`: cstring; len: csize) {.
    importc: "LLVMSetModuleInlineAsm2", dynlib: LLVMLib.}
## *
##  Append inline assembly to a module.
##
##  @see Module::appendModuleInlineAsm()
##

proc appendModuleInlineAsm*(m: ModuleRef; `asm`: cstring; len: csize) {.
    importc: "LLVMAppendModuleInlineAsm", dynlib: LLVMLib.}
## *
##  Create the specified uniqued inline asm string.
##
##  @see InlineAsm::get()
##

proc getInlineAsm*(ty: TypeRef; asmString: cstring; asmStringSize: csize;
                  constraints: cstring; constraintsSize: csize;
                  hasSideEffects: Bool; isAlignStack: Bool;
                  dialect: InlineAsmDialect): ValueRef {.
    importc: "LLVMGetInlineAsm", dynlib: LLVMLib.}
## *
##  Obtain the context to which this module is associated.
##
##  @see Module::getContext()
##

proc getModuleContext*(m: ModuleRef): ContextRef {.importc: "LLVMGetModuleContext",
    dynlib: LLVMLib.}
## *
##  Obtain a Type from a module by its registered name.
##

proc getTypeByName*(m: ModuleRef; name: cstring): TypeRef {.
    importc: "LLVMGetTypeByName", dynlib: LLVMLib.}
## *
##  Obtain an iterator to the first NamedMDNode in a Module.
##
##  @see llvm::Module::named_metadata_begin()
##

proc getFirstNamedMetadata*(m: ModuleRef): NamedMDNodeRef {.
    importc: "LLVMGetFirstNamedMetadata", dynlib: LLVMLib.}
## *
##  Obtain an iterator to the last NamedMDNode in a Module.
##
##  @see llvm::Module::named_metadata_end()
##

proc getLastNamedMetadata*(m: ModuleRef): NamedMDNodeRef {.
    importc: "LLVMGetLastNamedMetadata", dynlib: LLVMLib.}
## *
##  Advance a NamedMDNode iterator to the next NamedMDNode.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  named metadata nodes.
##

proc getNextNamedMetadata*(namedMDNode: NamedMDNodeRef): NamedMDNodeRef {.
    importc: "LLVMGetNextNamedMetadata", dynlib: LLVMLib.}
## *
##  Decrement a NamedMDNode iterator to the previous NamedMDNode.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous named metadata nodes.
##

proc getPreviousNamedMetadata*(namedMDNode: NamedMDNodeRef): NamedMDNodeRef {.
    importc: "LLVMGetPreviousNamedMetadata", dynlib: LLVMLib.}
## *
##  Retrieve a NamedMDNode with the given name, returning NULL if no such
##  node exists.
##
##  @see llvm::Module::getNamedMetadata()
##

proc getNamedMetadata*(m: ModuleRef; name: cstring; nameLen: csize): NamedMDNodeRef {.
    importc: "LLVMGetNamedMetadata", dynlib: LLVMLib.}
## *
##  Retrieve a NamedMDNode with the given name, creating a new node if no such
##  node exists.
##
##  @see llvm::Module::getOrInsertNamedMetadata()
##

proc getOrInsertNamedMetadata*(m: ModuleRef; name: cstring; nameLen: csize): NamedMDNodeRef {.
    importc: "LLVMGetOrInsertNamedMetadata", dynlib: LLVMLib.}
## *
##  Retrieve the name of a NamedMDNode.
##
##  @see llvm::NamedMDNode::getName()
##

proc getNamedMetadataName*(namedMD: NamedMDNodeRef; nameLen: ptr csize): cstring {.
    importc: "LLVMGetNamedMetadataName", dynlib: LLVMLib.}
## *
##  Obtain the number of operands for named metadata in a module.
##
##  @see llvm::Module::getNamedMetadata()
##

proc getNamedMetadataNumOperands*(m: ModuleRef; name: cstring): cuint {.
    importc: "LLVMGetNamedMetadataNumOperands", dynlib: LLVMLib.}
## *
##  Obtain the named metadata operands for a module.
##
##  The passed LLVMValueRef pointer should refer to an array of
##  LLVMValueRef at least LLVMGetNamedMetadataNumOperands long. This
##  array will be populated with the LLVMValueRef instances. Each
##  instance corresponds to a llvm::MDNode.
##
##  @see llvm::Module::getNamedMetadata()
##  @see llvm::MDNode::getOperand()
##

proc getNamedMetadataOperands*(m: ModuleRef; name: cstring; dest: ptr ValueRef) {.
    importc: "LLVMGetNamedMetadataOperands", dynlib: LLVMLib.}
## *
##  Add an operand to named metadata.
##
##  @see llvm::Module::getNamedMetadata()
##  @see llvm::MDNode::addOperand()
##

proc addNamedMetadataOperand*(m: ModuleRef; name: cstring; val: ValueRef) {.
    importc: "LLVMAddNamedMetadataOperand", dynlib: LLVMLib.}
## *
##  Return the directory of the debug location for this value, which must be
##  an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
##
##  @see llvm::Instruction::getDebugLoc()
##  @see llvm::GlobalVariable::getDebugInfo()
##  @see llvm::Function::getSubprogram()
##

proc getDebugLocDirectory*(val: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetDebugLocDirectory", dynlib: LLVMLib.}
## *
##  Return the filename of the debug location for this value, which must be
##  an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
##
##  @see llvm::Instruction::getDebugLoc()
##  @see llvm::GlobalVariable::getDebugInfo()
##  @see llvm::Function::getSubprogram()
##

proc getDebugLocFilename*(val: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetDebugLocFilename", dynlib: LLVMLib.}
## *
##  Return the line number of the debug location for this value, which must be
##  an llvm::Instruction, llvm::GlobalVariable, or llvm::Function.
##
##  @see llvm::Instruction::getDebugLoc()
##  @see llvm::GlobalVariable::getDebugInfo()
##  @see llvm::Function::getSubprogram()
##

proc getDebugLocLine*(val: ValueRef): cuint {.importc: "LLVMGetDebugLocLine",
    dynlib: LLVMLib.}
## *
##  Return the column number of the debug location for this value, which must be
##  an llvm::Instruction.
##
##  @see llvm::Instruction::getDebugLoc()
##

proc getDebugLocColumn*(val: ValueRef): cuint {.importc: "LLVMGetDebugLocColumn",
    dynlib: LLVMLib.}
## *
##  Add a function to a module under a specified name.
##
##  @see llvm::Function::Create()
##

proc addFunction*(m: ModuleRef; name: cstring; functionTy: TypeRef): ValueRef {.
    importc: "LLVMAddFunction", dynlib: LLVMLib.}
## *
##  Obtain a Function value from a Module by its name.
##
##  The returned value corresponds to a llvm::Function value.
##
##  @see llvm::Module::getFunction()
##

proc getNamedFunction*(m: ModuleRef; name: cstring): ValueRef {.
    importc: "LLVMGetNamedFunction", dynlib: LLVMLib.}
## *
##  Obtain an iterator to the first Function in a Module.
##
##  @see llvm::Module::begin()
##

proc getFirstFunction*(m: ModuleRef): ValueRef {.importc: "LLVMGetFirstFunction",
    dynlib: LLVMLib.}
## *
##  Obtain an iterator to the last Function in a Module.
##
##  @see llvm::Module::end()
##

proc getLastFunction*(m: ModuleRef): ValueRef {.importc: "LLVMGetLastFunction",
    dynlib: LLVMLib.}
## *
##  Advance a Function iterator to the next Function.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  functions.
##

proc getNextFunction*(fn: ValueRef): ValueRef {.importc: "LLVMGetNextFunction",
    dynlib: LLVMLib.}
## *
##  Decrement a Function iterator to the previous Function.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous functions.
##

proc getPreviousFunction*(fn: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousFunction", dynlib: LLVMLib.}
## * Deprecated: Use LLVMSetModuleInlineAsm2 instead.

proc setModuleInlineAsm*(m: ModuleRef; `asm`: cstring) {.
    importc: "LLVMSetModuleInlineAsm", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreType Types
##
##  Types represent the type of a value.
##
##  Types are associated with a context instance. The context internally
##  deduplicates types so there is only 1 instance of a specific type
##  alive at a time. In other words, a unique type is shared among all
##  consumers within a context.
##
##  A Type in the C API corresponds to llvm::Type.
##
##  Types have the following hierarchy:
##
##    types:
##      integer type
##      real type
##      function type
##      sequence types:
##        array type
##        pointer type
##        vector type
##      void type
##      label type
##      opaque type
##
##  @{
##
## *
##  Obtain the enumerated type of a Type instance.
##
##  @see llvm::Type:getTypeID()
##

proc getTypeKind*(ty: TypeRef): TypeKind {.importc: "LLVMGetTypeKind", dynlib: LLVMLib.}
## *
##  Whether the type has a known size.
##
##  Things that don't have a size are abstract types, labels, and void.a
##
##  @see llvm::Type::isSized()
##

proc typeIsSized*(ty: TypeRef): Bool {.importc: "LLVMTypeIsSized", dynlib: LLVMLib.}
## *
##  Obtain the context to which this type instance is associated.
##
##  @see llvm::Type::getContext()
##

proc getTypeContext*(ty: TypeRef): ContextRef {.importc: "LLVMGetTypeContext",
    dynlib: LLVMLib.}
## *
##  Dump a representation of a type to stderr.
##
##  @see llvm::Type::dump()
##

proc dumpType*(val: TypeRef) {.importc: "LLVMDumpType", dynlib: LLVMLib.}
## *
##  Return a string representation of the type. Use
##  LLVMDisposeMessage to free the string.
##
##  @see llvm::Type::print()
##

proc printTypeToString*(val: TypeRef): cstring {.importc: "LLVMPrintTypeToString",
    dynlib: LLVMLib.}
## *
##  @defgroup LLVMCCoreTypeInt Integer Types
##
##  Functions in this section operate on integer types.
##
##  @{
##
## *
##  Obtain an integer type from a context with specified bit width.
##

proc int1TypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMInt1TypeInContext",
    dynlib: LLVMLib.}
proc int8TypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMInt8TypeInContext",
    dynlib: LLVMLib.}
proc int16TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt16TypeInContext", dynlib: LLVMLib.}
proc int32TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt32TypeInContext", dynlib: LLVMLib.}
proc int64TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt64TypeInContext", dynlib: LLVMLib.}
proc int128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMInt128TypeInContext", dynlib: LLVMLib.}
proc intTypeInContext*(c: ContextRef; numBits: cuint): TypeRef {.
    importc: "LLVMIntTypeInContext", dynlib: LLVMLib.}
## *
##  Obtain an integer type from the global context with a specified bit
##  width.
##

proc int1Type*(): TypeRef {.importc: "LLVMInt1Type", dynlib: LLVMLib.}
proc int8Type*(): TypeRef {.importc: "LLVMInt8Type", dynlib: LLVMLib.}
proc int16Type*(): TypeRef {.importc: "LLVMInt16Type", dynlib: LLVMLib.}
proc int32Type*(): TypeRef {.importc: "LLVMInt32Type", dynlib: LLVMLib.}
proc int64Type*(): TypeRef {.importc: "LLVMInt64Type", dynlib: LLVMLib.}
proc int128Type*(): TypeRef {.importc: "LLVMInt128Type", dynlib: LLVMLib.}
proc intType*(numBits: cuint): TypeRef {.importc: "LLVMIntType", dynlib: LLVMLib.}
proc getIntTypeWidth*(integerTy: TypeRef): cuint {.importc: "LLVMGetIntTypeWidth",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeFloat Floating Point Types
##
##  @{
##
## *
##  Obtain a 16-bit floating point type from a context.
##

proc halfTypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMHalfTypeInContext",
    dynlib: LLVMLib.}
## *
##  Obtain a 32-bit floating point type from a context.
##

proc floatTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMFloatTypeInContext", dynlib: LLVMLib.}
## *
##  Obtain a 64-bit floating point type from a context.
##

proc doubleTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMDoubleTypeInContext", dynlib: LLVMLib.}
## *
##  Obtain a 80-bit floating point type (X87) from a context.
##

proc x86FP80TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86FP80TypeInContext", dynlib: LLVMLib.}
## *
##  Obtain a 128-bit floating point type (112-bit mantissa) from a
##  context.
##

proc fP128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMFP128TypeInContext", dynlib: LLVMLib.}
## *
##  Obtain a 128-bit floating point type (two 64-bits) from a context.
##

proc pPCFP128TypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMPPCFP128TypeInContext", dynlib: LLVMLib.}
## *
##  Obtain a floating point type from the global context.
##
##  These map to the functions in this group of the same name.
##

proc halfType*(): TypeRef {.importc: "LLVMHalfType", dynlib: LLVMLib.}
proc floatType*(): TypeRef {.importc: "LLVMFloatType", dynlib: LLVMLib.}
proc doubleType*(): TypeRef {.importc: "LLVMDoubleType", dynlib: LLVMLib.}
proc x86FP80Type*(): TypeRef {.importc: "LLVMX86FP80Type", dynlib: LLVMLib.}
proc fP128Type*(): TypeRef {.importc: "LLVMFP128Type", dynlib: LLVMLib.}
proc pPCFP128Type*(): TypeRef {.importc: "LLVMPPCFP128Type", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeFunction Function Types
##
##  @{
##
## *
##  Obtain a function type consisting of a specified signature.
##
##  The function is defined as a tuple of a return Type, a list of
##  parameter types, and whether the function is variadic.
##

proc functionType*(returnType: TypeRef; paramTypes: ptr TypeRef; paramCount: cuint;
                  isVarArg: Bool): TypeRef {.importc: "LLVMFunctionType",
    dynlib: LLVMLib.}
## *
##  Returns whether a function type is variadic.
##

proc isFunctionVarArg*(functionTy: TypeRef): Bool {.importc: "LLVMIsFunctionVarArg",
    dynlib: LLVMLib.}
## *
##  Obtain the Type this function Type returns.
##

proc getReturnType*(functionTy: TypeRef): TypeRef {.importc: "LLVMGetReturnType",
    dynlib: LLVMLib.}
## *
##  Obtain the number of parameters this function accepts.
##

proc countParamTypes*(functionTy: TypeRef): cuint {.importc: "LLVMCountParamTypes",
    dynlib: LLVMLib.}
## *
##  Obtain the types of a function's parameters.
##
##  The Dest parameter should point to a pre-allocated array of
##  LLVMTypeRef at least LLVMCountParamTypes() large. On return, the
##  first LLVMCountParamTypes() entries in the array will be populated
##  with LLVMTypeRef instances.
##
##  @param FunctionTy The function type to operate on.
##  @param Dest Memory address of an array to be filled with result.
##

proc getParamTypes*(functionTy: TypeRef; dest: ptr TypeRef) {.
    importc: "LLVMGetParamTypes", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeStruct Structure Types
##
##  These functions relate to LLVMTypeRef instances.
##
##  @see llvm::StructType
##
##  @{
##
## *
##  Create a new structure type in a context.
##
##  A structure is specified by a list of inner elements/types and
##  whether these can be packed together.
##
##  @see llvm::StructType::create()
##

proc structTypeInContext*(c: ContextRef; elementTypes: ptr TypeRef;
                         elementCount: cuint; packed: Bool): TypeRef {.
    importc: "LLVMStructTypeInContext", dynlib: LLVMLib.}
## *
##  Create a new structure type in the global context.
##
##  @see llvm::StructType::create()
##

proc structType*(elementTypes: ptr TypeRef; elementCount: cuint; packed: Bool): TypeRef {.
    importc: "LLVMStructType", dynlib: LLVMLib.}
## *
##  Create an empty structure in a context having a specified name.
##
##  @see llvm::StructType::create()
##

proc structCreateNamed*(c: ContextRef; name: cstring): TypeRef {.
    importc: "LLVMStructCreateNamed", dynlib: LLVMLib.}
## *
##  Obtain the name of a structure.
##
##  @see llvm::StructType::getName()
##

proc getStructName*(ty: TypeRef): cstring {.importc: "LLVMGetStructName",
                                        dynlib: LLVMLib.}
## *
##  Set the contents of a structure type.
##
##  @see llvm::StructType::setBody()
##

proc structSetBody*(structTy: TypeRef; elementTypes: ptr TypeRef; elementCount: cuint;
                   packed: Bool) {.importc: "LLVMStructSetBody", dynlib: LLVMLib.}
## *
##  Get the number of elements defined inside the structure.
##
##  @see llvm::StructType::getNumElements()
##

proc countStructElementTypes*(structTy: TypeRef): cuint {.
    importc: "LLVMCountStructElementTypes", dynlib: LLVMLib.}
## *
##  Get the elements within a structure.
##
##  The function is passed the address of a pre-allocated array of
##  LLVMTypeRef at least LLVMCountStructElementTypes() long. After
##  invocation, this array will be populated with the structure's
##  elements. The objects in the destination array will have a lifetime
##  of the structure type itself, which is the lifetime of the context it
##  is contained in.
##

proc getStructElementTypes*(structTy: TypeRef; dest: ptr TypeRef) {.
    importc: "LLVMGetStructElementTypes", dynlib: LLVMLib.}
## *
##  Get the type of the element at a given index in the structure.
##
##  @see llvm::StructType::getTypeAtIndex()
##

proc structGetTypeAtIndex*(structTy: TypeRef; i: cuint): TypeRef {.
    importc: "LLVMStructGetTypeAtIndex", dynlib: LLVMLib.}
## *
##  Determine whether a structure is packed.
##
##  @see llvm::StructType::isPacked()
##

proc isPackedStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsPackedStruct",
    dynlib: LLVMLib.}
## *
##  Determine whether a structure is opaque.
##
##  @see llvm::StructType::isOpaque()
##

proc isOpaqueStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsOpaqueStruct",
    dynlib: LLVMLib.}
## *
##  Determine whether a structure is literal.
##
##  @see llvm::StructType::isLiteral()
##

proc isLiteralStruct*(structTy: TypeRef): Bool {.importc: "LLVMIsLiteralStruct",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeSequential Sequential Types
##
##  Sequential types represents "arrays" of types. This is a super class
##  for array, vector, and pointer types.
##
##  @{
##
## *
##  Obtain the type of elements within a sequential type.
##
##  This works on array, vector, and pointer types.
##
##  @see llvm::SequentialType::getElementType()
##

proc getElementType*(ty: TypeRef): TypeRef {.importc: "LLVMGetElementType",
    dynlib: LLVMLib.}
## *
##  Returns type's subtypes
##
##  @see llvm::Type::subtypes()
##

proc getSubtypes*(tp: TypeRef; arr: ptr TypeRef) {.importc: "LLVMGetSubtypes",
    dynlib: LLVMLib.}
## *
##   Return the number of types in the derived type.
##
##  @see llvm::Type::getNumContainedTypes()
##

proc getNumContainedTypes*(tp: TypeRef): cuint {.
    importc: "LLVMGetNumContainedTypes", dynlib: LLVMLib.}
## *
##  Create a fixed size array type that refers to a specific type.
##
##  The created type will exist in the context that its element type
##  exists in.
##
##  @see llvm::ArrayType::get()
##

proc arrayType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMArrayType", dynlib: LLVMLib.}
## *
##  Obtain the length of an array type.
##
##  This only works on types that represent arrays.
##
##  @see llvm::ArrayType::getNumElements()
##

proc getArrayLength*(arrayTy: TypeRef): cuint {.importc: "LLVMGetArrayLength",
    dynlib: LLVMLib.}
## *
##  Create a pointer type that points to a defined type.
##
##  The created type will exist in the context that its pointee type
##  exists in.
##
##  @see llvm::PointerType::get()
##

proc pointerType*(elementType: TypeRef; addressSpace: cuint): TypeRef {.
    importc: "LLVMPointerType", dynlib: LLVMLib.}
## *
##  Obtain the address space of a pointer type.
##
##  This only works on types that represent pointers.
##
##  @see llvm::PointerType::getAddressSpace()
##

proc getPointerAddressSpace*(pointerTy: TypeRef): cuint {.
    importc: "LLVMGetPointerAddressSpace", dynlib: LLVMLib.}
## *
##  Create a vector type that contains a defined type and has a specific
##  number of elements.
##
##  The created type will exist in the context thats its element type
##  exists in.
##
##  @see llvm::VectorType::get()
##

proc vectorType*(elementType: TypeRef; elementCount: cuint): TypeRef {.
    importc: "LLVMVectorType", dynlib: LLVMLib.}
## *
##  Obtain the number of elements in a vector type.
##
##  This only works on types that represent vectors.
##
##  @see llvm::VectorType::getNumElements()
##

proc getVectorSize*(vectorTy: TypeRef): cuint {.importc: "LLVMGetVectorSize",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreTypeOther Other Types
##
##  @{
##
## *
##  Create a void type in a context.
##

proc voidTypeInContext*(c: ContextRef): TypeRef {.importc: "LLVMVoidTypeInContext",
    dynlib: LLVMLib.}
## *
##  Create a label type in a context.
##

proc labelTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMLabelTypeInContext", dynlib: LLVMLib.}
## *
##  Create a X86 MMX type in a context.
##

proc x86MMXTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMX86MMXTypeInContext", dynlib: LLVMLib.}
## *
##  Create a token type in a context.
##

proc tokenTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMTokenTypeInContext", dynlib: LLVMLib.}
## *
##  Create a metadata type in a context.
##

proc metadataTypeInContext*(c: ContextRef): TypeRef {.
    importc: "LLVMMetadataTypeInContext", dynlib: LLVMLib.}
## *
##  These are similar to the above functions except they operate on the
##  global context.
##

proc voidType*(): TypeRef {.importc: "LLVMVoidType", dynlib: LLVMLib.}
proc labelType*(): TypeRef {.importc: "LLVMLabelType", dynlib: LLVMLib.}
proc x86MMXType*(): TypeRef {.importc: "LLVMX86MMXType", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValues Values
##
##  The bulk of LLVM's object model consists of values, which comprise a very
##  rich type hierarchy.
##
##  LLVMValueRef essentially represents llvm::Value. There is a rich
##  hierarchy of classes within this type. Depending on the instance
##  obtained, not all APIs are available.
##
##  Callers can determine the type of an LLVMValueRef by calling the
##  LLVMIsA* family of functions (e.g. LLVMIsAArgument()). These
##  functions are defined by a macro, so it isn't obvious which are
##  available by looking at the Doxygen source code. Instead, look at the
##  source definition of LLVM_FOR_EACH_VALUE_SUBCLASS and note the list
##  of value names given. These value names also correspond to classes in
##  the llvm::Value hierarchy.
##
##  @{
##
##
## #define LLVM_FOR_EACH_VALUE_SUBCLASS(macro) \
##   macro(Argument)                           \
##   macro(BasicBlock)                         \
##   macro(InlineAsm)                          \
##   macro(User)                               \
##     macro(Constant)                         \
##       macro(BlockAddress)                   \
##       macro(ConstantAggregateZero)          \
##       macro(ConstantArray)                  \
##       macro(ConstantDataSequential)         \
##         macro(ConstantDataArray)            \
##         macro(ConstantDataVector)           \
##       macro(ConstantExpr)                   \
##       macro(ConstantFP)                     \
##       macro(ConstantInt)                    \
##       macro(ConstantPointerNull)            \
##       macro(ConstantStruct)                 \
##       macro(ConstantTokenNone)              \
##       macro(ConstantVector)                 \
##       macro(GlobalValue)                    \
##         macro(GlobalAlias)                  \
##         macro(GlobalIFunc)                  \
##         macro(GlobalObject)                 \
##           macro(Function)                   \
##           macro(GlobalVariable)             \
##       macro(UndefValue)                     \
##     macro(Instruction)                      \
##       macro(BinaryOperator)                 \
##       macro(CallInst)                       \
##         macro(IntrinsicInst)                \
##           macro(DbgInfoIntrinsic)           \
##             macro(DbgVariableIntrinsic)     \
##               macro(DbgDeclareInst)         \
##             macro(DbgLabelInst)             \
##           macro(MemIntrinsic)               \
##             macro(MemCpyInst)               \
##             macro(MemMoveInst)              \
##             macro(MemSetInst)               \
##       macro(CmpInst)                        \
##         macro(FCmpInst)                     \
##         macro(ICmpInst)                     \
##       macro(ExtractElementInst)             \
##       macro(GetElementPtrInst)              \
##       macro(InsertElementInst)              \
##       macro(InsertValueInst)                \
##       macro(LandingPadInst)                 \
##       macro(PHINode)                        \
##       macro(SelectInst)                     \
##       macro(ShuffleVectorInst)              \
##       macro(StoreInst)                      \
##       macro(BranchInst)                     \
##       macro(IndirectBrInst)                 \
##       macro(InvokeInst)                     \
##       macro(ReturnInst)                     \
##       macro(SwitchInst)                     \
##       macro(UnreachableInst)                \
##       macro(ResumeInst)                     \
##       macro(CleanupReturnInst)              \
##       macro(CatchReturnInst)                \
##       macro(FuncletPadInst)                 \
##         macro(CatchPadInst)                 \
##         macro(CleanupPadInst)               \
##       macro(UnaryInstruction)               \
##         macro(AllocaInst)                   \
##         macro(CastInst)                     \
##           macro(AddrSpaceCastInst)          \
##           macro(BitCastInst)                \
##           macro(FPExtInst)                  \
##           macro(FPToSIInst)                 \
##           macro(FPToUIInst)                 \
##           macro(FPTruncInst)                \
##           macro(IntToPtrInst)               \
##           macro(PtrToIntInst)               \
##           macro(SExtInst)                   \
##           macro(SIToFPInst)                 \
##           macro(TruncInst)                  \
##           macro(UIToFPInst)                 \
##           macro(ZExtInst)                   \
##         macro(ExtractValueInst)             \
##         macro(LoadInst)                     \
##         macro(VAArgInst)
##
## *
##  @defgroup LLVMCCoreValueGeneral General APIs
##
##  Functions in this section work on all LLVMValueRef instances,
##  regardless of their sub-type. They correspond to functions available
##  on llvm::Value.
##
##  @{
##
## *
##  Obtain the type of a value.
##
##  @see llvm::Value::getType()
##

proc typeOfX*(val: ValueRef): TypeRef {.importc: "LLVMTypeOf", dynlib: LLVMLib.}
## *
##  Obtain the enumerated type of a Value instance.
##
##  @see llvm::Value::getValueID()
##

proc getValueKind*(val: ValueRef): ValueKind {.importc: "LLVMGetValueKind",
    dynlib: LLVMLib.}
## *
##  Obtain the string name of a value.
##
##  @see llvm::Value::getName()
##

proc getValueName2*(val: ValueRef; length: ptr csize): cstring {.
    importc: "LLVMGetValueName2", dynlib: LLVMLib.}
## *
##  Set the string name of a value.
##
##  @see llvm::Value::setName()
##

proc setValueName2*(val: ValueRef; name: cstring; nameLen: csize) {.
    importc: "LLVMSetValueName2", dynlib: LLVMLib.}
## *
##  Dump a representation of a value to stderr.
##
##  @see llvm::Value::dump()
##

proc dumpValue*(val: ValueRef) {.importc: "LLVMDumpValue", dynlib: LLVMLib.}
## *
##  Return a string representation of the value. Use
##  LLVMDisposeMessage to free the string.
##
##  @see llvm::Value::print()
##

proc printValueToString*(val: ValueRef): cstring {.
    importc: "LLVMPrintValueToString", dynlib: LLVMLib.}
## *
##  Replace all uses of a value with another one.
##
##  @see llvm::Value::replaceAllUsesWith()
##

proc replaceAllUsesWith*(oldVal: ValueRef; newVal: ValueRef) {.
    importc: "LLVMReplaceAllUsesWith", dynlib: LLVMLib.}
## *
##  Determine whether the specified value instance is constant.
##

proc isConstant*(val: ValueRef): Bool {.importc: "LLVMIsConstant", dynlib: LLVMLib.}
## *
##  Determine whether a value instance is undefined.
##

proc isUndef*(val: ValueRef): Bool {.importc: "LLVMIsUndef", dynlib: LLVMLib.}
## *
##  Convert value instances between types.
##
##  Internally, an LLVMValueRef is "pinned" to a specific type. This
##  series of functions allows you to cast an instance to a specific
##  type.
##
##  If the cast is not valid for the specified type, NULL is returned.
##
##  @see llvm::dyn_cast_or_null<>
##
## #define LLVM_DECLARE_VALUE_CAST(name) \
##   LLVMValueRef LLVMIsA##name(LLVMValueRef Val);
## LLVM_FOR_EACH_VALUE_SUBCLASS(LLVM_DECLARE_VALUE_CAST)

proc isAMDNode*(val: ValueRef): ValueRef {.importc: "LLVMIsAMDNode", dynlib: LLVMLib.}
proc isAMDString*(val: ValueRef): ValueRef {.importc: "LLVMIsAMDString",
    dynlib: LLVMLib.}
## * Deprecated: Use LLVMGetValueName2 instead.

proc getValueName*(val: ValueRef): cstring {.importc: "LLVMGetValueName",
    dynlib: LLVMLib.}
## * Deprecated: Use LLVMSetValueName2 instead.

proc setValueName*(val: ValueRef; name: cstring) {.importc: "LLVMSetValueName",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueUses Usage
##
##  This module defines functions that allow you to inspect the uses of a
##  LLVMValueRef.
##
##  It is possible to obtain an LLVMUseRef for any LLVMValueRef instance.
##  Each LLVMUseRef (which corresponds to a llvm::Use instance) holds a
##  llvm::User and llvm::Value.
##
##  @{
##
## *
##  Obtain the first use of a value.
##
##  Uses are obtained in an iterator fashion. First, call this function
##  to obtain a reference to the first use. Then, call LLVMGetNextUse()
##  on that instance and all subsequently obtained instances until
##  LLVMGetNextUse() returns NULL.
##
##  @see llvm::Value::use_begin()
##

proc getFirstUse*(val: ValueRef): UseRef {.importc: "LLVMGetFirstUse", dynlib: LLVMLib.}
## *
##  Obtain the next use of a value.
##
##  This effectively advances the iterator. It returns NULL if you are on
##  the final use and no more are available.
##

proc getNextUse*(u: UseRef): UseRef {.importc: "LLVMGetNextUse", dynlib: LLVMLib.}
## *
##  Obtain the user value for a user.
##
##  The returned value corresponds to a llvm::User type.
##
##  @see llvm::Use::getUser()
##

proc getUser*(u: UseRef): ValueRef {.importc: "LLVMGetUser", dynlib: LLVMLib.}
## *
##  Obtain the value this use corresponds to.
##
##  @see llvm::Use::get().
##

proc getUsedValue*(u: UseRef): ValueRef {.importc: "LLVMGetUsedValue", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueUser User value
##
##  Function in this group pertain to LLVMValueRef instances that descent
##  from llvm::User. This includes constants, instructions, and
##  operators.
##
##  @{
##
## *
##  Obtain an operand at a specific index in a llvm::User value.
##
##  @see llvm::User::getOperand()
##

proc getOperand*(val: ValueRef; index: cuint): ValueRef {.importc: "LLVMGetOperand",
    dynlib: LLVMLib.}
## *
##  Obtain the use of an operand at a specific index in a llvm::User value.
##
##  @see llvm::User::getOperandUse()
##

proc getOperandUse*(val: ValueRef; index: cuint): UseRef {.
    importc: "LLVMGetOperandUse", dynlib: LLVMLib.}
## *
##  Set an operand at a specific index in a llvm::User value.
##
##  @see llvm::User::setOperand()
##

proc setOperand*(user: ValueRef; index: cuint; val: ValueRef) {.
    importc: "LLVMSetOperand", dynlib: LLVMLib.}
## *
##  Obtain the number of operands in a llvm::User value.
##
##  @see llvm::User::getNumOperands()
##

proc getNumOperands*(val: ValueRef): cint {.importc: "LLVMGetNumOperands",
                                        dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstant Constants
##
##  This section contains APIs for interacting with LLVMValueRef that
##  correspond to llvm::Constant instances.
##
##  These functions will work for any LLVMValueRef in the llvm::Constant
##  class hierarchy.
##
##  @{
##
## *
##  Obtain a constant value referring to the null instance of a type.
##
##  @see llvm::Constant::getNullValue()
##

proc constNull*(ty: TypeRef): ValueRef {.importc: "LLVMConstNull", dynlib: LLVMLib.}
##  all zeroes
## *
##  Obtain a constant value referring to the instance of a type
##  consisting of all ones.
##
##  This is only valid for integer types.
##
##  @see llvm::Constant::getAllOnesValue()
##

proc constAllOnes*(ty: TypeRef): ValueRef {.importc: "LLVMConstAllOnes",
                                        dynlib: LLVMLib.}
## *
##  Obtain a constant value referring to an undefined value of a type.
##
##  @see llvm::UndefValue::get()
##

proc getUndef*(ty: TypeRef): ValueRef {.importc: "LLVMGetUndef", dynlib: LLVMLib.}
## *
##  Determine whether a value instance is null.
##
##  @see llvm::Constant::isNullValue()
##

proc isNull*(val: ValueRef): Bool {.importc: "LLVMIsNull", dynlib: LLVMLib.}
## *
##  Obtain a constant that is a constant pointer pointing to NULL for a
##  specified type.
##

proc constPointerNull*(ty: TypeRef): ValueRef {.importc: "LLVMConstPointerNull",
    dynlib: LLVMLib.}
## *
##  @defgroup LLVMCCoreValueConstantScalar Scalar constants
##
##  Functions in this group model LLVMValueRef instances that correspond
##  to constants referring to scalar types.
##
##  For integer types, the LLVMTypeRef parameter should correspond to a
##  llvm::IntegerType instance and the returned LLVMValueRef will
##  correspond to a llvm::ConstantInt.
##
##  For floating point types, the LLVMTypeRef returned corresponds to a
##  llvm::ConstantFP.
##
##  @{
##
## *
##  Obtain a constant value for an integer type.
##
##  The returned value corresponds to a llvm::ConstantInt.
##
##  @see llvm::ConstantInt::get()
##
##  @param IntTy Integer type to obtain value of.
##  @param N The value the returned instance should refer to.
##  @param SignExtend Whether to sign extend the produced value.
##

proc constInt*(intTy: TypeRef; n: culonglong; signExtend: Bool): ValueRef {.
    importc: "LLVMConstInt", dynlib: LLVMLib.}
## *
##  Obtain a constant value for an integer of arbitrary precision.
##
##  @see llvm::ConstantInt::get()
##

proc constIntOfArbitraryPrecision*(intTy: TypeRef; numWords: cuint;
                                  words: ptr uint64): ValueRef {.
    importc: "LLVMConstIntOfArbitraryPrecision", dynlib: LLVMLib.}
## *
##  Obtain a constant value for an integer parsed from a string.
##
##  A similar API, LLVMConstIntOfStringAndSize is also available. If the
##  string's length is available, it is preferred to call that function
##  instead.
##
##  @see llvm::ConstantInt::get()
##

proc constIntOfString*(intTy: TypeRef; text: cstring; radix: uint8T): ValueRef {.
    importc: "LLVMConstIntOfString", dynlib: LLVMLib.}
## *
##  Obtain a constant value for an integer parsed from a string with
##  specified length.
##
##  @see llvm::ConstantInt::get()
##

proc constIntOfStringAndSize*(intTy: TypeRef; text: cstring; sLen: cuint; radix: uint8T): ValueRef {.
    importc: "LLVMConstIntOfStringAndSize", dynlib: LLVMLib.}
## *
##  Obtain a constant value referring to a double floating point value.
##

proc constReal*(realTy: TypeRef; n: cdouble): ValueRef {.importc: "LLVMConstReal",
    dynlib: LLVMLib.}
## *
##  Obtain a constant for a floating point value parsed from a string.
##
##  A similar API, LLVMConstRealOfStringAndSize is also available. It
##  should be used if the input string's length is known.
##

proc constRealOfString*(realTy: TypeRef; text: cstring): ValueRef {.
    importc: "LLVMConstRealOfString", dynlib: LLVMLib.}
## *
##  Obtain a constant for a floating point value parsed from a string.
##

proc constRealOfStringAndSize*(realTy: TypeRef; text: cstring; sLen: cuint): ValueRef {.
    importc: "LLVMConstRealOfStringAndSize", dynlib: LLVMLib.}
## *
##  Obtain the zero extended value for an integer constant value.
##
##  @see llvm::ConstantInt::getZExtValue()
##

proc constIntGetZExtValue*(constantVal: ValueRef): culonglong {.
    importc: "LLVMConstIntGetZExtValue", dynlib: LLVMLib.}
## *
##  Obtain the sign extended value for an integer constant value.
##
##  @see llvm::ConstantInt::getSExtValue()
##

proc constIntGetSExtValue*(constantVal: ValueRef): clonglong {.
    importc: "LLVMConstIntGetSExtValue", dynlib: LLVMLib.}
## *
##  Obtain the double value for an floating point constant value.
##  losesInfo indicates if some precision was lost in the conversion.
##
##  @see llvm::ConstantFP::getDoubleValue
##

proc constRealGetDouble*(constantVal: ValueRef; losesInfo: ptr Bool): cdouble {.
    importc: "LLVMConstRealGetDouble", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstantComposite Composite Constants
##
##  Functions in this group operate on composite constants.
##
##  @{
##
## *
##  Create a ConstantDataSequential and initialize it with a string.
##
##  @see llvm::ConstantDataArray::getString()
##

proc constStringInContext*(c: ContextRef; str: cstring; length: cuint;
                          dontNullTerminate: Bool): ValueRef {.
    importc: "LLVMConstStringInContext", dynlib: LLVMLib.}
## *
##  Create a ConstantDataSequential with string content in the global context.
##
##  This is the same as LLVMConstStringInContext except it operates on the
##  global context.
##
##  @see LLVMConstStringInContext()
##  @see llvm::ConstantDataArray::getString()
##

proc constString*(str: cstring; length: cuint; dontNullTerminate: Bool): ValueRef {.
    importc: "LLVMConstString", dynlib: LLVMLib.}
## *
##  Returns true if the specified constant is an array of i8.
##
##  @see ConstantDataSequential::getAsString()
##

proc isConstantString*(c: ValueRef): Bool {.importc: "LLVMIsConstantString",
                                        dynlib: LLVMLib.}
## *
##  Get the given constant data sequential as a string.
##
##  @see ConstantDataSequential::getAsString()
##

proc getAsString*(c: ValueRef; length: ptr csize): cstring {.
    importc: "LLVMGetAsString", dynlib: LLVMLib.}
## *
##  Create an anonymous ConstantStruct with the specified values.
##
##  @see llvm::ConstantStruct::getAnon()
##

proc constStructInContext*(c: ContextRef; constantVals: ptr ValueRef; count: cuint;
                          packed: Bool): ValueRef {.
    importc: "LLVMConstStructInContext", dynlib: LLVMLib.}
## *
##  Create a ConstantStruct in the global Context.
##
##  This is the same as LLVMConstStructInContext except it operates on the
##  global Context.
##
##  @see LLVMConstStructInContext()
##

proc constStruct*(constantVals: ptr ValueRef; count: cuint; packed: Bool): ValueRef {.
    importc: "LLVMConstStruct", dynlib: LLVMLib.}
## *
##  Create a ConstantArray from values.
##
##  @see llvm::ConstantArray::get()
##

proc constArray*(elementTy: TypeRef; constantVals: ptr ValueRef; length: cuint): ValueRef {.
    importc: "LLVMConstArray", dynlib: LLVMLib.}
## *
##  Create a non-anonymous ConstantStruct from values.
##
##  @see llvm::ConstantStruct::get()
##

proc constNamedStruct*(structTy: TypeRef; constantVals: ptr ValueRef; count: cuint): ValueRef {.
    importc: "LLVMConstNamedStruct", dynlib: LLVMLib.}
## *
##  Get an element at specified index as a constant.
##
##  @see ConstantDataSequential::getElementAsConstant()
##

proc getElementAsConstant*(c: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetElementAsConstant", dynlib: LLVMLib.}
## *
##  Create a ConstantVector from values.
##
##  @see llvm::ConstantVector::get()
##

proc constVector*(scalarConstantVals: ptr ValueRef; size: cuint): ValueRef {.
    importc: "LLVMConstVector", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstantExpressions Constant Expressions
##
##  Functions in this group correspond to APIs on llvm::ConstantExpr.
##
##  @see llvm::ConstantExpr.
##
##  @{
##

proc getConstOpcode*(constantVal: ValueRef): Opcode {.importc: "LLVMGetConstOpcode",
    dynlib: LLVMLib.}
proc alignOf*(ty: TypeRef): ValueRef {.importc: "LLVMAlignOf", dynlib: LLVMLib.}
proc sizeOfX*(ty: TypeRef): ValueRef {.importc: "LLVMSizeOf", dynlib: LLVMLib.}
proc constNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNeg",
    dynlib: LLVMLib.}
proc constNSWNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNSWNeg",
    dynlib: LLVMLib.}
proc constNUWNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNUWNeg",
    dynlib: LLVMLib.}
proc constFNeg*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstFNeg",
    dynlib: LLVMLib.}
proc constNot*(constantVal: ValueRef): ValueRef {.importc: "LLVMConstNot",
    dynlib: LLVMLib.}
proc constAdd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstAdd", dynlib: LLVMLib.}
proc constNSWAdd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNSWAdd", dynlib: LLVMLib.}
proc constNUWAdd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNUWAdd", dynlib: LLVMLib.}
proc constFAdd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstFAdd", dynlib: LLVMLib.}
proc constSub*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstSub", dynlib: LLVMLib.}
proc constNSWSub*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNSWSub", dynlib: LLVMLib.}
proc constNUWSub*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNUWSub", dynlib: LLVMLib.}
proc constFSub*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstFSub", dynlib: LLVMLib.}
proc constMul*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstMul", dynlib: LLVMLib.}
proc constNSWMul*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNSWMul", dynlib: LLVMLib.}
proc constNUWMul*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstNUWMul", dynlib: LLVMLib.}
proc constFMul*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstFMul", dynlib: LLVMLib.}
proc constUDiv*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstUDiv", dynlib: LLVMLib.}
proc constExactUDiv*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstExactUDiv", dynlib: LLVMLib.}
proc constSDiv*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstSDiv", dynlib: LLVMLib.}
proc constExactSDiv*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstExactSDiv", dynlib: LLVMLib.}
proc constFDiv*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstFDiv", dynlib: LLVMLib.}
proc constURem*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstURem", dynlib: LLVMLib.}
proc constSRem*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstSRem", dynlib: LLVMLib.}
proc constFRem*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstFRem", dynlib: LLVMLib.}
proc constAnd*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstAnd", dynlib: LLVMLib.}
proc constOr*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstOr", dynlib: LLVMLib.}
proc constXor*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstXor", dynlib: LLVMLib.}
proc constICmp*(predicate: IntPredicate; lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstICmp", dynlib: LLVMLib.}
proc constFCmp*(predicate: RealPredicate; lHSConstant: ValueRef;
               rHSConstant: ValueRef): ValueRef {.importc: "LLVMConstFCmp",
    dynlib: LLVMLib.}
proc constShl*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstShl", dynlib: LLVMLib.}
proc constLShr*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstLShr", dynlib: LLVMLib.}
proc constAShr*(lHSConstant: ValueRef; rHSConstant: ValueRef): ValueRef {.
    importc: "LLVMConstAShr", dynlib: LLVMLib.}
proc constGEP*(constantVal: ValueRef; constantIndices: ptr ValueRef; numIndices: cuint): ValueRef {.
    importc: "LLVMConstGEP", dynlib: LLVMLib.}
proc constGEP2*(ty: TypeRef; constantVal: ValueRef; constantIndices: ptr ValueRef;
               numIndices: cuint): ValueRef {.importc: "LLVMConstGEP2",
    dynlib: LLVMLib.}
proc constInBoundsGEP*(constantVal: ValueRef; constantIndices: ptr ValueRef;
                      numIndices: cuint): ValueRef {.
    importc: "LLVMConstInBoundsGEP", dynlib: LLVMLib.}
proc constInBoundsGEP2*(ty: TypeRef; constantVal: ValueRef;
                       constantIndices: ptr ValueRef; numIndices: cuint): ValueRef {.
    importc: "LLVMConstInBoundsGEP2", dynlib: LLVMLib.}
proc constTrunc*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstTrunc", dynlib: LLVMLib.}
proc constSExt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstSExt", dynlib: LLVMLib.}
proc constZExt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstZExt", dynlib: LLVMLib.}
proc constFPTrunc*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPTrunc", dynlib: LLVMLib.}
proc constFPExt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPExt", dynlib: LLVMLib.}
proc constUIToFP*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstUIToFP", dynlib: LLVMLib.}
proc constSIToFP*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstSIToFP", dynlib: LLVMLib.}
proc constFPToUI*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPToUI", dynlib: LLVMLib.}
proc constFPToSI*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPToSI", dynlib: LLVMLib.}
proc constPtrToInt*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstPtrToInt", dynlib: LLVMLib.}
proc constIntToPtr*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstIntToPtr", dynlib: LLVMLib.}
proc constBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstBitCast", dynlib: LLVMLib.}
proc constAddrSpaceCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstAddrSpaceCast", dynlib: LLVMLib.}
proc constZExtOrBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstZExtOrBitCast", dynlib: LLVMLib.}
proc constSExtOrBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstSExtOrBitCast", dynlib: LLVMLib.}
proc constTruncOrBitCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstTruncOrBitCast", dynlib: LLVMLib.}
proc constPointerCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstPointerCast", dynlib: LLVMLib.}
proc constIntCast*(constantVal: ValueRef; toType: TypeRef; isSigned: Bool): ValueRef {.
    importc: "LLVMConstIntCast", dynlib: LLVMLib.}
proc constFPCast*(constantVal: ValueRef; toType: TypeRef): ValueRef {.
    importc: "LLVMConstFPCast", dynlib: LLVMLib.}
proc constSelect*(constantCondition: ValueRef; constantIfTrue: ValueRef;
                 constantIfFalse: ValueRef): ValueRef {.importc: "LLVMConstSelect",
    dynlib: LLVMLib.}
proc constExtractElement*(vectorConstant: ValueRef; indexConstant: ValueRef): ValueRef {.
    importc: "LLVMConstExtractElement", dynlib: LLVMLib.}
proc constInsertElement*(vectorConstant: ValueRef; elementValueConstant: ValueRef;
                        indexConstant: ValueRef): ValueRef {.
    importc: "LLVMConstInsertElement", dynlib: LLVMLib.}
proc constShuffleVector*(vectorAConstant: ValueRef; vectorBConstant: ValueRef;
                        maskConstant: ValueRef): ValueRef {.
    importc: "LLVMConstShuffleVector", dynlib: LLVMLib.}
proc constExtractValue*(aggConstant: ValueRef; idxList: ptr cuint; numIdx: cuint): ValueRef {.
    importc: "LLVMConstExtractValue", dynlib: LLVMLib.}
proc constInsertValue*(aggConstant: ValueRef; elementValueConstant: ValueRef;
                      idxList: ptr cuint; numIdx: cuint): ValueRef {.
    importc: "LLVMConstInsertValue", dynlib: LLVMLib.}
proc blockAddress*(f: ValueRef; bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBlockAddress", dynlib: LLVMLib.}
## * Deprecated: Use LLVMGetInlineAsm instead.

proc constInlineAsm*(ty: TypeRef; asmString: cstring; constraints: cstring;
                    hasSideEffects: Bool; isAlignStack: Bool): ValueRef {.
    importc: "LLVMConstInlineAsm", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueConstantGlobals Global Values
##
##  This group contains functions that operate on global values. Functions in
##  this group relate to functions in the llvm::GlobalValue class tree.
##
##  @see llvm::GlobalValue
##
##  @{
##

proc getGlobalParent*(global: ValueRef): ModuleRef {.importc: "LLVMGetGlobalParent",
    dynlib: LLVMLib.}
proc isDeclaration*(global: ValueRef): Bool {.importc: "LLVMIsDeclaration",
    dynlib: LLVMLib.}
proc getLinkage*(global: ValueRef): Linkage {.importc: "LLVMGetLinkage",
    dynlib: LLVMLib.}
proc setLinkage*(global: ValueRef; linkage: Linkage) {.importc: "LLVMSetLinkage",
    dynlib: LLVMLib.}
proc getSection*(global: ValueRef): cstring {.importc: "LLVMGetSection",
    dynlib: LLVMLib.}
proc setSection*(global: ValueRef; section: cstring) {.importc: "LLVMSetSection",
    dynlib: LLVMLib.}
proc getVisibility*(global: ValueRef): Visibility {.importc: "LLVMGetVisibility",
    dynlib: LLVMLib.}
proc setVisibility*(global: ValueRef; viz: Visibility) {.
    importc: "LLVMSetVisibility", dynlib: LLVMLib.}
proc getDLLStorageClass*(global: ValueRef): DLLStorageClass {.
    importc: "LLVMGetDLLStorageClass", dynlib: LLVMLib.}
proc setDLLStorageClass*(global: ValueRef; class: DLLStorageClass) {.
    importc: "LLVMSetDLLStorageClass", dynlib: LLVMLib.}
proc getUnnamedAddress*(global: ValueRef): UnnamedAddr {.
    importc: "LLVMGetUnnamedAddress", dynlib: LLVMLib.}
proc setUnnamedAddress*(global: ValueRef; unnamedAddr: UnnamedAddr) {.
    importc: "LLVMSetUnnamedAddress", dynlib: LLVMLib.}
## *
##  Returns the "value type" of a global value.  This differs from the formal
##  type of a global value which is always a pointer type.
##
##  @see llvm::GlobalValue::getValueType()
##

proc globalGetValueType*(global: ValueRef): TypeRef {.
    importc: "LLVMGlobalGetValueType", dynlib: LLVMLib.}
## * Deprecated: Use LLVMGetUnnamedAddress instead.

proc hasUnnamedAddr*(global: ValueRef): Bool {.importc: "LLVMHasUnnamedAddr",
    dynlib: LLVMLib.}
## * Deprecated: Use LLVMSetUnnamedAddress instead.

proc setUnnamedAddr*(global: ValueRef; hasUnnamedAddr: Bool) {.
    importc: "LLVMSetUnnamedAddr", dynlib: LLVMLib.}
## *
##  @defgroup LLVMCCoreValueWithAlignment Values with alignment
##
##  Functions in this group only apply to values with alignment, i.e.
##  global variables, load and store instructions.
##
## *
##  Obtain the preferred alignment of the value.
##  @see llvm::AllocaInst::getAlignment()
##  @see llvm::LoadInst::getAlignment()
##  @see llvm::StoreInst::getAlignment()
##  @see llvm::GlobalValue::getAlignment()
##

proc getAlignment*(v: ValueRef): cuint {.importc: "LLVMGetAlignment", dynlib: LLVMLib.}
## *
##  Set the preferred alignment of the value.
##  @see llvm::AllocaInst::setAlignment()
##  @see llvm::LoadInst::setAlignment()
##  @see llvm::StoreInst::setAlignment()
##  @see llvm::GlobalValue::setAlignment()
##

proc setAlignment*(v: ValueRef; bytes: cuint) {.importc: "LLVMSetAlignment",
    dynlib: LLVMLib.}
## *
##  Sets a metadata attachment, erasing the existing metadata attachment if
##  it already exists for the given kind.
##
##  @see llvm::GlobalObject::setMetadata()
##

proc globalSetMetadata*(global: ValueRef; kind: cuint; md: MetadataRef) {.
    importc: "LLVMGlobalSetMetadata", dynlib: LLVMLib.}
## *
##  Erases a metadata attachment of the given kind if it exists.
##
##  @see llvm::GlobalObject::eraseMetadata()
##

proc globalEraseMetadata*(global: ValueRef; kind: cuint) {.
    importc: "LLVMGlobalEraseMetadata", dynlib: LLVMLib.}
## *
##  Removes all metadata attachments from this value.
##
##  @see llvm::GlobalObject::clearMetadata()
##

proc globalClearMetadata*(global: ValueRef) {.importc: "LLVMGlobalClearMetadata",
    dynlib: LLVMLib.}
## *
##  Retrieves an array of metadata entries representing the metadata attached to
##  this value. The caller is responsible for freeing this array by calling
##  \c LLVMDisposeValueMetadataEntries.
##
##  @see llvm::GlobalObject::getAllMetadata()
##

proc globalCopyAllMetadata*(value: ValueRef; numEntries: ptr csize): ptr ValueMetadataEntry {.
    importc: "LLVMGlobalCopyAllMetadata", dynlib: LLVMLib.}
## *
##  Destroys value metadata entries.
##

proc disposeValueMetadataEntries*(entries: ptr ValueMetadataEntry) {.
    importc: "LLVMDisposeValueMetadataEntries", dynlib: LLVMLib.}
## *
##  Returns the kind of a value metadata entry at a specific index.
##

proc valueMetadataEntriesGetKind*(entries: ptr ValueMetadataEntry; index: cuint): cuint {.
    importc: "LLVMValueMetadataEntriesGetKind", dynlib: LLVMLib.}
## *
##  Returns the underlying metadata node of a value metadata entry at a
##  specific index.
##

proc valueMetadataEntriesGetMetadata*(entries: ptr ValueMetadataEntry; index: cuint): MetadataRef {.
    importc: "LLVMValueMetadataEntriesGetMetadata", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCoreValueConstantGlobalVariable Global Variables
##
##  This group contains functions that operate on global variable values.
##
##  @see llvm::GlobalVariable
##
##  @{
##

proc addGlobal*(m: ModuleRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMAddGlobal", dynlib: LLVMLib.}
proc addGlobalInAddressSpace*(m: ModuleRef; ty: TypeRef; name: cstring;
                             addressSpace: cuint): ValueRef {.
    importc: "LLVMAddGlobalInAddressSpace", dynlib: LLVMLib.}
proc getNamedGlobal*(m: ModuleRef; name: cstring): ValueRef {.
    importc: "LLVMGetNamedGlobal", dynlib: LLVMLib.}
proc getFirstGlobal*(m: ModuleRef): ValueRef {.importc: "LLVMGetFirstGlobal",
    dynlib: LLVMLib.}
proc getLastGlobal*(m: ModuleRef): ValueRef {.importc: "LLVMGetLastGlobal",
    dynlib: LLVMLib.}
proc getNextGlobal*(globalVar: ValueRef): ValueRef {.importc: "LLVMGetNextGlobal",
    dynlib: LLVMLib.}
proc getPreviousGlobal*(globalVar: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobal", dynlib: LLVMLib.}
proc deleteGlobal*(globalVar: ValueRef) {.importc: "LLVMDeleteGlobal",
                                       dynlib: LLVMLib.}
proc getInitializer*(globalVar: ValueRef): ValueRef {.importc: "LLVMGetInitializer",
    dynlib: LLVMLib.}
proc setInitializer*(globalVar: ValueRef; constantVal: ValueRef) {.
    importc: "LLVMSetInitializer", dynlib: LLVMLib.}
proc isThreadLocal*(globalVar: ValueRef): Bool {.importc: "LLVMIsThreadLocal",
    dynlib: LLVMLib.}
proc setThreadLocal*(globalVar: ValueRef; isThreadLocal: Bool) {.
    importc: "LLVMSetThreadLocal", dynlib: LLVMLib.}
proc isGlobalConstant*(globalVar: ValueRef): Bool {.importc: "LLVMIsGlobalConstant",
    dynlib: LLVMLib.}
proc setGlobalConstant*(globalVar: ValueRef; isConstant: Bool) {.
    importc: "LLVMSetGlobalConstant", dynlib: LLVMLib.}
proc getThreadLocalMode*(globalVar: ValueRef): ThreadLocalMode {.
    importc: "LLVMGetThreadLocalMode", dynlib: LLVMLib.}
proc setThreadLocalMode*(globalVar: ValueRef; mode: ThreadLocalMode) {.
    importc: "LLVMSetThreadLocalMode", dynlib: LLVMLib.}
proc isExternallyInitialized*(globalVar: ValueRef): Bool {.
    importc: "LLVMIsExternallyInitialized", dynlib: LLVMLib.}
proc setExternallyInitialized*(globalVar: ValueRef; isExtInit: Bool) {.
    importc: "LLVMSetExternallyInitialized", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCoreValueConstantGlobalAlias Global Aliases
##
##  This group contains function that operate on global alias values.
##
##  @see llvm::GlobalAlias
##
##  @{
##

proc addAlias*(m: ModuleRef; ty: TypeRef; aliasee: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMAddAlias", dynlib: LLVMLib.}
## *
##  Obtain a GlobalAlias value from a Module by its name.
##
##  The returned value corresponds to a llvm::GlobalAlias value.
##
##  @see llvm::Module::getNamedAlias()
##

proc getNamedGlobalAlias*(m: ModuleRef; name: cstring; nameLen: csize): ValueRef {.
    importc: "LLVMGetNamedGlobalAlias", dynlib: LLVMLib.}
## *
##  Obtain an iterator to the first GlobalAlias in a Module.
##
##  @see llvm::Module::alias_begin()
##

proc getFirstGlobalAlias*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetFirstGlobalAlias", dynlib: LLVMLib.}
## *
##  Obtain an iterator to the last GlobalAlias in a Module.
##
##  @see llvm::Module::alias_end()
##

proc getLastGlobalAlias*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetLastGlobalAlias", dynlib: LLVMLib.}
## *
##  Advance a GlobalAlias iterator to the next GlobalAlias.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  global aliases.
##

proc getNextGlobalAlias*(ga: ValueRef): ValueRef {.
    importc: "LLVMGetNextGlobalAlias", dynlib: LLVMLib.}
## *
##  Decrement a GlobalAlias iterator to the previous GlobalAlias.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous global aliases.
##

proc getPreviousGlobalAlias*(ga: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobalAlias", dynlib: LLVMLib.}
## *
##  Retrieve the target value of an alias.
##

proc aliasGetAliasee*(alias: ValueRef): ValueRef {.importc: "LLVMAliasGetAliasee",
    dynlib: LLVMLib.}
## *
##  Set the target value of an alias.
##

proc aliasSetAliasee*(alias: ValueRef; aliasee: ValueRef) {.
    importc: "LLVMAliasSetAliasee", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueFunction Function values
##
##  Functions in this group operate on LLVMValueRef instances that
##  correspond to llvm::Function instances.
##
##  @see llvm::Function
##
##  @{
##
## *
##  Remove a function from its containing module and deletes it.
##
##  @see llvm::Function::eraseFromParent()
##

proc deleteFunction*(fn: ValueRef) {.importc: "LLVMDeleteFunction", dynlib: LLVMLib.}
## *
##  Check whether the given function has a personality function.
##
##  @see llvm::Function::hasPersonalityFn()
##

proc hasPersonalityFn*(fn: ValueRef): Bool {.importc: "LLVMHasPersonalityFn",
    dynlib: LLVMLib.}
## *
##  Obtain the personality function attached to the function.
##
##  @see llvm::Function::getPersonalityFn()
##

proc getPersonalityFn*(fn: ValueRef): ValueRef {.importc: "LLVMGetPersonalityFn",
    dynlib: LLVMLib.}
## *
##  Set the personality function attached to the function.
##
##  @see llvm::Function::setPersonalityFn()
##

proc setPersonalityFn*(fn: ValueRef; personalityFn: ValueRef) {.
    importc: "LLVMSetPersonalityFn", dynlib: LLVMLib.}
## *
##  Obtain the intrinsic ID number which matches the given function name.
##
##  @see llvm::Function::lookupIntrinsicID()
##

proc lookupIntrinsicID*(name: cstring; nameLen: csize): cuint {.
    importc: "LLVMLookupIntrinsicID", dynlib: LLVMLib.}
## *
##  Obtain the ID number from a function instance.
##
##  @see llvm::Function::getIntrinsicID()
##

proc getIntrinsicID*(fn: ValueRef): cuint {.importc: "LLVMGetIntrinsicID",
                                        dynlib: LLVMLib.}
## *
##  Create or insert the declaration of an intrinsic.  For overloaded intrinsics,
##  parameter types must be provided to uniquely identify an overload.
##
##  @see llvm::Intrinsic::getDeclaration()
##

proc getIntrinsicDeclaration*(`mod`: ModuleRef; id: cuint; paramTypes: ptr TypeRef;
                             paramCount: csize): ValueRef {.
    importc: "LLVMGetIntrinsicDeclaration", dynlib: LLVMLib.}
## *
##  Retrieves the type of an intrinsic.  For overloaded intrinsics, parameter
##  types must be provided to uniquely identify an overload.
##
##  @see llvm::Intrinsic::getType()
##

proc intrinsicGetType*(ctx: ContextRef; id: cuint; paramTypes: ptr TypeRef;
                      paramCount: csize): TypeRef {.
    importc: "LLVMIntrinsicGetType", dynlib: LLVMLib.}
## *
##  Retrieves the name of an intrinsic.
##
##  @see llvm::Intrinsic::getName()
##

proc intrinsicGetName*(id: cuint; nameLength: ptr csize): cstring {.
    importc: "LLVMIntrinsicGetName", dynlib: LLVMLib.}
## *
##  Copies the name of an overloaded intrinsic identified by a given list of
##  parameter types.
##
##  Unlike LLVMIntrinsicGetName, the caller is responsible for freeing the
##  returned string.
##
##  @see llvm::Intrinsic::getName()
##

proc intrinsicCopyOverloadedName*(id: cuint; paramTypes: ptr TypeRef;
                                 paramCount: csize; nameLength: ptr csize): cstring {.
    importc: "LLVMIntrinsicCopyOverloadedName", dynlib: LLVMLib.}
## *
##  Obtain if the intrinsic identified by the given ID is overloaded.
##
##  @see llvm::Intrinsic::isOverloaded()
##

proc intrinsicIsOverloaded*(id: cuint): Bool {.importc: "LLVMIntrinsicIsOverloaded",
    dynlib: LLVMLib.}
## *
##  Obtain the calling function of a function.
##
##  The returned value corresponds to the LLVMCallConv enumeration.
##
##  @see llvm::Function::getCallingConv()
##

proc getFunctionCallConv*(fn: ValueRef): cuint {.importc: "LLVMGetFunctionCallConv",
    dynlib: LLVMLib.}
## *
##  Set the calling convention of a function.
##
##  @see llvm::Function::setCallingConv()
##
##  @param Fn Function to operate on
##  @param CC LLVMCallConv to set calling convention to
##

proc setFunctionCallConv*(fn: ValueRef; cc: cuint) {.
    importc: "LLVMSetFunctionCallConv", dynlib: LLVMLib.}
## *
##  Obtain the name of the garbage collector to use during code
##  generation.
##
##  @see llvm::Function::getGC()
##

proc getGC*(fn: ValueRef): cstring {.importc: "LLVMGetGC", dynlib: LLVMLib.}
## *
##  Define the garbage collector to use during code generation.
##
##  @see llvm::Function::setGC()
##

proc setGC*(fn: ValueRef; name: cstring) {.importc: "LLVMSetGC", dynlib: LLVMLib.}
## *
##  Add an attribute to a function.
##
##  @see llvm::Function::addAttribute()
##

proc addAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; a: AttributeRef) {.
    importc: "LLVMAddAttributeAtIndex", dynlib: LLVMLib.}
proc getAttributeCountAtIndex*(f: ValueRef; idx: AttributeIndex): cuint {.
    importc: "LLVMGetAttributeCountAtIndex", dynlib: LLVMLib.}
proc getAttributesAtIndex*(f: ValueRef; idx: AttributeIndex; attrs: ptr AttributeRef) {.
    importc: "LLVMGetAttributesAtIndex", dynlib: LLVMLib.}
proc getEnumAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; kindID: cuint): AttributeRef {.
    importc: "LLVMGetEnumAttributeAtIndex", dynlib: LLVMLib.}
proc getStringAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; k: cstring;
                               kLen: cuint): AttributeRef {.
    importc: "LLVMGetStringAttributeAtIndex", dynlib: LLVMLib.}
proc removeEnumAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; kindID: cuint) {.
    importc: "LLVMRemoveEnumAttributeAtIndex", dynlib: LLVMLib.}
proc removeStringAttributeAtIndex*(f: ValueRef; idx: AttributeIndex; k: cstring;
                                  kLen: cuint) {.
    importc: "LLVMRemoveStringAttributeAtIndex", dynlib: LLVMLib.}
## *
##  Add a target-dependent attribute to a function
##  @see llvm::AttrBuilder::addAttribute()
##

proc addTargetDependentFunctionAttr*(fn: ValueRef; a: cstring; v: cstring) {.
    importc: "LLVMAddTargetDependentFunctionAttr", dynlib: LLVMLib.}
## *
##  @defgroup LLVMCCoreValueFunctionParameters Function Parameters
##
##  Functions in this group relate to arguments/parameters on functions.
##
##  Functions in this group expect LLVMValueRef instances that correspond
##  to llvm::Function instances.
##
##  @{
##
## *
##  Obtain the number of parameters in a function.
##
##  @see llvm::Function::arg_size()
##

proc countParams*(fn: ValueRef): cuint {.importc: "LLVMCountParams", dynlib: LLVMLib.}
## *
##  Obtain the parameters in a function.
##
##  The takes a pointer to a pre-allocated array of LLVMValueRef that is
##  at least LLVMCountParams() long. This array will be filled with
##  LLVMValueRef instances which correspond to the parameters the
##  function receives. Each LLVMValueRef corresponds to a llvm::Argument
##  instance.
##
##  @see llvm::Function::arg_begin()
##

proc getParams*(fn: ValueRef; params: ptr ValueRef) {.importc: "LLVMGetParams",
    dynlib: LLVMLib.}
## *
##  Obtain the parameter at the specified index.
##
##  Parameters are indexed from 0.
##
##  @see llvm::Function::arg_begin()
##

proc getParam*(fn: ValueRef; index: cuint): ValueRef {.importc: "LLVMGetParam",
    dynlib: LLVMLib.}
## *
##  Obtain the function to which this argument belongs.
##
##  Unlike other functions in this group, this one takes an LLVMValueRef
##  that corresponds to a llvm::Attribute.
##
##  The returned LLVMValueRef is the llvm::Function to which this
##  argument belongs.
##

proc getParamParent*(inst: ValueRef): ValueRef {.importc: "LLVMGetParamParent",
    dynlib: LLVMLib.}
## *
##  Obtain the first parameter to a function.
##
##  @see llvm::Function::arg_begin()
##

proc getFirstParam*(fn: ValueRef): ValueRef {.importc: "LLVMGetFirstParam",
    dynlib: LLVMLib.}
## *
##  Obtain the last parameter to a function.
##
##  @see llvm::Function::arg_end()
##

proc getLastParam*(fn: ValueRef): ValueRef {.importc: "LLVMGetLastParam",
    dynlib: LLVMLib.}
## *
##  Obtain the next parameter to a function.
##
##  This takes an LLVMValueRef obtained from LLVMGetFirstParam() (which is
##  actually a wrapped iterator) and obtains the next parameter from the
##  underlying iterator.
##

proc getNextParam*(arg: ValueRef): ValueRef {.importc: "LLVMGetNextParam",
    dynlib: LLVMLib.}
## *
##  Obtain the previous parameter to a function.
##
##  This is the opposite of LLVMGetNextParam().
##

proc getPreviousParam*(arg: ValueRef): ValueRef {.importc: "LLVMGetPreviousParam",
    dynlib: LLVMLib.}
## *
##  Set the alignment for a function parameter.
##
##  @see llvm::Argument::addAttr()
##  @see llvm::AttrBuilder::addAlignmentAttr()
##

proc setParamAlignment*(arg: ValueRef; align: cuint) {.
    importc: "LLVMSetParamAlignment", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueGlobalIFunc IFuncs
##
##  Functions in this group relate to indirect functions.
##
##  Functions in this group expect LLVMValueRef instances that correspond
##  to llvm::GlobalIFunc instances.
##
##  @{
##
## *
##  Add a global indirect function to a module under a specified name.
##
##  @see llvm::GlobalIFunc::create()
##

proc addGlobalIFunc*(m: ModuleRef; name: cstring; nameLen: csize; ty: TypeRef;
                    addrSpace: cuint; resolver: ValueRef): ValueRef {.
    importc: "LLVMAddGlobalIFunc", dynlib: LLVMLib.}
## *
##  Obtain a GlobalIFunc value from a Module by its name.
##
##  The returned value corresponds to a llvm::GlobalIFunc value.
##
##  @see llvm::Module::getNamedIFunc()
##

proc getNamedGlobalIFunc*(m: ModuleRef; name: cstring; nameLen: csize): ValueRef {.
    importc: "LLVMGetNamedGlobalIFunc", dynlib: LLVMLib.}
## *
##  Obtain an iterator to the first GlobalIFunc in a Module.
##
##  @see llvm::Module::ifunc_begin()
##

proc getFirstGlobalIFunc*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetFirstGlobalIFunc", dynlib: LLVMLib.}
## *
##  Obtain an iterator to the last GlobalIFunc in a Module.
##
##  @see llvm::Module::ifunc_end()
##

proc getLastGlobalIFunc*(m: ModuleRef): ValueRef {.
    importc: "LLVMGetLastGlobalIFunc", dynlib: LLVMLib.}
## *
##  Advance a GlobalIFunc iterator to the next GlobalIFunc.
##
##  Returns NULL if the iterator was already at the end and there are no more
##  global aliases.
##

proc getNextGlobalIFunc*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetNextGlobalIFunc", dynlib: LLVMLib.}
## *
##  Decrement a GlobalIFunc iterator to the previous GlobalIFunc.
##
##  Returns NULL if the iterator was already at the beginning and there are
##  no previous global aliases.
##

proc getPreviousGlobalIFunc*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousGlobalIFunc", dynlib: LLVMLib.}
## *
##  Retrieves the resolver function associated with this indirect function, or
##  NULL if it doesn't not exist.
##
##  @see llvm::GlobalIFunc::getResolver()
##

proc getGlobalIFuncResolver*(iFunc: ValueRef): ValueRef {.
    importc: "LLVMGetGlobalIFuncResolver", dynlib: LLVMLib.}
## *
##  Sets the resolver function associated with this indirect function.
##
##  @see llvm::GlobalIFunc::setResolver()
##

proc setGlobalIFuncResolver*(iFunc: ValueRef; resolver: ValueRef) {.
    importc: "LLVMSetGlobalIFuncResolver", dynlib: LLVMLib.}
## *
##  Remove a global indirect function from its parent module and delete it.
##
##  @see llvm::GlobalIFunc::eraseFromParent()
##

proc eraseGlobalIFunc*(iFunc: ValueRef) {.importc: "LLVMEraseGlobalIFunc",
                                       dynlib: LLVMLib.}
## *
##  Remove a global indirect function from its parent module.
##
##  This unlinks the global indirect function from its containing module but
##  keeps it alive.
##
##  @see llvm::GlobalIFunc::removeFromParent()
##

proc removeGlobalIFunc*(iFunc: ValueRef) {.importc: "LLVMRemoveGlobalIFunc",
                                        dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueMetadata Metadata
##
##  @{
##
## *
##  Create an MDString value from a given string value.
##
##  The MDString value does not take ownership of the given string, it remains
##  the responsibility of the caller to free it.
##
##  @see llvm::MDString::get()
##

proc mDStringInContext2*(c: ContextRef; str: cstring; sLen: csize): MetadataRef {.
    importc: "LLVMMDStringInContext2", dynlib: LLVMLib.}
## *
##  Create an MDNode value with the given array of operands.
##
##  @see llvm::MDNode::get()
##

proc mDNodeInContext2*(c: ContextRef; mDs: ptr MetadataRef; count: csize): MetadataRef {.
    importc: "LLVMMDNodeInContext2", dynlib: LLVMLib.}
## *
##  Obtain a Metadata as a Value.
##

proc metadataAsValue*(c: ContextRef; md: MetadataRef): ValueRef {.
    importc: "LLVMMetadataAsValue", dynlib: LLVMLib.}
## *
##  Obtain a Value as a Metadata.
##

proc valueAsMetadata*(val: ValueRef): MetadataRef {.importc: "LLVMValueAsMetadata",
    dynlib: LLVMLib.}
## *
##  Obtain the underlying string from a MDString value.
##
##  @param V Instance to obtain string from.
##  @param Length Memory address which will hold length of returned string.
##  @return String data in MDString.
##

proc getMDString*(v: ValueRef; length: ptr cuint): cstring {.
    importc: "LLVMGetMDString", dynlib: LLVMLib.}
## *
##  Obtain the number of operands from an MDNode value.
##
##  @param V MDNode to get number of operands from.
##  @return Number of operands of the MDNode.
##

proc getMDNodeNumOperands*(v: ValueRef): cuint {.
    importc: "LLVMGetMDNodeNumOperands", dynlib: LLVMLib.}
## *
##  Obtain the given MDNode's operands.
##
##  The passed LLVMValueRef pointer should point to enough memory to hold all of
##  the operands of the given MDNode (see LLVMGetMDNodeNumOperands) as
##  LLVMValueRefs. This memory will be populated with the LLVMValueRefs of the
##  MDNode's operands.
##
##  @param V MDNode to get the operands from.
##  @param Dest Destination array for operands.
##

proc getMDNodeOperands*(v: ValueRef; dest: ptr ValueRef) {.
    importc: "LLVMGetMDNodeOperands", dynlib: LLVMLib.}
## * Deprecated: Use LLVMMDStringInContext2 instead.

proc mDStringInContext*(c: ContextRef; str: cstring; sLen: cuint): ValueRef {.
    importc: "LLVMMDStringInContext", dynlib: LLVMLib.}
## * Deprecated: Use LLVMMDStringInContext2 instead.

proc mDString*(str: cstring; sLen: cuint): ValueRef {.importc: "LLVMMDString",
    dynlib: LLVMLib.}
## * Deprecated: Use LLVMMDNodeInContext2 instead.

proc mDNodeInContext*(c: ContextRef; vals: ptr ValueRef; count: cuint): ValueRef {.
    importc: "LLVMMDNodeInContext", dynlib: LLVMLib.}
## * Deprecated: Use LLVMMDNodeInContext2 instead.

proc mDNode*(vals: ptr ValueRef; count: cuint): ValueRef {.importc: "LLVMMDNode",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueBasicBlock Basic Block
##
##  A basic block represents a single entry single exit section of code.
##  Basic blocks contain a list of instructions which form the body of
##  the block.
##
##  Basic blocks belong to functions. They have the type of label.
##
##  Basic blocks are themselves values. However, the C API models them as
##  LLVMBasicBlockRef.
##
##  @see llvm::BasicBlock
##
##  @{
##
## *
##  Convert a basic block instance to a value type.
##

proc basicBlockAsValue*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBasicBlockAsValue", dynlib: LLVMLib.}
## *
##  Determine whether an LLVMValueRef is itself a basic block.
##

proc valueIsBasicBlock*(val: ValueRef): Bool {.importc: "LLVMValueIsBasicBlock",
    dynlib: LLVMLib.}
## *
##  Convert an LLVMValueRef to an LLVMBasicBlockRef instance.
##

proc valueAsBasicBlock*(val: ValueRef): BasicBlockRef {.
    importc: "LLVMValueAsBasicBlock", dynlib: LLVMLib.}
## *
##  Obtain the string name of a basic block.
##

proc getBasicBlockName*(bb: BasicBlockRef): cstring {.
    importc: "LLVMGetBasicBlockName", dynlib: LLVMLib.}
## *
##  Obtain the function to which a basic block belongs.
##
##  @see llvm::BasicBlock::getParent()
##

proc getBasicBlockParent*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetBasicBlockParent", dynlib: LLVMLib.}
## *
##  Obtain the terminator instruction for a basic block.
##
##  If the basic block does not have a terminator (it is not well-formed
##  if it doesn't), then NULL is returned.
##
##  The returned LLVMValueRef corresponds to an llvm::Instruction.
##
##  @see llvm::BasicBlock::getTerminator()
##

proc getBasicBlockTerminator*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetBasicBlockTerminator", dynlib: LLVMLib.}
## *
##  Obtain the number of basic blocks in a function.
##
##  @param Fn Function value to operate on.
##

proc countBasicBlocks*(fn: ValueRef): cuint {.importc: "LLVMCountBasicBlocks",
    dynlib: LLVMLib.}
## *
##  Obtain all of the basic blocks in a function.
##
##  This operates on a function value. The BasicBlocks parameter is a
##  pointer to a pre-allocated array of LLVMBasicBlockRef of at least
##  LLVMCountBasicBlocks() in length. This array is populated with
##  LLVMBasicBlockRef instances.
##

proc getBasicBlocks*(fn: ValueRef; basicBlocks: ptr BasicBlockRef) {.
    importc: "LLVMGetBasicBlocks", dynlib: LLVMLib.}
## *
##  Obtain the first basic block in a function.
##
##  The returned basic block can be used as an iterator. You will likely
##  eventually call into LLVMGetNextBasicBlock() with it.
##
##  @see llvm::Function::begin()
##

proc getFirstBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetFirstBasicBlock", dynlib: LLVMLib.}
## *
##  Obtain the last basic block in a function.
##
##  @see llvm::Function::end()
##

proc getLastBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetLastBasicBlock", dynlib: LLVMLib.}
## *
##  Advance a basic block iterator.
##

proc getNextBasicBlock*(bb: BasicBlockRef): BasicBlockRef {.
    importc: "LLVMGetNextBasicBlock", dynlib: LLVMLib.}
## *
##  Go backwards in a basic block iterator.
##

proc getPreviousBasicBlock*(bb: BasicBlockRef): BasicBlockRef {.
    importc: "LLVMGetPreviousBasicBlock", dynlib: LLVMLib.}
## *
##  Obtain the basic block that corresponds to the entry point of a
##  function.
##
##  @see llvm::Function::getEntryBlock()
##

proc getEntryBasicBlock*(fn: ValueRef): BasicBlockRef {.
    importc: "LLVMGetEntryBasicBlock", dynlib: LLVMLib.}
## *
##  Insert the given basic block after the insertion point of the given builder.
##
##  The insertion point must be valid.
##
##  @see llvm::Function::BasicBlockListType::insertAfter()
##

proc insertExistingBasicBlockAfterInsertBlock*(builder: BuilderRef;
    bb: BasicBlockRef) {.importc: "LLVMInsertExistingBasicBlockAfterInsertBlock",
                       dynlib: LLVMLib.}
## *
##  Append the given basic block to the basic block list of the given function.
##
##  @see llvm::Function::BasicBlockListType::push_back()
##

proc appendExistingBasicBlock*(fn: ValueRef; bb: BasicBlockRef) {.
    importc: "LLVMAppendExistingBasicBlock", dynlib: LLVMLib.}
## *
##  Create a new basic block without inserting it into a function.
##
##  @see llvm::BasicBlock::Create()
##

proc createBasicBlockInContext*(c: ContextRef; name: cstring): BasicBlockRef {.
    importc: "LLVMCreateBasicBlockInContext", dynlib: LLVMLib.}
## *
##  Append a basic block to the end of a function.
##
##  @see llvm::BasicBlock::Create()
##

proc appendBasicBlockInContext*(c: ContextRef; fn: ValueRef; name: cstring): BasicBlockRef {.
    importc: "LLVMAppendBasicBlockInContext", dynlib: LLVMLib.}
## *
##  Append a basic block to the end of a function using the global
##  context.
##
##  @see llvm::BasicBlock::Create()
##

proc appendBasicBlock*(fn: ValueRef; name: cstring): BasicBlockRef {.
    importc: "LLVMAppendBasicBlock", dynlib: LLVMLib.}
## *
##  Insert a basic block in a function before another basic block.
##
##  The function to add to is determined by the function of the
##  passed basic block.
##
##  @see llvm::BasicBlock::Create()
##

proc insertBasicBlockInContext*(c: ContextRef; bb: BasicBlockRef; name: cstring): BasicBlockRef {.
    importc: "LLVMInsertBasicBlockInContext", dynlib: LLVMLib.}
## *
##  Insert a basic block in a function using the global context.
##
##  @see llvm::BasicBlock::Create()
##

proc insertBasicBlock*(insertBeforeBB: BasicBlockRef; name: cstring): BasicBlockRef {.
    importc: "LLVMInsertBasicBlock", dynlib: LLVMLib.}
## *
##  Remove a basic block from a function and delete it.
##
##  This deletes the basic block from its containing function and deletes
##  the basic block itself.
##
##  @see llvm::BasicBlock::eraseFromParent()
##

proc deleteBasicBlock*(bb: BasicBlockRef) {.importc: "LLVMDeleteBasicBlock",
    dynlib: LLVMLib.}
## *
##  Remove a basic block from a function.
##
##  This deletes the basic block from its containing function but keep
##  the basic block alive.
##
##  @see llvm::BasicBlock::removeFromParent()
##

proc removeBasicBlockFromParent*(bb: BasicBlockRef) {.
    importc: "LLVMRemoveBasicBlockFromParent", dynlib: LLVMLib.}
## *
##  Move a basic block to before another one.
##
##  @see llvm::BasicBlock::moveBefore()
##

proc moveBasicBlockBefore*(bb: BasicBlockRef; movePos: BasicBlockRef) {.
    importc: "LLVMMoveBasicBlockBefore", dynlib: LLVMLib.}
## *
##  Move a basic block to after another one.
##
##  @see llvm::BasicBlock::moveAfter()
##

proc moveBasicBlockAfter*(bb: BasicBlockRef; movePos: BasicBlockRef) {.
    importc: "LLVMMoveBasicBlockAfter", dynlib: LLVMLib.}
## *
##  Obtain the first instruction in a basic block.
##
##  The returned LLVMValueRef corresponds to a llvm::Instruction
##  instance.
##

proc getFirstInstruction*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetFirstInstruction", dynlib: LLVMLib.}
## *
##  Obtain the last instruction in a basic block.
##
##  The returned LLVMValueRef corresponds to an LLVM:Instruction.
##

proc getLastInstruction*(bb: BasicBlockRef): ValueRef {.
    importc: "LLVMGetLastInstruction", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstruction Instructions
##
##  Functions in this group relate to the inspection and manipulation of
##  individual instructions.
##
##  In the C++ API, an instruction is modeled by llvm::Instruction. This
##  class has a large number of descendents. llvm::Instruction is a
##  llvm::Value and in the C API, instructions are modeled by
##  LLVMValueRef.
##
##  This group also contains sub-groups which operate on specific
##  llvm::Instruction types, e.g. llvm::CallInst.
##
##  @{
##
## *
##  Determine whether an instruction has any metadata attached.
##

proc hasMetadata*(val: ValueRef): cint {.importc: "LLVMHasMetadata", dynlib: LLVMLib.}
## *
##  Return metadata associated with an instruction value.
##

proc getMetadata*(val: ValueRef; kindID: cuint): ValueRef {.
    importc: "LLVMGetMetadata", dynlib: LLVMLib.}
## *
##  Set metadata associated with an instruction value.
##

proc setMetadata*(val: ValueRef; kindID: cuint; node: ValueRef) {.
    importc: "LLVMSetMetadata", dynlib: LLVMLib.}
## *
##  Returns the metadata associated with an instruction value, but filters out
##  all the debug locations.
##
##  @see llvm::Instruction::getAllMetadataOtherThanDebugLoc()
##

proc instructionGetAllMetadataOtherThanDebugLoc*(instr: ValueRef;
    numEntries: ptr csize): ptr ValueMetadataEntry {.
    importc: "LLVMInstructionGetAllMetadataOtherThanDebugLoc", dynlib: LLVMLib.}
## *
##  Obtain the basic block to which an instruction belongs.
##
##  @see llvm::Instruction::getParent()
##

proc getInstructionParent*(inst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetInstructionParent", dynlib: LLVMLib.}
## *
##  Obtain the instruction that occurs after the one specified.
##
##  The next instruction will be from the same basic block.
##
##  If this is the last instruction in a basic block, NULL will be
##  returned.
##

proc getNextInstruction*(inst: ValueRef): ValueRef {.
    importc: "LLVMGetNextInstruction", dynlib: LLVMLib.}
## *
##  Obtain the instruction that occurred before this one.
##
##  If the instruction is the first instruction in a basic block, NULL
##  will be returned.
##

proc getPreviousInstruction*(inst: ValueRef): ValueRef {.
    importc: "LLVMGetPreviousInstruction", dynlib: LLVMLib.}
## *
##  Remove and delete an instruction.
##
##  The instruction specified is removed from its containing building
##  block but is kept alive.
##
##  @see llvm::Instruction::removeFromParent()
##

proc instructionRemoveFromParent*(inst: ValueRef) {.
    importc: "LLVMInstructionRemoveFromParent", dynlib: LLVMLib.}
## *
##  Remove and delete an instruction.
##
##  The instruction specified is removed from its containing building
##  block and then deleted.
##
##  @see llvm::Instruction::eraseFromParent()
##

proc instructionEraseFromParent*(inst: ValueRef) {.
    importc: "LLVMInstructionEraseFromParent", dynlib: LLVMLib.}
## *
##  Obtain the code opcode for an individual instruction.
##
##  @see llvm::Instruction::getOpCode()
##

proc getInstructionOpcode*(inst: ValueRef): Opcode {.
    importc: "LLVMGetInstructionOpcode", dynlib: LLVMLib.}
## *
##  Obtain the predicate of an instruction.
##
##  This is only valid for instructions that correspond to llvm::ICmpInst
##  or llvm::ConstantExpr whose opcode is llvm::Instruction::ICmp.
##
##  @see llvm::ICmpInst::getPredicate()
##

proc getICmpPredicate*(inst: ValueRef): IntPredicate {.
    importc: "LLVMGetICmpPredicate", dynlib: LLVMLib.}
## *
##  Obtain the float predicate of an instruction.
##
##  This is only valid for instructions that correspond to llvm::FCmpInst
##  or llvm::ConstantExpr whose opcode is llvm::Instruction::FCmp.
##
##  @see llvm::FCmpInst::getPredicate()
##

proc getFCmpPredicate*(inst: ValueRef): RealPredicate {.
    importc: "LLVMGetFCmpPredicate", dynlib: LLVMLib.}
## *
##  Create a copy of 'this' instruction that is identical in all ways
##  except the following:
##    * The instruction has no parent
##    * The instruction has no name
##
##  @see llvm::Instruction::clone()
##

proc instructionClone*(inst: ValueRef): ValueRef {.importc: "LLVMInstructionClone",
    dynlib: LLVMLib.}
## *
##  Determine whether an instruction is a terminator. This routine is named to
##  be compatible with historical functions that did this by querying the
##  underlying C++ type.
##
##  @see llvm::Instruction::isTerminator()
##

proc isATerminatorInst*(inst: ValueRef): ValueRef {.
    importc: "LLVMIsATerminatorInst", dynlib: LLVMLib.}
## *
##  @defgroup LLVMCCoreValueInstructionCall Call Sites and Invocations
##
##  Functions in this group apply to instructions that refer to call
##  sites and invocations. These correspond to C++ types in the
##  llvm::CallInst class tree.
##
##  @{
##
## *
##  Obtain the argument count for a call instruction.
##
##  This expects an LLVMValueRef that corresponds to a llvm::CallInst,
##  llvm::InvokeInst, or llvm:FuncletPadInst.
##
##  @see llvm::CallInst::getNumArgOperands()
##  @see llvm::InvokeInst::getNumArgOperands()
##  @see llvm::FuncletPadInst::getNumArgOperands()
##

proc getNumArgOperands*(instr: ValueRef): cuint {.importc: "LLVMGetNumArgOperands",
    dynlib: LLVMLib.}
## *
##  Set the calling convention for a call instruction.
##
##  This expects an LLVMValueRef that corresponds to a llvm::CallInst or
##  llvm::InvokeInst.
##
##  @see llvm::CallInst::setCallingConv()
##  @see llvm::InvokeInst::setCallingConv()
##

proc setInstructionCallConv*(instr: ValueRef; cc: cuint) {.
    importc: "LLVMSetInstructionCallConv", dynlib: LLVMLib.}
## *
##  Obtain the calling convention for a call instruction.
##
##  This is the opposite of LLVMSetInstructionCallConv(). Reads its
##  usage.
##
##  @see LLVMSetInstructionCallConv()
##

proc getInstructionCallConv*(instr: ValueRef): cuint {.
    importc: "LLVMGetInstructionCallConv", dynlib: LLVMLib.}
proc setInstrParamAlignment*(instr: ValueRef; index: cuint; align: cuint) {.
    importc: "LLVMSetInstrParamAlignment", dynlib: LLVMLib.}
proc addCallSiteAttribute*(c: ValueRef; idx: AttributeIndex; a: AttributeRef) {.
    importc: "LLVMAddCallSiteAttribute", dynlib: LLVMLib.}
proc getCallSiteAttributeCount*(c: ValueRef; idx: AttributeIndex): cuint {.
    importc: "LLVMGetCallSiteAttributeCount", dynlib: LLVMLib.}
proc getCallSiteAttributes*(c: ValueRef; idx: AttributeIndex; attrs: ptr AttributeRef) {.
    importc: "LLVMGetCallSiteAttributes", dynlib: LLVMLib.}
proc getCallSiteEnumAttribute*(c: ValueRef; idx: AttributeIndex; kindID: cuint): AttributeRef {.
    importc: "LLVMGetCallSiteEnumAttribute", dynlib: LLVMLib.}
proc getCallSiteStringAttribute*(c: ValueRef; idx: AttributeIndex; k: cstring;
                                kLen: cuint): AttributeRef {.
    importc: "LLVMGetCallSiteStringAttribute", dynlib: LLVMLib.}
proc removeCallSiteEnumAttribute*(c: ValueRef; idx: AttributeIndex; kindID: cuint) {.
    importc: "LLVMRemoveCallSiteEnumAttribute", dynlib: LLVMLib.}
proc removeCallSiteStringAttribute*(c: ValueRef; idx: AttributeIndex; k: cstring;
                                   kLen: cuint) {.
    importc: "LLVMRemoveCallSiteStringAttribute", dynlib: LLVMLib.}
## *
##  Obtain the function type called by this instruction.
##
##  @see llvm::CallBase::getFunctionType()
##

proc getCalledFunctionType*(c: ValueRef): TypeRef {.
    importc: "LLVMGetCalledFunctionType", dynlib: LLVMLib.}
## *
##  Obtain the pointer to the function invoked by this instruction.
##
##  This expects an LLVMValueRef that corresponds to a llvm::CallInst or
##  llvm::InvokeInst.
##
##  @see llvm::CallInst::getCalledValue()
##  @see llvm::InvokeInst::getCalledValue()
##

proc getCalledValue*(instr: ValueRef): ValueRef {.importc: "LLVMGetCalledValue",
    dynlib: LLVMLib.}
## *
##  Obtain whether a call instruction is a tail call.
##
##  This only works on llvm::CallInst instructions.
##
##  @see llvm::CallInst::isTailCall()
##

proc isTailCall*(callInst: ValueRef): Bool {.importc: "LLVMIsTailCall",
    dynlib: LLVMLib.}
## *
##  Set whether a call instruction is a tail call.
##
##  This only works on llvm::CallInst instructions.
##
##  @see llvm::CallInst::setTailCall()
##

proc setTailCall*(callInst: ValueRef; isTailCall: Bool) {.importc: "LLVMSetTailCall",
    dynlib: LLVMLib.}
## *
##  Return the normal destination basic block.
##
##  This only works on llvm::InvokeInst instructions.
##
##  @see llvm::InvokeInst::getNormalDest()
##

proc getNormalDest*(invokeInst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetNormalDest", dynlib: LLVMLib.}
## *
##  Return the unwind destination basic block.
##
##  Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
##  llvm::CatchSwitchInst instructions.
##
##  @see llvm::InvokeInst::getUnwindDest()
##  @see llvm::CleanupReturnInst::getUnwindDest()
##  @see llvm::CatchSwitchInst::getUnwindDest()
##

proc getUnwindDest*(invokeInst: ValueRef): BasicBlockRef {.
    importc: "LLVMGetUnwindDest", dynlib: LLVMLib.}
## *
##  Set the normal destination basic block.
##
##  This only works on llvm::InvokeInst instructions.
##
##  @see llvm::InvokeInst::setNormalDest()
##

proc setNormalDest*(invokeInst: ValueRef; b: BasicBlockRef) {.
    importc: "LLVMSetNormalDest", dynlib: LLVMLib.}
## *
##  Set the unwind destination basic block.
##
##  Works on llvm::InvokeInst, llvm::CleanupReturnInst, and
##  llvm::CatchSwitchInst instructions.
##
##  @see llvm::InvokeInst::setUnwindDest()
##  @see llvm::CleanupReturnInst::setUnwindDest()
##  @see llvm::CatchSwitchInst::setUnwindDest()
##

proc setUnwindDest*(invokeInst: ValueRef; b: BasicBlockRef) {.
    importc: "LLVMSetUnwindDest", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionTerminator Terminators
##
##  Functions in this group only apply to instructions for which
##  LLVMIsATerminatorInst returns true.
##
##  @{
##
## *
##  Return the number of successors that this terminator has.
##
##  @see llvm::Instruction::getNumSuccessors
##

proc getNumSuccessors*(term: ValueRef): cuint {.importc: "LLVMGetNumSuccessors",
    dynlib: LLVMLib.}
## *
##  Return the specified successor.
##
##  @see llvm::Instruction::getSuccessor
##

proc getSuccessor*(term: ValueRef; i: cuint): BasicBlockRef {.
    importc: "LLVMGetSuccessor", dynlib: LLVMLib.}
## *
##  Update the specified successor to point at the provided block.
##
##  @see llvm::Instruction::setSuccessor
##

proc setSuccessor*(term: ValueRef; i: cuint; `block`: BasicBlockRef) {.
    importc: "LLVMSetSuccessor", dynlib: LLVMLib.}
## *
##  Return if a branch is conditional.
##
##  This only works on llvm::BranchInst instructions.
##
##  @see llvm::BranchInst::isConditional
##

proc isConditional*(branch: ValueRef): Bool {.importc: "LLVMIsConditional",
    dynlib: LLVMLib.}
## *
##  Return the condition of a branch instruction.
##
##  This only works on llvm::BranchInst instructions.
##
##  @see llvm::BranchInst::getCondition
##

proc getCondition*(branch: ValueRef): ValueRef {.importc: "LLVMGetCondition",
    dynlib: LLVMLib.}
## *
##  Set the condition of a branch instruction.
##
##  This only works on llvm::BranchInst instructions.
##
##  @see llvm::BranchInst::setCondition
##

proc setCondition*(branch: ValueRef; cond: ValueRef) {.importc: "LLVMSetCondition",
    dynlib: LLVMLib.}
## *
##  Obtain the default destination basic block of a switch instruction.
##
##  This only works on llvm::SwitchInst instructions.
##
##  @see llvm::SwitchInst::getDefaultDest()
##

proc getSwitchDefaultDest*(switchInstr: ValueRef): BasicBlockRef {.
    importc: "LLVMGetSwitchDefaultDest", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionAlloca Allocas
##
##  Functions in this group only apply to instructions that map to
##  llvm::AllocaInst instances.
##
##  @{
##
## *
##  Obtain the type that is being allocated by the alloca instruction.
##

proc getAllocatedType*(alloca: ValueRef): TypeRef {.importc: "LLVMGetAllocatedType",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionGetElementPointer GEPs
##
##  Functions in this group only apply to instructions that map to
##  llvm::GetElementPtrInst instances.
##
##  @{
##
## *
##  Check whether the given GEP instruction is inbounds.
##

proc isInBounds*(gep: ValueRef): Bool {.importc: "LLVMIsInBounds", dynlib: LLVMLib.}
## *
##  Set the given GEP instruction to be inbounds or not.
##

proc setIsInBounds*(gep: ValueRef; inBounds: Bool) {.importc: "LLVMSetIsInBounds",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionPHINode PHI Nodes
##
##  Functions in this group only apply to instructions that map to
##  llvm::PHINode instances.
##
##  @{
##
## *
##  Add an incoming value to the end of a PHI list.
##

proc addIncoming*(phiNode: ValueRef; incomingValues: ptr ValueRef;
                 incomingBlocks: ptr BasicBlockRef; count: cuint) {.
    importc: "LLVMAddIncoming", dynlib: LLVMLib.}
## *
##  Obtain the number of incoming basic blocks to a PHI node.
##

proc countIncoming*(phiNode: ValueRef): cuint {.importc: "LLVMCountIncoming",
    dynlib: LLVMLib.}
## *
##  Obtain an incoming value to a PHI node as an LLVMValueRef.
##

proc getIncomingValue*(phiNode: ValueRef; index: cuint): ValueRef {.
    importc: "LLVMGetIncomingValue", dynlib: LLVMLib.}
## *
##  Obtain an incoming value to a PHI node as an LLVMBasicBlockRef.
##

proc getIncomingBlock*(phiNode: ValueRef; index: cuint): BasicBlockRef {.
    importc: "LLVMGetIncomingBlock", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreValueInstructionExtractValue ExtractValue
##  @defgroup LLVMCCoreValueInstructionInsertValue InsertValue
##
##  Functions in this group only apply to instructions that map to
##  llvm::ExtractValue and llvm::InsertValue instances.
##
##  @{
##
## *
##  Obtain the number of indices.
##  NB: This also works on GEP.
##

proc getNumIndices*(inst: ValueRef): cuint {.importc: "LLVMGetNumIndices",
    dynlib: LLVMLib.}
## *
##  Obtain the indices as an array.
##

proc getIndices*(inst: ValueRef): ptr cuint {.importc: "LLVMGetIndices",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##
## *
##  @defgroup LLVMCCoreInstructionBuilder Instruction Builders
##
##  An instruction builder represents a point within a basic block and is
##  the exclusive means of building instructions using the C interface.
##
##  @{
##

proc createBuilderInContext*(c: ContextRef): BuilderRef {.
    importc: "LLVMCreateBuilderInContext", dynlib: LLVMLib.}
proc createBuilder*(): BuilderRef {.importc: "LLVMCreateBuilder", dynlib: LLVMLib.}
proc positionBuilder*(builder: BuilderRef; `block`: BasicBlockRef; instr: ValueRef) {.
    importc: "LLVMPositionBuilder", dynlib: LLVMLib.}
proc positionBuilderBefore*(builder: BuilderRef; instr: ValueRef) {.
    importc: "LLVMPositionBuilderBefore", dynlib: LLVMLib.}
proc positionBuilderAtEnd*(builder: BuilderRef; `block`: BasicBlockRef) {.
    importc: "LLVMPositionBuilderAtEnd", dynlib: LLVMLib.}
proc getInsertBlock*(builder: BuilderRef): BasicBlockRef {.
    importc: "LLVMGetInsertBlock", dynlib: LLVMLib.}
proc clearInsertionPosition*(builder: BuilderRef) {.
    importc: "LLVMClearInsertionPosition", dynlib: LLVMLib.}
proc insertIntoBuilder*(builder: BuilderRef; instr: ValueRef) {.
    importc: "LLVMInsertIntoBuilder", dynlib: LLVMLib.}
proc insertIntoBuilderWithName*(builder: BuilderRef; instr: ValueRef; name: cstring) {.
    importc: "LLVMInsertIntoBuilderWithName", dynlib: LLVMLib.}
proc disposeBuilder*(builder: BuilderRef) {.importc: "LLVMDisposeBuilder",
    dynlib: LLVMLib.}
##  Metadata
## *
##  Get location information used by debugging information.
##
##  @see llvm::IRBuilder::getCurrentDebugLocation()
##

proc getCurrentDebugLocation2*(builder: BuilderRef): MetadataRef {.
    importc: "LLVMGetCurrentDebugLocation2", dynlib: LLVMLib.}
## *
##  Set location information used by debugging information.
##
##  To clear the location metadata of the given instruction, pass NULL to \p Loc.
##
##  @see llvm::IRBuilder::SetCurrentDebugLocation()
##

proc setCurrentDebugLocation2*(builder: BuilderRef; loc: MetadataRef) {.
    importc: "LLVMSetCurrentDebugLocation2", dynlib: LLVMLib.}
## *
##  Attempts to set the debug location for the given instruction using the
##  current debug location for the given builder.  If the builder has no current
##  debug location, this function is a no-op.
##
##  @see llvm::IRBuilder::SetInstDebugLocation()
##

proc setInstDebugLocation*(builder: BuilderRef; inst: ValueRef) {.
    importc: "LLVMSetInstDebugLocation", dynlib: LLVMLib.}
## *
##  Get the dafult floating-point math metadata for a given builder.
##
##  @see llvm::IRBuilder::getDefaultFPMathTag()
##

proc builderGetDefaultFPMathTag*(builder: BuilderRef): MetadataRef {.
    importc: "LLVMBuilderGetDefaultFPMathTag", dynlib: LLVMLib.}
## *
##  Set the default floating-point math metadata for the given builder.
##
##  To clear the metadata, pass NULL to \p FPMathTag.
##
##  @see llvm::IRBuilder::setDefaultFPMathTag()
##

proc builderSetDefaultFPMathTag*(builder: BuilderRef; fPMathTag: MetadataRef) {.
    importc: "LLVMBuilderSetDefaultFPMathTag", dynlib: LLVMLib.}
## *
##  Deprecated: Passing the NULL location will crash.
##  Use LLVMGetCurrentDebugLocation2 instead.
##

proc setCurrentDebugLocation*(builder: BuilderRef; L: ValueRef) {.
    importc: "LLVMSetCurrentDebugLocation", dynlib: LLVMLib.}
## *
##  Deprecated: Returning the NULL location will crash.
##  Use LLVMGetCurrentDebugLocation2 instead.
##

proc getCurrentDebugLocation*(builder: BuilderRef): ValueRef {.
    importc: "LLVMGetCurrentDebugLocation", dynlib: LLVMLib.}
##  Terminators

proc buildRetVoid*(a1: BuilderRef): ValueRef {.importc: "LLVMBuildRetVoid",
    dynlib: LLVMLib.}
proc buildRet*(a1: BuilderRef; v: ValueRef): ValueRef {.importc: "LLVMBuildRet",
    dynlib: LLVMLib.}
proc buildAggregateRet*(a1: BuilderRef; retVals: ptr ValueRef; n: cuint): ValueRef {.
    importc: "LLVMBuildAggregateRet", dynlib: LLVMLib.}
proc buildBr*(a1: BuilderRef; dest: BasicBlockRef): ValueRef {.importc: "LLVMBuildBr",
    dynlib: LLVMLib.}
proc buildCondBr*(a1: BuilderRef; `if`: ValueRef; then: BasicBlockRef;
                 `else`: BasicBlockRef): ValueRef {.importc: "LLVMBuildCondBr",
    dynlib: LLVMLib.}
proc buildSwitch*(a1: BuilderRef; v: ValueRef; `else`: BasicBlockRef; numCases: cuint): ValueRef {.
    importc: "LLVMBuildSwitch", dynlib: LLVMLib.}
proc buildIndirectBr*(b: BuilderRef; `addr`: ValueRef; numDests: cuint): ValueRef {.
    importc: "LLVMBuildIndirectBr", dynlib: LLVMLib.}
##  LLVMBuildInvoke is deprecated in favor of LLVMBuildInvoke2, in preparation
##  for opaque pointer types.

proc buildInvoke*(a1: BuilderRef; fn: ValueRef; args: ptr ValueRef; numArgs: cuint;
                 then: BasicBlockRef; catch: BasicBlockRef; name: cstring): ValueRef {.
    importc: "LLVMBuildInvoke", dynlib: LLVMLib.}
proc buildInvoke2*(a1: BuilderRef; ty: TypeRef; fn: ValueRef; args: ptr ValueRef;
                  numArgs: cuint; then: BasicBlockRef; catch: BasicBlockRef;
                  name: cstring): ValueRef {.importc: "LLVMBuildInvoke2",
    dynlib: LLVMLib.}
proc buildUnreachable*(a1: BuilderRef): ValueRef {.importc: "LLVMBuildUnreachable",
    dynlib: LLVMLib.}
##  Exception Handling

proc buildResume*(b: BuilderRef; exn: ValueRef): ValueRef {.
    importc: "LLVMBuildResume", dynlib: LLVMLib.}
proc buildLandingPad*(b: BuilderRef; ty: TypeRef; persFn: ValueRef; numClauses: cuint;
                     name: cstring): ValueRef {.importc: "LLVMBuildLandingPad",
    dynlib: LLVMLib.}
proc buildCleanupRet*(b: BuilderRef; catchPad: ValueRef; bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBuildCleanupRet", dynlib: LLVMLib.}
proc buildCatchRet*(b: BuilderRef; catchPad: ValueRef; bb: BasicBlockRef): ValueRef {.
    importc: "LLVMBuildCatchRet", dynlib: LLVMLib.}
proc buildCatchPad*(b: BuilderRef; parentPad: ValueRef; args: ptr ValueRef;
                   numArgs: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildCatchPad", dynlib: LLVMLib.}
proc buildCleanupPad*(b: BuilderRef; parentPad: ValueRef; args: ptr ValueRef;
                     numArgs: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildCleanupPad", dynlib: LLVMLib.}
proc buildCatchSwitch*(b: BuilderRef; parentPad: ValueRef; unwindBB: BasicBlockRef;
                      numHandlers: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildCatchSwitch", dynlib: LLVMLib.}
##  Add a case to the switch instruction

proc addCase*(switch: ValueRef; onVal: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddCase", dynlib: LLVMLib.}
##  Add a destination to the indirectbr instruction

proc addDestination*(indirectBr: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddDestination", dynlib: LLVMLib.}
##  Get the number of clauses on the landingpad instruction

proc getNumClauses*(landingPad: ValueRef): cuint {.importc: "LLVMGetNumClauses",
    dynlib: LLVMLib.}
##  Get the value of the clause at idnex Idx on the landingpad instruction

proc getClause*(landingPad: ValueRef; idx: cuint): ValueRef {.
    importc: "LLVMGetClause", dynlib: LLVMLib.}
##  Add a catch or filter clause to the landingpad instruction

proc addClause*(landingPad: ValueRef; clauseVal: ValueRef) {.
    importc: "LLVMAddClause", dynlib: LLVMLib.}
##  Get the 'cleanup' flag in the landingpad instruction

proc isCleanup*(landingPad: ValueRef): Bool {.importc: "LLVMIsCleanup",
    dynlib: LLVMLib.}
##  Set the 'cleanup' flag in the landingpad instruction

proc setCleanup*(landingPad: ValueRef; val: Bool) {.importc: "LLVMSetCleanup",
    dynlib: LLVMLib.}
##  Add a destination to the catchswitch instruction

proc addHandler*(catchSwitch: ValueRef; dest: BasicBlockRef) {.
    importc: "LLVMAddHandler", dynlib: LLVMLib.}
##  Get the number of handlers on the catchswitch instruction

proc getNumHandlers*(catchSwitch: ValueRef): cuint {.importc: "LLVMGetNumHandlers",
    dynlib: LLVMLib.}
## *
##  Obtain the basic blocks acting as handlers for a catchswitch instruction.
##
##  The Handlers parameter should point to a pre-allocated array of
##  LLVMBasicBlockRefs at least LLVMGetNumHandlers() large. On return, the
##  first LLVMGetNumHandlers() entries in the array will be populated
##  with LLVMBasicBlockRef instances.
##
##  @param CatchSwitch The catchswitch instruction to operate on.
##  @param Handlers Memory address of an array to be filled with basic blocks.
##

proc getHandlers*(catchSwitch: ValueRef; handlers: ptr BasicBlockRef) {.
    importc: "LLVMGetHandlers", dynlib: LLVMLib.}
##  Funclets
##  Get the number of funcletpad arguments.

proc getArgOperand*(funclet: ValueRef; i: cuint): ValueRef {.
    importc: "LLVMGetArgOperand", dynlib: LLVMLib.}
##  Set a funcletpad argument at the given index.

proc setArgOperand*(funclet: ValueRef; i: cuint; value: ValueRef) {.
    importc: "LLVMSetArgOperand", dynlib: LLVMLib.}
## *
##  Get the parent catchswitch instruction of a catchpad instruction.
##
##  This only works on llvm::CatchPadInst instructions.
##
##  @see llvm::CatchPadInst::getCatchSwitch()
##

proc getParentCatchSwitch*(catchPad: ValueRef): ValueRef {.
    importc: "LLVMGetParentCatchSwitch", dynlib: LLVMLib.}
## *
##  Set the parent catchswitch instruction of a catchpad instruction.
##
##  This only works on llvm::CatchPadInst instructions.
##
##  @see llvm::CatchPadInst::setCatchSwitch()
##

proc setParentCatchSwitch*(catchPad: ValueRef; catchSwitch: ValueRef) {.
    importc: "LLVMSetParentCatchSwitch", dynlib: LLVMLib.}
##  Arithmetic

proc buildAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAdd", dynlib: LLVMLib.}
proc buildNSWAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWAdd", dynlib: LLVMLib.}
proc buildNUWAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWAdd", dynlib: LLVMLib.}
proc buildFAdd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFAdd", dynlib: LLVMLib.}
proc buildSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSub", dynlib: LLVMLib.}
proc buildNSWSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWSub", dynlib: LLVMLib.}
proc buildNUWSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWSub", dynlib: LLVMLib.}
proc buildFSub*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFSub", dynlib: LLVMLib.}
proc buildMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildMul", dynlib: LLVMLib.}
proc buildNSWMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWMul", dynlib: LLVMLib.}
proc buildNUWMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWMul", dynlib: LLVMLib.}
proc buildFMul*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFMul", dynlib: LLVMLib.}
proc buildUDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildUDiv", dynlib: LLVMLib.}
proc buildExactUDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildExactUDiv", dynlib: LLVMLib.}
proc buildSDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSDiv", dynlib: LLVMLib.}
proc buildExactSDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildExactSDiv", dynlib: LLVMLib.}
proc buildFDiv*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFDiv", dynlib: LLVMLib.}
proc buildURem*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildURem", dynlib: LLVMLib.}
proc buildSRem*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSRem", dynlib: LLVMLib.}
proc buildFRem*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFRem", dynlib: LLVMLib.}
proc buildShl*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildShl", dynlib: LLVMLib.}
proc buildLShr*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildLShr", dynlib: LLVMLib.}
proc buildAShr*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAShr", dynlib: LLVMLib.}
proc buildAnd*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAnd", dynlib: LLVMLib.}
proc buildOr*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildOr", dynlib: LLVMLib.}
proc buildXor*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildXor", dynlib: LLVMLib.}
proc buildBinOp*(b: BuilderRef; op: Opcode; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildBinOp", dynlib: LLVMLib.}
proc buildNeg*(a1: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNeg", dynlib: LLVMLib.}
proc buildNSWNeg*(b: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNSWNeg", dynlib: LLVMLib.}
proc buildNUWNeg*(b: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNUWNeg", dynlib: LLVMLib.}
proc buildFNeg*(a1: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFNeg", dynlib: LLVMLib.}
proc buildNot*(a1: BuilderRef; v: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildNot", dynlib: LLVMLib.}
##  Memory

proc buildMalloc*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildMalloc", dynlib: LLVMLib.}
proc buildArrayMalloc*(a1: BuilderRef; ty: TypeRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildArrayMalloc", dynlib: LLVMLib.}
## *
##  Creates and inserts a memset to the specified pointer and the
##  specified value.
##
##  @see llvm::IRRBuilder::CreateMemSet()
##

proc buildMemSet*(b: BuilderRef; `ptr`: ValueRef; val: ValueRef; len: ValueRef;
                 align: cuint): ValueRef {.importc: "LLVMBuildMemSet",
                                        dynlib: LLVMLib.}
## *
##  Creates and inserts a memcpy between the specified pointers.
##
##  @see llvm::IRRBuilder::CreateMemCpy()
##

proc buildMemCpy*(b: BuilderRef; dst: ValueRef; dstAlign: cuint; src: ValueRef;
                 srcAlign: cuint; size: ValueRef): ValueRef {.
    importc: "LLVMBuildMemCpy", dynlib: LLVMLib.}
## *
##  Creates and inserts a memmove between the specified pointers.
##
##  @see llvm::IRRBuilder::CreateMemMove()
##

proc buildMemMove*(b: BuilderRef; dst: ValueRef; dstAlign: cuint; src: ValueRef;
                  srcAlign: cuint; size: ValueRef): ValueRef {.
    importc: "LLVMBuildMemMove", dynlib: LLVMLib.}
proc buildAlloca*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAlloca", dynlib: LLVMLib.}
proc buildArrayAlloca*(a1: BuilderRef; ty: TypeRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildArrayAlloca", dynlib: LLVMLib.}
proc buildFree*(a1: BuilderRef; pointerVal: ValueRef): ValueRef {.
    importc: "LLVMBuildFree", dynlib: LLVMLib.}
##  LLVMBuildLoad is deprecated in favor of LLVMBuildLoad2, in preparation for
##  opaque pointer types.

proc buildLoad*(a1: BuilderRef; pointerVal: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildLoad", dynlib: LLVMLib.}
proc buildLoad2*(a1: BuilderRef; ty: TypeRef; pointerVal: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildLoad2", dynlib: LLVMLib.}
proc buildStore*(a1: BuilderRef; val: ValueRef; `ptr`: ValueRef): ValueRef {.
    importc: "LLVMBuildStore", dynlib: LLVMLib.}
##  LLVMBuildGEP, LLVMBuildInBoundsGEP, and LLVMBuildStructGEP are deprecated in
##  favor of LLVMBuild*GEP2, in preparation for opaque pointer types.

proc buildGEP*(b: BuilderRef; pointer: ValueRef; indices: ptr ValueRef;
              numIndices: cuint; name: cstring): ValueRef {.importc: "LLVMBuildGEP",
    dynlib: LLVMLib.}
proc buildInBoundsGEP*(b: BuilderRef; pointer: ValueRef; indices: ptr ValueRef;
                      numIndices: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildInBoundsGEP", dynlib: LLVMLib.}
proc buildStructGEP*(b: BuilderRef; pointer: ValueRef; idx: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildStructGEP", dynlib: LLVMLib.}
proc buildGEP2*(b: BuilderRef; ty: TypeRef; pointer: ValueRef; indices: ptr ValueRef;
               numIndices: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildGEP2", dynlib: LLVMLib.}
proc buildInBoundsGEP2*(b: BuilderRef; ty: TypeRef; pointer: ValueRef;
                       indices: ptr ValueRef; numIndices: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildInBoundsGEP2", dynlib: LLVMLib.}
proc buildStructGEP2*(b: BuilderRef; ty: TypeRef; pointer: ValueRef; idx: cuint;
                     name: cstring): ValueRef {.importc: "LLVMBuildStructGEP2",
    dynlib: LLVMLib.}
proc buildGlobalString*(b: BuilderRef; str: cstring; name: cstring): ValueRef {.
    importc: "LLVMBuildGlobalString", dynlib: LLVMLib.}
proc buildGlobalStringPtr*(b: BuilderRef; str: cstring; name: cstring): ValueRef {.
    importc: "LLVMBuildGlobalStringPtr", dynlib: LLVMLib.}
proc getVolatile*(memoryAccessInst: ValueRef): Bool {.importc: "LLVMGetVolatile",
    dynlib: LLVMLib.}
proc setVolatile*(memoryAccessInst: ValueRef; isVolatile: Bool) {.
    importc: "LLVMSetVolatile", dynlib: LLVMLib.}
proc getOrdering*(memoryAccessInst: ValueRef): AtomicOrdering {.
    importc: "LLVMGetOrdering", dynlib: LLVMLib.}
proc setOrdering*(memoryAccessInst: ValueRef; ordering: AtomicOrdering) {.
    importc: "LLVMSetOrdering", dynlib: LLVMLib.}
##  Casts

proc buildTrunc*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildTrunc", dynlib: LLVMLib.}
proc buildZExt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildZExt", dynlib: LLVMLib.}
proc buildSExt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSExt", dynlib: LLVMLib.}
proc buildFPToUI*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPToUI", dynlib: LLVMLib.}
proc buildFPToSI*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPToSI", dynlib: LLVMLib.}
proc buildUIToFP*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildUIToFP", dynlib: LLVMLib.}
proc buildSIToFP*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSIToFP", dynlib: LLVMLib.}
proc buildFPTrunc*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPTrunc", dynlib: LLVMLib.}
proc buildFPExt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPExt", dynlib: LLVMLib.}
proc buildPtrToInt*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildPtrToInt", dynlib: LLVMLib.}
proc buildIntToPtr*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIntToPtr", dynlib: LLVMLib.}
proc buildBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildBitCast", dynlib: LLVMLib.}
proc buildAddrSpaceCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildAddrSpaceCast", dynlib: LLVMLib.}
proc buildZExtOrBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildZExtOrBitCast", dynlib: LLVMLib.}
proc buildSExtOrBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildSExtOrBitCast", dynlib: LLVMLib.}
proc buildTruncOrBitCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildTruncOrBitCast", dynlib: LLVMLib.}
proc buildCast*(b: BuilderRef; op: Opcode; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildCast", dynlib: LLVMLib.}
proc buildPointerCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildPointerCast", dynlib: LLVMLib.}
proc buildIntCast2*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; isSigned: Bool;
                   name: cstring): ValueRef {.importc: "LLVMBuildIntCast2",
    dynlib: LLVMLib.}
proc buildFPCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildFPCast", dynlib: LLVMLib.}
## * Deprecated: This cast is always signed. Use LLVMBuildIntCast2 instead.

proc buildIntCast*(a1: BuilderRef; val: ValueRef; destTy: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIntCast", dynlib: LLVMLib.}
  ## Signed cast!
##  Comparisons

proc buildICmp*(a1: BuilderRef; op: IntPredicate; lhs: ValueRef; rhs: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMBuildICmp", dynlib: LLVMLib.}
proc buildFCmp*(a1: BuilderRef; op: RealPredicate; lhs: ValueRef; rhs: ValueRef;
               name: cstring): ValueRef {.importc: "LLVMBuildFCmp", dynlib: LLVMLib.}
##  Miscellaneous instructions

proc buildPhi*(a1: BuilderRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildPhi", dynlib: LLVMLib.}
##  LLVMBuildCall is deprecated in favor of LLVMBuildCall2, in preparation for
##  opaque pointer types.

proc buildCall*(a1: BuilderRef; fn: ValueRef; args: ptr ValueRef; numArgs: cuint;
               name: cstring): ValueRef {.importc: "LLVMBuildCall", dynlib: LLVMLib.}
proc buildCall2*(a1: BuilderRef; a2: TypeRef; fn: ValueRef; args: ptr ValueRef;
                numArgs: cuint; name: cstring): ValueRef {.importc: "LLVMBuildCall2",
    dynlib: LLVMLib.}
proc buildSelect*(a1: BuilderRef; `if`: ValueRef; then: ValueRef; `else`: ValueRef;
                 name: cstring): ValueRef {.importc: "LLVMBuildSelect",
    dynlib: LLVMLib.}
proc buildVAArg*(a1: BuilderRef; list: ValueRef; ty: TypeRef; name: cstring): ValueRef {.
    importc: "LLVMBuildVAArg", dynlib: LLVMLib.}
proc buildExtractElement*(a1: BuilderRef; vecVal: ValueRef; index: ValueRef;
                         name: cstring): ValueRef {.
    importc: "LLVMBuildExtractElement", dynlib: LLVMLib.}
proc buildInsertElement*(a1: BuilderRef; vecVal: ValueRef; eltVal: ValueRef;
                        index: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildInsertElement", dynlib: LLVMLib.}
proc buildShuffleVector*(a1: BuilderRef; v1: ValueRef; v2: ValueRef; mask: ValueRef;
                        name: cstring): ValueRef {.
    importc: "LLVMBuildShuffleVector", dynlib: LLVMLib.}
proc buildExtractValue*(a1: BuilderRef; aggVal: ValueRef; index: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildExtractValue", dynlib: LLVMLib.}
proc buildInsertValue*(a1: BuilderRef; aggVal: ValueRef; eltVal: ValueRef;
                      index: cuint; name: cstring): ValueRef {.
    importc: "LLVMBuildInsertValue", dynlib: LLVMLib.}
proc buildIsNull*(a1: BuilderRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIsNull", dynlib: LLVMLib.}
proc buildIsNotNull*(a1: BuilderRef; val: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildIsNotNull", dynlib: LLVMLib.}
proc buildPtrDiff*(a1: BuilderRef; lhs: ValueRef; rhs: ValueRef; name: cstring): ValueRef {.
    importc: "LLVMBuildPtrDiff", dynlib: LLVMLib.}
proc buildFence*(b: BuilderRef; ordering: AtomicOrdering; singleThread: Bool;
                name: cstring): ValueRef {.importc: "LLVMBuildFence", dynlib: LLVMLib.}
proc buildAtomicRMW*(b: BuilderRef; op: AtomicRMWBinOp; `ptr`: ValueRef; val: ValueRef;
                    ordering: AtomicOrdering; singleThread: Bool): ValueRef {.
    importc: "LLVMBuildAtomicRMW", dynlib: LLVMLib.}
proc buildAtomicCmpXchg*(b: BuilderRef; `ptr`: ValueRef; cmp: ValueRef; new: ValueRef;
                        successOrdering: AtomicOrdering;
                        failureOrdering: AtomicOrdering; singleThread: Bool): ValueRef {.
    importc: "LLVMBuildAtomicCmpXchg", dynlib: LLVMLib.}
proc isAtomicSingleThread*(atomicInst: ValueRef): Bool {.
    importc: "LLVMIsAtomicSingleThread", dynlib: LLVMLib.}
proc setAtomicSingleThread*(atomicInst: ValueRef; singleThread: Bool) {.
    importc: "LLVMSetAtomicSingleThread", dynlib: LLVMLib.}
proc getCmpXchgSuccessOrdering*(cmpXchgInst: ValueRef): AtomicOrdering {.
    importc: "LLVMGetCmpXchgSuccessOrdering", dynlib: LLVMLib.}
proc setCmpXchgSuccessOrdering*(cmpXchgInst: ValueRef; ordering: AtomicOrdering) {.
    importc: "LLVMSetCmpXchgSuccessOrdering", dynlib: LLVMLib.}
proc getCmpXchgFailureOrdering*(cmpXchgInst: ValueRef): AtomicOrdering {.
    importc: "LLVMGetCmpXchgFailureOrdering", dynlib: LLVMLib.}
proc setCmpXchgFailureOrdering*(cmpXchgInst: ValueRef; ordering: AtomicOrdering) {.
    importc: "LLVMSetCmpXchgFailureOrdering", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreModuleProvider Module Providers
##
##  @{
##
## *
##  Changes the type of M so it can be passed to FunctionPassManagers and the
##  JIT.  They take ModuleProviders for historical reasons.
##

proc createModuleProviderForExistingModule*(m: ModuleRef): ModuleProviderRef {.
    importc: "LLVMCreateModuleProviderForExistingModule", dynlib: LLVMLib.}
## *
##  Destroys the module M.
##

proc disposeModuleProvider*(m: ModuleProviderRef) {.
    importc: "LLVMDisposeModuleProvider", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreMemoryBuffers Memory Buffers
##
##  @{
##

proc createMemoryBufferWithContentsOfFile*(path: cstring;
    outMemBuf: ptr MemoryBufferRef; outMessage: cstringArray): Bool {.
    importc: "LLVMCreateMemoryBufferWithContentsOfFile", dynlib: LLVMLib.}
proc createMemoryBufferWithSTDIN*(outMemBuf: ptr MemoryBufferRef;
                                 outMessage: cstringArray): Bool {.
    importc: "LLVMCreateMemoryBufferWithSTDIN", dynlib: LLVMLib.}
proc createMemoryBufferWithMemoryRange*(inputData: cstring; inputDataLength: csize;
                                       bufferName: cstring;
                                       requiresNullTerminator: Bool): MemoryBufferRef {.
    importc: "LLVMCreateMemoryBufferWithMemoryRange", dynlib: LLVMLib.}
proc createMemoryBufferWithMemoryRangeCopy*(inputData: cstring;
    inputDataLength: csize; bufferName: cstring): MemoryBufferRef {.
    importc: "LLVMCreateMemoryBufferWithMemoryRangeCopy", dynlib: LLVMLib.}
proc getBufferStart*(memBuf: MemoryBufferRef): cstring {.
    importc: "LLVMGetBufferStart", dynlib: LLVMLib.}
proc getBufferSize*(memBuf: MemoryBufferRef): csize {.importc: "LLVMGetBufferSize",
    dynlib: LLVMLib.}
proc disposeMemoryBuffer*(memBuf: MemoryBufferRef) {.
    importc: "LLVMDisposeMemoryBuffer", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCorePassRegistry Pass Registry
##
##  @{
##
## * Return the global pass registry, for use with initialization functions.
##     @see llvm::PassRegistry::getPassRegistry

proc getGlobalPassRegistry*(): PassRegistryRef {.
    importc: "LLVMGetGlobalPassRegistry", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCorePassManagers Pass Managers
##
##  @{
##
## * Constructs a new whole-module pass pipeline. This type of pipeline is
##     suitable for link-time optimization and whole-module transformations.
##     @see llvm::PassManager::PassManager

proc createPassManager*(): PassManagerRef {.importc: "LLVMCreatePassManager",
    dynlib: LLVMLib.}
## * Constructs a new function-by-function pass pipeline over the module
##     provider. It does not take ownership of the module provider. This type of
##     pipeline is suitable for code generation and JIT compilation tasks.
##     @see llvm::FunctionPassManager::FunctionPassManager

proc createFunctionPassManagerForModule*(m: ModuleRef): PassManagerRef {.
    importc: "LLVMCreateFunctionPassManagerForModule", dynlib: LLVMLib.}
## * Deprecated: Use LLVMCreateFunctionPassManagerForModule instead.

proc createFunctionPassManager*(mp: ModuleProviderRef): PassManagerRef {.
    importc: "LLVMCreateFunctionPassManager", dynlib: LLVMLib.}
## * Initializes, executes on the provided module, and finalizes all of the
##     passes scheduled in the pass manager. Returns 1 if any of the passes
##     modified the module, 0 otherwise.
##     @see llvm::PassManager::run(Module&)

proc runPassManager*(pm: PassManagerRef; m: ModuleRef): Bool {.
    importc: "LLVMRunPassManager", dynlib: LLVMLib.}
## * Initializes all of the function passes scheduled in the function pass
##     manager. Returns 1 if any of the passes modified the module, 0 otherwise.
##     @see llvm::FunctionPassManager::doInitialization

proc initializeFunctionPassManager*(fpm: PassManagerRef): Bool {.
    importc: "LLVMInitializeFunctionPassManager", dynlib: LLVMLib.}
## * Executes all of the function passes scheduled in the function pass manager
##     on the provided function. Returns 1 if any of the passes modified the
##     function, false otherwise.
##     @see llvm::FunctionPassManager::run(Function&)

proc runFunctionPassManager*(fpm: PassManagerRef; f: ValueRef): Bool {.
    importc: "LLVMRunFunctionPassManager", dynlib: LLVMLib.}
## * Finalizes all of the function passes scheduled in the function pass
##     manager. Returns 1 if any of the passes modified the module, 0 otherwise.
##     @see llvm::FunctionPassManager::doFinalization

proc finalizeFunctionPassManager*(fpm: PassManagerRef): Bool {.
    importc: "LLVMFinalizeFunctionPassManager", dynlib: LLVMLib.}
## * Frees the memory of a pass pipeline. For function pipelines, does not free
##     the module provider.
##     @see llvm::PassManagerBase::~PassManagerBase.

proc disposePassManager*(pm: PassManagerRef) {.importc: "LLVMDisposePassManager",
    dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @defgroup LLVMCCoreThreading Threading
##
##  Handle the structures needed to make LLVM safe for multithreading.
##
##  @{
##
## * Deprecated: Multi-threading can only be enabled/disabled with the compile
##     time define LLVM_ENABLE_THREADS.  This function always returns
##     LLVMIsMultithreaded().

proc startMultithreaded*(): Bool {.importc: "LLVMStartMultithreaded", dynlib: LLVMLib.}
## * Deprecated: Multi-threading can only be enabled/disabled with the compile
##     time define LLVM_ENABLE_THREADS.

proc stopMultithreaded*() {.importc: "LLVMStopMultithreaded", dynlib: LLVMLib.}
## * Check whether LLVM is executing in thread-safe mode or not.
##     @see llvm::llvm_is_multithreaded

proc isMultithreaded*(): Bool {.importc: "LLVMIsMultithreaded", dynlib: LLVMLib.}
## *
##  @}
##
## *
##  @}
##
## *
##  @}
##
