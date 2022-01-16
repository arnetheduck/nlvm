
## ===---------------- llvm-c/Orc.h - OrcV2 C bindings -----------*- C++ -*-===*\
## |*                                                                            *|
## |* Part of the LLVM Project, under the Apache License v2.0 with LLVM          *|
## |* Exceptions.                                                                *|
## |* See https://llvm.org/LICENSE.txt for license information.                  *|
## |* SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception                    *|
## |*                                                                            *|
## |*===----------------------------------------------------------------------===*|
## |*                                                                            *|
## |* This header declares the C interface to libLLVMOrcJIT.a, which implements  *|
## |* JIT compilation of LLVM IR. Minimal documentation of C API specific issues *|
## |* (especially memory ownership rules) is provided. Core Orc concepts are     *|
## |* documented in llvm/docs/ORCv2.rst and APIs are documented in the C++       *|
## |* headers                                                                    *|
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
#  Error, TargetMachine, Types

##  LLVM_C_EXTERN_C_BEGIN
## *
##  Represents an address in the executor process.
##

type
  OrcJITTargetAddress* = uint64_t

## *
##  Represents an address in the executor process.
##

type
  OrcExecutorAddress* = uint64_t

## *
##  Represents generic linkage flags for a symbol definition.
##

type
  JITSymbolGenericFlags* {.size: sizeof(cint).} = enum
    JITSymbolGenericFlagsExported = 1'u shl 0,
    JITSymbolGenericFlagsWeak = 1'u shl 1,
    JITSymbolGenericFlagsCallable = 1'u shl 2,
    JITSymbolGenericFlagsMaterializationSideEffectsOnly = 1'u shl 3


## *
##  Represents target specific flags for a symbol definition.
##

type
  JITSymbolTargetFlags* = uint8_t

## *
##  Represents the linkage flags for a symbol definition.
##

type
  JITSymbolFlags* {.bycopy.} = object
    GenericFlags*: uint8_t
    TargetFlags*: uint8_t


## *
##  Represents an evaluated symbol address and flags.
##

type
  JITEvaluatedSymbol* {.bycopy.} = object
    Address*: OrcExecutorAddress
    Flags*: JITSymbolFlags


## *
##  A reference to an orc::ExecutionSession instance.
##

type
  OrcExecutionSessionRef* = ptr OrcOpaqueExecutionSession

## *
##  Error reporter function.
##

type
  OrcErrorReporterFunction* = proc (Ctx: pointer; Err: ErrorRef)

## *
##  A reference to an orc::SymbolStringPool.
##

type
  OrcSymbolStringPoolRef* = ptr OrcOpaqueSymbolStringPool

## *
##  A reference to an orc::SymbolStringPool table entry.
##

type
  OrcSymbolStringPoolEntryRef* = ptr OrcOpaqueSymbolStringPoolEntry

## *
##  Represents a pair of a symbol name and LLVMJITSymbolFlags.
##

type
  OrcCSymbolFlagsMapPair* {.bycopy.} = object
    Name*: OrcSymbolStringPoolEntryRef
    Flags*: JITSymbolFlags


## *
##  Represents a list of (SymbolStringPtr, JITSymbolFlags) pairs that can be used
##  to construct a SymbolFlagsMap.
##

type
  OrcCSymbolFlagsMapPairs* = ptr OrcCSymbolFlagsMapPair

## *
##  Represents a pair of a symbol name and an evaluated symbol.
##

type
  JITCSymbolMapPair* {.bycopy.} = object
    Name*: OrcSymbolStringPoolEntryRef
    Sym*: JITEvaluatedSymbol


## *
##  Represents a list of (SymbolStringPtr, JITEvaluatedSymbol) pairs that can be
##  used to construct a SymbolMap.
##

type
  OrcCSymbolMapPairs* = ptr JITCSymbolMapPair

## *
##  Represents a SymbolAliasMapEntry
##

type
  OrcCSymbolAliasMapEntry* {.bycopy.} = object
    Name*: OrcSymbolStringPoolEntryRef
    Flags*: JITSymbolFlags


## *
##  Represents a pair of a symbol name and SymbolAliasMapEntry.
##

type
  OrcCSymbolAliasMapPair* {.bycopy.} = object
    Name*: OrcSymbolStringPoolEntryRef
    Entry*: OrcCSymbolAliasMapEntry


## *
##  Represents a list of (SymbolStringPtr, (SymbolStringPtr, JITSymbolFlags))
##  pairs that can be used to construct a SymbolFlagsMap.
##

type
  OrcCSymbolAliasMapPairs* = ptr OrcCSymbolAliasMapPair

## *
##  A reference to an orc::JITDylib instance.
##

type
  OrcJITDylibRef* = ptr OrcOpaqueJITDylib

## *
##  Represents a list of OrcSymbolStringPoolEntryRef and the associated
##  length.
##

type
  OrcCSymbolsList* {.bycopy.} = object
    Symbols*: ptr OrcSymbolStringPoolEntryRef
    Length*: csize_t


## *
##  Represents a pair of a JITDylib and OrcCSymbolsList.
##

type
  OrcCDependenceMapPair* {.bycopy.} = object
    JD*: OrcJITDylibRef
    Names*: OrcCSymbolsList


## *
##  Represents a list of (JITDylibRef, (OrcSymbolStringPoolEntryRef*,
##  size_t)) pairs that can be used to construct a SymbolDependenceMap.
##

type
  OrcCDependenceMapPairs* = ptr OrcCDependenceMapPair

## *
##  Lookup kind. This can be used by definition generators when deciding whether
##  to produce a definition for a requested symbol.
##
##  This enum should be kept in sync with llvm::orc::LookupKind.
##

type
  OrcLookupKind* {.size: sizeof(cint).} = enum
    OrcLookupKindStatic, OrcLookupKindDLSym


## *
##  JITDylib lookup flags. This can be used by definition generators when
##  deciding whether to produce a definition for a requested symbol.
##
##  This enum should be kept in sync with llvm::orc::JITDylibLookupFlags.
##

type
  OrcJITDylibLookupFlags* {.size: sizeof(cint).} = enum
    OrcJITDylibLookupFlagsMatchExportedSymbolsOnly,
    OrcJITDylibLookupFlagsMatchAllSymbols


## *
##  Symbol lookup flags for lookup sets. This should be kept in sync with
##  llvm::orc::SymbolLookupFlags.
##

type
  OrcSymbolLookupFlags* {.size: sizeof(cint).} = enum
    OrcSymbolLookupFlagsRequiredSymbol,
    OrcSymbolLookupFlagsWeaklyReferencedSymbol


## *
##  An element type for a symbol lookup set.
##

type
  OrcCLookupSetElement* {.bycopy.} = object
    Name*: OrcSymbolStringPoolEntryRef
    LookupFlags*: OrcSymbolLookupFlags


## *
##  A set of symbols to look up / generate.
##
##  The list is terminated with an element containing a null pointer for the
##  Name field.
##
##  If a client creates an instance of this type then they are responsible for
##  freeing it, and for ensuring that all strings have been retained over the
##  course of its life. Clients receiving a copy from a callback are not
##  responsible for managing lifetime or retain counts.
##

type
  OrcCLookupSet* = ptr OrcCLookupSetElement

## *
##  A reference to a uniquely owned orc::MaterializationUnit instance.
##

type
  OrcMaterializationUnitRef* = ptr OrcOpaqueMaterializationUnit

## *
##  A reference to a uniquely owned orc::MaterializationResponsibility instance.
##
##  Ownership must be passed to a lower-level layer in a JIT stack.
##

type
  OrcMaterializationResponsibilityRef* = ptr OrcOpaqueMaterializationResponsibility

## *
##  A MaterializationUnit materialize callback.
##
##  Ownership of the Ctx and MR arguments passes to the callback which must
##  adhere to the OrcMaterializationResponsibilityRef contract (see comment
##  for that type).
##
##  If this callback is called then the OrcMaterializationUnitDestroy
##  callback will NOT be called.
##

type
  OrcMaterializationUnitMaterializeFunction* = proc (Ctx: pointer;
      MR: OrcMaterializationResponsibilityRef)

## *
##  A MaterializationUnit discard callback.
##
##  Ownership of JD and Symbol remain with the caller: These arguments should
##  not be disposed of or released.
##

type
  OrcMaterializationUnitDiscardFunction* = proc (Ctx: pointer;
      JD: OrcJITDylibRef; Symbol: OrcSymbolStringPoolEntryRef)

## *
##  A MaterializationUnit destruction callback.
##
##  If a custom MaterializationUnit is destroyed before its Materialize
##  function is called then this function will be called to provide an
##  opportunity for the underlying program representation to be destroyed.
##

type
  OrcMaterializationUnitDestroyFunction* = proc (Ctx: pointer)

## *
##  A reference to an orc::ResourceTracker instance.
##

type
  OrcResourceTrackerRef* = ptr OrcOpaqueResourceTracker

## *
##  A reference to an orc::DefinitionGenerator.
##

type
  OrcDefinitionGeneratorRef* = ptr OrcOpaqueDefinitionGenerator

## *
##  An opaque lookup state object. Instances of this type can be captured to
##  suspend a lookup while a custom generator function attempts to produce a
##  definition.
##
##  If a client captures a lookup state object then they must eventually call
##  OrcLookupStateContinueLookup to restart the lookup. This is required
##  in order to release memory allocated for the lookup state, even if errors
##  have occurred while the lookup was suspended (if these errors have made the
##  lookup impossible to complete then it will issue its own error before
##  destruction).
##

type
  OrcLookupStateRef* = ptr OrcOpaqueLookupState

## *
##  A custom generator function. This can be used to create a custom generator
##  object using OrcCreateCustomCAPIDefinitionGenerator. The resulting
##  object can be attached to a JITDylib, via OrcJITDylibAddGenerator, to
##  receive callbacks when lookups fail to match existing definitions.
##
##  GeneratorObj will contain the address of the custom generator object.
##
##  Ctx will contain the context object passed to
##  OrcCreateCustomCAPIDefinitionGenerator.
##
##  LookupState will contain a pointer to an OrcLookupStateRef object. This
##  can optionally be modified to make the definition generation process
##  asynchronous: If the LookupStateRef value is copied, and the original
##  OrcLookupStateRef set to null, the lookup will be suspended. Once the
##  asynchronous definition process has been completed clients must call
##  OrcLookupStateContinueLookup to continue the lookup (this should be
##  done unconditionally, even if errors have occurred in the mean time, to
##  free the lookup state memory and notify the query object of the failures).
##  If LookupState is captured this function must return ErrorSuccess.
##
##  The Kind argument can be inspected to determine the lookup kind (e.g.
##  as-if-during-static-link, or as-if-during-dlsym).
##
##  The JD argument specifies which JITDylib the definitions should be generated
##  into.
##
##  The JDLookupFlags argument can be inspected to determine whether the original
##  lookup included non-exported symobls.
##
##  Finally, the LookupSet argument contains the set of symbols that could not
##  be found in JD already (the set of generation candidates).
##

type
  OrcCAPIDefinitionGeneratorTryToGenerateFunction* = proc (
      GeneratorObj: OrcDefinitionGeneratorRef; Ctx: pointer;
      LookupState: ptr OrcLookupStateRef; Kind: OrcLookupKind;
      JD: OrcJITDylibRef; JDLookupFlags: OrcJITDylibLookupFlags;
      LookupSet: OrcCLookupSet; LookupSetSize: csize_t): ErrorRef

## *
##  Predicate function for SymbolStringPoolEntries.
##

type
  OrcSymbolPredicate* = proc (Ctx: pointer; Sym: OrcSymbolStringPoolEntryRef): cint

## *
##  A reference to an orc::ThreadSafeContext instance.
##

type
  OrcThreadSafeContextRef* = ptr OrcOpaqueThreadSafeContext

## *
##  A reference to an orc::ThreadSafeModule instance.
##

type
  OrcThreadSafeModuleRef* = ptr OrcOpaqueThreadSafeModule

## *
##  A function for inspecting/mutating IR modules, suitable for use with
##  OrcThreadSafeModuleWithModuleDo.
##

type
  OrcGenericIRModuleOperationFunction* = proc (Ctx: pointer; M: ModuleRef): ErrorRef

## *
##  A reference to an orc::JITTargetMachineBuilder instance.
##

type
  OrcJITTargetMachineBuilderRef* = ptr OrcOpaqueJITTargetMachineBuilder

## *
##  A reference to an orc::ObjectLayer instance.
##

type
  OrcObjectLayerRef* = ptr OrcOpaqueObjectLayer

## *
##  A reference to an orc::ObjectLinkingLayer instance.
##

type
  OrcObjectLinkingLayerRef* = ptr OrcOpaqueObjectLinkingLayer

## *
##  A reference to an orc::IRTransformLayer instance.
##

type
  OrcIRTransformLayerRef* = ptr OrcOpaqueIRTransformLayer

## *
##  A function for applying transformations as part of an transform layer.
##
##  Implementations of this type are responsible for managing the lifetime
##  of the Module pointed to by ModInOut: If the ModuleRef value is
##  overwritten then the function is responsible for disposing of the incoming
##  module. If the module is simply accessed/mutated in-place then ownership
##  returns to the caller and the function does not need to do any lifetime
##  management.
##
##  Clients can call OrcLLJITGetIRTransformLayer to obtain the transform
##  layer of a LLJIT instance, and use OrcIRTransformLayerSetTransform
##  to set the function. This can be used to override the default transform
##  layer.
##

type
  OrcIRTransformLayerTransformFunction* = proc (Ctx: pointer;
      ModInOut: ptr OrcThreadSafeModuleRef;
      MR: OrcMaterializationResponsibilityRef): ErrorRef

## *
##  A reference to an orc::ObjectTransformLayer instance.
##

type
  OrcObjectTransformLayerRef* = ptr OrcOpaqueObjectTransformLayer

## *
##  A function for applying transformations to an object file buffer.
##
##  Implementations of this type are responsible for managing the lifetime
##  of the memory buffer pointed to by ObjInOut: If the MemoryBufferRef
##  value is overwritten then the function is responsible for disposing of the
##  incoming buffer. If the buffer is simply accessed/mutated in-place then
##  ownership returns to the caller and the function does not need to do any
##  lifetime management.
##
##  The transform is allowed to return an error, in which case the ObjInOut
##  buffer should be disposed of and set to null.
##

type
  OrcObjectTransformLayerTransformFunction* = proc (Ctx: pointer;
      ObjInOut: ptr MemoryBufferRef): ErrorRef

## *
##  A reference to an orc::IndirectStubsManager instance.
##

type
  OrcIndirectStubsManagerRef* = ptr OrcOpaqueIndirectStubsManager

## *
##  A reference to an orc::LazyCallThroughManager instance.
##

type
  OrcLazyCallThroughManagerRef* = ptr OrcOpaqueLazyCallThroughManager

## *
##  A reference to an orc::DumpObjects object.
##
##  Can be used to dump object files to disk with unique names. Useful as an
##  ObjectTransformLayer transform.
##

type
  OrcDumpObjectsRef* = ptr OrcOpaqueDumpObjects

## *
##  Attach a custom error reporter function to the ExecutionSession.
##
##  The error reporter will be called to deliver failure notices that can not be
##  directly reported to a caller. For example, failure to resolve symbols in
##  the JIT linker is typically reported via the error reporter (callers
##  requesting definitions from the JIT will typically be delivered a
##  FailureToMaterialize error instead).
##

proc orcExecutionSessionSetErrorReporter*(ES: OrcExecutionSessionRef;
    ReportError: OrcErrorReporterFunction; Ctx: pointer) {.
    importc: "LLVMOrcExecutionSessionSetErrorReporter", dynlib: LLVMLib.}
## *
##  Return a reference to the SymbolStringPool for an ExecutionSession.
##
##  Ownership of the pool remains with the ExecutionSession: The caller is
##  not required to free the pool.
##

proc orcExecutionSessionGetSymbolStringPool*(ES: OrcExecutionSessionRef): OrcSymbolStringPoolRef {.
    importc: "LLVMOrcExecutionSessionGetSymbolStringPool", dynlib: LLVMLib.}
## *
##  Clear all unreferenced symbol string pool entries.
##
##  This can be called at any time to release unused entries in the
##  ExecutionSession's string pool. Since it locks the pool (preventing
##  interning of any new strings) it is recommended that it only be called
##  infrequently, ideally when the caller has reason to believe that some
##  entries will have become unreferenced, e.g. after removing a module or
##  closing a JITDylib.
##

proc orcSymbolStringPoolClearDeadEntries*(SSP: OrcSymbolStringPoolRef) {.
    importc: "LLVMOrcSymbolStringPoolClearDeadEntries", dynlib: LLVMLib.}
## *
##  Intern a string in the ExecutionSession's SymbolStringPool and return a
##  reference to it. This increments the ref-count of the pool entry, and the
##  returned value should be released once the client is done with it by
##  calling OrReleaseSymbolStringPoolEntry.
##
##  Since strings are uniqued within the SymbolStringPool
##  OrcSymbolStringPoolEntryRefs can be compared by value to test string
##  equality.
##
##  Note that this function does not perform linker-mangling on the string.
##

proc orcExecutionSessionIntern*(ES: OrcExecutionSessionRef; Name: cstring): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcExecutionSessionIntern", dynlib: LLVMLib.}
## *
##  Increments the ref-count for a SymbolStringPool entry.
##

proc orcRetainSymbolStringPoolEntry*(S: OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcRetainSymbolStringPoolEntry", dynlib: LLVMLib.}
## *
##  Reduces the ref-count for of a SymbolStringPool entry.
##

proc orcReleaseSymbolStringPoolEntry*(S: OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcReleaseSymbolStringPoolEntry", dynlib: LLVMLib.}
proc orcSymbolStringPoolEntryStr*(S: OrcSymbolStringPoolEntryRef): cstring {.
    importc: "LLVMOrcSymbolStringPoolEntryStr", dynlib: LLVMLib.}
## *
##  Reduces the ref-count of a ResourceTracker.
##

proc orcReleaseResourceTracker*(RT: OrcResourceTrackerRef) {.
    importc: "LLVMOrcReleaseResourceTracker", dynlib: LLVMLib.}
## *
##  Transfers tracking of all resources associated with resource tracker SrcRT
##  to resource tracker DstRT.
##

proc orcResourceTrackerTransferTo*(SrcRT: OrcResourceTrackerRef;
                                      DstRT: OrcResourceTrackerRef) {.
    importc: "LLVMOrcResourceTrackerTransferTo", dynlib: LLVMLib.}
## *
##  Remove all resources associated with the given tracker. See
##  ResourceTracker::remove().
##

proc orcResourceTrackerRemove*(RT: OrcResourceTrackerRef): ErrorRef {.
    importc: "LLVMOrcResourceTrackerRemove", dynlib: LLVMLib.}
## *
##  Dispose of a JITDylib::DefinitionGenerator. This should only be called if
##  ownership has not been passed to a JITDylib (e.g. because some error
##  prevented the client from calling OrcJITDylibAddGenerator).
##

proc orcDisposeDefinitionGenerator*(DG: OrcDefinitionGeneratorRef) {.
    importc: "LLVMOrcDisposeDefinitionGenerator", dynlib: LLVMLib.}
## *
##  Dispose of a MaterializationUnit.
##

proc orcDisposeMaterializationUnit*(MU: OrcMaterializationUnitRef) {.
    importc: "LLVMOrcDisposeMaterializationUnit", dynlib: LLVMLib.}
## *
##  Create a custom MaterializationUnit.
##
##  Name is a name for this MaterializationUnit to be used for identification
##  and logging purposes (e.g. if this MaterializationUnit produces an
##  object buffer then the name of that buffer will be derived from this name).
##
##  The Syms list contains the names and linkages of the symbols provided by this
##  unit. This function takes ownership of the elements of the Syms array. The
##  Name fields of the array elements are taken to have been retained for this
##  function. The client should *not* release the elements of the array, but is
##  still responsible for destroyingthe array itself.
##
##  The InitSym argument indicates whether or not this MaterializationUnit
##  contains static initializers. If three are no static initializers (the common
##  case) then this argument should be null. If there are static initializers
##  then InitSym should be set to a unique name that also appears in the Syms
##  list with the JITSymbolGenericFlagsMaterializationSideEffectsOnly flag
##  set. This function takes ownership of the InitSym, which should have been
##  retained twice on behalf of this function: once for the Syms entry and once
##  for InitSym. If clients wish to use the InitSym value after this function
##  returns they must retain it once more for themselves.
##
##  If any of the symbols in the Syms list is looked up then the Materialize
##  function will be called.
##
##  If any of the symbols in the Syms list is overridden then the Discard
##  function will be called.
##
##  The caller owns the underling MaterializationUnit and is responsible for
##  either passing it to a JITDylib (via OrcJITDylibDefine) or disposing
##  of it by calling OrcDisposeMaterializationUnit.
##

proc orcCreateCustomMaterializationUnit*(Name: cstring; Ctx: pointer;
    Syms: OrcCSymbolFlagsMapPairs; NumSyms: csize_t;
    InitSym: OrcSymbolStringPoolEntryRef;
    Materialize: OrcMaterializationUnitMaterializeFunction;
    Discard: OrcMaterializationUnitDiscardFunction;
    Destroy: OrcMaterializationUnitDestroyFunction): OrcMaterializationUnitRef {.
    importc: "LLVMOrcCreateCustomMaterializationUnit", dynlib: LLVMLib.}
## *
##  Create a MaterializationUnit to define the given symbols as pointing to
##  the corresponding raw addresses.
##
##  This function takes ownership of the elements of the Syms array. The Name
##  fields of the array elements are taken to have been retained for this
##  function. This allows the following pattern...
##
##    size_t NumPairs;
##    OrcCSymbolMapPairs Sym;
##    -- Build Syms array --
##    OrcMaterializationUnitRef MU =
##        OrcAbsoluteSymbols(Syms, NumPairs);
##
##  ... without requiring cleanup of the elements of the Sym array afterwards.
##
##  The client is still responsible for deleting the Sym array itself.
##
##  If a client wishes to reuse elements of the Sym array after this call they
##  must explicitly retain each of the elements for themselves.
##

proc orcAbsoluteSymbols*(Syms: OrcCSymbolMapPairs; NumPairs: csize_t): OrcMaterializationUnitRef {.
    importc: "LLVMOrcAbsoluteSymbols", dynlib: LLVMLib.}
## *
##  Create a MaterializationUnit to define lazy re-expots. These are callable
##  entry points that call through to the given symbols.
##
##  This function takes ownership of the CallableAliases array. The Name
##  fields of the array elements are taken to have been retained for this
##  function. This allows the following pattern...
##
##    size_t NumPairs;
##    OrcCSymbolAliasMapPairs CallableAliases;
##    -- Build CallableAliases array --
##    OrcMaterializationUnitRef MU =
##       OrcLazyReexports(LCTM, ISM, JD, CallableAliases, NumPairs);
##
##  ... without requiring cleanup of the elements of the CallableAliases array afterwards.
##
##  The client is still responsible for deleting the CallableAliases array itself.
##
##  If a client wishes to reuse elements of the CallableAliases array after this call they
##  must explicitly retain each of the elements for themselves.
##

proc orcLazyReexports*(LCTM: OrcLazyCallThroughManagerRef;
                          ISM: OrcIndirectStubsManagerRef;
                          SourceRef: OrcJITDylibRef;
                          CallableAliases: OrcCSymbolAliasMapPairs;
                          NumPairs: csize_t): OrcMaterializationUnitRef {.
    importc: "LLVMOrcLazyReexports", dynlib: LLVMLib.}
##  TODO: ImplSymbolMad SrcJDLoc
## *
##  Disposes of the passed MaterializationResponsibility object.
##
##  This should only be done after the symbols covered by the object have either
##  been resolved and emitted (via
##  OrcMaterializationResponsibilityNotifyResolved and
##  OrcMaterializationResponsibilityNotifyEmitted) or failed (via
##  OrcMaterializationResponsibilityFailMaterialization).
##

proc orcDisposeMaterializationResponsibility*(
    MR: OrcMaterializationResponsibilityRef) {.
    importc: "LLVMOrcDisposeMaterializationResponsibility", dynlib: LLVMLib.}
## *
##  Returns the target JITDylib that these symbols are being materialized into.
##

proc orcMaterializationResponsibilityGetTargetDylib*(
    MR: OrcMaterializationResponsibilityRef): OrcJITDylibRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetTargetDylib", dynlib: LLVMLib.}
## *
##  Returns the ExecutionSession for this MaterializationResponsibility.
##

proc orcMaterializationResponsibilityGetExecutionSession*(
    MR: OrcMaterializationResponsibilityRef): OrcExecutionSessionRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetExecutionSession",
    dynlib: LLVMLib.}
## *
##  Returns the symbol flags map for this responsibility instance.
##
##  The length of the array is returned in NumPairs and the caller is responsible
##  for the returned memory and needs to call OrcDisposeCSymbolFlagsMap.
##
##  To use the returned symbols beyond the livetime of the
##  MaterializationResponsibility requires the caller to retain the symbols
##  explicitly.
##

proc orcMaterializationResponsibilityGetSymbols*(
    MR: OrcMaterializationResponsibilityRef; NumPairs: ptr csize_t): OrcCSymbolFlagsMapPairs {.
    importc: "LLVMOrcMaterializationResponsibilityGetSymbols", dynlib: LLVMLib.}
## *
##  Disposes of the passed OrcCSymbolFlagsMap.
##
##  Does not release the entries themselves.
##

proc orcDisposeCSymbolFlagsMap*(Pairs: OrcCSymbolFlagsMapPairs) {.
    importc: "LLVMOrcDisposeCSymbolFlagsMap", dynlib: LLVMLib.}
## *
##  Returns the initialization pseudo-symbol, if any. This symbol will also
##  be present in the SymbolFlagsMap for this MaterializationResponsibility
##  object.
##
##  The returned symbol is not retained over any mutating operation of the
##  MaterializationResponsbility or beyond the lifetime thereof.
##

proc orcMaterializationResponsibilityGetInitializerSymbol*(
    MR: OrcMaterializationResponsibilityRef): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetInitializerSymbol",
    dynlib: LLVMLib.}
## *
##  Returns the names of any symbols covered by this
##  MaterializationResponsibility object that have queries pending. This
##  information can be used to return responsibility for unrequested symbols
##  back to the JITDylib via the delegate method.
##

proc orcMaterializationResponsibilityGetRequestedSymbols*(
    MR: OrcMaterializationResponsibilityRef; NumSymbols: ptr csize_t): ptr OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetRequestedSymbols",
    dynlib: LLVMLib.}
## *
##  Disposes of the passed OrcSymbolStringPoolEntryRef* .
##
##  Does not release the symbols themselves.
##

proc orcDisposeSymbols*(Symbols: ptr OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcDisposeSymbols", dynlib: LLVMLib.}
##
##  Notifies the target JITDylib that the given symbols have been resolved.
##  This will update the given symbols' addresses in the JITDylib, and notify
##  any pending queries on the given symbols of their resolution. The given
##  symbols must be ones covered by this MaterializationResponsibility
##  instance. Individual calls to this method may resolve a subset of the
##  symbols, but all symbols must have been resolved prior to calling emit.
##
##  This method will return an error if any symbols being resolved have been
##  moved to the error state due to the failure of a dependency. If this
##  method returns an error then clients should log it and call
##  OrcMaterializationResponsibilityFailMaterialization. If no dependencies
##  have been registered for the symbols covered by this
##  MaterializationResponsibiility then this method is guaranteed to return
##  ErrorSuccess.
##

proc orcMaterializationResponsibilityNotifyResolved*(
    MR: OrcMaterializationResponsibilityRef; Symbols: OrcCSymbolMapPairs;
    NumPairs: csize_t): ErrorRef {.importc: "LLVMOrcMaterializationResponsibilityNotifyResolved",
                                    dynlib: LLVMLib.}
## *
##  Notifies the target JITDylib (and any pending queries on that JITDylib)
##  that all symbols covered by this MaterializationResponsibility instance
##  have been emitted.
##
##  This method will return an error if any symbols being resolved have been
##  moved to the error state due to the failure of a dependency. If this
##  method returns an error then clients should log it and call
##  OrcMaterializationResponsibilityFailMaterialization.
##  If no dependencies have been registered for the symbols covered by this
##  MaterializationResponsibiility then this method is guaranteed to return
##  ErrorSuccess.
##

proc orcMaterializationResponsibilityNotifyEmitted*(
    MR: OrcMaterializationResponsibilityRef): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityNotifyEmitted", dynlib: LLVMLib.}
## *
##  Attempt to claim responsibility for new definitions. This method can be
##  used to claim responsibility for symbols that are added to a
##  materialization unit during the compilation process (e.g. literal pool
##  symbols). Symbol linkage rules are the same as for symbols that are
##  defined up front: duplicate strong definitions will result in errors.
##  Duplicate weak definitions will be discarded (in which case they will
##  not be added to this responsibility instance).
##
##  This method can be used by materialization units that want to add
##  additional symbols at materialization time (e.g. stubs, compile
##  callbacks, metadata)
##

proc orcMaterializationResponsibilityDefineMaterializing*(
    MR: OrcMaterializationResponsibilityRef;
    Pairs: OrcCSymbolFlagsMapPairs; NumPairs: csize_t): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityDefineMaterializing",
    dynlib: LLVMLib.}
## *
##  Notify all not-yet-emitted covered by this MaterializationResponsibility
##  instance that an error has occurred.
##  This will remove all symbols covered by this MaterializationResponsibilty
##  from the target JITDylib, and send an error to any queries waiting on
##  these symbols.
##

proc orcMaterializationResponsibilityFailMaterialization*(
    MR: OrcMaterializationResponsibilityRef) {.
    importc: "LLVMOrcMaterializationResponsibilityFailMaterialization",
    dynlib: LLVMLib.}
## *
##  Transfers responsibility to the given MaterializationUnit for all
##  symbols defined by that MaterializationUnit. This allows
##  materializers to break up work based on run-time information (e.g.
##  by introspecting which symbols have actually been looked up and
##  materializing only those).
##

proc orcMaterializationResponsibilityReplace*(
    MR: OrcMaterializationResponsibilityRef; MU: OrcMaterializationUnitRef): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityReplace", dynlib: LLVMLib.}
## *
##  Delegates responsibility for the given symbols to the returned
##  materialization responsibility. Useful for breaking up work between
##  threads, or different kinds of materialization processes.
##
##  The caller retains responsibility of the the passed
##  MaterializationResponsibility.
##

proc orcMaterializationResponsibilityDelegate*(
    MR: OrcMaterializationResponsibilityRef;
    Symbols: ptr OrcSymbolStringPoolEntryRef; NumSymbols: csize_t;
    Result: ptr OrcMaterializationResponsibilityRef): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityDelegate", dynlib: LLVMLib.}
## *
##  Adds dependencies to a symbol that the MaterializationResponsibility is
##  responsible for.
##
##  This function takes ownership of Dependencies struct. The Names
##  array have been retained for this function. This allows the following
##  pattern...
##
##    OrcSymbolStringPoolEntryRef Names[] = {...};
##    OrcCDependenceMapPair Dependence = {JD, {Names, sizeof(Names)}}
##    OrcMaterializationResponsibilityAddDependencies(JD, Name, &Dependence,
##  1);
##
##  ... without requiring cleanup of the elements of the Names array afterwards.
##
##  The client is still responsible for deleting the Dependencies.Names array
##  itself.
##

proc orcMaterializationResponsibilityAddDependencies*(
    MR: OrcMaterializationResponsibilityRef;
    Name: OrcSymbolStringPoolEntryRef;
    Dependencies: OrcCDependenceMapPairs; NumPairs: csize_t) {.
    importc: "LLVMOrcMaterializationResponsibilityAddDependencies",
    dynlib: LLVMLib.}
## *
##  Adds dependencies to all symbols that the MaterializationResponsibility is
##  responsible for. See OrcMaterializationResponsibilityAddDependencies for
##  notes about memory responsibility.
##

proc orcMaterializationResponsibilityAddDependenciesForAll*(
    MR: OrcMaterializationResponsibilityRef;
    Dependencies: OrcCDependenceMapPairs; NumPairs: csize_t) {.
    importc: "LLVMOrcMaterializationResponsibilityAddDependenciesForAll",
    dynlib: LLVMLib.}
## *
##  Create a "bare" JITDylib.
##
##  The client is responsible for ensuring that the JITDylib's name is unique,
##  e.g. by calling OrcExecutionSessionGetJTIDylibByName first.
##
##  This call does not install any library code or symbols into the newly
##  created JITDylib. The client is responsible for all configuration.
##

proc orcExecutionSessionCreateBareJITDylib*(ES: OrcExecutionSessionRef;
    Name: cstring): OrcJITDylibRef {.importc: "LLVMOrcExecutionSessionCreateBareJITDylib",
                                      dynlib: LLVMLib.}
## *
##  Create a JITDylib.
##
##  The client is responsible for ensuring that the JITDylib's name is unique,
##  e.g. by calling OrcExecutionSessionGetJTIDylibByName first.
##
##  If a Platform is attached to the ExecutionSession then
##  Platform::setupJITDylib will be called to install standard platform symbols
##  (e.g. standard library interposes). If no Platform is installed then this
##  call is equivalent to ExecutionSessionRefCreateBareJITDylib and will
##  always return success.
##

proc orcExecutionSessionCreateJITDylib*(ES: OrcExecutionSessionRef;
    Result: ptr OrcJITDylibRef; Name: cstring): ErrorRef {.
    importc: "LLVMOrcExecutionSessionCreateJITDylib", dynlib: LLVMLib.}
## *
##  Returns the JITDylib with the given name, or NULL if no such JITDylib
##  exists.
##

proc orcExecutionSessionGetJITDylibByName*(ES: OrcExecutionSessionRef;
    Name: cstring): OrcJITDylibRef {.importc: "LLVMOrcExecutionSessionGetJITDylibByName",
                                      dynlib: LLVMLib.}
## *
##  Return a reference to a newly created resource tracker associated with JD.
##  The tracker is returned with an initial ref-count of 1, and must be released
##  with OrcReleaseResourceTracker when no longer needed.
##

proc orcJITDylibCreateResourceTracker*(JD: OrcJITDylibRef): OrcResourceTrackerRef {.
    importc: "LLVMOrcJITDylibCreateResourceTracker", dynlib: LLVMLib.}
## *
##  Return a reference to the default resource tracker for the given JITDylib.
##  This operation will increase the retain count of the tracker: Clients should
##  call OrcReleaseResourceTracker when the result is no longer needed.
##

proc orcJITDylibGetDefaultResourceTracker*(JD: OrcJITDylibRef): OrcResourceTrackerRef {.
    importc: "LLVMOrcJITDylibGetDefaultResourceTracker", dynlib: LLVMLib.}
## *
##  Add the given MaterializationUnit to the given JITDylib.
##
##  If this operation succeeds then JITDylib JD will take ownership of MU.
##  If the operation fails then ownership remains with the caller who should
##  call OrcDisposeMaterializationUnit to destroy it.
##

proc orcJITDylibDefine*(JD: OrcJITDylibRef;
                           MU: OrcMaterializationUnitRef): ErrorRef {.
    importc: "LLVMOrcJITDylibDefine", dynlib: LLVMLib.}
## *
##  Calls remove on all trackers associated with this JITDylib, see
##  JITDylib::clear().
##

proc orcJITDylibClear*(JD: OrcJITDylibRef): ErrorRef {.
    importc: "LLVMOrcJITDylibClear", dynlib: LLVMLib.}
## *
##  Add a DefinitionGenerator to the given JITDylib.
##
##  The JITDylib will take ownership of the given generator: The client is no
##  longer responsible for managing its memory.
##

proc orcJITDylibAddGenerator*(JD: OrcJITDylibRef;
                                 DG: OrcDefinitionGeneratorRef) {.
    importc: "LLVMOrcJITDylibAddGenerator", dynlib: LLVMLib.}
## *
##  Create a custom generator.
##

proc orcCreateCustomCAPIDefinitionGenerator*(
    F: OrcCAPIDefinitionGeneratorTryToGenerateFunction; Ctx: pointer): OrcDefinitionGeneratorRef {.
    importc: "LLVMOrcCreateCustomCAPIDefinitionGenerator", dynlib: LLVMLib.}
## *
##  Get a DynamicLibrarySearchGenerator that will reflect process symbols into
##  the JITDylib. On success the resulting generator is owned by the client.
##  Ownership is typically transferred by adding the instance to a JITDylib
##  using OrcJITDylibAddGenerator,
##
##  The GlobalPrefix argument specifies the character that appears on the front
##  of linker-mangled symbols for the target platform (e.g. '_' on MachO).
##  If non-null, this character will be stripped from the start of all symbol
##  strings before passing the remaining substring to dlsym.
##
##  The optional Filter and Ctx arguments can be used to supply a symbol name
##  filter: Only symbols for which the filter returns true will be visible to
##  JIT'd code. If the Filter argument is null then all process symbols will
##  be visible to JIT'd code. Note that the symbol name passed to the Filter
##  function is the full mangled symbol: The client is responsible for stripping
##  the global prefix if present.
##

proc orcCreateDynamicLibrarySearchGeneratorForProcess*(
    Result: ptr OrcDefinitionGeneratorRef; GlobalPrefx: char;
    Filter: OrcSymbolPredicate; FilterCtx: pointer): ErrorRef {.
    importc: "LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess",
    dynlib: LLVMLib.}
## *
##  Create a ThreadSafeContext containing a new Context.
##
##  Ownership of the underlying ThreadSafeContext data is shared: Clients
##  can and should dispose of their ThreadSafeContext as soon as they no longer
##  need to refer to it directly. Other references (e.g. from ThreadSafeModules)
##  will keep the data alive as long as it is needed.
##

proc orcCreateNewThreadSafeContext*(): OrcThreadSafeContextRef {.
    importc: "LLVMOrcCreateNewThreadSafeContext", dynlib: LLVMLib.}
## *
##  Get a reference to the wrapped Context.
##

proc orcThreadSafeContextGetContext*(TSCtx: OrcThreadSafeContextRef): ContextRef {.
    importc: "LLVMOrcThreadSafeContextGetContext", dynlib: LLVMLib.}
## *
##  Dispose of a ThreadSafeContext.
##

proc orcDisposeThreadSafeContext*(TSCtx: OrcThreadSafeContextRef) {.
    importc: "LLVMOrcDisposeThreadSafeContext", dynlib: LLVMLib.}
## *
##  Create a ThreadSafeModule wrapper around the given  module. This takes
##  ownership of the M argument which should not be disposed of or referenced
##  after this function returns.
##
##  Ownership of the ThreadSafeModule is unique: If it is transferred to the JIT
##  (e.g. by OrcLLJITAddIRModule) then the client is no longer
##  responsible for it. If it is not transferred to the JIT then the client
##  should call OrcDisposeThreadSafeModule to dispose of it.
##

proc orcCreateNewThreadSafeModule*(M: ModuleRef;
                                   TSCtx: OrcThreadSafeContextRef): OrcThreadSafeModuleRef {.
    importc: "LLVMOrcCreateNewThreadSafeModule", dynlib: LLVMLib.}
## *
##  Dispose of a ThreadSafeModule. This should only be called if ownership has
##  not been passed to LLJIT (e.g. because some error prevented the client from
##  adding this to the JIT).
##

proc orcDisposeThreadSafeModule*(TSM: OrcThreadSafeModuleRef) {.
    importc: "LLVMOrcDisposeThreadSafeModule", dynlib: LLVMLib.}
## *
##  Apply the given function to the module contained in this ThreadSafeModule.
##

proc orcThreadSafeModuleWithModuleDo*(TSM: OrcThreadSafeModuleRef;
    F: OrcGenericIRModuleOperationFunction; Ctx: pointer): ErrorRef {.
    importc: "LLVMOrcThreadSafeModuleWithModuleDo", dynlib: LLVMLib.}
## *
##  Create a JITTargetMachineBuilder by detecting the host.
##
##  On success the client owns the resulting JITTargetMachineBuilder. It must be
##  passed to a consuming operation (e.g.
##  OrcLLJITBuilderSetJITTargetMachineBuilder) or disposed of by calling
##  OrcDisposeJITTargetMachineBuilder.
##

proc orcJITTargetMachineBuilderDetectHost*(
    Result: ptr OrcJITTargetMachineBuilderRef): ErrorRef {.
    importc: "LLVMOrcJITTargetMachineBuilderDetectHost", dynlib: LLVMLib.}
## *
##  Create a JITTargetMachineBuilder from the given TargetMachine template.
##
##  This operation takes ownership of the given TargetMachine and destroys it
##  before returing. The resulting JITTargetMachineBuilder is owned by the client
##  and must be passed to a consuming operation (e.g.
##  OrcLLJITBuilderSetJITTargetMachineBuilder) or disposed of by calling
##  OrcDisposeJITTargetMachineBuilder.
##

proc orcJITTargetMachineBuilderCreateFromTargetMachine*(
    TM: TargetMachineRef): OrcJITTargetMachineBuilderRef {.
    importc: "LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine",
    dynlib: LLVMLib.}
## *
##  Dispose of a JITTargetMachineBuilder.
##

proc orcDisposeJITTargetMachineBuilder*(
    JTMB: OrcJITTargetMachineBuilderRef) {.
    importc: "LLVMOrcDisposeJITTargetMachineBuilder", dynlib: LLVMLib.}
## *
##  Returns the target triple for the given JITTargetMachineBuilder as a string.
##
##  The caller owns the resulting string as must dispose of it by calling
##  DisposeMessage
##

proc orcJITTargetMachineBuilderGetTargetTriple*(
    JTMB: OrcJITTargetMachineBuilderRef): cstring {.
    importc: "LLVMOrcJITTargetMachineBuilderGetTargetTriple", dynlib: LLVMLib.}
## *
##  Sets the target triple for the given JITTargetMachineBuilder to the given
##  string.
##

proc orcJITTargetMachineBuilderSetTargetTriple*(
    JTMB: OrcJITTargetMachineBuilderRef; TargetTriple: cstring) {.
    importc: "LLVMOrcJITTargetMachineBuilderSetTargetTriple", dynlib: LLVMLib.}
## *
##  Add an object to an ObjectLayer to the given JITDylib.
##
##  Adds a buffer representing an object file to the given JITDylib using the
##  given ObjectLayer instance. This operation transfers ownership of the buffer
##  to the ObjectLayer instance. The buffer should not be disposed of or
##  referenced once this function returns.
##
##  Resources associated with the given object will be tracked by the given
##  JITDylib's default ResourceTracker.
##

proc orcObjectLayerAddObjectFile*(ObjLayer: OrcObjectLayerRef;
                                     JD: OrcJITDylibRef;
                                     ObjBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcObjectLayerAddObjectFile", dynlib: LLVMLib.}
## *
##  Add an object to an ObjectLayer using the given ResourceTracker.
##
##  Adds a buffer representing an object file to the given ResourceTracker's
##  JITDylib using the given ObjectLayer instance. This operation transfers
##  ownership of the buffer to the ObjectLayer instance. The buffer should not
##  be disposed of or referenced once this function returns.
##
##  Resources associated with the given object will be tracked by
##  ResourceTracker RT.
##

proc orcObjectLayerAddObjectFileWithRT*(ObjLayer: OrcObjectLayerRef;
    RT: OrcResourceTrackerRef; ObjBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcObjectLayerAddObjectFileWithRT", dynlib: LLVMLib.}
## *
##  Emit an object buffer to an ObjectLayer.
##
##  Ownership of the responsibility object and object buffer pass to this
##  function. The client is not responsible for cleanup.
##

proc orcObjectLayerEmit*(ObjLayer: OrcObjectLayerRef;
                            R: OrcMaterializationResponsibilityRef;
                            ObjBuffer: MemoryBufferRef) {.
    importc: "LLVMOrcObjectLayerEmit", dynlib: LLVMLib.}
## *
##  Dispose of an ObjectLayer.
##

proc orcDisposeObjectLayer*(ObjLayer: OrcObjectLayerRef) {.
    importc: "LLVMOrcDisposeObjectLayer", dynlib: LLVMLib.}
proc orcIRTransformLayerEmit*(IRTransformLayer: OrcIRTransformLayerRef;
                                 MR: OrcMaterializationResponsibilityRef;
                                 TSM: OrcThreadSafeModuleRef) {.
    importc: "LLVMOrcIRTransformLayerEmit", dynlib: LLVMLib.}
## *
##  Set the transform function of the provided transform layer, passing through a
##  pointer to user provided context.
##

proc orcIRTransformLayerSetTransform*(
    IRTransformLayer: OrcIRTransformLayerRef;
    TransformFunction: OrcIRTransformLayerTransformFunction; Ctx: pointer) {.
    importc: "LLVMOrcIRTransformLayerSetTransform", dynlib: LLVMLib.}
## *
##  Set the transform function on an OrcObjectTransformLayer.
##

proc orcObjectTransformLayerSetTransform*(
    ObjTransformLayer: OrcObjectTransformLayerRef;
    TransformFunction: OrcObjectTransformLayerTransformFunction; Ctx: pointer) {.
    importc: "LLVMOrcObjectTransformLayerSetTransform", dynlib: LLVMLib.}
## *
##  Create a LocalIndirectStubsManager from the given target triple.
##
##  The resulting IndirectStubsManager is owned by the client
##  and must be disposed of by calling OrcDisposeDisposeIndirectStubsManager.
##

proc orcCreateLocalIndirectStubsManager*(TargetTriple: cstring): OrcIndirectStubsManagerRef {.
    importc: "LLVMOrcCreateLocalIndirectStubsManager", dynlib: LLVMLib.}
## *
##  Dispose of an IndirectStubsManager.
##

proc orcDisposeIndirectStubsManager*(ISM: OrcIndirectStubsManagerRef) {.
    importc: "LLVMOrcDisposeIndirectStubsManager", dynlib: LLVMLib.}
proc orcCreateLocalLazyCallThroughManager*(TargetTriple: cstring;
    ES: OrcExecutionSessionRef; ErrorHandlerAddr: OrcJITTargetAddress;
    LCTM: ptr OrcLazyCallThroughManagerRef): ErrorRef {.
    importc: "LLVMOrcCreateLocalLazyCallThroughManager", dynlib: LLVMLib.}
## *
##  Dispose of an LazyCallThroughManager.
##

proc orcDisposeLazyCallThroughManager*(LCTM: OrcLazyCallThroughManagerRef) {.
    importc: "LLVMOrcDisposeLazyCallThroughManager", dynlib: LLVMLib.}
## *
##  Create a DumpObjects instance.
##
##  DumpDir specifies the path to write dumped objects to. DumpDir may be empty
##  in which case files will be dumped to the working directory.
##
##  IdentifierOverride specifies a file name stem to use when dumping objects.
##  If empty then each MemoryBuffer's identifier will be used (with a .o suffix
##  added if not already present). If an identifier override is supplied it will
##  be used instead, along with an incrementing counter (since all buffers will
##  use the same identifier, the resulting files will be named <ident>.o,
##  <ident>.2.o, <ident>.3.o, and so on). IdentifierOverride should not contain
##  an extension, as a .o suffix will be added by DumpObjects.
##

proc orcCreateDumpObjects*(DumpDir: cstring; IdentifierOverride: cstring): OrcDumpObjectsRef {.
    importc: "LLVMOrcCreateDumpObjects", dynlib: LLVMLib.}
## *
##  Dispose of a DumpObjects instance.
##

proc orcDisposeDumpObjects*(DumpObjects: OrcDumpObjectsRef) {.
    importc: "LLVMOrcDisposeDumpObjects", dynlib: LLVMLib.}
## *
##  Dump the contents of the given MemoryBuffer.
##

proc orcDumpObjects_CallOperator*(DumpObjects: OrcDumpObjectsRef;
                                     ObjBuffer: ptr MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcDumpObjects_CallOperator", dynlib: LLVMLib.}
##  _C_EXTERN_C_END
