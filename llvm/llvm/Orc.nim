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

## !!!Ignored construct:  # LLVM_C_ORC_H [NewLine] # LLVM_C_ORC_H [NewLine] # llvm-c/Error.h [NewLine] # llvm-c/TargetMachine.h [NewLine] # llvm-c/Types.h [NewLine] LLVM_C_EXTERN_C_BEGIN *
##  @defgroup LLVMCExecutionEngineORC On-Request-Compilation
##  @ingroup LLVMCExecutionEngine
##
##  @{
##  *
##  Represents an address in the executor process.
##  typedef uint64_t LLVMOrcJITTargetAddress ;
## Error: expected ';'!!!

## *
##  Represents an address in the executor process.
##

type
  OrcExecutorAddress* = uint64

## *
##  Represents generic linkage flags for a symbol definition.
##

type
  JITSymbolGenericFlags* {.size: sizeof(cint).} = enum
    JITSymbolGenericFlagsNone = 0, JITSymbolGenericFlagsExported = 1'u shl 0,
    JITSymbolGenericFlagsWeak = 1'u shl 1, JITSymbolGenericFlagsCallable = 1'u shl 2,
    JITSymbolGenericFlagsMaterializationSideEffectsOnly = 1'u shl 3


## *
##  Represents target specific flags for a symbol definition.
##

type
  JITSymbolTargetFlags* = uint8T

## *
##  Represents the linkage flags for a symbol definition.
##

type
  JITSymbolFlags* {.bycopy.} = object
    genericFlags*: uint8T
    targetFlags*: uint8T


## *
##  Represents an evaluated symbol address and flags.
##

type
  JITEvaluatedSymbol* {.bycopy.} = object
    address*: OrcExecutorAddress
    flags*: JITSymbolFlags


## *
##  A reference to an orc::ExecutionSession instance.
##

type
  OrcExecutionSessionRef* = ptr OrcOpaqueExecutionSession

## *
##  Error reporter function.
##

type
  OrcErrorReporterFunction* = proc (ctx: pointer; err: ErrorRef)

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
    name*: OrcSymbolStringPoolEntryRef
    flags*: JITSymbolFlags


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
  OrcCSymbolMapPair* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    sym*: JITEvaluatedSymbol


## *
##  Represents a list of (SymbolStringPtr, JITEvaluatedSymbol) pairs that can be
##  used to construct a SymbolMap.
##

type
  OrcCSymbolMapPairs* = ptr OrcCSymbolMapPair

## *
##  Represents a SymbolAliasMapEntry
##

type
  OrcCSymbolAliasMapEntry* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    flags*: JITSymbolFlags


## *
##  Represents a pair of a symbol name and SymbolAliasMapEntry.
##

type
  OrcCSymbolAliasMapPair* {.bycopy.} = object
    name*: OrcSymbolStringPoolEntryRef
    entry*: OrcCSymbolAliasMapEntry


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
##  Represents a list of LLVMOrcSymbolStringPoolEntryRef and the associated
##  length.
##

type
  OrcCSymbolsList* {.bycopy.} = object
    symbols*: ptr OrcSymbolStringPoolEntryRef
    length*: csize_t


## *
##  Represents a pair of a JITDylib and LLVMOrcCSymbolsList.
##

type
  OrcCDependenceMapPair* {.bycopy.} = object
    jd*: OrcJITDylibRef
    names*: OrcCSymbolsList


## *
##  Represents a list of (JITDylibRef, (LLVMOrcSymbolStringPoolEntryRef*,
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
##  An element type for a JITDylib search order.
##

type
  OrcCJITDylibSearchOrderElement* {.bycopy.} = object
    jd*: OrcJITDylibRef
    jDLookupFlags*: OrcJITDylibLookupFlags


## *
##  A JITDylib search order.
##
##  The list is terminated with an element containing a null pointer for the JD
##  field.
##

type
  OrcCJITDylibSearchOrder* = ptr OrcCJITDylibSearchOrderElement

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
    name*: OrcSymbolStringPoolEntryRef
    lookupFlags*: OrcSymbolLookupFlags


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
##  adhere to the LLVMOrcMaterializationResponsibilityRef contract (see comment
##  for that type).
##
##  If this callback is called then the LLVMOrcMaterializationUnitDestroy
##  callback will NOT be called.
##

type
  OrcMaterializationUnitMaterializeFunction* = proc (ctx: pointer;
      mr: OrcMaterializationResponsibilityRef)

## *
##  A MaterializationUnit discard callback.
##
##  Ownership of JD and Symbol remain with the caller: These arguments should
##  not be disposed of or released.
##

type
  OrcMaterializationUnitDiscardFunction* = proc (ctx: pointer; jd: OrcJITDylibRef;
      symbol: OrcSymbolStringPoolEntryRef)

## *
##  A MaterializationUnit destruction callback.
##
##  If a custom MaterializationUnit is destroyed before its Materialize
##  function is called then this function will be called to provide an
##  opportunity for the underlying program representation to be destroyed.
##

type
  OrcMaterializationUnitDestroyFunction* = proc (ctx: pointer)

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
##  LLVMOrcLookupStateContinueLookup to restart the lookup. This is required
##  in order to release memory allocated for the lookup state, even if errors
##  have occurred while the lookup was suspended (if these errors have made the
##  lookup impossible to complete then it will issue its own error before
##  destruction).
##

type
  OrcLookupStateRef* = ptr OrcOpaqueLookupState

## *
##  A custom generator function. This can be used to create a custom generator
##  object using LLVMOrcCreateCustomCAPIDefinitionGenerator. The resulting
##  object can be attached to a JITDylib, via LLVMOrcJITDylibAddGenerator, to
##  receive callbacks when lookups fail to match existing definitions.
##
##  GeneratorObj will contain the address of the custom generator object.
##
##  Ctx will contain the context object passed to
##  LLVMOrcCreateCustomCAPIDefinitionGenerator.
##
##  LookupState will contain a pointer to an LLVMOrcLookupStateRef object. This
##  can optionally be modified to make the definition generation process
##  asynchronous: If the LookupStateRef value is copied, and the original
##  LLVMOrcLookupStateRef set to null, the lookup will be suspended. Once the
##  asynchronous definition process has been completed clients must call
##  LLVMOrcLookupStateContinueLookup to continue the lookup (this should be
##  done unconditionally, even if errors have occurred in the mean time, to
##  free the lookup state memory and notify the query object of the failures).
##  If LookupState is captured this function must return LLVMErrorSuccess.
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
      generatorObj: OrcDefinitionGeneratorRef; ctx: pointer;
      lookupState: ptr OrcLookupStateRef; kind: OrcLookupKind; jd: OrcJITDylibRef;
      jDLookupFlags: OrcJITDylibLookupFlags; lookupSet: OrcCLookupSet;
      lookupSetSize: csize_t): ErrorRef

## *
##  Disposer for a custom generator.
##
##  Will be called by ORC when the JITDylib that the generator is attached to
##  is destroyed.
##

type
  OrcDisposeCAPIDefinitionGeneratorFunction* = proc (ctx: pointer)

## *
##  Predicate function for SymbolStringPoolEntries.
##

type
  OrcSymbolPredicate* = proc (ctx: pointer; sym: OrcSymbolStringPoolEntryRef): cint

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
##  LLVMOrcThreadSafeModuleWithModuleDo.
##

type
  OrcGenericIRModuleOperationFunction* = proc (ctx: pointer; m: ModuleRef): ErrorRef

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
##  of the Module pointed to by ModInOut: If the LLVMModuleRef value is
##  overwritten then the function is responsible for disposing of the incoming
##  module. If the module is simply accessed/mutated in-place then ownership
##  returns to the caller and the function does not need to do any lifetime
##  management.
##
##  Clients can call LLVMOrcLLJITGetIRTransformLayer to obtain the transform
##  layer of a LLJIT instance, and use LLVMOrcIRTransformLayerSetTransform
##  to set the function. This can be used to override the default transform
##  layer.
##

type
  OrcIRTransformLayerTransformFunction* = proc (ctx: pointer;
      modInOut: ptr OrcThreadSafeModuleRef; mr: OrcMaterializationResponsibilityRef): ErrorRef

## *
##  A reference to an orc::ObjectTransformLayer instance.
##

type
  OrcObjectTransformLayerRef* = ptr OrcOpaqueObjectTransformLayer

## *
##  A function for applying transformations to an object file buffer.
##
##  Implementations of this type are responsible for managing the lifetime
##  of the memory buffer pointed to by ObjInOut: If the LLVMMemoryBufferRef
##  value is overwritten then the function is responsible for disposing of the
##  incoming buffer. If the buffer is simply accessed/mutated in-place then
##  ownership returns to the caller and the function does not need to do any
##  lifetime management.
##
##  The transform is allowed to return an error, in which case the ObjInOut
##  buffer should be disposed of and set to null.
##

type
  OrcObjectTransformLayerTransformFunction* = proc (ctx: pointer;
      objInOut: ptr MemoryBufferRef): ErrorRef

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

proc orcExecutionSessionSetErrorReporter*(es: OrcExecutionSessionRef;
    reportError: OrcErrorReporterFunction; ctx: pointer) {.
    importc: "LLVMOrcExecutionSessionSetErrorReporter", dynlib: LLVMLib.}
## *
##  Return a reference to the SymbolStringPool for an ExecutionSession.
##
##  Ownership of the pool remains with the ExecutionSession: The caller is
##  not required to free the pool.
##

proc orcExecutionSessionGetSymbolStringPool*(es: OrcExecutionSessionRef): OrcSymbolStringPoolRef {.
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

proc orcSymbolStringPoolClearDeadEntries*(ssp: OrcSymbolStringPoolRef) {.
    importc: "LLVMOrcSymbolStringPoolClearDeadEntries", dynlib: LLVMLib.}
## *
##  Intern a string in the ExecutionSession's SymbolStringPool and return a
##  reference to it. This increments the ref-count of the pool entry, and the
##  returned value should be released once the client is done with it by
##  calling LLVMOrReleaseSymbolStringPoolEntry.
##
##  Since strings are uniqued within the SymbolStringPool
##  LLVMOrcSymbolStringPoolEntryRefs can be compared by value to test string
##  equality.
##
##  Note that this function does not perform linker-mangling on the string.
##

proc orcExecutionSessionIntern*(es: OrcExecutionSessionRef; name: cstring): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcExecutionSessionIntern", dynlib: LLVMLib.}
## *
##  Callback type for ExecutionSession lookups.
##
##  If Err is LLVMErrorSuccess then Result will contain a pointer to a
##  list of ( SymbolStringPtr, JITEvaluatedSymbol ) pairs of length NumPairs.
##
##  If Err is a failure value then Result and Ctx are undefined and should
##  not be accessed. The Callback is responsible for handling the error
##  value (e.g. by calling LLVMGetErrorMessage + LLVMDisposeErrorMessage).
##
##  The caller retains ownership of the Result array and will release all
##  contained symbol names. Clients are responsible for retaining any symbol
##  names that they wish to hold after the function returns.
##

type
  OrcExecutionSessionLookupHandleResultFunction* = proc (err: ErrorRef;
      result: OrcCSymbolMapPairs; numPairs: csize_t; ctx: pointer)

## *
##  Look up symbols in an execution session.
##
##  This is a wrapper around the general ExecutionSession::lookup function.
##
##  The SearchOrder argument contains a list of (JITDylibs, JITDylibSearchFlags)
##  pairs that describe the search order. The JITDylibs will be searched in the
##  given order to try to find the symbols in the Symbols argument.
##
##  The Symbols argument should contain a null-terminated array of
##  (SymbolStringPtr, SymbolLookupFlags) pairs describing the symbols to be
##  searched for. This function takes ownership of the elements of the Symbols
##  array. The Name fields of the Symbols elements are taken to have been
##  retained by the client for this function. The client should *not* release the
##  Name fields, but are still responsible for destroying the array itself.
##
##  The HandleResult function will be called once all searched for symbols have
##  been found, or an error occurs. The HandleResult function will be passed an
##  LLVMErrorRef indicating success or failure, and (on success) a
##  null-terminated LLVMOrcCSymbolMapPairs array containing the function result,
##  and the Ctx value passed to the lookup function.
##
##  The client is fully responsible for managing the lifetime of the Ctx object.
##  A common idiom is to allocate the context prior to the lookup and deallocate
##  it in the handler.
##
##  THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
##

proc orcExecutionSessionLookup*(es: OrcExecutionSessionRef; k: OrcLookupKind;
                               searchOrder: OrcCJITDylibSearchOrder;
                               searchOrderSize: csize_t; symbols: OrcCLookupSet;
                               symbolsSize: csize_t; handleResult: OrcExecutionSessionLookupHandleResultFunction;
                               ctx: pointer) {.
    importc: "LLVMOrcExecutionSessionLookup", dynlib: LLVMLib.}
## *
##  Increments the ref-count for a SymbolStringPool entry.
##

proc orcRetainSymbolStringPoolEntry*(s: OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcRetainSymbolStringPoolEntry", dynlib: LLVMLib.}
## *
##  Reduces the ref-count for of a SymbolStringPool entry.
##

proc orcReleaseSymbolStringPoolEntry*(s: OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcReleaseSymbolStringPoolEntry", dynlib: LLVMLib.}
## *
##  Return the c-string for the given symbol. This string will remain valid until
##  the entry is freed (once all LLVMOrcSymbolStringPoolEntryRefs have been
##  released).
##

proc orcSymbolStringPoolEntryStr*(s: OrcSymbolStringPoolEntryRef): cstring {.
    importc: "LLVMOrcSymbolStringPoolEntryStr", dynlib: LLVMLib.}
## *
##  Reduces the ref-count of a ResourceTracker.
##

proc orcReleaseResourceTracker*(rt: OrcResourceTrackerRef) {.
    importc: "LLVMOrcReleaseResourceTracker", dynlib: LLVMLib.}
## *
##  Transfers tracking of all resources associated with resource tracker SrcRT
##  to resource tracker DstRT.
##

proc orcResourceTrackerTransferTo*(srcRT: OrcResourceTrackerRef;
                                  dstRT: OrcResourceTrackerRef) {.
    importc: "LLVMOrcResourceTrackerTransferTo", dynlib: LLVMLib.}
## *
##  Remove all resources associated with the given tracker. See
##  ResourceTracker::remove().
##

proc orcResourceTrackerRemove*(rt: OrcResourceTrackerRef): ErrorRef {.
    importc: "LLVMOrcResourceTrackerRemove", dynlib: LLVMLib.}
## *
##  Dispose of a JITDylib::DefinitionGenerator. This should only be called if
##  ownership has not been passed to a JITDylib (e.g. because some error
##  prevented the client from calling LLVMOrcJITDylibAddGenerator).
##

proc orcDisposeDefinitionGenerator*(dg: OrcDefinitionGeneratorRef) {.
    importc: "LLVMOrcDisposeDefinitionGenerator", dynlib: LLVMLib.}
## *
##  Dispose of a MaterializationUnit.
##

proc orcDisposeMaterializationUnit*(mu: OrcMaterializationUnitRef) {.
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
##  still responsible for destroying the array itself.
##
##  The InitSym argument indicates whether or not this MaterializationUnit
##  contains static initializers. If three are no static initializers (the common
##  case) then this argument should be null. If there are static initializers
##  then InitSym should be set to a unique name that also appears in the Syms
##  list with the LLVMJITSymbolGenericFlagsMaterializationSideEffectsOnly flag
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
##  either passing it to a JITDylib (via LLVMOrcJITDylibDefine) or disposing
##  of it by calling LLVMOrcDisposeMaterializationUnit.
##

proc orcCreateCustomMaterializationUnit*(name: cstring; ctx: pointer;
                                        syms: OrcCSymbolFlagsMapPairs;
                                        numSyms: csize_t;
                                        initSym: OrcSymbolStringPoolEntryRef;
    materialize: OrcMaterializationUnitMaterializeFunction; `discard`: OrcMaterializationUnitDiscardFunction;
    destroy: OrcMaterializationUnitDestroyFunction): OrcMaterializationUnitRef {.
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
##    LLVMOrcCSymbolMapPairs Sym;
##    -- Build Syms array --
##    LLVMOrcMaterializationUnitRef MU =
##        LLVMOrcAbsoluteSymbols(Syms, NumPairs);
##
##  ... without requiring cleanup of the elements of the Sym array afterwards.
##
##  The client is still responsible for deleting the Sym array itself.
##
##  If a client wishes to reuse elements of the Sym array after this call they
##  must explicitly retain each of the elements for themselves.
##

proc orcAbsoluteSymbols*(syms: OrcCSymbolMapPairs; numPairs: csize_t): OrcMaterializationUnitRef {.
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
##    LLVMOrcCSymbolAliasMapPairs CallableAliases;
##    -- Build CallableAliases array --
##    LLVMOrcMaterializationUnitRef MU =
##       LLVMOrcLazyReexports(LCTM, ISM, JD, CallableAliases, NumPairs);
##
##  ... without requiring cleanup of the elements of the CallableAliases array afterwards.
##
##  The client is still responsible for deleting the CallableAliases array itself.
##
##  If a client wishes to reuse elements of the CallableAliases array after this call they
##  must explicitly retain each of the elements for themselves.
##

proc orcLazyReexports*(lctm: OrcLazyCallThroughManagerRef;
                      ism: OrcIndirectStubsManagerRef; sourceRef: OrcJITDylibRef;
                      callableAliases: OrcCSymbolAliasMapPairs; numPairs: csize_t): OrcMaterializationUnitRef {.
    importc: "LLVMOrcLazyReexports", dynlib: LLVMLib.}
##  TODO: ImplSymbolMad SrcJDLoc
## *
##  Disposes of the passed MaterializationResponsibility object.
##
##  This should only be done after the symbols covered by the object have either
##  been resolved and emitted (via
##  LLVMOrcMaterializationResponsibilityNotifyResolved and
##  LLVMOrcMaterializationResponsibilityNotifyEmitted) or failed (via
##  LLVMOrcMaterializationResponsibilityFailMaterialization).
##

proc orcDisposeMaterializationResponsibility*(
    mr: OrcMaterializationResponsibilityRef) {.
    importc: "LLVMOrcDisposeMaterializationResponsibility", dynlib: LLVMLib.}
## *
##  Returns the target JITDylib that these symbols are being materialized into.
##

proc orcMaterializationResponsibilityGetTargetDylib*(
    mr: OrcMaterializationResponsibilityRef): OrcJITDylibRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetTargetDylib", dynlib: LLVMLib.}
## *
##  Returns the ExecutionSession for this MaterializationResponsibility.
##

proc orcMaterializationResponsibilityGetExecutionSession*(
    mr: OrcMaterializationResponsibilityRef): OrcExecutionSessionRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetExecutionSession",
    dynlib: LLVMLib.}
## *
##  Returns the symbol flags map for this responsibility instance.
##
##  The length of the array is returned in NumPairs and the caller is responsible
##  for the returned memory and needs to call LLVMOrcDisposeCSymbolFlagsMap.
##
##  To use the returned symbols beyond the livetime of the
##  MaterializationResponsibility requires the caller to retain the symbols
##  explicitly.
##

proc orcMaterializationResponsibilityGetSymbols*(
    mr: OrcMaterializationResponsibilityRef; numPairs: ptr csize_t): OrcCSymbolFlagsMapPairs {.
    importc: "LLVMOrcMaterializationResponsibilityGetSymbols", dynlib: LLVMLib.}
## *
##  Disposes of the passed LLVMOrcCSymbolFlagsMap.
##
##  Does not release the entries themselves.
##

proc orcDisposeCSymbolFlagsMap*(pairs: OrcCSymbolFlagsMapPairs) {.
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
    mr: OrcMaterializationResponsibilityRef): OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetInitializerSymbol",
    dynlib: LLVMLib.}
## *
##  Returns the names of any symbols covered by this
##  MaterializationResponsibility object that have queries pending. This
##  information can be used to return responsibility for unrequested symbols
##  back to the JITDylib via the delegate method.
##

proc orcMaterializationResponsibilityGetRequestedSymbols*(
    mr: OrcMaterializationResponsibilityRef; numSymbols: ptr csize_t): ptr OrcSymbolStringPoolEntryRef {.
    importc: "LLVMOrcMaterializationResponsibilityGetRequestedSymbols",
    dynlib: LLVMLib.}
## *
##  Disposes of the passed LLVMOrcSymbolStringPoolEntryRef* .
##
##  Does not release the symbols themselves.
##

proc orcDisposeSymbols*(symbols: ptr OrcSymbolStringPoolEntryRef) {.
    importc: "LLVMOrcDisposeSymbols", dynlib: LLVMLib.}
## *
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
##  LLVMOrcMaterializationResponsibilityFailMaterialization. If no dependencies
##  have been registered for the symbols covered by this
##  MaterializationResponsibiility then this method is guaranteed to return
##  LLVMErrorSuccess.
##

proc orcMaterializationResponsibilityNotifyResolved*(
    mr: OrcMaterializationResponsibilityRef; symbols: OrcCSymbolMapPairs;
    numPairs: csize_t): ErrorRef {.importc: "LLVMOrcMaterializationResponsibilityNotifyResolved",
                                dynlib: LLVMLib.}
## *
##  Notifies the target JITDylib (and any pending queries on that JITDylib)
##  that all symbols covered by this MaterializationResponsibility instance
##  have been emitted.
##
##  This method will return an error if any symbols being resolved have been
##  moved to the error state due to the failure of a dependency. If this
##  method returns an error then clients should log it and call
##  LLVMOrcMaterializationResponsibilityFailMaterialization.
##  If no dependencies have been registered for the symbols covered by this
##  MaterializationResponsibiility then this method is guaranteed to return
##  LLVMErrorSuccess.
##

proc orcMaterializationResponsibilityNotifyEmitted*(
    mr: OrcMaterializationResponsibilityRef): ErrorRef {.
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
    mr: OrcMaterializationResponsibilityRef; pairs: OrcCSymbolFlagsMapPairs;
    numPairs: csize_t): ErrorRef {.importc: "LLVMOrcMaterializationResponsibilityDefineMaterializing",
                                dynlib: LLVMLib.}
## *
##  Notify all not-yet-emitted covered by this MaterializationResponsibility
##  instance that an error has occurred.
##  This will remove all symbols covered by this MaterializationResponsibilty
##  from the target JITDylib, and send an error to any queries waiting on
##  these symbols.
##

proc orcMaterializationResponsibilityFailMaterialization*(
    mr: OrcMaterializationResponsibilityRef) {.
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
    mr: OrcMaterializationResponsibilityRef; mu: OrcMaterializationUnitRef): ErrorRef {.
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
    mr: OrcMaterializationResponsibilityRef;
    symbols: ptr OrcSymbolStringPoolEntryRef; numSymbols: csize_t;
    result: ptr OrcMaterializationResponsibilityRef): ErrorRef {.
    importc: "LLVMOrcMaterializationResponsibilityDelegate", dynlib: LLVMLib.}
## *
##  Adds dependencies to a symbol that the MaterializationResponsibility is
##  responsible for.
##
##  This function takes ownership of Dependencies struct. The Names
##  array have been retained for this function. This allows the following
##  pattern...
##
##    LLVMOrcSymbolStringPoolEntryRef Names[] = {...};
##    LLVMOrcCDependenceMapPair Dependence = {JD, {Names, sizeof(Names)}}
##    LLVMOrcMaterializationResponsibilityAddDependencies(JD, Name, &Dependence,
##  1);
##
##  ... without requiring cleanup of the elements of the Names array afterwards.
##
##  The client is still responsible for deleting the Dependencies.Names array
##  itself.
##

proc orcMaterializationResponsibilityAddDependencies*(
    mr: OrcMaterializationResponsibilityRef; name: OrcSymbolStringPoolEntryRef;
    dependencies: OrcCDependenceMapPairs; numPairs: csize_t) {.
    importc: "LLVMOrcMaterializationResponsibilityAddDependencies",
    dynlib: LLVMLib.}
## *
##  Adds dependencies to all symbols that the MaterializationResponsibility is
##  responsible for. See LLVMOrcMaterializationResponsibilityAddDependencies for
##  notes about memory responsibility.
##

proc orcMaterializationResponsibilityAddDependenciesForAll*(
    mr: OrcMaterializationResponsibilityRef; dependencies: OrcCDependenceMapPairs;
    numPairs: csize_t) {.importc: "LLVMOrcMaterializationResponsibilityAddDependenciesForAll",
                       dynlib: LLVMLib.}
## *
##  Create a "bare" JITDylib.
##
##  The client is responsible for ensuring that the JITDylib's name is unique,
##  e.g. by calling LLVMOrcExecutionSessionGetJTIDylibByName first.
##
##  This call does not install any library code or symbols into the newly
##  created JITDylib. The client is responsible for all configuration.
##

proc orcExecutionSessionCreateBareJITDylib*(es: OrcExecutionSessionRef;
    name: cstring): OrcJITDylibRef {.importc: "LLVMOrcExecutionSessionCreateBareJITDylib",
                                  dynlib: LLVMLib.}
## *
##  Create a JITDylib.
##
##  The client is responsible for ensuring that the JITDylib's name is unique,
##  e.g. by calling LLVMOrcExecutionSessionGetJTIDylibByName first.
##
##  If a Platform is attached to the ExecutionSession then
##  Platform::setupJITDylib will be called to install standard platform symbols
##  (e.g. standard library interposes). If no Platform is installed then this
##  call is equivalent to LLVMExecutionSessionRefCreateBareJITDylib and will
##  always return success.
##

proc orcExecutionSessionCreateJITDylib*(es: OrcExecutionSessionRef;
                                       result: ptr OrcJITDylibRef; name: cstring): ErrorRef {.
    importc: "LLVMOrcExecutionSessionCreateJITDylib", dynlib: LLVMLib.}
## *
##  Returns the JITDylib with the given name, or NULL if no such JITDylib
##  exists.
##

proc orcExecutionSessionGetJITDylibByName*(es: OrcExecutionSessionRef;
    name: cstring): OrcJITDylibRef {.importc: "LLVMOrcExecutionSessionGetJITDylibByName",
                                  dynlib: LLVMLib.}
## *
##  Return a reference to a newly created resource tracker associated with JD.
##  The tracker is returned with an initial ref-count of 1, and must be released
##  with LLVMOrcReleaseResourceTracker when no longer needed.
##

proc orcJITDylibCreateResourceTracker*(jd: OrcJITDylibRef): OrcResourceTrackerRef {.
    importc: "LLVMOrcJITDylibCreateResourceTracker", dynlib: LLVMLib.}
## *
##  Return a reference to the default resource tracker for the given JITDylib.
##  This operation will increase the retain count of the tracker: Clients should
##  call LLVMOrcReleaseResourceTracker when the result is no longer needed.
##

proc orcJITDylibGetDefaultResourceTracker*(jd: OrcJITDylibRef): OrcResourceTrackerRef {.
    importc: "LLVMOrcJITDylibGetDefaultResourceTracker", dynlib: LLVMLib.}
## *
##  Add the given MaterializationUnit to the given JITDylib.
##
##  If this operation succeeds then JITDylib JD will take ownership of MU.
##  If the operation fails then ownership remains with the caller who should
##  call LLVMOrcDisposeMaterializationUnit to destroy it.
##

proc orcJITDylibDefine*(jd: OrcJITDylibRef; mu: OrcMaterializationUnitRef): ErrorRef {.
    importc: "LLVMOrcJITDylibDefine", dynlib: LLVMLib.}
## *
##  Calls remove on all trackers associated with this JITDylib, see
##  JITDylib::clear().
##

proc orcJITDylibClear*(jd: OrcJITDylibRef): ErrorRef {.
    importc: "LLVMOrcJITDylibClear", dynlib: LLVMLib.}
## *
##  Add a DefinitionGenerator to the given JITDylib.
##
##  The JITDylib will take ownership of the given generator: The client is no
##  longer responsible for managing its memory.
##

proc orcJITDylibAddGenerator*(jd: OrcJITDylibRef; dg: OrcDefinitionGeneratorRef) {.
    importc: "LLVMOrcJITDylibAddGenerator", dynlib: LLVMLib.}
## *
##  Create a custom generator.
##
##  The F argument will be used to implement the DefinitionGenerator's
##  tryToGenerate method (see
##  LLVMOrcCAPIDefinitionGeneratorTryToGenerateFunction).
##
##  Ctx is a context object that will be passed to F. This argument is
##  permitted to be null.
##
##  Dispose is the disposal function for Ctx. This argument is permitted to be
##  null (in which case the client is responsible for the lifetime of Ctx).
##

proc orcCreateCustomCAPIDefinitionGenerator*(
    f: OrcCAPIDefinitionGeneratorTryToGenerateFunction; ctx: pointer;
    dispose: OrcDisposeCAPIDefinitionGeneratorFunction): OrcDefinitionGeneratorRef {.
    importc: "LLVMOrcCreateCustomCAPIDefinitionGenerator", dynlib: LLVMLib.}
## *
##  Continue a lookup that was suspended in a generator (see
##  LLVMOrcCAPIDefinitionGeneratorTryToGenerateFunction).
##

proc orcLookupStateContinueLookup*(s: OrcLookupStateRef; err: ErrorRef) {.
    importc: "LLVMOrcLookupStateContinueLookup", dynlib: LLVMLib.}
## *
##  Get a DynamicLibrarySearchGenerator that will reflect process symbols into
##  the JITDylib. On success the resulting generator is owned by the client.
##  Ownership is typically transferred by adding the instance to a JITDylib
##  using LLVMOrcJITDylibAddGenerator,
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
    result: ptr OrcDefinitionGeneratorRef; globalPrefx: char;
    filter: OrcSymbolPredicate; filterCtx: pointer): ErrorRef {.
    importc: "LLVMOrcCreateDynamicLibrarySearchGeneratorForProcess",
    dynlib: LLVMLib.}
## *
##  Get a LLVMOrcCreateDynamicLibararySearchGeneratorForPath that will reflect
##  library symbols into the JITDylib. On success the resulting generator is
##  owned by the client. Ownership is typically transferred by adding the
##  instance to a JITDylib using LLVMOrcJITDylibAddGenerator,
##
##  The GlobalPrefix argument specifies the character that appears on the front
##  of linker-mangled symbols for the target platform (e.g. '_' on MachO).
##  If non-null, this character will be stripped from the start of all symbol
##  strings before passing the remaining substring to dlsym.
##
##  The optional Filter and Ctx arguments can be used to supply a symbol name
##  filter: Only symbols for which the filter returns true will be visible to
##  JIT'd code. If the Filter argument is null then all library symbols will
##  be visible to JIT'd code. Note that the symbol name passed to the Filter
##  function is the full mangled symbol: The client is responsible for stripping
##  the global prefix if present.
##
##  THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
##
##

proc orcCreateDynamicLibrarySearchGeneratorForPath*(
    result: ptr OrcDefinitionGeneratorRef; fileName: cstring; globalPrefix: char;
    filter: OrcSymbolPredicate; filterCtx: pointer): ErrorRef {.
    importc: "LLVMOrcCreateDynamicLibrarySearchGeneratorForPath", dynlib: LLVMLib.}
## *
##  Get a LLVMOrcCreateStaticLibrarySearchGeneratorForPath that will reflect
##  static library symbols into the JITDylib. On success the resulting
##  generator is owned by the client. Ownership is typically transferred by
##  adding the instance to a JITDylib using LLVMOrcJITDylibAddGenerator,
##
##  Call with the optional TargetTriple argument will succeed if the file at
##  the given path is a static library or a MachO universal binary containing a
##  static library that is compatible with the given triple. Otherwise it will
##  return an error.
##
##  THIS API IS EXPERIMENTAL AND LIKELY TO CHANGE IN THE NEAR FUTURE!
##
##

proc orcCreateStaticLibrarySearchGeneratorForPath*(
    result: ptr OrcDefinitionGeneratorRef; objLayer: OrcObjectLayerRef;
    fileName: cstring; targetTriple: cstring): ErrorRef {.
    importc: "LLVMOrcCreateStaticLibrarySearchGeneratorForPath", dynlib: LLVMLib.}
## *
##  Create a ThreadSafeContext containing a new LLVMContext.
##
##  Ownership of the underlying ThreadSafeContext data is shared: Clients
##  can and should dispose of their ThreadSafeContext as soon as they no longer
##  need to refer to it directly. Other references (e.g. from ThreadSafeModules)
##  will keep the data alive as long as it is needed.
##

proc orcCreateNewThreadSafeContext*(): OrcThreadSafeContextRef {.
    importc: "LLVMOrcCreateNewThreadSafeContext", dynlib: LLVMLib.}
## *
##  Get a reference to the wrapped LLVMContext.
##

proc orcThreadSafeContextGetContext*(tSCtx: OrcThreadSafeContextRef): ContextRef {.
    importc: "LLVMOrcThreadSafeContextGetContext", dynlib: LLVMLib.}
## *
##  Dispose of a ThreadSafeContext.
##

proc orcDisposeThreadSafeContext*(tSCtx: OrcThreadSafeContextRef) {.
    importc: "LLVMOrcDisposeThreadSafeContext", dynlib: LLVMLib.}
## *
##  Create a ThreadSafeModule wrapper around the given LLVM module. This takes
##  ownership of the M argument which should not be disposed of or referenced
##  after this function returns.
##
##  Ownership of the ThreadSafeModule is unique: If it is transferred to the JIT
##  (e.g. by LLVMOrcLLJITAddLLVMIRModule) then the client is no longer
##  responsible for it. If it is not transferred to the JIT then the client
##  should call LLVMOrcDisposeThreadSafeModule to dispose of it.
##

proc orcCreateNewThreadSafeModule*(m: ModuleRef; tSCtx: OrcThreadSafeContextRef): OrcThreadSafeModuleRef {.
    importc: "LLVMOrcCreateNewThreadSafeModule", dynlib: LLVMLib.}
## *
##  Dispose of a ThreadSafeModule. This should only be called if ownership has
##  not been passed to LLJIT (e.g. because some error prevented the client from
##  adding this to the JIT).
##

proc orcDisposeThreadSafeModule*(tsm: OrcThreadSafeModuleRef) {.
    importc: "LLVMOrcDisposeThreadSafeModule", dynlib: LLVMLib.}
## *
##  Apply the given function to the module contained in this ThreadSafeModule.
##

proc orcThreadSafeModuleWithModuleDo*(tsm: OrcThreadSafeModuleRef;
                                     f: OrcGenericIRModuleOperationFunction;
                                     ctx: pointer): ErrorRef {.
    importc: "LLVMOrcThreadSafeModuleWithModuleDo", dynlib: LLVMLib.}
## *
##  Create a JITTargetMachineBuilder by detecting the host.
##
##  On success the client owns the resulting JITTargetMachineBuilder. It must be
##  passed to a consuming operation (e.g.
##  LLVMOrcLLJITBuilderSetJITTargetMachineBuilder) or disposed of by calling
##  LLVMOrcDisposeJITTargetMachineBuilder.
##

proc orcJITTargetMachineBuilderDetectHost*(
    result: ptr OrcJITTargetMachineBuilderRef): ErrorRef {.
    importc: "LLVMOrcJITTargetMachineBuilderDetectHost", dynlib: LLVMLib.}
## *
##  Create a JITTargetMachineBuilder from the given TargetMachine template.
##
##  This operation takes ownership of the given TargetMachine and destroys it
##  before returing. The resulting JITTargetMachineBuilder is owned by the client
##  and must be passed to a consuming operation (e.g.
##  LLVMOrcLLJITBuilderSetJITTargetMachineBuilder) or disposed of by calling
##  LLVMOrcDisposeJITTargetMachineBuilder.
##

proc orcJITTargetMachineBuilderCreateFromTargetMachine*(tm: TargetMachineRef): OrcJITTargetMachineBuilderRef {.
    importc: "LLVMOrcJITTargetMachineBuilderCreateFromTargetMachine",
    dynlib: LLVMLib.}
## *
##  Dispose of a JITTargetMachineBuilder.
##

proc orcDisposeJITTargetMachineBuilder*(jtmb: OrcJITTargetMachineBuilderRef) {.
    importc: "LLVMOrcDisposeJITTargetMachineBuilder", dynlib: LLVMLib.}
## *
##  Returns the target triple for the given JITTargetMachineBuilder as a string.
##
##  The caller owns the resulting string as must dispose of it by calling
##  LLVMDisposeMessage
##

proc orcJITTargetMachineBuilderGetTargetTriple*(
    jtmb: OrcJITTargetMachineBuilderRef): cstring {.
    importc: "LLVMOrcJITTargetMachineBuilderGetTargetTriple", dynlib: LLVMLib.}
## *
##  Sets the target triple for the given JITTargetMachineBuilder to the given
##  string.
##

proc orcJITTargetMachineBuilderSetTargetTriple*(
    jtmb: OrcJITTargetMachineBuilderRef; targetTriple: cstring) {.
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

proc orcObjectLayerAddObjectFile*(objLayer: OrcObjectLayerRef; jd: OrcJITDylibRef;
                                 objBuffer: MemoryBufferRef): ErrorRef {.
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

proc orcObjectLayerAddObjectFileWithRT*(objLayer: OrcObjectLayerRef;
                                       rt: OrcResourceTrackerRef;
                                       objBuffer: MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcObjectLayerAddObjectFileWithRT", dynlib: LLVMLib.}
## *
##  Emit an object buffer to an ObjectLayer.
##
##  Ownership of the responsibility object and object buffer pass to this
##  function. The client is not responsible for cleanup.
##

proc orcObjectLayerEmit*(objLayer: OrcObjectLayerRef;
                        r: OrcMaterializationResponsibilityRef;
                        objBuffer: MemoryBufferRef) {.
    importc: "LLVMOrcObjectLayerEmit", dynlib: LLVMLib.}
## *
##  Dispose of an ObjectLayer.
##

proc orcDisposeObjectLayer*(objLayer: OrcObjectLayerRef) {.
    importc: "LLVMOrcDisposeObjectLayer", dynlib: LLVMLib.}
proc orcIRTransformLayerEmit*(iRTransformLayer: OrcIRTransformLayerRef;
                             mr: OrcMaterializationResponsibilityRef;
                             tsm: OrcThreadSafeModuleRef) {.
    importc: "LLVMOrcIRTransformLayerEmit", dynlib: LLVMLib.}
## *
##  Set the transform function of the provided transform layer, passing through a
##  pointer to user provided context.
##

proc orcIRTransformLayerSetTransform*(iRTransformLayer: OrcIRTransformLayerRef;
    transformFunction: OrcIRTransformLayerTransformFunction; ctx: pointer) {.
    importc: "LLVMOrcIRTransformLayerSetTransform", dynlib: LLVMLib.}
## *
##  Set the transform function on an LLVMOrcObjectTransformLayer.
##

proc orcObjectTransformLayerSetTransform*(
    objTransformLayer: OrcObjectTransformLayerRef;
    transformFunction: OrcObjectTransformLayerTransformFunction; ctx: pointer) {.
    importc: "LLVMOrcObjectTransformLayerSetTransform", dynlib: LLVMLib.}
## *
##  Create a LocalIndirectStubsManager from the given target triple.
##
##  The resulting IndirectStubsManager is owned by the client
##  and must be disposed of by calling LLVMOrcDisposeDisposeIndirectStubsManager.
##

proc orcCreateLocalIndirectStubsManager*(targetTriple: cstring): OrcIndirectStubsManagerRef {.
    importc: "LLVMOrcCreateLocalIndirectStubsManager", dynlib: LLVMLib.}
## *
##  Dispose of an IndirectStubsManager.
##

proc orcDisposeIndirectStubsManager*(ism: OrcIndirectStubsManagerRef) {.
    importc: "LLVMOrcDisposeIndirectStubsManager", dynlib: LLVMLib.}
proc orcCreateLocalLazyCallThroughManager*(targetTriple: cstring;
    es: OrcExecutionSessionRef; errorHandlerAddr: OrcJITTargetAddress;
    lctm: ptr OrcLazyCallThroughManagerRef): ErrorRef {.
    importc: "LLVMOrcCreateLocalLazyCallThroughManager", dynlib: LLVMLib.}
## *
##  Dispose of an LazyCallThroughManager.
##

proc orcDisposeLazyCallThroughManager*(lctm: OrcLazyCallThroughManagerRef) {.
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

proc orcCreateDumpObjects*(dumpDir: cstring; identifierOverride: cstring): OrcDumpObjectsRef {.
    importc: "LLVMOrcCreateDumpObjects", dynlib: LLVMLib.}
## *
##  Dispose of a DumpObjects instance.
##

proc orcDisposeDumpObjects*(dumpObjects: OrcDumpObjectsRef) {.
    importc: "LLVMOrcDisposeDumpObjects", dynlib: LLVMLib.}
## *
##  Dump the contents of the given MemoryBuffer.
##

proc orcDumpObjectsCallOperator*(dumpObjects: OrcDumpObjectsRef;
                                objBuffer: ptr MemoryBufferRef): ErrorRef {.
    importc: "LLVMOrcDumpObjects_CallOperator", dynlib: LLVMLib.}
## *
##  @}
##

## !!!Ignored construct:  LLVM_C_EXTERN_C_END #  LLVM_C_ORC_H [NewLine]
## Error: expected ';'!!!
