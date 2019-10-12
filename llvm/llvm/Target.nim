## ===-- llvm-c/Target.h - Target Lib C Iface --------------------*- C++ -*-===
##
##  Part of the LLVM Project, under the Apache License v2.0 with LLVM
##  Exceptions.
##  See https://llvm.org/LICENSE.txt for license information.
##  SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
##
## ===----------------------------------------------------------------------===
##
##  This header declares the C interface to libLLVMTarget.a, which
##  implements target information.
##
##  Many exotic languages can interoperate with C code but have a harder time
##  with C++ due to name mangling. So in addition to C, this interface enables
##  tools written in such languages.
##
## ===----------------------------------------------------------------------===

## *
##  @defgroup LLVMCTarget Target information
##  @ingroup LLVMC
##
##  @{
##

type
  ByteOrdering* {.size: sizeof(cint).} = enum
    BigEndian, LittleEndian


type
  TargetDataRef* = ptr OpaqueTargetData
  TargetLibraryInfoRef* = ptr OpaqueTargetLibraryInfotData

##  Declare all of the target-initialization functions that are available.
## #define LLVM_TARGET(TargetName) \
##   void LLVMInitialize##TargetName##TargetInfo(void);

## #define LLVM_TARGET(TargetName) void LLVMInitialize##TargetName##Target(void);

## #define LLVM_TARGET(TargetName) \
##   void LLVMInitialize##TargetName##TargetMC(void);

##  Declare all of the available assembly printer initialization functions.
## #define LLVM_ASM_PRINTER(TargetName) \
##   void LLVMInitialize##TargetName##AsmPrinter(void);

##  Declare all of the available assembly parser initialization functions.
## #define LLVM_ASM_PARSER(TargetName) \
##   void LLVMInitialize##TargetName##AsmParser(void);

##  Declare all of the available disassembler initialization functions.
## #define LLVM_DISASSEMBLER(TargetName) \
##   void LLVMInitialize##TargetName##Disassembler(void);

## * LLVMInitializeAllTargetInfos - The main program should call this function if
##     it wants access to all available targets that LLVM is configured to
##     support.
## static inline void LLVMInitializeAllTargetInfos(void) {
## #define LLVM_TARGET(TargetName) LLVMInitialize##TargetName##TargetInfo();
## #include "llvm/Config/Targets.def"
## #undef LLVM_TARGET  /* Explicit undef to make SWIG happier */
## }
## * LLVMInitializeAllTargets - The main program should call this function if it
##     wants to link in all available targets that LLVM is configured to
##     support.
## static inline void LLVMInitializeAllTargets(void) {
## #define LLVM_TARGET(TargetName) LLVMInitialize##TargetName##Target();
## #include "llvm/Config/Targets.def"
## #undef LLVM_TARGET  /* Explicit undef to make SWIG happier */
## }
## * LLVMInitializeAllTargetMCs - The main program should call this function if
##     it wants access to all available target MC that LLVM is configured to
##     support.
## static inline void LLVMInitializeAllTargetMCs(void) {
## #define LLVM_TARGET(TargetName) LLVMInitialize##TargetName##TargetMC();
## #include "llvm/Config/Targets.def"
## #undef LLVM_TARGET  /* Explicit undef to make SWIG happier */
## }
## * LLVMInitializeAllAsmPrinters - The main program should call this function if
##     it wants all asm printers that LLVM is configured to support, to make them
##     available via the TargetRegistry.
## static inline void LLVMInitializeAllAsmPrinters(void) {
## #define LLVM_ASM_PRINTER(TargetName) LLVMInitialize##TargetName##AsmPrinter();
## #include "llvm/Config/AsmPrinters.def"
## #undef LLVM_ASM_PRINTER  /* Explicit undef to make SWIG happier */
## }
## * LLVMInitializeAllAsmParsers - The main program should call this function if
##     it wants all asm parsers that LLVM is configured to support, to make them
##     available via the TargetRegistry.
## static inline void LLVMInitializeAllAsmParsers(void) {
## #define LLVM_ASM_PARSER(TargetName) LLVMInitialize##TargetName##AsmParser();
## #include "llvm/Config/AsmParsers.def"
## #undef LLVM_ASM_PARSER  /* Explicit undef to make SWIG happier */
## }
## * LLVMInitializeAllDisassemblers - The main program should call this function
##     if it wants all disassemblers that LLVM is configured to support, to make
##     them available via the TargetRegistry.
## static inline void LLVMInitializeAllDisassemblers(void) {
## #define LLVM_DISASSEMBLER(TargetName) \
##   LLVMInitialize##TargetName##Disassembler();
## #include "llvm/Config/Disassemblers.def"
## #undef LLVM_DISASSEMBLER  /* Explicit undef to make SWIG happier */
## }
## * LLVMInitializeNativeTarget - The main program should call this function to
##     initialize the native target corresponding to the host.  This is useful
##     for JIT applications to ensure that the target gets linked in correctly.


## * LLVMInitializeNativeTargetAsmParser - The main program should call this
##     function to initialize the parser for the native target corresponding to the
##     host.

## * LLVMInitializeNativeTargetAsmPrinter - The main program should call this
##     function to initialize the printer for the native target corresponding to
##     the host.

## * LLVMInitializeNativeTargetDisassembler - The main program should call this
##     function to initialize the disassembler for the native target corresponding
##     to the host.

## ===-- Target Data -------------------------------------------------------===
## *
##  Obtain the data layout for a module.
##
##  @see Module::getDataLayout()
##

proc getModuleDataLayout*(m: ModuleRef): TargetDataRef {.
    importc: "LLVMGetModuleDataLayout", dynlib: LLVMLib.}
## *
##  Set the data layout for a module.
##
##  @see Module::setDataLayout()
##

proc setModuleDataLayout*(m: ModuleRef; dl: TargetDataRef) {.
    importc: "LLVMSetModuleDataLayout", dynlib: LLVMLib.}
## * Creates target data from a target layout string.
##     See the constructor llvm::DataLayout::DataLayout.

proc createTargetData*(stringRep: cstring): TargetDataRef {.
    importc: "LLVMCreateTargetData", dynlib: LLVMLib.}
## * Deallocates a TargetData.
##     See the destructor llvm::DataLayout::~DataLayout.

proc disposeTargetData*(td: TargetDataRef) {.importc: "LLVMDisposeTargetData",
    dynlib: LLVMLib.}
## * Adds target library information to a pass manager. This does not take
##     ownership of the target library info.
##     See the method llvm::PassManagerBase::add.

proc addTargetLibraryInfo*(tli: TargetLibraryInfoRef; pm: PassManagerRef) {.
    importc: "LLVMAddTargetLibraryInfo", dynlib: LLVMLib.}
## * Converts target data to a target layout string. The string must be disposed
##     with LLVMDisposeMessage.
##     See the constructor llvm::DataLayout::DataLayout.

proc copyStringRepOfTargetData*(td: TargetDataRef): cstring {.
    importc: "LLVMCopyStringRepOfTargetData", dynlib: LLVMLib.}
## * Returns the byte order of a target, either LLVMBigEndian or
##     LLVMLittleEndian.
##     See the method llvm::DataLayout::isLittleEndian.

proc byteOrder*(td: TargetDataRef): ByteOrdering {.importc: "LLVMByteOrder",
    dynlib: LLVMLib.}
## * Returns the pointer size in bytes for a target.
##     See the method llvm::DataLayout::getPointerSize.

proc pointerSize*(td: TargetDataRef): cuint {.importc: "LLVMPointerSize",
    dynlib: LLVMLib.}
## * Returns the pointer size in bytes for a target for a specified
##     address space.
##     See the method llvm::DataLayout::getPointerSize.

proc pointerSizeForAS*(td: TargetDataRef; `as`: cuint): cuint {.
    importc: "LLVMPointerSizeForAS", dynlib: LLVMLib.}
## * Returns the integer type that is the same size as a pointer on a target.
##     See the method llvm::DataLayout::getIntPtrType.

proc intPtrType*(td: TargetDataRef): TypeRef {.importc: "LLVMIntPtrType",
    dynlib: LLVMLib.}
## * Returns the integer type that is the same size as a pointer on a target.
##     This version allows the address space to be specified.
##     See the method llvm::DataLayout::getIntPtrType.

proc intPtrTypeForAS*(td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForAS", dynlib: LLVMLib.}
## * Returns the integer type that is the same size as a pointer on a target.
##     See the method llvm::DataLayout::getIntPtrType.

proc intPtrTypeInContext*(c: ContextRef; td: TargetDataRef): TypeRef {.
    importc: "LLVMIntPtrTypeInContext", dynlib: LLVMLib.}
## * Returns the integer type that is the same size as a pointer on a target.
##     This version allows the address space to be specified.
##     See the method llvm::DataLayout::getIntPtrType.

proc intPtrTypeForASInContext*(c: ContextRef; td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForASInContext", dynlib: LLVMLib.}
## * Computes the size of a type in bytes for a target.
##     See the method llvm::DataLayout::getTypeSizeInBits.

proc sizeOfXTypeInBits*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMSizeOfTypeInBits", dynlib: LLVMLib.}
## * Computes the storage size of a type in bytes for a target.
##     See the method llvm::DataLayout::getTypeStoreSize.

proc storeSizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMStoreSizeOfType", dynlib: LLVMLib.}
## * Computes the ABI size of a type in bytes for a target.
##     See the method llvm::DataLayout::getTypeAllocSize.

proc aBISizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMABISizeOfType", dynlib: LLVMLib.}
## * Computes the ABI alignment of a type in bytes for a target.
##     See the method llvm::DataLayout::getTypeABISize.

proc aBIAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMABIAlignmentOfType", dynlib: LLVMLib.}
## * Computes the call frame alignment of a type in bytes for a target.
##     See the method llvm::DataLayout::getTypeABISize.

proc callFrameAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMCallFrameAlignmentOfType", dynlib: LLVMLib.}
## * Computes the preferred alignment of a type in bytes for a target.
##     See the method llvm::DataLayout::getTypeABISize.

proc preferredAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMPreferredAlignmentOfType", dynlib: LLVMLib.}
## * Computes the preferred alignment of a global variable in bytes for a target.
##     See the method llvm::DataLayout::getPreferredAlignment.

proc preferredAlignmentOfGlobal*(td: TargetDataRef; globalVar: ValueRef): cuint {.
    importc: "LLVMPreferredAlignmentOfGlobal", dynlib: LLVMLib.}
## * Computes the structure element that contains the byte offset for a target.
##     See the method llvm::StructLayout::getElementContainingOffset.

proc elementAtOffset*(td: TargetDataRef; structTy: TypeRef; offset: culonglong): cuint {.
    importc: "LLVMElementAtOffset", dynlib: LLVMLib.}
## * Computes the byte offset of the indexed struct element for a target.
##     See the method llvm::StructLayout::getElementContainingOffset.

proc offsetOfElement*(td: TargetDataRef; structTy: TypeRef; element: cuint): culonglong {.
    importc: "LLVMOffsetOfElement", dynlib: LLVMLib.}
## *
##  @}
##
