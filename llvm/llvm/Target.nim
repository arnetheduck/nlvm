## #===-- llvm-c/Target.h - Target Lib C Iface --------------------*- C++ -*-===
## #
## #                     The LLVM Compiler Infrastructure
## #
## # This file is distributed under the University of Illinois Open Source
## # License. See LICENSE.TXT for details.
## #
## #===----------------------------------------------------------------------===
## #
## # This header declares the C interface to libLLVMTarget.a, which
## # implements target information.
## #
## # Many exotic languages can interoperate with C code but have a harder time
## # with C++ due to name mangling. So in addition to C, this interface enables
## # tools written in such languages.
## #
## #===----------------------------------------------------------------------===

when defined(msc_Ver) and not defined(inline):
  const
    inline* = inline
## #*
## #  @defgroup LLVMCTarget Target information
## #  @ingroup LLVMC
## #
## #  @{
## #

type
  ByteOrdering* {.size: sizeof(cint).} = enum
    BigEndian, LittleEndian


type
  TargetDataRef* = ptr OpaqueTargetData
  TargetLibraryInfoRef* = ptr OpaqueTargetLibraryInfotData

## #===-- Target Data -------------------------------------------------------===
## #*
## #  Obtain the data layout for a module.
## #
## #  @see Module::getDataLayout()
## #

proc getModuleDataLayout*(m: ModuleRef): TargetDataRef {.
    importc: "LLVMGetModuleDataLayout", llvmImport.}
## #*
## #  Set the data layout for a module.
## #
## #  @see Module::setDataLayout()
## #

proc setModuleDataLayout*(m: ModuleRef; dl: TargetDataRef) {.
    importc: "LLVMSetModuleDataLayout", llvmImport.}
## #* Creates target data from a target layout string.
## #    See the constructor llvm::DataLayout::DataLayout.

proc createTargetData*(stringRep: cstring): TargetDataRef {.
    importc: "LLVMCreateTargetData", llvmImport.}
## #* Deallocates a TargetData.
## #    See the destructor llvm::DataLayout::~DataLayout.

proc disposeTargetData*(td: TargetDataRef) {.importc: "LLVMDisposeTargetData",
    llvmImport.}
## #* Adds target library information to a pass manager. This does not take
## #    ownership of the target library info.
## #    See the method llvm::PassManagerBase::add.

proc addTargetLibraryInfo*(tli: TargetLibraryInfoRef; pm: PassManagerRef) {.
    importc: "LLVMAddTargetLibraryInfo", llvmImport.}
## #* Converts target data to a target layout string. The string must be disposed
## #    with LLVMDisposeMessage.
## #    See the constructor llvm::DataLayout::DataLayout.

proc copyStringRepOfTargetData*(td: TargetDataRef): cstring {.
    importc: "LLVMCopyStringRepOfTargetData", llvmImport.}
## #* Returns the byte order of a target, either LLVMBigEndian or
## #    LLVMLittleEndian.
## #    See the method llvm::DataLayout::isLittleEndian.

proc byteOrder*(td: TargetDataRef): ByteOrdering {.importc: "LLVMByteOrder",
    llvmImport.}
## #* Returns the pointer size in bytes for a target.
## #    See the method llvm::DataLayout::getPointerSize.

proc pointerSize*(td: TargetDataRef): cuint {.importc: "LLVMPointerSize",
    llvmImport.}
## #* Returns the pointer size in bytes for a target for a specified
## #    address space.
## #    See the method llvm::DataLayout::getPointerSize.

proc pointerSizeForAS*(td: TargetDataRef; `as`: cuint): cuint {.
    importc: "LLVMPointerSizeForAS", llvmImport.}
## #* Returns the integer type that is the same size as a pointer on a target.
## #    See the method llvm::DataLayout::getIntPtrType.

proc intPtrType*(td: TargetDataRef): TypeRef {.importc: "LLVMIntPtrType",
    llvmImport.}
## #* Returns the integer type that is the same size as a pointer on a target.
## #    This version allows the address space to be specified.
## #    See the method llvm::DataLayout::getIntPtrType.

proc intPtrTypeForAS*(td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForAS", llvmImport.}
## #* Returns the integer type that is the same size as a pointer on a target.
## #    See the method llvm::DataLayout::getIntPtrType.

proc intPtrTypeInContext*(c: ContextRef; td: TargetDataRef): TypeRef {.
    importc: "LLVMIntPtrTypeInContext", llvmImport.}
## #* Returns the integer type that is the same size as a pointer on a target.
## #    This version allows the address space to be specified.
## #    See the method llvm::DataLayout::getIntPtrType.

proc intPtrTypeForASInContext*(c: ContextRef; td: TargetDataRef; `as`: cuint): TypeRef {.
    importc: "LLVMIntPtrTypeForASInContext", llvmImport.}
## #* Computes the size of a type in bytes for a target.
## #    See the method llvm::DataLayout::getTypeSizeInBits.

proc sizeOfXXXTypeInBits*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMSizeOfTypeInBits", llvmImport.}
## #* Computes the storage size of a type in bytes for a target.
## #    See the method llvm::DataLayout::getTypeStoreSize.

proc storeSizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMStoreSizeOfType", llvmImport.}
## #* Computes the ABI size of a type in bytes for a target.
## #    See the method llvm::DataLayout::getTypeAllocSize.

proc aBISizeOfType*(td: TargetDataRef; ty: TypeRef): culonglong {.
    importc: "LLVMABISizeOfType", llvmImport.}
## #* Computes the ABI alignment of a type in bytes for a target.
## #    See the method llvm::DataLayout::getTypeABISize.

proc aBIAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMABIAlignmentOfType", llvmImport.}
## #* Computes the call frame alignment of a type in bytes for a target.
## #    See the method llvm::DataLayout::getTypeABISize.

proc callFrameAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMCallFrameAlignmentOfType", llvmImport.}
## #* Computes the preferred alignment of a type in bytes for a target.
## #    See the method llvm::DataLayout::getTypeABISize.

proc preferredAlignmentOfType*(td: TargetDataRef; ty: TypeRef): cuint {.
    importc: "LLVMPreferredAlignmentOfType", llvmImport.}
## #* Computes the preferred alignment of a global variable in bytes for a target.
## #    See the method llvm::DataLayout::getPreferredAlignment.

proc preferredAlignmentOfGlobal*(td: TargetDataRef; globalVar: ValueRef): cuint {.
    importc: "LLVMPreferredAlignmentOfGlobal", llvmImport.}
## #* Computes the structure element that contains the byte offset for a target.
## #    See the method llvm::StructLayout::getElementContainingOffset.

proc elementAtOffset*(td: TargetDataRef; structTy: TypeRef; offset: culonglong): cuint {.
    importc: "LLVMElementAtOffset", llvmImport.}
## #* Computes the byte offset of the indexed struct element for a target.
## #    See the method llvm::StructLayout::getElementContainingOffset.

proc offsetOfElement*(td: TargetDataRef; structTy: TypeRef; element: cuint): culonglong {.
    importc: "LLVMOffsetOfElement", llvmImport.}
## #*
## #  @}
## #
