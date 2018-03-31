#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Module.h"

#include "llvm-c/Types.h"

// wrapper based on the rust wrapper that will hopefully find its way to
// upstream one fine day
using namespace llvm;

typedef DIBuilder *LLVMNimDIBuilderRef;

template <typename DIT> inline DIT *unwrapDIPtr(LLVMMetadataRef Ref) {
  return (DIT *)(Ref ? unwrap<MDNode>(Ref) : nullptr);
}

using LLVMNimDIFlags = uint32_t;

extern "C" void LLVMNimAddModuleFlag(LLVMModuleRef M, const char *Name,
                                     uint32_t Value) {
  unwrap(M)->addModuleFlag(Module::Warning, Name, Value);
}

extern "C" LLVMMetadataRef
LLVMNimDIBuilderCreateFile(LLVMNimDIBuilderRef Builder, const char *Filename,
                           const char *Directory) {
  return wrap(Builder->createFile(Filename, Directory));
}

extern "C" LLVMMetadataRef
LLVMNimDIBuilderCreateSubroutineType(LLVMNimDIBuilderRef Builder,
                                     LLVMMetadataRef ParameterTypes) {
  return wrap(Builder->createSubroutineType(
      DITypeRefArray(unwrap<MDTuple>(ParameterTypes))));
}

extern "C" LLVMMetadataRef LLVMNimDIBuilderCreateFunction(
    LLVMNimDIBuilderRef Builder, LLVMMetadataRef Scope, const char *Name,
    const char *LinkageName, LLVMMetadataRef File, unsigned LineNo,
    LLVMMetadataRef Ty, bool IsLocalToUnit, bool IsDefinition,
    unsigned ScopeLine, LLVMNimDIFlags Flags, bool IsOptimized, LLVMValueRef Fn,
    LLVMMetadataRef TParam, LLVMMetadataRef Decl) {
  DITemplateParameterArray TParams =
      DITemplateParameterArray(unwrap<MDTuple>(TParam));
  DISubprogram *Sub = Builder->createFunction(
      unwrapDIPtr<DIScope>(Scope), Name, LinkageName, unwrapDIPtr<DIFile>(File),
      LineNo, unwrapDIPtr<DISubroutineType>(Ty), IsLocalToUnit, IsDefinition,
      ScopeLine, DINode::DIFlags(Flags), IsOptimized, TParams,
      unwrapDIPtr<DISubprogram>(Decl));
  unwrap<Function>(Fn)->setSubprogram(Sub);
  return wrap(Sub);
}

extern "C" LLVMMetadataRef
LLVMNimDIBuilderCreateBasicType(LLVMNimDIBuilderRef Builder, const char *Name,
                                uint64_t SizeInBits, unsigned Encoding) {
  return wrap(Builder->createBasicType(Name, SizeInBits, Encoding));
}

extern "C" LLVMMetadataRef LLVMNimDIBuilderCreatePointerType(
    LLVMNimDIBuilderRef Builder, LLVMMetadataRef PointeeTy, uint64_t SizeInBits,
    uint32_t AlignInBits, const char *Name) {
  return wrap(Builder->createPointerType(unwrapDIPtr<DIType>(PointeeTy),
                                         SizeInBits, AlignInBits, llvm::None,
                                         Name));
}

extern "C" LLVMMetadataRef LLVMNimDIBuilderCreateStructType(
    LLVMNimDIBuilderRef Builder, LLVMMetadataRef Scope, const char *Name,
    LLVMMetadataRef File, unsigned LineNumber, uint64_t SizeInBits,
    uint32_t AlignInBits, LLVMNimDIFlags Flags, LLVMMetadataRef DerivedFrom,
    LLVMMetadataRef Elements, unsigned RunTimeLang,
    LLVMMetadataRef VTableHolder, const char *UniqueId) {
  return wrap(Builder->createStructType(
      unwrapDIPtr<DIScope>(Scope), Name, unwrapDIPtr<DIFile>(File), LineNumber,
      SizeInBits, AlignInBits, DINode::DIFlags(Flags),
      unwrapDIPtr<DIType>(DerivedFrom),
      DINodeArray(unwrapDIPtr<MDTuple>(Elements)), RunTimeLang,
      unwrapDIPtr<DIType>(VTableHolder), UniqueId));
}

extern "C" LLVMMetadataRef LLVMNimDIBuilderCreateMemberType(
    LLVMNimDIBuilderRef Builder, LLVMMetadataRef Scope, const char *Name,
    LLVMMetadataRef File, unsigned LineNo, uint64_t SizeInBits,
    uint32_t AlignInBits, uint64_t OffsetInBits, LLVMNimDIFlags Flags,
    LLVMMetadataRef Ty) {
  return wrap(Builder->createMemberType(
      unwrapDIPtr<DIScope>(Scope), Name, unwrapDIPtr<DIFile>(File), LineNo,
      SizeInBits, AlignInBits, OffsetInBits, DINode::DIFlags(Flags),
      unwrapDIPtr<DIType>(Ty)));
}

extern "C" LLVMMetadataRef LLVMNimDIBuilderCreateStaticVariable(
    LLVMNimDIBuilderRef Builder, LLVMMetadataRef Context, const char *Name,
    const char *LinkageName, LLVMMetadataRef File, unsigned LineNo,
    LLVMMetadataRef Ty, bool IsLocalToUnit, LLVMValueRef V,
    LLVMMetadataRef Decl = nullptr, uint32_t AlignInBits = 0) {
  llvm::GlobalVariable *InitVal = cast<llvm::GlobalVariable>(unwrap(V));

  llvm::DIExpression *InitExpr = nullptr;
  if (llvm::ConstantInt *IntVal = llvm::dyn_cast<llvm::ConstantInt>(InitVal)) {
    InitExpr = Builder->createConstantValueExpression(
        IntVal->getValue().getSExtValue());
  } else if (llvm::ConstantFP *FPVal =
                 llvm::dyn_cast<llvm::ConstantFP>(InitVal)) {
    InitExpr = Builder->createConstantValueExpression(
        FPVal->getValueAPF().bitcastToAPInt().getZExtValue());
  }

  llvm::DIGlobalVariableExpression *VarExpr =
      Builder->createGlobalVariableExpression(
          unwrapDIPtr<DIScope>(Context), Name, LinkageName,
          unwrapDIPtr<DIFile>(File), LineNo, unwrapDIPtr<DIType>(Ty),
          IsLocalToUnit, InitExpr, unwrapDIPtr<MDNode>(Decl), AlignInBits);

  InitVal->setMetadata("dbg", VarExpr);

  return wrap(VarExpr);
}

extern "C" LLVMMetadataRef LLVMNimDIBuilderCreateVariable(
    LLVMNimDIBuilderRef Builder, unsigned Tag, LLVMMetadataRef Scope,
    const char *Name, LLVMMetadataRef File, unsigned LineNo, LLVMMetadataRef Ty,
    bool AlwaysPreserve, LLVMNimDIFlags Flags, unsigned ArgNo,
    uint32_t AlignInBits) {
  if (Tag == 0x100) { // DW_TAG_auto_variable
    return wrap(Builder->createAutoVariable(
        unwrapDIPtr<DIScope>(Scope), Name, unwrapDIPtr<DIFile>(File), LineNo,
        unwrapDIPtr<DIType>(Ty), AlwaysPreserve, DINode::DIFlags(Flags),
        AlignInBits));
  } else {
    return wrap(Builder->createParameterVariable(
        unwrapDIPtr<DIScope>(Scope), Name, ArgNo, unwrapDIPtr<DIFile>(File),
        LineNo, unwrapDIPtr<DIType>(Ty), AlwaysPreserve,
        DINode::DIFlags(Flags)));
  }
}

extern "C" LLVMMetadataRef
LLVMNimDIBuilderCreateArrayType(LLVMNimDIBuilderRef Builder, uint64_t Size,
                                uint32_t AlignInBits, LLVMMetadataRef Ty,
                                LLVMMetadataRef Subscripts) {
  return wrap(
      Builder->createArrayType(Size, AlignInBits, unwrapDIPtr<DIType>(Ty),
                               DINodeArray(unwrapDIPtr<MDTuple>(Subscripts))));
}

extern "C" LLVMMetadataRef
LLVMNimDIBuilderCreateSubrange(LLVMNimDIBuilderRef Builder, int64_t Lo,
                               int64_t Count) {
  return wrap(Builder->getOrCreateSubrange(Lo, Count));
}

extern "C" LLVMMetadataRef
LLVMNimDIBuilderGetOrCreateArray(LLVMNimDIBuilderRef Builder,
                                 LLVMMetadataRef *Ptr, unsigned Count) {
  Metadata **DataValue = unwrap(Ptr);
  return wrap(
      Builder->getOrCreateArray(ArrayRef<Metadata *>(DataValue, Count)).get());
}

extern "C" LLVMValueRef
LLVMNimDIBuilderInsertDeclareAtEnd(LLVMNimDIBuilderRef Builder, LLVMValueRef V,
                                   LLVMMetadataRef VarInfo, int64_t *AddrOps,
                                   unsigned AddrOpsCount, LLVMValueRef DL,
                                   LLVMBasicBlockRef InsertAtEnd) {
  return wrap(Builder->insertDeclare(
      unwrap(V), unwrap<DILocalVariable>(VarInfo),
      Builder->createExpression(llvm::ArrayRef<int64_t>(AddrOps, AddrOpsCount)),
      DebugLoc(cast<MDNode>(unwrap<MetadataAsValue>(DL)->getMetadata())),
      unwrap(InsertAtEnd)));
}

extern "C" void LLVMNimDICompositeTypeSetTypeArray(LLVMNimDIBuilderRef Builder,
                                                   LLVMMetadataRef CompositeTy,
                                                   LLVMMetadataRef TyArray) {
  DICompositeType *Tmp = unwrapDIPtr<DICompositeType>(CompositeTy);
  Builder->replaceArrays(Tmp, DINodeArray(unwrap<MDTuple>(TyArray)));
}

extern "C" LLVMValueRef
LLVMNimDIBuilderCreateDebugLocation(LLVMContextRef ContextRef, unsigned Line,
                                    unsigned Column, LLVMMetadataRef Scope,
                                    LLVMMetadataRef InlinedAt) {
  LLVMContext &Context = *unwrap(ContextRef);

  DebugLoc debug_loc = DebugLoc::get(Line, Column, unwrapDIPtr<MDNode>(Scope),
                                     unwrapDIPtr<MDNode>(InlinedAt));

  return wrap(MetadataAsValue::get(Context, debug_loc.getAsMDNode()));
}
