#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Module.h"

#include "lld/Common/Driver.h"

#include "llvm-c/Types.h"
#include "llvm-c/Core.h"

using namespace llvm;

typedef DIBuilder *LLVMNimDIBuilderRef;

template <typename DIT> inline DIT *unwrapDIPtr(LLVMMetadataRef Ref) {
  return (DIT *)(Ref ? unwrap<MDNode>(Ref) : nullptr);
}

static MDNode *extractMDNode(MetadataAsValue *MAV) {
  Metadata *MD = MAV->getMetadata();
  assert((isa<MDNode>(MD) || isa<ConstantAsMetadata>(MD)) &&
      "Expected a metadata node or a canonicalized constant");

  if (MDNode *N = dyn_cast<MDNode>(MD))
    return N;

  return MDNode::get(MAV->getContext(), MD);
}

using LLVMNimDIFlags = uint32_t;

extern "C" void LLVMNimDICompositeTypeSetTypeArray(LLVMNimDIBuilderRef Builder,
                                                   LLVMMetadataRef CompositeTy,
                                                   LLVMMetadataRef TyArray) {
  DICompositeType *Tmp = unwrapDIPtr<DICompositeType>(CompositeTy);
  Builder->replaceArrays(Tmp, DINodeArray(unwrap<MDTuple>(TyArray)));
}

extern "C" void LLVMNimSetMetadataGlobal(LLVMValueRef Global,
                                         unsigned KindID,
                                         LLVMValueRef Val) {
  MDNode *N = Val ? extractMDNode(unwrap<MetadataAsValue>(Val)) : nullptr;

  unwrap<GlobalObject>(Global)->setMetadata(KindID, N);
}

extern "C" const char* LLVMNimLLDLinkElf(const char **args, size_t arg_count) {
    ArrayRef<const char *> array_ref_args(args, arg_count);
    SmallVector<char, 128> sv;
    raw_svector_ostream os(sv);

    if (!lld::elf::link(array_ref_args, false, os)) {
      sv.push_back(0);
      return LLVMCreateMessage(&sv[0]);
    }

    return nullptr;
}

extern "C" const char* LLVMNimLLDLinkWasm(const char **args, size_t arg_count) {
    ArrayRef<const char *> array_ref_args(args, arg_count);
    SmallVector<char, 128> sv;
    raw_svector_ostream os(sv);

    if (!lld::wasm::link(array_ref_args, false, os)) {
      sv.push_back(0);
      return LLVMCreateMessage(&sv[0]);
    }

    return nullptr;
}
