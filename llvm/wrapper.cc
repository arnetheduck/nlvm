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

extern "C" void LLVMNimDICompositeTypeSetTypeArray(LLVMNimDIBuilderRef Builder,
                                                   LLVMMetadataRef CompositeTy,
                                                   LLVMMetadataRef TyArray) {
  DICompositeType *Tmp = unwrapDIPtr<DICompositeType>(CompositeTy);
  Builder->replaceArrays(Tmp, DINodeArray(unwrap<MDTuple>(TyArray)));
}
