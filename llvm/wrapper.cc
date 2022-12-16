#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfo.h"
#include "llvm/IR/Module.h"

#include "llvm/CodeGen/CommandFlags.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Target/CodeGenCWrappers.h"
#include "llvm/Target/TargetMachine.h"

#include "lld/Common/Driver.h"

#include "llvm-c/Types.h"
#include "llvm-c/Core.h"
#include "llvm-c/TargetMachine.h"

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

extern "C" bool LLVMNimLLDLinkElf(const char **args, size_t arg_count) {
  ArrayRef<const char *> array_ref_args(args, arg_count);
  return lld::elf::link(array_ref_args, llvm::outs(), llvm::errs(), false, false);
}

extern "C" bool LLVMNimLLDLinkWasm(const char **args, size_t arg_count) {
  ArrayRef<const char *> array_ref_args(args, arg_count);
  return lld::wasm::link(array_ref_args, llvm::outs(), llvm::errs(), false, false);
}

static codegen::RegisterCodeGenFlags CGF;

static Target *unwrap(LLVMTargetRef P) {
  return reinterpret_cast<Target*>(P);
}
static LLVMTargetMachineRef wrap(const TargetMachine *P) {
  return reinterpret_cast<LLVMTargetMachineRef>(const_cast<TargetMachine *>(P));
}

extern "C" LLVMTargetMachineRef LLVMNimCreateTargetMachine(LLVMTargetRef T,
        const char *TT, const char *CPU, const char *Features,
        LLVMCodeGenOptLevel Level, LLVMRelocMode Reloc,
        LLVMCodeModel CodeModel) {
  // This function is needed to register and use the common codegen flags -
  // in particular when using lld which doesn't support mixed `.ctors` and
  // `.init_array` sections and is made to work by using `.init_array`
  // alone

  Optional<Reloc::Model> RM;
  switch (Reloc){
    case LLVMRelocStatic:
      RM = Reloc::Static;
      break;
    case LLVMRelocPIC:
      RM = Reloc::PIC_;
      break;
    case LLVMRelocDynamicNoPic:
      RM = Reloc::DynamicNoPIC;
      break;
    case LLVMRelocROPI:
      RM = Reloc::ROPI;
      break;
    case LLVMRelocRWPI:
      RM = Reloc::RWPI;
      break;
    case LLVMRelocROPI_RWPI:
      RM = Reloc::ROPI_RWPI;
      break;
    default:
      break;
  }

  bool JIT;
  Optional<CodeModel::Model> CM = unwrap(CodeModel, JIT);

  CodeGenOpt::Level OL;
  switch (Level) {
    case LLVMCodeGenLevelNone:
      OL = CodeGenOpt::None;
      break;
    case LLVMCodeGenLevelLess:
      OL = CodeGenOpt::Less;
      break;
    case LLVMCodeGenLevelAggressive:
      OL = CodeGenOpt::Aggressive;
      break;
    default:
      OL = CodeGenOpt::Default;
      break;
  }

  TargetOptions opt = codegen::InitTargetOptionsFromCodeGenFlags(Triple(TT));
  return wrap(unwrap(T)->createTargetMachine(TT, CPU, Features, opt, RM, CM,
                                             OL, JIT));
}
