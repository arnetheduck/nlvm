LLVM_INC=llvm-project/llvm/include

DEFS="\
--def:LLVM_C_EXTERN_C_BEGIN= \
--def:LLVM_C_EXTERN_C_END= \
--def:LLVM_C_ABI= \
--def:LLVM_ATTRIBUTE_C_DEPRECATED= \
--def:LLVM_CLANG_C_EXTERN_C_BEGIN= \
--def:LLVM_CLANG_C_EXTERN_C_END= \
--def:CINDEX_LINKAGE= \
--def:CINDEX_DEPRECATED= \
--def:time_t=Time \
"
C2NIM="../../c2nim/c2nim"
C2NIMFLAGS="--nep1 --skipinclude --prefix:LLVM --dynlib:LLVMLib $DEFS --stdints"

mkdir -p llvm

HEADERS="Analysis.h BitReader.h BitWriter.h Comdat.h Core.h Error.h ExecutionEngine.h DebugInfo.h IRReader.h Linker.h LLJIT.h OrcEE.h Orc.h Target.h TargetMachine.h Support.h Types.h Transforms/PassBuilder.h"

# Clang-c headers (exclude ExternC.h - just macros)
CLANG_HEADERS="BuildSystem.h CXCompilationDatabase.h CXDiagnostic.h CXErrorCode.h CXFile.h CXSourceLocation.h CXString.h Documentation.h FatalErrorHandler.h Index.h Rewrite.h"

for a in $HEADERS; do
  OUT="llvm/${a%.h}.nim"
  $C2NIM $C2NIMFLAGS $LLVM_INC/llvm-c/$a -o:$OUT

  # Seems to be no way to get just importc and not dynlib, but since
  # we'll be linking llvm statically, we'll need it just so
  # perl -i -p -e 's/",\s*/",/g' $OUT
  # perl -i -p -e 's/,\s*dynlib: LLVMLib//g' $OUT

  # workaround for upstream bug
  sed -i -e 's/ptr opaque/ptr Opaque/' $OUT
  sed -i -e 's/ptr orcOpaque/ptr OrcOpaque/' $OUT

  # workaround for reserved keword
  sed -i -e 's/sizeOf/sizeOfX/' $OUT
  # workaround for reserved keword
  sed -i -e 's/typeOf/typeOfX/' $OUT

  sed -i -e "s/uintptrT/uint/" $OUT

  nph llvm

done

mkdir -p clang

# Generate clang-c wrappers
CLANG_INC=llvm-project/clang/include
CLANG_CFLAGS="--nep1 --skipinclude --prefix:clang_ --dynlib:CLangLib $DEFS --stdints"

for a in $CLANG_HEADERS; do
  OUT="clang/${a%.h}.nim"
  $C2NIM $CLANG_CFLAGS $CLANG_INC/clang-c/$a -o:$OUT

  # workaround for upstream bug
  sed -i -e 's/ptr opaque/ptr Opaque/' $OUT

  # workaround for reserved keywords
  sed -i -e 's/typeOf/typeOfX/' $OUT
  sed -i -e 's/sizeOf/sizeOfX/' $OUT

  nph clang

done
