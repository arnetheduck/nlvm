LLVM_INC=../ext/llvm-17.0.2.src/include

C2NIM="../../c2nim/c2nim"
C2NIMFLAGS="--nep1 --skipinclude --prefix:LLVM --dynlib:LLVMLib --def:LLVM_C_EXTERN_C_BEGIN= --def:LLVM_C_EXTERN_C_END= --stdints"

HEADERS="BitReader.h BitWriter.h Comdat.h Core.h Error.h ExecutionEngine.h DebugInfo.h IRReader.h Linker.h LLJIT.h OrcEE.h Orc.h Target.h TargetMachine.h Support.h Types.h Transforms/PassBuilder.h"

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

done
