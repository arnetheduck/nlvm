LLVM_HOME=../../llvm-3.6.1.src/build/Debug+Asserts/
LLVM_INC=../../llvm-3.6.1.src/include

C2NIM="../../c2nim/c2nim"
C2NIMFLAGS="--nep1 --skipinclude --prefix:LLVM --dynlib:LLVMLib"

HEADERS="BitReader.h BitWriter.h Core.h Support.h"

for a in $HEADERS; do
  OUT="llvm/${a%.h}.nim"
  $C2NIM $C2NIMFLAGS $LLVM_INC/llvm-c/$a -o:$OUT

  # Seems to be no way to get just importc and not dynlib, but since
  # we'll be linking llvm statically, we'll need it just so
  # perl -i -p -e 's/",\s*/",/g' $OUT
  # perl -i -p -e 's/,\s*dynlib: LLVMLib//g' $OUT

  # workaround for upstream bug
  sed -i -e 's/ptr opaque/ptr Opaque/' $OUT

  # workaround for reserved keword
  sed -i -e 's/sizeOf/sizeOfX/' $OUT
done
