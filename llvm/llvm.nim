# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016
# See the LICENSE file for license info (doh!)

const LLVMLib = "libLLVM-5.0.so"

{.passC: "-I../ext/llvm-5.0.1.src/include".}
{.passC: "-I../ext/llvm-5.0.1.src/rel/include".}

{.compile: "wrapper.cc".}

# Includes and helpers for generated code
type
  OpaqueMemoryBuffer = object

type
  # Opaque handles to various things
  OpaqueAttributeRef{.pure, final.} = object
  OpaqueContext{.pure, final.} = object
  OpaqueModule{.pure, final.} = object
  OpaqueType{.pure, final.} = object
  OpaqueValue{.pure, final.} = object
  OpaqueBasicBlock{.pure, final.} = object
  OpaqueBuilder{.pure, final.} = object
  OpaqueModuleProvider{.pure, final.} = object
  OpaquePassManager{.pure, final.} = object
  OpaquePassRegistry{.pure, final.} = object
  OpaqueUse{.pure, final.} = object
  OpaqueDiagnosticInfo{.pure, final.} = object
  OpaqueTargetMachine{.pure, final.} = object
  OpaquePassManagerBuilder{.pure, final.} = object
  OpaqueMetaData{.pure, final.} = object
  OpaqueDIBuilder{.pure, final.} = object
  target{.pure, final.} = object

  # Funny type names that came out of c2nim
  uint64T = uint64
  uint8T = uint8

  OpaqueTargetData{.pure, final.} = object
  OpaqueTargetLibraryInfotData{.pure, final.} = object

include llvm/Types
include llvm/Support

# Target.h is quite a mess with lots of site-specific stuff - some of the parts
# that c2nim can't deal with:
proc initializeX86AsmParser*() {.importc: "LLVMInitializeX86AsmParser", dynlib: LLVMLib.}
proc initializeX86AsmPrinter*() {.importc: "LLVMInitializeX86AsmPrinter", dynlib: LLVMLib.}
proc initializeX86Disassembler*() {.importc: "LLVMInitializeX86Disassembler", dynlib: LLVMLib.}
proc initializeX86Target*() {.importc: "LLVMInitializeX86Target", dynlib: LLVMLib.}
proc initializeX86TargetInfo*() {.importc: "LLVMInitializeX86TargetInfo", dynlib: LLVMLib.}
proc initializeX86TargetMC*() {.importc: "LLVMInitializeX86TargetMC", dynlib: LLVMLib.}

include llvm/Core
include llvm/BitReader
include llvm/BitWriter
include llvm/IRReader
include llvm/Linker
include llvm/Target
include llvm/TargetMachine
include llvm/Transforms/PassManagerBuilder

# Our wrapper

type
  DIBuilder{.pure, final.} = object
  OpaqueNimMetadata{.pure, final.} = object

  NimDIBuilderRef* = ptr DIBuilder
  NimMetadataRef* = ptr OpaqueNimMetadata

# http://www.dwarfstd.org/doc/DWARF4.pdf
const
  DW_ATE_address* = 0x01.cuint
  DW_ATE_boolean* = 0x02.cuint
  DW_ATE_complex_float* = 0x03.cuint
  DW_ATE_float* = 0x04.cuint
  DW_ATE_signed* = 0x05.cuint
  DW_ATE_signed_char* = 0x06.cuint
  DW_ATE_unsigned* = 0x07.cuint
  DW_ATE_unsigned_char* = 0x08.cuint
  DW_ATE_imaginary_float* = 0x09.cuint
  DW_ATE_packed_decimal* = 0x0a.cuint
  DW_ATE_numeric_string* = 0x0b.cuint
  DW_ATE_edited* = 0x0c.cuint
  DW_ATE_signed_fixed* = 0x0d.cuint
  DW_ATE_unsigned_fixed* = 0x0e.cuint
  DW_ATE_decimal_float* = 0x0f.cuint
  DW_ATE_UTF* = 0x10.cuint
  DW_ATE_lo_user* = 0x80.cuint
  DW_ATE_hi_user* = 0xff.cuint

proc nimDebugMetadataVersion*(): uint32 {.importc: "LLVMNimDebugMetadataVersion".}
proc nimAddModuleFlag*(m: ModuleRef, name: cstring, value: uint32) {.importc: "LLVMNimAddModuleFlag".}

proc nimDIBuilderCreate*(m: ModuleRef): NimDIBuilderRef {.importc: "LLVMNimDIBuilderCreate".}
proc nimDIBuilderDispose*(d: NimDIBuilderRef) {.importc: "LLVMNimDIBuilderDispose".}
proc nimDIBuilderFinalize*(d: NimDIBuilderRef) {.importc: "LLVMNimDIBuilderFinalize".}
proc nimDIBuilderCreateCompileUnit*(
  d: NimDIBuilderRef, lang: cuint,
  fileRef: NimMetadataRef, producer: cstring, isOptimized: bool,
  flags: cstring, runtimeVer: cuint, splitName: cstring): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateCompileUnit".}
proc nimDIBuilderCreateSubroutineType*(
  d: NimDIBuilderRef, parameterTypes: NimMetadataRef): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateSubroutineType".}
proc nimDIBuilderCreateFile*(
  d: NimDIBuilderRef, filename: cstring,
  directory: cstring): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateFile".}
proc nimDIBuilderCreateFunction*(d: NimDIBuilderRef, scope: NimMetadataRef,
  name: cstring, linkageName: cstring, file: NimMetadataRef, lineNo: cuint,
  ty: NimMetadataRef, isLocalToUnit: bool, isDefinition: bool, scopeLine: cuint,
  flags: cuint, isOptimized: bool, fn: llvm.ValueRef, tparam: NimMetadataRef,
  decl: NimMetadataRef): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateFunction".}
proc nimDIBuilderCreateBasicType*(
  d: NimDIBuilderRef, name: cstring, bits: uint64,
  encoding: cuint): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateBasicType".}
proc nimDIBuilderCreatePointerType*(
  d: NimDIBuilderRef, pointeeTy: NimMetadataRef, bits: uint64, align: uint32,
  name: cstring): NimMetadataRef {.importc: "LLVMNimDIBuilderCreatePointerType".}
proc nimDIBuilderCreateStructType*(
  d: NimDIBuilderRef, scope: NimMetadataRef, name: cstring,
  file: NimMetadataRef, lineNumber: cuint, sizeBits: uint64,
  alignBits: uint32, flags: cuint, derivedFrom: NimMetadataRef,
  elements: NimMetadataRef, runtimeLang: cuint, vtableHolder: NimMetadataRef,
  uniqueId: cstring): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateStructType".}
proc nimDIBuilderCreateMemberType*(
  d: NimDIBuilderRef, scope: NimMetadataRef, name: cstring,
  file: NimMetadataRef, lineNo: cuint, sizeBits: uint64, alignBits: uint32,
  offsetBits: uint64, flags: cuint,
  ty: NimMetadataRef): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateMemberType".}
proc nimDIBuilderCreateStaticVariable*(
  d: NimDIBuilderRef, context: NimMetadataRef, name: cstring,
  linkageName: cstring, file: NimMetadataRef, lineNo: cuint, ty: NimMetadataRef,
  isLocalToUnit: bool, v: ValueRef, decl: NimMetadataRef,
  alignBits: uint32): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateStaticVariable".}
proc nimDIBuilderCreateVariable*(
  d: NimDIBuilderRef, tag: cuint, scope: NimMetadataRef, name: cstring,
  file: NimMetadataRef, lineNo: cuint, ty: NimMetadataRef, alwaysPreserve: bool,
  flags: cuint, argNo: cuint, alignBits: uint32): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateVariable".}
proc nimDIBuilderCreateArrayType*(
  d: NimDIBuilderRef, size: uint64, alignBits: uint32, ty: NimMetadataRef,
  subscripts: NimMetadataRef): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateArrayType".}
proc nimDIBuilderCreateSubrange*(
  d: NimDIBuilderRef, lo, count: int64): NimMetadataRef {.importc: "LLVMNimDIBuilderCreateSubrange".}
proc nimDIBuilderGetOrCreateArray*(d: NimDIBuilderRef, p: ptr NimMetadataRef,
  count: cuint): NimMetadataRef {.importc: "LLVMNimDIBuilderGetOrCreateArray".}
proc nimDIBuilderInsertDeclareAtEnd*(
  d: NimDIBuilderRef, v: ValueRef, varInfo: NimMetadataRef, addrOps: ptr int64,
  addrOpsCount: cuint, dl: ValueRef,
  insertAtEnd: BasicBlockRef): ValueRef {.importc: "LLVMNimDIBuilderInsertDeclareAtEnd".}
proc nimDICompositeTypeSetTypeArray*(
  d: NimDIBuilderRef, compositeTy: NimMetadataRef,
  tyArray: NimMetadataRef) {.importc: "LLVMNimDICompositeTypeSetTypeArray".}
proc nimDIBuilderCreateDebugLocation*(
  ctx: ContextRef, line: cuint, column: cuint, scope: NimMetadataRef,
  inlinedAt: NimMetadataRef): ValueRef {.importc: "LLVMNimDIBuilderCreateDebugLocation".}

# A few helpers to make things more smooth

proc `$`*(v: ValueRef): string =
  let tmp = v.printValueToString()
  result = $tmp
  disposeMessage(tmp)

proc `$`*(v: TypeRef): string =
  let tmp = v.printTypeToString()
  result = $tmp
  disposeMessage(tmp)

const
  False*: Bool = 0
  True*: Bool = 1

template checkErr(body: expr): expr =
  var err: cstring
  let e {.inject.} = cast[cstringArray](addr(err))
  let res = body
  if res != 0:
    echo err
    quit 1

proc createMemoryBufferWithContentsOfFile(file: string): MemoryBufferRef =
  checkErr: createMemoryBufferWithContentsOfFile(file, addr(result), e)

proc parseIRInContext*(contextRef: ContextRef, file: string): ModuleRef =
  let mb = createMemoryBufferWithContentsOfFile(file)

  checkErr: parseIRInContext(contextRef, mb, addr(result), e)

proc getOrInsertFunction*(m: ModuleRef, name: cstring, functionTy: TypeRef): ValueRef =
  result = m.getNamedFunction(name)
  if result == nil:
    result = m.addFunction(name, functionTy)

iterator params*(fn: ValueRef): ValueRef =
  var it = fn.getFirstParam()
  while it != nil:
    yield it
    it = it.getNextParam()

# openArray -> pointer + len
template asRaw(arr: expr, body: expr): expr =
  var s = @arr
  let n {.inject.} = s.len.cuint
  let p {.inject.} = if s.len > 0: addr(s[0]) else: nil
  body

proc functionType*(returnType: TypeRef, paramTypes: openarray[TypeRef],
                   isVarArg = false): TypeRef =
  asRaw(paramTypes, functionType(returnType, p, n, if isVarArg: llvm.True else: llvm.False))

proc getParamTypes*(functionTy: TypeRef): seq[TypeRef] =
  result = newSeq[TypeRef](functionTy.countParamTypes())
  if result.len > 0:
    functionTy.getParamTypes(addr(result[0]))

proc structType*(elementTypes: openarray[TypeRef],
                 packed = False): TypeRef =
  asRaw(elementTypes, structType(p, n, packed))

proc structSetBody*(structTy: TypeRef; elementTypes: openarray[TypeRef];
                    packed = False) =
  asRaw(elementTypes, structSetBody(structTy, p, n, packed))

proc getStructElementTypes*(structTy: TypeRef): seq[TypeRef] =
  result = newSeq[TypeRef](structTy.countStructElementTypes())
  if result.len > 0:
    structTy.getStructElementTypes(addr(result[0]))

proc pointerType*(elementType: TypeRef): TypeRef =
  pointerType(elementType, 0)

proc constString*(s: string, dontNullTerminate = False): ValueRef =
  constString(s, s.len.cuint, dontNullTerminate)

proc constStruct*(constantVals: openarray[ValueRef]; packed = False): ValueRef =
  asRaw(constantVals, constStruct(p, n, packed))

proc constArray*(t: TypeRef, constantVals: openarray[ValueRef]): ValueRef =
  asRaw(constantVals, constArray(t, p, n))

proc constNamedStruct*(structTy: TypeRef;
                       constantVals: openarray[ValueRef]): ValueRef =
  asRaw(constantVals, constNamedStruct(structTy, p, n))

proc constGEP*(constantVal: ValueRef;
               constantIndices: openarray[ValueRef]): ValueRef =
  asRaw(constantIndices, constGEP(constantVal, p, n))

proc addIncoming*(phiNode: ValueRef; incomingValues: openarray[ValueRef];
                  incomingBlocks: openarray[BasicBlockRef]) =
  var s0 = @incomingValues
  let n0 = s0.len.cuint
  let p0 = if s0.len > 0: addr(s0[0]) else: nil
  var s1 = @incomingBlocks
  let p1 = if s1.len > 0: addr(s1[0]) else: nil
  addIncoming(phiNode, p0, p1, n0)

proc buildGEP*(b: BuilderRef; pointer: ValueRef; indices: openarray[ValueRef];
               name: cstring = ""): ValueRef =
  asRaw(indices, buildGEP(b, pointer, p, n, name))

proc buildInBoundsGEP*(b: BuilderRef; pointer: ValueRef; indices: openarray[ValueRef];
               name: cstring = ""): ValueRef =
  asRaw(indices, buildInBoundsGEP(b, pointer, p, n, name))

proc buildCall*(a2: BuilderRef; fn: ValueRef; args: openarray[ValueRef];
                name: cstring = ""): ValueRef =
  asRaw(args, buildCall(a2, fn, p, n, name))

proc nimDIBuilderGetOrCreateArray*(
  d: NimDIBuilderRef, elems: openarray[NimMetadataRef]): NimMetadataRef =
  var tmp = @elems
  var p = if tmp.len > 0: addr(tmp[0]) else: nil
  d.nimDIBuilderGetOrCreateArray(p, tmp.len.cuint)

template getEnumAttrKind(x: expr): expr = getEnumAttributeKindForName(x, x.len)

let
  attrNoReturn* = getEnumAttrKind("noreturn")
  attrNoInline* = getEnumAttrKind("noinline")

proc addFuncAttribute*(f: ValueRef, v: AttributeRef) =
  addAttributeAtIndex(f, cast[AttributeIndex](AttributeFunctionIndex), v)
