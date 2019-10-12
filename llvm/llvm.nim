# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016
# See the LICENSE file for license info (doh!)

import strutils

const
  LLVMLib = "libLLVM-9.so"
  LLVMRoot = "../ext/llvm-9.0.0.src/"
  LLDRoot = "../ext/lld-9.0.0.src/"
  LLVMVersion* = "9.0.0"

{.passL: "-llldDriver" .}
{.passL: "-llldELF" .}
{.passL: "-llldWasm" .}
{.passL: "-llldCore" .}
{.passL: "-llldCommon" .}

when defined(staticLLVM):
  const
    LLVMOut = LLVMRoot & "sta/"

  {.passL: gorge(LLVMRoot & "sta/bin/llvm-config --libs all").}

else:
  const
    LLVMOut = LLVMRoot & "sha/"

  {.passL: "-lLLVM-9".}
  {.passL: "-Wl,'-rpath=$ORIGIN/" & LLVMOut & "lib/'".}

{.passC: "-I" & LLVMRoot & "include".}
{.passC: "-I" & LLVMOut & "include".}

{.passC: "-I" & LLDRoot & "include".}

{.passL: "-Wl,--as-needed".}
{.passL: gorge(LLVMOut & "bin/llvm-config --ldflags").}
{.passL: gorge(LLVMOut & "bin/llvm-config --system-libs").}

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
  OpaqueJITEventListener{.pure, final.} = object
  OpaqueNamedMDNode{.pure, final.} = object
  opaqueValueMetadataEntry{.pure, final.} = object
  comdat{.pure, final.} = object
  opaqueModuleFlagEntry{.pure, final.} = object
  OpaqueBinary{.pure, final.} = object

  # Funny type names that came out of c2nim
  int64T = int64
  uint64T = uint64
  uint8T = uint8

  OpaqueTargetData{.pure, final.} = object
  OpaqueTargetLibraryInfotData{.pure, final.} = object

include llvm/Types
include llvm/Support

include llvm/Core
include llvm/DebugInfo
include llvm/BitReader
include llvm/BitWriter
include llvm/IRReader
include llvm/Linker
include llvm/Target
include llvm/TargetMachine
include llvm/Transforms/PassManagerBuilder

include preprocessed

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

template oaAddr(v: openArray): untyped =
  if v.len > 0: v[0].unsafeAddr else: nil
template oaLen(v: openArray): cuint = v.len.uint32

proc nimAddModuleFlag*(m: ModuleRef, name: cstring, value: uint32) {.importc: "LLVMNimAddModuleFlag".}

proc dIBuilderCreateSubroutineType*(
  d: DIBuilderRef, file: MetadataRef, parameterTypes: openArray[MetadataRef]): MetadataRef =
  dIBuilderCreateSubroutineType(d, nil, parameterTypes.oaAddr,
  parameterTypes.oaLen, DIFlagZero)
proc dIBuilderCreateFunction*(d: DIBuilderRef, scope: MetadataRef,
  name: string, linkageName: string, file: MetadataRef, lineNo: cuint,
  ty: MetadataRef, isLocalToUnit: bool, isDefinition: bool, scopeLine: cuint,
  flags: cuint, isOptimized: bool): MetadataRef =
  dIBuilderCreateFunction(d, scope, name.cstring, name.len, linkageName.cstring,
  linkageName.len, file, lineNo, ty, isLocalToUnit.Bool, isDefinition.Bool,
  scopeLine, flags.DIFlags, isOptimized.Bool)
proc dIBuilderCreateBasicType*(
  d: DIBuilderRef, name: string, bits: uint64,
  encoding: cuint, flags: DIFlags = DIFlagZero): MetadataRef =
  dIBuilderCreateBasicType(d, name.cstring, name.len, bits, encoding, flags)
proc dIBuilderCreatePointerType*(
  d: DIBuilderRef, pointeeTy: MetadataRef, bits: uint64, align: uint32,
  name: string): MetadataRef =
  dIBuilderCreatePointerType(d, pointeeTy, bits, align, 0, name.cstring, name.len)
proc dIBuilderCreateStructType*(
  d: DIBuilderRef, scope: MetadataRef, name: string,
  file: MetadataRef, lineNumber: cuint, sizeBits: uint64,
  alignBits: uint32, flags: cuint, derivedFrom: MetadataRef,
  elements: openArray[MetadataRef], runtimeLang: cuint, vtableHolder: MetadataRef,
  uniqueId: string): MetadataRef =
  dIBuilderCreateStructType(d, scope, name.cstring, name.len, file, lineNumber,
  sizeBits, alignBits, flags.DIFlags, derivedFrom, elements.oaAddr, elements.oaLen,
  runtimeLang, vtableHolder, uniqueId.cstring, uniqueId.len)
proc dIBuilderCreateMemberType*(
  d: DIBuilderRef, scope: MetadataRef, name: string,
  file: MetadataRef, lineNo: cuint, sizeBits: uint64, alignBits: uint32,
  offsetBits: uint64, flags: cuint,
  ty: MetadataRef): MetadataRef =
  dIBuilderCreateMemberType(d, scope, name.cstring, name.len, file, lineNo,
  sizeBits, alignBits, offsetBits, flags.DIFlags, ty)
proc dIBuilderCreateGlobalVariableExpression*(
  d: DIBuilderRef, context: MetadataRef, name: string,
  linkageName: string, file: MetadataRef, lineNo: cuint, ty: MetadataRef,
  isLocalToUnit: bool, exp: MetadataRef, decl: MetadataRef,
  alignBits: uint32): MetadataRef =
  dIBuilderCreateGlobalVariableExpression(d, context, name.cstring, name.len,
  linkageName.cstring, linkageName.len, file, lineNo, ty, isLocalToUnit.Bool,
  exp, decl, alignBits)

proc dIBuilderCreateAutoVariable*(
  d: DIBuilderRef, scope: MetadataRef, name: string,
  file: MetadataRef, lineNo: cuint, ty: MetadataRef, alwaysPreserve: bool,
  flags: cuint, alignBits: uint32): MetadataRef =
  dIBuilderCreateAutoVariable(d, scope, name.cstring, name.len, file, lineNo,
  ty, alwaysPreserve.Bool, flags.DIFlags, alignBits)
proc dIBuilderCreateParameterVariable*(
  d: DIBuilderRef, scope: MetadataRef, name: string, argNo: cuint,
  file: MetadataRef, lineNo: cuint, ty: MetadataRef, alwaysPreserve: bool,
  flags: cuint): MetadataRef =
  dIBuilderCreateParameterVariable(d, scope, name.cstring, name.len, argNo,
  file, lineNo, ty, alwaysPreserve.Bool, flags.DIFlags)
proc dIBuilderCreateArrayType*(
  d: DIBuilderRef, size: uint64, alignBits: uint32, ty: MetadataRef,
  subscripts: openArray[MetadataRef]): MetadataRef =
  dIBuilderCreateArrayType(d, size, alignBits, ty, subscripts.oaAddr, subscripts.oaLen)
proc dIBuilderGetOrCreateArray*(d: DIBuilderRef, p: openArray[MetadataRef]): MetadataRef =
  dIBuilderGetOrCreateArray(d, p.oaAddr, p.oaLen.csize)
proc nimDICompositeTypeSetTypeArray*(
  d: DIBuilderRef, compositeTy: MetadataRef,
  tyArray: MetadataRef) {.importc: "LLVMNimDICompositeTypeSetTypeArray".}
proc addModuleFlag*(
  m: ModuleRef, behavior: ModuleFlagBehavior, key: string, val: MetadataRef) =
  addModuleFlag(m, behavior, key.cstring, key.len, val)
proc nimSetMetadataGlobal*(
  val: ValueRef; kindID: cuint; node: ValueRef) {.importc: "LLVMNimSetMetadataGlobal".}

proc nimLLDLinkElf*(
  argv: cstringArray, argc: cint): cstring {.importc: "LLVMNimLLDLinkElf".}

proc nimLLDLinkElf*(args: openArray[string]): string =
  let argv = allocCStringArray(args)
  defer: deallocCStringArray(argv)

  let tmp = nimLLDLinkElf(argv, args.len.cint)
  if not tmp.isNil:
    result = strip($tmp)
    disposeMessage(tmp)

proc nimLLDLinkWasm*(
  argv: cstringArray, argc: cint): cstring {.importc: "LLVMNimLLDLinkWasm".}

proc nimLLDLinkWasm*(args: openArray[string]): string =
  let argv = allocCStringArray(args)
  defer: deallocCStringArray(argv)

  let tmp = nimLLDLinkWasm(argv, args.len.cint)
  if not tmp.isNil:
    result = strip($tmp)
    disposeMessage(tmp)

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

template checkErr(body: untyped) =
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
template asRaw(arr: untyped, body: untyped): untyped =
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

proc structTypeInContext*(c: ContextRef, elementTypes: openarray[TypeRef],
                          packed = False): TypeRef =
  asRaw(elementTypes, structTypeInContext(c, p, n, packed))

proc structSetBody*(structTy: TypeRef; elementTypes: openarray[TypeRef];
                    packed = False) =
  asRaw(elementTypes, structSetBody(structTy, p, n, packed))

proc getStructElementTypes*(structTy: TypeRef): seq[TypeRef] =
  result = newSeq[TypeRef](structTy.countStructElementTypes())
  if result.len > 0:
    structTy.getStructElementTypes(addr(result[0]))

proc pointerType*(elementType: TypeRef): TypeRef =
  pointerType(elementType, 0)

proc constStringInContext*(c: ContextRef, s: string, dontNullTerminate = False): ValueRef =
  constStringInContext(c, s, s.len.cuint, dontNullTerminate)

proc constStructInContext*(c: ContextRef, constantVals: openarray[ValueRef]; packed = False): ValueRef =
  asRaw(constantVals, constStructInContext(c, p, n, packed))

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

proc buildInvoke*(a1: BuilderRef; fn: ValueRef;
    args: openArray[ValueRef]; then: BasicBlockRef; catch: BasicBlockRef;
    name: cstring = ""): ValueRef =
  asRaw(args, buildInvoke(a1, fn, p, n, then, catch, name))

proc diBuilderCreateFile*(builder: DIBuilderRef, filename: string, directory: string): MetadataRef =
  diBuilderCreateFile(builder, filename.cstring, filename.len.csize,
    directory.cstring, directory.len.csize)

template getEnumAttrKind(x: untyped): untyped = getEnumAttributeKindForName(x, x.len)

let
  attrNoReturn* = getEnumAttrKind("noreturn")
  attrNoInline* = getEnumAttrKind("noinline")
  attrCold* = getEnumAttrKind("cold")

proc addFuncAttribute*(f: ValueRef, v: AttributeRef) =
  addAttributeAtIndex(f, cast[AttributeIndex](AttributeFunctionIndex), v)

proc getMDKindIDInContext*(c: ContextRef, name: string): cuint =
  getMDKindIDInContext(c, name.cstring, name.len.cuint)

proc createStringAttribute*(c: ContextRef, k, v: string): AttributeRef =
  createStringAttribute(c, k.cstring, k.len.cuint, v.cstring, v.len.cuint)


proc appendBasicBlockInContext*(
    b: BuilderRef, c: ContextRef, name: cstring): BasicBlockRef =
  let
    pre = b.getInsertBlock()
    f = pre.getBasicBlockParent()
  appendBasicBlockInContext(c, f, name)
