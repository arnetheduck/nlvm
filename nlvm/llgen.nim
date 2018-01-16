# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016
# See the LICENSE file for license info (doh!)

import
  math,
  os,
  strutils,
  sequtils,
  tables

import
  compiler/ast,
  compiler/astalgo,
  compiler/bitsets,
  compiler/cgmeth,
  compiler/ccgutils,
  compiler/extccomp,
  compiler/idents,
  compiler/lowerings,
  compiler/magicsys,
  compiler/modulegraphs,
  compiler/msgs,
  compiler/nimsets,
  compiler/options,
  compiler/passes,
  compiler/platform,
  compiler/rodread,
  compiler/ropes,
  compiler/semparallel,
  compiler/sighashes,
  compiler/trees,
  compiler/types,
  compiler/wordrecg

import llvm/llvm

var
  llctxt = llvm.getGlobalContext()
  llBoolType = llvm.int8Type()  # needs 8 bits for atomics to work...
  llCharType = llvm.int8Type()
  llCIntType = llvm.int32Type()  # c int on linux
  llCSizeTType = llvm.int64Type()  # c size_t on linux
  llIntType = llvm.int64Type()  # TODO == sizeof(void*)
  llNumTypes: array[tyInt..tyUInt64, llvm.TypeRef] = [
      llIntType,
      llvm.int8Type(),
      llvm.int16Type(),
      llvm.int32Type(),
      llvm.int64Type(),
      llvm.doubleType(),
      llvm.floatType(),
      llvm.doubleType(),
      llvm.fP128Type(),
      llIntType,
      llvm.int8Type(),
      llvm.int16Type(),
      llvm.int32Type(),
      llvm.int64Type(),
  ]
  llVoidPtrType = llvm.int8Type().pointerType()  # no void* in llvm
  llCStringType = llvm.int8Type().pointerType()
  llGenericSeqType = llvm.structCreateNamed(llctxt, "TGenericSeq")
  llNimStringDesc = llvm.structCreateNamed(llctxt, "NimStringDesc")
  llProcPtrType = llVoidPtrType
  llClosureType = llvm.structCreateNamed(llctxt, "llnim.Closure")
  llNimStringDescPtr = llNimStringDesc.pointerType()
  llInitFuncType = llvm.functionType(llvm.voidType(), [])
  llPrintfType = llvm.functionType(llCIntType, [llCStringType], true)
  llStrlenType = llvm.functionType(llCSizeTType, [llCStringType])
  llJmpbuf = llvm.structCreateNamed(llctxt, "jmp_buf")
  llJmpbufp = llJmpbuf.pointerType()
  llSetjmpType = llvm.functionType(llCIntType, [llJmpbufp])
  llFabsType = llvm.functionType(llvm.doubleType(), [llvm.doubleType()])
  llMemsetType = llvm.functionType(llvm.voidType(), [llVoidPtrType, llvm.int8Type(), llvm.int64Type(), llvm.int32Type(), llvm.int1Type()])
  llMemcpyType = llvm.functionType(llvm.voidType(), [llVoidPtrType, llVoidPtrType, llvm.int64Type(), llvm.int32Type(), llvm.int1Type()])
  llErrnoType = llvm.functionType(llCIntType.pointerType(), [])
  llAttrNoInline = llvm.getGlobalContext().createEnumAttribute(llvm.attrNoInline, 0)
  llAttrNoReturn = llvm.getGlobalContext().createEnumAttribute(llvm.attrNoReturn, 0)

llvm.structSetBody(llJmpbuf, [llvm.arrayType(llvm.int64Type(), 8), llvm.int32Type(), llvm.arrayType(llvm.int64Type(), 16)])
llvm.structSetBody(llGenericSeqType, [llIntType, llIntType])
llvm.structSetBody(llNimStringDesc, [llGenericSeqType, llvm.arrayType(llvm.int8Type(), 0)])
llvm.structSetBody(llClosureType, [llVoidPtrType, llVoidPtrType])

type LLScope = ref object
  n: PNode
  exit: llvm.BasicBlockRef
  goto: llvm.ValueRef
  nestedTryStmts: int
  nestedExceptStmts: int

type LLFunc = ref object
  vars: Table[int, llvm.ValueRef]
  scope: seq[LLScope]
  ret: llvm.BasicBlockRef
  options: TOptions
  nestedTryStmts: seq[PNode]
  inExceptBlock: int
  finallySafePoints: seq[llvm.ValueRef]
  loopNesting: int
  init: llvm.BasicBlockRef

  ds: llvm.NimMetadataRef

type LLGenObj = object of TPassContext
  m: llvm.ModuleRef
  b: llvm.BuilderRef
  d: llvm.NimDIBuilderRef
  f: LLFunc

  values: Table[int, llvm.ValueRef]
  gmarkers: Table[int, llvm.ValueRef]
  markers: Table[SigHash, llvm.ValueRef]
  nodeInfos: Table[SigHash, llvm.ValueRef]
  typeInfos: Table[SigHash, llvm.ValueRef]
  types: Table[SigHash, llvm.TypeRef]

  depth: int

  init: LLFunc

  syms: seq[PSym]

  markerBody: seq[tuple[v: llvm.ValueRef, typ: PType]]  # Markers looking for a body

  sigConflicts: CountTable[SigHash]

  # Debug stuff
  dfiles: Table[int, llvm.NimMetadataRef]
  dscopes: Table[int, llvm.NimMetadataRef]
  dtypes: array[TTypeKind, llvm.NimMetadataRef]
  dstructs: Table[SigHash, llvm.NimMetadataRef]

  # Compile unit
  dcu: llvm.NimMetadataRef

type LLGen = ref LLGenObj

# Using an ugly global - haven't found a way to keep per-project data other than
# this...
var uglyGen: LLGen

# Helpers
proc newLLFunc(ret: llvm.BasicBlockRef): LLFunc
proc llType(g: LLGen, typ: PType): llvm.TypeRef
proc fieldIndex(typ: PType, sym: PSym): seq[int]
proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef)
proc callErrno(g: LLGen, prefix: string): llvm.ValueRef
proc callCompilerProc(g: LLGen, name: string, args: openarray[llvm.ValueRef]): llvm.ValueRef

proc genFunction(g: LLGen, s: PSym): llvm.ValueRef
proc genFunctionWithBody(g: LLGen, s: PSym): llvm.ValueRef

# Magic expressions
proc genMagicLength(g: LLGen, n: PNode): llvm.ValueRef

# Node handling
proc genNode(g: LLGen, n: PNode, load: bool = false): llvm.ValueRef {.discardable.}

proc nn(s: string, li: TLineInfo): string =
  # later on, we'll just return an empty string here
  let fn =
    if li.fileIndex in {0..fileInfos.high}: "." & fileInfos[li.fileIndex].shortName
    else: ""

  let ln = if li.line >= 0: "." & $li.line else: ""
  let col = if li.col >= 0: "." & $li.col else: ""

  result = s & fn & ln & col

proc nn(s: string): string = s
proc nn(s: string, n: PNode): string =
  if n == nil: nn(s) else: nn(s, n.info)
proc nn(s: string, sym: PSym): string =
  if s == nil: nn(s) else: nn(s, sym.info)
proc nn(s: string, v: llvm.ValueRef): string =
  var vn = v.getValueName()
  if vn == nil: vn = ""
  result = s & "." & $vn

proc scopePush(f: LLFunc, n: PNode, exit: llvm.BasicBlockRef) =
  f.scope.add(
    LLScope(n: n, exit: exit, nestedTryStmts: f.nestedTryStmts.len,
      nestedExceptStmts: f.inExceptBlock))

proc scopeIdx(f: LLFunc, sym: PSym): int =
  for i in 0..f.scope.len - 1:
    let scope = f.scope[f.scope.len - 1 - i]
    if scope.n != nil and scope.n.kind == nkSym and scope.n.sym == sym:
      return f.scope.len - 1 - i

  return -1

proc scopePop(f: LLFunc): LLScope {.discardable.} =
  f.scope.pop

proc scopeGet(f: LLFunc, id: int): llvm.ValueRef =
  if f.vars.hasKey(id): return f.vars[id]

  internalError("Symbol missing from scope: " & $id & ", " & $f.vars)

proc scopePut(f: LLFunc, id: int, value: llvm.ValueRef) =
  f.vars[id] = value

proc getInitBlock(llf: LLFunc): llvm.BasicBlockRef =
  if llf.init == nil:
    let pre = llf.ret.getBasicBlockParent()
    llf.init = pre.appendBasicBlock(nn("init"))
  result = llf.init

proc positionAndMoveToEnd(b: llvm.BuilderRef, bl: llvm.BasicBlockRef) =
  bl.moveBasicBlockAfter(b.getInsertBlock().getBasicBlockParent().getLastBasicBlock())
  b.positionBuilderAtEnd(bl)

proc localAlloca(b: llvm.BuilderRef, typ: llvm.TypeRef, name: string): llvm.ValueRef =
  # alloca will allocate memory on the stack that will be released at function
  # exit - thus for a variable local to a loop we would actually be creating a
  # new location for every iteration. Thus, we make space for all locals at
  # the start of the function.
  # This also helps with another degenerate case:
  # in a moment of truimph, code like `if a and (let m = 1; m != 1): ...` was
  # allowed, creating a problematic scope for m - it should ideally only exist
  # when a is true, but by putting it at the entry, it lives through the
  # the expression (with an undef value until the right branch is taken)

  let pre = b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  var entry = f.getEntryBasicBlock()

  var first = entry.getFirstInstruction()
  if first == nil or first.getInstructionOpcode() != llvm.Alloca:
    # Allocate a specific alloca block and place it first in the function
    let newEntry = entry.insertBasicBlock(nn("alloca"))
    b.positionBuilderAtEnd(newEntry)
    let br = b.buildBr(entry)
    entry = newEntry
    b.positionBuilderBefore(br)
  else:
    while first.getInstructionOpcode() == llvm.Alloca:
      first = first.getNextInstruction()
    b.positionBuilderBefore(first)

  result = b.buildAlloca(typ, name)
  b.positionBuilderAtEnd(pre)

proc addPrivateConstant*(m: ModuleRef, ty: llvm.TypeRef, name: cstring): llvm.ValueRef =
  result = m.addGlobal(ty, name)
  result.setGlobalConstant(llvm.True)
  # TODO when enabled, ptr equality checks (for example in isObj) get
  #      optimized away - need to consider when it's safe
  # result.setUnnamedAddr(llvm.True)
  result.setLinkage(llvm.PrivateLinkage)

proc idOrSig(g: LLGen; s: PSym): Rope =
  # from ccgtypes.nim
  if s.kind in routineKinds and s.typ != nil:
    let sig = hashProc(s)
    result = rope($sig)
    let counter = g.sigConflicts.getOrDefault(sig)
    if counter != 0:
      result.add "_" & rope(counter+1)
    if s.typ.callConv == ccInline:
      result.add rope(g.syms[^1].name.s)
    g.sigConflicts.inc(sig)
  else:
    let sig = hashNonProc(s)
    result = rope($sig)
    let counter = g.sigConflicts.getOrDefault(sig)
    if counter != 0:
      result.add "_" & rope(counter+1)
    g.sigConflicts.inc(sig)

# from c gen
proc fillLoc(a: var TLoc, k: TLocKind, lode: PNode, r: Rope, s: TStorageLoc) =
  # fills the loc if it is not already initialized
  if a.k == locNone:
    a.k = k
    a.lode = lode
    a.storage = s
    if a.r == nil: a.r = r

proc mangleName(g: LLGen; s: PSym): Rope =
  result = s.loc.r
  if result == nil:
    result = s.name.s.mangle.rope
    add(result, g.idOrSig(s))
    s.loc.r = result

proc llName(s: PSym): string =
  result = $s.loc.r

  # NCSTRING is a char*, so functions that return const char* need this ugly
  # cast in their importc to be rid of the const.. *sigh*
  if result.startsWith("(char*)"):
    result = result[7..^1]
  elif result.startsWith("(char *)"):
   result = result[8..^1]

const
  irrelevantForBackend = {tyGenericBody, tyGenericInst, tyGenericInvocation,
                          tyDistinct, tyRange, tyStatic, tyAlias, tyInferred}

proc llName(typ: PType, sig: SigHash): string =
  # getTypeName from ccgtypes.nim
  var t = typ
  while true:
    if t.sym != nil and {sfImportc, sfExportc} * t.sym.flags != {}:
      return $t.sym.loc.r

    if t.kind in irrelevantForBackend:
      t = t.lastSon
    else:
      break
  let typ = if typ.kind == tyAlias: typ.lastSon else: typ
  if typ.loc.r == nil:
    let typ = typ.skipTypes(irrelevantForBackend)

    let tn = if typ.sym != nil and typ.kind in {tyObject, tyEnum}:
               ~typ.sym.name.s.mangle
             else:
               ~"TY"
    typ.loc.r = tn & $sig
  result = $typ.loc.r

iterator procParams(typ: PType): PNode =
  for a in typ.n.sons[1..^1]:
    let param = a.sym
    if isCompileTimeOnly(param.typ): continue
    yield a

proc procParams(typ: PType): seq[PNode] =
  accumulateResult(procParams(typ))

proc `$`(t: PType): string

proc `$`(n: PSym): string =
  if n == nil: return "PSym(nil)"
  let name = if n.loc.r == nil: n.name.s else: $n.loc.r
  result = name & " " & $n.id & " " & $n.kind & " " & $n.magic & " " & $n.flags & " " &
    $n.loc.flags & " " & $n.info.line & " " & $n.typ

proc `$`(t: PType): string =
  if t == nil: return "PType(nil)"
  result = $t.kind & " " & $t.flags & " "& $t.id & " "
  if t.sons != nil:
    result &= $t.sonsLen
  else:
    result &= "nil"
  result &= " "

  if t.sym != nil:
    result &= t.sym.name.s
  else:
    result &= "nil"

  if t.kind == tyProc:
    result &= " " & $t.callConv

  if t.n != nil:
    result &= " " & $t.n.kind

proc `$`(n: PNode): string =
  if n == nil: return "PNode(nil)"
  result = $n.kind & " "
  case n.kind
  of nkCharLit..nkUInt64Lit: result &= $n.typ.kind & " " & $n.intVal
  of nkFloatLit..nkFloat128Lit: result &= $n.typ.kind & " " & $n.floatVal
  of nkStrLit..nkTripleStrLit: result &= (if n.strVal == nil: "" else: n.strVal)
  of nkSym: result &= $n.sym
  of nkIdent: result &= n.ident.s
  of nkProcDef:
    let s = n[namePos].sym
    result &= $s & " " & $n.sonsLen
  else: result &= $n.flags & " " & $n.sonslen

proc p(t: string, n: PNode, depth: int) =
  if gVerbosity == 2:
    echo(spaces(depth * 2), t, " ", n)

proc p(t: string, n: PType, depth: int) =
  if gVerbosity == 2:
    echo(spaces(depth * 2), t, " ", n)

proc p(t: string, n: PSym, depth: int) =
  if gVerbosity == 2:
    echo(spaces(depth * 2), t, " ", n)

proc constInt1(v: bool): llvm.ValueRef =
  llvm.constInt(llvm.int1Type(), v.culonglong, llvm.False)

proc constNimBool(v: bool): llvm.ValueRef =
  llvm.constInt(llBoolType, v.culonglong, llvm.False)

proc constInt8(v: int8): ValueRef =
  llvm.constInt(llvm.int8Type(), v.culonglong, llvm.False)

proc constInt32(v: int32): ValueRef =
  llvm.constInt(llvm.int32Type(), v.culonglong, llvm.False)

proc constInt64(v: int64): ValueRef =
  llvm.constInt(int64Type(), v.culonglong, llvm.False)

proc constCInt(val: int): llvm.ValueRef =
  llvm.constInt(llCIntType, val.culonglong, llvm.True)

proc constNimInt(val: int): llvm.ValueRef =
  llvm.constInt(llIntType, val.culonglong, llvm.True)

proc isArrayPtr(t: llvm.TypeRef): bool =
  t.getTypeKind() == llvm.PointerTypeKind and
    t.getElementType().getTypeKind() == llvm.ArrayTypeKind

# Int constant suitable for indexing into structs when doing GEP
# Has to be i32 per LLVM docs
proc constGEPIdx(val: int): llvm.ValueRef =
  constInt32(val.int32)

let
  gep0 = constGEPIdx(0)
  gep1 = constGEPIdx(1)
  ni0 = constNimInt(0)

proc constOffsetOf(g: LLGen, t: PType, sym: PSym): llvm.ValueRef =
  let
    typ = t.skipTypes(abstractPtrs)
    llt = g.llType(typ)
    ind = fieldIndex(t, sym)
    gep = constGEP(constNull(llt.pointerType()), (@[0] & ind).map(constGEPIdx))
  result = constPtrToInt(gep, llIntType)

proc bitSetToWord(s: TBitSet, size: int): int64 =
  result = 0
  for j in countup(0, size - 1):
    if j < len(s): result = result or `shl`(ze64(s[j]), j * 8)

proc constNimSet(n: PNode): llvm.ValueRef =
  assert n.kind == nkCurly
  assert nfAllConst in n.flags
  let
    typ = skipTypes(n.typ, abstractVar)
    size = getSize(typ)

  var cs: TBitSet
  toBitSet(n, cs)
  if size <= 8:
    result = llvm.constInt(
      llvm.intType(size.cuint * 8), cs.bitSetToWord(size.int).culonglong,
      llvm.False)
  else:
    result = llvm.constArray(int8Type(), cs.map(constInt8))

proc constNimString(n: PNode): llvm.ValueRef =
  let s = llvm.constString(n.strVal)
  let ll = constNimInt(n.strVal.len)
  let x = llvm.constNamedStruct(llGenericSeqType, [ll, ll])
  result = llvm.constStruct([x, s])

proc buildNimSeqLenGEP(b: BuilderRef, s: llvm.ValueRef): llvm.ValueRef =
  b.buildGEP(s, [gep0, gep0, gep0], nn("seq.len", s))

proc buildNimSeqReservedGEP(b: BuilderRef, s: llvm.ValueRef): llvm.ValueRef =
  b.buildGEP(s, [gep0, gep0, gep1], nn("seq.res", s))

proc buildNimSeqDataGEP(b: BuilderRef, s: llvm.ValueRef, idx: llvm.ValueRef = nil): llvm.ValueRef =
  let idx = if idx == nil: gep0 else: idx
  b.buildGEP(s, [gep0, gep1, idx], nn("seq.data", s))

proc buildI1(b: llvm.BuilderRef, v: llvm.ValueRef): llvm.ValueRef =
  b.buildTrunc(v, int1Type(), nn("bool.i1", v))

proc buildI8(b: llvm.BuilderRef, v: llvm.ValueRef): llvm.ValueRef =
  b.buildZExt(v, int8Type(), nn("bool.i8", v))

proc isUnsigned(typ: PType): bool =
  let typ = typ.skipTypes(abstractVarRange)

  result = typ.kind in {tyUInt..tyUInt64, tyBool, tyChar, tySet} or
    (typ.kind == tyEnum and firstOrd(typ) >= 0)

proc buildNimIntExt(b: BuilderRef, v: llvm.ValueRef, unsigned: bool): llvm.ValueRef =
  let nt = llIntType
  result =
    if unsigned: b.buildZExt(v, nt, nn("nie.z", v))
    else: b.buildSExt(v, nt, nn("nie.s", v))

proc buildTruncOrExt(b: llvm.BuilderRef, v: llvm.ValueRef, nt: llvm.TypeRef,
                     unsigned: bool): llvm.ValueRef =
  let
    vt = v.typeOf()
    vtk = vt.getTypeKind()
    ntk = nt.getTypeKind()

  if vtk != llvm.IntegerTypeKind or ntk != llvm.IntegerTypeKind:
    result = v
    return

  let
    vw = vt.getIntTypeWidth()
    nw = nt.getIntTypeWidth()
  if vw == nw:
    result = v
  elif vw > nw:
    result = b.buildTrunc(v, nt, nn("toe.t", v))
  else:  # vw < nw
    result =
      if unsigned: b.buildZExt(v, nt, nn("toe.z", v))
      else: b.buildSExt(v, nt, nn("toe.s", v))

proc buildTruncOrExt(b: llvm.BuilderRef, v: llvm.ValueRef, nt: llvm.TypeRef,
                     typ: PType): llvm.ValueRef =
  b.buildTruncOrExt(v, nt, typ.isUnsigned())

proc needsBr(b: llvm.BuilderRef): bool =
  b.getInsertBlock().getBasicBlockTerminator() == nil

proc buildBrFallthrough(b: llvm.BuilderRef, next: llvm.BasicBlockRef) =
  # Add a br to the next block if the current block is not already terminated
  if b.needsBr():
    discard b.buildBr(next)

proc buildSetMask(b: llvm.BuilderRef, t: llvm.TypeRef, ix: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let mask =
    case size
    of 1: 7
    of 2: 15
    of 4: 31
    of 8: 63
    else: 7
  var shift = b.buildAnd(
    ix, llvm.constInt(ix.typeOf(), mask.culonglong, llvm.False),
    nn("set.mask", ix))
  if t != shift.typeOf():
    shift = b.buildTruncOrExt(shift, t, true)
  result = b.buildShl(llvm.constInt(t, 1, llvm.False), shift, nn("set.pos", ix))

proc buildSetGEPMask(b: llvm.BuilderRef, vx, ix: llvm.ValueRef): tuple[gep, mask: llvm.ValueRef] =
  let idx = b.buildUDiv(ix, llvm.constInt(ix.typeOf(), 8, llvm.False), nn("set.byte", ix))
  let gep = b.buildGEP(vx, [gep0, idx], nn("set.gep", vx))

  let mask = b.buildSetMask(llvm.int8Type(), ix, 1)
  result = (gep: gep, mask: mask)

proc buildLoadValue(b: BuilderRef, v: llvm.ValueRef): llvm.ValueRef =
  if v.typeOf().isArrayPtr():
    result = b.buildGEP(v, [gep0, gep0], nn("load", v))
  else:
    result = b.buildLoad(v, nn("load", v))

proc buildNot(b: BuilderRef, v: llvm.ValueRef): llvm.ValueRef =
  result = b.buildICmp(llvm.IntEQ, v, llvm.constInt(v.typeOf(), 0, llvm.False), nn("not", v))
  result = b.buildZExt(result, v.typeOf(), nn("zext", result))

proc buildBitnot(b: BuilderRef, v: llvm.ValueRef): llvm.ValueRef =
  b.buildXor(v, llvm.constInt(v.typeOf(), (-1).culonglong, llvm.False), nn("bitnot", v))

proc buildStoreNull(g: LLGen, v: llvm.ValueRef) =
  let t = v.typeOf()
  assert t.getTypeKind() == llvm.PointerTypeKind
  let et = t.getElementType()
  if et.getTypeKind() in {llvm.ArrayTypeKind, llvm.StructTypeKind}:
    g.callMemset(v, constInt8(0), et.sizeOfX())
  else:
    discard g.b.buildStore(constNull(et), v)

proc debugGetFile(g: LLGen, idx: int32): llvm.NimMetadataRef =
  if idx in g.dfiles: return g.dfiles[idx]

  let path = idx.toFullPath
  let (dir, fn) = path.splitPath()
  result = g.d.nimDIBuilderCreateFile(fn, dir)

  g.dfiles[idx] = result

proc debugStructType(g: LLGen, typ: PType): llvm.NimMetadataRef
proc debugTupleType(g: LLGen, typ: PType): llvm.NimMetadataRef
proc debugMagicType(g: LLGen, name: string): llvm.NimMetadataRef

proc debugType(g: LLGen, typ: PType): llvm.NimMetadataRef =
  if g.d == nil: return nil

  case typ.kind
  of tyBool: result = g.dtypes[tyBool]
  of tyChar: result = g.dtypes[tyChar]
  of tyNil: result = nil
  of tyGenericInst: result = g.debugType(typ.lastSon)
  of tyDistinct, tyAlias, tyInferred: result = g.debugType(typ.lastSon)
  of tyEnum:
    let bits = getSize(typ).cuint * 8
    result = g.d.nimDIBuilderCreateBasicType("enum", bits, DW_ATE_unsigned)
  of tyArray:
    let et = g.debugType(typ.elemType)
    let (s, c) = if tfUncheckedArray in typ.flags: (cuint(0), -1.int64)
                 else: (cuint(typ.lengthOrd()), int64(typ.lengthOrd()))
    result = g.d.nimDIBuilderCreateArrayType(s * 8, 0, et,
               g.d.nimDIBuilderGetOrCreateArray(
                [g.d.nimDIBuilderCreateSubrange(0, c)]))
  of tyObject: result = g.debugStructType(typ)
  of tyTuple: result = g.debugTupleType(typ)
  of tySet:
    let size = getSize(typ).cuint
    let bits = size * 8
    result = if size <= 8: g.d.nimDIBuilderCreateBasicType("set", bits, DW_ATE_unsigned)
             else: g.d.nimDIBuilderCreateArrayType(
              size * 8, 0, g.dtypes[tyUInt8], g.d.nimDIBuilderGetOrCreateArray(
                [g.d.nimDIBuilderCreateSubrange(0, size.int64)]))
  of tyRange: result = g.debugType(typ.sons[0])
  of tyPtr, tyRef, tyVar:
    result = g.d.nimDIBuilderCreatePointerType(
      g.debugType(typ.lastSon), 64, 64, "")
  of tySequence:
    var st: llvm.NimMetadataRef

    let sig = hashType(typ)

    if sig in g.dstructs:
      st = g.dstructs[sig]
    else:
      let name = typ.llName(sig)
      let file = g.debugGetFile(gProjectMainIdx)

      st = g.d.nimDIBuilderCreateStructType(g.dcu, name,
          file, 0, 128, 0, 0, nil,
        g.d.nimDIBuilderGetOrCreateArray([]), 0, nil, name)
      g.dstructs[sig] = st

      var elems = @[
        g.d.nimDIBuilderCreateMemberType(g.dcu, "len", file, 0, 64, 64, 0, 0,
          g.dtypes[tyInt]),
        g.d.nimDIBuilderCreateMemberType(g.dcu, "cap", file, 0, 64, 64, 64, 0,
          g.dtypes[tyInt])
      ]
      if typ.elemType.kind != tyEmpty:
        elems.add(
          g.d.nimDIBuilderCreateMemberType(g.dcu, "data", file, 0, 0, 0, 128, 0,
            g.d.nimDIBuilderCreateArrayType(0, 0, g.debugType(typ.elemType),
              g.d.nimDIBuilderGetOrCreateArray([]))))

      g.d.nimDICompositeTypeSetTypeArray(
        st, g.d.nimDIBuilderGetOrCreateArray(elems))

    result = g.d.nimDIBuilderCreatePointerType(st, 8, 8, "")
  of tyProc:
    if typ.callConv == ccClosure:
      result = g.d.nimDIBuilderCreateStructType(g.dcu, "closure",
        g.debugGetFile(gProjectMainIdx), 0, 128, 0, 0, nil,
        g.d.nimDIBuilderGetOrCreateArray([]), 0, nil, "closure")
      g.d.nimDICompositeTypeSetTypeArray(
        result, g.d.nimDIBuilderGetOrCreateArray(
          [g.dtypes[tyPointer], g.dtypes[tyPointer]]))
    else:
      result = g.dtypes[tyPointer]
  of tyPointer: result = g.dtypes[tyPointer]
  of tyOpenArray, tyVarargs: result = g.debugType(typ.elemType)
  of tyString:
    if g.dtypes[tyString] == nil:
      g.dtypes[tyString] = g.d.nimDIBuilderCreatePointerType(
        g.debugMagicType("NimStringDesc"), 64, 64, "")
    result = g.dtypes[tyString]
  of tyCString: result = g.dtypes[tyCString]
  of tyInt..tyUInt64: result = g.dtypes[typ.kind]
  of tyTypeDesc: result = g.debugType(typ.lastSon)
  of tyStatic: result = g.debugType(typ.lastSon)
  of tyUserTypeClasses: result = g.debugType(typ.lastSon)
  else: internalError("Unhandled debug type " & $typ.kind)

proc debugFieldName(field: PSym, typ: PType): string =
  if (typ.sym != nil) and ({sfImportc, sfExportc} * typ.sym.flags != {}):
    result = $field.loc.r
  else:
    result = mangle(field.name.s)

proc align(address, alignment: int): int =
  result = (address + (alignment - 1)) and not (alignment - 1)

proc debugStructFields(
    g: LLGen, elements: var seq[NimMetadataRef], n: PNode, typ: PType,
    offset: var int) =
  case n.kind
  of nkRecList:
    for child in n:
      g.debugStructFields(elements, child, typ, offset)
  of nkRecCase: # TODO Unionize
    if n[0].kind != nkSym: internalError(n.info, "debugStructFields")
    g.debugStructFields(elements, n[0], typ, offset)

    for i in 1..<n.sonsLen:
      case n[i].kind
      of nkOfBranch, nkElse:
        g.debugStructFields(elements, n[i].lastSon, typ, offset)
      else: internalError(n.info, "debugStructFields")
  of nkSym:
    let field = n.sym
    if field.typ.kind == tyEmpty: return
    let size = getSize(field.typ) * 8
    let name = debugFieldName(field, typ)
    offset = align(offset, field.typ.align * 8)
    let member = g.d.nimDIBuilderCreateMemberType(
      g.dcu, name, g.debugGetFile(gProjectMainIdx), 0, size.uint64,
      0, offset.uint64, 0, g.debugType(field.typ))
    offset = offset + size.int
    elements.add(member)
  else: internalError(n.info, "debugStructFields")

proc debugStructType(g: LLGen, typ: PType): llvm.NimMetadataRef =
  if typ == nil:
    return

  var typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString: return g.debugType(typ)

  let sig = hashType(typ)
  if sig in g.dstructs:
    return g.dstructs[sig]

  let name = typ.llName(sig)

#  result = g.headerType(name)
#  if result != nil:
#    g.types[sig] = result
#    return

#  p("llStructType " & $typ, typ, g.depth)

  # Create struct before setting body in case it's recursive

  let file = g.debugGetFile(gProjectMainIdx)
  let size = getSize(typ) * 8
  result = g.d.nimDIBuilderCreateStructType(g.dcu, name,
    file, 0, size.uint64, 0, 0, nil,
    g.d.nimDIBuilderGetOrCreateArray([]), 0, nil, name)

  g.dstructs[sig] = result

  var elements = newSeq[NimMetadataRef]()

  var offset: int

  let super = if typ.sons == nil: nil else: typ.sons[0]
  if super == nil:
    if (typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags:
      discard
    else:
      let size = 8 * 8
      let name = "m_type"
      let tnt = g.debugMagicType("TNimType")
      let tntp = g.d.nimDIBuilderCreatePointerType(tnt, 64, 64, "")
      let member = g.d.nimDIBuilderCreateMemberType(
        g.dcu, name, g.debugGetFile(gProjectMainIdx), 0, size.uint64,
        0, offset.uint64, 0, tntp)
      offset = offset + size.int

      elements.add(member)
  else:
    let size = getSize(super) * 8
    let name = "super"
    let member = g.d.nimDIBuilderCreateMemberType(
      g.dcu, name, g.debugGetFile(gProjectMainIdx), 0, size.uint64,
      0, offset.uint64, 0, g.debugType(super))
    offset = offset + size.int

    elements.add(member)

  g.debugStructFields(elements, typ.n, typ, offset)

  g.d.nimDICompositeTypeSetTypeArray(
    result, g.d.nimDIBuilderGetOrCreateArray(elements))

proc debugTupleType(g: LLGen, typ: PType): llvm.NimMetadataRef =
  if typ == nil:
    return

  let sig = hashType(typ)
  if sig in g.dstructs:
    return g.dstructs[sig]

  let name = typ.llName(sig)

  # Create struct before setting body in case it's recursive
  let file = g.debugGetFile(gProjectMainIdx)
  let size = getSize(typ) * 8
  result = g.d.nimDIBuilderCreateStructType(g.dcu, name,
        file, 0, size.uint64, 0, 0, nil,
        g.d.nimDIBuilderGetOrCreateArray([]), 0, nil, name)

  g.dstructs[sig] = result

  var elements = newSeq[NimMetadataRef]()
  var offset: int
  for t in typ.sons:
    let size = getSize(t) * 8
    let member = g.d.nimDIBuilderCreateMemberType(
      g.dcu, "tup" & $offset, g.debugGetFile(gProjectMainIdx), 0, size.uint64,
      0, offset.uint64, 0, g.debugType(t))
    offset = offset + size.int
    elements.add(member)

  g.d.nimDICompositeTypeSetTypeArray(
    result, g.d.nimDIBuilderGetOrCreateArray(elements))

proc debugMagicType(g: LLGen, name: string): llvm.NimMetadataRef =
  g.debugType(magicsys.getCompilerProc(name).typ)

proc debugProcParamType(g: LLGen, t: PType): llvm.NimMetadataRef =
  let typ = t.skipTypes(abstractInst)
  case typ.kind:
  of tyArray, tyOpenArray, tyVarargs, tyObject, tyTuple:
    result = g.d.nimDIBuilderCreatePointerType(g.debugType(t), 64, 64, "")
  of tySet:
    let size = getSize(typ).cuint
    result = if size <= 8: g.debugType(t)
             else: g.d.nimDIBuilderCreatePointerType(g.debugType(t), 64, 64, "")
  of tyProc:
    result =
      if typ.callConv == ccClosure:
        g.d.nimDIBuilderCreatePointerType(g.debugType(t), 64, 64, "")
      else: g.debugType(t)
  of tyDistinct, tyAlias, tyInferred: result = g.debugProcParamType(t.lastSon)
  else: result = g.debugType(t)

proc debugProcType(g: LLGen, typ: PType, closure: bool): llvm.NimMetadataRef =
  var argTypes = newSeq[llvm.NimMetadataRef]()

  let retType = if typ.sons[0] == nil: nil
                else: g.debugType(typ.sons[0])
  argTypes.add(retType)

  for param in typ.procParams():
    let t = param.sym.typ.skipTypes({tyGenericInst})
    let at = g.debugProcParamType(t)
    argTypes.add(at)

    if skipTypes(t, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      argTypes.add(g.dtypes[tyInt])  # Extra length parameter

  if closure:
    argTypes.add(g.dtypes[tyPointer])

  result = g.d.nimDIBuilderGetOrCreateArray(argTypes)

proc debugGetScope(g: LLGen, sym: PSym): llvm.NimMetadataRef =
  var sym = sym
  while sym != nil and sym.kind notin {skProc, skModule}:
    sym = sym.owner

  if sym != nil and sym.id in g.dscopes: return g.dscopes[sym.id]

  if g.f != nil:
    return g.f.ds

  result = g.dcu

proc debugGetLocation(g: LLGen, sym: PSym, li: TLineInfo): llvm.ValueRef =
  let scope = g.debugGetScope(sym)

  result = llvm.getGlobalContext().nimDIBuilderCreateDebugLocation(
      li.line.cuint, li.col.cuint, scope, nil)

proc debugUpdateLoc(g: LLGen, n: PNode) =
  if g.d == nil: return
  if n == nil:
    g.b.setCurrentDebugLocation(nil)
    return

  let sym = if n.kind == nkSym: n.sym else: nil

  let dl = g.debugGetLocation(sym, n.info)
  g.b.setCurrentDebugLocation(dl)

proc debugVariable(g: LLGen, sym: PSym, v: llvm.ValueRef, argNo = -1) =
  if g.d == nil: return

  var dt = g.debugType(sym.typ)

  let scope = g.debugGetScope(sym)
  let vd = g.d.nimDIBuilderCreateVariable(
    if argNo == -1: 0x100 else: 0, scope, sym.llName,
    g.debugGetFile(sym.info.fileIndex),
    sym.info.line.cuint, dt, false, 0, argNo.cuint, 0)
  discard g.d.nimDIBuilderInsertDeclareAtEnd(v, vd, nil, 0,
    g.b.getCurrentDebugLocation(), g.b.getInsertBlock())

proc debugGlobal(g: LLGen, sym: PSym, v: llvm.ValueRef, argNo = -1) =
  if g.d == nil: return

  var dt = g.debugType(sym.typ)

  discard g.d.nimDIBuilderCreateStaticVariable(
    g.dcu, sym.llName, "",
    g.debugGetFile(sym.info.fileIndex),
    sym.info.line.cuint, dt, false, v, nil, 0)

proc debugFunction(
    g: LLGen, s: PSym, params: llvm.NimMetadataRef,
    f: llvm.ValueRef): llvm.NimMetadataRef =
  let st = g.d.nimDIBuilderCreateSubroutineType(params)
  let df = g.debugGetFile(if s == nil: gProjectMainIdx else: s.info.fileIndex)
  result = g.d.nimDIBuilderCreateFunction(
    g.dcu, f.getValueName(), "", df, 0, st, true, true, 0, 0, false,
    f, g.d.nimDIBuilderGetOrCreateArray([]), nil)

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef
proc llTupleType(g: LLGen, typ: PType): llvm.TypeRef
proc llProcType(g: LLGen, typ: PType, closure = false): llvm.TypeRef

proc llType(g: LLGen, typ: PType): llvm.TypeRef =
  case typ.kind
  of tyBool: result = llBoolType
  of tyChar: result = llCharType
  of tyNil: result = llVoidPtrType
  of tyGenericInst: result = g.llType(typ.lastSon)
  of tyDistinct, tyAlias, tyInferred: result = g.llType(typ.lastSon)
  of tyEnum: result = llvm.intType(getSize(typ).cuint * 8)
  of tyArray:
    let et = g.llType(typ.elemType)
    let n = if tfUncheckedArray in typ.flags: cuint(0) else: cuint(typ.lengthOrd())
    result = llvm.arrayType(et, n)
  of tyObject: result = g.llStructType(typ)
  of tyTuple: result = g.llTupleType(typ)
  of tySet:
    let size = getSize(typ).cuint
    result = if size <= 8: llvm.intType(size * 8)
             else: llvm.arrayType(llvm.int8Type(), size)
  of tyRange: result = g.llType(typ.sons[0])
  of tyPtr, tyRef, tyVar:
    let et = typ.elemType
    result = g.llType(et).pointerType()
  of tySequence:  # TODO generate from nim types
    var st: llvm.TypeRef

    let sig = hashType(typ)

    if sig in g.types:
      st = g.types[sig]
    else:
      let name = typ.llName(sig)
      st = structCreateNamed(llctxt, name)
      g.types[sig] = st

      if typ.elemType.kind == tyEmpty:
        st.structSetBody([llGenericSeqType])
      else:
        st.structSetBody([llGenericSeqType, llvm.arrayType(g.llType(typ.elemType), 0)])
    result = st.pointerType()
  of tyProc:
    if typ.callConv == ccClosure:
      result = llClosureType
    else:
      result = llProcPtrType

  of tyPointer: result = llVoidPtrType
  of tyOpenArray, tyVarargs: result = g.llType(typ.elemType)
  of tyString: result = llNimStringDescPtr
  of tyCString: result = llCStringType
  of tyInt..tyUInt64: result = llNumTypes[typ.kind]
  of tyTypeDesc: result = g.llType(typ.lastSon)
  of tyStatic: result = g.llType(typ.lastSon)
  of tyUserTypeClasses: result = g.llType(typ.lastSon)
  else:
    internalError("Unhandled type " & $typ.kind)

template withBlock(b: llvm.BuilderRef, bb: llvm.BasicBlockRef, body: untyped) =
  block:
    let pre = b.getInsertBlock()
    let db = b.getCurrentDebugLocation()
    b.positionBuilderAtEnd(bb)
    body
    b.positionBuilderAtEnd(pre)
    if optCDebug in gGlobalOptions:
      b.setCurrentDebugLocation(db)

proc finalize(b: llvm.BuilderRef, llf: LLFunc) =
  if llf.init == nil: return

  let pre = llf.ret.getBasicBlockParent()
  var entry = pre.getEntryBasicBlock()
  if entry.getFirstInstruction().getInstructionOpcode() == llvm.Alloca:
    entry.getLastInstruction().instructionEraseFromParent()
    b.withBlock(entry):
      discard b.buildBr(llf.init)
    entry = entry.getNextBasicBlock()

  llf.init.moveBasicBlockBefore(entry)

  b.withBlock(llf.init):
    discard b.buildBr(entry)

proc llMagicType(g: LLGen, name: string): llvm.TypeRef =
  g.llType(magicsys.getCompilerProc(name).typ)

proc addStructFields(g: LLGen, elements: var seq[TypeRef], n: PNode, typ: PType) =
  p("addStructFields", n, g.depth)
  case n.kind
  of nkRecList:
    for child in n:
      g.addStructFields(elements, child, typ)
  of nkRecCase: # TODO Unionize
    if n[0].kind != nkSym: internalError(n.info, "addStructFields")
    g.addStructFields(elements, n[0], typ)

    for i in 1..<n.sonsLen:
      case n[i].kind
      of nkOfBranch, nkElse:
        g.addStructFields(elements, n[i].lastSon, typ)
      else: internalError(n.info, "addStructFields")
  of nkSym:
    let field = n.sym
    if field.typ.kind == tyEmpty: return
    elements.add(g.llType(field.typ))
  else: internalError(n.info, "addStructFields")

proc headerType(g: LLGen, name: string): llvm.TypeRef =
  # Here are replacements for some of the types in the nim standard library that
  # rely on c header parsing to work. This is a crude workaround that needs to
  # be replaced, but works for now, on linux/x86_64
  case name
  of "struct stat":
    result = structCreateNamed(llctxt, name)
    result.structSetBody([
      llvm.int64Type(),  # __dev_t st_dev;
      llvm.int64Type(),  # __ino_t st_ino;
      llvm.int64Type(),  # __nlink_t st_nlink;
      llvm.int32Type(),  # __mode_t st_mode;
      llvm.int32Type(),  # __uid_t st_uid;
      llvm.int32Type(),  # __gid_t st_gid;
      llvm.int32Type(),  # int __pad0;
      llvm.int64Type(),  # __dev_t st_rdev;
      llvm.int64Type(),  # __off_t st_size;
      llvm.int64Type(),  # __blksize_t st_blksize;
      llvm.int64Type(),  # __blkcnt_t st_blocks;
      llvm.int64Type(),  # struct timespec st_atim;
      llvm.int64Type(),  # struct timespec st_atim;
      llvm.int64Type(),  # struct timespec st_mtim;
      llvm.int64Type(),  # struct timespec st_mtim;
      llvm.int64Type(),  # struct timespec st_ctim;
      llvm.int64Type(),  # struct timespec st_ctim;
      llvm.int64Type(),  # __syscall_slong_t __glibc_reserved[3];
      llvm.int64Type(),  # __syscall_slong_t __glibc_reserved[3];
      llvm.int64Type(),  # __syscall_slong_t __glibc_reserved[3];
      ])
  of "TGenericSeq": result = llGenericSeqType
  of "NimStringDesc": result = llNimStringDesc
  of "jmp_buf": result = llJmpbuf
  else: result = nil

proc headerTypeIndex(typ: PType, sym: PSym): seq[int] =
  let sig = hashType(typ)
  case typ.llName(sig)
  of "struct stat":
    let names = [
      "st_dev",
      "st_ino",
      "st_nlink",
      "st_mode",
      "st_uid",
      "st_gid",
      "__pad0",
      "st_rdev",
      "st_size",
      "st_blksize",
      "st_blocks",
      "st_atime",
      "st_atime_ns",
      "st_mtime",
      "st_mtime_ns",
      "st_ctime",
      "st_ctime_ns",
      "__glibc_reserved",
      "__glibc_reserved",
      "__glibc_reserved",
    ]
    result = @[names.find(sym.llName)]

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef =
  if typ == nil:
    return

  var typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString:
    return llNimStringDescPtr

  let sig = hashType(typ)
  if sig in g.types:
    return g.types[sig]

  let name = typ.llName(sig)

  result = g.headerType(name)
  if result != nil:
    g.types[sig] = result
    return

  p("llStructType " & $typ, typ, g.depth)

  # Create struct before setting body in case it's recursive
  result = structCreateNamed(llctxt, name)
  g.types[sig] = result

  var elements = newSeq[TypeRef]()

  let super = if typ.sons == nil: nil else: typ.sons[0]
  if super == nil:
    if (typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags:
      discard
    else:
      elements.add(g.llMagicType("TNimType").pointerType())
  else:
    elements.add(g.llStructType(super))

  g.addStructFields(elements, typ.n, typ)

  result.structSetBody(elements)

proc llTupleType(g: LLGen, typ: PType): llvm.TypeRef =
  if typ == nil:
    return

  let sig = hashType(typ)
  if sig in g.types:
    return g.types[sig]

  let name = typ.llName(sig)

  # Create struct before setting body in case it's recursive
  result = structCreateNamed(llctxt, name)
  g.types[sig] = result

  var elements = newSeq[TypeRef]()
  for t in typ.sons:
    elements.add(g.llType(t))

  p("llTupleType " & $name & " " & $elements, typ, g.depth)

  result.structSetBody(elements)

proc hasTypeField(typ: PType): bool =
  if typ.kind != tyObject: return false
  if typ.isPureObject(): return false
  if tfFinal in typ.flags and typ.sons[0] == nil: return false
  return true

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef)
proc genMarker(g: LLGen, typ: PType, n: PNode, v, op: llvm.ValueRef, start: var int) =
  if n == nil: return

  case n.kind
  of nkRecList:
    for s in n.sons:
      g.genMarker(typ, s, v, op, start)
  of nkRecCase:
    let kind = n[0].sym
    var gep = g.b.buildGEP(v, (@[0] & start).map(constGEPIdx), nn("mk.kind", kind))
    let vk = g.b.buildLoad(gep, nn("mk.kind.load", kind))

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let caseend = f.appendBasicBlock(nn("mk.kind.end", kind))

    inc(start)
    var hasElse = false

    for i in 1..<n.sonsLen:
      let branch = n[i]

      let ctrue = f.appendBasicBlock(nn("mk.kind.true", branch))
      if branch.kind == nkOfBranch:

        var length = branch.len
        for j in 0 .. length-2:
          var cmp: llvm.ValueRef

          if branch[j].kind == nkRange:
            let s = branch[j][0].intVal
            let e = branch[j][1].intVal

            let scmp = g.b.buildICmp(
              llvm.IntSGE, vk, llvm.constInt(vk.typeOf(), s.culonglong, llvm.True),
                nn("mk.kind.rng.s", branch))
            let ecmp = g.b.buildICmp(
              llvm.IntSLE, vk, llvm.constInt(vk.typeOf(), e.culonglong, llvm.True),
                nn("mk.kind.rng.e", branch))

            cmp = g.b.buildAnd(scmp, ecmp, nn("mk.kind.rng.cmp", branch))

          else:
            let k = branch[j].intVal
            cmp = g.b.buildICmp(
              llvm.IntEQ, vk, llvm.constInt(vk.typeOf(), k.culonglong, llvm.True),
              nn("mk.kind.cmp", branch))

          let cfalse = f.appendBasicBlock(nn("mk.kind.false", branch))
          discard g.b.buildCondBr(cmp, ctrue, cfalse)
          g.b.positionBuilderAtEnd(cfalse)
      else:
        hasElse = true
        discard g.b.buildBr(ctrue)

      let x = g.b.getInsertBlock()
      g.b.positionBuilderAtEnd(ctrue)

      g.genMarker(typ, branch.lastSon, v, op, start)

      discard g.b.buildBr(caseend)
      g.b.positionBuilderAtEnd(x)

    if not hasElse:
      discard g.b.buildBr(caseend)
    g.b.positionAndMoveToEnd(caseend)

  of nkSym:
    let field = n.sym
    var gep = g.b.buildGEP(v, (@[0] & start).map(constGEPIdx), nn("mk", field))
    if field.typ.skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
      gep = g.b.buildLoad(gep, nn("mk.load", field))

    g.genMarker(field.typ, gep, op)
    inc(start)
  else: internalError(n.info, "genMarker()")

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ == nil: return

  case typ.kind
  of tyGenericInst, tyGenericBody, tyTypeDesc:
    g.genMarker(typ.lastSon(), v, op)
  of tyArray:
    let arraySize = lengthOrd(typ.sons[0])

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let wcmp = f.appendBasicBlock(nn("mk.arr.cmp"))
    let wtrue = f.appendBasicBlock(nn("mk.arr.true"))
    let wfalse = f.appendBasicBlock(nn("mk.arr.false"))

    let cnt = g.b.localAlloca(llIntType, nn("mk.arr.cnt"))
    discard g.b.buildStore(constNimInt(0), cnt)

    # jump to comparison
    discard g.b.buildBr(wcmp)

    # generate condition expression in cmp block
    g.b.positionBuilderAtEnd(wcmp)
    let c = g.b.buildLoad(cnt, nn("mk.arr.c"))
    let cond = g.b.buildICmp(llvm.IntULT, c, constNimInt(arraySize.int), nn("mk.arr.lt"))

    discard g.b.buildCondBr(cond, wtrue, wfalse)

    # loop body
    g.b.positionBuilderAtEnd(wtrue)

    var gep =
      if v.typeOf().isArrayPtr(): g.b.buildGEP(v, [gep0, c], nn("while.data"))
      else: g.b.buildGEP(v, [c], nn("while.data"))

    if typ.sons[1].skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
      gep = g.b.buildLoad(gep, nn("mk.arr.load"))

    genMarker(g, typ.sons[1], gep, op)

    # back to comparison
    let cn = g.b.buildAdd(c, constNimInt(1), nn("mk.arr.add"))
    discard g.b.buildStore(cn, cnt)
    discard g.b.buildBr(wcmp)

    # continue at the end
    g.b.positionAndMoveToEnd(wfalse)

  of tyTuple:
    var i = 0
    g.genMarker(typ, typ.n, v, op, i)

  of tyObject:
    var start = 0
    if typ.sons != nil and typ.sons[0] != nil:
      let gep = g.b.buildGEP(v, [gep0, gep0])
      g.genMarker(typ.sons[0], gep, op)
      start = 1
    elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
      # TODO skip m_type in a nicer way
      start = 1

    if typ.n == nil: internalError("expected node")

    g.genMarker(typ, typ.n, v, op, start)
  of tyRef, tyString, tySequence:
    let p = g.b.buildBitCast(v, llVoidPtrType, nn("mk.p"))
    discard g.callCompilerProc("nimGCVisit", [p, op])
  of tyProc:
    if typ.callConv == ccClosure:
      let p = g.b.buildGEP(v, [gep0, gep1])
      let x = g.b.buildLoad(p, nn("mk.x"))
      discard g.callCompilerProc("nimGCVisit", [x, op])
  else:
    discard

proc genMarkerSeq(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ.elemType.kind == tyEmpty: return

  let seqlenp = g.b.buildNimSeqLenGEP(v)
  let seqlen = g.b.buildLoad(seqlenp, nn("mk.seq.len"))

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let wcmp = f.appendBasicBlock(nn("mk.seq.cmp"))
  let wtrue = f.appendBasicBlock(nn("mk.seq.true"))
  let wfalse = f.appendBasicBlock(nn("while.false"))

  let cnt = g.b.localAlloca(llIntType, nn("mk.seq.cnt"))
  discard g.b.buildStore(constNimInt(0), cnt)

  # jump to comparison
  discard g.b.buildBr(wcmp)

  # generate condition expression in cmp block
  g.b.positionBuilderAtEnd(wcmp)
  let c = g.b.buildLoad(cnt, nn("mk.seq.c"))
  let cond = g.b.buildICmp(llvm.IntULT, c, seqlen, nn("mk.seq.lt"))

  discard g.b.buildCondBr(cond, wtrue, wfalse)

  # loop body
  g.b.positionBuilderAtEnd(wtrue)

  var gep = g.b.buildNimSeqDataGEP(v, c)
  if typ.sons[0].skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
    gep = g.b.buildLoad(gep, nn("mk.seq.load"))
  genMarker(g, typ.sons[0], gep, op)

  # back to comparison
  let cn = g.b.buildAdd(c, constNimInt(1), nn("mk.seq.add"))
  discard g.b.buildStore(cn, cnt)
  discard g.b.buildBr(wcmp)

  # continue at the end
  g.b.positionAndMoveToEnd(wfalse)

proc genMarkerProcBody(g: LLGen, f: llvm.ValueRef, typ: PType) =
  g.b.withBlock(llvm.appendBasicBlock(f, nn("entry"))):
    var scope: llvm.NimMetadataRef

    if g.d != nil:
      let arr = g.d.nimDIBuilderGetOrCreateArray(
        [nil, g.dtypes[tyPointer], g.dtypes[tyInt]])
      scope = g.debugFunction(typ.sym, arr, f)

      let dl = llvm.getGlobalContext().nimDIBuilderCreateDebugLocation(
          0, 0, scope, nil)
      g.b.setCurrentDebugLocation(dl)

    let v = f.getFirstParam()
    v.setValueName("v")
    let op = v.getNextParam()
    op.setValueName("op")

    let vs = g.b.buildAlloca(v.typeOf(), "")
    discard g.b.buildStore(v, vs)

    let ops = g.b.buildAlloca(op.typeOf(), "")
    discard g.b.buildStore(op, ops)

    if g.d != nil:
      let dt = if typ.kind == tySequence: g.debugType(typ)
               else: g.d.nimDIBuilderCreatePointerType(
        g.debugType(typ.elemType), 64, 64, "")

      let file = g.debugGetFile(
        if typ.sym == nil: gProjectMainIdx else: typ.sym.info.fileIndex)
      let vd = g.d.nimDIBuilderCreateVariable(
        0, scope, v.getValueName(), file, 0, dt, false, 0, 1, 0)
      discard g.d.nimDIBuilderInsertDeclareAtEnd(vs, vd, nil, 0,
        g.b.getCurrentDebugLocation(), g.b.getInsertBlock())

      let opd = g.d.nimDIBuilderCreateVariable(
        0, scope, op.getValueName(), file, 0, g.dtypes[tyInt], false, 0, 2, 0)
      discard g.d.nimDIBuilderInsertDeclareAtEnd(ops, opd, nil, 0,
        g.b.getCurrentDebugLocation(), g.b.getInsertBlock())

    if typ.kind == tySequence:
      g.genMarkerSeq(typ, v, op)
    else:
      g.genMarker(typ.sons[0], v, op)

    discard g.b.buildRetVoid()

proc genMarkerProc(g: LLGen, typ: PType): llvm.ValueRef =
  if gSelectedGC < gcMarkAndSweep:
    return

  let sig = hashType(typ)
  if sig in g.markers:
    return g.markers[sig]

  let name = ".marker." & typ.llName(sig)

  let pt =
    if typ.kind == tySequence: g.llType(typ)
    else: g.llType(typ.elemType).pointerType()

  let ft = llvm.functionType(llvm.voidType(), @[pt, llIntType], false)

  result = g.m.addFunction(name, ft)
  g.markers[sig] = result

  # Because we generate only one module, we can tag all functions internal
  result.setLinkage(llvm.InternalLinkage)

  # Can't generate body yet - some magics might not yet exist
  g.markerBody.add((result, typ))

proc genGlobalMarkerProc(g: LLGen, sym: PSym, v: llvm.ValueRef): llvm.ValueRef =
  if sym.id in g.gmarkers:
    return g.gmarkers[sym.id]

  let name = ".marker.g." & sym.llName
  let typ = sym.typ.skipTypes(abstractInst)

  let ft = llvm.functionType(llvm.voidType(), @[], false)
  result = g.m.addFunction(name, ft)
  g.gmarkers[sym.id] = result

  # Because we generate only one module, we can tag all functions internal
  result.setLinkage(llvm.InternalLinkage)

  g.b.withBlock(llvm.appendBasicBlock(result, nn("entry"))):
    if g.d != nil:
      let arr = g.d.nimDIBuilderGetOrCreateArray([])
      let scope = g.debugFunction(sym, arr, result)

      let dl = llvm.getGlobalContext().nimDIBuilderCreateDebugLocation(
          0, 0, scope, nil)
      g.b.setCurrentDebugLocation(dl)

    var v = v
    if typ.kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
      v = g.b.buildLoad(v, nn("mk.load"))

    g.genMarker(typ, v, constInt8(0))

    discard g.b.buildRetVoid()

proc registerGcRoot(g: LLGen, sym: PSym, v: llvm.ValueRef) =
  if gSelectedGC in {gcMarkAndSweep, gcGenerational, gcV2, gcRefc} and
    sym.typ.containsGarbageCollectedRef():
    g.b.withBlock(g.init.getInitBlock()):
      let prc = g.genGlobalMarkerProc(sym, v)
      let scope = g.init.ds
      if scope != nil:
        let dl = llvm.getGlobalContext().nimDIBuilderCreateDebugLocation(
          0, 0, scope, nil)
        g.b.setCurrentDebugLocation(dl)

      discard g.callCompilerProc("nimRegisterGlobalMarker", [prc])

proc genTypeInfoInit(g: LLGen, t: PType, ntlt, lt: llvm.TypeRef,
                     baseVar, nodeVar, finalizerVar, markerVar,
                     deepcopyVar: llvm.ValueRef): llvm.ValueRef =
  let sizeVar = if lt == nil: ni0 else: llvm.sizeOfX(lt)

  let kind =
    if t.kind == tyObject and not t.hasTypeField(): tyPureObject
    elif t.kind == tyProc and t.callConv == ccClosure: tyTuple
    else: t.kind
  let kindVar = constInt8(int8(ord(kind)))

  var flags = 0'i8
  if not containsGarbageCollectedRef(t): flags = flags or 1
  if not canFormAcycle(t) or (t.kind == tyProc and t.callConv == ccClosure):
    flags = flags or 2
  if t.kind == tyEnum:
    var hasHoles = false
    for i in 0..t.sonsLen-1:
      let n = t.n[i].sym
      # why isn't tfEnumHasHoles always set? is it ever set at all?
      if n.position != i or tfEnumHasHoles in t.flags:
        hasHoles = true
    if hasHoles:
      # C gen overwrites flags in this case!
      flags = 1 shl 2

  let flagsVar = constInt8(flags)

  let values = [
    sizeVar, kindVar, flagsVar, baseVar, nodeVar, finalizerVar,
    markerVar, deepcopyVar
  ]
  result = llvm.constNamedStruct(ntlt, values)

proc genNodeInfo(g: LLGen, t: PType): llvm.ValueRef
proc genObjectNodeInfo(g: LLGen, t: PType, n: PNode, suffix: string): llvm.ValueRef
proc genTypeInfo(g: LLGen, t: PType): llvm.ValueRef

proc constNimNodeNone(g: LLGen, length: int): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  result = llvm.constNamedStruct(tnn,
    [constInt8(0), constNull(els[1]), constNull(els[2]), constNull(els[3]),
    constInt64(length), constNull(els[5])])

proc constNimNodeSlot(g: LLGen, offset, typeInfo: llvm.ValueRef, name: string): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  result = llvm.constNamedStruct(tnn,
    [constInt8(1), offset, typeInfo,
    g.b.buildGlobalStringPtr(name, ".nimnode.slot." & name),
    constNull(els[4]), constNull(els[5])])

proc constNimNodeList(g: LLGen, nodes: openarray[llvm.ValueRef]): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    tnnp = tnn.pointerType()
    els = tnn.getStructElementTypes()

  var nodesVal: llvm.ValueRef

  if nodes.len == 0:
    nodesVal = constNull(els[5])
  else:
    let nodesType = llvm.arrayType(tnnp, nodes.len.cuint)
    let tmp = g.m.addPrivateConstant(nodesType, nn(".nodes"))
    tmp.setInitializer(constArray(tnnp, nodes))
    nodesVal = constBitCast(tmp, els[5])

  result = llvm.constNamedStruct(tnn,
    [constInt8(2), constNull(els[1]), constNull(els[2]), constNull(els[3]),
    constInt64(nodes.len), nodesVal])

proc constNimNodeCase(
    g: LLGen, offset, typeInfo: llvm.ValueRef, name: string, nodesLen: int,
    nodes: openarray[llvm.ValueRef]): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    tnnp = tnn.pointerType()
    els = tnn.getStructElementTypes()

  var nodesVal: llvm.ValueRef

  if nodes.len == 0:
    nodesVal = constNull(els[5])
  else:
    let nodesType = llvm.arrayType(tnnp, nodes.len.cuint)
    let tmp = g.m.addPrivateConstant(nodesType, nn(".nodes"))
    tmp.setInitializer(constArray(tnnp, nodes))
    nodesVal = constBitCast(tmp, els[5])

  result = llvm.constNamedStruct(tnn,
    [constInt8(3), offset, typeInfo,
    g.b.buildGlobalStringPtr(name, ".nimnode.case." & name),
    constInt64(nodesLen), nodesVal])

proc genObjectNodeInfoInit(g: LLGen, t: PType, n: PNode, suffix: string): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    tnnp = tnn.pointerType()

  case n.kind
  of nkRecList:
    let l = n.sonsLen
    if l == 1:
      result = g.genObjectNodeInfoInit(t, n[0], suffix)
    else:
      var fields: seq[ValueRef] = @[]
      for i in 0..l-1:
        fields.add(g.genObjectNodeInfo(t, n[i], suffix & "." & $i))
      result = g.constNimNodeList(fields)

  of nkRecCase:
    let field = n[0].sym
    let l = lengthOrd(field.typ).int

    var fields: seq[ValueRef]
    newSeq(fields, l + 1)

    for i in 1..<n.sonsLen:
      let b = n[i]
      let bi = g.genObjectNodeInfo(t, b.lastSon, suffix & "." & $i)
      case b.kind
      of nkOfBranch:
        for j in 0..b.sonsLen - 2:
          if b[j].kind == nkRange:
            for a in getOrdValue(b[j][0])..getOrdValue(b[j][1]):
              fields[a.int] = bi
          else:
            fields[getOrdValue(b[j]).int] = bi
      else:
        fields[l] = bi

    # fill in holes or last element when there's no else
    for i in 0..l:
      if fields[i].isNil:
        fields[i] = constNull(tnnp)

    result = g.constNimNodeCase(
      g.constOffsetOf(t, field), g.genTypeInfo(field.typ),
      field.name.s, l, fields)

  of nkSym:
    let field = n.sym
    result = g.constNimNodeSlot(
      g.constOffsetOf(t, field), g.genTypeInfo(field.typ), field.name.s)

  else: internalError(n.info, "genObjectNodeInfoInit")

proc genObjectNodeInfo(g: LLGen, t: PType, n: PNode, suffix: string): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos and len(suffix) == 0:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & t.llName(sig) & suffix
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  if len(suffix) == 0:
    g.nodeInfos[sig] = result
  result.setInitializer(g.genObjectNodeInfoInit(t, n, suffix))

proc genTupleNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  let tnn = g.llMagicType("TNimNode")

  var fields: seq[ValueRef] = @[]

  let sig = hashType(t)
  let prefix = ".nodeinfo." & t.llName(sig) & "."

  let l = t.sonsLen
  for i in 0..<l:
    let
      name = prefix & $i
      field = g.m.addPrivateConstant(tnn, name)
      offset = constPtrToInt(constGEP(constNull(g.llType(t).pointerType()),
        [gep0, constGEPIdx(i)]), int64Type())
      fieldInit = g.constNimNodeSlot(offset, g.genTypeInfo(t.sons[i]),
        "Field" & $i)

    field.setInitializer(fieldInit)
    fields.add(field)

  result = g.constNimNodeList(fields)

proc genTupleNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & t.llName(sig)
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  g.nodeInfos[sig] = result
  result.setInitializer(g.genTupleNodeInfoInit(t))

proc genEnumNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  let l = t.n.sonsLen

  let sig = hashType(t)
  let prefix = ".nodeinfo." & t.llName(sig) & "."
  var fields: seq[ValueRef] = @[]
  for i in 0..<l:
    let
      name = prefix & $i
      n = t.n[i].sym
      fieldName = if n.ast == nil: n.name.s else: n.ast.strVal

    # type info not needed for enum members
    let fieldInit = g.constNimNodeSlot(
      constInt64(n.position), constNull(els[2]), fieldName)

    let field = g.m.addPrivateConstant(tnn, name)
    field.setInitializer(fieldInit)
    fields.add(field)

  result = g.constNimNodeList(fields)

  # TODO c gen sets ntfEnumHole as well on TNimType after generating TNimNode.. odd.

proc genEnumNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & t.llName(sig)
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  g.nodeInfos[sig] = result
  result.setInitializer(g.genEnumNodeInfoInit(t))

proc genSetNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  result = g.constNimNodeNone(firstOrd(t).int)

proc genSetNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & t.llName(sig)
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  g.nodeInfos[sig] = result
  result.setInitializer(g.genSetNodeInfoInit(t))

proc fakeClosureType(owner: PSym): PType =
  # generate same rtti as c generator - why does it generate it this way??
  result = newType(tyTuple, owner)
  result.rawAddSon(newType(tyPointer, owner))
  var r = newType(tyRef, owner)
  r.rawAddSon(newType(tyTuple, owner))
  result.rawAddSon(r)

proc genNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let tnn = g.llMagicType("TNimNode")

  case t.kind
  of tyObject: result = g.genObjectNodeInfo(t, t.n, "")
  of tyTuple: result = g.genTupleNodeInfo(t)
  of tyEnum: result = g.genEnumNodeInfo(t)
  of tySet: result = g.genSetNodeInfo(t)
  of tyProc:
    if t.callConv == ccClosure:
      result = g.genTupleNodeInfo(fakeClosureType(t.owner))
    else:
      result = constNull(tnn.pointerType())
  else: result = constNull(tnn.pointerType())

proc genTypeInfoBase(g: LLGen, t: PType): llvm.ValueRef =
  if t.kind in {tyArray}:
    result = g.genTypeInfo(t.sons[1])
  elif t.kind in {tySequence, tyRef, tyPtr, tyRange, tySet, tyObject} and
    t.sons.len > 0 and t.sons[0] != nil:
    result = g.genTypeInfo(t.sons[0])

  if result == nil:
    result = g.llMagicType("TNimType").pointerType().constNull()

proc genTypeInfo(g: LLGen, t: PType): llvm.ValueRef =
  var t = t.skipTypes(
    {tyDistinct, tyAlias, tyInferred, tyGenericBody, tyGenericParam, tyGenericInst})

  let sig = hashType(t)
  if sig in g.typeInfos:
    return g.typeInfos[sig]

  let name = ".typeinfo." & t.llName(sig)

  p("genTypeInfo", t, g.depth + 1)

  let
    ntlt = g.llMagicType("TNimType")
    lt = if t.kind == tyEmpty: nil else: g.llType(t)
    els = ntlt.getStructElementTypes()

  result = g.m.addPrivateConstant(ntlt, name)
  if g.d != nil:
    let
      sym = t.sym
      dt = g.debugMagicType("TNimType")

    discard g.d.nimDIBuilderCreateStaticVariable(
      g.dcu, name, "",
      g.debugGetFile(if sym == nil: gProjectMainIdx else: sym.info.fileIndex),
      if sym == nil: 0.cuint else: sym.info.line.cuint, dt, false, result, nil, 0)
  g.typeInfos[sig] = result

  var finalizerVar, markerVar, deepcopyVar: llvm.ValueRef
  let baseVar = g.genTypeInfoBase(t)
  let nodeVar = g.genNodeInfo(t)

  if t.kind in {tySequence, tyRef}:
    markerVar = g.genMarkerProc(t)

  if finalizerVar == nil: finalizerVar = llvm.constNull(els[5])
  if markerVar == nil:
    markerVar = llvm.constNull(els[6])
  else:
    markerVar = llvm.constBitCast(markerVar, els[6])
  if deepcopyVar == nil: deepcopyVar = llvm.constNull(els[7])

  result.setInitializer(g.genTypeInfoInit(t, ntlt, lt, baseVar,
    nodeVar, finalizerVar, markerVar, deepcopyVar))

proc llProcParamType(g: LLGen, t: PType): llvm.TypeRef =
  let typ = t.skipTypes(abstractInst)
  case typ.kind:
  of tyArray, tyOpenArray, tyVarargs, tyObject, tyTuple:
    result = g.llType(t).pointerType()
  of tySet:
    let size = getSize(typ).cuint
    result = if size <= 8: g.llType(t)
             else: g.llType(t).pointerType()
  of tyProc:
    result = if typ.callConv == ccClosure: g.llType(t).pointerType()
             else: g.llType(t)
  of tyDistinct, tyAlias, tyInferred: result = g.llProcParamType(t.lastSon)
  else: result = g.llType(t)

proc llPassAsPtr(g: LLGen, t: PType): bool =
  g.llType(t) != g.llProcParamType(t)

proc llProcType(g: LLGen, typ: PType, closure: bool): llvm.TypeRef =
  let retType = if typ.sons[0] == nil: llvm.voidType()
                else: g.llType(typ.sons[0])
  var argTypes = newSeq[llvm.TypeRef]()

  for param in typ.procParams():
    let t = param.sym.typ.skipTypes({tyGenericInst})
    let at = g.llProcParamType(t)
    argTypes.add(at)

    if skipTypes(t, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      argTypes.add(llIntType)  # Extra length parameter

  if closure:
    argTypes.add(llVoidPtrType)

  result = llvm.functionType(retType, argTypes, tfVarArgs in typ.flags)

proc fieldIndexRecs(n: PNode, sym: PSym, start: var int): seq[int] =
  case n.kind
  of nkRecList:
    for s in n:
      result = fieldIndexRecs(s, sym, start)
      if result.len > 0: return

  of nkRecCase:
    if n[0].kind != nkSym: internalError(n.info, "fieldIndex " & $n[0].kind)

    if n[0].sym.id == sym.id: return @[start]
    inc(start)
    for j in 1..<n.sonsLen:
      result = fieldIndexRecs(n[j].lastSon, sym, start)
      if result.len > 0: return
  of nkSym:
    if n.sym.id == sym.id: return @[start]
    inc(start)
  else:
    internalError(n.info, "Unhandled field index")
  return @[]

proc fieldIndex(typ: PType, sym: PSym): seq[int] =
  var typ = skipTypes(typ, abstractPtrs + tyUserTypeClasses)

  result = headerTypeIndex(typ, sym)
  if result != nil: return

  var start = 0
  if typ.kind != tyTuple:
    if typ.sons != nil and typ.sons[0] != nil:
      let s = fieldIndex(typ.sons[0], sym)
      if s.len > 0:
        return @[0] & s
      start = 1
    elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
      # TODO skip m_type in a nicer way
      start = 1

  let n = typ.n
  result = fieldIndexRecs(n, sym, start)

proc mtypeIndex(typ: PType): seq[llvm.ValueRef] =
  let zero = gep0
  result = @[zero]
  var t = skipTypes(typ, abstractInst)
  while t.kind in {tyVar, tyPtr, tyRef}:
    result = result & @[zero]
    t = skipTypes(t.lastSon, typedescInst)

  while t.kind == tyObject and t.sons[0] != nil:
    result = result & @[zero]
    t = skipTypes(t.sons[0], abstractPtrs)

  if not t.hasTypeField():
    # TODO why is this check in the generator???
    internalError("no 'of' operator available for pure objects")
  result = result & @[zero]

proc setElemIndex(g: LLGen, typ: PType, x: llvm.ValueRef): llvm.ValueRef =
  if firstOrd(typ) != 0:
    g.b.buildSub(
      x, constInt(x.typeOf(), firstOrd(typ).culonglong, llvm.False),
      nn("set.ord", x))
  else:
    x

proc isNimSeqLike(t: llvm.TypeRef): bool =
  if t.getTypeKind() != llvm.PointerTypeKind: return

  let et = t.getElementType()
  if et.getTypeKind() != llvm.StructTypeKind: return

  if et.countStructElementTypes() != 2: return

  let elems = et.getStructElementTypes()
  if elems[0] != llGenericSeqType: return

  if elems[1].getTypeKind() != llvm.PointerTypeKind and
     elems[1].getTypeKind() != llvm.ArrayTypeKind: return

  result = true

proc preCast(
    g: LLGen, unsigned: bool, ax: llvm.ValueRef, t: PType,
    lt: llvm.TypeRef = nil): llvm.ValueRef =
  let
    at = ax.typeOf()
    atk = at.getTypeKind()
    lt = if lt != nil: lt else: g.llType(t)
    ltk = lt.getTypeKind()

  if t.kind == tyCString and at != llCStringType and at.isNimSeqLike:
      result = g.b.buildNimSeqDataGEP(ax)
      return

  if ltk == PointerTypeKind and
      skipTypes(t, {tyVar}).kind in {tyVarargs, tyOpenArray, tyArray} and
      g.llType(t.elemType) == lt.getElementType() and
      at.isNimSeqLike:
    result = g.b.buildNimSeqDataGEP(ax)
    return

  if ltk == PointerTypeKind and atk == PointerTypeKind:
    result = g.b.buildBitCast(ax, lt, nn("pre", ax))
    return

  if ltk == IntegerTypeKind and atk == IntegerTypeKind and
      at.getIntTypeWidth() != lt.getIntTypeWidth():
    result = g.b.buildTruncOrExt(ax, lt, unsigned)
    return
  result = ax

proc externGlobal(g: LLGen, s: PSym): llvm.ValueRef =
  # Extra symbols that the compiler fetches from header files - we'll need
  # a better solution than hard-coding them, at some point
  # These are typically "nodecl", "importc" and/or "header" tagged in
  # the std library
  let
    name = s.llName
    t = g.llType(s.typ)

  case name
  of "errno": result = g.callErrno("")
  of "h_errno": result = g.callErrno("h_")
  else:
    if s.id in g.values:
      return g.values[s.id]
    result = g.m.addGlobal(t, name)
    g.values[s.id] = result

proc genLocal(g: LLGen, n: PNode): llvm.ValueRef =
  let s = n.sym
  if s.loc.k == locNone:
    fillLoc(s.loc, locLocalVar, n, s.name.s.mangle.rope, OnStack)
    if s.kind == skLet: incl(s.loc.flags, lfNoDeepCopy)

  let t = g.llType(s.typ)
  result = g.b.localAlloca(t, s.llName)
  g.f.scopePut(s.id, result)
  g.debugVariable(s, result)

proc genGlobal(g: LLGen, n: PNode): llvm.ValueRef =
  let s = n.sym
  if s.id in g.values:
    return g.values[s.id]

  if s.loc.k == locNone:
    fillLoc(s.loc, locGlobalVar, n, g.mangleName(s), OnHeap)

  let t = g.llType(s.typ.skipTypes(abstractInst))
  result = g.m.addGlobal(t, s.llName)
  g.values[s.id] = result

  if sfImportc in s.flags:
    result.setLinkage(llvm.ExternalLinkage)
  elif sfExportc in s.flags:
    result.setLinkage(llvm.CommonLinkage)
    result.setInitializer(llvm.constNull(t))
  else:
    result.setLinkage(llvm.PrivateLinkage)
    result.setInitializer(llvm.constNull(t))

  if sfThread in s.flags and optThreads in gGlobalOptions:
    result.setThreadLocal(llvm.True)
  g.debugGlobal(s, result)

proc callCompilerProc(g: LLGen, name: string, args: openarray[llvm.ValueRef]): llvm.ValueRef =
  let sym = magicsys.getCompilerProc(name)
  if sym == nil: internalError("compiler proc not found: " & name)

  let f = g.genFunctionWithBody(sym)
  let pars = sym.typ.procParams()

  var i = 0
  var args = @args
  for param in pars:
    let v = args[i]

    # TODO unsigned
    let a = g.preCast(false, v, param.typ, g.llProcParamType(param.typ))
    args[i] = a

    if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      i += 1
    i += 1

  result = g.b.buildCall(
    f, args,
    if f.typeOf().getElementType().getReturnType().getTypeKind() == llvm.VoidTypeKind: ""
    else: nn("call.cp." & name))

proc callBSwap(g: LLGen, v: llvm.ValueRef, n: cuint): llvm.ValueRef =
  let it = llvm.intType(n)
  let fty = llvm.functionType(it, [it])

  let
    f = g.m.getOrInsertFunction("llvm.bswap." & $n, fty)

  result = g.b.buildCall(f, [v])

proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef) =
  let
    f = g.m.getOrInsertFunction("llvm.memset.p0i8.i64", llMemsetType)
    t = g.b.buildBitCast(tgt, llVoidPtrType, nn("memset.tgt", v))

  discard g.b.buildCall(f, [t, v, len, constInt32(0), constInt1(false)])

proc callMemcpy(g: LLGen, tgt, src, len: llvm.ValueRef) =
  let
    f = g.m.getOrInsertFunction("llvm.memcpy.p0i8.p0i8.i64", llMemcpyType)
    t = g.b.buildBitCast(tgt, llVoidPtrType, nn("memcpy.tgt", tgt))
    s = g.b.buildBitCast(src, llVoidPtrType, nn("memcpy.src", src))

  discard g.b.buildCall(f, [t, s, len, constInt32(0), constInt1(false)])

proc callCtpop(g: LLGen, v: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let
    bits = (size * 8).cuint
    t = llvm.functionType(llvm.intType(bits), [llvm.intType(bits)])
    f = g.m.getOrInsertFunction("llvm.ctpop.i" & $bits, t)

  result = g.b.buildCall(f, [v])

proc callErrno(g: LLGen, prefix: string): llvm.ValueRef =
  # on linux errno is a function, so we call it here. not at all portable.
  let f = g.m.getOrInsertFunction("__" & prefix & "errno_location", llErrnoType)

  result = g.b.buildCall(f, [], nn(prefix & "errno"))

proc callWithOverflow(g: LLGen, op: string, a, b: llvm.ValueRef, name: string): llvm.ValueRef =
  let t = a.typeOf()
  let ft = llvm.functionType(llvm.structType([t, int1Type()]), [t, t])
  let f = g.m.getOrInsertFunction(
    "llvm." & op & ".with.overflow.i" & $t.getIntTypeWidth(), ft)

  result = g.b.buildCall(f, [a, b], name)

proc callBinOpWithOver(
    g: LLGen, a, b: llvm.ValueRef, op: Opcode, n: PNode): llvm.ValueRef =
  # like tyChar, for example
  let u = n != nil and n.typ.skipTypes(abstractVar).isUnsigned()

  var opfn: string
  case op
  of llvm.Add: opfn = if u: "uadd" else: "sadd"
  of llvm.Sub: opfn = if u: "usub" else: "ssub"
  of llvm.Mul: opfn = if u: "umul" else: "smul"
  else: internalError("bad overflow op")

  let bo = g.callWithOverflow(opfn, a, b, nn("binop.over." & $op, n))

  let over = g.b.buildExtractValue(bo, 1, nn("binop.isover", n))
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let lover = f.appendBasicBlock(nn("binop.over", n))
  let lok = f.appendBasicBlock(nn("binop.over.ok", n))

  discard g.b.buildCondBr(over, lover, lok)
  g.b.positionBuilderAtEnd(lover)
  discard g.callCompilerProc("raiseOverflow", [])
  discard g.b.buildUnreachable()
  g.b.positionBuilderAtEnd(lok)

  result = g.b.buildExtractValue(bo, 0, nn("binop.over.result", n))

  if n == nil: return

  let t = n.typ.skipTypes(abstractVar)
  if t.kind in {tyRange, tyEnum}:
    let lt = g.b.buildICmp(
      llvm.IntSLT, result, constInt(result.typeOf(),
      firstOrd(t).culonglong, llvm.False), nn("binop.over.rng.lt", n))
    let gt = g.b.buildICmp(
      llvm.IntSGT, result, constInt(result.typeOf(),
      lastOrd(t).culonglong, llvm.False), nn("binop.over.rng.gt", n))

    let ltgt = g.b.buildOr(lt, gt, nn("binop.over.rng.or", n))

    let lrok = f.appendBasicBlock(nn("binop.over.rng.ok", n))
    discard g.b.buildCondBr(ltgt, lover, lrok)
    g.b.positionBuilderAtEnd(lrok)

proc genObjectInit(g: LLGen, t: PType, v: llvm.ValueRef) =
  case analyseObjectWithTypeField(t)
  of frNone:
    discard
  of frHeader:
    let tgt = g.b.buildGEP(v, mtypeIndex(t), nn("mtype", v))
    discard g.b.buildStore(g.genTypeInfo(t), tgt)
  of frEmbedded:
    discard g.callCompilerProc("objectInit", [v, g.genTypeInfo(t)])

proc cpNewObj(g: LLGen, typ: PType): llvm.ValueRef =
  let
    refType = typ.skipTypes(abstractVarRange)
    objType = refType.sons[0].skipTypes(abstractRange)
    ti = g.genTypeInfo(refType)
    t = g.llType(objType)
    so = t.sizeOfX()
    x = g.callCompilerProc("newObj", [ti, so])

  result = g.b.buildBitCast(x, t.pointerType(), nn("newObj", x))
  g.genObjectInit(objType, result)

proc cpNewSeq(g: LLGen, typ: PType, len: llvm.ValueRef, name: string): llvm.ValueRef =
  let
    t = skipTypes(typ, abstractVarRange)
    ti = g.genTypeInfo(t)
    tmp = g.callCompilerProc("newSeq", [ti, len])

  result = g.b.buildBitCast(tmp, g.llType(t), name)

template withRangeItems(il: untyped, n: PNode, body: untyped) =
  let
    ax = g.genNode(s[0], true)
    bx = g.genNode(s[1], true)
    b = g.setElemIndex(typ, bx)

  # loop! init idx
  let i = g.b.localAlloca(ax.typeOf(), nn("rng.i", n))
  discard g.b.buildStore(ax, i)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let rcmp = f.appendBasicBlock(nn("rng.cmp", n))
  let rloop = f.appendBasicBlock(nn("rng.loop", n))
  let rdone = f.appendBasicBlock(nn("rng.done", n))

  # jump to comparison
  discard g.b.buildBr(rcmp)

  # check idx
  g.b.positionBuilderAtEnd(rcmp)
  let il = g.b.buildLoad(i, nn("rng.il", n))
  let cond = g.b.buildICmp(llvm.IntSLE, il, b, nn("rng.sle", n))
  discard g.b.buildCondBr(cond, rloop, rdone)

  # loop body
  g.b.positionBuilderAtEnd(rloop)

  body

  # inc idx
  let next = g.b.buildAdd(
    il, constInt(il.typeOf(), 1, llvm.False), nn("rng.inc", n))
  discard g.b.buildStore(next, i)
  # back to comparison
  discard g.b.buildBr(rcmp)

  # continue at the end
  g.b.positionAndMoveToEnd(rdone)

proc genFakeImpl(g: LLGen, s: PSym, f: llvm.ValueRef): bool =
  # sometimes the implementation in the nim std library doesn't work for llvm
  # but really needs to be there.. candidate for upstreaming...
  if s.name.s == "finished" and s.typ.sons.len == 2 and
      s.typ.sons[1].kind == tyProc:

    g.b.withBlock(f.appendBasicBlock(nn("entry.fake", s))):
      let gep = g.b.buildGEP(f.getParam(0), [gep0, gep1])
      let p = g.b.buildLoad(gep, "")
      let ax = g.b.buildBitCast(p, llIntType.pointerType(), "")
      let a = g.b.buildLoad(g.b.buildGEP(ax, [gep1]), "")
      let cmp = g.b.buildI8(g.b.buildICmp(llvm.IntSLT, a, ni0, ""))
      discard g.b.buildRet(cmp)
    return true

  if (s.name.s == "addInt" or s.name.s == "subInt" or s.name.s == "mulInt") and
      s.typ.sons.len == 3 and
      s.typ.sons[0].kind == tyInt and
      s.typ.sons[1].kind == tyInt and
      s.typ.sons[2].kind == tyInt:
    # prefer intrinsic for these...
    g.b.withBlock(f.appendBasicBlock(nn("entry.fake", s))):
      var res: llvm.ValueRef
      let
        a = f.getParam(0)
        b = f.getParam(1)
      if s.name.s == "addInt": res = g.callBinOpWithOver(a, b, llvm.Add, nil)
      if s.name.s == "subInt": res = g.callBinOpWithOver(a, b, llvm.Sub, nil)
      if s.name.s == "mulInt": res = g.callBinOpWithOver(a, b, llvm.Mul, nil)
      discard g.b.buildRet(res)
    return true

  if s.name.s == "cpuRelax":
    g.b.withBlock(f.appendBasicBlock(nn("entry.fake", s))):
      discard g.b.buildRetVoid()
    return true

proc genFunction(g: LLGen, s: PSym): llvm.ValueRef =
  if s.id in g.values:
    return g.values[s.id]

  if s.loc.k == locNone:
    fillLoc(s.loc, locProc, s.ast[namePos], g.mangleName(s), OnStack)

  let name = s.llName

  var s = s
  # Some compiler proc's have two syms essentially, because of an importc trick
  # in system.nim...
  if sfImportc in s.flags:
    result = g.m.getNamedFunction(name)
    if result != nil:
      g.values[s.id] = result
      return

  let typ = s.typ.skipTypes(abstractInst)

  let ft = g.llProcType(typ, typ.callConv == ccClosure)
  let f = g.m.addFunction(name, ft)

  g.values[s.id] = f

  if sfNoReturn in s.flags:
    f.addFuncAttribute(llAttrNoReturn)

  if typ.callConv == ccNoInline:
    f.addFuncAttribute(llAttrNoInline)

  if g.genFakeImpl(s, f):
    f.setLinkage(llvm.InternalLinkage)

  result = f

proc paramStorageLoc(param: PSym): TStorageLoc =
  if param.typ.skipTypes({tyVar, tyTypeDesc}).kind notin {
          tyArray, tyOpenArray, tyVarargs}:
    result = OnStack
  else:
    result = OnUnknown

proc genFunctionWithBody(g: LLGen, s: PSym): llvm.ValueRef =
  result = g.genFunction(s)

  if result.countBasicBlocks() != 0:
    return  # already has body

  if sfForward in s.flags:
    return

  if sfImportc in s.flags:
    return

  # Because we generate only one module, we can tag all functions internal
  result.setLinkage(llvm.InternalLinkage)

  var i = 1
  var lastIsArr = false

  let oldF = g.f

  let typ = s.typ.skipTypes(abstractInst)

  # generate body
  g.b.withBlock(llvm.appendBasicBlock(result, nn("entry", s))):

    g.f = newLLFunc(llvm.appendBasicBlock(result, nn("return", s)))
    if g.d != nil:
      let arr = g.debugProcType(typ, typ.callConv == ccClosure)
      g.f.ds = g.debugFunction(s, arr, result)
    g.f.options = s.options
    g.f.scopePush(s.ast, g.f.ret)

    g.debugUpdateLoc(s.ast)

    var argNo = 1
    for arg in llvm.params(result):
      while i < typ.n.len and isCompileTimeOnly(typ.n[i].sym.typ):
        i += 1
      if i >= typ.len:
        if typ.callConv == ccClosure:
          arg.setValueName("ClEnv")
          g.f.scopePut(-1, arg)

          continue
        else:
          internalError("Params missing in function call: " & $s)
          return
      let param = typ.n[i]

      fillLoc(param.sym.loc, locParam, param, param.sym.name.s.mangle.rope,
              param.sym.paramStorageLoc)  # TODO sometimes OnStack

      p("a", param, g.depth + 1)
      p("a", param.typ, g.depth + 2)

      let name = if lastIsArr: param.sym.llName & "len" else: param.sym.llName
      arg.setValueName(name)

      let av = g.b.localAlloca(arg.typeOf(), nn("arg", arg))
      discard g.b.buildStore(arg, av)

      g.debugVariable(param.sym, av, argNo)

      if lastIsArr:
        lastIsArr = false
        i += 1
        g.f.scopePut(-param.sym.id, av)
      else:
        g.f.scopePut(param.sym.id, av)

        if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
          lastIsArr = true
        else:
          i += 1

      argNo += 1

    # Scope for local variables
    g.f.scopePush(s.ast, g.f.ret)

    var ret: llvm.ValueRef
    if sfPure notin s.flags and typ.sons[0] != nil:
      let resNode = s.ast[resultPos]
      let res = resNode.sym
      if sfNoInit in s.flags: incl(res.flags, sfNoInit)
      ret = g.genLocal(resNode)
      g.buildStoreNull(ret)

    if tfCapturesEnv in typ.flags:
      let ls = lastSon(s.ast[paramsPos])
      let lt = g.llType(ls.sym.typ)
      let lx = g.b.buildBitCast(g.f.scopeGet(-1), lt, nn("ClEnvX"))
      let av = g.b.localAlloca(lx.typeOf(), nn("ClEnvX.a"))
      discard g.b.buildStore(lx, av)
      g.f.scopePut(ls.sym.id, av)

      g.debugVariable(ls.sym, av, argNo)

    g.genNode(s.getBody())

    g.b.buildBrFallthrough(g.f.ret)
    g.b.positionAndMoveToEnd(g.f.ret)

    if ret != nil:
      discard g.b.buildRet(g.b.buildLoad(ret, nn("load.result")))
    else:
      discard g.b.buildRetVoid()

    g.b.finalize(g.f)
    g.f = oldF

proc genFakeCall(g: LLGen, n: PNode, o: var llvm.ValueRef): bool =
  let nf = n[0]
  if nf.kind != nkSym: return false

  let s = nf.sym

  if s.originatingModule().name.s == "system":
    if s.name.s == "atomicLoadN":
      let p0 = g.genNode(n[1], false)
      o = g.b.buildLoad(p0, nn("a.load.n"))
      o.setOrdering(llvm.AtomicOrderingSequentiallyConsistent)
      return true

    if s.name.s == "atomicStoreN":
      let p0 = g.genNode(n[1], false)
      let p1 = g.genNode(n[2], true)
      let ax = g.b.buildStore(p1, p0)
      ax.setOrdering(llvm.AtomicOrderingSequentiallyConsistent)
      ax.setAlignment(1.cuint)  # TODO(j) align all over the place.
      return true

    if s.name.s == "atomicAddFetch":
      let p0 = g.genNode(n[1], false)
      let p1 = g.genNode(n[2], true)
      o = g.b.buildAtomicRMW(
        llvm.AtomicRMWBinOpAdd, p0, p1,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False)
      return true

    if s.name.s == "atomicSubFetch":
      let p0 = g.genNode(n[1], false)
      let p1 = g.genNode(n[2], true)
      o = g.b.buildAtomicRMW(
        llvm.AtomicRMWBinOpSub, p0, p1,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False)
      return true

    if s.name.s == "atomicThreadFence":
      o = g.b.buildFence(
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False, "")
      return true

    if s.name.s == "cas":
      let p0 = g.genNode(n[1], false)
      let p1 = g.genNode(n[2], true)
      let p2 = g.genNode(n[3], true)
      o = g.b.buildAtomicCmpXchg(p0, p1, p2,
        llvm.AtomicOrderingSequentiallyConsistent,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False)
      o = g.b.buildI8(g.b.buildExtractValue(o, 1.cuint, nn("cas.b", n)))
      return true
  elif s.originatingModule().name.s == "endians":
    case $s.loc.r
    of "__builtin_bswap16":
      o = g.callBSwap(g.genNode(n[1], true), 16)
      return true
    of "__builtin_bswap32":
      o = g.callBSwap(g.genNode(n[1], true), 32)
      return true
    of "__builtin_bswap64":
      o = g.callBSwap(g.genNode(n[1], true), 64)
      return true

proc genCallArgs(g: LLGen, n: PNode, fxt: llvm.TypeRef, ftyp: PType): seq[llvm.ValueRef] =
  var args: seq[ValueRef] = @[]

  let parTypes = fxt.getParamTypes()
  for i in 1..<n.sonsLen:
    let p = n[i]
    let pr = if p.kind == nkHiddenAddr: p[0] else: p
    if i >= ftyp.n.len: # varargs like printf, for example
      let v = g.genNode(pr, true)
      # In some transformations, the compiler produces a call node to a
      # parameterless function, and then adds statements as child nodes where
      # the parameter value expressions would normally go - decidedly, odd -
      # see tautoproc.nim
      if v != nil and v.typeOf().getTypeKind() != llvm.VoidTypeKind:
        args.add(v)
      continue

    let param = ftyp.n[i]

    if param.typ.isCompileTimeOnly(): continue

    let pt = parTypes[args.len]

    var v: llvm.ValueRef

    if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      var len: llvm.ValueRef

      if pr.skipConv().getMagic() == mSlice:
        let p = pr.skipConv()
        let
          ax = g.genNode(p[1], true)
          bx = g.genNode(p[2], true)
          cx = g.genNode(p[3], true)
        case p[1].typ.skipTypes(abstractVar + {tyPtr}).kind
        of tyOpenArray, tyVarargs, tyArray:
          let a = g.b.buildLoadValue(ax)
          v = g.b.buildGEP(a, [gep0, bx])

        of tyString, tySequence:
          let a =
            if n.typ.skipTypes(abstractInst).kind == tyVar: g.b.buildLoadValue(ax)
            else: ax
          v = g.b.buildNimSeqDataGEP(a, bx)
        else: internalError(n.info, "unknown slice type")

        len = g.b.buildSub(cx, bx, nn("slice.sub", n))
        len = g.b.buildAdd(len, constInt(len.typeOf(), 1, llvm.False), nn("slice.add", n))
      else:
        case pr.typ.skipTypes(abstractVar).kind
        of tyString, tySequence:
          v = g.genNode(pr, true)
          if pr.typ.skipTypes(abstractInst).kind == tyVar:
            v = g.b.buildLoad(v, "")
          len = g.b.buildNimSeqLenGEP(v)
          len = g.b.buildLoad(len, nn("call.seq.len", n))
          len = g.b.buildZExt(len, llIntType, nn("call.seq.len.ext", n))
          v = g.b.buildNimSeqDataGEP(v)
        of tyOpenArray, tyVarargs:
          v = g.genNode(p, param.typ.kind == tyVar)
          len = g.b.buildLoad(g.f.scopeGet(-pr.sym.id), "")
        of tyArray:
          v = g.genNode(p, true)
          len = constNimInt(lengthOrd(pr.typ).int)
        of tyPtr, tyRef:
          case pr.typ.lastSon().kind
          of tyString, tySequence:
            v = g.genNode(pr, true)
            v = g.b.buildLoad(v, nn("call.seq.load", n))
            len = g.b.buildNimSeqLenGEP(v)
            len = g.b.buildLoad(len, nn("call.seq.len", n))
            len = g.b.buildZExt(len, llIntType, nn("call.seq.len.ext", n))
            v = g.b.buildNimSeqDataGEP(v)
          else:
            internalError(n.info, "Unhandled ref length: " & $pr.typ)
        else:
          internalError(n.info, "Unhandled length: " & $pr.typ)

      if v.typeOf() != g.llProcParamType(param.typ):
        v = g.b.buildBitCast(v, g.llProcParamType(param.typ), nn("call.open", v))

      args.add(v)
      args.add(len)
    else:
      v = g.genNode(p, not g.llPassAsPtr(param.typ))

      # We need to use the type from the function, because with multimethods,
      # it looks like the type in param.typ changes during compilation!
      # seen with tmultim1.nim
      v = g.preCast(p.typ.isUnsigned(), v, param.typ, pt)
      args.add(v)

  result = args

proc genCall(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  if g.genFakeCall(n, result):
    return

  let nf = n[namePos]
  let typ = nf.typ.skipTypes(abstractInst)

  var fx = g.genNode(nf, true)

  let nfpt = g.llProcType(typ)
  let nft = nfpt.pointerType()
  let retty = nfpt.getReturnType()

  var callres: llvm.ValueRef

  if typ.callConv == ccClosure:
    let args = g.genCallArgs(n, nfpt, typ)

    let prc = g.b.buildExtractValue(fx, 0, nn("call.clo.prc.ptr", n))
    let env = g.b.buildExtractValue(fx, 1, nn("call.clo.env.ptr", n))

    if tfIterator in typ.flags:
      let cft = g.llProcType(typ, true).pointerType()
      let cfx = g.b.buildBitCast(prc, cft, nn("call.iter.prc", n))
      let clargs = args & @[env]
      callres = g.b.buildCall(cfx, clargs)
    else:
      let pre = g.b.getInsertBlock()
      let f = pre.getBasicBlockParent()

      let clonil = f.appendBasicBlock(nn("call.clo.noenv", n))
      let cloenv = f.appendBasicBlock(nn("call.clo.env", n))
      let cloend = f.appendBasicBlock(nn("call.clo.end", n))

      let cmp = g.b.buildICmp(
        llvm.IntEQ, env, llvm.constNull(env.typeOf()), nn("call.clo.noenv", n))
      discard g.b.buildCondBr(cmp, clonil, cloenv)

      g.b.positionBuilderAtEnd(clonil)

      fx = g.b.buildBitCast(prc, nft, nn("call.clo.prc.noenv", n))

      let res = g.b.buildCall(fx, args)
      discard g.b.buildBr(cloend)

      g.b.positionBuilderAtEnd(cloenv)

      let cft = g.llProcType(typ, true).pointerType()
      let cfx = g.b.buildBitCast(prc, cft, nn("call.clo.prc", n))

      let clargs = args & @[env]
      let cres = g.b.buildCall(cfx, clargs)

      discard g.b.buildBr(cloend)

      g.b.positionBuilderAtEnd(cloend)

      if retty.getTypeKind() != llvm.VoidTypeKind:
        callres = g.b.buildPHI(res.typeOf(), nn("call.clo.res", n))
        callres.addIncoming([res, cres], [clonil, cloenv])
  else:
    if fx.typeOf().getElementType().getTypeKind() != llvm.FunctionTypeKind:
      fx = g.b.buildBitCast(fx, nft, nn("call.fx", n))

    let args = g.genCallArgs(n, fx.typeOf().getElementType(), typ)
    let varname =
      if retty.getTypeKind() != llvm.VoidTypeKind: nn("call.res", n) else: ""
    callres = g.b.buildCall(fx, args, varname)

  if retty.getTypeKind() != llvm.VoidTypeKind and not load and
      nf.typ.sons[0].kind != tyRef:
    # if the originator of the call wants a pointer, we'll have
    # to create one for them - this is interesting for example
    # when a struct is returned "by value"
    result = g.b.localAlloca(retty, nn("call.res.ptr", n))
    discard g.b.buildStore(callres, result)
  else:
    result = callres

proc genMagicCall(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let s = n[namePos].sym
  if lfNoDecl notin s.loc.flags:
    let m = magicsys.getCompilerProc($s.loc.r)
    if m == nil: internalError(n.info, "Missing magic: " & $s.loc.r)
    discard g.genFunctionWithBody(m)

  result = g.genCall(n, load)

proc genRefAssign(g: LLGen, v, t: llvm.ValueRef) =
  if usesNativeGc():
    discard g.callCompilerProc("unsureAsgnRef", [t, v])
  else:
    discard g.b.buildStore(
      g.b.buildBitCast(v, t.typeOf().getElementType(), nn("bc", v)), t)

proc callGenericAssign(g: LLGen, t, x: llvm.ValueRef, ty: PType, deep: bool) =
  let f = if deep: "genericAssign" else: "genericShallowAssign"
  discard g.callCompilerProc(f, [t, x, g.genTypeInfo(ty)])

proc genAsgnFromRef(g: LLGen, v, t: llvm.ValueRef, typ: PType, deep = true) =
  let tt = t.typeOf()

  if tt.getTypeKind() != llvm.PointerTypeKind:
    internalError("Ptr required in genAssignment: " & $t)

  let tet = tt.getElementType()

  let ty = typ.skipTypes(abstractRange)
  case ty.kind
  of tyRef:
    let x = g.b.buildLoadValue(v)
    g.genRefAssign(x, t)
  of tySequence:
    let x = g.b.buildLoadValue(v)
    if deep:  # TODO or OnStack?
      discard g.callCompilerProc("genericSeqAssign", [t, x, g.genTypeInfo(ty)])
    else:
      g.genRefAssign(x, t)
  of tyString:
    let x = g.b.buildLoadValue(v)
    if deep:
      let sx = g.callCompilerProc("copyString", [x])
      g.genRefAssign(sx, t)
    else:
      g.genRefAssign(x, t)
  of tyProc:
    let x = g.b.buildLoadValue(v)

    if ty.containsGarbageCollectedRef():
      let tp = g.b.buildGEP(t, [gep0, gep0])
      let p = g.b.buildExtractValue(x, 0, nn("asgn.p", v))
      discard g.b.buildStore(
        g.b.buildBitCast(p, tp.typeOf().getElementType(), nn("asgn.pc", v)), tp)

      let te = g.b.buildGEP(t, [gep0, gep1])
      let e = g.b.buildExtractValue(x, 1, nn("asgn.e", v))

      g.genRefAssign(e, te)
    else:
      discard g.b.buildStore(g.b.buildBitCast(x, tet, nn("asgn.xc")), t)
  of tyTuple:
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, v, ty, deep)
    else:
      g.callMemcpy(t, v, t.typeOf().getElementType().sizeOfX())

  of tyObject:
    if ty.hasTypeField() or ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, v, ty, deep)
    else:
      g.callMemcpy(t, v, t.typeOf().getElementType().sizeOfX())

  of tyArray:
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, v, ty, deep)
    else:
      g.callMemcpy(t, v, t.typeOf().getElementType().sizeOfX())

  of tySet:
    let size = getSize(ty)

    if size <= 8:
      let x = g.b.buildLoadValue(v)
      discard g.b.buildStore(x, t)
    else:
      g.callMemcpy(t, v, t.typeOf().getElementType().sizeOfX())
  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar:
    let x = g.b.buildLoadValue(v)
    discard g.b.buildStore(g.b.buildBitCast(x, tet, nn("asgn.c", v)), t)

  else: internalError("genAssignment: " & $ty.kind)

proc genAssignment(
  g: LLGen, v: PNode, t: llvm.ValueRef, typ: PType, deep = true) =

  let tt = t.typeOf()

  if tt.getTypeKind() != llvm.PointerTypeKind:
    internalError("Ptr required in genAssignment: " & $t)

  let tet = tt.getElementType()

  let ty = typ.skipTypes(abstractRange + tyUserTypeClasses)
  case ty.kind
  of tyRef:
    g.genRefAssign(g.genNode(v, true), t)
  of tySequence:
    let x = g.genNode(v, true)
    if deep:  # TODO or OnStack?
      discard g.callCompilerProc("genericSeqAssign", [t, x, g.genTypeInfo(ty)])
    else:
      g.genRefAssign(x, t)
  of tyString:
    let x = g.genNode(v, true)
    if deep:
      let sx = g.callCompilerProc("copyString", [x])
      g.genRefAssign(sx, t)
    else:
      g.genRefAssign(x, t)
  of tyProc:
    let x = g.genNode(v, true)
    if ty.containsGarbageCollectedRef():
      let tp = g.b.buildGEP(t, [gep0, gep0])
      let p = g.b.buildExtractValue(x, 0, nn("asgn.p", v))
      discard g.b.buildStore(
        g.b.buildBitCast(p, tp.typeOf().getElementType(), nn("asgn.pc", v)), tp)

      let te = g.b.buildGEP(t, [gep0, gep1])
      let e = g.b.buildExtractValue(x, 1, nn("asgn.e", v))

      g.genRefAssign(e, te)
    else:
      discard g.b.buildStore(g.b.buildBitCast(x, tet, nn("asgn.xc")), t)
  of tyTuple:
    let x = g.genNode(v, false)
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, x, ty, deep)
    else:
      g.callMemcpy(t, x, t.typeOf().getElementType().sizeOfX())

  of tyObject:
    let x = g.genNode(v, false)
    if ty.hasTypeField() or ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, x, ty, deep)
    else:
      g.callMemcpy(t, x, t.typeOf().getElementType().sizeOfX())

  of tyArray:
    let x = g.genNode(v, false)
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, x, ty, deep)
    else:
      g.callMemcpy(t, x, t.typeOf().getElementType().sizeOfX())
  of tyOpenArray, tyVarargs:
    let s = if v.kind == nkHiddenDeref: v[0] else: v
    let p = g.b.buildLoad(g.f.scopeGet(s.sym.id), "")
    let len = g.b.buildLoad(g.f.scopeGet(-s.sym.id), "")
    if ty.containsGarbageCollectedRef():
      discard g.callCompilerProc(
        "genericAssignOpenArray", [t, p, len, g.genTypeInfo(ty)])
    else:
      g.callMemcpy(t, p, t.typeOf().getElementType().sizeOfX())
  of tySet:
    let size = getSize(ty)

    if size <= 8:
      let x = g.genNode(v, true)
      discard g.b.buildStore(x, t)
    else:
      let x = g.genNode(v, false)
      g.callMemcpy(t, x, t.typeOf().getElementType().sizeOfX())
  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar:
    let x = g.genNode(v, true)
    let pc = g.preCast(typ.isUnsigned(), x, typ, tet)
    discard g.b.buildStore(pc, t)

  of tyNil:
    discard g.b.buildStore(constNull(tet), t)

  else:
    internalError(v.info, "genAssignment: " & $ty.kind)

proc isDeepConstExprLL(n: PNode, strict: bool): bool =
  # strict means that anything involving a separate constant and a cast is
  # not allowed - as would happen with strings for example in a var string
  # initializer
  # note; callers are only using strict mode now because otherwise, string and
  # seq constants end up being passed to the garbage collector that tries to
  # incref them - c compiler keeps track of where a var is allocated and avoids
  # this
  case n.kind
  of nkCharLit..nkFloat128Lit, nkNilLit: result = true
  of nkStrLit..nkTripleStrLit: result = not strict
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = n[1].isDeepConstExprLL(strict)
  of nkPar:
    if strict: return false
    for s in n.sons:
      if not s.isDeepConstExprLL(strict): return false
    result = true

  of nkCurly: result = nfAllConst in n.flags
  of nkBracket:
    if n.typ.kind notin {tyArray, tySequence}: return false
    if n.typ.kind == tySequence and strict: return false

    for s in n.sons:
      if not s.isDeepConstExprLL(strict): return false
    result = true
  of nkObjConstr:
    let typ = n.typ.skipTypes(abstractInst)
    # TODO inheritance can be done, just being lazy...
    if typ.sons[0] != nil: return false

    for s in n.sons[1..<n.sonsLen]:
      if not s.isDeepConstExprLL(strict): return false
    result = true
  else: result = false

proc isSeqLike(n: PNode): bool =
  case n.kind
  of nkStrLit..nkTripleStrLit: result = true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = n[1].isSeqLike()
  else:
    if n.typ.kind == tySequence:
      result = true
    else:
      result = false

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef =
  case n.kind
  of nkCharLit..nkFloat128Lit, nkNilLit: result = g.genNode(n, true)
  of nkStrLit..nkTripleStrLit: result = constNimString(n)
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = g.genConstInitializer(n[1])

  of nkPar:
    let t = g.llType(n.typ)
    var vals: seq[llvm.ValueRef] = @[]
    for i in 0..<n.sonsLen:
      let s = n[i]

      if s.isSeqLike():
        # Need pointer for string literals
        vals.add(constBitCast(g.genNode(s, false), t.structGetTypeAtIndex(i.cuint)))
      else:
        vals.add(g.genConstInitializer(s))

    result = llvm.constNamedStruct(t, vals)

  of nkCurly: result = if nfAllConst in n.flags: constNimSet(n) else: nil
  of nkBracket:
    if n.typ.kind notin {tyArray, tySequence}: return nil

    var vals: seq[llvm.ValueRef] = @[]
    for s in n.sons:
      if s.isSeqLike():
        # Need pointer for string literals
        vals.add(g.genNode(s, false))
      else:
        vals.add(g.genConstInitializer(s))

    if n.typ.elemType.kind == tyEmpty:
      assert n.typ.kind == tySequence

      let x = llvm.constNamedStruct(llGenericSeqType, [ni0, ni0])
      result = llvm.constStruct([x])
    else:
      let et = g.llType(n.typ.elemType)

      let s = constArray(et, vals)
      if n.typ.kind in {tyArray}:
        result = s
      else:
        let ll = constNimInt(vals.len)
        let x = llvm.constNamedStruct(llGenericSeqType, [ll, ll])
        result = llvm.constStruct([x, s])

  of nkObjConstr:
    let t = g.llType(n.typ)
    if n.isDeepConstExprLL(false):
      var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())
      for i in 0..<vals.len:
        vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))
      for i in 1..<n.len:
        let s = n[i]
        let ind = fieldIndex(n.typ, s[0].sym)[0]
        if n[i].isSeqLike():
          # Need pointer for string literals
          vals[ind] = g.genNode(n[i], true)
        else:
          vals[ind] = g.genConstInitializer(n[i])
      result = constNamedStruct(t, vals)
    else:
      result = nil
  else: result = nil

proc genMagicHigh(g: LLGen, n: PNode): llvm.ValueRef =
  let len = g.genMagicLength(n)
  result = g.b.buildSub(
    len, llvm.constInt(len.typeOf(), 1, llvm.False), nn("high", len))

proc genMagicSizeOf(g: LLGen, n: PNode): llvm.ValueRef =
  let t = n[1].typ.skipTypes({tyTypeDesc})

  result = llvm.sizeOfX(g.llType(t))

proc genMagicOf(g: LLGen, n: PNode): llvm.ValueRef =
  var ax = g.genNode(n[1], false)

  # C generator does this extra dereferencing here - looks odd that it's needed
  # but there are test case fails because of it - needs more investigation
  var t = skipTypes(n[1].typ, abstractInst)
  while t.kind in {tyVar, tyPtr, tyRef}:
    t = skipTypes(t.lastSon, typedescInst)

  while ax.typeOf().getElementType().getTypeKind() == llvm.PointerTypeKind:
    ax = g.b.buildLoad(ax, nn("of.ind.load", n))

  let mt_gep = g.b.buildGEP(ax, mtypeIndex(t), nn("mtype", ax))
  let m_type = g.b.buildLoad(mt_gep, nn("of.load", n))
  let typ = n[2].typ.skipTypes(typedescPtrs)
  # TODO nil check if n is a pointer
  result = g.callCompilerProc("isObj", [m_type, g.genTypeInfo(typ)])

proc genMagicEcho(g: LLGen, n: PNode) =
  let b = n[1].skipConv

  if b.len == 0:
    let t = llNimStringDescPtr.pointerType()
    discard g.callCompilerProc("echoBinSafe", [constNull(t), constNimInt(0)])

  let x = g.genNode(b, true)
  let y = constNimInt(b.len)
  discard g.callCompilerProc("echoBinSafe", [x, y])

proc genMagicUnaryLt(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = llvm.constInt(ax.typeOf(), 1, False)
  if optOverflowCheck in g.f.options:
    result = g.callBinOpWithOver(ax, bx, llvm.Sub, n)
  else:
    result = g.b.buildSub(ax, bx, nn("lt", n))

proc genMagicIncDec(g: LLGen, n: PNode, op: Opcode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    a = g.b.buildLoad(ax, nn("inc.a", n))
    b = g.b.buildTruncOrExt(bx, a.typeOf(), n[2].typ)

  let t = n[1].typ.skipTypes({tyGenericInst, tyVar, tyRange})
  if optOverflowCheck notin g.f.options or t.kind in {tyUInt..tyUInt64}:
    let nv = g.b.buildBinOp(op, a, b, nn("inc.nv", n))
    discard g.b.buildStore(nv, ax)
  else:
    let nv = g.callBinOpWithOver(a, b, op, n[1])
    discard g.b.buildStore(nv, ax)

proc genMagicOrd(g: LLGen, n: PNode): llvm.ValueRef =
  var ax = g.genNode(n[1], true)

  if n[1].typ.skipTypes(abstractRange).kind == tyBool:
    # make sure we don't get more bits set than necessary
    ax = g.b.buildAnd(ax, llvm.constInt(ax.typeOf(), 1.cuint, llvm.False),
      nn("ord.bool", n))
  result = g.b.buildTruncOrExt(ax, g.llType(n.typ), n[1].typ)

proc genMagicNew(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    a = g.cpNewObj(n[1].typ.skipTypes(abstractVarRange))
  g.genRefAssign(a, ax)

proc genMagicNewFinalize(g: LLGen, n: PNode) =
  let typ = n[1].typ.skipTypes(abstractVarRange)

  let f = g.genNode(n[2], false)

  let ti = g.genTypeInfo(typ)

  # Funny enough, the finalizer is set for all objects of this type, not
  # just the one that's being created (!)
  let init = ti.getInitializer()
  ti.setInitializer(constNamedStruct(init.typeOf(), [
    init.getOperand(0),
    init.getOperand(1),
    init.getOperand(2),
    init.getOperand(3),
    init.getOperand(4),
    llvm.constBitCast(f, llVoidPtrType),
    init.getOperand(6),
    init.getOperand(7)]))

  let
    ax = g.genNode(n[1], false)
    a = g.cpNewObj(typ)
  g.genRefAssign(a, ax)

proc genMagicNewSeq(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    at = ax.typeOf()

  if at.getTypeKind() != llvm.PointerTypeKind:
    internalError("expected pointer, not " & $at)

  let x = g.cpNewSeq(n[1].typ, bx, nn("newseq", n))
  g.genRefAssign(x, ax)

proc genMagicNewSeqOfCap(g: LLGen, n: PNode): llvm.ValueRef =
  let seqtype = n.typ.skipTypes(abstractVarRange)
  let ax = g.genNode(n[1], true)
  let ti =  g.genTypeInfo(seqtype)

  result = g.callCompilerProc("nimNewSeqOfCap", [ti, ax])

proc genMagicLengthOpenArray(g: LLGen, n: PNode): llvm.ValueRef =
  # openarray must be a parameter so we should find it in the scope
  let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]
  result = g.b.buildLoad(g.f.scopeGet(-s.sym.id), "")

proc genMagicLengthStr(g: LLGen, n: PNode): llvm.ValueRef =
  let v = g.genNode(n[1], true)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let lload = f.appendBasicBlock(nn("str.len.load", n))
  let ldone = f.appendBasicBlock(nn("str.len.done", n))

  # nil check
  let cond = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constNull(v.typeOf()), nn("str.len.isnil", n))

  discard g.b.buildCondBr(cond, ldone, lload)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(lload)
  let strlen = g.m.getOrInsertFunction("strlen", llStrlenType)

  let v1 = g.b.buildCall(strlen, [v])
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(llIntType, nn("str.len", n))

  let v0 = ni0
  phi.addIncoming([v0, v1], [pre, lload])

  result = phi

proc genMagicLengthSeq(g: LLGen, n: PNode): llvm.ValueRef =
  let v = g.genNode(n[1], true)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let lload = f.appendBasicBlock(nn("seq.len.load", n))
  let ldone = f.appendBasicBlock(nn("seq.len.done", n))

  # nil check
  let cond = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constNull(v.typeOf()), nn("seq.len.isnil", n))

  discard g.b.buildCondBr(cond, ldone, lload)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(lload)
  let gep = g.b.buildNimSeqLenGEP(v)
  let v1 = g.b.buildLoad(gep, nn("seq.len.load", n))
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(llIntType, nn("seq.len", n))

  let v0 = ni0
  phi.addIncoming([v0, v1], [pre, lload])

  result = phi

proc genMagicLength(g: LLGen, n: PNode): llvm.ValueRef =
  let typ = skipTypes(n[1].typ, abstractVar + tyUserTypeClasses)
  case typ.kind
  of tyOpenArray, tyVarargs: result = g.genMagicLengthOpenArray(n)
  of tyCString: result = g.genMagicLengthStr(n)
  of tySequence, tyString: result = g.genMagicLengthSeq(n)
  of tyArray: result = constNimInt(lengthOrd(typ).int)
  else: internalError(n.info, "genMagicLength " & $n[1].typ)


proc genMagicXLen(g: LLGen, n: PNode): llvm.ValueRef =
  let v = g.genNode(n[1], true)

  # load length if v is not nil
  let gep = g.b.buildNimSeqLenGEP(v)
  result = g.b.buildLoad(gep, nn("seq.len.load", n))

proc genMagicIncl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = getSize(typ)

  if size <= 8:
    let b = g.b.buildSetMask(ax.typeOf().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildOr(g.b.buildLoad(ax, ""), b, "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], true)

    let (gep, mask) = g.b.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildOr(a, mask, "")
    discard g.b.buildStore(res, gep)

proc genMagicExcl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = getSize(typ)

  if size <= 8:
    let b = g.b.buildSetMask(ax.typeOf().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildAnd(g.b.buildLoad(ax, ""), g.b.buildBitnot(b), "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], true)

    let (gep, mask) = g.b.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildAnd(a, g.b.buildBitnot(mask), "")
    discard g.b.buildStore(res, gep)

proc genMagicCard(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = getSize(typ)

  if size <= 8:
    result = g.callCtpop(ax, size)
    result = g.b.buildTruncOrExt(result, g.llType(n.typ), true)
  else:
    # loop! init idx
    let i = g.b.localAlloca(llIntType, nn("card.i", n))
    discard g.b.buildStore(ni0, i)

    let tot = g.b.localAlloca(g.llType(n.typ), nn("card.tot", n))
    g.buildStoreNull(tot)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(nn("card.cmp", n))
    let rloop = f.appendBasicBlock(nn("card.loop", n))
    let rdone = f.appendBasicBlock(nn("card.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, nn("card.il", n))
    let cond = g.b.buildICmp(
      llvm.IntSLT, il, constNimInt(size.int), nn("card.slt", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let ai = g.b.buildLoad(g.b.buildGEP(ax, [il]), nn("card.ax", n))
    let a = g.callCtpop(ai, 1)
    let b = g.b.buildLoad(tot, nn("card.tot.load", n))
    let c = g.b.buildAdd(
      g.b.buildTruncOrExt(a, b.typeOf(), true), b, nn("card.add", n))
    discard g.b.buildStore(c, tot)

    # inc idx
    let next = g.b.buildAdd(
      il, constInt(il.typeOf(), 1, llvm.False), nn("card.inc", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildLoad(tot, nn("card", n))

proc genMagicChr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let t = g.llType(n.typ)
  if t != ax.typeOf():
    result = g.b.buildTrunc(ax, t, nn("chr.t", n))
  else:
    result = ax

proc genMagicGCref(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], false)
  discard g.callCompilerProc("nimGCref", [ax])

proc genMagicGCunref(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], false)
  discard g.callCompilerProc("nimGCunref", [ax])

proc genMagicBinOp(g: LLGen, n: PNode, op: Opcode): llvm.ValueRef =
  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  if ax.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      bx.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      ax.typeOf().getIntTypeWidth() != bx.typeOf().getIntTypeWidth():

    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    ax = g.b.buildNimIntExt(ax, n[1].typ.isUnsigned())
    bx = g.b.buildNimIntExt(bx, n[2].typ.isUnsigned())

  let bo = g.b.buildBinOp(op, ax, bx, nn("binop." & $op, n))
  result = g.b.buildTrunc(bo, g.llType(n.typ), nn("binop.trunc", n))

proc genMagicBinOpOverflow(g: LLGen, n: PNode, op: Opcode): llvm.ValueRef =
  if optOverflowCheck notin g.f.options:
    return g.genMagicBinOp(n, op)

  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  if n.typ.isUnsigned() or n[1].typ.isUnsigned() or n[2].typ.isUnsigned():
    internalError(n.info, "Unsigned types shouldn't use overflow semantics")

  if ax.typeOf().getTypeKind() != llvm.IntegerTypeKind or
      bx.typeOf().getTypeKind() != llvm.IntegerTypeKind:
    internalError(n.info, "Integers only here, please")

  if ax.typeOf().getIntTypeWidth() != bx.typeOf().getIntTypeWidth():
    internalError(n.info, "Expected nkHiddenStdConv to take care of this")

  let bo = g.callBinOpWithOver(ax, bx, op, n)
  result = g.b.buildTrunc(bo, g.llType(n.typ), nn("binop.over.trunc", n))

proc genMagicBinOpCall(g: LLGen, n: PNode, op: Opcode): llvm.ValueRef =
  if optOverflowCheck notin g.f.options:
    return g.genMagicBinOp(n, op)

  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  ax = g.b.buildNimIntExt(ax, n[1].typ.isUnsigned())
  bx = g.b.buildNimIntExt(bx, n[2].typ.isUnsigned())

  var opfn: string
  case op
  of llvm.SDiv: opfn = "divInt"
  of llvm.SRem: opfn = "modInt"
  else: internalError("Unexpected op: " & $op)

  let bo = g.callCompilerProc(opfn, [ax, bx])
  result = g.b.buildTrunc(bo, g.llType(n.typ), nn("binop.call.trunc", n))

proc genMagicBinOpF(g: LLGen, n: PNode, op: Opcode, unsigned = false): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  result = g.b.buildBinOp(op, ax, bx, nn("binop." & $op, n))

  if optNaNCheck in g.f.options:
    discard g.callCompilerProc("nanCheck", [result])
  if optInfCheck in g.f.options:
    discard g.callCompilerProc("infCheck", [result])

proc genMagicShr(g: LLGen, n: PNode): llvm.ValueRef =
  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  # right shifts are always logical in nim, apparently

  if ax.typeOf().getIntTypeWidth() != bx.typeOf().getIntTypeWidth():

    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    bx = g.b.buildTruncOrExt(bx, ax.typeOf(), true)

  result = g.b.buildBinOp(llvm.LShr, ax, bx, nn("binop." & $llvm.LShr, n))

proc genMagicMinMaxI(g: LLGen, n: PNode, op: IntPredicate): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    cmp = g.b.buildICmp(op, ax, bx, nn($op & ".cmp", n))

  result = g.b.buildSelect(cmp, ax, bx, nn($op, n))

proc genMagicMinMaxF(g: LLGen, n: PNode, op: RealPredicate): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    cmp = g.b.buildFCmp(op, ax, bx, nn($op & ".cmp", n))

  result = g.b.buildSelect(cmp, ax, bx, nn($op, n))

proc genMagicCmpI(g: LLGen, n: PNode, op: IntPredicate): llvm.ValueRef =
  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  if ax.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      bx.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      ax.typeOf().getIntTypeWidth() != bx.typeOf().getIntTypeWidth():
    # TODO should probably extend to the biggest of the two, rather than
    # full int
    ax = g.b.buildNimIntExt(ax, n[1].typ.isUnsigned())
    bx = g.b.buildNimIntExt(bx, n[2].typ.isUnsigned())

  # unsigned doesn't matter here - we've already normalized ints
  result = g.b.buildI8(g.b.buildICmp(
    op, g.preCast(false, ax, n[1].typ), g.preCast(false, bx, n[1].typ),
    nn("icmp." & $op, n)))

proc genMagicCmpF(g: LLGen, n: PNode, op: RealPredicate): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  result = g.b.buildI8(g.b.buildFCmp(op, ax, bx, nn($op, n)))

proc genMagicEqProc(g: LLGen, n: PNode): llvm.ValueRef =
  if n[1].typ.skipTypes(abstractInst).callConv == ccClosure:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], false)
      a0 = g.b.buildGEP(ax, [gep0, gep0])
      a1 = g.b.buildGEP(ax, [gep0, gep1])
      b0 = g.b.buildGEP(bx, [gep0, gep0])
      b1 = g.b.buildGEP(bx, [gep0, gep1])

    let x0 = g.b.buildICmp(
      llvm.IntEQ,
      g.b.buildLoad(a0, nn("eq.prc.a0", n)),
      g.b.buildLoad(b0, nn("eq.prc.b0", n)), nn("eq.prc.0", n))
    let x1 = g.b.buildICmp(
      llvm.IntEQ,
      g.b.buildLoad(a1, nn("eq.prc.a1", n)),
      g.b.buildLoad(b1, nn("eq.prc.b1", n)), nn("eq.prc.1", n))

    result = g.b.buildAnd(x0, x1, nn("eq.prc", n))
  else:
    let
      ax = g.genNode(n[1], true)
      bx = g.genNode(n[2], true)

    result = g.b.buildICmp(llvm.IntEQ, ax, bx, nn("eq.prc", n))
  result = g.b.buildI8(result)

proc genMagicUnaryMinus(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  return g.b.buildSub(llvm.constInt(ax.typeOf(), 0, llvm.False),
                      ax, nn("neg", ax))

proc genMagicAbsI(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let linv = f.appendBasicBlock(nn("abs.inv", n))
  let ldone = f.appendBasicBlock(nn("abs.done", n))

  # >=0 check
  let cmp = g.b.buildICmp(
    llvm.IntSGE, ax, llvm.constInt(ax.typeOf(), 0, llvm.False), nn("abs.sge", n))

  discard g.b.buildCondBr(cmp, ldone, linv)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(linv)
  let v1 = g.b.buildSub(llvm.constInt(ax.typeOf(), 0, llvm.False),
                        ax, nn("abs.neg", n))
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(ax.typeOf(), nn("abs", n))

  phi.addIncoming([ax, v1], [pre, linv])

  result = phi

proc genMagicNot(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  result = g.b.buildNot(ax)

proc genMagicBitnot(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  result = g.b.buildBitnot(ax)

proc genMagicUnaryMinusF64(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  result = g.b.buildFSub(
    llvm.constReal(ax.typeOf(), 0.0.cdouble), ax, nn("neg", ax))

proc genMagicAbsF64(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let fabs = g.m.getOrInsertFunction("llvm.fabs.f64", llFabsType)

  result = g.b.buildCall(fabs, [ax], nn("fabs", n))

proc genMagicZe(g: LLGen, n: PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildZExt(a, g.llType(n.typ), nn("zext", n))

proc genMagicToU(g: LLGen, n: PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildTrunc(a, g.llType(n.typ), nn("trunc", n))

proc genMagicToFloat(g: LLGen, n:PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildSIToFP(a, g.llType(n.typ), nn("sitofp", n))

proc genMagicToInt(g: LLGen, n:PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildFPToSI(a, g.llType(n.typ), nn("fptosi", n))

proc genMagicToStr(g: LLGen, n: PNode, f: string): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.callCompilerProc(f, [a])

proc genMagicStrToStr(g: LLGen, n: PNode): llvm.ValueRef =
  return g.genNode(n[1], true)

proc genMagicEnumToStr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let typ = skipTypes(n[1].typ, abstractVarRange)
  result = g.callCompilerProc(
    "reprEnum", [g.b.buildTruncOrExt(ax, llIntType, typ), g.genTypeInfo(typ)])

proc genMagicAndOr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)
  let a = g.b.buildI1(ax)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let aonext = f.appendBasicBlock(nn("andor.next", n))
  let aoend = f.appendBasicBlock(nn("andor.end", n))

  if n[0].sym.magic == mAnd:
    discard g.b.buildCondBr(a, aonext, aoend)
  else:
    discard g.b.buildCondBr(a, aoend, aonext)

  g.b.positionBuilderAtEnd(aonext)

  let bx = g.genNode(n[2], true)
  let bend = g.b.getInsertBlock()
  discard g.b.buildBr(aoend)

  g.b.positionAndMoveToEnd(aoend)

  let phi = g.b.buildPHI(ax.typeOf(), nn("and.res", n))

  phi.addIncoming([ax, bx], [pre, bend])

  result = phi

proc genMagicEqStr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  result = g.callCompilerProc("eqStrings", [ax, bx])

proc genMagicLeStr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    cmp = g.callCompilerProc("cmpStrings", [ax, bx])

  result = g.b.buildI8(g.b.buildICmp(llvm.IntSLE, cmp, ni0, nn("le.str", n)))

proc genMagicLtStr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    cmp = g.callCompilerProc("cmpStrings", [ax, bx])

  result = g.b.buildI8(g.b.buildICmp(llvm.IntSLT, cmp, ni0, nn("lt.str", n)))

proc genMagicSetCmp(g: LLGen, strict: bool, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = getSize(typ)

  if size <= 8:
    let b = g.b.buildBitnot(bx)
    let ab = g.b.buildAnd(ax, b, nn("setcmp.ab"))
    let le = g.b.buildICmp(
      llvm.IntEQ, ab, llvm.constInt(ab.typeOf(), 0, llvm.False), nn("setcmp.le", n))
    if strict:
      let ne = g.b.buildICmp(llvm.IntNE, ax, bx, nn("setcmp.ne", n))
      result = g.b.buildAnd(le, ne, nn("setcmp.res", n))
    else:
      result = le
    result = g.b.buildI8(result)
  else:
    let o = g.b.localAlloca(llBoolType, nn("setcmp.o", n))
    discard g.b.buildStore(constNimBool(true), o)

    # loop! init idx
    let i = g.b.localAlloca(llIntType, nn("set.i", n))
    discard g.b.buildStore(ni0, i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(nn("setcmp.cmp", n))
    let rloop = f.appendBasicBlock(nn("setcmp.loop", n))
    let rinc = f.appendBasicBlock(nn("setcmp.inc", n))
    let rfalse = f.appendBasicBlock(nn("setcmp.false", n))
    let rdone = f.appendBasicBlock(nn("setcmp.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, nn("setcmp.il", n))
    let cond = g.b.buildICmp(
      llvm.IntSLT, il, constNimInt(size.int), nn("setcmp.isdone", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let al = g.b.buildLoad(g.b.buildGEP(ax, [il]), nn("setcmp.al", n))
    let bl = g.b.buildLoad(g.b.buildGEP(bx, [il]), nn("setcmp.bl", n))
    let b = g.b.buildBitnot(bl)

    var cont: llvm.ValueRef
    let ab = g.b.buildAnd(al, b, nn("setcmp.ab", n))
    let le = g.b.buildICmp(
      llvm.IntEQ, ab, llvm.constInt(ab.typeOf(), 0, llvm.False), nn("setcmp.eq", n))
    if strict:
      let ne = g.b.buildICmp(llvm.IntNE, ax, bx, nn("setcmp.ne"))
      cont = g.b.buildAnd(le, ne, nn("setcmp.cont", n))
    else:
      cont = le

    discard g.b.buildCondBr(cont, rinc, rfalse)

    g.b.positionBuilderAtEnd(rinc)

    # inc idx
    let next = g.b.buildAdd(
      il, constInt(il.typeOf(), 1, llvm.False), nn("setcmp.inc", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    g.b.positionBuilderAtEnd(rfalse)
    discard g.b.buildStore(constNimBool(false), o)
    discard g.b.buildBr(rdone)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildLoadValue(o)

proc genMagicEqSet(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = getSize(typ)

  if size <= 8:
    result = g.b.buildI8(g.b.buildICmp(llvm.IntEQ, ax, bx, nn("seteq.eq", n)))
  else:
    # loop! init idx
    let i = g.b.localAlloca(llIntType, nn("seteq.i", n))
    discard g.b.buildStore(ni0, i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(nn("seteq.cmp", n))
    let rloop = f.appendBasicBlock(nn("seteq.loop", n))
    let rne = f.appendBasicBlock(nn("seteq.ne", n))
    let rinc = f.appendBasicBlock(nn("seteq.inc", n))
    let rdone = f.appendBasicBlock(nn("seteq.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, nn("seteq.il", n))
    let cond = g.b.buildICmp(
      llvm.IntSLT, il, constNimInt(size.int), nn("seteq.cond", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let a = g.b.buildLoad(g.b.buildGEP(ax, [il]), nn("seteq.a", n))
    let b = g.b.buildLoad(g.b.buildGEP(bx, [il]), nn("seteq.b", n))

    let cmp = g.b.buildICmp(llvm.IntEQ, a, b, nn("seteq.cmp", n))
    discard g.b.buildCondBr(cmp, rinc, rne)
    g.b.positionBuilderAtEnd(rne)

    discard g.b.buildBr(rdone)

    # inc idx
    g.b.positionBuilderAtEnd(rinc)
    let next = g.b.buildAdd(il, constInt(il.typeOf(), 1, llvm.False), nn("set.inc", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildPHI(llBoolType, nn("set.eq", n))
    result.addIncoming([constNimBool(false), constNimBool(true)], [rne, rcmp])

proc genMagicSetBinOp(g: LLGen, op: llvm.Opcode, invert: bool, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)

  let size = getSize(typ)
  if size <= 8:
    let
      a = ax
      b = bx
    let s = if invert: g.b.buildBitnot(b)
            else: b
    result = g.b.buildBinOp(op, a, s, nn("setbo.res"))
  else:
    let tmp = g.b.localAlloca(g.llType(typ), nn("setbo.tmp", n))

    # loop! init idx
    let i = g.b.localAlloca(llIntType, nn("setbo.i", n))
    discard g.b.buildStore(ni0, i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(nn("setbo.cmp", n))
    let rloop = f.appendBasicBlock(nn("setbo.loop", n))
    let rdone = f.appendBasicBlock(nn("setbo.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, nn("setbo.il", n))
    let cond = g.b.buildICmp(llvm.IntSLT, il, constNimInt(size.int), nn("setbo.cond", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let a = g.b.buildLoad(g.b.buildGEP(ax, [il]), nn("setbo.al", n))
    let b = g.b.buildLoad(g.b.buildGEP(bx, [il]), nn("setbo.bl", n))

    let s = if invert: g.b.buildBitnot(b) else: b
    let o = g.b.buildBinOp(op, a, s, nn("setbo.op", n))

    let p = g.b.buildGEP(tmp, [gep0, il])
    discard g.b.buildStore(o, p)

    # inc idx
    let next = g.b.buildAdd(il, constInt(il.typeOf(), 1, llvm.False), nn("setbo.next", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildLoadValue(tmp)

proc genMagicConStrStr(g: LLGen, n: PNode): llvm.ValueRef =
  # First, find out total length of all strings
  var tgtlen = ni0

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]

  for s in n.sons[1..<n.sonsLen]:
    let sx = g.genNode(s, true)
    exprs.add(sx)

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit..nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.b.buildLoad(g.b.buildNimSeqLenGEP(sx), nn("constrstr.slen", n))
      tgtlen = g.b.buildAdd(tgtlen, slen, nn("constrstr.tgtlen", n))

  if constlen > 0:
    tgtlen = g.b.buildAdd(tgtlen, constNimInt(constlen), nn("constrstr.tgtlen", n))

  # Allocate result
  let tgt = g.callCompilerProc("rawNewString", [tgtlen])

  # Copy data
  for i in 1..<n.sonsLen:
    let sx = exprs[i - 1]

    let s = n[i]

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [tgt, sx])
    else:
      discard g.callCompilerProc("appendString", [tgt, sx])

  result = tgt

proc genMagicDotDot(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  # a compiler magic with implementation in system.nim.. hm.
  let s = n[namePos].sym
  let f = g.genFunctionWithBody(s)
  # will be generated in multiple modules, so hide it..
  f.setLinkage(llvm.PrivateLinkage)

  result = g.genCall(n, load)

proc genMagicAppendStrCh(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    a = g.b.buildLoad(ax, nn("load.str.addc", n))
    ret = g.callCompilerProc("addChar", [a, bx])

  discard g.b.buildStore(ret, ax)

proc genMagicAppendStrStr(g: LLGen, n: PNode) =
  let tgtp = g.genNode(n[1], false)
  var tgt = g.b.buildLoad(tgtp, nn("load.str.adds", n))

  # First, find out total length of the new strings
  var tgtlen = ni0

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]
  for i in 2..<n.sonsLen:
    let s = n[i]
    let sx = g.genNode(s, true)
    exprs.add(sx)

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit..nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.b.buildLoad(
        g.b.buildNimSeqLenGEP(sx), nn("load.str.adds.len." & $i, n))
      tgtlen = g.b.buildAdd(tgtlen, slen, nn("str.adds.tot." & $i, n))

  if constlen > 0:
    tgtlen = g.b.buildAdd(
      tgtlen, constNimInt(constlen), nn("str.adds.tot.const", n))

  # Make room
  tgt = g.callCompilerProc("resizeString", [tgt, tgtlen])
  discard g.b.buildStore(tgt, tgtp)

  # Copy data
  for i in 2..<n.sonsLen:
    let sx = exprs[i - 2]

    let s = n[i]
    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [tgt, sx])
    else:
      discard g.callCompilerProc("appendString", [tgt, sx])

proc genMagicAppendSeqElem(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    et = n[1].typ.skipTypes(abstractVar).elemType
    a = g.b.buildLoad(ax, nn("seq.add.old", n))
    ap = g.b.buildGEP(a, [gep0, gep0])
    newseq = g.callCompilerProc("incrSeqV2", [ap, g.llType(et).sizeOfX()])
    tgt = g.b.buildBitCast(newseq, a.typeOf(), nn("seq.add.new", n))
    lenp = g.b.buildNimSeqLenGEP(tgt)
    len = g.b.buildLoad(lenp, nn("load.seq.add.last", n))

  g.genAssignment(n[2], g.b.buildNimSeqDataGEP(tgt, len), et)

  let newlen = g.b.buildAdd(
    len, llvm.constInt(len.typeOf(), 1, llvm.False), nn("seq.add.newlen", n))
  discard g.b.buildStore(newlen, lenp)
  discard g.b.buildStore(tgt, ax)

# Here, we need to emulate the C compiler and generate comparisons instead of
# sets, else we'll have crashes when out-of-range ints are compared against
# curlies

# from C compiler
proc fewCmps(s: PNode): bool =
  # this function estimates whether it is better to emit code
  # for constructing the set or generating a bunch of comparisons directly
  if s.kind != nkCurly: internalError(s.info, "fewCmps")
  if (getSize(s.typ) <= platform.intSize) and (nfAllConst in s.flags):
    result = false            # it is better to emit the set generation code
  elif elemType(s.typ).kind in {tyInt, tyInt16..tyInt64}:
    result = true             # better not emit the set if int is basetype!
  else:
    result = s.sonsLen <= 8  # 8 seems to be a good value

proc genMagicInSet(g: LLGen, n: PNode): llvm.ValueRef =
  if (n[1].kind == nkCurly) and fewCmps(n[1]):
    let s = if n[2].kind in {nkChckRange, nkChckRange64}: n[2][0]
            else: n[2]
    let sx = g.genNode(s, true)

    let res = g.b.localAlloca(llBoolType, nn("inset.res", n))
    discard g.b.buildStore(constNimBool(false), res)
    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let strue = f.appendBasicBlock(nn("inset.true", n))

    let length = n[1].sonsLen
    let u = s.typ.isUnsigned()
    for i in 0..<length:
      var cmp: llvm.ValueRef
      if n[1][i].kind == nkRange:
        let
          ax = g.genNode(n[1][i][0], true)
          bx = g.genNode(n[1][i][1], true)
          acmp = g.b.buildICmp(
            if u: llvm.IntUGE else: llvm.IntSGE, sx,
            g.b.buildTruncOrExt(ax, sx.typeOf(), u), nn("inset.acmp", n))
          bcmp = g.b.buildICmp(
            if u: llvm.IntULE else: llvm.IntSLE, sx,
            g.b.buildTruncOrExt(bx, sx.typeOf(), u), nn("inset.bcmp", n))

        cmp = g.b.buildAnd(acmp, bcmp, "")
      else:
        let ax = g.genNode(n[1][i], true)
        cmp = g.b.buildICmp(llvm.IntEQ, sx, g.b.buildTruncOrExt(ax, sx.typeOf(), u), "")

      let snext = f.appendBasicBlock(nn("inset.cmp", n))
      discard g.b.buildCondBr(cmp, strue, snext)
      g.b.positionBuilderAtEnd(snext)

    let send = f.appendBasicBlock(nn("inset.end", n))
    discard g.b.buildBr(send)

    g.b.positionBuilderAtEnd(strue)
    discard g.b.buildStore(constNimBool(true), res)
    discard g.b.buildBr(send)

    g.b.positionAndMoveToEnd(send)
    result = g.b.buildLoad(res, nn("inset.result", n))
    return

  let typ = skipTypes(n[1].typ, abstractVar)
  let size = getSize(typ)

  if size <= 8:
    let
      ax = g.genNode(n[1], true)
      bx = g.genNode(n[2], true)

    let b = g.b.buildSetMask(ax.typeOf(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildAnd(ax, b, nn("inset.res", n))
    result = g.b.buildICmp(
      llvm.IntNE, res, llvm.constInt(ax.typeOf(), 0, llvm.False),
      nn("inset.result", n))
  else:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], true)

    let (gep, mask) = g.b.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, nn("inset.a", n))
    let res = g.b.buildAnd(a, mask, nn("inset.res", n))
    result = g.b.buildICmp(
      llvm.IntNE, res, llvm.constInt(a.typeOf(), 0, llvm.False),
      nn("inset.result", n))
  result = g.b.buildI8(result)

proc genMagicRepr(g: LLGen, n: PNode): llvm.ValueRef =
  let t = skipTypes(n[1].typ, abstractVarRange)
  case t.kind
  of tyInt..tyInt64, tyUInt..tyUInt64:
    let ax = g.genNode(n[1], true)
    result = g.callCompilerProc("reprInt", [g.b.buildTruncOrExt(ax, llIntType, t)])
  of tyFloat..tyFloat128:
    let ax = g.genNode(n[1], true)
    result = g.callCompilerProc("reprFloat", [ax])
  of tyBool:
    let ax = g.genNode(n[1], true)
    result = g.callCompilerProc("reprBool", [ax])
  of tyChar:
    let ax = g.genNode(n[1], true)
    result = g.callCompilerProc("reprChar", [ax])
  of tyEnum, tyOrdinal:
    let ax = g.genNode(n[1], true)
    let a = g.b.buildTruncOrExt(ax, llIntType, t)
    result = g.callCompilerProc("reprEnum", [a, g.genTypeInfo(t)])
  of tyString:
    let ax = g.genNode(n[1], true)
    result = g.callCompilerProc("reprStr", [ax])
  of tySet:
    let ax = g.genNode(n[1], false)
    result = g.callCompilerProc("reprSet", [ax, g.genTypeInfo(t)])
  of tyOpenArray, tyVarargs:
    let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]
    let a = g.b.buildLoad(g.f.scopeGet(s.sym.id), "")
    let l = g.b.buildLoad(g.f.scopeGet(-s.sym.id), "")
    result = g.callCompilerProc("reprOpenArray", [a, l, g.genTypeInfo(t.elemType)])
  of tyCString, tyArray, tyRef, tyPtr, tyPointer, tyNil,
     tySequence:
    let ax = g.genNode(n[1], t.kind in {tyRef, tyPtr, tyVar, tyPointer})
    result = g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])
  of tyEmpty:
    localError(n.info, "'repr' doesn't support 'void' type")
  else:
    let ax = g.genNode(n[1], false)
    result = g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])

proc genMagicSetLengthStr(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    a = g.b.buildLoad(ax, nn("setlen.str", n))

  discard g.b.buildStore(g.callCompilerProc("setLengthStr", [a, bx]), ax)

proc genMagicSetLengthSeq(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    a = g.b.buildLoad(ax, nn("setlen.seq", n))
    ap = g.b.buildGEP(a, [gep0, gep0])
    at = a.typeOf()
    so = at.getElementType().structGetTypeAtIndex(1).getElementType().sizeOfX()
    x = g.callCompilerProc("setLengthSeq", [ap, so, bx])

  discard g.b.buildStore(g.b.buildBitCast(x, at, nn("setlen.cast", n)), ax)

proc genMagicParallel(g: LLGen, n: PNode) =
  # oddly, semparallel is called from the c gen (!)
  let n2 = semparallel.liftParallel(g.syms[^1], n)
  g.genNode(n2)

proc genMagicSwap(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], false)
    t = g.llType(n[1].typ)
    tmpx = g.b.localAlloca(t, nn("swap.tmp", n))
  g.buildStoreNull(tmpx)

  g.genAsgnFromRef(ax, tmpx, n[1].typ)
  g.genAsgnFromRef(bx, ax, n[1].typ)
  g.genAsgnFromRef(tmpx, bx, n[1].typ)

proc genMagicReset(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], false)

  discard g.callCompilerProc("genericReset",
    [ax, g.genTypeInfo(skipTypes(n[1].typ, abstractVarRange))])

proc genMagicIsNil(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let typ = skipTypes(n[1].typ, abstractPtrs)
  if typ.kind == tyProc and typ.callConv == ccClosure:
    result = g.b.buildICmp(
      llvm.IntEQ, g.b.buildExtractValue(ax, 0, nn("isnil.prc", n)),
        llvm.constNull(llVoidPtrType), nn("isnil", n))
  else:
    result = g.b.buildICmp(
      llvm.IntEQ, ax, llvm.constNull(ax.typeOf()), nn("isnil", n))
  result = g.b.buildI8(result)

proc genMagicArrToSeq(g: LLGen, n: PNode): llvm.ValueRef =
  let l = int(lengthOrd(skipTypes(n[1].typ, abstractInst)))
  let t = n.typ.skipTypes(abstractVarRange)
  result = g.cpNewSeq(t, constNimInt(l), nn("arrtoseq", n))

  let ax = g.genNode(n[1], true)

  for i in 0..<l:
    let tgt = g.b.buildNimSeqDataGEP(result, constGEPIdx(i))
    let src = g.b.buildGEP(ax, [constGEPIdx(i)])
    g.genAsgnFromRef(src, tgt, t.elemType)

proc genMagicSpawn(g: LLGen, n: PNode): llvm.ValueRef =
  let n2 = lowerings.wrapProcForSpawn(g.syms[^1], n, n.typ, nil, nil)
  result = g.genNode(n2, true)

proc genMagicDeepCopy(g: LLGen, n: PNode) =
  let x = if n[1].kind in {nkAddr, nkHiddenAddr}: n[1][0] else: n[1]
  let ax = g.genNode(x, false)

  let ty = n[2].typ.skipTypes(abstractVarRange)
  case ty.kind
  of tyPtr, tyRef, tyProc, tyTuple, tyObject, tyArray:
    let bx = g.genNode(n[2], false)
    discard g.callCompilerProc("genericDeepCopy", [ax, bx, g.genTypeInfo(ty)])

  of tySequence, tyString:
    let bx = g.genNode(n[2], false)
    discard g.callCompilerProc("genericSeqDeepCopy", [ax, bx, g.genTypeInfo(ty)])

  of tyOpenArray, tyVarargs:
    internalError(n.info, "todo")

  of tySet:
    let size = getSize(ty)

    if size <= 8:
      let bx = g.genNode(n[2], true)
      discard g.b.buildStore(bx, ax)
    else:
      let bx = g.genNode(n[2], false)
      g.callMemcpy(ax, bx, ax.typeOf().getElementType().sizeOfX())
  of tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar:
    let bx = g.genNode(n[2], true)
    discard g.b.buildStore(bx, ax)
  else:
    internalError(n.info, "genDeepCopy: " & $ty.kind)

proc genMagicGetTypeInfo(g: LLGen, n: PNode): llvm.ValueRef =
  let typ = n[1].typ.skipTypes(abstractVarRange)
  result = g.genTypeInfo(typ)

proc genMagic(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var op = n[0].sym.magic
  p("genMagic " & $op, n[0], g.depth + 1)
  case op
  of mHigh: result = g.genMagicHigh(n)
  of mSizeOf: result = g.genMagicSizeOf(n)
  of mOf: result = g.genMagicOf(n)
  of mEcho: g.genMagicEcho(n)
  of mUnaryLt: result = g.genMagicUnaryLt(n)
  of mInc: g.genMagicIncDec(n, llvm.Add)
  of mDec: g.genMagicIncDec(n, llvm.Sub)
  of mOrd: result = g.genMagicOrd(n)
  of mNew: g.genMagicNew(n)
  of mNewFinalize: g.genMagicNewFinalize(n)
  of mNewSeq: g.genMagicNewSeq(n)
  of mNewSeqOfCap: result = g.genMagicNewSeqOfCap(n)
  of mLengthOpenArray: result = g.genMagicLength(n)
  of mLengthStr: result = g.genMagicLength(n)
  of mLengthArray: result = g.genMagicLength(n)
  of mLengthSeq: result = g.genMagicLength(n)
  of mXLenStr, mXLenSeq: result = g.genMagicXLen(n)
  of mIncl: g.genMagicIncl(n)
  of mExcl: g.genMagicExcl(n)
  of mCard: result = g.genMagicCard(n)
  of mChr: result = g.genMagicChr(n)
  of mGCref: g.genMagicGCref(n)
  of mGCunref: g.genMagicGCunref(n)
  of mAddI: result = g.genMagicBinOpOverflow(n, llvm.Add)
  of mSubI: result = g.genMagicBinOpOverflow(n, llvm.Sub)
  of mMulI: result = g.genMagicBinOpOverflow(n, llvm.Mul)
  of mDivI: result = g.genMagicBinOpCall(n, llvm.SDiv)
  of mModI: result = g.genMagicBinOpCall(n, llvm.SRem) # TODO verify
  of mSucc: result = g.genMagicBinOp(n, llvm.Add)
  of mPred: result = g.genMagicBinOp(n, llvm.Sub)
  of mAddF64: result = g.genMagicBinOpF(n, llvm.FAdd)
  of mSubF64: result = g.genMagicBinOpF(n, llvm.FSub)
  of mMulF64: result = g.genMagicBinOpF(n, llvm.FMul)
  of mDivF64: result = g.genMagicBinOpF(n, llvm.FDiv)
  of mShrI: result = g.genMagicShr(n)
  of mShlI: result = g.genMagicBinOp(n, llvm.Shl)
  of mBitandI: result = g.genMagicBinOp(n, llvm.And)
  of mBitorI: result = g.genMagicBinOp(n, llvm.Or)
  of mBitxorI: result = g.genMagicBinOp(n, llvm.Xor)
  of mMinI: result = g.genMagicMinMaxI(n, llvm.IntSLE) # sign
  of mMaxI: result = g.genMagicMinMaxI(n, llvm.IntSGE) # sign
  of mMinF64: result = g.genMagicMinMaxF(n, llvm.RealOLE) # ordered?
  of mMaxF64: result = g.genMagicMinMaxF(n, llvm.RealOGE) # ordered?
  of mAddU: result = g.genMagicBinOp(n, llvm.Add)
  of mSubU: result = g.genMagicBinOp(n, llvm.Sub)
  of mMulU: result = g.genMagicBinOp(n, llvm.Mul)
  of mDivU: result = g.genMagicBinOp(n, llvm.UDiv)
  of mModU: result = g.genMagicBinOp(n, llvm.URem) # TODO verify
  of mEqI: result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeI: result = g.genMagicCmpI(n, llvm.IntSLE)
  of mLtI: result = g.genMagicCmpI(n, llvm.IntSLT)
  of mEqF64: result = g.genMagicCmpF(n, llvm.RealOEQ) # TODO ordered?
  of mLeF64: result = g.genMagicCmpF(n, llvm.RealOLE) # TODO ordered?
  of mLtF64: result = g.genMagicCmpF(n, llvm.RealOLT) # TODO ordered?
  of mLeU: result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtU: result = g.genMagicCmpI(n, llvm.IntULT)
  of mLeU64: result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtU64: result = g.genMagicCmpI(n, llvm.IntULT)
  of mEqEnum: result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeEnum: result = g.genMagicCmpI(n, llvm.IntULE) # TODO underlying
  of mLtEnum: result = g.genMagicCmpI(n, llvm.IntULT) # TODO underlying
  of mEqCh: result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeCh: result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtCh: result = g.genMagicCmpI(n, llvm.IntULT)
  of mEqB: result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeB: result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtB: result = g.genMagicCmpI(n, llvm.IntULT)
  of mEqRef: result = g.genMagicCmpI(n, llvm.IntEQ)
  of mEqUntracedRef: result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLePtr: result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtPtr: result = g.genMagicCmpI(n, llvm.IntULT)
  of mEqCString: result = g.genMagicCmpI(n, llvm.IntEQ)
  of mXor: result = g.genMagicCmpI(n, llvm.IntNE)
  of mEqProc: result = g.genMagicEqProc(n)
  of mUnaryMinusI, mUnaryMinusI64: result = g.genMagicUnaryMinus(n)
  of mAbsI: result = g.genMagicAbsI(n)
  of mNot: result = g.genMagicNot(n)
  of mBitnotI: result = g.genMagicBitnot(n)
  of mUnaryMinusF64: result = g.genMagicUnaryMinusF64(n)
  of mAbsF64: result = g.genMagicAbsF64(n)
  of mZe8ToI..mZeIToI64: result = g.genMagicZe(n)
  of mToU8, mToU16, mToU32: result = g.genMagicToU(n)
  of mToFloat, mToBiggestFloat: result = g.genMagicToFloat(n)
  of mToInt, mToBiggestInt: result = g.genMagicToInt(n)
  of mCharToStr: result = g.genMagicToStr(n, "nimCharToStr")
  of mBoolToStr: result = g.genMagicToStr(n, "nimBoolToStr")
  of mIntToStr: result = g.genMagicToStr(n, "nimIntToStr")
  of mInt64ToStr: result = g.genMagicToStr(n, "nimInt64ToStr")
  of mFloatToStr: result = g.genMagicToStr(n, "nimFloatToStr")
  of mCStrToStr: result = g.genMagicToStr(n, "cstrToNimstr")
  of mStrToStr: result = g.genMagicStrToStr(n)
  of mEnumToStr: result = g.genMagicEnumToStr(n)
  of mAnd, mOr: result = g.genMagicAndOr(n)
  of mEqStr: result = g.genMagicEqStr(n)
  of mLeStr: result = g.genMagicLeStr(n)
  of mLtStr: result = g.genMagicLtStr(n)
  of mEqSet: result = g.genMagicEqSet(n)
  of mLeSet: result = g.genMagicSetCmp(false, n)
  of mLtSet: result = g.genMagicSetCmp(true, n)
  of mMulSet: result = g.genMagicSetBinOp(llvm.And, false, n)
  of mPlusSet: result = g.genMagicSetBinOp(llvm.Or, false, n)
  of mMinusSet: result = g.genMagicSetBinOp(llvm.And, true, n)
  of mSymDiffSet: result = g.genMagicSetBinOp(llvm.Xor, false, n)
  of mConStrStr: result = g.genMagicConStrStr(n)
  of mDotDot: result = g.genMagicDotDot(n, load)
  of mAppendStrCh: g.genMagicAppendStrCh(n)
  of mAppendStrStr: g.genMagicAppendStrStr(n)
  of mAppendSeqElem: g.genMagicAppendSeqElem(n)
  of mInSet: result = g.genMagicInSet(n)
  of mRepr: result = g.genMagicRepr(n)
  of mExit: discard g.genCall(n, false)
  of mSetLengthStr: g.genMagicSetLengthStr(n)
  of mSetLengthSeq: g.genMagicSetLengthSeq(n)
  of mParallel: g.genMagicParallel(n)
  of mSwap: g.genMagicSwap(n)
  of mReset: g.genMagicReset(n)
  of mIsNil: result = g.genMagicIsNil(n)
  of mArrToSeq: result = g.genMagicArrToSeq(n)
  of mCopyStr, mCopyStrLast, mNewString, mNewStringOfCap, mParseBiggestFloat:
    result = g.genMagicCall(n, load)
  of mSpawn: result = g.genMagicSpawn(n)
  of mDeepCopy: g.genMagicDeepCopy(n)
  of mGetTypeInfo: result = g.genMagicGetTypeInfo(n)
  else: internalError(n.info, "Unhandled magic: " & $op)

# Nodes
proc genNodeSym(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var s = n.sym
  case s.kind
  of skConst, skVar, skLet, skTemp, skResult, skForVar:
    var v: llvm.ValueRef
    if lfHeader in s.loc.flags or lfNoDecl in s.loc.flags:
      v = g.externGlobal(s)
    elif sfGlobal in s.flags or
        (s.kind == skConst and (n.sym.ast.isDeepConstExprLL(true) or
        s.typ.kind notin ConstantDataTypes)):
      v = g.genGlobal(n)
    else:
      v = g.f.scopeGet(s.id)

    if v == nil:
      internalError(n.info, "Symbol not found: " & s.llName)

    if load and v.typeOf().getTypeKind() == llvm.PointerTypeKind:
      result = g.b.buildLoadValue(v)
    else:
      result = v
  of skParam:
    var v = g.f.scopeGet(s.id)

    if v == nil:
      internalError(n.info, "Symbol not found: " & s.llName)

    var toload = g.llPassAsPtr(n.typ)
    if toload and load and
        v.typeOf().getElementType().getTypeKind() == llvm.PointerTypeKind:
      v = g.b.buildLoad(v, "")
      result = g.b.buildLoadValue(v)
    elif not load and not toload and
        s.typ.kind notin {tyPtr, tyVar, tyRef, tyPointer}:
      # Someone wants an address, but all we have is a value...
      result = v
    else:
      result = g.b.buildLoad(v, "")
  of skType:
    result = g.genTypeInfo(s.typ)
  of skMethod:
    if {sfDispatcher, sfForward} * s.flags != {}:
      result = g.genFunction(s)
    else:
      result = g.genFunctionWithBody(s)
  of skProc, skConverter, skIterator:
    if (lfNoDecl in s.loc.flags or s.magic != mNone or
         {sfImportc, sfInfixCall} * s.flags != {}) and
         lfImportCompilerProc notin s.loc.flags:
      result = g.genFunction(s)
    else:
      result = g.genFunctionWithBody(s)
  else:
    internalError(n.info, "Unhandled kind: " & $s.kind)

proc genNodeIntLit(g: LLGen, n: PNode): llvm.ValueRef =
  let nt = g.llType(n.typ)
  if nt.getTypeKind == llvm.PointerTypeKind:
    result = llvm.constIntToPtr(
      llvm.constInt(llIntType, n.intVal.culonglong, llvm.False), nt)
  else:
    result = llvm.constInt(nt, n.intVal.culonglong, llvm.False)

proc genNodeFloatLit(g: LLGen, n: PNode): llvm.ValueRef =
  llvm.constReal(g.llType(n.typ), n.floatVal)

proc genNodeStrLit(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  if n.typ.skipTypes(abstractInst).kind == tyString:
    let ss = constNimString(n)
    let s = g.m.addPrivateConstant(ss.typeOf(), nn(".str", n))
    s.setInitializer(ss)
    result = constBitCast(s, llNimStringDescPtr)
  else:
    let init = constString(n.strVal)
    let s = g.m.addPrivateConstant(init.typeOf(), nn(".cstr", n))
    s.setInitializer(init)
    result = constBitCast(s, llCStringType)

proc genNodeNilLit(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  # proc x() = nil
  if n.typ.isEmptyType:
    return nil

  let t = g.llType(n.typ.skipTypes(abstractVarRange))
  if load:
    result = llvm.constNull(t)
  else:
    result = g.m.addPrivateConstant(t, nn("nil", n))
    result.setInitializer(llvm.constNull(t))

proc genNodeCall(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let nf = n[namePos]

  if nf.kind == nkSym:
    let sym = nf.sym
    if sym.magic != mNone:
      result = g.genMagic(n, load)
      return

  result = g.genCall(n, load)

proc genSingleVar(g: LLGen, n: PNode) =
  let vn = n[0]
  let v = vn.sym

  if sfCompileTime in v.flags: return
  if sfGoto in v.flags: internalError(n.info, "Goto vars not supported")

  var x: llvm.ValueRef

  let init = n[2]
  if sfGlobal in v.flags:
    if v.flags * {sfImportc, sfExportc} == {sfImportc} and
        n[2].kind == nkEmpty and
        v.loc.flags * {lfHeader, lfNoDecl} != {}:
      return

    x = g.genGlobal(vn)

    if v.kind == skLet or g.f.loopNesting == 0:
      if init.isDeepConstExprLL(true):
        let i = g.genConstInitializer(init)
        if i != nil:
          x.setInitializer(i)
          return

    g.genObjectInit(v.typ, x)
    g.registerGcRoot(v, x)

    # oddly, variables in a loop in the global scope are tagged "global" even
    # though they're local to the looping block
    if g.f.loopNesting > 0:
      discard g.callCompilerProc("genericReset", [x, g.genTypeInfo(v.typ)])
  else:
    x = g.genLocal(vn)

    # Some initializers expect value to be null, so we always set it so
    g.buildStoreNull(x)
    g.genObjectInit(v.typ, x)

  if init.kind != nkEmpty:
    g.genAssignment(init, x, v.typ)

proc genClosureVar(g: LLGen, n: PNode) =
  if n[2].kind == nkEmpty: return

  let x = g.genNode(n[0], false)
  g.genAssignment(n[2], x, n[0].typ)

proc genNodeIdentDefs(g: LLGen, n: PNode) =
  for s in n.sons: p("genIdentDefsStmt", s, g.depth + 1)
  if n[0].kind == nkSym:
    g.genSingleVar(n)
  else:
    g.genClosureVar(n)

proc genNodeVarTuple(g: LLGen, n: PNode) =
  for s in n.sons[0..n.sonsLen - 3]:
    if s.kind != nkSym:
      g.genNode(lowerTupleUnpacking(n, g.syms[^1]))
      return

  let t = g.genNode(n.lastSon, false)

  for i in 0..n.sonsLen - 3:
    let vn = n[i]
    let v = vn.sym

    var x: llvm.ValueRef

    if sfGlobal in v.flags:
      x = g.genGlobal(vn)
      g.genObjectInit(v.typ, x)
      g.registerGcRoot(v, x)
    else:
      x = g.genLocal(vn)
      g.buildStoreNull(x)
      g.genObjectInit(v.typ, x)

    let tv = g.b.buildGEP(t, [gep0, constGEPIdx(i)], nn("vartuple." & $i, vn))

    g.genAsgnFromRef(tv, x, vn.typ)

proc genNodePar(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  if n.isDeepConstExprLL(true):
    let init = g.genConstInitializer(n)
    result = g.m.addPrivateConstant(init.typeOf(), nn("par.init", n))
    result.setInitializer(init)
    return

  result = g.b.localAlloca(g.llType(n.typ), nn("par", n))
  g.buildStoreNull(result)

  for i in 0..<n.sonsLen:
    var s = n[i]

    if s.kind == nkExprColonExpr: s = s[1]
    let tgt = g.b.buildGEP(result, [gep0, constGEPIdx(i.int32)])
    g.genAssignment(s, tgt, s.typ)

  if load:
    result = g.b.buildLoad(result, nn("load.par", n))

proc genNodeObjConstr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)
    isRef = typ.kind == tyRef

  if isRef:
    result = g.cpNewObj(typ)
    typ = typ.lastSon.skipTypes(abstractInst)
  else:
    result = g.b.localAlloca(t, nn("objconstr", n))

    g.buildStoreNull(result)
    g.genObjectInit(typ, result)

  for i in 1 .. <n.len:
    let s = n[i]
    let ind = fieldIndex(typ, s[0].sym)
    let gep = g.b.buildInBoundsGEP(result, (@[0] & ind).map(constGEPIdx))
    g.genAssignment(s[1], gep, s[1].typ) # TODO should be dest typ

  if load and not isRef:
    result = g.b.buildLoad(result, nn("objconstr.load", n))
  elif not load and isRef:
    let tmp = g.b.localAlloca(t, nn("objconstr", n))
    discard g.b.buildStore(result, tmp)
    result = tmp

proc genNodeCurly(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    typ = n.typ
    size = getSize(skipTypes(n.typ, abstractVar))
    t = g.llType(typ)

  if size <= 8:
    let tmp = g.b.localAlloca(t, nn("curly", n))

    if nfAllConst in n.flags:
      discard g.b.buildStore(constNimSet(n), tmp)
    else:
      discard g.b.buildStore(llvm.constNull(t), tmp)

      for s in n:
        if s.kind == nkRange:
          withRangeItems(it, s):
            let mask = g.b.buildSetMask(t, g.setElemIndex(typ, it), size)
            let v = g.b.buildLoad(tmp, nn("curly.v", n))
            let nv = g.b.buildOr(v, mask, nn("curly.nv", n))
            discard g.b.buildStore(nv, tmp)

        else:
          let ax = g.genNode(s, true)

          let mask = g.b.buildSetMask(t, g.setElemIndex(typ, ax), size)
          let v = g.b.buildLoad(tmp, nn("curly.v"))
          let nv = g.b.buildOr(v, mask, nn("curly.nv", n))
          discard g.b.buildStore(nv, tmp)
    if load:
      result = g.b.buildLoad(tmp, nn("curly.load", n))
  else:
    result = g.b.localAlloca(t, nn("curly", n))
    if nfAllConst in n.flags:
      discard g.b.buildStore(constNimSet(n), result)
    else:
      g.callMemset(g.b.buildGEP(result, [gep0, gep0]), constInt8(0), constInt64(size))

      for s in n:
        if s.kind == nkRange:
          withRangeItems(it, s):
            let (gep, mask) = g.b.buildSetGEPMask(result, g.setElemIndex(typ, it))
            let v = g.b.buildLoad(gep, nn("curly.v", n))
            let nv = g.b.buildOr(v, mask, nn("curly.nv", n))
            discard g.b.buildStore(nv, gep)
        else:
          let ax = g.genNode(s, true)

          let (gep, mask) = g.b.buildSetGEPMask(result, g.setElemIndex(typ, ax))
          let v = g.b.buildLoad(gep, nn("curly.v", n))
          let nv = g.b.buildOr(v, mask, nn("curly.nv", n))
          discard g.b.buildStore(nv, gep)
    if load:
      result = g.b.buildLoadValue(result)

proc genNodeBracket(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let typ = n.typ.skipTypes(abstractVarRange)
  let t = g.llType(typ)

  if n.isDeepConstExprLL(true):
    let init = g.genConstInitializer(n)
    let c = g.m.addPrivateConstant(init.typeOf(), nn("bracket.init", n))
    c.setInitializer(init)

    if load and c.typeOf().isArrayPtr():
      result = g.b.buildGEP(c, [gep0, gep0], nn("bracket.const", n))
    else:
      result = c

    if n.isSeqLike() and c != g.llType(n.typ):
      result = llvm.constBitCast(c, g.llType(n.typ))

    return

  case typ.kind
  of tyArray:
    result = g.b.localAlloca(t, nn("bracket.arr", n))
    g.buildStoreNull(result)

    for i in 0..<n.sonsLen:
      let gep = g.b.buildGEP(result, [gep0, constGEPIdx(i)])
      g.genAssignment(n[i], gep, typ.elemType)

    if load:
      result = g.b.buildLoadValue(result)
  of tySequence:
    result = g.cpNewSeq(typ, constNimInt(n.sonsLen), nn("bracket.seq", n))
    for i in 0..<n.sonsLen:
      let gep = g.b.buildNimSeqDataGEP(result, constGEPIdx(i))
      g.genAssignment(n[i], gep, typ.elemType)
  else:
    internalError(n.info, "Unhandled kind: " & $typ.kind)

proc genNodeBracketExprArray(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true)
    ty = skipTypes(skipTypes(n[0].typ, abstractVarRange), abstractPtrs)
    first = firstOrd(ty)

  assert bx.typeOf().getTypeKind() == llvm.IntegerTypeKind

  # GEP indices are signed, so if a char appears here we want to make sure
  # it's zero-extended
  let bi = g.b.buildNimIntExt(bx, n[1].typ.isUnsigned())
  let b =
    if first != 0:
      g.b.buildSub(bi, constNimInt(first.int), nn("bra.arr.first", n))
    else: bi

  if ax.typeOf().isArrayPtr():
    result = g.b.buildGEP(ax, [gep0, b])
  else:
    result = g.b.buildGEP(ax, [b])

  if load:
    result = g.b.buildLoad(result, nn("bra.arr.load", n))

proc genNodeBracketExprOpenArray(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    s = if n[0].kind == nkHiddenDeref: n[0][0] else: n[0]
    ax = g.b.buildLoad(g.f.scopeGet(s.sym.id), "")
    bx = g.genNode(n[1], true)
  result = g.b.buildGEP(ax, [bx])

  if load:
    result = g.b.buildLoad(result, nn("bra.oa.load", n))

proc genNodeBracketExprSeq(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true)

  result = g.b.buildNimSeqDataGEP(ax, bx)

  if load:
    result = g.b.buildLoad(result, nn("bra.seq.load", n))

proc genNodeBracketExprCString(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true)

  result = g.b.buildGEP(ax, [bx], nn("bra.cstr.gep", n))

  if load:
    result = g.b.buildLoad(result, nn("bra.cstr.load", n))

proc genNodeBracketExprTuple(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true)

  if bx.typeOf().getIntTypeWidth() > 32.cuint:
    bx = g.b.buildTrunc(bx, llvm.int32Type(), nn("bra.tup.trunc", n))
  result = g.b.buildGEP(ax, [gep0, bx], nn("bra.tup.gep", n))

  if load:
    result = g.b.buildLoad(result, nn("bra.tup.load", n))

proc genNodeBracketExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var typ = skipTypes(n[0].typ, abstractVarRange + tyUserTypeClasses)
  if typ.kind in {tyRef, tyPtr}: typ = typ.lastSon.skipTypes(abstractVarRange)
  case typ.kind
  of tyArray: result = g.genNodeBracketExprArray(n, load)
  of tyOpenArray, tyVarargs: result = g.genNodeBracketExprOpenArray(n, load)
  of tySequence, tyString: result = g.genNodeBracketExprSeq(n, load)
  of tyCString: result = g.genNodeBracketExprCString(n, load)
  of tyTuple: result = g.genNodeBracketExprTuple(n, load)
  else: internalError(n.info, "Unhandled kind: " & $typ.kind)

proc genNodeDot(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  p("genDotExpr", n[1].sym, g.depth + 1)
  let v = g.genNode(n[0], false)

  let typ = skipTypes(n[0].typ, abstractInst)
  let i = fieldIndex(typ, n[1].sym)
  result = g.b.buildInBoundsGEP(v, (@[0] & i).map(constGEPIdx), nn("dot.gep", n))

  if load:
    result = g.b.buildLoadValue(result)

proc genNodeCheckedField(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  result = g.genNode(n[0], load)

proc genNodeDeref(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let v = g.genNode(n[0], true)

  result = if load: g.b.buildLoadValue(v) else: v

proc genNodeIfExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  # Sometimes an nkIfStmt appears in the ast even though it looks more like
  # an expression (see tcasestmt with an if in a case else).. it won't have
  # a type of its own so we'll have to cheat..
  let typ = if n.typ == nil: n[0][1].typ else: n.typ
  result = g.b.localAlloca(g.llType(typ), nn("ifx.res", n))
  g.buildStoreNull(result)

  let iend = f.appendBasicBlock(nn("ifx.end", n))

  for i in 0..<n.sonsLen:
    let s = n[i]

    if s.sonsLen == 1:
      # else branch
      g.f.scopePush(n, nil)
      g.genAssignment(s[0], result, typ)

      g.f.scopePop()

      discard g.b.buildBr(iend)
    else:
      let cond = g.b.buildI1(g.genNode(s[0], true))

      let itrue = f.appendBasicBlock(nn("ifx.true", n))
      let ifalse = f.appendBasicBlock(nn("ifx.false", n))

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      g.f.scopePush(n, nil)
      g.genAssignment(s[1], result, typ)
      g.f.scopePop()

      discard g.b.buildBr(iend)

      g.b.positionAndMoveToEnd(ifalse)

  if n[n.sonsLen - 1].sonsLen != 1:
    discard g.b.buildBr(iend)

  g.b.positionAndMoveToEnd(iend)

  if load:
    result = g.b.buildLoad(result, nn("ifx.load", n))

proc genNodeLambda(g: LLGen, n: PNode): llvm.ValueRef =
  let sym = n[namePos].sym
  result = g.genFunctionWithBody(sym)

proc genNodeConv(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    v = g.genNode(n[1], load)
    vt = v.typeOf()
    nt = if load: g.llType(n.typ) else: llvm.pointerType(g.llType(n.typ))
    vtk = vt.getTypeKind()
    ntk = nt.getTypeKind()
    vtyp = skipTypes(n[1].typ, abstractInst)
    ntyp = skipTypes(n.typ, abstractInst)

  if vt == nt:
    result = v
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    result = g.b.buildTruncOrExt(v, nt, n[1].typ)
  elif vtk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind} and ntk == llvm.IntegerTypeKind:
    if ntyp.kind in {tyUInt..tyUInt64}:
      result = g.b.buildFPToUI(v, nt, nn("conv.fptoui", n))
    else:
      result = g.b.buildFPToSI(v, nt, nn("conv.fptosi", n))
  elif vtk == llvm.IntegerTypeKind and ntk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind}:
    if vtyp.kind in {tyUInt..tyUInt64}:
      result = g.b.buildUIToFP(v, nt, nn("conv.uitofp", n))
    else:
      result = g.b.buildSIToFP(v, nt, nn("conv.sitofp", n))
  elif n[1].typ.kind == tyPtr and n.typ.kind == tyPointer:
    result = g.b.buildBitCast(v, nt, nn("conv.ptr", n))
  elif vtk == llvm.FloatTypeKind and ntk == llvm.DoubleTypeKind:
    result = g.b.buildFPExt(v, nt, nn("conv.fd", n))
  elif vtk == llvm.PointerTypeKind and ntk  == llvm.PointerTypeKind:
    result = g.b.buildBitCast(v, nt, nn("conv.pointer", n))
  elif n.typ.kind == tyProc and ntk == llvm.PointerTypeKind or nt == llClosureType:
    result = g.b.buildBitCast(v, llVoidPtrType, nn("conv.proc", n))
  elif vtk == llvm.DoubleTypeKind and ntk == llvm.FloatTypeKind:
    result = g.b.buildFPTrunc(v, nt, nn("conv.df", n))
  elif vtyp.kind == tyArray and ntyp.kind == tyArray:
    result = v

  if result == nil:
    internalError(n.info, "Unhandled conversion: " & $vt & " " & $nt & " " &
      $n[1].typ.kind & " " & $n.typ.kind)

proc genNodeCast(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ntyp = n[1].typ.skipTypes(abstractRange)

  let v =
    if ntyp.kind in {tyOpenArray, tyVarargs}: g.genNode(n[1], false)
    else: g.genNode(n[1], load)

  let nt = g.llType(n.typ)

  let vtk = v.typeOf().getTypeKind()
  let ntk = nt.getTypeKind()
  if vtk == llvm.PointerTypeKind and ntk == llvm.IntegerTypeKind:
    result = g.b.buildPtrToInt(v, nt, nn("cast.pi", n))
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.PointerTypeKind:
    result = g.b.buildIntToPtr(v, nt, nn("cast.ip", n))
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    result = g.b.buildTruncOrExt(v, nt, ntyp)
  else:
    result = g.b.buildBitCast(v, nt, nn("cast.bit", n))

proc genNodeAddr(g: LLGen, n: PNode): llvm.ValueRef =
  g.genNode(n[0], false)

proc genNodeObjDownConv(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ax = g.genNode(n[0], load)

  let at = ax.typeOf()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  if at == nt:
    result = ax
  elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
    result = g.b.buildBitCast(ax, nt, nn("obj.down", n))
  else:
    internalError(n.info, "Unhandled nkObjDownConv")

proc genNodeObjUpConv(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ax = g.genNode(n[0], n.typ.kind == tyRef)

  let at = ax.typeOf()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  if at == nt:
    result = ax
  elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
    result = g.b.buildBitCast(ax, nt, nn("obj.up", n))
  else:
    internalError(n.info, "Unhandled nkUpDownConv")

  if load and n.typ.kind notin {tyRef, tyVar}:
    result = g.b.buildLoad(result, nn("load.obj.up", n))

proc genNodeChckRange(g: LLGen, n: PNode, fn: string): llvm.ValueRef =
  let v = g.genNode(n[0], true)

  let
    nt = g.llType(n.typ)

  # c gen says range check for uints is problematic..
  if optRangeCheck in g.f.options and
      n.typ.skipTypes(abstractVarRange).kind notin {tyUInt..tyUInt64}:
    let
      bx = g.genNode(n[1], true)
      cx = g.genNode(n[2], true)
      b = g.b.buildNimIntExt(bx, n[1].typ.isUnsigned())
      c = g.b.buildNimIntExt(cx, n[2].typ.isUnsigned())
    discard g.callCompilerProc(fn, [v, b, c])

  result = g.b.buildTruncOrExt(v, nt, n.typ)

proc genNodeStringToCString(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[0], true)

  result = g.b.buildNimSeqDataGEP(ax)

proc genNodeAsgn(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], false)
  g.genAssignment(n[1], ax, n[0].typ, true)

proc genNodeFastAsgn(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], false)
  g.genAssignment(n[1], ax, n[0].typ, false)

proc genNodeProcDef(g: LLGen, n: PNode) =
  if n.sons[genericParamsPos].kind != nkEmpty: return

  var s = n.sons[namePos].sym

  if s.typ == nil: return
  if sfBorrow in s.flags: return
  if s.skipGenericOwner.kind != skModule or sfCompileTime in s.flags: return
  if s.getBody.kind == nkEmpty and lfDynamicLib notin s.loc.flags: return

  if (optDeadCodeElim notin gGlobalOptions and
      sfDeadCodeElim notin getModule(s).flags) or
      ({sfExportc, sfCompilerProc} * s.flags == {sfExportc}) or
      (sfExportc in s.flags and lfExportLib in s.loc.flags) or
      (s.kind == skMethod):
    discard g.genFunctionWithBody(s)

proc genNodeIfStmt(g: LLGen, n: PNode): llvm.ValueRef =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  # TODO Single scope enough?
  g.f.scopePush(n, nil)
  var iend: llvm.BasicBlockRef
  for i in 0..<n.sonsLen:
    let s = n[i]

    if s.sons.len == 1:
      # else branch
      g.f.scopePush(n, nil)
      g.genNode(s[0])
      g.f.scopePop()

      if g.b.needsBr():
        if iend == nil:
          iend = f.appendBasicBlock(nn("if.end", n))
        discard g.b.buildBr(iend)
    else:
      let cond = g.b.buildI1(g.genNode(s[0], true))

      let itrue = f.appendBasicBlock(nn("if.true", n))
      let ifalse = f.appendBasicBlock(nn("if.false", n))

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      g.f.scopePush(n, nil)
      g.genNode(s[1])
      g.f.scopePop()

      if g.b.needsBr():
        if iend == nil:
          iend = f.appendBasicBlock(nn("if.end", n))
        discard g.b.buildBr(iend)

      g.b.positionAndMoveToEnd(ifalse)

  if iend != nil:
    if n[n.sonsLen - 1].sonsLen != 1:
      discard g.b.buildBr(iend)
    g.b.positionAndMoveToEnd(iend)
  discard g.f.scopePop()

proc genNodeWhenStmt(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  g.genNode(n[1][0], load)

proc genNodeWhileStmt(g: LLGen, n: PNode) =
  inc(g.f.loopNesting)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let wcmp = f.appendBasicBlock(nn("while.cmp", n))
  let wtrue = f.appendBasicBlock(nn("while.true", n))
  let wfalse = f.appendBasicBlock(nn("while.false", n))

  # jump to comparison
  discard g.b.buildBr(wcmp)

  # generate condition expression in cmp block
  g.b.positionBuilderAtEnd(wcmp)
  let cond = g.b.buildI1(g.genNode(n[0], true))

  discard g.b.buildCondBr(cond, wtrue, wfalse)

  # loop body
  g.b.positionBuilderAtEnd(wtrue)
  g.f.scopePush(n, wfalse)
  g.genNode(n[1])
  g.f.scopePop()

  # back to comparison
  discard g.b.buildBr(wcmp)

  # continue at the end
  g.b.positionAndMoveToEnd(wfalse)

  dec(g.f.loopNesting)

proc genNodeCaseStmt(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let isString = skipTypes(n[0].typ, abstractVarRange).kind == tyString

  let ax = g.genNode(n[0], true)

  let caseend = f.appendBasicBlock(nn("case.end", n))

  if not n.typ.isEmptyType():
    result = g.b.localAlloca(g.llType(n.typ), nn("case.res", n))
    g.buildStoreNull(result)

  for i in 1..<n.sonsLen:
    let s = n[i]
    p("genNodeCaseStmt", s, g.depth)

    let isLast = i == n.sonsLen - 1

    let cur = g.b.getInsertBlock()

    let lbl = if s.kind == nkElse: "case.else" else: "case.of." & $i
    let casedo = f.appendBasicBlock(nn(lbl & ".do", n))

    let next =
      if isLast: caseend
      elif n[i+1].kind == nkElse: f.appendBasicBlock(nn("case.else", n))
      else: f.appendBasicBlock(nn("case.of." & $(i+1), n))

    g.f.scopePush(n, caseend)
    g.b.positionBuilderAtEnd(casedo)
    if not n.typ.isEmptyType():
      g.genAssignment(s.lastSon, result, n.typ)
    else:
      g.genNode(s.lastSon)

    g.b.buildBrFallthrough(caseend)

    g.b.positionBuilderAtEnd(cur)

    case s.kind
    of nkOfBranch:
      # sons here is a list of candidates to match against, which may
      # be values or ranges, except the last one which is the action
      for j in 0..s.sonsLen - 2:
        let isLast2 = j == s.sonsLen - 2
        let cond = s[j]
        if cond.kind == nkRange:
          let bx = g.genNode(cond.sons[0], true)
          let cx = g.genNode(cond.sons[1], true)

          let cmpb = g.b.buildICmp(llvm.IntSGE, ax, bx, nn("case.cmpb", n))
          let cmpc = g.b.buildICmp(llvm.IntSLE, ax, cx, nn("case.cmpc", n))
          let cmp = g.b.buildI1(g.b.buildAnd(cmpb, cmpc, nn("case.cmp", n)))

          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(nn(lbl & ".or." & $j, n))
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)
        else:
          let bx = g.genNode(cond, true)

          var cmp: llvm.ValueRef
          if isString:
            let tmp = g.callCompilerProc("cmpStrings", [ax, bx])
            cmp = g.b.buildI1(
              g.b.buildICmp(llvm.IntEQ, tmp, ni0, nn("case.cmp", n)))
          else:
            let b = g.b.buildTruncOrExt(bx, ax.typeOf(), cond.typ)
            cmp = g.b.buildI1(
              g.b.buildICmp(llvm.IntEQ, ax, b, nn("case.cmp", n)))
          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(nn(lbl & ".or." & $j, n))
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)

        # Moving block is not necessary but makes generated code easier
        # to follow, placing action just after conditions
        if not isLast2:
          g.b.getInsertBlock().moveBasicBlockBefore(casedo)

    of nkElse:
      discard g.b.buildBr(casedo)
    else:
      internalError(s.info, "Unexpected case kind " & $s.kind)
    g.f.scopePop()

  g.b.positionAndMoveToEnd(caseend)

  if f.getLastBasicBlock() != caseend:
    caseend.moveBasicBlockAfter(f.getLastBasicBlock())

  if load and result != nil:
    result = g.b.buildLoadValue(result)

proc genNodeConstDef(g: LLGen, n: PNode) =
  # TODO generate lazily
  for s in n.sons: p("genConstDefStmt", s, g.depth + 1)

  let vn = n[0]
  let v = vn.sym

  if v.typ.containsCompileTimeOnly: return
  if lfNoDecl in v.loc.flags: return
  if v.typ.kind notin ConstantDataTypes: return

  let init = v.ast

  if init.isDeepConstExprLL(true):
    let ci = g.genConstInitializer(init)
    if ci == nil: internalError(n.info, "Unable to generate const initializer: " & $init)

    let x = g.genGlobal(vn)
    x.setGlobalConstant(llvm.True)

    # TODO when enabled, ptr equality checks (for example in isObj) get
    #      optimized away - need to consider when it's safe
    # x.setUnnamedAddr(llvm.True)

    case v.typ.kind
    of tyArray, tySet, tyTuple: x.setInitializer(ci)
    else:
      let c = g.m.addPrivateConstant(ci.typeOf(), nn(".const.init", n))
      c.setInitializer(ci)
      x.setInitializer(llvm.constBitCast(c, x.typeOf().getElementType()))
    return

  var x: llvm.ValueRef
  if sfGlobal in v.flags:
    x = g.genGlobal(vn)
    g.registerGcRoot(v, x)
  else:
    x = g.b.localAlloca(g.llType(v.typ), v.llName)
    # Some initializers expect value to be null, so we always set it so
    g.buildStoreNull(x)

  if init.kind != nkEmpty:
    g.genAssignment(init, x, v.typ)

  # Put in scope only once init is finished (init might refence
  # a var with the same name)
  if sfGlobal notin v.flags:
    g.f.scopePut(v.id, x)

proc genNodeTryStmt(g: LLGen, n: PNode) =
  # create safe point
  let tsp = magicsys.getCompilerProc("TSafePoint").typ
  let spt = g.llStructType(tsp)

  let sp = g.b.localAlloca(spt, nn("sp", n))
  g.buildStoreNull(sp)
  discard g.callCompilerProc("pushSafePoint", [sp])

  g.f.scopePush(nil, nil)

  # call setjmp
  let setjmp = g.m.getOrInsertFunction("_setjmp", llSetjmpType)

  let contextP = g.b.buildGEP(sp, [gep0, constGEPIdx(2)])
  let res = g.b.buildCall(setjmp, [g.b.buildBitCast(contextP, llJmpbufp, "")])

  let statusP = g.b.buildGEP(sp, [gep0, constGEPIdx(1)])
  discard g.b.buildStore(g.b.buildNimIntExt(res, false), statusP)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let sjok = f.appendBasicBlock(nn("sj.ok", n))
  let sjexc = f.appendBasicBlock(nn("sj.exc", n))
  let sjend = f.appendBasicBlock(nn("sj.end", n))

  # see if we're returning normally or from a longjmp
  let cmp = g.b.buildICmp(llvm.IntEQ, res, constCInt(0), "")
  discard g.b.buildCondBr(cmp, sjok, sjexc)

  g.b.positionBuilderAtEnd(sjok)

  g.f.scopePush(nil, nil)
  g.f.nestedTryStmts.add(n)

  g.genNode(n[0])

  discard g.callCompilerProc("popSafePoint", [])
  g.f.scopePop()

  discard g.b.buildBr(sjend)

  g.b.positionBuilderAtEnd(sjexc)
  g.f.scopePush(nil, nil)

  discard g.callCompilerProc("popSafePoint", [])

  inc g.f.inExceptBlock
  var i = 1
  let length = n.sonsLen
  while (i < length) and (n[i].kind == nkExceptBranch):
    let b = n[i]
    inc(i)

    let blen = b.sonsLen
    if blen == 1:
      # catch-all
      discard g.b.buildStore(ni0, statusP)
      g.genNode(b[0])

      discard g.callCompilerProc("popCurrentException", [])
      discard g.b.buildBr(sjend)
    else:
      let sjfound = f.appendBasicBlock(nn("sj.found", n))
      let sjnext =
        if (i < length) and (n[i].kind == nkExceptBranch): f.appendBasicBlock(nn("sj.next", n))
        else: sjend
      # catch one or more types
      for j in 0..blen - 2:
        assert(b[j].kind == nkType)

        let exc = g.callCompilerProc("getCurrentException", [])
        let m_type = g.b.buildLoad(
          g.b.buildGEP(exc, [gep0, gep0, gep0]), "")
        let ti = g.genTypeInfo(b[j].typ)
        let found = g.b.buildI1(g.callCompilerProc("isObj", [m_type, ti]))

        if j == blen - 2:
          discard g.b.buildCondBr(found, sjfound, sjnext)
          g.b.positionBuilderAtEnd(sjnext)
        else:
          let sjor = f.appendBasicBlock(nn("sj.or", n))
          sjor.moveBasicBlockBefore(sjfound)
          discard g.b.buildCondBr(found, sjfound, sjor)
          g.b.positionBuilderAtEnd(sjor)

      g.b.positionBuilderAtEnd(sjfound)
      discard g.b.buildStore(ni0, statusP)
      g.genNode(b[blen-1])
      discard g.callCompilerProc("popCurrentException", [])

      discard g.b.buildBr(sjend)

      g.b.positionBuilderAtEnd(sjnext)

  dec g.f.inExceptBlock
  discard pop(g.f.nestedTryStmts)
  g.f.scopePop()

  if i == 1:
    # finally without catch!
    discard g.b.buildBr(sjend)
  g.b.positionBuilderAtEnd(sjend)

  if i < length and n[i].kind == nkFinally:
    g.f.finallySafePoints.add(sp)
    g.f.scopePush(nil, nil)
    g.genNode(n[i][0])
    g.f.scopePop()
    discard g.f.finallySafePoints.pop()

  # TODO is the load needed?
  # TODO is this needed if we have a catch-all?
  let s = g.b.buildLoad(statusP, "")
  let sjrr = f.appendBasicBlock(nn("sj.rr", n))
  let sjnm = f.appendBasicBlock(nn("sj.nomore", n))
  # In case it wasn't handled...
  let scmp = g.b.buildICmp(llvm.IntNE, s, ni0, "")
  discard g.b.buildCondBr(scmp, sjrr, sjnm)
  g.b.positionBuilderAtEnd(sjrr)
  discard g.callCompilerProc("reraiseException", [])
  # TODO get rid of br somehow? reraise shoudn't return
  discard g.b.buildBr(sjnm)

  g.b.positionBuilderAtEnd(sjnm)
  sjend.moveBasicBlockBefore(sjrr)
  g.f.scopePop()

proc genNodeRaiseStmt(g: LLGen, n: PNode) =
  if g.f.inExceptBlock > 0:
    let finallyBlock = g.f.nestedTryStmts[g.f.nestedTryStmts.len - 1].lastSon
    if finallyBlock.kind == nkFinally:
      g.f.scopePush(nil, nil)
      g.genNode(finallyBlock.sons[0])
      g.f.scopePop()

  if n[0].kind != nkEmpty:
    let ax = g.genNode(n[0], true)

    let typ = skipTypes(n[0].typ, abstractPtrs)
    let name = g.b.buildGlobalStringPtr(typ.sym.name.s, "raise." & typ.sym.name.s)
    discard g.callCompilerProc("raiseException", [ax, name])
  else:
    discard g.callCompilerProc("reraiseException", [])

proc blockLeave(g: LLGen, howManyTrys, howManyExcepts: int) =
  var stack: seq[PNode] = @[]

  var alreadyPoppedCnt = g.f.inExceptBlock
  for i in 1..howManyTrys:
    if alreadyPoppedCnt > 0:
      dec alreadyPoppedCnt
    else:
      discard g.callCompilerProc("popSafePoint", [])

    let tryStmt = g.f.nestedTryStmts.pop
    stack.add(tryStmt)

    let finallyStmt = lastSon(tryStmt)
    if finallyStmt.kind == nkFinally:
      g.genNode(finallyStmt[0])

  for i in countdown(howManyTrys-1, 0):
    g.f.nestedTryStmts.add(stack[i])

  for i in countdown(howManyExcepts-1, 0):
    discard g.callCompilerProc("popCurrentException", [])

proc genNodeReturnStmt(g: LLGen, n: PNode) =
  if (n[0].kind != nkEmpty):
    g.genNode(n[0])

  g.blockLeave(g.f.nestedTryStmts.len, g.f.inExceptBlock)

  if (g.f.finallySafePoints.len > 0):
    let sp = g.f.finallySafePoints[g.f.finallySafePoints.len-1]
    let statusP = g.b.buildGEP(sp, [gep0, constGEPIdx(1)])

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let s = g.b.buildLoad(statusP, "")
    let retpop = f.appendBasicBlock(nn("ret.pop", n))
    let retdone = f.appendBasicBlock(nn("ret.done", n))
    let scmp = g.b.buildICmp(llvm.IntNE, s, ni0, "")
    discard g.b.buildCondBr(scmp, retpop, retdone)
    g.b.positionBuilderAtEnd(retpop)
    discard g.callCompilerProc("popCurrentException", [])
    discard g.b.buildBr(retdone)
    g.b.positionBuilderAtEnd(retdone)

  discard g.b.buildBr(g.f.ret)

  # Sometimes, there might be dead code after the return statement.. as a hack
  # we add a block that will have no predecessors, to avoid dealing with it
  # elsewhere
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let cont = f.appendBasicBlock(nn("return.dead", n))
  g.b.positionBuilderAtEnd(cont)

proc genNodeBreakStmt(g: LLGen, n: PNode) =
  p("b", n[0], g.depth)

  if n[0].kind == nkEmpty:
    internalError(n.info, "Unexpected nkBreakStmt with nkEmpty")

  let idx = g.f.scopeIdx(n[0].sym)
  if idx == -1:
    internalError(n.info, "Scope not found: " & $n[0].sym)

  let s = g.f.scope[idx]

  g.blockLeave(
    g.f.nestedTryStmts.len - s.nestedTryStmts,
    g.f.inExceptBlock - s.nestedExceptStmts)

  # similar to return, there might be more stuff after a break that messes
  # things up - we add an extra block just in case
  # TODO: one case when this happens is with finally blocks, which are
  # currently broken anyway, but this way at least the generated bytecode is
  # valid
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let cont = f.appendBasicBlock(nn("block.cont", n))
  if s.exit == nil:
    s.exit = f.appendBasicBlock(nn("block.exit", n))
  discard g.b.buildBr(s.exit)
  g.b.positionBuilderAtEnd(cont)

proc genNodeBlockStmt(g: LLGen, n: PNode) =
  g.f.scopePush(n[0], nil)
  g.genNode(n[1])
  let scope = g.f.scopePop()

  if scope.exit != nil:
    g.b.buildBrFallthrough(scope.exit)
    g.b.positionAndMoveToEnd(scope.exit)

proc genNodeDiscardStmt(g: LLGen, n: PNode) =
  if n[0].kind != nkEmpty:
    discard g.genNode(n[0], true)

proc genNodeStmtListExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let length = n.sonsLen
  for i in 0..length - 2:
    g.genNode(n[i])
  if length > 0:
    result = g.genNode(n[length - 1], load)

proc genNodeBlockExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  g.f.scopePush(n[0], nil)
  result = g.genNode(n[1], load)
  let scope = g.f.scopePop()

  if scope.exit != nil:
    g.b.buildBrFallthrough(scope.exit)
    g.b.positionAndMoveToEnd(scope.exit)

proc genNodeClosure(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true)

  # TODO workaround for how params are loaded by nkSym - this needs a review
  if bx.typeOf().getElementType().getTypeKind() == llvm.PointerTypeKind:
    bx = g.b.buildLoad(bx, nn("clox.hack", n))

  let
    a = g.b.buildBitCast(ax, llVoidPtrType, nn("clox.ptr", n))
    b = g.b.buildBitCast(bx, llVoidPtrType, nn("clox.env", n))
  result = g.b.localAlloca(llClosureType, nn("clox.res", n))

  discard g.b.buildStore(a, g.b.buildGEP(result, [gep0, gep0]))
  discard g.b.buildStore(b, g.b.buildGEP(result, [gep0, gep1]))

  if load:
    result = g.b.buildLoad(result, nn("load.clox.res", n))

proc genNodeGotoState(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], true)

  let l = n[0].typ.lastOrd()

  g.f.scope[g.f.scope.len - 1].goto = g.b.buildSwitch(ax, g.f.ret, (l + 1).cuint)

proc genNodeState(g: LLGen, n: PNode) =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let state = f.appendBasicBlock(nn("state." & $n[0].intVal, n))
  g.b.buildBrFallthrough(state)
  g.b.positionBuilderAtEnd(state)
  for i in 0..g.f.scope.len-1:
    if g.f.scope[g.f.scope.len - i - 1].goto != nil:
      g.f.scope[g.f.scope.len - i - 1].goto.addCase(constNimInt(n[0].intVal.int), state)
      break

proc genNodeBreakState(g: LLGen, n: PNode) =
  # TODO C code casts to int* and reads second value.. uh, I guess we should be
  # able to do better
  var ax: llvm.ValueRef
  if n[0].kind == nkClosure:
    ax = g.genNode(n[0][1], false)
  else:
    ax = g.genNode(n[0], false)
    ax = g.b.buildGEP(ax, [gep0, gep1])

  ax = g.b.buildLoad(ax, nn("load.state.break", n))
  ax = g.b.buildBitCast(ax, llIntType.pointerType(), nn("state.break.intptr", n))
  let s = g.b.buildLoad(g.b.buildGEP(ax, [gep1]), nn("state.break.s", n))
  let cmp = g.b.buildICmp(llvm.IntSLT, s, ni0, nn("state.break.cmp", n))
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let state = f.appendBasicBlock(nn("state.break.cont", n))

  # nkBreakState happens in a loop, so we need to find the end of that loop...
  var whileExit: llvm.BasicBlockRef
  for i in 0..g.f.scope.len-1:
    if g.f.scope[g.f.scope.len - i - 1].exit != nil:
      whileExit = g.f.scope[g.f.scope.len - i - 1].exit
      break

  if whileExit == nil:
    internalError(n.info, "no enclosing while loop in nkBreakState")

  discard g.b.buildCondBr(cmp, whileExit, state)
  g.b.positionBuilderAtEnd(state)

proc genSons(g: LLGen, n: PNode) =
  for s in n: g.genNode(s)

proc genNode(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  p(if load: "l" else: "p", n, g.depth)

  g.depth += 1

  g.debugUpdateLoc(n)

  case n.kind
  of nkEmpty: discard
  of nkSym: result = g.genNodeSym(n, load)
  of nkCharLit..nkUInt64Lit: result = g.genNodeIntLit(n)
  of nkFloatLit..nkFloat128Lit: result = g.genNodeFloatLit(n)
  of nkStrLit..nkTripleStrLit: result = g.genNodeStrLit(n, load)
  of nkNilLit: result = g.genNodeNilLit(n, load)
  of nkCallKinds: result = g.genNodeCall(n, load)
  of nkExprColonExpr: result = g.genNode(n[1], load)
  of nkIdentDefs: g.genNodeIdentDefs(n)
  of nkVarTuple: g.genNodeVarTuple(n)
  of nkPar: result = g.genNodePar(n, load)
  of nkObjConstr: result = g.genNodeObjConstr(n, load)
  of nkCurly: result = g.genNodeCurly(n, load)
  of nkBracket: result = g.genNodeBracket(n, load)
  of nkBracketExpr: result = g.genNodeBracketExpr(n, load)
  of nkDotExpr: result = g.genNodeDot(n, load)
  of nkCheckedFieldExpr: result = g.genNodeCheckedField(n, load)
  of nkDerefExpr, nkHiddenDeref: result = g.genNodeDeref(n, load)
  of nkIfExpr: result = g.genNodeIfExpr(n, load)
  of nkLambda, nkDo: result = g.genNodeLambda(n)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv: result = g.genNodeConv(n, load)
  of nkCast: result = g.genNodeCast(n, load)
  of nkAddr, nkHiddenAddr: result = g.genNodeAddr(n)
  of nkObjDownConv: result = g.genNodeObjDownConv(n, load)
  of nkObjUpConv: result = g.genNodeObjUpConv(n, load)
  of nkChckRangeF: result = g.genNodeChckRange(n, "chckRangeF")
  of nkChckRange64: result = g.genNodeChckRange(n, "chckRange64")
  of nkChckRange: result = g.genNodeChckRange(n, "chckRange")
  of nkStringToCString: result = g.genNodeStringToCString(n)
  of nkCStringToString: result = g.genMagicToStr(n, "cstrToNimstr")
  of nkAsgn: g.genNodeAsgn(n)
  of nkFastAsgn: g.genNodeFastAsgn(n)
  of nkProcDef, nkMethodDef, nkConverterDef: g.genNodeProcDef(n)
  of nkPragma:
    for s in n:
      let p = whichPragma(s)
      case p
      of wAsm, wEmit:
        internalError(n.info, substr($p, 1) & " pragma not supported")
      else: discard
  of nkPragmaBlock: result = g.genNode(n.lastSon, load)
  of nkIfStmt: result = g.genNodeIfStmt(n) # if in case! see tcaststm.nim
  of nkWhenStmt: result = g.genNodeWhenStmt(n, load)
  of nkWhileStmt: g.genNodeWhileStmt(n)
  of nkCaseStmt: result = g.genNodeCaseStmt(n, load)  # Sometimes seen as expression!
  of nkVarSection, nkLetSection, nkConstSection: g.genSons(n)
  of nkConstDef: g.genNodeConstDef(n)  # do we need something else?
  of nkTryStmt: g.genNodeTryStmt(n)
  of nkRaiseStmt: g.genNodeRaiseStmt(n)
  of nkReturnStmt: g.genNodeReturnStmt(n)
  of nkBreakStmt: g.genNodeBreakStmt(n)
  of nkBlockStmt: g.genNodeBlockStmt(n)
  of nkDiscardStmt: g.genNodeDiscardStmt(n)
  of nkStmtList: g.genSons(n)
  of nkStmtListExpr: result = g.genNodeStmtListExpr(n, load)
  of nkBlockExpr: result = g.genNodeBlockExpr(n, load)
  of nkClosure: result = g.genNodeClosure(n, load)
  of nkGotoState: g.genNodeGotoState(n)
  of nkState: g.genNodeState(n)
  of nkBreakState: g.genNodeBreakState(n)

  of nkTypeSection, nkCommentStmt, nkIteratorDef, nkIncludeStmt,
     nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt,
     nkFromStmt, nkTemplateDef, nkMacroDef: discard
  else:
    internalError(n.info, "Unhandled node: " & $n)
  g.depth -= 1

proc newLLFunc(ret: llvm.BasicBlockRef): LLFunc =
  new(result)
  result.vars = initTable[int, llvm.ValueRef]()
  result.scope = @[]
  result.options = gOptions
  result.ret = ret
  result.nestedTryStmts = @[]
  result.finallySafePoints = @[]

proc newLLGen(s: PSym): LLGen =
  new(result)
  let name = fileInfos[gProjectMainIdx].shortName

  result.m = llvm.moduleCreateWithName(name)
  result.b = llvm.createBuilder()
  result.values = initTable[int, llvm.ValueRef]()
  result.gmarkers = initTable[int, llvm.ValueRef]()
  result.markers = initTable[SigHash, llvm.ValueRef]()
  result.nodeInfos = initTable[SigHash, llvm.ValueRef]()
  result.typeInfos = initTable[SigHash, llvm.ValueRef]()
  result.types = initTable[SigHash, llvm.TypeRef]()
  result.syms = @[]
  result.markerBody = @[]
  result.sigConflicts = initCountTable[SigHash]()

  if optCDebug in gGlobalOptions:
    let d = nimDIBuilderCreate(result.m)
    result.d = d
    result.dfiles = initTable[int, llvm.NimMetadataRef]()
    result.dscopes = initTable[int, llvm.NimMetadataRef]()
    result.dstructs = initTable[SigHash, llvm.NimMetadataRef]()
    let df = result.debugGetFile(gProjectMainIdx)
    result.dcu = d.nimDIBuilderCreateCompileUnit(
      2, df, "", false, "", 0, "")

    var g = result
    proc add(ty: TTypeKind, n: cstring, sz: uint64, enc: cuint) =
      g.dtypes[ty] = d.nimDIBuilderCreateBasicType(n, sz, enc)

    add(tyBool, "bool", 8, DW_ATE_boolean)
    add(tyChar, "char", 8, DW_ATE_unsigned_char)
    result.dtypes[tyPointer] = d.nimDIBuilderCreatePointerType(
      g.dtypes[tyChar], 64, 64, "pointer")

    add(tyInt, "int", 64, DW_ATE_signed)
    add(tyInt8, "int8", 8, DW_ATE_signed)
    add(tyInt16, "int16", 16, DW_ATE_signed)
    add(tyInt32, "int32", 32, DW_ATE_signed)
    add(tyInt64, "int64", 64, DW_ATE_signed)
    add(tyFloat, "float", 32, DW_ATE_float)
    add(tyFloat32, "float32", 32, DW_ATE_float)
    add(tyFloat64, "float64", 64, DW_ATE_float)
    add(tyFloat128, "float128", 128, DW_ATE_float)
    add(tyUInt, "uint", 64, DW_ATE_unsigned)
    add(tyUInt8, "uint8", 8, DW_ATE_unsigned)
    add(tyUInt16, "uint16", 16, DW_ATE_unsigned)
    add(tyUInt32, "uint32", 32, DW_ATE_unsigned)
    add(tyUInt64, "uint64", 64, DW_ATE_unsigned)

    g.dtypes[tyCString] = g.d.nimDIBuilderCreatePointerType(
      g.dtypes[tyChar], 64, 64, "")

proc genMain(g: LLGen) =
  let llMainType = llvm.functionType(llCIntType, [llCIntType, llCStringType.pointerType()])

  let f = g.m.addFunction("main", llMainType)

  let b = f.appendBasicBlock(nn("entry"))
  g.b.positionBuilderAtEnd(b)


  if g.d != nil:
    let types = [
      g.dtypes[tyInt32],
      g.dtypes[tyInt32],
      g.d.nimDIBuilderCreatePointerType(
        g.d.nimDIBuilderCreatePointerType(g.dtypes[tyChar], 64, 64, ""),
        64, 64, "")
    ]
    let arr = g.d.nimDIBuilderGetOrCreateArray(types)
    let scope = g.debugFunction(nil, arr, f)

    let dl = llvm.getGlobalContext().nimDIBuilderCreateDebugLocation(
        0, 0, scope, nil)
    g.b.setCurrentDebugLocation(dl)

    let f0 = f.getFirstParam()
    let f1 = f0.getNextParam()
    let argc = g.b.buildAlloca(f0.typeOf(), nn("argc"))
    let argv = g.b.buildAlloca(f1.typeOf(), nn("argv"))
    discard g.b.buildStore(f0, argc)
    discard g.b.buildStore(f1, argv)

    let vd0 = g.d.nimDIBuilderCreateVariable(
      0, scope, "argc", g.debugGetFile(gProjectMainIdx), 0, g.dtypes[tyInt],
      false, 0, 1, 0)
    discard g.d.nimDIBuilderInsertDeclareAtEnd(argc, vd0, nil, 0,
      dl, g.b.getInsertBlock())

    let vd1 = g.d.nimDIBuilderCreateVariable(
      0, scope, "argv", g.debugGetFile(gProjectMainIdx), 0,
      g.d.nimDIBuilderCreatePointerType(g.dtypes[tyCString], 64, 64, ""),
      false, 0, 2, 0)
    discard g.d.nimDIBuilderInsertDeclareAtEnd(argv, vd1, nil, 0,
      dl, g.b.getInsertBlock())

  if platform.targetOS != osStandAlone and gSelectedGC != gcNone:
    let bottom = g.b.buildAlloca(llIntType, nn("bottom"))
    discard g.callCompilerProc("initStackBottomWith", [bottom])

  let cmdLine = g.m.getNamedGlobal("cmdLine")
  if cmdLine != nil:
    cmdLine.setLinkage(llvm.CommonLinkage)
    cmdLine.setInitializer(llvm.constNull(cmdLine.typeOf().getElementType()))
    discard g.b.buildStore(g.b.buildBitCast(f.getParam(1), cmdLine.typeOf().getElementType(), ""), cmdLine)

  let cmdCount = g.m.getNamedGlobal("cmdCount")
  if cmdCount != nil:
    cmdCount.setLinkage(llvm.CommonLinkage)
    cmdCount.setInitializer(llvm.constNull(cmdCount.typeOf().getElementType()))
    discard g.b.buildStore(f.getParam(0), cmdCount)

  discard g.b.buildCall(g.init.ret.getBasicBlockParent(), [], "")

  discard g.b.buildRet(constInt(llCIntType, 0, False))

proc loadBase(g: LLGen) =
  let m = parseIRInContext(
    llctxt, options.gPrefixDir / "../nlvm-lib/nlvmbase-linux-amd64.ll")

  if g.m.linkModules2(m) != 0:
    internalError("module link failed")

proc runOptimizers(g: LLGen) =
  if {optOptimizeSpeed, optOptimizeSize} * gOptions == {}:
    return

  let pmb = llvm.passManagerBuilderCreate()

  if optOptimizeSize in gOptions:
    pmb.passManagerBuilderSetOptLevel(2)
    pmb.passManagerBuilderSetSizeLevel(2)
    # pmb.passManagerBuilderUseInlinerWithThreshold(25)
  else:
    pmb.passManagerBuilderSetOptLevel(3)
    pmb.passManagerBuilderSetSizeLevel(0)
    # pmb.passManagerBuilderUseInlinerWithThreshold(275)

  let fpm = g.m.createFunctionPassManagerForModule()
  let mpm = llvm.createPassManager()

  pmb.passManagerBuilderPopulateFunctionPassManager(fpm)
  pmb.passManagerBuilderPopulateModulePassManager(mpm)

  var f = g.m.getFirstFunction()
  while f != nil:
    discard fpm.runFunctionPassManager(f)
    f = f.getNextFunction()
  discard mpm.runPassManager(g.m)

  fpm.disposePassManager()
  mpm.disposePassManager()

proc writeOutput(g: LLGen, project: string) =
  var outFile: string
  if options.outFile.len > 0:
    if options.outFile.isAbsolute:
      outFile = options.outFile
    else:
      outFile = getCurrentDir() / options.outFile
  else:
    if optCompileOnly in gGlobalOptions:
      outFile = project & ".ll"
    elif optNoLinking in gGLobalOptions:
      outFile = project & ".o"
    else:
      outFile = project

  initializeX86AsmPrinter()
  initializeX86Target()
  initializeX86TargetInfo()
  initializeX86TargetMC()

  let triple = llvm.getDefaultTargetTriple()

  var tr: llvm.TargetRef
  discard getTargetFromTriple(triple, addr(tr), nil)

  var reloc = llvm.RelocDefault
  if optGenDynLib in gGlobalOptions and
      ospNeedsPIC in platform.OS[targetOS].props:
    reloc = llvm.RelocPIC

  let cgl =
    if optOptimizeSpeed in gOptions: llvm.CodeGenLevelAggressive
    else: llvm.CodeGenLevelDefault

  let tm = createTargetMachine(tr, triple, "", "", cgl,
    reloc, llvm.CodeModelDefault)

  let layout = tm.createTargetDataLayout()
  g.m.setModuleDataLayout(layout)
  g.m.setTarget(triple)

  g.runOptimizers()

  if optCompileOnly in gGlobalOptions:
    discard g.m.printModuleToFile(outfile, nil)
    return

  let ofile =
    if optNoLinking in gGlobalOptions:
      outFile
    else:
      completeCFilePath(project & ".o")

  var err: cstring
  if llvm.targetMachineEmitToFile(tm, g.m, ofile, llvm.ObjectFile,
    cast[cstringArray](addr(err))) == llvm.True:
    internalError($err[0])
    return

  if optNoLinking in gGlobalOptions:
    return

  # the c generator loads libraries using dlopen/dlsym/equivalent, which nlvm
  # doesn't support, so here, we add a few libraries..
  cLinkedLibs.add("pcre")

  addExternalFileToLink(ofile)

  # Linking is a horrible mess - let's reuse the c compiler for now
  callCCompiler(project)

proc myClose(graph: ModuleGraph, b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  p("Close", n, 0)

  let g = LLGen(b)

  g.genNode(n)

  let s = g.syms[^1]

  if sfCompileToCpp in s.getModule().flags:
    internalError("Compile-to-c++ not supported (did you use importcpp?)")

  for m in g.markerBody:
    g.genMarkerProcBody(m[0], m[1])
  g.markerBody.setLen(0)

  if sfMainModule notin s.flags:
    for m in g.markerBody:
      g.genMarkerProcBody(m[0], m[1])
    g.markerBody.setLen(0)
    discard g.syms.pop()
    return n

  # return from nlvmInit

  g.debugUpdateLoc(n)
  g.b.buildBrFallthrough(g.f.ret)
  g.b.positionBuilderAtEnd(g.f.ret)

  discard g.b.buildRetVoid()

  let fn = g.f.ret.getBasicBlockParent()
  if fn.getLastBasicBlock() != g.f.ret:
    g.f.ret.moveBasicBlockAfter(fn.getLastBasicBlock())

  g.b.finalize(g.f)

  g.genMain()

  var disp = generateMethodDispatchers(graph)
  for i in 0..<disp.sonsLen:
    discard g.genFunctionWithBody(disp[i].sym)

  for m in g.markerBody:
    g.genMarkerProcBody(m[0], m[1])
  g.markerBody.setLen(0)

  g.loadBase()

  if g.d != nil:
    g.d.nimDIBuilderFinalize()

    # Magic string, see https://groups.google.com/forum/#!topic/llvm-dev/1O955wQjmaQ
    g.m.nimAddModuleFlag("Debug Info Version", nimDebugMetadataVersion())
  g.writeOutput(changeFileExt(gProjectFull, ""))

  result = n
  discard g.syms.pop()

  if g.d != nil:
    g.d.nimDIBuilderDispose()
    g.d = nil

  g.m.disposeModule()

  uglyGen = nil

proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  p("Process", n, 0)

  let g = LLGen(b)
  g.f.options = gOptions

  g.genNode(n)

  result = n

proc myOpenCached(graph: ModuleGraph, s: PSym, rd: PRodReader): PPassContext =
  result = nil

proc myOpen(graph: ModuleGraph, s: PSym, cache: IdentCache): PPassContext =
  # In the C generator, a separate C file is generated for every module,
  # but the rules governing what goes into which module are shrouded by a
  # layer of globals and conditionals.
  # Rather than deciphering all that, we simply generate a single module
  # with all the code in it, like the JS generator does.

  p("Opening", s, 0)
  if uglyGen == nil:
    let g = newLLGen(s)

    let init = g.m.addFunction(".nlvmInit", llInitFuncType)

    let b = llvm.appendBasicBlock(init, nn("entry", s))
    g.b.positionBuilderAtEnd(b)

    g.f = newLLFunc(llvm.appendBasicBlock(init, nn("return")))

    if g.d != nil:
      let arr = g.d.nimDIBuilderGetOrCreateArray([])
      g.f.ds = g.debugFunction(s, arr, init)

    g.f.scopePush(nil, g.f.ret)

    g.init = g.f

    uglyGen = g
  else:
    let g = uglyGen
    let entry = llvm.appendBasicBlock(g.init.ret.getBasicBlockParent(), nn("entry", s))
    g.b.buildBrFallthrough(entry)
    g.b.positionBuilderAtEnd(entry)

  uglyGen.syms.add(s)

  result = uglyGen

const llgenPass* = makePass(myOpen, myOpenCached, myProcess, myClose)
