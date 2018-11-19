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
  compiler/[
    ast,
    astalgo,
    bitsets,
    cgmeth,
    ccgutils,
    extccomp,
    idents,
    lineinfos,
    lowerings,
    magicsys,
    modulegraphs,
    msgs,
    nimsets,
    options,
    passes,
    pathutils,
    platform,
    ropes,
    semparallel,
    sighashes,
    transf,
    trees,
    types,
    wordrecg
  ],
  llvm/llvm

type
  LLScope = ref object
    n: PNode
    exit: llvm.BasicBlockRef
    goto: llvm.ValueRef
    nestedTryStmts: int
    nestedExceptStmts: int

  LLFunc = ref object
    scope: seq[LLScope]
    ret: llvm.BasicBlockRef
    options: TOptions
    nestedTryStmts*: seq[tuple[n: PNode, inExcept: bool]]
    finallySafePoints: seq[llvm.ValueRef]
    loopNesting: int
    init: llvm.BasicBlockRef
    clenv: llvm.ValueRef
    ds: llvm.MetadataRef

  LLModule = ref object of PPassContext
    ## One LLModule per .nim file (module)
    ## Notably though, we don't create an LLVM module per nim module - instead,
    ## everything is dumped one giant thing and compiled as if by LTO
    g: LLGen

    sym: PSym

  LLGen = ref object of RootObj
    ## One LLGen per compile / project
    graph: ModuleGraph
    lc: llvm.ContextRef
    m: llvm.ModuleRef
    b: llvm.BuilderRef
    d: llvm.DIBuilderRef
    f: LLFunc

    # Cached types
    primitives: array[TTypeKind, llvm.TypeRef]
    cintType: llvm.TypeRef
    csizetType: llvm.TypeRef
    seqBaseType: llvm.TypeRef
    closureType: llvm.TypeRef
    procPtrType: llvm.TypeRef
    voidPtrType: llvm.TypeRef
    nimStringDescType: llvm.TypeRef
    nimStringDescPtrType: llvm.TypeRef
    jmpBufType: llvm.TypeRef

    gep0: llvm.ValueRef
    gep1: llvm.ValueRef
    ni0: llvm.ValueRef
    nb: array[bool, llvm.ValueRef]

    attrNoInline: AttributeRef
    attrNoReturn: AttributeRef

    symbols: Table[int, llvm.ValueRef]
    gmarkers: Table[int, llvm.ValueRef]
    markers: Table[SigHash, llvm.ValueRef]
    nodeInfos: Table[SigHash, llvm.ValueRef]
    typeInfos: Table[SigHash, llvm.ValueRef]
    types: Table[SigHash, llvm.TypeRef]

    depth: int

    init: LLFunc

    module: LLModule

    markerBody: seq[tuple[v: llvm.ValueRef, typ: PType]]  # Markers looking for a body

    sigConflicts: CountTable[SigHash]

    # Debug stuff
    dfiles: Table[int, llvm.MetadataRef]
    dscopes: Table[int, llvm.MetadataRef]
    dtypes: array[TTypeKind, llvm.MetadataRef]
    dstructs: Table[SigHash, llvm.MetadataRef]

    # Compile unit
    dcu: llvm.MetadataRef
    dbgKind: cuint

    # target specific stuff
    tgt: string
    tgtExportLinkage: llvm.Linkage

template config(g: LLGen): untyped = g.graph.config
template fileInfos(g: LLGen): untyped = g.config.m.fileInfos

# Helpers
proc newLLFunc(g: LLGen, ret: llvm.BasicBlockRef): LLFunc
proc llType(g: LLGen, typ: PType): llvm.TypeRef
proc fieldIndex(g: LLGen, typ: PType, sym: PSym): seq[int]
proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef)
proc callErrno(g: LLGen, prefix: string): llvm.ValueRef
proc callCompilerProc(g: LLGen, name: string, args: openarray[llvm.ValueRef]): llvm.ValueRef

proc genFunction(g: LLGen, s: PSym): llvm.ValueRef
proc genFunctionWithBody(g: LLGen, s: PSym): llvm.ValueRef

# Magic expressions
proc genMagicLength(g: LLGen, n: PNode): llvm.ValueRef

# Node handling
proc genNode(g: LLGen, n: PNode, load: bool = false): llvm.ValueRef {.discardable.}

proc nn(g: LLGen, s: string, li: TLineInfo): string =
  # later on, we'll just return an empty string here
  let x = int(g.fileInfos.high)
  let fn =
    if int(li.fileIndex) in {0..x}: "." & g.fileInfos[int(li.fileIndex)].shortName
    else: ""

  let ln = if li.line >= 0.uint16: "." & $li.line else: ""
  let col = if li.col >= 0: "." & $li.col else: ""

  result = s & fn & ln & col

proc nn(g: LLGen, s: string): string = s
proc nn(g: LLGen, s: string, n: PNode): string =
  if n == nil: g.nn(s) else: g.nn(s, n.info)
proc nn(g: LLGen, s: string, sym: PSym): string =
  if sym == nil: g.nn(s) else: g.nn(s, sym.info)
proc nn(g: LLGen, s: string, v: llvm.ValueRef): string =
  var vn = v.getValueName()
  if vn == nil: vn = ""
  result = s & "." & $vn

proc inExceptBlockLen(p: LLFunc): int =
  for x in p.nestedTryStmts:
    if x.inExcept: result.inc

proc scopePush(f: LLFunc, n: PNode, exit: llvm.BasicBlockRef) =
  f.scope.add(
    LLScope(n: n, exit: exit, nestedTryStmts: f.nestedTryStmts.len,
      nestedExceptStmts: f.inExceptBlockLen))

proc scopeIdx(f: LLFunc, sym: PSym): int =
  for i in 0..f.scope.len - 1:
    let scope = f.scope[f.scope.len - 1 - i]
    if scope.n != nil and scope.n.kind == nkSym and scope.n.sym == sym:
      return f.scope.len - 1 - i

  return -1

proc scopePop(f: LLFunc): LLScope {.discardable.} =
  f.scope.pop

proc getInitBlock(g: LLGen, llf: LLFunc): llvm.BasicBlockRef =
  if llf.init == nil:
    let pre = llf.ret.getBasicBlockParent()
    llf.init = pre.appendBasicBlock(g.nn("init"))
  result = llf.init

proc positionAndMoveToEnd(b: llvm.BuilderRef, bl: llvm.BasicBlockRef) =
  bl.moveBasicBlockAfter(b.getInsertBlock().getBasicBlockParent().getLastBasicBlock())
  b.positionBuilderAtEnd(bl)

proc localAlloca(g: LLGen, typ: llvm.TypeRef, name: string): llvm.ValueRef =
  # alloca will allocate memory on the stack that will be released at function
  # exit - thus for a variable local to a loop we would actually be creating a
  # new location for every iteration. Thus, we make space for all locals at
  # the start of the function.
  # This also helps with another degenerate case:
  # in a moment of truimph, code like `if a and (let m = 1; m != 1): ...` was
  # allowed, creating a problematic scope for m - it should ideally only exist
  # when a is true, but by putting it at the entry, it lives through the
  # the expression (with an undef value until the right branch is taken)

  let b = g.b
  let pre = b.getInsertBlock()
  let dbg = b.getCurrentDebugLocation()
  if g.d != nil: b.setCurrentDebugLocation(nil)
  let f = pre.getBasicBlockParent()
  var entry = f.getEntryBasicBlock()

  var first = entry.getFirstInstruction()
  if first == nil or first.getInstructionOpcode() != llvm.Alloca:
    # Allocate a specific alloca block and place it first in the function
    let newEntry = g.lc.insertBasicBlockInContext(entry, g.nn("alloca"))
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
  if g.d != nil: b.setCurrentDebugLocation(dbg)

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

proc typeName(typ: PType): Rope =
  let typ = typ.skipTypes(irrelevantForBackend)
  result =
    if typ.sym != nil and typ.kind in {tyObject, tyEnum}:
      rope($typ.kind & '_' & typ.sym.name.s.mangle)
    else:
      rope($typ.kind)

proc llName(g: LLGen, typ: PType, sig: SigHash): string =
  # getTypeName from ccgtypes.nim
  var t = typ
  while true:
    if t.sym != nil and {sfImportc, sfExportc} * t.sym.flags != {}:
      return $t.sym.loc.r

    if t.kind in irrelevantForBackend:
      t = t.lastSon
    else:
      break
  let typ = if typ.kind in {tyAlias, tySink}: typ.lastSon else: typ
  if typ.loc.r == nil:
    typ.loc.r = typ.typeName & $sig
  else:
    when defined(debugSigHashes):
      # check consistency:
      assert($typ.loc.r == $(typ.typeName & $sig))
  result = $typ.loc.r
  if result == "": g.config.internalError("getTypeName: " & $typ.kind)

iterator procParams(typ: PType): PNode =
  for a in typ.n.sons[1..^1]:
    let param = a.sym
    if isCompileTimeOnly(param.typ): continue
    yield a

proc `$`(t: PType): string

proc `$`(n: PSym): string =
  if n == nil: return "PSym(nil)"
  let name = if n.loc.r == nil: n.name.s else: $n.loc.r
  result = name & " " & $n.id & " " & $n.kind & " " & $n.magic & " " & $n.flags & " " &
    $n.loc.flags & " " & $n.info.line & " " & $n.typ

proc `$`(t: PType): string =
  if t == nil: return "PType(nil)"
  result = $t.kind & " " & $t.flags & " " & $t.id & " "
  if t.sonsLen > 0:
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
  of nkStrLit..nkTripleStrLit: result &= n.strVal
  of nkSym: result &= $n.sym
  of nkIdent: result &= n.ident.s
  of nkProcDef, nkFuncDef:
    let s = n[namePos].sym
    result &= $s & " " & $n.sonsLen
  else:
    result &= $n.flags & " " & $n.sonslen
    if n.typ != nil:
      result &= " " & $n.typ.kind

template p(t: string, n: PNode, depth: int) =
  if g.graph.config.verbosity == 2:
    echo(spaces(depth * 2), t, " ", n)

template p(t: string, n: PType, depth: int) =
  if g.graph.config.verbosity == 2:
    echo(spaces(depth * 2), t, " ", n)

template p(t: string, n: PSym, depth: int) =
  if g.graph.config.verbosity == 2:
    echo(spaces(depth * 2), t, " ", n)

proc constInt(g: LLGen, bits: int64, v: SomeInteger): llvm.ValueRef =
  llvm.constInt(llvm.intTypeInContext(g.lc, bits.cuint), v.culonglong, llvm.False)

proc constInt1(g: LLGen, v: bool): llvm.ValueRef =
  llvm.constInt(llvm.int1TypeInContext(g.lc), v.culonglong, llvm.False)

proc constInt8(g: LLGen, v: int8): ValueRef =
  llvm.constInt(g.primitives[tyInt8], v.culonglong, llvm.False)

proc constInt32(g: LLGen, v: int32): ValueRef =
  llvm.constInt(g.primitives[tyInt32], v.culonglong, llvm.False)

proc constInt64(g: LLGen, v: int64): ValueRef =
  llvm.constInt(g.primitives[tyInt64], v.culonglong, llvm.False)

proc constCInt(g: LLGen, val: int): llvm.ValueRef =
  llvm.constInt(g.cintType, val.culonglong, llvm.True)

proc constNimInt(g: LLGen, val: int): llvm.ValueRef =
  llvm.constInt(g.primitives[tyInt], val.culonglong, llvm.True)

proc isArrayPtr(t: llvm.TypeRef): bool =
  t.getTypeKind() == llvm.PointerTypeKind and
    t.getElementType().getTypeKind() == llvm.ArrayTypeKind

# Int constant suitable for indexing into structs when doing GEP
# Has to be i32 per LLVM docs
proc constGEPIdx(g: LLGen, val: int): llvm.ValueRef =
  g.constInt32(val.int32)

proc constOffsetOf(g: LLGen, t: PType, sym: PSym): llvm.ValueRef =
  let
    typ = t.skipTypes(abstractPtrs)
    llt = g.llType(typ)
    ind = g.fieldIndex(t, sym)
    cgi = proc (v: int): llvm.ValueRef = g.constGEPIdx(v)
    gep = constGEP(constNull(llt.pointerType()), (@[0] & ind).map(cgi))
  result = constPtrToInt(gep, g.primitives[tyInt])

proc bitSetToWord(s: TBitSet, size: int): int64 =
  result = 0
  for j in countup(0, size - 1):
    if j < len(s): result = result or `shl`(ze64(s[j]), j * 8)

proc constNimSet(g: LLGen, n: PNode): llvm.ValueRef =
  assert n.kind == nkCurly
  let
    typ = skipTypes(n.typ, abstractVar)
    size = g.config.getSize(typ)

  var cs: TBitSet
  g.config.toBitSet(n, cs)
  if size <= 8:
    result = g.constInt(size * 8, cs.bitSetToWord(size.int))
  else:
    proc ci8(v: int8): llvm.ValueRef = g.constInt8(v)
    result = llvm.constArray(g.primitives[tyInt8], cs.map(ci8))

proc constNimString(g: LLGen, n: PNode): llvm.ValueRef =
  let s = llvm.constString(n.strVal)
  let ll = g.constNimInt(n.strVal.len)
  let x = llvm.constNamedStruct(g.seqBaseType, [ll, ll])
  result = llvm.constStruct([x, s])

proc buildNimSeqLenGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildGEP(s, [g.gep0, g.gep0, g.gep0], g.nn("seq.len", s))

proc buildNimSeqDataGEP(g: LLGen, s: llvm.ValueRef, idx: llvm.ValueRef = nil): llvm.ValueRef =
  let idx = if idx == nil: g.gep0 else: idx
  g.b.buildGEP(s, [g.gep0, g.gep1, idx], g.nn("seq.data", s))

proc buildI1(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
 g.b.buildTrunc(v, int1TypeInContext(g.lc), g.nn("bool.i1", v))

proc buildI8(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.b.buildZExt(v, int8TypeInContext(g.lc), g.nn("bool.i8", v))

proc isUnsigned(g: LLGen, typ: PType): bool =
  let typ = typ.skipTypes(abstractVarRange)

  result = typ.kind in {tyUInt..tyUInt64, tyBool, tyChar, tySet} or
    (typ.kind == tyEnum and g.config.firstOrd(typ) >= 0)

proc buildNimIntExt(g: LLGen, v: llvm.ValueRef, unsigned: bool): llvm.ValueRef =
  let nt = g.primitives[tyInt]
  result =
    if unsigned: g.b.buildZExt(v, nt, g.nn("nie.z", v))
    else: g.b.buildSExt(v, nt, g.nn("nie.s", v))

proc buildTruncOrExt(g: LLGen, v: llvm.ValueRef, nt: llvm.TypeRef,
                     unsigned: bool): llvm.ValueRef =
  let
    vt = v.typeOfX()
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
    result = g.b.buildTrunc(v, nt, g.nn("toe.t", v))
  else:  # vw < nw
    result =
      if unsigned: g.b.buildZExt(v, nt, g.nn("toe.z", v))
      else: g.b.buildSExt(v, nt, g.nn("toe.s", v))

proc buildTruncOrExt(g: LLGen, v: llvm.ValueRef, nt: llvm.TypeRef,
                     typ: PType): llvm.ValueRef =
  g.buildTruncOrExt(v, nt, g.isUnsigned(typ))

proc needsBr(b: llvm.BuilderRef): bool =
  b.getInsertBlock().getBasicBlockTerminator() == nil

proc buildBrFallthrough(b: llvm.BuilderRef, next: llvm.BasicBlockRef) =
  # Add a br to the next block if the current block is not already terminated
  if b.needsBr():
    discard b.buildBr(next)

proc buildSetMask(g: LLGen, t: llvm.TypeRef, ix: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let mask =
    case size
    of 1: 7
    of 2: 15
    of 4: 31
    of 8: 63
    else: 7
  var shift = g.b.buildAnd(
    ix, constInt(ix.typeOfX(), mask.culonglong, llvm.False),
    g.nn("set.mask", ix))
  if t != shift.typeOfX():
    shift = g.buildTruncOrExt(shift, t, true)
  result = g.b.buildShl(llvm.constInt(t, 1, llvm.False), shift, g.nn("set.pos", ix))

proc buildSetGEPMask(g: LLGen, vx, ix: llvm.ValueRef): tuple[gep, mask: llvm.ValueRef] =
  let idx = g.b.buildUDiv(ix, llvm.constInt(ix.typeOfX(), 8, llvm.False), g.nn("set.byte", ix))
  let gep = g.b.buildGEP(vx, [g.gep0, idx], g.nn("set.gep", vx))

  let mask = g.buildSetMask(llvm.int8TypeInContext(g.lc), ix, 1)
  result = (gep: gep, mask: mask)

proc buildLoadValue(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  if v.typeOfX().isArrayPtr():
    result = g.b.buildGEP(v, [g.gep0, g.gep0], g.nn("load", v))
  else:
    result = g.b.buildLoad(v, g.nn("load", v))

proc buildNot(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  result = g.b.buildICmp(llvm.IntEQ, v, llvm.constInt(v.typeOfX(), 0, llvm.False), g.nn("not", v))
  result = g.b.buildZExt(result, v.typeOfX(), g.nn("zext", result))

proc buildBitnot(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.b.buildXor(v, llvm.constInt(v.typeOfX(), (-1).culonglong, llvm.False), g.nn("bitnot", v))

proc buildStoreNull(g: LLGen, v: llvm.ValueRef) =
  let t = v.typeOfX()
  assert t.getTypeKind() == llvm.PointerTypeKind
  let et = t.getElementType()
  if et.getTypeKind() in {llvm.ArrayTypeKind, llvm.StructTypeKind}:
    g.callMemset(v, g.constInt8(0), et.sizeOfX())
  else:
    discard g.b.buildStore(constNull(et), v)

proc debugGetFile(g: LLGen, idx: FileIndex): llvm.MetadataRef =
  if int(idx) in g.dfiles: return g.dfiles[int(idx)]

  let path = g.config.toFullPath(idx)
  let (dir, fn) = path.splitPath()
  result = g.d.dIBuilderCreateFile(fn, dir)

  g.dfiles[int(idx)] = result

proc debugStructType(g: LLGen, typ: PType): llvm.MetadataRef
proc debugTupleType(g: LLGen, typ: PType): llvm.MetadataRef
proc debugMagicType(g: LLGen, name: string): llvm.MetadataRef

proc debugType(g: LLGen, typ: PType): llvm.MetadataRef =
  if g.d == nil: return nil

  case typ.kind
  of tyBool: result = g.dtypes[tyBool]
  of tyChar: result = g.dtypes[tyChar]
  of tyNil, tyStmt: result = nil
  of tyGenericInst: result = g.debugType(typ.lastSon)
  of tyDistinct, tyAlias, tyInferred: result = g.debugType(typ.lastSon)
  of tyEnum:
    let bits = getSize(g.config, typ).cuint * 8
    result = g.d.dIBuilderCreateBasicType("enum", bits, DW_ATE_unsigned)
  of tyArray:
    let et = g.debugType(typ.elemType)
    # Aribrary limit of 100 items here - large numbers seem to have a poor
    # impact on some debuggers that insist on displaying all sub-items (looking
    # at you, eclipse+gdb) - need to investigate this more
    let (s, c) = if g.config.lengthOrd(typ) > 100: (cuint(0), -1.int64)
                 else: (cuint(g.config.lengthOrd(typ)), int64(g.config.lengthOrd(typ)))
    result = g.d.dIBuilderCreateArrayType(s * 8, 0, et,
              [g.d.dIBuilderGetOrCreateSubrange(0, c)])
  of tyUncheckedArray:
    let et = g.debugType(typ.elemType)
    let (s, c) = (cuint(0), -1.int64)
    result = g.d.dIBuilderCreateArrayType(s * 8, 0, et,
                [g.d.dIBuilderGetOrCreateSubrange(0, c)])
  of tyObject: result = g.debugStructType(typ)
  of tyTuple: result = g.debugTupleType(typ)
  of tySet:
    let size = g.config.getSize(typ).cuint
    let bits = size * 8
    result = if size <= 8: g.d.dIBuilderCreateBasicType("set", bits, DW_ATE_unsigned)
             else: g.d.dIBuilderCreateArrayType(
              size * 8, 0, g.dtypes[tyUInt8],
              [g.d.dIBuilderGetOrCreateSubrange(0, size.int64)])
  of tyRange: result = g.debugType(typ.sons[0])
  of tyPtr, tyRef, tyVar:
    result = g.d.dIBuilderCreatePointerType(
      g.debugType(typ.lastSon), 64, 64, "")
  of tySequence:
    var st: llvm.MetadataRef

    let sig = hashType(typ)

    if sig in g.dstructs:
      st = g.dstructs[sig]
    else:
      let name = g.llName(typ, sig)
      let file = g.debugGetFile(g.config.projectMainIdx)

      st = g.d.dIBuilderCreateStructType(g.dcu, name,
          file, 0, 128, 0, 0, nil, [], 0, nil, name)
      g.dstructs[sig] = st

      var elems = @[
        g.d.dIBuilderCreateMemberType(g.dcu, "len", file, 0, 64, 64, 0, 0,
          g.dtypes[tyInt]),
        g.d.dIBuilderCreateMemberType(g.dcu, "cap", file, 0, 64, 64, 64, 0,
          g.dtypes[tyInt])
      ]
      if typ.elemType.kind != tyEmpty:
        elems.add(
          g.d.dIBuilderCreateMemberType(g.dcu, "data", file, 0, 0, 0, 128, 0,
            g.d.dIBuilderCreateArrayType(0, 0, g.debugType(typ.elemType),[])))

      g.d.nimDICompositeTypeSetTypeArray(
        st, g.d.dIBuilderGetOrCreateArray(elems))

    result = g.d.dIBuilderCreatePointerType(st, 8, 8, "")
  of tyProc:
    if typ.callConv == ccClosure:
      result = g.d.dIBuilderCreateStructType(g.dcu, "closure",
        g.debugGetFile(g.config.projectMainIdx), 0, 128, 0, 0, nil,
        [g.dtypes[tyPointer], g.dtypes[tyPointer]], 0, nil, "closure")
    else:
      result = g.dtypes[tyPointer]
  of tyPointer: result = g.dtypes[tyPointer]
  of tyOpenArray, tyVarargs: result = g.debugType(typ.elemType)
  of tyString:
    if g.dtypes[tyString] == nil:
      g.dtypes[tyString] = g.d.dIBuilderCreatePointerType(
        g.debugMagicType("NimStringDesc"), 64, 64, "")
    result = g.dtypes[tyString]
  of tyCString: result = g.dtypes[tyCString]
  of tyInt..tyUInt64: result = g.dtypes[typ.kind]
  of tyTypeDesc: result = g.debugType(typ.lastSon)
  of tyStatic: result = g.debugType(typ.lastSon)
  of tyUserTypeClasses: result = g.debugType(typ.lastSon)
  else: g.config.internalError("Unhandled debug type " & $typ.kind)

proc debugFieldName(field: PSym, typ: PType): string =
  if (typ.sym != nil) and ({sfImportc, sfExportc} * typ.sym.flags != {}):
    result = $field.loc.r
  else:
    result = mangle(field.name.s)

proc align(address, alignment: int): int =
  result = (address + (alignment - 1)) and not (alignment - 1)

proc debugStructFields(
    g: LLGen, elements: var seq[MetadataRef], n: PNode, typ: PType,
    offset: var int) =
  case n.kind
  of nkRecList:
    for child in n:
      g.debugStructFields(elements, child, typ, offset)
  of nkRecCase: # TODO Unionize
    if n[0].kind != nkSym: g.config.internalError(n.info, "debugStructFields")
    g.debugStructFields(elements, n[0], typ, offset)

    for i in 1..<n.sonsLen:
      case n[i].kind
      of nkOfBranch, nkElse:
        g.debugStructFields(elements, n[i].lastSon, typ, offset)
      else: g.config.internalError(n.info, "debugStructFields")
  of nkSym:
    let field = n.sym
    if field.typ.kind == tyEmpty: return
    let size = g.config.getSize(field.typ) * 8
    let name = debugFieldName(field, typ)
    offset = align(offset, field.typ.align * 8)
    let member = g.d.dIBuilderCreateMemberType(
      g.dcu, name, g.debugGetFile(g.config.projectMainIdx), 0, size.uint64,
      0, offset.uint64, 0, g.debugType(field.typ))
    offset = offset + size.int
    elements.add(member)
  else: g.config.internalError(n.info, "debugStructFields")

proc debugStructType(g: LLGen, typ: PType): llvm.MetadataRef =
  if typ == nil:
    return

  var typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString: return g.debugType(typ)

  let sig = hashType(typ)
  if sig in g.dstructs:
    return g.dstructs[sig]

  let name = g.llName(typ, sig)

#  result = g.headerType(name)
#  if result != nil:
#    g.types[sig] = result
#    return

#  p("llStructType " & $typ, typ, g.depth)

  # Create struct before setting body in case it's recursive

  let file = g.debugGetFile(g.config.projectMainIdx)
  let size = g.config.getSize(typ) * 8
  result = g.d.dIBuilderCreateStructType(g.dcu, name,
    file, 0, size.uint64, 0, 0, nil, [], 0, nil, name)

  g.dstructs[sig] = result

  var elements = newSeq[MetadataRef]()

  var offset: int

  let super = if typ.sons.len == 0: nil else: typ.sons[0]
  if super == nil:
    if (typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags:
      discard
    else:
      let size = 8 * 8
      let name = "m_type"
      let tnt = g.debugMagicType("TNimType")
      let tntp = g.d.dIBuilderCreatePointerType(tnt, 64, 64, "")
      let member = g.d.dIBuilderCreateMemberType(
        g.dcu, name, g.debugGetFile(g.config.projectMainIdx), 0, size.uint64,
        0, offset.uint64, 0, tntp)
      offset = offset + size.int

      elements.add(member)
  else:
    let size = g.config.getSize(super) * 8
    let name = "super"
    let member = g.d.dIBuilderCreateMemberType(
      g.dcu, name, g.debugGetFile(g.config.projectMainIdx), 0, size.uint64,
      0, offset.uint64, 0, g.debugType(super))
    offset = offset + size.int

    elements.add(member)

  g.debugStructFields(elements, typ.n, typ, offset)

  g.d.nimDICompositeTypeSetTypeArray(
    result, g.d.dIBuilderGetOrCreateArray(elements))

proc debugTupleType(g: LLGen, typ: PType): llvm.MetadataRef =
  if typ == nil:
    return

  let sig = hashType(typ)
  if sig in g.dstructs:
    return g.dstructs[sig]

  let name = g.llName(typ, sig)

  # Create struct before setting body in case it's recursive
  let file = g.debugGetFile(g.config.projectMainIdx)
  let size = g.config.getSize(typ) * 8
  result = g.d.dIBuilderCreateStructType(g.dcu, name,
        file, 0, size.uint64, 0, 0, nil, [], 0, nil, name)

  g.dstructs[sig] = result

  var elements = newSeq[MetadataRef]()
  var offset: int
  for t in typ.sons:
    let size = g.config.getSize(t) * 8
    let member = g.d.dIBuilderCreateMemberType(
      g.dcu, "tup" & $offset, g.debugGetFile(g.config.projectMainIdx), 0, size.uint64,
      0, offset.uint64, 0, g.debugType(t))
    offset = offset + size.int
    elements.add(member)

  g.d.nimDICompositeTypeSetTypeArray(
    result, g.d.dIBuilderGetOrCreateArray(elements))

proc debugMagicType(g: LLGen, name: string): llvm.MetadataRef =
  g.debugType(g.graph.getCompilerProc(name).typ)

proc debugProcParamType(g: LLGen, t: PType): llvm.MetadataRef =
  let typ = t.skipTypes(abstractInst)
  case typ.kind:
  of tyArray, tyOpenArray, tyUncheckedArray, tyVarargs, tyObject, tyTuple:
    result = g.d.dIBuilderCreatePointerType(g.debugType(t), 64, 64, "")
  of tySet:
    let size = g.graph.config.getSize(typ).cuint
    result = if size <= 8: g.debugType(t)
             else: g.d.dIBuilderCreatePointerType(g.debugType(t), 64, 64, "")
  of tyProc:
    result =
      if typ.callConv == ccClosure:
        g.d.dIBuilderCreatePointerType(g.debugType(t), 64, 64, "")
      else: g.debugType(t)
  of tyDistinct, tyAlias, tyInferred: result = g.debugProcParamType(t.lastSon)
  else: result = g.debugType(t)

proc debugProcType(g: LLGen, typ: PType, closure: bool): seq[llvm.MetadataRef] =
  let retType = if typ.sons[0] == nil: nil
                else: g.debugType(typ.sons[0])
  result.add(retType)

  for param in typ.procParams():
    let t = param.sym.typ.skipTypes({tyGenericInst})
    let at = g.debugProcParamType(t)
    result.add(at)

    if skipTypes(t, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      result.add(g.dtypes[tyInt])  # Extra length parameter

  if closure:
    result.add(g.dtypes[tyPointer])

proc debugGetScope(g: LLGen, sym: PSym): llvm.MetadataRef =
  var sym = sym
  while sym != nil and sym.kind notin {skProc, skFunc, skModule}:
    sym = sym.owner

  if sym != nil and sym.id in g.dscopes: return g.dscopes[sym.id]

  if g.f != nil:
    return g.f.ds

  result = g.dcu

proc debugGetLocation(g: LLGen, sym: PSym, li: TLineInfo): llvm.MetadataRef =
  let scope = g.debugGetScope(sym)

  result = llvm.getGlobalContext().dIBuilderCreateDebugLocation(
      li.line.cuint, li.col.cuint, scope, nil)

proc debugUpdateLoc(g: LLGen, n: PNode) =
  if g.d == nil: return
  if n == nil:
    g.b.setCurrentDebugLocation(nil)
    return

  let sym = if n.kind == nkSym: n.sym else: nil

  let dl = metadataAsValue(llvm.getGlobalContext(), g.debugGetLocation(sym, n.info))
  g.b.setCurrentDebugLocation(dl)

proc debugVariable(g: LLGen, sym: PSym, v: llvm.ValueRef, argNo = -1) =
  if g.d == nil: return

  var dt = g.debugType(sym.typ)

  let scope = g.debugGetScope(sym)

  let vd =
    if argNo == -1:
      g.d.dIBuilderCreateAutoVariable(
        scope, sym.llName,
        g.debugGetFile(sym.info.fileIndex),
        sym.info.line.cuint, dt, false, 0, 0)
    else:
      g.d.dIBuilderCreateParameterVariable(
        scope, sym.llName, argNo.cuint,
        g.debugGetFile(sym.info.fileIndex),
        sym.info.line.cuint, dt, false, 0)

  discard g.d.dIBuilderInsertDeclareAtEnd(v, vd,
    g.d.dIBuilderCreateExpression(nil, 0),
    llvm.valueAsMetadata(g.b.getCurrentDebugLocation()),
    g.b.getInsertBlock())

proc debugGlobal(g: LLGen, sym: PSym, v: llvm.ValueRef) =
  if g.d == nil: return

  var dt = g.debugType(sym.typ)
  let scope = g.debugGetScope(sym)

  let gve = dIBuilderCreateGlobalVariableExpression(
    g.d, scope, sym.llName, "",
    g.debugGetFile(sym.info.fileIndex), sym.info.line.cuint, dt, false,
    dIBuilderCreateExpression(g.d, nil, 0), nil, 0
  )
  v.nimSetMetadataGlobal(g.dbgKind, g.lc.metadataAsValue(gve))

proc debugFunction(
    g: LLGen, s: PSym, params: openArray[llvm.MetadataRef],
    f: llvm.ValueRef): llvm.MetadataRef =
  let df = g.debugGetFile(if s == nil: g.config.projectMainIdx else: s.info.fileIndex)
  let st = g.d.dIBuilderCreateSubroutineType(df, params)
  result = g.d.dIBuilderCreateFunction(
    g.dcu, $f.getValueName(), "", df, 0, st, true, true, 0, 0, false)
  f.setSubprogram(result)

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef
proc llTupleType(g: LLGen, typ: PType): llvm.TypeRef
proc llProcType(g: LLGen, typ: PType, closure = false): llvm.TypeRef

proc llType(g: LLGen, typ: PType): llvm.TypeRef =
  case typ.kind
  of tyBool, tyChar, tyNil, tyStmt: result = g.primitives[typ.kind]
  of tyGenericInst, tyDistinct, tyAlias, tyInferred:
    result = g.llType(typ.lastSon)
  of tyEnum:
    result = llvm.intTypeInContext(g.lc, g.config.getSize(typ).cuint * 8)
  of tyArray, tyUncheckedArray:
    let et = g.llType(typ.elemType)
    # Even for unchecked arrays, we use lengthord here - echo for
    # example generates unchecked arrays with correct length set, and
    # if we skip length, there will be a local stack array of length
    # 0 here!
    let n = cuint(g.config.lengthOrd(typ))
    result = llvm.arrayType(et, n)
  of tyObject: result = g.llStructType(typ)
  of tyTuple: result = g.llTupleType(typ)
  of tySet:
    let size = g.config.getSize(typ).cuint
    result = if size <= 8: llvm.intTypeInContext(g.lc, size * 8)
             else: llvm.arrayType(llvm.int8TypeInContext(g.lc), size)
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
      let name = g.llName(typ, sig)
      st = structCreateNamed(g.lc, name)
      g.types[sig] = st

      if typ.elemType.kind == tyEmpty:
        st.structSetBody([g.seqBaseType])
      else:
        st.structSetBody([g.seqBaseType, llvm.arrayType(g.llType(typ.elemType), 0)])
    result = st.pointerType()
  of tyProc:
    result = if typ.callConv == ccClosure:
      g.closureType
    else:
      g.procPtrType

  of tyPointer: result = g.primitives[typ.kind]
  of tyOpenArray, tyVarargs: result = g.llType(typ.elemType)
  of tyString: result = g.nimStringDescPtrType
  of tyCString, tyInt..tyUInt64: result = g.primitives[typ.kind]
  of tyTypeDesc, tyStatic, tyUserTypeClasses: result = g.llType(typ.lastSon)
  else:
    g.config.internalError("Unhandled type " & $typ.kind)

template withBlock(g: LLGen, bb: llvm.BasicBlockRef, body: untyped) =
  block:
    let b = g.b
    let pre = b.getInsertBlock()
    let db = b.getCurrentDebugLocation()
    b.positionBuilderAtEnd(bb)
    body
    b.positionBuilderAtEnd(pre)
    if optCDebug in g.config.globalOptions:
      b.setCurrentDebugLocation(db)

proc finalize(g: LLGen, llf: LLFunc) =
  if llf.init == nil: return

  let pre = llf.ret.getBasicBlockParent()
  var entry = pre.getEntryBasicBlock()
  if entry.getFirstInstruction().getInstructionOpcode() == llvm.Alloca:
    entry.getLastInstruction().instructionEraseFromParent()
    g.withBlock(entry):
      discard g.b.buildBr(llf.init)
    entry = entry.getNextBasicBlock()

  llf.init.moveBasicBlockBefore(entry)

  g.withBlock(llf.init):
    discard g.b.buildBr(entry)

proc llMagicType(g: LLGen, name: string): llvm.TypeRef =
  g.llType(g.graph.getCompilerProc(name).typ)

proc addStructFields(g: LLGen, elements: var seq[TypeRef], n: PNode, typ: PType) =
  p("addStructFields", n, g.depth)
  case n.kind
  of nkRecList:
    for child in n:
      g.addStructFields(elements, child, typ)
  of nkRecCase: # TODO Unionize
    if n[0].kind != nkSym: g.config.internalError(n.info, "addStructFields")
    g.addStructFields(elements, n[0], typ)

    for i in 1..<n.sonsLen:
      case n[i].kind
      of nkOfBranch, nkElse:
        g.addStructFields(elements, n[i].lastSon, typ)
      else: g.config.internalError(n.info, "addStructFields")
  of nkSym:
    let field = n.sym
    if field.typ.kind == tyEmpty: return
    elements.add(g.llType(field.typ))
  else: g.config.internalError(n.info, "addStructFields")

proc headerType(g: LLGen, name: string): llvm.TypeRef =
  # Here are replacements for some of the types in the nim standard library that
  # rely on c header parsing to work. This is a crude workaround that needs to
  # be replaced, but works for now, on linux/x86_64
  case name
  of "TGenericSeq": result = g.seqBaseType
  of "NimStringDesc": result = g.nimStringDescType
  of "jmp_buf": result = g.jmpbufType
  else: result = nil

proc headerTypeIndex(g: LLGen, typ: PType, sym: PSym): seq[int] =
  let sig = hashType(typ)
  case g.llName(typ, sig)
  else: discard

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef =
  if typ == nil:
    return

  var typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString:
    return g.nimStringDescPtrType

  let sig = hashType(typ)
  if sig in g.types:
    return g.types[sig]

  let name = g.llName(typ, sig)

  result = g.headerType(name)
  if result != nil:
    g.types[sig] = result
    return

  p("llStructType " & $typ, typ, g.depth)

  # Create struct before setting body in case it's recursive
  result = structCreateNamed(g.lc, name)
  g.types[sig] = result

  var elements = newSeq[TypeRef]()

  let super = if typ.sons.len == 0: nil else: typ.sons[0]
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

  let name = g.llName(typ, sig)

  # Create struct before setting body in case it's recursive
  result = structCreateNamed(g.lc, name)
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
    proc cgi(v: int): llvm.ValueRef = g.constGEPIdx(v)
    var gep = g.b.buildGEP(v, (@[0] & start).map(cgi), g.nn("mk.kind", kind))
    let vk = g.b.buildLoad(gep, g.nn("mk.kind.load", kind))

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let caseend = f.appendBasicBlock(g.nn("mk.kind.end", kind))

    inc(start)
    var hasElse = false

    for i in 1..<n.sonsLen:
      let branch = n[i]

      let ctrue = f.appendBasicBlock(g.nn("mk.kind.true", branch))
      if branch.kind == nkOfBranch:

        var length = branch.len
        for j in 0 .. length-2:
          var cmp: llvm.ValueRef

          if branch[j].kind == nkRange:
            let s = branch[j][0].intVal
            let e = branch[j][1].intVal

            let scmp = g.b.buildICmp(
              llvm.IntSGE, vk, llvm.constInt(vk.typeOfX(), s.culonglong, llvm.True),
                g.nn("mk.kind.rng.s", branch))
            let ecmp = g.b.buildICmp(
              llvm.IntSLE, vk, llvm.constInt(vk.typeOfX(), e.culonglong, llvm.True),
                g.nn("mk.kind.rng.e", branch))

            cmp = g.b.buildAnd(scmp, ecmp, g.nn("mk.kind.rng.cmp", branch))

          else:
            let k = branch[j].intVal
            cmp = g.b.buildICmp(
              llvm.IntEQ, vk, llvm.constInt(vk.typeOfX(), k.culonglong, llvm.True),
              g.nn("mk.kind.cmp", branch))

          let cfalse = f.appendBasicBlock(g.nn("mk.kind.false", branch))
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
    proc cgi(v: int): llvm.ValueRef = g.constGEPIdx(v)
    var gep = g.b.buildGEP(v, (@[0] & start).map(cgi), g.nn("mk", field))
    if field.typ.skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
      gep = g.b.buildLoad(gep, g.nn("mk.load", field))

    g.genMarker(field.typ, gep, op)
    inc(start)
  else: g.config.internalError(n.info, "genMarker()")

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ == nil: return

  case typ.kind
  of tyGenericInst, tyGenericBody, tyTypeDesc, tyAlias, tyDistinct, tyInferred,
     tySink:
    g.genMarker(typ.lastSon(), v, op)
  of tyArray, tyUncheckedArray:
    let arraySize = g.config.lengthOrd(typ.sons[0])

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let wcmp = f.appendBasicBlock(g.nn("mk.arr.cmp"))
    let wtrue = f.appendBasicBlock(g.nn("mk.arr.true"))
    let wfalse = f.appendBasicBlock(g.nn("mk.arr.false"))

    let cnt = g.localAlloca(g.primitives[tyInt], g.nn("mk.arr.cnt"))
    discard g.b.buildStore(g.constNimInt(0), cnt)

    # jump to comparison
    discard g.b.buildBr(wcmp)

    # generate condition expression in cmp block
    g.b.positionBuilderAtEnd(wcmp)
    let c = g.b.buildLoad(cnt, g.nn("mk.arr.c"))
    let cond = g.b.buildICmp(llvm.IntULT, c, g.constNimInt(arraySize.int), g.nn("mk.arr.lt"))

    discard g.b.buildCondBr(cond, wtrue, wfalse)

    # loop body
    g.b.positionBuilderAtEnd(wtrue)

    var gep =
      if v.typeOfX().isArrayPtr(): g.b.buildGEP(v, [g.gep0, c], g.nn("while.data"))
      else: g.b.buildGEP(v, [c], g.nn("while.data"))

    if typ.sons[1].skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
      gep = g.b.buildLoad(gep, g.nn("mk.arr.load"))

    genMarker(g, typ.sons[1], gep, op)

    # back to comparison
    let cn = g.b.buildAdd(c, g.constNimInt(1), g.nn("mk.arr.add"))
    discard g.b.buildStore(cn, cnt)
    discard g.b.buildBr(wcmp)

    # continue at the end
    g.b.positionAndMoveToEnd(wfalse)

  of tyTuple:
    var i = 0
    g.genMarker(typ, typ.n, v, op, i)

  of tyObject:
    var start = 0
    if typ.sons.len > 0 and typ.sons[0] != nil:
      let gep = g.b.buildGEP(v, [g.gep0, g.gep0])
      g.genMarker(typ.sons[0], gep, op)
      start = 1
    elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
      # TODO skip m_type in a nicer way
      start = 1

    if typ.n == nil: g.config.internalError("expected node")

    g.genMarker(typ, typ.n, v, op, start)
  of tyRef, tyString, tySequence:
    let p = g.b.buildBitCast(v, g.voidPtrType, g.nn("mk.p"))
    discard g.callCompilerProc("nimGCVisit", [p, op])
  of tyProc:
    if typ.callConv == ccClosure:
      let p = g.b.buildGEP(v, [g.gep0, g.gep1])
      let x = g.b.buildLoad(p, g.nn("mk.x"))
      discard g.callCompilerProc("nimGCVisit", [x, op])
  else:
    discard

proc genMarkerSeq(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ.elemType.kind == tyEmpty: return

  let seqlenp = g.buildNimSeqLenGEP(v)
  let seqlen = g.b.buildLoad(seqlenp, g.nn("mk.seq.len"))

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let wcmp = f.appendBasicBlock(g.nn("mk.seq.cmp"))
  let wtrue = f.appendBasicBlock(g.nn("mk.seq.true"))
  let wfalse = f.appendBasicBlock(g.nn("while.false"))

  let cnt = g.localAlloca(g.primitives[tyInt], g.nn("mk.seq.cnt"))
  discard g.b.buildStore(g.constNimInt(0), cnt)

  # jump to comparison
  discard g.b.buildBr(wcmp)

  # generate condition expression in cmp block
  g.b.positionBuilderAtEnd(wcmp)
  let c = g.b.buildLoad(cnt, g.nn("mk.seq.c"))
  let cond = g.b.buildICmp(llvm.IntULT, c, seqlen, g.nn("mk.seq.lt"))

  discard g.b.buildCondBr(cond, wtrue, wfalse)

  # loop body
  g.b.positionBuilderAtEnd(wtrue)

  var gep = g.buildNimSeqDataGEP(v, c)
  if typ.sons[0].skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
    gep = g.b.buildLoad(gep, g.nn("mk.seq.load"))
  genMarker(g, typ.sons[0], gep, op)

  # back to comparison
  let cn = g.b.buildAdd(c, g.constNimInt(1), g.nn("mk.seq.add"))
  discard g.b.buildStore(cn, cnt)
  discard g.b.buildBr(wcmp)

  # continue at the end
  g.b.positionAndMoveToEnd(wfalse)

proc genMarkerProcBody(g: LLGen, f: llvm.ValueRef, typ: PType) =
  g.withBlock(llvm.appendBasicBlock(f, g.nn("entry"))):
    var scope: llvm.MetadataRef

    if g.d != nil:
      scope = g.debugFunction(
        typ.sym, [nil, g.dtypes[tyPointer], g.dtypes[tyInt]], f)

      let dl = llvm.getGlobalContext().dIBuilderCreateDebugLocation(
          0, 0, scope, nil)
      g.b.setCurrentDebugLocation(llvm.getGlobalContext().metadataAsValue(dl))

    let v = f.getFirstParam()
    v.setValueName("v")
    let op = v.getNextParam()
    op.setValueName("op")

    let vs = g.b.buildAlloca(v.typeOfX(), "")
    discard g.b.buildStore(v, vs)

    let ops = g.b.buildAlloca(op.typeOfX(), "")
    discard g.b.buildStore(op, ops)

    if g.d != nil:
      let dt = if typ.kind == tySequence: g.debugType(typ)
               else: g.d.dIBuilderCreatePointerType(
        g.debugType(typ.elemType), 64, 64, "")

      let file = g.debugGetFile(
        if typ.sym == nil: g.config.projectMainIdx else: typ.sym.info.fileIndex)
      let vd = g.d.dIBuilderCreateParameterVariable(
        scope, $v.getValueName(), 1, file, 0, dt, false, 0)
      discard g.d.dIBuilderInsertDeclareAtEnd(vs, vd,
        g.d.dIBuilderCreateExpression(nil, 0),
        valueAsMetadata(g.b.getCurrentDebugLocation()), g.b.getInsertBlock())

      let opd = g.d.dIBuilderCreateParameterVariable(
        scope, $op.getValueName(), 2, file, 0, g.dtypes[tyInt], false, 0)
      discard g.d.dIBuilderInsertDeclareAtEnd(ops, opd,
        g.d.dIBuilderCreateExpression(nil, 0),
        valueAsMetadata(g.b.getCurrentDebugLocation()), g.b.getInsertBlock())

    if typ.kind == tySequence:
      g.genMarkerSeq(typ, v, op)
    else:
      g.genMarker(typ.sons[0], v, op)

    discard g.b.buildRetVoid()

proc genMarkerProc(g: LLGen, typ: PType, sig: SigHash): llvm.ValueRef =
  if g.config.selectedGC < gcMarkAndSweep:
    return

  if sig in g.markers:
    return g.markers[sig]

  let name = "Marker_" & g.llName(typ, sig)

  let pt =
    if typ.kind == tySequence: g.llType(typ)
    else: g.llType(typ.elemType).pointerType()

  let ft = llvm.functionType(llvm.voidTypeInContext(g.lc), @[pt, g.primitives[tyInt]], false)

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

  let ft = llvm.functionType(llvm.voidTypeInContext(g.lc), @[], false)
  result = g.m.addFunction(name, ft)
  g.gmarkers[sym.id] = result

  # Because we generate only one module, we can tag all functions internal
  result.setLinkage(llvm.InternalLinkage)

  g.withBlock(llvm.appendBasicBlock(result, g.nn("entry"))):
    if g.d != nil:
      let scope = g.debugFunction(sym, [], result)

      let dl = llvm.getGlobalContext().dIBuilderCreateDebugLocation(
          0, 0, scope, nil)
      g.b.setCurrentDebugLocation(llvm.getGlobalContext().metadataAsValue(dl))

    var v = v
    if typ.kind in {tyRef, tyPtr, tyVar, tyString, tySequence}:
      v = g.b.buildLoad(v, g.nn("mk.load"))

    g.genMarker(typ, v, g.constInt8(0))

    discard g.b.buildRetVoid()

proc registerGcRoot(g: LLGen, sym: PSym, v: llvm.ValueRef) =
  if g.config.selectedGC in {gcMarkAndSweep, gcDestructors, gcV2, gcRefc} and
    sym.typ.containsGarbageCollectedRef():
    g.withBlock(g.getInitBlock(g.init)):
      let prc = g.genGlobalMarkerProc(sym, v)
      let scope = g.init.ds
      if scope != nil:
        let dl = llvm.getGlobalContext().dIBuilderCreateDebugLocation(
          0, 0, scope, nil)
        g.b.setCurrentDebugLocation(llvm.getGlobalContext().metadataAsValue(dl))

      discard g.callCompilerProc("nimRegisterGlobalMarker", [prc])

proc genTypeInfoInit(g: LLGen, t: PType, ntlt, lt: llvm.TypeRef,
                     baseVar, nodeVar, finalizerVar, markerVar,
                     deepcopyVar: llvm.ValueRef): llvm.ValueRef =
  let sizeVar = if lt == nil: g.ni0 else: llvm.sizeOfX(lt)

  let kind =
    if t.kind == tyObject and not t.hasTypeField(): tyPureObject
    elif t.kind == tyProc and t.callConv == ccClosure: tyTuple
    else: t.kind
  let kindVar = g.constInt8(int8(ord(kind)))

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

  let flagsVar = g.constInt8(flags)

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
    [g.constInt8(0), constNull(els[1]), constNull(els[2]), constNull(els[3]),
    g.constInt64(length), constNull(els[5])])

proc constNimNodeSlot(g: LLGen, offset, typeInfo: llvm.ValueRef, name: string): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  result = llvm.constNamedStruct(tnn,
    [g.constInt8(1), offset, typeInfo,
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
    let tmp = g.m.addPrivateConstant(nodesType, g.nn(".nodes"))
    tmp.setInitializer(constArray(tnnp, nodes))
    nodesVal = constBitCast(tmp, els[5])

  result = llvm.constNamedStruct(tnn,
    [g.constInt8(2), constNull(els[1]), constNull(els[2]), constNull(els[3]),
    g.constInt64(nodes.len), nodesVal])

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
    let tmp = g.m.addPrivateConstant(nodesType, g.nn(".nodes"))
    tmp.setInitializer(constArray(tnnp, nodes))
    nodesVal = constBitCast(tmp, els[5])

  result = llvm.constNamedStruct(tnn,
    [g.constInt8(3), offset, typeInfo,
    g.b.buildGlobalStringPtr(name, ".nimnode.case." & name),
    g.constInt64(nodesLen), nodesVal])

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
    let l = g.config.lengthOrd(field.typ).int

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

  else: g.config.internalError(n.info, "genObjectNodeInfoInit")

proc genObjectNodeInfo(g: LLGen, t: PType, n: PNode, suffix: string): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos and len(suffix) == 0:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & g.llName(t, sig) & suffix
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  if len(suffix) == 0:
    g.nodeInfos[sig] = result
  result.setInitializer(g.genObjectNodeInfoInit(t, n, suffix))

proc genTupleNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  let tnn = g.llMagicType("TNimNode")

  var fields: seq[ValueRef] = @[]

  let sig = hashType(t)
  let prefix = ".nodeinfo." & g.llName(t, sig) & "."

  let l = t.sonsLen
  for i in 0..<l:
    let
      name = prefix & $i
      field = g.m.addPrivateConstant(tnn, name)
      offset = constPtrToInt(constGEP(constNull(g.llType(t).pointerType()),
        [g.gep0, g.constGEPIdx(i)]), int64TypeInContext(g.lc))
      fieldInit = g.constNimNodeSlot(offset, g.genTypeInfo(t.sons[i]),
        "Field" & $i)

    field.setInitializer(fieldInit)
    fields.add(field)

  result = g.constNimNodeList(fields)

proc genTupleNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & g.llName(t, sig)
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
  let prefix = ".nodeinfo." & g.llName(t, sig) & "."
  var fields: seq[ValueRef] = @[]
  for i in 0..<l:
    let
      name = prefix & $i
      n = t.n[i].sym
      fieldName = if n.ast == nil: n.name.s else: n.ast.strVal

    # type info not needed for enum members
    let fieldInit = g.constNimNodeSlot(
      g.constInt64(n.position), constNull(els[2]), fieldName)

    let field = g.m.addPrivateConstant(tnn, name)
    field.setInitializer(fieldInit)
    fields.add(field)

  result = g.constNimNodeList(fields)

  # TODO c gen sets ntfEnumHole as well on TNimType after generating TNimNode.. odd.

proc genEnumNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & g.llName(t, sig)
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  g.nodeInfos[sig] = result
  result.setInitializer(g.genEnumNodeInfoInit(t))

proc genSetNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  result = g.constNimNodeNone(g.config.firstOrd(t).int)

proc genSetNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & g.llName(t, sig)
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
  if t.kind == tyArray:
    result = g.genTypeInfo(t.sons[1])
  elif t.kind in {tyUncheckedArray, tySequence, tyRef, tyPtr, tyRange, tySet, tyObject} and
    t.sons.len > 0 and t.sons[0] != nil:
    result = g.genTypeInfo(t.sons[0])

  if result == nil:
    result = g.llMagicType("TNimType").pointerType().constNull()

proc genTypeInfo(g: LLGen, t: PType): llvm.ValueRef =
  var t = t.skipTypes(irrelevantForBackend + tyUserTypeClasses)

  let sig = hashType(t)
  if sig in g.typeInfos:
    return g.typeInfos[sig]

  let name = ".typeinfo." & g.llName(t, sig)

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

    let gve = g.d.dIBuilderCreateGlobalVariableExpression(
      g.dcu, name, "",
      g.debugGetFile(if sym == nil: g.config.projectMainIdx else: sym.info.fileIndex),
      if sym == nil: 0.cuint else: sym.info.line.cuint, dt, false,
      dIBuilderCreateExpression(g.d, nil, 0), nil, 0)
    result.nimSetMetadataGlobal(g.dbgKind, g.lc.metadataAsValue(gve))

  g.typeInfos[sig] = result

  var finalizerVar, markerVar, deepcopyVar: llvm.ValueRef
  let baseVar = g.genTypeInfoBase(t)
  let nodeVar = g.genNodeInfo(t)

  if t.kind in {tySequence, tyRef}:
    markerVar = g.genMarkerProc(t, sig)

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
  of tyArray, tyOpenArray, tyUncheckedArray, tyVarargs, tyObject, tyTuple:
    result = g.llType(t).pointerType()
  of tySet:
    let size = g.config.getSize(typ).cuint
    result = if size <= 8: g.llType(t)
             else: g.llType(t).pointerType()
  of tyProc:
    result = if typ.callConv == ccClosure: g.llType(t).pointerType()
             else: g.llType(t)
  of tyDistinct, tyAlias, tyInferred, tyUserTypeClasses, tySink:
    result = g.llProcParamType(t.lastSon)
  else: result = g.llType(t)

proc llPassAsPtr(g: LLGen, t: PType): bool =
  g.llType(t) != g.llProcParamType(t)

proc llProcType(g: LLGen, typ: PType, closure: bool): llvm.TypeRef =
  let retType = if typ.sons[0] == nil: llvm.voidType()
                else: g.llType(typ.sons[0])
  var argTypes = newSeq[llvm.TypeRef]()

  for param in typ.procParams():
    let t = param.sym.typ.skipTypes({tyGenericInst, tyAlias, tySink})
    let at = g.llProcParamType(t)
    argTypes.add(at)

    if skipTypes(t, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      argTypes.add(g.primitives[tyInt])  # Extra length parameter

  if closure:
    argTypes.add(g.voidPtrType)

  result = llvm.functionType(retType, argTypes, tfVarArgs in typ.flags)

proc fieldIndexRecs(g: LLGen, n: PNode, sym: PSym, start: var int): seq[int] =
  case n.kind
  of nkRecList:
    for s in n:
      result = g.fieldIndexRecs(s, sym, start)
      if result.len > 0: return

  of nkRecCase:
    if n[0].kind != nkSym: g.config.internalError(n.info, "fieldIndex " & $n[0].kind)

    if n[0].sym.id == sym.id: return @[start]
    inc(start)
    for j in 1..<n.sonsLen:
      result = g.fieldIndexRecs(n[j].lastSon, sym, start)
      if result.len > 0: return
  of nkSym:
    if n.sym.id == sym.id: return @[start]
    inc(start)
  else:
    g.config.internalError(n.info, "Unhandled field index")
  return @[]

proc fieldIndex(g: LLGen, typ: PType, sym: PSym): seq[int] =
  var typ = typ.skipTypes(abstractInst + tyUserTypeClasses + skipPtrs)

  result = g.headerTypeIndex(typ, sym)
  if result.len > 0: return

  var start = 0
  if typ.kind != tyTuple:
    if typ.sons.len > 0 and typ.sons[0] != nil:
      let s = g.fieldIndex(typ.sons[0], sym)
      if s.len > 0:
        return @[0] & s
      start = 1
    elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
      # TODO skip m_type in a nicer way
      start = 1

  let n = typ.n
  result = g.fieldIndexRecs(n, sym, start)

proc mtypeIndex(g: LLGen, typ: PType): seq[llvm.ValueRef] =
  let zero = g.gep0
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
    g.config.internalError("no 'of' operator available for pure objects")
  result = result & @[zero]

proc setElemIndex(g: LLGen, typ: PType, x: llvm.ValueRef): llvm.ValueRef =
  if g.config.firstOrd(typ) != 0:
    g.b.buildSub(
      x, constInt(x.typeOfX(), g.config.firstOrd(typ).culonglong, llvm.False),
      g.nn("set.ord", x))
  else:
    x

proc isNimSeqLike(g: LLGen, t: llvm.TypeRef): bool =
  if t.getTypeKind() != llvm.PointerTypeKind: return

  let et = t.getElementType()
  if et.getTypeKind() != llvm.StructTypeKind: return

  if et.countStructElementTypes() != 2: return

  let elems = et.getStructElementTypes()
  if elems[0] != g.seqBaseType: return

  if elems[1].getTypeKind() != llvm.PointerTypeKind and
     elems[1].getTypeKind() != llvm.ArrayTypeKind: return

  result = true

proc preCast(
    g: LLGen, unsigned: bool, ax: llvm.ValueRef, t: PType,
    lt: llvm.TypeRef = nil): llvm.ValueRef =
  let
    at = ax.typeOfX()
    atk = at.getTypeKind()
    lt = if lt != nil: lt else: g.llType(t)
    ltk = lt.getTypeKind()

  if t.kind == tyCString and at != g.primitives[tyCString] and g.isNimSeqLike(at):
      result = g.buildNimSeqDataGEP(ax)
      return

  if ltk == PointerTypeKind and
      skipTypes(t, {tyVar}).kind in {tyVarargs, tyOpenArray, tyArray, tyUncheckedArray} and
      g.llType(t.elemType) == lt.getElementType() and
      g.isNimSeqLike(at):
    result = g.buildNimSeqDataGEP(ax)
    return

  if ltk == PointerTypeKind and atk == PointerTypeKind:
    result = g.b.buildBitCast(ax, lt, g.nn("pre", ax))
    return

  if ltk == IntegerTypeKind and atk == IntegerTypeKind and
      at.getIntTypeWidth() != lt.getIntTypeWidth():
    result = g.buildTruncOrExt(ax, lt, unsigned)
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
    if s.id in g.symbols:
      return g.symbols[s.id]
    result = g.m.addGlobal(t, name)
    g.symbols[s.id] = result

proc genLocal(g: LLGen, n: PNode): llvm.ValueRef =
  let s = n.sym
  if s.loc.k == locNone:
    fillLoc(s.loc, locLocalVar, n, s.name.s.mangle.rope, OnStack)
    if s.kind == skLet: incl(s.loc.flags, lfNoDeepCopy)

  let t = g.llType(s.typ)
  result = g.localAlloca(t, s.llName)
  g.symbols[s.id] = result
  g.debugVariable(s, result)

proc genGlobal(g: LLGen, n: PNode): llvm.ValueRef =
  let s = n.sym
  if s.id in g.symbols:
    return g.symbols[s.id]

  if s.loc.k == locNone:
    fillLoc(s.loc, locGlobalVar, n, g.mangleName(s), OnHeap)

  let t = g.llType(s.typ.skipTypes(abstractInst))
  result = g.m.addGlobal(t, s.llName)
  g.symbols[s.id] = result

  if sfImportc in s.flags:
    result.setLinkage(llvm.ExternalLinkage)
  elif sfExportc in s.flags:
    result.setLinkage(g.tgtExportLinkage)
    result.setInitializer(llvm.constNull(t))
  else:
    result.setLinkage(llvm.PrivateLinkage)
    result.setInitializer(llvm.constNull(t))

  if sfThread in s.flags and optThreads in g.config.globalOptions:
    result.setThreadLocal(llvm.True)
  g.debugGlobal(s, result)

proc callCompilerProc(g: LLGen, name: string, args: openarray[llvm.ValueRef]): llvm.ValueRef =
  let sym = g.graph.getCompilerProc(name)
  if sym == nil: g.config.internalError("compiler proc not found: " & name)

  let f = g.genFunctionWithBody(sym)

  var i = 0
  var args = @args
  for param in sym.typ.procParams():
    let v = args[i]

    # TODO unsigned
    let a = g.preCast(false, v, param.typ, g.llProcParamType(param.typ))
    args[i] = a

    if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      i += 1
    i += 1

  result = g.b.buildCall(
    f, args,
    if f.typeOfX().getElementType().getReturnType().getTypeKind() == llvm.VoidTypeKind: ""
    else: g.nn("call.cp." & name))

proc callBSwap(g: LLGen, v: llvm.ValueRef, n: cuint): llvm.ValueRef =
  let it = llvm.intTypeInContext(g.lc, n)
  let fty = llvm.functionType(it, [it])

  let
    f = g.m.getOrInsertFunction("llvm.bswap." & $n, fty)

  result = g.b.buildCall(f, [v])

proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef) =
  let
    memsetType = llvm.functionType(llvm.voidTypeInContext(g.lc),
      [g.voidPtrType, llvm.int8TypeInContext(g.lc), llvm.int64TypeInContext(g.lc),
       llvm.int1TypeInContext(g.lc)])
    f = g.m.getOrInsertFunction("llvm.memset.p0i8.i64", memsetType)
    t = g.b.buildBitCast(tgt, g.voidPtrType, g.nn("memset.tgt", v))

  discard g.b.buildCall(f, [t, v, len, g.constInt1(false)])

proc callMemcpy(g: LLGen, tgt, src, len: llvm.ValueRef) =
  let
    memcpyType = llvm.functionType(llvm.voidTypeInContext(g.lc), [
      g.voidPtrType, g.voidPtrType, llvm.int64TypeInContext(g.lc),
      llvm.int1TypeInContext(g.lc)])
    f = g.m.getOrInsertFunction("llvm.memcpy.p0i8.p0i8.i64", memcpyType)
    t = g.b.buildBitCast(tgt, g.voidPtrType, g.nn("memcpy.tgt", tgt))
    s = g.b.buildBitCast(src, g.voidPtrType, g.nn("memcpy.src", src))

  discard g.b.buildCall(f, [t, s, len, g.constInt1(false)])

proc callCtpop(g: LLGen, v: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let
    bits = (size * 8).cuint
    t = llvm.functionType(llvm.intTypeInContext(g.lc, bits), [llvm.intTypeInContext(g.lc, bits)])
    f = g.m.getOrInsertFunction("llvm.ctpop.i" & $bits, t)

  result = g.b.buildCall(f, [v])

proc callErrno(g: LLGen, prefix: string): llvm.ValueRef =
  # on linux errno is a function, so we call it here. not at all portable.

  let
    errnoType = llvm.functionType(g.cintType.pointerType(), [])
    f = g.m.getOrInsertFunction("__" & prefix & "errno_location", errnoType)

  result = g.b.buildCall(f, [], g.nn(prefix & "errno"))

proc callWithOverflow(g: LLGen, op: string, a, b: llvm.ValueRef, name: string): llvm.ValueRef =
  let t = a.typeOfX()
  let ft = llvm.functionType(llvm.structType([t, int1TypeInContext(g.lc)]), [t, t])
  let f = g.m.getOrInsertFunction(
    "llvm." & op & ".with.overflow.i" & $t.getIntTypeWidth(), ft)

  result = g.b.buildCall(f, [a, b], name)

proc callBinOpWithOver(
    g: LLGen, a, b: llvm.ValueRef, op: Opcode, n: PNode): llvm.ValueRef =
  # like tyChar, for example
  let u = n != nil and g.isUnsigned(n.typ.skipTypes(abstractVar))

  var opfn: string
  case op
  of llvm.Add: opfn = if u: "uadd" else: "sadd"
  of llvm.Sub: opfn = if u: "usub" else: "ssub"
  of llvm.Mul: opfn = if u: "umul" else: "smul"
  else: g.config.internalError("bad overflow op")

  let bo = g.callWithOverflow(opfn, a, b, g.nn("binop.over." & $op, n))

  let over = g.b.buildExtractValue(bo, 1, g.nn("binop.isover", n))
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let lover = f.appendBasicBlock(g.nn("binop.over", n))
  let lok = f.appendBasicBlock(g.nn("binop.over.ok", n))

  discard g.b.buildCondBr(over, lover, lok)
  g.b.positionBuilderAtEnd(lover)
  discard g.callCompilerProc("raiseOverflow", [])
  discard g.b.buildUnreachable()
  g.b.positionBuilderAtEnd(lok)

  result = g.b.buildExtractValue(bo, 0, g.nn("binop.over.result", n))

  if n == nil: return

  let t = n.typ.skipTypes(abstractVar)
  if t.kind in {tyRange, tyEnum}:
    let lt = g.b.buildICmp(
      llvm.IntSLT, result, constInt(result.typeOfX(),
      g.config.firstOrd(t).culonglong, llvm.False), g.nn("binop.over.rng.lt", n))
    let gt = g.b.buildICmp(
      llvm.IntSGT, result, constInt(result.typeOfX(),
      g.config.lastOrd(t).culonglong, llvm.False), g.nn("binop.over.rng.gt", n))

    let ltgt = g.b.buildOr(lt, gt, g.nn("binop.over.rng.or", n))

    let lrok = f.appendBasicBlock(g.nn("binop.over.rng.ok", n))
    discard g.b.buildCondBr(ltgt, lover, lrok)
    g.b.positionBuilderAtEnd(lrok)

proc genObjectInit(g: LLGen, t: PType, v: llvm.ValueRef) =
  case analyseObjectWithTypeField(t)
  of frNone:
    discard
  of frHeader:
    let tgt = g.b.buildGEP(v, g.mtypeIndex(t), g.nn("mtype", v))
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

  result = g.b.buildBitCast(x, t.pointerType(), g.nn("newObj", x))
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
  let i = g.localAlloca(ax.typeOfX(), g.nn("rng.i", n))
  discard g.b.buildStore(ax, i)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let rcmp = f.appendBasicBlock(g.nn("rng.cmp", n))
  let rloop = f.appendBasicBlock(g.nn("rng.loop", n))
  let rdone = f.appendBasicBlock(g.nn("rng.done", n))

  # jump to comparison
  discard g.b.buildBr(rcmp)

  # check idx
  g.b.positionBuilderAtEnd(rcmp)
  let il = g.b.buildLoad(i, g.nn("rng.il", n))
  let cond = g.b.buildICmp(llvm.IntSLE, il, b, g.nn("rng.sle", n))
  discard g.b.buildCondBr(cond, rloop, rdone)

  # loop body
  g.b.positionBuilderAtEnd(rloop)

  body

  # inc idx
  let next = g.b.buildAdd(
    il, constInt(il.typeOfX(), 1, llvm.False), g.nn("rng.inc", n))
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

    g.withBlock(f.appendBasicBlock(g.nn("entry.fake", s))):
      let gep = g.b.buildGEP(f.getParam(0), [g.gep0, g.gep1])
      let p = g.b.buildLoad(gep, "")
      let ax = g.b.buildBitCast(p, g.primitives[tyInt].pointerType(), "")
      let a = g.b.buildLoad(g.b.buildGEP(ax, [g.gep1]), "")
      let cmp = g.buildI8(g.b.buildICmp(llvm.IntSLT, a, g.ni0, ""))
      discard g.b.buildRet(cmp)
    return true

  if (s.name.s == "addInt" or s.name.s == "subInt" or s.name.s == "mulInt") and
      s.typ.sons.len == 3 and
      s.typ.sons[0].kind == tyInt and
      s.typ.sons[1].kind == tyInt and
      s.typ.sons[2].kind == tyInt:
    # prefer intrinsic for these...
    g.withBlock(f.appendBasicBlock(g.nn("entry.fake", s))):
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
    g.withBlock(f.appendBasicBlock(g.nn("entry.fake", s))):
      discard g.b.buildRetVoid()
    return true

proc genFunction(g: LLGen, s: PSym): llvm.ValueRef =
  if s.id in g.symbols: return g.symbols[s.id]

  if s.loc.k == locNone:
    fillLoc(s.loc, locProc, s.ast[namePos], g.mangleName(s), OnStack)

  let name = s.llName

  var s = s
  # Some compiler proc's have two syms essentially, because of an importc trick
  # in system.nim...
  if sfImportc in s.flags:
    result = g.m.getNamedFunction(name)
    if result != nil:
      g.symbols[s.id] = result
      return

  let typ = s.typ.skipTypes(abstractInst)

  let ft = g.llProcType(typ, typ.callConv == ccClosure)
  let f = g.m.addFunction(name, ft)

  g.symbols[s.id] = f

  if sfNoReturn in s.flags:
    f.addFuncAttribute(g.attrNoReturn)

  if typ.callConv == ccNoInline:
    f.addFuncAttribute(g.attrNoInline)

  if g.genFakeImpl(s, f):
    f.setLinkage(llvm.InternalLinkage)

  result = f

proc paramStorageLoc(param: PSym): TStorageLoc =
  if param.typ.skipTypes({tyVar, tyTypeDesc}).kind notin {
          tyArray, tyOpenArray, tyUncheckedArray, tyVarargs}:
    result = OnStack
  else:
    result = OnUnknown

proc genFunctionWithBody(g: LLGen, s: PSym): llvm.ValueRef =
  result = g.genFunction(s)

  if result.countBasicBlocks() != 0:
    return  # already has body

  if sfForward in s.flags: return
  if sfImportc in s.flags: return

  # Because we generate only one module, we can tag all functions internal, except
  # those that should be importable from c
  if sfExportc notin s.flags:
    result.setLinkage(llvm.InternalLinkage)

  var i = 1
  var lastIsArr = false

  let oldF = g.f

  let typ = s.typ.skipTypes(abstractInst)

  # generate body
  g.withBlock(llvm.appendBasicBlock(result, g.nn("entry", s))):

    g.f = g.newLLFunc(llvm.appendBasicBlock(result, g.nn("return", s)))
    g.f.options = s.options
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
          g.f.clenv = arg

          continue
        else:
          g.config.internalError("Params missing in function call: " & $s)
          return
      let param = typ.n[i]

      fillLoc(param.sym.loc, locParam, param, param.sym.name.s.mangle.rope,
              param.sym.paramStorageLoc)  # TODO sometimes OnStack

      p("a", param, g.depth + 1)
      p("a", param.typ, g.depth + 2)

      let name = if lastIsArr: param.sym.llName & "len" else: param.sym.llName
      arg.setValueName(name)

      let av = g.localAlloca(arg.typeOfX(), g.nn("arg", arg))
      discard g.b.buildStore(arg, av)

      g.debugVariable(param.sym, av, argNo)

      if lastIsArr:
        lastIsArr = false
        i += 1
        g.symbols[-param.sym.id] = av
      else:
        g.symbols[param.sym.id] = av

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
      if g.f.clenv == nil: g.config.internalError(s.ast.info, "env missing")
      let lx = g.b.buildBitCast(g.f.clenv, lt, g.nn("ClEnvX"))
      let av = g.localAlloca(lx.typeOfX(), g.nn("ClEnvX.a"))
      discard g.b.buildStore(lx, av)
      g.symbols[ls.sym.id] = av

      g.debugVariable(ls.sym, av, argNo)

    let procBody = transformBody(g.graph, s, cache = false)
    g.genNode(procBody)

    g.b.buildBrFallthrough(g.f.ret)
    g.b.positionAndMoveToEnd(g.f.ret)

    if ret != nil:
      discard g.b.buildRet(g.b.buildLoad(ret, g.nn("load.result")))
    else:
      discard g.b.buildRetVoid()

    g.finalize(g.f)
    g.f = oldF

proc genFakeCall(g: LLGen, n: PNode, o: var llvm.ValueRef): bool =
  let nf = n[0]
  if nf.kind != nkSym: return false

  let s = nf.sym

  if s.originatingModule().name.s == "system":
    if s.name.s == "atomicLoadN":
      let p0 = g.genNode(n[1], false)
      o = g.b.buildLoad(p0, g.nn("a.load.n"))
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
      o = g.buildI8(g.b.buildExtractValue(o, 1.cuint, g.nn("cas.b", n)))
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
    let pr = (if p.kind == nkHiddenAddr: p[0] else: p).skipConv()
    if i >= ftyp.n.len: # varargs like printf, for example
      let v = g.genNode(pr, true)
      # In some transformations, the compiler produces a call node to a
      # parameterless function, and then adds statements as child nodes where
      # the parameter value expressions would normally go - decidedly, odd -
      # see tautoproc.nim
      if v != nil and v.typeOfX().getTypeKind() != llvm.VoidTypeKind:
        args.add(v)
      continue

    let param = ftyp.n[i]

    if param.typ.isCompileTimeOnly(): continue

    let pt = parTypes[args.len]

    var v: llvm.ValueRef

    if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      var len: llvm.ValueRef

      if pr.getMagic() == mSlice:
        let
          ax = g.genNode(pr[1], true)
          bx = g.genNode(pr[2], true)
          cx = g.genNode(pr[3], true)
        case pr[1].typ.skipTypes(abstractVar + {tyPtr}).kind
        of tyOpenArray, tyVarargs, tyArray, tyUncheckedArray:
          let a = g.buildLoadValue(ax)
          v = g.b.buildGEP(a, [g.gep0, bx])

        of tyString, tySequence:
          let a =
            if n.typ.skipTypes(abstractInst).kind == tyVar: g.buildLoadValue(ax)
            else: ax
          v = g.buildNimSeqDataGEP(a, bx)
        else: g.config.internalError(n.info, "unknown slice type")

        len = g.b.buildSub(cx, bx, g.nn("slice.sub", n))
        len = g.b.buildAdd(len, constInt(len.typeOfX(), 1, llvm.False), g.nn("slice.add", n))
      else:
        case pr.typ.skipTypes(abstractVar).kind
        of tyString, tySequence:
          v = g.genNode(pr, true)
          if pr.typ.skipTypes(abstractInst).kind == tyVar:
            v = g.b.buildLoad(v, "")
          len = g.buildNimSeqLenGEP(v)
          len = g.b.buildLoad(len, g.nn("call.seq.len", n))
          len = g.b.buildZExt(len, g.primitives[tyInt], g.nn("call.seq.len.ext", n))
          v = g.buildNimSeqDataGEP(v)
        of tyOpenArray, tyVarargs:
          v = g.genNode(p, param.typ.kind == tyVar)
          len = g.b.buildLoad(g.symbols[-pr.sym.id], "")
        of tyArray, tyUncheckedArray:
          v = g.genNode(p, true)
          len = g.constNimInt(g.config.lengthOrd(pr.typ).int)
        of tyPtr, tyRef:
          case pr.typ.lastSon().kind
          of tyString, tySequence:
            v = g.genNode(pr, true)
            v = g.b.buildLoad(v, g.nn("call.seq.load", n))
            len = g.buildNimSeqLenGEP(v)
            len = g.b.buildLoad(len, g.nn("call.seq.len", n))
            len = g.b.buildZExt(len, g.primitives[tyInt], g.nn("call.seq.len.ext", n))
            v = g.buildNimSeqDataGEP(v)
          else:
            g.config.internalError(n.info, "Unhandled ref length: " & $pr.typ)
        else:
          g.config.internalError(n.info, "Unhandled length: " & $pr.typ)

      if v.typeOfX() != g.llProcParamType(param.typ):
        v = g.b.buildBitCast(v, g.llProcParamType(param.typ), g.nn("call.open", v))

      args.add(v)
      args.add(len)
    else:
      v = g.genNode(p, not g.llPassAsPtr(param.typ))

      # We need to use the type from the function, because with multimethods,
      # it looks like the type in param.typ changes during compilation!
      # seen with tmultim1.nim
      v = g.preCast(g.isUnsigned(p.typ), v, param.typ, pt)
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

    let prc = g.b.buildExtractValue(fx, 0, g.nn("call.clo.prc.ptr", n))
    let env = g.b.buildExtractValue(fx, 1, g.nn("call.clo.env.ptr", n))

    if tfIterator in typ.flags:
      let cft = g.llProcType(typ, true).pointerType()
      let cfx = g.b.buildBitCast(prc, cft, g.nn("call.iter.prc", n))
      let clargs = args & @[env]
      callres = g.b.buildCall(cfx, clargs)
    else:
      let pre = g.b.getInsertBlock()
      let f = pre.getBasicBlockParent()

      let clonil = f.appendBasicBlock(g.nn("call.clo.noenv", n))
      let cloenv = f.appendBasicBlock(g.nn("call.clo.env", n))
      let cloend = f.appendBasicBlock(g.nn("call.clo.end", n))

      let cmp = g.b.buildICmp(
        llvm.IntEQ, env, llvm.constNull(env.typeOfX()), g.nn("call.clo.noenv", n))
      discard g.b.buildCondBr(cmp, clonil, cloenv)

      g.b.positionBuilderAtEnd(clonil)

      fx = g.b.buildBitCast(prc, nft, g.nn("call.clo.prc.noenv", n))

      let res = g.b.buildCall(fx, args)
      discard g.b.buildBr(cloend)

      g.b.positionBuilderAtEnd(cloenv)

      let cft = g.llProcType(typ, true).pointerType()
      let cfx = g.b.buildBitCast(prc, cft, g.nn("call.clo.prc", n))

      let clargs = args & @[env]
      let cres = g.b.buildCall(cfx, clargs)

      discard g.b.buildBr(cloend)

      g.b.positionBuilderAtEnd(cloend)

      if retty.getTypeKind() != llvm.VoidTypeKind:
        callres = g.b.buildPHI(res.typeOfX(), g.nn("call.clo.res", n))
        callres.addIncoming([res, cres], [clonil, cloenv])
  else:
    if fx.typeOfX().getElementType().getTypeKind() != llvm.FunctionTypeKind:
      fx = g.b.buildBitCast(fx, nft, g.nn("call.fx", n))

    let args = g.genCallArgs(n, fx.typeOfX().getElementType(), typ)
    let varname =
      if retty.getTypeKind() != llvm.VoidTypeKind: g.nn("call.res", n) else: ""
    callres = g.b.buildCall(fx, args, varname)

  if retty.getTypeKind() != llvm.VoidTypeKind and not load and
      nf.typ.sons[0].kind != tyRef:
    # if the originator of the call wants a pointer, we'll have
    # to create one for them - this is interesting for example
    # when a struct is returned "by value"
    result = g.localAlloca(retty, g.nn("call.res.ptr", n))
    discard g.b.buildStore(callres, result)
  else:
    result = callres

proc genMagicCall(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let s = n[namePos].sym
  if lfNoDecl notin s.loc.flags:
    let m = g.graph.getCompilerProc($s.loc.r)
    if m == nil: g.config.internalError(n.info, "Missing magic: " & $s.loc.r)
    discard g.genFunctionWithBody(m)

  result = g.genCall(n, load)

proc genRefAssign(g: LLGen, v, t: llvm.ValueRef) =
  if g.config.usesWriteBarrier():
    discard g.callCompilerProc("unsureAsgnRef", [t, v])
  else:
    discard g.b.buildStore(
      g.b.buildBitCast(v, t.typeOfX().getElementType(), g.nn("bc", v)), t)

proc callGenericAssign(g: LLGen, t, x: llvm.ValueRef, ty: PType, deep: bool) =
  let f = if deep: "genericAssign" else: "genericShallowAssign"
  discard g.callCompilerProc(f, [t, x, g.genTypeInfo(ty)])

proc genAsgnFromRef(g: LLGen, v, t: llvm.ValueRef, typ: PType, deep = true) =
  let tt = t.typeOfX()

  if tt.getTypeKind() != llvm.PointerTypeKind:
    g.config.internalError("Ptr required in genAssignment: " & $t)

  let tet = tt.getElementType()

  let ty = typ.skipTypes(abstractRange)
  case ty.kind
  of tyRef:
    let x = g.buildLoadValue(v)
    g.genRefAssign(x, t)
  of tySequence:
    let x = g.buildLoadValue(v)
    if deep:  # TODO or OnStack?
      discard g.callCompilerProc("genericSeqAssign", [t, x, g.genTypeInfo(ty)])
    else:
      g.genRefAssign(x, t)
  of tyString:
    let x = g.buildLoadValue(v)
    if deep:
      let sx = g.callCompilerProc("copyString", [x])
      g.genRefAssign(sx, t)
    else:
      g.genRefAssign(x, t)
  of tyProc:
    let x = g.buildLoadValue(v)

    if ty.containsGarbageCollectedRef():
      let tp = g.b.buildGEP(t, [g.gep0, g.gep0])
      let p = g.b.buildExtractValue(x, 0, g.nn("asgn.p", v))
      discard g.b.buildStore(
        g.b.buildBitCast(p, tp.typeOfX().getElementType(), g.nn("asgn.pc", v)), tp)

      let te = g.b.buildGEP(t, [g.gep0, g.gep1])
      let e = g.b.buildExtractValue(x, 1, g.nn("asgn.e", v))

      g.genRefAssign(e, te)
    else:
      discard g.b.buildStore(g.b.buildBitCast(x, tet, g.nn("asgn.xc")), t)
  of tyTuple:
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, v, ty, deep)
    else:
      g.callMemcpy(t, v, t.typeOfX().getElementType().sizeOfX())

  of tyObject:
    if ty.hasTypeField() or ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, v, ty, deep)
    else:
      g.callMemcpy(t, v, t.typeOfX().getElementType().sizeOfX())

  of tyArray:
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, v, ty, deep)
    else:
      g.callMemcpy(t, v, t.typeOfX().getElementType().sizeOfX())

  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      let x = g.buildLoadValue(v)
      discard g.b.buildStore(x, t)
    else:
      g.callMemcpy(t, v, t.typeOfX().getElementType().sizeOfX())
  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar:
    let x = g.buildLoadValue(v)
    discard g.b.buildStore(g.b.buildBitCast(x, tet, g.nn("asgn.c", v)), t)

  else: g.config.internalError("genAssignment: " & $ty.kind)

proc genAssignment(
  g: LLGen, v: PNode, t: llvm.ValueRef, typ: PType, deep = true) =

  let tt = t.typeOfX()

  if tt.getTypeKind() != llvm.PointerTypeKind:
    g.config.internalError("Ptr required in genAssignment: " & $t)

  let tet = tt.getElementType()

  let ty = typ.skipTypes(abstractRange + tyUserTypeClasses + {tyStatic})
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
      let tp = g.b.buildGEP(t, [g.gep0, g.gep0])
      let p = g.b.buildExtractValue(x, 0, g.nn("asgn.p", v))
      discard g.b.buildStore(
        g.b.buildBitCast(p, tp.typeOfX().getElementType(), g.nn("asgn.pc", v)), tp)

      let te = g.b.buildGEP(t, [g.gep0, g.gep1])
      let e = g.b.buildExtractValue(x, 1, g.nn("asgn.e", v))

      g.genRefAssign(e, te)
    else:
      discard g.b.buildStore(g.b.buildBitCast(x, tet, g.nn("asgn.xc")), t)
  of tyTuple:
    let x = g.genNode(v, false)
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, x, ty, deep)
    else:
      g.callMemcpy(t, x, t.typeOfX().getElementType().sizeOfX())

  of tyObject:
    let x = g.genNode(v, false)
    if ty.hasTypeField() or ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, x, ty, deep)
    else:
      g.callMemcpy(t, x, t.typeOfX().getElementType().sizeOfX())

  of tyArray:
    let x = g.genNode(v, false)
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(t, x, ty, deep)
    else:
      g.callMemcpy(t, x, t.typeOfX().getElementType().sizeOfX())
  of tyOpenArray, tyVarargs:
    let s = if v.kind == nkHiddenDeref: v[0] else: v
    let p = g.b.buildLoad(g.symbols[s.sym.id], "")
    let len = g.b.buildLoad(g.symbols[-s.sym.id], "")
    if ty.containsGarbageCollectedRef():
      discard g.callCompilerProc(
        "genericAssignOpenArray", [t, p, len, g.genTypeInfo(ty)])
    else:
      g.callMemcpy(t, p, t.typeOfX().getElementType().sizeOfX())
  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      let x = g.genNode(v, true)
      discard g.b.buildStore(x, t)
    else:
      let x = g.genNode(v, false)
      g.callMemcpy(t, x, t.typeOfX().getElementType().sizeOfX())
  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar:
    let x = g.genNode(v, true)
    let pc = g.preCast(g.isUnsigned(typ), x, typ, tet)
    discard g.b.buildStore(pc, t)

  of tyNil:
    discard g.b.buildStore(constNull(tet), t)

  else:
    g.config.internalError(v.info, "genAssignment: " & $ty.kind)

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
    if n.typ.kind notin {tyArray, tySequence, tyUncheckedArray}: return false
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
  of nkStrLit..nkTripleStrLit: result = g.constNimString(n)
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

  of nkCurly: result = if n.isDeepConstExpr(): g.constNimSet(n) else: nil
  of nkBracket:
    if n.typ.kind notin {tyArray, tySequence, tyUncheckedArray}: return nil

    var vals: seq[llvm.ValueRef] = @[]
    for s in n.sons:
      if s.isSeqLike():
        # Need pointer for string literals
        vals.add(g.genNode(s, false))
      else:
        vals.add(g.genConstInitializer(s))

    if n.typ.elemType.kind == tyEmpty:
      assert n.typ.kind == tySequence

      let x = llvm.constNamedStruct(g.seqBaseType, [g.ni0, g.ni0])
      result = llvm.constStruct([x])
    else:
      let et = g.llType(n.typ.elemType)

      let s = constArray(et, vals)
      if n.typ.kind in {tyArray, tyUncheckedArray}:
        result = s
      else:
        let ll = g.constNimInt(vals.len)
        let x = llvm.constNamedStruct(g.seqBaseType, [ll, ll])
        result = llvm.constStruct([x, s])

  of nkObjConstr:
    let t = g.llType(n.typ)
    if n.isDeepConstExprLL(false):
      var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())
      for i in 0..<vals.len:
        vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))
      for i in 1..<n.len:
        let s = n[i]
        let ind = g.fieldIndex(n.typ, s[0].sym)[0]
        if n[i].isSeqLike():
          # Need pointer for string literals
          vals[ind] = g.genNode(n[i], true)
        else:
          vals[ind] = g.genConstInitializer(n[i])
      result = constNamedStruct(t, vals)
    else:
      result = nil
  else: result = nil

proc genConst(g: LLGen, n: PNode): llvm.ValueRef =
  let sym = n.sym
  if sym.id in g.symbols: return g.symbols[sym.id]

  let init = sym.ast

  if init.isDeepConstExprLL(true):
    result = g.genGlobal(n)

    result.setGlobalConstant(llvm.True)

    let ci = g.genConstInitializer(init)
    if ci == nil: g.config.internalError(n.info, "Unable to generate const initializer: " & $init)

    # TODO when enabled, ptr equality checks (for example in isObj) get
    #      optimized away - need to consider when it's safe
    # result.setUnnamedAddr(llvm.True)

    case sym.typ.kind
    of tyArray, tyUncheckedArray, tySet, tyTuple: result.setInitializer(ci)
    else:
      let c = g.m.addPrivateConstant(ci.typeOfX(), g.nn(".const.init", n))
      c.setInitializer(ci)
      result.setInitializer(llvm.constBitCast(c, result.typeOfX().getElementType()))
    return

  if sfGlobal in sym.flags:
    result = g.genGlobal(n)
    g.registerGcRoot(sym, result)
  else:
    if sym.id in g.symbols: return g.symbols[sym.id]

    result = g.localAlloca(g.llType(sym.typ), sym.llName)
    # Some initializers expect value to be null, so we always set it so
    g.buildStoreNull(result)

  if init.kind != nkEmpty:
    g.genAssignment(init, result, sym.typ)

  if sfGlobal notin sym.flags:
    g.symbols[sym.id] = result

proc genMagicHigh(g: LLGen, n: PNode): llvm.ValueRef =
  let len = g.genMagicLength(n)
  result = g.b.buildSub(
    len, llvm.constInt(len.typeOfX(), 1, llvm.False), g.nn("high", len))

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

  while ax.typeOfX().getElementType().getTypeKind() == llvm.PointerTypeKind:
    ax = g.b.buildLoad(ax, g.nn("of.ind.load", n))

  let mt_gep = g.b.buildGEP(ax, g.mtypeIndex(t), g.nn("mtype", ax))
  let m_type = g.b.buildLoad(mt_gep, g.nn("of.load", n))
  let typ = n[2].typ.skipTypes(typedescPtrs)
  # TODO nil check if n is a pointer
  result = g.callCompilerProc("isObj", [m_type, g.genTypeInfo(typ)])

proc genMagicEcho(g: LLGen, n: PNode) =
  let b = n[1].skipConv

  if b.len == 0:
    let t = g.nimStringDescPtrType.pointerType()
    discard g.callCompilerProc("echoBinSafe", [constNull(t), g.constNimInt(0)])

  let x = g.genNode(b, true)
  let y = g.constNimInt(b.len)
  discard g.callCompilerProc("echoBinSafe", [x, y])

proc genMagicUnaryLt(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = llvm.constInt(ax.typeOfX(), 1, False)
  if optOverflowCheck in g.f.options:
    result = g.callBinOpWithOver(ax, bx, llvm.Sub, n)
  else:
    result = g.b.buildSub(ax, bx, g.nn("lt", n))

proc genMagicIncDec(g: LLGen, n: PNode, op: Opcode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    a = g.b.buildLoad(ax, g.nn("inc.a", n))
    b = g.buildTruncOrExt(bx, a.typeOfX(), n[2].typ)

  let t = n[1].typ.skipTypes({tyGenericInst, tyVar, tyRange})
  if optOverflowCheck notin g.f.options or t.kind in {tyUInt..tyUInt64}:
    let nv = g.b.buildBinOp(op, a, b, g.nn("inc.nv", n))
    discard g.b.buildStore(nv, ax)
  else:
    let nv = g.callBinOpWithOver(a, b, op, n[1])
    discard g.b.buildStore(nv, ax)

proc genMagicOrd(g: LLGen, n: PNode): llvm.ValueRef =
  var ax = g.genNode(n[1], true)

  if n[1].typ.skipTypes(abstractRange).kind == tyBool:
    # make sure we don't get more bits set than necessary
    ax = g.b.buildAnd(ax, llvm.constInt(ax.typeOfX(), 1.cuint, llvm.False),
      g.nn("ord.bool", n))
  result = g.buildTruncOrExt(ax, g.llType(n.typ), n[1].typ)

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
  ti.setInitializer(constNamedStruct(init.typeOfX(), [
    init.getOperand(0),
    init.getOperand(1),
    init.getOperand(2),
    init.getOperand(3),
    init.getOperand(4),
    llvm.constBitCast(f, g.voidPtrType),
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
    at = ax.typeOfX()

  if at.getTypeKind() != llvm.PointerTypeKind:
    g.config.internalError("expected pointer, not " & $at)

  let x = g.cpNewSeq(n[1].typ, bx, g.nn("newseq", n))
  g.genRefAssign(x, ax)

proc genMagicNewSeqOfCap(g: LLGen, n: PNode): llvm.ValueRef =
  let seqtype = n.typ.skipTypes(abstractVarRange)
  let ax = g.genNode(n[1], true)
  let ti =  g.genTypeInfo(seqtype)

  result = g.callCompilerProc("nimNewSeqOfCap", [ti, ax])

proc genMagicLengthOpenArray(g: LLGen, n: PNode): llvm.ValueRef =
  # openarray must be a parameter so we should find it in the scope
  let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]
  result = g.b.buildLoad(g.symbols[-s.sym.id], "")

proc genMagicLengthStr(g: LLGen, n: PNode): llvm.ValueRef =
  let v = g.genNode(n[1], true)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let lload = f.appendBasicBlock(g.nn("str.len.load", n))
  let ldone = f.appendBasicBlock(g.nn("str.len.done", n))

  # nil check
  let cond = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("str.len.isnil", n))

  discard g.b.buildCondBr(cond, ldone, lload)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(lload)
  let
    strlenType = llvm.functionType(g.csizetType, [g.primitives[tyCString]])
    strlen = g.m.getOrInsertFunction("strlen", strlenType)

  let v1 = g.b.buildCall(strlen, [v])
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(g.primitives[tyInt], g.nn("str.len", n))

  let v0 = g.ni0
  phi.addIncoming([v0, v1], [pre, lload])

  result = phi

proc genMagicLengthSeq(g: LLGen, n: PNode): llvm.ValueRef =
  let v = g.genNode(n[1], true)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let lload = f.appendBasicBlock(g.nn("seq.len.load", n))
  let ldone = f.appendBasicBlock(g.nn("seq.len.done", n))

  # nil check
  let cond = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("seq.len.isnil", n))

  discard g.b.buildCondBr(cond, ldone, lload)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(lload)
  let gep = g.buildNimSeqLenGEP(v)
  let v1 = g.b.buildLoad(gep, g.nn("seq.len.load", n))
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(g.primitives[tyInt], g.nn("seq.len", n))

  let v0 = g.ni0
  phi.addIncoming([v0, v1], [pre, lload])

  result = phi

proc genMagicLength(g: LLGen, n: PNode): llvm.ValueRef =
  let typ = skipTypes(n[1].typ, abstractVar + tyUserTypeClasses)
  case typ.kind
  of tyOpenArray, tyVarargs: result = g.genMagicLengthOpenArray(n)
  of tyCString: result = g.genMagicLengthStr(n)
  of tySequence, tyString: result = g.genMagicLengthSeq(n)
  of tyArray: result = g.constNimInt(g.config.lengthOrd(typ).int)
  else: g.config.internalError(n.info, "genMagicLength " & $n[1].typ)

proc genMagicXLen(g: LLGen, n: PNode): llvm.ValueRef =
  let v = g.genNode(n[1], true)

  # load length if v is not nil
  let gep = g.buildNimSeqLenGEP(v)
  result = g.b.buildLoad(gep, g.nn("seq.len.load", n))

proc genMagicIncl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    let b = g.buildSetMask(ax.typeOfX().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildOr(g.b.buildLoad(ax, ""), b, "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], true)

    let (gep, mask) = g.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildOr(a, mask, "")
    discard g.b.buildStore(res, gep)

proc genMagicExcl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    let b = g.buildSetMask(ax.typeOfX().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildAnd(g.b.buildLoad(ax, ""), g.buildBitnot(b), "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], true)

    let (gep, mask) = g.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildAnd(a, g.buildBitnot(mask), "")
    discard g.b.buildStore(res, gep)

proc genMagicCard(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    result = g.callCtpop(ax, size)
    result = g.buildTruncOrExt(result, g.llType(n.typ), true)
  else:
    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("card.i", n))
    discard g.b.buildStore(g.ni0, i)

    let tot = g.localAlloca(g.llType(n.typ), g.nn("card.tot", n))
    g.buildStoreNull(tot)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(g.nn("card.cmp", n))
    let rloop = f.appendBasicBlock(g.nn("card.loop", n))
    let rdone = f.appendBasicBlock(g.nn("card.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, g.nn("card.il", n))
    let cond = g.b.buildICmp(
      llvm.IntSLT, il, g.constNimInt(size.int), g.nn("card.slt", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let ai = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("card.ax", n))
    let a = g.callCtpop(ai, 1)
    let b = g.b.buildLoad(tot, g.nn("card.tot.load", n))
    let c = g.b.buildAdd(
      g.buildTruncOrExt(a, b.typeOfX(), true), b, g.nn("card.add", n))
    discard g.b.buildStore(c, tot)

    # inc idx
    let next = g.b.buildAdd(
      il, constInt(il.typeOfX(), 1, llvm.False), g.nn("card.inc", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildLoad(tot, g.nn("card", n))

proc genMagicChr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let t = g.llType(n.typ)
  if t != ax.typeOfX():
    result = g.b.buildTrunc(ax, t, g.nn("chr.t", n))
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

  if ax.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():

    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    ax = g.buildNimIntExt(ax, g.isUnsigned(n[1].typ))
    bx = g.buildNimIntExt(bx, g.isUnsigned(n[2].typ))

  let bo = g.b.buildBinOp(op, ax, bx, g.nn("binop." & $op, n))
  result = g.b.buildTrunc(bo, g.llType(n.typ), g.nn("binop.trunc", n))

proc genMagicBinOpOverflow(g: LLGen, n: PNode, op: Opcode): llvm.ValueRef =
  if optOverflowCheck notin g.f.options:
    return g.genMagicBinOp(n, op)

  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  if g.isUnsigned(n.typ) or g.isUnsigned(n[1].typ) or g.isUnsigned(n[2].typ):
    g.config.internalError(n.info, "Unsigned types shouldn't use overflow semantics")

  if ax.typeOfX().getTypeKind() != llvm.IntegerTypeKind or
      bx.typeOfX().getTypeKind() != llvm.IntegerTypeKind:
    g.config.internalError(n.info, "Integers only here, please")

  if ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():
    g.config.internalError(n.info, "Expected nkHiddenStdConv to take care of this")

  let bo = g.callBinOpWithOver(ax, bx, op, n)
  result = g.b.buildTrunc(bo, g.llType(n.typ), g.nn("binop.over.trunc", n))

proc genMagicBinOpCall(g: LLGen, n: PNode, op: Opcode): llvm.ValueRef =
  if optOverflowCheck notin g.f.options:
    return g.genMagicBinOp(n, op)

  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  ax = g.buildNimIntExt(ax, g.isUnsigned(n[1].typ))
  bx = g.buildNimIntExt(bx, g.isUnsigned(n[2].typ))

  var opfn: string
  case op
  of llvm.SDiv: opfn = "divInt"
  of llvm.SRem: opfn = "modInt"
  else: g.config.internalError("Unexpected op: " & $op)

  let bo = g.callCompilerProc(opfn, [ax, bx])
  result = g.b.buildTrunc(bo, g.llType(n.typ), g.nn("binop.call.trunc", n))

proc genMagicBinOpF(g: LLGen, n: PNode, op: Opcode, unsigned = false): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  result = g.b.buildBinOp(op, ax, bx, g.nn("binop." & $op, n))

  if optNaNCheck in g.f.options:
    discard g.callCompilerProc("nanCheck", [result])
  if optInfCheck in g.f.options:
    discard g.callCompilerProc("infCheck", [result])

proc genMagicShr(g: LLGen, n: PNode): llvm.ValueRef =
  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  if ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():

    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    bx = g.buildTruncOrExt(bx, ax.typeOfX(), true)

  result = g.b.buildBinOp(llvm.LShr, ax, bx, g.nn("binop." & $llvm.LShr, n))

proc genMagicAShr(g: LLGen, n: PNode): llvm.ValueRef =
  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  if ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():

    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    bx = g.buildTruncOrExt(bx, ax.typeOfX(), true)

  result = g.b.buildBinOp(llvm.AShr, ax, bx, g.nn("binop." & $llvm.AShr, n))

proc genMagicMinMaxI(g: LLGen, n: PNode, op: IntPredicate): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    cmp = g.b.buildICmp(op, ax, bx, g.nn($op & ".cmp", n))

  result = g.b.buildSelect(cmp, ax, bx, g.nn($op, n))

proc genMagicMinMaxF(g: LLGen, n: PNode, op: RealPredicate): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    cmp = g.b.buildFCmp(op, ax, bx, g.nn($op & ".cmp", n))

  result = g.b.buildSelect(cmp, ax, bx, g.nn($op, n))

proc genMagicCmpI(g: LLGen, n: PNode, op: IntPredicate): llvm.ValueRef =
  var
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  if ax.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():
    # TODO should probably extend to the biggest of the two, rather than
    # full int
    ax = g.buildNimIntExt(ax, g.isUnsigned(n[1].typ))
    bx = g.buildNimIntExt(bx, g.isUnsigned(n[2].typ))

  # unsigned doesn't matter here - we've already normalized ints
  result = g.buildI8(g.b.buildICmp(
    op, g.preCast(false, ax, n[1].typ), g.preCast(false, bx, n[1].typ),
    g.nn("icmp." & $op, n)))

proc genMagicCmpF(g: LLGen, n: PNode, op: RealPredicate): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)

  result = g.buildI8(g.b.buildFCmp(op, ax, bx, g.nn($op, n)))

proc genMagicEqProc(g: LLGen, n: PNode): llvm.ValueRef =
  if n[1].typ.skipTypes(abstractInst).callConv == ccClosure:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], false)
      a0 = g.b.buildGEP(ax, [g.gep0, g.gep0])
      a1 = g.b.buildGEP(ax, [g.gep0, g.gep1])
      b0 = g.b.buildGEP(bx, [g.gep0, g.gep0])
      b1 = g.b.buildGEP(bx, [g.gep0, g.gep1])

    let x0 = g.b.buildICmp(
      llvm.IntEQ,
      g.b.buildLoad(a0, g.nn("eq.prc.a0", n)),
      g.b.buildLoad(b0, g.nn("eq.prc.b0", n)), g.nn("eq.prc.0", n))
    let x1 = g.b.buildICmp(
      llvm.IntEQ,
      g.b.buildLoad(a1, g.nn("eq.prc.a1", n)),
      g.b.buildLoad(b1, g.nn("eq.prc.b1", n)), g.nn("eq.prc.1", n))

    result = g.b.buildAnd(x0, x1, g.nn("eq.prc", n))
  else:
    let
      ax = g.genNode(n[1], true)
      bx = g.genNode(n[2], true)

    result = g.b.buildICmp(llvm.IntEQ, ax, bx, g.nn("eq.prc", n))
  result = g.buildI8(result)

proc genMagicUnaryMinus(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  return g.b.buildSub(llvm.constInt(ax.typeOfX(), 0, llvm.False),
                      ax, g.nn("neg", ax))

proc genMagicAbsI(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let linv = f.appendBasicBlock(g.nn("abs.inv", n))
  let ldone = f.appendBasicBlock(g.nn("abs.done", n))

  # >=0 check
  let cmp = g.b.buildICmp(
    llvm.IntSGE, ax, llvm.constInt(ax.typeOfX(), 0, llvm.False), g.nn("abs.sge", n))

  discard g.b.buildCondBr(cmp, ldone, linv)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(linv)
  let v1 = g.b.buildSub(llvm.constInt(ax.typeOfX(), 0, llvm.False),
                        ax, g.nn("abs.neg", n))
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(ax.typeOfX(), g.nn("abs", n))

  phi.addIncoming([ax, v1], [pre, linv])

  result = phi

proc genMagicNot(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  result = g.buildNot(ax)

proc genMagicBitnot(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  result = g.buildBitnot(ax)

proc genMagicUnaryMinusF64(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  result = g.b.buildFSub(
    llvm.constReal(ax.typeOfX(), 0.0.cdouble), ax, g.nn("neg", ax))

proc genMagicAbsF64(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let
    fabsType = llvm.functionType(llvm.doubleTypeInContext(g.lc), [llvm.doubleTypeInContext(g.lc)])
    fabs = g.m.getOrInsertFunction("llvm.fabs.f64", fabsType)

  result = g.b.buildCall(fabs, [ax], g.nn("fabs", n))

proc genMagicZe(g: LLGen, n: PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildZExt(a, g.llType(n.typ), g.nn("zext", n))

proc genMagicToU(g: LLGen, n: PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildTrunc(a, g.llType(n.typ), g.nn("trunc", n))

proc genMagicToFloat(g: LLGen, n:PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildSIToFP(a, g.llType(n.typ), g.nn("sitofp", n))

proc genMagicToInt(g: LLGen, n:PNode): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.b.buildFPToSI(a, g.llType(n.typ), g.nn("fptosi", n))

proc genMagicToStr(g: LLGen, n: PNode, f: string): llvm.ValueRef =
  let a = g.genNode(n[1], true)

  result = g.callCompilerProc(f, [a])

proc genMagicStrToStr(g: LLGen, n: PNode): llvm.ValueRef =
  return g.genNode(n[1], true)

proc genMagicEnumToStr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)

  let typ = skipTypes(n[1].typ, abstractVarRange)
  result = g.callCompilerProc(
    "reprEnum", [g.buildTruncOrExt(ax, g.primitives[tyInt], typ), g.genTypeInfo(typ)])

proc genMagicAndOr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[1], true)
  let a = g.buildI1(ax)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let aonext = f.appendBasicBlock(g.nn("andor.next", n))
  let aoend = f.appendBasicBlock(g.nn("andor.end", n))

  if n[0].sym.magic == mAnd:
    discard g.b.buildCondBr(a, aonext, aoend)
  else:
    discard g.b.buildCondBr(a, aoend, aonext)

  g.b.positionBuilderAtEnd(aonext)

  let bx = g.genNode(n[2], true)
  let bend = g.b.getInsertBlock()
  discard g.b.buildBr(aoend)

  g.b.positionAndMoveToEnd(aoend)

  let phi = g.b.buildPHI(ax.typeOfX(), g.nn("and.res", n))

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

  result = g.buildI8(g.b.buildICmp(llvm.IntSLE, cmp, g.ni0, g.nn("le.str", n)))

proc genMagicLtStr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    cmp = g.callCompilerProc("cmpStrings", [ax, bx])

  result = g.buildI8(g.b.buildICmp(llvm.IntSLT, cmp, g.ni0, g.nn("lt.str", n)))

proc genMagicSetCmp(g: LLGen, strict: bool, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    let b = g.buildBitnot(bx)
    let ab = g.b.buildAnd(ax, b, g.nn("setcmp.ab"))
    let le = g.b.buildICmp(
      llvm.IntEQ, ab, llvm.constInt(ab.typeOfX(), 0, llvm.False), g.nn("setcmp.le", n))
    if strict:
      let ne = g.b.buildICmp(llvm.IntNE, ax, bx, g.nn("setcmp.ne", n))
      result = g.b.buildAnd(le, ne, g.nn("setcmp.res", n))
    else:
      result = le
    result = g.buildI8(result)
  else:
    let o = g.localAlloca(g.primitives[tyBool], g.nn("setcmp.o", n))
    discard g.b.buildStore(g.nb[true], o)

    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("set.i", n))
    discard g.b.buildStore(g.ni0, i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(g.nn("setcmp.cmp", n))
    let rloop = f.appendBasicBlock(g.nn("setcmp.loop", n))
    let rinc = f.appendBasicBlock(g.nn("setcmp.inc", n))
    let rfalse = f.appendBasicBlock(g.nn("setcmp.false", n))
    let rdone = f.appendBasicBlock(g.nn("setcmp.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, g.nn("setcmp.il", n))
    let cond = g.b.buildICmp(
      llvm.IntSLT, il, g.constNimInt(size.int), g.nn("setcmp.isdone", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let al = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("setcmp.al", n))
    let bl = g.b.buildLoad(g.b.buildGEP(bx, [il]), g.nn("setcmp.bl", n))
    let b = g.buildBitnot(bl)

    var cont: llvm.ValueRef
    let ab = g.b.buildAnd(al, b, g.nn("setcmp.ab", n))
    let le = g.b.buildICmp(
      llvm.IntEQ, ab, llvm.constInt(ab.typeOfX(), 0, llvm.False), g.nn("setcmp.eq", n))
    if strict:
      let ne = g.b.buildICmp(llvm.IntNE, ax, bx, g.nn("setcmp.ne"))
      cont = g.b.buildAnd(le, ne, g.nn("setcmp.cont", n))
    else:
      cont = le

    discard g.b.buildCondBr(cont, rinc, rfalse)

    g.b.positionBuilderAtEnd(rinc)

    # inc idx
    let next = g.b.buildAdd(
      il, constInt(il.typeOfX(), 1, llvm.False), g.nn("setcmp.inc", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    g.b.positionBuilderAtEnd(rfalse)
    discard g.b.buildStore(g.nb[false], o)
    discard g.b.buildBr(rdone)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.buildLoadValue(o)

proc genMagicEqSet(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    result = g.buildI8(g.b.buildICmp(llvm.IntEQ, ax, bx, g.nn("seteq.eq", n)))
  else:
    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("seteq.i", n))
    discard g.b.buildStore(g.ni0, i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(g.nn("seteq.cmp", n))
    let rloop = f.appendBasicBlock(g.nn("seteq.loop", n))
    let rne = f.appendBasicBlock(g.nn("seteq.ne", n))
    let rinc = f.appendBasicBlock(g.nn("seteq.inc", n))
    let rdone = f.appendBasicBlock(g.nn("seteq.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, g.nn("seteq.il", n))
    let cond = g.b.buildICmp(
      llvm.IntSLT, il, g.constNimInt(size.int), g.nn("seteq.cond", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let a = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("seteq.a", n))
    let b = g.b.buildLoad(g.b.buildGEP(bx, [il]), g.nn("seteq.b", n))

    let cmp = g.b.buildICmp(llvm.IntEQ, a, b, g.nn("seteq.cmp", n))
    discard g.b.buildCondBr(cmp, rinc, rne)
    g.b.positionBuilderAtEnd(rne)

    discard g.b.buildBr(rdone)

    # inc idx
    g.b.positionBuilderAtEnd(rinc)
    let next = g.b.buildAdd(il, constInt(il.typeOfX(), 1, llvm.False), g.nn("set.inc", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildPHI(g.primitives[tyBool], g.nn("set.eq", n))
    result.addIncoming([g.nb[false], g.nb[true]], [rne, rcmp])

proc genMagicSetBinOp(g: LLGen, op: llvm.Opcode, invert: bool, n: PNode): llvm.ValueRef =
  let
    ax = g.genNode(n[1], true)
    bx = g.genNode(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)

  let size = g.config.getSize(typ)
  if size <= 8:
    let
      a = ax
      b = bx
    let s = if invert: g.buildBitnot(b)
            else: b
    result = g.b.buildBinOp(op, a, s, g.nn("setbo.res"))
  else:
    let tmp = g.localAlloca(g.llType(typ), g.nn("setbo.tmp", n))

    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("setbo.i", n))
    discard g.b.buildStore(g.ni0, i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock(g.nn("setbo.cmp", n))
    let rloop = f.appendBasicBlock(g.nn("setbo.loop", n))
    let rdone = f.appendBasicBlock(g.nn("setbo.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, g.nn("setbo.il", n))
    let cond = g.b.buildICmp(llvm.IntSLT, il, g.constNimInt(size.int), g.nn("setbo.cond", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let a = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("setbo.al", n))
    let b = g.b.buildLoad(g.b.buildGEP(bx, [il]), g.nn("setbo.bl", n))

    let s = if invert: g.buildBitnot(b) else: b
    let o = g.b.buildBinOp(op, a, s, g.nn("setbo.op", n))

    let p = g.b.buildGEP(tmp, [g.gep0, il])
    discard g.b.buildStore(o, p)

    # inc idx
    let next = g.b.buildAdd(il, constInt(il.typeOfX(), 1, llvm.False), g.nn("setbo.next", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.buildLoadValue(tmp)

proc genMagicConStrStr(g: LLGen, n: PNode): llvm.ValueRef =
  # First, find out total length of all strings
  var tgtlen = g.ni0

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
      let slen = g.b.buildLoad(g.buildNimSeqLenGEP(sx), g.nn("constrstr.slen", n))
      tgtlen = g.b.buildAdd(tgtlen, slen, g.nn("constrstr.tgtlen", n))

  if constlen > 0:
    tgtlen = g.b.buildAdd(tgtlen, g.constNimInt(constlen), g.nn("constrstr.tgtlen", n))

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
    a = g.b.buildLoad(ax, g.nn("load.str.addc", n))
    ret = g.callCompilerProc("addChar", [a, bx])

  discard g.b.buildStore(ret, ax)

proc genMagicAppendStrStr(g: LLGen, n: PNode) =
  let tgtp = g.genNode(n[1], false)
  var tgt = g.b.buildLoad(tgtp, g.nn("load.str.adds", n))

  # First, find out total length of the new strings
  var tgtlen = g.ni0

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
        g.buildNimSeqLenGEP(sx), g.nn("load.str.adds.len." & $i, n))
      tgtlen = g.b.buildAdd(tgtlen, slen, g.nn("str.adds.tot." & $i, n))

  if constlen > 0:
    tgtlen = g.b.buildAdd(
      tgtlen, g.constNimInt(constlen), g.nn("str.adds.tot.const", n))

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
    bx = g.genNode(n[2], false)
    seqType = n[1].typ.skipTypes({tyVar})
    a = g.b.buildLoad(ax, g.nn("seq.add.old", n))
    ap = g.b.buildGEP(a, [g.gep0, g.gep0])
    newseq = g.callCompilerProc("incrSeqV3", [ap, g.genTypeInfo(seqType)])
    tgt = g.b.buildBitCast(newseq, a.typeOfX(), g.nn("seq.add.new", n))
    lenp = g.buildNimSeqLenGEP(tgt)
    len = g.b.buildLoad(lenp, g.nn("load.seq.add.last", n))

  discard g.b.buildStore(tgt, ax)

  g.genAssignment(n[2], g.buildNimSeqDataGEP(tgt, len), seqType.elemType)

  let newlen = g.b.buildAdd(
    len, llvm.constInt(len.typeOfX(), 1, llvm.False), g.nn("seq.add.newlen", n))
  discard g.b.buildStore(newlen, lenp)

# Here, we need to emulate the C compiler and generate comparisons instead of
# sets, else we'll have crashes when out-of-range ints are compared against
# curlies

# from C compiler
proc fewCmps(g: LLGen, s: PNode): bool =
  # this function estimates whether it is better to emit code
  # for constructing the set or generating a bunch of comparisons directly
  if s.kind != nkCurly: g.config.internalError(s.info, "fewCmps")
  if (g.config.getSize(s.typ) <= g.config.target.intSize) and (nfAllConst in s.flags):
    result = false            # it is better to emit the set generation code
  elif elemType(s.typ).kind in {tyInt, tyInt16..tyInt64}:
    result = true             # better not emit the set if int is basetype!
  else:
    result = s.sonsLen <= 8  # 8 seems to be a good value

proc genMagicInSet(g: LLGen, n: PNode): llvm.ValueRef =
  if (n[1].kind == nkCurly) and g.fewCmps(n[1]):
    let s = if n[2].kind in {nkChckRange, nkChckRange64}: n[2][0]
            else: n[2]
    let sx = g.genNode(s, true)

    let res = g.localAlloca(g.primitives[tyBool], g.nn("inset.res", n))
    discard g.b.buildStore(g.nb[false], res)
    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let strue = f.appendBasicBlock(g.nn("inset.true", n))

    let length = n[1].sonsLen
    let u = g.isUnsigned(s.typ)
    for i in 0..<length:
      var cmp: llvm.ValueRef
      if n[1][i].kind == nkRange:
        let
          ax = g.genNode(n[1][i][0], true)
          bx = g.genNode(n[1][i][1], true)
          acmp = g.b.buildICmp(
            if u: llvm.IntUGE else: llvm.IntSGE, sx,
            g.buildTruncOrExt(ax, sx.typeOfX(), u), g.nn("inset.acmp", n))
          bcmp = g.b.buildICmp(
            if u: llvm.IntULE else: llvm.IntSLE, sx,
            g.buildTruncOrExt(bx, sx.typeOfX(), u), g.nn("inset.bcmp", n))

        cmp = g.b.buildAnd(acmp, bcmp, "")
      else:
        let ax = g.genNode(n[1][i], true)
        cmp = g.b.buildICmp(llvm.IntEQ, sx, g.buildTruncOrExt(ax, sx.typeOfX(), u), "")

      let snext = f.appendBasicBlock(g.nn("inset.cmp", n))
      discard g.b.buildCondBr(cmp, strue, snext)
      g.b.positionBuilderAtEnd(snext)

    let send = f.appendBasicBlock(g.nn("inset.end", n))
    discard g.b.buildBr(send)

    g.b.positionBuilderAtEnd(strue)
    discard g.b.buildStore(g.nb[true], res)
    discard g.b.buildBr(send)

    g.b.positionAndMoveToEnd(send)
    result = g.b.buildLoad(res, g.nn("inset.result", n))
    return

  let typ = skipTypes(n[1].typ, abstractVar)
  let size = g.config.getSize(typ)

  if size <= 8:
    let
      ax = g.genNode(n[1], true)
      bx = g.genNode(n[2], true)

    let b = g.buildSetMask(ax.typeOfX(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildAnd(ax, b, g.nn("inset.res", n))
    result = g.b.buildICmp(
      llvm.IntNE, res, llvm.constInt(ax.typeOfX(), 0, llvm.False),
      g.nn("inset.result", n))
  else:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], true)

    let (gep, mask) = g.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, g.nn("inset.a", n))
    let res = g.b.buildAnd(a, mask, g.nn("inset.res", n))
    result = g.b.buildICmp(
      llvm.IntNE, res, llvm.constInt(a.typeOfX(), 0, llvm.False),
      g.nn("inset.result", n))
  result = g.buildI8(result)

proc genMagicRepr(g: LLGen, n: PNode): llvm.ValueRef =
  let t = skipTypes(n[1].typ, abstractVarRange)
  case t.kind
  of tyInt..tyInt64, tyUInt..tyUInt64:
    let ax = g.genNode(n[1], true)
    result = g.callCompilerProc("reprInt", [g.buildTruncOrExt(ax, g.primitives[tyInt], t)])
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
    let a = g.buildTruncOrExt(ax, g.primitives[tyInt], t)
    result = g.callCompilerProc("reprEnum", [a, g.genTypeInfo(t)])
  of tyString:
    let ax = g.genNode(n[1], true)
    result = g.callCompilerProc("reprStr", [ax])
  of tySet:
    let ax = g.genNode(n[1], false)
    result = g.callCompilerProc("reprSet", [ax, g.genTypeInfo(t)])
  of tyOpenArray, tyVarargs:
    let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]
    let a = g.b.buildLoad(g.symbols[s.sym.id], "")
    let l = g.b.buildLoad(g.symbols[-s.sym.id], "")
    result = g.callCompilerProc("reprOpenArray", [a, l, g.genTypeInfo(t.elemType)])
  of tyCString, tyArray, tyUncheckedArray, tyRef, tyPtr, tyPointer, tyNil,
     tySequence:
    let ax = g.genNode(n[1], t.kind in {tyRef, tyPtr, tyVar, tyPointer})
    result = g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])
  of tyEmpty:
    g.config.localError(n.info, "'repr' doesn't support 'void' type")
  else:
    let ax = g.genNode(n[1], false)
    result = g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])

proc genMagicSetLengthStr(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    a = g.b.buildLoad(ax, g.nn("setlen.str", n))

  discard g.b.buildStore(g.callCompilerProc("setLengthStr", [a, bx]), ax)

proc genMagicSetLengthSeq(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true)
    a = g.b.buildLoad(ax, g.nn("setlen.seq", n))
    ap = g.b.buildGEP(a, [g.gep0, g.gep0])
    at = a.typeOfX()
    so = at.getElementType().structGetTypeAtIndex(1).getElementType().sizeOfX()
    x = g.callCompilerProc("setLengthSeq", [ap, so, bx])

  discard g.b.buildStore(g.b.buildBitCast(x, at, g.nn("setlen.cast", n)), ax)

proc genMagicParallel(g: LLGen, n: PNode) =
  # oddly, semparallel is called from the c gen (!)
  let n2 = g.graph.liftParallel(g.module.sym, n)
  g.genNode(n2)

proc genMagicSwap(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], false)
    t = g.llType(n[1].typ)
    tmpx = g.localAlloca(t, g.nn("swap.tmp", n))
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
      llvm.IntEQ, g.b.buildExtractValue(ax, 0, g.nn("isnil.prc", n)),
        llvm.constNull(g.voidPtrType), g.nn("isnil", n))
  else:
    result = g.b.buildICmp(
      llvm.IntEQ, ax, llvm.constNull(ax.typeOfX()), g.nn("isnil", n))
  result = g.buildI8(result)

proc genMagicArrToSeq(g: LLGen, n: PNode): llvm.ValueRef =
  let l = int(g.config.lengthOrd(skipTypes(n[1].typ, abstractInst)))
  let t = n.typ.skipTypes(abstractVarRange)
  result = g.cpNewSeq(t, g.constNimInt(l), g.nn("arrtoseq", n))

  let ax = g.genNode(n[1], true)

  for i in 0..<l:
    let tgt = g.buildNimSeqDataGEP(result, g.constGEPIdx(i))
    let src = g.b.buildGEP(ax, [g.constGEPIdx(i)])
    g.genAsgnFromRef(src, tgt, t.elemType)

proc genMagicSpawn(g: LLGen, n: PNode): llvm.ValueRef =
  let n2 = g.graph.wrapProcForSpawn(n.typ.sym.getModule(), n, n.typ, nil, nil)
  result = g.genNode(n2, true)

proc genMagicDeepCopy(g: LLGen, n: PNode) =
  let x = if n[1].kind in {nkAddr, nkHiddenAddr}: n[1][0] else: n[1]
  let ax = g.genNode(x, false)

  let ty = n[2].typ.skipTypes(abstractVarRange)
  case ty.kind
  of tyPtr, tyRef, tyProc, tyTuple, tyObject, tyArray, tyUncheckedArray:
    let bx = g.genNode(n[2], false)
    discard g.callCompilerProc("genericDeepCopy", [ax, bx, g.genTypeInfo(ty)])

  of tySequence, tyString:
    let bx = g.genNode(n[2], false)
    discard g.callCompilerProc("genericSeqDeepCopy", [ax, bx, g.genTypeInfo(ty)])

  of tyOpenArray, tyVarargs:
    g.config.internalError(n.info, "todo")

  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      let bx = g.genNode(n[2], true)
      discard g.b.buildStore(bx, ax)
    else:
      let bx = g.genNode(n[2], false)
      g.callMemcpy(ax, bx, ax.typeOfX().getElementType().sizeOfX())
  of tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar:
    let bx = g.genNode(n[2], true)
    discard g.b.buildStore(bx, ax)
  else:
    g.config.internalError(n.info, "genDeepCopy: " & $ty.kind)

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
  of mAShrI: result = g.genMagicAShr(n)
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
  else: g.config.internalError(n.info, "Unhandled magic: " & $op)

# Nodes
proc genNodeSym(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let sym = n.sym
  case sym.kind
  of skVar, skLet, skTemp, skResult, skForVar:
    var v: llvm.ValueRef
    if lfHeader in sym.loc.flags or lfNoDecl in sym.loc.flags:
      v = g.externGlobal(sym)
    elif sfGlobal in sym.flags:
      v = g.genGlobal(n)
    else:
      v = g.symbols[sym.id]

    if load and v.typeOfX().getTypeKind() == llvm.PointerTypeKind:
      result = g.buildLoadValue(v)
      if sfVolatile in sym.flags:
        # TODO writes...
        result.setVolatile(llvm.True)
    else:
      result = v
  of skParam:
    var v = g.symbols[sym.id]

    var toload = g.llPassAsPtr(n.typ)
    if toload and load and
        v.typeOfX().getElementType().getTypeKind() == llvm.PointerTypeKind:
      v = g.b.buildLoad(v, "")
      result = g.buildLoadValue(v)
    elif not load and not toload and
        sym.typ.kind notin {tyPtr, tyVar, tyRef, tyPointer}:
      # Someone wants an address, but all we have is a value...
      result = v
    else:
      result = g.b.buildLoad(v, "")
  of skType:
    result = g.genTypeInfo(sym.typ)
  of skMethod:
    if {sfDispatcher, sfForward} * sym.flags != {}:
      result = g.genFunction(sym)
    else:
      result = g.genFunctionWithBody(sym)
  of skProc, skConverter, skIterator, skFunc:
    if (lfNoDecl in sym.loc.flags or sym.magic != mNone or
         {sfImportc, sfInfixCall} * sym.flags != {}) and
         lfImportCompilerProc notin sym.loc.flags:
      result = g.genFunction(sym)
    else:
      result = g.genFunctionWithBody(sym)
  of skConst:
    let v = g.genConst(n)

    if load and v.typeOfX().getTypeKind() == llvm.PointerTypeKind:
      result = g.buildLoadValue(v)
    else:
      result = v

  of skEnumField:
    result = llvm.constInt(g.llType(sym.typ), sym.position.culonglong, llvm.False)

  else:
    g.config.internalError(n.info, "Unhandled nodesym kind: " & $sym.kind)

proc genNodeIntLit(g: LLGen, n: PNode): llvm.ValueRef =
  let nt = g.llType(n.typ)
  if nt.getTypeKind == llvm.PointerTypeKind:
    result = llvm.constIntToPtr(
      llvm.constInt(g.primitives[tyInt], n.intVal.culonglong, llvm.False), nt)
  else:
    result = llvm.constInt(nt, n.intVal.culonglong, llvm.False)

proc genNodeFloatLit(g: LLGen, n: PNode): llvm.ValueRef =
  llvm.constReal(g.llType(n.typ), n.floatVal)

proc genNodeStrLit(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  if n.typ.skipTypes(abstractVarRange).kind == tyString:
    let ss = g.constNimString(n)
    let s = g.m.addPrivateConstant(ss.typeOfX(), g.nn(".str", n))
    s.setInitializer(ss)
    result = constBitCast(s, g.nimStringDescPtrType)
  else:
    let init = constString(n.strVal)
    let s = g.m.addPrivateConstant(init.typeOfX(), g.nn(".cstr", n))
    s.setInitializer(init)
    result = constBitCast(s, g.primitives[tyCString])

proc genNodeNilLit(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  # proc x() = nil
  if n.typ.isEmptyType:
    return nil

  let t = g.llType(n.typ.skipTypes(abstractVarRange))
  if load:
    result = llvm.constNull(t)
  else:
    result = g.m.addPrivateConstant(t, g.nn("nil", n))
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
  if sfGoto in v.flags: g.config.internalError(n.info, "Goto vars not supported")

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
      g.genNode(g.graph.lowerTupleUnpacking(n, g.module.sym))
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

    let tv = g.b.buildGEP(t, [g.gep0, g.constGEPIdx(i)], g.nn("vartuple." & $i, vn))

    g.genAsgnFromRef(tv, x, vn.typ)

proc genNodePar(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  if n.isDeepConstExprLL(true):
    let init = g.genConstInitializer(n)
    result = g.m.addPrivateConstant(init.typeOfX(), g.nn("par.init", n))
    result.setInitializer(init)
    return

  result = g.localAlloca(g.llType(n.typ), g.nn("par", n))
  g.buildStoreNull(result)

  for i in 0..<n.sonsLen:
    var s = n[i]

    if s.kind == nkExprColonExpr: s = s[1]
    let tgt = g.b.buildGEP(result, [g.gep0, g.constGEPIdx(i.int32)])
    g.genAssignment(s, tgt, s.typ)

  if load:
    result = g.b.buildLoad(result, g.nn("load.par", n))

proc genNodeObjConstr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)
    isRef = typ.kind == tyRef

  if isRef:
    result = g.cpNewObj(typ)
    typ = typ.lastSon.skipTypes(abstractInst)
  else:
    result = g.localAlloca(t, g.nn("objconstr", n))

    g.buildStoreNull(result)
    g.genObjectInit(typ, result)

  for i in 1 ..< n.len:
    let s = n[i]
    let ind = g.fieldIndex(typ, s[0].sym)
    proc cgi(v: int): llvm.ValueRef = g.constGEPIdx(v)
    let gep = g.b.buildInBoundsGEP(result, (@[0] & ind).map(cgi))
    g.genAssignment(s[1], gep, s[1].typ) # TODO should be dest typ

  if load and not isRef:
    result = g.b.buildLoad(result, g.nn("objconstr.load", n))
  elif not load and isRef:
    let tmp = g.localAlloca(t, g.nn("objconstr", n))
    discard g.b.buildStore(result, tmp)
    result = tmp

proc genNodeCurly(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    typ = n.typ
    size = g.config.getSize(skipTypes(n.typ, abstractVar))
    t = g.llType(typ)

  if size <= 8:
    let tmp = g.localAlloca(t, g.nn("curly", n))

    if n.isDeepConstExpr():
      discard g.b.buildStore(g.constNimSet(n), tmp)
    else:
      discard g.b.buildStore(llvm.constNull(t), tmp)

      for s in n:
        if s.kind == nkRange:
          withRangeItems(it, s):
            let mask = g.buildSetMask(t, g.setElemIndex(typ, it), size)
            let v = g.b.buildLoad(tmp, g.nn("curly.v", n))
            let nv = g.b.buildOr(v, mask, g.nn("curly.nv", n))
            discard g.b.buildStore(nv, tmp)

        else:
          let ax = g.genNode(s, true)

          let mask = g.buildSetMask(t, g.setElemIndex(typ, ax), size)
          let v = g.b.buildLoad(tmp, g.nn("curly.v"))
          let nv = g.b.buildOr(v, mask, g.nn("curly.nv", n))
          discard g.b.buildStore(nv, tmp)
    if load:
      result = g.b.buildLoad(tmp, g.nn("curly.load", n))
  else:
    result = g.localAlloca(t, g.nn("curly", n))
    if n.isDeepConstExpr():
      discard g.b.buildStore(g.constNimSet(n), result)
    else:
      g.callMemset(g.b.buildGEP(result, [g.gep0, g.gep0]), g.constInt8(0), g.constInt64(size))

      for s in n:
        if s.kind == nkRange:
          withRangeItems(it, s):
            let (gep, mask) = g.buildSetGEPMask(result, g.setElemIndex(typ, it))
            let v = g.b.buildLoad(gep, g.nn("curly.v", n))
            let nv = g.b.buildOr(v, mask, g.nn("curly.nv", n))
            discard g.b.buildStore(nv, gep)
        else:
          let ax = g.genNode(s, true)

          let (gep, mask) = g.buildSetGEPMask(result, g.setElemIndex(typ, ax))
          let v = g.b.buildLoad(gep, g.nn("curly.v", n))
          let nv = g.b.buildOr(v, mask, g.nn("curly.nv", n))
          discard g.b.buildStore(nv, gep)
    if load:
      result = g.buildLoadValue(result)

proc genNodeBracket(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let typ = n.typ.skipTypes(abstractVarRange)
  let t = g.llType(typ)
  if n.isDeepConstExprLL(true):
    let init = g.genConstInitializer(n)
    let c = g.m.addPrivateConstant(init.typeOfX(), g.nn("bracket.init", n))
    c.setInitializer(init)

    if load and c.typeOfX().isArrayPtr():
      result = g.b.buildGEP(c, [g.gep0, g.gep0], g.nn("bracket.const", n))
    else:
      result = c

    if n.isSeqLike() and c != g.llType(n.typ):
      result = llvm.constBitCast(c, g.llType(n.typ))

    return

  case typ.kind
  of tyArray, tyUncheckedArray:
    result = g.localAlloca(t, g.nn("bracket.arr", n))
    g.buildStoreNull(result)

    for i in 0..<n.sonsLen:
      let gep = g.b.buildGEP(result, [g.gep0, g.constGEPIdx(i)])
      g.genAssignment(n[i], gep, typ.elemType)

    if load:
      result = g.buildLoadValue(result)
  of tySequence:
    result = g.cpNewSeq(typ, g.constNimInt(n.sonsLen), g.nn("bracket.seq", n))
    for i in 0..<n.sonsLen:
      let gep = g.buildNimSeqDataGEP(result, g.constGEPIdx(i))
      g.genAssignment(n[i], gep, typ.elemType)
  else:
    g.config.internalError(n.info, "Unhandled nodebracket kind: " & $typ.kind)

proc genNodeBracketExprArray(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true)
    ty = n[0].typ.skipTypes(abstractVarRange + abstractPtrs + tyUserTypeClasses)
    first = g.config.firstOrd(ty)

  assert bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind

  # GEP indices are signed, so if a char appears here we want to make sure
  # it's zero-extended
  let bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
  let b =
    if first != 0:
      g.b.buildSub(bi, g.constNimInt(first.int), g.nn("bra.arr.first", n))
    else: bi

  if ax.typeOfX().isArrayPtr():
    result = g.b.buildGEP(ax, [g.gep0, b])
  else:
    result = g.b.buildGEP(ax, [b])

  if load:
    result = g.b.buildLoad(result, g.nn("bra.arr.load", n))

proc genNodeBracketExprUncheckedArray(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true)
    ty = n[0].typ.skipTypes(abstractVarRange + abstractPtrs + tyUserTypeClasses)
    first = g.config.firstOrd(ty)

  assert bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind

  # GEP indices are signed, so if a char appears here we want to make sure
  # it's zero-extended
  let bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
  let b =
    if first != 0:
      g.b.buildSub(bi, g.constNimInt(first.int), g.nn("bra.arr.first", n))
    else: bi

  if ax.typeOfX().isArrayPtr():
    result = g.b.buildGEP(ax, [g.gep0, b])
  else:
    result = g.b.buildGEP(ax, [b])

  if load:
    result = g.b.buildLoad(result, g.nn("bra.arr.load", n))

proc genNodeBracketExprOpenArray(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    s = if n[0].kind == nkHiddenDeref: n[0][0] else: n[0]
    ax = g.b.buildLoad(g.symbols[s.sym.id], "")
    bx = g.genNode(n[1], true)
  result = g.b.buildGEP(ax, [bx])

  if load:
    result = g.b.buildLoad(result, g.nn("bra.oa.load", n))

proc genNodeBracketExprSeq(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true)

  result = g.buildNimSeqDataGEP(ax, bx)

  if load:
    result = g.b.buildLoad(result, g.nn("bra.seq.load", n))

proc genNodeBracketExprCString(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true)

  result = g.b.buildGEP(ax, [bx], g.nn("bra.cstr.gep", n))

  if load:
    result = g.b.buildLoad(result, g.nn("bra.cstr.load", n))

proc genNodeBracketExprTuple(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true)

  if bx.typeOfX().getIntTypeWidth() > 32.cuint:
    bx = g.b.buildTrunc(bx, llvm.int32TypeInContext(g.lc), g.nn("bra.tup.trunc", n))
  result = g.b.buildGEP(ax, [g.gep0, bx], g.nn("bra.tup.gep", n))

  if load:
    result = g.b.buildLoad(result, g.nn("bra.tup.load", n))

proc genNodeBracketExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var typ = skipTypes(n[0].typ, abstractVarRange + tyUserTypeClasses)
  if typ.kind in {tyRef, tyPtr}: typ = typ.lastSon.skipTypes(abstractVarRange)
  case typ.kind
  of tyArray: result = g.genNodeBracketExprArray(n, load)
  of tyUncheckedArray: result = g.genNodeBracketExprUncheckedArray(n, load)
  of tyOpenArray, tyVarargs: result = g.genNodeBracketExprOpenArray(n, load)
  of tySequence, tyString: result = g.genNodeBracketExprSeq(n, load)
  of tyCString: result = g.genNodeBracketExprCString(n, load)
  of tyTuple: result = g.genNodeBracketExprTuple(n, load)
  else: g.config.internalError(n.info, "Unhandled nodebracketexpr kind: " & $typ.kind)

proc genNodeDot(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  p("genDotExpr", n[1].sym, g.depth + 1)
  let v = g.genNode(n[0], false)

  let typ = skipTypes(n[0].typ, abstractInst + tyUserTypeClasses)

  let i = g.fieldIndex(typ, n[1].sym)
  proc cgi(v: int): llvm.ValueRef = g.constGEPIdx(v)
  result = g.b.buildInBoundsGEP(v, (@[0] & i).map(cgi), g.nn("dot.gep", n))

  if load:
    result = g.buildLoadValue(result)

proc genNodeCheckedField(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  result = g.genNode(n[0], load)

proc genNodeDeref(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let v = g.genNode(n[0], true)

  result = if load: g.buildLoadValue(v) else: v

proc genNodeIfExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  # Sometimes an nkIfStmt appears in the ast even though it looks more like
  # an expression (see tcasestmt with an if in a case else).. it won't have
  # a type of its own so we'll have to cheat..
  let typ = if n.typ == nil: n[0][1].typ else: n.typ
  result = g.localAlloca(g.llType(typ), g.nn("ifx.res", n))
  g.buildStoreNull(result)

  let iend = f.appendBasicBlock(g.nn("ifx.end", n))

  for i in 0..<n.sonsLen:
    let s = n[i]

    if s.sonsLen == 1:
      # else branch
      if not typ.isEmptyType():
        g.f.scopePush(n, nil)
        g.genAssignment(s[0], result, typ)

        g.f.scopePop()

      discard g.b.buildBr(iend)
    else:
      let cond = g.buildI1(g.genNode(s[0], true))

      let itrue = f.appendBasicBlock(g.nn("ifx.true", n))
      let ifalse = f.appendBasicBlock(g.nn("ifx.false", n))

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      if not typ.isEmptyType():
        g.f.scopePush(n, nil)
        g.genAssignment(s[1], result, typ)
        g.f.scopePop()

      discard g.b.buildBr(iend)

      g.b.positionAndMoveToEnd(ifalse)

  if n[n.sonsLen - 1].sonsLen != 1:
    discard g.b.buildBr(iend)

  g.b.positionAndMoveToEnd(iend)

  if load:
    result = g.b.buildLoad(result, g.nn("ifx.load", n))

proc genNodeLambda(g: LLGen, n: PNode): llvm.ValueRef =
  let sym = n[namePos].sym
  result = g.genFunctionWithBody(sym)

proc genNodeConv(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    v = g.genNode(n[1], load)
    vt = v.typeOfX()
    nt = if load: g.llType(n.typ) else: llvm.pointerType(g.llType(n.typ))
    vtk = vt.getTypeKind()
    ntk = nt.getTypeKind()
    vtyp = skipTypes(n[1].typ, abstractInst)
    ntyp = skipTypes(n.typ, abstractInst)

  if vt == nt:
    result = v
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    result = g.buildTruncOrExt(v, nt, n[1].typ)
  elif vtk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind} and ntk == llvm.IntegerTypeKind:
    if ntyp.kind in {tyUInt..tyUInt64}:
      result = g.b.buildFPToUI(v, nt, g.nn("conv.fptoui", n))
    else:
      result = g.b.buildFPToSI(v, nt, g.nn("conv.fptosi", n))
  elif vtk == llvm.IntegerTypeKind and ntk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind}:
    if vtyp.kind in {tyUInt..tyUInt64}:
      result = g.b.buildUIToFP(v, nt, g.nn("conv.uitofp", n))
    else:
      result = g.b.buildSIToFP(v, nt, g.nn("conv.sitofp", n))
  elif n[1].typ.kind == tyPtr and n.typ.kind == tyPointer:
    result = g.b.buildBitCast(v, nt, g.nn("conv.ptr", n))
  elif vtk == llvm.FloatTypeKind and ntk == llvm.DoubleTypeKind:
    result = g.b.buildFPExt(v, nt, g.nn("conv.fd", n))
  elif vtk == llvm.PointerTypeKind and ntk == llvm.PointerTypeKind:
    result = g.b.buildBitCast(v, nt, g.nn("conv.pointer", n))
  elif n.typ.kind == tyProc and ntk == llvm.PointerTypeKind or nt == g.closureType:
    result = g.b.buildBitCast(v, g.voidPtrType, g.nn("conv.proc", n))
  elif vtk == llvm.DoubleTypeKind and ntk == llvm.FloatTypeKind:
    result = g.b.buildFPTrunc(v, nt, g.nn("conv.df", n))
  elif vtyp.kind == tyArray and ntyp.kind == tyArray:
    result = v

  if result == nil:
    g.config.internalError(n.info, "Unhandled conversion: " & $vt & " " & $nt & " " &
      $n[1].typ.kind & " " & $n.typ.kind)

proc genNodeCast(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ntyp = n[1].typ.skipTypes(abstractRange)

  let v =
    if ntyp.kind in {tyOpenArray, tyVarargs}: g.genNode(n[1], false)
    else: g.genNode(n[1], load)

  let nt = g.llType(n.typ)

  let vtk = v.typeOfX().getTypeKind()
  let ntk = nt.getTypeKind()
  if vtk == llvm.PointerTypeKind and ntk == llvm.IntegerTypeKind:
    result = g.b.buildPtrToInt(v, nt, g.nn("cast.pi", n))
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.PointerTypeKind:
    result = g.b.buildIntToPtr(v, nt, g.nn("cast.ip", n))
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    result = g.buildTruncOrExt(v, nt, ntyp)
  else:
    result = g.b.buildBitCast(v, nt, g.nn("cast.bit", n))

proc genNodeAddr(g: LLGen, n: PNode): llvm.ValueRef =
  g.genNode(n[0], false)

proc genNodeObjDownConv(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ax = g.genNode(n[0], load)

  let at = ax.typeOfX()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  if at == nt:
    result = ax
  elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
    result = g.b.buildBitCast(ax, nt, g.nn("obj.down", n))
  else:
    g.config.internalError(n.info, "Unhandled nkObjDownConv")

proc genNodeObjUpConv(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ax = g.genNode(n[0], n.typ.kind == tyRef)

  let at = ax.typeOfX()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  if at == nt:
    result = ax
  elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
    result = g.b.buildBitCast(ax, nt, g.nn("obj.up", n))
  else:
    g.config.internalError(n.info, "Unhandled nkUpDownConv")

  if load and n.typ.kind notin {tyRef, tyVar}:
    result = g.b.buildLoad(result, g.nn("load.obj.up", n))

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
      b = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
      c = g.buildNimIntExt(cx, g.isUnsigned(n[2].typ))
    discard g.callCompilerProc(fn, [v, b, c])

  result = g.buildTruncOrExt(v, nt, n.typ)

proc genNodeStringToCString(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genNode(n[0], true)

  result = g.buildNimSeqDataGEP(ax)

proc genNodeAsgn(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], false)
  g.genAssignment(n[1], ax, n[0].typ, true)

proc genNodeFastAsgn(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], false)
  g.genAssignment(n[1], ax, n[0].typ, false)

proc genNodeProcDef(g: LLGen, n: PNode) =
  if n.sons[genericParamsPos].kind != nkEmpty: return

  let s = n.sons[namePos].sym

  if s.typ == nil: return
  if sfBorrow in s.flags: return
  if s.skipGenericOwner.kind != skModule or sfCompileTime in s.flags: return
  if s.getBody.kind == nkEmpty and lfDynamicLib notin s.loc.flags: return

  if ({sfExportc, sfCompilerProc} * s.flags == {sfExportc}) or
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
          iend = f.appendBasicBlock(g.nn("if.end", n))
        discard g.b.buildBr(iend)
    else:
      let cond = g.buildI1(g.genNode(s[0], true))

      let itrue = f.appendBasicBlock(g.nn("if.true", n))
      let ifalse = f.appendBasicBlock(g.nn("if.false", n))

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      g.f.scopePush(n, nil)
      g.genNode(s[1])
      g.f.scopePop()

      if g.b.needsBr():
        if iend == nil:
          iend = f.appendBasicBlock(g.nn("if.end", n))
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

  let wcmp = f.appendBasicBlock(g.nn("while.cmp", n))
  let wtrue = f.appendBasicBlock(g.nn("while.true", n))
  let wfalse = f.appendBasicBlock(g.nn("while.false", n))

  # jump to comparison
  discard g.b.buildBr(wcmp)

  # generate condition expression in cmp block
  g.b.positionBuilderAtEnd(wcmp)
  let cond = g.buildI1(g.genNode(n[0], true))

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
  let u = g.isUnsigned(n[0].typ)

  let caseend = f.appendBasicBlock(g.nn("case.end", n))

  if not n.typ.isEmptyType():
    result = g.localAlloca(g.llType(n.typ), g.nn("case.res", n))
    g.buildStoreNull(result)

  for i in 1..<n.sonsLen:
    let s = n[i]
    p("genNodeCaseStmt", s, g.depth)

    let isLast = i == n.sonsLen - 1

    let cur = g.b.getInsertBlock()

    let lbl = if s.kind == nkElse: "case.else" else: "case.of." & $i
    let casedo = f.appendBasicBlock(g.nn(lbl & ".do", n))

    let next =
      if isLast: caseend
      elif n[i+1].kind == nkElse: f.appendBasicBlock(g.nn("case.else", n))
      else: f.appendBasicBlock(g.nn("case.of." & $(i+1), n))

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
          let
            bx = g.genNode(cond.sons[0], true)
            cx = g.genNode(cond.sons[1], true)
            ub = if u: llvm.IntUGE else: llvm.IntSGE
            cmpb = g.b.buildICmp(ub, ax, bx, g.nn("case.cmpb", n))
            uc = if u: llvm.IntULE else: llvm.IntSLE
            cmpc = g.b.buildICmp(uc, ax, cx, g.nn("case.cmpc", n))
            cmp = g.buildI1(g.b.buildAnd(cmpb, cmpc, g.nn("case.cmp", n)))

          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(g.nn(lbl & ".or." & $j, n))
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)
        else:
          let bx = g.genNode(cond, true)

          var cmp: llvm.ValueRef
          if isString:
            let tmp = g.callCompilerProc("cmpStrings", [ax, bx])
            cmp = g.buildI1(
              g.b.buildICmp(llvm.IntEQ, tmp, g.ni0, g.nn("case.cmp", n)))
          else:
            let b = g.buildTruncOrExt(bx, ax.typeOfX(), cond.typ)
            cmp = g.buildI1(
              g.b.buildICmp(llvm.IntEQ, ax, b, g.nn("case.cmp", n)))
          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(g.nn(lbl & ".or." & $j, n))
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)

        # Moving block is not necessary but makes generated code easier
        # to follow, placing action just after conditions
        if not isLast2:
          g.b.getInsertBlock().moveBasicBlockBefore(casedo)

    of nkElse:
      discard g.b.buildBr(casedo)
    else:
      g.config.internalError(s.info, "Unexpected case kind " & $s.kind)
    g.f.scopePop()

  g.b.positionAndMoveToEnd(caseend)

  if f.getLastBasicBlock() != caseend:
    caseend.moveBasicBlockAfter(f.getLastBasicBlock())

  if load and result != nil:
    result = g.buildLoadValue(result)

proc genNodeConstDef(g: LLGen, n: PNode) =
  # we emit these on demand!
  discard

proc genNodeTryStmt(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  # create safe point
  let tsp = g.graph.getCompilerProc("TSafePoint").typ
  let spt = g.llStructType(tsp)

  if not n.typ.isEmptyType():
    result = g.localAlloca(g.llType(n.typ), g.nn("case.res", n))
    g.buildStoreNull(result)

  let sp = g.localAlloca(spt, g.nn("sp", n))
  g.buildStoreNull(sp)
  discard g.callCompilerProc("pushSafePoint", [sp])

  g.f.scopePush(nil, nil)

  # call setjmp
  let
    jmpBufPtrType = g.jmpBufType.pointerType()
    setjmpType = llvm.functionType(g.cintType, [jmpBufPtrType])
    setjmp = g.m.getOrInsertFunction("_setjmp", setjmpType)

  let contextP = g.b.buildGEP(sp, [g.gep0, g.constGEPIdx(2)])
  let res = g.b.buildCall(setjmp, [g.b.buildBitCast(contextP, jmpBufPtrType, "sj.ptr")])

  let statusP = g.b.buildGEP(sp, [g.gep0, g.constGEPIdx(1)])
  discard g.b.buildStore(g.buildNimIntExt(res, false), statusP)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let sjok = f.appendBasicBlock(g.nn("sj.ok", n))
  let sjexc = f.appendBasicBlock(g.nn("sj.exc", n))
  let sjend = f.appendBasicBlock(g.nn("sj.end", n))

  # see if we're returning normally or from a longjmp
  let cmp = g.b.buildICmp(llvm.IntEQ, res, g.constCInt(0), "sj.cmp")
  discard g.b.buildCondBr(cmp, sjok, sjexc)

  g.b.positionBuilderAtEnd(sjok)

  g.f.scopePush(nil, nil)
  g.f.nestedTryStmts.add((n, false))

  if not n.typ.isEmptyType():
    g.genAssignment(n[0], result, n.typ)
  else:
    g.genNode(n[0])

  discard g.callCompilerProc("popSafePoint", [])
  g.f.scopePop()

  discard g.b.buildBr(sjend)

  g.b.positionBuilderAtEnd(sjexc)
  g.f.scopePush(nil, nil)

  discard g.callCompilerProc("popSafePoint", [])

  g.f.nestedTryStmts[^1].inExcept = true
  var i = 1
  let length = n.sonsLen
  while (i < length) and (n[i].kind == nkExceptBranch):
    let b = n[i]
    inc(i)

    let blen = b.sonsLen
    if blen == 1:
      # catch-all
      discard g.b.buildStore(g.ni0, statusP)
      if not n.typ.isEmptyType():
        g.genAssignment(b[0], result, n.typ)
      else:
        g.genNode(b[0])

      discard g.callCompilerProc("popCurrentException", [])
      discard g.b.buildBr(sjend)
    else:
      let sjfound = f.appendBasicBlock(g.nn("sj.found", n))
      let sjnext =
        if (i < length) and (n[i].kind == nkExceptBranch): f.appendBasicBlock(g.nn("sj.next", n))
        else: sjend
      # catch one or more types
      for j in 0..blen - 2:
        assert(b[j].kind == nkType)

        let exc = g.callCompilerProc("getCurrentException", [])
        let m_type = g.b.buildLoad(
          g.b.buildGEP(exc, [g.gep0, g.gep0, g.gep0]), "")
        let ti = g.genTypeInfo(b[j].typ)
        let found = g.buildI1(g.callCompilerProc("isObj", [m_type, ti]))

        if j == blen - 2:
          discard g.b.buildCondBr(found, sjfound, sjnext)
          g.b.positionBuilderAtEnd(sjnext)
        else:
          let sjor = f.appendBasicBlock(g.nn("sj.or", n))
          sjor.moveBasicBlockBefore(sjfound)
          discard g.b.buildCondBr(found, sjfound, sjor)
          g.b.positionBuilderAtEnd(sjor)

      g.b.positionBuilderAtEnd(sjfound)
      discard g.b.buildStore(g.ni0, statusP)
      if not n.typ.isEmptyType():
        g.genAssignment(b[blen-1], result, n.typ)
      else:
        g.genNode(b[blen-1])
      discard g.callCompilerProc("popCurrentException", [])

      discard g.b.buildBr(sjend)

      g.b.positionBuilderAtEnd(sjnext)

  discard pop(g.f.nestedTryStmts)
  g.f.scopePop()

  if i == 1:
    # finally without catch!
    discard g.b.buildBr(sjend)
  g.b.positionBuilderAtEnd(sjend)

  if i < length and n[i].kind == nkFinally:
    g.f.finallySafePoints.add(sp)
    g.f.scopePush(nil, nil)
    if not n.typ.isEmptyType():
      g.genAssignment(n[i][0], result, n.typ)
    else:
      g.genNode(n[i][0])
    g.f.scopePop()
    discard g.f.finallySafePoints.pop()

  # TODO is the load needed?
  # TODO is this needed if we have a catch-all?
  let s = g.b.buildLoad(statusP, "")
  let sjrr = f.appendBasicBlock(g.nn("sj.rr", n))
  let sjnm = f.appendBasicBlock(g.nn("sj.nomore", n))
  # In case it wasn't handled...
  let scmp = g.b.buildICmp(llvm.IntNE, s, g.ni0, "")
  discard g.b.buildCondBr(scmp, sjrr, sjnm)
  g.b.positionBuilderAtEnd(sjrr)
  discard g.callCompilerProc("reraiseException", [])
  # TODO get rid of br somehow? reraise shoudn't return
  discard g.b.buildBr(sjnm)

  g.b.positionBuilderAtEnd(sjnm)
  sjend.moveBasicBlockBefore(sjrr)
  g.f.scopePop()

  if load and result != nil:
    result = g.buildLoadValue(result)

proc genNodeRaiseStmt(g: LLGen, n: PNode) =
  if g.f.nestedTryStmts.len > 0 and g.f.nestedTryStmts[^1].inExcept:
    let finallyBlock = g.f.nestedTryStmts[^1].n.lastSon
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
  var stack = newSeq[tuple[n: PNode, inExcept: bool]](0)

  for i in 1..howManyTrys:
    let tryStmt = g.f.nestedTryStmts.pop
    if not tryStmt.inExcept:
      discard g.callCompilerProc("popSafePoint", [])

    stack.add(tryStmt)

    let finallyStmt = lastSon(tryStmt.n)
    if finallyStmt.kind == nkFinally:
      g.genNode(finallyStmt[0])

  for i in countdown(howManyTrys-1, 0):
    g.f.nestedTryStmts.add(stack[i])

  for i in countdown(howManyExcepts-1, 0):
    discard g.callCompilerProc("popCurrentException", [])

proc genNodeReturnStmt(g: LLGen, n: PNode) =
  if (n[0].kind != nkEmpty):
    g.genNode(n[0])

  g.blockLeave(g.f.nestedTryStmts.len, g.f.inExceptBlockLen)

  if (g.f.finallySafePoints.len > 0):
    let sp = g.f.finallySafePoints[g.f.finallySafePoints.len-1]
    let statusP = g.b.buildGEP(sp, [g.gep0, g.constGEPIdx(1)])

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let s = g.b.buildLoad(statusP, "")
    let retpop = f.appendBasicBlock(g.nn("ret.pop", n))
    let retdone = f.appendBasicBlock(g.nn("ret.done", n))
    let scmp = g.b.buildICmp(llvm.IntNE, s, g.ni0, "")
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
  let cont = f.appendBasicBlock(g.nn("return.dead", n))
  g.b.positionBuilderAtEnd(cont)

proc genNodeBreakStmt(g: LLGen, n: PNode) =
  p("b", n[0], g.depth)

  if n[0].kind == nkEmpty:
    g.config.internalError(n.info, "Unexpected nkBreakStmt with nkEmpty")

  let idx = g.f.scopeIdx(n[0].sym)
  if idx == -1:
    g.config.internalError(n.info, "Scope not found: " & $n[0].sym)

  let s = g.f.scope[idx]

  g.blockLeave(
    g.f.nestedTryStmts.len - s.nestedTryStmts,
    g.f.inExceptBlockLen - s.nestedExceptStmts)

  # similar to return, there might be more stuff after a break that messes
  # things up - we add an extra block just in case
  # TODO: one case when this happens is with finally blocks, which are
  # currently broken anyway, but this way at least the generated bytecode is
  # valid
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let cont = f.appendBasicBlock(g.nn("block.cont", n))
  if s.exit == nil:
    s.exit = f.appendBasicBlock(g.nn("block.exit", n))
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
  if bx.typeOfX().getElementType().getTypeKind() == llvm.PointerTypeKind:
    bx = g.b.buildLoad(bx, g.nn("clox.hack", n))

  let
    a = g.b.buildBitCast(ax, g.voidPtrType, g.nn("clox.ptr", n))
    b = g.b.buildBitCast(bx, g.voidPtrType, g.nn("clox.env", n))
  result = g.localAlloca(g.llType(n.typ), g.nn("clox.res", n))

  discard g.b.buildStore(a, g.b.buildGEP(result, [g.gep0, g.gep0]))
  discard g.b.buildStore(b, g.b.buildGEP(result, [g.gep0, g.gep1]))

  if load:
    result = g.b.buildLoad(result, g.nn("load.clox.res", n))

proc genNodeGotoState(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], true)

  let l = g.config.lastOrd(n[0].typ)

  g.f.scope[g.f.scope.len - 1].goto = g.b.buildSwitch(ax, g.f.ret, (l + 1).cuint)

proc genNodeState(g: LLGen, n: PNode) =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let state = f.appendBasicBlock(g.nn("state." & $n[0].intVal, n))
  g.b.buildBrFallthrough(state)
  g.b.positionBuilderAtEnd(state)
  for i in 0..g.f.scope.len-1:
    if g.f.scope[g.f.scope.len - i - 1].goto != nil:
      g.f.scope[g.f.scope.len - i - 1].goto.addCase(g.constNimInt(n[0].intVal.int), state)
      break

proc genNodeBreakState(g: LLGen, n: PNode) =
  # TODO C code casts to int* and reads second value.. uh, I guess we should be
  # able to do better
  var ax: llvm.ValueRef
  if n[0].kind == nkClosure:
    ax = g.genNode(n[0][1], false)
  else:
    ax = g.genNode(n[0], false)
    ax = g.b.buildGEP(ax, [g.gep0, g.gep1])

  ax = g.b.buildLoad(ax, g.nn("load.state.break", n))
  let s = g.b.buildLoad(g.b.buildGEP(ax, [g.gep0, g.gep1]), g.nn("state.break.s", n))
  let cmp = g.b.buildICmp(llvm.IntSLT, s, g.ni0, g.nn("state.break.cmp", n))
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let state = f.appendBasicBlock(g.nn("state.break.cont", n))

  # nkBreakState happens in a loop, so we need to find the end of that loop...
  var whileExit: llvm.BasicBlockRef
  for i in 0..g.f.scope.len-1:
    if g.f.scope[g.f.scope.len - i - 1].exit != nil:
      whileExit = g.f.scope[g.f.scope.len - i - 1].exit
      break

  if whileExit == nil:
    g.config.internalError(n.info, "no enclosing while loop in nkBreakState")

  discard g.b.buildCondBr(cmp, whileExit, state)
  g.b.positionBuilderAtEnd(state)

proc genNodeTupleConstr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  genNodePar(g, n, load)

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
  of nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef: g.genNodeProcDef(n)
  of nkPragma:
    for s in n:
      let p = whichPragma(s)
      case p
      of wAsm, wEmit:
        g.config.internalError(n.info, substr($p, 1) & " pragma not supported")
      else: discard
  of nkPragmaBlock: result = g.genNode(n.lastSon, load)
  of nkIfStmt: result = g.genNodeIfStmt(n) # if in case! see tcaststm.nim
  of nkWhenStmt: result = g.genNodeWhenStmt(n, load)
  of nkWhileStmt: g.genNodeWhileStmt(n)
  of nkCaseStmt: result = g.genNodeCaseStmt(n, load)  # Sometimes seen as expression!
  of nkVarSection, nkLetSection, nkConstSection: g.genSons(n)
  of nkConstDef: g.genNodeConstDef(n)
  of nkTryStmt: result = g.genNodeTryStmt(n, load)
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
  of nkTupleConstr: result = g.genNodeTupleConstr(n, load)

  of nkTypeSection, nkCommentStmt, nkIteratorDef, nkIncludeStmt,
     nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt,
     nkFromStmt, nkTemplateDef, nkMacroDef: discard
  else:
    g.config.internalError(n.info, "Unhandled node: " & $n)

  g.depth -= 1

proc newLLFunc(g: LLGen, ret: llvm.BasicBlockRef): LLFunc =
  new(result)
  result.scope = @[]
  result.ret = ret
  result.nestedTryStmts = @[]
  result.finallySafePoints = @[]

proc newLLGen(graph: ModuleGraph): LLGen =
  let
    lc = llvm.getGlobalContext()
    name = graph.config.m.fileInfos[graph.config.projectMainIdx.int].shortName
    intType = llvm.intTypeInContext(lc, graph.config.target.intSize.cuint * 8)
    charType = llvm.int8TypeInContext(lc)
    nimStringDescType = llvm.structCreateNamed(lc, "NimStringDesc")

  result = LLGen(
    graph: graph,
    lc: lc,
    m: llvm.moduleCreateWithNameInContext(name, lc),
    b: llvm.createBuilderInContext(lc),

    cintType: llvm.int32TypeInContext(lc),  # c int on linux
    csizetType: llvm.int64TypeInContext(lc),  # c size_t on linux
    seqBaseType: llvm.structCreateNamed(lc, "TGenericSeq"),
    closureType: llvm.structCreateNamed(lc, "llnim.Closure"),
    procPtrType: llvm.int8TypeInContext(lc).pointerType(),  # no void* in llvm
    voidPtrType: llvm.int8TypeInContext(lc).pointerType(),  # no void* in llvm
    nimStringDescType: nimStringDescType,
    nimStringDescPtrType: nimStringDescType.pointerType(),
    jmpbufType: llvm.structCreateNamed(lc, "jmp_buf"),

    attrNoInline: lc.createEnumAttribute(llvm.attrNoInline, 0),
    attrNoReturn: lc.createEnumAttribute(llvm.attrNoReturn, 0),

    symbols: initTable[int, llvm.ValueRef](),
    gmarkers: initTable[int, llvm.ValueRef](),
    markers: initTable[SigHash, llvm.ValueRef](),
    nodeInfos: initTable[SigHash, llvm.ValueRef](),
    typeInfos: initTable[SigHash, llvm.ValueRef](),
    types: initTable[SigHash, llvm.TypeRef](),
    sigConflicts: initCountTable[SigHash](),
  )

  var g = result

  block:
    proc s(t: TTypeKind, v: llvm.TypeRef) =
      g.primitives[t] = v

    s(tyBool, llvm.int8TypeInContext(lc)) # needs 8 bits for atomics to work...
    s(tyChar, charType)
    s(tyNil, g.voidPtrType)
    # tyStmt appears for example in `echo()` as the element type of the array
    s(tyStmt, g.voidPtrType)
    s(tyPointer, g.voidPtrType)
    s(tyCString, charType.pointerType())
    s(tyInt, intType)
    s(tyInt8, llvm.int8TypeInContext(lc))
    s(tyInt16, llvm.int16TypeInContext(lc))
    s(tyInt32, llvm.int32TypeInContext(lc))
    s(tyInt64, llvm.int64TypeInContext(lc))
    s(tyFloat, llvm.doubleTypeInContext(lc))
    s(tyFloat32, llvm.floatTypeInContext(lc))
    s(tyFloat64, llvm.doubleTypeInContext(lc))
    s(tyFloat128, llvm.fP128TypeInContext(lc))
    s(tyUInt, g.primitives[tyInt])
    s(tyUInt8, llvm.int8TypeInContext(lc))
    s(tyUInt16, llvm.int16TypeInContext(lc))
    s(tyUInt32, llvm.int32TypeInContext(lc))
    s(tyUInt64, llvm.int64TypeInContext(lc))

  llvm.structSetBody(
    g.jmpBufType, [llvm.arrayType(llvm.int64TypeInContext(g.lc), 8), llvm.int32TypeInContext(g.lc), llvm.arrayType(llvm.int64TypeInContext(g.lc), 16)])
  llvm.structSetBody(g.seqBaseType, [g.primitives[tyInt], g.primitives[tyInt]])
  llvm.structSetBody(g.nimStringDescType, [g.seqBaseType, llvm.arrayType(llvm.int8TypeInContext(g.lc), 0)])
  llvm.structSetBody(g.closureType, [g.voidPtrType, g.voidPtrType])

  g.gep0 = g.constGEPIdx(0)
  g.gep1 = g.constGEPIdx(1)
  g.ni0 = g.constNimInt(0)
  g.nb = [
    llvm.constInt(g.primitives[tyBool], 0, llvm.False),
    llvm.constInt(g.primitives[tyBool], 1, llvm.False)
  ]

  if optCDebug in graph.config.globalOptions:
    let d = llvm.createDIBuilder(g.m)
    g.d = d
    g.dfiles = initTable[int, llvm.MetadataRef]()
    g.dscopes = initTable[int, llvm.MetadataRef]()
    g.dstructs = initTable[SigHash, llvm.MetadataRef]()
    g.dbgKind = llvm.getMDKindIDInContext(g.lc, "dbg")
    let df = g.debugGetFile(graph.config.projectMainIdx)
    let isOptimized = False # TODO fetch from flags?
    let flags = "" # TODO Compiler flags
    let runtimeVer = 0.cuint # TODO not used for nim?
    g.dcu = d.dIBuilderCreateCompileUnit(
      DWARFSourceLanguageC99, df, "nlvm", 4, isOptimized, flags, len(flags),
      runtimeVer, "", 0, DWARFEmissionFull, 0, False, False)

    proc add(ty: TTypeKind, n: string, sz: uint64, enc: cuint) =
      g.dtypes[ty] = d.dIBuilderCreateBasicType(n, sz, enc)

    add(tyBool, "bool", 8, DW_ATE_boolean)
    add(tyChar, "char", 8, DW_ATE_unsigned_char)
    g.dtypes[tyPointer] = d.dIBuilderCreatePointerType(
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

    g.dtypes[tyCString] = g.d.dIBuilderCreatePointerType(
      g.dtypes[tyChar], 64, 64, "")

  if g.config.existsConfigVar("nlvm.target"):
    g.tgt = g.config.getConfigVar("nlvm.target")
  else:
    let p = llvm.getDefaultTargetTriple()
    g.tgt = $p
    disposeMessage(p)

  # TODO somehow, clang knows this! figure out how and why...
  result.tgtExportLinkage = llvm.CommonLinkage

  if result.tgt.startsWith("wasm"):
    result.tgtExportLinkage = llvm.ExternalLinkage

proc genMain(g: LLGen) =
  let llMainType = llvm.functionType(
    g.cintType, [g.cintType, g.primitives[tyCString].pointerType()])

  let f = g.m.addFunction("main", llMainType)

  let b = f.appendBasicBlock(g.nn("entry"))
  g.b.positionBuilderAtEnd(b)

  if g.d != nil:
    let types = [
      g.dtypes[tyInt32],
      g.dtypes[tyInt32],
      g.d.dIBuilderCreatePointerType(
        g.d.dIBuilderCreatePointerType(g.dtypes[tyChar], 64, 64, ""),
        64, 64, "")
    ]
    let scope = g.debugFunction(nil, types, f)

    let dl = llvm.getGlobalContext().dIBuilderCreateDebugLocation(
        0, 0, scope, nil)
    g.b.setCurrentDebugLocation(llvm.getGlobalContext().metadataAsValue(dl))

    let f0 = f.getFirstParam()
    let f1 = f0.getNextParam()
    let argc = g.b.buildAlloca(f0.typeOfX(), g.nn("argc"))
    let argv = g.b.buildAlloca(f1.typeOfX(), g.nn("argv"))
    discard g.b.buildStore(f0, argc)
    discard g.b.buildStore(f1, argv)

    let vd0 = g.d.dIBuilderCreateParameterVariable(
      scope, "argc", 1, g.debugGetFile(g.config.projectMainIdx), 0, g.dtypes[tyInt],
      false, 0)
    discard g.d.dIBuilderInsertDeclareAtEnd(argc, vd0,
      g.d.dIBuilderCreateExpression(nil, 0),
      dl, g.b.getInsertBlock())

    let vd1 = g.d.dIBuilderCreateParameterVariable(
      scope, "argv", 2, g.debugGetFile(g.config.projectMainIdx), 0,
      g.d.dIBuilderCreatePointerType(g.dtypes[tyCString], 64, 64, ""),
      false, 0)
    discard g.d.dIBuilderInsertDeclareAtEnd(argv, vd1,
      g.d.dIBuilderCreateExpression(nil, 0),
      dl, g.b.getInsertBlock())

  if g.config.target.targetOS != osStandAlone and g.config.selectedGC != gcNone:
    let bottom = g.b.buildAlloca(g.primitives[tyInt], g.nn("bottom"))
    discard g.callCompilerProc("initStackBottomWith", [bottom])

  let cmdLine = g.m.getNamedGlobal("cmdLine")
  if cmdLine != nil:
    cmdLine.setLinkage(g.tgtExportLinkage)
    cmdLine.setInitializer(llvm.constNull(cmdLine.typeOfX().getElementType()))
    discard g.b.buildStore(g.b.buildBitCast(f.getParam(1), cmdLine.typeOfX().getElementType(), ""), cmdLine)

  let cmdCount = g.m.getNamedGlobal("cmdCount")
  if cmdCount != nil:
    cmdCount.setLinkage(g.tgtExportLinkage)
    cmdCount.setInitializer(llvm.constNull(cmdCount.typeOfX().getElementType()))
    discard g.b.buildStore(f.getParam(0), cmdCount)

  discard g.b.buildCall(g.init.ret.getBasicBlockParent(), [], "")

  discard g.b.buildRet(constInt(g.cintType, 0, False))

proc loadBase(g: LLGen) =
  let m = parseIRInContext(
    g.lc, g.config.prefixDir.string / "../nlvm-lib/nlvmbase-linux-amd64.ll")

  if g.m.linkModules2(m) != 0:
    g.config.internalError("module link failed")

proc runOptimizers(g: LLGen) =
  if {optOptimizeSpeed, optOptimizeSize} * g.config.options == {}:
    return

  let pmb = llvm.passManagerBuilderCreate()

  if optOptimizeSize in g.config.options:
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
  if g.config.outFile.string.len > 0:
    if g.config.outFile.string.isAbsolute:
      outFile = g.config.outFile.string
    else:
      outFile = getCurrentDir() / g.config.outFile.string
  else:
    if optCompileOnly in g.config.globalOptions:
      outFile = project & ".ll"
    elif optNoLinking in g.config.globalOptions:
      outFile = project & ".o"
    else:
      outFile = project

  initializeX86AsmPrinter()
  initializeX86Target()
  initializeX86TargetInfo()
  initializeX86TargetMC()

  initializeWebAssemblyAsmPrinter()
  initializeWebAssemblyTarget()
  initializeWebAssemblyTargetInfo()
  initializeWebAssemblyTargetMC()

  var tr: llvm.TargetRef
  discard getTargetFromTriple(g.tgt, addr(tr), nil)

  var reloc = llvm.RelocDefault
  if optGenDynLib in g.config.globalOptions and
      ospNeedsPIC in platform.OS[g.config.target.targetOS].props:
    reloc = llvm.RelocPIC

  let cgl =
    if optOptimizeSpeed in g.config.options: llvm.CodeGenLevelAggressive
    else: llvm.CodeGenLevelDefault

  let tm = createTargetMachine(tr, g.tgt, "", "", cgl,
    reloc, llvm.CodeModelDefault)

  let layout = tm.createTargetDataLayout()
  g.m.setModuleDataLayout(layout)
  g.m.setTarget(g.tgt)

  g.runOptimizers()

  if optCompileOnly in g.config.globalOptions:
    discard g.m.printModuleToFile(outfile, nil)
    return

  let ofile =
    if optNoLinking in g.config.globalOptions:
      outFile
    else:
      g.config.completeCFilePath(AbsoluteFile(project & ".o")).string

  var err: cstring
  if llvm.targetMachineEmitToFile(tm, g.m, ofile, llvm.ObjectFile,
    cast[cstringArray](addr(err))) == llvm.True:
    g.config.internalError($err[0])
    return

  if optNoLinking in g.config.globalOptions:
    return

  # the c generator loads libraries using dlopen/dlsym/equivalent, which nlvm
  # doesn't support, so here, we add a few libraries..
  g.config.cLinkedLibs.add("pcre")

  g.config.addExternalFileToLink(ofile.AbsoluteFile)

  # Linking is a horrible mess - let's reuse the c compiler for now
  g.config.callCCompiler(project.AbsoluteFile)

proc myClose(graph: ModuleGraph, b: PPassContext, n: PNode): PNode =
  if graph.config.skipCodegen(n): return n

  let pc = LLModule(b)
  let g = pc.g
  p("Close", n, 0)

  let om = g.module
  g.module = pc
  defer:
    g.module = om
  g.genNode(n)

  let s = pc.sym

  if sfCompileToCpp in s.flags:
    g.config.internalError("Compile-to-c++ not supported (did you use importcpp?)")

  for m in g.markerBody:
    g.genMarkerProcBody(m[0], m[1])
  g.markerBody.setLen(0)

  if sfMainModule notin s.flags:
    for m in g.markerBody:
      g.genMarkerProcBody(m[0], m[1])
    g.markerBody.setLen(0)
    return n

  # return from nlvmInit

  g.debugUpdateLoc(n)
  g.b.buildBrFallthrough(g.f.ret)
  g.b.positionBuilderAtEnd(g.f.ret)

  discard g.b.buildRetVoid()

  let fn = g.f.ret.getBasicBlockParent()
  if fn.getLastBasicBlock() != g.f.ret:
    g.f.ret.moveBasicBlockAfter(fn.getLastBasicBlock())

  g.finalize(g.f)

  g.genMain()

  var disp = generateMethodDispatchers(graph)
  for i in 0..<disp.sonsLen:
    discard g.genFunctionWithBody(disp[i].sym)

  for m in g.markerBody:
    g.genMarkerProcBody(m[0], m[1])
  g.markerBody.setLen(0)

  g.loadBase()

  if g.d != nil:
    g.d.dIBuilderFinalize()

    # Magic string, see https://groups.google.com/forum/#!topic/llvm-dev/1O955wQjmaQ
    g.m.addModuleFlag(
      ModuleFlagBehaviorWarning, "Debug Info Version",
      valueAsMetadata(g.constInt32(llvm.debugMetadataVersion().int32)))
  g.writeOutput(changeFileExt(g.config.projectFull, "").string)

  result = n

  if g.d != nil:
    g.d.disposeDIBuilder()
    g.d = nil

  g.m.disposeModule()

proc myProcess(b: PPassContext, n: PNode): PNode =
  let pc = LLModule(b)
  let g = pc.g

  let om = g.module
  g.module = pc
  defer: g.module = om

  if g.config.skipCodegen(n): return n

  p("Process", n, 0)
  let newN = transformStmt(g.graph, pc.sym, n)
  # g.f.options = n.sym.options

  g.genNode(newN)

  result = n

proc myOpen(graph: ModuleGraph, s: PSym): PPassContext =
  # In the C generator, a separate C file is generated for every module,
  # but the rules governing what goes into which module are shrouded by a
  # layer of globals and conditionals.
  # Rather than deciphering all that, we simply generate a single module
  # with all the code in it, like the JS generator does.

  # p("Opening", s, 0)

  var g: LLGen

  if graph.backend == nil:
    g = newLLGen(graph)
    graph.backend = g

    let llInitFuncType = llvm.functionType(llvm.voidType(), [])
    let init = g.m.addFunction(".nlvmInit", llInitFuncType)

    let b = llvm.appendBasicBlock(init, g.nn("entry", s))
    g.b.positionBuilderAtEnd(b)

    g.f = g.newLLFunc(llvm.appendBasicBlock(init, g.nn("return")))

    if g.d != nil:
      g.f.ds = g.debugFunction(s, [], init)

    g.f.scopePush(nil, g.f.ret)

    g.init = g.f

  else:
    g = LLGen(graph.backend)
    let entry = llvm.appendBasicBlock(g.init.ret.getBasicBlockParent(), g.nn("entry", s))
    g.b.buildBrFallthrough(entry)
    g.b.positionBuilderAtEnd(entry)

  var llm = LLModule(g: g, sym: s)
  result = llm

const llgenPass* = makePass(myOpen, myProcess, myClose)
