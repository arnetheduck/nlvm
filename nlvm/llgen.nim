# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016-2019
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
    spawn,
    transf,
    trees,
    types,
    wordrecg
  ],
  llvm/llvm,

  lllink,
  llplatform

type
  SectionKind = enum
    secAlloca # Local stack allocations (see localAlloca)
    secArgs # Function argument initialization
    secPreinit # Globals initialization in main
    secLastPreinit
    secBody # User code
    secLastBody # Last piece of body code that was written
    secReturn # Final ret instruction

  LLBlock = ref object
    n: PNode
    exit: llvm.BasicBlockRef
    goto: llvm.ValueRef
    nestedTryStmts: int
    isLoop: bool

  LLEhEntry = tuple[
    n: PNode,
    inExcept: bool,
    tryPad: llvm.BasicBlockRef,
    excPad: llvm.BasicBlockRef,
    extraPads: seq[tuple[pad, src: llvm.BasicBlockRef]]
  ]

  LLFunc = ref object
    f: llvm.ValueRef
    blocks: seq[LLBlock]
    options: TOptions
    nestedTryStmts: seq[LLEhEntry]
    withinLoop: int
    clenv: llvm.ValueRef
    ds: llvm.MetadataRef
    breakIdx: int
    sections: array[SectionKind, llvm.BasicBlockRef]
    cleanupPad: llvm.BasicBlockRef
    badCleanupPad: llvm.BasicBlockRef
    unreachableBlock: llvm.BasicBlockRef
    deadBlocks: seq[llvm.BasicBlockRef]

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
    tm: llvm.TargetMachineRef
    b: llvm.BuilderRef
    d: llvm.DIBuilderRef

    f: LLFunc

    # Cached types
    primitives: array[TTypeKind, llvm.TypeRef]
    cintType: llvm.TypeRef
    csizetType: llvm.TypeRef
    closureType: llvm.TypeRef
    procPtrType: llvm.TypeRef
    voidPtrType: llvm.TypeRef
    jmpBufType: llvm.TypeRef

    strLitFlag: int64 # 1 shl (sizeof(int)*8 - 2) (for target!)

    gep0: llvm.ValueRef
    gep1: llvm.ValueRef
    ni0: llvm.ValueRef
    nb: array[bool, llvm.ValueRef]

    attrNoInline: AttributeRef
    attrNoReturn: AttributeRef
    attrNoOmitFP: AttributeRef
    attrCold: AttributeRef

    symbols: Table[int, LLValue]
    gmarkers: Table[int, llvm.ValueRef]
    markers: Table[SigHash, llvm.ValueRef]
    nodeInfos: Table[SigHash, llvm.ValueRef]
    typeInfos: Table[SigHash, llvm.ValueRef]
    types: Table[SigHash, llvm.TypeRef]

    depth: int

    init: LLFunc
    registrar: LLFunc

    module: LLModule

    markerBody: seq[tuple[v: llvm.ValueRef, typ: PType]]  # Markers looking for a body
    forwardedProcs: seq[PSym] # Proc's looking for a body
    gcRoots: seq[tuple[sym: PSym, v: llvm.ValueRef]] # gcRoots looking for registration

    sigConflicts: CountTable[SigHash]

    # Debug stuff
    dfiles: Table[int, llvm.MetadataRef]
    dtypes: array[TTypeKind, llvm.MetadataRef]
    dstructs: Table[SigHash, llvm.MetadataRef]

    # Compile unit
    dcu: llvm.MetadataRef
    dbgKind: cuint

    # target specific stuff
    tgtExportLinkage: llvm.Linkage

    # Exception handling
    personalityFn: llvm.ValueRef
    landingPadTy: llvm.TypeRef

  LLValue = object
    v: llvm.ValueRef
    lode: PNode # Origin node of this value
    storage: TStorageLoc

  # Same as cgen, for easier genAssignment comparison
  TAssignmentFlag = enum
    needToCopy
  TAssignmentFlags = set[TAssignmentFlag]

template config(g: LLGen): untyped = g.graph.config
template fileInfos(g: LLGen): untyped = g.config.m.fileInfos

# Helpers
proc llType(g: LLGen, typ: PType): llvm.TypeRef
proc llGenericSeqType(g: LLGen): llvm.TypeRef
proc fieldIndex(g: LLGen, typ: PType, sym: PSym): seq[int]
proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef)
proc callErrno(g: LLGen, prefix: string): llvm.ValueRef
proc callCompilerProc(
  g: LLGen, name: string, args: openarray[llvm.ValueRef], noInvoke = false,
  noReturn = false): llvm.ValueRef

proc genFunction(g: LLGen, s: PSym): LLValue
proc genFunctionWithBody(g: LLGen, s: PSym): LLValue
proc genRefAssign(g: LLGen, dest, src: LLValue)
proc genBoundsCheck(g: LLGen, typ: PType, arr, a, b: llvm.ValueRef)

# Magic expressions
proc genMagicLength(g: LLGen, n: PNode): LLValue

# Node handling
proc genNode(g: LLGen, n: PNode, load: bool = false): LLValue {.discardable.}

proc deepTyp(n: PNode): PType =
  if n.typ != nil:
    n.typ
  elif n.kind in {nkTryStmt, nkIfStmt, nkCaseStmt}:
    n[0].deepTyp
  elif n.kind in {nkStmtListExpr}:
    n.lastSon.deepTyp
  else:
    nil

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
proc nn(g: LLGen, s: string, v: LLValue): string = g.nn(s, v.v)

proc newLLFunc(g: LLGen, f: llvm.ValueRef, sym: PSym): LLFunc =
  LLFunc(
    f: f,
    options:
      if sym != nil: sym.options
      else: g.config.options
  )

func pad(e: LLEhEntry): llvm.BasicBlockRef =
  if e.inExcept: e.excPad else: e.tryPad

func fin(e: LLEhEntry): PNode =
  if e.n[^1].kind == nkFinally: e.n[^1] else: nil

proc startBlock(f: LLFunc, n: PNode, exit: llvm.BasicBlockRef): int =
  result = f.blocks.len
  f.blocks.add(
    LLBlock(n: n, exit: exit, nestedTryStmts: f.nestedTryStmts.len))

proc endBlock(f: LLFunc): LLBlock {.discardable.} =
  f.blocks.pop

proc section(g: LLGen, llf: LLFunc, sk: SectionKind): llvm.BasicBlockRef =
  if llf.sections[sk] == nil:
    llf.sections[sk] = appendBasicBlockInContext(g.lc, llf.f, g.nn($sk))
  llf.sections[sk]

proc positionAndMoveToEnd(b: llvm.BuilderRef, bl: llvm.BasicBlockRef) =
  bl.moveBasicBlockAfter(b.getInsertBlock().getBasicBlockParent().getLastBasicBlock())
  b.positionBuilderAtEnd(bl)

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

template withFunc(g: LLGen, llf: LLFunc, body: untyped) =
  block:
    let f = g.f
    g.f = llf
    let db = g.b.getCurrentDebugLocation()
    if optCDebug in g.config.globalOptions:
      g.b.setCurrentDebugLocation(metadataAsValue(g.lc, g.f.ds))
    body
    g.f = f
    if optCDebug in g.config.globalOptions:
      g.b.setCurrentDebugLocation(db)

template withNotNil(g: LLGen, v: llvm.ValueRef, body: untyped) =
  let
    notnil = g.b.appendBasicBlockInContext(g.lc, g.nn("nilcheck.notnil", v))
    done = g.b.appendBasicBlockInContext(g.lc, g.nn("nilcheck.done", v))

  # nil check
  let cond = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("nilcheck.isnil", v))

  discard g.b.buildCondBr(cond, done, notnil)

  g.b.positionBuilderAtEnd(notnil)

  body

  discard g.b.buildBr(done)

  g.b.positionBuilderAtEnd(done)

template withNotNilOrNull(
    g: LLGen, v: llvm.ValueRef, body: untyped): llvm.ValueRef =
  let
    pre = g.b.getInsertBlock()
    lload = g.b.appendBasicBlockInContext(g.lc, g.nn("nilcheck.notnil", v))
    ldone = g.b.appendBasicBlockInContext(g.lc, g.nn("nilcheck.done", v))

  # nil check
  let cond = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("nilcheck.isnil", v))

  discard g.b.buildCondBr(cond, ldone, lload)

  # run body if v is not nil
  g.b.positionBuilderAtEnd(lload)
  # Careful - the PHI instruction below assumes v1 comes from lload block...
  let v1 = body
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # body or default
  let phi = g.b.buildPHI(v1.typeOfX(), g.nn("nilcheck.phi", v))

  phi.addIncoming([constNull(v1.typeOfX()), v1], [pre, lload])
  phi

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

  var v: llvm.ValueRef
  g.withBlock(g.section(g.f, secAlloca)):
    v = g.b.buildAlloca(typ, name)
  v

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
  irrelevantForBackend = {
    tyGenericBody, tyGenericInst, tyGenericInvocation, tyGenericParam,
    tyDistinct, tyOrdinal, tyTypeDesc, tyAlias, tySink, tyUserTypeClass,
    tyUserTypeClassInst, tyInferred, tyStatic}

func assignCopy(sym: PSym): set[TAssignmentFlag] =
  if lfNoDeepCopy in sym.loc.flags: {} else: {needToCopy}

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
  let typ = if typ.kind in {tyAlias, tySink, tyOwned}: typ.lastSon else: typ
  if typ.loc.r == nil:
    typ.loc.r = typ.typeName & $sig
  else:
    when defined(debugSigHashes):
      # check consistency:
      assert($typ.loc.r == $(typ.typeName & $sig))
  if typ.loc.r == nil: internalError(g.config, "getTypeName: " & $typ.kind)
  $typ.loc.r

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
  if t.len > 0:
    result &= $t.len
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
    result &= $s & " " & $n.len
  else:
    result &= $n.flags & " " & $n.len
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

proc constUInt8(g: LLGen, v: uint8): ValueRef =
  llvm.constInt(g.primitives[tyUInt8], v.culonglong, llvm.False)

proc constInt32(g: LLGen, v: int32): ValueRef =
  llvm.constInt(g.primitives[tyInt32], v.culonglong, llvm.False)

proc constInt64(g: LLGen, v: int64): ValueRef =
  llvm.constInt(g.primitives[tyInt64], v.culonglong, llvm.False)

proc constCInt(g: LLGen, val: int): llvm.ValueRef =
  llvm.constInt(g.cintType, val.culonglong, llvm.True)

proc constNimInt(g: LLGen, val: int64): llvm.ValueRef =
  # int64 because we could compile 64-bit programs on 32-bit
  llvm.constInt(g.primitives[tyInt], val.culonglong, llvm.True)

proc constNimInt(g: LLGen, val: Int128): llvm.ValueRef =
  g.constNimInt(val.toInt64())

proc isZero(v: llvm.ValueRef): bool =
  v.isConstant() == llvm.True and
    v.typeOfX().getTypeKind() == IntegerTypeKind and
    v.constIntGetZExtValue() == 0

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

proc bitSetToWord(s: TBitSet, size: int): BiggestUInt =
  result = 0
  for j in 0 ..< size:
    if j < len(s): result = result or (BiggestUInt(s[j]) shl (j * 8))

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
    proc ci8(v: byte): llvm.ValueRef = g.constUInt8(v)
    result = llvm.constArray(g.primitives[tyUInt8], cs.map(ci8))

proc constNimString(g: LLGen, n: PNode): llvm.ValueRef =
  if n.typ.kind == tyCString:
    return g.lc.constStringInContext(n.strVal)

  if n.strVal.len == 0:
    llvm.constNull(g.llType(n.typ))
  else:
    let
      s = g.lc.constStringInContext(n.strVal)
      ll = g.constNimInt(n.strVal.len)
      cap = g.constNimInt(n.strVal.len + g.strLitFlag)
      x = llvm.constNamedStruct(g.llGenericSeqType(), [ll, cap])
    llvm.constStructInContext(g.lc, [x, s])

proc buildExtractValue(b: llvm.BuilderRef, v: LLValue, index: cuint,
                       name: cstring): LLValue =
  LLValue(
    v: b.buildExtractValue(v.v, index, name), lode: v.lode, storage: v.storage)

proc buildGEP(b: llvm.BuilderRef, v: LLValue, indices: openarray[ValueRef],
              name: cstring): LLValue =
  LLValue(v: b.buildGEP(v.v, indices, name), lode: v.lode, storage: v.storage)

proc buildNimSeqLenGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildGEP(s, [g.gep0, g.gep0, g.gep0], g.nn("seq.len", s))

proc buildNimSeqDataGEP(g: LLGen, s: llvm.ValueRef, idx: llvm.ValueRef = nil): llvm.ValueRef =
  let idx = if idx == nil: g.gep0 else: idx
  g.b.buildGEP(s, [g.gep0, g.gep1, idx], g.nn("seq.data", s))

proc buildNimSeqDataGEP(g: LLGen, s: LLValue, idx: llvm.ValueRef = nil): LLValue =
  LLValue(
    v: g.buildNimSeqDataGEP(s.v, idx), lode: s.lode,
    storage: if s.storage == OnStatic: OnStatic else: OnHeap)

proc buildI1(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
 g.b.buildTrunc(v, int1TypeInContext(g.lc), g.nn("bool.i1", v))

proc buildI8(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.b.buildZExt(v, int8TypeInContext(g.lc), g.nn("bool.i8", v))

proc isUnsigned(g: LLGen, typ: PType): bool =
  let typ = typ.skipTypes(abstractVarRange)

  typ.kind in {tyUInt..tyUInt64, tyBool, tyChar, tySet} or
    (typ.kind == tyEnum and g.config.firstOrd(typ) >= 0)

proc buildNimIntExt(g: LLGen, v: llvm.ValueRef, unsigned: bool): llvm.ValueRef =
  let nt = g.primitives[tyInt]

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

proc needsTerminator(b: llvm.BasicBlockRef): bool =
  b.getBasicBlockTerminator() == nil

proc needsTerminator(b: llvm.BuilderRef): bool =
  b.getInsertBlock().needsTerminator()

proc buildBrFallthrough(b: llvm.BuilderRef, next: llvm.BasicBlockRef) =
  # Add a br to the next block if the current block is not already terminated
  if b.needsTerminator():
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

  g.b.buildShl(llvm.constInt(t, 1, llvm.False), shift, g.nn("set.pos", ix))

proc buildSetGEPMask(g: LLGen, vx, ix: llvm.ValueRef): tuple[gep, mask: llvm.ValueRef] =
  let idx = g.b.buildUDiv(ix, llvm.constInt(ix.typeOfX(), 8, llvm.False), g.nn("set.byte", ix))
  let gep = g.b.buildGEP(vx, [g.gep0, idx], g.nn("set.gep", vx))

  let mask = g.buildSetMask(llvm.int8TypeInContext(g.lc), ix, 1)

  (gep: gep, mask: mask)

proc buildLoadValue(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  if v.typeOfX().isArrayPtr():
    g.b.buildGEP(v, [g.gep0, g.gep0], g.nn("load", v))
  else:
    g.b.buildLoad(v, g.nn("load", v))

proc buildLoadValue(g: LLGen, v: LLValue): LLValue =
  # The source remains the same, even though we're turning addess into value
  LLValue(
    v: g.buildLoadValue(v.v),
    lode: v.lode,
    storage: v.storage
  )

proc maybeLoadValue(g: LLGen, v: LLValue, load: bool): LLValue =
  if load: g.buildLoadValue(v)
  else: v

proc buildNot(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  let cmp = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constInt(v.typeOfX(), 0, llvm.False), g.nn("not", v))
  g.b.buildZExt(cmp, v.typeOfX(), g.nn("zext", cmp))

proc buildBitnot(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.b.buildXor(v, llvm.constInt(v.typeOfX(), not culonglong(0), llvm.False), g.nn("bitnot", v))

proc buildStoreNull(g: LLGen, v: llvm.ValueRef) =
  let t = v.typeOfX()
  assert t.getTypeKind() == llvm.PointerTypeKind
  let et = t.getElementType()
  # TODO I have a vague recollection of seeing constNull explode in size when
  #      storing zero like this, so we use memset instead for large entities -
  #      this needs revisiting however, perhaps it was an old llvm or something?
  if et.getTypeKind() in {llvm.ArrayTypeKind, llvm.StructTypeKind} and
      g.m.getModuleDataLayout().sizeOfXTypeInBits(et) > (64'u64 * 8):
    g.callMemset(v, g.constInt8(0), et.sizeOfX())
  else:
    discard g.b.buildStore(constNull(et), v)

proc buildCallOrInvoke(
    g: LLGen, fx: llvm.ValueRef, args: openArray[llvm.ValueRef], name: cstring = ""): llvm.ValueRef =
  if g.f.nestedTryStmts.len > 0:
    let
      then = g.b.appendBasicBlockInContext(g.lc, g.nn("invoke.then", fx))
      res = g.b.buildInvoke(fx, args, then, g.f.nestedTryStmts[^1].pad, name)
    g.b.positionBuilderAtEnd(then)
    res
  else:
    g.b.buildCall(fx, args, name)

proc buildCallOrInvokeBr(
    g: LLGen, fx: llvm.ValueRef, then: llvm.BasicBlockRef,
    args: openArray[llvm.ValueRef], name: cstring = ""): llvm.ValueRef =
  if g.f.nestedTryStmts.len > 0:
    let res = g.b.buildInvoke(fx, args, then, g.f.nestedTryStmts[^1].pad, name)
    g.b.positionBuilderAtEnd(then)
    res
  else:
    let res = g.b.buildCall(fx, args, name)
    discard g.b.buildBr(then)
    res

proc loadNimSeqLen(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.withNotNilOrNull(v):
    let gep = g.buildNimSeqLenGEP(v)
    g.b.buildLoad(gep, g.nn("nilcheck.load", v))

proc getNimSeqDataPtr(g: LLGen, v: llvm.ValueRef, idx: llvm.ValueRef = nil): llvm.ValueRef =
  g.withNotNilOrNull(v):
    g.buildNimSeqDataGEP(v, idx)

proc isObjLackingTypeField(typ: PType): bool {.inline.} =
  result = (typ.kind == tyObject) and ((tfFinal in typ.flags) and
      (typ.sons[0] == nil) or isPureObject(typ))

proc debugSize(g: LLGen, typ: llvm.TypeRef):
    tuple[typeBits, storeBits, allocBits, abiBits: uint32] =
  let
    tgt = g.m.getModuleDataLayout()
  (sizeOfXTypeInBits(tgt, typ).uint32, storeSizeOfType(tgt, typ).uint32,
    aBISizeOfType(tgt, typ).uint32 * 8,
    preferredAlignmentOfType(tgt, typ).uint32 * 8)

proc debugSize(g: LLGen, typ: PType):
    tuple[typeBits, storeBits, allocBits, abiBits: uint32] =
  g.debugSize(g.llType(typ))

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
  let ptrBits = g.m.getModuleDataLayout().pointerSize()
  case typ.kind
  of tyBool: g.dtypes[tyBool]
  of tyChar: g.dtypes[tyChar]
  of tyNil, tyTyped: nil
  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyGenericParam,
      tyDistinct, tyOrdinal, tyTypeDesc, tyAlias, tySink, tyUserTypeClass,
      tyUserTypeClassInst, tyInferred, tyStatic:
    g.debugType(typ.lastSon)
  of tyEnum:
    let (bits, _, _, _) = g.debugSize(typ)
    g.d.dIBuilderCreateBasicType(
      g.llName(typ, hashType(typ)), bits, DW_ATE_unsigned)
  of tyArray:
    let et = g.debugType(typ.elemType)
    # Arbitrary limit of 1024 items here - large numbers seem to have a poor
    # impact on some debuggers that insist on displaying all sub-items (looking
    # at you, eclipse+gdb) - need to investigate this more
    # TODO compiler uses the Int128 abomination here - need to rework accordingly
    let (s, c) =
      if g.config.lengthOrd(typ) > 1024: (cuint(0), -1.int64)
      else:
        (cuint(g.config.lengthOrd(typ).toInt64),
          g.config.lengthOrd(typ).toInt64)
    g.d.dIBuilderCreateArrayType(s * 8, 0, et,
              [g.d.dIBuilderGetOrCreateSubrange(0, c)])
  of tyUncheckedArray:
    let et = g.debugType(typ.elemType)
    let (s, c) = (cuint(0), -1.int64)
    g.d.dIBuilderCreateArrayType(s * 8, 0, et,
                [g.d.dIBuilderGetOrCreateSubrange(0, c)])
  of tyObject: g.debugStructType(typ)
  of tyTuple: g.debugTupleType(typ)
  of tySet:
    let (bits, _, _, abi) = g.debugSize(typ)
    if bits <= 8 * 8:
      g.d.dIBuilderCreateBasicType("set" & $bits, abi, DW_ATE_unsigned)
    else:
      g.d.dIBuilderCreateArrayType(
        bits, abi, g.dtypes[tyUInt8],
        [g.d.dIBuilderGetOrCreateSubrange(0, bits.int64 div 8)])
  of tyRange: g.debugType(typ.sons[0])
  of tyPtr, tyRef, tyVar, tyLent:
    g.d.dIBuilderCreatePointerType(
      g.debugType(typ.lastSon), ptrBits, ptrBits, "")
  of tySequence:
    var st: llvm.MetadataRef

    let sig = hashType(typ)

    if sig in g.dstructs:
      st = g.dstructs[sig]
    else:
      let
        name = g.llName(typ, sig)
        file = g.debugGetFile(g.config.projectMainIdx)
        line = if typ.sym != nil: typ.sym.info.line else: 0
        sup = g.debugMagicType("TGenericSeq")
        supt = g.llGenericSeqType()
        (bits, _, _, abi) = g.debugSize(supt)

        st = g.d.dIBuilderCreateStructType(g.dcu, name,
          file, line.cuint, bits, abi, 0, nil, [], 0, nil, name)
      g.dstructs[sig] = st

      var elems = @[
        g.d.dIBuilderCreateMemberType(g.dcu, "Sup", file, 0, bits, abi, 0, 0,
          sup)
      ]
      if typ.elemType.kind != tyEmpty:
        let
          dt = g.debugType(typ.elemType)
          (_, _, _, eabi) = g.debugSize(typ.elemType)

        elems.add(
          g.d.dIBuilderCreateMemberType(g.dcu, "data", file, 0, 0, eabi, bits, 0,
            g.d.dIBuilderCreateArrayType(0, eabi, dt,[
              g.d.dIBuilderGetOrCreateSubrange(0, -1)])))

      g.d.nimDICompositeTypeSetTypeArray(
        st, g.d.dIBuilderGetOrCreateArray(elems))

    g.d.dIBuilderCreatePointerType(st, ptrBits, ptrBits, "")
  of tyProc:
    if typ.callConv == ccClosure:
      g.d.dIBuilderCreateStructType(g.dcu, "closure",
        g.debugGetFile(g.config.projectMainIdx), 0, ptrBits * 2, ptrBits, 0, nil,
        [g.dtypes[tyPointer], g.dtypes[tyPointer]], 0, nil, "closure")
    else:
      g.dtypes[tyPointer]
  of tyPointer: g.dtypes[tyPointer]
  of tyOpenArray, tyVarargs: g.debugType(typ.elemType)
  of tyString:
    if g.dtypes[tyString] == nil:
      g.dtypes[tyString] = g.d.dIBuilderCreatePointerType(
        g.debugMagicType("NimStringDesc"), ptrBits, ptrBits, "")
    g.dtypes[tyString]
  of tyCString: g.dtypes[tyCString]
  of tyInt..tyUInt64: g.dtypes[typ.kind]
  else: g.config.internalError("Unhandled debug type " & $typ.kind); nil

proc debugFieldName(field: PSym, typ: PType): string =
  if (typ.sym != nil) and ({sfImportc, sfExportc} * typ.sym.flags != {}):
    $field.loc.r
  else:
    mangle(field.name.s)

proc align(address, alignment: cuint): cuint =
  (address + (alignment - 1)) and not (alignment - 1)

proc debugStructFields(
    g: LLGen, elements: var seq[MetadataRef], n: PNode, typ: PType,
    offset: var uint32) =
  case n.kind
  of nkRecList:
    for child in n:
      g.debugStructFields(elements, child, typ, offset)
  of nkRecCase: # TODO Unionize
    if n[0].kind != nkSym: g.config.internalError(n.info, "debugStructFields")
    g.debugStructFields(elements, n[0], typ, offset)

    for i in 1..<n.len:
      case n[i].kind
      of nkOfBranch, nkElse:
        g.debugStructFields(elements, n[i].lastSon, typ, offset)
      else: g.config.internalError(n.info, "debugStructFields")
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType(): return
    let
      (mbits, _, malloc, mabi) = g.debugSize(field.typ)
      name = debugFieldName(field, typ)
      line = field.info.line

    offset = align(offset, mabi)
    let member = g.d.dIBuilderCreateMemberType(
      g.dcu, name, g.debugGetFile(g.config.projectMainIdx), line.cuint, mbits,
      mabi, offset, 0, g.debugType(field.typ))
    offset += malloc
    elements.add(member)
  else: g.config.internalError(n.info, "debugStructFields")

proc debugStructType(g: LLGen, typ: PType): llvm.MetadataRef =
  if typ == nil:
    return

  let typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString: return g.debugType(typ)

  let sig = hashType(typ)
  if sig in g.dstructs:
    return g.dstructs[sig]

  let
    name = g.llName(typ, sig)
    line = if typ.sym != nil: typ.sym.info.line else: 0
    file = g.debugGetFile(g.config.projectMainIdx)
    (bits, _, _, abi) = g.debugSize(typ)

  # Create struct before setting body in case it's recursive
  result = g.d.dIBuilderCreateStructType(g.dcu, name,
    file, line.cuint, bits, abi, 0, nil, [], 0, nil, name)

  g.dstructs[sig] = result

  var
    elements = newSeq[MetadataRef]()
    offset: uint32

  let super = if typ.sons.len == 0: nil else: typ.sons[0]
  if super == nil:
    if (typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags:
      discard
    else:
      let
        ptrBits = g.m.getModuleDataLayout().pointerSize().uint32 * 8
        tnt = g.debugMagicType("TNimType")
        tntp = g.d.dIBuilderCreatePointerType(tnt, ptrBits, ptrBits, "")
        member = g.d.dIBuilderCreateMemberType(
          g.dcu, "m_type", g.debugGetFile(g.config.projectMainIdx), 0, ptrBits,
          ptrBits, offset, 0, tntp)
      offset += bits

      elements.add(member)
  else:
    let
      (bits, _, alloc, abi) = g.debugSize(super)
      member = g.d.dIBuilderCreateMemberType(
        g.dcu, "Sup", g.debugGetFile(g.config.projectMainIdx), 0, bits,
        abi, offset, 0, g.debugType(super))
    offset += alloc

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

  let
    name = g.llName(typ, sig)
    line = if typ.sym != nil: typ.sym.info.line else: 0

  # Create struct before setting body in case it's recursive
  let
    file = g.debugGetFile(g.config.projectMainIdx)
    (bits, _, _, abi) = g.debugSize(typ)

  result = g.d.dIBuilderCreateStructType(
    g.dcu, name, file, line.cuint, bits, abi, 0, nil, [], 0, nil, name)

  g.dstructs[sig] = result

  var elements = newSeq[MetadataRef]()
  var offset: uint32
  for t in typ.sons:
    let
      (mbits, _, malloc, mabi) = g.debugSize(t)
      mline = if t.sym != nil: t.sym.info.line else: line

    offset = align(offset, mabi)
    let member = g.d.dIBuilderCreateMemberType(
      g.dcu, "tup" & $offset, g.debugGetFile(g.config.projectMainIdx), mline,
      mbits, mabi, offset, 0, g.debugType(t))
    offset += malloc
    elements.add(member)

  g.d.nimDICompositeTypeSetTypeArray(
    result, g.d.dIBuilderGetOrCreateArray(elements))

proc debugMagicType(g: LLGen, name: string): llvm.MetadataRef =
  g.debugType(g.graph.getCompilerProc(name).typ)

proc debugProcParamType(g: LLGen, t: PType): llvm.MetadataRef =
  let
    typ = t.skipTypes(abstractInst)
    ptrBits = g.m.getModuleDataLayout().pointerSize().uint32 * 8
  case typ.kind:
  of tyArray, tyOpenArray, tyUncheckedArray, tyVarargs, tyObject, tyTuple:
    g.d.dIBuilderCreatePointerType(g.debugType(t), ptrBits, ptrBits, "")
  of tySet:
    let (bits, _, _, _) = g.debugSize(typ)
    if bits <= 8 * 8: g.debugType(t)
    else: g.d.dIBuilderCreatePointerType(g.debugType(t), ptrBits, ptrBits, "")
  of tyProc:
    if typ.callConv == ccClosure:
      g.d.dIBuilderCreatePointerType(g.debugType(t), ptrBits, ptrBits, "")
    else: g.debugType(t)
  of tyDistinct, tyAlias, tyInferred, tySink: g.debugProcParamType(t.lastSon)
  else: g.debugType(t)

proc debugProcType(g: LLGen, typ: PType, closure: bool): seq[llvm.MetadataRef] =
  let retType = if typ.sons[0] == nil: nil
                else: g.debugType(typ.sons[0])
  result.add(retType)

  for param in typ.procParams():
    let t = param.sym.typ.skipTypes({tyGenericInst})
    let at = g.debugProcParamType(t)
    result.add(at)

    if skipTypes(t, {tyVar, tyLent}).kind in {tyOpenArray, tyVarargs}:
      result.add(g.dtypes[tyInt])  # Extra length parameter

  if closure:
    result.add(g.dtypes[tyPointer])

proc debugGetScope(g: LLGen): llvm.MetadataRef =
  if g.f != nil:
    g.f.ds
  else:
    g.dcu

proc debugUpdateLoc(g: LLGen, n: PNode) =
  if g.d == nil: return
  if n == nil:
    g.b.setCurrentDebugLocation(nil)
    return

  # This scope ensures that even for top-level statements, the right file is
  # used together with the line and column - it happens this way because we
  # dump such statements from all nim modules into a single `main` function.
  # It's a bit of a hack, better would be to control this more tightly so as to
  # avoid creating all these scopes - we should also be creating
  # a new lexical scope whenever a new block begins - probably somewhere around
  # startBlock..
  let scope = g.d.dIBuilderCreateLexicalBlockFile(
    g.debugGetScope(), g.debugGetFile(n.info.fileIndex), 0)
  let dlm = g.lc.dIBuilderCreateDebugLocation(
    max(n.info.line.cuint, 1), max(n.info.col, 1).cuint, scope, nil)

  let dl = metadataAsValue(g.lc, dlm)
  g.b.setCurrentDebugLocation(dl)

proc debugVariable(g: LLGen, sym: PSym, v: llvm.ValueRef, argNo = -1) =
  if g.d == nil: return

  var dt = g.debugType(sym.typ)

  let scope = g.debugGetScope()

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

  let
    dt = g.debugType(sym.typ)
    scope = g.debugGetScope()
    linkageName = sym.llName()
    name = if sym.name.s.len == 0: linkageName else: sym.name.s

  let gve = dIBuilderCreateGlobalVariableExpression(
    g.d, scope, name, linkageName,
    g.debugGetFile(sym.info.fileIndex), sym.info.line.cuint, dt, false,
    dIBuilderCreateExpression(g.d, nil, 0), nil, 0
  )
  v.nimSetMetadataGlobal(g.dbgKind, g.lc.metadataAsValue(gve))

proc debugFunction(
    g: LLGen, s: PSym, params: openArray[llvm.MetadataRef],
    f: llvm.ValueRef): llvm.MetadataRef =
  let
    df = g.debugGetFile(if s == nil: g.config.projectMainIdx else: s.info.fileIndex)
    line = if s == nil: 0.cuint else: s.info.line.cuint
    st = g.d.dIBuilderCreateSubroutineType(df, params)
    linkageName = $f.getValueName()
    name = if s == nil or s.name.s.len == 0: linkageName else: s.name.s
  result = g.d.dIBuilderCreateFunction(
    g.dcu, name, linkageName, df, line, st, false, true, line, 0, false)
  f.setSubprogram(result)

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef
proc llTupleType(g: LLGen, typ: PType): llvm.TypeRef
proc llProcType(g: LLGen, typ: PType, closure = false): llvm.TypeRef

proc llMagicType(g: LLGen, name: string): llvm.TypeRef =
  g.llType(g.graph.getCompilerProc(name).typ)

proc llStringType(g: LLGen): llvm.TypeRef =
  if g.primitives[tyString] == nil:
    g.primitives[tyString] = g.llMagicType("NimStringDesc").pointerType
  g.primitives[tyString]

proc llGenericSeqType(g: LLGen): llvm.TypeRef =
  if g.primitives[tySequence] == nil:
    g.primitives[tySequence] = g.llMagicType("TGenericSeq")
  g.primitives[tySequence]

proc llType(g: LLGen, typ: PType): llvm.TypeRef =
  case typ.kind
  of tyBool, tyChar, tyNil, tyTyped: g.primitives[typ.kind]
  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyGenericParam,
     tyDistinct, tyOrdinal, tyTypeDesc, tyAlias, tySink, tyUserTypeClass,
     tyUserTypeClassInst, tyInferred, tyStatic:
    g.llType(typ.lastSon)
  of tyEnum: llvm.intTypeInContext(g.lc, g.config.getSize(typ).cuint * 8)
  of tyArray:
    let et = g.llType(typ.elemType)
    # Even for unchecked arrays, we use lengthord here - echo for
    # example generates unchecked arrays with correct length set, and
    # if we skip length, there will be a local stack array of length
    # 0 here!
    # TODO check Int128 usage
    let n = cuint(g.config.lengthOrd(typ).toUInt)
    llvm.arrayType(et, n)
  of tyUncheckedArray:
    let et = g.llType(typ.elemType)
    let n = cuint(0)
    llvm.arrayType(et, n)
  of tyObject: g.llStructType(typ)
  of tyTuple: g.llTupleType(typ)
  of tySet:
    let size = g.config.getSize(typ).cuint
    if size <= 8: llvm.intTypeInContext(g.lc, size * 8)
    else: llvm.arrayType(llvm.int8TypeInContext(g.lc), size)
  of tyRange: g.llType(typ.sons[0])
  of tyPtr, tyRef, tyVar, tyLent: g.llType(typ.elemType).pointerType()
  of tySequence:
    var st: llvm.TypeRef

    let sig = hashType(typ)

    if sig in g.types:
      st = g.types[sig]
    else:
      let name = g.llName(typ, sig)
      st = structCreateNamed(g.lc, name)
      g.types[sig] = st

      if typ.elemType.kind == tyEmpty:
        st.structSetBody([g.llGenericSeqType()])
      else:
        st.structSetBody(
          [g.llGenericSeqType(), llvm.arrayType(g.llType(typ.elemType), 0)])
    st.pointerType()
  of tyProc:
    if typ.callConv == ccClosure: g.closureType
    else: g.procPtrType
  of tyPointer: g.primitives[typ.kind]
  of tyOpenArray, tyVarargs: g.llType(typ.elemType)
  of tyString: g.llStringType()
  of tyCString, tyInt..tyUInt64: g.primitives[typ.kind]
  else:
    g.config.internalError("Unhandled type " & $typ.kind); nil

template preserve(v: typed, body: untyped): untyped =
  let old = v
  body
  v = old

proc finalize(g: LLGen) =
  let ret = g.section(g.f, secReturn)

  var last, cur: llvm.BasicBlockRef

  for sk in SectionKind:
    if last == g.f.sections[sk]:
      continue # secLastBody!

    cur = g.f.sections[sk]
    if cur == nil: continue

    if last == nil:
      if cur != g.f.f.getEntryBasicBlock():
        cur.moveBasicBlockBefore(g.f.f.getEntryBasicBlock())
    else:
      if sk notin {secLastPreinit, secLastBody}: cur.moveBasicBlockAfter(last)
      g.withBlock(last):
        g.b.buildBrFallthrough(cur)
    last = cur

  ret.moveBasicBlockAfter(g.f.f.getLastBasicBlock())

  for dead in g.f.deadBlocks:
    if dead.needsTerminator():
      g.withBlock(dead):
        discard g.b.buildUnreachable()

proc addStructFields(g: LLGen, elements: var seq[TypeRef], n: PNode, typ: PType) =
  p("addStructFields", n, g.depth)
  case n.kind
  of nkRecList:
    for child in n:
      g.addStructFields(elements, child, typ)
  of nkRecCase: # TODO Unionize
    if n[0].kind != nkSym: g.config.internalError(n.info, "addStructFields")
    g.addStructFields(elements, n[0], typ)

    for i in 1..<n.len:
      case n[i].kind
      of nkOfBranch, nkElse:
        g.addStructFields(elements, n[i].lastSon, typ)
      else: g.config.internalError(n.info, "addStructFields")
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType(): return
    fillLoc(field.loc, locField, n, ~"", OnUnknown)
    elements.add(g.llType(field.typ))
  else: g.config.internalError(n.info, "addStructFields")

proc headerType(g: LLGen, name: string): llvm.TypeRef =
  # Here are replacements for some of the types in the nim standard library that
  # rely on c header parsing to work. This is a crude workaround that needs to
  # be replaced, but works for now, on linux/x86_64
  case name
  of "jmp_buf": g.jmpbufType
  else: nil

proc headerTypeIndex(g: LLGen, typ: PType, sym: PSym): seq[int] =
  let sig = hashType(typ)
  case g.llName(typ, sig)
  else: discard

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef =
  if typ == nil:
    return

  var typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString:
    return g.llMagicType("NimStringDesc")

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

  # Ugly workaround on linux..
  let packed = "epoll_event" in name

  result.structSetBody(elements, if packed: llvm.True else: llvm.False)

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

proc isDeepConstExprLL(n: PNode): bool =
  # Upstream treats unsigned integers as non-const:
  # https://github.com/nim-lang/Nim/pull/9873
  # .. and a few other small differences that nlvm needs to address ..
  case n.kind
  of nkCharLit..nkNilLit:
    result = true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = isDeepConstExprLL(n.sons[1])
  of nkCurly, nkBracket, nkPar, nkTupleConstr, nkObjConstr, nkClosure, nkRange:
     # TODO inheritance can be done, just being lazy...
    if n.typ == nil: return true
    let typ = n.typ.skipTypes(abstractInst)
    if n.kind == nkObjConstr and typ.sons[0] != nil: return false

    for i in ord(n.kind == nkObjConstr) ..< n.len:
      if not isDeepConstExprLL(n.sons[i]): return false
    if n.typ.isNil: result = true
    else:
      let t = n.typ.skipTypes({tyGenericInst, tyDistinct, tyAlias, tySink})
      if t.kind in {tyRef, tyPtr}: return false
      if t.kind != tyObject or not isCaseObj(t.n):
        result = true
  else: discard

proc canMove(g: LLGen, n: PNode): bool =
  if n == nil: return false

  # From cgen
  if n.kind == nkBracket:
    # This needs to be kept consistent with 'const' seq code
    # generation!
    if not isDeepConstExprLL(n) or n.len == 0:
      if skipTypes(n.typ, abstractVarRange).kind == tySequence:
        return true
  elif optNilSeqs notin g.config.options and
    n.kind in nkStrKinds and n.strVal.len == 0:
    # Empty strings are codegen'd as NIM_NIL so it's just a pointer copy
    return true
  result = n.kind in nkCallKinds

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
    let
      gep = g.b.buildGEP(v, (@[0] & start).map(cgi), g.nn("mk.kind", kind))
      vk = g.b.buildLoad(gep, g.nn("mk.kind.load", kind))
      caseend = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.kind.end", kind))

    inc(start)
    var hasElse = false

    for i in 1..<n.len:
      let
        branch = n[i]
        ctrue = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.kind.true", branch))

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

          let cfalse = g.b.appendBasicBlockInContext(
            g.lc, g.nn("mk.kind.false", branch))
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
    if field.typ.isEmptyType(): return
    proc cgi(v: int): llvm.ValueRef = g.constGEPIdx(v)
    var gep = g.b.buildGEP(v, (@[0] & start).map(cgi), g.nn("mk", field))
    if field.typ.skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
      gep = g.b.buildLoad(gep, g.nn("mk.load", field))

    g.genMarker(field.typ, gep, op)
    inc(start)
  else: g.config.internalError(n.info, "genMarker()")

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ == nil: return

  case typ.kind
  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyGenericParam,
      tyDistinct, tyOrdinal, tyTypeDesc, tyAlias, tySink, tyUserTypeClass,
      tyUserTypeClassInst, tyInferred, tyStatic:
    g.genMarker(typ.lastSon(), v, op)
  of tyArray:
    let
      arraySize = g.config.lengthOrd(typ.sons[0])
      wcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.arr.cmp"))
      wtrue = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.arr.true"))
      wfalse = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.arr.false"))
      cnt = g.localAlloca(g.primitives[tyInt], g.nn("mk.arr.cnt"))

    discard g.b.buildStore(g.constNimInt(0), cnt)

    # jump to comparison
    discard g.b.buildBr(wcmp)

    # generate condition expression in cmp block
    g.b.positionBuilderAtEnd(wcmp)
    let c = g.b.buildLoad(cnt, g.nn("mk.arr.c"))
    let cond = g.b.buildICmp(llvm.IntULT, c, g.constNimInt(arraySize), g.nn("mk.arr.lt"))

    discard g.b.buildCondBr(cond, wtrue, wfalse)

    # loop body
    g.b.positionBuilderAtEnd(wtrue)

    var gep =
      if v.typeOfX().isArrayPtr(): g.b.buildGEP(v, [g.gep0, c], g.nn("while.data"))
      else: g.b.buildGEP(v, [c], g.nn("while.data"))

    if typ.sons[1].skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
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

  let
    seqlen = g.loadNimSeqLen(v)
    wcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.seq.cmp"))
    wtrue = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.seq.true"))
    wfalse = g.b.appendBasicBlockInContext(g.lc, g.nn("while.false"))
    cnt = g.localAlloca(g.primitives[tyInt], g.nn("mk.seq.cnt"))

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
  if typ.sons[0].skipTypes(abstractInst).kind in
      {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
    gep = g.b.buildLoad(gep, g.nn("mk.seq.load"))
  genMarker(g, typ.sons[0], gep, op)

  # back to comparison
  let cn = g.b.buildAdd(c, g.constNimInt(1), g.nn("mk.seq.add"))
  discard g.b.buildStore(cn, cnt)
  discard g.b.buildBr(wcmp)

  # continue at the end
  g.b.positionAndMoveToEnd(wfalse)

proc genMarkerProcBody(g: LLGen, f: llvm.ValueRef, typ: PType) =
  let llf = g.newLLFunc(f, nil)
  if g.d != nil:
    llf.ds = g.debugFunction(
      typ.sym, [nil, g.dtypes[tyPointer], g.dtypes[tyInt]], f)

  g.withFunc(llf):
    if g.d != nil:
      let dl = g.lc.dIBuilderCreateDebugLocation(
          1, 1, llf.ds, nil)
      g.b.setCurrentDebugLocation(g.lc.metadataAsValue(dl))

    g.withBlock(g.section(g.f, secBody)):

      let v = f.getFirstParam()
      v.setValueName("v")
      let op = v.getNextParam()
      op.setValueName("op")

      let vs = g.localAlloca(v.typeOfX(), g.nn("mkr.vs", v))
      discard g.b.buildStore(v, vs)

      let ops = g.localAlloca(op.typeOfX(), g.nn("mkr.ops", op))
      discard g.b.buildStore(op, ops)

      if g.d != nil:
        let dt = if typ.kind == tySequence: g.debugType(typ)
                else: g.d.dIBuilderCreatePointerType(
          g.debugType(typ.elemType), 64, 64, "")

        let file = g.debugGetFile(
          if typ.sym == nil: g.config.projectMainIdx else: typ.sym.info.fileIndex)
        let vd = g.d.dIBuilderCreateParameterVariable(
          llf.ds, $v.getValueName(), 1, file, 0, dt, false, 0)
        discard g.d.dIBuilderInsertDeclareAtEnd(vs, vd,
          g.d.dIBuilderCreateExpression(nil, 0),
          valueAsMetadata(g.b.getCurrentDebugLocation()), g.b.getInsertBlock())

        let opd = g.d.dIBuilderCreateParameterVariable(
          llf.ds, $op.getValueName(), 2, file, 0, g.dtypes[tyInt], false, 0)
        discard g.d.dIBuilderInsertDeclareAtEnd(ops, opd,
          g.d.dIBuilderCreateExpression(nil, 0),
          valueAsMetadata(g.b.getCurrentDebugLocation()), g.b.getInsertBlock())

      if typ.kind == tySequence:
        g.genMarkerSeq(typ, v, op)
      else:
        g.genMarker(typ.sons[0], v, op)

      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      discard g.b.buildRetVoid()

    g.finalize()

proc genMarkerProc(g: LLGen, typ: PType, sig: SigHash): llvm.ValueRef =
  if g.config.selectedGC < gcMarkAndSweep:
    return

  if sig in g.markers:
    return g.markers[sig]

  let
    name = "Marker_" & g.llName(typ, sig)
    pt =
      if typ.kind == tySequence: g.llType(typ)
      else: g.llType(typ.elemType).pointerType()
    ft = llvm.functionType(
      llvm.voidTypeInContext(g.lc), @[pt, g.primitives[tyInt]], false)

  result = g.m.addFunction(name, ft)
  g.markers[sig] = result

  # Because we generate only one module, we can tag all functions internal
  result.setLinkage(llvm.InternalLinkage)

  # Can't generate body yet - some magics might not yet exist
  g.markerBody.add((result, typ))

proc genGlobalMarkerProc(g: LLGen, sym: PSym, v: llvm.ValueRef): llvm.ValueRef =
  if sym.id in g.gmarkers:
    return g.gmarkers[sym.id]

  let
    name = ".marker.g." & sym.llName
    ft = llvm.functionType(llvm.voidTypeInContext(g.lc), @[], false)

  result = g.m.addFunction(name, ft)
  g.gmarkers[sym.id] = result

  # Because we generate only one module, we can tag all functions internal
  result.setLinkage(llvm.InternalLinkage)

  let f = g.newLLFunc(result, nil)

  if g.d != nil:
    f.ds = g.debugFunction(sym, [], result)

  g.withFunc(f):
    if g.d != nil:
      let dl = g.lc.dIBuilderCreateDebugLocation(
          1, 1, f.ds, nil)
      g.b.setCurrentDebugLocation(g.lc.metadataAsValue(dl))

    g.withBlock(g.section(g.f, secBody)):
      var v = v
      let typ = sym.typ.skipTypes(abstractInst)
      if typ.kind in {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
        v = g.b.buildLoad(v, g.nn("mk.load"))

      g.genMarker(typ, v, g.constInt8(0))
      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      discard g.b.buildRetVoid()

    g.finalize()

proc registerGcRoot(g: LLGen, sym: PSym, v: llvm.ValueRef) =
  if g.config.selectedGC in {gcMarkAndSweep, gcDestructors, gcV2, gcRefc} and
      sym.typ.containsGarbageCollectedRef() and sym.id notin g.gmarkers:
    g.gcRoots.add((sym, v))

proc genGcRegistrar(g: LLGen, sym: PSym, v: llvm.ValueRef) =
    # This is a bit backwards - better would be to rewrite nimRegisterGlobalMarker!
    if g.registrar.isNil:
      let
        name = ".nlvm.registrar"
        registrarType = llvm.functionType(llvm.voidTypeInContext(g.lc), @[], false)
        registrar = g.m.addFunction(name, registrarType)

      registrar.setLinkage(llvm.PrivateLinkage)

      let
        ctorsInit = llvm.constStructInContext(g.lc, [
          g.constInt32(65535), registrar, constNull(g.voidPtrType)])
        ctorsType = ctorsInit.typeOfX()
        ctorsArrayType = llvm.arrayType(ctorsType, 1)
        ctors = g.m.addGlobal(ctorsArrayType, "llvm.global_ctors")

      ctors.setLinkage(llvm.AppendingLinkage)
      ctors.setInitializer(llvm.constArray(ctorsType, [ctorsInit]))

      let f = g.newLLFunc(registrar, nil)
      if g.d != nil:
        f.ds = g.debugFunction(nil, [], registrar)
      g.registrar = f

      g.withFunc(g.registrar): g.withBlock(g.section(g.f, secReturn)):
        if g.f.ds != nil:
          let dl = g.lc.dIBuilderCreateDebugLocation(
              1, 1, g.f.ds, nil)
          g.b.setCurrentDebugLocation(g.lc.metadataAsValue(dl))
        discard g.b.buildRetVoid()

    let prc = g.genGlobalMarkerProc(sym, v)

    g.withFunc(g.registrar):
      g.withBlock(g.section(g.f, secBody)):
        if g.f.ds != nil:
          let dl = g.lc.dIBuilderCreateDebugLocation(
              1, 1, g.f.ds, nil)
          g.b.setCurrentDebugLocation(g.lc.metadataAsValue(dl))

        discard g.callCompilerProc("nimRegisterGlobalMarker", [prc])

proc genTypeInfoInit(g: LLGen, t: PType, ntlt, lt: llvm.TypeRef,
                     baseVar, nodeVar, finalizerVar, markerVar,
                     deepcopyVar: llvm.ValueRef): llvm.ValueRef =
  let sizeVar = if lt == nil: g.ni0 else: llvm.sizeOfX(lt)

  let kind =
    if t.isObjLackingTypeField(): tyPureObject
    elif t.kind == tyProc and t.callConv == ccClosure: tyTuple
    else: t.kind
  let kindVar = g.constInt8(int8(ord(kind)))

  var flags = 0'i8
  if not containsGarbageCollectedRef(t): flags = flags or 1
  if not canFormAcycle(t) or (t.kind == tyProc and t.callConv == ccClosure):
    flags = flags or 2
  if t.kind == tyEnum:
    var hasHoles = false
    for i in 0..t.len-1:
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

  llvm.constNamedStruct(tnn,
    [g.constInt8(0), constNull(els[1]), constNull(els[2]), constNull(els[3]),
    g.constInt64(length), constNull(els[5])])

proc constNimNodeSlot(g: LLGen, offset, typeInfo: llvm.ValueRef, name: string): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  llvm.constNamedStruct(tnn,
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

  llvm.constNamedStruct(tnn,
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

  llvm.constNamedStruct(tnn,
    [g.constInt8(3), offset, typeInfo,
    g.b.buildGlobalStringPtr(name, ".nimnode.case." & name),
    g.constInt64(nodesLen), nodesVal])

proc genObjectNodeInfoInit(g: LLGen, t: PType, n: PNode, suffix: string): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    tnnp = tnn.pointerType()

  case n.kind
  of nkRecList:
    let l = n.len
    if l == 1:
      result = g.genObjectNodeInfoInit(t, n[0], suffix)
    else:
      var fields: seq[ValueRef] = @[]
      for i in 0..l-1:
        fields.add(g.genObjectNodeInfo(t, n[i], suffix & "." & $i))
      result = g.constNimNodeList(fields)

  of nkRecCase:
    let field = n[0].sym
    let l = g.config.lengthOrd(field.typ).toInt # TODO Int128

    var fields: seq[ValueRef]
    newSeq(fields, l + 1)

    for i in 1..<n.len:
      let b = n[i]
      let bi = g.genObjectNodeInfo(t, b.lastSon, suffix & "." & $i)
      case b.kind
      of nkOfBranch:
        for j in 0..b.len - 2:
          if b[j].kind == nkRange:
            # TODO Int128
            for a in getOrdValue(b[j][0]).toInt()..getOrdValue(b[j][1]).toInt():
              fields[a] = bi
          else:
            fields[getOrdValue(b[j]).toInt] = bi # TODO Int128
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

  let l = t.len
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

  g.constNimNodeList(fields)

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

  let l = t.n.len

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

  g.constNimNodeList(fields)

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
  g.constNimNodeNone(g.config.firstOrd(t).toInt) # TODO Int128

proc genSetNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & g.llName(t, sig)
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  g.nodeInfos[sig] = result
  result.setInitializer(g.genSetNodeInfoInit(t))

proc fakeClosureType(g: LLGen, owner: PSym): PType =
  # generate same rtti as c generator - why does it generate it this way??
  result = newType(tyTuple, owner)
  result.rawAddSon(newType(tyPointer, owner))
  var r = newType(tyRef, owner)
  let obj = createObj(g.graph, owner, owner.info, final=false)
  r.rawAddSon(obj)
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
      result = g.genTupleNodeInfo(g.fakeClosureType(t.owner))
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
  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyGenericParam,
      tyDistinct, tyOrdinal, tyTypeDesc, tyAlias, tySink, tyUserTypeClass,
      tyUserTypeClassInst, tyInferred, tyStatic:
    result = g.llProcParamType(t.lastSon)
  else: result = g.llType(t)

proc llPassAsPtr(g: LLGen, t: PType): bool =
  g.llType(t) != g.llProcParamType(t)

proc paramStorageLoc(param: PSym): TStorageLoc =
  if param.typ.skipTypes({tyVar, tyTypeDesc, tyLent}).kind notin {
          tyArray, tyOpenArray, tyUncheckedArray, tyVarargs}:
    result = OnStack
  else:
    result = OnUnknown

proc llProcType(g: LLGen, typ: PType, closure: bool): llvm.TypeRef =
  let retType = if typ.sons[0] == nil: llvm.voidType()
                else: g.llType(typ.sons[0])
  var argTypes = newSeq[llvm.TypeRef]()

  for param in typ.procParams():
    fillLoc(param.sym.loc, locParam, param, param.sym.name.s.mangle.rope,
      param.sym.paramStorageLoc)

    if g.llPassAsPtr(param.typ):
      incl(param.sym.loc.flags, lfIndirect)
      param.sym.loc.storage = OnUnknown

    let at = g.llProcParamType(param.typ)
    argTypes.add(at)

    if skipTypes(param.typ, {tyVar, tyLent}).kind in {tyOpenArray, tyVarargs}:
      if param.sym.typ.kind in {tyVar, tyLent}:
        param.sym.loc.storage = OnUnknown
      argTypes.add(g.primitives[tyInt])  # Extra length parameter

  if closure:
    argTypes.add(g.voidPtrType)

  llvm.functionType(retType, argTypes, tfVarArgs in typ.flags)

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
    for j in 1..<n.len:
      result = g.fieldIndexRecs(n[j].lastSon, sym, start)
      if result.len > 0: return
  of nkSym:
    if n.sym.id == sym.id: return @[start]
    let field = n.sym
    if not field.typ.isEmptyType(): inc(start)
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

proc rootIndex(g: LLGen, typ: PType): seq[llvm.ValueRef] =
  let zero = g.gep0
  result = @[zero]
  var t = skipTypes(typ, abstractInst)
  while t.kind in {tyVar, tyPtr, tyRef, tyLent}:
    result = result & @[zero]
    t = skipTypes(t.lastSon, typedescInst)

  while t.kind == tyObject and t.sons[0] != nil:
    result = result & @[zero]
    t = skipTypes(t.sons[0], abstractPtrs)

  if t.isObjLackingTypeField():
    # TODO why is this check in the generator???
    g.config.internalError("no 'of' operator available for pure objects")

proc mtypeIndex(g: LLGen, typ: PType): seq[llvm.ValueRef] =
  g.rootIndex(typ) & g.gep0

proc excNameIndex(g: LLGen, typ: PType): seq[llvm.ValueRef] =
  g.rootIndex(typ)[0..^2] & g.constGEPIdx(2)

proc setElemIndex(g: LLGen, typ: PType, x: llvm.ValueRef): llvm.ValueRef =
  if g.config.firstOrd(typ) != 0:
    # TODO Int128
    g.b.buildSub(
      x, constInt(x.typeOfX(), g.config.firstOrd(typ).toInt64().culonglong, llvm.False),
      g.nn("set.ord", x))
  else:
    x

proc isNimSeqLike(g: LLGen, t: llvm.TypeRef): bool =
  if t.getTypeKind() != llvm.PointerTypeKind: return

  let et = t.getElementType()
  if et.getTypeKind() != llvm.StructTypeKind: return

  if et.countStructElementTypes() != 2: return

  let elems = et.getStructElementTypes()
  if elems[0] != g.llGenericSeqType(): return

  if elems[1].getTypeKind() != llvm.PointerTypeKind and
     elems[1].getTypeKind() != llvm.ArrayTypeKind: return

  result = true

proc preCast(
    g: LLGen, unsigned: bool, ax: llvm.ValueRef, t: PType,
    lt: llvm.TypeRef = nil): llvm.ValueRef =
  if ax == nil: # tyTyped works like this...
    return constNull(g.llType(t))

  let
    at = ax.typeOfX()
    atk = at.getTypeKind()
    lt = if lt != nil: lt else: g.llType(t)
    ltk = lt.getTypeKind()

  if t.kind == tyCString and at != g.primitives[tyCString] and g.isNimSeqLike(at):
      result = g.getNimSeqDataPtr(ax)
      return

  if ltk == PointerTypeKind and
      skipTypes(t, {tyVar, tyLent}).kind in {tyVarargs, tyOpenArray, tyArray, tyUncheckedArray} and
      g.llType(t.elemType) == lt.getElementType() and
      g.isNimSeqLike(at):
    result = g.getNimSeqDataPtr(ax)
    return

  if ltk == PointerTypeKind and atk == PointerTypeKind:
    result = g.b.buildBitCast(ax, lt, g.nn("pre", ax))
    return

  if ltk == IntegerTypeKind and atk == IntegerTypeKind and
      at.getIntTypeWidth() != lt.getIntTypeWidth():
    result = g.buildTruncOrExt(ax, lt, unsigned)
    return
  result = ax

proc externGlobal(g: LLGen, s: PSym): LLValue =
  # Extra symbols that the compiler fetches from header files - we'll need
  # a better solution than hard-coding them, at some point
  # These are typically "nodecl", "importc" and/or "header" tagged in
  # the std library
  let
    name = s.llName
    t = g.llType(s.typ)

  case name
  of "errno": LLValue(v: g.callErrno(""))
  of "h_errno": LLValue(v: g.callErrno("h_"))
  else:
    if s.id in g.symbols:
      g.symbols[s.id]
    elif (let v = g.m.getNamedGlobal(name); v != nil):
      let tmp = LLValue(v: v, storage: s.loc.storage)
      g.symbols[s.id] = tmp
      tmp
    else:
      let tmp = LLValue(v: g.m.addGlobal(t, name))
      g.symbols[s.id] = tmp
      tmp

proc genLocal(g: LLGen, n: PNode): LLValue =
  let s = n.sym
  if s.loc.k == locNone:
    fillLoc(s.loc, locLocalVar, n, s.name.s.mangle.rope, OnStack)
    if s.kind == skLet: incl(s.loc.flags, lfNoDeepCopy)

  let
    t = g.llType(s.typ)
    v = g.localAlloca(t, s.llName)
  g.debugVariable(s, v)

  let lv = LLValue(v: v, lode: n, storage: s.loc.storage)
  g.symbols[s.id] = lv
  lv

proc genGlobal(g: LLGen, n: PNode, isConst: bool): LLValue =
  let s = n.sym
  if s.id in g.symbols:
    return g.symbols[s.id]

  if s.loc.k == locNone:
    fillLoc(
      s.loc, locGlobalVar, n, g.mangleName(s), if isConst: OnStatic else: OnHeap)

  # Couldn't find by id - should we get by name also? this seems to happen for
  # stderr for example which turns up with two different id:s.. what a shame!
  if (let v = g.m.getNamedGlobal(s.llName); v != nil):
    let tmp = LLValue(v: v, storage: s.loc.storage)
    g.symbols[s.id] = tmp
    return tmp

  let
    t = g.llType(s.typ.skipTypes(abstractInst))
    v = g.m.addGlobal(t, s.llName)

  if sfImportc in s.flags:
    v.setLinkage(llvm.ExternalLinkage)
  elif sfExportc in s.flags:
    v.setLinkage(g.tgtExportLinkage)
    v.setInitializer(llvm.constNull(t))
  else:
    v.setLinkage(llvm.PrivateLinkage)
    v.setInitializer(llvm.constNull(t))

  if sfThread in s.flags and optThreads in g.config.globalOptions:
    v.setThreadLocal(llvm.True)
  g.debugGlobal(s, v)

  if isConst:
    v.setGlobalConstant(llvm.True)

  result = LLValue(v: v, lode: n, storage: s.loc.storage)
  g.symbols[s.id] = result

proc getUnreachableBlock(g: LLGen): llvm.BasicBlockRef =
  if g.f.unreachableBlock == nil:
    g.f.unreachableBlock =
      g.b.appendBasicBlockInContext(
        g.lc, g.nn("unreachable", g.b.getInsertBlock().getBasicBlockParent()))

    g.withBlock(g.f.unreachableBlock):
      discard g.b.buildUnreachable()

  g.f.unreachableBlock

proc getDeadBlock(g: LLGen): llvm.BasicBlockRef =
  # Sometimes, there might be dead code after a return statement or a noreturn
  # call such as an exception being raised. We use a block with no predecessors
  # to collect such code and let it be optimized away..

  g.f.deadBlocks.add(
    g.b.getInsertBlock().getBasicBlockParent().appendBasicBlock(
      g.nn("dead", g.b.getInsertBlock().getLastInstruction()))
  )

  g.f.deadBlocks[^1]

proc callCompilerProc(
    g: LLGen, name: string, args: openarray[llvm.ValueRef], noInvoke = false,
    noReturn = false): llvm.ValueRef =
  let sym = g.graph.getCompilerProc(name)
  if sym == nil: g.config.internalError("compiler proc not found: " & name)

  let f = g.genFunctionWithBody(sym).v

  var i = 0
  var args = @args
  for param in sym.typ.procParams():
    let v = args[i]

    # TODO unsigned
    let a = g.preCast(false, v, param.typ, g.llProcParamType(param.typ))
    args[i] = a

    if skipTypes(param.typ, {tyVar, tyLent}).kind in {tyOpenArray, tyVarargs}:
      i += 1
    i += 1

  template callName(): untyped =
    if f.typeOfX().getElementType().getReturnType().getTypeKind() == llvm.VoidTypeKind: ""
    else: g.nn("call.cp." & name)

  if noInvoke:
    let ret = g.b.buildCall(f, args, callName())
    if noReturn:
      discard g.b.buildUnreachable()
      g.b.positionBuilderAtEnd(g.getDeadBlock())
    ret
  elif noReturn:
    let ret = g.buildCallOrInvokeBr(f, g.getUnreachableBlock(), args, callName())
    g.b.positionBuilderAtEnd(g.getDeadBlock())
    ret
  else:
    g.buildCallOrInvoke(f, args, callName())

proc callBSwap(g: LLGen, v: llvm.ValueRef, n: cuint): llvm.ValueRef =
  let it = llvm.intTypeInContext(g.lc, n)
  let fty = llvm.functionType(it, [it])

  let
    f = g.m.getOrInsertFunction("llvm.bswap.i" & $n, fty)

  result = g.b.buildCall(f, [v])

proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef) =
  let
    t = g.b.buildBitCast(tgt, g.voidPtrType, g.nn("memset.tgt", v))
    i8t = llvm.int8TypeInContext(g.lc)
    v8 = g.buildTruncOrExt(v, i8t, true)
  if len.typeOfX().getIntTypeWidth() == 64:
    let
      memsetType = llvm.functionType(llvm.voidTypeInContext(g.lc),
        [g.voidPtrType, i8t, llvm.int64TypeInContext(g.lc),
        llvm.int1TypeInContext(g.lc)])
      f = g.m.getOrInsertFunction("llvm.memset.p0i8.i64", memsetType)

    discard g.b.buildCall(f, [t, v8, len, g.constInt1(false)])
  else:
    let
      i32t = llvm.int32TypeInContext(g.lc)
      memsetType = llvm.functionType(llvm.voidTypeInContext(g.lc),
        [g.voidPtrType, llvm.int8TypeInContext(g.lc), i32t,
        llvm.int1TypeInContext(g.lc)])
      f = g.m.getOrInsertFunction("llvm.memset.p0i8.i32", memsetType)
      len32 = g.b.buildZExt(len, i32t, "")

    discard g.b.buildCall(f, [t, v8, len32, g.constInt1(false)])

proc callMemcpy(g: LLGen, tgt, src, len: llvm.ValueRef) =
  let
    t = g.b.buildBitCast(tgt, g.voidPtrType, g.nn("memcpy.tgt", tgt))
    s = g.b.buildBitCast(src, g.voidPtrType, g.nn("memcpy.src", src))

  if len.typeOfX().getIntTypeWidth() == 64:
    let
      memcpyType = llvm.functionType(llvm.voidTypeInContext(g.lc), [
        g.voidPtrType, g.voidPtrType, llvm.int64TypeInContext(g.lc),
        llvm.int1TypeInContext(g.lc)])
      f = g.m.getOrInsertFunction("llvm.memcpy.p0i8.p0i8.i64", memcpyType)

    discard g.b.buildCall(f, [t, s, len, g.constInt1(false)])
  else:
    let
      i32t = llvm.int32TypeInContext(g.lc)
      memcpyType = llvm.functionType(llvm.voidTypeInContext(g.lc), [
        g.voidPtrType, g.voidPtrType, i32t, llvm.int1TypeInContext(g.lc)])
      f = g.m.getOrInsertFunction("llvm.memcpy.p0i8.p0i8.i32", memcpyType)
      len32 = g.b.buildZExt(len, i32t, "")

    discard g.b.buildCall(f, [t, s, len32, g.constInt1(false)])

proc callCtpop(g: LLGen, v: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let
    bits = (size * 8).cuint
    t = llvm.functionType(llvm.intTypeInContext(g.lc, bits), [llvm.intTypeInContext(g.lc, bits)])
    f = g.m.getOrInsertFunction("llvm.ctpop.i" & $bits, t)

  g.b.buildCall(f, [v])

proc callErrno(g: LLGen, prefix: string): llvm.ValueRef =
  # on linux errno is a function, so we call it here. not at all portable.

  let
    errnoType = llvm.functionType(g.cintType.pointerType(), [])
    f = g.m.getOrInsertFunction("__" & prefix & "errno_location", errnoType)

  g.b.buildCall(f, [], g.nn(prefix & "errno"))

proc callWithOverflow(g: LLGen, op: string, a, b: llvm.ValueRef, name: string): llvm.ValueRef =
  let
    t = a.typeOfX()
    ft = llvm.functionType(llvm.structTypeInContext(
      g.lc, [t, int1TypeInContext(g.lc)]), [t, t])
    f = g.m.getOrInsertFunction(
      "llvm." & op & ".with.overflow.i" & $t.getIntTypeWidth(), ft)

  g.b.buildCall(f, [a, b], name)

proc callRaise(g: LLGen, cond: llvm.ValueRef, raiser: string, args: varargs[llvm.ValueRef]) =
  let
    raised = g.b.appendBasicBlockInContext(g.lc, g.nn("raise.call", cond))
    cont = g.b.appendBasicBlockInContext(g.lc, g.nn("raise.cont", cond))

  discard g.b.buildCondBr(cond, raised, cont)

  g.b.positionBuilderAtEnd(raised)
  discard g.callCompilerProc(raiser, args)
  discard g.b.buildUnreachable()
  g.b.positionBuilderAtEnd(cont)

proc callBinOpWithOver(
    g: LLGen, a, b: llvm.ValueRef, op: Opcode, typ: PType): llvm.ValueRef =
  # like tyChar, for example
  let
    u = g.isUnsigned(typ.skipTypes(abstractVar))

  proc doRangeCheck(v: llvm.ValueRef, typ: PType) =
    # TODO Int128
    let lt = g.b.buildICmp(
      llvm.IntSLT, v,
      constInt(v.typeOfX(), g.config.firstOrd(typ).toInt64.culonglong, llvm.False),
      g.nn("binop.over.rng.lt", a))
    let gt = g.b.buildICmp(
      llvm.IntSGT, v,
      constInt(v.typeOfX(), g.config.lastOrd(typ).toInt64.culonglong, llvm.False),
      g.nn("binop.over.rng.gt", a))

    let ltgt = g.b.buildOr(lt, gt, g.nn("binop.over.rng.or", a))
    g.callRaise(ltgt, "raiseOverflow")

  if op in [llvm.Add, llvm.Sub, llvm.Mul]:
    let opfn =
      case op
      of llvm.Add:
        if u: "uadd" else: "sadd"
      of llvm.Sub:
        if u: "usub" else: "ssub"
      of llvm.Mul:
        if u: "umul" else: "smul"
      else: g.config.internalError("bad overflow op"); ""

    let
      bo = g.callWithOverflow(opfn, a, b, g.nn("binop.over." & $op, a))
      isover = g.b.buildExtractValue(bo, 1, g.nn("binop.isover", a))

    g.callRaise(isover, "raiseOverflow")

    result = g.b.buildExtractValue(bo, 0, g.nn("binop.v", a))

    let
      rangeTyp = typ.skipTypes({tyGenericInst, tyAlias, tySink, tyVar, tyLent})
      rangeCheck = rangeTyp.kind in {tyRange, tyEnum}
    if rangeCheck: doRangeCheck(result, rangeTyp)
  else:
    let
      typ = typ.skipTypes(abstractVar)
      i64 = typ.kind == tyInt64
      ax =
        if i64: g.buildTruncOrExt(a, g.primitives[tyInt64], u)
        else: g.buildNimIntExt(a, u)
      bx =
        if i64: g.buildTruncOrExt(b, g.primitives[tyInt64], u)
        else: g.buildNimIntExt(b, u)
      opfn = case op
        of llvm.SDiv:
          if i64: "divInt64" else: "divInt"
        of llvm.SRem:
          if i64: "modInt64" else: "modInt"
        else: g.config.internalError("Unexpected op: " & $op); ""
      bo = g.callCompilerProc(opfn, [ax, bx])

    let rangeCheck = typ.kind in
      {tyRange, tyEnum, tyInt8, tyInt16, tyInt32, tyUInt8, tyUInt16, tyUint32}
    if rangeCheck: doRangeCheck(bo, typ)

    result =
      g.b.buildTrunc(bo, g.llType(typ), g.nn("binop.call.trunc", a))

proc callExpect(g: LLGen, v: llvm.ValueRef, expected: bool): llvm.ValueRef =
  let
    typ = llvm.functionType(g.primitives[tyBool], [
      g.primitives[tyBool], g.primitives[tyBool]])
    f = g.m.getOrInsertFunction("llvm.expect.i8", typ)

  g.b.buildCall(f, [v, g.constInt8(int8(ord expected))])

proc callEhTypeIdFor(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  let
    ft = llvm.functionType(g.primitives[tyInt32], [g.voidPtrType])
    f = g.m.getOrInsertFunction("llvm.eh.typeid.for", ft)

  g.b.buildCall(f, [v.constBitCast(g.voidPtrType)])

proc genObjectInit(g: LLGen, t: PType, v: llvm.ValueRef) =
  case analyseObjectWithTypeField(t)
  of frNone:
    discard
  of frHeader:
    let tgt = g.b.buildGEP(v, g.mtypeIndex(t), g.nn("mtype", v))
    discard g.b.buildStore(g.genTypeInfo(t), tgt)
  of frEmbedded:
    discard g.callCompilerProc("objectInit", [v, g.genTypeInfo(t)])

  if isException(t):
    let
      tgt = g.b.buildGEP(v, g.excNameIndex(t), g.nn("name", v))
      ename = t.skipTypes(abstractInst).sym.name.s
      init = g.lc.constStringInContext(ename)
      name = g.m.addPrivateConstant(
        init.typeOfX(), g.nn(".cstr", t.skipTypes(abstractInst).sym))
    name.setInitializer(init)

    discard g.b.buildStore(constBitCast(name, g.primitives[tyCString]), tgt)

proc constructLoc(g: LLGen, t: PType, v:llvm.ValueRef) =
  g.buildStoreNull(v)
  g.genObjectInit(t, v)

proc initLocalVar(
    g: LLGen, s: PSym, t: PType, v: llvm.ValueRef, immediateAssign: bool) =
  if sfNoInit notin s.flags:
    if not immediateAssign:
      g.constructLoc(t, v)

proc isAssignedImmediately(g: LLGen, n: PNode): bool {.inline.} =
  if n.kind == nkEmpty: false
  # TODO RVO and return-by-pointer
  else: true

template withRangeItems(il: untyped, n: PNode, body: untyped) =
  let
    ax = g.genNode(s[0], true).v
    bx = g.genNode(s[1], true).v
    b = g.setElemIndex(typ, bx)

  # loop! init idx
  let i = g.localAlloca(ax.typeOfX(), g.nn("rng.i", n))
  discard g.b.buildStore(ax, i)

  let
    rcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("rng.cmp", n))
    rloop = g.b.appendBasicBlockInContext(g.lc, g.nn("rng.loop", n))
    rdone = g.b.appendBasicBlockInContext(g.lc, g.nn("rng.done", n))

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
  if s.name.s == "rawProc" and s.typ.sons.len == 2 and
      s.typ.sons[1].kind == tyProc:

    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let gep = g.b.buildGEP(f.getParam(0), [g.gep0, g.gep0])
      let p = g.b.buildLoad(gep, "ClP_0")
      let ax = g.b.buildBitCast(p, g.voidPtrType, "ClP_0_vp")
      discard g.b.buildRet(ax)
    return true

  if s.name.s == "rawEnv" and s.typ.sons.len == 2 and
    s.typ.sons[1].kind == tyProc:

    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let gep = g.b.buildGEP(f.getParam(0), [g.gep0, g.gep1])
      let p = g.b.buildLoad(gep, "ClE_0")
      let ax = g.b.buildBitCast(p, g.voidPtrType, "ClE_0_vp")
      discard g.b.buildRet(ax)
    return true

  if s.name.s == "finished" and s.typ.sons.len == 2 and
      s.typ.sons[1].kind == tyProc:

    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let gep = g.b.buildGEP(f.getParam(0), [g.gep0, g.gep1])
      let p = g.b.buildLoad(gep, "")
      let ax = g.b.buildBitCast(p, g.primitives[tyInt].pointerType(), "")
      let a = g.b.buildLoad(g.b.buildGEP(ax, [g.gep1]), "")
      let cmp = g.buildI8(g.b.buildICmp(llvm.IntSLT, a, g.ni0, ""))
      discard g.b.buildRet(cmp)
    return true

  if (s.name.s in
      ["addInt", "subInt", "mulInt", "addInt64", "subInt64", "mulInt64"]) and
      s.typ.sons.len == 3 and
      s.typ[0] != nil and
      s.typ.sons[0].kind == s.typ.sons[1].kind and
      s.typ.sons[1].kind == s.typ.sons[2].kind:
    # prefer intrinsic for these...
    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let
        a = f.getParam(0)
        b = f.getParam(1)
        res = case s.name.s
          of "addInt", "addInt64":
            g.callBinOpWithOver(a, b, llvm.Add, s.typ.sons[0])
          of "subInt", "subInt64":
            g.callBinOpWithOver(a, b, llvm.Sub, s.typ.sons[0])
          of "mulInt", "mulInt64":
            g.callBinOpWithOver(a, b, llvm.Mul, s.typ.sons[0])
          else: g.config.internalError("checked above!"); nil
      discard g.b.buildRet(res)
    return true

  if s.name.s == "cpuRelax":
    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      discard g.b.buildRetVoid()
    return true

proc isComplexValueType(t: PType): bool {.inline.} =
  let t = t.skipTypes(abstractInst + tyUserTypeClasses)
  result = t.kind in {tyArray, tySet, tyTuple, tyObject} or
    (t.kind == tyProc and t.callConv == ccClosure)

proc resetLoc(g: LLGen, typ: PType, v: LLValue) =
  let containsGcRef = containsGarbageCollectedRef(typ)
  let typ = skipTypes(typ, abstractVarRange)
  if not isComplexValueType(typ):
    if containsGcRef:
      g.genRefAssign(v, LLValue(v: constNull(v.v.typeOfX()), storage: OnStack))
    else:
      g.buildStoreNull(v.v)
  else:
    if optNilCheck in g.f.options:
      discard g.callCompilerProc("chckNil", [v.v])

    if v.storage != OnStack and containsGcRef:
      discard g.callCompilerProc("genericReset", [v.v, g.genTypeInfo(typ)])
      # XXX: generated reset procs should not touch the m_type
      # field, so disabling this should be safe:
      g.genObjectInit(typ, v.v)
    else:
      g.buildStoreNull(v.v)
      g.genObjectInit(typ, v.v)

proc genFunction(g: LLGen, s: PSym): LLValue =
  if s.id in g.symbols: return g.symbols[s.id]

  fillLoc(s.loc, locProc, s.ast[namePos], g.mangleName(s), OnStack)

  let name = s.llName

  var s = s
  # Some compiler proc's have two syms essentially, because of an importc trick
  # in system.nim...
  if sfImportc in s.flags:
    result = LLValue(v: g.m.getNamedFunction(name))
    if result.v != nil:
      g.symbols[s.id] = result
      return

  let typ = s.typ.skipTypes(abstractInst)

  let ft = g.llProcType(typ, typ.callConv == ccClosure)
  let f = g.m.addFunction(name, ft)

  if sfNoReturn in s.flags:
    f.addFuncAttribute(g.attrNoReturn)

  if typ.callConv == ccNoInline:
    f.addFuncAttribute(g.attrNoInline)

  # This attribute hopefully works around
  # https://github.com/nim-lang/Nim/issues/10625
  f.addFuncAttribute(g.attrNoOmitFP)

  if s.name.s in [
      "sysFatal", "raiseOverFlow", "raiseIndexError", "raiseIndexError2",
      "raiseIndexError3", "raiseFieldError"]:
    f.addFuncAttribute(g.attrCold)

  if g.genFakeImpl(s, f):
    f.setLinkage(llvm.InternalLinkage)

  result = LLValue(v: f, storage: s.loc.storage)
  g.symbols[s.id] = result

proc genFunctionWithBody(g: LLGen, s: PSym): LLValue =
  var s = s
  if lfImportCompilerProc in s.loc.flags:
    s = magicsys.getCompilerProc(g.graph, s.name.s)

  result = g.genFunction(s)

  if result.v.countBasicBlocks() != 0:
    return  # already has body

  if sfForward in s.flags:
    g.forwardedProcs.add(s)
    return

  if sfImportc in s.flags: return

  if sfExportc notin s.flags or sfCompilerProc in s.flags:
    # Because we generate only one module, we can tag all functions internal,
    # except those that should be importable from c
    # compilerproc are marker exportc to get a stable name, but it doesn't seem
    # they need to be exported to C - this might change if we start supporting
    # dll:s
    result.v.setLinkage(llvm.InternalLinkage)

  let
    typ = s.typ.skipTypes(abstractInst)
    f = g.newLLFunc(result.v, s)

  g.withFunc(f):
    var ret: llvm.ValueRef

    discard g.f.startBlock(s.ast, g.section(f, secReturn))

    g.f.options = s.options

    if g.d != nil:
      let arr = g.debugProcType(typ, typ.callConv == ccClosure)
      g.f.ds = g.debugFunction(s, arr, result.v)
    g.debugUpdateLoc(s.ast)

    g.withBlock(g.section(f, secArgs)):
      var i = 0

      # Function arguments
      for param in typ.procParams():
        p("a", param, g.depth + 1)
        p("a", param.typ, g.depth + 2)

        let arg = result.v.getParam(i.cuint)
        arg.setValueName(param.sym.llName)

        let av = g.localAlloca(arg.typeOfX(), g.nn("arg", arg))
        discard g.b.buildStore(arg, av)

        g.debugVariable(param.sym, av, i + 1)

        g.symbols[param.sym.id] =
          LLValue(v: av, lode: param, storage: param.sym.loc.storage)

        i += 1

        if skipTypes(param.typ, {tyVar, tyLent}).kind in {tyOpenArray, tyVarargs}:
          let argLen = result.v.getParam(i.cuint)
          argLen.setValueName(param.sym.llName & "len")

          let avLen = g.localAlloca(argLen.typeOfX(), g.nn("argLen", argLen))
          discard g.b.buildStore(argLen, avLen)
          g.symbols[-param.sym.id] =
            LLValue(v: avLen, lode: param, storage: OnStack)
          i += 1

      if sfPure notin s.flags and typ.sons[0] != nil:
        let resNode = s.ast[resultPos]
        let res = resNode.sym
        if sfNoInit in s.flags: incl(res.flags, sfNoInit)
        let resv = g.genLocal(resNode)
        ret = resv.v
        g.initLocalVar(res, res.typ, ret, false)

      if tfCapturesEnv in typ.flags:
        let arg = result.v.getParam(i.cuint)
        arg.setValueName("ClE_0")
        g.f.clenv = arg

        let ls = lastSon(s.ast[paramsPos])
        let lt = g.llType(ls.sym.typ)
        if g.f.clenv == nil: g.config.internalError(s.ast.info, "env missing")
        let lx = g.b.buildBitCast(g.f.clenv, lt, g.nn("ClEnvX"))
        let av = g.localAlloca(lx.typeOfX(), g.nn("ClEnvX.a"))
        discard g.b.buildStore(lx, av)
        g.symbols[ls.sym.id] = LLValue(v: av)

        g.debugVariable(ls.sym, av, i + 1)
        i += 1

    g.withBlock(g.section(g.f, secBody)):
      let procBody = transformBody(g.graph, s, cache = false)
      g.genNode(procBody)
      g.b.buildBrFallthrough(g.section(g.f, secReturn))

      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      if ret != nil:
        discard g.b.buildRet(g.b.buildLoad(ret, g.nn("load.result")))
      else:
        discard g.b.buildRetVoid()

    g.finalize()

proc genFakeCall(g: LLGen, n: PNode, o: var LLValue): bool =
  let nf = n[0]
  if nf.kind != nkSym: return false

  let s = nf.sym
  if s.originatingModule().name.s == "system":
    if s.name.s == "atomicLoad":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], false).v
      let ld = g.b.buildLoad(p0, g.nn("a.load"))
      ld.setOrdering(llvm.AtomicOrderingSequentiallyConsistent)
      ld.setAlignment(1) # TODO
      discard g.b.buildStore(ld, p1)
      return true

    if s.name.s == "atomicLoadN":
      let p0 = g.genNode(n[1], false).v
      o = LLValue(v: g.b.buildLoad(p0, g.nn("a.load.n")))
      o.v.setAlignment(1) # TODO
      o.v.setOrdering(llvm.AtomicOrderingSequentiallyConsistent)
      return true

    if s.name.s == "atomicStore":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], false).v
      let ld = g.b.buildLoad(p1, g.nn("a.load"))
      let ax = g.b.buildStore(ld, p0)
      ax.setOrdering(llvm.AtomicOrderingSequentiallyConsistent)
      ax.setAlignment(1.cuint)  # TODO(j) align all over the place.
      return true

    if s.name.s == "atomicStoreN":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], true).v
      let ax = g.b.buildStore(p1, p0)
      ax.setOrdering(llvm.AtomicOrderingSequentiallyConsistent)
      ax.setAlignment(1.cuint)  # TODO(j) align all over the place.
      return true

    if s.name.s == "atomicAddFetch":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], true).v
      o = LLValue(v: g.b.buildAtomicRMW(
        llvm.AtomicRMWBinOpAdd, p0, p1,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False))
      return true

    if s.name.s == "atomicSubFetch":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], true).v
      o = LLValue(v: g.b.buildAtomicRMW(
        llvm.AtomicRMWBinOpSub, p0, p1,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False))
      return true

    if s.name.s == "atomicThreadFence":
      o = LLValue(v: g.b.buildFence(
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False, ""))
      return true

    if s.name.s == "cas":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], true).v
      let p2 = g.genNode(n[3], true).v
      let x = g.b.buildAtomicCmpXchg(p0, p1, p2,
        llvm.AtomicOrderingSequentiallyConsistent,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False)
      o = LLValue(v: g.buildI8(
        g.b.buildExtractValue(x, 1.cuint, g.nn("cas.b", n))))
      return true

    if s.name.s == "atomicCompareExchange":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], false).v
      let p2 = g.genNode(n[3], false).v
      let l1 = g.b.buildLoad(p1, g.nn("ace.1"))
      let l2 = g.b.buildLoad(p2, g.nn("ace.2"))
      let x = g.b.buildAtomicCmpXchg(p0, l1, l2,
        llvm.AtomicOrderingSequentiallyConsistent,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False)
      o = LLValue(v: g.buildI8(
        g.b.buildExtractValue(x, 1.cuint, g.nn("cas.b", n))))
      return true

    if s.name.s in ["likelyProc", "unlikelyProc"]:
      let tmp = g.genNode(n[1], true)
      o = LLValue(v: g.callExpect(tmp.v, s.name.s == "likelyProc"))
      return true

    if s.name.s == "getCurrentException":
      o = LLValue(
        v: g.callCompilerProc("nlvmGetCurrentException", [], noInvoke=true))
      return true

    if s.name.s == "closureIterSetupExc":
      let p0 = g.genNode(n[1], true).v
      o = LLValue(
        v: g.callCompilerProc("nlvmSetClosureException", [p0], noInvoke=true))
      return true

  elif s.originatingModule.name.s == "memory":
    if s.name.s == "nimCopyMem":
      let p0 = g.genNode(n[1], true).v
      let p1 = g.genNode(n[2], true).v
      let p2 = g.genNode(n[3], true).v

      g.callMemcpy(p0, p1, p2)
      return true

    if s.name.s == "nimSetMem":
      let p0 = g.genNode(n[1], true).v
      let p1 = g.genNode(n[2], true).v
      let p2 = g.genNode(n[3], true).v

      g.callMemset(p0, p1, p2)
      return true

    if s.name.s == "nimZeroMem":
      let p0 = g.genNode(n[1], true).v
      let p2 = g.genNode(n[2], true).v

      g.callMemset(p0, g.constInt8(0), p2)
      return true

  else:
    case $s.loc.r
    of "__builtin_bswap16":
      o = LLValue(v: g.callBSwap(g.genNode(n[1], true).v, 16))
      return true
    of "__builtin_bswap32":
      o = LLValue(v: g.callBSwap(g.genNode(n[1], true).v, 32))
      return true
    of "__builtin_bswap64":
      o = LLValue(v: g.callBSwap(g.genNode(n[1], true).v, 64))
      return true

proc genCallArgs(g: LLGen, n: PNode, fxt: llvm.TypeRef, ftyp: PType): seq[llvm.ValueRef] =
  var args: seq[ValueRef] = @[]

  let parTypes = fxt.getParamTypes()
  for i in 1..<n.len:
    let p = n[i]
    let pr = (if p.kind == nkHiddenAddr: p[0] else: p)

    if i >= ftyp.n.len: # varargs like printf, for example
      let v = g.genNode(pr, true).v
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

    var q = skipConv(pr)
    var skipped = false
    while q.kind == nkStmtListExpr and q.len > 0:
      skipped = true
      q = q.lastSon

    if skipTypes(param.typ, abstractVar).kind in {tyOpenArray, tyVarargs}:
      var len: llvm.ValueRef
      if q.getMagic() == mSlice:
        if skipped:
          q = skipConv(pr)
          while q.kind == nkStmtListExpr and q.len > 0:
            for i in 0..q.len-2:
              discard g.genNode(q[i])
            q = q.lastSon

        let
          bx = g.genNode(q[2], true).v
          cx = g.genNode(q[3], true).v

        let ty = q[1].typ.skipTypes(abstractVar + {tyPtr})
        case ty.kind
        of tyArray:
          let
            ax = g.genNode(q[1], false)
            # TODO Int128
            first = constInt(
              bx.typeOfX(), g.config.firstOrd(ty).toInt64.culonglong, llvm.True)
            idx = g.b.buildSub(bx, first, g.nn("slice.sub", bx))
          if optBoundsCheck in g.f.options:
            g.genBoundsCheck(ty, ax.v, bx, cx)
          v = g.b.buildGEP(ax, [g.gep0, idx], g.nn("arg" & $i, q)).v

        of tyOpenArray, tyVarargs:
          let
            ax = g.genNode(q[1], false)
          v = g.b.buildGEP(ax, [bx], g.nn("arg" & $i, q)).v

        of tyCString:
          let
            ax = g.genNode(q[1], true)
          v = g.b.buildGEP(ax, [bx], g.nn("arg" & $i, q)).v

        of tyUncheckedArray:
          let
            ax = g.genNode(q[1], true)
          v = g.b.buildGEP(ax, [g.gep0, bx], g.nn("arg" & $i, q)).v

        of tyString, tySequence:
          let
            ax = g.genNode(q[1], true)
          let a =
            if q.typ.skipTypes(abstractInst).kind in {tyVar, tyLent}:
              g.buildLoadValue(ax)
            else: ax
          if optBoundsCheck in g.f.options:
            g.genBoundsCheck(ty, a.v, bx, cx)
          v = g.getNimSeqDataPtr(a.v, bx)
        else: g.config.internalError(n.info, "unknown slice type " & $ty.kind)

        len = g.b.buildSub(cx, bx, g.nn("slice.sub", n))
        len = g.b.buildAdd(len, constInt(len.typeOfX(), 1, llvm.False), g.nn("slice.add", n))
      else:
        case pr.typ.skipTypes(abstractVar).kind
        of tyString, tySequence:
          v = g.genNode(pr, true).v
          if pr.typ.skipTypes(abstractInst).kind in {tyVar, tyLent}:
            v = g.b.buildLoad(v, g.nn("call.seq.var", n))
          len = g.loadNimSeqLen(v)
          len = g.b.buildZExt(len, g.primitives[tyInt], g.nn("call.seq.len.ext", n))
          v = g.getNimSeqDataPtr(v)
        of tyOpenArray, tyVarargs:
          v = g.genNode(p, param.typ.kind in {tyVar, tyLent}).v
          len = g.b.buildLoad(g.symbols[-q.sym.id].v, g.nn("call.seq.oa.len", n))
        of tyArray, tyUncheckedArray:
          v = g.genNode(p, true).v
          len = g.constNimInt(g.config.lengthOrd(pr.typ))
        of tyPtr, tyRef:
          case pr.typ.lastSon().kind
          of tyString, tySequence:
            v = g.genNode(pr, true).v
            v = g.b.buildLoad(v, g.nn("call.seq.load", n))
            len = g.loadNimSeqLen(v)
            len = g.b.buildZExt(len, g.primitives[tyInt], g.nn("call.seq.len.ext", n))
            v = g.getNimSeqDataPtr(v)
          else:
            g.config.internalError(n.info, "Unhandled ref length: " & $pr.typ)
        else:
          g.config.internalError(n.info, "Unhandled length: " & $pr.typ)

      if v.typeOfX() != g.llProcParamType(param.typ):
        v = g.b.buildBitCast(v, g.llProcParamType(param.typ), g.nn("call.open", v))

      args.add(v)
      args.add(len)
    else:
      v = g.genNode(p, not g.llPassAsPtr(param.typ)).v

      # We need to use the type from the function, because with multimethods,
      # it looks like the type in param.typ changes during compilation!
      # seen with tmultim1.nim
      v = g.preCast(g.isUnsigned(p.deepTyp), v, param.typ, pt)
      args.add(v)

  result = args

proc genCall(g: LLGen, n: PNode, load: bool): LLValue =
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
    let
      args = g.genCallArgs(n, nfpt, typ)
      prc = g.b.buildExtractValue(fx, 0, g.nn("call.clo.prc.ptr", n))
      env = g.b.buildExtractValue(fx, 1, g.nn("call.clo.env.ptr", n))

    if tfIterator in typ.flags:
      let
        cft = g.llProcType(typ, true).pointerType()
        cfx = g.b.buildBitCast(prc.v, cft, g.nn("call.iter.prc", n))
        clargs = args & @[env.v]
      callres = g.buildCallOrInvoke(cfx, clargs)
    else:
      let
        clonil = g.b.appendBasicBlockInContext(g.lc, g.nn("call.clo.noenv", n))
        cloenv = g.b.appendBasicBlockInContext(g.lc, g.nn("call.clo.env", n))
        cloend = g.b.appendBasicBlockInContext(g.lc, g.nn("call.clo.end", n))
        cmp = g.b.buildICmp(
         llvm.IntEQ, env.v, llvm.constNull(env.v.typeOfX()),
         g.nn("call.clo.noenv", n))

      discard g.b.buildCondBr(cmp, clonil, cloenv)

      g.b.positionBuilderAtEnd(clonil)

      let
        fxc = g.b.buildBitCast(prc.v, nft, g.nn("call.clo.prc.noenv", n))
        res = g.buildCallOrInvokeBr(fxc, cloend, args)

      g.b.positionBuilderAtEnd(cloenv)

      let
        cft = g.llProcType(typ, true).pointerType()
        cfx = g.b.buildBitCast(prc.v, cft, g.nn("call.clo.prc", n))
        clargs = args & @[env.v]
        cres = g.buildCallOrInvokeBr(cfx, cloend, clargs)

      g.b.positionBuilderAtEnd(cloend)

      if retty.getTypeKind() != llvm.VoidTypeKind:
        callres = g.b.buildPHI(res.typeOfX(), g.nn("call.clo.res", n))
        callres.addIncoming([res, cres], [clonil, cloenv])
  else:
    let fxc =
      if fx.v.typeOfX().getElementType().getTypeKind() != llvm.FunctionTypeKind:
        g.b.buildBitCast(fx.v, nft, g.nn("call.fx", n))
      else:
        fx.v

    let
      args = g.genCallArgs(n, fxc.typeOfX().getElementType(), typ)
      varname =
        if retty.getTypeKind() != llvm.VoidTypeKind: g.nn("call.res", n) else: ""
    callres = g.buildCallOrInvoke(fxc, args, varname)

  if (retty.getTypeKind() != llvm.VoidTypeKind and not load and
      nf.typ.sons[0].kind != tyRef):
    # if the originator of the call wants a pointer, we'll have
    # to create one for them - this is interesting for example
    # when a struct is returned "by value"
    let v = g.localAlloca(retty, g.nn("call.res.ptr", n))
    discard g.b.buildStore(callres, v)
    LLValue(v: v)
  elif (retty.getTypeKind() == llvm.ArrayTypeKind):
    let v = g.localAlloca(retty, g.nn("call.res.ptr", n))
    discard g.b.buildStore(callres, v)
    LLValue(v: v)
  else:
    LLValue(v: callres)

proc genMagicCall(g: LLGen, n: PNode, load: bool): LLValue =
  let s = n[namePos].sym
  if lfNoDecl notin s.loc.flags:
    let m = g.graph.getCompilerProc($s.loc.r)
    if m == nil: g.config.internalError(n.info, "Missing magic: " & $s.loc.r)
    discard g.genFunctionWithBody(m)

  g.genCall(n, load)

proc genRefAssign(g: LLGen, dest, src: LLValue) =
  if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
      not usesWriteBarrier(g.config):
    discard g.b.buildStore(g.b.buildBitCast(
      src.v, dest.v.typeOfX().getElementType(), g.nn("bc", dest.v)), dest.v)
  elif dest.storage == OnHeap:
    discard g.callCompilerProc("asgnRef", [dest.v, src.v])
  else:
    discard g.callCompilerProc("unsureAsgnRef", [dest.v, src.v])

proc callGenericAssign(
    g: LLGen, dest, src: LLValue, ty: PType, flags: TAssignmentFlags) =
  if needToCopy notin flags or
      tfShallow in ty.skipTypes(abstractVarRange).flags:
    if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
        not usesWriteBarrier(g.config):
      g.callMemcpy(dest.v, src.v, dest.v.typeOfX.getElementType().sizeOfX())
    else:
      discard g.callCompilerProc(
        "genericShallowAssign", [dest.v, src.v, g.genTypeInfo(ty)])
  else:
    discard g.callCompilerProc(
      "genericAssign", [dest.v, src.v, g.genTypeInfo(ty)])

proc genAsgnFromRef(g: LLGen, dest, src: LLValue, typ: PType,
    flags: TAssignmentFlags) =
  let destt = dest.v.typeOfX()

  if destt.getTypeKind() != llvm.PointerTypeKind:
    g.config.internalError("Ptr required in genAssignment: " & $dest)

  let
    destet = destt.getElementType()
    ty = typ.skipTypes(abstractRange)

  case ty.kind
  of tyRef:
    let srcl = g.buildLoadValue(src)
    g.genRefAssign(dest, srcl)
  of tySequence:
    let srcl = g.buildLoadValue(src)
    if (needToCopy notin flags and src.storage != OnStatic) or
        canMove(g, src.lode):
      g.genRefAssign(dest, srcl)
    else:
      discard g.callCompilerProc(
        "genericSeqAssign", [dest.v, srcl.v, g.genTypeInfo(ty)])
  of tyString:
    let srcl = g.buildLoadValue(src)
    if (needToCopy notin flags and src.storage != OnStatic) or
        canMove(g, src.lode):
      g.genRefAssign(dest, srcl)
    else:
      if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
          not usesWriteBarrier(g.config):
        let srcc = g.callCompilerProc("copyString", [srcl.v])
        discard g.b.buildStore(
          g.b.buildBitCast(srcc, destet, g.nn("asgn.xc", src)), dest.v)
      elif dest.storage == OnHeap:
        let destl = g.b.buildLoad(dest.v, g.nn("asgn.old", dest))
        let srcc = g.callCompilerProc("copyStringRC1", [srcl.v])
        discard g.b.buildStore(
          g.b.buildBitCast(srcc, destet, g.nn("asgn.xc", src)), dest.v)
        g.withNotNil(destl):
          discard g.callCompilerProc("nimGCunrefNoCycle", [destl])
      else:
        let srcc = g.callCompilerProc("copyString", [srcl.v])
        discard g.callCompilerProc("unsureAsgnRef", [dest.v, srcc])
  of tyProc:
    let srcl = g.buildLoadValue(src)

    if ty.containsGarbageCollectedRef():
      let destp = g.b.buildGEP(dest, [g.gep0, g.gep0], g.nn("asgn.dest.p", dest))
      let srcp = g.b.buildExtractValue(srcl, 0, g.nn("asgn.p", src))
      discard g.b.buildStore(
        g.b.buildBitCast(
          srcp.v, destp.v.typeOfX().getElementType(), g.nn("asgn.pc", src)), destp.v)

      let deste = g.b.buildGEP(dest, [g.gep0, g.gep1], "asgn.dest.e")
      let srce = g.b.buildExtractValue(srcl, 1, g.nn("asgn.e", src))

      g.genRefAssign(deste, srce)
    else:
      discard g.b.buildStore(
        g.b.buildBitCast(srcl.v, destet, g.nn("asgn.xc")), dest.v)
  of tyTuple:
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(dest, src, ty, flags)
    else:
      g.callMemcpy(dest.v, src.v, destet.sizeOfX())

  of tyObject:
    if not ty.isObjLackingTypeField() or ty.containsGarbageCollectedRef():
      g.callGenericAssign(dest, src, ty, flags)
    else:
      g.callMemcpy(dest.v, src.v, destet.sizeOfX())

  of tyArray:
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(dest, src, ty, flags)
    else:
      g.callMemcpy(dest.v, src.v, destet.sizeOfX())

  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      let srcl = g.buildLoadValue(src)
      discard g.b.buildStore(srcl.v, dest.v)
    else:
      g.callMemcpy(dest.v, src.v, destet.sizeOfX())
  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar, tyLent:
    let srcl = g.buildLoadValue(src)
    discard g.b.buildStore(
      g.b.buildBitCast(srcl.v, destet, g.nn("asgn.c", src)), dest.v)

  else: g.config.internalError("genAssignment: " & $ty.kind)

proc genAssignment(
    g: LLGen, dest: LLValue, src: PNode, typ: PType,
    flags: TAssignmentFlags) =

  # This can happen in constants like array[4, xx] which only have a few for the
  # first elements set - see tseq_badinit
  if src.kind == nkEmpty:
    # weird AST's come out of macros sometimes..
    return

  let destt = dest.v.typeOfX()

  if destt.getTypeKind() != llvm.PointerTypeKind:
    g.config.internalError(src.info, "Ptr required in genAssignment: " & $dest.v)

  let destet = destt.getElementType()

  let ty = typ.skipTypes(abstractRange + tyUserTypeClasses + {tyStatic})
  case ty.kind
  of tyRef:
    let srcx = g.genNode(src, true)
    if srcx.v != nil:
      g.genRefAssign(dest, srcx)

  of tySequence:
    let srcx = g.genNode(src, true)
    if (needToCopy notin flags and srcx.storage != OnStatic) or
        canMove(g, srcx.lode):
      g.genRefAssign(dest, srcx)
    else:
      discard g.callCompilerProc(
        "genericSeqAssign", [dest.v, srcx.v, g.genTypeInfo(ty)])

  of tyString:
    let srcx = g.genNode(src, true)
    if (needToCopy notin flags and srcx.storage != OnStatic) or
        canMove(g, srcx.lode):
      g.genRefAssign(dest, srcx)
    else:
      if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
          not usesWriteBarrier(g.config):
        let srcc = g.callCompilerProc("copyString", [srcx.v])
        discard g.b.buildStore(
          g.b.buildBitCast(srcc, destet, g.nn("asgn.xc", src)), dest.v)
      elif dest.storage == OnHeap:
        let destl = g.b.buildLoad(dest.v, g.nn("asgn.old", dest))
        let srcc = g.callCompilerProc("copyStringRC1", [srcx.v])
        discard g.b.buildStore(
          g.b.buildBitCast(srcc, destet, g.nn("asgn.xc", src)), dest.v)
        g.withNotNil(destl):
          discard g.callCompilerProc("nimGCunrefNoCycle", [destl])
      else:
        let srcc = g.callCompilerProc("copyString", [srcx.v])
        discard g.callCompilerProc("unsureAsgnRef", [dest.v, srcc])

  of tyProc:
    let srcx = g.genNode(src, true)
    if ty.containsGarbageCollectedRef():
      let destp =
        g.b.buildGEP(dest, [g.gep0, g.gep0], g.nn("asgn.dest.p", dest))
      let srcp = g.b.buildExtractValue(srcx, 0, g.nn("asgn.p", src))
      discard g.b.buildStore(
        g.b.buildBitCast(
          srcp.v, destp.v.typeOfX().getElementType(), g.nn("asgn.pc", srcx)),
          destp.v)

      let deste = g.b.buildGEP(dest, [g.gep0, g.gep1], g.nn("asgn.dest.e"))
      let srce = g.b.buildExtractValue(srcx, 1, g.nn("asgn.e", src))

      g.genRefAssign(deste, srce)
    else:
      discard g.b.buildStore(
        g.b.buildBitCast(srcx.v, destet, g.nn("asgn.xc")), dest.v)

  of tyTuple:
    let srcx = g.genNode(src, false)
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(dest, srcx, ty, flags)
    else:
      g.callMemcpy(dest.v, srcx.v, destet.sizeOfX())

  of tyObject:
    let srcx = g.genNode(src, false)
    if not ty.isObjLackingTypeField() or ty.containsGarbageCollectedRef():
      g.callGenericAssign(dest, srcx, ty, flags)
    else:
      g.callMemcpy(dest.v, srcx.v, destet.sizeOfX())

  of tyArray:
    let srcx = g.genNode(src, false)
    if ty.containsGarbageCollectedRef():
      g.callGenericAssign(dest, srcx, ty, flags)
    else:
      g.callMemcpy(dest.v, srcx.v, destet.sizeOfX())

  of tyOpenArray, tyVarargs:
    let s = if src.kind == nkHiddenDeref: src[0] else: src
    let p = g.b.buildLoad(g.symbols[s.sym.id].v, "asgn.oa.p")
    let len = g.b.buildLoad(g.symbols[-s.sym.id].v, "asgn.oa.l")
    if ty.containsGarbageCollectedRef():
      discard g.callCompilerProc(
        "genericAssignOpenArray", [dest.v, p, len, g.genTypeInfo(ty)])
    else:
      g.callMemcpy(dest.v, p, destet.sizeOfX())

  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      let srcx = g.genNode(src, true)
      discard g.b.buildStore(srcx.v, dest.v)
    else:
      let srcx = g.genNode(src, false)
      g.callMemcpy(dest.v, srcx.v, destet.sizeOfX())

  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar, tyLent:
    let srcx = g.genNode(src, true)
    let pc = g.preCast(g.isUnsigned(typ), srcx.v, typ, destet)

    discard g.b.buildStore(pc, dest.v)

  of tyNil:
    discard g.b.buildStore(constNull(destet), dest.v)

  else:
    g.config.internalError(src.info, "genAssignment: " & $ty.kind)

proc isSeqLike(n: PNode): bool =
  case n.kind
  of nkStrLit..nkTripleStrLit: true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    n[1].isSeqLike()
  else:
    n.typ.kind == tySequence

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef

proc genConstSeq(g: LLGen, n: PNode): llvm.ValueRef =
  if n.typ.elemType.kind == tyEmpty or n.len == 0:
    llvm.constNull(g.llType(n.typ))
  else:
    let
      et = g.llType(n.typ.elemType)

    var vals = newSeq[llvm.ValueRef](n.len)
    for i, s in n.sons:
      vals[i] =
        if s.isSeqLike():
          # Need pointer for string literals
          g.genNode(s, true).v
        else:
          g.genConstInitializer(s)

    let s = constArray(et, vals)
    if n.typ.kind in {tyArray, tyUncheckedArray}:
      s
    else:
      let
        ll = g.constNimInt(vals.len)
        cap = g.constNimInt(vals.len + g.strLitFlag)
        x = llvm.constNamedStruct(g.llGenericSeqType(), [ll, cap])

      llvm.constStructInContext(g.lc, [x, s])

proc genConstObjConstr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)

  if typ.kind == tyRef: g.config.internalError(n.info, "no const objs with refs")

  var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())
  for i in 0..<vals.len:
    vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))

  for i in 1 ..< n.len:
    let
      s = n[i]
      ind = g.fieldIndex(typ, s[0].sym)
    if ind.len != 1: g.config.internalError(s.info, "unexpected field depth")

    if s.isSeqLike():
      # Need pointer for string literals
      vals[ind[0]] = g.genNode(s, true).v
    else:
      vals[ind[0]] = g.genConstInitializer(s)

  constNamedStruct(t, vals)

proc genConstTupleConstr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)

  var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())
  for i in 0..<vals.len:
    vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))

  for i in 0 ..< n.len:
    let s = n[i]
    if s.isSeqLike():
      # Need pointer for string literals
      vals[i] = g.genNode(s, true).v
    else:
      vals[i] = g.genConstInitializer(s)

  constNamedStruct(t, vals)

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef =
  case n.kind
  of nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = g.genConstInitializer(n[1])
  of nkCurly: result = g.constNimSet(n)
  of nkBracket: result = g.genConstSeq(n)
  of nkObjConstr: result = g.genConstObjConstr(n)
  of nkTupleConstr, nkPar, nkClosure: result = g.genConstTupleConstr(n)
  of nkCharLit..nkFloat128Lit, nkNilLit: result = g.genNode(n, true).v
  of nkStrLit..nkTripleStrLit: result = g.constNimString(n)
  of nkEmpty: result = constNull(g.llType(n.typ))
  else: g.config.internalError(n.info, "Can't gen const initializer " & $n.kind)

proc genConst(g: LLGen, n: PNode): LLValue =
  let sym = n.sym
  if sym.id in g.symbols: return g.symbols[sym.id]

  let init = sym.ast

  if init.isDeepConstExprLL():
    result = g.genGlobal(n, true)

    let ci = g.genConstInitializer(init)
    if ci == nil:
      g.config.internalError(n.info, "Unable to generate const initializer: " & $init)

    # TODO when enabled, ptr equality checks (for example in isObj) get
    #      optimized away - need to consider when it's safe
    # result.setUnnamedAddr(llvm.True)

    case sym.typ.skipTypes(abstractVarRange).kind
    of tyString, tySequence:
      let c = g.m.addPrivateConstant(ci.typeOfX(), g.nn(".const.init", n))
      c.setInitializer(ci)

      result.v.setInitializer(
        llvm.constBitCast(c, result.v.typeOfX().getElementType()))
    else:
      result.v.setInitializer(ci)
    return

  if sfGlobal in sym.flags:
    result = g.genGlobal(n, false)
    g.registerGcRoot(sym, result.v)
  else:
    if sym.id in g.symbols: return g.symbols[sym.id]

    result = LLValue(
      v: g.localAlloca(g.llType(sym.typ), sym.llName), storage: OnStack)
    # Some initializers expect value to be null, so we always set it so
    g.buildStoreNull(result.v)
    g.genObjectInit(sym.typ, result.v)

  if init.kind != nkEmpty:
    g.genAssignment(result, init, sym.typ, sym.assignCopy)

  if sfGlobal notin sym.flags:
    g.symbols[sym.id] = result

proc genMagicHigh(g: LLGen, n: PNode): LLValue =
  let len = g.genMagicLength(n).v
  LLValue(v: g.b.buildSub(
    len, llvm.constInt(len.typeOfX(), 1, llvm.False), g.nn("high", len)))

proc genMagicSizeOf(g: LLGen, n: PNode): LLValue =
  let t = n[1].typ.skipTypes({tyTypeDesc})

  LLValue(v: llvm.sizeOfX(g.llType(t)))

proc genMagicOf(g: LLGen, n: PNode): LLValue =
  var ax = g.genNode(n[1], false)

  # C generator does this extra dereferencing here - looks odd that it's needed
  # but there are test case fails because of it - needs more investigation
  var t = skipTypes(n[1].typ, abstractInst)
  while t.kind in {tyVar, tyLent, tyPtr, tyRef}:
    t = skipTypes(t.lastSon, typedescInst)

  while ax.v.typeOfX().getElementType().getTypeKind() == llvm.PointerTypeKind:
    ax = g.buildLoadValue(ax)

  let
    mt_gep = g.b.buildGEP(ax, g.mtypeIndex(t), g.nn("mtype", ax))
    m_type = g.buildLoadValue(mt_gep)
    typ = n[2].typ.skipTypes(typedescPtrs)
  # TODO nil check if n is a pointer
  LLValue(v: g.callCompilerProc("isObj", [m_type.v, g.genTypeInfo(typ)]))

proc genMagicEcho(g: LLGen, n: PNode) =
  let b = n[1].skipConv

  if b.len == 0:
    let t = g.llStringType().pointerType()
    discard g.callCompilerProc("echoBinSafe", [constNull(t), g.constNimInt(0)])
  else:
    let x = g.genNode(b, true).v
    let y = g.constNimInt(b.len)
    discard g.callCompilerProc("echoBinSafe", [x, y])

proc genMagicUnaryLt(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = llvm.constInt(ax.typeOfX(), 1, False)
  LLValue(v:
    if optOverflowCheck in g.f.options:
      g.callBinOpWithOver(ax, bx, llvm.Sub, n.typ)
    else:
      g.b.buildSub(ax, bx, g.nn("lt", n)))

proc genMagicIncDec(g: LLGen, n: PNode, op: Opcode) =
  let
    ax = g.genNode(n[1], false).v
    bx = g.genNode(n[2], true).v
    a = g.b.buildLoad(ax, g.nn("inc.a", n))
    b = g.buildTruncOrExt(bx, a.typeOfX(), n[2].typ)

  let t = n[1].typ.skipTypes({tyGenericInst, tyAlias, tySink, tyVar, tyLent, tyRange})
  if optOverflowCheck notin g.f.options or t.kind in {tyUInt..tyUInt64}:
    let nv = g.b.buildBinOp(op, a, b, g.nn("inc.nv", n))
    discard g.b.buildStore(nv, ax)
  else:
    let nv = g.callBinOpWithOver(a, b, op, n[1].typ)
    discard g.b.buildStore(nv, ax)

proc genMagicOrd(g: LLGen, n: PNode): LLValue =
  var ax = g.genNode(n[1], true).v

  if n[1].typ.skipTypes(abstractRange).kind == tyBool:
    # make sure we don't get more bits set than necessary
    ax = g.b.buildAnd(ax, llvm.constInt(ax.typeOfX(), 1.cuint, llvm.False),
      g.nn("ord.bool", n))
  LLValue(v: g.buildTruncOrExt(ax, g.llType(n.typ), n[1].typ))

proc rawGenNew(g: LLGen, dest: LLValue, typ: PType) =
  let
    refType = typ.skipTypes(abstractInst)
    bt = refType.lastSon
    lbt = g.llType(bt)
    sizeExpr = lbt.sizeOfX()
    ti = g.genTypeInfo(typ)

  assert refType.kind == tyRef

  if dest.storage == OnHeap and usesWriteBarrier(g.config):
    let destl = g.buildLoadValue(dest)
    g.withNotNil(destl.v):
      if canFormAcycle(typ):
        discard g.callCompilerProc("nimGCunrefRC1", [destl.v])
      else:
        discard g.callCompilerProc("nimGCunrefNoCycle", [destl.v])
      discard g.b.buildStore(constNull(destl.v.typeOfX()), dest.v)

    if g.config.selectedGC == gcGo:
      # newObjRC1() would clash with unsureAsgnRef() - which is used by gcGo to
      # implement the write barrier
      let src = g.callCompilerProc("newObj", [ti, sizeExpr])
      discard g.callCompilerProc("unsureAsgnRef", [dest.v, src])
    else:
      # use newObjRC1 as an optimization
      let src = g.callCompilerProc("newObjRC1", [ti, sizeExpr])
      discard g.b.buildStore(
        g.b.buildBitCast(
          src, dest.v.typeOfX.getElementType(), g.nn("new", src)), dest.v)
  else:
    let src = g.callCompilerProc("newObj", [ti, sizeExpr])
    g.genRefAssign(dest, LLValue(v: src, storage: OnHeap))

  g.genObjectInit(bt, g.b.buildLoad(dest.v, g.nn("new.load")))

proc genMagicNew(g: LLGen, n: PNode) =
  if n.len == 3: g.config.internalError(n.info, "unsafeNew unimplemented")
  let ax = g.genNode(n[1], false)

  g.rawGenNew(ax, n[1].typ)

proc genMagicNewFinalize(g: LLGen, n: PNode) =
  let
    dest = g.genNode(n[1], false)
    refType = n[1].typ.skipTypes(abstractVarRange)
    f = g.genNode(n[2], false).v
    ti = g.genTypeInfo(refType)
    bt = refType.lastSon.skipTypes(abstractRange)

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
    src = LLValue(v: g.callCompilerProc(
      "newObj", [ti, g.llType(bt).sizeOfX()]),
      storage: OnHeap)
  g.genRefAssign(dest, src)
  g.genObjectInit(bt, g.b.buildLoad(dest.v, g.nn("new.load")))

proc genNewSeqAux(g: LLGen, dest: LLValue, destTyp: PType, len: llvm.ValueRef) =
  let
    seqType = destTyp.skipTypes(abstractVarRange)
    ti = g.genTypeInfo(seqType)
    args = [ti, len]

  if dest.storage == OnHeap and usesWriteBarrier(g.config):
    let destl = g.buildLoadValue(dest)
    g.withNotNil(destl.v):
      if canFormAcycle(destTyp):
        discard g.callCompilerProc("nimGCunrefRC1", [destl.v])
      else:
        discard g.callCompilerProc("nimGCunrefNoCycle", [destl.v])
      discard g.b.buildStore(constNull(destl.v.typeOfX()), dest.v)

    if not len.isZero():
      if g.config.selectedGC == gcGo:
        # we need the write barrier
        let src = g.callCompilerProc("newSeq", args)
        discard g.callCompilerProc("unsureAsgnRef", [dest.v, src])
      else:
        let src = g.callCompilerProc("newSeqRC1", args)
        discard g.b.buildStore(
          g.b.buildBitCast(src, g.llType(seqType), g.nn("newseq", src)), dest.v)
  else:
    let src =
      if len.isZero():
        llvm.constNull(g.llType(seqType))
      else:
        let v = g.callCompilerProc("newSeq", args)
        g.b.buildBitCast(v, g.llType(seqType), g.nn("newseq", v))
    g.genRefAssign(dest, LLValue(v: src, storage: OnHeap))

proc genMagicNewSeq(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true).v

  g.genNewSeqAux(ax, n[1].typ, bx)

proc genMagicNewSeqOfCap(g: LLGen, n: PNode): LLValue =
  let seqtype = n.typ.skipTypes(abstractVarRange)
  let ax = g.genNode(n[1], true).v
  let ti =  g.genTypeInfo(seqtype)

  LLValue(v: g.callCompilerProc("nimNewSeqOfCap", [ti, ax]), storage: OnHeap)

proc lenOpenArray(g: LLGen, s: PNode): llvm.ValueRef =
  if s.kind in nkCallKinds and s[0].kind == nkSym and
    s[0].sym.magic == mSlice:
    # len(toOpenArray(...))
    let
      bx = g.genNode(s[2], true).v
      cx = g.genNode(s[3], true).v

    g.b.buildAdd(constInt(bx.typeOfX(), 1, llvm.False),
      g.b.buildSub(cx, bx, g.nn("oa.sub", s)), g.nn("oa.add", s))
  else:
    g.buildLoadValue(g.symbols[-s.sym.id]).v

proc genMagicLengthOpenArray(g: LLGen, n: PNode): LLValue =
  # openarray must be a parameter so we should find it in the scope
  let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]

  LLValue(v: g.lenOpenArray(s))

proc genMagicLengthStr(g: LLGen, n: PNode): LLValue =
  let
    v = g.genNode(n[1], true).v
    pre = g.b.getInsertBlock()
    lload = g.b.appendBasicBlockInContext(g.lc, g.nn("str.len.load", n))
    ldone = g.b.appendBasicBlockInContext(g.lc, g.nn("str.len.done", n))

  # nil check
  let cond = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("str.len.isnil", n))

  discard g.b.buildCondBr(cond, ldone, lload)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(lload)
  let
    strlenType = llvm.functionType(g.csizetType, [g.primitives[tyCString]])
    strlen = g.m.getOrInsertFunction("strlen", strlenType)

  let v1 = g.buildTruncOrExt(
    g.b.buildCall(strlen, [v]), g.primitives[tyInt], false)
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(g.primitives[tyInt], g.nn("str.len", n))

  let v0 = g.ni0
  phi.addIncoming([v0, v1], [pre, lload])

  LLValue(v: phi)

proc genMagicLengthSeq(g: LLGen, n: PNode): LLValue =
  let v = g.genNode(n[1], true).v

  LLValue(v: g.loadNimSeqLen(v))

proc genMagicLength(g: LLGen, n: PNode): LLValue =
  let typ = skipTypes(n[1].typ, abstractVar + tyUserTypeClasses)
  case typ.kind
  of tyOpenArray, tyVarargs: g.genMagicLengthOpenArray(n)
  of tyCString: g.genMagicLengthStr(n)
  of tySequence, tyString: g.genMagicLengthSeq(n)
  of tyArray: LLValue(v: g.constNimInt(g.config.lengthOrd(typ)))
  else:
    g.config.internalError(n.info, "genMagicLength " & $n[1].typ)
    LLValue()

proc genMagicXLen(g: LLGen, n: PNode): LLValue =
  let v = g.genNode(n[1], true).v

  # load length if v is not nil
  let gep = g.buildNimSeqLenGEP(v)
  LLValue(v: g.b.buildLoad(gep, g.nn("seq.len.load", n)))

proc genMagicIncl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false).v
    bx = g.genNode(n[2], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    let b = g.buildSetMask(
      ax.typeOfX().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildOr(g.b.buildLoad(ax, ""), b, "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genNode(n[1], false).v
      bx = g.genNode(n[2], true).v

    let (gep, mask) = g.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildOr(a, mask, "")
    discard g.b.buildStore(res, gep)

proc genMagicExcl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false).v
    bx = g.genNode(n[2], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    let b = g.buildSetMask(
      ax.typeOfX().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildAnd(g.b.buildLoad(ax, ""), g.buildBitnot(b), "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genNode(n[1], false).v
      bx = g.genNode(n[2], true).v

    let (gep, mask) = g.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildAnd(a, g.buildBitnot(mask), "")
    discard g.b.buildStore(res, gep)

proc genMagicCard(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    let ctp = g.callCtpop(ax, size)
    LLValue(v: g.buildTruncOrExt(ctp, g.llType(n.typ), true))
  else:
    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("card.i", n))
    discard g.b.buildStore(g.ni0, i)

    let tot = g.localAlloca(g.llType(n.typ), g.nn("card.tot", n))
    g.buildStoreNull(tot)

    let
      rcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("card.cmp", n))
      rloop = g.b.appendBasicBlockInContext(g.lc, g.nn("card.loop", n))
      rdone = g.b.appendBasicBlockInContext(g.lc, g.nn("card.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let
      il = g.b.buildLoad(i, g.nn("card.il", n))
      cond = g.b.buildICmp(
        llvm.IntSLT, il, g.constNimInt(size.int), g.nn("card.slt", n))

    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let
      ai = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("card.ax", n))
      a = g.callCtpop(ai, 1)
      b = g.b.buildLoad(tot, g.nn("card.tot.load", n))
      c = g.b.buildAdd(
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

    LLValue(v: g.b.buildLoad(tot, g.nn("card", n)))

proc genMagicChr(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v

  let t = g.llType(n.typ)
  LLValue(v:
    if t != ax.typeOfX(): g.b.buildTrunc(ax, t, g.nn("chr.t", n))
    else: ax
  )

proc genMagicGCref(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], false).v
  g.withNotNil(ax):
    discard g.callCompilerProc("nimGCref", [ax])

proc genMagicGCunref(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], false).v
  g.withNotNil(ax):
    discard g.callCompilerProc("nimGCunref", [ax])

proc genMagicBinOp(g: LLGen, n: PNode, op: Opcode): LLValue =
  var
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  if ax.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():

    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176

    if ax.typeOfX().getIntTypeWidth() > bx.typeOfX().getIntTypeWidth():
      bx = g.buildTruncOrExt(bx, ax.typeOfX(), g.isUnsigned(n[2].typ))
    else:
      ax = g.buildTruncOrExt(ax, bx.typeOfX(), g.isUnsigned(n[1].typ))

  let bo = g.b.buildBinOp(op, ax, bx, g.nn("binop." & $op, n))
  LLValue(v: g.b.buildTrunc(bo, g.llType(n.typ), g.nn("binop.trunc", n)))

proc genMagicBinOpOverflow(g: LLGen, n: PNode, op: Opcode): LLValue =
  if optOverflowCheck notin g.f.options:
    return g.genMagicBinOp(n, op)

  var
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  if ax.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():

    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    if ax.typeOfX().getIntTypeWidth() > bx.typeOfX().getIntTypeWidth():
      bx = g.buildTruncOrExt(bx, ax.typeOfX(), g.isUnsigned(n[2].typ))
    else:
      ax = g.buildTruncOrExt(ax, bx.typeOfX(), g.isUnsigned(n[1].typ))

  let bo = g.callBinOpWithOver(ax, bx, op, n.typ)
  LLValue(v: g.b.buildTrunc(bo, g.llType(n.typ), g.nn("binop.over.trunc", n)))

proc genMagicBinOpF(g: LLGen, n: PNode, op: Opcode, unsigned = false): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  let v = g.b.buildBinOp(op, ax, bx, g.nn("binop." & $op, n))

  if optNaNCheck in g.f.options:
    discard g.callCompilerProc("nanCheck", [v])
  if optInfCheck in g.f.options:
    discard g.callCompilerProc("infCheck", [v])

  LLValue(v: v)

proc genMagicShr(g: LLGen, n: PNode): LLValue =
  var
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  if ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():
    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    bx = g.buildTruncOrExt(bx, ax.typeOfX(), true)

  LLValue(v: g.b.buildBinOp(llvm.LShr, ax, bx, g.nn("binop." & $llvm.LShr, n)))

proc genMagicAShr(g: LLGen, n: PNode): LLValue =
  var
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  if ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():
    # This seems to happen with unsigned ints for example, see
    # https://github.com/nim-lang/Nim/issues/4176
    bx = g.buildTruncOrExt(bx, ax.typeOfX(), true)

  LLValue(v: g.b.buildBinOp(llvm.AShr, ax, bx, g.nn("binop." & $llvm.AShr, n)))

proc genMagicMinMaxI(g: LLGen, n: PNode, op: IntPredicate): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v
    cmp = g.b.buildICmp(op, ax, bx, g.nn($op & ".cmp", n))

  LLValue(v: g.b.buildSelect(cmp, ax, bx, g.nn($op, n)))

proc genMagicCmpI(g: LLGen, n: PNode, op: IntPredicate): LLValue =
  var
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  if ax.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind and
      ax.typeOfX().getIntTypeWidth() != bx.typeOfX().getIntTypeWidth():
    # TODO should probably extend to the biggest of the two, rather than
    # full int
    ax = g.buildNimIntExt(ax, g.isUnsigned(n[1].typ))
    bx = g.buildNimIntExt(bx, g.isUnsigned(n[2].typ))

  # unsigned doesn't matter here - we've already normalized ints
  LLValue(v: g.buildI8(g.b.buildICmp(
    op, g.preCast(false, ax, n[1].typ), g.preCast(false, bx, n[1].typ),
    g.nn("icmp." & $op, n))))

proc genMagicCmpF(g: LLGen, n: PNode, op: RealPredicate): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  LLValue(v: g.buildI8(g.b.buildFCmp(op, ax, bx, g.nn($op, n))))

proc genMagicDotDot(g: LLGen, n: PNode, load: bool): LLValue
proc genMagicEqCString(g: LLGen, n: PNode): LLValue =
  g.genMagicDotDot(n, true)

proc genMagicEqProc(g: LLGen, n: PNode): LLValue =
  let v =
    if n[1].typ.skipTypes(abstractInst).callConv == ccClosure:
      let
        ax = g.genNode(n[1], false).v
        bx = g.genNode(n[2], false).v
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

      g.b.buildAnd(x0, x1, g.nn("eq.prc", n))
    else:
      let
        ax = g.genNode(n[1], true).v
        bx = g.genNode(n[2], true).v

      g.b.buildICmp(llvm.IntEQ, ax, bx, g.nn("eq.prc", n))
  LLValue(v: g.buildI8(v))

proc genMagicUnaryMinus(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = llvm.constInt(ax.typeOfX(), 0, llvm.False)

  LLValue(v:
    if optOverflowCheck in g.f.options:
      g.callBinOpWithOver(bx, ax, llvm.Sub, n.typ)
    else:
      g.b.buildSub(bx, ax, g.nn("neg", ax)))

proc genMagicAbsI(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = llvm.constInt(ax.typeOfX(), 0, llvm.False)

  let
    pre = g.b.getInsertBlock()
    linv = g.b.appendBasicBlockInContext(g.lc, g.nn("abs.inv", n))
    ldone = g.b.appendBasicBlockInContext(g.lc, g.nn("abs.done", n))

  # >=0 check
  let cmp = g.b.buildICmp(llvm.IntSGE, ax, bx, g.nn("abs.sge", n))

  discard g.b.buildCondBr(cmp, ldone, linv)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(linv)

  let
    v1 =
      if optOverflowCheck in g.f.options:
        g.callBinOpWithOver(bx, ax, llvm.Sub, n.typ)
      else: g.b.buildSub(bx, ax, g.nn("abs.neg", n))
    v1src = g.b.getInsertBlock() # callBinOpWithOver will change insert block

  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(ax.typeOfX(), g.nn("abs", n))

  phi.addIncoming([ax, v1], [pre, v1src])

  LLValue(v: phi)

proc genMagicNot(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v
  LLValue(v: g.buildNot(ax))

proc genMagicBitnot(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v
  LLValue(v: g.buildBitnot(ax))

proc genMagicUnaryMinusF64(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v
  LLValue(v: g.b.buildFSub(
    llvm.constReal(ax.typeOfX(), 0.0.cdouble), ax, g.nn("neg", ax)))

proc genMagicToStr(g: LLGen, n: PNode, f: string): LLValue =
  let a = g.genNode(n[1], true).v
  LLValue(v: g.callCompilerProc(f, [a]))

proc genMagicStrToStr(g: LLGen, n: PNode): LLValue =
  g.genNode(n[1], true)

proc genMagicEnumToStr(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v

  let typ = skipTypes(n[1].typ, abstractVarRange)
  LLValue(v:
    g.callCompilerProc(
      "reprEnum", [g.buildTruncOrExt(ax, g.primitives[tyInt], typ),
        g.genTypeInfo(typ)]))

proc genMagicAndOr(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v
  let a = g.buildI1(ax)

  let
    pre = g.b.getInsertBlock()
    aonext = g.b.appendBasicBlockInContext(g.lc, g.nn("andor.next", n))
    aoend = g.b.appendBasicBlockInContext(g.lc, g.nn("andor.end", n))

  if n[0].sym.magic == mAnd:
    discard g.b.buildCondBr(a, aonext, aoend)
  else:
    discard g.b.buildCondBr(a, aoend, aonext)

  g.b.positionBuilderAtEnd(aonext)

  let bx = g.genNode(n[2], true).v
  let bend = g.b.getInsertBlock()
  discard g.b.buildBr(aoend)

  g.b.positionAndMoveToEnd(aoend)

  let phi = g.b.buildPHI(ax.typeOfX(), g.nn("and.res", n))

  phi.addIncoming([ax, bx], [pre, bend])

  LLValue(v:  phi)

proc genMagicEqStr(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  LLValue(v: g.callCompilerProc("eqStrings", [ax, bx]))

proc genMagicLeStr(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v
    cmp = g.callCompilerProc("cmpStrings", [ax, bx])

  LLValue(v: g.buildI8(g.b.buildICmp(llvm.IntSLE, cmp, g.ni0, g.nn("le.str", n))))

proc genMagicLtStr(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v
    cmp = g.callCompilerProc("cmpStrings", [ax, bx])

  LLValue(v: g.buildI8(g.b.buildICmp(llvm.IntSLT, cmp, g.ni0, g.nn("lt.str", n))))

proc genMagicSetCmp(g: LLGen, strict: bool, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    let b = g.buildBitnot(bx)
    let ab = g.b.buildAnd(ax, b, g.nn("setcmp.ab"))
    let le = g.b.buildICmp(
      llvm.IntEQ, ab, llvm.constInt(ab.typeOfX(), 0, llvm.False), g.nn("setcmp.le", n))
    let v =
      if strict:
        let ne = g.b.buildICmp(llvm.IntNE, ax, bx, g.nn("setcmp.ne", n))
        g.b.buildAnd(le, ne, g.nn("setcmp.res", n))
      else:
        le
    LLValue(v: g.buildI8(v))
  else:
    let o = g.localAlloca(g.primitives[tyBool], g.nn("setcmp.o", n))
    discard g.b.buildStore(g.nb[true], o)

    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("set.i", n))
    discard g.b.buildStore(g.ni0, i)

    let
      rcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("setcmp.cmp", n))
      rloop = g.b.appendBasicBlockInContext(g.lc, g.nn("setcmp.loop", n))
      rinc = g.b.appendBasicBlockInContext(g.lc, g.nn("setcmp.inc", n))
      rfalse = g.b.appendBasicBlockInContext(g.lc, g.nn("setcmp.false", n))
      rdone = g.b.appendBasicBlockInContext(g.lc, g.nn("setcmp.done", n))

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

    let
      al = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("setcmp.al", n))
      bl = g.b.buildLoad(g.b.buildGEP(bx, [il]), g.nn("setcmp.bl", n))
      b = g.buildBitnot(bl)

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

    LLValue(v: g.buildLoadValue(o))

proc genMagicEqSet(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)

  if size <= 8:
    LLValue(v: g.buildI8(g.b.buildICmp(llvm.IntEQ, ax, bx, g.nn("seteq.eq", n))))
  else:
    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("seteq.i", n))
    discard g.b.buildStore(g.ni0, i)

    let
      rcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("seteq.cmp", n))
      rloop = g.b.appendBasicBlockInContext(g.lc, g.nn("seteq.loop", n))
      rne = g.b.appendBasicBlockInContext(g.lc, g.nn("seteq.ne", n))
      rinc = g.b.appendBasicBlockInContext(g.lc, g.nn("seteq.inc", n))
      rdone = g.b.appendBasicBlockInContext(g.lc, g.nn("seteq.done", n))

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

    let
      a = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("seteq.a", n))
      b = g.b.buildLoad(g.b.buildGEP(bx, [il]), g.nn("seteq.b", n))
      cmp = g.b.buildICmp(llvm.IntEQ, a, b, g.nn("seteq.cmp", n))
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

    let phi = g.b.buildPHI(g.primitives[tyBool], g.nn("set.eq", n))
    phi.addIncoming([g.nb[false], g.nb[true]], [rne, rcmp])
    LLValue(v: phi)

proc genMagicSetBinOp(g: LLGen, op: llvm.Opcode, invert: bool, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v
    typ = skipTypes(n[1].typ, abstractVar)

  let size = g.config.getSize(typ)
  if size <= 8:
    let
      a = ax
      b = bx
    let s = if invert: g.buildBitnot(b)
            else: b
    LLValue(v: g.b.buildBinOp(op, a, s, g.nn("setbo.res")))
  else:
    let tmp = g.localAlloca(g.llType(typ), g.nn("setbo.tmp", n))

    # loop! init idx
    let i = g.localAlloca(g.primitives[tyInt], g.nn("setbo.i", n))
    discard g.b.buildStore(g.ni0, i)

    let
      rcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("setbo.cmp", n))
      rloop = g.b.appendBasicBlockInContext(g.lc, g.nn("setbo.loop", n))
      rdone = g.b.appendBasicBlockInContext(g.lc, g.nn("setbo.done", n))

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, g.nn("setbo.il", n))
    let cond = g.b.buildICmp(llvm.IntSLT, il, g.constNimInt(size.int), g.nn("setbo.cond", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let
      a = g.b.buildLoad(g.b.buildGEP(ax, [il]), g.nn("setbo.al", n))
      b = g.b.buildLoad(g.b.buildGEP(bx, [il]), g.nn("setbo.bl", n))
      s = if invert: g.buildBitnot(b) else: b
      o = g.b.buildBinOp(op, a, s, g.nn("setbo.op", n))
      p = g.b.buildGEP(tmp, [g.gep0, il])

    discard g.b.buildStore(o, p)

    # inc idx
    let next = g.b.buildAdd(il, constInt(il.typeOfX(), 1, llvm.False), g.nn("setbo.next", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    LLValue(v: g.buildLoadValue(tmp))

proc genMagicConStrStr(g: LLGen, n: PNode): LLValue =
  var tgtlen = g.ni0

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]

  for s in n.sons[1..<n.len]:
    let sx = g.genNode(s, true).v
    exprs.add(sx)

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit..nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.loadNimSeqLen(sx)
      tgtlen = g.b.buildAdd(tgtlen, slen, g.nn("constrstr.tgtlen", n))

  if constlen > 0:
    tgtlen = g.b.buildAdd(tgtlen, g.constNimInt(constlen), g.nn("constrstr.tgtlen", n))

  # Allocate result
  let tgt = g.callCompilerProc("rawNewString", [tgtlen])

  # Copy data
  for i in 1..<n.len:
    let sx = exprs[i - 1]

    let s = n[i]

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [tgt, sx])
    else:
      discard g.callCompilerProc("appendString", [tgt, sx])

  LLValue(v: tgt, storage: OnHeap)

proc genMagicDotDot(g: LLGen, n: PNode, load: bool): LLValue =
  # a compiler magic with implementation in system.nim.. hm.
  let s = n[namePos].sym
  let f = g.genFunctionWithBody(s).v
  # will be generated in multiple modules, so hide it..
  f.setLinkage(llvm.PrivateLinkage)

  result = g.genCall(n, load)

proc genMagicAppendStrCh(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true).v
    a = g.buildLoadValue(ax)
    ret = LLValue(v: g.callCompilerProc("addChar", [a.v, bx]), storage: OnHeap)

  g.genRefAssign(ax, ret)

proc genMagicAppendStrStr(g: LLGen, n: PNode) =
  let tgtp = g.genNode(n[1], false)
  var tgt = g.buildLoadValue(tgtp)

  # First, find out total length of the new strings
  var tgtlen = g.ni0

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]
  for i in 2..<n.len:
    let s = n[i]
    let sx = g.genNode(s, true).v
    exprs.add(sx)

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit..nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.loadNimSeqLen(sx)
      tgtlen = g.b.buildAdd(tgtlen, slen, g.nn("str.adds.tot." & $i, n))

  if constlen > 0:
    tgtlen = g.b.buildAdd(
      tgtlen, g.constNimInt(constlen), g.nn("str.adds.tot.const", n))

  # Make room
  let newstr = LLValue(
    v: g.callCompilerProc("resizeString", [tgt.v, tgtlen]), storage: OnHeap)
  g.genRefAssign(tgtp, newstr)

  # Copy data
  for i in 2..<n.len:
    let sx = exprs[i - 2]

    let s = n[i]
    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [newstr.v, sx])
    else:
      discard g.callCompilerProc("appendString", [newstr.v, sx])

proc genMagicAppendSeqElem(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    seqType = n[1].typ.skipTypes({tyVar, tyLent})
    a = g.buildLoadValue(ax)
    ap = g.b.buildGEP(a, [g.gep0, g.gep0], g.nn("seq.add.gep", n))
    newseq = g.callCompilerProc("incrSeqV3", [ap.v, g.genTypeInfo(seqType)])
    tgt = LLValue(
      v: g.b.buildBitCast(newseq, a.v.typeOfX(), g.nn("seq.add.new", n)),
      storage: OnHeap)
    lenp = g.buildNimSeqLenGEP(tgt.v) # guaranteed not nil!
    len = g.b.buildLoad(lenp, g.nn("load.seq.add.last", n))

  g.genRefAssign(ax, tgt)
  g.genAssignment(g.buildNimSeqDataGEP(tgt, len), n[2], seqType.elemType,
    {needToCopy})

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
    result = s.len <= 8  # 8 seems to be a good value

proc genMagicInSet(g: LLGen, n: PNode): LLValue =
  if (n[1].kind == nkCurly) and g.fewCmps(n[1]):
    let s = if n[2].kind in {nkChckRange, nkChckRange64}: n[2][0]
            else: n[2]
    let sx = g.genNode(s, true).v

    let res = g.localAlloca(g.primitives[tyBool], g.nn("inset.res", n))
    discard g.b.buildStore(g.nb[false], res)

    let strue = g.b.appendBasicBlockInContext(g.lc, g.nn("inset.true", n))

    let length = n[1].len
    let u = g.isUnsigned(s.typ)
    for i in 0..<length:
      var cmp: llvm.ValueRef
      if n[1][i].kind == nkRange:
        let
          ax = g.genNode(n[1][i][0], true).v
          bx = g.genNode(n[1][i][1], true).v
          acmp = g.b.buildICmp(
            if u: llvm.IntUGE else: llvm.IntSGE, sx,
            g.buildTruncOrExt(ax, sx.typeOfX(), u), g.nn("inset.acmp", n))
          bcmp = g.b.buildICmp(
            if u: llvm.IntULE else: llvm.IntSLE, sx,
            g.buildTruncOrExt(bx, sx.typeOfX(), u), g.nn("inset.bcmp", n))

        cmp = g.b.buildAnd(acmp, bcmp, g.nn("inset.and", n))
      else:
        let ax = g.genNode(n[1][i], true).v
        cmp = g.b.buildICmp(llvm.IntEQ, sx, g.buildTruncOrExt(ax, sx.typeOfX(), u), "")

      let snext = g.b.appendBasicBlockInContext(g.lc, g.nn("inset.cmp", n))
      discard g.b.buildCondBr(cmp, strue, snext)
      g.b.positionBuilderAtEnd(snext)

    let send = g.b.appendBasicBlockInContext(g.lc, g.nn("inset.end", n))
    discard g.b.buildBr(send)

    g.b.positionBuilderAtEnd(strue)
    discard g.b.buildStore(g.nb[true], res)
    discard g.b.buildBr(send)

    g.b.positionAndMoveToEnd(send)
    return LLValue(v: g.b.buildLoad(res, g.nn("inset.result", n)))

  let typ = skipTypes(n[1].typ, abstractVar)
  let size = g.config.getSize(typ)

  let v =
    if size <= 8:
      let
        ax = g.genNode(n[1], true).v
        bx = g.genNode(n[2], true).v

      let b = g.buildSetMask(ax.typeOfX(), g.setElemIndex(typ, bx), size)
      let res = g.b.buildAnd(ax, b, g.nn("inset.res", n))
      g.b.buildICmp(
        llvm.IntNE, res, llvm.constInt(ax.typeOfX(), 0, llvm.False),
        g.nn("inset.result", n))
    else:
      let
        ax = g.genNode(n[1], false).v
        bx = g.genNode(n[2], true).v

      let (gep, mask) = g.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
      let a = g.b.buildLoad(gep, g.nn("inset.a", n))
      let res = g.b.buildAnd(a, mask, g.nn("inset.res", n))
      g.b.buildICmp(
        llvm.IntNE, res, llvm.constInt(a.typeOfX(), 0, llvm.False),
        g.nn("inset.result", n))
  LLValue(v: g.buildI8(v))

proc genMagicRepr(g: LLGen, n: PNode): LLValue =
  let t = skipTypes(n[1].typ, abstractVarRange)
  let v = case t.kind
    of tyInt..tyInt64, tyUInt..tyUInt64:
      let ax = g.genNode(n[1], true).v
      g.callCompilerProc("reprInt", [g.buildTruncOrExt(ax, g.primitives[tyInt], t)])
    of tyFloat..tyFloat128:
      let ax = g.genNode(n[1], true).v
      g.callCompilerProc("reprFloat", [ax])
    of tyBool:
      let ax = g.genNode(n[1], true).v
      g.callCompilerProc("reprBool", [ax])
    of tyChar:
      let ax = g.genNode(n[1], true).v
      g.callCompilerProc("reprChar", [ax])
    of tyEnum, tyOrdinal:
      let ax = g.genNode(n[1], true).v
      let a = g.buildTruncOrExt(ax, g.primitives[tyInt], t)
      g.callCompilerProc("reprEnum", [a, g.genTypeInfo(t)])
    of tyString:
      let ax = g.genNode(n[1], true).v
      g.callCompilerProc("reprStr", [ax])
    of tySet:
      let ax = g.genNode(n[1], false).v
      g.callCompilerProc("reprSet", [ax, g.genTypeInfo(t)])
    of tyOpenArray, tyVarargs:
      let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]
      let a = g.b.buildLoad(g.symbols[s.sym.id].v, g.nn("repr.a", s))
      let l = g.b.buildLoad(g.symbols[-s.sym.id].v, g.nn("repr.l", s))
      g.callCompilerProc("reprOpenArray", [a, l, g.genTypeInfo(t.elemType)])
    of tyCString, tyArray, tyUncheckedArray, tyRef, tyPtr, tyPointer, tyNil,
      tySequence:
      let ax = g.genNode(n[1], t.kind in {tyRef, tyPtr, tyVar, tyPointer, tyLent, tySequence}).v
      g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])
    of tyEmpty, tyVoid:
      g.config.localError(n.info, "'repr' doesn't support 'void' type")
      nil
    else:
      let ax = g.genNode(n[1], false).v
      g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])
  LLValue(v: v)

proc genMagicSetLengthStr(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true).v
    a = g.buildLoadValue(ax)
    newstr = LLValue(
      v: g.callCompilerProc("setLengthStr", [a.v, bx]), storage: OnHeap)

  g.genRefAssign(ax, newstr)

proc genMagicSetLengthSeq(g: LLGen, n: PNode) =
  let
    axn = if n[1].kind in {nkAddr, nkHiddenAddr}: n[1][0] else: n[1]
    ax = g.genNode(axn, false)
    bx = g.genNode(n[2], true).v
    a = g.buildLoadValue(ax)
    ap = g.b.buildGEP(a.v, [g.gep0, g.gep0])
    typ = n[1].typ.skipTypes({tyVar, tyLent} + abstractInst)
    x = LLValue(
      v: g.callCompilerProc("setLengthSeqV2", [ap, g.genTypeInfo(typ), bx]),
      storage: OnHeap)

  g.genRefAssign(ax, x)

proc genMagicParallel(g: LLGen, n: PNode) =
  let n2 = g.graph.liftParallel(g.module.sym, n)
  g.genNode(n2)

proc genMagicSwap(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], false)
    t = g.llType(n[1].typ)
    tmpx = LLValue(v: g.localAlloca(t, g.nn("swap.tmp", n)), storage: OnStack)
  g.buildStoreNull(tmpx.v)
  g.genObjectInit(n[1].typ, tmpx.v)
  g.genAsgnFromRef(tmpx, ax, n[1].typ, {})
  g.genAsgnFromRef(ax, bx, n[1].typ, {})
  g.genAsgnFromRef(bx, tmpx, n[1].typ, {})

proc genMagicMove(g: LLGen, n: PNode, load: bool): LLValue =
  g.genNode(n[1], load)

proc skipAddr(n: PNode): PNode =
  result = if n.kind in {nkAddr, nkHiddenAddr}: n[0] else: n

proc genMagicWasMoved(g: LLGen, n: PNode) =
  g.resetLoc(n[1].skipAddr.typ, g.genNode(n[1], false))

proc genMagicDefault(g: LLGen, n: PNode, load: bool): LLValue =
  let typ = n.typ.skipTypes(abstractInst)

  if typ.kind == tyArray or not load:
    let
      t = g.llType(typ)
      v = g.m.addGlobal(t, "")

    v.setInitializer(llvm.constNull(t))
    v.setGlobalConstant(llvm.True)

    LLValue(v: v, lode: n, storage: OnStatic)
  else:
    LLValue(v: constNull(g.llType(n.typ)), lode: n)

proc genMagicReset(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], false).v

  discard g.callCompilerProc("genericReset",
    [ax, g.genTypeInfo(skipTypes(n[1].typ, abstractVarRange))])

proc genMagicIsNil(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true)

  let typ = skipTypes(n[1].typ, abstractPtrs)
  let v =
    if typ.kind == tyProc and typ.callConv == ccClosure:
      g.b.buildICmp(
        llvm.IntEQ, g.b.buildExtractValue(ax, 0, g.nn("isnil.prc", n)).v,
          llvm.constNull(g.voidPtrType), g.nn("isnil", n))
    else:
      g.b.buildICmp(
        llvm.IntEQ, ax.v, llvm.constNull(ax.v.typeOfX()), g.nn("isnil", n))
  LLValue(v: g.buildI8(v))

proc genMagicArrToSeq(g: LLGen, n: PNode): LLValue =
  if n[1].kind == nkBracket:
    n[1].typ = n.typ
    return g.genNode(n[1], true)

  let
    tmp = LLValue(v:
      g.localAlloca(g.llType(n.typ), g.nn("arrtoseq", n)), storage: OnStack)
    l = g.config.lengthOrd(skipTypes(n[1].typ, abstractInst))

  g.buildStoreNull(tmp.v)
  g.genNewSeqAux(tmp, n.typ, g.constNimInt(l))
  # In-flight values can be considered to live OnHeap
  let tmpl = g.buildLoadValue(tmp)
  let src = g.genNode(n[1], true)

  for i in 0..<l.toInt():
    let tgt = g.buildNimSeqDataGEP(tmpl, g.constGEPIdx(i))
    let srcg = g.b.buildGEP(src, [g.constGEPIdx(i)], g.nn("arrtoseq.gep", n))
    g.genAsgnFromRef(
      tgt, srcg, n[1].typ.skipTypes(abstractInst).elemType(), {needToCopy})

  tmpl

proc genMagicSpawn(g: LLGen, n: PNode): LLValue =
  let n2 = g.graph.wrapProcForSpawn(g.module.sym, n, n.typ, nil, nil)
  g.genNode(n2, true)

proc genMagicDeepCopy(g: LLGen, n: PNode) =
  let x = if n[1].kind in {nkAddr, nkHiddenAddr}: n[1][0] else: n[1]
  let ax = g.genNode(x, false).v

  let ty = n[2].typ.skipTypes(abstractVarRange)
  case ty.kind
  of tyPtr, tyRef, tyProc, tyTuple, tyObject, tyArray, tyUncheckedArray:
    let bx = g.genNode(n[2], false).v
    discard g.callCompilerProc("genericDeepCopy", [ax, bx, g.genTypeInfo(ty)])

  of tySequence, tyString:
    let bx = g.genNode(n[2], true).v
    discard g.callCompilerProc("genericSeqDeepCopy", [ax, bx, g.genTypeInfo(ty)])

  of tyOpenArray, tyVarargs:
    g.config.internalError(n.info, "todo")

  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      let bx = g.genNode(n[2], true).v
      discard g.b.buildStore(bx, ax)
    else:
      let bx = g.genNode(n[2], false).v
      g.callMemcpy(ax, bx, ax.typeOfX().getElementType().sizeOfX())
  of tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar, tyLent:
    let bx = g.genNode(n[2], true).v
    discard g.b.buildStore(bx, ax)
  else:
    g.config.internalError(n.info, "genDeepCopy: " & $ty.kind)

proc genMagicGetTypeInfo(g: LLGen, n: PNode): LLValue =
  let typ = n[1].typ.skipTypes(abstractVarRange)
  LLValue(v: g.genTypeInfo(typ))

proc genMagic(g: LLGen, n: PNode, load: bool): LLValue =
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
  of mDivI: result = g.genMagicBinOpOverflow(n, llvm.SDiv)
  of mModI: result = g.genMagicBinOpOverflow(n, llvm.SRem) # TODO verify
  of mSucc: result = g.genMagicBinOpOverflow(n, llvm.Add)
  of mPred: result = g.genMagicBinOpOverflow(n, llvm.Sub)
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
  of mXor: result = g.genMagicCmpI(n, llvm.IntNE)
  of mEqCString: result = g.genMagicEqCString(n)
  of mEqProc: result = g.genMagicEqProc(n)
  of mUnaryMinusI, mUnaryMinusI64: result = g.genMagicUnaryMinus(n)
  of mAbsI: result = g.genMagicAbsI(n)
  of mNot: result = g.genMagicNot(n)
  of mBitnotI: result = g.genMagicBitnot(n)
  of mUnaryMinusF64: result = g.genMagicUnaryMinusF64(n)
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
  of mMove: result = g.genMagicMove(n, load)
  of mWasMoved: g.genMagicWasMoved(n)
  of mDefault: result = g.genMagicDefault(n, load)
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
proc genNodeSym(g: LLGen, n: PNode, load: bool): LLValue =
  let sym = n.sym
  case sym.kind
  of skVar, skLet, skTemp, skResult, skForVar:
    let v =
      if lfHeader in sym.loc.flags or lfNoDecl in sym.loc.flags:
        g.externGlobal(sym)
      elif sfGlobal in sym.flags:
        g.genGlobal(n, false)
      else:
        g.symbols[sym.id]

    if load and v.v.typeOfX().getTypeKind() == llvm.PointerTypeKind:
      let tmp = g.buildLoadValue(v)
      if sfVolatile in sym.flags:
        # TODO writes...
        tmp.v.setVolatile(llvm.True)
      tmp
    else:
      v
  of skParam:
    var v = g.symbols[sym.id]

    var toload = g.llPassAsPtr(n.typ)
    if toload and load and
        v.v.typeOfX().getElementType().getTypeKind() == llvm.PointerTypeKind:
      let tmp = g.buildLoadValue(v)
      g.buildLoadValue(tmp)
    elif not load and not toload and
        sym.typ.kind notin {tyPtr, tyVar, tyRef, tyPointer, tyLent}:
      # Someone wants an address, but all we have is a value...
      v
    else:
      g.buildLoadValue(v)
  of skType:
    LLValue(v: g.genTypeInfo(sym.typ))
  of skMethod:
    if {sfDispatcher, sfForward} * sym.flags != {}:
      g.genFunction(sym)
    else:
      g.genFunctionWithBody(sym)

  of skProc, skConverter, skIterator, skFunc:
    if sfCompileTime in sym.flags:
      g.config.localError(
        n.info, "request to generate code for .compileTime proc: " & sym.name.s)

    if (lfNoDecl in sym.loc.flags or sym.magic != mNone or
         {sfImportc, sfInfixCall} * sym.flags != {}) and
         lfImportCompilerProc notin sym.loc.flags:
      g.genFunction(sym)
    else:
      g.genFunctionWithBody(sym)
  of skConst:
    let v = g.genConst(n)

    g.maybeLoadValue(
      v, load and v.v.typeOfX().getTypeKind() == llvm.PointerTypeKind)

  of skEnumField:
    LLValue(v: llvm.constInt(g.llType(sym.typ), sym.position.culonglong, llvm.False))

  else:
    g.config.internalError(n.info, "Unhandled nodesym kind: " & $sym.kind)
    LLValue()

proc genNodeIntLit(g: LLGen, n: PNode): LLValue =
  let nt = g.llType(n.typ)

  let v =
    case nt.getTypeKind
    of llvm.PointerTypeKind:
      llvm.constIntToPtr(
        llvm.constInt(g.primitives[tyInt], n.intVal.culonglong, llvm.False), nt)
    of llvm.HalfTypeKind..llvm.PPC_FP128TypeKind:
      # This is pretty nuts, but an example can be seen in `lib/pure/json.nim`
      # where `Q7` gets initialized..
      llvm.constReal(nt, n.intVal.cdouble)
    else:
      llvm.constInt(nt, n.intVal.culonglong, llvm.False)

  LLValue(v: v, lode: n, storage: OnStatic)

proc genNodeFloatLit(g: LLGen, n: PNode): LLValue =
  let v = llvm.constReal(g.llType(n.typ), n.floatVal)
  LLValue(v: v, lode: n, storage: OnStatic)

proc genNodeStrLit(g: LLGen, n: PNode, load: bool): LLValue =
  let v =
    if n.typ.skipTypes(
        abstractVarRange + {tyStatic, tyUserTypeClass, tyUserTypeClassInst}).kind == tyString:
      if n.strVal.len == 0:
        constNull(g.llStringType())
      else:
        let ss = g.constNimString(n)
        let s = g.m.addPrivateConstant(ss.typeOfX(), g.nn(".str", n))
        s.setInitializer(ss)
        constBitCast(s, g.llStringType())
    else:
      let init = g.lc.constStringInContext(n.strVal)
      let s = g.m.addPrivateConstant(init.typeOfX(), g.nn(".cstr", n))
      s.setInitializer(init)
      constBitCast(s, g.primitives[tyCString])

  LLValue(v: v, lode: n, storage: OnStatic)

proc genNodeNilLit(g: LLGen, n: PNode, load: bool): LLValue =
  # proc x() = nil
  if n.typ.isEmptyType:
    return LLValue()

  let t = g.llType(n.typ.skipTypes(abstractVarRange))
  let v =
    if load:
      llvm.constNull(t)
    else:
      let tmp = g.m.addPrivateConstant(t, g.nn("nil", n))
      tmp.setInitializer(llvm.constNull(t))
      tmp
  LLValue(v: v, lode: n, storage: OnStatic)

proc genNodeCall(g: LLGen, n: PNode, load: bool): LLValue =
  let nf = n[namePos]

  if nf.kind == nkSym:
    let sym = nf.sym
    if sym.magic != mNone:
      return g.genMagic(n, load)

  g.genCall(n, load)

proc genSingleVar(g: LLGen, n: PNode) =
  let vn = n[0]
  let v = vn.sym

  if sfCompileTime in v.flags: return
  if sfGoto in v.flags: g.config.internalError(n.info, "Goto vars not supported")

  let init = n[2]
  let x =
    if sfGlobal in v.flags:
      if v.flags * {sfImportc, sfExportc} == {sfImportc} and
          n[2].kind == nkEmpty and
          v.loc.flags * {lfHeader, lfNoDecl} != {}:
        return

      let isConst =
        (v.kind == skLet and g.f.withinLoop == 0) and init.isDeepConstExprLL()
      var tmp = g.genGlobal(vn, isConst)

      if isConst:
        let ci = g.genConstInitializer(init)
        if ci == nil:
          g.config.internalError(n.info, "Unable to generate const initializer: " & $init)

        # TODO when enabled, ptr equality checks (for example in isObj) get
        #      optimized away - need to consider when it's safe
        # result.setUnnamedAddr(llvm.True)
        case v.typ.kind
        of tySequence, tyString, tyCString:
          let c = g.m.addPrivateConstant(ci.typeOfX(), g.nn(".const.init", n))
          c.setInitializer(ci)
          tmp.v.setInitializer(constPointerCast(c, tmp.v.typeOfX.getElementType))
        else:
          tmp.v.setInitializer(ci)
        return

      g.genObjectInit(v.typ, tmp.v)
      g.registerGcRoot(v, tmp.v)
      # oddly, variables in a loop in the global scope are tagged "global" even
      # though they're local to the looping block
      if g.f.withinLoop > 0:
        g.resetLoc(v.typ, tmp)

      if init.kind != nkEmpty and sfPure in v.flags:
        if g.init.sections[secLastPreinit] == nil:
          g.init.sections[secLastPreinit] = g.section(g.init, secPreinit)

        g.withFunc(g.init): g.withBlock(g.section(g.init, secLastPreinit)):
          # Avoid using the wrong exception handling context when initializing
          # globals..
          # TODO use the right exception handling context
          let nts = g.f.nestedTryStmts
          g.f.nestedTryStmts = @[]

          g.genAssignment(tmp, init, v.typ, v.assignCopy)
          g.init.sections[secLastPreinit] = g.b.getInsertBlock()
          g.f.nestedTryStmts = nts
        LLValue()
      else:
        tmp
    else:
      let tmp = g.genLocal(vn)
      g.initLocalVar(v, v.typ, tmp.v, g.isAssignedImmediately(init))

      tmp

  if init.kind != nkEmpty and x.v != nil:
    g.genAssignment(x, init, v.typ, v.assignCopy)

proc genClosureVar(g: LLGen, n: PNode) =
  let x = g.genNode(n[0], false)

  if n[2].kind == nkEmpty:
    g.buildStoreNull(x.v)
    g.genObjectInit(n[0].typ, x.v)
  else:
    g.genAssignment(x, n[2], n[0].typ, {needToCopy})

proc genNodeIdentDefs(g: LLGen, n: PNode) =
  for s in n.sons: p("genIdentDefsStmt", s, g.depth + 1)
  if n[0].kind == nkSym:
    g.genSingleVar(n)
  else:
    g.genClosureVar(n)

proc genNodeVarTuple(g: LLGen, n: PNode) =
  for s in n.sons[0..n.len - 3]:
    if s.kind != nkSym:
      g.genNode(g.graph.lowerTupleUnpacking(n, g.module.sym))
      return

  let t = g.genNode(n.lastSon, false)

  for i in 0..n.len - 3:
    let vn = n[i]
    let v = vn.sym

    let x =
      if sfGlobal in v.flags:
        let tmp = g.genGlobal(vn, false)
        g.genObjectInit(v.typ, tmp.v)
        g.registerGcRoot(v, tmp.v)
        tmp
      else:
        let tmp = g.genLocal(vn)
        g.initLocalVar(v, v.typ, tmp.v, g.isAssignedImmediately(n.lastSon))
        tmp

    let tv = g.b.buildGEP(t, [g.gep0, g.constGEPIdx(i)], g.nn("vartuple." & $i, vn))

    g.genAsgnFromRef(x, tv, vn.typ, v.assignCopy)

proc genNodePar(g: LLGen, n: PNode, load: bool): LLValue =
  if n.isDeepConstExprLL():
    let init = g.genConstInitializer(n)
    if load: return LLValue(v: init, lode: n, storage: OnStatic)

    let v = g.m.addPrivateConstant(init.typeOfX(), g.nn("par.init", n))
    v.setInitializer(init)
    return LLValue(v: v, lode: n, storage: OnStatic)

  let v = LLValue(
    v: g.localAlloca(g.llType(n.typ), g.nn("par", n)), storage: OnStack)
  g.buildStoreNull(v.v)

  for i in 0..<n.len:
    var s = n[i]

    if s.kind == nkExprColonExpr: s = s[1]
    let tgt = g.b.buildGEP(v, [g.gep0, g.constGEPIdx(i.int32)], g.nn("par.gep", n))
    g.genAssignment(tgt, s, s.typ, {needToCopy})

  g.maybeLoadValue(v, load)

proc genNodeObjConstr(g: LLGen, n: PNode, load: bool): LLValue =
  var
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)
    isRef = typ.kind == tyRef


  let v =
    if isRef:
      let tmp = LLValue(v:
        g.localAlloca(t, g.nn("objconstr", n)), storage: OnStack)
      g.rawGenNew(tmp, typ)
      typ = typ.lastSon.skipTypes(abstractInst)
      LLValue(v: g.b.buildLoad(tmp.v, ""), storage: OnHeap)
    else:
      let tmp = LLValue(v:
        g.localAlloca(t, g.nn("objconstr", n)), storage: OnStack)
      g.buildStoreNull(tmp.v)
      g.genObjectInit(typ, tmp.v)
      tmp

  for i in 1 ..< n.len:
    let s = n[i]
    let ind = g.fieldIndex(typ, s[0].sym)
    let gep = g.b.buildGEP(v, (@[0] & ind).mapIt(g.constGEPIdx(it)), g.nn("objconstr", n))
    g.genAssignment(gep, s[1], s[1].typ, {needToCopy}) # TODO should be dest typ

  if load and not isRef:
    LLValue(v: g.b.buildLoad(v.v, g.nn("objconstr.load", n)), storage: OnStack)
  elif not load and isRef:
    let tmp = g.localAlloca(t, g.nn("objconstr", n))
    discard g.b.buildStore(v.v, tmp)
    LLValue(v: tmp, storage: OnStack)
  else:
    v

proc genNodeCurly(g: LLGen, n: PNode, load: bool): LLValue =
  let
    typ = n.typ
    size = g.config.getSize(skipTypes(n.typ, abstractVar))
    t = g.llType(typ)

  if size <= 8:
    let tmp = g.localAlloca(t, g.nn("curly", n))

    if n.isDeepConstExprLL():
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
          let ax = g.genNode(s, true).v

          let mask = g.buildSetMask(t, g.setElemIndex(typ, ax), size)
          let v = g.b.buildLoad(tmp, g.nn("curly.v"))
          let nv = g.b.buildOr(v, mask, g.nn("curly.nv", n))
          discard g.b.buildStore(nv, tmp)
    g.maybeLoadValue(LLValue(v: tmp), load)
  else:
    let v = g.localAlloca(t, g.nn("curly", n))
    if n.isDeepConstExprLL():
      discard g.b.buildStore(g.constNimSet(n), v)
    else:
      g.buildStoreNull(v)

      for s in n:
        if s.kind == nkRange:
          withRangeItems(it, s):
            let (gep, mask) = g.buildSetGEPMask(v, g.setElemIndex(typ, it))
            let v = g.b.buildLoad(gep, g.nn("curly.v", n))
            let nv = g.b.buildOr(v, mask, g.nn("curly.nv", n))
            discard g.b.buildStore(nv, gep)
        else:
          let ax = g.genNode(s, true).v

          let (gep, mask) = g.buildSetGEPMask(v, g.setElemIndex(typ, ax))
          let v = g.b.buildLoad(gep, g.nn("curly.v", n))
          let nv = g.b.buildOr(v, mask, g.nn("curly.nv", n))
          discard g.b.buildStore(nv, gep)
    g.maybeLoadValue(LLValue(v: v), load)

proc genNodeBracket(g: LLGen, n: PNode, load: bool): LLValue =
  let typ = n.typ.skipTypes(abstractVarRange)
  let t = g.llType(typ)
  if n.isDeepConstExprLL():
    if typ.kind == tySequence and n.len == 0:
      return LLValue(v: constNull(g.llType(typ)), lode: n, storage: OnStatic)

    let init = g.genConstInitializer(n)
    let c = g.m.addPrivateConstant(init.typeOfX(), g.nn("bracket.init", n))
    c.setInitializer(init)

    let v =
      if n.isSeqLike() and c != g.llType(n.typ):
        llvm.constBitCast(c, g.llType(n.typ))
      else:
        if load and c.typeOfX().isArrayPtr():
          g.b.buildGEP(c, [g.gep0, g.gep0], g.nn("bracket.const", n))
        else:
          c

    return LLValue(v: v, lode: n, storage: OnStatic)

  case typ.kind
  of tyArray, tyUncheckedArray:
    result = LLvalue(
      v: g.localAlloca(t, g.nn("bracket.arr", n)), lode: n, storage: OnStack)
    g.buildStoreNull(result.v)

    for i in 0..<n.len:
      let gep =
        g.b.buildGEP(result, [g.gep0, g.constGEPIdx(i)], g.nn("bracket.n", n))
      g.genAssignment(gep, n[i], typ.elemType, {needToCopy})

    result = g.maybeLoadValue(result, load)
  of tySequence:
    let
      tmp = LLValue(v: g.localAlloca(t, g.nn("bracket", n)), storage: OnStack)
      l = int(n.len)

    g.buildStoreNull(tmp.v)
    g.genNewSeqAux(tmp, n.typ, g.constNimInt(l))
    let tmpl = g.buildLoadValue(tmp)

    for i in 0..<l:
      let tgt = g.buildNimSeqDataGEP(tmpl, g.constGEPIdx(i))
      g.genAssignment(tgt, n[i], typ.elemType, {needToCopy})
    result = tmpl
  else:
    g.config.internalError(n.info, "Unhandled nodebracket kind: " & $typ.kind)

proc genBoundsCheck(g: LLGen, typ: PType, arr, a, b: llvm.ValueRef) =
  let ty = skipTypes(typ, abstractVarRange)

  let cond =
    case ty.kind
    of tyOpenArray, tyVarargs:
      if true: return; nil
      else:
        let
          diff = g.b.buildSub(b, a, g.nn("bc.diff", arr))
          minusOne = constInt(diff.typeOfX, not culonglong(0), llvm.True)
          diffne = g.b.buildICmp(
            llvm.IntNE, minusOne, diff, g.nn("bc.diffne", arr))
          len: llvm.ValueRef = nil # TODO!
          aok = g.b.buildICmp(llvm.IntUGE, a, len, g.nn("bc.aok", arr))
          bok = g.b.buildICmp(llvm.IntUGE, b, len, g.nn("bc.bok", arr))
          aorb = g.b.buildBinOp(llvm.Or, aok, bok, g.nn("bc.aorb", arr))
        g.b.buildBinOp(llvm.And, diffne, aorb, g.nn("bc.cond", arr))

    of tyArray:
      let
        diff = g.b.buildSub(b, a, g.nn("bc.diff", arr))
        minusOne = constInt(diff.typeOfX, not culonglong(0), llvm.True)
        diffne = g.b.buildICmp(
          llvm.IntNE, minusOne, diff, g.nn("bc.diffne", arr))

        # TODO Int128
        first = constInt(a.typeOfX, g.config.firstOrd(ty).toInt64.culongLong, llvm.True)
        last = constInt(a.typeOfX, g.config.lastOrd(ty).toInt64.culongLong, llvm.True)

        difflt = g.b.buildICmp(llvm.IntSLT, diff, minusone, g.nn("bc.dlt", arr))
        alt = g.b.buildICmp(llvm.IntSLT, a, first, g.nn("bc.alt", arr))
        or0 = g.b.buildBinOp(llvm.Or, difflt, alt, g.nn("bc.or0", arr))

        agt = g.b.buildICmp(llvm.IntSGT, a, last, g.nn("bc.agt", arr))
        or1 = g.b.buildBinOp(llvm.Or, or0, agt, g.nn("bc.or1", arr))

        blt = g.b.buildICmp(llvm.IntSLT, b, first, g.nn("bc.blt", arr))
        or2 = g.b.buildBinOp(llvm.Or, or1, blt, g.nn("bc.or2", arr))

        bgt = g.b.buildICmp(llvm.IntSGT, b, last, g.nn("bc.bgt", arr))
        or3 = g.b.buildBinOp(llvm.Or, or2, bgt, g.nn("bc.or3", arr))

      g.b.buildBinOp(llvm.And, diffne, or3, g.nn("bc.cond", arr))
    of tySequence, tyString:
      let
        diff = g.b.buildSub(b, a, g.nn("bc.diff", arr))
        minusOne = constInt(diff.typeOfX, not culonglong(0), llvm.True)
        diffne = g.b.buildICmp(
          llvm.IntNE, minusOne, diff, g.nn("bc.diffne", arr))
        len = g.loadNimSeqLen(arr)
        aok = g.b.buildICmp(llvm.IntUGE, a, len, g.nn("bc.aok", arr))
        bok = g.b.buildICmp(llvm.IntUGE, b, len, g.nn("bc.bok", arr))
        aorb = g.b.buildBinOp(llvm.Or, aok, bok, g.nn("bc.aorb", arr))

      g.b.buildBinOp(llvm.And, diffne, aorb, g.nn("bc.cond", arr))

    else: return; nil

  g.callRaise(cond, "raiseIndexError")

proc genNodeBracketExprArray(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true).v
    ty = n[0].typ.skipTypes(abstractVarRange + abstractPtrs + tyUserTypeClasses)
    first = g.config.firstOrd(ty)

  assert bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind
  assert ty.kind != tyUncheckedArray

  # GEP indices are signed, so if a char appears here we want to make sure
  # it's zero-extended
  let
    bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
    fi = g.constNimInt(first)
    b =
      if first != 0: g.b.buildSub(bi, fi, g.nn("bra.arr.first", n))
      else: bi

  if optBoundsCheck in g.f.options:
    let
      len = g.constNimInt(g.config.lastOrd(ty) - first + 1)
      cond = g.b.buildICmp(llvm.IntUGE, b, len, "bes.bounds")
    if first == 0:
      g.callRaise(cond, "raiseIndexError2", [bi, len])
    else:
      g.callRaise(cond, "raiseIndexError3", [bi, fi, len])

  g.maybeLoadValue(
    if ax.v.typeOfX().isArrayPtr():
      g.b.buildGEP(ax, [g.gep0, b], g.nn("bra.arr.geparr"))
    else:
      g.b.buildGEP(ax, [b], g.nn("bra.arr.gep"))
  , load)

proc genNodeBracketExprUncheckedArray(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], false).v
    bx = g.genNode(n[1], true).v
    ty = n[0].typ.skipTypes(abstractVarRange + abstractPtrs + tyUserTypeClasses)
    first = g.config.firstOrd(ty)

  assert bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind

  # GEP indices are signed, so if a char appears here we want to make sure
  # it's zero-extended
  let bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
  let b =
    if first != 0:
      g.b.buildSub(bi, g.constNimInt(first), g.nn("bra.arr.first", n))
    else: bi

  g.maybeLoadValue(LLValue(v:
    if ax.typeOfX().isArrayPtr():
      g.b.buildGEP(ax, [g.gep0, b])
    else:
      g.b.buildGEP(ax, [b])
  ), load)

proc genNodeBracketExprOpenArray(g: LLGen, n: PNode, load: bool): LLValue =
  let
    s = if n[0].kind == nkHiddenDeref: n[0][0] else: n[0]
    ax = g.buildLoadValue(g.symbols[s.sym.id])
    bx = g.genNode(n[1], true).v

  if optBoundsCheck in g.f.options:
    let
      len = g.lenOpenArray(s)
      bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
      cond = g.b.buildICmp(llvm.IntUGE, bi, len, "beoa.bounds")
      len2 = g.b.buildSub(len, g.constNimInt(1), "")
    g.callRaise(cond, "raiseIndexError2", [bi, len2])

  g.maybeLoadValue(g.b.buildGEP(ax, [bx], g.nn("beoa.gep")), load)

proc genNodeBracketExprSeq(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true).v

  if optBoundsCheck in g.f.options:
    let
      len = g.loadNimSeqLen(ax.v)
      bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
      cond = g.b.buildICmp(llvm.IntUGE, bi, len, "bes.bounds")
      len2 = g.b.buildSub(len, g.constNimInt(1), "")
    g.callRaise(cond, "raiseIndexError2", [bi, len2])

  g.maybeLoadValue(g.buildNimSeqDataGEP(ax, bx), load)

proc genNodeBracketExprCString(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true).v

  g.maybeLoadValue(
    g.b.buildGEP(ax, [bx], g.nn("bra.cstr.gep", n)), load)

proc genNodeBracketExprTuple(g: LLGen, n: PNode, load: bool): LLValue =
  var
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true).v

  if bx.typeOfX().getIntTypeWidth() > 32.cuint:
    bx = g.b.buildTrunc(bx, llvm.int32TypeInContext(g.lc), g.nn("bra.tup.trunc", n))

  g.maybeLoadValue(
    g.b.buildGEP(ax, [g.gep0, bx], g.nn("bra.tup.gep", n)), load)

proc genNodeBracketExpr(g: LLGen, n: PNode, load: bool): LLValue =
  var typ = skipTypes(n[0].typ, abstractVarRange + tyUserTypeClasses)
  if typ.kind in {tyRef, tyPtr}: typ = typ.lastSon.skipTypes(abstractVarRange)
  case typ.kind
  of tyArray: g.genNodeBracketExprArray(n, load)
  of tyUncheckedArray: g.genNodeBracketExprUncheckedArray(n, load)
  of tyOpenArray, tyVarargs: g.genNodeBracketExprOpenArray(n, load)
  of tySequence, tyString: g.genNodeBracketExprSeq(n, load)
  of tyCString: g.genNodeBracketExprCString(n, load)
  of tyTuple: g.genNodeBracketExprTuple(n, load)
  else:
    g.config.internalError(n.info, "Unhandled nodebracketexpr kind: " & $typ.kind)
    LLValue()

proc genNodeDot(g: LLGen, n: PNode, load: bool): LLValue =
  p("genDotExpr", n[1].sym, g.depth + 1)
  let v = g.genNode(n[0], false)

  let typ = skipTypes(n[0].typ, abstractInst + tyUserTypeClasses)

  let i = g.fieldIndex(typ, n[1].sym)
  g.maybeLoadValue(
    g.b.buildGEP(v, (@[0] & i).mapIt(g.constGEPIdx(it)), g.nn("dot.gep", n)),
    load)

proc genNodeCheckedField(g: LLGen, n: PNode, load: bool): LLValue =
  g.genNode(n[0], load)

proc genNodeDeref(g: LLGen, n: PNode, load: bool): LLValue =
  let
    v = g.genNode(n[0], true)

  var typ = n[0].typ
  if typ.kind in {tyUserTypeClass, tyUserTypeClassInst} and
      typ.isResolvedUserTypeClass:
    typ = typ.lastSon
  typ = typ.skipTypes(abstractInst)
  let storage =
    case typ.kind
    of tyRef: OnHeap
    of tyPtr, tyVar, tyLent: OnUnknown
    else: g.config.internalError(n.info, "deref " & $n[0].typ.kind); OnUnknown

  let tmp = LLValue(v: v.v, lode: v.lode, storage: storage)
  g.maybeLoadValue(tmp, load)

proc genNodeIfExpr(g: LLGen, n: PNode, load: bool): LLValue =
  # Sometimes an nkIfStmt appears in the ast even though it looks more like
  # an expression (see tcasestmt with an if in a case else).. it won't have
  # a type of its own so we'll have to cheat..
  let typ = n.deepTyp
  let v = LLValue(v:
    g.localAlloca(g.llType(typ), g.nn("ifx.res", n)), storage: OnStack)
  g.buildStoreNull(v.v)

  let iend = g.b.appendBasicBlockInContext(g.lc, g.nn("ifx.end", n))

  for i in 0..<n.len:
    let s = n[i]

    if s.len == 1:
      # else branch
      discard g.f.startBlock(n, nil)
      # branches might lack return value if they exit instead (quit?)
      if not typ.isEmptyType() and not s[0].typ.isEmptyType():
        g.genAssignment(v, s[0], typ, {needToCopy})
      else:
        g.genNode(s[0])
      g.f.endBlock()

      discard g.b.buildBr(iend)
    else:
      let cond = g.buildI1(g.genNode(s[0], true).v)

      let
        itrue = g.b.appendBasicBlockInContext(g.lc, g.nn("ifx.true", n))
        ifalse = g.b.appendBasicBlockInContext(g.lc, g.nn("ifx.false", n))

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      discard g.f.startBlock(n, nil)
      # branches might lack return value if they exit instead (quit?)
      if not typ.isEmptyType() and not s[1].typ.isEmptyType():
        g.genAssignment(v, s[1], typ, {needToCopy})
      else:
        g.genNode(s[1])
      g.f.endBlock()

      discard g.b.buildBr(iend)

      g.b.positionAndMoveToEnd(ifalse)

  if n[n.len - 1].len != 1:
    discard g.b.buildBr(iend)

  g.b.positionAndMoveToEnd(iend)

  g.maybeLoadValue(v, load)

proc genNodeLambda(g: LLGen, n: PNode): LLValue =
  let sym = n[namePos].sym
  g.genFunctionWithBody(sym)

proc genNodeConv(g: LLGen, n: PNode, load: bool): LLValue =
  let
    v = g.genNode(n[1], load)
    vt = v.v.typeOfX()
    nt = if load: g.llType(n.typ) else: llvm.pointerType(g.llType(n.typ))
    vtk = vt.getTypeKind()
    ntk = nt.getTypeKind()
    vtyp = skipTypes(n[1].typ, abstractInst)
    ntyp = skipTypes(n.typ, abstractInst)

  if vt == nt:
    v
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    LLValue(v: g.buildTruncOrExt(v.v, nt, n[1].typ))
  elif vtk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind} and ntk == llvm.IntegerTypeKind:
    if ntyp.kind in {tyUInt..tyUInt64}:
      LLValue(v: g.b.buildFPToUI(v.v, nt, g.nn("conv.fptoui", n)))
    else:
      LLValue(v: g.b.buildFPToSI(v.v, nt, g.nn("conv.fptosi", n)))
  elif vtk == llvm.IntegerTypeKind and ntk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind}:
    if vtyp.kind in {tyUInt..tyUInt64}:
      LLValue(v: g.b.buildUIToFP(v.v, nt, g.nn("conv.uitofp", n)))
    else:
      LLValue(v: g.b.buildSIToFP(v.v, nt, g.nn("conv.sitofp", n)))
  elif n[1].typ.kind == tyPtr and n.typ.kind == tyPointer:
    LLValue(v: g.b.buildBitCast(v.v, nt, g.nn("conv.ptr", n)))
  elif vtk == llvm.FloatTypeKind and ntk == llvm.DoubleTypeKind:
    LLValue(v: g.b.buildFPExt(v.v, nt, g.nn("conv.fd", n)))
  elif vtk == llvm.PointerTypeKind and ntk == llvm.PointerTypeKind:
    LLValue(v: g.b.buildBitCast(v.v, nt, g.nn("conv.pointer", n)))
  elif n.typ.kind == tyProc and ntk == llvm.PointerTypeKind or nt == g.closureType:
    LLValue(v: g.b.buildBitCast(v.v, g.voidPtrType, g.nn("conv.proc", n)))
  elif vtk == llvm.DoubleTypeKind and ntk == llvm.FloatTypeKind:
    LLValue(v: g.b.buildFPTrunc(v.v, nt, g.nn("conv.df", n)))
  elif vtyp.kind == tyArray and ntyp.kind == tyArray:
    v
  else:
    g.config.internalError(n.info, "Unhandled conversion: " & $vt & " " & $nt & " " &
      $n[1].typ.kind & " " & $n.typ.kind)
    LLValue()

proc genNodeCast(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ntyp = n[1].typ.skipTypes(abstractRange)
    v =
      if ntyp.kind in {tyOpenArray, tyVarargs}: g.genNode(n[1], false).v
      else: g.genNode(n[1], load).v
    vt = v.typeOfX()
    vtk = vt.getTypeKind()
    nt = g.llType(n.typ)
    ntk = nt.getTypeKind()

  LLValue(v:
    if vtk == llvm.PointerTypeKind and ntk == llvm.IntegerTypeKind:
      g.b.buildPtrToInt(v, nt, g.nn("cast.pi", n))
    elif vtk == llvm.IntegerTypeKind and ntk == llvm.PointerTypeKind:
      g.b.buildIntToPtr(v, nt, g.nn("cast.ip", n))
    elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
      g.buildTruncOrExt(v, nt, ntyp)
    elif vtk == llvm.PointerTypeKind and ntk == llvm.PointerTypeKind:
      g.b.buildBitCast(v, nt, g.nn("cast.bit", n))
    else:
      let
        size = max(
          g.m.getModuleDataLayout().sizeOfXTypeInBits(vt),
          g.m.getModuleDataLayout().sizeOfXTypeInBits(nt))
        tmp = g.localAlloca(
          llvm.arrayType(g.primitives[tyUInt8], size.cuint), g.nn("cast.tmp", n))
      discard g.b.buildStore(
        v, g.b.buildBitCast(tmp, vt.pointerType(), g.nn("cast.v", n)))
      g.b.buildLoad(
        g.b.buildBitCast(tmp, nt.pointerType(), g.nn("cast.n", n)), g.nn("cast", n))
  )

proc genNodeAddr(g: LLGen, n: PNode): LLValue =
  g.genNode(n[0], false)

proc genNodeObjDownConv(g: LLGen, n: PNode, load: bool): LLValue =
  let ax = g.genNode(n[0], load)

  let at = ax.v.typeOfX()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  if at == nt:
    ax
  elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
    LLValue(v: g.b.buildBitCast(ax.v, nt, g.nn("obj.down", n)))
  else:
    g.config.internalError(n.info, "Unhandled nkObjDownConv")
    LLValue()

proc genNodeObjUpConv(g: LLGen, n: PNode, load: bool): LLValue =
  let ax = g.genNode(n[0], n.typ.skipTypes(abstractInst).kind == tyRef)

  let dest = n.typ.skipTypes(abstractPtrs)
  if optObjCheck in g.f.options and not isObjLackingTypeField(dest):
    var r = ax
    var nilCheck: LLValue
    var t = n[0].typ.skipTypes(abstractInst)
    var first = true
    while t.kind in {tyVar, tyLent, tyPtr, tyRef}:
      if t.kind notin {tyVar, tyLent}: nilCheck = r
      if first:
        first = false
      else:
        r = g.buildLoadValue(r)

      t = t.lastSon.skipTypes(abstractInst)
    let
      ti = g.genTypeInfo(t)
      mt_gep = g.b.buildGEP(r, g.mtypeIndex(t), g.nn("mt_gep", r))
      mt = g.b.buildLoad(mt_gep.v, g.nn("mt", mt_gep))

    if not nilCheck.v.isNil():
      g.withNotNil(nilCheck.v):
        discard g.callCompilerProc("chckObj", [mt, ti])
    else:
      discard g.callCompilerProc("chckObj", [mt, ti])

  let at = ax.v.typeOfX()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  let v =
    if at == nt:
      ax
    elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
      LLValue(v: g.b.buildBitCast(ax.v, nt, g.nn("obj.up", n)))
    else:
      g.config.internalError(n.info, "Unhandled nkUpDownConv")
      LLValue()

  g.maybeLoadValue(v, load and n.typ.skipTypes(abstractInst).kind notin {tyVar, tyLent, tyPtr, tyRef})

proc genNodeChckRangeF(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[0], true)

  if optRangeCheck in g.f.options:
    let
      bx = g.genNode(n[1], true).v
      cx = g.genNode(n[2], true).v

    let args = [
      g.b.buildFPExt(ax.v, g.primitives[tyFloat], g.nn("fpext", ax.v)),
      g.b.buildFPExt(bx, g.primitives[tyFloat], g.nn("fpext", bx)),
      g.b.buildFPExt(cx, g.primitives[tyFloat], g.nn("fpext", cx))
    ]
    discard g.callCompilerProc("chckRangeF", args)

  ax

proc genNodeChckRange64(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[0], true).v
    nt = g.llType(n.typ)

  # c gen says range check for uints is problematic..
  if optRangeCheck in g.f.options and
      n.typ.skipTypes(abstractVarRange).kind notin {tyUInt..tyUInt64}:
    let
      bx = g.genNode(n[1], true).v
      cx = g.genNode(n[2], true).v
      # Make use of ext sign!
      a = g.buildTruncOrExt(ax, g.primitives[tyInt64], n[0].typ)
      b = g.buildTruncOrExt(bx, g.primitives[tyInt64], n[1].typ)
      c = g.buildTruncOrExt(cx, g.primitives[tyInt64], n[2].typ)
    discard g.callCompilerProc("chckRange64", [a, b, c])

  LLValue(v: g.buildTruncOrExt(ax, nt, n[0].typ))

proc genNodeChckRange(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[0], true).v
    nt = g.llType(n.typ)

  # c gen says range check for uints is problematic..
  if optRangeCheck in g.f.options and
      n.typ.skipTypes(abstractVarRange).kind notin {tyUInt..tyUInt64}:
    let
      bx = g.genNode(n[1], true).v
      cx = g.genNode(n[2], true).v
      # Make use of ext sign!
      a = g.buildTruncOrExt(ax, g.primitives[tyInt], n[0].typ)
      b = g.buildTruncOrExt(bx, g.primitives[tyInt], n[1].typ)
      c = g.buildTruncOrExt(cx, g.primitives[tyInt], n[2].typ)
    discard g.callCompilerProc("chckRange", [a, b, c])

  LLValue(v: g.buildTruncOrExt(ax, nt, n[0].typ))

proc genNodeStringToCString(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[0], true)
    cstr = g.callCompilerProc("nimToCStringConv", [ax.v])
  LLValue(v: cstr, lode: ax.lode, storage: ax.storage)

proc genNodeAsgn(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], false)
  g.genAssignment(ax, n[1], n[0].typ, {needToCopy})

proc genNodeFastAsgn(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], false)
  # there's a strange if p.prc == nil check in the cgen noting that transf
  # is too aggressive.. let's try to replicated it here, though it's probably
  # buggy:
  g.genAssignment(ax, n[1], n[0].typ, if g.f == g.init: {needToCopy} else: {})

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

proc genNodeIfStmt(g: LLGen, n: PNode): LLValue =
  # TODO Single scope enough?
  discard g.f.startBlock(n, nil)
  var iend: llvm.BasicBlockRef
  for i in 0..<n.len:
    let s = n[i]

    if s.sons.len == 1:
      # else branch
      discard g.f.startBlock(n, nil)
      g.genNode(s[0])
      g.f.endBlock()

      if g.b.needsTerminator():
        if iend == nil:
          iend = g.b.appendBasicBlockInContext(g.lc, g.nn("if.end", n))
        discard g.b.buildBr(iend)
    else:
      let cond = g.buildI1(g.genNode(s[0], true).v)

      let
        itrue = g.b.appendBasicBlockInContext(g.lc, g.nn("if.true", n))
        ifalse = g.b.appendBasicBlockInContext(g.lc, g.nn("if.false", n))

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      discard g.f.startBlock(n, nil)
      g.genNode(s[1])
      g.f.endBlock()

      if g.b.needsTerminator():
        if iend == nil:
          iend = g.b.appendBasicBlockInContext(g.lc, g.nn("if.end", n))
        discard g.b.buildBr(iend)

      g.b.positionAndMoveToEnd(ifalse)

  if iend != nil:
    if n[n.len - 1].len != 1:
      discard g.b.buildBr(iend)
    g.b.positionAndMoveToEnd(iend)
  discard g.f.endBlock()

proc genNodeWhenStmt(g: LLGen, n: PNode, load: bool): LLValue =
  g.genNode(n[1][0], load)

proc genNodeWhileStmt(g: LLGen, n: PNode) =
  inc(g.f.withinLoop)

  preserve(g.f.breakIdx):
    let
      wcmp = g.b.appendBasicBlockInContext(g.lc, g.nn("while.cmp", n))
      wtrue = g.b.appendBasicBlockInContext(g.lc, g.nn("while.true", n))
      wfalse = g.b.appendBasicBlockInContext(g.lc, g.nn("while.false", n))

    # jump to comparison
    discard g.b.buildBr(wcmp)

    # generate condition expression in cmp block
    g.b.positionBuilderAtEnd(wcmp)
    let cond = g.buildI1(g.genNode(n[0], true).v)

    discard g.b.buildCondBr(cond, wtrue, wfalse)

    # loop body
    g.b.positionBuilderAtEnd(wtrue)
    g.f.breakIdx = g.f.startBlock(n, wfalse)
    g.f.blocks[g.f.breakIdx].isLoop = true
    g.genNode(n[1])
    g.f.endBlock()

    # back to comparison
    discard g.b.buildBr(wcmp)

    # continue at the end
    g.b.positionAndMoveToEnd(wfalse)

  dec(g.f.withinLoop)

proc genNodeCaseStmt(g: LLGen, n: PNode, load: bool): LLValue =
  let
    underlying = skipTypes(n[0].typ, abstractVarRange).kind
    typ = n.typ
    ax = g.genNode(n[0], true).v
    u = g.isUnsigned(n[0].typ)

  let caseend = g.b.appendBasicBlockInContext(g.lc, g.nn("case.end", n))

  if not typ.isEmptyType():
    result = LLValue(
      v: g.localAlloca(g.llType(typ), g.nn("case.res", n)),
      storage: OnStack)
    g.buildStoreNull(result.v)
    g.genObjectInit(typ, result.v)

  for i in 1..<n.len:
    let s = n[i]
    p("genNodeCaseStmt", s, g.depth)

    let isLast = i == n.len - 1

    let cur = g.b.getInsertBlock()

    let lbl = if s.kind == nkElse: "case.else" else: "case.of." & $i
    let casedo = g.b.appendBasicBlockInContext(g.lc, g.nn(lbl & ".do", n))

    let next =
      if isLast: caseend
      elif n[i+1].kind == nkElse: g.b.appendBasicBlockInContext(g.lc, g.nn("case.else", n))
      else: g.b.appendBasicBlockInContext(g.lc, g.nn("case.of." & $(i+1), n))

    discard g.f.startBlock(n, caseend)
    g.b.positionBuilderAtEnd(casedo)
    # branches might not have a return value if they exit instead (quit?)
    if result.v != nil and not s.lastSon.typ.isEmptyType():
      g.genAssignment(result, s.lastSon, typ, {needToCopy})
    else:
      g.genNode(s.lastSon)

    g.b.buildBrFallthrough(caseend)

    g.b.positionBuilderAtEnd(cur)

    case s.kind
    of nkOfBranch:
      # sons here is a list of candidates to match against, which may
      # be values or ranges, except the last one which is the action
      for j in 0..s.len - 2:
        let isLast2 = j == s.len - 2
        let cond = s[j]
        if cond.kind == nkRange:
          let
            bx = g.genNode(cond.sons[0], true).v
            cx = g.genNode(cond.sons[1], true).v
            ub = if u: llvm.IntUGE else: llvm.IntSGE
            cmpb = g.b.buildICmp(ub, ax, bx, g.nn("case.cmpb", n))
            uc = if u: llvm.IntULE else: llvm.IntSLE
            cmpc = g.b.buildICmp(uc, ax, cx, g.nn("case.cmpc", n))
            cmp = g.buildI1(g.b.buildAnd(cmpb, cmpc, g.nn("case.cmp", n)))

          let casenext =
            if isLast2: next
            else: g.b.appendBasicBlockInContext(g.lc, g.nn(lbl & ".or." & $j, n))
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)
        else:
          let bx = g.genNode(cond, true).v

          var cmp: llvm.ValueRef
          case underlying
          of tyString:
            cmp = g.buildI1(g.callCompilerProc("eqStrings", [ax, bx]))
          of tyFloat, tyFloat32, tyFloat64, tyFloat128:
            # TODO weirdly, Nim allows variant objects predicated on a float(!)
            #      not implemented for ranges, todo..
            let b = g.buildTruncOrExt(bx, ax.typeOfX(), cond.typ)
            cmp = g.buildI1(
              g.b.buildFCmp(llvm.RealOEQ, ax, b, g.nn("case.cmp", n)))
          else:
            let b = g.buildTruncOrExt(bx, ax.typeOfX(), cond.typ)
            cmp = g.buildI1(
              g.b.buildICmp(llvm.IntEQ, ax, b, g.nn("case.cmp", n)))
          let casenext =
            if isLast2: next
            else: g.b.appendBasicBlockInContext(g.lc, g.nn(lbl & ".or." & $j, n))
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)

        # Moving block is not necessary but makes generated code easier
        # to follow, placing action just after conditions
        if not isLast2:
          g.b.getInsertBlock().moveBasicBlockBefore(casedo)

      # it's actually possible to have a case without condition!
      if s.len < 2:
        discard g.b.buildBr(next)
        g.b.positionBuilderAtEnd(next)
    of nkElse:
      discard g.b.buildBr(casedo)
    else:
      g.config.internalError(s.info, "Unexpected case kind " & $s.kind)
    g.f.endBlock()

  g.b.positionAndMoveToEnd(caseend)

  if g.b.getInsertBlock().getBasicBlockParent().getLastBasicBlock() != caseend:
    caseend.moveBasicBlockAfter(
      g.b.getInsertBlock().getBasicBlockParent().getLastBasicBlock())

  result = g.maybeLoadValue(result, load and result.v != nil)

proc genNodeConstDef(g: LLGen, n: PNode) =
  # we emit these on demand!
  discard

proc getPersonalityFn(g: LLGen): ValueRef =
  if g.personalityFn.isNil:
    let sym = g.graph.getCompilerProc("nlvmEHPersonality")
    if sym == nil:
      g.config.internalError("compiler proc not found: nlvmEHPersonality")

    g.personalityFn = g.genFunctionWithBody(sym).v

  g.personalityFn

proc getLandingPadTy(g: LLGen): TypeRef =
  if g.landingPadTy.isNil:
    g.landingPadTy = g.lc.structCreateNamed(g.nn("landingPadTy"))
    g.landingPadTy.structSetBody(
      [g.voidPtrType, g.lc.int32TypeInContext()], False)

  g.landingPadTy

proc genLandingPad(g: LLGen, n: PNode, name: string): llvm.BasicBlockRef =
  let
    pad = g.b.appendBasicBlockInContext(g.lc, g.nn(name, n))
    fin = if n[^1].kind == nkFinally: n[^1] else: nil

  g.withBlock(pad):
    let
      landing =
        g.b.buildLandingPad(
          g.getLandingPadTy(), g.getPersonalityFn(), 0, g.nn("landing", n))

    var catchAllPresent = false

    for i in 1..<n.len:
      let ni = n[i]
      if ni.kind != nkExceptBranch: break

      if ni.len == 1:
        # catch-all
        catchAllPresent = true
        # Catch all exceptions

        landing.addClause(constNull(g.voidPtrType))
      else:
        for j in 0..ni.len-2:
          let
            etyp =
              if ni[j].isInfixAs(): ni[j][1].typ
              else: ni[j].typ
            eti = g.genTypeInfo(etyp)

          landing.addClause(eti)

    if fin != nil:
      if not catchAllPresent:
        # Catch all exceptions, do finally code, then reraise
        landing.addClause(constNull(g.voidPtrType))

  pad

proc getBadCleanupPad(g: LLGen): llvm.BasicBlockRef =
  # Bad cleanup is called when endCatch raises - basically when the destructor
  # of an exception raises, or other similar situations
  if g.f.badCleanupPad.isNil():
    g.f.badCleanupPad =
      g.b.appendBasicBlockInContext(g.lc, g.nn("cleanup.bad.pad"))

    g.withBlock(g.f.badCleanupPad):
      # if endCatch raises, this is where we land!
      let bad = g.b.buildLandingPad(
        g.getLandingPadTy(), g.getPersonalityFn(), 0,
        g.nn("cleanup.bad.landing"))

      # Pad of last resort - catch everything
      bad.addClause(constNull(g.voidPtrType))

      discard g.callCompilerProc(
        "nlvmBadCleanup", [], noInvoke = true, noReturn = true)
  g.f.badCleanupPad

template withBadCleanupPad(g: LLGen, body: untyped) =
  g.f.nestedTryStmts.add((nil, false, g.getBadCleanupPad(), nil, @[]))
  body
  discard g.f.nestedTryStmts.pop

proc getCleanupPadBlock(g: LLGen): llvm.BasicBlockRef =
  # Cleanup block is used when nlvmEndCatch needs to be called while an
  # exception is about to leave a function boundary

  if g.f.cleanupPad.isNil():
    let f = g.b.getInsertBlock().getBasicBlockParent()
    g.f.cleanupPad = g.b.appendBasicBlockInContext(g.lc, g.nn("cleanup.pad", f))

    g.withBlock(g.f.cleanupPad):
      let cleanup = g.b.buildLandingPad(
        g.getLandingPadTy(), g.getPersonalityFn(), 0, g.nn("cleanup", f))
      cleanup.setCleanup(llvm.True)

      g.withBadCleanupPad(): discard g.callCompilerProc("nlvmEndCatch", [])

      discard g.b.buildResume(cleanup)

  g.f.cleanupPad

proc genNodeTryStmt(g: LLGen, n: PNode, load: bool): LLValue =
  template genOrAssign(n: PNode) =
    if result.v != nil and not n.deepTyp.isEmptyType():
      g.genAssignment(result, n, typ, {needToCopy})
    else:
      g.genNode(n)

  let
    typ = n.deepTyp

  if not typ.isEmptyType():
    result = LLValue(
      v: g.localAlloca(g.llType(typ), g.nn("try.res", n)), storage: OnStack)
    g.buildStoreNull(result.v)
    g.genObjectInit(typ, result.v)

  let
    pad = g.genLandingPad(n, "try.pad")

  g.f.nestedTryStmts.add((n, false, pad, nil, @[]))

  let fin = g.f.nestedTryStmts[^1].fin

  # Generate code inside try block - anything that raises in here will jump
  # to pad
  discard g.f.startBlock(nil, nil)
  genOrAssign(n[0])
  g.f.endBlock()

  # Happy ending - no exception was raised or it was raised and caught
  let tryEnd = g.b.appendBasicBlockInContext(g.lc, g.nn("try.end", n))
  g.b.buildBrFallthrough(tryEnd)

  # While catching stuff, we want exceptions to pass through our cleanup pad
  # so that end catch is called properly
  if g.f.nestedTryStmts.len > 1 and not g.f.nestedTryStmts[^2].inExcept:
    # When we're catching exceptions, any exception that escapes the current
    # scope will need to call `nlvmEndCatch`. This means that the landingpad
    # that's used in invoke calls must make the call in the parent scope - we
    # end up with two landingpad blocks that are identical save for the call.
    # Once cleanup is done, we move on to the handler part that compares type.
    if g.f.nestedTryStmts[^2].extraPads.len > 0:
      # If there are two sibling try blocks nested inside another try block,
      # they can use the same exception cleanup pad as they will be identical.
      # TODO the code keeps a seq of extra pads here because that's what the
      #      clang/c++ eh code generates, but we could get away with a single
      #      entry - investigate!
      g.f.nestedTryStmts[^1].excPad = g.f.nestedTryStmts[^2].extraPads[0].pad
    else:
      g.f.nestedTryStmts[^1].excPad = g.genLandingPad(g.f.nestedTryStmts[^2].n, "try.pad.exc")

      g.withBlock(g.f.nestedTryStmts[^1].excPad):
        g.withBadCleanupPad(): discard g.callCompilerProc("nlvmEndCatch", [])

        g.f.nestedTryStmts[^2].extraPads.add((
          g.f.nestedTryStmts[^1].excPad, g.b.getInsertBlock()))

  else:
    let tmp = g.getCleanupPadBlock()
    g.f.nestedTryStmts[^1].excPad = tmp

  doAssert g.f.nestedTryStmts[^1].excPad != nil
  g.f.nestedTryStmts[^1].inExcept = true

  let
    land =
      if g.f.nestedTryStmts[^1].extraPads.len > 0:
        let
          cmp = g.b.appendBasicBlockInContext(g.lc, g.nn("try.type.cmp", n))

        g.b.positionBuilderAtEnd(cmp)

        let landing = g.b.buildPhi(g.getLandingPadTy(), g.nn("try.pad.phi", n))
        landing.addIncoming([pad.getFirstInstruction()], [pad])

        for (epad, esrc) in g.f.nestedTryStmts[^1].extraPads:
          g.withBlock(esrc):
            discard g.b.buildBr(cmp)
          landing.addIncoming([epad.getFirstInstruction()], [esrc])

        g.withBlock(pad):
          discard g.b.buildBr(cmp)

        landing
      else:
        let landing = pad.getFirstInstruction()
        g.b.positionBuilderAtEnd(pad)
        landing

  let
    landedPtr = g.b.buildExtractValue(land, 0, g.nn("landedptr", n))
    landedTypeId = g.b.buildExtractValue(land, 1, g.nn("landedti", n))

  var catchAllPresent = false

  for i in 1..<n.len:
    let ni = n[i]
    if ni.kind != nkExceptBranch: break

    if ni.len == 1:
      # catch-all
      catchAllPresent = true
      # Catch all exceptions

      discard g.callCompilerProc("nlvmBeginCatch", [landedPtr], true)

      discard g.f.startBlock(nil, nil)
      genOrAssign(ni[0])
      g.f.endBlock()

      if g.b.needsTerminator():
        # Looks like exception was handled, release and jump to end-of-block
        g.withBadCleanupPad(): discard g.callCompilerProc("nlvmEndCatch", [])
        discard g.b.buildBr(tryEnd)

    else:
      for j in 0..ni.len-2:
        # exception handler body will duplicated for every type
        # TODO we could avoid this..

        let
          etyp =
            if ni[j].isInfixAs(): ni[j][1].typ
            else: ni[j].typ
          eti = g.genTypeInfo(etyp)

        # Compare raised type with exception handler type
        let
          eq = g.b.appendBasicBlockInContext(g.lc, g.nn("try.eq", n))
          ne = g.b.appendBasicBlockInContext(g.lc, g.nn("try.ne", n))
          localTypeId = g.callEhTypeIdFor(eti)
          isEq = g.b.buildICmp(
            llvm.IntEQ, landedTypeId, localTypeId, g.nn("try.iseq", n))

        discard g.b.buildCondBr(isEq, eq, ne)

        g.b.positionBuilderAtEnd(eq)

        # Exception type matched, emit handler
        discard g.callCompilerProc("nlvmBeginCatch", [landedPtr], noInvoke = true)

        discard g.f.startBlock(nil, nil)
        genOrAssign(ni[^1])
        g.f.endBlock()

        if g.b.needsTerminator():
          # Looks like exception was handled, release and jump to end-of-block
          g.withBadCleanupPad(): discard g.callCompilerProc("nlvmEndCatch", [])

          discard g.b.buildBr(tryEnd)

        # No match, keep building
        g.b.positionBuilderAtEnd(ne)

  # We're now outside of the try part and the builder is positioned right after
  # the last type check
  discard g.f.nestedTryStmts.pop()

  if fin != nil:
    if not catchAllPresent:
      # If there's no catch-all, the exceptions that are not caught need to
      # have the finally code run as well - in the section above we tried
      # matching all types that were meant to be caught so here we build the
      # finally code in the section where none of the types matched - then
      # reraise.
      discard g.callCompilerProc("nlvmBeginCatch", [landedPtr], noInvoke = true)

      discard g.f.startBlock(nil, nil)
      g.genNode(fin[0])
      g.f.endBlock()

      discard g.callCompilerProc("nlvmReraise", [], noReturn = true)

    g.b.buildBrFallthrough(tryEnd)
    g.b.positionBuilderAtEnd(tryEnd)

    # Emit finalizer code
    discard g.f.startBlock(nil, nil)
    g.genNode(fin[0])
    g.f.endBlock()
  else:
    if not catchAllPresent:
      # TODO if we end up here we entered the landing pad but didn't match
      #      any type - how could that have happened?? Should we resume then?
      # discard g.b.buildResume(land)
      discard g.b.buildUnreachable()

    g.b.buildBrFallthrough(tryEnd)
    g.b.positionBuilderAtEnd(tryEnd)

  result = g.maybeLoadValue(result, load and result.v != nil)

proc genNodeRaiseStmt(g: LLGen, n: PNode) =
  if g.f.nestedTryStmts.len > 0 and g.f.nestedTryStmts[^1].inExcept:
    let fin = g.f.nestedTryStmts[^1].fin
    if fin != nil:
      discard g.f.startBlock(nil, nil)
      g.genNode(fin[0])
      g.f.endBlock()

  if n[0].kind != nkEmpty:
    let
      ax = g.genNode(n[0], true).v
      typ = skipTypes(n[0].typ, abstractPtrs)
      name = g.b.buildGlobalStringPtr(typ.sym.name.s, "raise." & typ.sym.name.s)

    discard g.callCompilerProc("nlvmRaise", [ax, name], noReturn = true)
  else:
    discard g.callCompilerProc("nlvmReraise", [], noReturn = true)

proc blockLeave(g: LLGen, howManyTrys: int) =
  var stack = newSeq[LLEhEntry](0)

  for i in 1..howManyTrys:
    let tryStmt = g.f.nestedTryStmts.pop

    stack.add(tryStmt)

    let fin = tryStmt.fin
    if fin != nil:
      discard g.f.startBlock(nil, nil)
      g.genNode(fin[0])
      g.f.endBlock()

  for i in countdown(howManyTrys-1, 0):
    g.f.nestedTryStmts.add(stack[i])

proc genNodeReturnStmt(g: LLGen, n: PNode) =
  if (n[0].kind != nkEmpty):
    g.genNode(n[0])

  g.blockLeave(g.f.nestedTryStmts.len)

  discard g.b.buildBr(g.section(g.f, secReturn))

  g.b.positionBuilderAtEnd(g.getDeadBlock())

proc genNodeBreakStmt(g: LLGen, n: PNode) =
  p("b", n[0], g.depth)

  var idx = g.f.breakIdx

  if n[0].kind != nkEmpty:
    # named break?
    let sym = n[0].sym
    doAssert(sym.loc.k == locOther)
    idx = sym.position-1
  else:
    # an unnamed 'break' can only break a loop after 'transf' pass:
    while idx >= 0 and not g.f.blocks[idx].isLoop: dec idx
    if idx < 0 or not g.f.blocks[idx].isLoop:
      internalError(g.config, n.info, "no loop to break")

  let s = g.f.blocks[idx]

  g.blockLeave(g.f.nestedTryStmts.len - s.nestedTryStmts)

  # similar to return, there might be more stuff after a break that messes
  # things up - we add an extra block just in case
  # TODO: one case when this happens is with finally blocks, which are
  # currently broken anyway, but this way at least the generated bytecode is
  # valid
  let cont = g.b.appendBasicBlockInContext(g.lc, g.nn("block.cont", n))
  if s.exit == nil:
    s.exit = g.b.appendBasicBlockInContext(g.lc, g.nn("block.exit", n))
  discard g.b.buildBr(s.exit)
  g.b.positionBuilderAtEnd(cont)

proc genNodeBlockStmt(g: LLGen, n: PNode) =
  preserve(g.f.breakIdx):
    g.f.breakIdx = g.f.startBlock(n[0], nil)
    if n[0].kind != nkEmpty:
      # named block?
      var sym = n[0].sym
      sym.loc.k = locOther
      sym.position = g.f.breakIdx+1
    g.genNode(n[1])
    let scope = g.f.endBlock()

    if scope.exit != nil:
      g.b.buildBrFallthrough(scope.exit)
      g.b.positionAndMoveToEnd(scope.exit)

proc genNodeDiscardStmt(g: LLGen, n: PNode) =
  if n[0].kind != nkEmpty:
    discard g.genNode(n[0], true)

proc genNodeStmtListExpr(g: LLGen, n: PNode, load: bool): LLValue =
  for s in n.sons[0..^2]:
    g.genNode(s)
  if n.sons.len > 0:
    g.genNode(n[^1], load)
  else:
    LLValue()

proc genNodeBlockExpr(g: LLGen, n: PNode, load: bool): LLValue =
  preserve(g.f.breakIdx):
    g.f.breakIdx = g.f.startBlock(n[0], nil)
    if n[0].kind != nkEmpty:
      # named block?
      var sym = n[0].sym
      sym.loc.k = locOther
      sym.position = g.f.breakIdx+1

    result = g.genNode(n[1], load)
    let scope = g.f.endBlock()

    if scope.exit != nil:
      g.b.buildBrFallthrough(scope.exit)
      g.b.positionAndMoveToEnd(scope.exit)

proc genNodeClosure(g: LLGen, n: PNode, load: bool): LLValue =
  var
    ax = g.genNode(n[0], true).v
    bx = g.genNode(n[1], true).v

  # TODO workaround for how params are loaded by nkSym - this needs a review
  if bx.typeOfX().getElementType().getTypeKind() == llvm.PointerTypeKind:
    bx = g.b.buildLoad(bx, g.nn("clox.hack", n))

  let
    a = g.b.buildBitCast(ax, g.voidPtrType, g.nn("clox.ptr", n))
    b = g.b.buildBitCast(bx, g.voidPtrType, g.nn("clox.env", n))
    v = g.localAlloca(g.llType(n.typ), g.nn("clox.res", n))

  discard g.b.buildStore(a, g.b.buildGEP(v, [g.gep0, g.gep0]))
  discard g.b.buildStore(b, g.b.buildGEP(v, [g.gep0, g.gep1]))

  g.maybeLoadValue(LLValue(v: v, storage: OnStack), load)

proc genNodeGotoState(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], true).v

  let l = g.config.lastOrd(n[0].typ).toInt # TODO Int128

  let prereturn = g.b.appendBasicBlockInContext(g.lc, g.nn("goto.prereturn", n))

  g.f.blocks[g.f.blocks.len - 1].goto =
    g.b.buildSwitch(ax, prereturn, (l + 1).cuint)

  g.b.positionBuilderAtEnd(prereturn)

  g.blockLeave(g.f.nestedTryStmts.len)
  discard g.b.buildBr(g.section(g.f, secReturn))

  # Sometimes, there's litter after the gotostate switch - add a block for it!
  g.b.positionBuilderAtEnd(g.getDeadBlock())

proc genNodeState(g: LLGen, n: PNode) =
  let state =
    g.b.appendBasicBlockInContext(g.lc, g.nn("state." & $n[0].intVal, n))
  g.b.buildBrFallthrough(state)
  g.b.positionBuilderAtEnd(state)
  for i in 0..g.f.blocks.len-1:
    if g.f.blocks[g.f.blocks.len - i - 1].goto != nil:
      g.f.blocks[g.f.blocks.len - i - 1].goto.addCase(g.constNimInt(n[0].intVal.int), state)
      break

proc genNodeBreakState(g: LLGen, n: PNode): LLValue =
  # TODO C code casts to int* and reads second value.. uh, I guess we should be
  # able to do better
  let a =
    if n[0].kind == nkClosure:
      let
        ax = g.genNode(n[0][1], false).v
        al = g.b.buildLoad(ax, g.nn("load.state.break", n))
      g.b.buildGEP(al, [g.gep0, g.gep1])
    else:
      let
        ax = g.genNode(n[0], false).v
        ag = g.b.buildGEP(ax, [g.gep0, g.gep1])
        al = g.b.buildLoad(ag, g.nn("load.state.break", n))
        ab = g.b.buildBitCast(al, g.primitives[tyInt].pointerType(), "")
      g.b.buildGEP(ab, [g.gep1])

  let s = g.b.buildLoad(a, g.nn("state.break.s", n))
  LLValue(v: g.b.buildICmp(llvm.IntSLT, s, g.ni0, g.nn("state.break.cmp", n)))

proc genNodeTupleConstr(g: LLGen, n: PNode, load: bool): LLValue =
  genNodePar(g, n, load)

proc genSons(g: LLGen, n: PNode) =
  for s in n: g.genNode(s)

proc genNode(g: LLGen, n: PNode, load: bool): LLValue =
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
  of nkChckRangeF: result = g.genNodeChckRangeF(n)
  of nkChckRange64: result = g.genNodeChckRange64(n)
  of nkChckRange: result = g.genNodeChckRange(n)
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
  of nkTryStmt, nkHiddenTryStmt: result = g.genNodeTryStmt(n, load)
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
  of nkBreakState: result = g.genNodeBreakState(n)
  of nkTupleConstr: result = g.genNodeTupleConstr(n, load)

  of nkTypeSection, nkCommentStmt, nkIteratorDef, nkIncludeStmt,
     nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt,
     nkFromStmt, nkTemplateDef, nkMacroDef: discard
  else:
    g.config.internalError(n.info, "Unhandled node: " & $n)

  g.depth -= 1

proc newLLGen(graph: ModuleGraph, tgt: string, tm: TargetMachineRef): LLGen =
  let
    lc = llvm.getGlobalContext()
    name = graph.config.m.fileInfos[graph.config.projectMainIdx.int].shortName
    intType = llvm.intTypeInContext(lc, graph.config.target.intSize.cuint * 8)
    charType = llvm.int8TypeInContext(lc)

  result = LLGen(
    graph: graph,
    lc: lc,
    m: llvm.moduleCreateWithNameInContext(name, lc),
    tm: tm,
    b: llvm.createBuilderInContext(lc),

    cintType: llvm.int32TypeInContext(lc),  # c int on linux
    csizetType: llvm.int64TypeInContext(lc),  # c size_t on linux
    closureType: llvm.structCreateNamed(lc, "llnim.Closure"),
    procPtrType: llvm.int8TypeInContext(lc).pointerType(),  # no void* in llvm
    voidPtrType: llvm.int8TypeInContext(lc).pointerType(),  # no void* in llvm
    jmpbufType: llvm.structCreateNamed(lc, "jmp_buf"),

    strLitFlag: int64(1'i64 shl (graph.config.target.intSize * 8 - 2)),

    attrNoInline: lc.createEnumAttribute(llvm.attrNoInline, 0),
    attrNoReturn: lc.createEnumAttribute(llvm.attrNoReturn, 0),
    attrNoOmitFP: lc.createStringAttribute("no-frame-pointer-elim", "true"),
    attrCold: lc.createEnumAttribute(llvm.attrCold, 0),

    symbols: initTable[int, LLValue](),
    gmarkers: initTable[int, llvm.ValueRef](),
    markers: initTable[SigHash, llvm.ValueRef](),
    nodeInfos: initTable[SigHash, llvm.ValueRef](),
    typeInfos: initTable[SigHash, llvm.ValueRef](),
    types: initTable[SigHash, llvm.TypeRef](),
    sigConflicts: initCountTable[SigHash](),

    tgtExportLinkage:
      if graph.config.target.targetCPU == cpuWasm32: llvm.ExternalLinkage
      else: llvm.llvm.CommonLinkage,
  )

  var g = result

  block:
    proc s(t: TTypeKind, v: llvm.TypeRef) =
      g.primitives[t] = v

    s(tyBool, llvm.int8TypeInContext(lc)) # needs 8 bits for atomics to work...
    s(tyChar, charType)
    s(tyNil, g.voidPtrType)
    # tyTyped appears for example in `echo()` as the element type of the array
    s(tyTyped, g.voidPtrType)
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
    g.jmpBufType, [
      llvm.arrayType(llvm.int64TypeInContext(g.lc), 8),
      llvm.int32TypeInContext(g.lc),
      llvm.int32TypeInContext(g.lc), # padding..
      llvm.arrayType(llvm.int64TypeInContext(g.lc), 16)])
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

    # Magic string, see https://groups.google.com/forum/#!topic/llvm-dev/1O955wQjmaQ
    g.m.addModuleFlag(
      ModuleFlagBehaviorWarning, "Dwarf Version",
      valueAsMetadata(g.constInt32(4)))
    g.m.addModuleFlag(
      ModuleFlagBehaviorWarning, "Debug Info Version",
      valueAsMetadata(g.constInt32(llvm.debugMetadataVersion().int32)))

proc genMain(g: LLGen) =
  g.withBlock(g.section(g.init, secArgs)):
    let
      f = g.init.f

    if g.d != nil:
      let dl = g.lc.dIBuilderCreateDebugLocation(
          1, 1, g.init.ds, nil)
      g.b.setCurrentDebugLocation(g.lc.metadataAsValue(dl))

      let f0 = f.getFirstParam()
      let f1 = f0.getNextParam()
      let argc = g.b.buildAlloca(f0.typeOfX(), g.nn("argc"))
      let argv = g.b.buildAlloca(f1.typeOfX(), g.nn("argv"))
      discard g.b.buildStore(f0, argc)
      discard g.b.buildStore(f1, argv)

      let vd0 = g.d.dIBuilderCreateParameterVariable(
        g.init.ds, "argc", 1,
        g.debugGetFile(g.config.projectMainIdx), 0, g.dtypes[tyInt],
        false, 0)
      discard g.d.dIBuilderInsertDeclareAtEnd(argc, vd0,
        g.d.dIBuilderCreateExpression(nil, 0),
        dl, g.b.getInsertBlock())

      let vd1 = g.d.dIBuilderCreateParameterVariable(
        g.init.ds, "argv", 2, g.debugGetFile(g.config.projectMainIdx), 0,
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

  g.withBlock(g.section(g.init, secReturn)):
    discard g.b.buildRet(constInt(g.cintType, 0, False))

proc loadBase(g: LLGen) =
  let
    base = g.config.prefixDir.string /
      "../nlvm-lib/nlvmbase-$1-$2.ll" %
        [platform.CPU[g.config.target.targetCPU].name,
        platform.OS[g.config.target.targetOS].name]
    m = parseIRInContext(g.lc, base)

  if g.m.linkModules2(m) != 0:
    g.config.internalError("module link failed")

proc runOptimizers(g: LLGen) =
  if {optOptimizeSpeed, optOptimizeSize} * g.config.options == {}:
    return

  let pmb = llvm.passManagerBuilderCreate()

  # See include/llvm/Analysis/InlineCost.h for inlining thresholds
  if optOptimizeSize in g.config.options:
    pmb.passManagerBuilderSetOptLevel(2)
    pmb.passManagerBuilderSetSizeLevel(2)
    # pmb.passManagerBuilderUseInlinerWithThreshold(5)
  else:
    pmb.passManagerBuilderSetOptLevel(3)
    pmb.passManagerBuilderSetSizeLevel(0)
    # pmb.passManagerBuilderUseInlinerWithThreshold(250)

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
  let ext =
    if optCompileOnly in g.config.globalOptions:
      ".ll"
    elif optNoLinking in g.config.globalOptions:
      ".o"
    else:
      ""

  let outFile = g.config.getOutFile(g.config.outFile, ext)

  g.runOptimizers()

  var err: cstring
  if optCompileOnly in g.config.globalOptions:
    if g.m.printModuleToFile(outfile.string, cast[cstringArray](addr(err))) == llvm.True:
      g.config.internalError($err)
    return

  let ofile =
    if optNoLinking in g.config.globalOptions:
      outFile.string
    else:
      g.config.completeCFilePath(AbsoluteFile(project & ".o")).string

  if llvm.targetMachineEmitToFile(g.tm, g.m, ofile, llvm.ObjectFile,
    cast[cstringArray](addr(err))) == llvm.True:
    g.config.internalError($err)
    return

  if optNoLinking in g.config.globalOptions:
    return

  if g.config.selectedGC == gcBoehm:
    g.config.cLinkedLibs.add("gc")

  g.config.addExternalFileToLink(ofile.AbsoluteFile)

  # Linking is a horrible mess - let's reuse the c compiler for now
  lllink(g.config)

proc genForwardedProcs(g: LLGen) =
  # Forward declared proc:s lack bodies when first encountered, so they're given
  # a second pass here
  # Note: ``genProcNoForward`` may add to ``forwardedProcs``
  while g.forwardedProcs.len > 0:
    let
      prc = g.forwardedProcs.pop()
    if sfForward in prc.flags:
      g.config.internalError(prc.info, "still forwarded: " & prc.name.s)

    discard g.genFunctionWithBody(prc)

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
  g.f.sections[secLastBody] = g.b.getInsertBlock()

  let s = pc.sym

  if sfCompileToCpp in s.flags:
    g.config.internalError("Compile-to-c++ not supported (did you use importcpp?)")

  if sfMainModule notin s.flags:
    return n

  let disp = generateMethodDispatchers(graph)
  for x in disp:
    discard g.genFunctionWithBody(x.sym)

  g.genMain()

  g.genForwardedProcs()

  for m in g.markerBody:
    g.genMarkerProcBody(m[0], m[1])
  g.markerBody.setLen(0)

  for m in g.gcRoots:
    g.genGcRegistrar(m.sym, m.v)

  if not g.registrar.isNil:
    g.withFunc(g.registrar):
      if g.f.ds != nil:
        let dl = g.lc.dIBuilderCreateDebugLocation(
            1, 1, g.f.ds, nil)
        g.b.setCurrentDebugLocation(g.lc.metadataAsValue(dl))

      g.finalize()

  g.finalize()

  if g.d != nil:
    g.d.dIBuilderFinalize()

  g.loadBase()

  g.writeOutput(changeFileExt(g.config.projectFull, "").string)

  if g.d != nil:
    g.d.disposeDIBuilder()
    g.d = nil

  g.m.disposeModule()

  n

proc myProcess(b: PPassContext, n: PNode): PNode =
  let pc = LLModule(b)
  let g = pc.g

  let om = g.module
  g.module = pc
  defer: g.module = om

  if g.config.skipCodegen(n): return n

  p("Process", n, 0)
  let newN = transformStmt(g.graph, pc.sym, n)
  g.f.options = g.config.options
  g.genNode(newN)
  g.f.sections[secLastBody] = g.b.getInsertBlock()

  n

proc myOpen(graph: ModuleGraph, s: PSym): PPassContext =
  # In the C generator, a separate C file is generated for every module,
  # but the rules governing what goes into which module are shrouded by a
  # layer of globals and conditionals.
  # Rather than deciphering all that, we simply generate a single module
  # with all the code in it, like the JS generator does.

  # p("Opening", s, 0)

  # TODO: A total hack that needs to go away:
  case s.name.s
  of "pcre": graph.config.cLinkedLibs.add("pcre")
  of "sqlite3": graph.config.cLinkedLibs.add("sqlite3")
  of "openssl":
    graph.config.cLinkedLibs.add("ssl")
    graph.config.cLinkedLibs.add("crypto")

  if graph.backend == nil:
    # Initialize LLVM
    initializeAllAsmPrinters()
    initializeAllTargets()
    initializeAllTargetInfos()
    initializeAllTargetMCs()

    block: # Handle llvm command line arguments
      var llvmArgs = @["nlvm"]
      for v in commandLineParams():
        let c = v.normalize()
        if c.startsWith("--llvm-"):
          llvmArgs.add(c[6..^1])

      if llvmArgs.len() > 1:
        let arr = allocCStringArray(llvmArgs)
        defer: deallocCStringArray(arr)

        parseCommandLineOptions(llvmArgs.len.cint, arr, "")

    # Before wasm32 was added as a CPU, we used nlvm.target - this is still
    # around in some tutorials so leave it around for now - perhaps it makes
    # sense to try to translate classic target triples to nim targets?

    let target =
      if graph.config.existsConfigVar("nlvm.target"):
        let
          tmp = graph.config.getConfigVar("nlvm.target")
          (cpu, os) = parseTarget(tmp)
        graph.config.target.setTarget(os, cpu)
        tmp
      else:
        toTriple(
          graph.config.target.targetCPU, graph.config.target.targetOS)

    var tr: llvm.TargetRef
    discard getTargetFromTriple(target, addr(tr), nil)

    # PIC/PIE is used by default when linking on certain platforms to enable address space randomization:
    # https://stackoverflow.com/q/43367427
    let
      reloc = llvm.RelocPIC
      cgl =
        if optOptimizeSpeed in graph.config.options: llvm.CodeGenLevelAggressive
        else: llvm.CodeGenLevelDefault
      tm = createTargetMachine(
        tr, target, "", "", cgl, reloc, llvm.CodeModelDefault)
      layout = tm.createTargetDataLayout()
      g = newLLGen(graph, target, tm)

    g.m.setModuleDataLayout(layout)
    g.m.setTarget(target)

    graph.backend = g

    let
      llMainType = llvm.functionType(
        g.cintType, [g.cintType, g.primitives[tyCString].pointerType()])
      mainName = # TODO hackish way to not steal the `main` symbol!
        if optNoMain in graph.config.globalOptions:
          ".nlvm.main." & s.name.s
        else:
          "main"
      main = g.m.addFunction(mainName, llMainType)
      f = g.newLLFunc(main, nil)

    if optNoMain in graph.config.globalOptions:
      main.setLinkage(llvm.InternalLinkage) # TODO export it?

    g.init = f
    g.f = f

    if g.d != nil:
      let
        ptrBits = layout.pointerSize() * 8
        types = [
          g.dtypes[tyInt32],
          g.dtypes[tyInt32],
          g.d.dIBuilderCreatePointerType(
            g.d.dIBuilderCreatePointerType(
              g.dtypes[tyChar], ptrBits , ptrBits, ""),
            ptrBits, ptrBits, "")
        ]
      g.f.ds = g.debugFunction(nil, types, main)

    discard g.f.startBlock(nil, g.section(g.f, secReturn))
    g.b.positionBuilderAtEnd(g.section(g.f, secBody))
    LLModule(g: g, sym: s)

  else:
    let g = LLGen(graph.backend)
    assert g.f == g.init
    LLModule(g: g, sym: s)

const llgenPass* = makePass(myOpen, myProcess, myClose)
