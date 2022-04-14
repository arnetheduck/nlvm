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
    aliases,
    ast,
    astalgo,
    bitsets,
    cgmeth,
    ccgutils,
    extccomp,
    idents,
    injectdestructors,
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
    sym: PSym
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

    sym: PSym # The symbol given in myOpen corresponding to the module
    init: LLFunc

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
    cstrings: Table[string, llvm.ValueRef]
    strings: Table[string, llvm.ValueRef]
    resets: Table[SigHash, llvm.ValueRef]

    depth: int

    registrar: LLFunc

    module: LLModule

    modules: seq[LLModule] # modules in position order
    closedModules: seq[LLModule] # modules in the order they were closed

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
proc llPassAsPtr(g: LLGen, s: PSym, retType: PType): bool
proc llType(g: LLGen, typ: PType): llvm.TypeRef
proc llStringType(g: LLGen): llvm.TypeRef
proc llGenericSeqType(g: LLGen): llvm.TypeRef
proc fieldIndex(g: LLGen, typ: PType, sym: PSym): seq[int]
proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef)
proc callErrno(g: LLGen, prefix: string): llvm.ValueRef
proc callCompilerProc(
  g: LLGen, name: string, args: openarray[llvm.ValueRef], noInvoke = false,
  noReturn = false): llvm.ValueRef

proc genFunction(g: LLGen, s: PSym): LLValue
proc genFunctionWithBody(g: LLGen, s: PSym): LLValue
proc genRefAssign(g: LLGen, dest: LLValue, src: llvm.ValueRef)

# Magic expressions
proc genMagicLength(g: LLGen, n: PNode): LLValue

# Node handling
proc genNode(g: LLGen, n: PNode, load: bool = false, tgt = LLValue()): LLValue {.discardable.}

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

  s & fn & ln & col

proc nn(g: LLGen, s: string): string = s
proc nn(g: LLGen, s: string, n: PNode): string =
  if n == nil: g.nn(s) else: g.nn(s, n.info)
proc nn(g: LLGen, s: string, sym: PSym): string =
  if sym == nil: g.nn(s) else: g.nn(s, sym.info)
proc nn(g: LLGen, s: string, v: llvm.ValueRef): string =
  var vn = v.getValueName()
  if vn == nil: vn = ""
  s & "." & $vn
proc nn(g: LLGen, s: string, v: LLValue): string = g.nn(s, v.v)

proc newLLFunc(g: LLGen, f: llvm.ValueRef, sym: PSym): LLFunc =
  LLFunc(
    f: f,
    sym: sym,
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
    let db = b.getCurrentDebugLocation2()
    b.positionBuilderAtEnd(bb)
    body
    b.positionBuilderAtEnd(pre)
    if optCDebug in g.config.globalOptions:
      b.setCurrentDebugLocation2(db)

template withFunc(g: LLGen, llf: LLFunc, body: untyped) =
  block:
    let f = g.f
    g.f = llf
    let db = g.b.getCurrentDebugLocation2()
    body
    g.f = f
    if optCDebug in g.config.globalOptions:
      g.b.setCurrentDebugLocation2(db)

template withModule(g: LLGen, llm: LLModule, body: untyped) =
  block:
    let f = g.module
    g.module = llm
    body
    g.module = f

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
    g: LLGen, v: llvm.ValueRef, nullTyp: llvm.TypeRef, body: untyped): llvm.ValueRef =
  # Fast path for trivial cases
  if v.isNull() > 0:
    constNull(nullTyp)
  else:
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

template withLoop(g: LLGen, size: llvm.ValueRef, name: string, body: untyped) =
  let
    cnt = g.localAlloca(g.primitives[tyInt], g.nn(name & ".cnt"))
    lcmp = g.b.appendBasicBlockInContext(g.lc, g.nn(name & ".cmp"))
    lbody = g.b.appendBasicBlockInContext(g.lc, g.nn(name & ".body"))
    ldone = g.b.appendBasicBlockInContext(g.lc, g.nn(name & ".done"))

  discard g.b.buildStore(g.constNimInt(0), cnt)

  # jump to comparison
  discard g.b.buildBr(lcmp)

  # check index
  g.b.positionBuilderAtEnd(lcmp)
  let
    i {.inject.} = g.b.buildLoad(cnt, g.nn(name & ".i"))
    cond = g.b.buildICmp(llvm.IntULT, i, size, g.nn(name & ".cond"))

  discard g.b.buildCondBr(cond, lbody, ldone)

  # loop body
  g.b.positionBuilderAtEnd(lbody)

  body

  # back to comparison
  let cn = g.b.buildAdd(i, g.constNimInt(1), g.nn(name & ".add"))
  discard g.b.buildStore(cn, cnt)
  discard g.b.buildBr(lcmp)

  # continue at the end
  g.b.positionAndMoveToEnd(ldone)

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
                          tyDistinct, tyRange, tyStatic, tyAlias, tySink,
                          tyInferred, tyOwned}

func assignCopy(sym: PSym): set[TAssignmentFlag] =
  if lfNoDeepCopy in sym.loc.flags: {} else: {needToCopy}

proc typeName(typ: PType): Rope =
  let typ = typ.skipTypes(irrelevantForBackend)
  if typ.sym != nil and typ.kind in {tyObject, tyEnum}:
    rope($typ.kind & '_' & typ.sym.name.s.mangle)
  else:
    rope($typ.kind)

proc supportsCopyMem(typ: PType): bool =
  let t = typ.skipTypes({tyVar, tyLent, tyGenericInst, tyAlias, tySink, tyInferred})
  not (containsGarbageCollectedRef(t) or hasDestructor(t))

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
  name & " " & $n.id & " " & $n.kind & " " & $n.magic & " " & $n.flags & " " &
    $n.loc.flags & " " & $n.info.line & " " & $n.typ & " " & $n.loc.flags

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
    echo(spaces(depth * 2), t, " ", n, " ", g.nn("", n.info))

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

proc constStoreSize(g: LLGen, typ: llvm.TypeRef): llvm.ValueRef =
  let dl = g.m.getModuleDataLayout()
  g.constInt32(dl.storeSizeOfType(typ).int32)

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
  constPtrToInt(gep, g.primitives[tyInt])

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
    g.constInt(size * 8, cs.bitSetToWord(size.int))
  else:
    proc ci8(v: byte): llvm.ValueRef = g.constUInt8(v)
    llvm.constArray(g.primitives[tyUInt8], cs.map(ci8))

proc constNimString(g: LLGen, val: string): llvm.ValueRef =
  let
    s = g.lc.constStringInContext(val)
    ll = g.constNimInt(val.len)
    cap = g.constNimInt(val.len + g.strLitFlag)
    x = llvm.constNamedStruct(g.llGenericSeqType(), [ll, cap])
  llvm.constStructInContext(g.lc, [x, s])

proc constCStringPtr(g: LLGen, val: string): llvm.ValueRef =
  g.cstrings.withValue(val, v) do:
    return v[]
  do:
    let init = g.lc.constStringInContext(val)
    let s = g.m.addPrivateConstant(init.typeOfX(), g.nn(".cstr"))
    s.setInitializer(init)
    let v = constBitCast(s, g.primitives[tyCString])
    g.cstrings[val] = v
    return v

proc constNimStringPtr(g: LLGen, val: string): llvm.ValueRef =
  if val.len == 0:
    return constNull(g.llStringType())

  g.strings.withValue(val, v) do:
    return v[]
  do:
    let
      init = g.constNimString(val)
      s = g.m.addPrivateConstant(init.typeOfX(), g.nn(".str"))
    s.setInitializer(init)
    let v = constBitCast(s, g.llStringType())
    g.strings[val] = v
    return v

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
    return v

  let
    vw = vt.getIntTypeWidth()
    nw = nt.getIntTypeWidth()
  if vw == nw:
    v
  elif vw > nw:
    g.b.buildTrunc(v, nt, g.nn("toe.t", v))
  else:  # vw < nw
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

proc isLargeType(g: LLGen, t: llvm.TypeRef): bool =
  # Large types in loads and stores lead to inefficient codegen - "large" is a
  # bit underdefined in this context, but it seems reasonable to base it off
  # pointers in general
  # See also https://llvm.org/docs/Frontend/PerformanceTips.html#avoid-loads-and-stores-of-large-aggregate-type
  let
    dl = g.m.getModuleDataLayout()
  (t.getTypeKind() in {llvm.ArrayTypeKind, llvm.StructTypeKind}) and
    (dl.storeSizeOfType(t) > (dl.pointerSize() * 8))

proc buildStoreNull(g: LLGen, tgt: llvm.ValueRef) =
  let t = tgt.typeOfX()
  assert t.getTypeKind() == llvm.PointerTypeKind
  let et = t.getElementType()
  # constNull can take up a lot of space - use memset instead for large values
  if g.isLargeType(et):
    g.callMemset(tgt, g.constInt8(0), g.constStoreSize(et))
  else:
    discard g.b.buildStore(constNull(et), tgt)

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
  g.withNotNilOrNull(v, g.primitives[tyInt]):
    let gep = g.buildNimSeqLenGEP(v)
    g.b.buildLoad(gep, g.nn("nilcheck.load", v))

proc getNimSeqDataPtr(g: LLGen, v: llvm.ValueRef, idx: llvm.ValueRef = nil): llvm.ValueRef =
  g.withNotNilOrNull(v, v.typeOfX().getElementType().structGetTypeAtIndex(1).pointerType()):
    g.buildNimSeqDataGEP(v, idx)

proc isObjLackingTypeField(typ: PType): bool =
  (typ.kind == tyObject) and ((tfFinal in typ.flags) and
      (typ.sons[0] == nil) or isPureObject(typ))

proc isInvalidReturnType(g: LLGen, rettype: PType): bool =
  # Large return types lead to inefficient codegen, so we avoid them here - the
  # cgen also points out that types with refs in them need special treatment -
  # it's not certain this is the case, but let's follow the practise..

  if rettype == nil:
    false
  else:
    let t = skipTypes(rettype, typedescInst)
    if t.kind in {tyObject, tyTuple, tyArray} or
        t.kind == tyProc and t.callConv == ccClosure:
      # GC and type fields are only problems when returning directly, apparently
      # In particular, the distinction is needed because the mNewString* magics
      # use different return value types through an importc trick in the
      # declaration of newStringOfCap vs rawNewString etc
      # cgen doesn't support returning any arrays by value - we'll avoid it
      # for arrays  of ref etc but value arrays will hopefully be fine
      # TODO this looks wrong, but let's follow the cgen for now
      g.isLargeType(g.llType(t)) or
        containsGarbageCollectedRef(t) or
          (t.kind == tyObject and not isObjLackingTypeField(t))
    else:
      g.isLargeType(g.llType(t)) and (tfByCopy notin rettype.flags)

proc debugSize(g: LLGen, typ: llvm.TypeRef):
    tuple[typeBits, storeBits, allocBits, abiBits: uint32] =
  let
    dl = g.m.getModuleDataLayout()
  (sizeOfXTypeInBits(dl, typ).uint32, storeSizeOfType(dl, typ).uint32 * 8,
    aBISizeOfType(dl, typ).uint32 * 8,
    preferredAlignmentOfType(dl, typ).uint32 * 8)

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
  let ptrBits = g.m.getModuleDataLayout().pointerSize() * 8
  case typ.kind
  of tyBool: g.dtypes[tyBool]
  of tyChar: g.dtypes[tyChar]
  of tyNil, tyTyped, tyNone: g.dtypes[tyChar] # void*?
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
    let (bits, _, _, _) = g.debugSize(typ)

    g.d.dIBuilderCreateArrayType(bits, 0, et,
              [g.d.dIBuilderGetOrCreateSubrange(0, g.config.lengthOrd(typ).toInt.int64)])
  of tyUncheckedArray:
    let et = g.debugType(typ.elemType)
    g.d.dIBuilderCreateArrayType(0, 0, et,
                [g.d.dIBuilderGetOrCreateSubrange(0, -1)])
  of tyObject: g.debugStructType(typ)
  of tyTuple: g.debugTupleType(typ)
  of tySet:
    let (bits, _, _, _) = g.debugSize(typ)
    if bits <= 8 * 8:
      g.d.dIBuilderCreateBasicType("set" & $bits, bits, DW_ATE_unsigned)
    else:
      g.d.dIBuilderCreateArrayType(
        bits, 0, g.dtypes[tyUInt8],
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
        (bits, _, _, _) = g.debugSize(supt)

        st = g.d.dIBuilderCreateStructType(g.dcu, name,
          file, line.cuint, bits, 0, 0, nil, [], 0, nil, name)
      g.dstructs[sig] = st

      var elems = @[
        g.d.dIBuilderCreateMemberType(g.dcu, "Sup", file, 0, bits, 0, 0, 0,
          sup)
      ]
      if typ.elemType.kind != tyEmpty:
        let
          dt = g.debugType(typ.elemType)

        elems.add(
          g.d.dIBuilderCreateMemberType(g.dcu, "data", file, 0, 0, 0, bits, 0,
            g.d.dIBuilderCreateArrayType(0, 0, dt,[
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
  of tyOpenArray, tyVarargs:
    g.d.dIBuilderCreatePointerType(
      g.debugType(typ.elemType), ptrBits, ptrBits, "openArray")
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
      0, offset, 0, g.debugType(field.typ))
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
    (bits, _, _, _) = g.debugSize(typ)

  # Create struct before setting body in case it's recursive
  result = g.d.dIBuilderCreateStructType(g.dcu, name,
    file, line.cuint, bits, 0, 0, nil, [], 0, nil, name)

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
      (bits, _, alloc, _) = g.debugSize(super)
      member = g.d.dIBuilderCreateMemberType(
        g.dcu, "Sup", g.debugGetFile(g.config.projectMainIdx), 0, bits,
        0, offset, 0, g.debugType(super))
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
    (bits, _, _, _) = g.debugSize(typ)

  result = g.d.dIBuilderCreateStructType(
    g.dcu, name, file, line.cuint, bits, 0, 0, nil, [], 0, nil, name)

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
      mbits, 0, offset, 0, g.debugType(t))
    offset += malloc
    elements.add(member)

  g.d.nimDICompositeTypeSetTypeArray(
    result, g.d.dIBuilderGetOrCreateArray(elements))

proc debugMagicType(g: LLGen, name: string): llvm.MetadataRef =
  g.debugType(g.graph.getCompilerProc(name).typ)

proc debugProcParamType(g: LLGen, sym: PSym, retType: PType): llvm.MetadataRef =
  let
    dt = g.debugType(sym.typ)
  if g.llPassAsPtr(sym, retType):
    let
      ptrBits = g.m.getModuleDataLayout().pointerSize() * 8
    g.d.dIBuilderCreatePointerType(dt, ptrBits, ptrBits, "")
  else:
    dt

proc debugProcType(g: LLGen, typ: PType, closure: bool): seq[llvm.MetadataRef] =
  let retType = if typ[0] == nil: nil
                else: g.debugType(typ[0])
  if retType != nil and g.isInvalidReturnType(typ[0]):
    result.add(nil)
    let ptrBits = g.m.getModuleDataLayout().pointerSize() * 8
    result.add(g.d.dIBuilderCreatePointerType(retType, ptrBits, ptrBits, ""))
  else:
    result.add(retType)

  for param in typ.procParams():
    let at = g.debugProcParamType(param.sym, typ[0])
    result.add(at)

    if skipTypes(param.sym.typ, {tyVar, tyLent, tySink}).kind in {tyOpenArray, tyVarargs}:
      result.add(g.dtypes[tyInt])  # Extra length parameter

  if closure:
    result.add(g.dtypes[tyPointer])

proc debugGetScope(g: LLGen): llvm.MetadataRef =
  if g.f != nil:
    g.f.ds
  else:
    g.dcu

proc debugLocation(g: LLGen, info: TLineInfo): MetadataRef =
  # This scope ensures that even for top-level statements, the right file is
  # used together with the line and column - it happens this way because we
  # dump such statements from all nim modules into a single `main` function.
  # It's a bit of a hack, better would be to control this more tightly so as to
  # avoid creating all these scopes - we should also be creating
  # a new lexical scope whenever a new block begins - probably somewhere around
  # startBlock..
  let
    scope = g.d.dIBuilderCreateLexicalBlockFile(
      g.debugGetScope(), g.debugGetFile(info.fileIndex), 0)

  g.lc.dIBuilderCreateDebugLocation(
    max(info.line, 1).cuint, max(info.col, 1).cuint, scope, nil)

proc debugUpdateLoc(g: LLGen, n: PNode) =
  if g.d == nil: return
  if n == nil:
    g.b.setCurrentDebugLocation2(
      g.lc.dIBuilderCreateDebugLocation(1, 1, g.debugGetScope(), nil))
  else:
    let
      dlm = g.debugLocation(n.info)
    g.b.setCurrentDebugLocation2(dlm)

proc debugUpdateLoc(g: LLGen, sym: PSym) =
  if g.d == nil: return
  if sym == nil:
    g.b.setCurrentDebugLocation2(
      g.lc.dIBuilderCreateDebugLocation(1, 1, g.debugGetScope(), nil))
  else:
    let
      dlm = g.debugLocation(sym.info)
    g.b.setCurrentDebugLocation2(dlm)

proc debugVariable(g: LLGen, sym: PSym, v: llvm.ValueRef, argNo = -1) =
  if g.d == nil: return

  let
    dt = g.debugType(sym.typ)
    scope = g.debugGetScope()
    vd =
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
  of tyBool, tyChar, tyNil, tyTyped, tyNone: g.primitives[typ.kind]
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

proc getInitFunc(llm: LLModule): LLFunc =
  if llm.init == nil:
    let
      g = llm.g
      name =
        if {sfSystemModule, sfMainModule} * llm.sym.flags == {}:
          llm.sym.owner.name.s.mangle & "_" & llm.sym.name.s.mangle
        else:
          llm.sym.name.s.mangle

    let initType = llvm.functionType(llvm.voidTypeInContext(g.lc), [])
    llm.init = g.newLLFunc(
      g.m.addFunction("." & name & ".init", initType), nil)
    if g.d != nil:
      llm.init.ds = g.debugFunction(llm.sym, [], llm.init.f)

    llm.init.f.setLinkage(llvm.InternalLinkage)

    llm.init.sections[secLastBody] = g.section(llm.init, secBody)
    discard llm.init.startBlock(nil, g.section(llm.init, secReturn))

  llm.init

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
        if g.d != nil:
          let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, g.f.ds, nil)
          g.b.setCurrentDebugLocation2(dl)

        g.b.buildBrFallthrough(cur)
    last = cur

  ret.moveBasicBlockAfter(g.f.f.getLastBasicBlock())

  for dead in g.f.deadBlocks:
    if dead.needsTerminator():
      g.withBlock(dead):
        if g.d != nil:
          let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, g.f.ds, nil)
          g.b.setCurrentDebugLocation2(dl)
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
  const preventInheritance = true # Inheritance not supported by llvm yet
  case n.kind
  of nkCharLit..nkNilLit:
    result = true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = isDeepConstExprLL(n[1])
  of nkCurly, nkBracket, nkPar, nkTupleConstr, nkObjConstr, nkClosure, nkRange:
    for i in ord(n.kind == nkObjConstr)..<n.len:
      if not isDeepConstExprLL(n[i]): return false
    if n.typ.isNil: result = true
    else:
      let t = n.typ.skipTypes({tyGenericInst, tyDistinct, tyAlias, tySink, tyOwned})
      if t.kind in {tyRef, tyPtr} or tfUnion in t.flags: return false
      if t.kind == tyObject:
        if preventInheritance and t[0] != nil:
          result = false
        elif isCaseObj(t.n):
          result = false
        else:
          result = true
      else:
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

  n.kind in nkCallKinds

template withRecCase(n: PNode, v: llvm.ValueRef, body: untyped) =
  let
    kind = n[0].sym
    gep {.inject.} = g.b.buildGEP(v, [g.gep0, g.constGEPIdx(start)], g.nn("mk.kind", kind))
    vk = g.b.buildLoad(gep, g.nn("mk.kind.load", kind))
    caseend = g.b.appendBasicBlockInContext(g.lc, g.nn("mk.kind.end", kind))

  inc(start)
  var hasElse = false

  for i in 1..<n.len:
    let
      branch {.inject.} = n[i]
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

    body

    discard g.b.buildBr(caseend)
    g.b.positionBuilderAtEnd(x)

  if not hasElse:
    discard g.b.buildBr(caseend)
  g.b.positionAndMoveToEnd(caseend)

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef)
proc genMarker(g: LLGen, typ: PType, n: PNode, v, op: llvm.ValueRef, start: var int) =
  if n == nil: return

  case n.kind
  of nkRecList:
    for i in 0..<n.len:
      g.genMarker(typ, n[i], v, op, start)
  of nkRecCase:
    withRecCase(n, v):
      g.genMarker(typ, branch.lastSon, v, op, start)

  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType(): return
    var gep = g.b.buildGEP(v, [g.gep0, g.constGEPIdx(start)], g.nn("mk", field))
    if field.typ.skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
      gep = g.b.buildLoad(gep, g.nn("mk.load", field))

    g.genMarker(field.typ, gep, op)
    inc(start)
  else: g.config.internalError(n.info, "genMarker()")

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ == nil: return
  case typ.kind
  of tyGenericInst, tyGenericBody, tyTypeDesc, tyAlias, tyDistinct, tyInferred,
     tySink, tyOwned:
    g.genMarker(typ.lastSon(), v, op)
  of tyArray:
    let
      arraySize = g.constNimInt(g.config.lengthOrd(typ[0]))

    g.withLoop(arraySize, "mk.arr"):
      var gep =
        if v.typeOfX().isArrayPtr(): g.b.buildGEP(v, [g.gep0, i], g.nn("while.data"))
        else: g.b.buildGEP(v, [i], g.nn("while.data"))

      if typ[1].skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
        gep = g.b.buildLoad(gep, g.nn("mk.arr.load"))

      genMarker(g, typ[1], gep, op)

  of tyObject:
    var start = 0
    if typ.len > 0 and typ[0] != nil:
      let gep = g.b.buildGEP(v, [g.gep0, g.gep0])
      g.genMarker(typ[0].skipTypes(skipPtrs), gep, op)
      start = 1 # Skip super type field
    elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
      start = 1 # Skip m_type in inheritable root object

    if typ.n != nil:
      g.genMarker(typ, typ.n, v, op, start)

  of tyTuple:
    for i in 0..<typ.len:
      var gep = g.b.buildGEP(v, [g.gep0, g.constGEPIdx(i)])
      if typ[i].skipTypes(abstractInst).kind in
          {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
        gep = g.b.buildLoad(gep, g.nn("mk.tuple.load"))

      genMarker(g, typ[i], gep, op)

  of tyRef, tySequence:
    discard g.callCompilerProc("nimGCVisit", [v, op])
  of tyString:
    if tfHasAsgn notin typ.flags:
      discard g.callCompilerProc("nimGCVisit", [v, op])
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

  g.withLoop(seqlen, "mk.seq"):
    var gep = g.buildNimSeqDataGEP(v, i)
    if typ[0].skipTypes(abstractInst).kind in
        {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
      gep = g.b.buildLoad(gep, g.nn("mk.seq.load"))
    genMarker(g, typ[0], gep, op)

proc genMarkerProcBody(g: LLGen, f: llvm.ValueRef, typ: PType) =
  let llf = g.newLLFunc(f, nil)
  if g.d != nil:
    llf.ds = g.debugFunction(
      typ.sym, [nil, g.dtypes[tyPointer], g.dtypes[tyInt]], f)

  g.withFunc(llf):
    g.debugUpdateLoc(typ.sym)

    g.withBlock(g.section(g.f, secBody)):

      var v = f.getFirstParam()
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
        if typ[0].skipTypes(abstractInst).kind in {tyRef, tyPtr, tyVar, tyLent, tyString, tySequence}:
          v = g.b.buildLoad(v, g.nn("mk.proc.load"))
        g.genMarker(typ[0], v, op)

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
    typ = typ.skipTypes(abstractInstOwned)
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
    g.debugUpdateLoc(sym)

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
  if g.config.selectedGC in  {gcMarkAndSweep, gcHooks, gcV2, gcRefc} and
      optOwnedRefs notin g.config.globalOptions and
      sym.typ.containsGarbageCollectedRef() and sym.id notin g.gmarkers:
    g.gcRoots.add((sym, v))

proc genGcRegistrar(g: LLGen, sym: PSym, v: llvm.ValueRef) =
    # This is a bit backwards - better would be to rewrite nimRegisterGlobalMarker!
    if g.registrar.isNil:
      let
        name = ".nlvm.registrar"
        registrarType = llvm.functionType(llvm.voidTypeInContext(g.lc), @[], false)
        registrar = g.m.addFunction(name, registrarType)

      registrar.setLinkage(llvm.InternalLinkage)

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
          let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, g.f.ds, nil)
          g.b.setCurrentDebugLocation2(dl)
        discard g.b.buildRetVoid()

    let prc = g.genGlobalMarkerProc(sym, v)

    g.withFunc(g.registrar):
      g.debugUpdateLoc(sym)
      g.withBlock(g.section(g.f, secBody)):
        discard g.callCompilerProc("nimRegisterGlobalMarker", [prc])

proc genTypeInfoInit(g: LLGen, t: PType, ntlt, lt: llvm.TypeRef,
                     baseVar, nodeVar, finalizerVar, markerVar,
                     deepcopyVar: llvm.ValueRef): llvm.ValueRef =
  let
    dl = g.m.getModuleDataLayout()
    sizeVar = if lt == nil: g.ni0 else: g.constNimInt(dl.aBISizeOfType(lt).int)
    kind =
      if t.isObjLackingTypeField(): tyPureObject
      elif t.kind == tyProc and t.callConv == ccClosure: tyTuple
      else: t.kind
    kindVar = g.constInt8(int8(ord(kind)))

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
  if isDefined(g.config, "nimTypeNames"):
    var typename = typeToString(if t.typeInst != nil: t.typeInst
                                else: t, preferName)
    if typename == "ref object" and t.skipTypes(skipPtrs).sym != nil:
      typename = "anon ref object from " & g.config$t.skipTypes(skipPtrs).sym.info
    let lltn = g.constCStringPtr(typename)
    let values = [
      sizeVar, kindVar, flagsVar, baseVar, nodeVar, finalizerVar,
      markerVar, deepcopyVar, lltn, constPointerNull(ntlt.pointerType()),
      g.ni0, g.ni0
    ]
    llvm.constNamedStruct(ntlt, values)
  else:
    let values = [
      sizeVar, kindVar, flagsVar, baseVar, nodeVar, finalizerVar,
      markerVar, deepcopyVar
    ]

    llvm.constNamedStruct(ntlt, values)

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

  result =
    if isDefined(g.config, "nimTypeNames"):
      # nimTypeNames also includes counters for number of allocations, meaning
      # the type info is written to :O
      let x = g.m.addGlobal(ntlt, name)
      x.setLinkage(llvm.PrivateLinkage)
      x
    else:
      g.m.addPrivateConstant(ntlt, name)

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

proc llPassAsPtr(g: LLGen, s: PSym, retType: PType): bool =
  # We strive to replicate a similar parameter passing strategy as Nim
  # because for FFI in particular, existing code implicitly tends to depend on
  # the details herein - see also ccgIntroducedPtr

  let
    pt = s.typ.skipTypes(typedescInst + tyUserTypeClasses)

  if tfByRef in pt.flags:
    true
  elif tfByCopy in pt.flags:
    # Hack to work around jmp_buf being an array but represented as an object
    # in the std lib
    if $s.typ.sym.loc.r == "jmp_buf":
      true
    else:
      false
  else:
    case pt.kind
    of tyObject:
      if s.typ.sym != nil and sfForward in s.typ.sym.flags:
        # forwarded objects are *always* passed by pointers for consistency!
        true
      elif (optByRef in s.options) or (getSize(g.config, pt) > g.config.target.floatSize * 3):
        true           # requested anyway
      elif retType != nil and retType.kind == tyLent:
        true
      elif (tfFinal in pt.flags) and (pt[0] == nil):
        false          # no need, because no subtyping possible
      else:
        true           # ordinary objects are always passed by reference,
                                # otherwise casting doesn't work

    of tyTuple:
      if retType != nil and retType.kind == tyLent:
        true
      else:
        (getSize(g.config, pt) > g.config.target.floatSize*3) or
          (optByRef in s.options)

    of tyArray, tyOpenArray, tyUncheckedArray, tyVarargs:
      true
    of tySet:
      let size = g.config.getSize(pt).cuint
      size > 8
    of tyProc:
      pt.callConv == ccClosure
    else:
      false

proc llProcParamType(g: LLGen, s: PSym, retType: PType): llvm.TypeRef =
  let
    llt = g.llType(s.typ)
  if g.llPassAsPtr(s, retType):
    llt.pointerType()
  else:
    llt

proc paramStorageLoc(param: PSym): TStorageLoc =
  if param.typ.skipTypes({tyVar, tyTypeDesc, tyLent}).kind notin {
          tyArray, tyOpenArray, tyUncheckedArray, tyVarargs}:
    OnStack
  else:
    OnUnknown

proc llProcType(g: LLGen, typ: PType, closure: bool): llvm.TypeRef =
  var
    retType = if typ[0] == nil: llvm.voidType()
              else: g.llType(typ[0])
    argTypes = newSeq[llvm.TypeRef]()

  if g.isInvalidReturnType(typ[0]):
    argTypes.add(retType.pointerType())
    if typ[0].sym != nil:
      incl(typ[0].sym.loc.flags, lfIndirect)
      typ[0].sym.loc.storage = OnUnknown
    retType = llvm.voidType()

  for param in typ.procParams():
    fillLoc(param.sym.loc, locParam, param, param.sym.name.s.mangle.rope,
      param.sym.paramStorageLoc)

    if g.llPassAsPtr(param.sym, typ[0]):
      incl(param.sym.loc.flags, lfIndirect)
      param.sym.loc.storage = OnUnknown

    let at = g.llProcParamType(param.sym, typ[0])
    argTypes.add(at)

    if skipTypes(param.sym.typ, {tyVar, tyLent, tySink}).kind in {tyOpenArray, tyVarargs}:
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

    if n[0].sym.name.s == sym.name.s: return @[start]
    inc(start)
    for j in 1..<n.len:
      result = g.fieldIndexRecs(n[j].lastSon, sym, start)
      if result.len > 0: return
  of nkSym:
    if n.sym.name.s == sym.name.s: return @[start]
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
      start = 1 # Skip super type field
    elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
      start = 1 # Skip m_type in inheritable root object

  let n = typ.n
  return g.fieldIndexRecs(n, sym, start)

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
  if t.getTypeKind() != llvm.PointerTypeKind: return false

  let et = t.getElementType()
  if et.getTypeKind() != llvm.StructTypeKind: return false

  if et.countStructElementTypes() != 2: return false

  let elems = et.getStructElementTypes()
  if elems[0] != g.llGenericSeqType(): return false

  if elems[1].getTypeKind() != llvm.PointerTypeKind and
     elems[1].getTypeKind() != llvm.ArrayTypeKind: return false

  true

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
      return g.getNimSeqDataPtr(ax)

  if ltk == PointerTypeKind and
      skipTypes(t, {tyVar, tyLent}).kind in {tyVarargs, tyOpenArray, tyArray, tyUncheckedArray} and
      g.llType(t.elemType) == lt.getElementType() and
      g.isNimSeqLike(at):
    return g.getNimSeqDataPtr(ax)

  if ltk == PointerTypeKind and atk == PointerTypeKind:
    return g.b.buildBitCast(ax, lt, g.nn("pre", ax))

  if ltk == IntegerTypeKind and atk == IntegerTypeKind and
      at.getIntTypeWidth() != lt.getIntTypeWidth():
    return g.buildTruncOrExt(ax, lt, unsigned)

  if ltk in llvm.HalfTypeKind..llvm.PPC_FP128TypeKind and
      atk in llvm.HalfTypeKind..llvm.PPC_FP128TypeKind:
    if ltk > atk:
      return g.b.buildFPExt(ax, lt, g.nn("pre.fpe", ax))
    elif ltk < atk:
      return g.b.buildFPTrunc(ax, lt, g.nn("pre.fpt", ax))

  if ltk in llvm.HalfTypeKind..llvm.PPC_FP128TypeKind and
      atk in [llvm.IntegerTypeKind]:
    if unsigned:
      return g.b.buildSIToFP(ax, lt, g.nn("pre.sf", ax))
    else:
      return g.b.buildUIToFP(ax, lt, g.nn("pre.uf", ax))

  if ltk in [llvm.IntegerTypeKind] and
      atk in llvm.HalfTypeKind..llvm.PPC_FP128TypeKind:
    if unsigned:
      return g.b.buildFPToUI(ax, lt, g.nn("pre.fu", ax))
    else:
      return g.b.buildFPToSI(ax, lt, g.nn("pre.fs", ax))

  ax

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
    let a = g.preCast(false, v, param.sym.typ, g.llProcParamType(param.sym, sym.typ[0]))
    args[i] = a

    if skipTypes(param.sym.typ, {tyVar, tyLent, tySink}).kind in {tyOpenArray, tyVarargs}:
      i += 1 # Extra length parameter
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

  g.b.buildCall(f, [v])

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
      divzero = g.b.buildICmp(llvm.IntEQ, bx, constInt(ax.typeOfX(), 0, llvm.False), "")

    g.callRaise(divzero, "raiseDivByZero")

    let bo =
      if op == llvm.SDiv:
        let
          opfn = if i64: "nimDivInt64" else: "nimDivInt"
          res = g.localAlloca(ax.typeOfX(), g.nn("res", a))
          isover = g.buildI1(g.callCompilerProc(opfn, [ax, bx, res]))

        g.callRaise(isover, "raiseOverflow")
        g.b.buildLoad(res, "")
      else:
        g.b.buildBinOp(op, ax, bx, "")

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
      pname = g.constCStringPtr(ename)

    discard g.b.buildStore(pname, tgt)

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
  t.kind in {tyArray, tySet, tyTuple, tyObject} or
    (t.kind == tyProc and t.callConv == ccClosure)

proc supportsMemset(typ: PType): bool =
  supportsCopyMem(typ) and analyseObjectWithTypeField(typ) == frNone

proc callReset(g: LLGen, typ: PType, v: LLValue)
proc genResetT(g: LLGen, typ: PType, v: LLValue)

proc genResetN(g: LLGen, v: LLValue, n: PNode, typ: PType, start: var int) =
  if n == nil: return

  case n.kind
  of nkRecList:
    for s in n.sons:
      g.genResetN(v, s, typ, start)
  of nkRecCase:
    withRecCase(n, v.v):
      g.genResetN(v, lastSon(branch), typ, start)

    g.buildStoreNull(gep)
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType(): return
    var gep = g.b.buildGEP(v, [g.gep0, g.constGEPIdx(start)], g.nn("reset", field))
    g.genResetT(field.typ, gep)
    inc(start)
  else:
     g.config.internalError("Unexpected kind in genResetN: " & $typ.kind)

proc genResetT(g: LLGen, typ: PType, v: LLValue) =
  let typ = typ.skipTypes(irrelevantForBackend)
  case typ.kind
  of tyArray:
    if supportsMemset(typ):
      g.buildStoreNull(v.v)
    else:
      let
        et = typ.elemType
        arraySize = g.constNimInt(lengthOrd(g.config, typ[0]))
      g.withLoop(arraySize, "reset.arr"):
        let gep = g.b.buildGEP(v, [g.gep0, i], g.nn("reset.gep", v))
        g.callReset(et, gep)

  of tyObject:
    if supportsMemset(typ):
      g.buildStoreNull(v.v)
    else:
      var start = 0

      if typ.len > 0 and typ[0] != nil:
        let gep = g.b.buildGEP(v, [g.gep0, g.gep0], g.nn("reset", v))
        g.callReset(typ[0], gep)
        start = 1 # Skip super type field
      elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
        start = 1 # Skip m_type in inheritable root object

      g.genResetN(v, typ.n, typ, start)
  of tyTuple:
    if supportsMemset(typ):
      g.buildStoreNull(v.v)
    else:
      for i in 0..<typ.len:
        var gep = g.b.buildGEP(v, [g.gep0, g.constGEPIdx(i)], g.nn("reset", v))
        g.callReset(typ[i], gep)
  of tyString, tyRef, tySequence:
    g.genRefAssign(v, constNull(v.v.typeOfX().getElementType()))

  of tyProc:
    if typ.callConv == ccClosure:
      let p = g.b.buildGEP(v.v, [g.gep0, g.gep0])
      g.buildStoreNull(p)

      let e = g.b.buildGEP(v, [g.gep0, g.gep1], g.nn("reset", v))
      g.genRefAssign(e, constNull(v.v.typeOfX().getElementType()))

    else:
      g.buildStoreNull(v.v)

  of tyChar, tyBool, tyEnum, tyInt..tyUInt64, tyCString, tyPointer, tyPtr,
      tyVar, tyLent:
    g.buildStoreNull(v.v)
  else:
     g.config.internalError("Unexpected kind in genResetT: " & $typ.kind)

proc genResetFunc(g: LLGen, typ: PType): llvm.ValueRef =
  let
    typ = skipTypes(typ, abstractVarRange)
    sig = hashType(typ)

  g.resets.withValue(sig, reset) do:
    return reset[]

  let
    name = ".reset." & g.llname(typ, sig)
    llt = g.llType(typ)
    ft = llvm.functionType(llvm.voidTypeInContext(g.lc), [llt.pointerType], false)

  let f = g.m.addFunction(name, ft)
  g.resets[sig] = f

  f.getParam(0).setValueName("v")

  # Because we generate only one module, we can tag all functions internal
  f.setLinkage(llvm.InternalLinkage)

  let llf = g.newLLFunc(f, nil)

  if g.d != nil:
    let
      dt = g.debugType(typ)

    llf.ds = g.debugFunction(nil, [nil, dt], f)

  g.withFunc(llf):
    g.debugUpdateLoc(typ.sym)

    g.withBlock(g.section(g.f, secBody)):
      g.genResetT(typ, LLValue(v: f.getParam(0)))
      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      discard g.b.buildRetVoid()

    g.finalize()

  return f

proc callReset(g: LLGen, typ: PType, v: LLValue) =
  let
    typ = skipTypes(typ, abstractVarRange + irrelevantForBackend)

  if supportsMemset(typ): # Fast path
    g.buildStoreNull(v.v)
  elif typ.kind in {tyString, tyRef, tySequence}:
      g.genRefAssign(v, constNull(v.v.typeOfX().getElementType()))
  else:
    discard g.b.buildCall(g.genResetFunc(typ), [v.v])

proc resetLoc(g: LLGen, typ: PType, v: LLValue) =
  let typ = skipTypes(typ, abstractVarRange)
  let containsGcRef = containsGarbageCollectedRef(typ)
  if not isComplexValueType(typ):
    if containsGcRef:
      g.genRefAssign(v, constNull(v.v.typeOfX().getElementType()))
    else:
      g.buildStoreNull(v.v)
  else:
    if optNilCheck in g.f.options:
      discard g.callCompilerProc("chckNil", [v.v])

    if v.storage != OnStack and containsGcRef:
      g.callReset(typ, v)
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

  let ft = g.llProcType(s.typ, typ.callConv == ccClosure)
  let f = g.m.addFunction(name, ft)

  if sfNoReturn in s.flags:
    f.addFuncAttribute(g.attrNoReturn)

  if typ.callConv == ccNoInline:
    f.addFuncAttribute(g.attrNoInline)

  # This attribute hopefully works around
  # https://github.com/nim-lang/Nim/issues/10625
  f.addFuncAttribute(g.attrNoOmitFP)

  if s.name.s in [
      "sysFatal", "raiseOverflow", "raiseDivByZero", "raiseFloatInvalidOp",
      "raiseFloatOverflow", "raiseAssert", "raiseRangeError",
      "raiseIndexError", "raiseIndexError2", "raiseIndexError3",
      "raiseFieldError"]:
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

  # Although we only generate one LLVM module, we must use the correct Nim
  # module context when generating function context so that global init order is
  # preserved - this gets tricky in particular when dealing with inline modules
  # which we ignore for now
  let module = g.modules[s.getModule().position]

  g.withModule(module): g.withFunc(f):
    var ret: llvm.ValueRef

    discard g.f.startBlock(s.ast, g.section(f, secReturn))

    g.f.options = s.options

    if g.d != nil:
      let arr = g.debugProcType(typ, typ.callConv == ccClosure)
      g.f.ds = g.debugFunction(s, arr, result.v)
    g.debugUpdateLoc(s.ast)

    g.withBlock(g.section(f, secArgs)):
      var i = 0

      # Fake return
      if sfPure notin s.flags and typ[0] != nil:
        let resNode = s.ast[resultPos]
        let res = resNode.sym
        if sfNoInit in s.flags: incl(res.flags, sfNoInit)
        if not g.isInvalidReturnType(res.typ):
          let resv = g.genLocal(resNode)
          ret = resv.v
          g.initLocalVar(res, res.typ, ret, false)
        else:
          let arg = result.v.getParam(i.cuint)
          arg.setValueName("Result")
          g.symbols[res.id] = LLValue(v: arg)
          g.initLocalVar(res, res.typ, arg, false)
          i += 1

      # Function arguments

      for param in typ.procParams():
        p("a", param, g.depth + 1)
        p("a", param.sym.typ, g.depth + 2)

        let arg = result.v.getParam(i.cuint)
        arg.setValueName(param.sym.llName)

        let av = g.localAlloca(arg.typeOfX(), g.nn("arg", arg))
        discard g.b.buildStore(arg, av)

        g.debugVariable(param.sym, av, i + 1)

        g.symbols[param.sym.id] =
          LLValue(v: av, lode: param, storage: param.sym.loc.storage)

        i += 1

        if skipTypes(param.sym.typ, {tyVar, tyLent, tySink}).kind in {tyOpenArray, tyVarargs}:
          let argLen = result.v.getParam(i.cuint)
          argLen.setValueName(param.sym.llName & "len")

          let avLen = g.localAlloca(argLen.typeOfX(), g.nn("argLen", argLen))
          discard g.b.buildStore(argLen, avLen)
          g.symbols[-param.sym.id] =
            LLValue(v: avLen, lode: param, storage: OnStack)
          i += 1

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
      var procBody = transformBody(g.graph, s, cache = false)
      if sfInjectDestructors in s.flags:
        procBody = injectDestructorCalls(g.graph, s, procBody)

      g.genNode(procBody)
      g.b.buildBrFallthrough(g.section(g.f, secReturn))

      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      if ret != nil:
        discard g.b.buildRet(g.b.buildLoad(ret, g.nn("load.result")))
      else:
        if sfNoReturn in s.flags:
          discard g.b.buildUnreachable()
        else:
          discard g.b.buildRetVoid()

    g.finalize()

proc getOrdering(n: PNode): llvm.AtomicOrdering =
  if n.kind == nkSym:
    case n.sym.name.s
    of "ATOMIC_RELAXED": llvm.AtomicOrderingMonotonic
    of "ATOMIC_CONSUME": llvm.AtomicOrderingAcquire # TODO clang uses this for __atomic_add_fetch, but what about other ops?
    of "ATOMIC_ACQUIRE": llvm.AtomicOrderingAcquire
    of "ATOMIC_RELEASE": llvm.AtomicOrderingRelease
    of "ATOMIC_ACQ_REL": llvm.AtomicOrderingAcquireRelease
    of "ATOMIC_SEQ_CST": llvm.AtomicOrderingSequentiallyConsistent
    else: llvm.AtomicOrderingSequentiallyConsistent
  else:
    llvm.AtomicOrderingSequentiallyConsistent

proc genFakeCall(g: LLGen, n: PNode, o: var LLValue): bool =
  let nf = n[0]
  if nf.kind != nkSym: return false

  let s = nf.sym
  if s.originatingModule().name.s == "system":
    if s.name.s == "atomicLoad":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], false).v
      let ord = getOrdering(n[3])
      let ld = g.b.buildLoad(p0, g.nn("a.load"))
      ld.setOrdering(ord)
      ld.setAlignment(1) # TODO
      discard g.b.buildStore(ld, p1)
      return true

    if s.name.s == "atomicLoadN":
      let p0 = g.genNode(n[1], false).v
      let ord = getOrdering(n[2])
      o = LLValue(v: g.b.buildLoad(p0, g.nn("a.load.n")))
      o.v.setAlignment(1) # TODO
      o.v.setOrdering(ord)
      return true

    if s.name.s == "atomicStore":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], false).v
      let ord = getOrdering(n[3])
      let ld = g.b.buildLoad(p1, g.nn("a.load"))
      let ax = g.b.buildStore(ld, p0)
      ax.setOrdering(ord)
      ax.setAlignment(1.cuint)  # TODO(j) align all over the place.
      return true

    if s.name.s == "atomicStoreN":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], true).v
      let ord = getOrdering(n[3])
      let ax = g.b.buildStore(p1, p0)
      ax.setOrdering(ord)
      ax.setAlignment(1.cuint)  # TODO(j) align all over the place.
      return true

    if s.name.s == "atomicAddFetch":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], true).v
      let ord = getOrdering(n[3])
      o = LLValue(v: g.b.buildAtomicRMW(
        llvm.AtomicRMWBinOpAdd, p0, p1, ord, llvm.False))
      o.v = g.b.buildAdd(o.v, p1, g.nn("atomic.addf", n))
      return true

    if s.name.s == "atomicSubFetch":
      let p0 = g.genNode(n[1], false).v
      let p1 = g.genNode(n[2], true).v
      let ord = getOrdering(n[3])
      o = LLValue(v: g.b.buildAtomicRMW(
        llvm.AtomicRMWBinOpSub, p0, p1, ord, llvm.False))
      o.v = g.b.buildSub(o.v, p1, g.nn("atomic.subf", n))
      return true

    if s.name.s == "atomicThreadFence":
      let ord = getOrdering(n[1])
      o = LLValue(v: g.b.buildFence(ord, llvm.False, ""))
      return true

    if s.name.s == "cas":
      let p0 = g.genNode(n[1], true).v
      let p1 = g.genNode(n[2], true).v
      let p2 = g.genNode(n[3], true).v
      let x = g.b.buildAtomicCmpXchg(p0, p1, p2,
        llvm.AtomicOrderingSequentiallyConsistent,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False)
      o = LLValue(v: g.buildI8(
        g.b.buildExtractValue(x, 1.cuint, g.nn("cas.b", n))))
      return true

    if s.name.s == "atomicCompareExchangeN":
      let p0 = g.genNode(n[1], true).v
      let p1 = g.genNode(n[2], true).v
      let p2 = g.genNode(n[3], true).v
      let ord1 = getOrdering(n[5])
      let ord2 = getOrdering(n[6])
      let l1 = g.b.buildLoad(p1, g.nn("ace.1"))
      let x = g.b.buildAtomicCmpXchg(p0, l1, p2, ord1, ord2, llvm.False)
      o = LLValue(v: g.buildI8(
        g.b.buildExtractValue(x, 1.cuint, g.nn("cas.b", n))))
      return true

    if s.name.s == "atomicCompareExchange":
      let p0 = g.genNode(n[1], true).v
      let p1 = g.genNode(n[2], true).v
      let p2 = g.genNode(n[3], true).v
      let ord1 = getOrdering(n[5])
      let ord2 = getOrdering(n[6])
      let l1 = g.b.buildLoad(p1, g.nn("ace.1"))
      let l2 = g.b.buildLoad(p2, g.nn("ace.1"))
      let x = g.b.buildAtomicCmpXchg(p0, l1, l2, ord1, ord2, llvm.False)
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

proc genBoundsCheck(g: LLGen, arr, len, a, b: llvm.ValueRef) =
  let
    diff = g.b.buildSub(b, a, g.nn("bc.diff", arr))
    minusOne = constInt(diff.typeOfX, not culonglong(0), llvm.True)
    diffne = g.b.buildICmp(
      llvm.IntNE, minusOne, diff, g.nn("bc.diffne", arr))
    aok = g.b.buildICmp(llvm.IntUGE, a, len, g.nn("bc.aok", arr))
    bok = g.b.buildICmp(llvm.IntUGE, b, len, g.nn("bc.bok", arr))
    aorb = g.b.buildBinOp(llvm.Or, aok, bok, g.nn("bc.aorb", arr))
    cond = g.b.buildBinOp(llvm.And, diffne, aorb, g.nn("bc.cond", arr))

  g.callRaise(cond, "raiseIndexError")

proc genBoundsCheckArray(g: LLGen, arr, firstOrd, lastOrd, a, b: llvm.ValueRef) =
  # For some reason, these are different
  let
    diff = g.b.buildSub(b, a, g.nn("bca.diff", arr))
    minusOne = constInt(diff.typeOfX, not culonglong(0), llvm.True)
    diffne = g.b.buildICmp(
      llvm.IntNE, minusOne, diff, g.nn("bca.diffne", arr))
    difflt = g.b.buildICmp(
      llvm.IntSLT, diff, minusOne, g.nn("bc.diffne", arr))
    alt = g.b.buildICmp(llvm.IntSLT, a, firstOrd, g.nn("bca.alt", arr))
    agt = g.b.buildICmp(llvm.IntSGT, a, lastOrd, g.nn("bca.agt", arr))
    blt = g.b.buildICmp(llvm.IntSLT, b, firstOrd, g.nn("bca.alt", arr))
    bgt = g.b.buildICmp(llvm.IntSGT, b, lastOrd, g.nn("bca.agt", arr))
    or0 = g.b.buildBinOp(llvm.Or, alt, agt, g.nn("bca.or0", arr))
    or1 = g.b.buildBinOp(llvm.Or, or0, blt, g.nn("bca.or1", arr))
    or2 = g.b.buildBinOp(llvm.Or, or1, bgt, g.nn("bca.or2", arr))
    or3 = g.b.buildBinOp(llvm.Or, or2, difflt, g.nn("bca.or3", arr))
    cond = g.b.buildBinOp(llvm.And, diffne, or3, g.nn("bca.cond", arr))

  g.callRaise(cond, "raiseIndexError")

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
    let s = if s.kind == nkHiddenDeref: s[0] else: s
    g.buildLoadValue(g.symbols[-s.sym.id]).v

proc genCallArgs(g: LLGen, n: PNode, fxt: llvm.TypeRef, ftyp: PType, extra: int): seq[llvm.ValueRef] =
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

    if param.sym.typ.isCompileTimeOnly(): continue

    let pt = parTypes[args.len + extra]

    var v: llvm.ValueRef

    var q = skipConv(pr)
    var skipped = false
    while q.kind == nkStmtListExpr and q.len > 0:
      skipped = true
      q = q.lastSon

    if skipTypes(param.sym.typ, abstractVar + {tyStatic}).kind in {tyOpenArray, tyVarargs}:
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
            idx = g.b.buildSub(bx, first, g.nn("slice.subb", bx))
          if optBoundsCheck in g.f.options:
            let
              last = constInt(
                bx.typeOfX(), g.config.lastOrd(ty).toInt64.culonglong, llvm.True)

            g.genBoundsCheckArray(ax.v, first, last, bx, cx)
          v = g.b.buildGEP(ax, [g.gep0, idx], g.nn("arr.arg" & $i, q)).v

        of tyOpenArray, tyVarargs:
          let
            ax = g.genNode(q[1], false)
          if optBoundsCheck in g.f.options:
            let tot = g.lenOpenArray(q[1])
            g.genBoundsCheck(ax.v, tot, bx, cx)

          v = g.b.buildGEP(ax, [bx], g.nn("oa.arg" & $i, q)).v

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
            let tot = g.loadNimSeqLen(a.v)
            g.genBoundsCheck(a.v, tot, bx, cx)
          v = g.getNimSeqDataPtr(a.v, bx)
        else: g.config.internalError(n.info, "unknown slice type " & $ty.kind)

        len = g.b.buildSub(cx, bx, g.nn("slice.sub", n))
        len = g.b.buildAdd(len, constInt(len.typeOfX(), 1, llvm.False), g.nn("slice.add", n))
      else:
        case pr.typ.skipTypes(abstractVar+{tyStatic}).kind
        of tyString, tySequence:
          v = g.genNode(pr, true).v
          if pr.typ.skipTypes(abstractInst).kind in {tyVar, tyLent}:
            v = g.b.buildLoad(v, g.nn("call.seq.var", n))
          len = g.loadNimSeqLen(v)
          len = g.b.buildZExt(len, g.primitives[tyInt], g.nn("call.seq.len.ext", n))
          v = g.getNimSeqDataPtr(v)
        of tyOpenArray, tyVarargs:
          v = g.genNode(p, param.sym.typ.kind in {tyVar, tyLent, tySink}).v
          let s = if q.kind == nkHiddenDeref: q[0] else: q
          len = g.b.buildLoad(g.symbols[-s.sym.id].v, g.nn("call.seq.oa.len", n))
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
          of tyArray:
            v = g.genNode(p, true).v
            len = g.constNimInt(g.config.lengthOrd(pr.typ.lastSon))
          else:
            g.config.internalError(n.info, "Unhandled ref length: " & $pr.typ.lastSon())
        else:
          g.config.internalError(n.info, "Unhandled length: " & $pr.typ)

      if v.typeOfX() != g.llProcParamType(param.sym, ftyp[0]):
        v = g.b.buildBitCast(v, g.llProcParamType(param.sym, ftyp[0]), g.nn("call.open", v))

      args.add(v)
      args.add(len)
    else:
      v = g.genNode(p, not g.llPassAsPtr(param.sym, ftyp[0])).v

      # We need to use the type from the function, because with multimethods,
      # it looks like the type in param.sym.typ changes during compilation!
      # seen with tmultim1.nim
      v = g.preCast(g.isUnsigned(p.deepTyp), v, param.sym.typ, pt)
      args.add(v)

  args

proc preventNrvo(g: LLGen, le, ri: PNode): bool =
  # from cgen, this detects when it is safe to use an existing memory location
  # to collect results from a call
  proc locationEscapes(g: LLGen; le: PNode; inTryStmt: bool): bool =
    var n = le
    while true:
      # do NOT follow nkHiddenDeref here!
      case n.kind
      of nkSym:
        # we don't own the location so it escapes:
        if n.sym.owner != g.f.sym:
          return true
        elif inTryStmt and sfUsedInFinallyOrExcept in n.sym.flags:
          # it is also an observable store if the location is used
          # in 'except' or 'finally'
          return true
        return false
      of nkDotExpr, nkBracketExpr, nkObjUpConv, nkObjDownConv,
          nkCheckedFieldExpr:
        n = n[0]
      of nkHiddenStdConv, nkHiddenSubConv, nkConv:
        n = n[1]
      else:
        # cannot analyse the location; assume the worst
        return true

  if le != nil:
    for i in 1..<ri.len:
      let r = ri[i]
      if isPartOf(le, r) != arNo: return true
    # we use the weaker 'canRaise' here in order to prevent too many
    # annoying warnings, see #14514
    if canRaise(ri[0]) and
        locationEscapes(g, le, g.f.nestedTryStmts.len > 0):
      message(g.config, le.info, warnObservableStores, $le)

proc genCall(g: LLGen, le, n: PNode, load: bool, tgt: LLValue): LLValue =
  # The `tgt` variable contains information about the placement of the result of
  # the call - in certain cases, the return value of the function can be
  # placed directly in `tgt` in which case the ordinary `LLValue` returned will
  # be `nil`.
  if g.genFakeCall(n, result):
    return

  let
    nf = n[namePos]
    typ = nf.typ.skipTypes(abstractInstOwned)
    fx = g.genNode(nf, true)
    nfpt = g.llProcType(typ)
    nft = nfpt.pointerType()
    retty = nfpt.getReturnType()
    retArgType =
      if typ[0] == nil: llvm.voidType()
      else: g.llType(typ[0])
    retArgs =
      if g.isInvalidReturnType(typ[0]):
        if tgt.v != nil and tgt.v.typeOfX() == retArgType.pointerType() and
            not g.preventNrvo(le, n):
          @[tgt.v]
        else:
          let tmp = g.localAlloca(retArgType, g.nn("call.res.stack", n))
          g.callMemset(
            tmp, g.constInt8(0),
            g.constStoreSize((tmp.typeOfX().getElementType())))
          @[tmp]
      else:
        @[]

  var callres: llvm.ValueRef
  if typ.callConv == ccClosure:
    let
      args = g.genCallArgs(n, nfpt, typ, retArgs.len)
      prc = g.b.buildExtractValue(fx, 0, g.nn("call.clo.prc.ptr", n))
      env = g.b.buildExtractValue(fx, 1, g.nn("call.clo.env.ptr", n))

    if tfIterator in typ.flags:
      let
        cft = g.llProcType(typ, true).pointerType()
        cfx = g.b.buildBitCast(prc.v, cft, g.nn("call.iter.prc", n))
        clargs = retArgs & args & @[env.v]
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
        res = g.buildCallOrInvokeBr(fxc, cloend, retArgs & args)

      g.b.positionBuilderAtEnd(cloenv)

      let
        cft = g.llProcType(typ, true).pointerType()
        cfx = g.b.buildBitCast(prc.v, cft, g.nn("call.clo.prc", n))
        clargs = retArgs & args & @[env.v]
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
      args = g.genCallArgs(n, fxc.typeOfX().getElementType(), typ, retArgs.len)
      varname =
        if retty.getTypeKind() != llvm.VoidTypeKind: g.nn("call.res", n) else: ""
    callres = g.buildCallOrInvoke(fxc, retArgs & args, varname)

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
    g.maybeLoadValue(LLValue(v: v), load)
  elif retArgs.len > 0:
    if retArgs[0] == tgt.v: # Consumed by RVO
      LLValue()
    else:
      g.maybeLoadValue(LLValue(v: retArgs[0]), load)
  else:
    LLValue(v: callres)

proc genMagicCall(g: LLGen, n: PNode, load: bool, tgt: LLValue): LLValue =
  let s = n[namePos].sym
  if lfNoDecl notin s.loc.flags:
    let m = g.graph.getCompilerProc($s.loc.r)
    if m == nil: g.config.internalError(n.info, "Missing magic: " & $s.loc.r)
    discard g.genFunctionWithBody(m)

  g.genCall(nil, n, load, tgt)

proc genRefAssign(g: LLGen, dest: LLValue, src: llvm.ValueRef) =
  if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
      not usesWriteBarrier(g.config):
    discard g.b.buildStore(g.b.buildBitCast(
      src, dest.v.typeOfX().getElementType(), g.nn("refasgn", dest.v)), dest.v)
  elif dest.storage == OnHeap:
    discard g.callCompilerProc("asgnRef", [dest.v, src])
  else:
    discard g.callCompilerProc("unsureAsgnRef", [dest.v, src])

proc callGenericAssign(
    g: LLGen, dest, src: LLValue, ty: PType, flags: TAssignmentFlags) =
  if needToCopy notin flags or
      tfShallow in ty.skipTypes(abstractVarRange).flags:
    if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
        not usesWriteBarrier(g.config):
      g.callMemcpy(dest.v, src.v, g.constStoreSize(dest.v.typeOfX.getElementType()))
    else:
      discard g.callCompilerProc(
        "genericShallowAssign", [dest.v, src.v, g.genTypeInfo(ty)])
  else:
    discard g.callCompilerProc(
      "genericAssign", [dest.v, src.v, g.genTypeInfo(ty)])

proc genAssignment(g: LLGen, dest, src: LLValue, typ: PType,
    flags: TAssignmentFlags) =
  # Assign src to dest - src needs to be a value or pointer depending on what
  # loadAssignment returns for `typ`
  if src.v == nil or src.v.typeOfX().getTypeKind() == llvm.VoidTypeKind:
    # When RVO kicks in or when a branch of a control-flow expression that is
    # guaranteed to not be returned from but still has to be generated,
    # we may end up with a nil here
    return
  let destt = dest.v.typeOfX()

  if destt.getTypeKind() != llvm.PointerTypeKind:
    g.config.internalError("Ptr required in genAssignment: " & $dest)

  let
    destet = destt.getElementType()
    ty = typ.skipTypes(abstractRange + tyUserTypeClasses + {tyStatic})

  case ty.kind
  of tyRef:
    g.genRefAssign(dest, src.v)
  of tySequence:
    if (needToCopy notin flags and src.storage != OnStatic) or
        canMove(g, src.lode):
      g.genRefAssign(dest, src.v)
    else:
      discard g.callCompilerProc(
        "genericSeqAssign", [dest.v, src.v, g.genTypeInfo(ty)])
  of tyString:
    if (needToCopy notin flags and src.storage != OnStatic) or
        canMove(g, src.lode):
      g.genRefAssign(dest, src.v)
    else:
      if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
          not usesWriteBarrier(g.config):
        let srcc = g.callCompilerProc("copyString", [src.v])
        discard g.b.buildStore(
          g.b.buildBitCast(srcc, destet, g.nn("asgn.xc", src)), dest.v)
      elif dest.storage == OnHeap:
        let destl = g.b.buildLoad(dest.v, g.nn("asgnr.old", dest))
        let srcc = g.callCompilerProc("copyStringRC1", [src.v])
        discard g.b.buildStore(
          g.b.buildBitCast(srcc, destet, g.nn("asgnr.xc", src)), dest.v)
        g.withNotNil(destl):
          discard g.callCompilerProc("nimGCunrefNoCycle", [destl])
      else:
        let srcc = g.callCompilerProc("copyString", [src.v])
        discard g.callCompilerProc("unsureAsgnRef", [dest.v, srcc])
  of tyProc:
    if ty.containsGarbageCollectedRef():
      let
        destp = g.b.buildGEP(dest, [g.gep0, g.gep0], g.nn("asgnr.dest.p", dest))
        srcp = g.b.buildExtractValue(src, 0, g.nn("asgnr.p", src))
      discard g.b.buildStore(
        g.b.buildBitCast(
          srcp.v, destp.v.typeOfX().getElementType(), g.nn("asgnr.pc", src)), destp.v)

      let
        deste = g.b.buildGEP(dest, [g.gep0, g.gep1], g.nn("asgnr.dest.e", src))
        srce = g.b.buildExtractValue(src, 1, g.nn("asgnr.e", src))
      g.genRefAssign(deste, srce.v)
    else:
      discard g.b.buildStore(
        g.b.buildBitCast(src.v, destet, g.nn("asgnr.xc")), dest.v)
  of tyTuple, tyArray:
    if ty.supportsCopyMem():
      g.callMemcpy(dest.v, src.v, g.constStoreSize(destet))
    else:
      g.callGenericAssign(dest, src, ty, flags)

  of tyObject:
    if ty.supportsCopyMem() and ty.isObjLackingTypeField():
      g.callMemcpy(dest.v, src.v, g.constStoreSize(destet))
    else:
      g.callGenericAssign(dest, src, ty, flags)

  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      discard g.b.buildStore(src.v, dest.v)
    else:
      g.callMemcpy(dest.v, src.v, g.constStoreSize(destet))
  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar, tyLent, tyNil:
    let pc = g.preCast(g.isUnsigned(typ), src.v, typ, destet)
    discard g.b.buildStore(
      g.b.buildBitCast(pc, destet, g.nn("asgnr.c", src)), dest.v)

  else: g.config.internalError("genAssignment: " & $ty.kind)

proc loadAssignment(g: LLGen, typ: PType): bool =
  # Depending on the type, either a pointer or a value should be passed to
  # the right-hand side in genAssignment
  let ty = typ.skipTypes(abstractRange + tyUserTypeClasses + {tyStatic})
  case ty.kind
  of tyRef:
    true
  of tySequence:
    true
  of tyString:
    true
  of tyProc:
    true
  of tyTuple, tyArray:
    false
  of tyObject:
    false
  of tyOpenArray, tyVarargs:
    g.config.internalError("TODO " & $typ)
    false
  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8:
      true
    else:
      false

  of tyPtr, tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar, tyLent:
    true
  of tyNil:
    true
  else:
    g.config.internalError("loadAssignment: " & $typ)
    false

proc isSeqLike(n: PNode): bool =
  case n.kind
  of nkStrLit..nkTripleStrLit: true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    n[1].isSeqLike()
  else:
    n.typ.skipTypes(irrelevantForBackend).kind == tySequence

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef

proc genConstSeq(g: LLGen, n: PNode): llvm.ValueRef =
  let typ = n.typ.skipTypes(irrelevantForBackend)
  if typ.elemType.kind == tyEmpty or n.len == 0:
    llvm.constNull(g.llType(typ))
  else:
    let
      et = g.llType(typ.elemType)

    var vals = newSeq[llvm.ValueRef](n.len)
    for i, s in n.sons:
      vals[i] =
        if s.isSeqLike():
          # Need pointer for string literals
          g.genNode(s, true).v
        else:
          g.genConstInitializer(s)
    let s = constArray(et, vals)
    if typ.kind in {tyArray, tyUncheckedArray}:
      s
    else:
      let
        ll = g.constNimInt(vals.len)
        cap = g.constNimInt(vals.len + g.strLitFlag)
        x = llvm.constNamedStruct(g.llGenericSeqType(), [ll, cap])

      llvm.constStructInContext(g.lc, [x, s])

proc genConstObjConstr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    typ = n.typ.skipTypes(abstractInst + irrelevantForBackend)
    t = g.llType(typ)

  if typ.kind == tyRef: g.config.internalError(n.info, "no const objs with refs")

  var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())

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

  for i in 0..<vals.len:
    if isNil(vals[i]):
      vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))

  constNamedStruct(t, vals)

proc genConstTupleConstr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)

  var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())

  for i in 0 ..< n.len:
    let s = n[i]
    if s.isSeqLike():
      # Need pointer for string literals
      vals[i] = g.genNode(s, true).v
    else:
      vals[i] = g.genConstInitializer(s)

  for i in 0..<vals.len:
    if isNil(vals[i]):
      vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))

  constNamedStruct(t, vals)

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef =
  case n.kind
  of nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    g.genConstInitializer(n[1])
  of nkCurly: g.constNimSet(n)
  of nkBracket: g.genConstSeq(n)
  of nkObjConstr: g.genConstObjConstr(n)
  of nkTupleConstr, nkPar, nkClosure: g.genConstTupleConstr(n)
  of nkCharLit..nkFloat128Lit, nkNilLit: g.genNode(n, true).v
  of nkStrLit..nkTripleStrLit:
    if n.typ.skipTypes(
        abstractVarRange + {tyStatic, tyUserTypeClass, tyUserTypeClassInst}).kind == tyString:
      g.constNimString(n.strVal)
    else:
      g.lc.constStringInContext(n.strVal)
  of nkEmpty: constNull(g.llType(n.typ))
  else:
    g.config.internalError(n.info, "Can't gen const initializer " & $n.kind)
    quit 1

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
    let
      lx = g.loadAssignment(sym.typ)
      bx = g.genNode(init, lx, result)

    g.genAssignment(result, bx, sym.typ, sym.assignCopy)

  if sfGlobal notin sym.flags:
    g.symbols[sym.id] = result

proc genCallOrNode(g: LLGen, le, ri: PNode, ax: LLValue): LLValue =
  let lx = g.loadAssignment(le.typ)
  if ri.kind in nkCallKinds and
      (ri[0].kind != nkSym or ri[0].sym.magic == mNone):
    g.genCall(le, ri, lx, ax)
  else:
    g.genNode(ri, lx, ax)

proc genAsgn(g: LLGen, n: PNode, fast: bool) =
  if nfPreventCg in n.flags:
    return

  if n[0].kind == nkSym and sfGoto in n[0].sym.flags:
    g.config.internalError(n.info, "Goto variables not supported")
  elif optFieldCheck in g.f.options and isDiscriminantField(n[0]):
    var dotExpr = n[0]
    if dotExpr.kind == nkCheckedFieldExpr: dotExpr = dotExpr[0]
    let
      ax = g.genNode(n[0], false)
      bx = g.genNode(n[1], true)

    if optTinyRtti notin g.config.globalOptions:
      let field = dotExpr[1].sym
      # TODO genDiscriminantCheck(p, a, tmp, dotExpr[0].typ, field)
      message(g.config, n.info, warnCaseTransition)
    g.genAssignment(ax, bx, n[0].typ, if fast: {} else: {needToCopy})
  else:
    let
      le = n[0]
      ri = n[1]
    let
      ax = g.genNode(le, false)
      bx = g.genCallOrNode(le, ri, ax)

    # TODO the cgen uses a fast assignment when assigning the return value
    #      of a call - this code emulates that but we should really mark
    #      the LLValue instead so as to proparely propagate this fact from
    #      calls nested in no-ops etc
    g.genAssignment(
      ax, bx, le.typ, if fast or ri.kind in nkCallKinds: {} else: {needToCopy})

proc genCallMagic(g: LLGen, n: PNode, load: bool, tgt: LLValue): LLValue =
  # Some magics have implementations in the std lib that we can use - make sure
  # it's been processed!
  discard g.genFunctionWithBody(n[namePos].sym).v

  g.genCall(nil, n, load, tgt)

proc genMagicHigh(g: LLGen, n: PNode): LLValue =
  let len = g.genMagicLength(n).v
  LLValue(v: g.b.buildSub(
    len, llvm.constInt(len.typeOfX(), 1, llvm.False), g.nn("high", len)))

proc genMagicSizeOf(g: LLGen, n: PNode): LLValue =
  let
    t = n[1].typ.skipTypes({tyTypeDesc})
    dl = g.m.getModuleDataLayout()

  LLValue(v: g.constNimInt(dl.aBISizeOfType(g.llType(t)).int))

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
    dl = g.m.getModuleDataLayout()
    sizeExpr = g.constNimInt(dl.aBISizeOfType(lbt).int)
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
    g.genRefAssign(dest, src)

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
    lbt = g.llType(bt)
    dl = g.m.getModuleDataLayout()
    sizeOfExpr = g.constNimInt(dl.aBISizeOfType(lbt).int)

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
    src = g.callCompilerProc("newObj", [ti, sizeOfExpr])
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
    g.genRefAssign(dest, src)

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
    let tot = g.localAlloca(g.llType(n.typ), g.nn("card.tot", n))
    g.buildStoreNull(tot)

    let size = g.constNimInt(size.int)
    g.withLoop(size, "card"):
      let
        ai = g.b.buildLoad(g.b.buildGEP(ax, [i]), g.nn("card.ax", n))
        a = g.callCtpop(ai, 1)
        b = g.b.buildLoad(tot, g.nn("card.tot.load", n))
        c = g.b.buildAdd(
          g.buildTruncOrExt(a, b.typeOfX(), true), b, g.nn("card.add", n))
      discard g.b.buildStore(c, tot)

    LLValue(v: g.b.buildLoad(tot, g.nn("card", n)))

proc genMagicChr(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v

  let t = g.llType(n.typ)
  LLValue(v:
    if t != ax.typeOfX(): g.b.buildTrunc(ax, t, g.nn("chr.t", n))
    else: ax
  )

proc genMagicGCref(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], true).v
  g.withNotNil(ax):
    discard g.callCompilerProc("nimGCref", [ax])

proc genMagicGCunref(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], true).v
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
    let isnan = g.b.buildFCmp(llvm.RealUNO, v, v, g.nn("isnan", n))
    g.callRaise(isnan, "raiseFloatInvalidOp")
  if optInfCheck in g.f.options:
    # same as cgen: v != 0.0 and (v * 0.5 == v)
    let
      notZero = g.b.buildFCmp(llvm.RealUNE, v, constReal(v.typeOfX(), cdouble(0)), g.nn("iszero", n))
      half = g.b.buildFMul(v, constReal(v.typeOfX(), cdouble(0.5)), g.nn("half", n))
      isEq = g.b.buildFCmp(llvm.RealOEQ, v, half, g.nn("iseq", n))
      isinf = g.b.buildBinOp(llvm.And, notZero, isEq, g.nn("isinf", n))
      v64 = g.b.buildFPExt(v, g.primitives[tyFloat64], g.nn("fpext", v))
    g.callRaise(isinf, "raiseFloatOverflow", [v64])

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

proc genMagicEqCString(g: LLGen, n: PNode, load: bool, tgt: LLValue): LLValue =
  g.genCallMagic(n, load, tgt) # Implemented in std lib

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

proc genMagicDotDot(g: LLGen, n: PNode, load: bool, tgt: LLValue): LLValue =
  g.genCallMagic(n, load, tgt) # Implemented in std lib

proc genMagicAppendStrCh(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true).v
    a = g.buildLoadValue(ax)
    ret = g.callCompilerProc("addChar", [a.v, bx])

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
  let newstr = g.callCompilerProc("resizeString", [tgt.v, tgtlen])
  g.genRefAssign(tgtp, newstr)

  # Copy data
  for i in 2..<n.len:
    let sx = exprs[i - 2]

    let s = n[i]
    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [newstr, sx])
    else:
      discard g.callCompilerProc("appendString", [newstr, sx])

proc genMagicAppendSeqElem(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    seqType = n[1].typ.skipTypes({tyVar, tyLent})
    # Evaluate n[2] before calling incrSeqV3 as it will reset the length of the
    # old instance when it grows!
    bx = g.genNode(n[2], g.loadAssignment(seqType.elemType))
    a = g.buildLoadValue(ax)
    ap = g.b.buildGEP(a, [g.gep0, g.gep0], g.nn("seq.add.gep", n))
    newseq = g.callCompilerProc("incrSeqV3", [ap.v, g.genTypeInfo(seqType)])
    tgt = LLValue(
      v: g.b.buildBitCast(newseq, a.v.typeOfX(), g.nn("seq.add.new", n)),
      storage: OnHeap)
    lenp = g.buildNimSeqLenGEP(tgt.v) # guaranteed not nil!
    len = g.b.buildLoad(lenp, g.nn("load.seq.add.last", n))

  g.genRefAssign(ax, tgt.v)

  let newlen = g.b.buildAdd(
    len, llvm.constInt(len.typeOfX(), 1, llvm.False), g.nn("seq.add.newlen", n))
  discard g.b.buildStore(newlen, lenp)

  g.genAssignment(g.buildNimSeqDataGEP(tgt, len), bx, seqType.elemType,
    {needToCopy})

# Here, we need to emulate the C compiler and generate comparisons instead of
# sets, else we'll have crashes when out-of-range ints are compared against
# curlies

# from C compiler
proc fewCmps(g: LLGen, s: PNode): bool =
  # this function estimates whether it is better to emit code
  # for constructing the set or generating a bunch of comparisons directly
  if s.kind != nkCurly: g.config.internalError(s.info, "fewCmps")
  if (g.config.getSize(s.typ) <= g.config.target.intSize) and (nfAllConst in s.flags):
    false            # it is better to emit the set generation code
  elif elemType(s.typ).kind in {tyInt, tyInt16..tyInt64}:
    true             # better not emit the set if int is basetype!
  else:
    s.len <= 8  # 8 seems to be a good value

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
    newstr = g.callCompilerProc("setLengthStr", [a.v, bx])

  g.genRefAssign(ax, newstr)

proc genMagicSetLengthSeq(g: LLGen, n: PNode) =
  let
    axn = if n[1].kind in {nkAddr, nkHiddenAddr}: n[1][0] else: n[1]
    ax = g.genNode(axn, false)
    bx = g.genNode(n[2], true).v
    a = g.buildLoadValue(ax)
    ap = g.b.buildGEP(a.v, [g.gep0, g.gep0])
    typ = n[1].typ.skipTypes({tyVar, tyLent} + abstractInst)
    x = g.callCompilerProc("setLengthSeqV2", [ap, g.genTypeInfo(typ), bx])

  g.genRefAssign(ax, x)

proc genMagicParallel(g: LLGen, n: PNode) =
  let n2 = g.graph.liftParallel(g.module.sym, n)
  g.genNode(n2)

proc genMagicSwap(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], false)
    lx = g.loadAssignment(n[1].typ)
    t = g.llType(n[1].typ)
    tmpx = LLValue(v: g.localAlloca(t, g.nn("swap.tmp", n)), storage: OnStack)
  g.buildStoreNull(tmpx.v)
  g.genObjectInit(n[1].typ, tmpx.v)
  g.genAssignment(tmpx, g.maybeLoadValue(ax, lx), n[1].typ, {})
  g.genAssignment(ax, g.maybeLoadValue(bx, lx), n[1].typ, {})
  g.genAssignment(bx, g.maybeLoadValue(tmpx, lx), n[1].typ, {})

proc genMagicMove(g: LLGen, n: PNode, load: bool): LLValue =
  g.genNode(n[1], load)

proc skipAddr(n: PNode): PNode =
  if n.kind in {nkAddr, nkHiddenAddr}: n[0] else: n

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
  let ax = g.genNode(n[1], false)
  g.callReset(n[1].typ, ax)

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
    typ = n[1].typ.skipTypes(abstractInst).elemType()
    lx = g.loadAssignment(typ)

  g.buildStoreNull(tmp.v)
  g.genNewSeqAux(tmp, n.typ, g.constNimInt(l))
  # In-flight values can be considered to live OnHeap
  let tmpl = g.buildLoadValue(tmp)
  let src = g.genNode(n[1], true)

  for i in 0..<l.toInt():
    let tgt = g.buildNimSeqDataGEP(tmpl, g.constGEPIdx(i))
    let srcg = g.b.buildGEP(src, [g.constGEPIdx(i)], g.nn("arrtoseq.gep", n))
    g.genAssignment(
      tgt, g.maybeLoadValue(srcg, lx), typ, {needToCopy})

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
      g.callMemcpy(ax, bx, g.constStoreSize(ax.typeOfX().getElementType()))
  of tyPointer, tyChar, tyBool, tyEnum, tyCString,
     tyInt..tyUInt64, tyRange, tyVar, tyLent:
    let bx = g.genNode(n[2], true).v
    discard g.b.buildStore(bx, ax)
  else:
    g.config.internalError(n.info, "genDeepCopy: " & $ty.kind)

proc genMagicGetTypeInfo(g: LLGen, n: PNode): LLValue =
  let typ = n[1].typ.skipTypes(abstractVarRange)
  LLValue(v: g.genTypeInfo(typ))

proc genMagic(g: LLGen, n: PNode, load: bool, tgt: LLValue): LLValue =
  var op = n[0].sym.magic
  p("genMagic " & $op, n[0], g.depth + 1)
  case op
  of mHigh: result = g.genMagicHigh(n)
  of mSizeOf: result = g.genMagicSizeOf(n)
  of mOf: result = g.genMagicOf(n)
  of mEcho: g.genMagicEcho(n)
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
  of mLePtr: result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtPtr: result = g.genMagicCmpI(n, llvm.IntULT)
  of mXor: result = g.genMagicCmpI(n, llvm.IntNE)
  of mEqCString: result = g.genMagicEqCString(n, load, tgt)
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
  of mConStrStr: result = g.genMagicConStrStr(n)
  of mDotDot: result = g.genMagicDotDot(n, load, tgt)
  of mAppendStrCh: g.genMagicAppendStrCh(n)
  of mAppendStrStr: g.genMagicAppendStrStr(n)
  of mAppendSeqElem: g.genMagicAppendSeqElem(n)
  of mInSet: result = g.genMagicInSet(n)
  of mRepr: result = g.genMagicRepr(n)
  of mExit: discard g.genCall(nil, n, false, LLValue())
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
  of mNewString, mNewStringOfCap, mParseBiggestFloat:
    result = g.genMagicCall(n, load, tgt)
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

    var toload = g.llPassAsPtr(sym, g.f.sym.typ[0])
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
      g.constNimStringPtr(n.strVal)
    else:
      g.constCStringPtr(n.strVal)

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

proc genNodeCall(g: LLGen, n: PNode, load: bool, tgt: LLValue): LLValue =
  let nf = n[namePos]

  if nf.kind == nkSym:
    let sym = nf.sym
    if sym.magic != mNone:
      return g.genMagic(n, load, tgt)

  g.genCall(nil, n, load, tgt)

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
        # Globals marked {.global.} get an `sfPure` flag and are initialized
        # before other globals
        let initFunc = g.module.getInitFunc()

        if initFunc.sections[secLastPreinit] == nil:
          initFunc.sections[secLastPreinit] = g.section(initFunc, secPreinit)

        g.withFunc(initFunc): g.withBlock(g.section(initFunc, secLastPreinit)):
          # Avoid using the wrong exception handling context when initializing
          # globals..
          # TODO use the right exception handling context
          let nts = g.f.nestedTryStmts
          g.f.nestedTryStmts = @[]

          let
            lx = g.loadAssignment(v.typ)
            bx = g.genNode(init, lx, tmp)
          g.genAssignment(tmp, bx, v.typ, v.assignCopy)
          initFunc.sections[secLastPreinit] = g.b.getInsertBlock()
          g.f.nestedTryStmts = nts
        LLValue()
      else:
        tmp
    else:
      let tmp = g.genLocal(vn)
      g.initLocalVar(v, v.typ, tmp.v, g.isAssignedImmediately(init))

      tmp

  if init.kind != nkEmpty and x.v != nil:
    let
      bx = g.genCallOrNode(n[0], init, x)
    g.genAssignment(x, bx, v.typ, v.assignCopy)

proc genClosureVar(g: LLGen, n: PNode) =
  let x = g.genNode(n[0], false)

  if n[2].kind == nkEmpty:
    g.buildStoreNull(x.v)
    g.genObjectInit(n[0].typ, x.v)
  else:
    let
      bx = g.genCallOrNode(n[0], n[2], x)
    g.genAssignment(x, bx, n[0].typ, {needToCopy})

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

    let
      tv = g.b.buildGEP(t, [g.gep0, g.constGEPIdx(i)], g.nn("vartuple." & $i, vn))
      lx = g.loadAssignment(vn.typ)

    g.genAssignment(x, g.maybeLoadValue(tv, lx), vn.typ, v.assignCopy)

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
    let
      tgt = g.b.buildGEP(v, [g.gep0, g.constGEPIdx(i.int32)], g.nn("par.gep", n))
      lx = g.loadAssignment(s.typ)
      bx = g.genNode(s, lx, tgt)

    g.genAssignment(tgt, bx, s.typ, {needToCopy})

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
    let
      s = n[i]
      ind = g.fieldIndex(typ, s[0].sym)
      gep = g.b.buildGEP(v, (@[0] & ind).mapIt(g.constGEPIdx(it)), g.nn("objconstr", n))
      lx = g.loadAssignment(s[1].typ)
      bx = g.genNode(s[1], lx, gep)
    g.genAssignment(gep, bx, s[0].sym.typ, {needToCopy})

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

    let lx = g.loadAssignment(typ.elemType)
    for i in 0..<n.len:
      let
        gep = g.b.buildGEP(result, [g.gep0, g.constGEPIdx(i)], g.nn("bracket.n", n))
        bx = g.genNode(n[i], lx, gep)

      g.genAssignment(gep, bx, typ.elemType, {needToCopy})

    result = g.maybeLoadValue(result, load)
  of tySequence:
    let
      tmp = LLValue(v: g.localAlloca(t, g.nn("bracket", n)), storage: OnStack)
      lx = g.loadAssignment(typ.elemType)

    g.buildStoreNull(tmp.v)
    g.genNewSeqAux(tmp, n.typ, g.constNimInt(n.len))
    let tmpl = g.buildLoadValue(tmp)

    for i in 0..<n.len:
      let
        gep = g.buildNimSeqDataGEP(tmpl, g.constGEPIdx(i))
        bx = g.genNode(n[i], lx, gep)
      g.genAssignment(gep, bx, typ.elemType, {needToCopy})
    result = tmpl
  else:
    g.config.internalError(n.info, "Unhandled nodebracket kind: " & $typ.kind)

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
    if first == 0 and g.config.lastOrd(ty) >= 0:
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
  typ = typ.skipTypes(abstractInstOwned)
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
  let
    typ = n.deepTyp
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
        let
          lx = g.loadAssignment(typ)
          bx = g.genNode(s[0], lx, v)
        g.genAssignment(v, bx, typ, {needToCopy})
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
        let
          lx = g.loadAssignment(typ)
          bx = g.genNode(s[1], lx, v)
        g.genAssignment(v, bx, typ, {needToCopy})
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
    vtyp = skipTypes(n[1].typ, abstractRange)
    ntyp = skipTypes(n.typ, abstractRange+{tyOwned})

  if vt == nt:
    v
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    LLValue(v:
      if ntyp.kind == tyBool:
        g.buildTruncOrExt(
          g.b.buildICmp(llvm.IntNE, v.v, constNull(v.v.typeOfX()), g.nn("conv.bool", n)),
          nt, true)
      else:
        g.buildTruncOrExt(v.v, nt, n[1].typ))
  elif vtk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind} and ntk == llvm.IntegerTypeKind:
    LLValue(v:
      if ntyp.kind == tyBool:
        g.buildTruncOrExt(
          g.b.buildFCmp(llvm.RealUNE, v.v, constNull(v.v.typeOfX()), g.nn("conv.bool", n)),
          nt, true)
      elif ntyp.kind in {tyUInt..tyUInt64}:
        g.b.buildFPToUI(v.v, nt, g.nn("conv.fptoui", n))
      else:
        g.b.buildFPToSI(v.v, nt, g.nn("conv.fptosi", n)))
  elif vtk == llvm.IntegerTypeKind and ntk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind}:
    LLValue(v:
      if vtyp.kind in {tyUInt..tyUInt64}:
        g.b.buildUIToFP(v.v, nt, g.nn("conv.uitofp", n))
      else:
        g.b.buildSIToFP(v.v, nt, g.nn("conv.sitofp", n)))
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
        dl = g.m.getModuleDataLayout()
        size = max(dl.storeSizeOfType(vt), dl.storeSizeOfType(nt))
        tmp = g.localAlloca(
          llvm.arrayType(g.primitives[tyUInt8], size.cuint), g.nn("cast.tmp", n))
      discard g.b.buildStore(
        v, g.b.buildBitCast(tmp, vt.pointerType(), g.nn("cast.v", n)))
      g.buildLoadValue(
        g.b.buildBitCast(tmp, nt.pointerType(), g.nn("cast.n", n)))
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
    let axf =
      if ax.v.typeOfX.getTypeKind() == llvm.IntegerTypeKind:
        g.b.buildSIToFP(ax.v, g.primitives[tyFloat], g.nn("rng.sf"))
      else: ax.v

    let args = [
      g.b.buildFPExt(axf, g.primitives[tyFloat], g.nn("fpext", ax.v)),
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
  g.genAsgn(n, false)

proc genNodeFastAsgn(g: LLGen, n: PNode) =
  g.genAsgn(n, g.f != g.module.init)

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
      let
        lx = g.loadAssignment(typ)
        bx = g.genNode(s.lastSon, lx, result)
      g.genAssignment(result, bx, typ, {needToCopy})
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
    if result.v != nil and not typ.isEmptyType():
      let
        lx = g.loadAssignment(typ)
        bx = g.genNode(n, lx, result)
      g.genAssignment(result, bx, typ, {needToCopy})
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

proc genNode(g: LLGen, n: PNode, load: bool, tgt: LLValue): LLValue =
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
  of nkCallKinds: result = g.genNodeCall(n, load, tgt)
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

  of nkMixinStmt, nkBindStmt: discard

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
    s(tyNone, g.voidPtrType) # found in `func f(s: openarray, ...)`
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
      DWARFSourceLanguageC99, df, "nlvm", 4, isOptimized, flags, flags.len.csize_t,
      runtimeVer, "", 0, DWARFEmissionFull, 0, False, False, nil, 0, nil, 0)

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
    add(tyFloat, "float", 64, DW_ATE_float)
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
  let
    llMainType = llvm.functionType(
      g.cintType, [g.cintType, g.primitives[tyCString].pointerType()])
    mainName = # TODO hackish way to not steal the `main` symbol!
      if optNoMain in g.graph.config.globalOptions:
        ".nlvm.main." & g.module.sym.name.s
      else:
        "main"
    main = g.m.addFunction(mainName, llMainType)
    f = g.newLLFunc(main, nil)

  if optNoMain in g.graph.config.globalOptions:
    main.setLinkage(llvm.InternalLinkage) # TODO export it?

  discard f.startBlock(nil, g.section(f, secReturn))

  g.withFunc(f):
    g.withBlock(g.section(f, secArgs)):
      if g.d != nil:
        let
          ptrBits = g.m.getModuleDataLayout().pointerSize() * 8
          types = [
            g.dtypes[tyInt32],
            g.dtypes[tyInt32],
            g.d.dIBuilderCreatePointerType(
              g.d.dIBuilderCreatePointerType(
                g.dtypes[tyChar], ptrBits , ptrBits, ""),
              ptrBits, ptrBits, "")
          ]
        f.ds = g.debugFunction(nil, types, main)
        let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, f.ds, nil)
        g.b.setCurrentDebugLocation2(dl)

        let f0 = main.getFirstParam()
        let f1 = f0.getNextParam()
        let argc = g.localAlloca(f0.typeOfX(), g.nn("argc"))
        let argv = g.localAlloca(f1.typeOfX(), g.nn("argv"))
        discard g.b.buildStore(f0, argc)
        discard g.b.buildStore(f1, argv)

        let vd0 = g.d.dIBuilderCreateParameterVariable(
          f.ds, "argc", 1,
          g.debugGetFile(g.config.projectMainIdx), 0, g.dtypes[tyInt],
          false, 0)
        discard g.d.dIBuilderInsertDeclareAtEnd(argc, vd0,
          g.d.dIBuilderCreateExpression(nil, 0),
          dl, g.b.getInsertBlock())

        let vd1 = g.d.dIBuilderCreateParameterVariable(
          g.f.ds, "argv", 2, g.debugGetFile(g.config.projectMainIdx), 0,
          g.d.dIBuilderCreatePointerType(g.dtypes[tyCString], 64, 64, ""),
          false, 0)
        discard g.d.dIBuilderInsertDeclareAtEnd(argv, vd1,
          g.d.dIBuilderCreateExpression(nil, 0),
          dl, g.b.getInsertBlock())

      if g.config.target.targetOS != osStandAlone and g.config.selectedGC != gcNone:
        let bottom = g.localAlloca(g.primitives[tyInt], g.nn("bottom"))
        discard g.callCompilerProc("initStackBottomWith", [bottom])

      let cmdLine = g.m.getNamedGlobal("cmdLine")
      if cmdLine != nil:
        cmdLine.setLinkage(g.tgtExportLinkage)
        cmdLine.setInitializer(llvm.constNull(cmdLine.typeOfX().getElementType()))
        discard g.b.buildStore(
          g.b.buildBitCast(main.getParam(1), cmdLine.typeOfX().getElementType(), ""),
          cmdLine)

      let cmdCount = g.m.getNamedGlobal("cmdCount")
      if cmdCount != nil:
        cmdCount.setLinkage(g.tgtExportLinkage)
        cmdCount.setInitializer(llvm.constNull(cmdCount.typeOfX().getElementType()))
        discard g.b.buildStore(main.getParam(0), cmdCount)

      for m in g.closedModules: # First the system module
        if sfSystemModule notin m.sym.flags: continue

        if m.init != nil:
          discard g.b.buildCall(m.init.f, [])
        break

      for m in g.closedModules: # then the others
        if sfSystemModule in m.sym.flags: continue

        if m.init != nil:
          discard g.b.buildCall(m.init.f, [])

    g.withBlock(g.section(f, secReturn)):
      if g.d != nil:
        let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, f.ds, nil)
        g.b.setCurrentDebugLocation2(dl)
      discard g.b.buildRet(constInt(g.cintType, 0, False))

    if g.d != nil:
      let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, f.ds, nil)
      g.b.setCurrentDebugLocation2(dl)

    g.finalize()

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
    pmb.passManagerBuilderUseInlinerWithThreshold(5)
  else:
    pmb.passManagerBuilderSetOptLevel(3)
    pmb.passManagerBuilderSetSizeLevel(0)
    pmb.passManagerBuilderUseInlinerWithThreshold(250)

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

    let
      ms = getModule(prc)
      m = g.modules[ms.position]

    g.withModule(m):
      discard g.genFunctionWithBody(prc)

proc myClose(graph: ModuleGraph, b: PPassContext, n: PNode): PNode =
  if graph.config.skipCodegen(n): return n

  let pc = LLModule(b)
  let g = pc.g
  p("Close", n, 0)
  g.closedModules.add(pc)

  g.withModule(pc): g.withFunc(pc.getInitFunc()):
    if g.f.ds != nil:
      let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, g.f.ds, nil)
      g.b.setCurrentDebugLocation2(dl)

    g.withBlock(g.section(g.f, secLastBody)):
      g.debugUpdateLoc(n)
      g.genNode(n)
    g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(pc.init, secReturn)):
      discard g.b.buildRetVoid()

  let s = pc.sym

  if sfCompileToCpp in s.flags:
    g.config.internalError("Compile-to-c++ not supported (did you use importcpp?)")

  if sfMainModule notin s.flags:
    return n

  g.withModule(pc):
    let disp = generateMethodDispatchers(graph)
    for x in disp:
      discard g.genFunctionWithBody(x.sym)

  g.genForwardedProcs()

  for m in g.closedModules:
    if m.init != nil:
      g.withFunc(m.init):
        g.finalize()

  g.genMain()

  for m in g.markerBody:
    g.genMarkerProcBody(m[0], m[1])
  g.markerBody.setLen(0)

  for m in g.gcRoots:
    g.genGcRegistrar(m.sym, m.v)

  if not g.registrar.isNil:
    g.withFunc(g.registrar):
      if g.f.ds != nil:
        let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, g.f.ds, nil)
        g.b.setCurrentDebugLocation2(dl)

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

  if g.config.skipCodegen(n): return n

  p("Process", n, 0)
  var
    transformedN = transformStmt(g.graph, pc.sym, n)
  if sfInjectDestructors in pc.sym.flags:
    transformedN = injectDestructorCalls(g.graph, pc.sym, transformedN)

  g.withModule(pc): g.withFunc(pc.getInitFunc()):
    g.b.positionBuilderAtEnd(g.section(g.f, secLastBody))
    g.debugUpdateLoc(n)

    g.f.options = g.config.options
    g.genNode(transformedN)
    g.f.sections[secLastBody] = g.b.getInsertBlock()

  n

proc myOpen(graph: ModuleGraph, s: PSym): PPassContext =
  # In the C generator, a separate C file is generated for every module,
  # but the rules governing what goes into which module are shrouded by a
  # layer of globals and conditionals.
  # Rather than deciphering all that, we simply generate a single module
  # with all the code in it, like the JS generator does.

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

  let g = LLGen(graph.backend)

  if g.modules.len <= s.position:
    g.modules.setLen(s.position + 1)

  g.modules[s.position] = LLModule(g: LLGen(graph.backend), sym: s)
  g.modules[s.position]

const llgenPass* = makePass(myOpen, myProcess, myClose)
