# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016-2023
# See the LICENSE file for license info (doh!)

import std/[algorithm, os, strutils, sequtils, sets, tables]

import
  compiler/[
    aliases, ast, astalgo, astmsgs, bitsets, cgmeth, ccgutils, extccomp, idents,
    injectdestructors, lineinfos, lowerings, magicsys, modulegraphs, msgs, nimsets,
    options, passes, pathutils, platform, ropes, semparallel, sighashes, spawn, transf,
    trees, types, wordrecg,
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

  LLEhEntry =
    tuple[
      n: PNode,
      inExcept: bool,
      inFin: bool,
      tryPad: llvm.BasicBlockRef,
      excPad: llvm.BasicBlockRef,
      cmps: llvm.BasicBlockRef,
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
    unreachableBlock: llvm.BasicBlockRef
    deadBlocks: seq[llvm.BasicBlockRef]
    landingRes: llvm.ValueRef
    resumeBlock: llvm.BasicBlockRef

  LLModule = ref object of PPassContext
    ## One LLModule per .nim file (module)
    ## Notably though, we don't create an LLVM module per nim module - instead,
    ## everything is dumped one giant thing and compiled as if by LTO
    g: LLGen

    sym: PSym # The symbol given in myOpen corresponding to the module

  LLGen = ref object of RootObj ## One LLGen per compile / project
    graph: ModuleGraph
    lc: llvm.ContextRef
    m: llvm.ModuleRef
    tm: llvm.TargetMachineRef
    b: llvm.BuilderRef
    d: llvm.DIBuilderRef
    idgen: IdGenerator

    init: LLFunc
    f: LLFunc

    # Cached types
    primitives: array[TTypeKind, llvm.TypeRef]
    cintTy: llvm.TypeRef
    csizetTy: llvm.TypeRef
    closureTy: llvm.TypeRef
    jmpBufTy: llvm.TypeRef
    stringTy: llvm.TypeRef
    genericSeqTy: llvm.TypeRef

    strLitFlag: int64 # 1 shl (sizeof(int)*8 - 2) (for target!)

    gep0: llvm.ValueRef
    gep1: llvm.ValueRef
    ni0: llvm.ValueRef
    nb: array[bool, llvm.ValueRef]

    attrNoInline: AttributeRef
    attrNoReturn: AttributeRef
    attrNoUnwind: AttributeRef
    attrNoOmitFP: AttributeRef
    attrCold: AttributeRef
    attrNonnull: AttributeRef
    attrNoalias: AttributeRef

    symbols: Table[int, LLValue]
    gmarkers: Table[int, llvm.ValueRef]
    markers: Table[SigHash, llvm.ValueRef]
    nodeInfos: Table[SigHash, llvm.ValueRef]
    typeInfos: Table[SigHash, llvm.ValueRef]
    typeInfosV2: Table[SigHash, llvm.ValueRef]
    types: Table[SigHash, llvm.TypeRef]
    opaques: Table[SigHash, llvm.TypeRef]
      ## `object` types that have not yet had their fields assigned
    cstrings: Table[string, llvm.ValueRef]
    strings: Table[string, llvm.ValueRef]
    resets: Table[SigHash, llvm.ValueRef]
    assigns: Table[SigHash, llvm.ValueRef]

    depth: int

    registrar: LLFunc

    module: LLModule

    modules: seq[LLModule] # modules in position order
    inits: seq[llvm.ValueRef] # init functions that main should call

    markerBody: seq[tuple[v: llvm.ValueRef, typ: PType]] # Markers looking for a body
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

    lto: LtoKind

    orc: OrcLLJITRef
    done: HashSet[int] ## Symbols that we have emitted a definition for
    round: int

    mangledPrcs*: HashSet[string]

  LLValue = object
    v: llvm.ValueRef
    lode: PNode # Origin node of this value
    storage: TStorageLoc

  # Same as cgen, for easier genAssignment comparison
  TAssignmentFlag = enum
    needToCopy

  TAssignmentFlags = set[TAssignmentFlag]

  FieldPath = tuple[structTy: llvm.TypeRef, index: int, fieldTy: llvm.TypeRef]

  LtoKind = enum
    None
    # TODO Thin
    Full

  FieldMapper = object
    ## Helper for mapping object field entries to LLVM struct elements
    ## inserting padding where needed for proper alignment - the construct
    ## depends on iterating over the nodes of the object definition in the
    ## same order every time
    packed: bool
    element: int
    offset: int
    maxSize: int
    maxAlign: int

template config(g: LLGen): untyped =
  g.graph.config

template fileInfos(g: LLGen): untyped =
  g.config.m.fileInfos

template ptrBits(g: LLGen): untyped =
  g.m.getModuleDataLayout().pointerSize() * 8

# Helpers
proc llPassAsPtr(g: LLGen, s: PSym, retType: PType): bool
proc llType(g: LLGen, typ: PType, deep = true): llvm.TypeRef
proc llStructType(g: LLGen, typ: PType, deep: bool): llvm.TypeRef
proc llTupleType(g: LLGen, typ: PType, deep: bool): llvm.TypeRef
proc llProcType(g: LLGen, typ: PType, closure = false): llvm.TypeRef
proc llStringType(g: LLGen): llvm.TypeRef
proc llGenericSeqType(g: LLGen): llvm.TypeRef
proc llOpenArrayType(g: LLGen): llvm.TypeRef
proc llClosureType(g: LLGen): llvm.TypeRef
proc llMagicType(g: LLGen, name: string): llvm.TypeRef
proc llSeqType(g: LLGen, typ: PType): llvm.TypeRef
proc fieldIndex(g: LLGen, typ: PType, sym: PSym): seq[FieldPath]
proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef)
proc callErrno(g: LLGen, prefix: string): llvm.ValueRef
proc callCompilerProc(
  g: LLGen,
  name: string,
  args: openArray[llvm.ValueRef],
  noInvoke = false,
  noReturn = false,
): llvm.ValueRef

proc genFunction(g: LLGen, s: PSym): LLValue
proc genFunctionWithBody(g: LLGen, s: PSym): LLValue
proc genRefAssign(g: LLGen, dest: LLValue, src: llvm.ValueRef)
proc genAssignment(g: LLGen, dest, src: LLValue, typ: PType, flags: TAssignmentFlags)
# Magic expressions
proc genMagicLength(g: LLGen, n: PNode): LLValue

# Node handling
proc genNode(
  g: LLGen, n: PNode, load: bool = false, dest = LLValue(), prepareMutation = false
): LLValue {.discardable.}

proc genAsgnNode(g: LLGen, n: PNode, typ: PType, dest: LLValue): LLValue

proc deepTyp(n: PNode): PType =
  if n.typ != nil:
    n.typ
  elif n.kind in {nkTryStmt, nkHiddenTryStmt, nkIfStmt, nkCaseStmt}:
    n[0].deepTyp
  elif n.kind in {nkStmtListExpr}:
    n.lastSon.deepTyp
  else:
    nil

proc nn(g: LLGen, s: string, li: TLineInfo): string =
  # later on, we'll just return an empty string here
  let
    fi = int(li.fileIndex)
    fn =
      if fi >= 0 and fi < g.fileInfos.len:
        "." & g.fileInfos[fi].shortName
      else:
        ""
    ln =
      if li.line >= 0.uint16:
        "." & $li.line
      else:
        ""
    col =
      if li.col >= 0:
        "." & $li.col
      else:
        ""

  s & fn & ln & col

proc nn(g: LLGen, s: string): string =
  s

proc nn(g: LLGen, s: string, n: PNode): string =
  if n == nil:
    g.nn(s)
  else:
    g.nn(s, n.info)

proc nn(g: LLGen, s: string, sym: PSym): string =
  if sym == nil:
    g.nn(s)
  else:
    g.nn(s, sym.info)

proc nn(s: string, v: llvm.ValueRef): string =
  var vn = v.getValueName()
  if vn == nil or vn.len == 0:
    s
  else:
    s & "." & $vn

template nn(g: LLGen, s: string, v: llvm.ValueRef): string = # for convenience
  nn(s, v)

proc nn(g: LLGen, s: string, v: LLValue): string =
  g.nn(s, v.v)

template ptrTy(g: LLGen): llvm.TypeRef =
  g.lc.pointerTypeInContext(0)

template voidTy(g: LLGen): llvm.TypeRef =
  g.lc.voidTypeInContext()

template intTy(g: LLGen): llvm.TypeRef =
  g.primitives[tyInt]

template int1Ty(g: LLGen): llvm.TypeRef =
  g.lc.int1TypeInContext()

template int8Ty(g: LLGen): llvm.TypeRef =
  g.lc.int8TypeInContext()

template int16Ty(g: LLGen): llvm.TypeRef =
  g.lc.int16TypeInContext()

template int32Ty(g: LLGen): llvm.TypeRef =
  g.lc.int32TypeInContext()

template int64Ty(g: LLGen): llvm.TypeRef =
  g.lc.int64TypeInContext()

template interactive(g: LLGen): bool =
  g.orc != nil

template defaultFunctionLinkage(g: LLGen): Linkage =
  # In interactive mode, we use linkonce linkage to allow sharing
  # implementations as we'll pass the code to ORC in several LLVM modules
  if g.interactive: LinkOnceAnyLinkage else: InternalLinkage

template defaultGlobalLinkage(g: LLGen): Linkage =
  if g.interactive: LinkOnceAnyLinkage else: PrivateLinkage

proc newLLFunc(g: LLGen, f: llvm.ValueRef, sym: PSym): LLFunc =
  LLFunc(f: f, sym: sym, options: if sym != nil: sym.options else: g.config.options)

func pad(e: LLEhEntry): llvm.BasicBlockRef =
  if e.inExcept: e.excPad else: e.tryPad

proc startBlock(f: LLFunc, n: PNode, exit: llvm.BasicBlockRef): int =
  result = f.blocks.len
  f.blocks.add(LLBlock(n: n, exit: exit, nestedTryStmts: f.nestedTryStmts.len))

proc endBlock(f: LLFunc): LLBlock {.discardable.} =
  f.blocks.pop

proc section(g: LLGen, llf: LLFunc, sk: SectionKind): llvm.BasicBlockRef =
  if llf.sections[sk] == nil:
    llf.sections[sk] = appendBasicBlockInContext(g.lc, llf.f, g.nn($sk))
  llf.sections[sk]

proc moveToEnd(b: llvm.BuilderRef, bl: llvm.BasicBlockRef) =
  bl.moveBasicBlockAfter(b.getInsertBlock().getBasicBlockParent().getLastBasicBlock())

proc positionAndMoveToEnd(b: llvm.BuilderRef, bl: llvm.BasicBlockRef) =
  b.moveToEnd(bl)
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
    g.b.setCurrentDebugLocation2(nil)
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
  let cond =
    g.b.buildICmp(llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("nilcheck.isnil", v))

  discard g.b.buildCondBr(cond, done, notnil)

  g.b.positionBuilderAtEnd(notnil)

  body

  discard g.b.buildBr(done)

  g.b.positionBuilderAtEnd(done)

template withNotNilOrNull(
    g: LLGen, v: llvm.ValueRef, nullTyp: llvm.TypeRef, body: untyped
): llvm.ValueRef =
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
      llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("nilcheck.isnil", v)
    )

    discard g.b.buildCondBr(cond, ldone, lload)

    # run body if v is not nil
    g.b.positionBuilderAtEnd(lload)
    let
      v1 = body
      v1src = g.b.getInsertBlock()
    discard g.b.buildBr(ldone)

    g.b.positionBuilderAtEnd(ldone)

    # body or default
    let phi = g.b.buildPHI(v1.typeOfX(), g.nn("nilcheck.phi", v))

    phi.addIncoming([constNull(v1.typeOfX()), v1], [pre, v1src])
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
    cnt = g.localAlloca(g.intTy, g.nn(name & ".cnt"))
    lcmp = g.b.appendBasicBlockInContext(g.lc, g.nn(name & ".cmp"))
    lbody = g.b.appendBasicBlockInContext(g.lc, g.nn(name & ".body"))
    ldone = g.b.appendBasicBlockInContext(g.lc, g.nn(name & ".done"))

  discard g.b.buildStore(g.constNimInt(0), cnt)

  # jump to comparison
  discard g.b.buildBr(lcmp)

  # check index
  g.b.positionBuilderAtEnd(lcmp)
  let
    i {.inject.} = g.b.buildLoad2(cnt.getAllocatedType(), cnt)
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

proc idOrSig(g: LLGen, s: PSym): Rope =
  # from ccgtypes.nim
  if s.kind in routineKinds and s.typ != nil:
    let sig = sigHash(s, g.config)
    result = rope($sig)
    let counter = g.sigConflicts.getOrDefault(sig)
    if counter != 0:
      result.add "_" & rope(counter + 1)
    g.sigConflicts.inc(sig)
  else:
    let sig = hashNonProc(s)
    result = rope($sig)
    let counter = g.sigConflicts.getOrDefault(sig)
    if counter != 0:
      result.add "_" & rope(counter + 1)
    g.sigConflicts.inc(sig)

# from c gen
proc fillLoc(a: var TLoc, k: TLocKind, lode: PNode, r: Rope, s: TStorageLoc) =
  # fills the loc if it is not already initialized
  if a.k == locNone:
    a.k = k
    a.lode = lode
    a.storage = s
    if a.r == nil:
      a.r = r

proc mangleName(g: LLGen, s: PSym): Rope =
  result = s.loc.r
  if result == nil:
    result = s.name.s.mangle.rope
    add(result, g.idOrSig(s))
    s.loc.r = result

proc encodeName*(name: string): string =
  result = mangle(name)
  result = $result.len & result

proc makeUnique(g: LLGen, s: PSym, name: string = ""): string =
  result = if name == "": s.name.s else: name
  result.add "__"
  result.add g.graph.ifaces[s.itemId.module].uniqueName
  result.add "_u"
  result.add $s.itemId.item

proc encodeSym(g: LLGen, s: PSym, makeUnique: bool = false): string =
  #Module::Type
  var name = s.name.s
  if makeUnique:
    name = makeUnique(g, s, name)
  "N" & encodeName(s.skipGenericOwner.name.s) & encodeName(name) & "E"

proc encodeType*(g: LLGen, t: PType): string =
  result = ""
  var kindName = ($t.kind)[2 ..^ 1]
  kindName[0] = toLower($kindName[0])[0]
  case t.kind
  of tyObject, tyEnum, tyDistinct, tyUserTypeClass, tyGenericParam:
    result = encodeSym(g, t.sym)
  of tyGenericInst, tyUserTypeClassInst, tyGenericBody:
    result = encodeName(t[0].sym.name.s)
    result.add "I"
    for i in 1 ..< t.len - 1:
      result.add encodeType(g, t[i])
    result.add "E"
  of tySequence, tyOpenArray, tyArray, tyVarargs, tyTuple, tyProc, tySet, tyTypeDesc,
      tyPtr, tyRef, tyVar, tyLent, tySink, tyStatic, tyUncheckedArray, tyOr, tyAnd,
      tyBuiltInTypeClass:
    result =
      case t.kind
      of tySequence:
        encodeName("seq")
      else:
        encodeName(kindName)
    result.add "I"
    for i in 0 ..< t.len:
      let s = t[i]
      if s.isNil:
        continue
      result.add encodeType(g, s)
    result.add "E"
  of tyRange:
    var val = "range_"
    if t.n[0].typ.kind in {tyFloat .. tyFloat128}:
      val.addFloat t.n[0].floatVal
      val.add "_"
      val.addFloat t.n[1].floatVal
    else:
      val.add $t.n[0].intVal & "_" & $t.n[1].intVal
    result = encodeName(val)
  of tyString .. tyUInt64, tyPointer, tyBool, tyChar, tyVoid, tyAnything, tyNil, tyEmpty:
    result = encodeName(kindName)
  of tyAlias, tyInferred, tyOwned:
    result = encodeType(g, t.elementType)
  else:
    assert false, "encodeType " & $t.kind

proc mangleProc(g: LLGen, s: PSym, makeUnique: bool): string =
  result = "_Z" # Common prefix in Itanium ABI
  result.add encodeSym(g, s, makeUnique)
  if s.typ.len > 1: #we dont care about the return param
    for i in 1 ..< s.typ.len:
      if s.typ[i].isNil:
        continue
      result.add encodeType(g, s.typ[i])

  if result in g.mangledPrcs:
    result = mangleProc(g, s, true)
  else:
    g.mangledPrcs.incl(result)

proc llName(g: LLGen, s: PSym): string =
  if s.loc.r == "":
    var result: Rope
    if s.kind in routineKinds:
      result = mangleProc(g, s, false).rope
    else:
      result = s.name.s.mangle.rope
      result.add "__"
      result.add g.graph.ifaces[s.itemId.module].uniqueName
      result.add "_u"
      result.addInt s.itemId.item # s.disamb #
    s.loc.r = result

  result = $s.loc.r

  # NCSTRING is a char*, so functions that return const char* need this ugly
  # cast in their importc to be rid of the const.. *sigh*
  if result.startsWith("(char*)"):
    result = result[7 ..^ 1]
  elif result.startsWith("(char *)"):
    result = result[8 ..^ 1]

const irrelevantForBackend = {
  tyGenericBody, tyGenericInst, tyGenericInvocation, tyDistinct, tyRange, tyStatic,
  tyAlias, tySink, tyInferred, tyOwned,
}

func assignCopy(sym: PSym): set[TAssignmentFlag] =
  if lfNoDeepCopy in sym.loc.flags:
    {}
  else:
    {needToCopy}

proc typeName(typ: PType, result: var Rope) =
  let typ = typ.skipTypes(irrelevantForBackend)
  result.add $typ.kind
  if typ.sym != nil and typ.kind in {tyObject, tyEnum}:
    result.add "_"
    result.add typ.sym.name.s.mangle

proc supportsCopyMem(typ: PType): bool =
  not (containsGarbageCollectedRef(typ) or hasDestructor(typ))

template hasMTypeField(typ: PType): bool =
  # Check if this object type specifically has a type info field - does not
  # consider base types
  assert typ.kind == tyObject
  not typ.lacksMTypeField()

proc llName(g: LLGen, typ: PType, sig: SigHash): string =
  # getTypeName from ccgtypes.nim
  var t = typ
  while true:
    if t.sym != nil and {sfImportc, sfExportc} * t.sym.flags != {}:
      return t.sym.loc.r

    if t.kind in irrelevantForBackend:
      t = t.lastSon
    else:
      break
  let typ = if typ.kind in {tyAlias, tySink, tyOwned}: typ.lastSon else: typ
  if typ.loc.r == "":
    typ.typeName(typ.loc.r)
    typ.loc.r.add $sig
  else:
    when defined(debugSigHashes):
      # check consistency:
      var tn = newRopeAppender()
      typ.typeName(tn)
      assert($typ.loc.r == $(tn & $sig))
  result = typ.loc.r
  if result == "":
    internalError(g.config, "getTypeName: " & $typ.kind)

iterator procParams(typ: PType): PNode =
  for a in typ.n.sons[1 ..^ 1]:
    let param = a.sym
    if isCompileTimeOnly(param.typ):
      continue
    yield a

proc `$`(t: PType): string

proc `$`(n: PSym): string =
  if n == nil:
    return "PSym(nil)"
  let name =
    if n.loc.r == nil:
      n.name.s
    else:
      $n.loc.r
  name & " " & $n.id & " " & $n.kind & " " & $n.magic & " " & $n.flags & " " &
    $n.loc.flags & " " & $n.info.line & " " & $n.typ & " " & $n.loc.flags

proc `$`(t: PType): string =
  if t == nil:
    return "PType(nil)"
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
  if n == nil:
    return "PNode(nil)"
  result = $n.kind & " "
  case n.kind
  of nkCharLit .. nkUInt64Lit:
    result &= $n.typ.kind & " " & $n.intVal
  of nkFloatLit .. nkFloat128Lit:
    result &= $n.typ.kind & " " & $n.floatVal
  of nkStrLit .. nkTripleStrLit:
    result &= n.strVal
  of nkSym:
    result &= $n.sym
  of nkIdent:
    result &= n.ident.s
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
  llvm.constInt(g.int1Ty, v.culonglong, llvm.False)

proc constInt8(g: LLGen, v: int8): ValueRef =
  llvm.constInt(g.primitives[tyInt8], v.culonglong, llvm.False)

proc constUInt8(g: LLGen, v: uint8): ValueRef =
  llvm.constInt(g.primitives[tyUInt8], v.culonglong, llvm.False)

proc constInt32(g: LLGen, v: int32): ValueRef =
  llvm.constInt(g.primitives[tyInt32], v.culonglong, llvm.False)

proc constInt64(g: LLGen, v: int64): ValueRef =
  llvm.constInt(g.primitives[tyInt64], v.culonglong, llvm.False)

proc constCInt(g: LLGen, val: int): llvm.ValueRef =
  llvm.constInt(g.cintTy, val.culonglong, llvm.True)

proc constNimInt(g: LLGen, val: int64): llvm.ValueRef =
  # int64 because we could compile 64-bit programs on 32-bit
  llvm.constInt(g.primitives[tyInt], val.culonglong, llvm.True)

proc constNimInt(g: LLGen, val: Int128): llvm.ValueRef =
  g.constNimInt(val.toInt64())

proc constStoreSize(g: LLGen, typ: llvm.TypeRef): llvm.ValueRef =
  let dl = g.m.getModuleDataLayout()
  g.constInt32(dl.storeSizeOfType(typ).int32)

proc isZero(v: llvm.ValueRef): bool =
  v.isConstant() == llvm.True and v.typeOfX().getTypeKind() == IntegerTypeKind and
    v.constIntGetZExtValue() == 0

# Int constant suitable for indexing into structs when doing GEP
# Has to be i32 per LLVM docs
proc constGEPIdx(g: LLGen, val: int): llvm.ValueRef =
  g.constInt32(val.int32)

proc toConstGEP(g: LLGen, v: llvm.ValueRef, path: openArray[FieldPath]): ValueRef =
  var ret = v
  for entry in path:
    ret = constGEP2(entry.structTy, ret, [g.gep0, g.constGEPIdx(entry.index)])
  ret

proc toGEP(
    g: LLGen, v: llvm.ValueRef, path: openArray[FieldPath], name: string
): ValueRef =
  var ret = v
  for entry in path:
    ret = g.b.buildStructGEP2(
      entry.structTy, ret, cuint entry.index, g.nn(name & ".f" & $entry.index, ret)
    )
  ret

proc constOffsetOf(g: LLGen, typ: PType, sym: PSym): llvm.ValueRef =
  let
    typ = typ.skipTypes(abstractPtrs)
    ty = g.llType(typ)
    gep = g.toConstGEP(constNull(ty.pointerType()), g.fieldIndex(typ, sym))
  constPtrToInt(gep, g.primitives[tyInt])

proc bitSetToWord(s: TBitSet, size: int): BiggestUInt =
  result = 0
  for j in 0 ..< size:
    if j < len(s):
      result = result or (BiggestUInt(s[j]) shl (j * 8))

proc constNimStringPayload(g: LLGen, val: string): llvm.ValueRef =
  let
    s = g.lc.constStringInContext(val)
    cap = g.constNimInt(val.len + g.strLitFlag)
  if optSeqDestructors in g.config.globalOptions:
    llvm.constStructInContext(g.lc, [cap, s])
  else:
    let
      ll = g.constNimInt(val.len)
      x = llvm.constNamedStruct(g.llGenericSeqType(), [ll, cap])
    llvm.constStructInContext(g.lc, [x, s])

proc simplifyStr(s: string): string =
  for c in s:
    if c in IdentChars:
      result.add c
    else:
      result.add '_'

    if result.len > 15:
      result.add ".."
      break

proc genStrLit(g: LLGen, ty: llvm.TypeRef, strVal: string): llvm.ValueRef =
  let lit =
    if strVal in g.strings:
      g.strings[strVal]
    else:
      let
        payload = g.constNimStringPayload(strVal)
        lit =
          g.m.addPrivateConstant(payload.typeOfX, g.nn(".str." & simplifyStr(strVal)))
      lit.setInitializer(payload)
      g.strings[strVal] = lit
      lit
  if optSeqDestructors in g.config.globalOptions:
    constNamedStruct(ty, [g.constNimInt(strVal.len), lit])
  else:
    lit

proc constCStringPtr(g: LLGen, val: string): llvm.ValueRef =
  g.cstrings.withValue(val, v):
    return v[]
  do:
    let init = g.lc.constStringInContext(val)
    let s = g.m.addPrivateConstant(init.typeOfX(), g.nn(".cstr." & simplifyStr(val)))
    s.setInitializer(init)
    g.cstrings[val] = s
    return s

proc buildExtractValue(
    b: llvm.BuilderRef, v: LLValue, index: cuint, name: cstring
): LLValue =
  LLValue(v: b.buildExtractValue(v.v, index, name), lode: v.lode, storage: v.storage)

proc buildInboundsGEP2(
    b: llvm.BuilderRef,
    ty: llvm.TypeRef,
    v: LLValue,
    indices: openArray[ValueRef],
    name: cstring,
): LLValue =
  LLValue(
    v: b.buildInboundsGEP2(ty, v.v, indices, name), lode: v.lode, storage: v.storage
  )

proc buildStructGEP2(
    b: llvm.BuilderRef, ty: llvm.TypeRef, v: LLValue, index: cuint, name: cstring
): LLValue =
  LLValue(v: b.buildStructGEP2(ty, v.v, index, name), lode: v.lode, storage: v.storage)

proc buildNimSeqLenGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildStructGEP2(g.llGenericSeqType(), s, 0, g.nn("seq.len", s))

proc buildNimSeqCapGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildStructGEP2(g.llGenericSeqType(), s, 1, g.nn("seq.cap", s))

proc buildClosurePrcGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildStructGEP2(g.llClosureType(), s, 0, g.nn("ClP_0", s))

proc buildClosureEnvGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildStructGEP2(g.llClosureType(), s, 1, g.nn("ClE_0", s))

proc buildOpenArrayDataGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildStructGEP2(g.llOpenArrayType(), s, 0, g.nn("data", s))

proc buildOpenArrayLenGEP(g: LLGen, s: llvm.ValueRef): llvm.ValueRef =
  g.b.buildStructGEP2(g.llOpenArrayType(), s, 1, g.nn("len", s))

proc buildNimSeqDataGEP(
    g: LLGen, seqTy: llvm.TypeRef, s: llvm.ValueRef, idx: llvm.ValueRef = nil
): llvm.ValueRef =
  let idx = if idx == nil: g.gep0 else: idx
  if optSeqDestructors in g.config.globalOptions:
    let payload = g.b.buildExtractValue(s, 1, g.nn("payload", s))
    g.b.buildInboundsGEP2(seqTy, payload, [g.gep0, g.gep1, idx], g.nn("seq.data", s))
  else:
    g.b.buildInboundsGEP2(seqTy, s, [g.gep0, g.gep1, idx], g.nn("seq.data", s))

proc buildNimSeqDataGEP(
    g: LLGen, seqTy: llvm.TypeRef, s: LLValue, idx: llvm.ValueRef = nil
): LLValue =
  LLValue(
    v: g.buildNimSeqDataGEP(seqTy, s.v, idx),
    lode: s.lode,
    storage: if s.storage == OnStatic: OnStatic else: OnHeap,
  )

proc buildI1(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.b.buildICmp(llvm.IntNE, v, constNull(v.typeOfX), g.nn("i1", v))

proc buildI8(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.b.buildZExt(v, g.int8Ty, g.nn("bool.i8", v))

proc isUnsigned(g: LLGen, typ: PType): bool =
  let typ = typ.skipTypes(abstractVarRange)

  typ.kind in {tyUInt .. tyUInt64, tyBool, tyChar, tySet} or
    (typ.kind == tyEnum and g.config.firstOrd(typ) >= 0)

proc buildNimIntExt(g: LLGen, v: llvm.ValueRef, unsigned: bool): llvm.ValueRef =
  let nt = g.primitives[tyInt]

  if unsigned:
    g.b.buildZExt(v, nt, g.nn("nie.z", v))
  else:
    g.b.buildSExt(v, nt, g.nn("nie.s", v))

proc buildTruncOrExt(
    g: LLGen, v: llvm.ValueRef, nt: llvm.TypeRef, unsigned: bool
): llvm.ValueRef =
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
  else: # vw < nw
    if unsigned:
      g.b.buildZExt(v, nt, g.nn("toe.z", v))
    else:
      g.b.buildSExt(v, nt, g.nn("toe.s", v))

proc buildTruncOrExt(
    g: LLGen, v: llvm.ValueRef, nt: llvm.TypeRef, typ: PType
): llvm.ValueRef =
  g.buildTruncOrExt(v, nt, g.isUnsigned(typ))

proc needsTerminator(b: llvm.BasicBlockRef): bool =
  b.getBasicBlockTerminator() == nil

proc needsTerminator(b: llvm.BuilderRef): bool =
  b.getInsertBlock().needsTerminator()

proc buildBrFallthrough(b: llvm.BuilderRef, next: llvm.BasicBlockRef) =
  # Add a br to the next block if the current block is not already terminated
  if b.needsTerminator():
    discard b.buildBr(next)

proc buildSetMask(
    g: LLGen, ty: llvm.TypeRef, ix: llvm.ValueRef, size: BiggestInt
): llvm.ValueRef =
  let
    mask =
      case size
      of 1: 7
      of 2: 15
      of 4: 31
      of 8: 63
      else: 7
    ty = if size <= 8: ty else: g.int8Ty
  var shift = g.b.buildAnd(
    ix, constInt(ix.typeOfX(), mask.culonglong, llvm.False), g.nn("set.mask", ix)
  )
  if ty != shift.typeOfX():
    shift = g.buildTruncOrExt(shift, ty, true)

  g.b.buildShl(llvm.constInt(ty, 1, llvm.False), shift, g.nn("set.pos", ix))

proc buildSetGEP(g: LLGen, ty: llvm.TypeRef, vx, ix: llvm.ValueRef): llvm.ValueRef =
  let idx =
    g.b.buildLShr(ix, llvm.constInt(ix.typeOfX(), 3, llvm.False), g.nn("set.byte", ix))
  g.b.buildInboundsGEP2(ty, vx, [g.gep0, idx], g.nn("set.gep", vx))

proc buildSetGEPMask(
    g: LLGen, ty: llvm.TypeRef, vx, ix: llvm.ValueRef
): tuple[gep, mask: llvm.ValueRef] =
  (gep: g.buildSetGEP(ty, vx, ix), mask: g.buildSetMask(g.int8Ty, ix, 1))

proc buildLoad2(b: BuilderRef, ty: llvm.TypeRef, v: llvm.ValueRef): llvm.ValueRef =
  b.buildLoad2(ty, v, nn("load", v))

proc buildLoadValue(
    g: LLGen, ty: llvm.TypeRef, v: llvm.ValueRef, name: string = ""
): llvm.ValueRef =
  if ty.getTypeKind() == llvm.ArrayTypeKind:
    g.b.buildInboundsGEP2(ty, v, [g.gep0, g.gep0], g.nn("loaa", v) & name)
  else:
    g.b.buildLoad2(ty, v, g.nn("load", v) & name)

proc buildLoadValue(g: LLGen, ty: llvm.TypeRef, v: LLValue, name = ""): LLValue =
  # The source remains the same, even though we're turning addess into value
  LLValue(v: g.buildLoadValue(ty, v.v, name), lode: v.lode, storage: v.storage)

proc maybeLoadValue(
    g: LLGen, ty: llvm.TypeRef, v: LLValue, load: bool, name = ""
): LLValue =
  if load:
    g.buildLoadValue(ty, v, name)
  else:
    v

proc buildNot(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  let cmp = g.b.buildICmp(
    llvm.IntEQ, v, llvm.constInt(v.typeOfX(), 0, llvm.False), g.nn("not", v)
  )
  g.b.buildZExt(cmp, v.typeOfX(), g.nn("zext", cmp))

proc buildBitnot(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  g.b.buildXor(
    v, llvm.constInt(v.typeOfX(), not culonglong(0), llvm.False), g.nn("bitnot", v)
  )

proc isLargeType(g: LLGen, t: llvm.TypeRef): bool =
  # Large types in loads and stores lead to inefficient codegen - "large" is a
  # bit underdefined in this context, but it seems reasonable to base it off
  # pointers in general
  # See also https://llvm.org/docs/Frontend/PerformanceTips.html#avoid-loads-and-stores-of-large-aggregate-type
  let dl = g.m.getModuleDataLayout()
  (t.getTypeKind() in {llvm.ArrayTypeKind, llvm.StructTypeKind}) and
    (dl.storeSizeOfType(t) > (dl.pointerSize() * 2))

proc buildStoreNull(g: LLGen, ty: llvm.TypeRef, tgt: llvm.ValueRef) =
  # constNull can take up a lot of space - use memset instead for large values
  if g.isLargeType(ty):
    g.callMemset(tgt, g.constInt8(0), g.constStoreSize(ty))
  else:
    discard g.b.buildStore(constNull(ty), tgt)

proc buildCallOrInvoke(
    g: LLGen,
    fty: llvm.TypeRef,
    f: llvm.ValueRef,
    args: openArray[llvm.ValueRef],
    name: cstring,
): llvm.ValueRef =
  let name =
    if fty.getReturnType().getTypeKind() == llvm.VoidTypeKind:
      cstring("")
    else:
      name

  if g.f.nestedTryStmts.len > 0:
    let
      then = g.b.appendBasicBlockInContext(g.lc, g.nn("invoke.then", f))
      res = g.b.buildInvoke2(fty, f, args, then, g.f.nestedTryStmts[^1].pad, name)
    g.b.positionAndMoveToEnd(then)
    res
  else:
    g.b.buildCall2(fty, f, args, name)

proc buildCallOrInvokeBr(
    g: LLGen,
    fty: llvm.TypeRef,
    f: llvm.ValueRef,
    then: llvm.BasicBlockRef,
    args: openArray[llvm.ValueRef],
    name: cstring,
): llvm.ValueRef =
  let name =
    if fty.getReturnType().getTypeKind() == llvm.VoidTypeKind:
      cstring("")
    else:
      name
  if g.f.nestedTryStmts.len > 0:
    let res = g.b.buildInvoke2(fty, f, args, then, g.f.nestedTryStmts[^1].pad, name)
    res
  else:
    let res = g.b.buildCall2(fty, f, args, name)
    discard g.b.buildBr(then)
    res

proc buildOpenArray(g: LLGen, data, len: llvm.ValueRef): llvm.ValueRef =
  let a0 = g.b.buildInsertValue(constNull(g.llOpenArrayType), data, 0, g.nn("a0", data))
  g.b.buildInsertValue(a0, len, 1, g.nn("a1", data))

proc loadNimSeqLen(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  if optSeqDestructors in g.config.globalOptions:
    g.b.buildExtractValue(v, 0, g.nn("len", v))
  else:
    g.withNotNilOrNull(v, g.intTy):
      let gep = g.buildNimSeqLenGEP(v)
      g.b.buildLoad2(g.intTy, gep)

proc getNimSeqDataPtr(
    g: LLGen, seqTy: llvm.TypeRef, v: llvm.ValueRef, idx: llvm.ValueRef = g.gep0
): llvm.ValueRef =
  if optSeqDestructors in g.config.globalOptions:
    let payload = g.b.buildExtractValue(v, 1, g.nn("payload", v))
    g.withNotNilOrNull(payload, g.ptrTy):
      g.b.buildInboundsGEP2(seqTy, payload, [g.gep0, g.gep1, idx], g.nn("data", v))
  else:
    g.withNotNilOrNull(v, g.ptrTy):
      g.buildNimSeqDataGEP(seqTy, v, idx)

proc isObjLackingTypeField(typ: PType): bool =
  (typ.kind == tyObject) and
    ((tfFinal in typ.flags) and (typ.sons[0] == nil) or isPureObject(typ))

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
      g.isLargeType(g.llType(t)) or containsGarbageCollectedRef(t) or
        (t.kind == tyObject and not isObjLackingTypeField(t))
    else:
      g.isLargeType(g.llType(t)) and (tfByCopy notin rettype.flags)

proc debugSize(g: LLGen, ty: llvm.TypeRef): uint32 =
  let dl = g.m.getModuleDataLayout()
  sizeOfXTypeInBits(dl, ty).uint32

proc debugSize(g: LLGen, typ: PType): uint32 =
  g.debugSize(g.llType(typ))

proc debugOffset(g: LLGen, ty: llvm.TypeRef, element: int): uint32 =
  let dl = g.m.getModuleDataLayout()
  dl.offsetOfElement(ty, cuint element).uint32 * 8

proc debugGetLine(g: LLGen, sym: PSym): (llvm.MetadataRef, cuint) =
  let
    idx = if sym == nil: g.config.projectMainIdx else: sym.info.fileIndex
    line =
      if sym == nil:
        cuint 1
      else:
        cuint sym.info.line

  if int(idx) in g.dfiles:
    return (g.dfiles[int(idx)], line)

  let
    path = g.config.toFullPath(idx)
    (dir, fn) = path.splitPath()
    df = g.d.dIBuilderCreateFile(fn, dir)

  g.dfiles[int(idx)] = df
  (df, line)

proc debugGetFile(g: LLGen, idx: FileIndex): llvm.MetadataRef =
  if int(idx) in g.dfiles:
    return g.dfiles[int(idx)]

  let path = g.config.toFullPath(idx)
  let (dir, fn) = path.splitPath()
  result = g.d.dIBuilderCreateFile(fn, dir)

  g.dfiles[int(idx)] = result

proc debugStructType(g: LLGen, typ: PType): llvm.MetadataRef
proc debugTupleType(g: LLGen, typ: PType): llvm.MetadataRef
proc debugMagicType(g: LLGen, name: string): llvm.MetadataRef

proc debugType(g: LLGen, typ: PType): llvm.MetadataRef =
  if g.d == nil:
    return nil
  case typ.kind
  of tyBool:
    g.dtypes[tyBool]
  of tyChar:
    g.dtypes[tyChar]
  of tyNil, tyTyped, tyNone:
    g.dtypes[tyChar]
  # void*?
  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyGenericParam, tyDistinct,
      tyOrdinal, tyTypeDesc, tyAlias, tySink, tyUserTypeClass, tyUserTypeClassInst,
      tyInferred, tyStatic, tyOwned:
    g.debugType(typ.lastSon)
  of tyEnum:
    g.d.dIBuilderCreateBasicType(
      g.llName(typ, hashType(typ, g.config)), g.debugSize(typ), DW_ATE_unsigned
    )
  of tyArray:
    let et = g.debugType(typ.elemType)
    g.d.dIBuilderCreateArrayType(
      g.debugSize(typ),
      0,
      et,
      [g.d.dIBuilderGetOrCreateSubrange(0, g.config.lengthOrd(typ).toInt.int64)],
    )
  of tyUncheckedArray:
    let et = g.debugType(typ.elemType)
    g.d.dIBuilderCreateArrayType(0, 0, et, [g.d.dIBuilderGetOrCreateSubrange(0, -1)])
  of tyObject:
    g.debugStructType(typ)
  of tyTuple:
    g.debugTupleType(typ)
  of tySet:
    let bits = g.debugSize(typ)
    if bits <= 8 * 8:
      g.d.dIBuilderCreateBasicType("set" & $bits, bits, DW_ATE_unsigned)
    else:
      g.d.dIBuilderCreateArrayType(
        bits,
        0,
        g.dtypes[tyUInt8],
        [g.d.dIBuilderGetOrCreateSubrange(0, bits.int64 div 8)],
      )
  of tyRange:
    g.debugType(typ[0])
  of tyPtr, tyRef, tyVar, tyLent:
    g.d.dIBuilderCreatePointerType(g.debugType(typ.lastSon), g.ptrBits, g.ptrBits, "")
  of tySequence:
    let sig = hashType(typ, g.config)
    if sig in g.dstructs:
      g.dstructs[sig]
    else:
      let
        name = g.llName(typ, sig)
        (df, line) = g.debugGetLine(typ.sym)
        payloadTy = g.llSeqType(typ)
        payloadBits = g.debugSize(payloadTy)
        payload = g.d.dIBuilderCreateStructType(
          df, name, df, line, payloadBits, 0, 0, nil, [], 0, nil, name
        )
        payloadPtr = g.d.dIBuilderCreatePointerType(payload, g.ptrBits, g.ptrBits, "")

        st =
          if optSeqDestructors in g.config.globalOptions:
            let
              seqTy = g.llType(typ)
              seqBits = g.debugSize(seqTy)

              st = g.d.dIBuilderCreateStructType(
                df, name & "V", df, line, seqBits, 0, 0, nil, [], 0, nil, name & "V"
              )
              elems = [
                g.d.dIBuilderCreateMemberType(
                  st, "len", df, line, payloadBits, 0, 0, 0, g.dtypes[tyInt]
                ),
                g.d.dIBuilderCreateMemberType(
                  st, "p", df, line, payloadBits, 0, payloadBits, 0, payloadPtr
                ),
              ]
            g.d.nimDICompositeTypeSetTypeArray(st, g.d.dIBuilderGetOrCreateArray(elems))
            st
          else:
            payloadPtr

      g.dstructs[sig] = st

      var elems =
        if optSeqDestructors in g.config.globalOptions:
          @[
            g.d.dIBuilderCreateMemberType(
              st, "cap", df, line, payloadBits, 0, 0, 0, g.dtypes[tyInt]
            )
          ]
        else:
          let sup = g.debugMagicType("TGenericSeq")
          @[
            g.d.dIBuilderCreateMemberType(
              st, "Sup", df, line, payloadBits, 0, 0, 0, sup
            )
          ]
      if typ.elemType.kind != tyEmpty:
        let
          dt = g.debugType(typ.elemType)
          data = g.d.dIBuilderCreateMemberType(
            payload,
            "data",
            df,
            0,
            0,
            0,
            g.debugOffset(payloadTy, 1),
            0,
            g.d.dIBuilderCreateArrayType(
              0, 0, dt, [g.d.dIBuilderGetOrCreateSubrange(0, -1)]
            ),
          )
        elems.add(data)
      g.d.nimDICompositeTypeSetTypeArray(payload, g.d.dIBuilderGetOrCreateArray(elems))

      st
  of tyProc:
    if typ.callConv == ccClosure:
      let df = g.debugGetFile(g.config.projectMainIdx)
      g.d.dIBuilderCreateStructType(
        df,
        ".closure",
        df,
        0,
        g.ptrBits * 2,
        g.ptrBits,
        0,
        nil,
        [g.dtypes[tyPointer], g.dtypes[tyPointer]],
        0,
        nil,
        ".closure",
      )
    else:
      g.dtypes[tyPointer]
  of tyPointer:
    g.dtypes[tyPointer]
  of tyOpenArray, tyVarargs:
    let
      sig = hashType(typ, g.config)
      st =
        if sig in g.dstructs:
          g.dstructs[sig]
        else:
          let
            (df, line) = g.debugGetLine(typ.sym)
            name = g.llName(typ, sig)
            st = g.d.dIBuilderCreateStructType(
              df, name, df, 0, g.ptrBits * 2, g.ptrBits, 0, nil, [], 0, nil, name
            )

          g.dstructs[sig] = st

          let
            p = g.d.dIBuilderCreatePointerType(
              g.debugType(typ.elemType), g.ptrBits, g.ptrBits, ""
            )

            elems = [
              g.d.dIBuilderCreateMemberType(
                st, "data", df, line, g.ptrBits, g.ptrBits, 0, 0, p
              ),
              g.d.dIBuilderCreateMemberType(
                st, "len", df, line, g.ptrBits, g.ptrBits, g.ptrBits, 0, g.dtypes[tyInt]
              ),
            ]

          g.d.nimDICompositeTypeSetTypeArray(st, g.d.dIBuilderGetOrCreateArray(elems))

          st
    st
  of tyString:
    if g.dtypes[tyString] == nil:
      g.dtypes[tyString] =
        if optSeqDestructors in g.config.globalOptions:
          g.debugMagicType("NimStringV2")
        else:
          g.d.dIBuilderCreatePointerType(
            g.debugMagicType("NimStringDesc"), g.ptrBits, g.ptrBits, ""
          )
    g.dtypes[tyString]
  of tyCString:
    g.dtypes[tyCString]
  of tyInt .. tyUInt64:
    g.dtypes[typ.kind]
  else:
    g.config.internalError("Unhandled debug type " & $typ.kind)
    nil

proc debugFieldName(field: PSym, typ: PType): string =
  if (typ.sym != nil) and ({sfImportc, sfExportc} * typ.sym.flags != {}):
    $field.loc.r
  else:
    mangle(field.name.s)

proc aligned(address, alignment: int): int =
  (address + (alignment - 1)) and not (alignment - 1)

proc getBestSize(g: LLGen, typ: PType, ty: llvm.TypeRef): int =
  ## Nim can't compute size of C-derived structs properly so we make a guess
  ## which mostly works but might end up being wrong leading to memory
  ## corruption
  let v = g.config.getSize(typ).int
  if v == szUnknownSize:
    let dl = g.m.getModuleDataLayout()
    dl.aBISizeOfType(ty).int
  else:
    v

proc getBestAlign(g: LLGen, typ: PType, ty: llvm.TypeRef): int =
  let v = g.config.getAlign(typ).int
  if v == szUnknownSize:
    let dl = g.m.getModuleDataLayout()
    dl.aBIAlignmentOfType(ty).int
  else:
    v

proc maxAlign(g: LLGen, n: PNode): int =
  ## Largest best alignment for a type node
  case n.kind
  of nkRecList:
    foldl((0 ..< n.len), max(a, g.maxAlign(n[b])), 1)
  of nkRecCase:
    let
      tags = n[0].sym
      tagAlign = g.getBestAlign(tags.typ, g.llType(tags.typ))

    foldl((1 ..< n.len), max(a, g.maxAlign(n[b].lastSon)), tagAlign)
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      1
    else:
      g.getBestAlign(field.typ, g.llType(field.typ))
  else:
    g.config.internalError(n.info, "maxAlign")
    0

proc addField(
    g: LLGen,
    mapper: var FieldMapper,
    ty: llvm.TypeRef,
    size, align: int,
    pad: proc(bytes: int) = nil,
): int =
  let
    dl = g.m.getModuleDataLayout()
    abi = aligned(mapper.offset, int(dl.aBIAlignmentOfType(ty)))
    align = if mapper.packed: 1 else: align
    aligned = aligned(mapper.offset, align)

  assert align == 1 or not mapper.packed

  mapper.maxSize = max(mapper.maxSize, size)
  mapper.maxAlign = max(mapper.maxAlign, align)
  if aligned > abi:
    # LLVM takes care of ABI alignment, no need to emit a pad
    # TODO let's hope it doesn't use preferred alignment
    if pad != nil:
      pad(aligned - mapper.offset)
    mapper.element += 1

  mapper.offset = aligned + size

  let elem = mapper.element
  mapper.element += 1
  elem

proc addField(
    g: LLGen,
    mapper: var FieldMapper,
    ty: llvm.TypeRef,
    typ: PType,
    pad: proc(bytes: int) = nil,
): int =
  let
    size = g.getBestSize(typ, ty).int
    align =
      if mapper.packed:
        1
      else:
        g.getBestAlign(typ, ty).int

  g.addField(mapper, ty, size, align, pad)

proc addField(
    g: LLGen,
    mapper: var FieldMapper,
    ty: llvm.TypeRef,
    sym: PSym,
    pad: proc(bytes: int) = nil,
): int =
  let
    size = g.getBestSize(sym.typ, ty).int
    align =
      if mapper.packed:
        1
      else:
        max(g.getBestAlign(sym.typ, ty).int, sym.alignment)

  g.addField(mapper, ty, size, align, pad)

func caseTypeName(t: llvm.TypeRef, tag: PSym, index: int): string =
  $t.getStructName() & "_" & tag.name.s & "_" & $index

proc debugStructFields(
    g: LLGen,
    mapper: var FieldMapper,
    n: PNode,
    dty: llvm.MetadataRef,
    members: var seq[MetadataRef],
    typ: PType,
    ty: llvm.TypeRef,
) =
  case n.kind
  of nkRecList:
    for child in n:
      g.debugStructFields(mapper, child, dty, members, typ, ty)
  of nkRecCase:
    let tags = n[0].sym

    block:
      let
        element = g.addField(mapper, g.llType(tags.typ), tags)
        name = debugFieldName(tags, typ)

        (df, line) = g.debugGetLine(tags)
        member = g.d.dIBuilderCreateMemberType(
          dty,
          name,
          df,
          line.cuint,
          g.debugSize(tags.typ),
          0,
          g.debugOffset(ty, element),
          0,
          g.debugType(tags.typ),
        )

      members.add(member)

    block:
      let
        storeName = caseTypeName(ty, tags, 0)
        storeTy = g.lc.getTypeByName2(storeName)
        (df, line) = g.debugGetLine(tags)
        bits = g.debugSize(storeTy)
        storeType = g.d.dIBuilderCreateStructType(
          df, storeName, df, line, bits, 0, 0, nil, [], 0, nil, storeName
        )

        maxAlign = foldl(1 ..< n.len, max(a, g.maxAlign(n[b].lastSon)), 1)
        element = g.addField(
          mapper,
          storeTy,
          g.m.getModuleDataLayout().aBISizeOfType(storeTy).int,
          maxAlign,
        )

      var storeFields: seq[MetadataRef]

      g.d.nimDICompositeTypeSetTypeArray(
        storeType, g.d.dIBuilderGetOrCreateArray(storeFields)
      )

      let member = g.d.dIBuilderCreateMemberType(
        storeType,
        storeName,
        df,
        line.cuint,
        bits,
        0,
        g.debugOffset(ty, element),
        0,
        storeType,
      )

      members.add(member)
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return

    let
      element = g.addField(mapper, g.llType(field.typ), field)
      name = debugFieldName(field, typ)
      (df, line) = g.debugGetLine(field)
      member = g.d.dIBuilderCreateMemberType(
        dty,
        name,
        df,
        line.cuint,
        g.debugSize(field.typ),
        0,
        g.debugOffset(ty, element),
        0,
        g.debugType(field.typ),
      )

    members.add(member)
  else:
    g.config.internalError(n.info, "debugStructFields")

proc debugStructType(g: LLGen, typ: PType): llvm.MetadataRef =
  if typ == nil:
    return

  let typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString:
    return g.debugType(typ)

  let sig = hashType(typ, g.config)
  if sig in g.dstructs:
    return g.dstructs[sig]

  let
    name = g.llName(typ, sig)
    ty = g.llType(typ)
    (df, line) = g.debugGetLine(typ.sym)

  # Create struct before setting body in case it's recursive
  result = g.d.dIBuilderCreateStructType(
    df, name, df, line, g.debugSize(typ), 0, 0, nil, [], 0, nil, name
  )

  g.dstructs[sig] = result

  var members = newSeq[MetadataRef]()
  if tfUnion in typ.flags and ty.countStructElementTypes() > 0:
    let
      dl = g.m.getModuleDataLayout()
      data = ty.structGetTypeAtIndex(0)
      bytes = dl.aBISizeOfType(data)
      bits = bytes * 8
      at = g.d.dIBuilderCreateArrayType(
        bits, 0, g.dtypes[tyUInt8], [g.d.dIBuilderGetOrCreateSubrange(0, int64(bytes))]
      )
      # TODO make actual union metadata instead
      member =
        g.d.dIBuilderCreateMemberType(result, "union", df, line, bits, 0, 0, 0, at)
    members.add(member)
  else:
    var mapper = FieldMapper(packed: tfPacked in typ.flags)

    if typ[0] != nil:
      let supTyp = typ[0].skipTypes(skipPtrs)
      discard g.addField(mapper, g.llType(supTyp), supTyp)

      let member = g.d.dIBuilderCreateMemberType(
        result, "Sup", df, line, g.debugSize(supTyp), 0, 0, 0, g.debugType(supTyp)
      )

      members.add(member)
    elif typ.hasMTypeField():
      discard g.addField(mapper, g.ptrTy, int(g.ptrBits div 8), int(g.ptrBits div 8))
      let
        tnt = g.debugMagicType("TNimType")
        tntp = g.d.dIBuilderCreatePointerType(tnt, g.ptrBits, g.ptrBits, "")
        member = g.d.dIBuilderCreateMemberType(
          result, "m_type", df, line, g.ptrBits, g.ptrBits, 0, 0, tntp
        )

      members.add(member)

    g.debugStructFields(mapper, typ.n, result, members, typ, ty)

  g.d.nimDICompositeTypeSetTypeArray(result, g.d.dIBuilderGetOrCreateArray(members))

proc debugTupleType(g: LLGen, typ: PType): llvm.MetadataRef =
  if typ == nil:
    return

  let sig = hashType(typ, g.config)
  if sig in g.dstructs:
    return g.dstructs[sig]

  let
    name = g.llName(typ, sig)
    (df, line) = g.debugGetLine(typ.sym)
    ty = g.llType(typ)

  # Create struct before setting body in case it's recursive
  result = g.d.dIBuilderCreateStructType(
    df, name, df, line, g.debugSize(ty), 0, 0, nil, [], 0, nil, name
  )

  g.dstructs[sig] = result

  var members = newSeq[MetadataRef]()
  for field in typ.sons:
    let
      mline =
        if field.sym != nil:
          cuint field.sym.info.line
        else:
          line
      member = g.d.dIBuilderCreateMemberType(
        result,
        "tup" & $members.len(),
        df,
        mline,
        g.debugSize(field),
        0,
        g.debugOffset(ty, members.len()),
        0,
        g.debugType(field),
      )
    members.add(member)

  g.d.nimDICompositeTypeSetTypeArray(result, g.d.dIBuilderGetOrCreateArray(members))

proc debugMagicType(g: LLGen, name: string): llvm.MetadataRef =
  g.debugType(g.graph.getCompilerProc(name).typ)

proc debugProcParamType(g: LLGen, sym: PSym, retType: PType): llvm.MetadataRef =
  let dt = g.debugType(sym.typ)
  if g.llPassAsPtr(sym, retType):
    g.d.dIBuilderCreatePointerType(dt, g.ptrBits, g.ptrBits, "")
  else:
    dt

proc debugProcType(g: LLGen, typ: PType, closure: bool): seq[llvm.MetadataRef] =
  let retType =
    if typ[0] == nil:
      nil
    else:
      g.debugType(typ[0])
  if retType != nil and g.isInvalidReturnType(typ[0]):
    result.add(nil)
    result.add(g.d.dIBuilderCreatePointerType(retType, g.ptrBits, g.ptrBits, ""))
  else:
    result.add(retType)

  for param in typ.procParams():
    let at = g.debugProcParamType(param.sym, typ[0])
    result.add(at)

  if closure:
    result.add(g.dtypes[tyPointer])

proc debugGetScope(g: LLGen): llvm.MetadataRef =
  if g.f != nil: g.f.ds else: g.dcu

proc debugLocation(g: LLGen, info: TLineInfo): MetadataRef =
  # This scope ensures that even for top-level statements, the right file is
  # used together with the line and column - it happens this way because we
  # dump such statements from all nim modules into a single `main` function.
  # It's a bit of a hack, better would be to control this more tightly so as to
  # avoid creating all these scopes - we should also be creating
  # a new lexical scope whenever a new block begins - probably somewhere around
  # startBlock..
  let scope = g.d.dIBuilderCreateLexicalBlockFile(
    g.debugGetScope(), g.debugGetFile(info.fileIndex), 0
  )

  g.lc.dIBuilderCreateDebugLocation(
    max(info.line, 1).cuint, max(info.col, 1).cuint, scope, nil
  )

proc debugUpdateLoc(g: LLGen, n: PNode) =
  if g.d == nil:
    return
  if n == nil:
    g.b.setCurrentDebugLocation2(
      g.lc.dIBuilderCreateDebugLocation(1, 1, g.debugGetScope(), nil)
    )
  else:
    let dlm = g.debugLocation(n.info)
    g.b.setCurrentDebugLocation2(dlm)

proc debugUpdateLoc(g: LLGen, sym: PSym) =
  if g.d == nil:
    return
  if sym == nil:
    g.b.setCurrentDebugLocation2(
      g.lc.dIBuilderCreateDebugLocation(1, 1, g.debugGetScope(), nil)
    )
  else:
    let dlm = g.debugLocation(sym.info)
    g.b.setCurrentDebugLocation2(dlm)

proc debugVariable(g: LLGen, sym: PSym, v: llvm.ValueRef, argNo = -1) =
  if g.d == nil:
    return

  let
    dt = g.debugType(sym.typ)
    scope = g.debugGetScope()
    (df, line) = g.debugGetLine(sym)
    vd =
      if argNo == -1:
        g.d.dIBuilderCreateAutoVariable(scope, g.llName(sym), df, line, dt, false, 0, 0)
      else:
        g.d.dIBuilderCreateParameterVariable(
          scope, g.llName(sym), argNo.cuint, df, line, dt, false, 0
        )

  discard g.d.dIBuilderInsertDeclareAtEnd(
    v,
    vd,
    g.d.dIBuilderCreateExpression(nil, 0),
    llvm.valueAsMetadata(g.b.getCurrentDebugLocation()),
    g.b.getInsertBlock(),
  )

proc debugGlobal(g: LLGen, sym: PSym, v: llvm.ValueRef) =
  if g.d == nil:
    return

  let
    dt = g.debugType(sym.typ)
    linkageName = g.llName(sym)
    name = if sym.name.s.len == 0: linkageName else: sym.name.s
    (df, line) = g.debugGetLine(sym)

  let gve = g.d.dIBuilderCreateGlobalVariableExpression(
    g.dcu,
    name,
    linkageName,
    df,
    line,
    dt,
    false,
    dIBuilderCreateExpression(g.d, nil, 0),
    nil,
    0,
  )
  v.nimSetMetadataGlobal(g.dbgKind, g.lc.metadataAsValue(gve))

proc debugFunction(
    g: LLGen, sym: PSym, params: openArray[llvm.MetadataRef], f: llvm.ValueRef
): llvm.MetadataRef =
  let
    (df, line) = g.debugGetLine(sym)
    st = g.d.dIBuilderCreateSubroutineType(df, params)
    linkageName = $f.getValueName()
    name = if sym == nil or sym.name.s.len == 0: linkageName else: sym.name.s
  result = g.d.dIBuilderCreateFunction(
    df, name, linkageName, df, line, st, false, true, line, 0, false
  )
  f.setSubprogram(result)

proc llMagicType(g: LLGen, name: string): llvm.TypeRef =
  g.llType(g.graph.getCompilerProc(name).typ)

proc llStringType(g: LLGen): llvm.TypeRef =
  if g.stringTy == nil:
    if optSeqDestructors in g.config.globalOptions:
      g.stringTy = g.llMagicType("NimStrPayload")
    else:
      g.stringTy = g.llMagicType("NimStringDesc")
  g.stringTy

proc llGenericSeqType(g: LLGen): llvm.TypeRef =
  if g.genericSeqTy == nil:
    g.genericSeqTy = g.llMagicType("TGenericSeq")
  g.genericSeqTy

proc llOpenArrayType(g: LLGen): llvm.TypeRef =
  if g.primitives[tyOpenArray] == nil:
    g.primitives[tyOpenArray] = llvm.structCreateNamed(g.lc, ".openArray")
    llvm.structSetBody(g.primitives[tyOpenArray], [g.ptrTy, g.intTy])
  g.primitives[tyOpenArray]

proc llClosureType(g: LLGen): llvm.TypeRef =
  if g.closureTy == nil:
    g.closureTy = llvm.structCreateNamed(g.lc, ".closure")
    llvm.structSetBody(g.closureTy, [g.ptrTy, g.ptrTy])

  g.closureTy

proc llSeqType(g: LLGen, typ: PType): llvm.TypeRef =
  let typ = typ.skipTypes(abstractInst + tyUserTypeClasses)
  if typ.kind == tyString:
    return g.llStringType()

  let sig = hashType(typ, g.config)

  g.types.withValue(sig, st):
    return st[]

  let
    name = g.llName(typ, sig)
    st = structCreateNamed(g.lc, name)
  g.types[sig] = st

  let dataTy = llvm.arrayType(g.llType(typ.elemType), 0)
  if optSeqDestructors in g.config.globalOptions:
    if typ.elemType.kind == tyEmpty:
      st.structSetBody([g.intTy])
    else:
      st.structSetBody([g.intTy, dataTy])
  else:
    if typ.elemType.kind == tyEmpty:
      st.structSetBody([g.llGenericSeqType()])
    else:
      st.structSetBody([g.llGenericSeqType(), dataTy])
  st

proc llType(g: LLGen, typ: PType, deep = true): llvm.TypeRef =
  case typ.kind
  of tyBool, tyChar, tyNil, tyTyped, tyNone:
    g.primitives[typ.kind]
  of tyGenericBody, tyGenericInst, tyGenericInvocation, tyGenericParam, tyDistinct,
      tyOrdinal, tyTypeDesc, tyAlias, tySink, tyUserTypeClass, tyUserTypeClassInst,
      tyInferred, tyStatic, tyOwned:
    g.llType(typ.lastSon, deep)
  of tyEnum:
    llvm.intTypeInContext(g.lc, g.config.getSize(typ).cuint * 8)
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
  of tyObject:
    g.llStructType(typ, deep)
  of tyTuple:
    g.llTupleType(typ, deep)
  of tySet:
    let size = g.config.getSize(typ).cuint
    if size <= 8:
      llvm.intTypeInContext(g.lc, size * 8)
    else:
      llvm.arrayType(g.primitives[tyUInt8], size)
  of tyRange:
    g.llType(typ.sons[0])
  of tyPtr, tyRef, tyVar, tyLent:
    g.ptrTy # g.llType(typ.elemType, deep = false).pointerType()
  of tySequence:
    if g.primitives[tySequence] == nil:
      g.primitives[tySequence] =
        # In v1, a sequence is a pointer to a "len-capacity-data" blob - in v2, the
        # length moves up a level and a sequence is a "len-ptr" tuple where the
        # pointer points to a "capacity-data" blob
        if optSeqDestructors in g.config.globalOptions:
          let st = g.lc.structCreateNamed(".seq")
          st.structSetBody([g.intTy, g.ptrTy])
          st
        else:
          g.ptrTy # g.llSeqType(typ).pointerType()
    g.primitives[tySequence]
  of tyProc:
    if typ.callConv == ccClosure:
      g.llClosureType()
    else:
      g.ptrTy
  of tyPointer:
    g.ptrTy
  of tyOpenArray, tyVarargs:
    g.llOpenArrayType()
  of tyString:
    if g.primitives[tyString] == nil:
      g.primitives[tyString] =
        if optSeqDestructors in g.config.globalOptions:
          g.llMagicType("NimStringV2")
        else:
          g.ptrTy # g.llStringType().pointerType()
    g.primitives[tyString]
  of tyCString, tyInt .. tyUInt64:
    g.primitives[typ.kind]
  else:
    g.config.internalError("Unhandled type " & $typ.kind)
    nil

template preserve(v: typed, body: untyped): untyped =
  let old = v
  body
  v = old

proc addNimFunction(g: LLGen, name: cstring, ty: llvm.TypeRef): llvm.ValueRef =
  let f = g.m.addFunction(name, ty)
  nimSetFunctionAttributes(f)
  f

proc refFunction(
    g: LLGen, v: llvm.ValueRef, name: cstring, ty: llvm.TypeRef
): llvm.ValueRef =
  # Reference a global potentially declared in a different module - `v` might
  # not be valid at this point
  if g.round > 0:
    let tmp = g.m.getNamedFunction(name)
    if tmp == nil:
      let r = g.addNimFunction(name, ty)
      # TODO copy attributes
      # r.setLinkage(v.getLinkage())
      r
    else:
      tmp
  else:
    v

proc refGlobal(
    g: LLGen, v: llvm.ValueRef, name: cstring, ty: llvm.TypeRef
): llvm.ValueRef =
  # Reference a global potentially declared in a different module - `v` might
  # not be valid at this point
  if g.round > 0:
    let tmp = g.m.getNamedGlobal(name)
    if tmp == nil:
      let r = g.m.addGlobal(ty, name)
      # TODO copy attributes
      # r.setLinkage(v.getLinkage())
      r
    else:
      tmp
  else:
    v

proc refFunction(g: LLGen, v: LLValue, name: cstring, ty: llvm.TypeRef): LLValue =
  LLValue(v: g.refFunction(v.v, name, ty), lode: v.lode, storage: v.storage)

proc refGlobal(g: LLGen, v: LLValue, name: cstring, ty: llvm.TypeRef): LLValue =
  LLValue(v: g.refGlobal(v.v, name, ty), lode: v.lode, storage: v.storage)

proc getInitFunc(g: LLGen): LLFunc =
  if g.init == nil:
    let name =
      if {sfSystemModule, sfMainModule} * g.module.sym.flags == {}:
        g.module.sym.owner.name.s.mangle & "_" & g.module.sym.name.s.mangle
      else:
        g.module.sym.name.s.mangle

    let initType = llvm.functionType(g.voidTy, [])
    g.init =
      g.newLLFunc(g.addNimFunction("." & name & ".init." & $g.round, initType), nil)
    if g.d != nil:
      g.init.ds = g.debugFunction(g.module.sym, [], g.init.f)

    g.init.f.setLinkage(g.defaultFunctionLinkage())

    g.init.sections[secLastBody] = g.section(g.init, secBody)
    discard g.init.startBlock(nil, g.section(g.init, secReturn))

  g.init

proc finalize(g: LLGen) =
  let ret = g.section(g.f, secReturn)

  var last, cur: llvm.BasicBlockRef

  for sk in SectionKind:
    if last == g.f.sections[sk]:
      continue # secLastBody!

    cur = g.f.sections[sk]
    if cur == nil:
      continue

    if last == nil:
      if cur != g.f.f.getEntryBasicBlock():
        cur.moveBasicBlockBefore(g.f.f.getEntryBasicBlock())
    else:
      if sk notin {secLastPreinit, secLastBody}:
        cur.moveBasicBlockAfter(last)
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

proc maxABIAlignment(dl: TargetDataRef, t: llvm.TypeRef): (culonglong, llvm.TypeRef) =
  case t.getTypeKind()
  of StructTypeKind:
    # Instead of returning the struct (whose alignment is based on its members),
    # we'll base maximum alignment on the members instead, which helps creating
    # efficient union stores
    if t.countStructElementTypes() == 0: # empty structs are possible
      (culonglong dl.aBIAlignmentOfType(t), t)
    else:
      var (maxAlign, biggest) = dl.maxABIAlignment(t.structGetTypeAtIndex(0))
      for i in cuint(1) ..< t.countStructElementTypes():
        let
          member = t.structGetTypeAtIndex(i)
          (align, max) = dl.maxABIAlignment(member)
        if align > maxAlign or (
          align == maxAlign and
          # Use store size as tie breaker to get a nice "natural" large type
          dl.storeSizeOfType(max) > dl.storeSizeOfType(biggest)
        ):
          biggest = max
          maxAlign = align
      if biggest == nil:
        (culonglong dl.aBIAlignmentOfType(t), t)
      else:
        (maxAlign, biggest)
  of ArrayTypeKind:
    dl.maxABIAlignment(t.getElementType())
  else:
    (culonglong dl.aBIAlignmentOfType(t), t)

proc unionStore(g: LLGen, fields: openArray[llvm.TypeRef]): seq[llvm.TypeRef] =
  ## Return a struct body suitable for storing the union of the given fields
  ## in a struct.
  ## The storage is constructed such that:
  ## * it is at least as large as the largest field, including any ABI padding
  ##   it might need
  ## * the first member is the most primitive type with the largest alignment
  ##   requirements - this ensures that the struct gets the correct "outer"
  ##   alignment
  var
    mostAligned: TypeRef
    maxSize, maxAlign, maxAlignSize: culonglong

  let dl = g.m.getModuleDataLayout()
  for field in fields:
    # We'll need as many bytes as the largest member of the variant needs
    let bsize = dl.aBISizeOfType(field)
    if bsize > maxSize:
      maxSize = bsize

    let (align, t) = dl.maxABIAlignment(field)

    if align > maxAlign or align == maxAlign and bsize > maxAlignSize:
      maxAlign = align
      mostAligned = t
      maxAlignSize = bsize

  let
    mostAlignedSize = max(dl.aBISizeOfType(mostAligned), culonglong(1))
    members = maxSize div mostAlignedSize
    spill = maxSize - (members * mostAlignedSize)
    head =
      if members == 1:
        mostAligned
      # Using an array here ensures storage is aligned and amenable to bulk
      # memory operations - we could also an array of i8, but empirically this
      # seems to lead to poor codegen as the bytes get copied one-by-one in
      # some cases - this could be investigated better
      else:
        llvm.arrayType(mostAligned, cuint members)

  if spill > 0:
    @[head, llvm.arrayType(g.primitives[tyUInt8], cuint spill)]
  else:
    @[head]

proc addStructFields(
    g: LLGen,
    elements: var seq[TypeRef],
    mapper: var FieldMapper,
    n: PNode,
    ty: llvm.TypeRef,
) =
  p("addStructFields", n, g.depth)
  case n.kind
  of nkRecList:
    for child in n:
      g.addStructFields(elements, mapper, child, ty)
  of nkRecCase:
    # In case objects ("tagged variants" or "tagged unions"), we'll create a
    # struct for every variant, then use a primitive blob type to reserve enough
    # memory based on the largest variant and the most aligned type
    let
      tags = n[0].sym
      tagTy = g.llType(tags.typ)

    fillLoc(tags.loc, locField, n, tags.name.s.rope, OnUnknown)
    let ep = addr elements
    discard g.addField(mapper, tagTy, tags) do(pad: int):
      ep[].add(llvm.arrayType(g.primitives[tyUInt8], cuint pad))
    elements.add(tagTy)

    var
      branches: seq[llvm.TypeRef]
      maxAlign = 1

    for i in 1 ..< n.len:
      assert n[i].kind in {nkOfBranch, nkElse}
      let branch = g.lc.structCreateNamed(caseTypeName(ty, tags, i))

      var
        recElements: seq[TypeRef]
        recMapper = FieldMapper(packed: mapper.packed)
      g.addStructFields(recElements, recMapper, n[i].lastSon, branch)

      let
        tailPad = aligned(recMapper.maxSize, recMapper.maxAlign) - recMapper.maxSize

        dl = g.m.getModuleDataLayout()
        maxABIAlignment = foldl(recElements, max(a, dl.maxABIAlignment(b)[0].int), 0)
      if tailPad > maxABIAlignment:
        elements.add(llvm.arrayType(g.primitives[tyUInt8], cuint tailPad))

      branch.structSetBody(recElements, llvm.False)
      branches.add(branch)

      maxAlign = max(maxAlign, recMapper.maxAlign)

    let
      storeName = caseTypeName(ty, tags, 0)
      storeTy = g.lc.structCreateNamed(storeName)
      storeFields = g.unionStore(branches)
      dl = g.m.getModuleDataLayout()
    storeTy.structSetBody(storeFields, llvm.False)

    discard g.addField(mapper, storeTy, dl.aBISizeOfType(storeTy).int, maxAlign) do(
      pad: int
    ):
      ep[].add(llvm.arrayType(g.primitives[tyUInt8], cuint pad))
    elements.add(storeTy)
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return

    fillLoc(field.loc, locField, n, field.name.s.rope, OnUnknown)

    let fieldTy = g.llType(field.typ)
    let ep = addr elements
    discard g.addField(mapper, fieldTy, field) do(pad: int):
      ep[].add(llvm.arrayType(g.primitives[tyUInt8], cuint pad))
    elements.add(fieldTy)
  else:
    g.config.internalError(n.info, "addStructFields")

proc headerType(g: LLGen, name: string): llvm.TypeRef =
  # Here are replacements for some of the types in the nim standard library that
  # rely on c header parsing to work. This is a crude workaround that needs to
  # be replaced, but works for now, on linux/x86_64

  case name
  of "jmp_buf":
    g.jmpBufTy
  of "TFrame":
    let res = structCreateNamed(g.lc, "TFrame")
    res.structSetBody(
      [
        g.ptrTy, # res.pointerType(),
        g.primitives[tyCString],
        g.primitives[tyInt],
        g.primitives[tyCString],
        g.primitives[tyInt16],
        g.primitives[tyInt16],
        g.primitives[tyInt],
      ],
      llvm.False,
    )

    res
  of "TGenericSeq":
    # typeinfo.nim importc workaround
    g.lc.getTypeByName2("TGenericSeq")
  else:
    nil

proc llStructType(g: LLGen, typ: PType, deep: bool): llvm.TypeRef =
  if typ == nil:
    return

  let typ = typ.skipTypes(abstractPtrs)
  if typ.kind == tyString:
    return g.llStringType

  let sig = hashType(typ, g.config)
  if sig in g.types:
    return g.types[sig]

  let name = g.llName(typ, sig)

  result = g.headerType(name)
  if result != nil:
    g.types[sig] = result
    return

  # check if we have an opaque already - because llStructType may be called
  # recursively, we need to keep track of which opaques are being filled with
  # members (as opposed to simply not having members)
  if sig in g.opaques:
    result = g.opaques[sig]
    if not deep:
      return
    g.opaques.del(sig)
  else:
    # Create struct before setting body in case it's recursive
    result = structCreateNamed(g.lc, name)

    if not deep:
      g.opaques[sig] = result
      return

  g.types[sig] = result

  p("llStructType " & $typ, typ, g.depth)

  var
    elements = newSeq[TypeRef]()
    mapper = FieldMapper(packed: tfPacked in typ.flags)

  if typ[0] != nil:
    let
      supTyp = typ[0].skipTypes(skipPtrs)
      supTy = g.llType(supTyp)
    discard g.addField(mapper, supTy, supTyp)
    elements.add(g.llStructType(supTyp, deep)) # Sup
  elif typ.hasMTypeField():
    discard g.addField(mapper, g.ptrTy, int(g.ptrBits div 8), int(g.ptrBits div 8))
    elements.add(g.ptrTy) # ptr TNimType

  let packed = if tfPacked in typ.flags: llvm.True else: llvm.False

  g.addStructFields(elements, mapper, typ.n, result)

  let
    size = g.config.getSize(typ)
    tailPad = size - mapper.offset
    dl = g.m.getModuleDataLayout()
    maxABIAlignment = foldl(elements, max(a, dl.maxABIAlignment(b)[0].int), 0)

  if tailPad > maxABIAlignment:
    elements.add(llvm.arrayType(g.primitives[tyUInt8], cuint tailPad))

  if tfUnion in typ.flags and elements.len > 0:
    result.structSetBody(g.unionStore(elements), packed)
  elif elements.len > 0:
    result.structSetBody(elements, packed)
  else:
    # cgen adds an empty dummy field in this case - is is needed for the memory
    # allocator to work correctly it seems - investigate
    result.structSetBody([g.primitives[tyUInt8]], packed)

  if tfIncompleteStruct notin typ.flags:
    let
      nimSize = g.config.getSize(typ)
      llvmSize = dl.aBISizeOfType(result)
      tname =
        if typ.sym != nil:
          if typ.sym.owner != nil:
            typ.sym.owner.name.s & "." & typ.sym.name.s
          else:
            typ.sym.name.s
        else:
          $typ
      info =
        if typ.sym != nil:
          typ.sym.info
        else:
          TLineInfo()
    if nimSize != szUnknownSize:
      if nimSize.culonglong != llvmSize:
        g.config.message(
          info,
          warnUser,
          "Nim and LLVM disagree about type size for " & tname & ": " & $nimSize & " vs " &
            $llvmSize,
        )
    else:
      g.config.message(
        info,
        hintUser,
        "Using LLVM size on incomplete object - check ABI and mark with {.completeStruct.}: " &
          tname & " = " & $llvmSize,
      )

proc llTupleType(g: LLGen, typ: PType, deep: bool): llvm.TypeRef =
  if typ == nil:
    return

  let sig = hashType(typ, g.config)
  if sig in g.types:
    return g.types[sig]

  let name = g.llName(typ, sig)

  if sig in g.opaques:
    result = g.opaques[sig]
    if not deep:
      return
    g.opaques.del(sig)
  else:
    # Create struct before setting body in case it's recursive
    result = structCreateNamed(g.lc, name)

    if not deep:
      g.opaques[sig] = result
      return

  g.types[sig] = result

  var elements = newSeq[TypeRef]()
  for t in typ.sons:
    elements.add(g.llType(t))

  p("llTupleType " & $name & " " & $elements, typ, g.depth)

  result.structSetBody(elements)

proc isDeepConstExprLL(n: PNode): bool =
  const preventInheritance = true # Inheritance not supported by llvm yet
  case n.kind
  of nkCharLit .. nkNilLit:
    result = true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = isDeepConstExprLL(n[1])
  of nkCurly, nkBracket, nkPar, nkTupleConstr, nkObjConstr, nkClosure, nkRange:
    for i in ord(n.kind == nkObjConstr) ..< n.len:
      if not isDeepConstExprLL(n[i]):
        return false
    if n.typ.isNil:
      result = true
    else:
      let t = n.typ.skipTypes({tyGenericInst, tyDistinct, tyAlias, tySink, tyOwned})
      if t.kind in {tyRef, tyPtr} or tfUnion in t.flags:
        return false
      if t.kind == tyObject:
        if preventInheritance and t[0] != nil:
          result = false
        elif isCaseObj(t.n):
          result = false
        else:
          result = true
      else:
        result = true
  else:
    discard

proc canMove(g: LLGen, n: PNode): bool =
  if n == nil:
    return false

  # From cgen
  if n.kind == nkBracket:
    # This needs to be kept consistent with 'const' seq code
    # generation!
    if not isDeepConstExprLL(n) or n.len == 0:
      if skipTypes(n.typ, abstractVarRange).kind == tySequence:
        return true
  elif n.kind in nkStrKinds and n.strVal.len == 0:
    # Empty strings are codegen'd as NIM_NIL so it's just a pointer copy
    return true

  n.kind in nkCallKinds

template withRecCase(n: PNode, ty: llvm.TypeRef, v: llvm.ValueRef, body: untyped) =
  let
    tags = n[0].sym
    tagTy {.inject.} = g.llType(tags.typ)
    tagElement {.inject.} = g.addField(mapper, tagTy, tags)
    tagGEP {.inject.} =
      g.b.buildStructGEP2(ty, v, cuint tagElement, g.nn("vari.tag", v))
    tag {.inject.} = g.b.buildLoad2(tagTy, tagGEP)

  let
    maxAlign = foldl(1 ..< n.len, max(a, g.maxAlign(n[b].lastSon)), 1)
    storeTy = g.lc.getTypeByName2(caseTypeName(ty, tags, 0))
    storeElement {.inject.} = g.addField(
      mapper, storeTy, g.m.getModuleDataLayout().aBISizeOfType(storeTy).int, maxAlign
    )
    storeGEP {.inject.} =
      g.b.buildStructGEP2(ty, v, cuint storeElement, g.nn("vari.branch", v))
    caseend = g.b.appendBasicBlockInContext(g.lc, g.nn("vari.tag.end", v))

  var hasElse = false

  for i in 1 ..< n.len:
    let
      branch {.inject.} = n[i]
      ctrue = g.b.appendBasicBlockInContext(g.lc, g.nn("vari.tag.true", branch))
      branchTy {.inject.} = g.lc.getTypeByName2(caseTypeName(ty, tags, i))

    if branch.kind == nkOfBranch:
      var length = branch.len
      for j in 0 .. length - 2:
        let cmp =
          if branch[j].kind == nkRange:
            let
              s = branch[j][0].intVal
              e = branch[j][1].intVal

              scmp = g.b.buildICmp(
                llvm.IntSGE,
                tag,
                llvm.constInt(tag.typeOfX(), s.culonglong, llvm.True),
                g.nn("vari.rng.s", branch),
              )
              ecmp = g.b.buildICmp(
                llvm.IntSLE,
                tag,
                llvm.constInt(tag.typeOfX(), e.culonglong, llvm.True),
                g.nn("vari.tag.rng.e", branch),
              )

            g.b.buildAnd(scmp, ecmp, g.nn("vari.tag.rng.cmp", branch))
          else:
            let k = branch[j].intVal
            g.b.buildICmp(
              llvm.IntEQ,
              tag,
              llvm.constInt(tag.typeOfX(), k.culonglong, llvm.True),
              g.nn("vari.tag.cmp", branch),
            )

        let cfalse = g.b.appendBasicBlockInContext(g.lc, g.nn("vari.tag.false", branch))
        discard g.b.buildCondBr(cmp, ctrue, cfalse)
        g.b.positionBuilderAtEnd(cfalse)
    else:
      hasElse = true
      discard g.b.buildBr(ctrue)

    let x = g.b.getInsertBlock()
    g.b.positionBuilderAtEnd(ctrue)

    var recMapper {.inject.} = FieldMapper(packed: mapper.packed)

    body

    discard g.b.buildBr(caseend)
    g.b.positionBuilderAtEnd(x)

  if not hasElse:
    discard g.b.buildBr(caseend)
  g.b.positionAndMoveToEnd(caseend)

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef)

proc genMarkerFields(
    g: LLGen, mapper: var FieldMapper, n: PNode, ty: llvm.TypeRef, v, op: llvm.ValueRef
) =
  if n == nil:
    return
  case n.kind
  of nkRecList:
    for child in n:
      g.genMarkerFields(mapper, child, ty, v, op)
  of nkRecCase:
    withRecCase(n, ty, v):
      g.genMarkerFields(recMapper, branch.lastSon, branchTy, storeGEP, op)
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return

    g.debugUpdateLoc(field)
    let
      element = g.addField(mapper, g.llType(field.typ), field)
      gep = g.b.buildStructGEP2(ty, v, cuint element, g.nn("mk", field))
    g.genMarker(field.typ, gep, op)
  else:
    g.config.internalError(n.info, "genMarker()")

proc genMarker(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ == nil:
    return
  let
    typ = typ.skipTypes(irrelevantForBackend)
    ty = g.llType(typ)
  case typ.kind
  of tyArray:
    let arraySize = g.constNimInt(g.config.lengthOrd(typ[0]))

    g.withLoop(arraySize, "mk.arr"):
      var gep =
        if ty.getTypeKind == llvm.ArrayTypeKind:
          g.b.buildInboundsGEP2(ty, v, [g.gep0, i], g.nn("while.data"))
        else:
          g.b.buildInboundsGEP2(ty, v, [i], g.nn("while.data"))

      g.genMarker(typ[1], gep, op)
  of tyObject:
    var mapper = FieldMapper(packed: tfPacked in typ.flags)
    if typ[0] != nil: # Skip super type field
      let supTyp = typ[0].skipTypes(skipPtrs)
      discard g.addField(mapper, g.llType(supTyp), supTyp)
      g.genMarker(supTyp, v, op)
    elif typ.hasMTypeField: # Skip m_type in inheritable root object
      discard g.addField(mapper, ty, int(g.ptrBits div 8), int(g.ptrBits div 8))

    # TODO what if there's a pointer hiding in a union?
    if typ.n != nil and tfUnion notin typ.flags:
      g.genMarkerFields(mapper, typ.n, ty, v, op)
  of tyTuple:
    for i in 0 ..< typ.len:
      let gep = g.b.buildStructGEP2(ty, v, cuint i, g.nn("mk.tuple.field" & $i, v))
      g.genMarker(typ[i], gep, op)
  of tyRef, tySequence:
    let vl = g.b.buildLoad2(ty, v)
    discard g.callCompilerProc("nimGCVisit", [vl, op])
  of tyString:
    if tfHasAsgn notin typ.flags:
      let vl = g.b.buildLoad2(ty, v)
      discard g.callCompilerProc("nimGCVisit", [vl, op])
  of tyProc:
    if typ.callConv == ccClosure:
      let
        ve = g.buildClosureEnvGEP(v)
        vel = g.b.buildLoad2(g.ptrTy, ve)
      discard g.callCompilerProc("nimGCVisit", [vel, op])
  else:
    discard

proc genMarkerSeq(g: LLGen, typ: PType, v, op: llvm.ValueRef) =
  if typ.elemType.kind == tyEmpty:
    return

  let
    seqTy = g.llSeqType(typ)
    seqlen = g.loadNimSeqLen(v)

  g.withLoop(seqlen, "mk.seq"):
    let gep = g.buildNimSeqDataGEP(seqTy, v, i)
    g.genMarker(typ[0], gep, op)

proc genMarkerProcBody(g: LLGen, f: llvm.ValueRef, typ: PType) =
  let llf = g.newLLFunc(f, nil)
  if g.d != nil:
    llf.ds = g.debugFunction(typ.sym, [nil, g.dtypes[tyPointer], g.dtypes[tyInt]], f)

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
        let
          dt =
            if typ.kind == tySequence:
              g.debugType(typ)
            else:
              g.d.dIBuilderCreatePointerType(
                g.debugType(typ.elemType), g.ptrBits, g.ptrBits, ""
              )
          (df, line) = g.debugGetLine(typ.sym)
          vd = g.d.dIBuilderCreateParameterVariable(
            llf.ds, $v.getValueName(), 1, df, line, dt, false, 0
          )
        discard g.d.dIBuilderInsertDeclareAtEnd(
          vs,
          vd,
          g.d.dIBuilderCreateExpression(nil, 0),
          valueAsMetadata(g.b.getCurrentDebugLocation()),
          g.b.getInsertBlock(),
        )

        let opd = g.d.dIBuilderCreateParameterVariable(
          llf.ds, $op.getValueName(), 2, df, line, g.dtypes[tyInt], false, 0
        )
        discard g.d.dIBuilderInsertDeclareAtEnd(
          ops,
          opd,
          g.d.dIBuilderCreateExpression(nil, 0),
          valueAsMetadata(g.b.getCurrentDebugLocation()),
          g.b.getInsertBlock(),
        )

      if typ.kind == tySequence:
        g.genMarkerSeq(typ, v, op)
      else:
        g.genMarker(typ[0], v, op)

      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      discard g.b.buildRetVoid()

    g.finalize()

proc genMarkerProc(g: LLGen, typ: PType, sig: SigHash): llvm.ValueRef =
  if g.config.selectedGC < gcMarkAndSweep:
    return

  let
    typ = typ.skipTypes(abstractInstOwned)
    name = "Marker_" & g.llName(typ, sig)
    ft = llvm.functionType(g.voidTy, [g.ptrTy, g.intTy], false)

  if sig in g.markers:
    return g.refFunction(g.markers[sig], name, ft)

  result = g.addNimFunction(name, ft)
  g.markers[sig] = result

  result.setLinkage(g.defaultFunctionLinkage())

  # Can't generate body yet - some magics might not yet exist
  g.markerBody.add((result, typ))

proc genGlobalMarkerProc(g: LLGen, sym: PSym, v: llvm.ValueRef): llvm.ValueRef =
  let
    name = ".marker.g." & g.llName(sym)
    ft = llvm.functionType(g.voidTy, @[], false)

  if sym.id in g.gmarkers:
    return g.refFunction(g.gmarkers[sym.id], name, ft)

  result = g.addNimFunction(name, ft)
  g.gmarkers[sym.id] = result

  result.setLinkage(g.defaultFunctionLinkage())

  let f = g.newLLFunc(result, nil)

  if g.d != nil:
    f.ds = g.debugFunction(sym, [], result)

  g.withFunc(f):
    g.debugUpdateLoc(sym)

    g.withBlock(g.section(g.f, secBody)):
      var v = v
      let typ = sym.typ.skipTypes(abstractInst)

      g.genMarker(typ, v, g.constNimInt(0))
      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      discard g.b.buildRetVoid()

    g.finalize()

proc registerGcRoot(g: LLGen, sym: PSym, v: llvm.ValueRef) =
  if g.config.selectedGC in {gcMarkAndSweep, gcHooks, gcRefc} and
      optOwnedRefs notin g.config.globalOptions and sym.typ.containsGarbageCollectedRef() and
      sym.id notin g.gmarkers:
    g.gcRoots.add((sym, v))

proc genGcRegistrar(g: LLGen, sym: PSym, v: llvm.ValueRef) =
  # This is a bit backwards - better would be to rewrite nimRegisterGlobalMarker!
  if g.registrar.isNil:
    let
      name = ".nlvm.registrar"
      registrarType = llvm.functionType(g.voidTy, @[], false)
      registrar = g.addNimFunction(name, registrarType)

    registrar.setLinkage(g.defaultFunctionLinkage())

    let
      ctorsInit = llvm.constStructInContext(
        g.lc, [g.constInt32(65535), registrar, constNull(g.ptrTy)]
      )
      ctorsType = ctorsInit.typeOfX()
      ctorsArrayType = llvm.arrayType(ctorsType, 1)
      ctors = g.m.addGlobal(ctorsArrayType, "llvm.global_ctors")

    ctors.setLinkage(llvm.AppendingLinkage)
    ctors.setInitializer(llvm.constArray(ctorsType, [ctorsInit]))

    let f = g.newLLFunc(registrar, nil)
    if g.d != nil:
      f.ds = g.debugFunction(nil, [], registrar)
    g.registrar = f

    g.withFunc(g.registrar):
      g.debugUpdateLoc(sym)
      g.withBlock(g.section(g.f, secReturn)):
        discard g.b.buildRetVoid()

  let prc = g.genGlobalMarkerProc(sym, v)

  g.withFunc(g.registrar):
    g.debugUpdateLoc(sym)
    g.withBlock(g.section(g.f, secBody)):
      discard g.callCompilerProc("nimRegisterGlobalMarker", [prc])

proc genTypeInfoInit(
    g: LLGen,
    t: PType,
    ntlt, lt: llvm.TypeRef,
    baseVar, nodeVar, finalizerVar, markerVar, deepcopyVar, typeInfoV2: llvm.ValueRef,
): llvm.ValueRef =
  let
    dl = g.m.getModuleDataLayout()
    sizeVar =
      if lt == nil:
        g.ni0
      else:
        g.constNimInt(dl.aBISizeOfType(lt).int)
    alignVar =
      if lt == nil:
        g.constNimInt(1)
      else:
        g.constNimInt(dl.preferredAlignmentOfType(lt).int)
    kind =
      if t.isObjLackingTypeField():
        tyPureObject
      elif t.kind == tyProc and t.callConv == ccClosure:
        tyTuple
      else:
        t.kind
    kindVar = g.constInt8(int8(ord(kind)))

  var flags = 0'i8
  if not containsGarbageCollectedRef(t):
    flags = flags or 1
  if not g.graph.canFormAcycle(t) or (t.kind == tyProc and t.callConv == ccClosure):
    flags = flags or 2
  if t.kind == tyEnum:
    var hasHoles = false
    for i in 0 .. t.len - 1:
      let n = t.n[i].sym
      # why isn't tfEnumHasHoles always set? is it ever set at all?
      if n.position != i or tfEnumHasHoles in t.flags:
        hasHoles = true
    if hasHoles:
      # C gen overwrites flags in this case!
      flags = 1 shl 2

  let flagsVar = g.constInt8(flags)

  var values =
    @[
      sizeVar, alignVar, kindVar, flagsVar, baseVar, nodeVar, finalizerVar, markerVar,
      deepcopyVar,
    ]

  if isDefined(g.config, "nimSeqsV2"):
    values.add(typeInfoV2)

  if isDefined(g.config, "nimTypeNames"):
    var typename = typeToString(if t.typeInst != nil: t.typeInst else: t, preferName)
    if typename == "ref object" and t.skipTypes(skipPtrs).sym != nil:
      typename = "anon ref object from " & g.config $ t.skipTypes(skipPtrs).sym.info
    let lltn = g.constCStringPtr(typename)
    values.add([lltn, constNull(g.ptrTy), g.ni0, g.ni0])

  llvm.constNamedStruct(ntlt, values)

proc genNodeInfo(g: LLGen, typ: PType): llvm.ValueRef
proc genObjectNodeInfo(g: LLGen, typ: PType, n: PNode, suffix: string): llvm.ValueRef
proc genTypeInfo(g: LLGen, typ: PType): llvm.ValueRef

proc constNimNodeNone(g: LLGen, length: int): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  llvm.constNamedStruct(
    tnn,
    [
      g.constInt8(0),
      constNull(els[1]),
      constNull(els[2]),
      constNull(els[3]),
      g.constInt64(length),
      constNull(els[5]),
    ],
  )

proc constNimNodeSlot(
    g: LLGen, offset, typeInfo: llvm.ValueRef, name: string
): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  llvm.constNamedStruct(
    tnn,
    [
      g.constInt8(1),
      offset,
      typeInfo,
      g.b.buildGlobalStringPtr(name, ".nimnode.slot." & name),
      constNull(els[4]),
      constNull(els[5]),
    ],
  )

proc constNimNodeList(g: LLGen, nodes: openArray[llvm.ValueRef]): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  var nodesVal: llvm.ValueRef

  if nodes.len == 0:
    nodesVal = constNull(els[5])
  else:
    let nodesType = llvm.arrayType(g.ptrTy, nodes.len.cuint) # ptr TNimNode
    let tmp = g.m.addPrivateConstant(nodesType, g.nn(".nodes"))
    tmp.setInitializer(constArray(g.ptrTy, nodes)) # ptr TNimNode
    nodesVal = constBitCast(tmp, els[5])

  llvm.constNamedStruct(
    tnn,
    [
      g.constInt8(2),
      constNull(els[1]),
      constNull(els[2]),
      constNull(els[3]),
      g.constInt64(nodes.len),
      nodesVal,
    ],
  )

proc constNimNodeCase(
    g: LLGen,
    offset, typeInfo: llvm.ValueRef,
    name: string,
    nodesLen: int,
    nodes: openArray[llvm.ValueRef],
): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  var nodesVal: llvm.ValueRef

  if nodes.len == 0:
    nodesVal = constNull(els[5])
  else:
    let nodesType = llvm.arrayType(g.ptrTy, nodes.len.cuint) # ptr TNimNode
    let tmp = g.m.addPrivateConstant(nodesType, g.nn(".nodes"))
    tmp.setInitializer(constArray(g.ptrTy, nodes)) # ptr TNimNode
    nodesVal = constBitCast(tmp, els[5])

  llvm.constNamedStruct(
    tnn,
    [
      g.constInt8(3),
      offset,
      typeInfo,
      g.b.buildGlobalStringPtr(name, ".nimnode.case." & name),
      g.constInt64(nodesLen),
      nodesVal,
    ],
  )

proc genObjectNodeInfoInit(
    g: LLGen, typ: PType, n: PNode, suffix: string
): llvm.ValueRef =
  case n.kind
  of nkRecList:
    let l = n.len
    if l == 1:
      result = g.genObjectNodeInfoInit(typ, n[0], suffix)
    else:
      var fields: seq[ValueRef] = @[]
      for i in 0 .. l - 1:
        fields.add(g.genObjectNodeInfo(typ, n[i], suffix & "." & $i))
      result = g.constNimNodeList(fields)
  of nkRecCase:
    let
      tags = n[0].sym
      variants = g.config.lengthOrd(tags.typ).toInt # TODO Int128

    var fields: seq[ValueRef]
    newSeq(fields, variants + 1)

    for i in 1 ..< n.len:
      let b = n[i]
      let bi = g.genObjectNodeInfo(typ, b.lastSon, suffix & "." & $i)
      case b.kind
      of nkOfBranch:
        for j in 0 .. b.len - 2:
          if b[j].kind == nkRange:
            # TODO Int128
            for a in getOrdValue(b[j][0]).toInt() .. getOrdValue(b[j][1]).toInt():
              fields[a] = bi
          else:
            fields[getOrdValue(b[j]).toInt] = bi # TODO Int128
      else:
        fields[variants] = bi

    # fill in holes or last element when there's no else
    for i in 0 .. variants:
      if fields[i].isNil:
        fields[i] = constNull(g.ptrTy) # ptr TNimNode

    result = g.constNimNodeCase(
      g.constOffsetOf(typ, tags), g.genTypeInfo(tags.typ), tags.name.s, variants, fields
    )
  of nkSym:
    let field = n.sym
    result = g.constNimNodeSlot(
      g.constOffsetOf(typ, field), g.genTypeInfo(field.typ), field.name.s
    )
  else:
    g.config.internalError(n.info, "genObjectNodeInfoInit")

proc genObjectNodeInfo(g: LLGen, typ: PType, n: PNode, suffix: string): llvm.ValueRef =
  let sig = hashType(typ, g.config)
  if sig in g.nodeInfos and len(suffix) == 0:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & g.llName(typ, sig) & suffix
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  if len(suffix) == 0:
    g.nodeInfos[sig] = result
  result.setInitializer(g.genObjectNodeInfoInit(typ, n, suffix))

proc genTupleNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  let tnn = g.llMagicType("TNimNode")

  var fields: seq[ValueRef] = @[]

  let sig = hashType(t, g.config)
  let prefix = ".nodeinfo." & g.llName(t, sig) & "."

  for i in 0 ..< t.len:
    let
      name = prefix & $i
      field = g.m.addPrivateConstant(tnn, name)
      offset = constPtrToInt(
        constGEP2(g.llType(t), constNull(g.ptrTy), [g.gep0, g.constGEPIdx(i)]),
        g.int64Ty,
      )
      fieldInit = g.constNimNodeSlot(offset, g.genTypeInfo(t.sons[i]), "Field" & $i)

    field.setInitializer(fieldInit)
    fields.add(field)

  g.constNimNodeList(fields)

proc genTupleNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t, g.config)
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

  let sig = hashType(t, g.config)
  let prefix = ".nodeinfo." & g.llName(t, sig) & "."
  var fields: seq[ValueRef] = @[]
  for i in 0 ..< l:
    let
      name = prefix & $i
      n = t.n[i].sym
      fieldName = if n.ast == nil: n.name.s else: n.ast.strVal

    # type info not needed for enum members
    let fieldInit =
      g.constNimNodeSlot(g.constInt64(n.position), constNull(els[2]), fieldName)

    let field = g.m.addPrivateConstant(tnn, name)
    field.setInitializer(fieldInit)
    fields.add(field)

  g.constNimNodeList(fields)

  # TODO c gen sets ntfEnumHole as well on TNimType after generating TNimNode.. odd.

proc genEnumNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let sig = hashType(t, g.config)
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
  let sig = hashType(t, g.config)
  if sig in g.nodeInfos:
    return g.nodeInfos[sig]

  let name = ".nodeinfo." & g.llName(t, sig)
  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  g.nodeInfos[sig] = result
  result.setInitializer(g.genSetNodeInfoInit(t))

proc fakeclosureTy(g: LLGen, owner: PSym): PType =
  # proc + env ref
  result = newType(tyTuple, nextTypeId g.idgen, owner)
  result.rawAddSon(newType(tyPointer, nextTypeId g.idgen, owner))
  var r = newType(tyRef, nextTypeId g.idgen, owner)
  let obj = createObj(g.graph, g.idgen, owner, owner.info, final = false)
  r.rawAddSon(obj)
  result.rawAddSon(r)

proc genNodeInfo(g: LLGen, typ: PType): llvm.ValueRef =
  case typ.kind
  of tyObject:
    g.genObjectNodeInfo(typ, typ.n, "")
  of tyTuple:
    g.genTupleNodeInfo(typ)
  of tyEnum:
    g.genEnumNodeInfo(typ)
  of tySet:
    g.genSetNodeInfo(typ)
  of tyProc:
    if typ.callConv == ccClosure:
      g.genTupleNodeInfo(g.fakeclosureTy(typ.owner))
    else:
      constNull(g.ptrTy) # ptr TNimNode
  else:
    constNull(g.ptrTy) # ptr TNimNode

proc genTypeInfoV1Base(g: LLGen, typ: PType): llvm.ValueRef =
  if typ.kind == tyArray:
    g.genTypeInfo(typ[1])
  elif typ.kind in {
    tyUncheckedArray, tySequence, tyRef, tyPtr, tyRange, tySet, tyObject
  } and typ.len > 0 and typ[0] != nil:
    let base =
      if typ.kind == tyObject:
        typ[0].skipTypes(skipPtrs)
      else:
        typ[0]

    if typ.kind == tyPtr and base.kind == tyObject and incompleteType(base):
      constNull(g.ptrTy)
    else:
      g.genTypeInfo(base)
  else:
    constNull(g.ptrTy) # g.llMagicType("TNimType").pointerType().constNull()

proc genTypeInfoV1(
    g: LLGen, typ: PType, typeInfoV2: llvm.ValueRef = nil
): llvm.ValueRef =
  let
    typ = typ.skipTypes(irrelevantForBackend + tyUserTypeClasses)
    sig = hashType(typ, g.config)
  if sig in g.typeInfos:
    return g.typeInfos[sig]

  let name = ".typeinfo." & g.llName(typ, sig)

  p("genTypeInfo", typ, g.depth + 1)

  let
    ntlt = g.llMagicType("TNimType")
    lt =
      if typ.kind == tyEmpty:
        nil
      else:
        g.llType(typ)
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
      sym = typ.sym
      dt = g.debugMagicType("TNimType")
      (df, line) = g.debugGetLine(sym)

    let gve = g.d.dIBuilderCreateGlobalVariableExpression(
      g.dcu,
      name,
      "",
      df,
      line,
      dt,
      false,
      dIBuilderCreateExpression(g.d, nil, 0),
      nil,
      0,
    )
    result.nimSetMetadataGlobal(g.dbgKind, g.lc.metadataAsValue(gve))

  g.typeInfos[sig] = result

  let
    baseVar = g.genTypeInfoV1Base(typ)
    nodeVar = g.genNodeInfo(typ)
    finalizerVar = llvm.constNull(els[6])
    markerVar =
      if typ.kind in {tySequence, tyRef}:
        let tmp = g.genMarkerProc(typ, sig)
        if tmp == nil:
          llvm.constNull(els[7])
        else:
          tmp
      else:
        llvm.constNull(els[7])
    deepcopyVar = llvm.constNull(els[8])

  result.setInitializer(
    g.genTypeInfoInit(
      typ, ntlt, lt, baseVar, nodeVar, finalizerVar, markerVar, deepcopyVar, typeInfoV2
    )
  )

proc genTypeInfo2Name(g: LLGen, t: PType): string =
  var res = "|"
  var it = t
  while it != nil:
    it = it.skipTypes(skipPtrs)
    if it.sym != nil and tfFromGeneric notin it.flags:
      var m = it.sym.owner
      while m != nil and m.kind != skModule:
        m = m.owner
      if m == nil or sfSystemModule in m.flags:
        # produce short names for system types:
        res.add it.sym.name.s
      else:
        var p = m.owner
        if p != nil and p.kind == skPackage:
          res.add p.name.s & "."
        res.add m.name.s & "."
        res.add it.sym.name.s
    else:
      res.add $hashType(it, g.config)
    res.add "|"
    it = it[0]
  res

proc isTrivialProc(g: ModuleGraph, s: PSym): bool {.inline.} =
  getBody(g, s).len == 0

proc genHook(g: LLGen, t: PType, op: TTypeAttachedOp): llvm.ValueRef =
  let theProc = getAttachedOp(g.graph, t, op)
  if theProc != nil and not isTrivialProc(g.graph, theProc):
    # the prototype of a destructor is ``=destroy(x: var T)`` and that of a
    # finalizer is: ``proc (x: ref T) {.nimcall.}``. We need to check the calling
    # convention at least:
    if theProc.typ == nil or theProc.typ.callConv != ccNimCall:
      g.config.internalError(
        theProc.name.s & " needs to have the 'nimcall' calling convention"
      )

    g.genFunctionWithBody(theProc).v
  else:
    constNull(g.ptrTy)

proc genTypeInfoV2(g: LLGen, typ: PType): llvm.ValueRef =
  let
    origType = typ
    # distinct types can have their own destructors
    typ = typ.skipTypes(irrelevantForBackend + tyUserTypeClasses - {tyDistinct})
    sig = hashType(origType, g.config)

  if sig in g.typeInfosV2:
    return g.typeInfosV2[sig]

  let
    nimTypeTy = g.llMagicType("TNimTypeV2")
    name = ".typeinfoV2." & g.llName(origType, sig)

  result = g.m.addPrivateConstant(nimTypeTy, name)
  g.typeInfosV2[sig] = result

  var flags = 0
  if not g.graph.canFormAcycle(typ):
    flags = flags or 1
  let
    dl = g.m.getModuleDataLayout()
    lt = g.llType(typ)
    destroyImpl = g.genHook(typ, attachedDestructor)
    sizeVar =
      if lt == nil:
        g.ni0
      else:
        g.constNimInt(dl.aBISizeOfType(lt).int)
    alignVar =
      if lt == nil:
        g.constNimInt(1)
      else:
        g.constNimInt(dl.preferredAlignmentOfType(lt).int)
    nameVar =
      if typ.kind in {tyObject, tyDistinct}:
        if incompleteType(typ):
          g.config.internalError(
            "request for RTTI generation for incomplete object: " & typeToString(typ)
          )
        g.constCStringPtr(g.genTypeInfo2Name(typ))
      else:
        constNull(g.ptrTy)
    traceImpl = g.genHook(typ, attachedTrace)
    v1Var =
      if typ.kind == tyObject and typ.len > 0 and typ[0] != nil and
          optEnableDeepCopy in g.config.globalOptions:
        g.genTypeInfoV1(typ, result)
      else:
        constNull(g.ptrTy)
    flagsVar = g.constNimInt(flags)

    values = [destroyImpl, sizeVar, alignVar, nameVar, traceImpl, v1Var, flagsVar]

  result.setInitializer(llvm.constNamedStruct(nimTypeTy, values))

proc genTypeInfo(g: LLGen, typ: PType): llvm.ValueRef =
  if optTinyRtti in g.config.globalOptions:
    g.genTypeInfoV2(typ)
  else:
    g.genTypeInfoV1(typ)

proc llPassAsPtr(g: LLGen, s: PSym, retType: PType): bool =
  # We strive to replicate a similar parameter passing strategy as Nim
  # because for FFI in particular, existing code implicitly tends to depend on
  # the details herein - see also ccgIntroducedPtr

  let pt = s.typ.skipTypes(typedescInst + tyUserTypeClasses)

  if tfByRef in pt.flags:
    true
  elif tfByCopy in pt.flags:
    # Hack to work around jmp_buf being an array but represented as an object
    # in the std lib
    if $s.typ.sym.loc.r == "jmp_buf": true else: false
  elif s.position == 0 and retType != nil and retType.kind == tyLent:
    not (
      pt.kind in {tyVar, tyOpenArray, tyVarargs, tyRef, tyPtr, tyPointer} or
      pt.kind == tySet and g.llType(pt).getTypeKind() != IntegerTypeKind
    )
  else:
    case pt.kind
    of tyObject:
      if s.typ.sym != nil and sfForward in s.typ.sym.flags:
        # forwarded objects are *always* passed by pointers for consistency!
        true
      elif (optByRef in s.options) or
          (getSize(g.config, pt) > g.config.target.floatSize * 3):
        true # requested anyway
      elif retType != nil and retType.kind == tyLent:
        true
      elif (tfFinal in pt.flags) and (pt[0] == nil):
        false # no need, because no subtyping possible
      else:
        true
          # ordinary objects are always passed by reference,
          # otherwise casting doesn't work
    of tyTuple:
      if retType != nil and retType.kind == tyLent:
        true
      else:
        (getSize(g.config, pt) > g.config.target.floatSize * 3) or
          (optByRef in s.options)
    of tyArray, tyUncheckedArray:
      true
    of tyOpenArray, tyVarargs:
      false
    of tySet:
      let size = g.config.getSize(pt).cuint
      size > 8
    of tyProc:
      pt.callConv == ccClosure
    else:
      false

proc llProcParamType(g: LLGen, s: PSym, retType: PType): llvm.TypeRef =
  if g.llPassAsPtr(s, retType):
    g.ptrTy # g.llType(s.typ).pointerType
  else:
    g.llType(s.typ)

proc paramStorageLoc(param: PSym): TStorageLoc =
  if param.typ.skipTypes({tyVar, tyTypeDesc, tyLent}).kind notin
      {tyArray, tyOpenArray, tyUncheckedArray, tyVarargs}: OnStack else: OnUnknown

proc llProcType(g: LLGen, typ: PType, closure: bool): llvm.TypeRef =
  var
    retType =
      if typ[0] == nil:
        g.voidTy
      else:
        g.llType(typ[0])
    argTypes = newSeq[llvm.TypeRef]()

  if g.isInvalidReturnType(typ[0]):
    argTypes.add(g.ptrTy) # retType.pointerType()
    if typ[0].sym != nil:
      incl(typ[0].sym.loc.flags, lfIndirect)
      typ[0].sym.loc.storage = OnUnknown
    retType = g.voidTy

  for param in typ.procParams():
    fillLoc(
      param.sym.loc, locParam, param, param.sym.name.s.mangle.rope,
      param.sym.paramStorageLoc,
    )

    if g.llPassAsPtr(param.sym, typ[0]):
      incl(param.sym.loc.flags, lfIndirect)
      param.sym.loc.storage = OnUnknown

    let symTyp = param.sym.typ.skipTypes({tyGenericInst})
    if skipTypes(symTyp, {tyVar, tyLent, tySink}).kind in {tyOpenArray, tyVarargs}:
      if symTyp.kind in {tyVar, tyLent}:
        param.sym.loc.storage = OnUnknown
      # In call arguments, we'll split `.openArray` into its constituent parts
      # to match the cgen - then we'll reconstruct the .openArray in the
      # function preamble - in the cgen, this is called "reification" but
      # is done in a slightly different manner
      argTypes.add(g.ptrTy)
      argTypes.add(g.intTy)
    else:
      let at = g.llProcParamType(param.sym, typ[0])
      argTypes.add(at)

  if closure:
    argTypes.add(g.ptrTy) # environment

  llvm.functionType(retType, argTypes, tfVarArgs in typ.flags)

proc fieldIndexFields(
    g: LLGen,
    mapper: var FieldMapper,
    n: PNode,
    ty: llvm.TypeRef,
    sym: PSym,
    union: bool,
): seq[FieldPath] =
  case n.kind
  of nkRecList:
    for child in n:
      result = g.fieldIndexFields(mapper, child, ty, sym, union)
      if result.len > 0:
        return
  of nkRecCase:
    assert not union, "case + union not supported"
    let
      tags = n[0].sym
      tagTy = g.llType(tags.typ)
      tagElement = g.addField(mapper, tagTy, tags)

    if tags.name.s == sym.name.s:
      return @[FieldPath((ty, tagElement, tagTy))]

    let
      dl = g.m.getModuleDataLayout()
      storeName = caseTypeName(ty, tags, 0)
      storeTy = g.lc.getTypeByName2(storeName)
      maxAlign = foldl(1 ..< n.len, max(a, g.maxAlign(n[b].lastSon)), 1)
      storeElement =
        g.addField(mapper, storeTy, dl.aBISizeOfType(storeTy).int, maxAlign)

    for j in 1 ..< n.len:
      var recMapper = FieldMapper(packed: mapper.packed)
      let
        variantTy = g.lc.getTypeByName2(caseTypeName(ty, tags, j))
        inRec = g.fieldIndexFields(recMapper, n[j].lastSon, variantTy, sym, false)
      if inRec.len > 0:
        return @[FieldPath((ty, storeElement, variantTy))] & inRec
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return @[]

    let element = g.addField(mapper, g.llType(field.typ), field)

    if n.sym.name.s == sym.name.s:
      if union:
        return @[FieldPath((ty, 0, g.llType(field.typ)))]

      return @[FieldPath((ty, element, nil))]
  else:
    g.config.internalError(n.info, "Unhandled field index")
  return @[]

proc fieldIndex(g: LLGen, typ: PType, sym: PSym): seq[FieldPath] =
  let
    typ = typ.skipTypes(abstractInst)
    ty = g.llType(typ)

  assert typ.kind in {tyObject, tyTuple}, $typ

  var mapper = FieldMapper(packed: tfPacked in typ.flags)
  if typ.kind != tyTuple:
    if typ[0] != nil:
      # Look in base types first
      let supTyp = typ[0].skipTypes(skipPtrs)
      discard g.addField(mapper, g.llType(supTyp), supTyp)
      let s = g.fieldIndex(supTyp, sym)
      if s.len > 0:
        return @[FieldPath((ty, 0, nil))] & s
    elif typ.hasMTypeField: # Skip m_type in inheritable root object
      discard g.addField(mapper, g.ptrTy, int(g.ptrBits div 8), int(g.ptrBits div 8))

  g.fieldIndexFields(mapper, typ.n, ty, sym, tfUnion in typ.flags)

proc rootIndex(g: LLGen, typ: PType): seq[llvm.ValueRef] =
  let zero = g.gep0
  result = @[zero]
  var t = skipTypes(typ, abstractInst)
  while t.kind in {tyVar, tyPtr, tyRef, tyLent}:
    result = result & @[zero]
    t = skipTypes(t.lastSon, typedescInst)

  while t.kind == tyObject and t[0] != nil:
    result = result & @[zero]
    t = skipTypes(t[0], skipPtrs)

  if t.isObjLackingTypeField():
    # TODO why is this check in the generator???
    g.config.internalError("no 'of' operator available for pure objects")

proc excNameIndex(g: LLGen, typ: PType): seq[llvm.ValueRef] =
  g.rootIndex(typ)[0 ..^ 2] & g.constGEPIdx(2)

proc setElemIndex(g: LLGen, typ: PType, x: llvm.ValueRef): llvm.ValueRef =
  if g.config.firstOrd(typ) != 0:
    # TODO Int128
    g.b.buildSub(
      x,
      constInt(x.typeOfX(), g.config.firstOrd(typ).toInt64().culonglong, llvm.False),
      g.nn("set.ord", x),
    )
  else:
    x

proc preCast(
    g: LLGen, unsigned: bool, ax: llvm.ValueRef, t: PType, lt: llvm.TypeRef = nil
): llvm.ValueRef =
  if ax == nil: # tyTyped works like this...
    return constNull(g.llType(t))

  let
    at = ax.typeOfX()
    atk = at.getTypeKind()
    lt =
      if lt != nil:
        lt
      else:
        g.llType(t)
    ltk = lt.getTypeKind()

  if ltk == IntegerTypeKind and atk == IntegerTypeKind and
      at.getIntTypeWidth() != lt.getIntTypeWidth():
    return g.buildTruncOrExt(ax, lt, unsigned)

  if ltk in llvm.HalfTypeKind .. llvm.PPC_FP128TypeKind and
      atk in llvm.HalfTypeKind .. llvm.PPC_FP128TypeKind:
    if ltk > atk:
      return g.b.buildFPExt(ax, lt, g.nn("pre.fpe", ax))
    elif ltk < atk:
      return g.b.buildFPTrunc(ax, lt, g.nn("pre.fpt", ax))

  if ltk in llvm.HalfTypeKind .. llvm.PPC_FP128TypeKind and atk in [
    llvm.IntegerTypeKind
  ]:
    if unsigned:
      return g.b.buildSIToFP(ax, lt, g.nn("pre.sf", ax))
    else:
      return g.b.buildUIToFP(ax, lt, g.nn("pre.uf", ax))

  if ltk in [llvm.IntegerTypeKind] and atk in llvm.HalfTypeKind .. llvm.PPC_FP128TypeKind:
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
    name = g.llName(s)
    t = g.llType(s.typ)

  case name
  of "errno":
    LLValue(v: g.callErrno(""))
  of "h_errno":
    LLValue(v: g.callErrno("h_"))
  else:
    if s.id in g.symbols and not g.interactive():
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
    if s.kind == skLet:
      incl(s.loc.flags, lfNoDeepCopy)

  let
    t = g.llType(s.typ)
    v = g.localAlloca(t, g.llName(s))
  g.debugVariable(s, v)

  if s.kind in {skLet, skVar, skField, skForVar} and s.alignment > 0:
    v.setAlignment(cuint s.alignment)

  let lv = LLValue(v: v, lode: n, storage: s.loc.storage)
  g.symbols[s.id] = lv
  lv

proc genGlobal(g: LLGen, n: PNode, isConst: bool): LLValue =
  let s = n.sym

  if s.id in g.symbols:
    let
      name = g.llName(s)
      t = g.llType(s.typ.skipTypes(abstractInst))

    return g.refGlobal(g.symbols[s.id], name, t)

  if s.loc.k == locNone:
    fillLoc(s.loc, locGlobalVar, n, g.mangleName(s), if isConst: OnStatic else: OnHeap)

  let name = g.llName(s)

  # Couldn't find by id - should we get by name also? this seems to happen for
  # stderr for example which turns up with two different id:s.. what a shame!
  if (let v = g.m.getNamedGlobal(name); v != nil):
    let tmp = LLValue(v: v, storage: s.loc.storage)
    g.symbols[s.id] = tmp
    return tmp

  let
    t = g.llType(s.typ.skipTypes(abstractInst))
    v = g.m.addGlobal(t, name)

  if sfImportc in s.flags:
    v.setLinkage(llvm.ExternalLinkage)
  elif sfExportc in s.flags:
    v.setLinkage(g.tgtExportLinkage)
    v.setInitializer(llvm.constNull(t))
  else:
    v.setLinkage(g.defaultGlobalLinkage())
    v.setInitializer(llvm.constNull(t))

  if sfThread in s.flags and optThreads in g.config.globalOptions:
    v.setThreadLocal(llvm.True)
  g.debugGlobal(s, v)

  if isConst:
    v.setGlobalConstant(llvm.True)

  if s.kind in {skLet, skVar, skField, skForVar} and s.alignment > 0:
    v.setAlignment(cuint s.alignment)

  result = LLValue(v: v, lode: n, storage: s.loc.storage)
  g.symbols[s.id] = result

proc getUnreachableBlock(g: LLGen): llvm.BasicBlockRef =
  if g.f.unreachableBlock == nil:
    g.f.unreachableBlock = g.b.appendBasicBlockInContext(
      g.lc, g.nn("unreachable", g.b.getInsertBlock().getBasicBlockParent())
    )

    g.withBlock(g.f.unreachableBlock):
      discard g.b.buildUnreachable()

  g.f.unreachableBlock

proc getDeadBlock(g: LLGen): llvm.BasicBlockRef =
  # Sometimes, there might be dead code after a return statement or a noreturn
  # call such as an exception being raised. We use a block with no predecessors
  # to collect such code and let it be optimized away..

  g.f.deadBlocks.add(
    g.b.appendBasicBlockInContext(
      g.lc, g.nn("dead", g.b.getInsertBlock().getLastInstruction())
    )
  )

  g.f.deadBlocks[^1]

proc callCompilerProc(
    g: LLGen,
    name: string,
    args: openArray[llvm.ValueRef],
    noInvoke = false,
    noReturn = false,
): llvm.ValueRef =
  let sym = g.graph.getCompilerProc(name)
  if sym == nil:
    g.config.internalError("compiler proc not found: " & name)

  let
    f = g.genFunctionWithBody(sym).v
    fty = f.globalGetValueType()

  if noInvoke:
    let ret = g.b.buildCall2(
      fty,
      f,
      args,
      if fty.getReturnType().getTypeKind() == llvm.VoidTypeKind:
        ""
      else:
        g.nn("call.cp." & name),
    )
    if noReturn:
      discard g.b.buildUnreachable()
      g.b.positionBuilderAtEnd(g.getDeadBlock())
    ret
  elif noReturn:
    let ret = g.buildCallOrInvokeBr(
      fty, f, g.getUnreachableBlock(), args, g.nn("call.cp." & name)
    )
    g.b.positionBuilderAtEnd(g.getDeadBlock())
    ret
  else:
    g.buildCallOrInvoke(fty, f, args, g.nn("call.cp." & name))

proc callBSwap(g: LLGen, v: llvm.ValueRef, n: cuint): llvm.ValueRef =
  let
    it = llvm.intTypeInContext(g.lc, n)
    fty = llvm.functionType(it, [it])
    f = g.m.getOrInsertFunction("llvm.bswap.i" & $n, fty)

  g.b.buildCall2(fty, f, [v], g.nn("bswap", v))

proc callMemset(g: LLGen, tgt, v, len: llvm.ValueRef) =
  let v8 = g.buildTruncOrExt(v, g.int8Ty, true)
  if len.typeOfX().getIntTypeWidth() == 64:
    let
      fty = llvm.functionType(g.voidTy, [g.ptrTy, g.int8Ty, g.int64Ty, g.int1Ty])
      f = g.m.getOrInsertFunction("llvm.memset.p0.i64", fty)

    discard g.b.buildCall2(fty, f, [tgt, v8, len, g.constInt1(false)], "")
  else:
    let
      fty = llvm.functionType(g.voidTy, [g.ptrTy, g.int8Ty, g.int32Ty, g.int1Ty])
      f = g.m.getOrInsertFunction("llvm.memset.p0.i32", fty)
      len32 = g.b.buildZExt(len, g.int32Ty, "memset.l32")

    discard g.b.buildCall2(fty, f, [tgt, v8, len32, g.constInt1(false)], "")

proc callMemcpy(g: LLGen, tgt, src, len: llvm.ValueRef) =
  if len.typeOfX().getIntTypeWidth() == 64:
    let
      fty = llvm.functionType(g.voidTy, [g.ptrTy, g.ptrTy, g.int64Ty, g.int1Ty])
      f = g.m.getOrInsertFunction("llvm.memcpy.p0.p0.i64", fty)

    discard g.b.buildCall2(fty, f, [tgt, src, len, g.constInt1(false)], "")
  else:
    let
      fty = llvm.functionType(g.voidTy, [g.ptrTy, g.ptrTy, g.int32Ty, g.int1Ty])
      f = g.m.getOrInsertFunction("llvm.memcpy.p0.p0.i32", fty)
      len32 = g.b.buildZExt(len, g.int32Ty, "memcpy.l32")

    discard g.b.buildCall2(fty, f, [tgt, src, len32, g.constInt1(false)], "")

proc callCtpop(g: LLGen, v: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let
    bits = (size * 8).cuint
    fty = llvm.functionType(
      llvm.intTypeInContext(g.lc, bits), [llvm.intTypeInContext(g.lc, bits)]
    )
    f = g.m.getOrInsertFunction("llvm.ctpop.i" & $bits, fty)

  g.b.buildCall2(fty, f, [v], g.nn("ctpop", v))

proc callErrno(g: LLGen, prefix: string): llvm.ValueRef =
  # on linux errno is a function, so we call it here. not at all portable.

  let
    fty = llvm.functionType(g.ptrTy, [])
    f = g.m.getOrInsertFunction("__" & prefix & "errno_location", fty)

  g.b.buildCall2(fty, f, [], g.nn(prefix & "errno"))

proc callWithOverflow(
    g: LLGen, op: string, a, b: llvm.ValueRef, name: string
): llvm.ValueRef =
  let
    t = a.typeOfX()
    fty = llvm.functionType(llvm.structTypeInContext(g.lc, [t, g.int1Ty]), [t, t])
    f = g.m.getOrInsertFunction(
      "llvm." & op & ".with.overflow.i" & $t.getIntTypeWidth(), fty
    )

  g.b.buildCall2(fty, f, [a, b], name)

proc callRaise(
    g: LLGen, cond: llvm.ValueRef, raiser: string, args: varargs[llvm.ValueRef]
) =
  let
    raised = g.b.appendBasicBlockInContext(g.lc, g.nn("raise.call", cond))
    cont = g.b.appendBasicBlockInContext(g.lc, g.nn("raise.cont", cond))

  discard g.b.buildCondBr(cond, raised, cont)

  g.b.positionBuilderAtEnd(raised)
  discard g.callCompilerProc(raiser, args)
  discard g.b.buildUnreachable()
  g.b.positionBuilderAtEnd(cont)

proc callBinOpWithOver(
    g: LLGen, a, b: llvm.ValueRef, op: Opcode, typ: PType
): llvm.ValueRef =
  # like tyChar, for example
  let u = g.isUnsigned(typ.skipTypes(abstractVar))

  proc doRangeCheck(v: llvm.ValueRef, typ: PType) =
    # TODO Int128
    let lt = g.b.buildICmp(
      llvm.IntSLT,
      v,
      constInt(v.typeOfX(), g.config.firstOrd(typ).toInt64.culonglong, llvm.False),
      g.nn("binop.over.rng.lt", a),
    )
    let gt = g.b.buildICmp(
      llvm.IntSGT,
      v,
      constInt(v.typeOfX(), g.config.lastOrd(typ).toInt64.culonglong, llvm.False),
      g.nn("binop.over.rng.gt", a),
    )

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
      else:
        g.config.internalError("bad overflow op")
        ""

    let
      bo = g.callWithOverflow(opfn, a, b, g.nn("binop.over." & $op, a))
      isover = g.b.buildExtractValue(bo, 1, g.nn("binop.isover", a))

    g.callRaise(isover, "raiseOverflow")

    result = g.b.buildExtractValue(bo, 0, g.nn("binop.v", a))

    let
      rangeTyp = typ.skipTypes({tyGenericInst, tyAlias, tySink, tyVar, tyLent})
      rangeCheck = rangeTyp.kind in {tyRange, tyEnum}
    if rangeCheck:
      doRangeCheck(result, rangeTyp)
  else:
    let
      typ = typ.skipTypes(abstractVar)
      i64 = typ.kind == tyInt64
      ax =
        if i64:
          g.buildTruncOrExt(a, g.primitives[tyInt64], u)
        else:
          g.buildNimIntExt(a, u)
      bx =
        if i64:
          g.buildTruncOrExt(b, g.primitives[tyInt64], u)
        else:
          g.buildNimIntExt(b, u)
      divzero = g.b.buildICmp(
        llvm.IntEQ, bx, constInt(ax.typeOfX(), 0, llvm.False), "rc.divzero"
      )

    g.callRaise(divzero, "raiseDivByZero")

    let bo =
      if op == llvm.SDiv:
        let
          opfn = if i64: "nimDivInt64" else: "nimDivInt"
          res = g.localAlloca(ax.typeOfX(), g.nn("res", a))
          isover = g.buildI1(g.callCompilerProc(opfn, [ax, bx, res]))

        g.callRaise(isover, "raiseOverflow")
        g.b.buildLoad2(res.getAllocatedType(), res)
      else:
        g.b.buildBinOp(op, ax, bx, "rc.bin")

    let rangeCheck =
      typ.kind in
      {tyRange, tyEnum, tyInt8, tyInt16, tyInt32, tyUInt8, tyUInt16, tyUint32}
    if rangeCheck:
      doRangeCheck(bo, typ)

    result = g.b.buildTrunc(bo, g.llType(typ), g.nn("binop.call.trunc", a))

proc callExpect(g: LLGen, v: llvm.ValueRef, expected: bool): llvm.ValueRef =
  let
    fty = llvm.functionType(
      g.primitives[tyBool], [g.primitives[tyBool], g.primitives[tyBool]]
    )
    f = g.m.getOrInsertFunction("llvm.expect.i8", fty)

  g.b.buildCall2(fty, f, [v, g.constInt8(int8(ord expected))], g.nn("expect", v))

proc callEhTypeIdFor(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  let
    fty = llvm.functionType(g.int32Ty, [g.ptrTy])
    f = g.m.getOrInsertFunction("llvm.eh.typeid.for", fty)

  g.b.buildCall2(fty, f, [v], g.nn("eh.typeid", v))

proc callIsObj(g: LLGen, v: llvm.ValueRef, typ: PType): llvm.ValueRef =
  let
    # Type resides at the start of the object
    mtype = g.b.buildLoad2(g.ptrTy, v)
    cmpTo =
      if optTinyRtti in g.config.globalOptions:
        g.constCStringPtr(g.genTypeInfo2Name(typ))
      else:
        g.genTypeInfo(typ)

  g.callCompilerProc("isObj", [mtype, cmpTo])

# These are taken from cgen and take care of some of the fallout from the
# beautiful copy-on-write string literal pessimisation :/
proc cowBracket(g: LLGen, n: PNode) =
  if n.kind == nkBracketExpr and optSeqDestructors in g.config.globalOptions:
    let strCandidate = n[0]
    if strCandidate.typ.skipTypes(abstractInst).kind == tyString:
      let ax = g.genNode(strCandidate, false).v
      discard g.callCompilerProc("nimPrepareStrMutationV2", [ax])

proc cow(g: LLGen, n: PNode) =
  if n.kind == nkHiddenAddr:
    cowBracket(g, n[0])

proc typeSuffix(t: llvm.TypeRef): string =
  case t.getTypeKind
  of HalfTypeKind:
    "f16"
  of FloatTypeKind:
    "f32"
  of DoubleTypeKind:
    "f64"
  of X86FP80TypeKind:
    "f80"
  of FP128TypeKind:
    "f128"
  of PPC_FP128TypeKind:
    "ppcf128"
  of IntegerTypeKind:
    "i" & $llvm.getIntTypeWidth(t)
  else:
    raiseAssert "unexpected type suffix " & $t

proc callFabs(g: LLGen, v: llvm.ValueRef): llvm.ValueRef =
  let
    t = v.typeOfX()
    fty = llvm.functionType(t, [t])
    f = g.m.getOrInsertFunction("llvm.fabs." & typeSuffix(t), fty)

  g.b.buildCall2(fty, f, [v], g.nn("fabs", v))

proc callCopysign(g: LLGen, a, b: llvm.ValueRef): llvm.ValueRef =
  let
    t = a.typeOfX()
    fty = llvm.functionType(t, [t])
    f = g.m.getOrInsertFunction("llvm.copysign." & typeSuffix(t), fty)

  g.b.buildCall2(fty, f, [a, b], g.nn("copysign", a))

proc genObjectInit(g: LLGen, typ: PType, v: llvm.ValueRef, setType: bool = true)

proc genObjectInitFields(
    g: LLGen, mapper: var FieldMapper, n: PNode, ty: llvm.TypeRef, v: llvm.ValueRef
) =
  if n == nil:
    return
  case n.kind
  of nkRecList:
    for child in n:
      g.genObjectInitFields(mapper, child, ty, v)
  of nkRecCase:
    withRecCase(n, ty, v):
      g.genObjectInitFields(recMapper, branch.lastSon, branchTy, storeGEP)
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return

    let element = g.addField(mapper, g.llType(field.typ), field)
    if analyseObjectWithTypeField(field.typ) != frNone:
      let gep = g.b.buildStructGEP2(ty, v, cuint element, g.nn("oi", field))
      g.genObjectInit(field.typ, gep)
  else:
    g.config.internalError(n.info, "Unexpected kind in genObjectInitFields: " & $n.kind)

proc genObjectInit(g: LLGen, typ: PType, v: llvm.ValueRef, setType: bool) =
  ## Set the mtype field of the object to its static type - ie when traversing
  ## a type hierarchy of an object, we set it for the most derived type only -
  ## then we repeat the process for object and array fields
  let
    xtyp = typ.skipTypes(irrelevantForBackend)
    ty = g.llType(typ)

  case xtyp.kind
  of tyArray:
    let elemTyp = xtyp.elemType.skipTypes(irrelevantForBackend)
    if analyseObjectWithTypeField(elemTyp) != frNone:
      let arraySize = g.constNimInt(lengthOrd(g.config, xtyp[0]))
      g.withLoop(arraySize, "oi.arr"):
        let gep = g.b.buildInboundsGEP2(ty, v, [g.gep0, i], g.nn("oi.arr.gep", v))
        g.genObjectInit(xtyp.elemType, gep)
  of tyObject:
    if setType and not isObjLackingTypeField(xtyp):
      let ti = g.genTypeInfo(typ)
      # `mtype` field always at beginning of object
      discard g.b.buildStore(ti, v)

    var mapper = FieldMapper(packed: tfPacked in xtyp.flags)
    if xtyp[0] != nil:
      let supTyp = xtyp[0].skipTypes(skipPtrs)
      discard g.addField(mapper, g.llType(supTyp), supTyp)
      g.genObjectInit(supTyp, v, false)
    elif xtyp.hasMTypeField(): # Skip m_type in inheritable root object
      discard g.addField(mapper, g.ptrTy, int(g.ptrBits div 8), int(g.ptrBits div 8))

    if analyseObjectWithTypeField(xtyp) == frEmbedded:
      g.genObjectInitFields(mapper, xtyp.n, ty, v)

    if isException(xtyp):
      let
        tgt = g.b.buildInboundsGEP2(ty, v, g.excNameIndex(typ), g.nn("name", v))
        ename = typ.skipTypes(abstractInst).sym.name.s
        pname = g.constCStringPtr(ename)

      discard g.b.buildStore(pname, tgt)
  of tyTuple:
    for i in 0 ..< xtyp.len:
      if analyseObjectWithTypeField(xtyp[i]) != frNone:
        let gep = g.b.buildStructGEP2(ty, v, cuint i, g.nn("oi.field" & $i, v))
        g.genObjectInit(xtyp[i], gep)
  else:
    discard

proc supportsMemset(typ: PType): bool =
  supportsCopyMem(typ) and analyseObjectWithTypeField(typ) == frNone

proc initLocalVar(
    g: LLGen, s: PSym, t: PType, v: llvm.ValueRef, immediateAssign: bool
) =
  if sfNoInit notin s.flags:
    # When the type does not support copyMem assignment will be done with
    # the assumption that the "inital" value is 0 (for example via asgnRef)
    # This is a conservative estimate - in some cases it leads to dead zero
    # stores (TODO: figure it out)
    if not immediateAssign or not supportsMemset(t):
      g.buildStoreNull(g.lltype(t), v)
      g.genObjectInit(t, v)

proc isAssignedImmediately(g: LLGen, n: PNode): bool {.inline.} =
  if n.kind == nkEmpty:
    false
  # TODO RVO and return-by-pointer
  else:
    true

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
  let il = g.b.buildLoad2(i.getAllocatedType(), i)
  let cond = g.b.buildICmp(llvm.IntSLE, il, b, g.nn("rng.sle", n))
  discard g.b.buildCondBr(cond, rloop, rdone)

  # loop body
  g.b.positionBuilderAtEnd(rloop)

  body

  # inc idx
  let next = g.b.buildAdd(il, constInt(il.typeOfX(), 1, llvm.False), g.nn("rng.inc", n))
  discard g.b.buildStore(next, i)
  # back to comparison
  discard g.b.buildBr(rcmp)

  # continue at the end
  g.b.positionAndMoveToEnd(rdone)

proc genFakeImpl(g: LLGen, s: PSym, f: llvm.ValueRef): bool =
  # sometimes the implementation in the nim std library doesn't work for llvm
  # but really needs to be there.. candidate for upstreaming...
  if s.name.s == "rawProc" and s.typ.sons.len == 2 and s.typ.sons[1].kind == tyProc:
    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let
        gep = g.buildClosurePrcGEP(f.getParam(0))
        p = g.b.buildLoad2(g.ptrTy, gep)
      discard g.b.buildRet(p)
    return true

  if s.name.s == "rawEnv" and s.typ.sons.len == 2 and s.typ.sons[1].kind == tyProc:
    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let
        gep = g.buildClosureEnvGEP(f.getParam(0))
        p = g.b.buildLoad2(g.ptrTy, gep)
      discard g.b.buildRet(p)
    return true

  if s.name.s == "finished" and s.typ.sons.len == 2 and s.typ.sons[1].kind == tyProc:
    # TODO clean this up now that mFinished is a magic
    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let
        gep = g.buildClosureEnvGEP(f.getParam(0))
        env = g.b.buildLoad2(g.ptrTy, gep)
        stateGep = g.b.buildInboundsGEP2(g.intTy, env, [g.gep1], g.nn("state", env))
        state = g.b.buildLoad2(g.intTy, stateGep)
        cmp =
          g.buildI8(g.b.buildICmp(llvm.IntSLT, state, g.ni0, g.nn("finished.cmp", s)))
      discard g.b.buildRet(cmp)
    return true

  if (s.name.s in ["addInt", "subInt", "mulInt", "addInt64", "subInt64", "mulInt64"]) and
      s.typ.sons.len == 3 and s.typ[0] != nil and
      s.typ.sons[0].kind == s.typ.sons[1].kind and
      s.typ.sons[1].kind == s.typ.sons[2].kind:
    # prefer intrinsic for these...
    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      let
        a = f.getParam(0)
        b = f.getParam(1)
        res =
          case s.name.s
          of "addInt", "addInt64":
            g.callBinOpWithOver(a, b, llvm.Add, s.typ.sons[0])
          of "subInt", "subInt64":
            g.callBinOpWithOver(a, b, llvm.Sub, s.typ.sons[0])
          of "mulInt", "mulInt64":
            g.callBinOpWithOver(a, b, llvm.Mul, s.typ.sons[0])
          else:
            g.config.internalError("checked above!")
            nil
      discard g.b.buildRet(res)
    return true

  if s.name.s == "cpuRelax":
    g.withBlock(appendBasicBlockInContext(g.lc, f, g.nn("entry.fake", s))):
      discard g.b.buildRetVoid()
    return true

proc callReset(g: LLGen, typ: PType, v: LLValue)

proc genResetFields(
    g: LLGen, mapper: var FieldMapper, n: PNode, ty: llvm.TypeRef, v: LLValue
) =
  if n == nil:
    return
  case n.kind
  of nkRecList:
    for child in n:
      g.genResetFields(mapper, child, ty, v)
  of nkRecCase:
    withRecCase(n, ty, v.v):
      g.genResetFields(recMapper, branch.lastSon, branchTy, LLValue(v: storeGEP))

    g.buildStoreNull(tagTy, tagGEP)
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return

    g.debugUpdateLoc(field)

    let
      element = g.addField(mapper, g.llType(field.typ), field)
      gep = g.b.buildStructGEP2(ty, v, cuint element, g.nn("reset", field))
    g.callReset(field.typ, gep)
  else:
    g.config.internalError("Unexpected kind in genResetFields: " & $n.kind)

proc genReset(g: LLGen, typ: PType, v: LLValue) =
  let
    typ = typ.skipTypes(abstractInst)
    ty = g.llType(typ)

  if supportsMemset(typ):
    g.buildStoreNull(ty, v.v)
  else:
    case typ.kind
    of tyArray:
      let
        et = typ.elemType
        arraySize = g.constNimInt(lengthOrd(g.config, typ[0]))
      g.withLoop(arraySize, "reset.arr"):
        let gep = g.b.buildInboundsGEP2(ty, v.v, [g.gep0, i], g.nn("reset.gep", v))
        g.callReset(et, LLValue(v: gep))
    of tyObject:
      var mapper = FieldMapper(packed: tfPacked in typ.flags)

      if typ[0] != nil:
        let supTyp = typ[0].skipTypes(skipPtrs)
        discard g.addField(mapper, g.llType(supTyp), supTyp)
        g.callReset(supTyp, v)
      elif typ.hasMTypeField(): # Skip m_type in inheritable root object
        discard g.addField(mapper, g.ptrTy, int(g.ptrBits div 8), int(g.ptrBits div 8))

      g.genResetFields(mapper, typ.n, ty, v)
    of tyTuple:
      for i in 0 ..< typ.len:
        let gep = g.b.buildStructGEP2(ty, v, cuint i, g.nn("reset.field" & $i, v))
        g.callReset(typ[i], gep)
    of tyString, tyRef, tySequence:
      g.genRefAssign(v, constNull(ty))
    of tyProc:
      if typ.callConv == ccClosure:
        let p = g.buildClosurePrcGEP(v.v)
        g.buildStoreNull(g.ptrTy, p)

        let e = g.buildClosureEnvGEP(v.v)
        g.genRefAssign(LLValue(v: e), constNull(g.ptrTy))
      else:
        g.buildStoreNull(ty, v.v)
    else:
      g.config.internalError("Unexpected kind in genReset: " & $typ.kind)

proc genResetFunc(g: LLGen, typ: PType): llvm.ValueRef =
  let
    typ = skipTypes(typ, abstractInst)
    sig = hashType(typ, g.config)
    name = ".reset." & g.llname(typ, sig)
    ft = llvm.functionType(g.voidTy, [g.ptrTy], false) # g.llTyp(typ).pointerType

  g.resets.withValue(sig, reset):
    return g.refFunction(reset[], name, ft)

  let f = g.addNimFunction(name, ft)
  g.resets[sig] = f

  f.getParam(0).setValueName("v")

  f.setLinkage(g.defaultFunctionLinkage())

  let llf = g.newLLFunc(f, nil)

  if g.d != nil:
    let dt = g.debugType(typ)

    llf.ds = g.debugFunction(nil, [nil, dt], f)

  g.withFunc(llf):
    g.debugUpdateLoc(typ.sym)

    g.withBlock(g.section(g.f, secBody)):
      g.genReset(typ, LLValue(v: f.getParam(0)))
      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      discard g.b.buildRetVoid()

    g.finalize()

  return f

proc callReset(g: LLGen, typ: PType, v: LLValue) =
  ## Take a valid object and reset it to the default-initialized state - in
  ## particular, `mtype` must be set prior to calling this function and it is
  ## assumed that all data is in a valid pre-state (ie pointers are null or
  ## dereferencable)
  let
    typ = skipTypes(typ, abstractInst)
    ty = g.llType(typ)

  if supportsMemset(typ): # Fast path
    g.buildStoreNull(ty, v.v)
  elif typ.kind in {tyString, tyRef, tySequence}:
    g.genRefAssign(v, constNull(ty))
  else:
    let
      f = g.genResetFunc(typ)
      fty = f.globalGetValueType()
    discard g.b.buildCall2(fty, f, [v.v], "")

proc callAssign(
  g: LLGen, typ: PType, dest, src, shallow: llvm.ValueRef, checkType = true
)

proc genAssignFields(
    g: LLGen,
    mapper: var FieldMapper,
    n: PNode,
    ty: llvm.TypeRef,
    dest, src, shallow: llvm.ValueRef,
) =
  if n == nil:
    return
  case n.kind
  of nkRecList:
    for child in n:
      g.genAssignFields(mapper, child, ty, dest, src, shallow)
  of nkRecCase:
    block: # Reset kind-specific fields of the destination
      # TODO: we could skip this if tag is unchanged
      var mapper = mapper
      withRecCase(n, ty, dest):
        g.genResetFields(recMapper, branch.lastSon, branchTy, LLValue(v: storeGEP))

    block: # Set field discriminator and copy fields
      withRecCase(n, ty, src):
        let destTagGEP =
          g.b.buildStructGEP2(ty, dest, cuint tagElement, g.nn("asgn.tagd", dest))
        discard g.b.buildStore(tag, destTagGEP)

        let destGEP = g.b.buildStructGEP2(
          ty, dest, cuint storeElement, g.nn("vari.dest.branch", dest)
        )

        g.genAssignFields(
          recMapper, branch.lastSon, branchTy, destGEP, storeGEP, shallow
        )
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return

    g.debugUpdateLoc(field)

    let
      element = g.addField(mapper, g.llType(field.typ), field)
      gepd = g.b.buildStructGEP2(ty, dest, cuint element, g.nn("asgn.d", field))
      geps = g.b.buildStructGEP2(ty, src, cuint element, g.nn("asgn.s", field))
    g.callAssign(field.typ, gepd, geps, shallow)
  else:
    g.config.internalError("Unexpected kind in genResetFields: " & $n.kind)

proc loadNimSeqShallow(g: LLGen, srcl, shallow: llvm.ValueRef): llvm.ValueRef =
  # Return true iff srcl is nil or shallow
  const seqShallowFlag = int.low

  let
    pre = g.b.getInsertBlock()
    lnotnil = g.b.appendBasicBlockInContext(g.lc, g.nn("lnss.notnil", srcl))
    ldone = g.b.appendBasicBlockInContext(g.lc, g.nn("lnss.done", srcl))
    cond = g.b.buildICmp(
      llvm.IntEQ, srcl, llvm.constNull(srcl.typeOfX()), g.nn("lnss.isnil", srcl)
    )

  discard g.b.buildCondBr(cond, ldone, lnotnil)

  g.b.positionBuilderAtEnd(lnotnil)

  let
    lnotshallow = g.b.appendBasicBlockInContext(g.lc, g.nn("lnss.notshallow", srcl))
    isShallow = g.b.buildICmp(
      llvm.IntUGT,
      shallow,
      llvm.constNull(shallow.typeOfX()),
      g.nn("lnss.isshallow", shallow),
    )
  discard g.b.buildCondBr(isShallow, ldone, lnotshallow)

  g.b.positionBuilderAtEnd(lnotshallow)
  let
    cap = g.buildNimSeqCapGEP(srcl)
    capl = g.b.buildLoad2(g.intTy, cap)
    shallowCap =
      g.b.buildAnd(capl, g.constNimInt(seqShallowFlag), g.nn("lnss.flag", capl))
    shallowCapBool = g.b.buildICmp(
      llvm.IntEQ,
      shallowCap,
      g.constNimInt(seqShallowFlag),
      g.nn("lnss.cond", shallowCap),
    )

  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # body or default
  let phi = g.b.buildPHI(g.int1Ty, g.nn("lnss.phi", srcl))
  phi.addIncoming([cond, isShallow, shallowCapBool], [pre, lnotnil, lnotshallow])

  phi

proc genAssign(g: LLGen, typ: PType, dest, src, shallow: llvm.ValueRef) =
  assert optSeqDestructors notin g.config.globalOptions
  # non-generic version of `assign.genericAssign`
  let
    typ = typ.skipTypes(abstractInst)
    ty = g.llType(typ)

  if supportsCopyMem(typ):
    let size = g.constStoreSize(g.llType(typ))
    g.callMemcpy(dest, src, size)
  else:
    case typ.kind
    of tyArray:
      let
        et = typ.elemType
        arraySize = g.constNimInt(lengthOrd(g.config, typ[0]))
      g.withLoop(arraySize, "asgn.arr"):
        let
          gepd = g.b.buildInboundsGEP2(ty, dest, [g.gep0, i], g.nn("asgn.d", dest))
          geps = g.b.buildInboundsGEP2(ty, src, [g.gep0, i], g.nn("asgn.s", src))
        g.callAssign(et, gepd, geps, shallow, false)
    of tyObject:
      var mapper = FieldMapper(packed: tfPacked in typ.flags)

      if typ[0] != nil: # Skip super type field
        let supTyp = typ[0].skipTypes(skipPtrs)
        discard g.addField(mapper, g.llType(supTyp), supTyp)
        g.callAssign(supTyp, dest, src, shallow, false)
      elif typ.hasMTypeField: # Skip m_type in inheritable root object
        discard g.addField(mapper, g.ptrTy, int(g.ptrBits div 8), int(g.ptrBits div 8))

      g.genAssignFields(mapper, typ.n, ty, dest, src, shallow)
    of tyTuple:
      for i in 0 ..< typ.len:
        let
          gepd = g.b.buildStructGEP2(ty, dest, cuint i, g.nn("asgn.field" & $i, dest))
          geps = g.b.buildStructGEP2(ty, src, cuint i, g.nn("asgn.field" & $i, src))
        g.callAssign(typ[i], gepd, geps, shallow)
    of tyString:
      let srcl = g.b.buildLoad2(ty, src)

      if optSeqDestructors in g.config.globalOptions:
        discard g.b.buildStore(srcl, dest)
      else:
        let
          srcl = g.b.buildLoad2(g.ptrTy, src)
          pre = g.b.getInsertBlock()
          lcopy = g.b.appendBasicBlockInContext(g.lc, g.nn("asgn.copy", srcl))
          lasgn = g.b.appendBasicBlockInContext(g.lc, g.nn("asgn.asgn", srcl))

        discard g.b.buildCondBr(shallow, lasgn, lcopy)

        g.b.positionBuilderAtEnd(lcopy)

        let
          # copyString checks nil and flags in string
          v = g.callCompilerProc("copyString", [srcl])
        discard g.b.buildBr(lasgn)

        g.b.positionBuilderAtEnd(lasgn)

        # body or default
        let phi = g.b.buildPHI(v.typeOfX(), g.nn("nilcheck.phi", v))
        phi.addIncoming([srcl, v], [pre, lcopy])

        discard g.callCompilerProc("unsureAsgnRef", [dest, phi])
    of tyRef:
      let srcl = g.b.buildLoad2(g.ptrTy, src)
      discard g.callCompilerProc("unsureAsgnRef", [dest, srcl])
    of tySequence:
      let
        srcl = g.b.buildLoad2(g.ptrTy, src)
        shallowCond = g.loadNimSeqShallow(srcl, shallow)
        pre = g.b.getInsertBlock()
        lcopy = g.b.appendBasicBlockInContext(g.lc, g.nn("asgn.copy", srcl))
        lasgn = g.b.appendBasicBlockInContext(g.lc, g.nn("asgn.asgn", srcl))

      discard g.b.buildCondBr(shallowCond, lasgn, lcopy)

      g.b.positionBuilderAtEnd(lcopy)
      let
        elemTyp = typ.elemType()
        seqTy = g.llSeqType(typ)
        seqlen = g.loadNimSeqLen(srcl)

        srcphi = g.withNotNilOrNull(seqlen, g.ptrTy):
          if supportsCopyMem(elemTyp):
            let srcc =
              g.callCompilerProc("nimNewSeqOfCap", [g.genTypeInfo(typ), seqlen])
            discard g.b.buildStore(seqlen, srcc)

            let
              srcp = g.buildNimSeqDataGEP(seqTy, srcl)
              destp = g.buildNimSeqDataGEP(seqTy, srcc)
              dl = g.m.getModuleDataLayout()
              elemSize = g.constNimInt(int dl.storeSizeOfType(g.llType(elemTyp)))
              byteLen = g.b.buildMul(seqlen, elemSize, g.nn("asgn.bytes", src))
            g.callMemcpy(destp, srcp, byteLen)
            srcc
          else:
            let srcc = g.callCompilerProc("newSeq", [g.genTypeInfo(typ), seqlen])
            if elemTyp.kind != tyEmpty:
              g.withLoop(seqlen, "asgn.seq"):
                let
                  geps = g.buildNimSeqDataGEP(seqTy, srcl, i)
                  gepd = g.buildNimSeqDataGEP(seqTy, srcc, i)
                g.callAssign(elemTyp, gepd, geps, shallow, false)
            srcc
        postasgn = g.b.getInsertBlock()
      discard g.b.buildBr(lasgn)

      g.b.positionBuilderAtEnd(lasgn)
      let phi = g.b.buildPhi(g.ptrTy, g.nn("asgn.phi", srcl))
      phi.addIncoming([srcl, srcphi], [pre, postasgn])
      discard g.callCompilerProc("unsureAsgnRef", [dest, phi])
    of tyProc:
      if typ.callConv == ccClosure:
        block:
          let
            srcp = g.buildClosurePrcGEP(src)
            srcl = g.b.buildLoad2(g.ptrTy, srcp)
            destp = g.buildClosurePrcGEP(dest)

          discard g.b.buildStore(srcl, destp)
        block:
          let
            srce = g.buildClosureEnvGEP(src)
            srcl = g.b.buildLoad2(g.ptrTy, srce)
            destp = g.buildClosureEnvGEP(dest)
          discard g.callCompilerProc("unsureAsgnRef", [destp, srcl])
      else:
        let srcl = g.b.buildLoad2(g.ptrTy, src)
        discard g.b.buildStore(srcl, dest)
    else:
      g.config.internalError("Unexpected kind in genAssign: " & $typ.kind)

proc genAssignFunc(g: LLGen, typ: PType): llvm.ValueRef =
  ## Generate an assignment function for non-trivial types - takes the form:
  ## define internal void @.asgn.Xxx(ptr %dest, ptr %src, i8 %shallow) where
  ## the arguments are the same as for `assign.genericAssign`
  let
    typ = skipTypes(typ, abstractInst)
    sig = hashType(typ, g.config)
  g.assigns.withValue(sig, asgn):
    return asgn[]

  let
    name = ".asgn." & g.llname(typ, sig)
    ft = llvm.functionType(g.voidTy, [g.ptrTy, g.ptrTy, g.int1Ty], false)
      # g.llTyp(typ).pointerType

  let f = g.m.addFunction(name, ft)
  g.assigns[sig] = f

  f.getParam(0).setValueName("dest")
  f.getParam(1).setValueName("src")
  f.getParam(2).setValueName("shallow")

  f.setLinkage(g.defaultFunctionLinkage())
  nimSetFunctionAttributes(f)

  let llf = g.newLLFunc(f, nil)

  if g.d != nil:
    let
      dt = g.d.dIBuilderCreatePointerType(g.debugType(typ), g.ptrBits, g.ptrBits, "")
      db = g.dtypes[tyBool] # TODO do we need a `bit` debug type?

    llf.ds = g.debugFunction(nil, [nil, dt, dt, db], f)

  g.withFunc(llf):
    g.debugUpdateLoc(typ.sym)

    g.withBlock(g.section(g.f, secBody)):
      g.genAssign(typ, f.getParam(0), f.getParam(1), f.getParam(2))
      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      discard g.b.buildRetVoid()

    g.finalize()

  return f

proc callAssign(
    g: LLGen, typ: PType, dest, src, shallow: llvm.ValueRef, checkType = true
) =
  let typ = skipTypes(typ, abstractInst)

  if checkType and typ.kind == tyObject and not isObjLackingTypeField(typ):
    let mtype = g.b.buildLoad2(g.ptrTy, src)
    g.withNotNil(mtype):
      discard g.callCompilerProc("chckObjAsgn", [mtype, g.genTypeInfo(typ)])

  if supportsCopyMem(typ): # Fast path
    let size = g.constStoreSize(g.llType(typ))
    g.callMemcpy(dest, src, size)
  elif typ.kind == tyRef:
    let srcl = g.b.buildLoad2(g.ptrTy, src)
    discard g.callCompilerProc("unsureAsgnRef", [dest, srcl])
  else:
    let
      f = g.genAssignFunc(typ)
      fty = f.globalGetValueType()
    discard g.b.buildCall2(fty, f, [dest, src, shallow], "")

proc addNimFunction(g: LLGen, sym: PSym): llvm.ValueRef =
  ## Add a function prototype for a nim function to the given module
  let
    name = g.llName(sym)
    typ = sym.typ.skipTypes(abstractInst)
    ty = g.llProcType(typ, typ.callConv == ccClosure)

    f = g.addNimFunction(name, ty)

  if sfNoReturn in sym.flags:
    f.addFuncAttribute(g.attrNoReturn)

  if typ.callConv == ccNoInline:
    f.addFuncAttribute(g.attrNoInline)

  # This attribute hopefully works around
  # https://github.com/nim-lang/Nim/issues/10625
  f.addFuncAttribute(g.attrNoOmitFP)

  if sym.name.s in [
    "sysFatal", "raiseOverflow", "raiseDivByZero", "raiseFloatInvalidOp",
    "raiseFloatOverflow", "raiseAssert", "raiseRangeErrorNoArgs", "raiseRangeErrorU",
    "raiseRangeErrorF", "raiseRangeErrorI", "raiseIndexError", "raiseIndexError2",
    "raiseIndexError3", "raiseFieldError", "nlvmRaise", "nlvmReraise",
  ]:
    f.addFuncAttribute(g.attrCold)

  if (sym.originatingModule.name.s, sym.name.s) in
      [("system", "quit"), ("ansi_c", "c_abort")]:
    f.addFuncAttribute(g.attrNoUnwind)

  if sym.originatingModule.name.s == "system" and g.config.selectedGC notin {gcRegions}:
    if sym.name.s in ["allocImpl", "allocSharedImpl"]:
      f.addFuncAttribute(g.lc.createStringAttribute("alloc-family", "nimgc"))
      f.addFuncAttribute(
        g.lc.createEnumAttribute(
          attrAllockind, AllocFnKindAlloc or AllocFnKindUninitialized
        )
      )
      f.addFuncAttribute(g.lc.createEnumAttribute(attrAllocsize, cast[uint32](-1)))
      f.addAttributeAtIndex(
        AttributeIndex(AttributeReturnIndex), g.lc.createEnumAttribute(attrAlign, 16)
      )
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNonnull)
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNoalias)
    elif sym.name.s in ["alloc0Impl", "allocShared0Impl"]:
      f.addFuncAttribute(g.lc.createStringAttribute("alloc-family", "nimgc"))
      f.addFuncAttribute(
        g.lc.createEnumAttribute(attrAllockind, AllocFnKindAlloc or AllocFnKindZeroed)
      )
      f.addFuncAttribute(g.lc.createEnumAttribute(attrAllocsize, cast[uint32](-1)))
      f.addAttributeAtIndex(
        AttributeIndex(AttributeReturnIndex), g.lc.createEnumAttribute(attrAlign, 16)
      )
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNonnull)
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNoalias)
    elif sym.name.s in ["alloc0", "newObj", "newObjRC1", "rawAlloc0", "allocShared0"]:
      f.addFuncAttribute(g.lc.createStringAttribute("alloc-family", "nimgc"))
      f.addFuncAttribute(
        g.lc.createEnumAttribute(attrAllockind, AllocFnKindAlloc or AllocFnKindZeroed)
      )
      f.addFuncAttribute(
        g.lc.createEnumAttribute(attrAllocsize, uint64(1 shl 32) + cast[uint32](-1))
      )
      f.addAttributeAtIndex(
        AttributeIndex(AttributeReturnIndex), g.lc.createEnumAttribute(attrAlign, 16)
      )
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNonnull)
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNoalias)
    elif sym.name.s in
        ["alloc", "rawAlloc", "newObjNoInit", "rawNewObj", "rawAlloc", "allocShared"]:
      f.addFuncAttribute(g.lc.createStringAttribute("alloc-family", "nimgc"))
      f.addFuncAttribute(
        g.lc.createEnumAttribute(
          attrAllockind, AllocFnKindAlloc or AllocFnKindUninitialized
        )
      )
      f.addFuncAttribute(
        g.lc.createEnumAttribute(attrAllocsize, uint64(1 shl 32) + cast[uint32](-1))
      )
      f.addAttributeAtIndex(
        AttributeIndex(AttributeReturnIndex), g.lc.createEnumAttribute(attrAlign, 16)
      )
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNonnull)
      f.addAttributeAtIndex(AttributeIndex(AttributeReturnIndex), g.attrNoalias)

  f

proc genFunction(g: LLGen, s: PSym): LLValue =
  if s.id in g.symbols:
    let
      name = g.llName(s)
      typ = s.typ.skipTypes(abstractInst)
      ty = g.llProcType(typ, typ.callConv == ccClosure)
    return g.refFunction(g.symbols[s.id], name, ty)

  fillLoc(s.loc, locProc, s.ast[namePos], g.mangleName(s), OnStack)
  let name = g.llName(s)

  # Some compiler proc's have two syms essentially, because of an importc trick
  # in system.nim...
  if sfImportc in s.flags:
    result = LLValue(v: g.m.getNamedFunction(name))
    if result.v != nil:
      g.symbols[s.id] = result
      return

  let f = g.addNimFunction(s)

  if g.genFakeImpl(s, f):
    f.setLinkage(g.defaultFunctionLinkage())

  result = LLValue(v: f, storage: s.loc.storage)
  g.symbols[s.id] = result

proc genFunctionWithBody(g: LLGen, s: PSym): LLValue =
  var s = s
  if lfImportCompilerProc in s.loc.flags:
    s = magicsys.getCompilerProc(g.graph, s.name.s)

  result = g.genFunction(s)

  if (lfNoDecl in s.loc.flags or {sfImportc, sfInfixCall} * s.flags != {}) and
      lfImportCompilerProc notin s.loc.flags:
    return

  if result.v.countBasicBlocks() != 0:
    return # already has body

  if sfForward in s.flags:
    g.forwardedProcs.add(s)
    return

  if sfImportc in s.flags:
    return

  if s.id in g.done:
    # The body was added in a module that was already passed to orc
    return
  g.done.incl s.id

  if result.v.getEnumAttributeAtIndex(
    cast[AttributeIndex](AttributeFunctionIndex), attrAllocsize
  ) != nil:
    # TODO work around https://github.com/llvm/llvm-project/issues/66103
    result.v.setLinkage(llvm.LinkOnceODRLinkage)
  elif sfExportc notin s.flags or sfCompilerProc in s.flags:
    # Because we generate only one module, we can tag all functions internal,
    # except those that should be importable from c
    # compilerproc are marker exportc to get a stable name, but it doesn't seem
    # they need to be exported to C - this might change if we start supporting
    # dll:s
    result.v.setLinkage(g.defaultFunctionLinkage())

  let
    typ = s.typ.skipTypes(abstractInst)
    f = g.newLLFunc(result.v, s)

  g.withFunc(f):
    var ret: (llvm.TypeRef, llvm.ValueRef)

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
        if sfNoInit in s.flags:
          incl(res.flags, sfNoInit)
        if not g.isInvalidReturnType(res.typ):
          let resv = g.genLocal(resNode)
          ret = (g.llType(res.typ), resv.v)
          g.initLocalVar(res, res.typ, ret[1], false)
        else:
          let arg = result.v.getParam(i.cuint)
          arg.setValueName("Result")
          g.symbols[res.id] = LLValue(v: arg, lode: resNode)
          i += 1

      # Function arguments

      for param in typ.procParams():
        p("a", param, g.depth + 1)
        p("a", param.sym.typ, g.depth + 2)

        let arg = result.v.getParam(i.cuint)
        arg.setValueName(g.llName(param.sym))

        let symTyp = param.sym.typ.skipTypes({tyGenericInst})
        if skipTypes(symTyp, {tyVar, tyLent, tySink}).kind in {tyOpenArray, tyVarargs}:
          # openArray, regardless if it's var or not, gets passed as two args,
          # data and len - this corresponds to how the cgen passes them - here,
          # we extract the two arguments and stick them in a variable like
          # the rest of the type/load system expects them
          i += 1

          let argLen = result.v.getParam(i.cuint)
          argLen.setValueName(g.llName(param.sym) & "len")

          let av = g.localAlloca(g.llOpenArrayType(), g.nn("arg", arg))
          discard g.b.buildStore(arg, g.buildOpenArrayDataGEP(av))
          discard g.b.buildStore(argLen, g.buildOpenArrayLenGEP(av))

          if symTyp.kind in {tyVar, tyLent}:
            let avp = g.localAlloca(g.ptrTy, g.nn("var", av))
            discard g.b.buildStore(av, avp)

            g.debugVariable(param.sym, avp, i)

            g.symbols[param.sym.id] =
              LLValue(v: avp, lode: param, storage: param.sym.loc.storage)
          else:
            g.debugVariable(param.sym, av, i)

            g.symbols[param.sym.id] =
              LLValue(v: av, lode: param, storage: param.sym.loc.storage)
        else:
          let av = g.localAlloca(arg.typeOfX(), g.nn("arg", arg))
          discard g.b.buildStore(arg, av)

          g.debugVariable(param.sym, av, i + 1)

          g.symbols[param.sym.id] =
            LLValue(v: av, lode: param, storage: param.sym.loc.storage)

        i += 1

      if tfCapturesEnv in typ.flags:
        let arg = result.v.getParam(i.cuint)
        arg.setValueName("ClE_0")
        g.f.clenv = arg

        let ls = lastSon(s.ast[paramsPos])
        let lt = g.llType(ls.sym.typ)
        if g.f.clenv == nil:
          g.config.internalError(s.ast.info, "env missing")
        let lx = g.b.buildBitCast(g.f.clenv, lt, g.nn("ClEnvX"))
        let av = g.localAlloca(lx.typeOfX(), g.nn("ClEnvX.a"))
        discard g.b.buildStore(lx, av)
        g.symbols[ls.sym.id] = LLValue(v: av)

        g.debugVariable(ls.sym, av, i + 1)
        i += 1

      g.b.buildBrFallthrough(g.section(g.f, secBody))

    g.withBlock(g.section(g.f, secBody)):
      var procBody = transformBody(g.graph, g.idgen, s, dontUseCache)
      if sfInjectDestructors in s.flags:
        procBody = injectDestructorCalls(g.graph, g.idgen, s, procBody)

      g.genNode(procBody)
      g.b.buildBrFallthrough(g.section(g.f, secReturn))

      g.f.sections[secLastBody] = g.b.getInsertBlock()

    g.withBlock(g.section(g.f, secReturn)):
      if ret[1] != nil:
        discard g.b.buildRet(g.b.buildLoad2(ret[0], ret[1]))
      else:
        if sfNoReturn in s.flags:
          discard g.b.buildUnreachable()
        else:
          discard g.b.buildRetVoid()

    g.finalize()

proc getOrdering(n: PNode): llvm.AtomicOrdering =
  if n.kind == nkSym:
    case n.sym.name.s
    of "ATOMIC_RELAXED", "moRelaxed":
      llvm.AtomicOrderingMonotonic
    of "ATOMIC_CONSUME", "moConsume":
      llvm.AtomicOrderingAcquire
    # TODO clang uses this for __atomic_add_fetch, but what about other ops?
    of "ATOMIC_ACQUIRE", "moAcquire":
      llvm.AtomicOrderingAcquire
    of "ATOMIC_RELEASE", "moRelease":
      llvm.AtomicOrderingRelease
    of "ATOMIC_ACQ_REL", "moAcquireRelease":
      llvm.AtomicOrderingAcquireRelease
    of "ATOMIC_SEQ_CST", "moSequentiallyConsistent":
      llvm.AtomicOrderingSequentiallyConsistent
    else:
      llvm.AtomicOrderingSequentiallyConsistent
  else:
    llvm.AtomicOrderingSequentiallyConsistent

proc genFakeCall(g: LLGen, n: PNode, o: var LLValue, load: bool): bool =
  let nf = n[0]
  if nf.kind != nkSym:
    return false

  let s = nf.sym
  if s.originatingModule.name.s == "sysatomics":
    if s.name.s == "atomicLoad":
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        ord = getOrdering(n[3])
        ld = g.b.buildLoad2(g.llType(n[1].typ.elemType), p0)
      ld.setOrdering(ord)
      discard g.b.buildStore(ld, p1)
      return true

    if s.name.s == "atomicLoadN":
      let
        p0 = g.genNode(n[1], true).v
        ord = getOrdering(n[2])
      o = LLValue(v: g.b.buildLoad2(g.llType(n[1].typ.elemType), p0))
      o.v.setOrdering(ord)
      return true

    if s.name.s == "atomicStore":
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        ord = getOrdering(n[3])
        ld = g.b.buildLoad2(g.llType(n[2].typ.elemType), p1)
        ax = g.b.buildStore(ld, p0)
      ax.setOrdering(ord)
      return true

    if s.name.s == "atomicStoreN":
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        ord = getOrdering(n[3])
        ax = g.b.buildStore(p1, p0)
      ax.setOrdering(ord)
      return true

    if s.name.s == "atomicAddFetch":
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        ord = getOrdering(n[3])
      o =
        LLValue(v: g.b.buildAtomicRMW(llvm.AtomicRMWBinOpAdd, p0, p1, ord, llvm.False))
      o.v = g.b.buildAdd(o.v, p1, g.nn("atomic.addf", n))
      return true

    if s.name.s == "atomicSubFetch":
      let p0 = g.genNode(n[1], true).v
      let p1 = g.genNode(n[2], true).v
      let ord = getOrdering(n[3])
      o =
        LLValue(v: g.b.buildAtomicRMW(llvm.AtomicRMWBinOpSub, p0, p1, ord, llvm.False))
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
      let x = g.b.buildAtomicCmpXchg(
        p0, p1, p2, llvm.AtomicOrderingSequentiallyConsistent,
        llvm.AtomicOrderingSequentiallyConsistent, llvm.False,
      )
      o = LLValue(v: g.buildI8(g.b.buildExtractValue(x, 1.cuint, g.nn("cas.b", n))))
      return true

    if s.name.s in ["atomicCompareExchangeN", "atomicCompareExchange"]:
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        p2 = g.genNode(n[3], true).v
        ord1 = getOrdering(n[5])
        ord2 = getOrdering(n[6])
        l1 = g.b.buildLoad2(g.llType(n[2].typ.elemType), p1)
        l2 =
          if s.name.s == "atomicCompareExchangeN":
            p2
          else:
            g.b.buildLoad2(g.llType(n[3].typ.elemType), p2)
        x = g.b.buildAtomicCmpXchg(p0, l1, l2, ord1, ord2, llvm.False)
        ok = g.b.buildExtractValue(x, 1.cuint, g.nn("cas.b", n))
        lok = g.b.appendBasicBlockInContext(g.lc, g.nn("cas.ok", n))
        ldone = g.b.appendBasicBlockInContext(g.lc, g.nn("cas.done", n))

      discard g.b.buildCondBr(ok, lok, ldone)

      g.b.positionBuilderAtEnd(lok)

      let loaded = g.b.buildExtractValue(x, 0.cuint, g.nn("cas.b", n))

      discard g.b.buildStore(loaded, p1)
      discard g.b.buildBr(ldone)

      g.b.positionBuilderAtEnd(ldone)

      o = LLValue(v: g.buildI8(ok))
      return true
  elif s.originatingModule.name.s == "system":
    if s.name.s in ["likelyProc", "unlikelyProc"]:
      let tmp = g.genNode(n[1], true)
      o = LLValue(v: g.callExpect(tmp.v, s.name.s == "likelyProc"))
      return true

    if s.name.s == "getCurrentException":
      let ax = g.callCompilerProc("nlvmGetCurrentException", [], noInvoke = true)
      o = LLValue(
        v:
          if load:
            ax
          else:
            let tmp = g.localAlloca(ax.typeOfX, g.nn("tmp", ax))
            discard g.b.buildStore(ax, tmp)
            tmp
      )
      return true

    if s.name.s == "getCurrentExceptionMsg":
      let ax = g.callCompilerProc("nlvmGetCurrentExceptionMsg", [], noInvoke = true)
      o = LLValue(
        v:
          if load:
            ax
          else:
            let tmp = g.localAlloca(ax.typeOfX, g.nn("tmp", ax))
            discard g.b.buildStore(ax, tmp)
            tmp
      )
      return true

    if s.name.s in ["closureIterSetupExc", "setCurrentException"]:
      let p0 = g.genNode(n[1], true).v
      discard g.callCompilerProc("nlvmSetClosureException", [p0], noInvoke = true)
      return true

    if s.name.s == "abs":
      let p0 = g.genNode(n[1], true).v
      o = LLValue(v: g.callFabs(p0))
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
  elif s.originatingModule.name.s == "hashes":
    if s.name.s == "hiXorLo":
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        tyI128 = g.lc.intTypeInContext(128)
        p0128 = g.b.buildZExt(p0, tyI128, g.nn("hiXorLo.p0"))
        p1128 = g.b.buildZExt(p1, tyI128, g.nn("hiXorLo.p1"))
        prod128 = g.b.buildNUWMul(p0128, p1128, g.nn("hiXorLo.mul"))
        shr128 = g.b.buildLShr(
          prod128, llvm.constInt(tyI128, 64, llvm.False), g.nn("hiXorLo.shr")
        )
        xor128 = g.b.buildXor(prod128, shr128, g.nn("hiXorLo.xor"))
      o = LLValue(
        v: g.b.buildTrunc(xor128, g.primitives[tyUInt64], g.nn("hiXorLo.trunc"))
      )
      return true
  elif s.originatingModule.name.s == "math":
    if s.name.s == "signbit":
      # TODO Big endian for fp128, see https://github.com/llvm/llvm-project/blob/85ec8a9ac141a1807d907b7514546f531007d87d/clang/lib/CodeGen/CGBuiltin.cpp#L617=
      let
        p0 = g.genNode(n[1], true).v
        sz = g.lc.intTypeInContext(g.config.getSize(n[1].typ).cuint * 8)
        asint = g.b.buildBitCast(p0, sz, g.nn("signbit.cast"))
      o = LLValue(
        v: g.b.buildICmp(llvm.IntSLT, asint, constNull(sz), g.nn("signbit.slt"))
      )
      return true

    if s.name.s == "copysign":
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
      o = LLValue(v: g.callCopysign(p0, p1))
      return true
  elif s.originatingModule.name.s == "atomics":
    if s.name.s == "atomic_load_explicit":
      let
        p0 = g.genNode(n[1], false).v
        ord = getOrdering(n[2])
        ld = g.b.buildLoad2(g.llType(n[1].typ.elemType), p0)
      ld.setOrdering(ord)
      o = LLValue(v: ld)
      return true
    elif s.name.s == "atomic_store_explicit":
      let
        p0 = g.genNode(n[1], false).v
        p1 = g.genNode(n[2], true).v
        ord = getOrdering(n[3])
        ax = g.b.buildStore(p1, p0)
      ax.setOrdering(ord)
      return true
    elif s.name.s == "atomic_exchange_explicit":
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        ord = getOrdering(n[3])
      o = LLValue(v: g.b.buildAtomicRMW(AtomicRMWBinOpXchg, p0, p1, ord, llvm.False))

      return true
    elif s.name.s in
        [
          "atomic_compare_exchange_strong_explicit",
          "atomic_compare_exchange_weak_explicit",
        ]:
      let
        p0 = g.genNode(n[1], true).v
        p1 = g.genNode(n[2], true).v
        p2 = g.genNode(n[3], true).v
        ord1 = getOrdering(n[4])
        ord2 = getOrdering(n[5])
        l1 = g.b.buildLoad2(g.llType(n[2].typ.elemType), p1)
        x = g.b.buildAtomicCmpXchg(p0, l1, p2, ord1, ord2, llvm.False)
        ok = g.b.buildExtractValue(x, 1.cuint, g.nn("cas.b", n))
        loaded = g.b.buildExtractValue(x, 0.cuint, g.nn("cas.b", n))

      discard g.b.buildStore(loaded, p1)

      if s.name.s == "atomic_compare_exchange_weak_explicit":
        x.setWeak(llvm.True)
      o = LLValue(v: g.buildI8(ok))
      return true
    elif s.name.s in [
      "atomic_fetch_add_explicit", "atomic_fetch_sub_explicit",
      "atomic_fetch_and_explicit", "atomic_fetch_or_explicit",
      "atomic_fetch_xor_explicit",
    ]:
      let
        p0 = g.genNode(n[1], false).v
        p1 = g.genNode(n[2], true).v
        ord = getOrdering(n[3])
        op =
          case s.name.s
          of "atomic_fetch_add_explicit": llvm.AtomicRMWBinOpAdd
          of "atomic_fetch_sub_explicit": llvm.AtomicRMWBinOpSub
          of "atomic_fetch_and_explicit": llvm.AtomicRMWBinOpAnd
          of "atomic_fetch_or_explicit": llvm.AtomicRMWBinOpOr
          else: llvm.AtomicRMWBinOpXor

      o = LLValue(v: g.b.buildAtomicRMW(op, p0, p1, ord, llvm.False))
      return true
    elif s.name.s in ["testAndSet"]:
      let
        p0 = g.genNode(n[1], false).v
        ord = getOrdering(n[2])
        x = g.b.buildAtomicRMW(AtomicRMWBinOpXchg, p0, g.constInt8(1), ord, llvm.False)
        cmp = g.b.buildICmp(llvm.IntNE, x, g.constInt8(0), g.nn("tas.ret"))

      o = LLValue(v: g.buildI8(cmp))
      return true
    elif s.name.s in ["clear"]:
      let
        p0 = g.genNode(n[1], false).v
        ord = getOrdering(n[2])
        x = g.b.buildStore(g.constInt8(0), p0)

      x.setOrdering(ord)
      return true
    elif s.name.s == "fence":
      let ord = getOrdering(n[1])
      o = LLValue(v: g.b.buildFence(ord, llvm.False, ""))
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
    of "alloca", "__builtin_alloca":
      let p0 = g.genNode(n[1], true).v
      o = LLValue(v: g.b.buildArrayAlloca(g.int8Ty, p0, g.nn("alloca", n)))
      return true

proc genBoundsCheck(g: LLGen, arr, len, a, b: llvm.ValueRef) =
  let
    diff = g.b.buildSub(b, a, g.nn("bc.diff", arr))
    minusOne = constInt(diff.typeOfX, not culonglong(0), llvm.True)
    diffne = g.b.buildICmp(llvm.IntNE, minusOne, diff, g.nn("bc.diffne", arr))
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
    diffne = g.b.buildICmp(llvm.IntNE, minusOne, diff, g.nn("bca.diffne", arr))
    difflt = g.b.buildICmp(llvm.IntSLT, diff, minusOne, g.nn("bc.diffne", arr))
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

proc buildLoadVar(g: LLGen, typ: PType, v: llvm.ValueRef): llvm.ValueRef =
  if typ.skipTypes(abstractInst).kind in {tyVar, tyLent}:
    g.b.buildLoad2(g.ptrTy, v)
  else:
    v

proc genOpenArrayConv(
    g: LLGen, symTyp: PType, n: PNode
): (llvm.ValueRef, llvm.ValueRef) =
  # TODO when this is an mSlice coming from the parallel transform,
  #      the type of node will be wrong and this code will fail -
  #      see https://github.com/nim-lang/Nim/issues/20958 - we could work
  #      around it here by introducing special mSlice handling, but that seems
  #      wrong...
  let
    # TODO https://github.com/nim-lang/Nim/issues/23945
    n =
      if n.kind == nkWhenStmt:
        n[1][0]
      else:
        n
    # The nkHiddenAddr here may appear with the wrong type, ie tyOpenArray
    # and not tySequence! It doesn't matter for the code though
    typ = n.typ.skipTypes(abstractVar + {tyStatic})

  let (data, len) =
    case typ.kind
    of tyString, tySequence:
      let
        prepareForMutation =
          symTyp.skipTypes(abstractInst).kind in {tyVar} and typ.kind == tyString and
          optSeqDestructors in g.config.globalOptions
        seqTy = g.llSeqType(typ)
        axp = g.genNode(n, not prepareForMutation)
        ax =
          if prepareForMutation:
            discard g.callCompilerProc("nimPrepareStrMutationV2", [axp.v])
            g.buildLoadValue(g.llType(n.typ), axp).v
          else:
            axp.v
        v = g.buildLoadVar(n.typ, ax)

      (g.getNimSeqDataPtr(seqTy, v), g.loadNimSeqLen(v))
    of tyOpenArray, tyVarargs:
      let ax = g.buildLoadVar(n.typ, g.genNode(n, false).v)

      (
        g.b.buildLoad2(g.ptrTy, g.buildOpenArrayDataGEP(ax)),
        g.b.buildLoad2(g.intTy, g.buildOpenArrayLenGEP(ax)),
      )
    of tyArray, tyUncheckedArray:
      var v = g.genNode(n, true).v
      if n.typ.skipTypes(abstractInst).kind in {tyVar, tyLent}:
        let ty = g.llType(typ)
        v = g.buildLoadValue(ty, v)
      (v, g.constNimInt(g.config.lengthOrd(typ)))
    of tyPtr, tyRef:
      case typ.lastSon().kind
      of tyString, tySequence:
        let
          ty = g.llType(typ.lastSon())
          seqTy = g.llSeqType(typ.lastSon())
        var v = g.genNode(n, true).v
        v = g.b.buildLoad2(ty, v)
        (g.getNimSeqDataPtr(seqTy, v), g.loadNimSeqLen(v))
      of tyArray:
        (g.genNode(n, true).v, g.constNimInt(g.config.lengthOrd(typ.lastSon)))
      else:
        g.config.internalError(n.info, "Unhandled ref length: " & $typ.lastSon())
        raiseAssert "unreachable"
    else:
      g.config.internalError(n.info, "Unhandled length: " & $typ)
      raiseAssert "unreachable"
  (data, len)

from compiler/aliasanalysis import aliases, AliasKind

proc potentialAlias(n: PNode, potentialWrites: seq[PNode]): bool =
  for p in potentialWrites:
    if p.aliases(n) != no or n.aliases(p) != no:
      return true

proc skipTrivialIndirections(n: PNode): PNode =
  result = n
  while true:
    case result.kind
    of nkDerefExpr, nkHiddenDeref, nkAddr, nkHiddenAddr, nkObjDownConv, nkObjUpConv:
      result = result[0]
    of nkHiddenStdConv, nkHiddenSubConv:
      result = result[1]
    else:
      break

proc getPotentialWrites(n: PNode, mutate: bool, result: var seq[PNode]) =
  case n.kind
  of nkLiterals, nkIdent, nkFormalParams:
    discard
  of nkSym:
    if mutate:
      result.add n
  of nkAsgn, nkFastAsgn, nkSinkAsgn:
    getPotentialWrites(n[0], true, result)
    getPotentialWrites(n[1], mutate, result)
  of nkAddr, nkHiddenAddr:
    getPotentialWrites(n[0], true, result)
  of nkBracketExpr, nkDotExpr, nkCheckedFieldExpr:
    getPotentialWrites(n[0], mutate, result)
  of nkCallKinds:
    case n.getMagic
    of mIncl, mExcl, mInc, mDec, mAppendStrCh, mAppendStrStr, mAppendSeqElem, mAddr,
        mNew, mNewFinalize, mWasMoved, mDestroy, mReset:
      getPotentialWrites(n[1], true, result)
      for i in 2 ..< n.len:
        getPotentialWrites(n[i], mutate, result)
    of mSwap:
      for i in 1 ..< n.len:
        getPotentialWrites(n[i], true, result)
    else:
      for i in 1 ..< n.len:
        getPotentialWrites(n[i], mutate, result)
  else:
    for s in n:
      getPotentialWrites(s, mutate, result)

proc getPotentialReads(n: PNode, result: var seq[PNode]) =
  case n.kind
  of nkLiterals, nkIdent, nkFormalParams:
    discard
  of nkSym:
    result.add n
  else:
    for s in n:
      getPotentialReads(s, result)

proc genCallArgs(
    g: LLGen, n: PNode, fxt: llvm.TypeRef, ftyp: PType, extra: int
): seq[llvm.ValueRef] =
  # We must generate temporaries in cases like #14396
  # to keep the strict Left-To-Right evaluation
  var needTmp = newSeq[bool](n.len - 1)
  var potentialWrites: seq[PNode]
  for i in countdown(n.len - 1, 1):
    if n[i].skipTrivialIndirections.kind == nkSym:
      needTmp[i - 1] = potentialAlias(n[i], potentialWrites)
    else:
      #if not ri[i].typ.isCompileTimeOnly:
      var potentialReads: seq[PNode]
      getPotentialReads(n[i], potentialReads)
      for n in potentialReads:
        if not needTmp[i - 1]:
          needTmp[i - 1] = potentialAlias(n, potentialWrites)
      getPotentialWrites(n[i], false, potentialWrites)

  var args: seq[ValueRef] = @[]

  let parTypes = fxt.getParamTypes()
  for i in 1 ..< n.len:
    let
      p = n[i]
      pr = (if p.kind == nkHiddenAddr: p[0] else: p)

    if i >= ftyp.n.len: # varargs like printf, for example
      let v = g.genNode(pr, true).v
      # In some transformations, the compiler produces a call node to a
      # parameterless function, and then adds statements as child nodes where
      # the parameter value expressions would normally go - decidedly, odd -
      # see tautoproc.nim
      if v != nil and v.typeOfX().getTypeKind() != llvm.VoidTypeKind:
        args.add(v)
      continue

    let
      param = ftyp.n[i]
      symTyp = param.sym.typ.skipTypes({tyGenericInst})
    if symTyp.isCompileTimeOnly():
      continue

    if skipTypes(symTyp, abstractVar + {tyStatic}).kind in {tyOpenArray, tyVarargs}:
      let (data, len) = g.genOpenArrayConv(symTyp, pr)
      args.add([data, len])
    else:
      let pt = parTypes[args.len + extra]
      var v =
        if needTmp[i - 1]:
          let
            pty = g.llType(p.typ)
            tmp = LLValue(v: g.localAlloca(pty, g.nn("alias.tmp", n)), storage: OnStack)
          g.buildStoreNull(pty, tmp.v)
          g.genObjectInit(p.typ, tmp.v)
          let bx = g.genAsgnNode(n[i], p.typ, tmp)
          g.genAssignment(tmp, bx, p.typ, {})
          if g.llPassAsPtr(param.sym, ftyp[0]):
            tmp.v
          else:
            g.b.buildLoad2(pty, tmp.v)
        else:
          g.genNode(p, not g.llPassAsPtr(param.sym, ftyp[0])).v
      # We need to use the type from the function, because with multimethods,
      # it looks like the type in param.sym.typ changes during compilation!
      # seen with tmultim1.nim
      v = g.preCast(g.isUnsigned(p.deepTyp), v, symTyp, pt)
      args.add(v)

  args

proc preventNrvo(g: LLGen, dest, le, ri: PNode): bool =
  # from cgen, this detects when it is safe to use an existing memory location
  # to collect results from a call
  proc locationEscapes(g: LLGen, le: PNode, inTryStmt: bool): bool =
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
      of nkDotExpr, nkBracketExpr, nkObjUpConv, nkObjDownConv, nkCheckedFieldExpr:
        n = n[0]
      of nkHiddenStdConv, nkHiddenSubConv, nkConv:
        n = n[1]
      else:
        # cannot analyse the location; assume the worst
        return true

  if le != nil:
    for i in 1 ..< ri.len:
      let r = ri[i]
      if isPartOf(le, r) != arNo:
        return true
    # we use the weaker 'canRaise' here in order to prevent too many
    # annoying warnings, see #14514
    if canRaise(ri[0]) and locationEscapes(g, le, g.f.nestedTryStmts.len > 0):
      message(g.config, le.info, warnObservableStores, $le)
  # bug #19613 prevent dangerous aliasing too:
  if dest != nil and dest != le:
    for i in 1 ..< ri.len:
      let r = ri[i]
      if isPartOf(dest, r) != arNo:
        return true

proc genCall(g: LLGen, le, n: PNode, load: bool, dest: LLValue): LLValue =
  # The `dest` variable contains information about the placement of the result of
  # the call - in certain cases, the return value of the function can be
  # placed directly in `dest` in which case the ordinary `LLValue` returned will
  # be `nil`.
  if g.genFakeCall(n, result, load):
    return

  var needsReset = false
  let
    nf = n[namePos]
    ftyp = nf.typ.skipTypes(abstractInstOwned)
    fx = g.genNode(nf, true).v
    fty = g.llProcType(ftyp)
    retty = fty.getReturnType()
    retArgType =
      if ftyp[0] == nil:
        g.voidTy
      else:
        g.llType(ftyp[0])
    retArgs =
      if g.isInvalidReturnType(ftyp[0]):
        if dest.v != nil and dest.lode != nil and
            dest.v.typeOfX().getTypeKind == llvm.PointerTypeKind and
            not (isPartOf(dest.lode, n) != arNo) and not g.preventNrvo(dest.lode, le, n):
          needsReset = true
          @[dest.v]
        else:
          let tmp = g.localAlloca(retArgType, g.nn("call.res.stack", n))
          g.buildStoreNull(retArgType, tmp)
          g.genObjectInit(ftyp[0], tmp)
          @[tmp]
      else:
        @[]
    args = g.genCallArgs(n, fty, ftyp, retArgs.len)
  if needsReset:
    # Reset after process call arguments
    g.callReset(ftyp[0], LLValue(v: retArgs[0]))

  let callres =
    if ftyp.callConv == ccClosure:
      let
        prc = g.b.buildExtractValue(fx, 0, g.nn("call.clo.prc.ptr", n))
        env = g.b.buildExtractValue(fx, 1, g.nn("call.clo.env.ptr", n))
        cfty = g.llProcType(ftyp, true) # Closure function pointer with environment

      if tfIterator in ftyp.flags:
        g.buildCallOrInvoke(cfty, prc, retArgs & args & @[env], g.nn("call.iter", n))
      else:
        # If (dynamic) closure environment is `nil`, call function without it
        let
          clonil = g.b.appendBasicBlockInContext(g.lc, g.nn("call.clo.noenv", n))
          cloenv = g.b.appendBasicBlockInContext(g.lc, g.nn("call.clo.env", n))
          cloend = g.b.appendBasicBlockInContext(g.lc, g.nn("call.clo.end", n))
          cmp = g.b.buildICmp(
            llvm.IntEQ, env, llvm.constNull(env.typeOfX()), g.nn("call.clo.noenv", n)
          )

        discard g.b.buildCondBr(cmp, clonil, cloenv)

        g.b.positionBuilderAtEnd(clonil)

        let res =
          g.buildCallOrInvokeBr(fty, prc, cloend, retArgs & args, g.nn("call.clo", n))

        g.b.positionBuilderAtEnd(cloenv)

        let cres = g.buildCallOrInvokeBr(
          cfty, prc, cloend, retArgs & args & @[env], g.nn("call.clo", n)
        )

        g.b.positionBuilderAtEnd(cloend)

        if retty.getTypeKind() != llvm.VoidTypeKind:
          let phi = g.b.buildPHI(res.typeOfX(), g.nn("call.clo.res", n))
          phi.addIncoming([res, cres], [clonil, cloenv])
          phi
        else:
          nil
    else:
      g.buildCallOrInvoke(fty, fx, retArgs & args, g.nn("call.res", n))

  if (retty.getTypeKind() != llvm.VoidTypeKind and not load):
    # if the originator of the call wants a pointer, we'll have
    # to create one for them - this is interesting for example
    # when a struct is returned "by value"
    let v = g.localAlloca(retty, g.nn("call.res.ptr", n))
    discard g.b.buildStore(callres, v)
    LLValue(v: v)
  elif (retty.getTypeKind() == llvm.ArrayTypeKind):
    let v = g.localAlloca(retty, g.nn("call.res.ptr", n))
    discard g.b.buildStore(callres, v)
    g.maybeLoadValue(retty, LLValue(v: v), load)
  elif retArgs.len > 0:
    if retArgs[0] == dest.v: # Consumed by RVO
      LLValue()
    else:
      g.maybeLoadValue(retArgType, LLValue(v: retArgs[0]), load)
  else:
    LLValue(v: callres)

proc genMagicCall(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  let s = n[namePos].sym
  if lfNoDecl notin s.loc.flags:
    let m = g.graph.getCompilerProc($s.loc.r)
    if m == nil:
      g.config.internalError(n.info, "Missing magic: " & $s.loc.r)
    discard g.genFunctionWithBody(m)

  g.genCall(nil, n, load, dest)

proc genRefAssign(g: LLGen, dest: LLValue, src: llvm.ValueRef) =
  if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
      not usesWriteBarrier(g.config):
    discard g.b.buildStore(src, dest.v)
  elif dest.storage == OnHeap:
    discard g.callCompilerProc("asgnRef", [dest.v, src])
  else:
    discard g.callCompilerProc("unsureAsgnRef", [dest.v, src])

proc callGenericAssign(
    g: LLGen, dest, src: LLValue, typ: PType, flags: TAssignmentFlags
) =
  if optSeqDestructors in g.config.globalOptions:
    g.callMemcpy(
      dest.v, src.v, g.constStoreSize(g.llType(typ.skipTypes(abstractVarRange)))
    )
  elif (needToCopy notin flags or tfShallow in typ.skipTypes(abstractVarRange).flags) and
      src.storage != OnStatic:
    if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
        not usesWriteBarrier(g.config):
      g.callMemcpy(
        dest.v, src.v, g.constStoreSize(g.llType(typ.skipTypes(abstractVarRange)))
      )
    else:
      g.callAssign(typ, dest.v, src.v, g.constInt1(true))
  else:
    g.callAssign(typ, dest.v, src.v, g.constInt1(false))

proc genAssignment(g: LLGen, dest, src: LLValue, typ: PType, flags: TAssignmentFlags) =
  # Assign src to dest - src needs to be a value or pointer depending on what
  # loadAssignment returns for `typ`
  if src.v == nil or src.v.typeOfX().getTypeKind() == llvm.VoidTypeKind:
    # When RVO kicks in or when a branch of a control-flow expression that is
    # guaranteed to not be returned from but still has to be generated,
    # we may end up with a nil here
    return
  let
    typ = typ.skipTypes(abstractRange + tyUserTypeClasses + {tyStatic})
    ty = g.llType(typ)

  case typ.kind
  of tyRef:
    g.genRefAssign(dest, src.v)
  of tySequence:
    if optSeqDestructors in g.config.globalOptions:
      discard g.b.buildStore(src.v, dest.v)
    elif (needToCopy notin flags and src.storage != OnStatic) or canMove(g, src.lode):
      g.genRefAssign(dest, src.v)
    else:
      let srctmp = g.localAlloca(g.ptrTy, g.nn("tmp", src.v))
      discard g.b.buildStore(src.v, srctmp)
      g.callAssign(typ, dest.v, srctmp, g.constInt1(false))
  of tyString:
    if optSeqDestructors in g.config.globalOptions:
      discard g.b.buildStore(src.v, dest.v)
    elif ({needToCopy} * flags == {} and src.storage != OnStatic) or canMove(
      g, src.lode
    ):
      g.genRefAssign(dest, src.v)
    else:
      if (dest.storage == OnStack and g.config.selectedGC != gcGo) or
          not usesWriteBarrier(g.config):
        let srcc = g.callCompilerProc("copyString", [src.v])
        discard g.b.buildStore(srcc, dest.v)
      elif dest.storage == OnHeap:
        let destl = g.b.buildLoad2(ty, dest.v)
        let srcc = g.callCompilerProc("copyStringRC1", [src.v])
        discard g.b.buildStore(srcc, dest.v)
        g.withNotNil(destl):
          discard g.callCompilerProc("nimGCunrefNoCycle", [destl])
      else:
        let srcc = g.callCompilerProc("copyString", [src.v])
        discard g.callCompilerProc("unsureAsgnRef", [dest.v, srcc])
  of tyProc:
    if typ.containsGarbageCollectedRef():
      # TODO this is a hack to work around "automatic" conversion between
      #      function pointers and closures with nil environment - it
      #      probably needs improvement
      if src.v.typeOfX.getTypeKind() == llvm.PointerTypeKind:
        let destp = g.buildClosurePrcGEP(dest.v)
        discard g.b.buildStore(src.v, destp)
        let deste = g.buildClosureEnvGEP(dest.v)
        g.genRefAssign(LLValue(v: deste, storage: dest.storage), constNull(g.ptrTy))
      else:
        let
          destp = g.buildClosurePrcGEP(dest.v)
          srcp = g.b.buildExtractValue(src, 0, g.nn("asgnr.p", src))
        discard g.b.buildStore(srcp.v, destp)

        let
          deste = g.buildClosureEnvGEP(dest.v)
          srce = g.b.buildExtractValue(src, 1, g.nn("asgnr.e", src))
        g.genRefAssign(LLValue(v: deste, storage: dest.storage), srce.v)
    else:
      discard g.b.buildStore(g.b.buildBitCast(src.v, ty, g.nn("asgnr.xc")), dest.v)
  of tyOpenArray:
    discard g.b.buildStore(src.v, dest.v)
  of tyTuple, tyArray:
    if typ.supportsCopyMem():
      g.callMemcpy(dest.v, src.v, g.constStoreSize(ty))
    else:
      g.callGenericAssign(dest, src, typ, flags)
  of tyObject:
    if typ.supportsCopyMem() and typ.isObjLackingTypeField():
      g.callMemcpy(dest.v, src.v, g.constStoreSize(ty))
    else:
      g.callGenericAssign(dest, src, typ, flags)
  of tySet:
    let size = g.config.getSize(typ)

    if size <= 8:
      discard g.b.buildStore(src.v, dest.v)
    else:
      g.callMemcpy(dest.v, src.v, g.constStoreSize(ty))
  of tyPtr,
      tyPointer,
      tyChar,
      tyBool,
      tyEnum,
      tyCString,
      tyInt .. tyUInt64,
      tyRange,
      tyVar,
      tyLent,
      tyNil:
    let pc = g.preCast(g.isUnsigned(typ), src.v, typ, ty)
    discard g.b.buildStore(pc, dest.v)
  else:
    g.config.internalError("genAssignment: " & $typ.kind)

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
    true
  of tySet:
    let size = g.config.getSize(ty)

    if size <= 8: true else: false
  of tyPtr,
      tyPointer,
      tyChar,
      tyBool,
      tyEnum,
      tyCString,
      tyInt .. tyUInt64,
      tyRange,
      tyVar,
      tyLent:
    true
  of tyNil:
    true
  else:
    g.config.internalError("loadAssignment: " & $typ)
    false

proc genAsgnNode(g: LLGen, n: PNode, typ: PType, dest: LLValue): LLValue =
  # genNode, but for use as RHS in a call to genAssignment - requires special
  # care to convert things to openArray since this is not handled by the AST
  # typ is the destination type
  if skipTypes(typ, abstractVar + {tyStatic}).kind in {tyOpenArray, tyVarargs}:
    let (data, len) = g.genOpenArrayConv(typ, n)
    LLValue(v: g.buildOpenArray(data, len), lode: n)
  else:
    let load = g.loadAssignment(typ)
    g.genNode(n, load, dest)

proc isSeqLike(n: PNode): bool =
  case n.kind
  of nkStrLit .. nkTripleStrLit:
    true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    n[1].isSeqLike()
  else:
    n.typ.skipTypes(irrelevantForBackend).kind == tySequence

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef

proc genConstCurly(g: LLGen, n: PNode): llvm.ValueRef =
  assert n.kind == nkCurly
  let
    typ = skipTypes(n.typ, abstractVar)
    size = g.config.getSize(typ)
    cs = g.config.toBitSet(n)
  if size <= 8:
    g.constInt(size * 8, cs.bitSetToWord(size.int))
  else:
    proc ci8(v: byte): llvm.ValueRef =
      g.constUInt8(v)

    llvm.constArray(g.primitives[tyUInt8], cs.map(ci8))

proc genConstBracket(g: LLGen, n: PNode): llvm.ValueRef =
  let
    typ = n.typ.skipTypes(irrelevantForBackend)
    ty = g.llType(typ)
  if typ.elemType.kind == tyEmpty or n.len == 0:
    llvm.constNull(ty)
  else:
    let et = g.llType(typ.elemType)
    var vals = newSeq[llvm.ValueRef](n.len)
    for i, s in n.sons:
      vals[i] = g.genConstInitializer(s)
    let s = constArray(et, vals)
    if typ.kind in {tyArray, tyUncheckedArray}:
      s
    else:
      let
        ll = g.constNimInt(vals.len)
        cap = g.constNimInt(vals.len + g.strLitFlag)
      if optSeqDestructors in g.config.globalOptions:
        let
          payload = llvm.constStructInContext(g.lc, [cap, s])
          lit = g.m.addPrivateConstant(payload.typeOfX, g.nn(".seq", n))
        lit.setInitializer(payload)
        llvm.constNamedStruct(ty, [ll, lit])
      else:
        let
          x = llvm.constNamedStruct(g.llGenericSeqType(), [ll, cap])
          payload = llvm.constStructInContext(g.lc, [x, s])
          lit = g.m.addPrivateConstant(payload.typeOfX, g.nn(".seq", n))
        lit.setInitializer(payload)
        lit

proc genConstObjConstr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    typ = n.typ.skipTypes(abstractInst + irrelevantForBackend)
    ty = g.llType(typ)

  if typ.kind == tyRef:
    g.config.internalError(n.info, "no const objs with refs")

  var vals = newSeq[llvm.ValueRef](ty.countStructElementTypes())

  for i in 1 ..< n.len:
    let
      s = n[i]
      ind = g.fieldIndex(typ, s[0].sym)
    if ind.len != 1:
      g.config.internalError(s.info, "const case objects not yet supported")

    vals[ind[0].index] = g.genConstInitializer(s)

  for i in 0 ..< vals.len:
    if isNil(vals[i]):
      vals[i] = llvm.constNull(ty.structGetTypeAtIndex(i.cuint))

  constNamedStruct(ty, vals)

proc genConstTupleConstr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)

  var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())

  for i in 0 ..< n.len:
    vals[i] = g.genConstInitializer(n[i])

  for i in 0 ..< vals.len:
    if isNil(vals[i]):
      vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))

  constNamedStruct(t, vals)

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef =
  case n.kind
  of nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    g.genConstInitializer(n[1])
  of nkCurly:
    g.genConstCurly(n)
  of nkBracket:
    g.genConstBracket(n)
  of nkObjConstr:
    g.genConstObjConstr(n)
  of nkTupleConstr, nkPar, nkClosure:
    g.genConstTupleConstr(n)
  of nkCharLit .. nkFloat128Lit, nkNilLit:
    g.genNode(n, true).v
  of nkStrLit .. nkTripleStrLit:
    g.genNode(n, true).v
  of nkEmpty:
    constNull(g.llType(n.typ))
  else:
    g.config.internalError(n.info, "Can't gen const initializer " & $n.kind)
    quit 1

proc genFakeConstInitializer(g: LLGen, typ: PType, constr: PNode, v: LLValue)

proc caseObjDefaultBranch(obj: PNode, branch: Int128): int =
  # From ccgexprs
  for i in 1 ..< obj.len:
    for j in 0 .. obj[i].len - 2:
      if obj[i][j].kind == nkRange:
        let x = getOrdValue(obj[i][j][0])
        let y = getOrdValue(obj[i][j][1])
        if branch >= x and branch <= y:
          return i
      elif getOrdValue(obj[i][j]) == branch:
        return i
    if obj[i].len == 1:
      # else branch
      return i
  assert(false, "unreachable")

proc genFakeConstInitializerFields(
    g: LLGen,
    mapper: var FieldMapper,
    n: PNode,
    ty: llvm.TypeRef,
    constr: PNode,
    v: LLValue,
) =
  if n == nil:
    return
  case n.kind
  of nkRecList:
    for child in n:
      g.genFakeConstInitializerFields(mapper, child, ty, constr, v)
  of nkRecCase:
    var branchOrd = Zero
    if constr != nil:
      ## find kind value, default is zero if not specified
      for i in 1 ..< constr.len:
        if constr[i].kind == nkExprColonExpr:
          if constr[i][0].sym.name.id == n[0].sym.name.id:
            branchOrd = getOrdValue(constr[i][1])
            break
        elif i == n[0].sym.position:
          branchOrd = getOrdValue(constr[i])
          break
    let
      selectedBranch = caseObjDefaultBranch(n, branchOrd)
      branch = n[selectedBranch]
      tags = n[0].sym
      tagTy = g.llType(tags.typ)
      tagElement = g.addField(mapper, tagTy, tags)
      tagGEP = g.b.buildStructGEP2(ty, v, cuint tagElement, g.nn("const.branch", v))

    discard g.b.buildStore(
      constInt(tagTy, branchOrd.toInt().culonglong, llvm.False), tagGEP.v
    )

    let
      maxAlign = foldl(1 ..< n.len, max(a, g.maxAlign(n[b].lastSon)), 1)
      storeTy = g.lc.getTypeByName2(caseTypeName(ty, tags, 0))
      storeElement = g.addField(
        mapper, storeTy, g.m.getModuleDataLayout().aBISizeOfType(storeTy).int, maxAlign
      )
      storeGEP = g.b.buildStructGEP2(ty, v, cuint storeElement, g.nn("const.branch", v))
      branchTy = g.lc.getTypeByName2(caseTypeName(ty, tags, selectedBranch))

    var recMapper = FieldMapper(packed: mapper.packed)
    g.genFakeConstInitializerFields(
      recMapper, branch.lastSon, branchTy, constr, storeGEP
    )
  of nkSym:
    let field = n.sym
    if field.typ.isEmptyType():
      return

    let element = g.addField(mapper, g.llType(field.typ), field)

    # TODO This will result in out-of-order execution of the init funcs - not
    #      ideal but...
    if constr != nil:
      var init: PNode
      for i in 1 ..< constr.len:
        if constr[i].kind == nkExprColonExpr:
          if constr[i][0].sym.name.id == field.name.id:
            init = constr[i][1]
            break
        elif i == field.position:
          init = constr[i]
          break

      if init != nil and init.kind != nkEmpty:
        g.debugUpdateLoc(field)
        let fieldGEP = g.b.buildStructGEP2(ty, v, cuint element, g.nn("const", field))

        g.genFakeConstInitializer(field.typ, init, fieldGEP)
  else:
    g.config.internalError("Unexpected kind in genResetFields: " & $n.kind)

proc genFakeConstInitializer(g: LLGen, typ: PType, constr: PNode, v: LLValue) =
  # case objects and other special cases need special const inits - annoyingly,
  # the object constructor in the AST contains initializers also for the
  # inactive case branches meaning it's a mess to create an initializer
  # TODO for case objects in particular, we could generate a true constant
  #      but it's messy with padding
  let typ = typ.skipTypes(abstractRange + tyUserTypeClasses + {tyStatic})
  case typ.kind
  of tyObject:
    let ty = g.llType(typ)
    var mapper = FieldMapper(packed: tfPacked in typ.flags)

    if typ[0] != nil:
      let supTyp = typ[0].skipTypes(skipPtrs)
      discard g.addField(mapper, g.llType(supTyp), supTyp)
      g.genFakeConstInitializer(supTyp, constr, v)
    elif typ.hasMTypeField: # Skip m_type in inheritable root object
      discard g.addField(mapper, g.ptrTy, int(g.ptrBits div 8), int(g.ptrBits div 8))

    g.genFakeConstInitializerFields(mapper, typ.n, ty, constr, v)
  of tyTuple:
    let ty = g.llType(typ)
    var mapper = FieldMapper(packed: tfPacked in typ.flags)

    g.genFakeConstInitializerFields(mapper, typ.n, ty, constr, v)
  of tyArray:
    if constr != nil:
      let ty = g.llType(typ)
      doAssert constr.kind == nkBracket, $constr.kind
      for i in 0 ..< constr.len:
        let gep = g.b.buildInboundsGEP2(
          ty, v, [g.gep0, g.constGEPIdx(i)], g.nn("bracket.n", constr)
        )
        g.genFakeConstInitializer(typ.elemType, constr[i], gep)
  else:
    # TODO this doesn't work when case objects are nested .. for that, what's
    #      needed really is to make proper constants or fix genNodeObjConstr or
    #      fix upstream
    let bx = g.genAsgnNode(constr, typ, v)

    g.genAssignment(v, bx, typ, {})

proc genConst(g: LLGen, n: PNode): LLValue =
  let
    sym = n.sym
    init = sym.astdef

  if sym.id in g.symbols:
    let
      name = g.llName(sym)
      ty = g.llType(sym.typ)
    return g.refGlobal(g.symbols[sym.id], name, ty)

  if init.isDeepConstExprLL():
    result = g.genGlobal(n, true)

    let ci = g.genConstInitializer(init)
    if ci == nil:
      g.config.internalError(n.info, "Unable to generate const initializer: " & $init)

    # TODO when enabled, ptr equality checks (for example in isObj) get
    #      optimized away - need to consider when it's safe
    # result.setUnnamedAddr(llvm.True)

    result.v.setInitializer(ci)
    return

  if sfGlobal in sym.flags:
    result = g.genGlobal(n, false)
    g.registerGcRoot(sym, result.v)

    # Initialize global in init function
    let initFunc = g.getInitFunc()

    if initFunc.sections[secLastPreinit] == nil:
      initFunc.sections[secLastPreinit] = g.section(initFunc, secPreinit)

    g.withFunc(initFunc):
      g.debugUpdateLoc(sym)
      g.withBlock(g.section(initFunc, secLastPreinit)):
        # Avoid using the wrong exception handling context when initializing
        # globals..
        # TODO use the right exception handling context
        let nts = g.f.nestedTryStmts
        g.f.nestedTryStmts = @[]

        g.genFakeConstInitializer(sym.typ, init, result)

        initFunc.sections[secLastPreinit] = g.b.getInsertBlock()
        g.f.nestedTryStmts = nts
  else:
    let ty = g.llType(sym.typ)
    result = LLValue(v: g.localAlloca(ty, g.llName(sym)), storage: OnStack)
    # Some initializers expect value to be null, so we always set it so
    g.buildStoreNull(ty, result.v)
    g.genObjectInit(sym.typ, result.v)

    if init.kind != nkEmpty:
      g.genFakeConstInitializer(sym.typ, init, result)

    g.symbols[sym.id] = result

proc genCallOrNode(g: LLGen, le, ri: PNode, ax: LLValue): LLValue =
  if ri.kind in nkCallKinds and (ri[0].kind != nkSym or ri[0].sym.magic == mNone):
    let lx = g.loadAssignment(le.typ)
    g.genCall(le, ri, lx, ax)
  else:
    g.genAsgnNode(ri, le.typ, ax)

proc genSingleVar(g: LLGen, v: PSym, vn, value: PNode) =
  if sfGoto in v.flags:
    g.config.internalError(vn.info, "Goto vars not supported")

  let x =
    if sfGlobal in v.flags:
      if v.flags * {sfImportc, sfExportc} == {sfImportc} and value.kind == nkEmpty and
          v.loc.flags * {lfHeader, lfNoDecl} != {}:
        return

      let
        isConstInit = g.f.withinLoop == 0 and value.isDeepConstExprLL()
        isConst = isConstInit and v.kind == skLet
        tmp = g.genGlobal(vn, isConst)

      if isConstInit and (v.kind == skLet or not containsGarbageCollectedRef(v.typ)):
        let ci = g.genConstInitializer(value)
        if ci == nil:
          g.config.internalError(
            vn.info, "Unable to generate const initializer: " & $value
          )

        # TODO when enabled, ptr equality checks (for example in isObj) get
        #      optimized away - need to consider when it's safe
        # result.setUnnamedAddr(llvm.True)
        tmp.v.setInitializer(ci)
        return

      g.genObjectInit(v.typ, tmp.v)
      g.registerGcRoot(v, tmp.v)
      # oddly, variables in a loop in the global scope are tagged "global" even
      # though they're local to the looping block
      if g.f.withinLoop > 0:
        g.callReset(v.typ, tmp)

      if value.kind != nkEmpty and sfPure in v.flags:
        # Globals marked {.global.} get an `sfPure` flag and are initialized
        # before other globals
        let initFunc = g.getInitFunc()

        if initFunc.sections[secLastPreinit] == nil:
          initFunc.sections[secLastPreinit] = g.section(initFunc, secPreinit)

        g.withFunc(initFunc):
          g.debugUpdateLoc(v)
          g.withBlock(g.section(initFunc, secLastPreinit)):
            # Avoid using the wrong exception handling context when initializing
            # globals..
            # TODO use the right exception handling context
            let nts = g.f.nestedTryStmts
            g.f.nestedTryStmts = @[]

            let bx = g.genAsgnNode(value, v.typ, tmp)
            g.genAssignment(tmp, bx, v.typ, v.assignCopy)
            initFunc.sections[secLastPreinit] = g.b.getInsertBlock()
            g.f.nestedTryStmts = nts
        LLValue()
      else:
        tmp
    else:
      let tmp = g.genLocal(vn)
      g.initLocalVar(v, v.typ, tmp.v, g.isAssignedImmediately(value))

      tmp

  if value.kind != nkEmpty and x.v != nil:
    let bx = g.genCallOrNode(vn, value, x)
    g.genAssignment(x, bx, v.typ, v.assignCopy)

proc genSingleVar(g: LLGen, a: PNode) =
  let v = a[0].sym
  if sfCompileTime in v.flags:
    if sfGlobal in v.flags and g.f.sym != nil and g.f.sym.kind == skProc:
      discard
    else:
      return
  genSingleVar(g, v, a[0], a[2])

proc genClosureVar(g: LLGen, n: PNode) =
  let x = g.genNode(n[0], false)

  if n[2].kind == nkEmpty:
    if sfNoInit notin n[0][1].sym.flags:
      g.buildStoreNull(g.llType(n[0].typ), x.v)
      g.genObjectInit(n[0].typ, x.v)
  else:
    let bx = g.genCallOrNode(n[0], n[2], x)
    g.genAssignment(x, bx, n[0].typ, {needToCopy})

proc genAsgn(g: LLGen, n: PNode, fast: bool) =
  if nfPreventCg in n.flags:
    return

  g.cow(n[1])

  if n[0].kind == nkSym and sfGoto in n[0].sym.flags:
    g.config.internalError(n.info, "Goto variables not supported")
  elif optFieldCheck in g.f.options and isDiscriminantField(n[0]):
    var dotExpr = n[0]
    if dotExpr.kind == nkCheckedFieldExpr:
      dotExpr = dotExpr[0]
    let
      ax = g.genNode(n[0], false)
      bx = g.genNode(n[1], true)

    if optTinyRtti notin g.config.globalOptions:
      # let field = dotExpr[1].sym
      # TODO genDiscriminantCheck(p, a, tmp, dotExpr[0].typ, field)
      message(g.config, n.info, warnCaseTransition)
    g.genAssignment(
      ax,
      bx,
      n[0].typ,
      if fast:
        {}
      else:
        {needToCopy},
    )
  else:
    let
      le = n[0]
      ri = n[1]
    let
      ax = g.genNode(le, false, prepareMutation = true)
      bx = g.genCallOrNode(le, ri, ax)

    # TODO the cgen uses a fast assignment when assigning the return value
    #      of a call - this code emulates that but we should really mark
    #      the LLValue instead so as to proparely propagate this fact from
    #      calls nested in no-ops etc
    g.genAssignment(
      ax,
      bx,
      le.typ,
      if fast or ri.kind in nkCallKinds:
        {}
      else:
        {needToCopy},
    )

proc genCallMagic(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  # Some magics have implementations in the std lib that we can use - make sure
  # it's been processed!
  discard g.genFunctionWithBody(n[namePos].sym).v

  g.genCall(nil, n, load, dest)

proc genMagicSlice(g: LLGen, n: PNode, load: bool): LLValue =
  let
    bx = g.genNode(n[2], true).v
    cx = g.genNode(n[3], true).v
    typ = n[1].typ.skipTypes(abstractVar + {tyPtr})

  var v, len: llvm.ValueRef
  case typ.kind
  of tyArray:
    let
      ax = g.genNode(n[1], true)
      # TODO Int128
      first =
        constInt(bx.typeOfX(), g.config.firstOrd(typ).toInt64.culonglong, llvm.True)
      idx = g.b.buildSub(bx, first, g.nn("slice.subb", bx))
      ty = g.llType(typ.elemType)
    if optBoundsCheck in g.f.options:
      let last =
        constInt(bx.typeOfX(), g.config.lastOrd(typ).toInt64.culonglong, llvm.True)

      g.genBoundsCheckArray(ax.v, first, last, bx, cx)
    v = g.b.buildInboundsGEP2(ty, ax, [idx], g.nn("sa", n)).v
  of tyOpenArray, tyVarargs:
    let
      ax = g.buildLoadVar(n[1].typ, g.genNode(n[1], false).v)
      ty = g.llType(typ.elemType)
    if optBoundsCheck in g.f.options:
      let tot = g.b.buildLoad2(g.intTy, g.buildOpenArrayLenGEP(ax))
      g.genBoundsCheck(ax, tot, bx, cx)

    v = g.b.buildLoad2(g.ptrTy, g.buildOpenArrayDataGEP(ax))
    v = g.b.buildInboundsGEP2(ty, v, [bx], g.nn("oa", n))
  of tyCString:
    let ax = g.genNode(n[1], true)
    v = g.b.buildInboundsGEP2(g.primitives[tyChar], ax, [bx], g.nn("slice", n)).v
  of tyUncheckedArray:
    let ax = g.genNode(n[1], true)
    v = g.b.buildInboundsGEP2(g.llType(typ), ax, [g.gep0, bx], g.nn("slice", n)).v
  of tyString, tySequence:
    let
      prepareForMutation =
        optSeqDestructors in g.config.globalOptions and typ.kind == tyString and
        (n[1].kind == nkHiddenDeref or n.typ.skipTypes(abstractInst).kind == tyVar)
      axp = g.genNode(n[1], not prepareForMutation)
      ax =
        if prepareForMutation:
          discard g.callCompilerProc("nimPrepareStrMutationV2", [axp.v])
          g.buildLoadValue(g.llType(n[1].typ), axp)
        else:
          axp
      ty = g.llType(typ)
      seqTy = g.llSeqType(typ)
      a =
        if n.typ.skipTypes(abstractInst).kind in {tyVar, tyLent}:
          g.buildLoadValue(ty, ax)
        else:
          ax
    if optBoundsCheck in g.f.options:
      let tot = g.loadNimSeqLen(a.v)
      g.genBoundsCheck(a.v, tot, bx, cx)
    v = g.getNimSeqDataPtr(seqTy, a.v, bx)
  else:
    g.config.internalError(n.info, "unknown slice type " & $typ.kind)

  len = g.b.buildSub(cx, bx, g.nn("slice.sub", n))
  len = g.b.buildAdd(len, constInt(len.typeOfX(), 1, llvm.False), g.nn("slice.add", n))

  let tmp = g.localAlloca(g.llOpenArrayType(), g.nn("slice", n))
  discard g.b.buildStore(v, g.buildOpenArrayDataGEP(tmp))
  discard g.b.buildStore(len, g.buildOpenArrayLenGEP(tmp))

  g.maybeLoadValue(g.llOpenArrayType(), LLValue(v: tmp, lode: n[1]), load)

proc genMagicHigh(g: LLGen, n: PNode): LLValue =
  let len = g.genMagicLength(n).v
  LLValue(
    v: g.b.buildSub(len, llvm.constInt(len.typeOfX(), 1, llvm.False), g.nn("high", len))
  )

proc genMagicSizeOf(g: LLGen, n: PNode): LLValue =
  let
    t = n[1].typ.skipTypes({tyTypeDesc})
    dl = g.m.getModuleDataLayout()

  LLValue(v: g.constNimInt(dl.aBISizeOfType(g.llType(t)).int))

proc genMagicAlignOf(g: LLGen, n: PNode): LLValue =
  let
    t = n[1].typ.skipTypes({tyTypeDesc})
    dl = g.m.getModuleDataLayout()

  LLValue(v: g.constNimInt(dl.aBIAlignmentOfType(g.llType(t)).int))

proc genMagicOf(g: LLGen, n: PNode): LLValue =
  ## `of` operator - true iff object is not nil and matches type
  var
    ax = g.genNode(n[1], false).v
    nilCheck: llvm.ValueRef
    typ = n[1].typ.skipTypes(abstractInstOwned)

  while typ.kind in {tyVar, tyLent, tyPtr, tyRef}:
    ax = g.buildLoadValue(g.llType(typ), ax)
    if typ.kind notin {tyVar, tyLent}:
      nilCheck = ax
    typ = typ.lastSon.skipTypes(typedescInst + {tyOwned})

  let
    destTyp = n[2].typ.skipTypes(typedescPtrs)
    v =
      if nilCheck == nil:
        g.callIsObj(ax, destTyp)
      else:
        g.withNotNilOrNull(nilCheck, g.primitives[tyBool]):
          g.callIsObj(ax, destTyp)

  LLValue(v: v)

proc genMagicEcho(g: LLGen, n: PNode) =
  let b = n[1].skipConv

  if b.len == 0:
    let ty = g.ptrTy # g.llStringType().pointerType().pointerType()
    discard g.callCompilerProc("echoBinSafe", [constNull(ty), g.ni0])
  else:
    let x = g.genNode(b, true).v
    let y = g.constNimInt(b.len)
    discard g.callCompilerProc("echoBinSafe", [x, y])

proc genMagicIncDec(g: LLGen, n: PNode, op: Opcode) =
  let
    ax = g.genNode(n[1], false).v
    bx = g.genNode(n[2], true).v
    a = g.b.buildLoad2(g.llType(n[1].typ), ax)
    b = g.buildTruncOrExt(bx, a.typeOfX(), n[2].typ)

  let t = n[1].typ.skipTypes({tyGenericInst, tyAlias, tySink, tyVar, tyLent, tyRange})
  if optOverflowCheck notin g.f.options or t.kind in {tyUInt .. tyUInt64}:
    let nv = g.b.buildBinOp(op, a, b, g.nn("inc.nv", n))
    discard g.b.buildStore(nv, ax)
  else:
    let nv = g.callBinOpWithOver(a, b, op, n[1].typ)
    discard g.b.buildStore(nv, ax)

proc genMagicOrd(g: LLGen, n: PNode): LLValue =
  var ax = g.genNode(n[1], true).v

  if n[1].typ.skipTypes(abstractRange).kind == tyBool:
    # make sure we don't get more bits set than necessary
    ax = g.b.buildAnd(
      ax, llvm.constInt(ax.typeOfX(), 1.cuint, llvm.False), g.nn("ord.bool", n)
    )
  LLValue(v: g.buildTruncOrExt(ax, g.llType(n.typ), n[1].typ))

proc rawGenNew(g: LLGen, dest: LLValue, sizeExpr: llvm.ValueRef, typ: PType) =
  let
    refType = typ.skipTypes(abstractInst)
    bt = refType.lastSon
    lbt = g.llType(bt)
    dl = g.m.getModuleDataLayout()
    sizeExpr =
      if sizeExpr == nil:
        g.constNimInt(g.getBestSize(bt, lbt))
      else:
        sizeExpr

  assert refType.kind == tyRef

  if optTinyRtti in g.config.globalOptions:
    let
      alignExpr = g.constNimInt(dl.aBIAlignmentOfType(lbt).int)
      src = g.callCompilerProc("nimNewObj", [sizeExpr, alignExpr])
    g.genRefAssign(dest, src)
  else:
    let ti = g.genTypeInfo(typ)
    if dest.storage == OnHeap and usesWriteBarrier(g.config):
      let destl = g.buildLoadValue(g.llType(refType), dest)
      g.withNotNil(destl.v):
        if g.graph.canFormAcycle(typ):
          discard g.callCompilerProc("nimGCunrefRC1", [destl.v])
        else:
          discard g.callCompilerProc("nimGCunrefNoCycle", [destl.v])
        discard g.b.buildStore(constNull(g.llType(refType)), dest.v)

      if g.config.selectedGC == gcGo:
        # newObjRC1() would clash with unsureAsgnRef() - which is used by gcGo to
        # implement the write barrier
        let src = g.callCompilerProc("newObj", [ti, sizeExpr])
        discard g.callCompilerProc("unsureAsgnRef", [dest.v, src])
      else:
        # use newObjRC1 as an optimization
        let src = g.callCompilerProc("newObjRC1", [ti, sizeExpr])
        discard g.b.buildStore(src, dest.v)
    else:
      let src = g.callCompilerProc("newObj", [ti, sizeExpr])
      g.genRefAssign(dest, src)

  g.genObjectInit(bt, g.b.buildLoad2(g.ptrTy, dest.v)) # lbt.pointerType

proc genMagicNew(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    sx =
      if n.len == 3:
        g.genNode(n[2], true).v
      else:
        nil

  g.rawGenNew(ax, sx, n[1].typ)

proc genMagicNewFinalize(g: LLGen, n: PNode) =
  if optTinyRtti in g.config.globalOptions:
    let dest = g.genNode(n[1], false)
    g.rawGenNew(dest, nil, n[1].typ)
    return

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
  var newInit = newSeq[llvm.ValueRef](init.getNumOperands())

  for i in 0 ..< newInit.len:
    if i == 6:
      newInit[i] = f
    else:
      newInit[i] = init.getOperand(cuint i)

  ti.setInitializer(constNamedStruct(init.typeOfX(), newInit))

  let src = g.callCompilerProc("newObj", [ti, sizeOfExpr])
  g.genRefAssign(dest, src)
  g.genObjectInit(bt, g.b.buildLoad2(g.llType(refType), dest.v))

proc genNewSeqAux(g: LLGen, dest: LLValue, destTyp: PType, len: llvm.ValueRef) =
  let
    seqType = destTyp.skipTypes(abstractVarRange)
    ti = g.genTypeInfo(seqType)
    args = [ti, len]

  if dest.storage == OnHeap and usesWriteBarrier(g.config):
    let destl = g.buildLoadValue(g.llType(destTyp), dest)
    g.withNotNil(destl.v):
      if g.graph.canFormAcycle(destTyp):
        discard g.callCompilerProc("nimGCunrefRC1", [destl.v])
      else:
        discard g.callCompilerProc("nimGCunrefNoCycle", [destl.v])
      discard g.b.buildStore(constNull(g.llType(destTyp)), dest.v)

    if not len.isZero():
      if g.config.selectedGC == gcGo:
        # we need the write barrier
        let src = g.callCompilerProc("newSeq", args)
        discard g.callCompilerProc("unsureAsgnRef", [dest.v, src])
      else:
        let src = g.callCompilerProc("newSeqRC1", args)
        discard g.b.buildStore(src, dest.v)
  else:
    assert optSeqDestructors notin g.config.globalOptions
    let src =
      if len.isZero():
        llvm.constNull(g.llType(seqType))
      else:
        g.callCompilerProc("newSeq", args)
    g.genRefAssign(dest, src)

proc makePtrType(baseType: PType, idgen: IdGenerator): PType =
  result = newType(tyPtr, nextTypeId idgen, baseType.owner)
  addSonSkipIntLit(result, baseType, idgen)

proc makeAddr(n: PNode, idgen: IdGenerator): PNode =
  if n.kind == nkHiddenAddr:
    result = n
  else:
    result = newTree(nkHiddenAddr, n)
    result.typ = makePtrType(n.typ, idgen)

proc genMagicNewSeq(g: LLGen, n: PNode) =
  if optSeqDestructors in g.config.globalOptions:
    # cgen _actually_ modifies the ast :facepalm:
    n[1] = makeAddr(n[1], g.idgen)
    discard g.genCall(nil, n, false, LLValue())
  else:
    let
      ax = g.genNode(n[1], false)
      bx = g.genNode(n[2], true).v

    g.genNewSeqAux(ax, n[1].typ, bx)

proc genMagicNewSeqOfCap(g: LLGen, n: PNode): LLValue =
  let
    seqtype = n.typ.skipTypes(abstractVarRange)
    ty = g.llType(seqtype)
    ax = g.genNode(n[1], true).v
    v =
      if optSeqDestructors in g.config.globalOptions:
        let
          tmp = g.localAlloca(ty, g.nn("nsoc", n))
          elemTy = g.llType(seqtype.elemType)
          dl = g.m.getModuleDataLayout()

        let s = g.callCompilerProc(
          "newSeqPayload",
          [
            ax,
            g.constNimInt(int dl.aBISizeOfType(elemTy)),
            g.constNimInt(int dl.aBIAlignmentOfType(elemTy)),
          ],
        )
        discard g.b.buildStore(g.constNimInt(0), tmp) # length
        discard g.b.buildStore(
          s, g.b.buildInboundsGEP2(ty, tmp, [g.gep0, g.gep1], g.nn("payload", n))
        )
        g.b.buildLoad2(ty, tmp)
      else:
        let ti = g.genTypeInfo(seqtype)
        g.callCompilerProc("nimNewSeqOfCap", [ti, ax])

  LLValue(v: v, storage: OnHeap)

proc genMagicLengthOpenArray(g: LLGen, n: PNode): LLValue =
  let ax = g.buildLoadVar(n[1].typ, g.genNode(n[1], false).v)
  LLValue(v: g.b.buildLoad2(g.intTy, g.buildOpenArrayLenGEP(ax)))

proc genMagicLengthStr(g: LLGen, n: PNode): LLValue =
  let
    v = g.genNode(n[1], true).v
    pre = g.b.getInsertBlock()
    lload = g.b.appendBasicBlockInContext(g.lc, g.nn("str.len.load", n))
    ldone = g.b.appendBasicBlockInContext(g.lc, g.nn("str.len.done", n))

  # nil check
  let cond =
    g.b.buildICmp(llvm.IntEQ, v, llvm.constNull(v.typeOfX()), g.nn("str.len.isnil", n))

  discard g.b.buildCondBr(cond, ldone, lload)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(lload)
  let
    fty = llvm.functionType(g.csizetTy, [g.primitives[tyCString]])
    f = g.m.getOrInsertFunction("strlen", fty)

  let v1 = g.buildTruncOrExt(
    g.b.buildCall2(fty, f, [v], g.nn("str.len.call", n)), g.primitives[tyInt], false
  )
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
  of tyOpenArray, tyVarargs:
    g.genMagicLengthOpenArray(n)
  of tyCString:
    g.genMagicLengthStr(n)
  of tySequence, tyString:
    g.genMagicLengthSeq(n)
  of tyArray:
    LLValue(v: g.constNimInt(g.config.lengthOrd(typ)))
  else:
    g.config.internalError(n.info, "genMagicLength " & $n[1].typ)
    LLValue()

proc genMagicIncl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false).v
    bx = g.genNode(n[2], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)
    ty = g.llType(typ)

  if size <= 8:
    let
      mask = g.buildSetMask(ty, g.setElemIndex(typ, bx), size)
      a = g.b.buildLoad2(ty, ax)
      res = g.b.buildOr(a, mask, g.nn("masked", a))
    discard g.b.buildStore(res, ax)
  else:
    let
      (gep, mask) = g.buildSetGEPMask(ty, ax, g.setElemIndex(typ, bx))
      a = g.b.buildLoad2(ty.getElementType(), gep)
      res = g.b.buildOr(a, mask, g.nn("masked", a))
    discard g.b.buildStore(res, gep)

proc genMagicExcl(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false).v
    bx = g.genNode(n[2], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)
    ty = g.llType(typ)

  if size <= 8:
    let
      mask = g.buildSetMask(ty, g.setElemIndex(typ, bx), size)
      a = g.b.buildLoad2(ty, ax)
      res = g.b.buildAnd(a, g.buildBitnot(mask), g.nn("masked", a))
    discard g.b.buildStore(res, ax)
  else:
    let
      (gep, mask) = g.buildSetGEPMask(ty, ax, g.setElemIndex(typ, bx))
      a = g.b.buildLoad2(ty.getElementType(), gep)
      res = g.b.buildAnd(a, g.buildBitnot(mask), g.nn("masked", a))
    discard g.b.buildStore(res, gep)

proc genMagicCard(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[1], true).v
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)
    ty = g.llType(typ)
    cardTy = g.llType(n.typ)

  if size <= 8:
    let ctp = g.callCtpop(ax, size)
    LLValue(v: g.buildTruncOrExt(ctp, cardTy, true))
  else:
    let tot = g.localAlloca(cardTy, g.nn("card.tot", n))
    g.buildStoreNull(cardTy, tot)
    let size = g.constNimInt(size.int)
    g.withLoop(size, "card"):
      let
        ai = g.b.buildLoad2(
          ty.getElementType(),
          g.b.buildInboundsGEP2(ty.getElementType(), ax, [i], g.nn("card.i", i)),
        )
        a = g.callCtpop(ai, 1)
        b = g.b.buildLoad2(cardTy, tot)
        c =
          g.b.buildAdd(g.buildTruncOrExt(a, b.typeOfX(), true), b, g.nn("card.add", n))
      discard g.b.buildStore(c, tot)

    LLValue(v: g.b.buildLoad2(cardTy, tot))

proc genMagicChr(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true).v

  let t = g.llType(n.typ)
  LLValue(
    v:
      if t != ax.typeOfX():
        g.b.buildTrunc(ax, t, g.nn("chr.t", n))
      else:
        ax
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
      notZero = g.b.buildFCmp(
        llvm.RealUNE, v, constReal(v.typeOfX(), cdouble(0)), g.nn("iszero", n)
      )
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
  LLValue(
    v: g.buildI8(
      g.b.buildICmp(
        op,
        g.preCast(false, ax, n[1].typ),
        g.preCast(false, bx, n[1].typ),
        g.nn("icmp." & $op, n),
      )
    )
  )

proc genMagicCmpF(g: LLGen, n: PNode, op: RealPredicate): LLValue =
  let
    ax = g.genNode(n[1], true).v
    bx = g.genNode(n[2], true).v

  LLValue(v: g.buildI8(g.b.buildFCmp(op, ax, bx, g.nn($op, n))))

proc genMagicEqCString(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  g.genCallMagic(n, load, dest) # Implemented in std lib

proc genMagicEqProc(g: LLGen, n: PNode): LLValue =
  let v =
    if n[1].typ.skipTypes(abstractInst).callConv == ccClosure:
      let
        ax = g.genNode(n[1], false).v
        bx = g.genNode(n[2], false).v
        a0 = g.buildClosurePrcGEP(ax)
        a1 = g.buildClosureEnvGEP(ax)
        b0 = g.buildClosurePrcGEP(bx)
        b1 = g.buildClosureEnvGEP(bx)

      let x0 = g.b.buildICmp(
        llvm.IntEQ,
        g.b.buildLoad2(g.ptrTy, a0),
        g.b.buildLoad2(g.ptrTy, b0),
        g.nn("eq.prc.0", n),
      )
      let x1 = g.b.buildICmp(
        llvm.IntEQ,
        g.b.buildLoad2(g.ptrTy, a1),
        g.b.buildLoad2(g.ptrTy, b1),
        g.nn("eq.prc.1", n),
      )

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

  LLValue(
    v:
      if optOverflowCheck in g.f.options:
        g.callBinOpWithOver(bx, ax, llvm.Sub, n.typ)
      else:
        g.b.buildSub(bx, ax, g.nn("neg", ax))
  )

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
      else:
        g.b.buildSub(bx, ax, g.nn("abs.neg", n))
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
  LLValue(v: g.b.buildFNeg(ax, g.nn("fneg", ax)))

proc genMagicToStr(g: LLGen, n: PNode, f: string): LLValue =
  let a = g.genNode(n[1], true).v
  LLValue(v: g.callCompilerProc(f, [a]))

proc genMagicStrToStr(g: LLGen, n: PNode): LLValue =
  g.genNode(n[1], true)

proc genEnumToStr(g: LLGen, n: PNode): LLValue =
  let t = n[1].typ.skipTypes(abstractInst + {tyRange})
  let toStrProc = getToStringProc(g.module.g.graph, t)
  # XXX need to modify this logic for IC.
  var n = copyTree(n)
  n[0] = newSymNode(toStrProc)
  g.genNode(n, true)

proc genMagicEnumToStr(g: LLGen, n: PNode): LLValue =
  if optTinyRtti in g.config.globalOptions:
    g.genEnumToStr(n)
  else:
    let
      ax = g.genNode(n[1], true).v
      typ = skipTypes(n[1].typ, abstractVarRange)
    LLValue(
      v: g.callCompilerProc(
        "reprEnum",
        [g.buildTruncOrExt(ax, g.primitives[tyInt], typ), g.genTypeInfo(typ)],
      )
    )

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

  LLValue(v: phi)

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
    let
      b = g.buildBitnot(bx)
      ab = g.b.buildAnd(ax, b, g.nn("setcmp.ab"))
      le = g.b.buildICmp(
        llvm.IntEQ, ab, llvm.constInt(ab.typeOfX(), 0, llvm.False), g.nn("setcmp.le", n)
      )
      v =
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
    let il = g.b.buildLoad2(i.getAllocatedType(), i)
    let cond =
      g.b.buildICmp(llvm.IntSLT, il, g.constNimInt(size.int), g.nn("setcmp.isdone", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let
      ty = g.primitives[tyUInt8]
      al = g.b.buildLoad2(ty, g.b.buildInboundsGEP2(ty, ax, [il], g.nn("setcmp.al", n)))
      bl = g.b.buildLoad2(ty, g.b.buildInboundsGEP2(ty, bx, [il], g.nn("setcmp.bl", n)))
      b = g.buildBitnot(bl)

    var cont: llvm.ValueRef
    let
      ab = g.b.buildAnd(al, b, g.nn("setcmp.ab", n))
      le = g.b.buildICmp(
        llvm.IntEQ, ab, llvm.constInt(ab.typeOfX(), 0, llvm.False), g.nn("setcmp.eq", n)
      )
    if strict:
      let ne = g.b.buildICmp(llvm.IntNE, ax, bx, g.nn("setcmp.ne"))
      cont = g.b.buildAnd(le, ne, g.nn("setcmp.cont", n))
    else:
      cont = le

    discard g.b.buildCondBr(cont, rinc, rfalse)

    g.b.positionBuilderAtEnd(rinc)

    # inc idx
    let next =
      g.b.buildAdd(il, constInt(il.typeOfX(), 1, llvm.False), g.nn("setcmp.inc", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    g.b.positionBuilderAtEnd(rfalse)
    discard g.b.buildStore(g.nb[false], o)
    discard g.b.buildBr(rdone)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    LLValue(v: g.buildLoadValue(o.getAllocatedType(), o))

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
    let il = g.b.buildLoad2(i.getAllocatedType(), i)
    let cond =
      g.b.buildICmp(llvm.IntSLT, il, g.constNimInt(size.int), g.nn("seteq.cond", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let
      ty = g.primitives[tyUInt8]
      a = g.b.buildLoad2(ty, g.b.buildInboundsGEP2(ty, ax, [il], g.nn("seteq.a", n)))
      b = g.b.buildLoad2(ty, g.b.buildInboundsGEP2(ty, bx, [il], g.nn("seteq.b", n)))
      cmp = g.b.buildICmp(llvm.IntEQ, a, b, g.nn("seteq.cmp", n))
    discard g.b.buildCondBr(cmp, rinc, rne)
    g.b.positionBuilderAtEnd(rne)

    discard g.b.buildBr(rdone)

    # inc idx
    g.b.positionBuilderAtEnd(rinc)
    let next =
      g.b.buildAdd(il, constInt(il.typeOfX(), 1, llvm.False), g.nn("set.inc", n))
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
    let s =
      if invert:
        g.buildBitnot(b)
      else:
        b
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
    let il = g.b.buildLoad2(i.getAllocatedType(), i)
    let cond =
      g.b.buildICmp(llvm.IntSLT, il, g.constNimInt(size.int), g.nn("setbo.cond", n))
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let
      ty = g.primitives[tyUInt8]
      a = g.b.buildLoad2(ty, g.b.buildInboundsGEP2(ty, ax, [il], g.nn("setbo.al", n)))
      b = g.b.buildLoad2(ty, g.b.buildInboundsGEP2(ty, bx, [il], g.nn("setbo.bl", n)))
      s =
        if invert:
          g.buildBitnot(b)
        else:
          b
      o = g.b.buildBinOp(op, a, s, g.nn("setbo.op", n))
      p = g.b.buildInboundsGEP2(
        tmp.getAllocatedType(), tmp, [g.gep0, il], g.nn("setbo.p", n)
      )

    discard g.b.buildStore(o, p)

    # inc idx
    let next =
      g.b.buildAdd(il, constInt(il.typeOfX(), 1, llvm.False), g.nn("setbo.next", n))
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    LLValue(v: g.buildLoadValue(tmp.getAllocatedType(), tmp))

proc genMagicConStrStr(g: LLGen, n: PNode): LLValue =
  var tgtlen = g.ni0

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]

  for s in n.sons[1 ..< n.len]:
    let sx = g.genNode(s, true).v
    exprs.add(sx)

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit .. nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.loadNimSeqLen(sx)
      tgtlen = g.b.buildAdd(tgtlen, slen, g.nn("constrstr.tgtlen", n))

  if constlen > 0:
    tgtlen = g.b.buildAdd(tgtlen, g.constNimInt(constlen), g.nn("constrstr.tgtlen", n))

  # Allocate result
  let
    tgt = g.callCompilerProc("rawNewString", [tgtlen])
    arg =
      if optSeqDestructors in g.config.globalOptions:
        let tmp = g.localAlloca(tgt.typeOfX(), g.nn("tmp", n))
        discard g.b.buildStore(tgt, tmp)
        tmp
      else:
        tgt

  # Copy data
  for i in 1 ..< n.len:
    let
      sx = exprs[i - 1]
      s = n[i]

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [arg, sx])
    else:
      discard g.callCompilerProc("appendString", [arg, sx])

  let res =
    if optSeqDestructors in g.config.globalOptions:
      g.b.buildLoad2(tgt.typeOfX(), arg)
    else:
      arg

  LLValue(v: res, lode: n, storage: OnHeap)

proc genMagicDotDot(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  g.genCallMagic(n, load, dest) # Implemented in std lib

proc genMagicAppendStrCh(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true).v

  if optSeqDestructors in g.config.globalOptions:
    discard g.callCompilerProc("nimAddCharV1", [ax.v, bx])
  else:
    let
      a = g.buildLoadValue(g.llType(n[1].typ), ax)
      ret = g.callCompilerProc("addChar", [a.v, bx])

    g.genRefAssign(ax, ret)

proc genMagicAppendStrStr(g: LLGen, n: PNode) =
  let
    tgtp = g.genNode(n[1], false)
    tgt = g.b.buildLoad2(g.llType(n[1].typ), tgtp.v)

  # First, find out total length of the new strings
  var tgtlen = g.ni0

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]
  for i in 2 ..< n.len:
    let s = n[i]
    let sx = g.genNode(s, true).v
    exprs.add(sx)

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit .. nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.loadNimSeqLen(sx)
      tgtlen = g.b.buildAdd(tgtlen, slen, g.nn("str.adds.tot." & $i, n))

  if constlen > 0:
    tgtlen =
      g.b.buildAdd(tgtlen, g.constNimInt(constlen), g.nn("str.adds.tot.const", n))

  # Make room
  let dest =
    if optSeqDestructors in g.config.globalOptions:
      discard g.callCompilerProc("prepareAdd", [tgtp.v, tgtlen])
      tgtp.v
    else:
      let newstr = g.callCompilerProc("resizeString", [tgt, tgtlen])
      g.genRefAssign(tgtp, newstr)
      newstr

  # Copy data
  for i in 2 ..< n.len:
    let sx = exprs[i - 2]

    let s = n[i]
    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [dest, sx])
    else:
      discard g.callCompilerProc("appendString", [dest, sx])

proc genMagicAppendSeqElem(g: LLGen, n: PNode) =
  if optSeqDestructors in g.config.globalOptions:
    n[1] = makeAddr(n[1], g.idgen)
    discard g.genCall(nil, n, false, LLValue())
    return

  let
    ax = g.genNode(n[1], false)
    seqTyp = n[1].typ.skipTypes({tyVar, tyLent})
    ty = g.llType(seqTyp)
    seqTy = g.llSeqType(seqTyp)
    # Evaluate n[2] before calling incrSeqV3 as it will reset the length of the
    # old instance when it grows!
    bx = g.genNode(n[2], g.loadAssignment(seqTyp.elemType))
    a = g.b.buildLoad2(ty, ax.v)
    newseq = g.callCompilerProc("incrSeqV3", [a, g.genTypeInfo(seqTyp)])
    tgt = LLValue(v: newseq, storage: OnHeap)
    lenp = g.buildNimSeqLenGEP(tgt.v) # guaranteed not nil!
    len = g.b.buildLoad2(g.primitives[tyInt], lenp)

  g.genRefAssign(ax, tgt.v)

  let newlen = g.b.buildAdd(
    len, llvm.constInt(len.typeOfX(), 1, llvm.False), g.nn("seq.add.newlen", n)
  )
  discard g.b.buildStore(newlen, lenp)

  g.genAssignment(
    g.buildNimSeqDataGEP(seqTy, tgt, len), bx, seqTyp.elemType, {needToCopy}
  )

# Here, we need to emulate the C compiler and generate comparisons instead of
# sets, else we'll have crashes when out-of-range ints are compared against
# curlies

# from C compiler
proc fewCmps(g: LLGen, s: PNode): bool =
  # this function estimates whether it is better to emit code
  # for constructing the set or generating a bunch of comparisons directly
  if s.kind != nkCurly:
    g.config.internalError(s.info, "fewCmps")
  if (g.config.getSize(s.typ) <= g.config.target.intSize) and (nfAllConst in s.flags):
    false # it is better to emit the set generation code
  elif elemType(s.typ).kind in {tyInt, tyInt16 .. tyInt64}:
    true # better not emit the set if int is basetype!
  else:
    s.len <= 8 # 8 seems to be a good value

proc genMagicInSet(g: LLGen, n: PNode): LLValue =
  if (n[1].kind == nkCurly) and g.fewCmps(n[1]):
    let s =
      if n[2].kind in {nkChckRange, nkChckRange64}:
        n[2][0]
      else:
        n[2]
    let sx = g.genNode(s, true).v

    let res = g.localAlloca(g.primitives[tyBool], g.nn("inset.res", n))
    discard g.b.buildStore(g.nb[false], res)

    let strue = g.b.appendBasicBlockInContext(g.lc, g.nn("inset.true", n))

    let length = n[1].len
    let u = g.isUnsigned(s.typ)
    for i in 0 ..< length:
      let cmp =
        if n[1][i].kind == nkRange:
          let
            ax = g.genNode(n[1][i][0], true).v
            bx = g.genNode(n[1][i][1], true).v
            acmp = g.b.buildICmp(
              if u: llvm.IntUGE else: llvm.IntSGE,
              sx,
              g.buildTruncOrExt(ax, sx.typeOfX(), u),
              g.nn("inset.acmp", n),
            )
            bcmp = g.b.buildICmp(
              if u: llvm.IntULE else: llvm.IntSLE,
              sx,
              g.buildTruncOrExt(bx, sx.typeOfX(), u),
              g.nn("inset.bcmp", n),
            )

          g.b.buildAnd(acmp, bcmp, g.nn("inset.and", n))
        else:
          let ax = g.genNode(n[1][i], true).v
          g.b.buildICmp(
            llvm.IntEQ, sx, g.buildTruncOrExt(ax, sx.typeOfX(), u), g.nn("inset.cmp", n)
          )

      let snext = g.b.appendBasicBlockInContext(g.lc, g.nn("inset.next", n))
      discard g.b.buildCondBr(cmp, strue, snext)
      g.b.positionBuilderAtEnd(snext)

    let send = g.b.appendBasicBlockInContext(g.lc, g.nn("inset.end", n))
    discard g.b.buildBr(send)

    g.b.positionBuilderAtEnd(strue)
    discard g.b.buildStore(g.nb[true], res)
    discard g.b.buildBr(send)

    g.b.positionAndMoveToEnd(send)
    return LLValue(v: g.b.buildLoad2(res.getAllocatedType(), res))

  let
    typ = skipTypes(n[1].typ, abstractVar)
    size = g.config.getSize(typ)
    ty = g.llType(typ)
    masked =
      if size <= 8:
        let
          ax = g.genNode(n[1], true).v
          bx = g.genNode(n[2], true).v
          mask = g.buildSetMask(ty, g.setElemIndex(typ, bx), size)
        g.b.buildAnd(ax, mask, g.nn("inset.masked", n))
      else:
        let
          ax = g.genNode(n[1], false).v
          bx = g.genNode(n[2], true).v
          (gep, mask) = g.buildSetGEPMask(ty, ax, g.setElemIndex(typ, bx))
          a = g.b.buildLoad2(ty.getElementType(), gep)
        g.b.buildAnd(a, mask, g.nn("inset.masked", n))

    v = g.b.buildICmp(
      llvm.IntNE, masked, constNull(masked.typeOfX()), g.nn("inset.result", n)
    )

  LLValue(v: g.buildI8(v))

proc genMagicRepr(g: LLGen, n: PNode): LLValue =
  let t = skipTypes(n[1].typ, abstractVarRange)
  let v =
    case t.kind
    of tyInt .. tyInt64, tyUInt .. tyUInt64:
      let ax = g.genNode(n[1], true).v
      g.callCompilerProc("reprInt", [g.buildTruncOrExt(ax, g.primitives[tyInt], t)])
    of tyFloat .. tyFloat128:
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
      let
        ax = g.buildLoadVar(n[1].typ, g.genNode(n[1], false).v)
        data = g.b.buildLoad2(g.ptrTy, g.buildOpenArrayDataGEP(ax))
        len = g.b.buildLoad2(g.intTy, g.buildOpenArrayLenGEP(ax))
      g.callCompilerProc("reprOpenArray", [data, len, g.genTypeInfo(t.elemType)])
    of tyCString, tyArray, tyUncheckedArray, tyRef, tyPtr, tyPointer, tyNil, tySequence:
      let ax = g.genNode(
        n[1], t.kind in {tyCString, tyRef, tyPtr, tyVar, tyPointer, tyLent, tySequence}
      ).v
      g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])
    of tyEmpty, tyVoid:
      g.config.localError(n.info, "'repr' doesn't support 'void' type")
      nil
    else:
      let ax = g.genNode(n[1], false).v
      g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])
  LLValue(v: v)

proc genMagicSetLengthStr(g: LLGen, n: PNode) =
  if optSeqDestructors in g.config.globalOptions:
    let
      p0 = g.genNode(n[1], false).v
      p1 = g.genNode(n[2], true).v
    discard g.callCompilerProc("setLengthStrV2", [p0, p1])
    return

  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], true).v
    a = g.buildLoadValue(g.llType(n[1].typ), ax)
    newstr = g.callCompilerProc("setLengthStr", [a.v, bx])

  g.genRefAssign(ax, newstr)

proc genMagicSetLengthSeq(g: LLGen, n: PNode) =
  if optSeqDestructors in g.config.globalOptions:
    n[1] = makeAddr(n[1], g.idgen)
    discard g.genCall(nil, n, false, LLValue())
    return

  let
    axn =
      if n[1].kind in {nkAddr, nkHiddenAddr}:
        n[1][0]
      else:
        n[1]
    ax = g.genNode(axn, false)
    bx = g.genNode(n[2], true).v
    typ = n[1].typ.skipTypes({tyVar, tyLent} + abstractInst)
    ty = g.llType(typ)
    a =
      if optSeqDestructors in g.config.globalOptions:
        ax.v
      else:
        g.b.buildLoad2(ty, ax.v)
    x = g.callCompilerProc("setLengthSeqV2", [a, g.genTypeInfo(typ), bx])

  g.genRefAssign(ax, x)

proc genMagicParallel(g: LLGen, n: PNode) =
  let n2 = g.graph.liftParallel(g.idgen, g.module.sym, n)
  g.genNode(n2)

proc genMagicSwap(g: LLGen, n: PNode) =
  let
    ax = g.genNode(n[1], false)
    bx = g.genNode(n[2], false)
    lx = g.loadAssignment(n[1].typ)
    ty = g.llType(n[1].typ)
    tmpx = LLValue(v: g.localAlloca(ty, g.nn("swap.tmp", n)), storage: OnStack)
  g.cowBracket(n[1])
  g.cowBracket(n[2])

  g.buildStoreNull(ty, tmpx.v)
  g.genObjectInit(n[1].typ, tmpx.v)
  g.genAssignment(tmpx, g.maybeLoadValue(ty, ax, lx), n[1].typ, {})
  g.genAssignment(ax, g.maybeLoadValue(ty, bx, lx), n[1].typ, {})
  g.genAssignment(bx, g.maybeLoadValue(ty, tmpx, lx), n[1].typ, {})

proc skipAddr(n: PNode): PNode =
  if n.kind in {nkAddr, nkHiddenAddr}:
    n[0]
  else:
    n

proc genMagicMove(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[1], false)
    lx = g.loadAssignment(n[1].typ)
    ty = g.llType(n[1].typ)

  if n.len == 4:
    let
      axp = g.b.buildInboundsGEP2(ty, ax, [g.gep0, g.gep1], g.nn("move.a", n))
      ap = g.b.buildLoad2(g.ptrTy, axp.v)

      src = g.genNode(n[2], false)
      srcp = g.b.buildInboundsGEP2(ty, src, [g.gep0, g.gep1], g.nn("move.b", n))
      sp = g.b.buildLoad2(g.ptrTy, srcp.v)

      cmp = g.b.buildICmp(llvm.IntEQ, ap, sp, g.nn("cmp", n))
      moveit = g.b.appendBasicBlockInContext(g.lc, g.nn("move.do", n))
      done = g.b.appendBasicBlockInContext(g.lc, g.nn("nilcheck.done", n))

    discard g.b.buildCondBr(cmp, done, moveit)

    g.b.positionBuilderAtEnd(moveit)

    discard g.genNode(n[3], false)

    discard g.b.buildBr(done)

    g.b.positionBuilderAtEnd(done)

    let srcl = g.b.buildLoad2(ty, src.v)
    discard g.b.buildStore(srcl, ax.v)
    LLValue() # TODO ?
  else:
    let tmpx = LLValue(v: g.localAlloca(ty, g.nn("move.tmp", n[1])), storage: OnStack)

    g.buildStoreNull(ty, tmpx.v)
    g.genObjectInit(n[1].typ, tmpx.v)
    g.genAssignment(tmpx, g.maybeLoadValue(ty, ax, lx), n[1].typ, {})
    g.callReset(n[1].skipAddr.typ, ax)
    g.maybeLoadValue(ty, tmpx, load)

proc genMagicDestroy(g: LLGen, n: PNode) =
  if optSeqDestructors in g.config.globalOptions:
    let
      arg = n[1].skipAddr
      t = arg.typ.skipTypes(abstractInst)
    case t.kind
    of tyString, tySequence:
      let
        a = g.genNode(arg, true)
        payload = g.b.buildExtractValue(a, 1, g.nn("payload", a))
      g.withNotNil(payload.v):
        # if $1.p->cap & NIM_STRLIT_FLAG
        let
          cap = g.b.buildLoad2(g.intTy, payload.v, g.nn("cap", a))
          strlit = g.b.buildAnd(cap, g.constNimInt(g.strLitFlag), g.nn("sl", a))
          cond = g.b.buildICmp(llvm.IntNE, strlit, constNull(g.intTy), g.nn("cond", a))
          islit = g.b.appendBasicBlockInContext(g.lc, g.nn("islit", a))
          done = g.b.appendBasicBlockInContext(g.lc, g.nn("done", a))
        discard g.b.buildCondBr(cond, done, islit)
        g.b.positionBuilderAtEnd(islit)

        if t.kind == tyString:
          if optThreads in g.config.globalOptions:
            discard g.callCompilerProc("deallocShared", [payload.v])
          else:
            discard g.callCompilerProc("dealloc", [payload.v])
        else:
          let
            elemTy = g.llType(t.lastSon)
            dl = g.m.getModuleDataLayout()
            align = dl.aBIAlignmentOfType(elemTy)
          discard
            g.callCompilerProc("alignedDealloc", [payload.v, g.constNimInt(align.int)])
        discard g.b.buildBr(done)
        g.b.positionBuilderAtEnd(done)
    else:
      discard "nothing to do"
  else:
    g.config.internalError("TODO: mDestroy")
    discard

proc genMagicAccessEnv(g: LLGen, n: PNode, load: bool): LLValue =
  let
    p = g.genNode(n[1], false)
    env = g.buildClosureEnvGEP(p.v)

  g.maybeLoadValue(g.ptrTy, LLValue(v: env), load)

proc genMagicWasMoved(g: LLGen, n: PNode) =
  g.callReset(n[1].skipAddr.typ, g.genNode(n[1], false))

proc genMagicDefault(g: LLGen, n: PNode, load: bool): LLValue =
  let
    typ = n.typ.skipTypes(abstractInst)
    ty = g.llType(typ)

  if typ.kind != tyObject or typ.isObjLackingTypeField():
    if typ.kind == tyArray or not load:
      let v = g.m.addGlobal(ty, g.nn(".default", n))

      v.setInitializer(llvm.constNull(ty))
      v.setGlobalConstant(llvm.True)
      v.setLinkage(llvm.PrivateLinkage)
      LLValue(v: v, lode: n, storage: OnStatic)
    else:
      LLValue(v: constNull(ty), lode: n, storage: OnStack)
  else:
    let v = g.localAlloca(ty, g.nn("default.tmp", n))
    g.buildStoreNull(ty, v)
    g.genObjectInit(n.typ, v)

    g.maybeLoadValue(ty, LLValue(v: v, lode: n, storage: OnStack), load)

proc genMagicReset(g: LLGen, n: PNode) =
  let ax = g.genNode(n[1], false)
  g.callReset(n[1].typ, ax)

proc genMagicIsNil(g: LLGen, n: PNode): LLValue =
  let ax = g.genNode(n[1], true)

  let typ = skipTypes(n[1].typ, abstractPtrs)
  let v =
    if typ.kind == tyProc and typ.callConv == ccClosure:
      g.b.buildICmp(
        llvm.IntEQ,
        g.b.buildExtractValue(ax, 0, g.nn("isnil.prc", n)).v,
        llvm.constNull(g.ptrTy),
        g.nn("isnil", n),
      )
    else:
      g.b.buildICmp(llvm.IntEQ, ax.v, llvm.constNull(ax.v.typeOfX()), g.nn("isnil", n))
  LLValue(v: g.buildI8(v))

proc genMagicArrToSeq(g: LLGen, n: PNode): LLValue =
  if n[1].kind == nkBracket:
    n[1].typ = n.typ
    return g.genNode(n[1], true)

  let
    ty = g.llType(n.typ)
    seqTy = g.llSeqType(n.typ)
    tmp = LLValue(v: g.localAlloca(ty, g.nn("arrtoseq", n)), storage: OnStack)
    l = g.config.lengthOrd(skipTypes(n[1].typ, abstractInst))
    elemTyp = n[1].typ.skipTypes(abstractInst).elemType()
    elemTy = g.llType(elemTyp)
    lx = g.loadAssignment(elemTyp)

  if optSeqDestructors in g.config.globalOptions:
    let dl = g.m.getModuleDataLayout()

    let s = g.callCompilerProc(
      "newSeqPayload",
      [
        g.constNimInt(l),
        g.constNimInt(int dl.aBISizeOfType(elemTy)),
        g.constNimInt(int dl.aBIAlignmentOfType(elemTy)),
      ],
    )
    discard g.b.buildStore(g.constNimInt(l), tmp.v)
    discard g.b.buildStore(
      s, g.b.buildInboundsGEP2(ty, tmp.v, [g.gep0, g.gep1], g.nn("payload", n))
    )
  else:
    g.buildStoreNull(ty, tmp.v)
    g.genNewSeqAux(tmp, n.typ, g.constNimInt(l))

  # In-flight values can be considered to live OnHeap
  let
    tmpl = g.buildLoadValue(tmp.v.getAllocatedType(), tmp)
    src = g.genNode(n[1], true)

  for i in 0 ..< l.toInt():
    let
      tgt = g.buildNimSeqDataGEP(seqTy, tmpl, g.constGEPIdx(i))
      srcg = g.b.buildInboundsGEP2(
        elemTy, src, [g.constGEPIdx(i)], g.nn("arrtoseq.gep" & $i, n)
      )
    g.genAssignment(tgt, g.maybeLoadValue(elemTy, srcg, lx), elemTyp, {needToCopy})

  tmpl

proc genMagicSpawn(g: LLGen, n: PNode): LLValue =
  let n2 = g.graph.wrapProcForSpawn(g.idgen, g.module.sym, n, n.typ, nil, nil)
  g.genNode(n2, true)

proc genMagicDeepCopy(g: LLGen, n: PNode) =
  let
    x =
      if n[1].kind in {nkAddr, nkHiddenAddr}:
        n[1][0]
      else:
        n[1]
    ax = g.genNode(x, false).v
    ty = n[2].typ.skipTypes(abstractVarRange)

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
  of tyPointer,
      tyChar,
      tyBool,
      tyEnum,
      tyCString,
      tyInt .. tyUInt64,
      tyRange,
      tyVar,
      tyLent:
    let bx = g.genNode(n[2], true).v
    discard g.b.buildStore(bx, ax)
  else:
    g.config.internalError(n.info, "genDeepCopy: " & $ty.kind)

proc genMagicGetTypeInfo(g: LLGen, n: PNode): LLValue =
  let typ = n[1].typ.skipTypes(abstractVarRange)
  LLValue(v: g.genTypeInfoV1(typ))

proc genMagicGetTypeInfoV2(g: LLGen, n: PNode): LLValue =
  let
    typ = n[1].typ
    v =
      if isFinal(typ) or n[0].sym.name.s != "getDynamicTypeInfo":
        g.genTypeInfoV2(typ)
      else:
        let ax = g.genNode(n[1], false)

        g.b.buildLoad2(g.ptrTy, ax.v)
  LLValue(v: v, storage: OnStatic)

proc genMagicAccessTypeField(g: LLGen, n: PNode, load: bool): LLValue =
  let ax = g.genNode(n[1], false)

  g.maybeLoadValue(g.ptrTy, LLValue(v: ax.v, lode: n, storage: OnStatic), load)

proc genMagic(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  var op = n[0].sym.magic
  p("genMagic " & $op, n[0], g.depth + 1)
  case op
  of mHigh:
    result = g.genMagicHigh(n)
  of mSizeOf:
    result = g.genMagicSizeOf(n)
  of mAlignOf:
    result = g.genMagicAlignOf(n)
  of mOf:
    result = g.genMagicOf(n)
  of mEcho:
    g.genMagicEcho(n)
  of mInc:
    g.genMagicIncDec(n, llvm.Add)
  of mDec:
    g.genMagicIncDec(n, llvm.Sub)
  of mOrd:
    result = g.genMagicOrd(n)
  of mNew:
    g.genMagicNew(n)
  of mNewFinalize:
    g.genMagicNewFinalize(n)
  of mNewSeq:
    g.genMagicNewSeq(n)
  of mNewSeqOfCap:
    result = g.genMagicNewSeqOfCap(n)
  of mLengthOpenArray:
    result = g.genMagicLength(n)
  of mLengthStr:
    result = g.genMagicLength(n)
  of mLengthArray:
    result = g.genMagicLength(n)
  of mLengthSeq:
    result = g.genMagicLength(n)
  of mIncl:
    g.genMagicIncl(n)
  of mExcl:
    g.genMagicExcl(n)
  of mCard:
    result = g.genMagicCard(n)
  of mChr:
    result = g.genMagicChr(n)
  of mGCref:
    g.genMagicGCref(n)
  of mGCunref:
    g.genMagicGCunref(n)
  of mAddI:
    result = g.genMagicBinOpOverflow(n, llvm.Add)
  of mSubI:
    result = g.genMagicBinOpOverflow(n, llvm.Sub)
  of mMulI:
    result = g.genMagicBinOpOverflow(n, llvm.Mul)
  of mDivI:
    result = g.genMagicBinOpOverflow(n, llvm.SDiv)
  of mModI:
    result = g.genMagicBinOpOverflow(n, llvm.SRem)
  # TODO verify
  of mSucc:
    result = g.genMagicBinOpOverflow(n, llvm.Add)
  of mPred:
    result = g.genMagicBinOpOverflow(n, llvm.Sub)
  of mAddF64:
    result = g.genMagicBinOpF(n, llvm.FAdd)
  of mSubF64:
    result = g.genMagicBinOpF(n, llvm.FSub)
  of mMulF64:
    result = g.genMagicBinOpF(n, llvm.FMul)
  of mDivF64:
    result = g.genMagicBinOpF(n, llvm.FDiv)
  of mShrI:
    result = g.genMagicShr(n)
  of mShlI:
    result = g.genMagicBinOp(n, llvm.Shl)
  of mAShrI:
    result = g.genMagicAShr(n)
  of mBitandI:
    result = g.genMagicBinOp(n, llvm.And)
  of mBitorI:
    result = g.genMagicBinOp(n, llvm.Or)
  of mBitxorI:
    result = g.genMagicBinOp(n, llvm.Xor)
  of mMinI:
    result = g.genMagicMinMaxI(n, llvm.IntSLE)
  # sign
  of mMaxI:
    result = g.genMagicMinMaxI(n, llvm.IntSGE)
  # sign
  of mAddU:
    result = g.genMagicBinOp(n, llvm.Add)
  of mSubU:
    result = g.genMagicBinOp(n, llvm.Sub)
  of mMulU:
    result = g.genMagicBinOp(n, llvm.Mul)
  of mDivU:
    result = g.genMagicBinOp(n, llvm.UDiv)
  of mModU:
    result = g.genMagicBinOp(n, llvm.URem)
  # TODO verify
  of mEqI:
    result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeI:
    result = g.genMagicCmpI(n, llvm.IntSLE)
  of mLtI:
    result = g.genMagicCmpI(n, llvm.IntSLT)
  of mEqF64:
    result = g.genMagicCmpF(n, llvm.RealOEQ)
  # TODO ordered?
  of mLeF64:
    result = g.genMagicCmpF(n, llvm.RealOLE)
  # TODO ordered?
  of mLtF64:
    result = g.genMagicCmpF(n, llvm.RealOLT)
  # TODO ordered?
  of mLeU:
    result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtU:
    result = g.genMagicCmpI(n, llvm.IntULT)
  of mEqEnum:
    result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeEnum:
    result = g.genMagicCmpI(n, llvm.IntULE)
  # TODO underlying
  of mLtEnum:
    result = g.genMagicCmpI(n, llvm.IntULT)
  # TODO underlying
  of mEqCh:
    result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeCh:
    result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtCh:
    result = g.genMagicCmpI(n, llvm.IntULT)
  of mEqB:
    result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLeB:
    result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtB:
    result = g.genMagicCmpI(n, llvm.IntULT)
  of mEqRef:
    result = g.genMagicCmpI(n, llvm.IntEQ)
  of mLePtr:
    result = g.genMagicCmpI(n, llvm.IntULE)
  of mLtPtr:
    result = g.genMagicCmpI(n, llvm.IntULT)
  of mXor:
    result = g.genMagicCmpI(n, llvm.IntNE)
  of mEqCString:
    result = g.genMagicEqCString(n, load, dest)
  of mEqProc:
    result = g.genMagicEqProc(n)
  of mUnaryMinusI, mUnaryMinusI64:
    result = g.genMagicUnaryMinus(n)
  of mAbsI:
    result = g.genMagicAbsI(n)
  of mNot:
    result = g.genMagicNot(n)
  of mBitnotI:
    result = g.genMagicBitnot(n)
  of mUnaryMinusF64:
    result = g.genMagicUnaryMinusF64(n)
  of mCharToStr:
    result = g.genMagicToStr(n, "nimCharToStr")
  of mBoolToStr:
    result = g.genMagicToStr(n, "nimBoolToStr")
  of mIntToStr:
    result = g.genMagicToStr(n, "nimIntToStr")
  of mInt64ToStr:
    result = g.genMagicToStr(n, "nimInt64ToStr")
  of mFloatToStr:
    result = g.genMagicToStr(n, "nimFloatToStr")
  of mCStrToStr:
    result = g.genMagicToStr(n, "cstrToNimstr")
  of mStrToStr:
    result = g.genMagicStrToStr(n)
  of mEnumToStr:
    result = g.genMagicEnumToStr(n)
  of mAnd, mOr:
    result = g.genMagicAndOr(n)
  of mEqStr:
    result = g.genMagicEqStr(n)
  of mLeStr:
    result = g.genMagicLeStr(n)
  of mLtStr:
    result = g.genMagicLtStr(n)
  of mEqSet:
    result = g.genMagicEqSet(n)
  of mLeSet:
    result = g.genMagicSetCmp(false, n)
  of mLtSet:
    result = g.genMagicSetCmp(true, n)
  of mMulSet:
    result = g.genMagicSetBinOp(llvm.And, false, n)
  of mPlusSet:
    result = g.genMagicSetBinOp(llvm.Or, false, n)
  of mMinusSet:
    result = g.genMagicSetBinOp(llvm.And, true, n)
  of mConStrStr:
    result = g.genMagicConStrStr(n)
  of mDotDot:
    result = g.genMagicDotDot(n, load, dest)
  of mAppendStrCh:
    g.genMagicAppendStrCh(n)
  of mAppendStrStr:
    g.genMagicAppendStrStr(n)
  of mAppendSeqElem:
    g.genMagicAppendSeqElem(n)
  of mInSet:
    result = g.genMagicInSet(n)
  of mRepr:
    result = g.genMagicRepr(n)
  of mExit:
    discard g.genCall(nil, n, false, LLValue())
  of mSetLengthStr:
    g.genMagicSetLengthStr(n)
  of mSetLengthSeq:
    g.genMagicSetLengthSeq(n)
  of mParallel:
    g.genMagicParallel(n)
  of mSwap:
    g.genMagicSwap(n)
  of mMove:
    result = g.genMagicMove(n, load)
  of mDestroy:
    g.genMagicDestroy(n)
  of mAccessEnv:
    result = g.genMagicAccessEnv(n, load)
  of mWasMoved:
    g.genMagicWasMoved(n)
  of mDefault, mZeroDefault:
    result = g.genMagicDefault(n, load)
  of mReset:
    g.genMagicReset(n)
  of mIsNil:
    result = g.genMagicIsNil(n)
  of mArrToSeq:
    result = g.genMagicArrToSeq(n)
  of mNewString, mNewStringOfCap, mParseBiggestFloat:
    result = g.genMagicCall(n, load, dest)
  of mSpawn:
    result = g.genMagicSpawn(n)
  of mDeepCopy:
    g.genMagicDeepCopy(n)
  of mGetTypeInfo:
    result = g.genMagicGetTypeInfo(n)
  of mGetTypeInfoV2:
    result = g.genMagicGetTypeInfoV2(n)
  of mAccessTypeField:
    result = g.genMagicAccessTypeField(n, load)
  of generatedMagics:
    # TODO these are magics, but for some reason they don't have the loc.r set
    result = g.genCall(nil, n, load, dest)
  of mSlice:
    result = g.genMagicSlice(n, load)
  of mEnsureMove:
    result = g.genNode(n[1], load)
  else:
    g.config.internalError(n.info, "Unhandled magic: " & $op)

# Nodes
proc maybeConstPtr(g: LLGen, n: PNode, v: llvm.ValueRef, load: bool): LLValue =
  if load and v.typeOfX.getTypeKind() != llvm.ArrayTypeKind:
    LLValue(v: v, lode: n, storage: OnStatic)
  else:
    let res = g.m.addPrivateConstant(v.typeOfX(), g.nn(".const", n))
    res.setInitializer(v)
    # May need to load array pointer
    g.maybeLoadValue(v.typeOfX(), LLValue(v: res, lode: n, storage: OnStatic), load)

proc genNodeSym(g: LLGen, n: PNode, load: bool): LLValue =
  let sym = n.sym
  case sym.kind
  of skVar, skLet, skTemp, skResult, skForVar:
    if {sfGlobal, sfThread} * sym.flags != {}:
      if sfCompileTime in sym.flags and sym.id notin g.symbols:
        genSingleVar(g, sym, n, astdef(sym))
    let v =
      if lfHeader in sym.loc.flags or lfNoDecl in sym.loc.flags:
        g.externGlobal(sym)
      elif sfGlobal in sym.flags:
        g.genGlobal(n, false)
      else:
        g.symbols[sym.id]

    if load and v.v.typeOfX().getTypeKind() == llvm.PointerTypeKind:
      let tmp = g.buildLoadValue(g.llType(sym.typ), v, g.nn("", n.info))
      if sfVolatile in sym.flags:
        # TODO writes...
        tmp.v.setVolatile(llvm.True)
      tmp
    else:
      v
  of skParam:
    var v = g.symbols[sym.id]
    if g.llPassAsPtr(sym, g.f.sym.typ[0]):
      v = g.buildLoadValue(g.ptrTy, v)

    g.maybeLoadValue(g.llType(sym.typ), v, load)
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
        n.info, "request to generate code for .compileTime proc: " & sym.name.s
      )

    g.genFunctionWithBody(sym)
  of skConst:
    let
      v = g.genConst(n)
      ty = g.llType(n.typ)

    g.maybeLoadValue(
      ty, v, load and v.v.typeOfX().getTypeKind() == llvm.PointerTypeKind
    )
  of skEnumField:
    LLValue(v: llvm.constInt(g.llType(sym.typ), sym.position.culonglong, llvm.False))
  else:
    g.config.internalError(n.info, "Unhandled nodesym kind: " & $sym.kind)
    LLValue()

proc genNodeIntLit(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ty = g.llType(n.typ)
    v =
      case ty.getTypeKind
      of llvm.PointerTypeKind:
        llvm.constIntToPtr(
          llvm.constInt(g.primitives[tyInt], n.intVal.culonglong, llvm.False), ty
        )
      of llvm.HalfTypeKind .. llvm.PPC_FP128TypeKind:
        # This is pretty nuts, but an example can be seen in `lib/pure/json.nim`
        # where `Q7` gets initialized..
        llvm.constReal(ty, n.intVal.cdouble)
      else:
        llvm.constInt(ty, n.intVal.culonglong, llvm.False)

  g.maybeConstPtr(n, v, load)

proc genNodeFloatLit(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ty = g.llType(n.typ)
    v = llvm.constReal(ty, n.floatVal)

  g.maybeConstPtr(n, v, load)

proc genNodeStrLit(g: LLGen, n: PNode, load: bool): LLValue =
  let
    tt =
      if n.typ == nil:
        # debugEcho "TODO nil-typ strlit", n
        getSysType(g.graph, n.info, tyString)
      else:
        n.typ

    typ =
      tt.skipTypes(abstractVarRange + {tyStatic, tyUserTypeClass, tyUserTypeClassInst})
    ty = g.llType(typ)
  case typ.kind
  of tyString:
    if n.strVal.len == 0:
      g.maybeConstPtr(n, constNull(ty), load)
    else:
      let lit = g.genStrLit(ty, n.strVal)

      g.maybeConstPtr(n, lit, load)
  of tyCstring:
    LLValue(v: g.constCStringPtr(n.strVal), lode: n, storage: OnStatic)
  else:
    g.config.internalError("Unknown string literal kind: " & $typ.kind)
    LLValue()

proc genNodeNilLit(g: LLGen, n: PNode, load: bool): LLValue =
  # proc x() = nil
  if n.typ.isEmptyType:
    return LLValue()

  let
    ty = g.llType(n.typ.skipTypes(abstractVarRange))
    v = llvm.constNull(ty)

  g.maybeConstPtr(n, v, load)

proc genNodeCall(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  let nf = n[namePos]

  if nf.kind == nkSym:
    let sym = nf.sym
    if sym.magic != mNone:
      return g.genMagic(n, load, dest)

  g.genCall(nil, n, load, dest)

proc genNodeIdentDefs(g: LLGen, n: PNode) =
  for s in n.sons:
    p("genIdentDefsStmt", s, g.depth + 1)
  if n[0].kind == nkSym:
    g.genSingleVar(n)
  else:
    g.genClosureVar(n)

proc genNodeVarTuple(g: LLGen, n: PNode) =
  for s in n.sons[0 .. n.len - 3]:
    if s.kind != nkSym:
      g.genNode(g.graph.lowerTupleUnpacking(n, g.idgen, g.module.sym))
      return

  let
    t = g.genNode(n.lastSon, false)
    ty = g.llType(n.lastSon.typ)

  for i in 0 .. n.len - 3:
    let
      vn = n[i]
      v = vn.sym

      x =
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
      tv = g.b.buildStructGEP2(ty, t, cuint i, g.nn("vartuple.field" & $i, vn))
      lx = g.loadAssignment(vn.typ)

    g.genAssignment(x, g.maybeLoadValue(g.llType(vn.typ), tv, lx), vn.typ, v.assignCopy)

proc genNodePar(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  if n.isDeepConstExprLL() and dest.v == nil and
      optSeqDestructors notin g.config.globalOptions:
    let v = g.genConstTupleConstr(n)
    return g.maybeConstPtr(n, v, load)

  let
    useTmp = dest.v == nil or dest.lode == nil or (isPartOf(dest.lode, n) != arNo)
    ty = g.llType(n.typ)
    v =
      if useTmp:
        let tmp =
          LLValue(v: g.localAlloca(ty, g.nn("par", n)), lode: n, storage: OnStack)
        g.buildStoreNull(ty, tmp.v)
        tmp
      else:
        dest

  for i in 0 ..< n.len:
    var s = n[i]

    if s.kind == nkExprColonExpr:
      s = s[1]
    let
      tgt = g.b.buildStructGEP2(ty, v, cuint i, g.nn("par.gep.field" & $i, n))
      bx = g.genAsgnNode(s, s.typ, tgt)

    g.genAssignment(tgt, bx, s.typ, {needToCopy})

  if useTmp:
    g.maybeLoadValue(ty, v, load)
  else:
    LLValue()

proc genFieldCheck(g: LLGen, n: PNode, v: LLValue, field: PSym) =
  let typ = skipTypes(n[0][0].typ, abstractInst + tyUserTypeClasses)

  for i in 1 ..< n.len:
    var it = n[i]
    let op = it[0].sym
    if op.magic == mNot:
      it = it[1]

    let
      tags = it[2].skipConv.sym
      tagTy = g.llType(tags.typ)
      tagGEP = g.toGEP(v.v, g.fieldIndex(typ, tags), g.nn("fchck.tag.gep", tags))
      tag = g.b.buildLoad2(tagTy, tagGEP)
      uTyp = skipTypes(it[1].typ, abstractVar)
      uTy = g.llType(uTyp)
      uSize = getSize(g.config, uTyp)
      u =
        if uSize <= 8:
          g.genNode(it[1], true).v
        else:
          let
            tmp = g.genNode(it[1], false).v
            gep = g.buildSetGEP(uTy, tmp, tag)

          g.b.buildLoad2(g.int8Ty, gep)
      mask = g.buildSetMask(uTy, tag, uSize)
      masked = g.b.buildAnd(u, mask, g.nn("masked", tag))
      cmped =
        g.b.buildICmp(llvm.IntEQ, masked, constNull(masked.typeOfX()), g.nn("cmp", tag))
      notted =
        if op.magic == mNot:
          g.b.buildNot(cmped, g.nn("not", cmped))
        else:
          cmped
      msg = g.config.genFieldDefect(field.name.s, tags)
    g.callRaise(notted, "raiseFieldError", [g.genStrLit(g.primitives[tyString], msg)])

proc genNodeObjConstr(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  ## Object construction - either a fresh instance or in-place depending on
  ## `dest`
  if n.isDeepConstExprLL() and dest.v == nil and
      optSeqDestructors notin g.config.globalOptions:
    let v = g.genConstObjConstr(n)
    return g.maybeConstPtr(n, v, load)

  var
    typ = n.typ.skipTypes(abstractInstOwned)
    ty = g.llType(typ)
    isRef = typ.kind == tyRef
    useTmp =
      dest.v == nil or dest.lode == nil or isRef or (isPartOf(dest.lode, n) != arNo)

  let v =
    if useTmp:
      if isRef:
        let tmp = LLValue(v: g.localAlloca(ty, g.nn("objconstr", n)), storage: OnStack)
        g.rawGenNew(tmp, nil, typ)
        typ = typ.lastSon
        ty = g.llType(typ)
        LLValue(
          v: g.b.buildLoad2(tmp.v.getAllocatedType(), tmp.v),
          lode: tmp.lode,
          storage: OnHeap,
        )
      else:
        let tmp = LLValue(v: g.localAlloca(ty, g.nn("objconstr", n)), storage: OnStack)
        g.buildStoreNull(ty, tmp.v)
        g.genObjectInit(typ, tmp.v)
        tmp
    else:
      g.callReset(typ, dest)
      dest
  for i in 1 ..< n.len:
    let s = n[i]

    if s.len == 3 and optFieldCheck in g.f.options:
      g.genFieldCheck(s[2], v, s[0].sym)

    let
      gep = LLValue(
        v: g.toGEP(v.v, g.fieldIndex(typ, s[0].sym), "objconstr"),
        lode: s[1],
        storage: v.storage,
      )
      bx = g.genAsgnNode(s[1], s[0].sym.typ, gep)
    g.genAssignment(gep, bx, s[0].sym.typ, {needToCopy})

  if useTmp:
    if load and not isRef:
      LLValue(v: g.b.buildLoad2(ty, v.v), lode: n, storage: OnStack)
    elif not load and isRef:
      let tmp = g.localAlloca(ty, g.nn("objconstr", n))
      discard g.b.buildStore(v.v, tmp)
      LLValue(v: tmp, storage: OnStack)
    else:
      v
  else:
    LLValue()

proc genNodeCurly(g: LLGen, n: PNode, load: bool): LLValue =
  if n.isDeepConstExprLL():
    let v = g.genConstCurly(n)
    return g.maybeConstPtr(n, v, load)

  let
    typ = n.typ
    size = g.config.getSize(skipTypes(n.typ, abstractVar))
    ty = g.llType(typ)

  if size <= 8:
    let tmp = g.localAlloca(ty, g.nn("curly", n))
    discard g.b.buildStore(llvm.constNull(ty), tmp)

    for s in n:
      if s.kind == nkRange:
        withRangeItems(it, s):
          let
            mask = g.buildSetMask(ty, g.setElemIndex(typ, it), size)
            v = g.b.buildLoad2(ty, tmp)
            masked = g.b.buildOr(v, mask, g.nn("curly.masked", n))
          discard g.b.buildStore(masked, tmp)
      else:
        let
          ax = g.genNode(s, true).v
          mask = g.buildSetMask(ty, g.setElemIndex(typ, ax), size)
          v = g.b.buildLoad2(ty, tmp)
          masked = g.b.buildOr(v, mask, g.nn("curly.masked", n))
        discard g.b.buildStore(masked, tmp)
    g.maybeLoadValue(ty, LLValue(v: tmp), load)
  else:
    let v = g.localAlloca(ty, g.nn("curly", n))
    g.buildStoreNull(ty, v)

    for s in n:
      if s.kind == nkRange:
        withRangeItems(it, s):
          let
            (gep, mask) = g.buildSetGEPMask(ty, v, g.setElemIndex(typ, it))
            v = g.b.buildLoad2(ty.getElementType(), gep)
            masked = g.b.buildOr(v, mask, g.nn("curly.masked", n))
          discard g.b.buildStore(masked, gep)
      else:
        let
          ax = g.genNode(s, true).v
          (gep, mask) = g.buildSetGEPMask(ty, v, g.setElemIndex(typ, ax))
          v = g.b.buildLoad2(ty.getElementType(), gep)
          masked = g.b.buildOr(v, mask, g.nn("curly.masked", n))
        discard g.b.buildStore(masked, gep)
    g.maybeLoadValue(ty, LLValue(v: v), load)

proc genNodeBracket(g: LLGen, n: PNode, load: bool): LLValue =
  let
    typ = n.typ.skipTypes(abstractVarRange)
    ty = g.llType(typ)
  if n.isDeepConstExprLL() and optSeqDestructors notin g.config.globalOptions:
    let v = g.genConstBracket(n)
    return g.maybeConstPtr(n, v, load)

  case typ.kind
  of tyArray, tyUncheckedArray:
    result =
      LLvalue(v: g.localAlloca(ty, g.nn("bracket.arr", n)), lode: n, storage: OnStack)
    g.buildStoreNull(ty, result.v)
    for i in 0 ..< n.len:
      let
        gep = g.b.buildInboundsGEP2(
          ty, result, [g.gep0, g.constGEPIdx(i)], g.nn("bracket.n", n)
        )
        bx = g.genAsgnNode(n[i], typ.elemType, gep)
      g.genAssignment(gep, bx, typ.elemType, {needToCopy})

    result = g.maybeLoadValue(ty, result, load)
  of tySequence:
    let
      tmp = LLValue(v: g.localAlloca(ty, g.nn("bracket", n)), storage: OnStack)
      seqTy = g.llSeqType(typ)

    if optSeqDestructors in g.config.globalOptions:
      let
        elemTy = g.llType(typ.elemType)
        dl = g.m.getModuleDataLayout()

      let s = g.callCompilerProc(
        "newSeqPayload",
        [
          g.constNimInt(n.len),
          g.constNimInt(int dl.aBISizeOfType(elemTy)),
          g.constNimInt(int dl.aBIAlignmentOfType(elemTy)),
        ],
      )
      discard g.b.buildStore(g.constNimInt(n.len), tmp.v)
      discard g.b.buildStore(
        s, g.b.buildInboundsGEP2(ty, tmp.v, [g.gep0, g.gep1], g.nn("payload", n))
      )
    else:
      g.buildStoreNull(ty, tmp.v)
      g.genNewSeqAux(tmp, n.typ, g.constNimInt(n.len))
    let tmpl = g.buildLoadValue(tmp.v.getAllocatedType(), tmp)

    for i in 0 ..< n.len:
      let
        gep = g.buildNimSeqDataGEP(seqTy, tmpl, g.constGEPIdx(i))
        bx = g.genAsgnNode(n[i], typ.elemType, gep)
      g.genAssignment(gep, bx, typ.elemType, {needToCopy})
    result = tmpl
  else:
    g.config.internalError(n.info, "Unhandled nodebracket kind: " & $typ.kind)

proc genNodeBracketExprArray(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true).v
    typ = n[0].typ.skipTypes(abstractVarRange + abstractPtrs + tyUserTypeClasses)
    ty = g.llType(typ)
    first = g.config.firstOrd(typ)

  assert bx.typeOfX().getTypeKind() == llvm.IntegerTypeKind
  assert typ.kind != tyUncheckedArray

  # GEP indices are signed, so if a char appears here we want to make sure
  # it's zero-extended
  let
    bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
    fi = g.constNimInt(first)
    b =
      if first != 0:
        g.b.buildSub(bi, fi, g.nn("bra.arr.first", n))
      else:
        bi

  if optBoundsCheck in g.f.options:
    let
      len = g.constNimInt(g.config.lastOrd(typ) - first + 1)
      cond = g.b.buildICmp(llvm.IntUGE, b, len, "bes.bounds")
    if first == 0 and g.config.lastOrd(typ) >= 0:
      g.callRaise(cond, "raiseIndexError2", [bi, len])
    else:
      g.callRaise(cond, "raiseIndexError3", [bi, fi, len])

  if ty.getTypeKind() == llvm.ArrayTypeKind:
    g.maybeLoadValue(
      ty.getElementType(),
      g.b.buildInboundsGEP2(ty, ax, [g.gep0, b], g.nn("bra.arr.geparr", n)),
      load,
    )
  else:
    g.maybeLoadValue(
      ty, g.b.buildInboundsGEP2(ty, ax, [b], g.nn("bra.arr.gep", n)), load
    )

proc genNodeBracketExprUncheckedArray(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], false).v
    bx = g.genNode(n[1], true).v
    typ = n[0].typ.skipTypes(abstractVarRange + abstractPtrs + tyUserTypeClasses)
    ty = g.llType(typ)
    first = g.config.firstOrd(typ)
    # GEP indices are signed, so if a char appears here we want to make sure
    # it's zero-extended
    bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))

  let b =
    if first != 0:
      g.b.buildSub(bi, g.constNimInt(first), g.nn("bra.arr.first", n))
    else:
      bi

  g.maybeLoadValue(
    g.llType(n.typ),
    LLValue(
      v:
        if ty.getTypeKind == llvm.ArrayTypeKind:
          g.b.buildInboundsGEP2(ty, ax, [g.gep0, b], g.nn("bra.uarr.geparr", n))
        else:
          g.b.buildInboundsGEP2(ty, ax, [b], g.nn("bra.uarr.gep", n))
    ),
    load,
  )

proc genNodeBracketExprOpenArray(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.buildLoadVar(n[0].typ, g.genNode(n[0], false).v)
    elemTy = g.llType(n[0].typ.elemType)
    px = LLValue(v: g.b.buildLoad2(g.ptrTy, g.buildOpenArrayDataGEP(ax)))
    bx = g.genNode(n[1], true).v
    bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))

  if optBoundsCheck in g.f.options:
    let
      len = g.b.buildLoad2(g.intTy, g.buildOpenArrayLenGEP(ax))
      cond = g.b.buildICmp(llvm.IntUGE, bi, len, "beoa.bounds")
      len2 = g.b.buildSub(len, g.constNimInt(1), "")
    g.callRaise(cond, "raiseIndexError2", [bi, len2])

  g.maybeLoadValue(
    elemTy, g.b.buildInboundsGEP2(elemTy, px, [bi], g.nn("beoa.gep", n)), load
  )

proc genNodeBracketExprSeq(
    g: LLGen, n: PNode, load: bool, prepareMutation: bool
): LLValue =
  let
    prepareMutation =
      prepareMutation and n[0].typ.skipTypes(abstractVarRange).kind == tyString and
      optSeqDestructors in g.config.globalOptions

    axp = g.genNode(n[0], not prepareMutation)
    ax =
      if prepareMutation:
        discard g.callCompilerProc("nimPrepareStrMutationV2", [axp.v])
        g.buildLoadValue(g.llType(n[0].typ), axp)
      else:
        axp
    bx = g.genNode(n[1], true).v
    bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
    seqTy = g.llSeqType(n[0].typ)
  if optBoundsCheck in g.f.options:
    let
      len = g.loadNimSeqLen(ax.v)
      cond = g.b.buildICmp(llvm.IntUGE, bi, len, "bes.bounds")
      len2 = g.b.buildSub(len, g.constNimInt(1), "")
    g.callRaise(cond, "raiseIndexError2", [bi, len2])

  g.maybeLoadValue(g.llType(n.typ), g.buildNimSeqDataGEP(seqTy, ax, bi), load)

proc genNodeBracketExprCString(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], true)
    bx = g.genNode(n[1], true).v
    bi = g.buildNimIntExt(bx, g.isUnsigned(n[1].typ))
    ty = g.llType(n.typ) # n.typ can be var (!)

  g.maybeLoadValue(
    ty,
    g.b.buildInboundsGEP2(g.primitives[tyChar], ax, [bi], g.nn("bra.cstr.gep", n)),
    load,
  )

proc genNodeBracketExprTuple(g: LLGen, n: PNode, load: bool): LLValue =
  var
    ax = g.genNode(n[0], false)
    bx = g.genNode(n[1], true).v
    ty = g.llType(n[0].typ.skipTypes(abstractVar))

  if bx.typeOfX().getIntTypeWidth() != 32.cuint:
    bx = g.buildTruncOrExt(bx, llvm.int32TypeInContext(g.lc), g.isUnsigned(n[1].typ))

  g.maybeLoadValue(
    g.llType(n.typ),
    g.b.buildInBoundsGEP2(ty, ax, [g.gep0, bx], g.nn("bra.tup.gep", n)),
    load,
  )

proc genNodeBracketExpr(g: LLGen, n: PNode, load, prepareMutation: bool): LLValue =
  var typ = skipTypes(n[0].typ, abstractVarRange + tyUserTypeClasses)
  if typ.kind in {tyRef, tyPtr}:
    typ = typ.lastSon.skipTypes(abstractVarRange)
  case typ.kind
  of tyArray:
    g.genNodeBracketExprArray(n, load)
  of tyUncheckedArray:
    g.genNodeBracketExprUncheckedArray(n, load)
  of tyOpenArray, tyVarargs:
    g.genNodeBracketExprOpenArray(n, load)
  of tySequence, tyString:
    g.genNodeBracketExprSeq(n, load, prepareMutation)
  of tyCString:
    g.genNodeBracketExprCString(n, load)
  of tyTuple:
    g.genNodeBracketExprTuple(n, load)
  else:
    g.config.internalError(n.info, "Unhandled nodebracketexpr kind: " & $typ.kind)
    LLValue()

proc genNodeDot(g: LLGen, n: PNode, load: bool): LLValue =
  p("genDotExpr", n[1].sym, g.depth + 1)
  let
    v = g.genNode(n[0], false)
    typ = skipTypes(n[0].typ, abstractInst + tyUserTypeClasses)
    gep =
      LLValue(v: g.toGEP(v.v, g.fieldIndex(typ, n[1].sym), "dot"), storage: v.storage)

  g.maybeLoadValue(g.llType(n.typ), gep, load)

proc genNodeCheckedField(g: LLGen, n: PNode, load: bool): LLValue =
  if optFieldCheck in g.config.options:
    let
      v = g.genNode(n[0][0], false)
      typ = skipTypes(n[0][0].typ, abstractInst + tyUserTypeClasses)

      gep = LLValue(
        v: g.toGEP(v.v, g.fieldIndex(typ, n[0][1].sym), "dot"), storage: v.storage
      )

    g.genFieldCheck(n, v, n[0][1].sym)
    g.maybeLoadValue(g.llType(n[0].typ), gep, load)
  else:
    g.genNode(n[0], load)

proc genNodeDeref(g: LLGen, n: PNode, load: bool): LLValue =
  let v = g.genNode(n[0], true)

  var typ = n[0].typ
  if typ.kind in {tyUserTypeClass, tyUserTypeClassInst} and typ.isResolvedUserTypeClass:
    typ = typ.lastSon
  typ = typ.skipTypes(abstractInstOwned)
  let storage =
    case typ.kind
    of tyRef:
      OnHeap
    of tyPtr, tyVar, tyLent:
      OnUnknown
    else:
      g.config.internalError(n.info, "deref " & $n[0].typ.kind)
      OnUnknown

  let tmp = LLValue(v: v.v, lode: v.lode, storage: storage)
  g.maybeLoadValue(g.llType(n.typ), tmp, load)

proc genNodeIfExpr(g: LLGen, n: PNode, load: bool): LLValue =
  # Sometimes an nkIfStmt appears in the ast even though it looks more like
  # an expression (see tcasestmt with an if in a case else).. it won't have
  # a type of its own so we'll have to cheat..
  let
    typ = n.deepTyp
    ty = g.llType(typ)
    v = LLValue(v: g.localAlloca(ty, g.nn("ifx.res", n)), storage: OnStack)
  g.buildStoreNull(ty, v.v)

  let iend = g.b.appendBasicBlockInContext(g.lc, g.nn("ifx.end", n))

  for i in 0 ..< n.len:
    let s = n[i]

    if s.len == 1:
      # else branch
      discard g.f.startBlock(n, nil)
      # branches might lack return value if they exit instead (quit?)
      if not typ.isEmptyType() and not s[0].typ.isEmptyType():
        let bx = g.genAsgnNode(s[0], typ, v)
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
        let bx = g.genAsgnNode(s[1], typ, v)
        g.genAssignment(v, bx, typ, {needToCopy})
      else:
        g.genNode(s[1])
      g.f.endBlock()

      discard g.b.buildBr(iend)

      g.b.positionAndMoveToEnd(ifalse)

  if n[n.len - 1].len != 1:
    discard g.b.buildBr(iend)

  g.b.positionAndMoveToEnd(iend)

  g.maybeLoadValue(v.v.getAllocatedType(), v, load)

proc genNodeLambda(g: LLGen, n: PNode): LLValue =
  let sym = n[namePos].sym
  g.genFunctionWithBody(sym)

proc genNodeConv(g: LLGen, n: PNode, load: bool): LLValue =
  let
    v = g.genNode(n[1], load)
    vt = v.v.typeOfX()
    nt =
      if load:
        g.llType(n.typ)
      else:
        g.ptrTy #  llvm.pointerType(g.llType(n.typ))
    vtk = vt.getTypeKind()
    ntk = nt.getTypeKind()
    vtyp = skipTypes(n[1].typ, abstractRange)
    ntyp = skipTypes(n.typ, abstractRange + {tyOwned})

  if vt == nt:
    v
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    LLValue(
      v:
        if ntyp.kind == tyBool:
          g.buildTruncOrExt(
            g.b.buildICmp(
              llvm.IntNE, v.v, constNull(v.v.typeOfX()), g.nn("conv.bool", n)
            ),
            nt,
            true,
          )
        else:
          g.buildTruncOrExt(v.v, nt, n[1].typ)
    )
  elif vtk in {llvm.HalfTypeKind .. llvm.PPC_FP128TypeKind} and
      ntk == llvm.IntegerTypeKind:
    LLValue(
      v:
        if ntyp.kind == tyBool:
          g.buildTruncOrExt(
            g.b.buildFCmp(
              llvm.RealUNE, v.v, constNull(v.v.typeOfX()), g.nn("conv.bool", n)
            ),
            nt,
            true,
          )
        elif ntyp.kind in {tyUInt .. tyUInt64}:
          g.b.buildFPToUI(v.v, nt, g.nn("conv.fptoui", n))
        else:
          g.b.buildFPToSI(v.v, nt, g.nn("conv.fptosi", n))
    )
  elif vtk == llvm.IntegerTypeKind and
      ntk in {llvm.HalfTypeKind .. llvm.PPC_FP128TypeKind}:
    LLValue(
      v:
        if vtyp.kind in {tyUInt .. tyUInt64}:
          g.b.buildUIToFP(v.v, nt, g.nn("conv.uitofp", n))
        else:
          g.b.buildSIToFP(v.v, nt, g.nn("conv.sitofp", n))
    )
  elif vtk == llvm.FloatTypeKind and ntk == llvm.DoubleTypeKind:
    LLValue(v: g.b.buildFPExt(v.v, nt, g.nn("conv.fd", n)))
  elif n.typ.kind == tyProc and ntk == llvm.PointerTypeKind or nt == g.llClosureType():
    LLValue(v: g.b.buildBitCast(v.v, g.ptrTy, g.nn("conv.proc", n)))
  elif vtk == llvm.DoubleTypeKind and ntk == llvm.FloatTypeKind:
    LLValue(v: g.b.buildFPTrunc(v.v, nt, g.nn("conv.df", n)))
  elif vtyp.kind == tyArray and ntyp.kind == tyArray:
    v
  else:
    g.config.internalError(
      n.info,
      "Unhandled conversion: " & $vt & " " & $nt & " " & $n[1].typ.kind & " " &
        $n.typ.kind,
    )
    LLValue()

proc genNodeCast(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ntyp = n.typ.skipTypes(abstractRange)
    vtyp = n[1].typ.skipTypes(abstractRange)

  if vtyp.skipTypes(abstractVar).kind in {tyVar, tyLent}:
    if ntyp.kind != tyPtr:
      g.config.internalError(
        n.info, "Unsupported cast: " & $vtyp.kind & " -> " & $ntyp.kind
      )

    let
      sx = g.genNode(n[1], false).v
      s = g.buildLoadVar(vtyp, sx)

    g.maybeLoadValue(g.ptrTy, LLValue(v: g.buildOpenArrayDataGEP(s)), load)
  else:
    let
      v = g.genNode(n[1], load).v
      vt = v.typeOfX()
      vtk = vt.getTypeKind()
      nt = g.llType(n.typ)
      ntk = nt.getTypeKind()

    LLValue(
      v:
        if vt == nt:
          v
        elif vtk == llvm.PointerTypeKind and ntk == llvm.IntegerTypeKind:
          g.b.buildPtrToInt(v, nt, g.nn("cast.pi", n))
        elif vtk == llvm.IntegerTypeKind and ntk == llvm.PointerTypeKind:
          g.b.buildIntToPtr(v, nt, g.nn("cast.ip", n))
        elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
          g.buildTruncOrExt(v, nt, vtyp)
        elif vtk == llvm.PointerTypeKind and ntk == llvm.PointerTypeKind:
          v
        else:
          let
            dl = g.m.getModuleDataLayout()
            size = max(dl.aBISizeOfType(vt), dl.aBISizeOfType(nt))
            tmp = g.localAlloca(
              llvm.arrayType(g.primitives[tyUInt8], size.cuint), g.nn("cast.tmp", n)
            )
          if load: # TODO when not loading, we could simply return pointers...
            discard g.b.buildStore(v, tmp)
          else:
            g.callMemcpy(tmp, v, g.constNimInt(size.int))
          g.maybeLoadValue(nt, LLValue(v: tmp), load).v
    )

proc genNodeAddr(g: LLGen, n: PNode): LLValue =
  g.genNode(n[0], false)

proc genNodeObjDownConv(g: LLGen, n: PNode, load: bool): LLValue =
  g.genNode(n[0], load)

proc genNodeObjUpConv(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], false)
    destTyp = n.typ.skipTypes(abstractPtrs)

  if optObjCheck in g.f.options and not isObjLackingTypeField(destTyp):
    var
      r = ax
      nilCheck: LLValue
      t = n[0].typ.skipTypes(abstractInst)
    while t.kind in {tyVar, tyLent, tyPtr, tyRef}:
      r = g.buildLoadValue(g.llType(t), r)
      if t.kind notin {tyVar, tyLent}:
        nilCheck = r
      t = t.lastSon.skipTypes(abstractInst)

    let
      cond =
        if not nilCheck.v.isNil():
          let notObj = g.withNotNilOrNull(nilCheck.v, g.int1Ty()):
            let isObj = g.buildI1(g.callIsObj(r.v, destTyp))
            g.b.buildNot(isObj, g.nn("not", n))
          g.b.buildNot(notObj, g.nn("cond", n))
        else:
          g.buildI1(g.callIsObj(r.v, destTyp))

      bad = g.b.appendBasicBlockInContext(g.lc, g.nn("upconv.bad", n))
      ok = g.b.appendBasicBlockInContext(g.lc, g.nn("upconv.ok", n))

    discard g.b.buildCondBr(cond, ok, bad)

    g.b.positionBuilderAtEnd(bad)
    discard g.callCompilerProc("raiseObjectConversionError", [], noReturn = true)

    g.b.positionBuilderAtEnd(ok)

  g.maybeLoadValue(g.llType(n.typ), ax, load)

proc genNodeChckRange(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[0], true)
    n0t = n[0].typ.skipTypes(abstractVarRange)
    dest = skipTypes(n.typ, abstractVar)

  if optRangeCheck in g.f.options and dest.kind notin {tyUInt .. tyUInt64}:
    let
      bx = g.genNode(n[1], true).v
      cx = g.genNode(n[2], true).v
    if n0t.kind in {tyUInt, tyUInt64}:
      let
        a = g.buildTruncOrExt(ax.v, g.primitives[tyUInt64], n[0].typ)
        c = g.buildTruncOrExt(cx, ax.v.typeOfX, n[2].typ)
        cmp = g.b.buildICmp(llvm.IntUGT, a, c, g.nn("rng.ugt"))
      g.callRaise(cmp, "raiseRangeErrorNoArgs")
    else:
      case skipTypes(n.typ, abstractVarRange).kind
      of tyUInt .. tyUInt64, tyChar:
        let
          a = g.buildTruncOrExt(ax.v, g.primitives[tyUInt64], n[0].typ)
          b = g.buildTruncOrExt(bx, g.primitives[tyUInt64], n[1].typ)
          c = g.buildTruncOrExt(cx, g.primitives[tyUInt64], n[2].typ)
          lt = g.b.buildICmp(llvm.IntULT, a, b, g.nn("rng.b"))
          gt = g.b.buildICmp(llvm.IntUGT, a, c, g.nn("rng.c"))
          both = g.b.buildOr(lt, gt, g.nn("rng.or"))
        g.callRaise(both, "raiseRangeErrorU", [a, b, c])
      of tyFloat .. tyFloat128:
        let
          a = g.preCast(n0t.isUnsigned, ax.v, getSysType(g.graph, n.info, tyFloat64))
          b = g.b.buildFPExt(bx, g.primitives[tyFloat64], g.nn("rng.bx", bx))
          c = g.b.buildFPExt(cx, g.primitives[tyFloat64], g.nn("rng.cx", cx))
          lt = g.b.buildFCmp(llvm.RealULT, a, b, g.nn("rng.b"))
          gt = g.b.buildFCmp(llvm.RealUGT, a, c, g.nn("rng.c"))
          both = g.b.buildOr(lt, gt, g.nn("rng.or"))
        g.callRaise(both, "raiseRangeErrorF", [a, b, c])
      else:
        let
          a = g.buildTruncOrExt(ax.v, g.primitives[tyInt64], n[0].typ)
          b = g.buildTruncOrExt(bx, g.primitives[tyInt64], n[1].typ)
          c = g.buildTruncOrExt(cx, g.primitives[tyInt64], n[2].typ)
          lt = g.b.buildICmp(llvm.IntSLT, a, b, g.nn("rng.b"))
          gt = g.b.buildICmp(llvm.IntSGT, a, c, g.nn("rng.c"))
          both = g.b.buildOr(lt, gt, g.nn("rng.or"))
        g.callRaise(both, "raiseRangeErrorI", [a, b, c])

  LLValue(v: g.buildTruncOrExt(ax.v, g.llType(dest), g.isUnsigned(n0t)))

proc genNodeStringToCString(g: LLGen, n: PNode): LLValue =
  let
    ax = g.genNode(n[0], true)
    cstr = g.callCompilerProc("nimToCStringConv", [ax.v])
  LLValue(v: cstr, lode: ax.lode, storage: ax.storage)

proc genNodeAsgn(g: LLGen, n: PNode) =
  g.genAsgn(n, false)

proc genNodeFastAsgn(g: LLGen, n: PNode) =
  g.genAsgn(n, g.f != g.init)

proc genNodeProcDef(g: LLGen, n: PNode) =
  if n.sons[genericParamsPos].kind != nkEmpty:
    return

  let s = n.sons[namePos].sym

  if s.typ == nil:
    return
  if sfBorrow in s.flags:
    return
  if s.skipGenericOwner.kind != skModule or sfCompileTime in s.flags:
    return
  if g.graph.getBody(s).kind == nkEmpty and lfDynamicLib notin s.loc.flags:
    return

  if ({sfExportc, sfCompilerProc} * s.flags == {sfExportc}) or
      (sfExportc in s.flags and lfExportLib in s.loc.flags) or (s.kind == skMethod):
    discard g.genFunctionWithBody(s)

proc genNodeIfStmt(g: LLGen, n: PNode): LLValue =
  # TODO Single scope enough?
  discard g.f.startBlock(n, nil)
  var iend: llvm.BasicBlockRef
  for i in 0 ..< n.len:
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
    result =
      LLValue(v: g.localAlloca(g.llType(typ), g.nn("case.res", n)), storage: OnStack)
    g.buildStoreNull(result.v.getAllocatedType(), result.v)
    g.genObjectInit(typ, result.v)

  for i in 1 ..< n.len:
    let s = n[i]
    p("genNodeCaseStmt", s, g.depth)

    let isLast = i == n.len - 1

    let cur = g.b.getInsertBlock()

    let lbl =
      if s.kind == nkElse:
        "case.else"
      else:
        "case.of." & $i
    let casedo = g.b.appendBasicBlockInContext(g.lc, g.nn(lbl & ".do", n))

    let next =
      if isLast:
        caseend
      elif n[i + 1].kind == nkElse:
        g.b.appendBasicBlockInContext(g.lc, g.nn("case.else", n))
      else:
        g.b.appendBasicBlockInContext(g.lc, g.nn("case.of." & $(i + 1), n))

    discard g.f.startBlock(n, caseend)
    g.b.positionBuilderAtEnd(casedo)
    # branches might not have a return value if they exit instead (quit?)
    if result.v != nil and not s.lastSon.typ.isEmptyType():
      let bx = g.genAsgnNode(s.lastSon, typ, result)
      g.genAssignment(result, bx, typ, {needToCopy})
    else:
      g.genNode(s.lastSon)

    g.b.buildBrFallthrough(caseend)

    g.b.positionBuilderAtEnd(cur)

    case s.kind
    of nkOfBranch:
      # sons here is a list of candidates to match against, which may
      # be values or ranges, except the last one which is the action
      for j in 0 .. s.len - 2:
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
            if isLast2:
              next
            else:
              g.b.appendBasicBlockInContext(g.lc, g.nn(lbl & ".or." & $j, n))
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
            cmp = g.buildI1(g.b.buildFCmp(llvm.RealOEQ, ax, b, g.nn("case.cmp", n)))
          else:
            let b = g.buildTruncOrExt(bx, ax.typeOfX(), cond.typ)
            cmp = g.buildI1(g.b.buildICmp(llvm.IntEQ, ax, b, g.nn("case.cmp", n)))
          let casenext =
            if isLast2:
              next
            else:
              g.b.appendBasicBlockInContext(g.lc, g.nn(lbl & ".or." & $j, n))
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
      g.b.getInsertBlock().getBasicBlockParent().getLastBasicBlock()
    )

  if not typ.isEmptyType():
    result =
      g.maybeLoadValue(result.v.getAllocatedType(), result, load and result.v != nil)

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
    g.landingPadTy.structSetBody([g.ptrTy, g.lc.int32TypeInContext()], False)

  g.landingPadTy

proc finNode(n: PNode): PNode =
  assert n.kind in {nkTryStmt, nkHiddenTryStmt}, $n.kind
  if n[^1].kind == nkFinally:
    n[^1]
  else:
    nil

proc genLandingPad(
    g: LLGen, v: PNode, inExc: bool, cmps: llvm.BasicBlockRef
): llvm.BasicBlockRef =
  let pad =
    g.b.appendBasicBlockInContext(g.lc, g.nn(if inExc: "try.excPad" else: "try.pad", v))

  g.withBlock(pad):
    let landing = g.b.buildLandingPad(
      g.getLandingPadTy(),
      g.getPersonalityFn(),
      0,
      g.nn(if inExc: "land.exc" else: "land.try", v),
    )

    var hasCatchAll = false

    template addClauses(nParam: PNode) =
      let n = nParam
      for i in 1 ..< n.len:
        let ni = n[i]
        if ni.kind != nkExceptBranch:
          break

        if ni.len == 1: # `except:`
          hasCatchAll = true
          landing.addClause(constNull(g.ptrTy))
        else:
          for j in 0 .. ni.len - 2: #`except Type`
            let
              etyp =
                if ni[j].isInfixAs():
                  ni[j][1].typ
                else:
                  ni[j].typ
              eti = g.genTypeInfo(etyp)

            landing.addClause(eti)

      if not hasCatchAll:
        let fin = n.finNode
        if fin != nil:
          # If there is no catch-all, we generate a fake one where the finally
          # code can run
          landing.addClause(constNull(g.ptrTy))
          hasCatchAll = true

    if inExc:
      landing.setCleanup(llvm.True)
    else:
      addClauses(v)

    # When looking for an exception handler, we match all outer handlers as well
    # until we hit one that is guaranteed to have handled all exceptions - if
    # none of them match, we move on to the resume instruction which tries the
    # next function.

    # TODO skip subtypes if supertype has been caught?
    for entry in reversed(g.f.nestedTryStmts):
      if hasCatchAll:
        break
      if entry.inExcept:
        continue
      addClauses(entry.n)

    if g.f.landingRes == nil:
      g.f.landingRes = g.localAlloca(landing.typeOfX, g.nn("landingRes"))
      g.f.resumeBlock = g.b.appendBasicBlockInContext(g.lc, g.nn("eh.resume"))
      g.withBlock(g.f.resumeBlock):
        let land = g.b.buildLoad2(g.f.landingRes.getAllocatedType(), g.f.landingRes)
        discard g.b.buildResume(land)

    discard g.b.buildStore(landing, g.f.landingRes)

    if inExc:
      discard g.callCompilerProc("nlvmEndCatch", [], noInvoke = true)
      var nextCmps = g.f.resumeBlock
      for i in countdown(g.f.nestedTryStmts.high, 0):
        if not g.f.nestedTryStmts[i].inExcept:
          nextCmps = g.f.nestedTryStmts[i].cmps
          break

      discard g.b.buildBr(nextCmps)
    else:
      discard g.b.buildBr(cmps)

  pad

proc genNodeTryStmt(g: LLGen, n: PNode, load: bool): LLValue =
  template genOrAssign(n: PNode) =
    if result.v != nil and not typ.isEmptyType():
      let bx = g.genAsgnNode(n, typ, result)
      g.genAssignment(result, bx, typ, {needToCopy})
    else:
      g.genNode(n)

  let typ = n.deepTyp

  if not typ.isEmptyType():
    result =
      LLValue(v: g.localAlloca(g.llType(typ), g.nn("try.res", n)), storage: OnStack)
    g.buildStoreNull(result.v.getAllocatedType(), result.v)
    g.genObjectInit(typ, result.v)

  # We create two landing pads: one for when we're in "try" and the other
  # when we're in "except" and want to clean up after the caught
  # exception then continue to an outer try or next frame
  let
    cmps = g.b.appendBasicBlockInContext(g.lc, g.nn("try.cmps", n))
    tryPad = g.genLandingPad(n, false, cmps)
    excPad = g.genLandingPad(n, true, cmps)
    tryEnd = g.b.appendBasicBlockInContext(g.lc, g.nn("try.end", n))

  g.f.nestedTryStmts.add((n, false, false, tryPad, excPad, cmps))

  # Generate code inside try block - anything that raises in here will jump
  # to tryPad
  discard g.f.startBlock(nil, nil)
  genOrAssign(n[0])
  g.f.endBlock()

  # Happy ending - no exception was raised or it was raised and caught
  g.b.buildBrFallthrough(tryEnd)

  # After the current try is done, any nested try must no longer catch
  # exceptions of *this* try - codegen order is important here
  g.f.nestedTryStmts[^1].inExcept = true

  # `cmps` is where we look for handlers that match the exception type - we
  # search the current scope and any parent scopes until we hit a catch-all
  # or resume in the caller frame
  g.b.positionAndMoveToEnd(cmps)

  let
    land = g.b.buildLoad2(g.f.landingRes.getAllocatedType(), g.f.landingRes)
    landedPtr = g.b.buildExtractValue(land, 0, g.nn("landedptr", n))
    landedTypeId = g.b.buildExtractValue(land, 1, g.nn("landedti", n))

  var hasCatchAll = false

  for i in 1 ..< n.len:
    let ni = n[i]
    if ni.kind != nkExceptBranch:
      break

    if ni.len == 1: # `except:`
      # TODO detect type id here too for foreign exceptions?
      # catch-all
      hasCatchAll = true

      discard g.callCompilerProc("nlvmBeginCatch", [landedPtr], noInvoke = true)

      discard g.f.startBlock(nil, nil)
      genOrAssign(ni[0])
      g.f.endBlock()

      if g.b.needsTerminator():
        # Looks like exception was handled, release and jump to end-of-block
        discard g.callCompilerProc("nlvmEndCatch", [], noInvoke = true)
        discard g.b.buildBr(tryEnd)
    else:
      for j in 0 .. ni.len - 2: # `except A`, `except A as a`
        # exception handler body will duplicated for every type
        # TODO we could avoid this..

        let
          etyp =
            if ni[j].isInfixAs():
              ni[j][1].typ
            else:
              ni[j].typ
          eti = g.genTypeInfo(etyp)

        # Compare raised type with exception handler type
        let
          eq = g.b.appendBasicBlockInContext(g.lc, g.nn("try.eq", n))
          ne = g.b.appendBasicBlockInContext(g.lc, g.nn("try.ne", n))
          localTypeId = g.callEhTypeIdFor(eti)
          isEq =
            g.b.buildICmp(llvm.IntEQ, landedTypeId, localTypeId, g.nn("try.iseq", n))

        discard g.b.buildCondBr(isEq, eq, ne)

        g.b.positionBuilderAtEnd(eq)

        # Exception type matched, emit handler
        discard g.callCompilerProc("nlvmBeginCatch", [landedPtr], noInvoke = true)

        discard g.f.startBlock(nil, nil)
        genOrAssign(ni[^1])
        g.f.endBlock()

        if g.b.needsTerminator():
          # Looks like exception was handled, release and jump to end-of-block
          discard g.callCompilerProc("nlvmEndCatch", [], noInvoke = true)

          discard g.b.buildBr(tryEnd)

        # No match, keep building
        g.b.positionBuilderAtEnd(ne)

  g.b.moveToEnd(excPad)

  let fin = n.finNode
  if fin != nil:
    # The finalizer is added right before the end of ever possible exit from
    # the try scope - outside of try, but inside of except (ie when the finally
    # code runs inside an `except`, `getCurrentException` still points to the
    # exception corresponding to that `except`)
    g.f.nestedTryStmts[^1].inFin = true
    if not hasCatchAll:
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

      # With the finalizer done, we've done everything that happens "inside"
      # the context of the raised exception
      discard g.f.nestedTryStmts.pop()
      g.b.positionAndMoveToEnd(tryEnd)
    else:
      discard g.f.nestedTryStmts.pop()
      g.b.buildBrFallthrough(tryEnd)
      g.b.positionAndMoveToEnd(tryEnd)

    # Emit finalizer code in happy path
    discard g.f.startBlock(nil, nil)
    g.genNode(fin[0])
    g.f.endBlock()
  else:
    # There is no finalizer - the current exception handling context is finished
    # and we move on to search the next one
    discard g.f.nestedTryStmts.pop()

    if not hasCatchAll:
      # Move on to the parent type comparison section
      var nextCmps = g.f.resumeBlock
      for i in countdown(g.f.nestedTryStmts.high, 0):
        if not g.f.nestedTryStmts[i].inExcept:
          nextCmps = g.f.nestedTryStmts[i].cmps
          break

      discard g.b.buildBr(nextCmps)
    else:
      g.b.buildBrFallthrough(tryEnd)

    g.b.positionAndMoveToEnd(tryEnd)

  if not typ.isEmptyType():
    result =
      g.maybeLoadValue(result.v.getAllocatedType(), result, load and result.v != nil)

proc genNodeRaiseStmt(g: LLGen, n: PNode) =
  if g.f.nestedTryStmts.len > 0 and g.f.nestedTryStmts[^1].inExcept and
      not g.f.nestedTryStmts[^1].inFin:
    let fin = g.f.nestedTryStmts[^1].n.finNode
    if fin != nil:
      discard g.f.startBlock(nil, nil)
      g.genNode(fin[0])
      g.f.endBlock()

  if n[0].kind != nkEmpty:
    let ax = g.genNode(n[0], true).v

    discard g.callCompilerProc("nlvmRaise", [ax], noReturn = true)
  else:
    discard g.callCompilerProc("nlvmReraise", [], noReturn = true)

proc blockLeave(g: LLGen, howManyTrys: int) =
  var stack = newSeq[LLEhEntry](0)

  for i in 1 .. howManyTrys:
    # The finally block conceptually runs after the try block has ended, so
    # we pop entries here as we go through the list
    let tryStmt = g.f.nestedTryStmts.pop

    stack.add(tryStmt)

    let fin = tryStmt.n.finNode
    if fin != nil and not tryStmt.inFin:
      discard g.f.startBlock(nil, nil)
      g.genNode(fin[0])
      g.f.endBlock()

  for i in countdown(howManyTrys - 1, 0):
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
    assert(sym.loc.k == locOther)
    idx = sym.position - 1
  else:
    # an unnamed 'break' can only break a loop after 'transf' pass:
    while idx >= 0 and not g.f.blocks[idx].isLoop:
      dec idx
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
      sym.position = g.f.breakIdx + 1
    g.genNode(n[1])
    let scope = g.f.endBlock()

    if scope.exit != nil:
      g.b.buildBrFallthrough(scope.exit)
      g.b.positionAndMoveToEnd(scope.exit)

proc genNodeDiscardStmt(g: LLGen, n: PNode) =
  if n[0].kind != nkEmpty:
    discard g.genNode(n[0], true)

proc genNodeStmtListExpr(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  for s in n.sons[0 ..^ 2]:
    g.genNode(s)
  if n.sons.len > 0:
    g.genNode(n[^1], load, dest)
  else:
    LLValue()

proc genNodeBlockExpr(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  preserve(g.f.breakIdx):
    g.f.breakIdx = g.f.startBlock(n[0], nil)
    if n[0].kind != nkEmpty:
      # named block?
      var sym = n[0].sym
      sym.loc.k = locOther
      sym.position = g.f.breakIdx + 1

    result = g.genNode(n[1], load, dest)
    let scope = g.f.endBlock()

    if scope.exit != nil:
      g.b.buildBrFallthrough(scope.exit)
      g.b.positionAndMoveToEnd(scope.exit)

proc genNodeClosure(g: LLGen, n: PNode, load: bool): LLValue =
  let
    ax = g.genNode(n[0], true).v
    bx = g.genNode(n[1], true).v
    ty = g.llType(n.typ)
    v = g.localAlloca(ty, g.nn("clox.res", n))

  discard g.b.buildStore(ax, g.b.buildStructGEP2(ty, v, 0, g.nn("ClP_0", n)))
  discard g.b.buildStore(bx, g.b.buildStructGEP2(ty, v, 1, g.nn("ClE_0", n)))

  g.maybeLoadValue(ty, LLValue(v: v, storage: OnStack), load)

proc genNodeGotoState(g: LLGen, n: PNode) =
  let ax = g.genNode(n[0], true).v

  let l = g.config.lastOrd(n[0].typ).toInt # TODO Int128

  let prereturn = g.b.appendBasicBlockInContext(g.lc, g.nn("goto.prereturn", n))

  g.f.blocks[g.f.blocks.len - 1].goto = g.b.buildSwitch(ax, prereturn, (l + 1).cuint)

  g.b.positionBuilderAtEnd(prereturn)

  g.blockLeave(g.f.nestedTryStmts.len)
  discard g.b.buildBr(g.section(g.f, secReturn))

  # Sometimes, there's litter after the gotostate switch - add a block for it!
  g.b.positionBuilderAtEnd(g.getDeadBlock())

proc genNodeState(g: LLGen, n: PNode) =
  let state = g.b.appendBasicBlockInContext(g.lc, g.nn("state." & $n[0].intVal, n))
  g.b.buildBrFallthrough(state)
  g.b.positionBuilderAtEnd(state)
  for i in 0 .. g.f.blocks.len - 1:
    if g.f.blocks[g.f.blocks.len - i - 1].goto != nil:
      g.f.blocks[g.f.blocks.len - i - 1].goto.addCase(
        g.constNimInt(n[0].intVal.int), state
      )
      break

proc genNodeBreakState(g: LLGen, n: PNode): LLValue =
  # TODO C code casts to int* and reads second value.. uh, I guess we should be
  # able to do better
  let a =
    if n[0].kind == nkClosure:
      let
        ax = g.genNode(n[0][1], true).v
        ty = g.llType(n[0][1].typ[0]) # typ = ref Env
      g.b.buildStructGEP2(ty, ax, 1, g.nn("state.break.cnt", n))
    else:
      let
        ax = g.genNode(n[0], false).v
        ag = g.buildClosureEnvGEP(ax)
        al = g.b.buildLoad2(g.ptrTy, ag)
      g.b.buildInboundsGEP2(g.intTy, al, [g.gep1], g.nn("state.break.cnt", n))

  let s = g.b.buildLoad2(g.intTy, a)
  LLValue(v: g.b.buildICmp(llvm.IntSLT, s, g.ni0, g.nn("state.break.cmp", n)))

proc genNodeTupleConstr(g: LLGen, n: PNode, load: bool, dest: LLValue): LLValue =
  genNodePar(g, n, load, dest)

proc genSons(g: LLGen, n: PNode) =
  for s in n:
    g.genNode(s)

proc genNode(
    g: LLGen, n: PNode, load: bool, dest: LLValue, prepareMutation: bool
): LLValue =
  p(if load: "l" else: "p", n, g.depth)

  g.depth += 1

  g.debugUpdateLoc(n)

  case n.kind
  of nkEmpty:
    discard
  of nkSym:
    result = g.genNodeSym(n, load)
  of nkCharLit .. nkUInt64Lit:
    result = g.genNodeIntLit(n, load)
  of nkFloatLit .. nkFloat128Lit:
    result = g.genNodeFloatLit(n, load)
  of nkStrLit .. nkTripleStrLit:
    result = g.genNodeStrLit(n, load)
  of nkNilLit:
    result = g.genNodeNilLit(n, load)
  of nkCallKinds:
    result = g.genNodeCall(n, load, dest)
  of nkExprColonExpr:
    result = g.genNode(n[1], load, dest)
  of nkIdentDefs:
    g.genNodeIdentDefs(n)
  of nkVarTuple:
    g.genNodeVarTuple(n)
  of nkPar:
    result = g.genNodePar(n, load, dest)
  of nkObjConstr:
    result = g.genNodeObjConstr(n, load, dest)
  of nkCurly:
    result = g.genNodeCurly(n, load)
  of nkBracket:
    result = g.genNodeBracket(n, load)
  of nkBracketExpr:
    result = g.genNodeBracketExpr(n, load, prepareMutation)
  of nkDotExpr:
    result = g.genNodeDot(n, load)
  of nkCheckedFieldExpr:
    result = g.genNodeCheckedField(n, load)
  of nkDerefExpr, nkHiddenDeref:
    result = g.genNodeDeref(n, load)
  of nkIfExpr:
    result = g.genNodeIfExpr(n, load)
  of nkLambda, nkDo:
    result = g.genNodeLambda(n)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv:
    result = g.genNodeConv(n, load)
  of nkCast:
    result = g.genNodeCast(n, load)
  of nkAddr, nkHiddenAddr:
    result = g.genNodeAddr(n)
  of nkObjDownConv:
    result = g.genNodeObjDownConv(n, load)
  of nkObjUpConv:
    result = g.genNodeObjUpConv(n, load)
  of nkChckRangeF, nkChckRange64, nkChckRange:
    result = g.genNodeChckRange(n)
  of nkStringToCString:
    result = g.genNodeStringToCString(n)
  of nkCStringToString:
    result = g.genMagicToStr(n, "cstrToNimstr")
  of nkAsgn:
    g.genNodeAsgn(n)
  of nkFastAsgn:
    g.genNodeFastAsgn(n)
  of nkProcDef, nkFuncDef, nkMethodDef, nkConverterDef:
    g.genNodeProcDef(n)
  of nkPragma:
    for s in n:
      let p = whichPragma(s)
      case p
      of wAsm, wEmit:
        g.config.internalError(n.info, $p & " pragma not supported")
      else:
        discard
  of nkPragmaBlock:
    result = g.genNode(n.lastSon, load)
  of nkIfStmt:
    result = g.genNodeIfStmt(n)
  # if in case! see tcaststm.nim
  of nkWhenStmt:
    result = g.genNodeWhenStmt(n, load)
  of nkWhileStmt:
    g.genNodeWhileStmt(n)
  of nkCaseStmt:
    result = g.genNodeCaseStmt(n, load)
  # Sometimes seen as expression!
  of nkVarSection, nkLetSection, nkConstSection:
    g.genSons(n)
  of nkConstDef:
    g.genNodeConstDef(n)
  of nkTryStmt, nkHiddenTryStmt:
    result = g.genNodeTryStmt(n, load)
  of nkRaiseStmt:
    g.genNodeRaiseStmt(n)
  of nkReturnStmt:
    g.genNodeReturnStmt(n)
  of nkBreakStmt:
    g.genNodeBreakStmt(n)
  of nkBlockStmt:
    g.genNodeBlockStmt(n)
  of nkDiscardStmt:
    g.genNodeDiscardStmt(n)
  of nkStmtList:
    g.genSons(n)
  of nkStmtListExpr:
    result = g.genNodeStmtListExpr(n, load, dest)
  of nkBlockExpr:
    result = g.genNodeBlockExpr(n, load, dest)
  of nkClosure:
    result = g.genNodeClosure(n, load)
  of nkGotoState:
    g.genNodeGotoState(n)
  of nkState:
    g.genNodeState(n)
  of nkBreakState:
    result = g.genNodeBreakState(n)
  of nkTupleConstr:
    result = g.genNodeTupleConstr(n, load, dest)
  of nkMixinStmt, nkBindStmt:
    discard
  of nkTypeSection, nkCommentStmt, nkIteratorDef, nkIncludeStmt, nkImportStmt,
      nkImportExceptStmt, nkExportStmt, nkExportExceptStmt, nkFromStmt, nkTemplateDef,
      nkMacroDef, nkArgList:
    discard
  else:
    g.config.internalError(n.info, "Unhandled node: " & $n)

  g.depth -= 1

proc newLLGen(
    graph: ModuleGraph,
    idgen: IdGenerator,
    target: string,
    tm: TargetMachineRef,
    lto: LtoKind,
): LLGen =
  let
    lc = llvm.getGlobalContext()

    name = graph.config.m.fileInfos[graph.config.projectMainIdx.int].shortName
    intType = llvm.intTypeInContext(lc, graph.config.target.intSize.cuint * 8)
    charType = llvm.int8TypeInContext(lc)

  var orc: OrcLLJITRef

  if optWasNimscript in graph.config.globalOptions:
    block MakeJIT:
      let err = orcCreateLLJIT(addr orc, nil)
      if not err.isNil:
        graph.config.internalError($err.getErrorMessage)

    let mainJD = orcLLJITGetMainJITDylib(orc)

    block MakeResolver:
      # Resolve symbols in the currently running process - this is a lazy way of
      # making the JIT find the C library and anything else in the nlvm binary.
      var processSymbolsGenerator: OrcDefinitionGeneratorRef
      let err = orcCreateDynamicLibrarySearchGeneratorForProcess(
        addr processSymbolsGenerator, orcLLJITGetGlobalPrefix(orc), nil, nil
      )
      if not err.isNil:
        graph.config.internalError($err.getErrorMessage)

      orcJITDylibAddGenerator(mainJD, processSymbolsGenerator)

  result = LLGen(
    graph: graph,
    lc: lc,
    m: llvm.moduleCreateWithNameInContext(name, lc),
    tm: tm,
    b: llvm.createBuilderInContext(lc),
    idgen: idgen,
    cintTy: llvm.int32TypeInContext(lc), # c int on linux
    csizetTy: llvm.int64TypeInContext(lc), # c size_t on linux
    jmpBufTy: llvm.structCreateNamed(lc, "jmp_buf"),
    strLitFlag: int64(1'i64 shl (graph.config.target.intSize * 8 - 2)),
    attrNoInline: lc.createEnumAttribute(llvm.attrNoInline, 0),
    attrNoReturn: lc.createEnumAttribute(llvm.attrNoReturn, 0),
    attrNoUnwind: lc.createEnumAttribute(llvm.attrNoUnwind, 0),
    attrNoOmitFP: lc.createStringAttribute("no-frame-pointer-elim", "true"),
    attrCold: lc.createEnumAttribute(llvm.attrCold, 0),
    attrNonnull: lc.createEnumAttribute(llvm.attrNonnull, 0),
    attrNoalias: lc.createEnumAttribute(llvm.attrNoalias, 0),
    symbols: initTable[int, LLValue](),
    gmarkers: initTable[int, llvm.ValueRef](),
    markers: initTable[SigHash, llvm.ValueRef](),
    nodeInfos: initTable[SigHash, llvm.ValueRef](),
    typeInfos: initTable[SigHash, llvm.ValueRef](),
    types: initTable[SigHash, llvm.TypeRef](),
    sigConflicts: initCountTable[SigHash](),
    tgtExportLinkage:
      if graph.config.target.targetCPU == cpuWasm32:
        llvm.ExternalLinkage
      else:
        llvm.CommonLinkage,
    lto: lto,
    orc: orc,
  )

  var g = result

  g.m.setModuleDataLayout(tm.createTargetDataLayout())
  g.m.setTarget(target)

  block:
    proc s(t: TTypeKind, v: llvm.TypeRef) =
      g.primitives[t] = v

    s(tyBool, llvm.int8TypeInContext(lc)) # needs 8 bits for atomics to work...
    s(tyChar, charType)
    s(tyNil, g.ptrTy)
    s(tyNone, g.ptrTy) # found in `func f(s: openArray, ...)`
    # tyTyped appears for example in `echo()` as the element type of the array
    s(tyTyped, g.ptrTy)
    s(tyPointer, g.ptrTy)
    s(tyCString, g.ptrTy)
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
    g.jmpBufTy,
    [
      llvm.arrayType(llvm.int64TypeInContext(g.lc), 8),
      llvm.int32TypeInContext(g.lc),
      llvm.int32TypeInContext(g.lc), # padding..
      llvm.arrayType(llvm.int64TypeInContext(g.lc), 16),
    ],
  )

  g.gep0 = g.constGEPIdx(0)
  g.gep1 = g.constGEPIdx(1)
  g.ni0 = g.constNimInt(0)
  g.nb = [
    llvm.constInt(g.primitives[tyBool], 0, llvm.False),
    llvm.constInt(g.primitives[tyBool], 1, llvm.False),
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
      runtimeVer, "", 0, DWARFEmissionFull, 0, False, False, nil, 0, nil, 0,
    )

    proc add(ty: TTypeKind, n: string, sz: uint64, enc: cuint) =
      g.dtypes[ty] = d.dIBuilderCreateBasicType(n, sz, enc)

    add(tyBool, "bool", 8, DW_ATE_boolean)
    add(tyChar, "char", 8, DW_ATE_unsigned_char)
    g.dtypes[tyPointer] =
      d.dIBuilderCreatePointerType(g.dtypes[tyChar], g.ptrBits, g.ptrBits, "pointer")

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

    g.dtypes[tyCString] =
      g.d.dIBuilderCreatePointerType(g.dtypes[tyChar], g.ptrBits, g.ptrBits, "")

    # Magic string, see https://groups.google.com/forum/#!topic/llvm-dev/1O955wQjmaQ
    g.m.addModuleFlag(
      ModuleFlagBehaviorWarning, "Dwarf Version", valueAsMetadata(g.constInt32(4))
    )
    g.m.addModuleFlag(
      ModuleFlagBehaviorWarning,
      "Debug Info Version",
      valueAsMetadata(g.constInt32(llvm.debugMetadataVersion().int32)),
    )

proc genMain(g: LLGen) =
  let
    llMainType = llvm.functionType(g.cintTy, [g.cintTy, g.ptrTy])
      # g.primitives[tyCString].pointerType()
    mainName =
      # TODO hackish way to not steal the `main` symbol!
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
        let types = [
          g.dtypes[tyInt32],
          g.dtypes[tyInt32],
          g.d.dIBuilderCreatePointerType(
            g.d.dIBuilderCreatePointerType(g.dtypes[tyChar], g.ptrBits, g.ptrBits, ""),
            g.ptrBits,
            g.ptrBits,
            "",
          ),
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
          f.ds,
          "argc",
          1,
          g.debugGetFile(g.config.projectMainIdx),
          0,
          g.dtypes[tyInt],
          false,
          0,
        )
        discard g.d.dIBuilderInsertDeclareAtEnd(
          argc, vd0, g.d.dIBuilderCreateExpression(nil, 0), dl, g.b.getInsertBlock()
        )

        let vd1 = g.d.dIBuilderCreateParameterVariable(
          g.f.ds,
          "argv",
          2,
          g.debugGetFile(g.config.projectMainIdx),
          0,
          g.d.dIBuilderCreatePointerType(g.dtypes[tyCString], g.ptrBits, g.ptrBits, ""),
          false,
          0,
        )
        discard g.d.dIBuilderInsertDeclareAtEnd(
          argv, vd1, g.d.dIBuilderCreateExpression(nil, 0), dl, g.b.getInsertBlock()
        )

      if g.config.target.targetOS != osStandAlone and g.config.selectedGC != gcNone:
        let bottom = g.localAlloca(g.primitives[tyInt], g.nn("bottom"))
        discard g.callCompilerProc("initStackBottomWith", [bottom])

      let cmdLine = g.m.getNamedGlobal("cmdLine")
      if cmdLine != nil:
        cmdLine.setLinkage(g.tgtExportLinkage)
        cmdLine.setInitializer(llvm.constNull(cmdLine.globalGetValueType()))
        discard g.b.buildStore(
          g.b.buildBitCast(main.getParam(1), cmdLine.globalGetValueType(), ""), cmdLine
        )

      let cmdCount = g.m.getNamedGlobal("cmdCount")
      if cmdCount != nil:
        cmdCount.setLinkage(g.tgtExportLinkage)
        cmdCount.setInitializer(llvm.constNull(cmdCount.globalGetValueType()))
        discard g.b.buildStore(main.getParam(0), cmdCount)

      for init in g.inits:
        discard g.b.buildCall2(init.globalGetValueType(), init, [], "")

      g.inits.reset()

    g.withBlock(g.section(f, secReturn)):
      if g.d != nil:
        let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, f.ds, nil)
        g.b.setCurrentDebugLocation2(dl)

      let pr = g.m.getNamedGlobal("nim_program_result")
      let ret =
        if pr != nil:
          # Unfortunately, the program result is wrongly declared as an int
          let prl = g.b.buildLoad2(pr.globalGetValueType(), pr)
          g.buildTruncOrExt(prl, g.cintTy, true)
        else:
          g.constCInt(0)
      discard g.b.buildRet(ret)

    if g.d != nil:
      let dl = g.lc.dIBuilderCreateDebugLocation(1, 1, f.ds, nil)
      g.b.setCurrentDebugLocation2(dl)

    g.finalize()

proc loadBase(g: LLGen) =
  let
    base =
      g.config.prefixDir.string / "../nlvm-lib/nlvmbase-$1-$2.ll" % [
        platform.CPU[g.config.target.targetCPU].name,
        platform.OS[g.config.target.targetOS].name,
      ]
    m = parseIRInContext(g.lc, base)

  if g.m.linkModules2(m) != 0:
    g.config.internalError("module link failed")

proc runOptimizers(g: LLGen) =
  let
    options = llvm.createPassBuilderOptions()
    kind =
      case g.lto
      of LtoKind.None:
        "default"
      # of LtoKind.Thin: "thinlto-pre-link"
      of LtoKind.Full:
        "lto-pre-link"
    level =
      if optOptimizeSize in g.config.options:
        "<Os>"
      elif optOptimizeSpeed in g.config.options:
        "<O3>"
      else:
        "<O0>"

    error = runPasses(g.m, kind & level, g.tm, options)

  if error != nil:
    let err = getErrorMessage(error)
    g.config.internalError($err)
    disposeErrorMessage(err)

  disposePassBuilderOptions(options)

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
    if g.m.printModuleToFile(outfile.string, cast[cstringArray](addr(err))) != 0:
      g.config.internalError($err)
    return

  let ofile =
    if optNoLinking in g.config.globalOptions:
      outFile.string
    else:
      g.config.completeCFilePath(AbsoluteFile(project & ".o")).string

  if g.lto != LtoKind.None:
    # TODO thin lto mode which "probably" requires a separate optimiser pass
    if g.m.writeBitcodeToFile(ofile) != 0:
      g.config.internalError("Could not write output to " & ofile)
      return
  else:
    if llvm.targetMachineEmitToFile(
      g.tm, g.m, ofile, llvm.ObjectFile, cast[cstringArray](addr(err))
    ) != 0:
      g.config.internalError($err)
      return

  if optNoLinking in g.config.globalOptions:
    return

  if g.config.selectedGC == gcBoehm:
    g.config.cLinkedLibs.add("gc")

  g.config.addExternalFileToLink(ofile.AbsoluteFile)

  lllink(g.config)

proc genForwardedProcs(g: LLGen) =
  # Forward declared proc:s lack bodies when first encountered, so they're given
  # a second pass here
  # Note: ``genFunctionWithBody`` may add to ``forwardedProcs``
  var todo: seq[PSym]
  while g.forwardedProcs.len > 0:
    let prc = g.forwardedProcs.pop()

    if sfForward in prc.flags:
      todo.add prc
      continue

    let
      ms = getModule(prc)
      m = g.modules[ms.position]

    g.withModule(m):
      discard g.genFunctionWithBody(prc)

  g.forwardedProcs = todo

proc runModule(g: LLGen, m: llvm.ModuleRef, fcns: openArray[string]) =
  let mainJD = orcLLJITGetMainJITDylib(g.orc)

  var tsCtx = orcCreateNewThreadSafeContext()
  let tsm = orcCreateNewThreadSafeModule(m, tsCtx)

  orcDisposeThreadSafeContext(tsCtx)
  let err = orcLLJITAddLLVMIRModule(g.orc, mainJD, tsm)
  if not err.isNil:
    g.config.internalError($err.getErrorMessage)
    orcDisposeThreadSafeModule(tsm)

  for name in fcns:
    var main: OrcJITTargetAddress
    let err = orcLLJITLookup(g.orc, addr main, name)
    if not err.isNil:
      g.config.internalError($err.getErrorMessage)
    cast[proc() {.cdecl.}](main)()

proc myProcess(b: PPassContext, n: PNode): PNode =
  let
    m = LLModule(b)
    g = m.g

  if g.config.skipCodegen(n) and not g.interactive():
    return n

  p("Process", n, 0)
  var transformedN = transformStmt(g.graph, g.idgen, m.sym, n)
  if sfInjectDestructors in m.sym.flags:
    transformedN = injectDestructorCalls(g.graph, g.idgen, m.sym, transformedN)

  g.withModule(m):
    g.withFunc(g.getInitFunc()):
      # Process the code with any top-level stuff going to the init func
      g.b.positionBuilderAtEnd(g.section(g.f, secLastBody))
      g.debugUpdateLoc(n)

      g.f.options = g.config.options
      g.genNode(transformedN)
      g.f.sections[secLastBody] = g.b.getInsertBlock()

  if g.interactive() and g.init.sections[secBody].getFirstInstruction() != nil and
      m.sym.name.s == "stdin":
    # When we've reached the stdin module, it means we're processing the
    # prompt line - every time there's some new "top-level" code, we'll flush it
    # to orc
    g.withModule(m):
      g.withFunc(g.init):
        g.withBlock(g.section(g.init, secReturn)):
          discard g.b.buildRetVoid()
        g.finalize()
        g.inits.add(g.init.f)

        g.init = nil

    g.loadBase()

    let
      inits = g.inits.mapIt($it.getValueName())
      lm = g.m
      lc = lm.getModuleDataLayout()

    # When the llvm module ownership is transferred to ORC, all cached values
    # become invalid
    g.inits.reset()
    g.nodeInfos.reset()
    g.typeInfos.reset()
    g.typeInfosV2.reset()
    g.strings.reset()
    g.cstrings.reset()

    g.round += 1
    g.m = moduleCreateWithNameInContext(
      cstring("." & m.sym.name.s & ".m." & $g.round), g.lc
    )

    g.m.setModuleDataLayout(lc)
    # g.m.setTarget(tmpm.getTarget())

    g.runModule(lm, inits)

  n

proc myClose(graph: ModuleGraph, b: PPassContext, n: PNode): PNode =
  if b == nil:
    return n

  let
    m = LLModule(b)
    g = m.g
    s = m.sym

  if sfCompileToCpp in s.flags:
    g.config.internalError("Compile-to-c++ not supported (did you use importcpp?)")

  if sfMainModule in s.flags:
    if {optGenStaticLib, optGenDynLib, optNoMain} * graph.config.globalOptions == {}:
      for i in countdown(high(graph.globalDestructors), 0):
        n.add graph.globalDestructors[i]

  if graph.config.skipCodegen(n) and not g.interactive():
    return n

  p("Close", n, 0)

  result = myProcess(b, n)

  g.genForwardedProcs()

  if g.init != nil:
    g.withModule(m):
      g.withFunc(g.init):
        g.withBlock(g.section(g.init, secReturn)):
          discard g.b.buildRetVoid()
        g.finalize()

    if sfSystemModule in s.flags:
      g.inits.insert(g.init.f, 0)
    else:
      g.inits.add(g.init.f)
    g.init = nil

  if sfMainModule notin s.flags:
    return

  g.withModule(m):
    let disp = generateMethodDispatchers(graph, g.idgen)
    for x in disp:
      discard g.genFunctionWithBody(x.sym)

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

  g.loadBase()

  if g.d != nil:
    g.d.dIBuilderFinalize()
    g.d.disposeDIBuilder()
    g.d = nil

  if optWasNimscript in g.config.globalOptions:
    g.runModule(g.m, ["main"])
  else:
    g.writeOutput(changeFileExt(g.config.projectFull, "").string)
    g.m.disposeModule()

proc myOpen(graph: ModuleGraph, s: PSym, idgen: IdGenerator): PPassContext =
  # In the C generator, a separate C file is generated for every module,
  # but the rules governing what goes into which module are shrouded by a
  # layer of globals and conditionals.
  # Rather than deciphering all that, we simply generate a single module
  # with all the code in it, like the JS generator does.

  # TODO: A total hack that needs to go away:
  case s.name.s
  of "pcre":
    graph.config.cLinkedLibs.add("pcre")
  of "sqlite3":
    graph.config.cLinkedLibs.add("sqlite3")
  of "openssl":
    graph.config.cLinkedLibs.add("ssl")
    graph.config.cLinkedLibs.add("crypto")

  if graph.backend == nil:
    var lto =
      if graph.config.useBuiltinLinker() and
          {optOptimizeSize, optOptimizeSpeed} * graph.config.options != {}:
        # TODO investigate thin LTO mode which is probably faster
        LtoKind.Full
      else:
        LtoKind.None

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
          llvmArgs.add(c[6 ..^ 1])

      # Hack: pass --passc flags to llvm - this doesn't capture flags put in
      # {.passc: ....} in modules because these do not register in the AST
      # ... and those from other compile option sources - this parsing might
      # be removed down the line if a more ambitious parsing regime is
      # implemented
      for c in @[graph.config.compileOptions] & graph.config.compileOptionsCmd:
        for part in parseCmdLine(c):
          # Hack that translates common "clang" flags to their llvm equivalents
          # - this needs updating eventually - it's neither complete nor sustainable
          if part.len == 0:
            continue

          if part.startsWith("-flto"):
            lto = LtoKind.Full
          elif part.startsWith("-fno-lto"):
            lto = LtoKind.None

          if part.startsWith("-march="):
            llvmArgs.add("-mcpu=" & part[7 ..^ 1])
          else:
            llvmArgs.add(part)

      if llvmArgs.len() > 1:
        let arr = allocCStringArray(llvmArgs)
        defer:
          deallocCStringArray(arr)

        parseCommandLineOptions(llvmArgs.len.cint, arr, "")

    # Before wasm32 was added as a CPU, we used nlvm.target - this is still
    # around in some tutorials so leave it around for now - perhaps it makes
    # sense to try to translate classic target triples to nim targets?

    let target = normalizeTargetTriple(
      if graph.config.existsConfigVar("nlvm.target"):
        let
          tmp = graph.config.getConfigVar("nlvm.target")
          (cpu, os) = parseTarget(tmp)
        graph.config.target.setTarget(os, cpu)
        tmp
      else:
        toTriple(graph.config.target.targetCPU, graph.config.target.targetOS)
    )

    var tr: llvm.TargetRef
    discard getTargetFromTriple(target, addr(tr), nil)

    # PIC/PIE is used by default when linking on certain platforms to enable address space randomization:
    # https://stackoverflow.com/q/43367427
    let
      reloc = llvm.RelocPIC
      cgl =
        if optOptimizeSpeed in graph.config.options:
          llvm.CodeGenLevelAggressive
        else:
          llvm.CodeGenLevelDefault
      tm = nimCreateTargetMachine(tr, target, cgl, reloc, llvm.CodeModelDefault)
      g = newLLGen(graph, idgen, target, tm, lto)

    graph.backend = g

  let g = LLGen(graph.backend)

  if g.modules.len <= s.position:
    g.modules.setLen(s.position + 1)

  g.modules[s.position] = LLModule(g: LLGen(graph.backend), sym: s)
  g.modules[s.position]

const llgenPass* = makePass(myOpen, myProcess, myClose)
