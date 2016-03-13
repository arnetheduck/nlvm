# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016
# See the LICENSE file for license info (doh!)

import
  os,
  strutils,
  sequtils

import
  compiler/ast,
  compiler/astalgo,
  compiler/bitsets,
  compiler/cgmeth,
  compiler/ccgutils,
  compiler/extccomp,
  compiler/magicsys,
  compiler/msgs,
  compiler/nimsets,
  compiler/options,
  compiler/passes,
  compiler/rodread,
  compiler/ropes,
  compiler/trees,
  compiler/types

import llvm/llvm

var
  llctxt = llvm.getGlobalContext()
  llIntBits = 64u
  llBoolType = llvm.int1Type()
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
  llVoidPtrType = llvm.pointerType(llvm.int8Type(), 0)  # no void* in llvm
  llCStringType = llvm.pointerType(llvm.int8Type(), 0)
  llGenericSeqType = llvm.structCreateNamed(llctxt, "TGenericSeq")
  llNimStringDesc = llvm.structCreateNamed(llctxt, "NimStringDesc")
  llProcPtrType = llVoidPtrType
  llClosureType = llvm.structCreateNamed(llctxt, "llnim.Closure")
  llNimStringDescPtr = llvm.pointerType(llnimStringDesc)
  llInitFuncType = llvm.functionType(llvm.voidType(), [])
  llMainType = llvm.functionType(llCIntType, [llCIntType, llvm.pointerType(llCStringType)])
  llPrintfType = llvm.functionType(llCIntType, [llCStringType], true)
  llStrlenType = llvm.functionType(llCSizeTType, [llCStringType])
  llJmpbuf = llvm.structCreateNamed(llctxt, "jmp_buf")
  llJmpbufp = llvm.pointerType(llJmpbuf)
  llSetjmpType = llvm.functionType(llCIntType, [llJmpbufp])
  llLongjmpType = llvm.functionType(llvm.voidType(), [llJmpbufp, llCIntType])
  llFabsType = llvm.functionType(llvm.doubleType(), [llvm.doubleType()])
  llMemsetType = llvm.functionType(llvm.voidType(), [llVoidPtrType, llvm.int8Type(), llvm.int64Type(), llvm.int32Type(), llvm.int1Type()])
  llMemCpyType = llvm.functionType(llvm.voidType(), [llVoidPtrType, llVoidPtrType, llvm.int64Type(), llvm.int32Type(), llvm.int1Type()])
  llErrnoType = llvm.functionType(llvm.pointerType(llCIntType), [])

llvm.structSetBody(llJmpbuf, [llvm.arrayType(llvm.int64Type(), 8), llvm.int32Type(), llvm.arrayType(llvm.int64Type(), 16)])
llvm.structSetBody(llGenericSeqType, [llIntType, llIntType])
llvm.structSetBody(llNimStringDesc, [llGenericSeqType, llvm.arrayType(llvm.int8Type(), 0)])
llvm.structSetBody(llClosureType, [llVoidPtrType, llVoidPtrType])

type NamedVal = tuple[name: string, value: llvm.ValueRef]

type LLScope = ref object
  vars: seq[NamedVal]
  n: PNode
  exit: llvm.BasicBlockRef
  goto: llvm.ValueRef

proc `$`(s: LLScope): string =
  $s.vars

type LLGenObj = object of TPassContext
  m: llvm.ModuleRef
  b: llvm.BuilderRef

  returnBlock: llvm.BasicBlockRef

  depth: int

  scope: seq[LLScope]

  init: llvm.ValueRef

  syms: seq[PSym]
  options: TOptions

type LLGen = ref LLGenObj

# Using an ugly global - haven't found a way to keep per-project data other than
# this...
var uglyGen: LLGen

# Helpers
proc llType(g: LLGen, typ: PType): llvm.TypeRef
proc fieldIndex(typ: PType, name: string): seq[int]
proc callErrno(g: LLGen): llvm.ValueRef
proc genFunction(g: LLGen, s: PSym): llvm.ValueRef
proc genFunctionWithBody(g: LLGen, s: PSym): llvm.ValueRef

# Magic expressions
proc genLengthOpenArrayExpr(g: LLGen, n: PNode): llvm.ValueRef
proc genLengthStrExpr(g: LLGen, n: PNode): llvm.ValueRef
proc genLengthArrayExpr(g: LLGen, n: PNode): llvm.ValueRef
proc genLengthSeqExpr(g: LLGen, n: PNode): llvm.ValueRef

# Expressions
proc genCallExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef
proc genExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef

# Statements
proc genCallStmt(g: LLGen, n: PNode)
proc genProcStmt(g: LLGen, n: PNode)
proc genStmt(g: LLGen, n: PNode)

proc scopePush(g: LLGen, n: PNode, exit: llvm.BasicBlockRef) =
  g.scope.add(
    LLScope(vars: @[], n: n, exit: exit))

proc scopeFind(g: LLGen, sym: PSym): LLScope =
  for i in 0..g.scope.len - 1:
    let scope = g.scope[g.scope.len - 1 - i]
    if scope.n != nil and scope.n.kind == nkSym and scope.n.sym == sym:
      result = scope
      return

proc scopePop(g: LLGen): LLScope {.discardable.} =
  g.scope.pop

proc scopeGet(g: LLGen, s: string): llvm.ValueRef =
  for x in 0..g.scope.len - 1:
    let scope = g.scope[g.scope.len - 1 - x]
    for y in scope.vars:
      if y[0] == s:
        return y[1]
  internalError("Symbol missing from scope: " & $s & ", " & $g.scope)

proc scopePut(g: LLGen, name: string, value: llvm.ValueRef) =
  var s = g.scope[g.scope.len - 1]
  for i, x in pairs(s.vars):
    if x[0] == name:
      # TODO using var s, why can't I replace through s?
      g.scope[g.scope.len - 1].vars[i][1] = value
      return

  g.scope[g.scope.len - 1].vars.add((name, value))

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
  let entry = f.getEntryBasicBlock()

  var first = entry.getFirstInstruction()
  # Skip instructions that must be first in a block
  while first != nil and
    first.getInstructionOpcode() in {llvm.PHI, llvm.LandingPad, llvm.Alloca}:
    first = first.getNextInstruction()
  if first != nil:
    b.positionBuilder(entry, first)
  else:
    b.positionBuilderAtEnd(entry)
  result = b.buildAlloca(typ, name)
  b.positionBuilderAtEnd(pre)

proc addPrivateConstant*(m: ModuleRef, ty: llvm.TypeRef, name: cstring): llvm.ValueRef =
  result = m.addGlobal(ty, name)
  result.setGlobalConstant(llvm.True)
  result.setUnnamedAddr(llvm.True)
  result.setLinkage(llvm.PrivateLinkage)

proc llName(s: PSym): string =
  if sfImportc in s.flags or sfExportc in s.flags:
    result = $s.loc.r
  elif sfCompilerProc in s.flags:
    result = s.name.s
  elif s.kind in {skType, skModule, skResult}:
    result = s.name.s
  else:
    result = s.name.s.toLower
    result &= "_"
    result &= $s.id

proc llName(typ: PType): string =
  if typ.sym != nil:
    let s = typ.sym
    if sfImportc in s.flags or sfExportc in s.flags:
      result = $s.loc.r
    elif sfCompilerProc in s.flags:
      result = s.name.s
    elif s.magic != mNone:
      result = s.name.s
    else:
      result = s.name.s
      result &= "_"
      result &= $typ.id
  else:
    case typ.kind
    of tyEmpty: result = "empty_" & $typ.id
    of tySequence: result = "seq." & typ.elemType.llName
    of tyPtr: result = "ptr." & typ.elemType.llName
    of tyRef: result = "ref." & typ.elemType.llName
    of tyGenericInst: result = "geni." & typ.lastSon.llName & "_" & $typ.id
    of tyGenericBody: result = "genb." & typ.lastSon.llName & "_" & $typ.id
    of tyTypeDesc: result = "td." & typ.lastSon.llName & "_" & $typ.id
    of tyTuple: result = "tuple_" & $typ.id
    of tyArray: result = "arr." & typ.elemType.llName
    of tyRange: result = "rng." & typ.elemType.llName
    of tyArrayConstr: result = "arr." & typ.elemType.llName
    of tyObject: result = "object_" & $typ.id
    else:
      result = "TY_" & $typ.id

proc `$`(n: PSym): string =
  if n == nil: return "PSym(nil)"
  result = n.llName & " " & $n.kind & " " & $n.magic & " " & $n.flags & " " & $n.loc.flags & " " & $n.info.line
  if n.typ != nil:
    result = result & " type " & $n.typ.kind
    if n.typ.sym != nil:
      result &= " " & n.typ.sym.name.s

proc `$`(t: PType): string =
  if t == nil: return "PType(nil)"
  result = $t.kind & " " & $t.flags & " "
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

proc `$`(n: PNode): string =
  if n == nil: return "PNode(nil)"
  result = $n.kind & " "
  case n.kind
  of nkCharLit..nkUInt64Lit: result &= $n.intVal
  of nkFloatLit..nkFloat128Lit: result &= $n.floatVal
  of nkStrLit..nkTripleStrLit: result &= (if n.strVal == nil: "" else: n.strVal)
  of nkSym: result &= $n.sym
  of nkIdent: result &= n.ident.s
  of nkProcDef:
    let s = n.sons[namePos].sym
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

proc constCString(m: llvm.ModuleRef, val: string): llvm.ValueRef =
  let ss = llvm.constString(val)
  let tmp = m.addPrivateConstant(ss.typeOf(), "")
  tmp.setInitializer(ss)
  result = constGEP(tmp, [constGEPIdx(0), constGEPIdx(0)])

proc constOffsetOf(g: LLGen, t: PType, name: string): llvm.ValueRef =
  let
    typ = t.skipTypes(abstractPtrs)
    llt = g.llType(typ)
    ind = fieldIndex(t, name)
    gep = constGEP(constNull(llt.pointerType()), (@[0] & ind).map(constGEPIdx))
  result = constPtrToInt(gep, llIntType)

proc bitSetToWord(s: TBitSet, size: int): int64 =
  result = 0
  for j in countup(0, size - 1):
    if j < len(s): result = result or `shl`(ze64(s[j]), j * 8)

proc constNimSet(n: PNode): llvm.ValueRef =
  assert n.kind == nkCurly
  assert (nfAllConst in n.flags)
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
  b.buildGEP(s, [constGEPIdx(0), constGEPIdx(0), constGEPIdx(0)], "")

proc buildNimSeqReservedGEP(b: BuilderRef, s: llvm.ValueRef): llvm.ValueRef =
  b.buildGEP(s, [constGEPIdx(0), constGEPIdx(0), constGEPIdx(1)], "")

proc buildNimSeqDataGEP(b: BuilderRef, s: llvm.ValueRef, idx: llvm.ValueRef = nil): llvm.ValueRef =
  var idx = if idx == nil: constGEPIdx(0) else: idx
  b.buildGEP(s, [constGEPIdx(0), constGEPIdx(1), idx], "")

proc buildNimIntExt(b: BuilderRef, v: llvm.ValueRef, unsigned: bool): llvm.ValueRef =
  let nt = llIntType
  result = if unsigned: b.buildZExt(v, nt, "") else: b.buildSExt(v, nt, "")

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
    result = b.buildTrunc(v, nt, "")
  else:  # vw < nw
    result = if unsigned: b.buildZExt(v, nt, "") else: b.buildSExt(v, nt, "")

proc buildTruncOrExt(b: llvm.BuilderRef, v: llvm.ValueRef, nt: llvm.TypeRef,
                     tk: TTypeKind): llvm.ValueRef =
  let unsigned = tk in {tyUInt..tyUInt64, tyChar, tySet}
  result = b.buildTruncOrExt(v, nt, unsigned)

proc needsBr(b: llvm.BuilderRef): bool =
  b.getInsertBlock().getBasicBlockTerminator() == nil

proc buildBrFallthrough(b: llvm.BuilderRef, next: llvm.BasicBlockRef) =
  # Add a br to the next block if the current block is not already terminated
  if b.needsBr:
    discard b.buildBr(next)

proc buildSetMask(b: llvm.BuilderRef, t: llvm.TypeRef, ix: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let mask =
    case size
    of 1: 7
    of 2: 15
    of 4: 31
    of 8: 63
    else: 7
  var shift = b.buildAnd(ix, llvm.constInt(ix.typeOf(), mask.culonglong, llvm.False), "")
  if t != shift.typeOf():
    shift = b.buildTruncOrExt(shift, t, false)
  result = b.buildShl(llvm.constInt(t, 1, llvm.False), shift, "")

proc buildSetGEPMask(b: llvm.BuilderRef, vx, ix: llvm.ValueRef): tuple[gep, mask: llvm.ValueRef] =
  let idx = b.buildUDiv(ix, llvm.constInt(ix.typeOf(), 8, llvm.False), "")
  let gep = b.buildGEP(vx, [constGEPIdx(0), idx], "")

  let mask = b.buildSetMask(llvm.int8Type(), ix, 1)
  result = (gep: gep, mask: mask)

proc buildLoadValue(b: BuilderRef, v: llvm.ValueRef): llvm.ValueRef =
  if v.typeOf().isArrayPtr():
    result = b.buildGEP(v, [constGEPIdx(0), constGEPIdx(0)])
  else:
    result = b.buildLoad(v, "")

proc buildNot(b: BuilderRef, v: llvm.ValueRef): llvm.ValueRef =
  b.buildXor(v, llvm.constInt(v.typeOf(), (-1).culonglong, llvm.False), "")

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef
proc llTupleType(g: LLGen, typ: PType): llvm.TypeRef
proc llProcType(g: LLGen, typ: PType, closure = false): llvm.TypeRef

proc llType(g: LLGen, typ: PType): llvm.TypeRef =
  case typ.kind
  of tyBool: result = llBoolType
  of tyChar: result = llCharType
  of tyNil: result = llVoidPtrType
  of tyGenericInst: result = g.llType(typ.lastSon)
  of tyDistinct: result = g.llType(typ.lastSon)
  of tyEnum: result = llvm.intType(getSize(typ).cuint * 8)
  of tyArray, tyArrayConstr:
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
    result = llvm.pointerType(g.llType(et))
  of tySequence:  # TODO generate from nim types
    let name = typ.llName
    var st = g.m.getTypeByName(name)
    if st == nil:
      st = structCreateNamed(llctxt, name)  # TODO ends up in module?
      if typ.elemType.kind == tyEmpty:
        st.structSetBody([llGenericSeqType])
      else:
        st.structSetBody([llGenericSeqType, llvm.arrayType(g.llType(typ.elemType), 0)])
    result = llvm.pointerType(st)
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
  else:
    internalError("Unhandled type " & $typ.kind)

proc llMagicType(g: LLGen, name: string): llvm.TypeRef =
  g.llType(magicsys.getCompilerProc(name).typ)

proc addStructFields(g: LLGen, elements: var seq[TypeRef], n: PNode, typ: PType) =
  p("addStructFields", n, g.depth)
  case n.kind
  of nkRecList:
    for child in n:
      g.addStructFields(elements, child, typ)
  of nkRecCase: # TODO Unionize
    if n.sons[0].kind != nkSym: internalError(n.info, "addStructFields")
    g.addStructFields(elements, n.sons[0], typ)

    for i in countup(1, sonsLen(n) - 1):
      case n.sons[i].kind
      of nkOfBranch, nkElse:
        let k = lastSon(n.sons[i])
        if k.kind != nkSym:
           g.addStructFields(elements, k, typ)

        else:
          g.addStructFields(elements, k, typ)
      else: internalError("addStructFields")
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
  else: result = nil

proc headerTypeIndex(typ: PType, name: string): seq[int] =
  case typ.llName
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
    result = @[names.find(name)]

proc llStructType(g: LLGen, typ: PType): llvm.TypeRef =
  if typ == nil:
    return

  var typ = typ
  if typ.kind == tyString:
    typ = magicsys.getCompilerProc("NimStringDesc").typ

  let name = typ.llName
  result = g.m.getTypeByName(name)
  if result != nil:
    return

  result = g.headerType(name)
  if result != nil:
    return

  p("llStructType " & (if typ.sym != nil: $typ.sym else: "nil"), typ, g.depth)

  # Create struct before setting body in case it's recursive
  result = structCreateNamed(llctxt, name)

  var elements = newSeq[TypeRef]()

  let super = typ.sons[0]
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

  let name = typ.llName
  result = g.m.getTypeByName(name)
  if result != nil:
    return

  # Create struct before setting body in case it's recursive
  result = structCreateNamed(llctxt, name)

  var elements = newSeq[TypeRef]()
  for t in typ.sons:
    elements.add(g.llType(t))

  p("llTupleType " & $name & " " & $elements, typ, g.depth)

  result.structSetBody(elements)

proc hasTypeField(typ: PType): bool {.inline.} =
  if typ.kind != tyObject: return false
  if typ.isPureObject(): return false
  if tfFinal in typ.flags and typ.sons[0] == nil: return false
  return true

proc genTypeInfoInit(g: LLGen, t: PType, ntlt, lt: llvm.TypeRef,
                     baseVar, nodeVar, finalizerVar, markerVar,
                     deepcopyVar: llvm.ValueRef): llvm.ValueRef =
  let sizeVar = if lt == nil: constNimInt(0) else: llvm.sizeOfX(lt)

  let kind =
    if t.kind == tyObject and not t.hasTypeField(): tyPureObject
    elif t.kind == tyProc and t.callConv == ccClosure: tyTuple
    else: t.kind
  let kindVar = llvm.constInt(llvm.int8Type(), ord(kind).culonglong, llvm.False)

  var flags = 0
  if not containsGarbageCollectedRef(t): flags = flags or 1
  if not canFormAcycle(t) or (t.kind == tyProc and t.callConv == ccClosure):
    flags = flags or 2
  if t.kind == tyEnum:
    var hasHoles = false
    for i in 0..t.sonsLen-1:
      let n = t.n.sons[i].sym
      # why isn't tfEnumHasHoles always set? is it ever set at all?
      if n.position != i or tfEnumHasHoles in t.flags:
        hasHoles = true
    if hasHoles:
      # C gen overwrites flags in this case!
      flags = 1 shl 2

  let flagsVar = llvm.constInt(llvm.int8Type(), flags.culonglong, llvm.False)

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
    [constInt8(1), offset, typeInfo, g.m.constCString(name),
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
    let tmp = g.m.addPrivateConstant(nodesType, "")
    tmp.setInitializer(constArray(tnnp, nodes))
    nodesVal = constBitCast(tmp, els[5])

  result = llvm.constNamedStruct(tnn,
    [constInt8(2), constNull(els[1]), constNull(els[2]), constNull(els[3]),
    constInt64(nodes.len), nodesVal])

proc constNimNodeCase(g: LLGen, offset, typeInfo: llvm.ValueRef, name: string, nodes: openarray[llvm.ValueRef]): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    tnnp = tnn.pointerType()
    els = tnn.getStructElementTypes()

  var nodesVal: llvm.ValueRef

  if nodes.len == 0:
    nodesVal = constNull(els[5])
  else:
    let nodesType = llvm.arrayType(tnnp, nodes.len.cuint)
    let tmp = g.m.addPrivateConstant(nodesType, "")
    tmp.setInitializer(constArray(tnnp, nodes))
    nodesVal = constBitCast(tmp, els[5])

  result = llvm.constNamedStruct(tnn,
    [constInt8(3), offset, typeInfo, g.m.constCString(name),
    constInt64(nodes.len), nodesVal])

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

    for i in 1..n.sonsLen-1:
      let b = n[i]
      let bi = g.genObjectNodeInfo(t, b.lastSon, suffix & "." & $i)
      case b.kind
      of nkOfBranch:
        for j in 0..b.sonsLen() - 2:
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
      g.constOffsetOf(t, field.llName), g.genTypeInfo(field.typ),
      field.name.s, fields)

  of nkSym:
    let field = n.sym
    result = g.constNimNodeSlot(
      g.constOffsetOf(t, field.llName), g.genTypeInfo(field.typ), field.name.s)

  else: internalError(n.info, "genObjectNodeInfoInit")

proc genObjectNodeInfo(g: LLGen, t: PType, n: PNode, suffix: string): llvm.ValueRef =
  let name = ".nodeinfo." & t.llName & suffix
  result = g.m.getNamedGlobal(name)
  if result != nil:
    return

  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  result.setInitializer(g.genObjectNodeInfoInit(t, n, suffix))

proc genTupleNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  let tnn = g.llMagicType("TNimNode")

  var fields: seq[ValueRef] = @[]

  let l = t.sonsLen
  for i in 0..l-1:
    let
      name = ".nodeinfo." & t.llName & "." & $i
      field = g.m.addPrivateConstant(tnn, name)
      offset = constPtrToInt(constGEP(constNull(g.llType(t).pointerType()),
        [constGEPIdx(0), constGEPIdx(i)]), int64Type())
      fieldInit = g.constNimNodeSlot(offset, g.genTypeInfo(t.sons[i]),
        "Field" & $i)

    field.setInitializer(fieldInit)
    fields.add(field)

  result = g.constNimNodeList(fields)

proc genTupleNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let name = ".nodeinfo." & t.llName
  result = g.m.getNamedGlobal(name)
  if result != nil:
    return

  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  result.setInitializer(g.genTupleNodeInfoInit(t))

proc genEnumNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  let
    tnn = g.llMagicType("TNimNode")
    els = tnn.getStructElementTypes()

  let l = t.n.sonsLen

  var fields: seq[ValueRef] = @[]
  for i in 0..l-1:
    let
      name = ".nodeinfo." & t.llName & "." & $i
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
  let name = ".nodeinfo." & t.llName
  result = g.m.getNamedGlobal(name)
  if result != nil:
    return

  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
  result.setInitializer(g.genEnumNodeInfoInit(t))

proc genSetNodeInfoInit(g: LLGen, t: PType): llvm.ValueRef =
  result = g.constNimNodeNone(firstOrd(t).int)

proc genSetNodeInfo(g: LLGen, t: PType): llvm.ValueRef =
  let name = ".nodeinfo." & t.llName
  result = g.m.getNamedGlobal(name)
  if result != nil:
    return

  let tnn = g.llMagicType("TNimNode")

  result = g.m.addPrivateConstant(tnn, name)
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
  if t.kind in {tyArray, tyArrayConstr}:
    result = g.genTypeInfo(t.sons[1])
  elif t.kind in {tySequence, tyRef, tyPtr, tyRange, tySet, tyObject} and
    t.sons.len > 0 and t.sons[0] != nil:
    result = g.genTypeInfo(t.sons[0])

  if result == nil:
    result = g.llMagicType("TNimType").pointerType().constNull()

proc genTypeInfo(g: LLGen, t: PType): llvm.ValueRef =
  var t = t.skipTypes({tyDistinct, tyGenericBody, tyGenericParam})

  let name = ".typeinfo." & t.llName
  result = g.m.getNamedGlobal(name)
  if result != nil:
    return

  p("genTypeInfo", t, g.depth + 1)

  let
    ntlt = g.llMagicType("TNimType")
    lt = if t.kind == tyEmpty: nil else: g.llType(t)
    els = ntlt.getStructElementTypes()

  # TODO should use externally initialized global for types from other
  # modules, or?
  # TODO unnamed_addr?
  result = g.m.addPrivateConstant(ntlt, name)

  var finalizerVar, markerVar, deepcopyVar: llvm.ValueRef
  let baseVar = g.genTypeInfoBase(t)
  let nodeVar = g.genNodeInfo(t)

  if finalizerVar == nil: finalizerVar = llvm.constNull(els[5])
  if markerVar == nil: markerVar = llvm.constNull(els[6])
  if deepcopyVar == nil: deepcopyVar = llvm.constNull(els[7])

  result.setInitializer(g.genTypeInfoInit(t, ntlt, lt, baseVar,
    nodeVar, finalizerVar, markerVar, deepcopyVar))

iterator procParams(typ: PType): PNode =
  for a in typ.n.sons[1..typ.n.sons.len - 1]:
    let param = a.sym
    if isCompileTimeOnly(param.typ): continue
    yield a

proc procParams(typ: PType): seq[PNode] =
  accumulateResult(procParams(typ))

proc llProcParamType(g: LLGen, t: PType): llvm.TypeRef =
  case skipTypes(t, abstractInst).kind:
  of tyArray, tyArrayConstr, tyOpenArray, tyVarargs: result = llvm.pointerType(g.llType(t))
  of tyObject: result = llvm.pointerType(g.llType(t))
  of tyTuple: result = llvm.pointerType(g.llType(t))
  of tySet:
    let size = getSize(t).cuint
    result = if size <= 8: g.llType(t)
             else: llvm.pointerType(g.llType(t))
  of tyProc:
    result = if t.callConv == ccClosure: llvm.pointerType(g.llType(t))
             else: g.llType(t)
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

proc fieldIndexRecs(n: PNode, name: string, start: var int): seq[int] =
  case n.kind
  of nkRecList:
    for s in n:
      result = fieldIndexRecs(s, name, start)
      if result.len > 0: return

  of nkRecCase:
    if n[0].kind != nkSym: internalError(n.info, "fieldIndex " & $n[0].kind)

    if n[0].sym.llName == name: return @[start]
    inc(start)
    for j in 1..<n.sonsLen:
      result = fieldIndexRecs(n[j].lastSon, name, start)
      if result.len > 0: return
  of nkSym:
    if n.sym.llName == name: return @[start]
    inc(start)
  else:
    internalError(n.info, "Unhandled field index")
  return @[]

proc fieldIndex(typ: PType, name: string): seq[int] =
  var typ = skipTypes(typ, abstractPtrs)

  result = headerTypeIndex(typ, name)
  if result != nil: return

  var start = 0
  if typ.kind != tyTuple:
    if typ.sons != nil and typ.sons[0] != nil:
      let s = fieldIndex(typ.sons[0], name)
      if s.len > 0:
        return @[0] & s
      start = 1
    elif not ((typ.sym != nil and sfPure in typ.sym.flags) or tfFinal in typ.flags):
      # TODO skip m_type in a nicer way
      start = 1

  let n = typ.n
  result = fieldIndexRecs(n, name, start)

proc mtypeIndex(typ: PType): seq[llvm.ValueRef] =
  let zero = constGEPIdx(0)
  result = @[zero]
  var t = skipTypes(typ, abstractInst)
  while t.kind in {tyVar, tyPtr, tyRef}:
    result = result & @[zero]
    t = skipTypes(t.lastSon, typedescInst)

  while t.kind == tyObject and t.sons[0] != nil:
    result = result & @[zero]
    t = skipTypes(t.sons[0], typedescInst)

  if not t.hasTypeField():
    # TODO why is this check in the generator???
    internalError("no 'of' operator available for pure objects")
  result = result & @[zero]

proc setElemIndex(g: LLGen, typ: PType, x: llvm.ValueRef): llvm.ValueRef =
  if firstOrd(typ) > 0:
    g.b.buildSub(x, constInt(x.typeOf(), firstOrd(typ).culonglong, llvm.False), "")
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

proc preCast(g: LLGen, ax: llvm.ValueRef, t: PType, lt: llvm.TypeRef = nil): llvm.ValueRef =
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
    result = g.b.buildBitcast(ax, lt, "")
    return

  if ltk == IntegerTypeKind and atk == IntegerTypeKind and
      at.getIntTypeWidth() < lt.getIntTypeWidth():
    # unsigned
    result = g.b.buildSExt(ax, lt, "")
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
  of "errno": result = g.callErrno()
  else:
    result = g.m.getNamedGlobal(name)
    if result != nil: return
    result = g.m.addGlobal(t, name)

proc genGlobal(g: LLGen, s: PSym): llvm.ValueRef =
  let name = s.llName
  result = g.m.getNamedGlobal(name)
  if result == nil:
    let t = g.llType(s.typ)
    result = g.m.addGlobal(t, s.llName)

    if sfImportc in s.flags:
      result.setLinkage(llvm.ExternalLinkage)
    elif sfExportc in s.flags:
      result.setLinkage(llvm.CommonLinkage)
      result.setInitializer(llvm.constNull(t))
    else:
      result.setLinkage(llvm.PrivateLinkage)
      result.setInitializer(llvm.constNull(t))

proc callCompilerProc(g: LLGen, n: string, args: openarray[llvm.ValueRef]): llvm.ValueRef =
  let sym = magicsys.getCompilerProc(n)
  if sym == nil: internalError("compiler proc not found: " & n)

  let f = g.genFunctionWithBody(sym)
  let pars = sym.typ.procParams()

  var i = 0
  var args = @args
  for param in pars:
    let v = args[i]

    let a = g.preCast(v, param.typ, g.llProcParamType(param.typ))
    args[i] = a

    if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      i += 1
    i += 1

  result = g.b.buildCall(f, args)

proc callMemset(g: LLGen, p, v, len: llvm.ValueRef) =
  let f = g.m.getOrInsertFunction("llvm.memset.p0i8.i64", llMemsetType)

  discard g.b.buildCall(f, [p, v, len, constInt32(0), constInt1(false)])

proc callMemcpy(g: LLGen, tgt, src, len: llvm.ValueRef) =
  let f = g.m.getOrInsertFunction("llvm.memcpy.p0i8.p0i8.i64", llMemcpyType)

  discard g.b.buildCall(f, [tgt, src, len, constInt32(0), constInt1(false)])

proc callCtpop(g: LLGen, v: llvm.ValueRef, size: BiggestInt): llvm.ValueRef =
  let
    bits = (size * 8).cuint
    t = llvm.functionType(llvm.intType(bits), [llvm.intType(bits)])
    f = g.m.getOrInsertFunction("llvm.ctpop.i" & $bits, t)

  result = g.b.buildCall(f, [v])

proc callErrno(g: LLGen): llvm.ValueRef =
  # on linux errno is a function, so we call it here. not at all portable.
  let f = g.m.getOrInsertFunction("__errno_location", llErrnoType)

  result = g.b.buildCall(f, [])

proc genObjectInit(g: LLGen, t: PType, v: llvm.ValueRef) =
  case analyseObjectWithTypeField(t)
  of frNone:
    discard
  of frHeader:
    let tgt = g.b.buildGEP(v, mtypeIndex(t))
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

  let x = g.callCompilerProc("newObj", [ti, so])
  result = g.b.buildBitcast(x, t.pointerType(), "")
  g.genObjectInit(objType, result)

template withRangeItems(il: expr, n: PNode, body: stmt) {.immediate.} =
  let
    ax = g.genExpr(s[0], true)
    bx = g.genExpr(s[1], true)
  if ax == nil or bx == nil: return

  let b = g.setElemIndex(typ, bx)

  # loop! init idx
  let i = g.b.localAlloca(ax.typeOf(), "")
  discard g.b.buildStore(ax, i)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let rcmp = f.appendBasicBlock("rng.cmp")
  let rloop = f.appendBasicBlock("rng.loop")
  let rdone = f.appendBasicBlock("rng.done")

  # jump to comparison
  discard g.b.buildBr(rcmp)

  # check idx
  g.b.positionBuilderAtEnd(rcmp)
  let il = g.b.buildLoad(i, "")
  let cond = g.b.buildICmp(llvm.IntSLE, il, b, "")
  discard g.b.buildCondBr(cond, rloop, rdone)

  # loop body
  g.b.positionBuilderAtEnd(rloop)

  body

  # inc idx
  let next = g.b.buildAdd(il, constInt(il.typeOf(), 1, llvm.False), "")
  discard g.b.buildStore(next, i)
  # back to comparison
  discard g.b.buildBr(rcmp)

  # continue at the end
  g.b.positionBuilderAtEnd(rdone)

proc genFunction(g: LLGen, s: PSym): llvm.ValueRef =
  let name = s.llName
  result = g.m.getNamedFunction(name)

  if result != nil:
    return

  let ft = g.llProcType(s.typ, s.typ.callConv == ccClosure)
  let f = g.m.addFunction(name, ft)

  result = f

proc genFunctionWithBody(g: LLGen, s: PSym): llvm.ValueRef =
  result = g.genFunction(s)

  if result.countBasicBlocks() != 0:
    return  # already has body

  if sfForward in s.flags:
    return

  # Because we generate only one module, we can tag all functions internal
  result.setLinkage(llvm.InternalLinkage)

  let ft = result.typeOf().getElementType()

  var i = 1
  var lastIsArr = false

  # generate body
  let oldBlock = g.b.getInsertBlock()
  let oldScope = g.scope
  let oldReturn = g.returnBlock
  let oldOptions = g.options

  let entry = llvm.appendBasicBlock(result, "entry")
  g.returnBlock = llvm.appendBasicBlock(result, "return")

  g.scope = @[]
  g.scopePush(s.ast, g.returnBlock)

  g.options = s.options

  for arg in llvm.params(result):
    while i < s.typ.n.len and isCompileTimeOnly(s.typ.n[i].sym.typ):
      i += 1
    if i >= s.typ.len:
      if s.typ.callConv == ccClosure:
        arg.setValueName("ClEnv")
        g.scopePut("ClEnv", arg)

        continue
      else:
        internalError("Params missing in function call: " & $s)
        return
    let param = s.typ.n[i]

    p("a", param, g.depth + 1)
    p("a", param.typ, g.depth + 2)
    if lastIsArr:
      arg.setValueName(param.sym.llName & "len")
      lastIsArr = false
      i += 1
      g.scopePut(param.sym.llName & "len", arg)
    else:
      arg.setValueName(param.sym.llName)
      g.scopePut(param.sym.llName, arg)

      if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
        lastIsArr = true
      else:
        i += 1

  g.b.positionBuilderAtEnd(entry)

  # Scope for local variables
  g.scopePush(s.ast, g.returnBlock)

  if tfCapturesEnv in s.typ.flags:
    let ls = lastSon(s.ast[paramsPos])
    let lx = g.b.buildBitCast(g.scopeGet("ClEnv"), g.llType(ls.sym.typ), ls.sym.llName)

    g.scopePut(ls.sym.llName, lx)

  let rt = ft.getReturnType()
  if rt.getTypeKind() != llvm.VoidTypeKind:
    let res = g.b.localAlloca(rt, "result")
    discard g.b.buildStore(llvm.constNull(rt), res)
    g.scopePut("result", res)
    g.genStmt(s.ast.sons[bodyPos])

    g.b.buildBrFallthrough(g.returnBlock)
    g.b.positionBuilderAtEnd(g.returnBlock)

    discard g.b.buildRet(g.b.buildLoad(res, ""))
  else:
    g.genStmt(s.ast.sons[bodyPos])

    g.b.buildBrFallthrough(g.returnBlock)
    g.b.positionBuilderAtEnd(g.returnBlock)

    discard g.b.buildRetVoid()

  let fn = g.returnBlock.getBasicBlockParent()
  if fn.getLastBasicBlock() != g.returnBlock:
    g.returnBlock.moveBasicBlockAfter(fn.getLastBasicBlock())

  g.options = oldOptions
  g.returnBlock = oldReturn
  g.scope = oldScope
  g.b.positionBuilderAtEnd(oldBlock)

proc genCallArgs(g: LLGen, n: PNode, fxt: llvm.TypeRef, ftyp: PType): seq[llvm.ValueRef] =
  var args: seq[ValueRef] = @[]

  var i = 0

  let parTypes = fxt.getParamTypes()
  for i in 1..n.sonsLen - 1:
    let p = n[i]
    let pr = if p.kind == nkHiddenAddr: p[0] else: p
    if i >= ftyp.n.len: # varargs like printf, for example
      let v = g.genExpr(pr, true)
      args.add(v)
      continue

    let param = ftyp.n[i]
    if param.typ.isCompileTimeOnly(): continue

    let pt = parTypes[args.len]

    var v: llvm.ValueRef
    if skipTypes(param.typ, {tyVar}).kind in {tyOpenArray, tyVarargs}:
      var len: llvm.ValueRef
      case skipTypes(pr.typ, abstractVar).kind
      of tyString, tySequence:
        v = g.genExpr(pr, true)
        if v == nil: return
        len = g.b.buildNimSeqLenGEP(v)
        len = g.b.buildLoad(len, "")
        len = g.b.buildZExt(len, llIntType, "")
        v = g.b.buildNimSeqDataGEP(v)
      of tyOpenArray, tyVarargs:
        v = g.genExpr(p, param.typ.kind == tyVar)
        if v == nil: return
        len = g.scopeGet(pr.sym.llName & "len")
      of tyArray, tyArrayConstr:
        v = g.genExpr(p, false)
        if v == nil: return
        len = constNimInt(lengthOrd(pr.typ).int)
      else:
        v = g.genExpr(p, not g.llPassAsPtr(param.typ))
        if v == nil: return
        internalError(n.info, "Unhandled length: " & $pr.typ)

      if v.typeOf() != g.llProcParamType(param.typ):
        v = g.b.buildBitCast(v, g.llProcParamType(param.typ), "")

      args.add(v)
      args.add(len)
    else:
      v = g.genExpr(p, not g.llPassAsPtr(param.typ))
      if v == nil: return

      # We need to use the type from the function, because with multimethods,
      # it looks like the type in param.typ changes during compilation!
      # seen with tmultim1.nim
      v = g.preCast(v, param.typ, pt)
      args.add(v)

  result = args

proc genCall(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let nf = n[namePos]
  let typ = nf.typ.skipTypes(abstractInst)

  var fx = g.genExpr(nf, true)
  if fx == nil: return

  let nfpt = g.llProcType(typ)
  let nft = llvm.pointerType(nfpt)
  let retty = nfpt.getReturnType()

  var callres: llvm.ValueRef

  if nf.typ.callConv == ccClosure:
    let prc = g.b.buildExtractValue(fx, 0, "")
    let env = g.b.buildExtractValue(fx, 1, "")

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let clonil = f.appendBasicBlock("closure.nil")
    let cloenv = f.appendBasicBlock("closure.env")
    let cloend = f.appendBasicBlock("closure.end")

    let cmp = g.b.buildICmp(llvm.IntEQ, env, llvm.constNull(env.typeOf()), "")
    discard g.b.buildCondBr(cmp, clonil, cloenv)

    g.b.positionBuilderAtEnd(clonil)

    if prc.typeOf().getElementType().getTypeKind() != llvm.FunctionTypeKind:
      fx = g.b.buildBitcast(prc, nft, "")

    let args = g.genCallArgs(n, fx.typeOf().getElementType(), typ)
    let res = g.b.buildCall(fx, args)
    discard g.b.buildBr(cloend)

    g.b.positionBuilderAtEnd(cloenv)

    let cft = llvm.pointerType(g.llProcType(nf.typ, true))
    let cfx = g.b.buildBitcast(prc, cft, "")

    let args2 = g.genCallArgs(n, cfx.typeOf().getElementType(), typ)
    let clargs = args2 & @[env]
    let cres = g.b.buildCall(cfx, clargs)

    discard g.b.buildBr(cloend)

    g.b.positionBuilderAtEnd(cloend)

    if retty.getTypeKind() != llvm.VoidTypeKind:
      callres = g.b.buildPHI(res.typeOf(), "")
      callres.addIncoming([res, cres], [clonil, cloenv])
  else:
    if fx.typeOf().getElementType().getTypeKind() != llvm.FunctionTypeKind:
      fx = g.b.buildBitcast(fx, nft, "")

    let args = g.genCallArgs(n, fx.typeOf().getElementType(), typ)
    callres = g.b.buildCall(fx, args)

  if retty.getTypeKind() != llvm.VoidTypeKind and not load and
      nf.typ.sons[0].kind != tyRef:
    # if the originator of the call wants a pointer, we'll have
    # to create one for them - this is interesting for example
    # when a struct is returned "by value"
    result = g.b.localAlloca(retty, "")
    discard g.b.buildStore(callres, result)
  else:
    result = callres

proc genMagicCall(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let s = n[namePos].sym
  if lfNoDecl notin s.loc.flags:
    discard g.genFunctionWithBody(magicsys.getCompilerProc($s.loc.r))

  result = g.genCall(n, load)

proc genAssignment(g: LLGen, v, t: llvm.ValueRef, typ: PType) =
  let
    vt = v.typeOf()
    tt = t.typeOf()

  if tt.getTypeKind() != llvm.PointerTypeKind:
    internalError("Ptr required in genAssignment: " & $t)

  let tet = tt.getElementType()

  if typ.kind == tyString:
    # TODO always copying string is not necessary
    let sx = g.callCompilerProc("copyString", [v])
    if tet != sx.typeOf():
      internalError("string required in genAssignment: " & $t)

    discard g.b.buildStore(sx, t)
  elif typ.kind == tyTuple:
    for i in 0..<typ.sonsLen:
      let
        tp = g.b.buildGEP(t, [constGEPIdx(0), constGEPIdx(i.int32)])
        vp = g.b.buildExtractValue(v, i.cuint, "")
      g.genAssignment(vp, tp, typ.sons[i])
  elif typ.kind == tySequence:
    discard g.callCompilerProc("genericSeqAssign", [t, v, g.genTypeInfo(typ)])
  elif tet == vt:
    discard g.b.buildStore(v, t)
  elif tet.getTypeKind() == llvm.ArrayTypeKind and tt == vt:
    let
      tp = g.b.buildBitCast(g.b.buildGEP(t, [constGEPIdx(0), constGEPIdx(0)]), llVoidPtrType, "")
      vp = g.b.buildBitCast(g.b.buildGEP(v, [constGEPIdx(0), constGEPIdx(0)]), llVoidPtrType, "")
      so = tet.getElementType().sizeOfX()
      n = g.b.buildMul(so, constNimInt(tet.getArrayLength().int), "")
    g.callMemcpy(tp, vp, n)
  elif tet.getTypeKind() == llvm.ArrayTypeKind and
      vt.getTypeKind() == llvm.PointerTypeKind and
      vt.getElementType() == tet.getElementType():
    let
      tp = g.b.buildBitCast(g.b.buildGEP(t, [constGEPIdx(0), constGEPIdx(0)]), llVoidPtrType, "")
      vp = g.b.buildBitCast(v, llVoidPtrType, "")
      so = tet.getElementType().sizeOfX()
      n = g.b.buildMul(so, constNimInt(tet.getArrayLength().int), "")
    g.callMemcpy(tp, vp, n)
  elif typ.kind == tyProc:
    if typ.callConv == ccClosure:
      let clprc = g.b.buildGEP(t, [constGEPIdx(0), constGEPIdx(0)])
      let clv = g.b.buildBitcast(v, clprc.typeOf().getElementType(), "")
      discard g.b.buildStore(clv, clprc)
    else:
      discard g.b.buildStore(g.b.buildBitcast(v, tet, ""), t)
  elif tet == llCStringType and vt.isNimSeqLike:
    # nim string to cstring
    discard g.b.buildStore(g.b.buildNimSeqDataGEP(v), t)
  else:
    # TODO this is almost certainly wrong but handles cases like type aliases
    # which currently are broken in nlvm
    discard g.b.buildStore(v, g.b.buildBitCast(t, vt.pointerType(), ""))

proc isDeepConstExprLL(n: PNode, strict: bool): bool =
  # strict means that anything involving a separate constant and a cast is
  # not allowed - as would happen with strings for example in a var string
  # initializer
  case n.kind
  of nkCharLit..nkFloat128Lit, nkNilLit: result = true
  of nkStrLit..nkTripleStrLit: result = not strict
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = n[1].isDeepConstExprLL(strict)
  of nkCurly: result = nfAllConst in n.flags
  of nkBracket:
    for s in n.sons[0..<n.sonsLen]:
      if not s.isDeepConstExprLL(strict): return false
    result = n.typ.kind in {tyArray, tyArrayConstr}
  of nkObjConstr:
    let typ = n.typ.skipTypes(abstractInst)
    # TODO inheritance can be done, just being lazy...
    if typ.sons[0] != nil: return false

    for s in n.sons[1..<n.sonsLen]:
      if not s.isDeepConstExprLL(strict): return false
    result = true
  else: result = false

proc isConstString(n: PNode): bool =
  case n.kind
  of nkStrLit..nkTripleStrLit: result = true
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = n[1].isConstString()
  else: result = false

proc genConstInitializer(g: LLGen, n: PNode): llvm.ValueRef =
  case n.kind
  of nkCharLit..nkFloat128Lit, nkNilLit: result = g.genExpr(n, true)
  of nkStrLit..nkTripleStrLit: result = constNimString(n)
  of nkExprEqExpr, nkExprColonExpr, nkHiddenStdConv, nkHiddenSubConv:
    result = g.genConstInitializer(n[1])
  of nkCurly: result = if nfAllConst in n.flags: constNimSet(n) else: nil
  of nkBracket:
    let t = g.llType(n.typ)
    if n.typ.kind in {tyArray, tyArrayConstr} and n.isDeepConstExprLL(false):
      var vals: seq[llvm.ValueRef] = @[]
      for s in n.sons:
        if s.isConstString():
          # Need pointer for string literals
          vals.add(g.genExpr(s, true))
        else:
          vals.add(g.genConstInitializer(s))
      result = constArray(t.getElementType(), vals)
    else:
      result = nil
  of nkObjConstr:
    let t = g.llType(n.typ)
    if n.isDeepConstExprLL(false):
      var vals = newSeq[llvm.ValueRef](t.countStructElementTypes())
      for i in 0..<vals.len:
        vals[i] = llvm.constNull(t.structGetTypeAtIndex(i.cuint))
      for i in 1..<n.len:
        if n[i].isConstString():
          # Need pointer for string literals
          vals[i-1] = g.genExpr(n[i], true)
        else:
          vals[i-1] = g.genConstInitializer(n[i])
      result = constNamedStruct(t, vals)
    else:
      result = nil
  else: result = nil

# Magic expressions
proc genLowExpr(g: LLGen, n: PNode): llvm.ValueRef =
  case skipTypes(n[1].typ, abstractVar).kind
  of tyOpenArray, tyVarargs: result = constNimInt(0)
  of tyCString, tyString: result = constNimInt(0)
  of tySequence: result = constNimInt(0)
  of tyArray, tyArrayConstr: result = constNimInt(0)
  else: internalError(n.info, "genLowExpr " & $n[1])

proc genHighExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let typ = skipTypes(n[1].typ, abstractVar)
  case typ.kind
  of tyOpenArray, tyVarargs: result = g.genLengthOpenArrayExpr(n)
  of tyCString, tyString: result = g.genLengthStrExpr(n)
  of tySequence: result = g.genLengthSeqExpr(n)
  of tyArray, tyArrayConstr: result = constNimInt(lengthOrd(typ).int)
  else: internalError(n.info, "genHighExpr " & $n[1].typ)

  if result == nil: return

  result = g.b.buildSub(result, llvm.constInt(result.typeOf(), 1, llvm.False), "")

proc genSizeOfExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let t = n[1].typ.skipTypes({tyTypeDesc})

  result = llvm.sizeOfX(g.llType(t))

proc genOfExpr(g: LLGen, n: PNode): llvm.ValueRef =
  var ax = g.genExpr(n[1], false)
  if ax == nil: return

  # C generator does this extra dereferencing here - looks odd that it's needed
  # but there are test case fails because of it - needs more investigation
  var t = skipTypes(n[1].typ, abstractInst)
  while t.kind in {tyVar, tyPtr, tyRef}:
    t = skipTypes(t.lastSon, typedescInst)

  while ax.typeOf().getElementType().getTypeKind() == llvm.PointerTypeKind:
    ax = g.b.buildLoad(ax, "")

  let m_type = g.b.buildLoad(g.b.buildGEP(ax, mtypeIndex(t)), "")
  let typ = n[2].typ.skipTypes(typedescPtrs)
  # TODO nil check if n is a pointer
  result = g.callCompilerProc("isObj", [m_type, g.genTypeInfo(typ)])

proc genUnaryLtExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  result = g.b.buildSub(ax, llvm.constInt(ax.typeOf(), 1, False), "")

proc genOrdExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  result = g.b.buildTruncOrExt(ax, g.llType(n.typ), true)

proc genLengthOpenArrayExpr(g: LLGen, n: PNode): llvm.ValueRef =
  # openarray must be a parameter so we should find it in the scope
  let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]
  result = g.scopeGet(s.sym.llName & "len")

proc genLengthStrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  if n[1].typ.kind == tyCString:
    let v = g.genExpr(n[1], true)
    if v == nil: return

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let lfalse = f.appendBasicBlock("len.load")
    let ldone = f.appendBasicBlock("len.done")

    # nil check
    let cond = g.b.buildICmp(llvm.IntEQ, v, llvm.constNull(v.typeOf()), "")

    discard g.b.buildCondBr(cond, ldone, lfalse)

    # load length if v is not nil
    g.b.positionBuilderAtEnd(lfalse)
    let strlen = g.m.getOrInsertFunction("strlen", llStrlenType)

    let v1 = g.b.buildCall(strlen, [v])
    discard g.b.buildBr(ldone)

    g.b.positionBuilderAtEnd(ldone)

    # 0 from pre block or loaded length
    let phi = g.b.buildPHI(llIntType, "")

    let v0 = constNimInt(0)
    phi.addIncoming([v0, v1], [pre, lfalse])

    result = phi
  elif n[1].typ.kind == tyString:
    result = genLengthSeqExpr(g, n)
  else:
    internalError(n.info, "Unhandled kind " & $n[1].typ)

proc genLengthArrayExpr(g: LLGen, n: PNode): llvm.ValueRef =
  result = constNimInt(lengthOrd(n.typ).int)

proc genLengthSeqExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let v = g.genExpr(n[1], true)
  if v == nil: return

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let lfalse = f.appendBasicBlock("len.load")
  let ldone = f.appendBasicBlock("len.done")

  # nil check
  let cond = g.b.buildICmp(llvm.IntEQ, v, llvm.constNull(v.typeOf()), "")

  discard g.b.buildCondBr(cond, ldone, lfalse)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(lfalse)
  let gep = g.b.buildNimSeqLenGEP(v)
  let v1 = g.b.buildLoad(gep, "")
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(llIntType, "")

  let v0 = constNimInt(0)
  phi.addIncoming([v0, v1], [pre, lfalse])

  result = phi

proc genCardExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    typ = skipTypes(n[1].typ, abstractVar)

  let size = getSize(typ)
  if size <= 8:
    result = g.callCtpop(ax, size)
    result = g.b.buildTruncOrExt(result, g.llType(n.typ), typ.kind)
  else:
    # loop! init idx
    let i = g.b.localAlloca(llIntType, "card.idx")
    discard g.b.buildStore(constNimInt(0), i)

    let tot = g.b.localAlloca(g.llType(n.typ), "card.tot")
    discard g.b.buildStore(constNull(tot.typeOf().getElementType()), tot)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock("card.cmp")
    let rloop = f.appendBasicBlock("card.loop")
    let rdone = f.appendBasicBlock("card.done")

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, "")
    let cond = g.b.buildICmp(llvm.IntSLT, il, constNimInt(size.int), "")
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let ai = g.b.buildLoad(g.b.buildGEP(ax, [il]), "")
    let a = g.callCtpop(ai, 1)
    let b = g.b.buildLoad(tot, "")
    let c = g.b.buildAdd(g.b.buildTruncOrExt(a, b.typeOf(), typ.kind), b, "")
    discard g.b.buildStore(c, tot)

    # inc idx    g.b.positionBuilderAtEnd(rinc)
    let next = g.b.buildAdd(il, constInt(il.typeOf(), 1, llvm.False), "")
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildLoad(tot, "")

proc genChrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  let t = g.llType(n.typ)
  if t != ax.typeOf():
    result = g.b.buildTrunc(ax, t, "")
  else:
    result = ax

proc genBinOpExpr(g: LLGen, n: PNode, op: Opcode, unsigned = false): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  var
    a = ax
    b = bx
  if a.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      b.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      a.typeOf().getIntTypeWidth() != b.typeOf().getIntTypeWidth():
    # TODO should probably extend to the biggest of the two, rather than
    # full int
    a = g.b.buildNimIntExt(ax, unsigned)
    b = g.b.buildNimIntExt(bx, unsigned)

  result = g.b.buildTruncOrExt(g.b.buildBinOp(op, a, b, ""), g.llType(n.typ), unsigned)

proc genFBinOpExpr(g: LLGen, n: PNode, op: Opcode, unsigned = false): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  result = g.b.buildBinOp(op, ax, bx, "")

  if optNaNCheck in g.options:
    discard g.callCompilerProc("nanCheck", [result])
  if optInfCheck in g.options:
    discard g.callCompilerProc("infCheck", [result])

proc genMinMaxIExpr(g: LLGen, n: PNode, op: IntPredicate): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let cmp = g.b.buildICmp(op, ax, bx, "")
  return g.b.buildSelect(cmp, ax, bx, "")

proc genMinMaxFExpr(g: LLGen, n: PNode, op: RealPredicate): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let cmp = g.b.buildFCmp(op, ax, bx, "")
  return g.b.buildSelect(cmp, ax, bx, "")

proc genICmpExpr(g: LLGen, n: PNode, op: IntPredicate, unsigned = false): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  var
    a = ax
    b = bx
  if a.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      b.typeOf().getTypeKind() == llvm.IntegerTypeKind and
      a.typeOf().getIntTypeWidth() != b.typeOf().getIntTypeWidth():
    # TODO should probably extend to the biggest of the two, rather than
    # full int
    a = g.b.buildNimIntExt(ax, unsigned)
    b = g.b.buildNimIntExt(bx, unsigned)

  result = g.b.buildICmp(op, g.preCast(a, n[2].typ), g.preCast(b, n[1].typ), "")

proc genFCmpExpr(g: LLGen, n: PNode, op: RealPredicate): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  result = g.b.buildFCmp(op, ax, bx, "")

proc genEqProcExpr(g: LLGen, n: PNode): llvm.ValueRef =
  if n[1].typ.callConv == ccClosure:
    let
      ax = g.genExpr(n[1], false)
      bx = g.genExpr(n[2], false)
    if ax == nil or bx == nil: return

    let
      zero = constGEPIdx(0)
      one = constGEPIdx(1)
      a0 = g.b.buildGEP(ax, [zero, zero])
      a1 = g.b.buildGEP(ax, [zero, one])
      b0 = g.b.buildGEP(bx, [zero, zero])
      b1 = g.b.buildGEP(bx, [zero, one])

    let x0 = g.b.buildICmp(llvm.IntEQ, g.b.buildLoad(a0, ""),
                           g.b.buildLoad(b0, ""), "")
    let x1 = g.b.buildICmp(llvm.IntEQ, g.b.buildLoad(a1, ""),
                           g.b.buildLoad(b1, ""), "")

    result = g.b.buildAnd(x0, x1, "")
  else:
    let
      ax = g.genExpr(n[1], true)
      bx = g.genExpr(n[2], true)
    if ax == nil or bx == nil: return

    result = g.b.buildICmp(llvm.IntEQ, ax,
                           bx, "")

proc genUnaryMinusExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  return g.b.buildSub(llvm.constInt(ax.typeOf(), 0, llvm.False),
                      ax, "")

proc genAbsIExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let linv = f.appendBasicBlock("abs.inv")
  let ldone = f.appendBasicBlock("abs.done")

  # nil check
  let cmp = g.b.buildICmp(llvm.IntSGE, ax, llvm.constInt(ax.typeOf(), 0, llvm.False), "")

  discard g.b.buildCondBr(cmp, ldone, linv)

  # load length if v is not nil
  g.b.positionBuilderAtEnd(linv)
  let v1 = g.b.buildSub(llvm.constInt(ax.typeOf(), 0, llvm.False),
                        ax, "")
  discard g.b.buildBr(ldone)

  g.b.positionBuilderAtEnd(ldone)

  # 0 from pre block or loaded length
  let phi = g.b.buildPHI(ax.typeOf(), "")

  phi.addIncoming([ax, v1], [pre, linv])

  result = phi

proc genNotExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  result = g.b.buildNot(ax)

proc genUnaryMinusF64Expr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  result = g.b.buildFSub(llvm.constReal(ax.typeOf(), 0.0.cdouble), ax, "")

proc genAbsF64Expr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  let fabs = g.m.getOrInsertFunction("llvm.fabs.f64", llFabsType)

  result = g.b.buildCall(fabs, [ax])

proc genZeExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let a = g.genExpr(n[1], true)
  if a == nil: return

  result = g.b.buildZExt(a, g.llType(n.typ), "")

proc genToUExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let a = g.genExpr(n[1], true)
  if a == nil: return

  result = g.b.buildTrunc(a, g.llType(n.typ), "")

proc genToFloatExpr(g: LLGen, n:PNode): llvm.ValueRef =
  let a = g.genExpr(n[1], true)
  if a == nil: return

  result = g.b.buildSIToFP(a, g.llType(n.typ), "")

proc genToIntExpr(g: LLGen, n:PNode): llvm.ValueRef =
  let a = g.genExpr(n[1], true)
  if a == nil: return

  result = g.b.buildFPToSI(a, g.llType(n.typ), "")

proc genToStrExpr(g: LLGen, n: PNode, f: string): llvm.ValueRef =
  let a = g.genExpr(n[1], true)
  if a == nil: return

  result = g.callCompilerProc(f, [a])

proc genStrToStrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  return g.genExpr(n[1], true)

proc genEnumToStrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  let typ = skipTypes(n[1].typ, abstractVarRange)
  result = g.callCompilerProc("reprEnum", [g.b.buildTruncOrExt(ax, llIntType, false), g.genTypeInfo(typ)])

proc genAndOrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let aonext = f.appendBasicBlock("andor.next")
  let aoend = f.appendBasicBlock("andor.end")

  if n[0].sym.magic == mAnd:
    discard g.b.buildCondBr(ax, aonext, aoend)
  else:
    discard g.b.buildCondBr(ax, aoend, aonext)

  g.b.positionBuilderAtEnd(aonext)

  let bx = g.genExpr(n[2], true)
  let bend = g.b.getInsertBlock()
  discard g.b.buildBr(aoend)

  g.b.positionBuilderAtEnd(aoend)

  if bx == nil: return

  let phi = g.b.buildPHI(ax.typeOf(), "")

  phi.addIncoming([ax, bx], [pre, bend])

  result = phi

proc genEqStrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  result = g.callCompilerProc("eqStrings", [ax, bx])

proc genLeStrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let cmp = g.callCompilerProc("cmpStrings", [ax, bx])
  result = g.b.buildICmp(llvm.IntSLE, cmp, constNimInt(0), "")

proc genLtStrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let cmp = g.callCompilerProc("cmpStrings", [ax, bx])
  result = g.b.buildICmp(llvm.IntSLT, cmp, constNimInt(0), "")

proc genSetCmpExpr(g: LLGen, strict: bool, n: PNode): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
  if ax == nil or bx == nil: return

  let size = getSize(typ)
  if size <= 8:
    let b = g.b.buildNot(bx)
    let ab = g.b.buildAnd(ax, b, "")
    let le = g.b.buildICmp(llvm.IntEQ, ab, llvm.constInt(ab.typeOf(), 0, llvm.False), "")
    if strict:
      let ne = g.b.buildICmp(llvm.IntNE, ax, bx, "")
      result = g.b.buildAnd(le, ne, "")
    else:
      result = le
  else:
    let o = g.b.localAlloca(llvm.int1Type(), "")
    discard g.b.buildStore(constInt1(true), o)

    # loop! init idx
    let i = g.b.localAlloca(llIntType, "")
    discard g.b.buildStore(constNimInt(0), i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock("set.cmp")
    let rloop = f.appendBasicBlock("set.loop")
    let rinc = f.appendBasicBlock("set.inc")
    let rfalse = f.appendBasicBlock("set.false")
    let rdone = f.appendBasicBlock("set.done")

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, "")
    let cond = g.b.buildICmp(llvm.IntSLT, il, constNimInt(size.int), "")
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let al = g.b.buildLoad(g.b.buildGEP(ax, [il]), "")
    let bl = g.b.buildLoad(g.b.buildGEP(bx, [il]), "")
    let b = g.b.buildNot(bl)

    var cont: llvm.ValueRef
    let ab = g.b.buildAnd(al, b, "")
    let le = g.b.buildICmp(llvm.IntEQ, ab, llvm.constInt(ab.typeOf(), 0, llvm.False), "")
    if strict:
      let ne = g.b.buildICmp(llvm.IntNE, ax, bx, "")
      cont = g.b.buildAnd(le, ne, "")
    else:
      cont = le

    discard g.b.buildCondBr(cont, rinc, rfalse)

    g.b.positionBuilderAtEnd(rinc)

    # inc idx
    let next = g.b.buildAdd(il, constInt(il.typeOf(), 1, llvm.False), "")
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    g.b.positionBuilderAtEnd(rfalse)
    discard g.b.buildStore(constInt1(false), o)
    discard g.b.buildBr(rdone)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildLoadValue(o)

proc genEqSetExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
  if ax == nil or bx == nil: return

  let size = getSize(typ)
  if size <= 8:
    result = g.b.buildICmp(llvm.IntEQ, ax, bx, "")
  else:
    # loop! init idx
    let i = g.b.localAlloca(llIntType, "")
    discard g.b.buildStore(constNimInt(0), i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock("set.cmp")
    let rloop = f.appendBasicBlock("set.loop")
    let rne = f.appendBasicBlock("set.ne")
    let rinc = f.appendBasicBlock("set.inc")
    let rdone = f.appendBasicBlock("set.done")

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, "")
    let cond = g.b.buildICmp(llvm.IntSLT, il, constNimInt(size.int), "")
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let a = g.b.buildLoad(g.b.buildGEP(ax, [il]), "")
    let b = g.b.buildLoad(g.b.buildGEP(bx, [il]), "")

    let cmp = g.b.buildICmp(llvm.IntEQ, a, b, "")
    discard g.b.buildCondBr(cmp, rinc, rne)
    g.b.positionBuilderAtEnd(rne)

    discard g.b.buildBr(rdone)

    # inc idx
    g.b.positionBuilderAtEnd(rinc)
    let next = g.b.buildAdd(il, constInt(il.typeOf(), 1, llvm.False), "")
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildPHI(llBoolType, "")
    result.addIncoming([constInt1(false), constInt1(true)], [rne, rcmp])

proc genSetBinOpExpr(g: LLGen, op: llvm.Opcode, invert: bool, n: PNode): llvm.ValueRef =
  let
    ax = g.genExpr(n[1], true)
    bx = g.genExpr(n[2], true)
    typ = skipTypes(n[1].typ, abstractVar)
  if ax == nil or bx == nil: return

  let size = getSize(typ)
  if size <= 8:
    let
      a = ax
      b = bx
    let s = if invert: g.b.buildXor(b, llvm.constInt(b.typeOf(), (-1).culonglong, llvm.False), "")
            else: b
    result = g.b.buildBinOp(op, a, s, "")
  else:
    let tmp = g.b.localAlloca(g.llType(typ), "")

    # loop! init idx
    let i = g.b.localAlloca(llIntType, "")
    discard g.b.buildStore(constNimInt(0), i)

    let pre = g.b.getInsertBlock()
    let f = pre.getBasicBlockParent()

    let rcmp = f.appendBasicBlock("set.cmp")
    let rloop = f.appendBasicBlock("set.loop")
    let rdone = f.appendBasicBlock("set.done")

    # jump to comparison
    discard g.b.buildBr(rcmp)

    # check idx
    g.b.positionBuilderAtEnd(rcmp)
    let il = g.b.buildLoad(i, "")
    let cond = g.b.buildICmp(llvm.IntSLT, il, constNimInt(size.int), "")
    discard g.b.buildCondBr(cond, rloop, rdone)

    # loop body
    g.b.positionBuilderAtEnd(rloop)

    let a = g.b.buildLoad(g.b.buildGEP(ax, [il]), "")
    let b = g.b.buildLoad(g.b.buildGEP(bx, [il]), "")

    let s = if invert: g.b.buildXor(b, llvm.constInt(b.typeOf(), (-1).culonglong, llvm.False), "")
            else: b
    let o = g.b.buildBinOp(op, a, s, "")

    let p = g.b.buildGEP(tmp, [constGEPIdx(0), il])
    discard g.b.buildStore(o, p)

    # inc idx
    let next = g.b.buildAdd(il, constInt(il.typeOf(), 1, llvm.False), "")
    discard g.b.buildStore(next, i)
    # back to comparison
    discard g.b.buildBr(rcmp)

    # continue at the end
    g.b.positionBuilderAtEnd(rdone)

    result = g.b.buildLoadValue(tmp)

proc genConStrStrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  # First, find out total length of all strings
  var tgtlen = constNimInt(0)

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]

  for s in n.sons[1..n.sonsLen - 1]:
    let sx = g.genExpr(s, true)
    exprs.add(sx)
    if sx == nil: continue

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit..nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.b.buildLoad(g.b.buildNimSeqLenGEP(sx), "")
      tgtlen = g.b.buildAdd(tgtlen, slen, "")

  if constlen > 0:
    tgtlen = g.b.buildAdd(tgtlen, constNimInt(constlen), "")

  # Allocate result
  let tgt = g.callCompilerProc("rawNewString", [tgtlen])

  # Copy data
  for i in 1..n.sonsLen - 1:
    let sx = exprs[i - 1]
    if sx == nil: continue

    let s = n[i]

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [tgt, sx])
    else:
      discard g.callCompilerProc("appendString", [tgt, sx])

  result = tgt

proc genDotDotExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  # a compiler magic with implementation in system.nim.. hm.
  var s = n.sons[namePos].sym
  let f = g.genFunctionWithBody(s)
  # will be generated in multiple modules, so hide it..
  f.setLinkage(llvm.PrivateLinkage)

  result = g.genCall(n, load)

proc genInSetExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let typ = skipTypes(n[1].typ, abstractVar)
  let size = getSize(typ)

  if size <= 8:
    let
      ax = g.genExpr(n[1], true)
      bx = g.genExpr(n[2], true)
    if ax == nil or bx == nil: return

    let b = g.b.buildSetMask(ax.typeOf(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildAnd(ax, b, "")
    result = g.b.buildICmp(llvm.IntNE, res, llvm.constInt(ax.typeOf(), 0, llvm.False), "")
  else:
    let
      ax = g.genExpr(n[1], false)
      bx = g.genExpr(n[2], true)
    if ax == nil or bx == nil: return

    let (gep, mask) = g.b.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildAnd(a, mask, "")
    result = g.b.buildICmp(llvm.IntNE, res, llvm.constInt(a.typeOf(), 0, llvm.False), "")

proc genReprExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let t = skipTypes(n[1].typ, abstractVarRange)
  case t.kind
  of tyInt..tyInt64, tyUInt..tyUInt64:
    let ax = g.genExpr(n[1], true)
    result = g.callCompilerProc("reprInt", [g.b.buildTruncOrExt(ax, llIntType, t.kind)])
  of tyFloat..tyFloat128:
    let ax = g.genExpr(n[1], true)
    result = g.callCompilerProc("reprFloat", [ax])
  of tyBool:
    let ax = g.genExpr(n[1], true)
    result = g.callCompilerProc("reprBool", [ax])
  of tyChar:
    let ax = g.genExpr(n[1], true)
    result = g.callCompilerProc("reprChar", [ax])
  of tyEnum, tyOrdinal:
    let ax = g.genExpr(n[1], true)
    result = g.callCompilerProc("reprEnum", [ax, g.genTypeInfo(t)])
  of tyString:
    let ax = g.genExpr(n[1], true)
    result = g.callCompilerProc("reprStr", [ax])
  of tySet:
    let ax = g.genExpr(n[1], false)
    result = g.callCompilerProc("reprSet", [ax, g.genTypeInfo(t)])
  of tyOpenArray, tyVarargs:
    let s = if n[1].kind == nkHiddenDeref: n[1][0] else: n[1]
    let l = g.scopeGet(s.sym.llName & "len")
    let ax = g.genExpr(n[1], false)
    result = g.callCompilerProc("reprOpenArray", [ax, l, g.genTypeInfo(t.elemType)])
  of tyCString, tyArray, tyArrayConstr, tyRef, tyPtr, tyPointer, tyNil,
     tySequence:
    let ax = g.genExpr(n[1], n[1].typ.kind in {tyRef, tyPtr, tyVar})
    result = g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])
  of tyEmpty:
    localError(n.info, "'repr' doesn't support 'void' type")
  else:
    let ax = g.genExpr(n[1], false)
    result = g.callCompilerProc("reprAny", [ax, g.genTypeInfo(t)])

proc genIsNilExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[1], true)
  if ax == nil: return

  let typ = skipTypes(n[1].typ, abstractPtrs)
  if typ.kind == tyProc and typ.callConv == ccClosure:
    result = g.b.buildICmp(llvm.IntEQ, g.b.buildExtractValue(ax, 0, ""), llvm.constNull(llVoidPtrType), "")
  else:
    result = g.b.buildICmp(llvm.IntEQ, ax, llvm.constNull(ax.typeOf()), "")

proc genArrToSeq(g: LLGen, n: PNode): llvm.ValueRef =
  let
    t = skipTypes(n.typ, abstractInst)
    l = int(lengthOrd(skipTypes(n[1].typ, abstractInst)))
    ti = g.genTypeInfo(t)
    tmp = g.callCompilerProc("newSeq", [ti, constNimInt(l)])

  result = g.b.buildBitcast(tmp, g.llType(t), "")

  let ax = g.genExpr(n[1], true)
  if ax == nil:
    return

  for i in 0..<l:
    let tgt = g.b.buildNimSeqDataGEP(result, constGEPIdx(i))
    let src = g.b.buildGEP(ax, [constGEPIdx(i)])
    g.genAssignment(g.b.buildLoad(src, ""), tgt, t.elemType)

proc genGetTypeInfoExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let typ = n[1].typ.skipTypes(abstractVarRange)
  result = g.genTypeInfo(typ)

proc genMagicExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var op = n.sons[0].sym.magic
  p("genMagicExpr " & $op, n.sons[0], g.depth + 1)
  case op
  of mLow: result = g.genLowExpr(n)
  of mHigh: result = g.genHighExpr(n)
  of mSizeOf: result = g.genSizeOfExpr(n)
  of mOf: result = g.genOfExpr(n)
  of mUnaryLt: result = g.genUnaryLtExpr(n)
  of mOrd: result = g.genOrdExpr(n)
  of mLengthOpenArray: result = g.genLengthOpenArrayExpr(n)
  of mLengthStr: result = g.genLengthStrExpr(n)
  of mLengthArray: result = g.genLengthArrayExpr(n)
  of mLengthSeq: result = g.genLengthSeqExpr(n)
  of mCard: result = g.genCardExpr(n)
  of mChr: result = g.genChrExpr(n)
  of mAddI: result = g.genBinOpExpr(n, llvm.Add)
  of mSubI: result = g.genBinOpExpr(n, llvm.Sub)
  of mMulI: result = g.genBinOpExpr(n, llvm.Mul)
  of mDivI: result = g.genBinOpExpr(n, llvm.SDiv)
  of mModI: result = g.genBinOpExpr(n, llvm.SRem) # TODO verify
  of mSucc: result = g.genBinOpExpr(n, llvm.Add)
  of mPred: result = g.genBinOpExpr(n, llvm.Sub)
  of mAddF64: result = g.genFBinOpExpr(n, llvm.FAdd)
  of mSubF64: result = g.genFBinOpExpr(n, llvm.FSub)
  of mMulF64: result = g.genFBinOpExpr(n, llvm.FMul)
  of mDivF64: result = g.genFBinOpExpr(n, llvm.FDiv)
  of mShrI: result = g.genBinOpExpr(n, llvm.LShr) # TODO verify
  of mShlI: result = g.genBinOpExpr(n, llvm.Shl)
  of mBitandI: result = g.genBinOpExpr(n, llvm.And)
  of mBitorI: result = g.genBinOpExpr(n, llvm.Or)
  of mBitxorI: result = g.genBinOpExpr(n, llvm.Xor)
  of mMinI: result = g.genMinMaxIExpr(n, llvm.IntSLE) # sign
  of mMaxI: result = g.genMinMaxIExpr(n, llvm.IntSGE) # sign
  of mMinF64: result = g.genMinMaxFExpr(n, llvm.RealOLE) # ordered?
  of mMaxF64: result = g.genMinMaxFExpr(n, llvm.RealOGE) # ordered?
  of mAddU: result = g.genBinOpExpr(n, llvm.Add, true)
  of mSubU: result = g.genBinOpExpr(n, llvm.Sub, true)
  of mMulU: result = g.genBinOpExpr(n, llvm.Mul, true)
  of mDivU: result = g.genBinOpExpr(n, llvm.UDiv, true)
  of mModU: result = g.genBinOpExpr(n, llvm.URem, true) # TODO verify
  of mEqI: result = g.genICmpExpr(n, llvm.IntEQ)
  of mLeI: result = g.genICmpExpr(n, llvm.IntSLE)
  of mLtI: result = g.genICmpExpr(n, llvm.IntSLT)
  of mEqF64: result = g.genFCmpExpr(n, llvm.RealOEQ) # TODO ordered?
  of mLeF64: result = g.genFCmpExpr(n, llvm.RealOLE) # TODO ordered?
  of mLtF64: result = g.genFCmpExpr(n, llvm.RealOLT) # TODO ordered?
  of mLeU: result = g.genICmpExpr(n, llvm.IntULE, true)
  of mLtU: result = g.genICmpExpr(n, llvm.IntULT, true)
  of mLeU64: result = g.genICmpExpr(n, llvm.IntULE, true)
  of mLtU64: result = g.genICmpExpr(n, llvm.IntULT, true)
  of mEqEnum: result = g.genICmpExpr(n, llvm.IntEQ)
  of mLeEnum: result = g.genICmpExpr(n, llvm.IntULE, true) # TODO underlying
  of mLtEnum: result = g.genICmpExpr(n, llvm.IntULT, true) # TODO underlying
  of mEqCh: result = g.genICmpExpr(n, llvm.IntEQ)
  of mLeCh: result = g.genICmpExpr(n, llvm.IntULE, true)
  of mLtCh: result = g.genICmpExpr(n, llvm.IntULT, true)
  of mEqB: result = g.genICmpExpr(n, llvm.IntEQ)
  of mLeB: result = g.genICmpExpr(n, llvm.IntULE, true)
  of mLtB: result = g.genICmpExpr(n, llvm.IntULT, true)
  of mEqRef: result = g.genICmpExpr(n, llvm.IntEQ)
  of mEqUntracedRef: result = g.genICmpExpr(n, llvm.IntEQ)
  of mLePtr: result = g.genICmpExpr(n, llvm.IntULE, true)
  of mLtPtr: result = g.genICmpExpr(n, llvm.IntULT, true)
  of mEqCString: result = g.genICmpExpr(n, llvm.IntEQ)
  of mXor: result = g.genICmpExpr(n, llvm.IntNE)
  of mEqProc: result = g.genEqProcExpr(n)
  of mUnaryMinusI, mUnaryMinusI64: result = g.genUnaryMinusExpr(n)
  of mAbsI: result = g.genAbsIExpr(n)
  of mNot: result = g.genNotExpr(n)
  of mBitnotI: result = g.genNotExpr(n)
  of mUnaryMinusF64: result = g.genUnaryMinusF64Expr(n)
  of mAbsF64: result = g.genAbsF64Expr(n)
  of mZe8ToI..mZeIToI64: result = g.genZeExpr(n)
  of mToU8, mToU16, mToU32: result = g.genToUExpr(n)
  of mToFloat, mToBiggestFloat: result = g.genToFloatExpr(n)
  of mToInt, mToBiggestInt: result = g.genToIntExpr(n)
  of mCharToStr: result = g.genToStrExpr(n, "nimCharToStr")
  of mBoolToStr: result = g.genToStrExpr(n, "nimBoolToStr")
  of mIntToStr: result = g.genToStrExpr(n, "nimIntToStr")
  of mInt64ToStr: result = g.genToStrExpr(n, "nimInt64ToStr")
  of mFloatToStr: result = g.genToStrExpr(n, "nimFloatToStr")
  of mCStrToStr: result = g.genToStrExpr(n, "cstrToNimstr")
  of mStrToStr: result = g.genStrToStrExpr(n)
  of mEnumToStr: result = g.genEnumToStrExpr(n)
  of mAnd, mOr: result = g.genAndOrExpr(n)
  of mEqStr: result = g.genEqStrExpr(n)
  of mLeStr: result = g.genLeStrExpr(n)
  of mLtStr: result = g.genLtStrExpr(n)
  of mEqSet: result = g.genEqSetExpr(n)
  of mLeSet: result = g.genSetCmpExpr(false, n)
  of mLtSet: result = g.genSetCmpExpr(true, n)
  of mMulSet: result = g.genSetBinOpExpr(llvm.And, false, n)
  of mPlusSet: result = g.genSetBinOpExpr(llvm.Or, false, n)
  of mMinusSet: result = g.genSetBinOpExpr(llvm.And, true, n)
  of mSymDiffSet: result = g.genSetBinOpExpr(llvm.Xor, false, n)
  of mConStrStr: result = g.genConStrStrExpr(n)
  of mDotDot: result = g.genDotDotExpr(n, load)
  of mInSet: result = g.genInSetExpr(n)
  of mRepr: result = g.genReprExpr(n)
  of mIsNil: result = g.genIsNilExpr(n)
  of mArrToSeq: result = g.genArrToSeq(n)
  of mCopyStr, mCopyStrLast, mNewString, mNewStringOfCap, mParseBiggestFloat:
    result = g.genMagicCall(n, load)
  of mGetTypeInfo: result = g.genGetTypeInfoExpr(n)
  else: internalError(n.info, "Unhandled genMagicExpr: " & $op)

# Expressions
proc genSymExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var s = n.sym
  case s.kind
  of skConst, skVar, skLet, skParam, skTemp, skResult, skForVar:
    var v: llvm.ValueRef
    if lfHeader in s.loc.flags or lfNoDecl in s.loc.flags:
      v = g.externGlobal(s)
    elif sfGlobal in s.flags or
        (s.kind == skConst and n.sym.ast.isDeepConstExprLL(false)):
      v = g.genGlobal(s)
    else:
      v = g.scopeGet(s.llName)

    if v == nil:
      internalError(n.info, "Symbol not found: " & s.llName)

    var toload = s.kind != skParam or g.llPassAsPtr(n.typ)
    if toload and load and v.typeOf().getTypeKind() == llvm.PointerTypeKind:
      result = g.b.buildLoadValue(v)
    elif not load and s.kind == skParam and not g.llPassAsPtr(n.typ) and s.typ.kind != tyRef:
      # Someone wants an address, but all we have is a value...
      result = g.b.localAlloca(v.typeOf(), "")
      discard g.b.buildStore(v, result)
    else:
      result = v
  of skType:
    result = g.genTypeInfo(s.typ)
  of skMethod:
    if {sfDispatcher, sfForward} * s.flags != {}:
      result = g.genFunction(s)
    else:
      result = g.genFunctionWithBody(s)
  of skProc, skConverter, skIterators:
    if lfNoDecl in s.loc.flags or s.magic != mNone or
         {sfImportc, sfInfixCall} * s.flags != {}:
      result = g.genFunction(s)
    else:
      result = g.genFunctionWithBody(s)
  else:
    internalError(n.info, "Unhandled kind: " & $s.kind)

proc genIntLitExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let nt = g.llType(n.typ)
  if nt.getTypeKind == llvm.PointerTypeKind:
    result = llvm.constIntToPtr(
      llvm.constInt(llIntType, n.intVal.culonglong, llvm.False), nt)
  else:
    result = llvm.constInt(nt, n.intVal.culonglong, llvm.False)

proc genFloatLitExpr(g: LLGen, n: PNode): llvm.ValueRef =
  llvm.constReal(g.llType(n.typ), n.floatVal)

proc genStrLitExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ss = constNimString(n)
  let s = g.m.addPrivateConstant(ss.typeOf(), "")
  s.setInitializer(ss)
  result = constBitCast(s, llNimStringDescPtr)

proc genNilLitExpr(g: LLGen, n: PNode): llvm.ValueRef =
  if n.typ.kind == tyProc:
    if n.typ.callConv == ccClosure:
      let t = g.llType(n.typ)
      result = g.m.addPrivateConstant(t, "")
      result.setInitializer(llvm.constNull(t))
      return

  result = llvm.constNull(g.llType(n.typ))

proc genCallExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let nf = n[namePos]

  if nf.kind == nkSym:
    let sym = nf.sym
    if sym.magic != mNone:
      result = g.genMagicExpr(n, load)
      return

  result = g.genCall(n, load)

proc genParExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  result = g.b.localAlloca(g.llType(n.typ), "")

  for i in 0..<n.sonsLen:
    var s = n[i]

    if s.kind == nkExprColonExpr: s = s[1]
    let ax = g.genExpr(s, true)
    let tgt = g.b.buildGEP(result, [constGEPIdx(0), constGEPIdx(i.int32)])
    g.genAssignment(ax, tgt, s.typ)

  if load:
    result = g.b.buildLoad(result, "")

proc genObjConstrExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var
    typ = n.typ.skipTypes(abstractInst)
    t = g.llType(typ)
    isRef = typ.kind == tyRef

  if isRef:
    result = g.cpNewObj(typ)
    typ = typ.lastSon.skipTypes(abstractInst)
  else:
    result = g.b.localAlloca(t, "")

    g.callMemset(g.b.buildBitCast(result, llVoidPtrType, ""), constInt8(0), t.sizeOfX())
    g.genObjectInit(typ, result)

  for i in 1 .. <n.len:
    let s = n[i]
    let ind = fieldIndex(typ, s[0].sym.llName)
    let gep = g.b.buildGEP(result, (@[0] & ind).map(constGEPIdx))
    let init = g.genExpr(s[1], true)
    if init != nil:
      g.genAssignment(init, gep, s[1].typ)

  if load and not isRef:
    result = g.b.buildLoad(result, "")

proc genCurlyExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    typ = n.typ
    size = getSize(skipTypes(n.typ, abstractVar))
    t = g.llType(typ)

  if size <= 8:
    let tmp = g.b.localAlloca(t, "")

    if nfAllConst in n.flags:
      discard g.b.buildStore(constNimSet(n), tmp)
    else:
      discard g.b.buildStore(llvm.constNull(t), tmp)

      for s in n:
        if s.kind == nkRange:
          withRangeItems(it, s):
            let mask = g.b.buildSetMask(t, g.setElemIndex(typ, it), size)
            let v = g.b.buildLoad(tmp, "")
            let nv = g.b.buildOr(v, mask, "")
            discard g.b.buildStore(nv, tmp)

        else:
          let ax = g.genExpr(s, true)
          if ax == nil: return

          let mask = g.b.buildSetMask(t, g.setElemIndex(typ, ax), size)
          let v = g.b.buildLoad(tmp, "")
          let nv = g.b.buildOr(v, mask, "")
          discard g.b.buildStore(nv, tmp)
    if load:
      result = g.b.buildLoad(tmp, "")
  else:
    result = g.b.localAlloca(t, "")
    if nfAllConst in n.flags:
      discard g.b.buildStore(constNimSet(n), result)
    else:
      g.callMemset(g.b.buildGEP(result, [constGEPIdx(0), constGEPIdx(0)]), constInt8(0), constInt64(size))

      for s in n:
        if s.kind == nkRange:
          withRangeItems(it, s):
            let (gep, mask) = g.b.buildSetGEPMask(result, g.setElemIndex(typ, it))
            let v = g.b.buildLoad(gep, "")
            let nv = g.b.buildOr(v, mask, "")
            discard g.b.buildStore(nv, gep)
        else:
          let ax = g.genExpr(s, true)
          if ax == nil: return

          let (gep, mask) = g.b.buildSetGEPMask(result, g.setElemIndex(typ, ax))
          let v = g.b.buildLoad(gep, "")
          let nv = g.b.buildOr(v, mask, "")
          discard g.b.buildStore(nv, gep)
    if load:
      result = g.b.buildLoadValue(result)

proc genBracketExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let typ = n.typ.skipTypes(abstractVarRange)
  let t = g.llType(typ)

  case typ.kind
  of tyArray, tyArrayConstr:
    result = g.b.localAlloca(t, "")

    for i in 0..<sonsLen(n):
      let ax = g.genExpr(n[i], true)
      if ax != nil:
        let gep = g.b.buildGEP(result, [constGEPIdx(0), constGEPIdx(i)])
        g.genAssignment(ax, gep, n[i].typ)
    if load:
      result = g.b.buildLoadValue(result)
  of tySequence:
    let ti = g.genTypeInfo(typ)

    result = g.callCompilerProc("newSeq", [ti, constNimInt(n.sonsLen())])
    result = g.b.buildBitcast(result, t, "")
    for i in 0..<sonsLen(n):
      let ax = g.genExpr(n[i], true)
      if ax != nil:
        let gep = g.b.buildNimSeqDataGEP(result, constGEPIdx(i))
        g.genAssignment(ax, gep, n[i].typ)
  else:
    internalError(n.info, "Unhandled kind: " & $typ.kind)

proc genBracketArrayExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genExpr(n[0], false)
    bx = g.genExpr(n[1], true)
  if ax == nil or bx == nil: return

  assert bx.typeOf().getTypeKind() == llvm.IntegerTypeKind

  # GEP indices are signed, so if a char appears here we want to make sure
  # it's zero-extended
  let b = g.b.buildTruncOrExt(bx, llIntType, n[1].typ.kind)
  if ax.typeOf().isArrayPtr():
    result = g.b.buildGEP(ax, [constGEPIdx(0), b])
  else:
    result = g.b.buildGEP(ax, [b])

  if load:
    result = g.b.buildLoad(result, "")

proc genBracketOpenArrayExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genExpr(n[0], false)
    bx = g.genExpr(n[1], true)
  if ax == nil or bx == nil: return

  result = g.b.buildGEP(ax, [bx])

  if load:
    result = g.b.buildLoad(result, "")

proc genBracketSeqExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genExpr(n[0], true)
    bx = g.genExpr(n[1], true)
  if ax == nil or bx == nil: return

  result = g.b.buildNimSeqDataGEP(ax, bx)

  if load:
    result = g.b.buildLoad(result, "")

proc genBracketCStringExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genExpr(n[0], true)
    bx = g.genExpr(n[1], true)
  if ax == nil or bx == nil: return

  result = g.b.buildGEP(ax, [bx])

  if load:
    result = g.b.buildLoad(result, "")

proc genBracketTupleExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var
    ax = g.genExpr(n[0], false)
    bx = g.genExpr(n[1], true)
  if ax == nil or bx == nil: return

  if bx.typeOf().getIntTypeWidth() > 32.cuint:
    bx = g.b.buildTrunc(bx, llvm.int32Type(), "")
  result = g.b.buildGEP(ax, [constGEPIdx(0), bx])

  if load:
    result = g.b.buildLoad(result, "")

proc genBracketExprExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  var typ = skipTypes(n[0].typ, abstractInst)
  if typ.kind in {tyRef, tyPtr}: typ = typ.lastSon
  case typ.kind
  of tyArray, tyArrayConstr: result = g.genBracketArrayExpr(n, load)
  of tyOpenArray, tyVarargs: result = g.genBracketOpenArrayExpr(n, load)
  of tySequence, tyString: result = g.genBracketSeqExpr(n, load)
  of tyCString: result = g.genBracketCStringExpr(n, load)
  of tyTuple: result = g.genBracketTupleExpr(n, load)
  else: internalError(n.info, "Unhandled kind: " & $typ.kind)

proc genDotExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  p("genDotExpr", n[1].sym, g.depth + 1)
  let v = g.genExpr(n[0], false)
  if v == nil: return

  let typ = skipTypes(n[0].typ, abstractInst)
  let i = fieldIndex(typ, n[1].sym.llName)
  result = g.b.buildGEP(v, (@[0] & i).map(constGEPIdx))

  if load:
    result = g.b.buildLoadValue(result)

proc genCheckedFieldExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  result = g.genExpr(n[0], load)

proc genDerefExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let v = g.genExpr(n[0], true)
  if v == nil: return

  result = if load: g.b.buildLoadValue(v) else: v

proc genIfExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  # Sometimes an nkIfStmt appears in the ast even though it looks more like
  # an expression (see tcasestmt with an if in a case else).. it won't have
  # a type of its own so we'll have to cheat..
  let typ = if n.typ == nil: n[0][1].typ else: n.typ
  let tmp = g.b.localAlloca(if load: g.llType(typ) else: llvm.pointerType(g.llType(typ)), "")

  let iend = f.appendBasicBlock("ifx.end")

  for i in 0..sonsLen(n) - 1:
    let s = n[i]

    if s.sons.len == 1:
      # else branch
      g.scopePush(n, nil)
      let ax = g.genExpr(s[0], load)
      if ax != nil:
        g.genAssignment(ax, tmp, s[0].typ)
      g.scopePop()

      discard g.b.buildBr(iend)
    else:
      let cond = g.genExpr(s[0], true)
      if cond == nil:
        discard g.b.buildBr(iend)
        continue

      let itrue = f.appendBasicBlock("ifx.true")
      let ifalse = f.appendBasicBlock("ifx.false")

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      g.scopePush(n, nil)
      let ax = g.genExpr(s[1], load)
      if ax != nil:
        g.genAssignment(ax, tmp, s[0].typ)
      g.scopePop()

      discard g.b.buildBr(iend)

      g.b.positionBuilderAtEnd(ifalse)

  if n[n.sonsLen - 1].sonsLen != 1:
    discard g.b.buildBr(iend)

  llvm.moveBasicBlockAfter(iend, g.b.getInsertBlock())
  g.b.positionBuilderAtEnd(iend)

  result = g.b.buildLoad(tmp, "")

proc genLambdaExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let sym = n.sons[namePos].sym
  result = g.genFunctionWithBody(sym)

proc genConvExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let v = g.genExpr(n[1], load)
  if v == nil: return

  let
    vt = v.typeOf()
    nt = if load: g.llType(n.typ) else: llvm.pointerType(g.llType(n.typ))
    vtk = vt.getTypeKind()
    ntk = nt.getTypeKind()
    vtyp = skipTypes(n[1].typ, abstractInst)
    ntyp = skipTypes(n.typ, abstractInst)

  if vt == nt:
    result = v
  elif vtk == llvm.IntegerTypeKind and ntk == llvm.IntegerTypeKind:
    result = g.b.buildTruncOrExt(v, nt, n[1].typ.kind)
  elif vtk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind} and ntk == llvm.IntegerTypeKind:
    if ntyp.kind in {tyUInt..tyUInt64}:
      result = g.b.buildFPToUI(v, nt, "")
    else:
      result = g.b.buildFPToSI(v, nt, "")
  elif vtk == llvm.IntegerTypeKind and ntk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind}:
    if vtyp.kind in {tyUInt..tyUInt64}:
      result = g.b.buildUIToFP(v, nt, "")
    else:
      result = g.b.buildSIToFP(v, nt, "")
  elif n[1].typ.kind == tyPtr and n.typ.kind == tyPointer:
    result = g.b.buildBitcast(v, nt, "")
  elif vtk == llvm.FloatTypeKind and ntk == llvm.DoubleTypeKind:
    result = g.b.buildFPExt(v, nt, "")
  elif vtk == llvm.PointerTypeKind and ntk  == llvm.PointerTypeKind:
    result = g.b.buildBitcast(v, nt, "")
  elif n.typ.kind == tyProc and ntk == llvm.PointerTypeKind or nt == llClosureType:
    result = g.b.buildBitcast(v, llVoidPtrType, "")
  elif vtk == llvm.DoubleTypeKind and ntk == llvm.FloatTypeKind:
    result = g.b.buildFPTrunc(v, nt, "")
  elif vtyp.kind == tyArray and ntyp.kind == tyArray:
    result = v

  if result == nil:
    internalError(n.info, "Unhandled conversion: " & $vt & " " & $nt & " " &
      $n[1].typ.kind & " " & $n.typ.kind)

proc genCastExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ntyp = n[1].typ.skipTypes(abstractRange)

  let v =
    if ntyp.kind in {tyOpenArray, tyVarargs}: g.genExpr(n[1], false)
    else: g.genExpr(n[1], load)

  if v == nil: return
  let nt = g.llType(n.typ)

  let vtk = v.typeOf().getTypeKind()
  let ntk = nt.getTypeKind()
  if vtk != ntk:
    if vtk == llvm.PointerTypeKind and ntk == llvm.IntegerTypeKind:
      result = g.b.buildPtrToInt(v, nt, "")
    elif vtk == llvm.IntegerTypeKind and ntk == llvm.PointerTypeKind:
      result = g.b.buildIntToPtr(v, nt, "")
    elif vtk in {llvm.HalfTypeKind..llvm.PPC_FP128TypeKind} and ntk == llvm.IntegerTypeKind:
      if n.typ.kind in {tyUInt..tyUInt64}:
        result = g.b.buildFPToUI(v, nt, "")
      elif n.typ.kind in {tyInt..tyInt64}:
        result = g.b.buildFPToSI(v, nt, "")
    else:
      internalError(n.info, "Unhandled cast: " & $vtk & " " & $ntk)
  elif vtk == llvm.IntegerTypeKind:
    result = g.b.buildTruncOrExt(v, nt, n.typ.kind)
  else:
    result = g.b.buildBitcast(v, nt, "")

proc genAddrExpr(g: LLGen, n: PNode): llvm.ValueRef =
  g.genExpr(n[0], false)

proc genObjDownConvExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ax = g.genExpr(n[0], load)
  if ax == nil: return

  let at = ax.typeOf()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  if at == nt:
    result = ax
  elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
    result = g.b.buildBitcast(ax, nt, "")
  else:
    internalError(n.info, "Unhandled nkObjDownConv")

proc genObjUpConvExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let ax = g.genExpr(n[0], n.typ.kind == tyRef)
  if ax == nil: return

  let at = ax.typeOf()
  var nt = g.llType(n.typ)

  if nt.getTypeKind() != llvm.PointerTypeKind:
    nt = llvm.pointerType(nt)

  if at == nt:
    result = ax
  elif at.getTypeKind() == PointerTypeKind and nt.getTypeKind() == PointerTypeKind:
    result = g.b.buildBitcast(ax, nt, "")
  else:
    internalError(n.info, "Unhandled nkUpDownConv")

  if load and n.typ.kind != tyRef:
    result = g.b.buildLoad(result, "")

proc genChckRangeExpr(g: LLGen, n: PNode): llvm.ValueRef =
  # TODO actually make the range check call, maybe some truncating as well?
  let v = g.genExpr(n[0], true)
  if v == nil: return

  let
    vt = v.typeOf()
    nt = g.llType(n.typ)

  result = g.b.buildTruncOrExt(v, nt, n.typ.kind)

proc genStringToCStringExpr(g: LLGen, n: PNode): llvm.ValueRef =
  let ax = g.genExpr(n[0], true)
  if ax == nil: return

  result = g.b.buildNimSeqDataGEP(ax)

proc genStmtListExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let length = sonsLen(n)
  for i in 0..length - 2:
    g.genStmt(n[i])
  if length > 0:
    result = g.genExpr(n[length - 1], load)

proc genCaseExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let isString = skipTypes(n[0].typ, abstractInst).kind == tyString

  let ax = g.genExpr(n[0], true)
  if ax == nil: return

  let caseend = f.appendBasicBlock("case.end")

  result = g.b.localAlloca(g.llType(n.typ), "")

  for i in 1..n.sonsLen - 1:
    let s = n[i]
    p("genCaseExpr", s, g.depth)

    let isLast = i == n.sonsLen - 1

    let cur = g.b.getInsertBlock()

    let lbl = if s.kind == nkElse: "case.else" else: "case.of." & $i
    let casedo = f.appendBasicBlock(lbl & ".do")

    let next =
      if isLast: caseend
      elif n[i+1].kind == nkElse: f.appendBasicBlock("case.else")
      else: f.appendBasicBlock("case.of." & $(i+1))

    g.scopePush(n, caseend)
    g.b.positionBuilderAtEnd(casedo)
    let res = g.genExpr(s.lastSon, true)
    g.genAssignment(res, result, n.typ)

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
          let bx = g.genExpr(cond.sons[0], true)
          let cx = g.genExpr(cond.sons[1], true)
          if bx == nil:
            discard g.b.buildBr(caseend)
            g.b.positionBuilderAtEnd(caseend)
            g.scopePop()
            return

          let cmpb = g.b.buildICmp(llvm.IntSGE, ax, bx, "")
          let cmpc = g.b.buildICmp(llvm.IntSLE, ax, cx, "")
          let cmp = g.b.buildAnd(cmpb, cmpc, "")

          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(lbl & ".or." & $j)
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)
        else:
          let bx = g.genExpr(cond, true)
          if bx == nil:
            discard g.b.buildBr(caseend)
            g.b.positionBuilderAtEnd(caseend)
            g.scopePop()
            return

          var cmp: llvm.ValueRef
          if isString:
            let tmp = g.callCompilerProc("cmpStrings", [ax, bx])
            cmp = g.b.buildICmp(llvm.IntEQ, tmp, constNimInt(0), "")
          else:
            let b = g.b.buildTruncOrExt(bx, ax.typeOf(), cond.typ.kind)
            cmp = g.b.buildICmp(llvm.IntEQ, ax, b, "")
          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(lbl & ".or." & $j)
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
    g.scopePop()

  g.b.positionBuilderAtEnd(caseend)

  if f.getLastBasicBlock() != caseend:
    caseend.moveBasicBlockAfter(f.getLastBasicBlock())

  if load:
    result = g.b.buildLoadValue(result)

proc genClosureExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  let
    ax = g.genExpr(n[0], true)
    bx = g.genExpr(n[1], true)
  if ax == nil or bx == nil: return
  let
    a = g.b.buildBitcast(ax, llVoidPtrType, "")
    b = g.b.buildBitcast(bx, llVoidPtrType, "")
  result = g.b.localAlloca(llClosureType, "")
  discard g.b.buildStore(a, g.b.buildGEP(result, [constGEPIdx(0), constGEPIdx(0)]))
  discard g.b.buildStore(b, g.b.buildGEP(result, [constGEPIdx(0), constGEPIdx(1)]))

  if load:
    result = g.b.buildLoad(result, "")

# Magic statements
proc genEchoStmt(g: LLGen, n: PNode) =
  let b = n[1].skipConv

  let nilStr = g.m.constCString("nil")

  proc getCStr(v: llvm.ValueRef): llvm.ValueRef =
    let cmp = g.b.buildICmp(llvm.IntEQ, v, constNull(v.typeOf()), "")
    result = g.b.buildSelect(cmp, nilStr, g.b.buildNimSeqDataGEP(v), "")

  let args = b.sons.
    mapIt(llvm.ValueRef, g.genExpr(it, true)).
    filterIt(it != nil).
    mapIt(llvm.ValueRef, getCStr(it))

  let arg0 = "%s".repeat(args.len) & "\n"
  let v = g.b.buildGlobalStringPtr(arg0, "")
  let f = g.m.getOrInsertFunction("printf", llPrintfType)

  discard g.b.buildCall(f, v & args)

proc genIncDecStmt(g: LLGen, n: PNode, op: Opcode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let a = g.b.buildLoad(ax, "")
  let b = g.b.buildTruncOrExt(bx, a.typeOf(), n[1].typ.kind)

  let nv = g.b.buildBinOp(op, a, b, "")
  discard g.b.buildStore(nv, ax)

proc genNewStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
  if ax == nil: return

  let a = g.cpNewObj(n[1].typ)
  discard g.b.buildStore(a, ax)

proc genNewSeqStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let at = ax.typeOf()
  if at.getTypeKind() != llvm.PointerTypeKind:
    internalError("expected pointer, not " & $at)

  let ti = g.genTypeInfo(n[1].typ.skipTypes(abstractVarRange))

  let x = g.callCompilerProc("newSeq", [ti, bx])
  discard g.b.buildStore(g.b.buildBitcast(x, at.getElementType(), ""), ax)

proc genInclStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let typ = skipTypes(n[1].typ, abstractVar)
  let size = getSize(typ)

  if size <= 8:
    let b = g.b.buildSetMask(ax.typeOf().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildOr(g.b.buildLoad(ax, ""), b, "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genExpr(n[1], false)
      bx = g.genExpr(n[2], true)
    if ax == nil or bx == nil: return

    let (gep, mask) = g.b.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildOr(a, mask, "")
    discard g.b.buildStore(res, gep)

proc genExclStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let typ = skipTypes(n[1].typ, abstractVar)
  let size = getSize(typ)

  if size <= 8:
    let b = g.b.buildSetMask(ax.typeOf().getElementType(), g.setElemIndex(typ, bx), size)
    let res = g.b.buildAnd(g.b.buildLoad(ax, ""), g.b.buildNot(b), "")
    discard g.b.buildStore(res, ax)
  else:
    let
      ax = g.genExpr(n[1], false)
      bx = g.genExpr(n[2], true)
    if ax == nil or bx == nil: return

    let (gep, mask) = g.b.buildSetGEPMask(ax, g.setElemIndex(typ, bx))
    let a = g.b.buildLoad(gep, "")
    let res = g.b.buildAnd(a, g.b.buildNot(mask), "")
    discard g.b.buildStore(res, gep)

proc genAppendStrChStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let a = g.b.buildLoad(ax, "")
  let ret = g.callCompilerProc("addChar", [a, bx])
  discard g.b.buildStore(ret, ax)

proc genAppendStrStrStmt(g: LLGen, n: PNode) =
  let tgtp = g.genExpr(n[1], false)
  var tgt = g.b.buildLoad(tgtp, "")

  # First, find out total length of the new strings
  var tgtlen = constNimInt(0)

  var constlen = 0
  var exprs: seq[llvm.ValueRef] = @[]
  for s in n.sons[2..n.sonsLen - 1]:
    let sx = g.genExpr(s, true)
    exprs.add(sx)
    if sx == nil: continue

    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      inc(constlen)
    elif s.kind in {nkStrLit..nkTripleStrLit}:
      inc(constlen, len(s.strVal))
    else:
      let slen = g.b.buildLoad(g.b.buildNimSeqLenGEP(sx), "")
      tgtlen = g.b.buildAdd(tgtlen, slen, "")

  if constlen > 0:
    tgtlen = g.b.buildAdd(tgtlen, constNimInt(constlen), "")

  # Make room
  tgt = g.callCompilerProc("resizeString", [tgt, tgtlen])
  discard g.b.buildStore(tgt, tgtp)

  # Copy data
  for i in 2..n.sonsLen - 1:
    let sx = exprs[i - 2]
    if sx == nil: continue

    let s = n[i]
    if skipTypes(s.typ, abstractVarRange).kind == tyChar:
      discard g.callCompilerProc("appendChar", [tgt, sx])
    else:
      discard g.callCompilerProc("appendString", [tgt, sx])

proc genAppendSeqElemStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let
    a = g.b.buildLoad(ax, "")
    ap = g.b.buildGEP(a, [constGEPIdx(0), constGEPIdx(0)])
    newseq = g.callCompilerProc("incrSeqV2", [ap, bx.typeOf().sizeOfX()])
    tgt = g.b.buildBitcast(newseq, a.typeOf(), "")
    lenp = g.b.buildNimSeqLenGEP(tgt)
    len = g.b.buildLoad(lenp, "")

  g.genAssignment(bx, g.b.buildNimSeqDataGEP(tgt, len), n[2].typ)

  let newlen = g.b.buildAdd(len, llvm.constInt(len.typeOf(), 1, llvm.False), "")
  discard g.b.buildStore(newlen, lenp)
  discard g.b.buildStore(tgt, ax)

proc genSetLengthStrStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let a = g.b.buildLoad(ax, "")
  discard g.b.buildStore(g.callCompilerProc("setLengthStr", [a, bx]), ax)

proc genSetLengthSeqStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], true)
  if ax == nil or bx == nil: return

  let
    a = g.b.buildLoad(ax, "")
    ap = g.b.buildGEP(a, [constGEPIdx(0), constGEPIdx(0)])
    at = a.typeOf()
    so = at.getElementType().structGetTypeAtIndex(1).getElementType().sizeOfX()

  let x = g.callCompilerProc("setLengthSeq", [ap, so, bx])
  # TODO fix assignment
  discard g.b.buildStore(g.b.buildBitCast(x, at, ""), ax)

proc genSwapStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[1], false)
    bx = g.genExpr(n[2], false)
  if ax == nil or bx == nil: return

  let t = g.llType(n[1].typ)
  let tmpx = g.b.localAlloca(t, "")

  let a = g.b.buildLoad(ax, "")
  g.genAssignment(a, tmpx, n[1].typ)
  let b = g.b.buildLoad(bx, "")
  g.genAssignment(b, ax, n[1].typ)
  let tmp = g.b.buildLoad(tmpx, "")
  g.genAssignment(tmp, bx, n[1].typ)

proc genResetStmt(g: LLGen, n: PNode) =
  let ax = g.genExpr(n[1], false)

  discard g.callCompilerProc("genericReset",
    [ax, g.genTypeInfo(skipTypes(n[1].typ, abstractVarRange))])

proc genMagicStmt(g: LLGen, n: PNode) =
  var op = n.sons[0].sym.magic
  p("genMagicStmt " & $op, n.sons[0], g.depth + 1)
  case op
  of mEcho: g.genEchoStmt(n)
  of mInc: g.genIncDecStmt(n, llvm.Add)
  of mDec: g.genIncDecStmt(n, llvm.Sub)
  of mNew: g.genNewStmt(n)
  of mNewFinalize: g.genNewStmt(n)  # TODO
  of mNewSeq: g.genNewSeqStmt(n)
  of mIncl: g.genInclStmt(n)
  of mExcl: g.genExclStmt(n)
  of mAppendStrCh: g.genAppendStrChStmt(n)
  of mAppendStrStr: g.genAppendStrStrStmt(n)
  of mAppendSeqElem: g.genAppendSeqElemStmt(n)
  of mExit: discard g.genCall(n, false)
  of mSetLengthStr: g.genSetLengthStrStmt(n)
  of mSetLengthSeq: g.genSetLengthSeqStmt(n)
  of mSwap: g.genSwapStmt(n)
  of mReset: g.genResetStmt(n)
  else: internalError(n.info, "Unhandled genMagicStmt: " & $op)

# Statements
proc genCallStmt(g: LLGen, n: PNode) =
  let nf = n[namePos]

  if nf.kind == nkSym:
    let sym = nf.sym
    if sym.magic != mNone:
      g.genMagicStmt(n)
      return

  discard g.genCall(n, false)

proc genIdentDefsStmt(g: LLGen, n: PNode) =
  for s in n.sons: p("genIdentDefsStmt", s, g.depth + 1)
  var x: llvm.ValueRef
  var local = false

  let init = n[2]

  if n[0].kind == nkSym:
    let v = n[0].sym
    local = sfGlobal notin v.flags
    if lfNoDecl in v.loc.flags: return

    if sfGlobal in v.flags:
      x = g.genGlobal(v)
      if init.isDeepConstExprLL(true):
        let i = g.genConstInitializer(init)
        if i != nil:
          x.setInitializer(i)
          return
      else:
        g.genObjectInit(v.typ, x)
    else:
      let t = g.llType(v.typ)
      x = g.b.localAlloca(t, v.llName)
      if t.getTypeKind() in {llvm.ArrayTypeKind, llvm.StructTypeKind}:
        g.callMemset(g.b.buildBitCast(x, llVoidPtrType, ""), constInt8(0), t.sizeOfX())
      elif init.kind == nkEmpty:
        discard g.b.buildStore(constNull(t), x)

      g.genObjectInit(v.typ, x)
  else:
    # Closure...
    x = g.genExpr(n[0], false)

  if init.kind != nkEmpty:
    let i = g.genExpr(init, true)
    if i != nil:
      g.genAssignment(i, x, n[2].typ)

  # Put in scope only once init is finished (init might refence
  # a var with the same name)
  if local:
    g.scopePut(n[0].sym.llName, x)

proc genVarTupleStmt(g: LLGen, n: PNode) =
  for s in n.sons[0..n.sonsLen - 3]:
    if s.kind != nkSym:
      internalError("Expected nkSym: " & $s)

  let t = g.genExpr(n.lastSon, true)

  for i in 0..n.sonsLen - 3:
    let s = n[i]
    let v = s.sym

    var x: llvm.ValueRef

    if sfGlobal in v.flags:
      x = g.genGlobal(v)
    else:
      let t = g.llType(v.typ)
      x = g.b.localAlloca(t, v.llName)
      g.callMemset(g.b.buildBitCast(x, llVoidPtrType, ""), constInt8(0), t.sizeOfX())
      g.genObjectInit(v.typ, x)

    let tv = g.b.buildExtractValue(t, i.cuint, "")

    g.genAssignment(tv, x, s.typ)

    if sfGlobal notin v.flags:
      # Put in scope only once init is finished (init might refence
      # a var with the same name)
      g.scopePut(v.llName, x)

proc genAsgnStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[0], false)
    bx = g.genExpr(n[1], true)

  if ax == nil or bx == nil: return

  g.genAssignment(bx, ax, n[0].typ)

proc genFastAsgnStmt(g: LLGen, n: PNode) =
  let
    ax = g.genExpr(n[0], false)
    bx = g.genExpr(n[1], true)
  if ax == nil or bx == nil: return

  g.genAssignment(bx, ax, n[0].typ)

proc genProcStmt(g: LLGen, n: PNode) =
  if n.sons[genericParamsPos].kind != nkEmpty:
    return

  let s = n.sons[namePos].sym
  discard g.genFunctionWithBody(s)

proc genIfStmt(g: LLGen, n: PNode) =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  # TODO Single scope enough?
  g.scopePush(n, nil)
  var iend: llvm.BasicBlockRef
  for i in 0..sonsLen(n) - 1:
    let s = n[i]

    if s.sons.len == 1:
      # else branch
      g.scopePush(n, nil)
      g.genStmt(s[0])
      g.scopePop()

      if g.b.needsBr():
        if iend == nil:
          iend = f.appendBasicBlock("if.end")
        discard g.b.buildBr(iend)
    else:
      let cond = g.genExpr(s[0], true)
      if cond == nil:
        if iend == nil:
          iend = f.appendBasicBlock("if.end")
        discard g.b.buildBr(iend)
        continue

      let itrue = f.appendBasicBlock("if.true")
      let ifalse = f.appendBasicBlock("if.false")

      discard g.b.buildCondBr(cond, itrue, ifalse)

      g.b.positionBuilderAtEnd(itrue)
      g.scopePush(n, nil)
      g.genStmt(s[1])
      g.scopePop()

      if g.b.needsBr():
        if iend == nil:
          iend = f.appendBasicBlock("if.end")
        discard g.b.buildBr(iend)

      g.b.positionBuilderAtEnd(ifalse)

  if iend != nil:
    if n[n.sonsLen - 1].sonsLen != 1:
      discard g.b.buildBr(iend)
    llvm.moveBasicBlockAfter(iend, g.b.getInsertBlock())
    g.b.positionBuilderAtEnd(iend)
  g.scopePop()

proc genWhenStmt(g: LLGen, n: PNode) =
  g.genStmt(n[1][0])

proc genWhileStmt(g: LLGen, n: PNode) =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let wcmp = f.appendBasicBlock("while.cmp")
  let wtrue = f.appendBasicBlock("while.true")
  let wfalse = f.appendBasicBlock("while.false")

  # jump to comparison
  discard g.b.buildBr(wcmp)

  # generate condition expression in cmp block
  g.b.positionBuilderAtEnd(wcmp)
  let cond = g.genExpr(n[0], true)

  if cond == nil:
    # no condition, shouldn't happen..
    discard g.b.buildBr(wfalse)
  else:
    discard g.b.buildCondBr(cond, wtrue, wfalse)

  # loop body
  g.b.positionBuilderAtEnd(wtrue)
  g.scopePush(n, wfalse)
  g.genStmt(n[1])
  g.scopePop()

  # back to comparison
  discard g.b.buildBr(wcmp)

  # continue at the end
  g.b.positionBuilderAtEnd(wfalse)

proc genCaseStmt(g: LLGen, n: PNode) =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let isString = skipTypes(n[0].typ, abstractInst).kind == tyString

  let ax = g.genExpr(n[0], true)
  if ax == nil: return

  let caseend = f.appendBasicBlock("case.end")

  for i in 1..n.sonsLen - 1:
    let s = n[i]
    p("genCaseStmt", s, g.depth)

    let isLast = i == n.sonsLen - 1

    let cur = g.b.getInsertBlock()

    let lbl = if s.kind == nkElse: "case.else" else: "case.of." & $i
    let casedo = f.appendBasicBlock(lbl & ".do")

    let next =
      if isLast: caseend
      elif n[i+1].kind == nkElse: f.appendBasicBlock("case.else")
      else: f.appendBasicBlock("case.of." & $(i+1))

    g.scopePush(n, caseend)
    g.b.positionBuilderAtEnd(casedo)
    g.genStmt(s.lastSon)

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
          let bx = g.genExpr(cond.sons[0], true)
          let cx = g.genExpr(cond.sons[1], true)
          if bx == nil:
            discard g.b.buildBr(caseend)
            g.b.positionBuilderAtEnd(caseend)
            g.scopePop()
            return

          let cmpb = g.b.buildICmp(llvm.IntSGE, ax, bx, "")
          let cmpc = g.b.buildICmp(llvm.IntSLE, ax, cx, "")
          let cmp = g.b.buildAnd(cmpb, cmpc, "")

          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(lbl & ".or." & $j)
          discard g.b.buildCondBr(cmp, casedo, casenext)

          g.b.positionBuilderAtEnd(casenext)
        else:
          let bx = g.genExpr(cond, true)
          if bx == nil:
            discard g.b.buildBr(caseend)
            g.b.positionBuilderAtEnd(caseend)
            g.scopePop()
            return

          var cmp: llvm.ValueRef
          if isString:
            let tmp = g.callCompilerProc("cmpStrings", [ax, bx])
            cmp = g.b.buildICmp(llvm.IntEQ, tmp, constNimInt(0), "")
          else:
            let b = g.b.buildTruncOrExt(bx, ax.typeOf(), cond.typ.kind)
            cmp = g.b.buildICmp(llvm.IntEQ, ax, b, "")
          let casenext =
            if isLast2: next
            else: f.appendBasicBlock(lbl & ".or." & $j)
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
    g.scopePop()

  g.b.positionBuilderAtEnd(caseend)

  if f.getLastBasicBlock() != caseend:
    caseend.moveBasicBlockAfter(f.getLastBasicBlock())

proc genConstDefStmt(g: LLGen, n: PNode) =
  # TODO generate lazily
  for s in n.sons: p("genConstDefStmt", s, g.depth + 1)

  let v = n[0].sym

  if v.typ.containsCompileTimeOnly: return
  if lfNoDecl in v.loc.flags: return
  if v.typ.kind notin ConstantDataTypes: return

  let init = v.ast

  if init.isDeepConstExprLL(false):
    let ci = g.genConstInitializer(init)
    if ci == nil: internalError(n.info, "Unable to generate const initializer: " & $init)

    let c = g.m.addPrivateConstant(ci.typeOf(), v.llName)
    c.setInitializer(ci)
    return

  let x =
    if sfGlobal in v.flags:
      g.genGlobal(v)
    else:
      g.b.localAlloca(g.llType(v.typ), v.llName)

  if init.kind != nkEmpty:
    let i = g.genExpr(init, true)
    g.genAssignment(i, x, n[2].typ)

    # Put in scope only once init is finished (init might refence
    # a var with the same name)
    if not (sfGlobal in v.flags):
      g.scopePut(v.llName, x)

proc genTryStmt(g: LLGen, n: PNode) =
  # create safe point
  let tsp = magicsys.getCompilerProc("TSafePoint").typ
  let spt =  g.llStructType(tsp)

  let sp = g.b.localAlloca(spt, "sp")
  discard g.callCompilerProc("pushSafePoint", [sp])

  # call setjmp
  let setjmp = g.m.getOrInsertFunction("_setjmp", llSetjmpType)

  let contextP = g.b.buildGEP(sp, (@[0] & fieldIndex(tsp, "context")).map(constGEPIdx))
  let res = g.b.buildCall(setjmp, [g.b.buildBitcast(contextP, llJmpbufp, "")])

  let statusP = g.b.buildGEP(sp, (@[0] & fieldIndex(tsp, "status")).map(constGEPIdx))
  discard g.b.buildStore(g.b.buildNimIntExt(res, false), statusP)

  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()

  let sjok = f.appendBasicBlock("sj.ok")
  let sjexc = f.appendBasicBlock("sj.exc")
  let sjend = f.appendBasicBlock("sj.end")

  # see if we're returning normally or from a longjmp
  let cmp = g.b.buildICmp(llvm.IntEQ, res, constCInt(0), "")
  discard g.b.buildCondBr(cmp, sjok, sjexc)

  g.b.positionBuilderAtEnd(sjok)

  g.genStmt(n[0])
  discard g.callCompilerProc("popSafePoint", [])
  discard g.b.buildBr(sjend)

  g.b.positionBuilderAtEnd(sjexc)
  discard g.callCompilerProc("popSafePoint", [])

  var i = 1
  let length = sonsLen(n)
  while (i < length) and (n[i].kind == nkExceptBranch):
    let b = n[i]
    inc(i)

    let blen = sonsLen(b)
    if blen == 1:
      # catch-all
      discard g.b.buildStore(constNimInt(0), statusP)
      g.genStmt(b[0])

      discard g.callCompilerProc("popCurrentException", [])
      discard g.b.buildBr(sjend)
    else:
      let sjfound = f.appendBasicBlock("sj.found")
      let sjnext =
        if (i < length) and (n[i].kind == nkExceptBranch): f.appendBasicBlock("sj.next")
        else: sjend
      # catch one or more types
      for j in 0..blen - 2:
        assert(b[j].kind == nkType)

        let exc = g.callCompilerProc("getCurrentException", [])
        let m_type = g.b.buildLoad(g.b.buildGEP(exc, [constGEPIdx(0), constGEPIdx(0), constGEPIdx(0)]), "")
        let ti = g.genTypeInfo(b[j].typ)
        let found = g.callCompilerProc("isObj", [m_type, ti])

        if j == blen - 2:
          discard g.b.buildCondBr(found, sjfound, sjnext)
          g.b.positionBuilderAtEnd(sjnext)
        else:
          let sjor = f.appendBasicBlock("sj.or")
          sjor.moveBasicBlockBefore(sjfound)
          discard g.b.buildCondBr(found, sjfound, sjor)
          g.b.positionBuilderAtEnd(sjor)

      g.b.positionBuilderAtEnd(sjfound)
      discard g.b.buildStore(constNimInt(0), statusP)
      g.genStmt(b[blen-1])
      discard g.callCompilerProc("popCurrentException", [])

      discard g.b.buildBr(sjend)

      g.b.positionBuilderAtEnd(sjnext)

  if i == 1:
    # finally without catch!
    discard g.b.buildBr(sjend)
  g.b.positionBuilderAtEnd(sjend)

  if i < length and n[i].kind == nkFinally:
    g.genStmt(n[i][0])

  # TODO is the load needed?
  # TODO is this needed if we have a catch-all?
  let s = g.b.buildLoad(statusP, "")
  let sjrr = f.appendBasicBlock("sj.rr")
  let sjnm = f.appendBasicBlock("sj.nomore")
  # In case it wasn't handled...
  let scmp = g.b.buildICmp(llvm.IntNE, s, constNimInt(0), "")
  discard g.b.buildCondBr(scmp, sjrr, sjnm)
  g.b.positionBuilderAtEnd(sjrr)
  discard g.callCompilerProc("reraiseException", [])
  # TODO get rid of br somehow? reraise shoudn't return
  discard g.b.buildBr(sjnm)

  g.b.positionBuilderAtEnd(sjnm)
  sjend.moveBasicBlockBefore(sjrr)

proc genRaiseStmt(g: LLGen, n: PNode) =
  # TODO raise nested in try
  if n[0].kind != nkEmpty:
    let ax = g.genExpr(n[0], true)
    if (ax == nil): return

    let typ = skipTypes(n[0].typ, abstractPtrs)
    let name = g.b.buildGlobalStringPtr(typ.sym.name.s, "")
    discard g.callCompilerProc("raiseException", [ax, name])
  else:
    discard g.callCompilerProc("reraiseException", [])

proc genReturnStmt(g: LLGen, n: PNode) =
  if (n.sons[0].kind != nkEmpty):
    g.genStmt(n.sons[0])
  discard g.b.buildBr(g.returnBlock)

  # Sometimes, there might be dead code after the return statement.. as a hack
  # we add a block that will have no predecessors, to avoid dealing with it
  # elsewhere
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let cont = f.appendBasicBlock("")
  g.b.positionBuilderAtEnd(cont)

proc genBreakStmt(g: LLGen, n: PNode) =
  p("b", n[0], g.depth)

  if n[0].kind != nkEmpty:
    let s = g.scopeFind(n[0].sym)
    if s == nil:
      p("TODO NOT FOUND", n[0], 0)
    else:
      # similar to return, there might be more stuff after a break that messes
      # things up - we add an extra block just in case
      # TODO: one case when this happens is with finally blocks, which are
      # currently broken anyway, but this way at least the generated bytecode is
      # valid
      let pre = g.b.getInsertBlock()
      let f = pre.getBasicBlockParent()
      let cont = f.appendBasicBlock("block.cont." & $s.n.sym.id)
      if s.exit == nil:
        s.exit = f.appendBasicBlock("block.exit." & $s.n.sym.id)
      discard g.b.buildBr(s.exit)
      g.b.positionBuilderAtEnd(cont)
  else:
    p("TODO genBreakStmt", n, 0)

proc genBlockStmt(g: LLGen, n: PNode) =
  g.scopePush(n[0], nil)
  g.genStmt(n[1])
  let scope = g.scopePop()

  if scope.exit != nil:
    g.b.buildBrFallthrough(scope.exit)
    scope.exit.moveBasicBlockAfter(g.b.getInsertBlock())
    g.b.positionBuilderAtEnd(scope.exit)

proc genDiscardStmt(g: LLGen, n: PNode) =
  discard g.genExpr(n[0], true)

proc genGotoStateStmt(g: LLGen, n: PNode) =
  let ax = g.genExpr(n[0], true)
  if ax == nil: return

  let l = n[0].typ.lastOrd()

  g.scope[g.scope.len - 1].goto = g.b.buildSwitch(ax, g.returnBlock, (l + 1).cuint)

proc genStateStmt(g: LLGen, n: PNode) =
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let state = f.appendBasicBlock("state." & $n[0].intVal)
  g.b.buildBrFallthrough(state)
  g.b.positionBuilderAtEnd(state)
  for i in 0..g.scope.len-1:
    if g.scope[g.scope.len - i - 1].goto != nil:
      g.scope[g.scope.len - i - 1].goto.addCase(constNimInt(n[0].intVal.int), state)
      break

proc genBreakStateStmt(g: LLGen, n: PNode) =
  # TODO C code casts to int* and reads first value.. uh, I guess we should be
  # able to do better
  var ax: llvm.ValueRef
  if n[0].kind == nkClosure:
    ax = g.scopeGet(n[0][1].sym.llName)
  else:
    ax = g.genExpr(n[0], false)
    ax = g.b.buildGEP(ax, [constGEPIdx(0), constGEPIdx(1)])

  ax = g.b.buildLoad(ax, "")
  ax = g.b.buildBitCast(ax, llIntType.pointerType(), "")
  let s = g.b.buildLoad(ax, "")
  let cmp = g.b.buildICmp(llvm.IntSLT, s, constNimInt(0), "")
  let pre = g.b.getInsertBlock()
  let f = pre.getBasicBlockParent()
  let state = f.appendBasicBlock("state.break.cont")

  # nkBreakState happens in a loop, so we need to find the end of that loop...
  var whileExit: llvm.BasicBlockRef
  for i in 0..g.scope.len-1:
    if g.scope[g.scope.len - i - 1].exit != nil:
      whileExit = g.scope[g.scope.len - i - 1].exit
      break

  if whileExit == nil:
    internalError(n.info, "no enclosing while loop in nkBreakState")

  discard g.b.buildCondBr(cmp, whileExit, state)
  g.b.positionBuilderAtEnd(state)

proc genExpr(g: LLGen, n: PNode, load: bool): llvm.ValueRef =
  p(if load: "xl" else: "xp", n, g.depth)

  g.depth += 1
  case n.kind
  of nkEmpty: discard
  of nkSym: result = g.genSymExpr(n, load)
  of nkCharLit..nkUInt64Lit: result = g.genIntLitExpr(n)
  of nkFloatLit..nkFloat128Lit: result = g.genFloatLitExpr(n)
  of nkStrLit..nkTripleStrLit: result = g.genStrLitExpr(n, load)
  of nkNilLit: result = g.genNilLitExpr(n)
  of nkCallKinds: result = g.genCallExpr(n, load)
  of nkExprColonExpr: result = g.genExpr(n[1], load)
  of nkPar: result = g.genParExpr(n, load)
  of nkObjConstr: result = g.genObjConstrExpr(n, load)
  of nkCurly: result = g.genCurlyExpr(n, load)
  of nkBracket: result = g.genBracketExpr(n, load)
  of nkBracketExpr: result = g.genBracketExprExpr(n, load)
  of nkDotExpr: result = g.genDotExpr(n, load)
  of nkCheckedFieldExpr: result = g.genCheckedFieldExpr(n, load)
  of nkDerefExpr, nkHiddenDeref: result = g.genDerefExpr(n, load)
  of nkIfExpr: result = g.genIfExpr(n, load)
  of nkLambda, nkDo: result = g.genLambdaExpr(n)
  of nkHiddenStdConv, nkHiddenSubConv, nkConv: result = g.genConvExpr(n, load)
  of nkCast: result = g.genCastExpr(n, load)
  of nkAddr, nkHiddenAddr: result = g.genAddrExpr(n)
  of nkObjDownConv: result = g.genObjDownConvExpr(n, load)
  of nkObjUpConv: result = g.genObjUpConvExpr(n, load)
  of nkChckRange, nkChckRange64: result = g.genChckRangeExpr(n)
  of nkStringToCString: result = g.genStringToCStringExpr(n)
  of nkCStringToString: result = g.genToStrExpr(n, "cstrToNimstr")
  of nkIfStmt: result = g.genIfExpr(n, load) # if in case! see tcaststm.nim
  of nkCaseStmt: result = g.genCaseExpr(n, load)  # Sometimes seen as expression!
  of nkStmtListExpr: result = g.genStmtListExpr(n, load)
  of nkClosure: result = g.genClosureExpr(n, load)
  else: internalError(n.info, "Unhandled expression: " & $n.kind)
  g.depth -= 1

proc genSons(g: LLGen, n: PNode) =
  for s in n: g.genStmt(s)

proc genStmt(g: LLGen, n: PNode) =
  p("s", n, g.depth)
  g.depth += 1
  case n.kind
  of nkEmpty: discard
  of nkSym: discard  # labels etc
  of nkCallKinds: g.genCallStmt(n)
  of nkIdentDefs: g.genIdentDefsStmt(n)
  of nkVarTuple: g.genVarTupleStmt(n)
  of nkAsgn: g.genAsgnStmt(n)
  of nkFastAsgn: g.genFastAsgnStmt(n)
  of nkProcDef, nkMethodDef, nkConverterDef:
    var s = n[namePos].sym
    if {sfExportc, sfCompilerProc} * s.flags == {sfExportc} or
        s.kind == skMethod:
      g.genProcStmt(n)
  of nkIfStmt: g.genIfStmt(n)
  of nkWhenStmt: g.genWhenStmt(n)
  of nkWhileStmt: g.genWhileStmt(n)
  of nkCaseStmt: g.genCaseStmt(n)
  of nkVarSection, nkLetSection, nkConstSection: g.genSons(n)
  of nkConstDef: g.genConstDefStmt(n)  # do we need something else?
  of nkTryStmt: g.genTryStmt(n)
  of nkRaiseStmt: g.genRaiseStmt(n)
  of nkReturnStmt: g.genReturnStmt(n)
  of nkBreakStmt: g.genBreakStmt(n)
  of nkBlockStmt: g.genBlockStmt(n)
  of nkDiscardStmt: g.genDiscardStmt(n)
  of nkStmtList: g.genSons(n)
  of nkGotoState: g.genGotoStateStmt(n)
  of nkState: g.genStateStmt(n)
  of nkBreakState: g.genBreakStateStmt(n)

  of nkTypeSection, nkCommentStmt, nkIteratorDef, nkIncludeStmt,
     nkImportStmt, nkImportExceptStmt, nkExportStmt, nkExportExceptStmt,
     nkFromStmt, nkTemplateDef, nkMacroDef, nkPragma: discard
  of nkNilLit: discard  # expresssion?
  of nkStmtListExpr: discard g.genStmtListExpr(n, false)
  else: internalError(n.info, "Unhandled statement: " & $n.kind)
  g.depth -= 1

proc newLLGen(s: PSym): LLGen =
  new(result)
  let name = s.llName

  result.m = llvm.moduleCreateWithName(name)
  result.b = llvm.createBuilder()
  result.scope = @[]
  result.syms = @[]
  result.options = gOptions

proc genMain(g: LLGen) =
  let f = g.m.addFunction("main", llMainType)

  let b = llvm.appendBasicBlock(f, "entry")
  g.b.positionBuilderAtEnd(b)

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

  discard g.b.buildCall(g.init, [], "")

  discard g.b.buildRet(constInt(llCIntType, 0, False))

proc myClose(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  p("Close", n, 0)

  let g = LLGen(b)

  g.genStmt(n)

  let s = g.syms.pop()
  if sfMainModule notin s.flags: return n

  # return from nlvmInit

  g.b.buildBrFallthrough(g.returnBlock)
  g.b.positionBuilderAtEnd(g.returnBlock)

  discard g.b.buildRetVoid()

  let fn = g.returnBlock.getBasicBlockParent()
  if fn.getLastBasicBlock() != g.returnBlock:
    g.returnBlock.moveBasicBlockAfter(fn.getLastBasicBlock())

  g.genMain()

  var disp = generateMethodDispatchers()
  for i in 0..sonsLen(disp)-1:
    discard g.genFunctionWithBody(disp.sons[i].sym)

  let outfile =
    if options.outFile.len > 0:
      if options.outFile.isAbsolute: options.outFile
      else: getCurrentDir() / options.outFile
    else: changeFileExt(completeCFilePath(s.filename), "bc")
  discard g.m.writeBitcodeToFile(outfile)

  if gVerbosity == 2:
    # When we've generated some broken code, llvm-dis fails whereas
    # writing the module like this works
    discard g.m.printModuleToFile(outfile.changeFileExt(".ll"), nil)

  result = n

  g.m.disposeModule()
  uglyGen = nil

proc myProcess(b: PPassContext, n: PNode): PNode =
  if passes.skipCodegen(n): return n
  p("Process", n, 0)

  let g = LLGen(b)
  g.options = gOptions

  g.genStmt(n)

  result = n

proc myOpenCached(s: PSym, rd: PRodReader): PPassContext =
  result = nil

proc myOpen(s: PSym): PPassContext =
  # In the C generator, a separate C file is generated for every module,
  # but the rules governing what goes into which module are shrouded by a
  # layer of globals and conditionals.
  # Rather than deciphering all that, we simply generate a single module
  # with all the code in it, like the JS generator does.

  p("Opening", s, 0)
  if uglyGen == nil:
    let g = newLLGen(s)

    let init = g.m.addFunction(".nlvmInit", llInitFuncType)

    let b = llvm.appendBasicBlock(init, "entry." & s.llName)
    g.b.positionBuilderAtEnd(b)

    g.returnBlock = llvm.appendBasicBlock(init, "return")

    g.scopePush(nil, g.returnBlock)

    g.init = init

    uglyGen = g
  else:
    let g = uglyGen
    let entry = llvm.appendBasicBlock(g.init, "entry." & s.llName)
    g.b.buildBrFallthrough(entry)
    g.b.positionBuilderAtEnd(entry)

  uglyGen.syms.add(s)

  result = uglyGen

const llgenPass* = makePass(myOpen, myOpenCached, myProcess, myClose)
