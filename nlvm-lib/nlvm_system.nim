# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016-2019
# See the LICENSE file for license info (doh!)

# Exception handling helpers on top of libunwind - mostly based on the clang
# C++ exception handling personality function

# TODO we could actually provide some support for the GCC/clang EH ABI and
#      show better error messages

# https://itanium-cxx-abi.github.io/cxx-abi/abi-eh.html

# Nothing in here may raise
{.push raises: [].}

import system/ansi_c
import ./nlvm_unwind

const
  DW_EH_PE_omit = 0xFF'u8
  DW_EH_PE_absptr = 0x00'u8

  DW_EH_PE_uleb128 = 0x01'u8
  DW_EH_PE_udata2 = 0x02'u8
  DW_EH_PE_udata4 = 0x03'u8
  DW_EH_PE_udata8 = 0x04'u8
  DW_EH_PE_sleb128 = 0x09'u8
  DW_EH_PE_sdata2 = 0x0A'u8
  DW_EH_PE_sdata4 = 0x0B'u8
  DW_EH_PE_sdata8 = 0x0C'u8

  DW_EH_PE_pcrel = 0x10'u8
  DW_EH_PE_textrel = 0x20'u8
  DW_EH_PE_datarel = 0x30'u8
  DW_EH_PE_funcrel = 0x40'u8
  DW_EH_PE_aligned = 0x50'u8

  DW_EH_PE_indirect = 0x80'u8

  nlvmExceptionClass = 0x6e6c766d6e696d00'u64 # nlvmnim\0 (TODO: endian?)

# Need these for the runtime type information
include system/inclrtl, system/hti

template dprintf(s: cstring, x: varargs[untyped]) =
  when defined(nlvmDebugSystem): c_printf(s, x)
  else: discard

# Chicken and egg in system.nim...
template `+=`(x: var SomeInteger, y: SomeInteger) = x = x + y
template `-=`(x: var SomeInteger, y: SomeInteger) = x = x - y

type
  NlvmException = object
    ## Additional exception handling data needed by the personality function as
    ## well as a storage area for libunwind. By convention, the unwind
    ## data is placed last, allowing us to add fields at the beginning of this
    ## struct without breaking ABI - in nlvm, this is not strictly necessary
    ## because we don't support an ABI yet, but might as well prepare :)
    ## TODO technically, we could be allocating this memory together with the
    ##      memory for the nimException, instead of keeping a ref here!
    nimException: ref Exception
    nextException: ptr UnwindException
    handlerCount: int
    ttypeIndex: int
    lsda: pointer
    landingPad: pointer
    unwindException: UnwindException

  NlvmEhGlobals = object
    closureException: ref Exception
      ## The closure iterator transform will set the current exception that is
      ## supposed to be returned by getCurrentException, but by that time, we've
      ## already deallocated the nlvm-level exception handling data. We'll keep
      ## track of it here in a separate variable so that upstream generated code
      ## keeps working.
      ## TODO there's a bug in 0.20 where the code to unset it is missing from
      ##      the transform - thus we manually reset it every time a new
      ##      exception is raised
      ## TODO there's probably a better way to track these - just blindly
      ##      setting a global seems like trouble, for nested exceptions for
      ##      example...
    caughtExceptions: ptr UnwindException

proc isNil(x: UnwindContext): bool {.borrow.}

func isSet(action: UnwindAction, checkFor: UnwindAction): bool =
  (action and checkFor) > 0

func isNative(c: uint64): bool =
  c == nlvmExceptionClass

func isNative(e: UnwindException): bool =
  e.exceptionClass.isNative()

func offset(v: pointer, n: int): pointer =
  cast[pointer](cast[uint](v) + cast[uint](n))

func ptrAdd(v: var pointer, n: int) =
  v = v.offset(n)

func roundUp(v: var pointer, align: uint) =
  v = cast[pointer]((cast[uint](v) + align - 1) and not (align - 1))

func readBytes(v: var pointer, T: type): T =
  copyMem(addr result, v, sizeof(T))
  ptrAdd(v, sizeof(T))

# Compiler will write EH frame data using varible-length encoded integers - we
# trust that these fit in an uint64
func readUleb128(v: var pointer): uint64 =
  ## Read unsigned variable-length integer - no error checking
  var
    shift: uint

  while true:
    let b = v.readBytes(uint8)
    result = result or (cast[uint64](b and 0x7F) shl shift)
    shift += 7
    if (b and 0x80) == 0:
      break

func readSleb128(v: var pointer): int64 =
  ## Read signed variable-length integer - no error checking
  var
    shift: uint
    res: uint64
    b: uint8

  while true:
      b = v.readBytes(uint8)
      res = res or (cast[uint64](b and 0x7F) shl shift)
      shift += 7
      if (b and 0x80) == 0:
        break

  if shift < (8 * sizeof(uint64)) and ((b and 0x40) != 0):
    res = res or (not 0'u64) shl shift

  cast[int64](res)

proc readEncodedPointer(
    v: var pointer, ctx: UnwindContext, encoding: uint8): pointer =
  if encoding == DW_EH_PE_omit: return
  let tmp = v # when reading rel below, we have to use the original address, not
              # not the one incremented by readbytes.. interestingly, it seems
              # rust disagrees here - todo: investigate
  # DW_EH_PE_aligned implies it's an absolute pointer value
  if encoding == DW_EH_PE_aligned:
    roundUp(v, sizeof(uint).uint)
    return v.readBytes(pointer)

  let p =
    case encoding and 0x0F
    of DW_EH_PE_absptr: v.readBytes(uint)
    of DW_EH_PE_uleb128: cast[uint](v.readUleb128())
    of DW_EH_PE_udata2: cast[uint](v.readBytes(uint16))
    of DW_EH_PE_udata4: cast[uint](v.readBytes(uint32))
    of DW_EH_PE_udata8: cast[uint](v.readBytes(uint64))
    of DW_EH_PE_sleb128: cast[uint](v.readSleb128())
    of DW_EH_PE_sdata2: cast[uint](v.readBytes(int16))
    of DW_EH_PE_sdata4: cast[uint](v.readBytes(int32))
    of DW_EH_PE_sdata8: cast[uint](v.readBytes(int64))
    else: c_abort()

  if p == 0: return nil

  let rel =
    case encoding and 0x70
    of DW_EH_PE_absptr: 0.uint
    of DW_EH_PE_pcrel: cast[uint](tmp) # relative to address of the encoded value, despite the name
    of DW_EH_PE_funcrel: ctx.getRegionStart()
    of DW_EH_PE_textrel: ctx.getTextRelBase()
    of DW_EH_PE_datarel: ctx.getDataRelBase()
    else: c_abort()

  let loc = p + rel
  if (encoding and DW_EH_PE_indirect) != 0:
    cast[ptr pointer](loc)[]
  else:
    cast[pointer](loc)

func getThrownObjectPtr(e: ptr UnwindException): pointer =
  cast[pointer](e).offset(sizeof(UnwindException))

proc toUnwindException(e: ptr NlvmException): ptr UnwindException =
  addr e.unwindException

func toNlvmException(e: ptr UnwindException): ptr NlvmException =
  cast[ptr NlvmException](
    cast[uint](e) + sizeof(UnwindException).uint - sizeof(NlvmException).uint)

func toNimException(e: ptr NlvmException): ref Exception =
  e[].nimException

func toNimException(e: ptr UnwindException): ref Exception =
  e.toNlvmException().toNimException()

proc nlvmExceptionCleanup(
    unwindCode: UnwindReasonCode, exception: ptr UnwindException) {.cdecl.} =
  dprintf("Cleanup %p\n", exception)
  let nlvme = toNlvmException(exception)
  when declared(GC_unref):
    GC_unref(nlvme.nimException)
  c_free(cast[pointer](nlvme))

var ehGlobals{.threadvar.}: NlvmEhGlobals

proc unhandledException() {.noreturn.} =
  c_fprintf(cstderr, "Error: unhandled exception: [foreign]\n")

  quit(1) # TODO alternatively, quitOrDebug

proc unhandledException(e: ref Exception) {.noreturn.} =
  c_fprintf(
    cstderr, "Error: unhandled exception: %s [%s]\n", cstring(e.msg), e.name)

  quit(1) # TODO alternatively, quitOrDebug

func getNimTypePtr(
    ttypeIndex: int, classInfo: pointer, ttypeEncoding: uint8,
    ctx: UnwindContext): pointer =
  let
    ti =
      case ttypeEncoding and 0x0F'u8
      of DW_EH_PE_absptr: ttypeIndex * sizeof(pointer)
      of DW_EH_PE_udata2, DW_EH_PE_sdata2: ttypeIndex * 2
      of DW_EH_PE_udata4, DW_EH_PE_sdata4: ttypeIndex * 4
      of DW_EH_PE_udata8, DW_EH_PE_sdata8: ttypeIndex * 8
      else: c_abort()

  var tiPtr = classInfo.offset(-ti)
  tiPtr.readEncodedPointer(ctx, ttypeEncoding)

when defined(nimV2):
  type
    DestructorProc = proc (p: pointer) {.nimcall, benign, raises: [].}
    TNimTypeV2 = object
      destructor: pointer
      size: int
      align: int
      name: cstring
      traceImpl: pointer
      typeInfoV1: pointer # for backwards compat, usually nil
      flags: int
    PNimTypeV2 = ptr TNimTypeV2

  func exceptionType(e: ref Exception): PNimTypeV2 =
    # return the dynamic type of an exception, which nlvm stores at the beginning
    # of the object
    cast[ptr PNimTypeV2](unsafeAddr e[])[]

  func getNimType(
      ttypeIndex: int, classInfo: pointer, ttypeEncoding: uint8,
      ctx: UnwindContext): PNimTypeV2 =
    cast[PNimTypeV2](getNimTypePtr(ttypeIndex, classInfo, ttypeEncoding, ctx))

  proc memcmp(str1, str2: cstring, n: csize_t): cint {.importc, header: "<string.h>".}

  func endsWith(s, suffix: cstring): bool {.inline.} =
    let
      sLen = s.len
      suffixLen = suffix.len

    if suffixLen <= sLen:
      result = memcmp(cstring(unsafeAddr s[sLen - suffixLen]), suffix, csize_t(suffixLen)) == 0

  proc isObj(obj: PNimTypeV2, subclass: cstring): bool {.compilerRtl, inl.} =
    endsWith(obj.name, subclass)

  func canCatch(catchType, thrownType: PNimTypeV2): bool =
    isObj(thrownType, catchType.name)
else:
  func exceptionType(e: ref Exception): PNimType =
    # return the dynamic type of an exception, which nlvm stores at the beginning
    # of the object
    cast[ptr PNimType](unsafeAddr e[])[]

  func getNimType(
      ttypeIndex: int, classInfo: pointer, ttypeEncoding: uint8,
      ctx: UnwindContext): PNimType =
    cast[PNimType](getNimTypePtr(ttypeIndex, classInfo, ttypeEncoding, ctx))

import system/memory

proc nlvmRaise(e: ref Exception) {.compilerproc, noreturn.} =
  const esize = csize_t(sizeof NlvmException)
  let excMem = c_malloc(esize)
  nimZeroMem(excMem, esize)
  let exc = cast[ptr NlvmException](excMem)

  exc.unwindException.exceptionClass = nlvmExceptionClass
  exc.unwindException.exceptionCleanup = nlvmExceptionCleanup
  exc.nimException = e

  # Help keep Nim exception around
  when declared(GC_ref): # But only when using gc!
    dprintf("gcref %p\n", addr e[])
    GC_ref(e)

  let unwindException = exc.toUnwindException()
  dprintf(
    "Raising %s %p %p\n", cstring(e.name), unwindException, e.exceptionType())

  let reason = raiseException(unwindException)

  case reason
  of URC_END_OF_STACK:
    unhandledException(e)
  else:
    # "shouldn't happen"
    c_fprintf(cstderr, "Error: cannot raise exception: %d\n", reason)

  c_abort()

proc nlvmReraise() {.compilerproc, noreturn.} =
  let unwindException = ehGlobals.caughtExceptions
  if unwindException.isNil():
    nlvmRaise((ref ReraiseDefect)(msg: "no exception to reraise"))

  dprintf("Reraise %p\n", unwindException)
  ehGlobals.closureException = nil  # Just in case, see workaround notes

  if unwindException[].isNative():
    let exceptionHeader = unwindException.toNlvmException()
    exceptionHeader.handlerCount = -exceptionHeader.handlerCount

  else:
    ehGlobals.caughtExceptions = nil

  let reason = raiseException(unwindException)

  case reason
  of URC_END_OF_STACK:
    if unwindException[].isNative():
      unhandledException(unwindException.toNimException())
    else:
      unhandledException()
  else:
    # "shouldn't happen"
    c_fprintf(cstderr, "Error: cannot reraise exception: %d\n", reason)

  c_abort()

proc nlvmSetClosureException(e: ref Exception) {.compilerproc.} =
  ehGlobals.closureException = e

proc nlvmGetCurrentException(): ref Exception {.compilerproc.} =
  if not ehGlobals.closureException.isNil:
    ehGlobals.closureException
  else:
    let unwindException = ehGlobals.caughtExceptions
    dprintf("current: %p\n", unwindException)
    if unwindException.isNil(): nil
    else: unwindException.toNimException()

proc nlvmGetCurrentExceptionMsg(): string {.compilerproc.} =
  let e = nlvmGetCurrentException()
  if e != nil: e.msg else: ""

proc nlvmBeginCatch(unwindArg: pointer) {.compilerproc.} =
  dprintf("begin catch %p\n", unwindArg)
  ehGlobals.closureException = nil  # Just in case, see workaround notes

  let
    unwindException = cast[ptr UnwindException](unwindArg)

  if unwindException[].isNative():
    let exceptionHeader = unwindException.toNlvmException()
    dprintf("begin native %d\n", exceptionHeader.handlerCount)
    exceptionHeader.handlerCount =
      if exceptionHeader.handlerCount < 0:
        -exceptionHeader.handlerCount + 1
      else:
        exceptionHeader.handlerCount + 1

    if ehGlobals.caughtExceptions != unwindException:
      exceptionHeader.nextException = ehGlobals.caughtExceptions
      ehGlobals.caughtExceptions = unwindException
  else:
    if not ehGlobals.caughtExceptions.isNil():
      c_fprintf(cstderr, "Error: trying to catch nested foreign exception\n")
      c_abort()

    ehGlobals.caughtExceptions = unwindException

proc nlvmEndCatch() {.compilerproc.} =
  dprintf("end catch %p\n", ehGlobals.caughtExceptions)

  let unwindException = ehGlobals.caughtExceptions
  if unwindException.isNil():
    # rethrown foreign exception
    return

  ehGlobals.closureException = nil  # Just in case, see workaround notes

  if unwindException[].isNative():
    let exceptionHeader = unwindException.toNlvmException()
    dprintf("end native %d\n", exceptionHeader.handlerCount)
    if exceptionHeader.handlerCount < 0:
      # Rethrowing
      exceptionHeader.handlerCount += 1
      if exceptionHeader.handlerCount == 0:
        ehGlobals.caughtExceptions = exceptionHeader.nextException
    else:
      # TODO When passing exceptions between threads is supported, this needs
      #      to be atomic..
      exceptionHeader.handlerCount -= 1
      if exceptionHeader.handlerCount == 0:
        ehGlobals.caughtExceptions = exceptionHeader.nextException

        when declared(GC_unref):
          let e = exceptionHeader.toNimException()
          dprintf("gcunref %p\n", addr e[])
          GC_unref(e)

        c_free(toNlvmException(unwindException))
  else:
    deleteException(ehGlobals.caughtExceptions)
    ehGlobals.caughtExceptions = nil

func canCatch(catchType, thrownType: PNimType): bool =
  var tmp = thrownType

  while tmp != nil:
    if tmp == catchType: return true
    tmp = tmp.base

type
  ScanResult = object
    ttypeIndex: int # > 0 catch handler, < 0 exception spec handler, == 0 a cleanup
    landingPad: pointer # null -> nothing found, else something found
    reason: UnwindReasonCode

when defined(nimV2):
  type PNimTypeVX = PNimTypeV2
else:
  type PNimTypeVX = PNimType


func exceptionSpecCanCatch(
    specIndex: int, classInfo: pointer, ttypeEncoding: uint8,
    excpType: PNimTypeVX, ctx: UnwindContext): bool =
  # specIndex is negative of 1-based byte offset into classInfo
  var specIndex = -specIndex

  specIndex -= 1
  var temp = classInfo.offset(specIndex.int)

  # If any type in the spec list can catch excpType, return false, else return true
  while true:
    let ttypeIndex = cast[int](temp.readUleb128())
    if ttypeIndex == 0:
      break

    let catchType = getNimType(ttypeIndex, classInfo, ttypeEncoding, ctx)
    if catchType.canCatch(excpType):
      return false

  return true

func scanEHTable(
    actions: UnwindAction, native: bool, unwindException: ptr UnwindException,
    ctx: UnwindContext): ScanResult =
  # Sanity checking of actions
  if actions.isSet(UA_SEARCH_PHASE):
    if actions.isSet(UA_CLEANUP_PHASE or UA_HANDLER_FRAME or UA_FORCE_UNWIND):
      return ScanResult(reason: URC_FATAL_PHASE1_ERROR)

  elif actions.isSet(UA_CLEANUP_PHASE):
    if actions.isSet(UA_HANDLER_FRAME) and actions.isSet(UA_FORCE_UNWIND):
      return ScanResult(reason: URC_FATAL_PHASE2_ERROR)

  else: # Either of search/cleanup must be set
    return ScanResult(reason: URC_FATAL_PHASE1_ERROR)

  let lsda = ctx.getLanguageSpecificData()
  if lsda == 0:
    return ScanResult(reason: URC_CONTINUE_UNWIND)

  var ipBeforeInsn: cint
  let
    ipp = ctx.getIPInfo(addr ipBeforeInsn)
    ip = ipp - (if ipBeforeInsn == 0: 1 else: 0)
    funcStart = ctx.getRegionStart()
    ipOffset = ip - funcStart

  var header = cast[pointer](lsda)
  let
    startEncoding = header.readBytes(uint8)
    lpStartEncoded = header.readEncodedPointer(ctx, startEncoding)
    lpStart =
      if lpStartEncoded.isNil(): cast[pointer](funcStart) else: lpStartEncoded

  let
    ttypeEncoding = header.readBytes(uint8)
    classInfo =
      if ttypeEncoding != DW_EH_PE_omit:
        let ciOffset = header.readUleb128()
        header.offset(ciOffset.int)
      else: nil

  # Call site
  let
    csEncoding = header.readBytes(uint8)
    csTableLen = header.readUleb128().int
    csTableStart = header
    csTableEnd = header.offset(csTableLen)
    atStart = csTableEnd

  var csPtr = csTableStart
  while csPtr < csTableEnd:
    let
      csStart = cast[uint](csPtr.readEncodedPointer(ctx, csEncoding))
      csLen = cast[uint](csPtr.readEncodedPointer(ctx, csEncoding))
      cslpad = cast[int](csPtr.readEncodedPointer(ctx, csEncoding))
      actionEntry = cast[int](csPtr.readUleb128())

    if (csStart <= ipOffset) and (ipOffset < (csStart + csLen)):
      # Found call site
      if cslpad == 0:
        dprintf("no cslpad\n")
        return ScanResult(reason: URC_CONTINUE_UNWIND)

      let landingPad = lpStart.offset(cslpad)
      if actionEntry == 0:
        # Cleanup
        if actions.isSet(UA_CLEANUP_PHASE) and not actions.isSet(UA_HANDLER_FRAME):
          return ScanResult(
            ttypeIndex: 0,
            landingPad: landingPad,
            reason: URC_HANDLER_FOUND
          )
        dprintf("cleanup\n")
        return ScanResult(reason: URC_CONTINUE_UNWIND)

      var action = atStart.offset(actionEntry - 1)

      while true:
        # we're not going to have more types than fits in the natural pointer
        # size of the machine
        let ttypeIndex = cast[int](action.readSleb128())
        dprintf("trying index %d\n", ttypeIndex)

        if ttypeIndex > 0:
          # Catch
          let catchType =
            getNimType(ttypeIndex, classInfo, ttypeEncoding, ctx)
          dprintf("catch type %p\n", catchType)

          if catchType.isNil():
            # Catch-all
            if actions.isSet(UA_SEARCH_PHASE) or actions.isSet(UA_HANDLER_FRAME):
              # Handler found
              return ScanResult(
                ttypeIndex: ttypeIndex,
                landingPad: landingPad,
                reason: URC_HANDLER_FOUND
              )
            elif not actions.isSet(UA_FORCE_UNWIND):
              # stack corruption maybe, according to libcxxabi
              c_abort()
            # TODO drop off here?
          elif native:
            let
              exceptionHeader = unwindException.toNlvmException()
              excpType = exceptionHeader.nimException.exceptionType()

            if excpType.isNil():
              c_abort()

            if catchType.canCatch(excpType):
              if actions.isSet(UA_SEARCH_PHASE):
                return ScanResult(
                  ttypeIndex: ttypeIndex,
                  landingPad: landingPad,
                  reason: URC_HANDLER_FOUND
                )
              elif not actions.isSet(UA_FORCE_UNWIND):
                # stack corruption maybe, according to libcxxabi
                c_abort()

        elif ttypeIndex < 0:
          # Exception spec
          if native:
            let
              exceptionHeader = unwindException.toNlvmException()
              excpType = exceptionHeader.nimException.exceptionType()

            if excpType.isNil():
              c_abort()

            if exceptionSpecCanCatch(
                ttypeIndex, classInfo, ttypeEncoding, excpType, ctx):
              if actions.isSet(UA_SEARCH_PHASE):
                return ScanResult(
                  ttypeIndex: ttypeIndex,
                  landingPad: landingPad,
                  reason: URC_HANDLER_FOUND
                )
              elif not actions.isSet(UA_FORCE_UNWIND):
                # stack corruption maybe, according to libcxxabi
                c_abort()
          else:
            if actions.isSet(UA_SEARCH_PHASE) or actions.isSet(UA_HANDLER_FRAME):
              return ScanResult(
                ttypeIndex: ttypeIndex,
                landingPad: landingPad,
                reason: URC_HANDLER_FOUND
              )
            elif not actions.isSet(UA_FORCE_UNWIND):
              # stack corruption maybe, according to libcxxabi
              c_abort()
        else: # ttypeIndex == 0
          if actions.isSet(UA_CLEANUP_PHASE) and not actions.isSet(UA_HANDLER_FRAME):
            return ScanResult(
              ttypeIndex: ttypeIndex,
              landingPad: landingPad,
              reason: URC_HANDLER_FOUND
            )

        var temp = action
        let actionOffset = temp.readSleb128()
        if actionOffset == 0:
            # End of action list, no matching handler or cleanup found
            dprintf("noaction\n")
            return ScanResult(reason: URC_CONTINUE_UNWIND)

        action.ptrAdd(actionOffset.int)

  # No EH table entry matched..
  c_abort()

proc setRegisters(
    unwindException: ptr UnwindException, ctx: UnwindContext,
    results: ScanResult) =
  ctx.setGR(UNWIND_DATA_REG[0], cast[uint](unwindException))
  ctx.setGR(UNWIND_DATA_REG[1], cast[uint](results.ttypeIndex))
  ctx.setIP(cast[uint](results.landingPad))

proc nlvmEHPersonality(
    version: cint,
    actions: UnwindAction,
    exceptionClass: uint64,
    unwindException: ptr UnwindException,
    ctx: UnwindContext): UnwindReasonCode {.compilerproc.} =
  ## Personality function called whenever the exception handling stack is being
  ## examined by libunwind
  if version != 1 or unwindException.isNil() or ctx.isNil():
    return URC_FATAL_PHASE1_ERROR

  let native = exceptionClass.isNative()

  if actions.isSet(UA_SEARCH_PHASE):
    let results = scanEHTable(actions, native, unwindException, ctx)
    dprintf("Scan %d\n", (results.reason))
    if results.reason == URC_HANDLER_FOUND and native:
      # Cache values
      let exceptionHeader = unwindException.toNlvmException()
      exceptionHeader.ttypeIndex = results.ttypeIndex.int
      exceptionHeader.landingPad = results.landingPad

    results.reason

  elif actions.isSet(UA_CLEANUP_PHASE):
    if actions.isSet(UA_HANDLER_FRAME):
      # Found catch handler in phase one
      let results =
        if native:
          # Load cached values
          let exceptionHeader = unwindException.toNlvmException()
          ScanResult(
            ttypeIndex: exceptionHeader.ttypeIndex,
            landingPad: exceptionHeader.landingPad,
            reason: URC_HANDLER_FOUND
          )
        else:
          scanEHTable(actions, native, unwindException, ctx)

      if results.reason != URC_HANDLER_FOUND:
        # Found in phase1 but not in phase 2?
        c_abort()

      setRegisters(unwindException, ctx, results)
      URC_INSTALL_CONTEXT

    else:
      let results = scanEHTable(actions, native, unwindException, ctx)
      if results.reason == URC_HANDLER_FOUND:
        dprintf("scan install\n")
        setRegisters(unwindException, ctx, results)
        URC_INSTALL_CONTEXT
      else:
        results.reason
  else:
    URC_FATAL_PHASE1_ERROR

{.pop.}