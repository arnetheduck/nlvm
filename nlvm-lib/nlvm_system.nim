# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016-2019
# See the LICENSE file for license info (doh!)

# Exception handling helpers on top of libunwind - based on rust and c++
# variants..

# https://itanium-cxx-abi.github.io/cxx-abi/abi-eh.html

import system/ansi_c

const
  # UnwindReasonCode
  URC_NO_REASON = 0
  URC_FOREIGN_EXCEPTION_CAUGHT = 1
  URC_FATAL_PHASE2_ERROR = 2
  URC_FATAL_PHASE1_ERROR = 3
  URC_NORMAL_STOP = 4
  URC_END_OF_STACK = 5
  URC_HANDLER_FOUND = 6
  URC_INSTALL_CONTEXT = 7
  URC_CONTINUE_UNWIND = 8
  URC_FAILURE = 9 # used only by ARM EHABI

  # UnwindAction
  UA_SEARCH_PHASE = 1
  UA_CLEANUP_PHASE = 2
  UA_HANDLER_FRAME = 4
  UA_FORCE_UNWIND = 8
  UA_END_OF_STACK = 16

  UnwinderPrivateDataSize =
    when defined(x86): 5
    elif defined(amd64): 6
    else: {.fatal: "EH not implemented here yet..." .}

  UNWIND_DATA_REG =
    when defined(x86): [0.cint, 2] # EAX, EDX
    elif defined(amd64): [0.cint, 1] # RAX, RDX
    else: {.fatal: "EH not implemented here yet..." .}

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

# ok, this is a bit dumb:
include system/inclrtl, system/hti

# Chicken and egg in system.nim...
template `+=`(x: var SomeInteger, y: SomeInteger) = x = x + y
template `-=`(x: var SomeInteger, y: SomeInteger) = x = x + y

type
  UnwindReasonCode = int32 # TODO verify size
  UnwindAction = int32 # TODO verify size
  UnwindExceptionClass = uint64
  UnwindWord = uint # TODO verify size
  UnwindContext = distinct pointer
  UnwindTraceFn = proc(ctx: UnwindContext, arg: pointer): UnwindReasonCode {.cdecl.}
  UnwindExceptionCleanupFn = proc(unwindCode: UnwindReasonCode, exception: ptr UnwindException) {.cdecl.}

  UnwindException = object
    exceptionClass: UnwindExceptionClass
    exceptionCleanup: UnwindExceptionCleanupFn
    privateData: array[UnwinderPrivateDataSize, UnwindWord]

  NlvmException = object
    nimException: ref Exception
    nextException: ptr UnwindException
    handlerCount: int
    handlerSwitchValue: int
    actionRecord: pointer
    lsda: pointer
    catchTemp: pointer

    # Last by convention, so the object can grow "backwards"
    # TODO this doesn't quite make sense with nim's GC and keeping a ref above
    unwindException: UnwindException

  NlvmEhGlobals = object
    caughtExceptions: ptr UnwindException
    uncaughtExceptions: int

proc isNil(x: UnwindContext): bool {.borrow.}

func isSet(action: UnwindAction, checkFor: UnwindAction): bool =
  (action and checkFor) > 0

func isNative(c: UnwindExceptionClass): bool =
  c == nlvmExceptionClass

func isNative(e: UnwindException): bool =
  e.exceptionClass.isNative()

proc deleteException(exception: ptr UnwindException) {.importc: "_Unwind_DeleteException".}
proc getDataRelBase(ctx: UnwindContext): pointer {.importc: "_Unwind_GetDataRelBase".}
proc getIP(ctx: UnwindContext): UnwindWord {.importc: "_Unwind_GetIP".}
proc getLanguageSpecificData(ctx: UnwindContext): pointer {.importc: "_Unwind_GetLanguageSpecificData".}
proc getRegionStart(ctx: UnwindContext): pointer {.importc: "_Unwind_GetRegionStart".}
proc getTextRelBase(ctx: UnwindContext): pointer {.importc: "_Unwind_GetTextRelBase".}
proc raiseException(exception: ptr UnwindException): UnwindReasonCode {.importc: "_Unwind_RaiseException".}
proc resume(exception: ptr UnwindException) {.noreturn, importc: "_Unwind_Resume".}
proc setGR(ctx: UnwindContext, reg_index: cint, value: UnwindWord) {.importc: "_Unwind_SetGR".}
proc setIP(ctx: UnwindContext, value: UnwindWord) {.importc: "_Unwind_SetIP".}

# TODO upstream: needs noreturn!
proc c_abort() {.
  importc: "abort", header: "<stdlib.h>", noreturn.}
proc c_exit(v: cint) {.
  importc: "exit", header: "<stdlib.h>", noreturn.}

func offset(v: pointer, n: int): pointer =
  cast[pointer](cast[int](v) + n)

func ptrAdd(v: var pointer, n: int) =
  v = v.offset(n)

func roundUp(v: var pointer, align: uint) =
  v = cast[pointer]((cast[uint](v) + align - 1) and not (align - 1))

func readBytes(v: var pointer, T: type): T =
  copyMem(addr result, v, sizeof(T))
  ptrAdd(v, sizeof(T))

func readUleb128(v: var pointer): uint64 =
  var
    shift: uint

  while true:
    let b = v.readBytes(uint8)
    result = result or (((b and 0x7F).uint64) shl shift)
    shift += 7
    if (b and 0x80) == 0:
      break

func readSleb128(v: var pointer): int64 =
  var
    shift: uint
    res: uint64
    b: uint8

  while true:
      b = v.readBytes(uint8)
      res = res or ((b and 0x7F).uint64) shl shift
      shift += 7
      if (b and 0x80) == 0:
        break

  if shift < (8 * sizeof(uint64)) and (b and 0x40) != 0:
    res = res or (not 0'u64) shl shift

  cast[int64](res)

proc readEncodedPointer(
    v: var pointer,
    context: UnwindContext,
    encoding: uint8): pointer =
  if encoding == DW_EH_PE_omit: return
  let tmp = v # when reading rel below, we have to use the original address, not
              # not the one incremented by readbytes.. interestingly, it seems
              # rust disagrees here - todo: investigate
  # DW_EH_PE_aligned implies it's an absolute pointer value
  if encoding == DW_EH_PE_aligned:
    roundUp(v, sizeof(uint).uint)
    return v.readBytes(pointer)

  # TODO errors?
  let p =
    case encoding and 0x0F
    of DW_EH_PE_absptr: v.readBytes(uint)
    of DW_EH_PE_uleb128: v.readUleb128().uint
    of DW_EH_PE_udata2: v.readBytes(uint16).uint
    of DW_EH_PE_udata4: v.readBytes(uint32).uint
    of DW_EH_PE_udata8: v.readBytes(uint64).uint
    of DW_EH_PE_sleb128: v.readSleb128().uint
    of DW_EH_PE_sdata2: v.readBytes(int16).uint
    of DW_EH_PE_sdata4: v.readBytes(int32).uint
    of DW_EH_PE_sdata8: v.readBytes(int64).uint
    else: c_abort()

  let rel =
    case encoding and 0x70
    of DW_EH_PE_absptr: 0.uint
    of DW_EH_PE_pcrel: cast[uint](tmp) # relative to address of the encoded value, despite the name
    of DW_EH_PE_funcrel: cast[uint](context.getRegionStart())
    of DW_EH_PE_textrel: cast[uint](context.getTextRelBase())
    of DW_EH_PE_datarel: cast[uint](context.getDataRelBase())
    else: c_abort()

  let loc = p + rel
  if (encoding and DW_EH_PE_indirect) != 0:
    return cast[ptr pointer](loc)[]

  cast[pointer](loc)

func getThrownObjectPtr(e: ptr UnwindException): pointer =
  cast[pointer](e).offset(sizeof(UnwindException))

func toUnwindException(e: ptr NlvmException): ptr UnwindException =
  addr e.unwindException

func toNlvmException(e: ptr UnwindException): ref NlvmException =
  cast[ref NlvmException](
    cast[uint](e) + sizeof(UnwindException).uint - sizeof(NlvmException).uint)

func toNimException(e: ref NlvmException): ref Exception =
  e[].nimException

func toNimException(e: ptr UnwindException): ref Exception =
  e.toNlvmException().toNimException()

proc nlvmEHCleanup(
    unwindCode: UnwindReasonCode, exception: ptr UnwindException) {.cdecl.} =
  # c_printf("Cleaning up %p\n", exception)
  let nlvme = toNlvmException(exception)
  GC_unref(nlvme.nimException)
  # c_free(cast[pointer](nlvme))

# TODO avoid this global? we need it for.. reraising only?
var ehGlobals{.threadvar.}: NlvmEhGlobals

proc unhandledException(e: ref Exception) {.noreturn.} =
  rawWrite cstderr, "Error: unhandled exception: " & e.msg & " [" & $e.name & "]\n"
  c_exit(1) # TODO alternatively, quitOrDebug

func exceptionType(e: ref Exception): PNimType =
  # TODO another hack: we know that the dynamic type of the object is stored at
  #      the first location so we simply cast and get the type from there...
  cast[ptr PNimType](unsafeaddr e)[]

proc nlvmEHRaise(e: ref Exception, ename: cstring) {.compilerProc, noreturn.} =
  if e.name.isNil: e.name = ename

  # TODO it would have been nicer to allocate the exception header struct
  #      together with the rest of the exception.. hopefully the GC can deal
  #      with this stuff (?)

  # TODO for reasons unknown, using `new` aka the GC here fails, even if we
  #      GC_ref the given reference - in release mode, the personality function
  #      ends up not being called and the handler not found. hmm...
  var exc = cast[ptr NlvmException](c_malloc(sizeof NlvmException))

  exc.unwindException.exceptionClass = nlvmExceptionClass
  exc.unwindException.exceptionCleanup = nlvmEHCleanup
  exc.nimException = e

  # Help keep both Nim exception and unwind exception around
  GC_ref(e)
  # c_printf("ref %p\n", unsafeAddr exc)

  ehGlobals.uncaughtExceptions += 1

  let unwindException = exc.toUnwindException()

  # c_printf("Raising %p\n", unwindException)

  # TODO we rely here on the layout of NlvmException keeping the unwindException
  #      first in memory
  let reason = raiseException(unwindException)

  case reason
  of URC_END_OF_STACK:
    unhandledException(e)
  else:
    # "shouldn't happen"
    c_printf("Error while raising exception, terminating: %d\n", reason)

  c_abort()

proc nlvmEHReraise() {.compilerproc, noreturn.} =
  # c_printf("Re-raising\n")
  let unwindException = ehGlobals.caughtExceptions
  if unwindException.isNil():
    c_abort() # reraising outside of catch

  if unwindException[].isNative():
    let exceptionHeader = unwindException.toNlvmException()
    exceptionHeader.handlerCount = -exceptionHeader.handlerCount
    ehGlobals.uncaughtExceptions += 1
    # c_printf("ref reraise %p\n", unsafeAddr exceptionHeader)

  else:
    ehGlobals.caughtExceptions = nil

  let reason = raiseException(unwindException)

  case reason
  of URC_END_OF_STACK:
    if unwindException[].isNative():
      unhandledException(unwindException.toNimException())
    else:
      c_printf("Unhandled exception\n")
      c_exit(1)
  else:
    # "shouldn't happen"
    c_printf("Error while reraising exception, terminating: %d\n", reason)

  c_abort()

proc nlvmGetCurrentException(): ref Exception {.compilerproc.} =
  let unwindException = ehGlobals.caughtExceptions

  if unwindException.isNil(): nil
  else: unwindException.toNimException()

proc nlvmBeginCatch(unwindArg: pointer) {.compilerproc, raises: [].} =
  # c_printf("begin catch %p\n", unwindArg)

  let
    unwindException = cast[ptr UnwindException](unwindArg)

  if unwindException[].isNative():
    let exceptionHeader = unwindException.toNlvmException()
    exceptionHeader.handlerCount =
      if exceptionHeader.handlerCount < 0:
        -exceptionHeader.handlerCount + 1
      else:
        exceptionHeader.handlerCount + 1

    if ehGlobals.caughtExceptions != unwindException:
      exceptionHeader.nextException = ehGlobals.caughtExceptions
      ehGlobals.caughtExceptions = unwindException

    ehGlobals.uncaughtExceptions -= 1
  else:
    if not ehGlobals.caughtExceptions.isNil():
      c_abort()

    ehGlobals.caughtExceptions = unwindException

proc nlvmEndCatch() {.compilerproc.} =
  # c_printf("end catch %p\n", ehGlobals.caughtExceptions)

  let unwindException = ehGlobals.caughtExceptions
  if unwindException.isNil():
    # rethrown foreign exception
    return

  if unwindException[].isNative():
    let exceptionHeader = unwindException.toNlvmException()
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
        c_printf("unref  %p\n", unsafeAddr exceptionHeader)

        GC_unref(exceptionHeader.toNimException())
        c_free(unwindException)
  else:
    deleteException(ehGlobals.caughtExceptions)
    ehGlobals.caughtExceptions = nil

proc nlvmBadCleanup() {.compilerproc, noreturn.} =
  # Cleanup failed, exception was raised during end-catch (?)
  c_abort()

func getNimType(
    ttypeIndex: int64, classInfo: pointer, ttypeEncoding: uint8,
    context: UnwindContext): PNimType =
  if classInfo.isNil():
    c_abort()

  let
    ti =
      case ttypeEncoding and 0x0F'u8
      of DW_EH_PE_absptr: ttypeIndex.int * sizeof(pointer)
      of DW_EH_PE_udata2, DW_EH_PE_sdata2: ttypeIndex.int * 2
      of DW_EH_PE_udata4, DW_EH_PE_sdata4: ttypeIndex.int * 4
      of DW_EH_PE_udata8, DW_EH_PE_sdata8: ttypeIndex.int * 8
      else: c_abort()

  var tiPtr = classInfo.offset(-ti)
  cast[PNimType](tiPtr.readEncodedPointer(context, ttypeEncoding))

type
  ScanResult = object
    ttypeIndex: int64 # > 0 catch handler, < 0 exception spec handler, == 0 a cleanup
    lsda: pointer # Needed only for __cxa_call_unexpected
    landingPad: pointer # null -> nothing found, else something found
    reason: UnwindReasonCode

func canCatch(catchType, thrownType: PNimType, adjustedPtr: pointer): bool =
  var tmp = thrownType

  while tmp != nil:
    if tmp == catchType: return true
    tmp = tmp.base

func exceptionSpecCanCatch(
    specIndex: int64, classInfo: pointer, ttypeEncoding: uint8,
    excpType: PNimType, adjustedPtr: pointer,
    context: UnwindContext): bool =
  if classInfo.isNil():
    # corrupted eh_table
    c_abort()

  # specIndex is negative of 1-based byte offset into classInfo
  var specIndex = -specIndex

  specIndex -= 1
  var temp = classInfo.offset(specIndex.int)

  # If any type in the spec list can catch excpType, return false, else return true
  # adjustments to adjustedPtr are ignored.
  while true:
    let ttypeIndex = temp.readULEB128()
    if ttypeIndex == 0:
      break

    let catchType = getNimType(
      ttypeIndex.int64, classInfo, ttypeEncoding, context)

    let tempPtr = adjustedPtr
    if catchType.canCatch(excpType, tempPtr): return false

  return true

func scanEHTable(
    actions: UnwindAction, native: bool, unwindException: ptr UnwindException,
    context: UnwindContext): ScanResult =
  # Sanity checking of actions
  if actions.isSet(UA_SEARCH_PHASE):
    if actions.isSet(UA_CLEANUP_PHASE or UA_HANDLER_FRAME or UA_FORCE_UNWIND):
      return ScanResult(reason: URC_FATAL_PHASE1_ERROR)

  elif actions.isSet(UA_CLEANUP_PHASE):
    if actions.isSet(UA_HANDLER_FRAME) and actions.isSet(UA_FORCE_UNWIND):
      return ScanResult(reason: URC_FATAL_PHASE2_ERROR)

  else: # Either of search/cleanup must be set
    return ScanResult(reason: URC_FATAL_PHASE1_ERROR)

  let lsda = context.getLanguageSpecificData()
  if lsda.isNil():
    return ScanResult(reason: URC_CONTINUE_UNWIND)

  let
    ip = context.getIP() - 1
    funcStart = context.getRegionStart()
    ipOffset = cast[uint](ip) - cast[uint](funcStart)

  var header = lsda
  let
    startEncoding = header.readBytes(uint8)
    lpStartEncoded = header.readEncodedPointer(context, startEncoding)
    lpStart = if lpStartEncoded.isNil(): funcStart else: lpStartEncoded

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
      csStart = cast[uint](csPtr.readEncodedPointer(context, csEncoding))
      csLen = cast[uint](csPtr.readEncodedPointer(context, csEncoding))
      cslpad = cast[int](csPtr.readEncodedPointer(context, csEncoding))
      actionEntry = csPtr.readUleb128()

    if (csStart <= ipOffset) and (ipOffset < (csStart + csLen)):
      # Found call site
      if cslpad == 0:
        # c_printf("no cslpad\n")
        return ScanResult(reason: URC_CONTINUE_UNWIND)

      let landingPad = lpStart.offset(cslpad)
      if actionEntry == 0:
        # Cleanup
        if actions.isSet(UA_CLEANUP_PHASE) and not actions.isSet(UA_HANDLER_FRAME):
          return ScanResult(
            lsda: lsda,
            landingPad: landingPad,
            reason: URC_HANDLER_FOUND
          )
        # c_printf("cleanup\n")
        return ScanResult(reason: URC_CONTINUE_UNWIND)

      var action = atStart.offset(actionEntry.int - 1)

      while true:
        let ttypeIndex = action.readSleb128()
        if ttypeIndex > 0:
          # Catch
          let catchType =
            getNimType(ttypeIndex, classInfo, ttypeEncoding, context)

          if catchType.isNil():
            # Catch-all
            if actions.isSet(UA_SEARCH_PHASE) or actions.isSet(UA_HANDLER_FRAME):
              # Handler found
              return ScanResult(
                lsda: lsda,
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
              adjustedPtr = getThrownObjectPtr(unwindException)
              excpType = exceptionHeader.nimException.exceptionType()

            if adjustedPtr.isNil() or excpType.isNil():
              c_abort()

            if catchType.canCatch(excpType, adjustedPtr):
              if actions.isSet(UA_SEARCH_PHASE):
                return ScanResult(
                  lsda: lsda,
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
              adjustedPtr = getThrownObjectPtr(unwindException)
              excpType = exceptionHeader.nimException.exceptionType()

            if adjustedPtr.isNil() or excpType.isNil():
              c_abort()

            if exceptionSpecCanCatch(
                ttypeIndex, classInfo, ttypeEncoding, excpType, adjustedPtr,
                context):
              if actions.isSet(UA_SEARCH_PHASE):
                return ScanResult(
                  lsda: lsda,
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
                lsda: lsda,
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
              lsda: lsda,
              ttypeIndex: ttypeIndex,
              landingPad: landingPad,
              reason: URC_HANDLER_FOUND
            )

        var temp = action
        let actionOffset = temp.readSLEB128()
        if actionOffset == 0:
            # End of action list, no matching handler or cleanup found
            # c_printf("noaction\n")
            return ScanResult(reason: URC_CONTINUE_UNWIND)

        action.ptrAdd(actionOffset.int)

  # No EH table entry matched..
  c_abort()

proc setRegisters(
    unwindException: ptr UnwindException, context: UnwindContext,
    results: ScanResult) =
  context.setGR(UNWIND_DATA_REG[0], cast[UnwindWord](unwindException))
  context.setGR(UNWIND_DATA_REG[1], cast[UnwindWord](results.ttypeIndex))
  context.setIP(cast[UnwindWord](results.landingPad))

proc nlvmEHPersonality(
    version: cint,
    actions: UnwindAction,
    exceptionClass: UnwindExceptionClass,
    unwindException: ptr UnwindException,
    context: UnwindContext): UnwindReasonCode {.exportc: "__nlvm_personality_v0", compilerproc.} =

  # c_printf("Personality %d %d %p %p\n", version, actions, unwindException, context)

  if version != 1 or unwindException.isNil() or context.isNil():
    return URC_FATAL_PHASE1_ERROR

  let native = exceptionClass == nlvmExceptionClass

  if actions.isSet(UA_SEARCH_PHASE):
    let results = scanEHTable(actions, native, unwindException, context)
    # c_printf("Scan %d\n", (results.reason))
    if results.reason == URC_HANDLER_FOUND and native:
      # Cache values
      let exceptionHeader = unwindException.toNlvmException()
      exceptionHeader.handlerSwitchValue = results.ttypeIndex.int
      exceptionHeader.lsda = results.lsda
      exceptionHeader.catchTemp = results.landingPad

    return results.reason

  if actions.isSet(UA_CLEANUP_PHASE):
    if actions.isSet(UA_HANDLER_FRAME):
      # Found catch handler in phase one
      let results =
        if native:
          # Load cached values
          let exceptionHeader = unwindException.toNlvmException()
          ScanResult(
            lsda: exceptionHeader.lsda,
            ttypeIndex: exceptionHeader.handlerSwitchValue,
            landingPad: exceptionHeader.catchTemp,
            reason: URC_HANDLER_FOUND
          )
        else:
          scanEHTable(actions, native, unwindException, context)

      if results.reason != URC_HANDLER_FOUND:
        # Found in phase1 but not in phase 2?
        c_abort()
      # c_printf("cache install\n")
      setRegisters(unwindException, context, results)
      return URC_INSTALL_CONTEXT

    let results = scanEHTable(actions, native, unwindException, context)
    if results.reason == URC_HANDLER_FOUND:
      # c_printf("scan install\n")
      setRegisters(unwindException, context, results)
      return URC_INSTALL_CONTEXT
    # c_printf("scan reason\n")

    return results.reason

  return URC_FATAL_PHASE1_ERROR
