# Converted `unwind.h` from llvm libunwind - this _should_ be compatible
# with other unwind libraries as well (?)
# TODO investigate this further
# TODO arm EH ABI
# TODO sj/lj ABI
# TODO SEH
# TODO alignment!

const
  # UnwindReasonCode
  URC_NO_REASON* = 0
  URC_FOREIGN_EXCEPTION_CAUGHT* = 1
  URC_FATAL_PHASE2_ERROR* = 2
  URC_FATAL_PHASE1_ERROR* = 3
  URC_NORMAL_STOP* = 4
  URC_END_OF_STACK* = 5
  URC_HANDLER_FOUND* = 6
  URC_INSTALL_CONTEXT* = 7
  URC_CONTINUE_UNWIND* = 8

const
  # UnwindAction
  UA_SEARCH_PHASE* = 1
  UA_CLEANUP_PHASE* = 2
  UA_HANDLER_FRAME* = 4
  UA_FORCE_UNWIND* = 8
  UA_END_OF_STACK* = 16

  UNWIND_DATA_REG* =
    # see __builtin_eh_return_data / clang "getEHDataRegisterNumber" - this
    # list is incomplete
    when defined(x86): [0.cint, 2]
    else: [0.cint, 1]

type
  # c(uintptr_t) == nim(uint)
  UnwindReasonCode* = cint # c enum
  UnwindAction* = cint # c enum
  UnwindContext* = distinct pointer
  UnwindExceptionCleanupFn* = proc(
    unwindCode: UnwindReasonCode, exception: ptr UnwindException) {.cdecl.}
  UnwindStopFn* = proc(
    version: cint,
    actions: UnwindAction,
    exceptionClass: uint64,
    exception: ptr UnwindException,
    context: UnwindContext,
    parameter: pointer
  ) {.cdecl.}

  UnwindException* = object
    exceptionClass*: uint64
    exceptionCleanup*: UnwindExceptionCleanupFn
    private1, private2: uint

    when sizeof(pointer) == 4:
      reserved: array[3, uint32]

proc raiseException*(exception: ptr UnwindException): UnwindReasonCode {.importc: "_Unwind_RaiseException".}
proc resume*(exception: ptr UnwindException) {.importc: "_Unwind_Resume".}
proc deleteException*(exception: ptr UnwindException) {.importc: "_Unwind_DeleteException".}
func getGR*(ctx: UnwindContext, reg_index: cint): uint {.importc: "_Unwind_GetGR".}
proc setGR*(ctx: UnwindContext, reg_index: cint, value: uint) {.importc: "_Unwind_SetGR".}
func getIP*(ctx: UnwindContext): uint {.importc: "_Unwind_GetIP".}
proc setIP*(ctx: UnwindContext, value: uint) {.importc: "_Unwind_SetIP".}

func getRegionStart*(ctx: UnwindContext): uint {.importc: "_Unwind_GetRegionStart".}
func getLanguageSpecificData*(ctx: UnwindContext): uint {.importc: "_Unwind_GetLanguageSpecificData".}
proc forcedUnwind*(exception: ptr UnwindException, stop: UnwindStopFn, parameter: pointer) {.importc: "_Unwind_ForcedUnwind".}
proc resumeOrRethrow*(exception: ptr UnwindException): UnwindReasonCode {.importc: "_Unwind_Resume_Or_Rethrow".}

type UnwindTraceFn* = proc(context: UnwindContext, parameter: pointer) {.cdecl.}
proc backtrace*(traceFn: UnwindTraceFn, parameter: pointer) {.importc: "_Unwind_Backtrace".}

func getCFA*(ctx: UnwindContext): uint {.importc: "_Unwind_GetCFA".}

proc getIPInfo*(ctx: UnwindContext, ipBefore: ptr cint): uint {.importc: "_Unwind_GetIPInfo".}

type
  DwarfEhBases* = object
    tbase*: uint
    dbase*: uint
    fnc*: uint
proc findFDE*(pc: pointer, bases: ptr DwarfEhBases): pointer {.importc: "_Unwind_Find_FDE".}

proc findEnclosingFunction*(pc: pointer): pointer {.importc: "_Unwind_FindEnclosingFunction".}

func getDataRelBase*(ctx: UnwindContext): uint {.importc: "_Unwind_GetDataRelBase".}
func getTextRelBase*(ctx: UnwindContext): uint {.importc: "_Unwind_GetTextRelBase".}
