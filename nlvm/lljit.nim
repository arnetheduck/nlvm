# Run LLVM module using ORC jit

import
  tables, posix,

  compiler/[options, msgs],
  llvm/llvm

type
  RunContext* = object
    self: pointer ## Handle to main executable, used for symbol lookups
    store*: Table[string, seq[byte]]     ## Memory for global variables

proc orcResolver(name: cstring, lookupCtx: pointer): uint64 {.cdecl.} =
  let
    runCtx = cast[ptr RunContext](lookupCtx)
    name = $name

  if name in runCtx.store:
    cast[uint64](runCtx.store[name][0].addr)
  else:
    # TODO collect libraries that should be linked to and include them in the
    #      lookup instead of looking up in current executable
    cast[uint64](dlsym(runCtx.self, name))

proc runModule*(
    config: ConfigRef,
    ctx: var RunContext,
    tm: llvm.TargetMachineRef,
    m: llvm.ModuleRef) =
  let orc = orcCreateInstance(tm)

  ctx.self = dlopen(nil, 0)

  block:
    var om: OrcModuleHandle
    let err = orcAddEagerlyCompiledIR(
      orc, addr om, m, orcResolver, addr ctx)
    if not err.isNil:
      config.internalError($err.getErrorMessage)

  block:
    var main: OrcTargetAddress
    let err = orcGetSymbolAddress(orc, addr main, "main")

    if not err.isNil:
      config.internalError($err.getErrorMessage)

    cast[proc() {.cdecl.}](main)()
