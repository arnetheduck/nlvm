# Run LLVM module using ORCv2 jit

import
  tables, posix,

  compiler/[options, msgs],
  llvm/llvm

proc runModule*(
    config: ConfigRef,
    ctx: var RunContext,
    tm: llvm.TargetMachineRef,
    m: llvm.ModuleRef
              ) =
  # Note: the variable naming here somewhat reflects the ORCv2 examples in the LLVM dir
  # 1. create an instance of the JIT (second arg would be a JITBuilder, default is fine for now)
  var orc: OrcLLJITRef
  let err = orcCreateLLJIT(addr orc, nil)
  if not err.isNil:
    config.internalError($err.getErrorMessage)

  # 2. get the main JITDylib associated to our jit `orc`
  var mainJD = orcLLJITGetMainJITDylib(orc)

  # 3. add a library search generator for the current process. This makes all loaded symbols
  # of *this* executable visible to the JIT. Without it we get JIT symbol errors, as it cannot
  # find any of the libc functions.
  # Here we might have to extend this and add another search generator to load other shared
  # libraries on the fly?
  block DynLibSearchGenerator:
    # 3.1 create our generator for library symbols of the process
    var processSymbolsGenerator: OrcDefinitionGeneratorRef
    let err = orcCreateDynamicLibrarySearchGeneratorForProcess(
      addr processSymbolsGenerator,
      orcLLJITGetGlobalPrefix(orc),
      nil, nil
    )
    if not err.isNil:
      config.internalError($err.getErrorMessage)
    # 3.2 add the search generator
    orcJITDylibAddGenerator(mainJD, processSymbolsGenerator)

  # 4. Next step is to add the actual IR of our module to the JIT
  block AddModuleIR:
    # 4.1 need a thread safe context wrapper
    var tsCtx = orcCreateNewThreadSafeContext()
    # 4.2 and a thread safe module around our `ModuleRef`
    let tsm = orcCreateNewThreadSafeModule(m, tsCtx)
    # 4.3 context not needed anymore
    orcDisposeThreadSafeContext(tsCtx)
    # 4.4 add the IR of our module to the JIT
    let err = orcLLJITAddLLVMIRModule(orc, mainJD, tsm)
    if not err.isNil:
      # If adding the ThreadSafeModule fails then we need to clean it up
      # ourselves. If adding it succeeds the JIT will manage the memory.
      orcDisposeThreadSafeModule(tsm)
      config.internalError($err.getErrorMessage)

  # 5. finally we're ready to call our JIT'ed function
  block CallJITedFunction:
    # 5.1 need a target address
    var main: OrcJITTargetAddress
    # 5.2 to look up the function by name (`main` is of course just the default in a standalone module)
    let err = orcLLJITLookup(orc, addr main, "main")
    if not err.isNil:
      config.internalError($err.getErrorMessage)
    # 5.3 assuming main neither takes arguments, nor returns, cast and call it
    cast[proc() {.cdecl.}](main)()
    # for some other proc it might look like:
    # `let sumFn = cast[proc(a, b: cint): cint {.cdecl.}](sumAddr)`
    # `echo sumFn(1, 2)`
