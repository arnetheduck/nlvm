# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016-2019
# See the LICENSE file for license info (doh!)

import
  std/[browsers, sequtils, parseopt, strutils, times, os],
  llvm/llvm,
  compiler/[
    ast, cmdlinehelper, commands, condsyms, extccomp, idents, lineinfos, llstream,
    modulegraphs, modules, msgs, options, passes, passaux, pathutils, platform,
  ],
  ./[llgen, lllink, llplatform]

proc semanticPasses(g: ModuleGraph) =
  registerPass g, verbosePass
  registerPass g, semPass

const
  NlvmVersion = "0.0.2"
  NlvmHash = gorge("git rev-parse HEAD").strip
  NimHash = gorge("git -C ../lib/nim rev-parse HEAD").strip

  HelpHeader = """nlvm compiler for Nim, version $1 [$2: $3]

Copyright (c) 2015-2026 Jacek Sieka
Nim compiler (c) 2009-2026 Andreas Rumpf
"""

const
  Usage = slurp"../doc/basicopt.txt".replace("//", "")
  FeatureDesc = toSeq(Feature.items()).join("|")
  AdvancedUsage = slurp"../doc/advopt.txt".replace("//", "") % FeatureDesc

proc helpHeader(conf: ConfigRef): string =
  HelpHeader % [
    NlvmVersion,
    platform.OS[conf.target.hostOS].name,
    platform.CPU[conf.target.hostCPU].name,
  ]

proc getCommandLineDesc(conf: ConfigRef): string =
  helpHeader(conf) & Usage

proc writeHelp(conf: ConfigRef, pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, getCommandLineDesc(conf), {msgStdout})
    msgQuit(0)

proc writeAdvanced(conf: ConfigRef, pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, helpHeader(conf) & Usage, {msgStdout})
    msgQuit(0)

proc writeFullhelp(conf: ConfigRef, pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, helpHeader(conf) & Usage & AdvancedUsage, {msgStdout})
    msgQuit(0)

proc formatVersion(name, v, h: string): string =
  if h.len() > 0:
    "$1: $2 ($3)" % [name, v, h]
  else:
    "$1: $2" % [name, v]

proc writeVersionInfo(conf: ConfigRef, pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, helpHeader(conf), {msgStdout})

    msgWriteln(conf, formatVersion("nlvm", NlvmVersion, NlvmHash), {msgStdout})
    msgWriteln(conf, formatVersion("Nim", NimVersion, NimHash), {msgStdout})
    msgWriteln(conf, formatVersion("llvm", LLVMVersion, ""), {msgStdout})

    msgQuit(0)

proc addPrefix(switch: string): string =
  if len(switch) == 1:
    result = "-" & switch
  else:
    result = "--" & switch

proc expectNoArg(
    conf: ConfigRef, switch, arg: string, pass: TCmdLinePass, info: TLineInfo
) =
  if arg != "":
    localError(
      conf, info, "invalid argument for command line option: '$1'" % addPrefix(switch)
    )

proc processSwitch(
    switch, arg: string, pass: TCmdLinePass, info: TLineInfo, conf: ConfigRef
) =
  # helper to hijack some nlvm-specific options
  let sn = switch.normalize
  if sn.startsWith("llvm-"):
    if sn == "llvm-help":
      var llvmArgs = @["nlvm", "--help"]
      let arr = allocCStringArray(llvmArgs)
      defer:
        deallocCStringArray(arr)
      parseCommandLineOptions(llvmArgs.len.cint, arr, "")
      return

    return

  case switch.normalize
  of "opt":
    # Remember whether the user explicitly selected an optimization mode. BPF
    # defaults to size optimization only when no --opt flag was supplied.
    conf.setConfigVar("nlvm.bpf.opt", arg.normalize)
    commands.processSwitch(switch, arg, pass, info, conf)
  of "version", "v":
    expectNoArg(conf, switch, arg, pass, info)
    writeVersionInfo(conf, pass)
  of "advanced":
    expectNoArg(conf, switch, arg, pass, info)
    writeAdvanced(conf, pass)
  of "fullhelp":
    expectNoArg(conf, switch, arg, pass, info)
    writeFullhelp(conf, pass)
  of "help", "h":
    expectNoArg(conf, switch, arg, pass, info)
    writeHelp(conf, pass)
  else:
    commands.processSwitch(switch, arg, pass, info, conf)

proc processSwitch*(pass: TCmdLinePass, p: OptParser, config: ConfigRef) =
  # hijacked from commands.nim

  # hint[X]:off is parsed as (p.key = "hint[X]", p.val = "off")
  # we transform it to (key = hint, val = [X]:off)
  var bracketLe = strutils.find(p.key, '[')
  if bracketLe >= 0:
    var key = substr(p.key, 0, bracketLe - 1)
    var val = substr(p.key, bracketLe) & ':' & p.val
    processSwitch(key, val, pass, gCmdLineInfo, config)
  else:
    processSwitch(p.key, p.val, pass, gCmdLineInfo, config)

proc processCmdLine(pass: TCmdLinePass, cmd: string, config: ConfigRef) =
  var p = parseopt.initOptParser(cmd)
  var argsCount = 0
  while true:
    parseopt.next(p)
    case p.kind
    of cmdEnd:
      break
    of cmdLongoption, cmdShortOption:
      if p.key == " ":
        p.key = "-"
        if processArgument(pass, p, argsCount, config):
          break
      else:
        processSwitch(pass, p, config)
    of cmdArgument:
      if processArgument(pass, p, argsCount, config):
        break
      if p.key == "cc":
        break

  if pass == passCmd2:
    if optRun notin config.globalOptions and config.arguments.len > 0 and
        config.command.normalize != "run":
      rawMessage(config, errGenerated, errArgsNeedRunOption)

    if config.cmd == cmdCc:
      # For "cc`, we pass all remaining arguments unaltered
      config.commandArgs = @[]
      var seenCc = false
      for i in 0 .. paramCount():
        if seenCc:
          config.commandArgs.add paramStr(i)
        elif paramStr(i) == "cc":
          seenCc = true

proc commandCompile(graph: ModuleGraph) =
  let conf = graph.config

  if conf.outDir.isEmpty:
    conf.outDir = conf.projectPath
  if conf.outFile.isEmpty:
    let targetName =
      if optGenDynLib in conf.globalOptions:
        platform.OS[conf.target.targetOS].dllFrmt % conf.projectName
      else:
        conf.projectName & platform.OS[conf.target.targetOS].exeExt
    conf.outFile = RelativeFile targetName

  extccomp.initVars(conf)
  semanticPasses(graph)
  registerPass(graph, llgen.llgenPass)

  passes.compileProject(graph)

proc commandCheck(graph: ModuleGraph) =
  graph.config.errorMax = high(int) # do not stop after first error
  defineSymbol(graph.config.symbols, "nimcheck")
  semanticPasses(graph) # use an empty backend for semantic checking only
  passes.compileProject(graph)

proc interactivePasses(graph: ModuleGraph) =
  initDefines(graph.config.symbols)
  # defineSymbol(graph.config.symbols, "nimscript")
  incl graph.config.globalOptions, optWasNimscript

  registerPass(graph, verbosePass)
  registerPass(graph, semPass)
  registerPass(graph, llgen.llgenPass)

proc commandInteractive(graph: ModuleGraph) =
  graph.config.setErrorMaxHighMaybe
  # TODO: gc unsupported as of yet
  graph.config.selectedGC = gcNone

  defineSymbol(graph.config.symbols, "nogc")
  defineSymbol(graph.config.symbols, "useMalloc")

  interactivePasses(graph)
  compileSystemModule(graph)
  if graph.config.commandArgs.len > 0:
    discard graph.compileModule(fileInfoIdx(graph.config, graph.config.projectFull), {})
  else:
    var m = graph.makeStdinModule()
    incl(m.flags, sfMainModule)
    var idgen = IdGenerator(module: m.itemId.module, symId: m.itemId.item, typeId: 0)
    let s = llStreamOpenStdIn(
      onPrompt = proc() =
        flushDot(graph.config)
    )
    processModule(graph, m, idgen, s)

proc mainCommand*(graph: ModuleGraph) =
  let conf = graph.config

  conf.lastCmdTime = epochTime()
  conf.searchPaths.add(conf.libpath)

  # No support! but it might work anyway :)
  conf.globalOptions.excl optTlsEmulation

  # lib/pure/bitops.num
  defineSymbol(conf.symbols, "noIntrinsicsBitOpts")

  case conf.cmd
  # Take over the default compile command
  of cmdCompileToC, cmdCompileToCpp:
    if conf.exc == excNone:
      conf.exc = excSetjmp
    defineSymbol(graph.config.symbols, "c")
    commandCompile(graph)
  of cmdCc:
    ## "nlvm cc" passthrough: build argv as clang would see it.
    if conf.commandArgs.len == 0:
      rawMessage(conf, errGenerated, "usage: nlvm cc <clang-args...>")
      conf.cmd = cmdNop
      return
    let
      triple =
        if conf.commandArgs.anyIt(it.startsWith("--target=")):
          ""
        else:
          conf.toTriple()
      rc = conf.callBuiltinClang(conf.commandArgs, triple)
    conf.cmd = cmdNop # prevent falling through to default
    if rc != 0:
      conf.errorCounter = 1
  of cmdDump:
    msgWriteln(
      conf,
      "-- list of currently defined symbols --",
      {msgStdout, msgSkipHook, msgNoUnitSep},
    )
    for s in definedSymbolNames(conf.symbols):
      msgWriteln(conf, s, {msgStdout, msgSkipHook, msgNoUnitSep})
    msgWriteln(conf, "-- end of list --", {msgStdout, msgSkipHook})

    for it in conf.searchPaths:
      msgWriteln(conf, it.string)

  # of "scan":
  #   conf.cmd = cmdScan
  #   conf.wantMainModule()
  #   commandScan(conf)
  of cmdCheck:
    commandCheck(graph)
  of cmdCrun, cmdTcc:
    if not fileExists(conf.projectFull):
      conf.cmd = cmdInteractive
      commandInteractive(graph)
    elif conf.projectFull.string.endsWith(".nim"):
      conf.cmd = cmdCrun

      # TODO: gc unsupported as of yet
      conf.selectedGC = gcNone

      defineSymbol(conf.symbols, "nogc")
      defineSymbol(conf.symbols, "useMalloc")

      incl conf.globalOptions, optWasNimscript
      commandCompile(graph)
    elif not conf.projectFull.string.endsWith(".nims"):
      rawMessage(conf, errGenerated, "not a NimScript file: " & conf.projectFull.string)
  of cmdInteractive:
    commandInteractive(graph)
  of cmdNimscript:
    if conf.projectIsCmd or conf.projectIsStdin:
      discard
    elif not fileExists(conf.projectFull):
      rawMessage(
        conf, errGenerated, "NimScript file does not exist: " & conf.projectFull.string
      )
  of cmdNop:
    discard
  else:
    conf.rawMessage(errGenerated, "command not supported in nlvm: " & conf.command)

  if conf.errorCounter == 0 and conf.cmd notin {cmdTcc, cmdDump, cmdNop}:
    # if optProfileVM in conf.globalOptions:
    #   echo conf.dump(conf.vmProfileData)
    genSuccessX(conf)

proc getNimRunExe(conf: ConfigRef): string =
  # xxx consider defining `conf.getConfigVar("nimrun.exe")` to allow users to
  # customize the binary to run the command with, e.g. for custom `nodejs` or `wine`.
  if conf.isDefined("mingw"):
    if conf.isDefined("i386"):
      result = "wine"
    elif conf.isDefined("amd64"):
      result = "wine64"

proc handleCmdLine(cache: IdentCache, conf: ConfigRef) =
  # For now, we reuse the nim command line options parser, mainly because
  # the options are used all over the compiler, but also because we want to
  # act as a drop-in replacement (for now)
  # Most of this is taken from the main nim command
  let self = NimProg(supportsStdinFile: true, processCmdLine: processCmdLine)
  self.initDefinesProg(conf, "nlvm")

  if paramCount() == 0:
    writeCommandLineUsage(conf)
    return

  self.processCmdLineAndProjectPath(conf)
  # nlvm exception handling mostly resembles C++ but setjmp avoids some
  # C++-specific quirks
  conf.exc = excSetjmp

  if conf.cmd == cmdCrun:
    conf.verbosity = 0
    conf.notes = NotesVerbosity[0]
    conf.foreignPackageNotes = NotesVerbosity[0]
    conf.mainPackageNotes = NotesVerbosity[0]
    conf.options.excl(optHints)

  var graph = newModuleGraph(cache, conf)
  if not self.loadConfigsAndProcessCmdLine(cache, conf, graph):
    return

  if conf.existsConfigVar("nlvm.target"):
    let (cpu, os) = parseTarget(conf.getConfigVar("nlvm.target"))
    # LLVM supports more architectures than Nim's platform table - for those,
    # keep whatever target was selected on the command line and let LLVM
    # validate the triple during code generation.
    if cpu != cpuNone and os != osNone:
      conf.target.setTarget(os, cpu)

  if conf.target.targetCPU.isBpfCpu():
    # BPF programs run in the kernel, not in a hosted process: they get no
    # process startup, no libc and no libc-provided runtime.
    conf.target.setTarget(osStandalone, conf.target.targetCPU)
    unregisterArcOrc(conf)
    for gcSymbol in [
      "boehmgc", "gcrefc", "gcmarkandsweep", "gchooks", "gogc", "gcregions"
    ]:
      undefSymbol(conf.symbols, gcSymbol)
    conf.selectedGC = gcNone
    conf.exc = excNone
    # The kernel has no Nim thread runtime or TLS support.
    conf.globalOptions.excl {optThreads, optThreadAnalysis, optTlsEmulation}
    # BPF cannot call into the Nim runtime — disable all checks that
    # generate calls to raise / sysFatal / copyString / etc.
    conf.options.excl {optOverflowCheck, optObjCheck, optFieldCheck,
      optRangeCheck, optBoundsCheck, optAssert}
    defineSymbol(conf.symbols, "bpf")
    defineSymbol(conf.symbols, "nogc")
    defineSymbol(conf.symbols, "useMalloc")
    incl conf.globalOptions, optNoLinking
    incl conf.globalOptions, optNoMain
    if conf.cmd in cmdBackends:
      let
        entry = conf.getConfigVar("nlvm.bpf.entry", "")
        section = conf.getConfigVar("nlvm.bpf.section", "")
      if entry.len == 0 or section.len == 0:
        conf.internalError(
          "BPF compilation requires --nlvm.bpf.entry and --nlvm.bpf.section"
        )
      elif entry == section:
        conf.internalError("The BPF entry name must differ from its ELF section name")

  if conf.selectedGC == gcUnselected:
    initOrcDefines(conf)

  if conf.useBuiltinCC():
    conf.cCompiler = ccClang

  if conf.target.targetOS == osWindows:
    # `mingw` links these "automatically" - no need for `dynlib`
    for lib in ["gdi32", "comdlg32", "advapi32", "shell32", "user32", "kernel32"]:
      conf.inclDynlibOverride(lib)

  mainCommand(graph)

  if conf.hasHint(hintGCStats):
    echo(GC_getStatistics())
  #echo(GC_getStatistics())
  if conf.errorCounter != 0:
    return

  if optRun in conf.globalOptions:
    let output = conf.absOutFile
    case conf.cmd
    of cmdBackends, cmdTcc:
      let nimRunExe = getNimRunExe(conf)
      var cmdPrefix: string
      if nimRunExe.len > 0:
        cmdPrefix.add nimRunExe.quoteShell
      case conf.backend
      of backendC, backendCpp, backendObjc:
        discard
      else:
        doAssert false, $conf.backend
      if cmdPrefix.len > 0:
        cmdPrefix.add " "
        # without the `cmdPrefix.len > 0` check, on windows you'd get a cryptic:
        # `The parameter is incorrect`
      let cmd = cmdPrefix & output.quoteShell & ' ' & conf.arguments
      execExternalProgram(conf, cmd.strip(leading = false, trailing = true))
    of cmdDocLike, cmdRst2html, cmdRst2tex: # bugfix(cmdRst2tex was missing)
      if conf.arguments.len > 0:
        # reserved for future use
        rawMessage(conf, errGenerated, "'$1 cannot handle arguments" % [$conf.cmd])
      openDefaultBrowser($output)
    else:
      # support as needed
      rawMessage(conf, errGenerated, "'$1 cannot handle --run" % [$conf.cmd])

let rc =
  if paramCount() >= 1 and paramStr(1) == "-cc1":
    ## `clang` forks itself with "-cc1" as the first argument when running itself
    ## as the `cc1` tool as part of a multi-step compilation process (ie compiling
    ## and linking at the same time) - see `-fintegrated-cc1` - capture this early
    ## thus avoiding any nim processing.

    let clangArgs = @["clang"] & commandLineParams()
    int8(clangMain(clangArgs))
  else:
    # Find the installation top-level that's needed both for our own runtime, that
    # of the integrated clang and Nim itself
    var appDir = getAppDir()
    while not dirExists(appDir / "lib") and appDir.len > 1:
      appDir = appDir.parentDir()

    let
      conf = newConfigRef()
      cache = newIdentCache()

    conf.prefixDir = AbsoluteDir(appDir / "lib/nim")
    conf.searchPaths.insert(AbsoluteDir(appDir / "lib" / "nlvm"), 0)

    handleCmdLine(cache, conf)

    int8(conf.errorCounter > 0)

msgQuit(rc)
