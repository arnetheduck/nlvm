# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016-2019
# See the LICENSE file for license info (doh!)

import
  sequtils,
  strutils,
  times,
  os

import llgen, llvm/llvm

import
  compiler/[
    ast,
    cmdlinehelper,
    commands,
    condsyms,
    extccomp,
    idents,
    idgen,
    lexer,
    lineinfos,
    llstream,
    modulegraphs,
    modules,
    msgs,
    options,
    passes,
    passaux,
    pathutils,
    platform,
    sem
  ],
  parseopt

proc semanticPasses(g: ModuleGraph) =
  registerPass g, verbosePass
  registerPass g, semPass

const
  NlvmVersion = "0.0.1"
  NlvmHash = gorge("git rev-parse HEAD").strip
  NimHash = gorge("git -C ../Nim rev-parse HEAD").strip

  HelpHeader = """nlvm compiler for Nim, version $1 [$2: $3]

Copyright (c) 2015-2019 Jacek Sieka
Nim compiler (c) 2009-2019 Andreas Rumpf
"""

const
  Usage = slurp"../doc/basicopt.txt".replace("//", "")
  FeatureDesc = toSeq(Feature.items()).join("|")
  AdvancedUsage = slurp"../doc/advopt.txt".replace("//", "") % FeatureDesc

proc helpHeader(conf: ConfigRef): string =
  HelpHeader % [
    NlvmVersion,
    platform.OS[conf.target.hostOS].name,
    platform.CPU[conf.target.hostCPU].name]

proc getCommandLineDesc(conf: ConfigRef): string =
  helpHeader(conf) & Usage

proc writeHelp(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, getCommandLineDesc(conf), {msgStdout})
    msgQuit(0)

proc writeAdvanced(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, helpHeader(conf) & Usage, {msgStdout})
    msgQuit(0)

proc writeFullhelp(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, helpHeader(conf) & Usage & AdvancedUsage, {msgStdout})
    msgQuit(0)

proc formatVersion(name, v, h: string): string =
  if h.len() > 0:
    "$1: $2 ($3)" % [name, v, h]
  else:
    "$1: $2" % [name, v]

proc writeVersionInfo(conf: ConfigRef; pass: TCmdLinePass) =
  if pass == passCmd1:
    msgWriteln(conf, helpHeader(conf), {msgStdout})

    msgWriteln(conf, formatVersion("nlvm", NlvmVersion, NlvmHash), {msgStdout})
    msgWriteln(conf, formatVersion("Nim", NimVersion, NimHash), {msgStdout})
    msgWriteln(conf, formatVersion("llvm", LLVMVersion, ""), {msgStdout})

    msgQuit(0)

proc addPrefix(switch: string): string =
  if len(switch) == 1: result = "-" & switch
  else: result = "--" & switch

proc expectNoArg(
    conf: ConfigRef; switch, arg: string, pass: TCmdLinePass, info: TLineInfo) =
  if arg != "":
    localError(
      conf, info,
      "invalid argument for command line option: '$1'" % addPrefix(switch))

proc processSwitch(switch, arg: string, pass: TCmdLinePass,
    info: TLineInfo, conf: ConfigRef) =
  # helper to hijack some nlvm-specific options
  let sn = switch.normalize
  if sn.startsWith("llvm-"):
    if sn == "llvm-help":
      var llvmArgs = @["nlvm", "--help"]
      let arr = allocCStringArray(llvmArgs)
      defer: deallocCStringArray(arr)
      parseCommandLineOptions(llvmArgs.len.cint, arr, "")
      return

    return

  case switch.normalize
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

proc processSwitch*(pass: TCmdLinePass; p: OptParser; config: ConfigRef) =
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

proc processCmdLine(pass: TCmdLinePass, cmd: string; config: ConfigRef) =
  var p = parseopt.initOptParser(cmd)
  var argsCount = 0
  while true:
    parseopt.next(p)
    case p.kind
    of cmdEnd: break
    of cmdLongoption, cmdShortOption:
      if p.key == " ":
        p.key = "-"
        if processArgument(pass, p, argsCount, config): break
      else:
        processSwitch(pass, p, config)
    of cmdArgument:
      if processArgument(pass, p, argsCount, config): break
  if pass == passCmd2:
    if optRun notin config.globalOptions and config.arguments.len > 0 and config.command.normalize != "run":
      rawMessage(config, errGenerated, errArgsNeedRunOption)

proc llCompileProject(graph: ModuleGraph) =
  modules.connectCallbacks(graph)
  let conf = graph.config
  modules.wantMainModule(conf)
  let projectFile = conf.projectMainIdx
  graph.importStack.add projectFile

  graph.compileSystemModule()
  discard graph.compileModule(conf.projectMainIdx, {sfMainModule})

proc commandCompile(graph: ModuleGraph) =
  let conf = graph.config

  if conf.outDir.isEmpty:
    conf.outDir = conf.projectPath
  if conf.outFile.isEmpty:
    let targetName = if optGenDynLib in conf.globalOptions:
      platform.OS[conf.target.targetOS].dllFrmt % conf.projectName
    else:
      conf.projectName & platform.OS[conf.target.targetOS].exeExt
    conf.outFile = RelativeFile targetName

  semanticPasses(graph)
  registerPass(graph, llgen.llgenPass)

  llCompileProject(graph)

proc commandScan(conf: ConfigRef) =
  var f = addFileExt(mainCommandArg(conf), NimExt)
  var stream = llStreamOpen(f.AbsoluteFile, fmRead)
  if stream != nil:
    var
      L: TLexer
      tok: TToken
    initToken(tok)
    openLexer(L, f.AbsoluteFile, stream, newIdentCache(), conf)
    while true:
      rawGetTok(L, tok)
      conf.printTok(tok)
      if tok.tokType == tkEof: break
    closeLexer(L)
  else:
    conf.rawMessage(errCannotOpenFile, f)

proc commandCheck(graph: ModuleGraph) =
  graph.config.errorMax = high(int)  # do not stop after first error
  defineSymbol(graph.config.symbols, "nimcheck")
  semanticPasses(graph)  # use an empty backend for semantic checking only
  llCompileProject(graph)

proc mainCommand*(graph: ModuleGraph) =
  let conf = graph.config

  conf.lastCmdTime = epochTime()
  conf.searchPaths.add(conf.libpath)

  # No support! but it might work anyway :)
  conf.globalOptions.excl optTlsEmulation

  setId(100)

  # lib/pure/bitops.num
  defineSymbol(conf.symbols, "noIntrinsicsBitOpts")

  case conf.command.normalize
  # Take over the default compile command
  of "c", "cc", "compile", "compiletoc":
    conf.cmd = cmdCompileToC
    defineSymbol(graph.config.symbols, "c")
    commandCompile(graph)
  of "dump":
    conf.msgWriteln("-- list of currently defined symbols --")
    for s in definedSymbolNames(conf.symbols): conf.msgWriteln(s)
    conf.msgWriteln("-- end of list --")

    for it in conf.searchPaths: conf.msgWriteln(it.string)

  of "scan":
    conf.cmd = cmdScan
    conf.wantMainModule()
    commandScan(conf)

  of "check":
    conf.cmd = cmdCheck
    commandCheck(graph)

  else: conf.rawMessage(errGenerated, conf.command)

  if conf.errorCounter == 0 and
     conf.cmd notin {cmdInterpret, cmdRun, cmdDump}:
    let mem =
      when declared(system.getMaxMem): formatSize(getMaxMem()) & " peakmem"
      else: formatSize(getTotalMem()) & " totmem"
    let loc = $conf.linesCompiled
    let build = if isDefined(conf, "danger"): "Dangerous Release"
                elif isDefined(conf, "release"): "Release"
                else: "Debug"
    let sec = formatFloat(epochTime() - conf.lastCmdTime, ffDecimal, 3)
    let project = if optListFullPaths in conf.globalOptions: $conf.projectFull else: $conf.projectName
    var output = $conf.absOutFile
    if optListFullPaths notin conf.globalOptions: output = output.AbsoluteFile.extractFilename
    rawMessage(conf, hintSuccessX, [
      "loc", loc,
      "sec", sec,
      "mem", mem,
      "build", build,
      "project", project,
      "output", output,
      ])

proc handleCmdLine(cache: IdentCache, conf: ConfigRef) =
  # For now, we reuse the nim command line options parser, mainly because
  # the options are used all over the compiler, but also because we want to
  # act as a drop-in replacement (for now)
  # Most of this is taken from the main nim command
  let self = NimProg(
    supportsStdinFile: true,
    processCmdLine: processCmdLine,
    mainCommand: mainCommand
  )
  self.initDefinesProg(conf, "nlvm")

  if paramCount() == 0:
    writeHelp(conf, passCmd1)
    return

  self.processCmdLineAndProjectPath(conf)

  if not self.loadConfigsAndRunMainCommand(cache, conf):
    return
  if conf.errorCounter != 0:
    return

  if optRun in conf.globalOptions:
    let ex = quoteShell conf.absOutFile
    execExternalProgram(conf, ex & ' ' & conf.arguments)

# Beautiful...
var tmp = getAppDir()
while not dirExists(tmp / "nlvm-lib") and tmp.len > 1:
  tmp = tmp.parentDir()

let
  conf = newConfigRef()
  cache = newIdentCache()

conf.prefixDir = AbsoluteDir(tmp / "Nim")
conf.searchPaths.insert(conf.prefixDir / RelativeDir"../nlvm-lib", 0)

handleCmdLine(cache, conf)
