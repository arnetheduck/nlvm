# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016
# See the LICENSE file for license info (doh!)

import
  strutils,
  times,
  os

import llgen

import
  compiler/[
    commands,
    condsyms,
    idents,
    lexer,
    lineinfos,
    llstream,
    modulegraphs,
    modules,
    msgs,
    nimconf,
    options,
    passes,
    passaux,
    pathutils,
    sem
  ],
  parseopt

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

proc commandLL(graph: ModuleGraph) =
  registerPass(graph, verbosePass)
  registerPass(graph, sem.semPass)
  registerPass(graph, llgen.llgenPass)

  modules.compileProject(graph)

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

proc mainCommand(cache: IdentCache, conf: ConfigRef) =
  conf.lastCmdTime = epochTime()
  conf.searchPaths.add(conf.libpath)

  case conf.command.normalize
  # Take over the default compile command
  of "c", "cc", "compile", "compiletoc": commandLL(newModuleGraph(cache, conf))
  of "dump":
    conf.msgWriteln("-- list of currently defined symbols --")
    for s in definedSymbolNames(conf.symbols): conf.msgWriteln(s)
    conf.msgWriteln("-- end of list --")

    for it in conf.searchPaths: conf.msgWriteln(it.string)

  of "scan":
    conf.cmd = cmdScan
    conf.wantMainModule()
    commandScan(conf)

  else: conf.rawMessage(errGenerated, conf.command)

  if conf.errorCounter == 0 and
     conf.cmd notin {cmdInterpret, cmdRun, cmdDump}:
    when declared(system.getMaxMem):
      let usedMem = formatSize(getMaxMem()) & " peakmem"
    else:
      let usedMem = formatSize(getTotalMem())
    rawMessage(conf, hintSuccessX, [$conf.linesCompiled,
               formatFloat(epochTime() - conf.lastCmdTime, ffDecimal, 3),
               usedMem,
               if isDefined(conf, "release"): "Release Build"
               else: "Debug Build"])

proc handleCmdLine(cache: IdentCache, conf: ConfigRef) =
  # For now, we reuse the nim command line options parser, mainly because
  # the options are used all over the compiler, but also because we want to
  # act as a drop-in replacement (for now)
  # Most of this is taken from the main nim command
  if os.paramCount() == 0:
    echo """
you can: nlvm c <filename> (see standard nim compiler for options)
magic options:
  --nlvm.target=wasm32 cross-compile to WebAssembly
"""
  else:
    # Main nim compiler has some reaons for two-pass parsing
    processCmdLine(passCmd1, "", conf)

    # Use project name like main nim compiler
    # TODO upstream to common location...
    if conf.projectName == "-":
      conf.projectName = "stdinfile"
      conf.projectFull = "stdinfile".AbsoluteFile
      conf.projectPath = os.getCurrentDir().AbsoluteDir
      conf.projectIsStdin = true
    elif conf.projectName != "":
      try:
        conf.projectFull = conf.canonicalizePath(conf.projectName.AbsoluteFile)
      except OSError:
        conf.projectFull = conf.projectName.AbsoluteFile
      let p = splitFile(conf.projectFull)
      conf.projectPath = p.dir
      conf.projectName = p.name
    else:
      conf.projectPath = os.getCurrentDir().AbsoluteDir

    nimconf.loadConfigs(DefaultConfig, cache, conf)
    processCmdLine(passCmd2, "", conf)

    #gSelectedGC = gcMarkAndSweep
    #defineSymbol("gcmarkandsweep")

    # default signal handler does memory allocations and all kinds of
    # disallowed-in-signal-handler-stuff
    defineSymbol(conf.symbols, "noSignalHandler")

    # lib/pure/bitops.num
    defineSymbol(conf.symbols, "noIntrinsicsBitOpts")

    mainCommand(cache, conf)

# Beautiful...
var tmp = getAppDir()
while not dirExists(tmp / "nlvm-lib") and tmp.len > 1:
  tmp = tmp.parentDir()

let conf = newConfigRef()
let cache = newIdentCache()
condsyms.initDefines(conf.symbols)
conf.prefixDir = AbsoluteDir(tmp / "Nim")
handleCmdLine(cache, conf)
