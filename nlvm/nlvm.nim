# nlvm - llvm IR generator for Nim
# Copyright (c) Jacek Sieka 2016
# See the LICENSE file for license info (doh!)

import
  strutils,
  os

import llgen

import
  compiler/commands,
  compiler/condsyms,
  compiler/lexer,
  compiler/lists,
  compiler/llstream,
  compiler/modules,
  compiler/msgs,
  compiler/nimconf,
  compiler/options,
  compiler/passes,
  compiler/sem,
  compiler/service

proc commandLL() =
  registerPass(sem.semPass)
  registerPass(llgen.llgenPass)

  modules.compileProject()

proc commandScan =
  var f = addFileExt(mainCommandArg(), NimExt)
  var stream = llStreamOpen(f, fmRead)
  if stream != nil:
    var
      L: TLexer
      tok: TToken
    initToken(tok)
    openLexer(L, f, stream)
    while true:
      rawGetTok(L, tok)
      printTok(tok)
      if tok.tokType == tkEof: break
    closeLexer(L)
  else:
    rawMessage(errCannotOpenFile, f)

proc mainCommand() =
  lists.appendStr(searchPaths, options.libpath)

  case options.command.normalize
  # Take over the default compile command
  of "c", "cc", "compile", "compiletoc": commandLL()
  of "dump":
    msgWriteln("-- list of currently defined symbols --")
    for s in definedSymbolNames(): msgWriteln(s)
    msgWriteln("-- end of list --")

    for it in iterSearchPath(searchPaths): msgWriteln(it)

  of "scan":
    gCmd = cmdScan
    wantMainModule()
    commandScan()

  else: msgs.rawMessage(errInvalidCommandX, options.command)

  if msgs.gErrorCounter == 0 and
     gCmd notin {cmdInterpret, cmdRun, cmdDump}:
    rawMessage(hintSuccess, [])

proc handleCmdLine() =
  # For now, we reuse the nim command line options parser, mainly because
  # the options are used all over the compiler, but also because we want to
  # act as a drop-in replacement (for now)
  # Most of this is taken from the main nim command
  if os.paramCount() == 0:
    echo "you can: nlvm c <filename> (see standard nim compiler for options)"
  else:
    # Main nim compiler has some reaons for two-pass parsing
    service.processCmdLine(passCmd1, "")

    # Use project name like main nim compiler
    # TODO upstream to common location...
    if options.gProjectName == "-":
      options.gProjectName = "stdinfile"
      options.gProjectFull = "stdinfile"
      options.gProjectPath = os.getCurrentDir()
      options.gProjectIsStdin = true
    elif options.gProjectName != "":
      try:
        options.gProjectFull = canonicalizePath(options.gProjectName)
      except OSError:
        options.gProjectFull = options.gProjectName
      let p = splitFile(options.gProjectFull)
      options.gProjectPath = p.dir
      options.gProjectName = p.name
    else:
      gProjectPath = os.getCurrentDir()

    nimconf.loadConfigs(DefaultConfig)
    service.processCmdLine(passCmd2, "")

    # default signal handler does memory allocations and all kinds of
    # disallowed-in-signal-handler-stuff
    defineSymbol("noSignalHandler")

    mainCommand()

# Beautiful...
var tmp = getAppDir()
while not dirExists(tmp / "nlvm-lib") and tmp.len > 1:
  tmp = tmp.parentDir()

options.gPrefixDir = tmp / "Nim"
condsyms.initDefines()
handleCmdLine()
