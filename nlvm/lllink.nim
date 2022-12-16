import
  os,
  osproc,
  strutils,

  compiler/[
    extccomp,
    lineinfos,
    msgs,
    options,
    platform,
    pathutils,
  ],
  llvm/llvm

iterator passLinkOptions(opts: openArray[string]): string =
  var passNext = false
  template yields(x: untyped) = yield strip(x, true, true, {'\'', '\"'})
  for s in opts:
    if passNext:
      passNext = false
      yields s
    elif s == "-e":
      passNext = true
      yield s
    elif s == "--entry":
      yield s
    elif s.startsWith("-l"):
      yield s
    elif s == "--no-undefined":
      yield s
    elif s.startsWith("-rpath"):
      yield s
    elif s == "-r":
      yield s
    elif s == "-Xlinker":
      passNext = true
    elif s.startsWith("-Wl,"):
      # TODO comma support
      yields s[4..^1]
    elif not s.startsWith("-"):
      yields s

proc findLibraries(s: string): seq[string] =
  for line in s.split('\n'):
    if line.startsWith("libraries: ="):
      return line["libraries: =".len..^1].split(':')

proc findLastArg(args: openArray[string], opts: openArray[string]): string =
  for i in 1..args.len:
    if args[^i] in opts:
      return args[^i]
  return ""

proc findFile(candidates: openArray[string], name: string): string =
  for candidate in candidates:
    if os.fileExists(candidate / name):
      return candidate / name
  ""

proc linkLinuxAmd64(conf: ConfigRef) =
  # This function emulates clang which emulates gcc which builds on 50 years
  # of technical debt accumulation in the linker tooling space!

  # Ugly hack to reuse nim compiling of C files
  conf.globalOptions.incl optNoLinking
  callCCompiler(conf)
  conf.globalOptions.excl optNoLinking

  let filePaths =
    try:
      let driver = CC[conf.cCompiler].compilerExe
      findLibraries(execProcess(driver & " -print-search-dirs"))
    except CatchableError:
      @[]

  let linkOpts =
    parseCmdLine(conf.linkOptions) &
    parseCmdLine(conf.linkOptionsCmd)

  # Simplified version of (ie would need more work):
  # https://github.com/llvm/llvm-project/blob/llvmorg-15.0.6/clang/lib/Driver/ToolChains/Gnu.cpp#L402
  var args: seq[string]

  args.add "ld.lld"

  let
    isAndroid = conf.target.targetOS == osAndroid
    pieArg = linkOpts.findLastArg(["-pie", "-no-pie", "-nopie"])
    isPIE = not(
      "-shared" in linkOpts or "-static" in linkOpts or "-r" in linkOpts or
      "-static-pie" in linkOpts
    ) and pieArg in ["-pie"] # `-pie` by default
    isStaticPIE = "-static-pie" in linkOpts
    isStatic = "-static" in linkOpts and not isStaticPIE
    isCpp = optMixedMode in conf.globalOptions

  if isPIE:
    args.add "-pie"

  if isStaticPIE:
    args.add ["-static", "-pie", "--no-dynamic-linker", "-z", "text"]

  if "-rdynamic" in linkOpts:
    args.add ["-export-dynamic"]

  if "-s" in linkOpts:
    args.add ["-s"]

  # Next follows a section of distro-specific flags:
  # https://github.com/llvm/llvm-project/blob/llvmorg-15.0.6/clang/lib/Driver/ToolChains/Linux.cpp#L193

  if isAndroid: # TODO or alpine linux
    args.add ["-z", "now"]

  if isAndroid: # TODO or suse/ubuntu/alpine linux
    args.add ["-z", "relro"]

  # Ancient distros would need --hash-style=both
  args.add "--hash-style=gnu"

  if isAndroid: # TODO or opensuse
    args.add "--enable-new-dtags"

  # TODO multiarch / multilib

  args.add "--eh-frame-hdr"

  args.add ["-m", "elf_x86_64"]

  if "-shared" in linkOpts:
    args.add "-shared"

  if isStatic:
    args.add "-static"
  else:
    if "-rdynamic" in linkOpts:
      args.add "-export-dynamic"

    if "-shared" notin linkOpts and not isStaticPIE and "-r" notin linkOpts:
      args.add ["-dynamic-linker", "/lib64/ld-linux-x86-64.so.2"]

  let exeFile = conf.prepareToWriteOutput()

  args.add ["-o", exefile.string]

  if "-nostdlib" notin linkOpts and "-nostartfiles" notin linkOpts and "-r" notin linkOpts:
    if not isAndroid:
      let crt1 = if isPIE:
        "Scrt1.o"
      elif isStaticPIE:
        "rcrt1.o"
      else:
        "crt1.o"
      args.add findFile(filePaths, crt1)

    args.add findFile(filePaths, "crti.o")

    let crtbegin = if "-shared" in linkOpts:
      if isAndroid: "crtbegin_so.o" else: "crtbeginS.o"
    elif isStatic:
      if isAndroid: "crtbegin_static.o" else: "crtbeginT.o"
    elif isPIE or isStaticPIE:
      if isAndroid: "crtbegin_dynamic.o" else: "crtbeginS.o"
    else:
      if isAndroid: "crtbegin_dynamic.o" else: "crtbegin.o"

    args.add findFile(filePaths, crtbegin)
  for opt in linkOpts:
    if opt.startsWith("-L"):
      args.add opt
  for opt in linkOpts:
    if opt.startsWith("-u"):
      args.add opt

  for lib in filePaths:
    args.add "-L" & lib

  # Then the project object files...
  for it in conf.externalToLink:
    args.add addFileExt(it, CC[conf.cCompiler].objExt)

  for x in conf.toCompile:
    args.add x.obj.string

  for opt in passLinkOptions(linkOpts):
    args.add opt

  for linkedLib in items(conf.cLinkedLibs):
    args.add parseCmdLine(CC[conf.cCompiler].linkLibCmd % linkedLib.quoteShell)
  for libDir in items(conf.cLibs):
    args.add parseCmdLine(join([CC[conf.cCompiler].linkDirCmd, libDir.quoteShell]))

  if "-nostdlib" notin linkOpts and "-r" notin linkOpts:
    if isCpp:
      if "-nostdlibxx" notin linkOpts and "-nodefaultlibs" notin linkOpts:
        args.add "-lstdc++"
        args.add "-lm"

    if "-nodefaultlibs" notin linkOpts:
      if isStatic or isStaticPIE:
        args.add "--start-group"

      let wantPthread =
        "-pthread" in linkOpts or
        "-pthreads" in linkOpts or
        optThreads in conf.globalOptions

      template addRuntimeLibs() =
        if isStatic or isStaticPIE:
          args.add ["-lgcc", "-lgcc_eh"]
        elif "-shared" in linkOpts:
          args.add "-lgcc_s"
        else:
          args.add ["-lgcc", "--as-needed", "-lgcc_s", "--no-as-needed"]

      addRuntimeLibs()
      if wantPthread:
        args.add "-lpthread"

      if "-nolibc" notin linkOpts:
        args.add "-lc"

      if isStatic or isStaticPIE:
        args.add "--end-group"
      else:
        addRuntimeLibs()

    let crtend = if "-shared" in linkOpts:
      if isAndroid: "crtend_so.o" else: "crtendS.o"
    elif isPIE or isStaticPIE:
      if isAndroid: "crtend_android.o" else: "crtendS.o"
    else:
      if isAndroid: "crtend_android.o" else: "crtend.o"

    args.add findFile(filePaths, crtend)

    if not isAndroid:
      args.add findFile(filePaths, "crtn.o")

  rawMessage(conf, hintLinking, $args)

  if not nimLLDLinkElf(args):
    rawMessage(conf, errGenerated, "linking failed")
    quit(1)

proc linkWasm32(conf: ConfigRef) =
  var args: seq[string]

  args.add "wasm-ld"

  let exeFile = conf.getOutFile(conf.outFile, "wasm")

  let linkOpts =
    parseCmdLine(conf.linkOptions) &
    parseCmdLine(conf.linkOptionsCmd)

  args.add ["-o", exefile.string]

  for opt in passLinkOptions(linkOpts):
    args.add opt

  for it in conf.externalToLink:
    args.add addFileExt(it, CC[conf.cCompiler].objExt)

  for x in conf.toCompile:
    args.add x.obj.string

  # TODO this causes stuff to be included in the wasm file, but there's probably
  #      a better way
  args.add("--export-dynamic")

  rawMessage(conf, hintLinking, $args)
  if not nimLLDLinkWasm(args):
    rawMessage(conf, errGenerated, "linking failed")
    quit(1)

proc lllink*(conf: ConfigRef) =
  let builtinLinker =
    optGenStaticLib notin conf.globalOptions and
    optGenGuiApp notin conf.globalOptions and
    optGenDynLib notin conf.globalOptions and
    conf.getConfigVar("nlvm.linker", "builtin") == "builtin" and
    conf.cCompiler in {ccGcc, ccLLVM_Gcc, ccCLang}

  if builtinLinker and conf.target.targetOS == osLinux and
      conf.target.targetCPU == cpuAmd64:
    linkLinuxAmd64(conf)
  elif conf.target.targetCPU == cpuWasm32:
    linkWasm32(conf)
  else:
    # TODO configure this elsewhere? also, -Wl vs raw linker options..:/
    conf.addLinkOptionCmd("-Wl,--as-needed")

    callCCompiler(conf)
