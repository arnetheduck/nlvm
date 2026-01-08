import
  std/[os, osproc, strformat, strutils, sequtils],
  compiler/[extccomp, lineinfos, msgs, options, platform, pathutils],
  llvm/llvm,
  ./llplatform

iterator passLinkOptions(opts: openArray[string]): string =
  var passNext = false
  template yields(x: untyped) =
    yield strip(x, true, true, {'\'', '\"'})

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
      yields s[4 ..^ 1]
    elif not s.startsWith("-"):
      yields s

proc findLibraries(s: string): seq[string] =
  for line in s.split('\n'):
    if line.startsWith("libraries: ="):
      return line["libraries: =".len ..^ 1].split(PathSep).filterIt(it.len > 0)

proc findLastArg(args: openArray[string], opts: openArray[string]): string =
  for i in 1 .. args.len:
    if args[^i] in opts:
      return args[^i]
  return ""

proc findFile(candidates: openArray[string], name: string): string =
  for candidate in candidates:
    if os.fileExists(candidate / name):
      return candidate / name
  ""

proc getLinker(conf: ConfigRef, target: string): string =
  var linkerExe = getConfigVar(conf, conf.cCompiler, ".linkerexe")
  if linkerExe.len == 0:
    linkerExe = getLinkerExe(conf, conf.cCompiler)
  linkerExe = quoteShell(linkerExe)
  if conf.cCompiler == ccClang and target.len > 0:
    linkerExe = linkerExe & " --target=" & target
  linkerExe

proc printSearchDirs(conf: ConfigRef, target: string): seq[string] =
  try:
    let driver = conf.getLinker(target)
    findLibraries(strip(execProcess(&"{driver} --print-search-dirs"))).filterIt(
      it.len > 0
    )
  except CatchableError:
    @[]

proc printFileName(conf: ConfigRef, file, target: string): string =
  try:
    let driver = conf.getLinker(target)
    strip(execProcess(&"{driver} --print-file-name={quoteShell(file)}"))
  except CatchableError:
    file

proc linkGNU(conf: ConfigRef, target: string) =
  # This function emulates the clang linker driver which emulates gcc which
  # builds on 50 years os accumulated technical debt in the linker tooling
  # space!

  # It is loosely based on `Linker::ConstructJob` for the various platforms,
  # as found in:
  # https://github.com/llvm/llvm-project/blob/llvmorg-21.1.8/clang/lib/Driver/ToolChains/Gnu.cpp#L283

  let filePaths = conf.printSearchDirs(target)
  let linkOpts = parseCmdLine(conf.linkOptions) & parseCmdLine(conf.linkOptionsCmd)

  var args: seq[string]
  args.add "ld.lld"

  let
    isLinux = conf.target.targetOS in {osLinux, osAndroid}
    isAndroid = conf.target.targetOS == osAndroid
    isMips = conf.target.targetCPU in {cpuMips, cpuMipsel, cpuMips64, cpuMips64el}
    pieArg = linkOpts.findLastArg(["-pie", "-no-pie", "-nopie"])
    # isShared is set when we're creating a static library
    isShared = optGenDynLib in conf.globalOptions or "-shared" in linkOpts
    isPIE =
      not (
        isShared or "-static" in linkOpts or "-r" in linkOpts or
        "-static-pie" in linkOpts
      ) and pieArg in ["-pie"]
    isStaticPIE = "-static-pie" in linkOpts
    # isStatic is set when dependencies should be linked statically - if isShared
    # is also set, it means we're creating a shared library whose dependencies
    # are linked statically
    isStatic = "-static" in linkOpts and not isStaticPIE
    isCpp = optMixedMode in conf.globalOptions

  if "-s" in linkOpts:
    args.add ["-s"]

  if isAndroid: # TODO or alpine linux
    args.add ["-z", "now"]

  if isAndroid: # TODO or suse/ubuntu/alpine linux
    args.add ["-z", "relro"]

  if isLinux and not isMips:
    # Modern linux uses gnu hash style
    args.add "--hash-style=gnu"

  if isAndroid: # TODO or opensuse
    args.add "--enable-new-dtags"

  args.add "--eh-frame-hdr"

  # Android ARM/AArch64 use max-page-size=4096 to reduce VMA usage
  if (conf.target.targetCPU in {cpuArm, cpuArm64}) and isAndroid:
    args.add ["-z", "max-page-size=4096"]

  case conf.target.targetCPU
  of cpuAmd64:
    args.add ["-m", "elf_x86_64"]
  of cpuI386:
    args.add ["-m", "elf_i386"]
  of cpuArm64:
    discard
  of cpuArm:
    args.add ["-m", "armelf"]
  of cpuRiscv64:
    args.add ["-m", "elf64lriscv"]
    # On some BSDs clang adds an extra -X flag for riscv
    if conf.target.targetOS in {osNetbsd, osOpenbsd}:
      args.add "-X"
  of cpuMips:
    args.add ["-m", "elf32btsmip"]
  of cpuMipsel:
    args.add ["-m", "elf32ltsmip"]
  of cpuMips64:
    # prefer 64-bit ABI where available
    args.add ["-m", "elf64btsmip"]
  of cpuMips64el:
    args.add ["-m", "elf64ltsmip"]
  of cpuPowerpc:
    args.add ["-m", "elf32ppc"]
  of cpuPowerpc64, cpuPowerpc64el:
    args.add ["-m", "elf64ppc"]
  of cpuSparc:
    args.add ["-m", "elf32_sparc"]
  of cpuSparc64:
    args.add ["-m", "elf64_sparc"]
  of cpuLoongArch64:
    discard
  else:
    discard

  if isShared:
    args.add "-shared"

  if isStaticPIE:
    args.add ["-static", "-pie", "--no-dynamic-linker", "-z", "text"]
  elif isStatic:
    args.add "-static"
  elif "-r" notin linkOpts:
    if "-rdynamic" in linkOpts:
      args.add "-export-dynamic"

    if not isShared:
      if isPIE:
        args.add "-pie"

      # Per-OS dynamic linker defaults (clang sets these for many toolchains)
      case conf.target.targetOS
      of osLinux:
        case conf.target.targetCPU
        of cpuAmd64:
          args.add ["-dynamic-linker", "/lib64/ld-linux-x86-64.so.2"]
        of cpuI386:
          args.add ["-dynamic-linker", "/lib/ld-linux.so.2"]
        of cpuArm64:
          args.add ["-dynamic-linker", "/lib/ld-linux-aarch64.so.1"]
        else:
          discard
      of osFreebsd:
        args.add ["-dynamic-linker", "/libexec/ld-elf.so.1"]
      of osNetbsd:
        args.add ["-dynamic-linker", "/libexec/ld.elf_so"]
      of osOpenbsd:
        args.add ["-dynamic-linker", "/usr/libexec/ld.so"]
      else:
        discard

  let exeFile = conf.prepareToWriteOutput()

  args.add ["-o", exefile.string]

  if "-nostdlib" notin linkOpts and "-nostartfiles" notin linkOpts and
      "-r" notin linkOpts:
    if not isAndroid:
      let crt1 =
        if isPIE:
          "Scrt1.o"
        elif isStaticPIE:
          "rcrt1.o"
        else:
          "crt1.o"
      args.add conf.printFileName(crt1, target)

    args.add conf.printFileName("crti.o", target)

    let crtbegin =
      if isShared:
        if isAndroid: "crtbegin_so.o" else: "crtbeginS.o"
      elif isStatic:
        if isAndroid: "crtbegin_static.o" else: "crtbeginT.o"
      elif isPIE or isStaticPIE:
        if isAndroid: "crtbegin_dynamic.o" else: "crtbeginS.o"
      else:
        if isAndroid: "crtbegin_dynamic.o" else: "crtbegin.o"

    args.add conf.printFileName(crtbegin, target)

  for opt in linkOpts:
    if opt.startsWith("-L"):
      args.add opt
  for opt in linkOpts:
    if opt.startsWith("-u"):
      args.add opt

  for lib in filePaths:
    args.add "-L" & lib

  args.add "--as-needed"

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
        "-pthread" in linkOpts or "-pthreads" in linkOpts or
        optThreads in conf.globalOptions

      template addRuntimeLibs() =
        if isStatic or isStaticPIE:
          args.add ["-lgcc", "-lgcc_eh"]
        elif isShared:
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

    let crtend =
      if isShared:
        if isAndroid: "crtend_so.o" else: "crtendS.o"
      elif isPIE or isStaticPIE:
        if isAndroid: "crtend_android.o" else: "crtendS.o"
      else:
        if isAndroid: "crtend_android.o" else: "crtend.o"

    args.add conf.printFileName(crtend, target)

    if not isAndroid:
      args.add conf.printFileName("crtn.o", target)

  rawMessage(conf, hintLinking, $args)

  if not nimLLDLinkElf(args):
    rawMessage(conf, errGenerated, "linking failed")
    quit(1)

proc last(v: seq[string]): seq[string] =
  if v.len > 0:
    @[v[^1]]
  else:
    @[]

proc linkMingw(conf: ConfigRef, target: string) =
  # https://github.com/llvm/llvm-project/blob/llvmorg-21.1.8/clang/lib/Driver/ToolChains/MinGW.cpp#L103

  let
    filePaths = conf.printSearchDirs(target)
    linkOpts = parseCmdLine(conf.linkOptions) & parseCmdLine(conf.linkOptionsCmd)
    isShared = optGenDynLib in conf.globalOptions or "-shared" in linkOpts
    isCpp = optMixedMode in conf.globalOptions

  var args: seq[string]
  args.add "ld.lld"

  args.add linkOpts.filterIt(it.startsWith("--sysroot"))
  args.add linkOpts.filterIt(it == "-s")

  # -m emulation selection
  case conf.target.targetCPU
  of cpuI386:
    args.add ["-m", "i386pe"]
  of cpuAmd64:
    args.add ["-m", "i386pep"]
  of cpuArm:
    args.add ["-m", "thumb2pe"]
  of cpuArm64:
    args.add ["-m", "arm64pe"]
  else:
    discard

  # Subsystem
  if "-mwindows" in linkOpts or optGenGuiApp in conf.globalOptions:
    args.add ["--subsystem", "windows"]
  elif "-mconsole" in linkOpts:
    args.add ["--subsystem", "console"]

  let
    hasMdll = "-mdll" in linkOpts
    isStatic = "-static" in linkOpts

  if hasMdll:
    args.add "--dll"
  elif isShared:
    args.add "--shared"

  if isStatic:
    args.add "-Bstatic"
  else:
    args.add "-Bdynamic"

  if hasMdll or isShared:
    args.add ["-e", "DllMainCRTStartup"]
    args.add "--enable-auto-image-base"

  for opt in linkOpts:
    if opt.startsWith("-mguard=") or opt.startsWith("-mguard:"):
      if opt.contains("none"):
        args.add "--no-guard-cf"
      elif opt.contains("cf"):
        args.add "--guard-cf"

  args.add ["-o", addFileExt(conf.prepareToWriteOutput().string, "exe")]

  # Add startfiles unless requested not to
  if "-nostdlib" notin linkOpts and "-nostartfiles" notin linkOpts:
    if hasMdll or isShared:
      args.add conf.printFileName("dllcrt2.o", target)
    else:
      if "-municode" in linkOpts:
        args.add conf.printFileName("crt2u.o", target)
      else:
        args.add conf.printFileName("crt2.o", target)
    if "-pg" in linkOpts:
      args.add conf.printFileName("gcrt2.o", target)
    args.add conf.printFileName("crtbegin.o", target)

  args.add linkOpts.filterIt(it.startsWith("-L"))
  for lib in filePaths:
    args.add "-L" & lib

  args.add "--as-needed"

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
        if conf.printFileName("libc++.so", target) == "libc++.so" and
            conf.printFileName("libc++.a", target) == "libc++.a":
          args.add "-lstdc++"
        else:
          args.add "-lc++"

  if "-mthreads" in linkOpts:
    args.add "-lmingwthrd"
  args.add "-lmingw32"

  let archName = conf.target.targetCPU.toLLVMArch()

  # TODO we'll have to do something about this: if `gcc` is symlinked to `clang`
  #      conf.cCompiler will say gcc but we'll actually be using clang libraries
  #      on windows - at least this seems to hold in the clang64 environment
  let crt =
    if isStatic or "-static-libgcc" in linkOpts:
      [&"libclang_rt.builtins-{archName}.a", "-l:libunwind.a"]
    elif "-shared-libgcc" in linkOpts:
      [&"libclang_rt.builtins-{archName}.dll.a", "-l:libunwind.dll.a"]
    else:
      [&"libclang_rt.builtins-{archName}.a", "-lunwind"]

  if conf.printFileName(crt[0], target) == crt[0]:
    # Couldn't find clang_rt? Assume gcc..
    if isStatic or "-static-libgcc" in linkOpts:
      args.add ["-lgcc", "-lgcc_eh"]
    else:
      args.add ["-lgcc_s", "-lgcc"]
  else:
    args.add [conf.printFileName(crt[0], target), crt[1]]

  args.add "-lmoldname"
  args.add "-lmingwex"

  template isCRT(s: string): bool =
    s.startsWith("-lmsvcr") or s.startsWith("-lucrt") or s.startsWith("-lcrtdll") or
      s.startsWith("msvcr") or s.startsWith("ucrt") or s.startsWith("crtdll")

  if not (conf.cLinkedLibs.anyIt(it.isCRT) or linkOpts.anyIt(it.isCRT)):
    args.add "-lmsvcrt" # TODO default to ucrt? probably

  # user-specified -l flags
  for linkedLib in items(conf.cLinkedLibs):
    args.add parseCmdLine(CC[conf.cCompiler].linkLibCmd % linkedLib.quoteShell)

  # system libs
  if not ("-lwindowsapp" in linkOpts):
    if ("-mwindows" in linkOpts) or optGenGuiApp in conf.globalOptions:
      args.add "-lgdi32"
      args.add "-lcomdlg32"
    args.add "-ladvapi32"
    args.add "-lshell32"
    args.add "-luser32"
    args.add "-lkernel32"

  # crtend
  if "-nostdlib" notin linkOpts and "-nostartfiles" notin linkOpts:
    args.add conf.printFileName("crtend.o", target)

  rawMessage(conf, hintLinking, $args)
  if not nimLLDLinkMingw(args):
    rawMessage(conf, errGenerated, "linking failed")
    quit(1)

proc linkWasm(conf: ConfigRef) =
  var args: seq[string]

  args.add "wasm-ld"

  let
    exeFile = conf.getOutFile(conf.outFile, "wasm")
    linkOpts = parseCmdLine(conf.linkOptions) & parseCmdLine(conf.linkOptionsCmd)
    isShared = optGenDynLib in conf.globalOptions or "-shared" in linkOpts

  args.add ["-m", "wasm32"]

  if "-s" in linkOpts:
    args.add ["--strip-all"]

  if isShared:
    args.add ["-shared"]

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

proc linkMSVC(conf: ConfigRef) =
  # Generic COFF/PE linking using LLD's COFF driver (lld-link)
  # TODO actually test this - it is loosely based on translating the clang code

  var args: seq[string]
  args.add "lld-link"

  let exeFile = conf.prepareToWriteOutput()
  # MSVC-style output option
  args.add "/OUT:" & exefile.string

  # Add object files (.obj or .o depending on compiler)
  for it in conf.externalToLink:
    args.add addFileExt(it, CC[conf.cCompiler].objExt)
  for x in conf.toCompile:
    args.add x.obj.string

  let linkOpts = parseCmdLine(conf.linkOptions) & parseCmdLine(conf.linkOptionsCmd)
  echo linkOpts
  # Basic MSVC-like flags: MACHINE, SUBSYSTEM, DLL/static, optimisation
  case conf.target.targetCPU
  of cpuAmd64:
    args.add "/MACHINE:X64"
  of cpuI386:
    args.add "/MACHINE:X86"
  of cpuArm64:
    args.add "/MACHINE:ARM64"
  else:
    discard

  if optGenDynLib in conf.globalOptions:
    args.add "/DLL"

  # Subsystem: GUI vs console
  if optGenGuiApp in conf.globalOptions:
    args.add "/SUBSYSTEM:WINDOWS"
  else:
    args.add "/SUBSYSTEM:CONSOLE"

  # Optimize and strip options: when not requesting debug symbols, enable
  # optimizations similar to clang's MSVC driver.
  if optCDebug in conf.globalOptions:
    args.add "/DEBUG"
  else:
    args.add "/INCREMENTAL:NO"
    args.add "/OPT:REF"
    args.add "/OPT:ICF"

  # Choose default CRT libraries similar to MSVC driver logic.
  let wantStaticCRT =
    "/MT" in linkOpts or "/MTd" in linkOpts or "-MT" in linkOpts or "-MTd" in linkOpts or
    "-static" in linkOpts or conf.isDefined("static")

  if wantStaticCRT:
    # Static CRT
    args.add "libcmt.lib"
    args.add "vcruntime.lib"
  else:
    # Dynamic CRT (modern MSVC uses UCRT + VCRUNTIME)
    args.add "ucrt.lib"
    args.add "vcruntime.lib"

  # Always add core Windows libraries
  args.add "kernel32.lib"
  if optGenGuiApp in conf.globalOptions:
    args.add "user32.lib"

  # Translate lib dirs and linked libs into COFF-style flags
  for libDir in items(conf.cLibs):
    args.add "/LIBPATH:" & $libDir

  for linkedLib in items(conf.cLinkedLibs):
    let libcmd = CC[conf.cCompiler].linkLibCmd % linkedLib.quoteShell
    # Try to convert common "-lfoo" into "foo.lib" for COFF
    if libcmd.startsWith("-l"):
      let name = libcmd[2 ..^ 1]
      args.add name & ".lib"
    else:
      for part in parseCmdLine(libcmd):
        args.add part

  for opt in passLinkOptions(
    parseCmdLine(conf.linkOptions) & parseCmdLine(conf.linkOptionsCmd)
  ):
    # Convert -Ldir -> /LIBPATH:dir
    if opt.startsWith("-L"):
      args.add "/LIBPATH:" & opt[2 ..^ 1]
    elif opt.startsWith("-l"):
      args.add opt[2 ..^ 1] & ".lib"
    else:
      args.add opt

  rawMessage(conf, hintLinking, $args)
  if not nimLLDLinkCoff(args):
    rawMessage(conf, errGenerated, "linking failed")
    quit(1)

proc useBuiltinLinker*(conf: ConfigRef): bool =
  optGenStaticLib notin conf.globalOptions and
    conf.getConfigVar("nlvm.linker", "builtin") == "builtin"

proc lllink*(conf: ConfigRef, target: string) =
  if conf.useBuiltinLinker():
    # Compile {.compile.} files as usual
    conf.globalOptions.incl optNoLinking
    callCCompiler(conf)
    conf.globalOptions.excl optNoLinking

    if conf.target.targetCPU == cpuWasm32:
      linkWasm(conf)
      return

    if conf.cCompiler in {ccGcc, ccLLVM_Gcc, ccCLang}:
      if conf.target.targetOS == osWindows:
        linkMingw(conf, target)
      else:
        linkGnu(conf, target)
      return

    if conf.cCompiler in {ccVcc, ccClangCl}:
      linkMSVC(conf)
      return

  # Non-builtin linker fallback
  if conf.cCompiler in {ccGcc, ccLLVM_Gcc, ccCLang}:
    conf.addLinkOptionCmd("-Wl,--as-needed")
  callCCompiler(conf)
