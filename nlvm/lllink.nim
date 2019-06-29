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

proc stripLinkOption(s: string): string =
  # TODO the gcc driver has multiple ways of passing stuff, and then there's the
  #      shell to contend with, with its quoting etc.. this needs a better
  #      approach
  let tmp =
    if s.startsWith("-Wl,"):
      s[4..^1]
    else:
      s

  strip(tmp, chars={'\''})

proc stripLinkOption(s: openArray[string]): seq[string] =
  for v in s:  result.add stripLinkOption(v)

proc findLibraries(s: string): seq[string] =
  for line in s.split('\n'):
    if line.startsWith("libraries: ="):
      return line["libraries: =".len..^1].split(':')

proc linkLinuxAmd64(conf: ConfigRef) =
  # The command line built here is based on clang 7 on Fedora 29 / x86_64 -
  # it's likely to be wrong for anything else!

  # Ugly hack to reuse nim compiling of C files
  conf.globalOptions.incl optNoLinking
  callCCompiler(conf)
  conf.globalOptions.excl optNoLinking

  var args: seq[string]

  # TODO No idea what most of these options do or which are necessary..
  args.add "ld.lld"
  args.add "--hash-style=gnu"
  args.add "--no-add-needed"
  args.add "--build-id"
  args.add "--eh-frame-hdr"
  args.add ["-m", "elf_x86_64"]
  args.add ["-dynamic-linker", "/lib64/ld-linux-x86-64.so.2"]

  let exeFile = conf.getOutFile(conf.outFile, platform.OS[conf.target.targetOS].exeExt)

  args.add ["-o", exefile.string]

  args.add "/usr/lib64/crt1.o" # /usr/bin/../lib/gcc/x86_64-redhat-linux/8/../../../../lib64/crt1.o
  args.add "/usr/lib64/crti.o" # /usr/bin/../lib/gcc/x86_64-redhat-linux/8/../../../../lib64/crti.o

  # /usr/bin/../lib/gcc/x86_64-redhat-linux/8/crtbegin.o

  # -L/usr/bin/../lib/gcc/x86_64-redhat-linux/8

  # We're going to load all standard library locations that gcc uses, if possible
  let libs =
    try:
      findLibraries(execProcess("gcc -print-search-dirs").string)
    except:
      @[]

  for lib in libs:
    args.add "-L" & lib

  # Then the project object files...
  for it in conf.externalToLink:
    args.add addFileExt(it, CC[conf.cCompiler].objExt)

  for x in conf.toCompile:
    args.add x.obj.string

  args.add stripLinkOption(parseCmdLine(conf.linkOptions))
  args.add stripLinkOption(parseCmdLine(conf.linkOptionsCmd))

  for linkedLib in items(conf.cLinkedLibs):
    args.add CC[conf.cCompiler].linkLibCmd % linkedLib.quoteShell
  for libDir in items(conf.cLibs):
    args.add join([CC[conf.cCompiler].linkDirCmd, libDir.quoteShell])

  # And yet more standard linking options:

  args.add "-lstdc++"
  args.add "-lm"

  # args.add "-lgcc"

  # args.add "--as-needed"
  # args.add "-lgcc_s"
  # args.add "--no-as-needed"
  args.add "-lc"
  # args.add "-lgcc"
  # args.add "--as-needed"
  # args.add "-lgcc_s"
  # args.add "--no-as-needed"

  # /usr/bin/../lib/gcc/x86_64-redhat-linux/8/crtend.o

  args.add "/usr/lib64/crtn.o" # /usr/bin/../lib/gcc/x86_64-redhat-linux/8/../../../../lib64/crtn.o

  echo args
  rawMessage(conf, hintLinking, $args)

  discard nimLLDLinkElf(args)

proc linkWasm32(conf: ConfigRef) =
  var args: seq[string]

  args.add "wasm-ld"

  let exeFile = conf.getOutFile(conf.outFile, "wasm")

  args.add ["-o", exefile.string]

  # TODO this stripping of shell quoting and other stuff is pretty ugly - needs
  #      a holistic review
  args.add stripLinkOption(parseCmdLine(conf.linkOptions))
  args.add stripLinkOption(parseCmdLine(conf.linkOptionsCmd))

  for it in conf.externalToLink:
    args.add addFileExt(it, CC[conf.cCompiler].objExt)

  for x in conf.toCompile:
    args.add x.obj.string

  # TODO this causes stuff to be included in the wasm file, but there's probably
  #      a better way
  args.add("--export-dynamic")

  let result = nimLLDLinkWasm(args)
  if result.len > 0:
    rawMessage(conf, errGenerated, "linking failed: '$1'" % (result))
    quit(1)

proc lllink*(conf: ConfigRef) =
  if false and conf.target.targetOS == osLinux and conf.target.targetCPU == cpuAmd64:
    # TODO this stuff is not ready for prime time..
    linkLinuxAmd64(conf)
  elif conf.target.targetCPU == cpuWasm32:
    linkWasm32(conf)
  else:
    # TODO configure this elsewhere? also, -Wl vs raw linker options..:/
    conf.addLinkOptionCmd("-Wl,--as-needed")

    callCCompiler(conf)
