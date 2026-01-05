# Translate Nim platform stuff to target triples

# See https://github.com/llvm/llvm-project/blob/main/llvm/lib/TargetParser/Triple.cpp
# for some insight into how llvm deals with it..

import compiler/platform

import strutils

proc toLLVMArch*(cpu: TSystemCPU): string =
  case cpu
  of cpuNone: "unknown"
  of cpuI386: "i386"
  of cpuAmd64: "x86_64"
  of cpuArm: "arm"
  of cpuArm64: "aarch64"
  of cpuMips: "mips"
  of cpuMipsel: "mipsel"
  of cpuMips64: "mips64"
  of cpuMips64el: "mips64el"
  of cpuRiscV32: "riscv32"
  of cpuRiscV64: "riscv64"
  of cpuWasm32: "wasm32"
  of cpuLoongArch64: "loongarch64"
  of cpuPowerpc: "powerpc"
  of cpuPowerpc64: "powerpc64"
  of cpuPowerpc64el: "powerpc64le"
  of cpuSparc: "sparc"
  of cpuSparc64: "sparc64"
  of cpuAVR: "avr"
  of cpuIa64: "ia64"
  of cpuM68k: "m68k"
  of cpuAlpha: "alpha"
  of cpuVm: "vm"
  of cpuHppa: "hppa"
  of cpuE2k: "e2k"
  of cpuEsp: "xtensa"
  of cpuJS: "js"
  of cpuNimVM: "nimvm"
  of cpuMSP430: "msp430"

proc toTriple*(
    t: Target,
    abi: string = "",
    useWasi: bool = false,
    isMingw: bool = false,
    libc: string = "",
): string =
  ## Return a reasonable LLVM target triple for `t`.
  ##
  ## `abi` may be used to request alternate ABIs (e.g. "musl", "gnu",
  ## "wasi", "android", "mingw"). The boolean flags are provided as
  ## helpers for callers that have explicit booleans. If `abi` is set it
  ## takes precedence over `libc` and the boolean flags.
  let arch = toLLVMArch(t.targetCPU)

  var vendor = "unknown"
  var osPart = "unknown-unknown"
  let a = (if abi.len > 0: abi else: libc).toLowerAscii()
  let wantWasi = useWasi or a == "wasi"
  let wantMingw = isMingw or a == "mingw" or a == "gnu"
  case t.targetOS
  of osLinux:
    if a.len > 0:
      osPart = "linux-" & a
    else:
      osPart = "linux-gnu"
  of osAndroid:
    osPart = "linux-android"
  of osMacosx, osMacos:
    vendor = "apple"
    osPart = "darwin"
  of osIos:
    vendor = "apple"
    osPart = "ios"
  of osWindows:
    vendor = "pc"
    if wantMingw:
      osPart = "windows-gnu"
    else:
      osPart = "windows-msvc"
  of osFreebsd:
    osPart = "freebsd"
  of osOpenbsd:
    osPart = "openbsd"
  of osNetbsd:
    osPart = "netbsd"
  of osDragonfly:
    osPart = "dragonfly"
  of osSolaris:
    osPart = "solaris"
  of osAix:
    osPart = "aix"
  of osHaiku:
    osPart = "haiku"
  of osVxWorks:
    osPart = "vxworks"
  of osGenode:
    osPart = "unknown-elf"
  of osJS:
    # JS hosting: prefer WASM/WASI if requested
    if wantWasi:
      return "wasm32-unknown-wasi"
    osPart = "unknown-unknown"
  of osNintendoSwitch:
    osPart = "unknown-elf"
  of osFreeRTOS, osZephyr, osNuttX:
    osPart = "unknown-elf"
  of osAny:
    osPart = "unknown-unknown"
  else:
    osPart = "unknown-unknown"

  # Special-case wasm: support wasi if requested.
  if arch == "wasm32":
    if wantWasi:
      return "wasm32-unknown-wasi"
    # Default wasm target
    return "wasm32-unknown-unknown"

  arch & "-" & vendor & "-" & osPart

proc parseTarget*(target: string): tuple[cpu: TSystemCPU, os: TSystemOS] =
  ## Parse a target triple (or short triple) and return the corresponding
  ## `TSystemCPU` and `TSystemOS`. This attempts to recognize the same
  ## architectures and OS names used by `toTriple`.
  var s = target.toLowerAscii()

  # Split components of the triple (arch-vendor-os-environment)
  let parts = s.split('-')
  var archTok =
    if parts.len > 0:
      parts[0]
    else:
      s

  var cpu = cpuNone
  var os = osAny
  case archTok
  of "x86_64", "amd64":
    cpu = cpuAmd64
  of "i386", "i486", "i586", "i686":
    cpu = cpuI386
  of "aarch64", "arm64":
    cpu = cpuArm64
  of "arm":
    cpu = cpuArm
  of "powerpc64le", "ppc64le":
    cpu = cpuPowerpc64el
  of "powerpc64", "ppc64":
    cpu = cpuPowerpc64
  of "powerpc", "ppc":
    cpu = cpuPowerpc
  of "mips64el":
    cpu = cpuMips64el
  of "mips64":
    cpu = cpuMips64
  of "mipsel":
    cpu = cpuMipsel
  of "mips":
    cpu = cpuMips
  of "riscv64":
    cpu = cpuRiscV64
  of "riscv32":
    cpu = cpuRiscV32
  of "wasm", "wasm32", "wasm64":
    cpu = cpuWasm32
  of "loongarch64":
    cpu = cpuLoongArch64
  of "sparc64":
    cpu = cpuSparc64
  of "sparc":
    cpu = cpuSparc
  of "ia64", "itanium":
    cpu = cpuIa64
  of "avr":
    cpu = cpuAVR
  of "msp430":
    cpu = cpuMSP430
  of "xtensa", "esp":
    cpu = cpuEsp
  of "e2k":
    cpu = cpuE2k
  of "hppa":
    cpu = cpuHppa
  of "alpha":
    cpu = cpuAlpha
  of "m68k":
    cpu = cpuM68k
  of "js":
    cpu = cpuJS
  of "nimvm":
    cpu = cpuNimVM
  else:
    # Try looser substring matches over the whole triple.
    if s.contains("x86_64") or s.contains("amd64"):
      cpu = cpuAmd64
    elif s.contains("i386"):
      cpu = cpuI386
    elif s.contains("arm64") or s.contains("aarch64"):
      cpu = cpuArm64
    elif s.contains("arm"):
      cpu = cpuArm
    elif s.contains("powerpc64le") or s.contains("ppc64le"):
      cpu = cpuPowerpc64el
    elif s.contains("powerpc64") or s.contains("ppc64"):
      cpu = cpuPowerpc64
    elif s.contains("powerpc") or s.contains("ppc"):
      cpu = cpuPowerpc
    elif s.contains("mips64el"):
      cpu = cpuMips64el
    elif s.contains("mips64"):
      cpu = cpuMips64
    elif s.contains("mipsel"):
      cpu = cpuMipsel
    elif s.contains("mips"):
      cpu = cpuMips
    elif s.contains("riscv64"):
      cpu = cpuRiscV64
    elif s.contains("riscv32"):
      cpu = cpuRiscV32
    elif s.contains("wasm"):
      cpu = cpuWasm32
    elif s.contains("loongarch"):
      cpu = cpuLoongArch64

  # Determine OS by looking at later components and common tokens.
  for i in 1 .. parts.len - 1:
    let p = parts[i]
    case p
    of "linux":
      os = osLinux
    of "android":
      os = osAndroid
    of "darwin", "macos":
      os = osMacosx
    of "ios":
      os = osIos
    of "windows", "mingw32", "mingw", "win32":
      os = osWindows
    of "freebsd":
      os = osFreebsd
    of "openbsd":
      os = osOpenbsd
    of "netbsd":
      os = osNetbsd
    of "dragonfly":
      os = osDragonfly
    of "solaris", "sunos":
      os = osSolaris
    of "aix":
      os = osAix
    of "vxworks":
      os = osVxWorks
    of "haiku":
      os = osHaiku
    of "wasi":
      # wasm+wasi maps to wasm CPU and JS-like OS
      cpu = cpuWasm32
      os = osJS
    else:
      # no direct match in this part; continue to next part
      continue

  # If arch token indicates wasm, ensure CPU is set.
  if (archTok == "wasm" or archTok == "wasm32" or archTok == "wasm64") and cpu == cpuNone:
    cpu = cpuWasm32

  return (cpu, os)

when isMainModule:
  let cases = [
    (cpuAmd64, osLinux, ""),
    (cpuArm, osLinux, ""),
    (cpuAmd64, osWindows, "mingw"),
    (cpuWasm32, osJS, "wasi"),
    (cpuArm64, osMacosx, ""),
    (cpuI386, osNetbsd, ""),
    (cpuLoongArch64, osLinux, ""),
    (cpuRiscV64, osLinux, ""),
    (cpuPowerpc64el, osLinux, ""),
    (cpuMips64el, osLinux, ""),
  ]

  for c in cases:
    var T: Target
    T.setTarget(c[1], c[0])
    let abi = c[2]
    let triple =
      if abi.len > 0:
        toTriple(T, abi = abi)
      else:
        toTriple(T)
    let (cpu2, os2) = parseTarget(triple)
    assert(
      cpu2 == T.targetCPU,
      "cpu round-trip failed: " & triple & " -> " & $cpu2 & " (want " & $T.targetCPU &
        ")",
    )
    assert(
      os2 == T.targetOS,
      "os round-trip failed: " & triple & " -> " & $os2 & " (want " & $T.targetOS & ")",
    )

  echo "llplatform.nim: round-trip tests passed"
