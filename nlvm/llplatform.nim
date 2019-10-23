# Translate Nim platform stuff to target triples
# This is a complete mess.

# See https://github.com/llvm/llvm-project/blob/master/llvm/include/llvm/ADT/Triple.h
# for some insight into how llvm deals with it..

import
  compiler/platform

import strutils

# couldn't find those marked with ? in llvm targets..
# the first option will be used for output, but parsing will try all..
const
  cpuNames: array[TSystemCPU, seq[string]] =
    [
      @["unknown"],
      @["i386", "i486", "i586", "i686"],
      @["m68k"], # ?
      @["alpha"], # ?
      @["powerpc"],
      @["powerpc64"],
      @["powerpc64le"],
      @["sparc"],
      @["vm"], # ?
      @["ia64"], # ?
      @["x86_64", "amd64"],
      @["mips"],
      @["mips64el"],
      @["arm"],
      @["aarch64"],
      @["js"], # ? emscripten maybe?
      @["nimvm"], # ?
      @["avr"],
      @["msp430"],
      @["sparc64"], # ?
      @["mips64"],
      @["mips64el"],
      @["riscv64"],
      @["wasm32"]
    ]
  osNames: array[TSystemOS, seq[string]] =
    [
      @["unknown"],
      @["dos"], # ?
      @["windows"],
      @["os2"], # ?
      @["linux"],
      @["morphos"], # ?
      @["skyos"], # ?
      @["solaris"],
      @["irix"],
      @["netbsd"],
      @["freebsd"],
      @["openbsd"],
      @["dragonfly"],
      @["aix"],
      @["palmos"], # ?
      @["qnx"], # ?
      @["amiga"], # ?
      @["atari"], # ?
      @["netware"], # ?
      @["macos"], # ? how to disambiguate with osx?
      @["macos"],
      @["ios"], # ?
      @["haiku"],
      @["android"], # ?
      @["vxworks"], # ?
      @["genode"], # ?
      @["js"], # ?
      @["nimvm"], # ?
      @["unknown"], # ?
      @["nintendoswitch"] # ?
    ]

proc toTriple*(cpu: TSystemCPU, os: TSystemOS): string =
  # In most cases, we'll just use the string from above, but there are plenty
  # of unknowns and some special cases where we'll just randomly pick something
  # - this needs upstream fixes

  if cpu == cpuArm and os == osLinux:
    "arm-linux-gnueabihf" # assume rpi3
  elif cpu == cpuAmd64 and os == osLinux:
    "x86_64-unknown-linux-gnu" # distro missing! what about c library?
  else:
    cpuNames[cpu][0] & "-unknown-" & osNames[os][0]

proc parseTarget*(target: string): tuple[cpu: TSystemCPU, os: TSystemOS] =
  let t = target.normalize()
  if t == "arm-linux-gnueabihf":
    (cpuArm, osLinux)
  else:
    var
      cpu = cpuNone
      os = osStandalone

    for xcpu, names in cpuNames:
      for name in names:
        if t.startsWith(name): cpu = xcpu

    for xos, names in osNames:
      # TODO keeps looping because later names seem more recent, but this needs
      #      verifying.. "contains" is a bit too lax
      for name in names:
        if t.contains(name): os = xos

    (cpu, os)
