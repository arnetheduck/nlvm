import std/[os, strutils]

proc fib*(n: int): int {.exportc, dynlib.} =
  var
    curr = 0

    prev1 = 1
    prev2 = 0

  for i in 2 .. n:
    curr = prev1 + prev2
    prev2 = prev1
    prev1 = curr

  curr

if paramCount() == 1:
  stdout.writeLine($fib(parseInt(paramStr(1))))
else:
  stdout.writeLine("fib.wasm number")
