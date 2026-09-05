# `exportc` to export to JavaScript, `importc` to import from JavaScript!
# The `offset` function lives in the JS host (see index.html).
proc offset(): int {.importc.}

proc fib(n: int): int {.exportc, dynlib.} =
  var
    curr = 0

    prev1 = 1
    prev2 = 0

  for i in 2 .. n:
    curr = prev1 + prev2
    prev2 = prev1
    prev1 = curr

  curr + offset()
