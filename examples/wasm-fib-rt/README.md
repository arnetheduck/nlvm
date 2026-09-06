# fibonacci in WASM runtime

Compile and run using [`wasm3`](https://github.com/wasm3/wasm3):

```sh
# Compile to wasm (see fib.nim.cfg for options!)
nlvm c fib
# Run using wasm3
wasm3 fib.wasm 10
# Inspect WASM bytecode (using https://github.com/webassembly/binaryen)
wasm2wat fib.wasm -o fib.wat
```
