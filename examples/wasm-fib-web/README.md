# fibonacci / JavaScript

`fib.nim` exports `fib(n)` to the wasm module and imports `offset` from the
host, so the result is `fib(n) + offset()`. The host function lives in
`fib.js` and reads the text field in `index.html` (browser) or takes its
values from the command line (Node.js).

## Compile

```sh
nlvm c fib
```

## Run in the browser

Serve the directory (e.g. with python's http server), then open
`http://127.0.0.1:8000` in a browser:

```sh
python -m http.server --bind 127.0.0.1
```

Enter `n` and the offset in the text fields, then click **calculate**.

## Run in Node.js

```sh
node fib.js <n> [offset]
# e.g.
node fib.js 10 5   # → fib(10) = 60
```

Or use the module programmatically:

```sh
node -e "
  const { loadFib } = require('./fib.js');
  loadFib('./fib.wasm', () => 5).then(exports => {
    console.log(exports.fib(10));
  });
"
```
