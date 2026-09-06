// Shared by the browser (index.html) and Node.js.

// Reads the offset from the #offset text field (browser only).
function domOffset() {
  return parseInt(document.getElementById("offset").value, 10) || 0;
}

// Loads the wasm module and returns its exports.
// - wasmPath: path/URL of fib.wasm
// - offset: function returning the number to add (browser default: domOffset)
async function loadFib(wasmPath, offset = domOffset) {
  let bytes;
  if (typeof window !== "undefined") {
    bytes = await (await fetch(wasmPath)).arrayBuffer();
  } else {
    const fs = await import("node:fs");
    bytes = fs.readFileSync(wasmPath);
  }
  const { instance } = await WebAssembly.instantiate(bytes, { env: { offset } });
  return instance.exports;
}

// Exports for Node.js; globals for the browser.
if (typeof module !== "undefined") {
  module.exports = { loadFib, domOffset };
}

// CLI: node fib.js <n> [offset]
if (typeof require !== "undefined" && require.main === module) {
  const path = require("node:path");
  const n = parseInt(process.argv[2], 10) || 0;
  const offset = parseInt(process.argv[3], 10) || 0;
  loadFib(path.join(__dirname, "fib.wasm"), () => offset)
    .then(exports => console.log(`fib(${n}) = ${exports.fib(n)}`));
}
