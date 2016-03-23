NIM=Nim
NIMC=$(NIM)/bin/nim

NLVMC=nlvm/nlvm

.PHONY: all
all: $(NLVMC)

$(NLVMC): $(NIM)/compiler/*.nim  nlvm/*.nim llvm/*.nim
	cd nlvm && ../$(NIMC) c nlvm

$(NIM)/koch:
	cd $(NIM) && ./bootstrap.sh && $(NIMC) c koch

$(NIMC): $(NIM)/koch $(NIM)/compiler/*.nim
	cd $(NIM) && ./koch boot -d:release

nlvm/nimcache/nlvm.bc: $(NLVMC) nlvm/*.nim llvm/*.nim
	cd nlvm && ./nlvm c nlvm

nlvm/nimcache/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim
	llvm-dis nlvm/nimcache/nlvm.bc

nlvm/nlvm.self: $(NLVMC) nlvm/nimcache/nlvm.bc lib/*.ll
	clang -g -pthread lib/*.ll nlvm/nimcache/nlvm.bc  -ldl -lm -lpcre -lLLVM-3.7 -L ../llvm-3.7.1.src/build/Debug+Asserts/lib -v -Xlinker '-rpath=$$ORIGIN/../../llvm-3.7.1.src/build/Debug+Asserts/lib' -o nlvm/nlvm.self

nlvm/nimcache/nlvm.self.bc: nlvm/nlvm.self
	cd nlvm && ./nlvm.self c nlvm && mv nimcache/nlvm.bc nimcache/nlvm.self.bc

nlvm/nimcache/nlvm.self.ll: nlvm/nimcache/nlvm.self.bc
	llvm-dis nlvm/nimcache/nlvm.self.bc

.PHONE: compare
compare: nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll
	diff -u nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll

.PHONY: self
self: nlvm/nlvm.self
