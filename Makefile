NIM=Nim
NIMC=$(NIM)/bin/nim

NLVMC=nlvm/nlvm

.PHONY: all
all: $(NLVMC)

$(NLVMC): $(NIMC) nlvm/*.nim llvm/*.nim
	cd nlvm && ../$(NIMC) c nlvm

$(NIMC): $(NIM)/compiler/*.nim
	cd $(NIM) && ./bootstrap.sh

nlvm/nimcache/nlvm.bc: $(NIMC) nlvm/*.nim llvm/*.nim
	cd nlvm && ./nlvm c nlvm

nlvm/nlvm.self: nlvm/nimcache/nlvm.bc lib/*.ll
	clang -g -pthread lib/*.ll nlvm/nimcache/nlvm.bc  -ldl -lm -lpcre -lLLVM-3.7 -L ../llvm-3.7.1.src/build/Debug+Asserts/lib -o nlvm/nlvm.self

.PHONY: self
self: nlvm/nlvm.self
