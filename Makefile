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

nlvm/nimcache/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim
	cd nlvm && ./nlvm -o:nimcache/nlvm.ll -c c nlvm

nlvm/nlvm.self: $(NLVMC)
	cd nlvm && ./nlvm -o:nlvm.self "-l:-lLLVM-3.7" --clibdir:../../llvm-3.7.1.src/build/Debug+Asserts/lib "-l:-Xlinker '-rpath=\$$ORIGIN/../../llvm-3.7.1.src/build/Debug+Asserts/lib'" c nlvm

nlvm/nimcache/nlvm.self.ll: nlvm/nlvm.self
	cd nlvm && ./nlvm.self -c -o:nimcache/nlvm.self.ll c nlvm

.PHONY: compare
compare: nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll
	diff -u nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll

Nim/tests/testament/tester: $(NIMC) Nim/tests/testament/*.nim
	cd Nim && bin/nim c tests/testament/tester

.PHONY: test
test: Nim/tests/testament/tester $(NIMC)
	cp compiler/nim Nim/compiler
	cd Nim && tests/testament/tester --targets:c all
	cd Nim && tests/testament/tester html

.PHONY: self
self: nlvm/nlvm.self
