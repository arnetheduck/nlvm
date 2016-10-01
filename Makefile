NIMC=Nim/bin/nim

NLVMC=nlvm/nlvm

LLVMPATH=$(shell realpath ../llvm-3.9.0.src/build/lib)

.PHONY: all
all: $(NLVMC)

Nim/koch:
	cd Nim && sh ./bootstrap.sh

$(NIMC): Nim/koch Nim/compiler/*.nim
	cd Nim && ./koch boot -d:release

$(NLVMC): $(NIMC) Nim/compiler/*.nim  nlvm/*.nim llvm/*.nim
	cd nlvm && time ../$(NIMC) --debuginfo c -d:release "-l:-lLLVM-3.9" "--clibdir:$(LLVMPATH)"  "-l:-Xlinker '-rpath=$(LLVMPATH)'" nlvm

nlvm/nimcache/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim
	cd nlvm && time ./nlvm -d:release -o:nimcache/nlvm.ll -c c nlvm

nlvm/nlvm.self: $(NLVMC)
	cd nlvm && time ./nlvm -o:nlvm.self "-l:-lLLVM-3.9" "--clibdir:$(LLVMPATH)" "-l:-Xlinker '-rpath=$(LLVMPATH)'" c nlvm

nlvm/nimcache/nlvm.self.ll: nlvm/nlvm.self
	cd nlvm && time ./nlvm.self -d:release -c -o:nimcache/nlvm.self.ll c nlvm

.PHONY: compare
compare: nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll
	diff -u nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll

Nim/tests/testament/tester: $(NIMC) Nim/tests/testament/*.nim
	cd Nim && bin/nim c tests/testament/tester

.PHONY: test
test: Nim/tests/testament/tester $(NIMC)
	cp compiler/nim Nim/compiler
	cd Nim && time tests/testament/tester --targets:c all
	cd Nim && tests/testament/tester html

.PHONY: t2
t2:
	cp Nim/testresults.json Nim/t2.json

.PHONY: self
self: nlvm/nlvm.self

.PHONY: clean
clean:
	rm -rf $(NLVMC) nlvm/nimcache/nlvm.ll nlvm/nlvm.self nlvm/nimcache/nlvm.self.ll Nim/tests/testament/tester
