NIMC=Nim/bin/nim

NLVMC=nlvm/nlvm

LLVMPATH=ext

#NIMFLAGS=--opt:speed --gc:markandsweep
#NIMFLAGS=-d:release
NIMFLAGS=--opt:speed

NLVMFLAGS=--opt:speed --gc:markandsweep

LLVMLIBS="-l:-lLLVM-3.9" "--clibdir:$(LLVMPATH)"  "-l:-Xlinker '-rpath=$(LLVMPATH)'"

.PHONY: all
all: $(NLVMC)

Nim/koch:
	cd Nim ;\
	[[ -d csources ]] || git clone --depth 1 https://github.com/nim-lang/csources.git ;\
	cd csources ;\
	git pull ;\
	sh build.sh
	cd Nim ; bin/nim c koch

$(NIMC): Nim/koch Nim/compiler/*.nim
	cd Nim && ./koch boot -d:release

$(NLVMC): $(NIMC) Nim/compiler/*.nim  nlvm/*.nim llvm/*.nim
	cd nlvm && time ../$(NIMC) --debuginfo $(NIMFLAGS) $(LLVMLIBS) c nlvm

nlvm/nimcache/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim
	cd nlvm && time ./nlvm $(NLVMFLAGS) -o:nimcache/nlvm.ll -c c nlvm

nlvm/nlvm.self: $(NLVMC)
	cd nlvm && time ./nlvm -o:nlvm.self $(NLVMFLAGS) $(LLVMLIBS) c nlvm

nlvm/nimcache/nlvm.self.ll: nlvm/nlvm.self
	cd nlvm && time ./nlvm.self -c $(NLVMFLAGS) -o:nimcache/nlvm.self.ll c nlvm

.PHONY: compare
compare: nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll
	diff -u nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll

Nim/tests/testament/tester: $(NIMC) Nim/tests/testament/*.nim
	cd Nim && bin/nim -d:release c tests/testament/tester

.PHONY: test
test: Nim/tests/testament/tester $(NLVMC)
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
