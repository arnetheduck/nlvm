#NIMFLAGS=--opt:speed --gc:markandsweep
#NIMFLAGS=-d:release
NIMFLAGS=--debuginfo --linedir:on --cc=clang

NLVMFLAGS= --debuginfo --linedir:on --cc=clang

LLVM_MAJ:=$(shell cat llvm/llvm.version | cut -f1 -d.)
LLVM_MIN:=$(shell cat llvm/llvm.version | cut -f2 -d.)
LLVM_PAT:=$(shell cat llvm/llvm.version | cut -f3 -d.)

# Extension for executables on Windows
ifeq ($(OS),Windows_NT)
EXE := .exe
SKIP := -win
LLVM_DLL := llvm/sha/bin/libLLVM-$(LLVM_MAJ).dll
STATIC_OPT := -DLLVM_BUILD_STATIC=1 -DLIBCLANG_BUILD_STATIC=1
# The shared build seems to have trouble with C++ - skip it for now (TODO)
STATIC_LLVM := 1
else
EXE :=
SKIP :=
LLVM_DLL := llvm/sha/lib/libLLVM.so.$(LLVM_MAJ).$(LLVM_MIN)

# Fully static compilation of `nlvm` itself not supported (yet? patches welcome)
STATIC_OPT :=
endif

NIMC=lib/nim/bin/nim$(EXE)
NLVMC=nlvm/nlvm$(EXE)
NLVMR=nlvm/nlvmr$(EXE)

ifdef STATIC_LLVM
	NLVMCFLAGS=-d:staticLLVM
	LLVM_OUT := llvm/sta
	LLVM_DEP := $(LLVM_OUT)/bin/llvm-config$(EXE)
else
	LLVM_OUT := llvm/sha
	LLVM_DEP := $(LLVM_DLL)
	NLVMCFLAGS?=
endif

# On windows, we expect to find the nim DLL dependencies in lib/nim
export PATH := $(PWD)/$(LLVM_OUT)/bin:$(PWD)/lib/nim:$(PATH)

.PHONY: all
all: $(NLVMC)

lib/nim/koch$(EXE): $(LLVM_DEP)
	cd lib/nim ;\
	[ -d csources_v3 ] || git clone -q --depth 1 -b master https://github.com/nim-lang/csources_v3.git ;\
	cd csources_v3 ;\
	git pull ;\
	CC=clang $(MAKE) -f makefile
	cd lib/nim ; bin/nim $(NIMFLAGS) c koch

$(NIMC): lib/nim/koch$(EXE) lib/nim/compiler/*.nim
	cd lib/nim && ./koch boot $(NIMFLAGS) -d:release --passC:-fPIC --passl:-fPIC

lib/clang/$(LLVM_MAJ)/include/stdint.h: $(LLVM_OUT)/lib/clang/$(LLVM_MAJ)/include/stdint.h
	rm -rf lib/clang/$(LLVM_MAJ)/include
	mkdir -p lib/clang/$(LLVM_MAJ)/
	cp -ar $(LLVM_OUT)/lib/clang/$(LLVM_MAJ)/include lib/clang/$(LLVM_MAJ)/

$(NLVMC): $(LLVM_DEP) $(NIMC) lib/nim/compiler/*.nim  nlvm/*.nim llvm/*.nim lib/nlvm/* lib/clang/$(LLVM_MAJ)/include/stdint.h
	cd nlvm && time ../$(NIMC) $(NIMFLAGS) $(NLVMCFLAGS) c nlvm

$(NLVMR): $(LLVM_DEP) $(NIMC) lib/nim/compiler/*.nim  nlvm/*.nim llvm/*.nim lib/nlvm/* lib/clang/$(LLVM_MAJ)/include/stdint.h
	cd nlvm && time ../$(NIMC) $(NIMFLAGS) -d:release $(NLVMCFLAGS) -o:nlvmr$(EXE) c nlvm

nlvm/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim lib/nlvm/*
	cd nlvm && time ./nlvm $(NLVMFLAGS) -o:nlvm.ll $(NLVMCFLAGS) -c c nlvm

nlvm/nlvm.self$(EXE): $(NLVMC)
	cd nlvm && time ./nlvm -o:nlvm.self$(EXE) $(NLVMFLAGS) $(NLVMCFLAGS) c nlvm

nlvm/nlvmr.self$(EXE): $(NLVMR)
	cd nlvm && time ./nlvmr -o:nlvmr.self$(EXE) -d:release $(NLVMFLAGS) $(NLVMCFLAGS) c nlvm

nlvm/nlvm.self.ll: nlvm/nlvm.self$(EXE)
	cd nlvm && time ./nlvm.self -c $(NLVMFLAGS) $(NLVMCFLAGS) -o:nlvm.self.ll c nlvm

.PHONY: compare
compare: nlvm/nlvm.self.ll nlvm/nlvm.ll
	diff -u nlvm/nlvm.self.ll nlvm/nlvm.ll

lib/nim/testament/testament$(EXE): $(NIMC) lib/nim/testament/*.nim
	$(NIMC) -d:release --cc:clang c lib/nim/testament/testament

.PHONY: run-testament run-testament-noskip
run-testament: $(NLVMR) lib/nim/testament/testament$(EXE)
	cd lib/nim; time testament/testament --megatest:off --targets:c "--nim:../../nlvm/nlvmr" --skipFrom:../../skipped-tests$(SKIP).txt all

run-testament-noskip: $(NLVMR) lib/nim/testament/testament$(EXE)
	-cd lib/nim; time testament/testament --megatest:off --targets:c "--nim:../../nlvm/nlvmr" all

.PHONY: test
test: run-testament
	@-make stats

# Output suitable for sticking into skipped-tests.txt (with classification comments)
# duplicate entries are merged by the classifier
update-skipped: run-testament-noskip
	@-jq -s '[.[][]|select(.result != "reSuccess" and .result != "reDisabled")]' lib/nim/testresults/*.json \
	  | jq -f classify-errors.jq \
	  | jq -r '.[] | "\(.name) # \(.classification) / \(.result)"' | sort > skipped-tests$(SKIP).txt
	@-make stats

.PHONY: badeggs.json
badeggs.json:
	@-jq -s '[.[][]|select(.result != "reSuccess" and .result != "reDisabled" and .result != "reCodeNotFound")]' lib/nim/testresults/*.json | jq -f classify-errors.jq > badeggs.json

.PHONY: stats
stats: badeggs.json
	@-jq 'group_by(.classification) | map({(first.classification): length}) | add' badeggs.json
	@-jq -s '. | flatten | unique_by(.name) | group_by(.result) | map({(first.result): (length)}) | add' lib/nim/testresults/*json
	@-jq -s '. | flatten | unique_by(.name) | {bad: ([.[] | select(.result != "reSuccess" and .result != "reDisabled")] | length), ok: ([.[] | select(.result == "reSuccess")] | length)}' lib/nim/testresults/*json
.PHONY: t2
t2:
	cp -r lib/nim/testresults tr2

.PHONY: self
self: nlvm/nlvm.self

.PHONY: clean
clean:
	rm -rf $(NLVMC) $(NLVMR) nlvm/nlvm.ll nlvm/nlvm.self.ll nlvm/nlvm.self$(EXE) lib/nim/testresults/

# developer build - build all of llvm including tooling like IR inspectors etc
# for the right version of LLVM
$(LLVM_DLL):
	sh ./make-llvm.sh sha "" \
		-DLLVM_BUILD_LLVM_DYLIB=1 \
		-DLLVM_LINK_LLVM_DYLIB=1 \
		-DLLVM_ENABLE_ASSERTIONS=1 \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo

# We only need a subset of the build in CI / statically linked release builds
llvm/sta/bin/llvm-config$(EXE):
	sh ./make-llvm.sh sta "clang-libraries lld-libraries llvm-libraries llvm-config" \
		-DLLVM_BUILD_LLVM_DYLIB=0 \
		-DLLVM_LINK_LLVM_DYLIB=0 \
		-DLIBCLANG_BUILD_STATIC=On \
		$(STATIC_OPT) \
		-DLLVM_ENABLE_ASSERTIONS=0 \
		-DCMAKE_BUILD_TYPE=Release

.PHONY: prepare-llvm
prepare-llvm: $(LLVM_DEP)

.PHONY: prepare-nim
prepare-nim: $(NIMC)

.PHONY: docker
docker:
	docker build . -t nlvm --no-cache
