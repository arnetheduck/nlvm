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
LLVM_DLL := llvm/sha/bin/libLLVM-$(LLVM_MAJ).dll
STATIC_OPT := -DLLVM_BUILD_STATIC=1
else
EXE :=
LLVM_DLL := llvm/sha/lib/libLLVM.so.$(LLVM_MAJ).$(LLVM_MIN)

# Fully static compilation of `nlvm` itself not supported (yet? patches welcome)
STATIC_OPT :=
endif

NIMC=Nim/bin/nim$(EXE)
NLVMC=nlvm/nlvm$(EXE)
NLVMR=nlvm/nlvmr$(EXE)

ifdef STATIC_LLVM
	NLVMCFLAGS=-d:staticLLVM
	LLVM_DEP=llvm/sta/bin/llvm-config$(EXE)
	export PATH := $(PWD)/llvm/sta/bin:$(PATH)
else
	LLVM_DEP:=$(LLVM_DLL)
	NLVMCFLAGS?=
endif

.PHONY: all
all: $(NLVMC)

Nim/koch$(EXE):
	cd Nim ;\
	[ -d csources_v2 ] || git clone -q --depth 1 -b master https://github.com/nim-lang/csources_v2.git ;\
	cd csources_v2 ;\
	git pull ;\
	$(MAKE) -f makefile
	cd Nim ; bin/nim c koch

$(NIMC): Nim/koch$(EXE) Nim/compiler/*.nim
	cd Nim && ./koch boot -d:release

$(NLVMC): $(LLVM_DEP) $(NIMC) Nim/compiler/*.nim  nlvm/*.nim llvm/*.nim nlvm-lib/*.nim
	cd nlvm && time ../$(NIMC) $(NIMFLAGS) $(NLVMCFLAGS) c nlvm

$(NLVMR): $(LLVM_DEP) $(NIMC) Nim/compiler/*.nim  nlvm/*.nim llvm/*.nim nlvm-lib/*.nim
	cd nlvm && time ../$(NIMC) $(NIMFLAGS) -d:release $(NLVMCFLAGS) -o:nlvmr$(EXE) c nlvm

nlvm/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim nlvm-lib/*.nim
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

Nim/testament/testament$(EXE): $(NIMC) Nim/testament/*.nim
	$(NIMC) -d:release c Nim/testament/testament

.PHONY: run-testament run-testament-noskip
run-testament: $(NLVMR) Nim/testament/testament
	cd Nim; time testament/testament --megatest:off --targets:c "--nim:../nlvm/nlvmr" --skipFrom:../skipped-tests.txt all

run-testament-noskip: $(NLVMR) Nim/testament/testament
	-cd Nim; time testament/testament --megatest:off --targets:c "--nim:../nlvm/nlvmr" all

.PHONY: test
test: run-testament
	-make stats

update-skipped: run-testament-noskip
	# Output suitable for sticking into skipped-tests.txt
	-jq -r -s '([.[][]|select(.result != "reSuccess" and .result != "reDisabled")]) | .[].name' Nim/testresults/*json | sort | uniq > skipped-tests.txt
	make stats

.PHONY: badeggs.json
badeggs.json:
	-jq -s '[.[][]|select(.result != "reSuccess" and .result != "reDisabled" and .result != "reCodeNotFound")]' Nim/testresults/*.json > badeggs.json

.PHONY: stats
stats: badeggs.json
	-jq 'group_by(.category)|.[]|((unique_by(.category)|.[].category) + " " + (length| tostring))' badeggs.json
	-jq -s '. | flatten | group_by(.result) | map({(first.result): (length)}) | add' Nim/testresults/*json
	-jq -s '{bad: ([.[][]|select(.result != "reSuccess" and .result != "reDisabled")]) | length, ok: ([.[][]|select(.result == "reSuccess")]|length)}' Nim/testresults/*json
.PHONY: t2
t2:
	cp -r Nim/testresults tr2

.PHONY: self
self: nlvm/nlvm.self

.PHONY: clean
clean:
	rm -rf $(NLVMC) $(NLVMR) nlvm/nlvm.ll nlvm/nlvm.self.ll nlvm/nlvm.self$(EXE) Nim/testresults/

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
	sh ./make-llvm.sh sta "lld-libraries lib/all llvm-config" \
		-DLLVM_BUILD_LLVM_DYLIB=0 \
		-DLLVM_LINK_LLVM_DYLIB=0 \
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
