NIMC=Nim/bin/nim

NLVMC=nlvm/nlvm
NLVMR=nlvm/nlvmr

LLVMPATH=../ext

#NIMFLAGS=--opt:speed --gc:markandsweep
#NIMFLAGS=-d:release
NIMFLAGS=--debuginfo --linedir:on

NLVMFLAGS= --debuginfo --linedir:on

LLVM_MAJ=10
LLVM_MIN=0
LLVM_PAT=0

LLVM_DIR=llvm-$(LLVM_MAJ).$(LLVM_MIN).$(LLVM_PAT).src

ifdef STATIC_LLVM
	NLVMCFLAGS=-d:staticLLVM --dynliboverrideall
	LLVM_DEP=ext/$(LLVM_DIR)/sta/bin/llvm-config
	export PATH := $(PWD)/ext/$(LLVM_DIR)/sta/bin:$(PATH)
else
	LLVM_DEP=ext/$(LLVM_DIR)/sha/lib/libLLVM-$(LLVM_MAJ).so
	NLVMCFLAGS?=
endif

.PHONY: all
all: $(NLVMC)

Nim/koch:
	cd Nim ;\
	[ -d csources ] || git clone --depth 1 https://github.com/nim-lang/csources.git ;\
	cd csources ;\
	git pull ;\
	sh build.sh
	cd Nim ; bin/nim c koch

$(NIMC): Nim/koch Nim/compiler/*.nim
	cd Nim && ./koch boot -d:release

$(NLVMC): $(LLVM_DEP) $(NIMC) Nim/compiler/*.nim  nlvm/*.nim llvm/*.nim nlvm-lib/*.nim
	cd nlvm && time ../$(NIMC) $(NIMFLAGS) $(NLVMCFLAGS) c nlvm

$(NLVMR): $(LLVM_DEP) $(NIMC) Nim/compiler/*.nim  nlvm/*.nim llvm/*.nim nlvm-lib/*.nim
	cd nlvm && time ../$(NIMC) $(NIMFLAGS) -d:release $(NLVMCFLAGS) -o:nlvmr c nlvm

nlvm/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim nlvm-lib/*.nim
	cd nlvm && time ./nlvm $(NLVMFLAGS) -o:nlvm.ll -c c nlvm

nlvm/nlvm.self: $(NLVMC)
	cd nlvm && time ./nlvm -o:nlvm.self $(NLVMFLAGS) $(NLVMCFLAGS) c nlvm

nlvm/nlvm.self.ll: nlvm/nlvm.self
	cd nlvm && time ./nlvm.self -c $(NLVMFLAGS) $(NLVMCFLAGS) -o:nlvm.self.ll c nlvm

.PHONY: compare
compare: nlvm/nlvm.self.ll nlvm/nlvm.ll
	diff -u nlvm/nlvm.self.ll nlvm/nlvm.ll

Nim/testament/testament: $(NIMC) Nim/testament/*.nim
	$(NIMC) -d:release c Nim/testament/testament

.PHONY: run-testament
run-testament: $(NLVMR) Nim/testament/testament
	-cd Nim; time testament/testament --megatest:off --targets:c "--nim:../nlvm/nlvmr" --skipFrom:../skipped-tests.txt all

run-testament-noskip: $(NLVMR) Nim/testament/testament
	-cd Nim; time testament/testament --megatest:off --targets:c "--nim:../nlvm/nlvmr" all

.PHONY: test
test: run-testament stats
	-jq -s '{bad: ([.[][]|select(.result != "reSuccess" and .result != "reDisabled")]) | length, ok: ([.[][]|select(.result == "reSuccess")]|length)}' Nim/testresults/*json

update-skipped: run-testament-noskip stats
	-jq -s '{bad: ([.[][]|select(.result != "reSuccess" and .result != "reDisabled")]) | length, ok: ([.[][]|select(.result == "reSuccess")]|length)}' Nim/testresults/*json
	# Output suitable for sticking into skipped-tests.txt
	-jq -r -s '([.[][]|select(.result != "reSuccess" and .result != "reDisabled")]) | .[].name' Nim/testresults/*json | sed 's/ C.*//' | sort | uniq > skipped-tests.txt

.PHONY: badeggs.json
badeggs.json:
	-jq -s '[.[][]|select(.result != "reSuccess" and .result != "reDisabled")]' Nim/testresults/*.json > badeggs.json

.PHONY: stats
stats: badeggs.json
	-jq -s '{bad: ([.[][]|select(.result != "reSuccess" and .result != "reDisabled")]) | length, ok: ([.[][]|select(.result == "reSuccess")]|length)}' Nim/testresults/*json
	-jq 'group_by(.category)|.[]|((unique_by(.category)|.[].category) + " " + (length| tostring))' badeggs.json

.PHONY: t2
t2:
	cp -r Nim/testresults tr2

.PHONY: self
self: nlvm/nlvm.self

.PHONY: clean
clean:
	rm -rf $(NLVMC) $(NLVMR) nlvm/nlvm.ll nlvm/nlvm.self.ll nlvm/nlvm.self Nim/testresults/

ext/$(LLVM_DIR)/sha/lib/libLLVM-$(LLVM_MAJ).so:
	sh ./make-llvm.sh $(LLVM_MAJ) $(LLVM_MIN) $(LLVM_PAT) sha \
		-DLLVM_BUILD_LLVM_DYLIB=1 \
		-DLLVM_LINK_LLVM_DYLIB=1 \
		-DLLVM_ENABLE_ASSERTIONS=1 \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo

ext/$(LLVM_DIR)/sta/bin/llvm-config:
	sh ./dl-llvm.sh $(LLVM_MAJ) $(LLVM_MIN) $(LLVM_PAT) sta

.PHONY: docker
docker:
	docker build . -t nlvm --no-cache
