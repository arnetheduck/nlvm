NIMC=Nim/bin/nim

NLVMC=nlvm/nlvm

LLVMPATH=../ext

#NIMFLAGS=--opt:speed --gc:markandsweep
#NIMFLAGS=-d:release
NIMFLAGS=--debuginfo --linedir:on

NLVMFLAGS=--gc:markandsweep --debuginfo --linedir:on

LLVMLIB=LLVM-7

LLVMLIBS="-l:-l$(LLVMLIB)" "--clibdir:$(LLVMPATH)"  "-l:-Xlinker '-rpath=\$$ORIGIN/$(LLVMPATH)'"

ifeq (,$(wildcard ext/lib$(LLVMLIB).so))
    $(error run make-llvm.sh before trying to build nlvm)
endif

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
	cd nlvm && time ../$(NIMC) $(NIMFLAGS) $(LLVMLIBS) c nlvm

nlvm/nimcache/nlvm.ll: $(NLVMC) nlvm/*.nim llvm/*.nim
	cd nlvm && time ./nlvm $(NLVMFLAGS) -o:nimcache/nlvm.ll -c c nlvm

nlvm/nlvm.self: $(NLVMC)
	cd nlvm && time ./nlvm -o:nlvm.self $(NLVMFLAGS) $(LLVMLIBS) c nlvm

nlvm/nimcache/nlvm.self.ll: nlvm/nlvm.self
	cd nlvm && time ./nlvm.self -c $(NLVMFLAGS) -o:nimcache/nlvm.self.ll c nlvm

.PHONY: compare
compare: nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll
	diff -u nlvm/nimcache/nlvm.self.ll nlvm/nimcache/nlvm.ll

Nim/testament/tester: $(NIMC) Nim/testament/*.nim
	cd Nim && bin/nim -d:release c testament/tester

.PHONY: run-tester
run-tester: Nim/testament/tester $(NLVMC)
	cd Nim && time testament/tester --targets:c "--nim:../nlvm/nlvm " all

.PHONY: move-results
move-results:
	rm -rf testresults
	[ -d Nim/testresults ] && mv Nim/testresults .

.PHONY: test
test: move-results run-tester stats
	jq -s '{bad: ([.[][]|select(.result != "reSuccess" and .result != "reIgnored")]) | length, ok: ([.[][]|select(.result == "reSuccess")]|length)}' Nim/testresults/*json

.PHONY: badeggs.json
badeggs.json:
	jq -s '[.[][]|select(.result != "reSuccess" and .result != "reIgnored")]' Nim/testresults/*.json > badeggs.json

.PHONY: stats
stats: badeggs.json
	jq -s '{bad: ([.[][]|select(.result != "reSuccess" and .result != "reIgnored")]) | length, ok: ([.[][]|select(.result == "reSuccess")]|length)}' Nim/testresults/*json
	jq 'group_by(.category)|.[]|((unique_by(.category)|.[].category) + " " + (length| tostring))' badeggs.json

.PHONY: t2
t2:
	cp -r Nim/testresults Nim/tr2

.PHONY: self
self: nlvm/nlvm.self

.PHONY: clean
clean:
	rm -rf $(NLVMC) nlvm/nimcache nlvm/nlvm.self Nim/testament/tester

