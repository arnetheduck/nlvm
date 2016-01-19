NIM=Nim
NIMC=$(NIM)/bin/nim

NLVM=nlvm
NLVMC=$(NLVM)/nlvm

all: $(NLVMC)

$(NLVMC): $(NIMC) $(NLVM)/*.nim
	cd $(NLVM) && ../$(NIMC) c nlvm

$(NIMC): $(NIM)/compiler/*.nim
	cd $(NIM) && ./bootstrap.sh

.PHONY: all
