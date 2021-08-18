# Makefile for tiger compiler

# Unix
REMOVE=rm -f
MOVE=mv
CHANGE=cd
FIND=find
MAKE=make

.PHONY: clean
.PHONY: all

all:
	$(CHANGE) src; $(MAKE) depend
	$(CHANGE) src; $(MAKE) tiger
	$(CHANGE) src; $(MAKE) test
	$(MOVE) src/tiger tiger
	$(MOVE) src/test tiger-test

clean:
	$(CHANGE) src; $(MAKE) clean
	$(FIND) test/ -type f -not -name '*.tig' -delete
	$(REMOVE) tiger
	$(REMOVE) tiger-test
