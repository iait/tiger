# Makefile for tiger compiler

# Unix
REMOVE=rm -f
MOVE=mv
CHANGE=cd
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
	$(REMOVE) tiger
	$(REMOVE) tiger-test
