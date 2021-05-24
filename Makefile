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
	$(MOVE) src/tiger tiger

clean:
	$(CHANGE) src; $(MAKE) clean
	$(REMOVE) tiger
