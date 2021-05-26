# Makefile for tiger compiler

# Moscow ML
MOSMLHOME=${HOME}/mosml
MOSMLTOOLS=camlrunm $(MOSMLHOME)/tools
MOSMLC=mosmlc -c -liberal
MOSMLL=mosmlc
MOSMLLEX=mosmllex
MOSMLYACC=mosmlyac -v

# C
GCC=gcc
CFLAGS=-g

# Unix
REMOVE=rm -f
MOVE=mv

.PHONY: clean
.PHONY: all

.SUFFIXES:
.SUFFIXES: .sig .sml .ui .uo

GRALOBJS=ast.uo parser.uo lexer.uo nlin.uo pretty.uo tabla.uo main.uo

all: tiger

tiger: $(GRALOBJS)
	$(MOSMLL) -o tiger main.uo

parser.sml parser.sig: parser.grm
	$(MOSMLYACC) parser.grm

lexer.sml: lexer.lex
	$(MOSMLLEX) lexer.lex

clean:
	$(REMOVE) Makefile.bak
	$(REMOVE) parser.output
	$(REMOVE) parser.sig
	$(REMOVE) parser.sml
	$(REMOVE) lexer.sml
	$(REMOVE) *.ui
	$(REMOVE) *.uo

.sig.ui:
	$(MOSMLC) $<

.sml.uo:
	$(MOSMLC) $<

depend: ast.sml parser.sml lexer.sml nlin.sml pretty.sml tabla.sml main.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
parser.uo: parser.ui nlin.uo ast.uo 
tabla.uo: tabla.ui 
lexer.uo: parser.ui nlin.uo 
main.uo: pretty.uo parser.ui nlin.uo lexer.uo 
pretty.uo: ast.uo 
parser.ui: ast.uo 
