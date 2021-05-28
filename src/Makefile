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

GRALOBJS=ast.uo parser.uo lexer.uo line.uo pretty.uo table.uo escape.uo \
	main.uo

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

depend: ast.sml parser.sml lexer.sml line.sml pretty.sml table.sml escape.sml \
	main.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
escape.uo: escape.ui table.ui ast.uo 
parser.uo: parser.ui line.uo ast.uo 
lexer.uo: parser.ui line.uo 
entry.uo: table.ui ast.uo typ.uo 
escape.ui: ast.uo 
main.uo: pretty.uo parser.ui lexer.uo line.uo escape.ui 
pretty.uo: ast.uo 
table.uo: table.ui 
parser.ui: ast.uo 
