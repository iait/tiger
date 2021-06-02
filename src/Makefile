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

GRALOBJS=ast.uo parser.uo lexer.uo line.uo printast.uo table.uo escape.uo \
	translate.uo typ.uo entry.uo temp.uo main.uo

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

depend: ast.sml parser.sml lexer.sml line.sml printast.sml table.sml escape.sml \
	translate.sml typ.sml entry.sml temp.sml main.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
temp.uo: temp.ui 
printast.ui: ast.uo 
printtyp.ui: table.ui 
util.uo: util.ui 
escape.uo: escape.ui table.ui ast.uo 
topsort.ui: table.ui ast.uo typ.uo 
parser.uo: parser.ui line.uo ast.uo 
seman.uo: seman.ui table.ui translate.ui entry.uo ast.uo 
printtyp.uo: printtyp.ui table.ui typ.uo 
lexer.uo: parser.ui line.uo 
entry.uo: table.ui temp.ui ast.uo typ.uo 
escape.ui: ast.uo 
printast.uo: printast.ui ast.uo 
main.uo: parser.ui printast.ui lexer.uo line.uo escape.ui 
seman.ui: ast.uo 
table.uo: table.ui 
translate.uo: translate.ui 
parser.ui: ast.uo 
topsort.uo: topsort.ui 
