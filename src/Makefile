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

GRALOBJS=ast.uo parser.uo lexer.uo line.uo astpp.uo table.uo escape.uo \
	trans.uo tentry.uo ventry.uo temp.uo topsort.uo seman.uo typepp.uo \
	util.uo error.uo frame.uo tree.uo treepp.uo stack.uo canon.uo \
	interp.uo assem.uo codegen.uo main.uo

all: tiger

tiger: $(GRALOBJS)
	$(MOSMLL) -o tiger main.uo

test: $(GRALOBJS) test.uo
	$(MOSMLL) -o test test.uo

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

depend: ast.sml parser.sml lexer.sml line.sml astpp.sml table.sml escape.sml \
	trans.sml tentry.sml ventry.sml temp.sml topsort.sml seman.sml typepp.sml \
	util.sml error.sml frame.sml tree.sml treepp.sml stack.sml canon.sml \
	interp.sml assem.sml codegen.sml main.sml test.sml
	$(REMOVE) Makefile.bak
	$(MOVE) Makefile Makefile.bak
	$(MOSMLTOOLS)/cutdeps < Makefile.bak > Makefile
	$(MOSMLTOOLS)/mosmldep >> Makefile

### DO NOT DELETE THIS LINE
temp.uo: temp.ui 
interp.uo: interp.ui table.ui temp.ui frame.ui treepp.uo tree.uo 
tree.uo: temp.ui 
util.uo: util.ui ast.uo 
escape.uo: escape.ui table.ui ast.uo 
assem.uo: temp.ui 
topsort.ui: table.ui tentry.uo ast.uo 
frame.uo: frame.ui temp.ui tree.uo 
parser.uo: parser.ui line.uo ast.uo 
seman.uo: seman.ui table.ui trans.ui typepp.ui tentry.uo topsort.ui \
    ventry.uo ast.uo error.uo stack.ui util.ui 
astpp.uo: astpp.ui ast.uo 
canon.ui: temp.ui tree.uo 
frame.ui: temp.ui tree.uo 
lexer.uo: parser.ui line.uo 
canon.uo: canon.ui table.ui temp.ui tree.uo 
test.uo: parser.ui astpp.ui seman.ui lexer.uo line.uo ast.uo error.uo \
    escape.ui 
trans.ui: temp.ui frame.ui ast.uo tree.uo 
codegen.uo: codegen.ui temp.ui frame.ui assem.uo tree.uo 
escape.ui: ast.uo 
ventry.uo: table.ui trans.ui tentry.uo temp.ui ast.uo 
astpp.ui: ast.uo 
main.uo: parser.ui trans.ui interp.ui codegen.ui astpp.ui seman.ui lexer.uo \
    line.uo frame.ui error.uo ast.uo assem.uo tree.uo escape.ui 
typepp.uo: typepp.ui table.ui tentry.uo 
seman.ui: ast.uo 
util.ui: ast.uo 
codegen.ui: frame.ui assem.uo tree.uo 
regalloc.ui: frame.ui assem.uo 
treepp.uo: table.ui tree.uo 
table.uo: table.ui 
stack.uo: stack.ui 
parser.ui: ast.uo 
trans.uo: trans.ui temp.ui frame.ui ast.uo canon.ui treepp.uo tree.uo \
    stack.ui 
typepp.ui: table.ui tentry.uo 
regalloc.uo: regalloc.ui frame.ui assem.uo 
interp.ui: temp.ui frame.ui tree.uo 
topsort.uo: topsort.ui table.ui tentry.uo ast.uo util.ui 
