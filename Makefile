# where to install the command line?
BINDIR ?= /usr/bin/

# name of the library
NAME = datalog

CLI = datalog_cli.native
LIB = datalog.cmxa datalog.cma
DOC = datalog.docdir/index.html
TARGETS = $(DOC) $(LIB) $(CLI)

INSTALL_LIB = datalog.cmxa datalog.cma datalog.a datalog.cmi datalog.mli

# compilation options
OPTIONS ?= -classic-display

all: prod

prod: $(SUBMODULES) 
	ocamlbuild $(OPTIONS) -tag noassert $(TARGETS)

debug: $(SUBMODULES) 
	ocamlbuild $(OPTIONS) -tag debug $(TARGETS)

profile: $(SUBMODULES) 
	ocamlbuild $(OPTIONS) -tag profile $(TARGETS)

clean:
	ocamlbuild -clean

install: prod
	ocamlfind install $(NAME) META $(addprefix _build/,$(INSTALL_LIB))
	cp _build/$(CLI) $(BINDIR)/datalog_cli

.PHONY: all clean install
