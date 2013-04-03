# where to install the command line?
BINDIR ?= /usr/bin/

# name of the library
NAME = datalog

CLI = datalog_cli.native
LIB = datalog.cmxa datalog.cma
DOC = datalog.docdir/index.html
TARGETS = $(DOC) $(LIB) $(CLI)
SUBMODULES = containers logic-terms

INSTALL_LIB = datalog.cmxa datalog.cma datalog.a datalog.cmi datalog.mli

# compilation options
OPTIONS ?= -classic-display

all: prod

tests: submodules
	ocamlbuild -use-ocamlfind -I datalog -package oUnit tests/run_tests.native

prod: submodules
	ocamlbuild $(OPTIONS) -tag noassert $(TARGETS)

debug: submodules
	ocamlbuild $(OPTIONS) -tag debug $(TARGETS)

profile: submodules
	ocamlbuild $(OPTIONS) -tag profile $(TARGETS)

clean:
	ocamlbuild -clean

install: prod
	ocamlfind install $(NAME) META $(addprefix _build/,$(INSTALL_LIB))
	cp _build/$(CLI) $(BINDIR)/datalog_cli

tags:
	otags **/*.ml{,i}

submodules: $(SUBMODULES)

containers:
	make -C containers

logic-terms:
	make -C logic-terms

.PHONY: all clean install tags tests submodules
