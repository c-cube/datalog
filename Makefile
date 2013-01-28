# where to install the command line?
BINDIR ?= /usr/bin/

# name of the library
NAME = datalog

CLI = datalog_cli.native
LIB = datalog.cmxa datalog.cma
INSTALL_LIB = datalog.cmxa datalog.cma datalog.cmi datalog.mli

# compilation options
OPTIONS ?= -classic-display

all: prod

prod:
	ocamlbuild $(OPTIONS) -tag noassert $(LIB)
	ocamlbuild $(OPTIONS) -tag noassert -I _build/datalog/ $(CLI)

debug:
	ocamlbuild $(OPTIONS) -tag debug $(LIB) $(CLI)
	ocamlbuild $(OPTIONS) -tag debug _build/$(LIB) $(CLI)

profile:
	ocamlbuild $(OPTIONS) -tag profile $(LIB) $(CLI)
	ocamlbuild $(OPTIONS) -tag profile _build/$(LIB) $(CLI)

clean:
	ocamlbuild -clean

install: prod
	ocamlfind install $(NAME) META $(addprefix _build/,$(INSTALL_LIB))
	cp _build/$(CLI) $(BINDIR)/datalog_cli

.PHONY: all clean install
