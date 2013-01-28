# where to install the command line?
BINDIR ?= /usr/bin/

# name of the library
NAME = datalog

CLI = datalog_cli.native
LIB = datalog.cmxa datalog.cma datalog.a datalog.cmi

# compilation options
OPTIONS ?= -classic-display

all: prod

prod:
	ocamlbuild $(OPTIONS) -tag noassert $(LIB) $(CLI)

debug:
	ocamlbuild $(OPTIONS) -tag debug $(LIB) $(CLI)

profile:
	ocamlbuild $(OPTIONS) -tag profile $(LIB) $(CLI)

clean:
	ocamlbuild -clean

install: prod
	ocamlfind install $(NAME) META $(addprefix _build/,$(LIB))
	cp _build/$(CLI) $(BINDIR)/datalog_cli

.PHONY: all clean install
