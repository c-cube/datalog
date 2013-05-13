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

# constants
VERSION = 0.3.1

%.ml: %.mlp
	sed -e "s/DATALOG_VERSION/$(VERSION)/g" \
		 $< > $@

all: datalog/const.ml prod

prod:
	ocamlbuild $(OPTIONS) -tag noassert $(TARGETS)

debug:
	ocamlbuild $(OPTIONS) -tag debug $(TARGETS)

profile:
	ocamlbuild $(OPTIONS) -tag profile $(TARGETS)

clean:
	rm -f datalog/const.ml || true
	ocamlbuild -clean

install: prod
	ocamlfind install $(NAME) META $(addprefix _build/,$(INSTALL_LIB))
	cp _build/$(CLI) $(BINDIR)/datalog_cli

.PHONY: all clean install
