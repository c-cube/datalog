NAME = datalog
CLI = datalog_cli.native
LIB = datalog.cmxa datalog.cma datalog.a datalog.cmi
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

.PHONY: all clean install
