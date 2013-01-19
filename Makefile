CLI = datalog_cli.native
LIB = datalog.cmxa
OPTIONS = -classic-display

all: prod

prod:
	ocamlbuild $(OPTIONS) -tag noassert $(LIB) $(CLI)

debug:
	ocamlbuild $(OPTIONS) -tag debug $(LIB) $(CLI)

profile:
	ocamlbuild $(OPTIONS) -tag profile $(LIB) $(CLI)

clean:
	ocamlbuild -clean

.PHONY: all clean
