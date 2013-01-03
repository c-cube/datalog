CLI = datalog_cli.native
LIB = datalog.cmxa
OPTIONS = -classic-display

all: prod

prod:
	ocamlbuild $(OPTIONS) -tag noassert $(CLI) $(LIB)

debug:
	ocamlbuild $(OPTIONS) -tag debug $(CLI)

profile:
	ocamlbuild $(OPTIONS) -tag profile $(CLI)

clean:
	ocamlbuild -clean

