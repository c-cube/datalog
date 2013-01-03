CLI = datalog_cli.native
LIB = lib.cmxa

all: prod

prod:
	ocamlbuild -tag noassert $(CLI) $(LIB)

debug:
	ocamlbuild -tag debug $(CLI)

profile:
	ocamlbuild -tag profile $(CLI)

clean:
	ocamlbuild -clean

