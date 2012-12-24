all: tests datalog

datalog:
	ocamlbuild datalog.native

tests: datalog
	ocamlbuild -I . tests/run.native

clean:
	ocamlbuild -clean

