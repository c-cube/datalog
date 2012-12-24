all: tests datalog

datalog:
	ocamlbuild -tag noassert datalog.native

debug:
	ocamlbuild -tag debug datalog.native

profile:
	ocamlbuild -tag profile datalog.native

tests: datalog
	ocamlbuild -I . tests/run.native

clean:
	ocamlbuild -clean

