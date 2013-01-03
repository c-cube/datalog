## Datalog

An in-memory datalog implementation for OCaml. It focuses on big sets of rules
with small relations, with frequent updates of the relations. Therefore, it
tries to achieve good behavior in presence of incremental modifications of the
relations.

## License

The code is distributed under the [BSD license](http://opensource.org/licenses/BSD-2-Clause).
See the `LICENSE` file.

## Build

You need OCaml >= 3.12 with ocamlbuild. Just type in

    make

## How to use it

There are two ways to use `datalog`:

- With the command line tool, `datalog_cli.native`; just type in

    ./datalog_cli.native [problem_file]

A few example files, suffixed with `.pl`, can be found in `tests/`.

- The library, that should be in `_build/lib.a`. It exports a `Datalog` module.
