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

    $ make

## How to use it

There are two ways to use `datalog`:

- With the command line tool, `datalog_cli.native`; just type in

    $ ./datalog_cli.native [problem_file]

- The library, that should be in `_build/lib.a`. It exports a `Datalog` module; the most
important interface file is in `datalog/logic.mli`. The type for symbols is abstract, the library
provides a functor `Datalog.Logic.Make`, but a default implementation with strings as symbols
is available as `Datalog.Logic.Default`.

A few example files, suffixed with `.pl`, can be found in `tests/`. For instance, you
can try:

    $ cat tests/clique10.pl
    reachable(X,Y) :- edge(X,Y).
    reachable(X,Y) :- edge(X,Z), reachable(Z,Y).
    same_clique(X,Y) :- reachable(X,Y), reachable(Y,X).
    edge(0, 1).
    edge(1, 2).
    edge(2, 3).
    edge(3, 4).
    edge(4, 5).
    edge(5, 0).
    edge(5, 6).
    edge(6, 7).
    edge(7, 8).
    edge(8, 9).
    edge(9, 10).
    edge(10, 7).

    $ ./datalog_cli.native tests/clique10.pl -pattern 'same_clique(1,X)' 
    % start datalog
    % parse file tests/clique10.pl
    % process 15 rules
    % computing fixpoint...
    % done.
    % facts matching pattern same_clique(1, X1):
    same_clique(1, 0).
    same_clique(1, 1).
    same_clique(1, 3).
    same_clique(1, 2).
    same_clique(1, 5).
    same_clique(1, 4).
    % max_heap_size: 126976; minor_collections: 0; major collections: 0

Or

    $ ./datalog_cli.native tests/graph200.pl -size -sum reachable
    % start datalog
    % parse file tests/graph200.pl
    % process 203 rules
    % computing fixpoint...
    % done.
    % size of saturated set: 40805
    % number of fact with head reachable: 40401
    % max_heap_size: 1777664; minor_collections: 38; major collections: 9
