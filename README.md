## Datalog

An in-memory datalog implementation for OCaml. It focuses on big sets of rules
with small relations, with frequent updates of the relations. Therefore, it
tries to achieve good behavior in presence of incremental modifications of the
relations.

This version, `backward`, features a backward-chaining operation. It resembles
top-down algorithms because goals (possibly non-ground literals) can be
added to the `db`; it means that if `G` is a goal and `A :- B1,B2,...,Bn` is a clause,
if `A` and `B1` are unifiable with `subst`, then `subst(B1)` is also a goal.
Handlers (semantic attachments) can be provided by the user, to be called on
every goal. The point is that the handlers can add facts that **solve** the
goal by adding facts that match it.

For instance, a handler may solve goals of the form `lt(i,j)` (where
`i` and `j` are integers) by adding the fact `lt(i,j)` if `i < j` is
really true. Another example: if symbols are strings, then the goal
`concat("foo", "bar", X)` may be solved by adding the fact
`concat("foo", "bar", "foobar")`. The tool `datalog_cli` has build-in
definitions of `lt`, `le` (lower or equal) and `equal`; see the last example.
Thus, goals are a way to call semantic attachments in a goal-oriented way.

## License

The code is distributed under the [BSD license](http://opensource.org/licenses/BSD-2-Clause).
See the `LICENSE` file.

## Build

You need OCaml >= 3.12 with ocamlbuild. Just type in the root directory:

    $ git submodule update --init
    $ make

Then, you can install the library and the command line tool, `datalog_cli`,
by typing:

    $ sudo make install

## How to use it

There are two ways to use `datalog`:

- With the command line tool, `datalog_cli.native`, or `datalog_cli` if you
installed it on your system; just type in

    $ datalog_cli [problem_file]

- The library, that should be in `_build/datalog.a`. It is also registered to
  OCamlfind (in the `datalog` subdirectory). It exports a `Datalog`module, the
  interface of which is described in `datalog.mli`. A functor is provided
  in `Datalog.Logic.Make` to use your own datatype for symbols (constants);
  however, a default implementation with strings as symbols is available as
  `Datalog.Logic.Default` (which is used by the parser `Datalog.Parser`).

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

    $ datalog_cli tests/clique10.pl -pattern 'same_clique(1,X)' 
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

    $ datalog_cli tests/graph200.pl -size -sum reachable
    % start datalog
    % parse file tests/graph200.pl
    % process 203 rules
    % computing fixpoint...
    % done.
    % size of saturated set: 40805
    % number of fact with head reachable: 40401
    % max_heap_size: 1777664; minor_collections: 38; major collections: 9

Or

    $ datalog_cli tests/graph10.pl -goal 'increasing(3,7)' -pattern 'increasing(3,X)'
    % start datalog
    % parse file tests/graph10.pl
    % process 15 clauses
    % computing fixpoint...
    % done.
    % facts matching pattern increasing(3, X1):
    increasing(3, 5).
    increasing(3, 6).
    increasing(3, 8).
    increasing(3, 7).
    increasing(3, 4).
    increasing(3, 9).
    increasing(3, 10).
    % max_heap_size: 126976; minor_collections: 0; major collections: 0

## TODOs/ideas

- Use some basic rewriting for efficient **ground** equality (with a union-find)

- Goal subsumption
- Clause subsumption (when selected lit is ground)
- Clause retraction
- Library of standard interpreted predicates
- Substitution trees to avoid intermediate clauses (n-ary resolution) with
generic, functorial interface
- quoting mechanism?
