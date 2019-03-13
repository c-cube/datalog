# Datalog

[![build status](https://travis-ci.org/c-cube/datalog.svg?branch=master "build status")](https://travis-ci.org/c-cube/datalog)

An in-memory datalog implementation for OCaml.

It features two main algorithm:

- bottom-up focuses on big sets of rules with small relations, with frequent
  updates of the relations. Therefore, it tries to achieve good behavior in
  presence of incremental modifications of the relations.

- top-down resembles prolog (and allows nested subterms). It handles
  stratified negation and only explores the part of the search space that
  is relevant to a given query.

## Bottom-Up

This version, `backward`, features a backward-chaining operation. It resembles
top-down algorithms because goals (possibly non-ground literals) can be
added to the `db`; it means that if `G` is a goal and
`A :- B1,B2,...,Bn` is a clause,
if `A` and `B1` are unifiable with `subst`, then `subst(B1)` is also a goal.
Handlers (semantic attachments) can be provided by the user, to be called on
every goal. The point is that the handlers can add facts that **solve** the
goal by adding facts that match it.

For instance, a handler may solve goals of the form `lt_than(i,j)` (where
`i` and `j` are integers) by adding the fact `lt(i,j)` if `i < j` is
really true. Another example: if symbols are strings, then the goal
`concat("foo", "bar", X)` may be solved by adding the fact
`concat("foo", "bar", "foobar")`. The tool `datalog_cli` has build-in
definitions of `lt`, `le` (lower or equal) and `equal`; see the last example.
Thus, goals are a way to call semantic attachments in a goal-oriented way.

A relational query mode is available (its signature is in
`Datalog.BottomUp.S.Query`, see the
[module's documentation](http://c-cube.github.io/datalog/0.6/datalog/Datalog/BottomUp/module-type-S/Query/index.html).
It allows to make one-shot queries on a `db` (the result won't update
if facts or clauses are added later), with a simple relational model
with negation.

## Top-Down

There is also a top-down, prolog-like algorithm that should be very efficient
for querying only a subpart of the intensional database (the set of all
facts that can be deduced from rules). The main module is `Datalog_top_down`,
and it has its own parser and lexer. An executable (not installed but compiled)
is `topDownCli.exe`. A very important distinction is that terms
can be nested (hence the distinct AST and parsers).

The format of semantic attachments for symbols is simpler: a handler, when
queried with a given goal, can return a set of clauses whose heads will
then be unified with the goal.

## CamlInterface

The module `Datalog_caml_interface` in the library `datalog.caml_interface`
contains a universal embedding of OCaml's types,
with helpers to build unary, binary, and ternary atoms that directly relate
OCaml values.

Small example:

```ocaml
# #require "datalog";;
# #require "datalog.caml_interface";;
# module CI = Datalog_caml_interface;;
module CI = Datalog_caml_interface
# let edge = CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "edge";;
val edge : (int, int) CI.Rel2.t = <abstr>
# let db = CI.Logic.DB.create();;
val db : CI.Logic.DB.t = <abstr>
# CI.Rel2.symmetry db edge;;
- : unit = ()
# CI.Rel2.add_list db edge [1,2; 2,3; 3,4];;
- : unit = ()
# CI.Rel2.find db edge;;
- : (int * int) list = [(4, 3); (3, 2); (2, 1); (3, 4); (2, 3); (1, 2)]
```

The relation `edge` is really intensional: if we add axioms to it,
`CI.Rel2.find` will return an updated view.

```ocaml
# CI.Rel2.transitive db edge;;
- : unit = ()
# CI.Rel2.find db edge;;
- : (int * int) list =
[(1, 3); (2, 4); (1, 4); (4, 1); (3, 1); (4, 2); (4, 3); (3, 2); (2, 1);
 (1, 1); (3, 3); (4, 4); (2, 2); (3, 4); (2, 3); (1, 2)]
```

One can also directly load a Datalog file (atoms: ints and strings) and access
it using (properly typed) relations:

```ocaml
# let db = CI.Logic.DB.create ();;
val db : CI.Logic.DB.t = <abstr>
# CI.Parse.load_file db "tests/clique10.pl";;
- : bool = true
# let edge = CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "edge";;
val edge : (int, int) CI.Rel2.t = <abstr>
# let reachable = CI.Rel2.create ~k1:CI.Univ.int ~k2:CI.Univ.int "reachable";;
val reachable : (int, int) CI.Rel2.t = <abstr>
# CI.Rel2.find db reachable;;
- : (int * int) list =
[(5, 0); (5, 1); (4, 0); (5, 2); (4, 1); (3, 0); (10, 7); (5, 3); (10, 8);
 (9, 7); (4, 2); (3, 1); (2, 0); (5, 4); (10, 9); (9, 8); (8, 7); (4, 3);
 (3, 2); (2, 1); (1, 0); (5, 5); (10, 10); (9, 9); (8, 8); (7, 7); (4, 4);
 (3, 3); (2, 2); (1, 1); (0, 0); (0, 1); (1, 2); (2, 3); (3, 4); (4, 5);
 (5, 6); (6, 7); (7, 8); (8, 9); (9, 10); (8, 10); (7, 9); (6, 8); (5, 7);
 (4, 6); (3, 5); (2, 4); (1, 3); (0, 2); (7, 10); (6, 9); (5, 8); (4, 7);
 (3, 6); (2, 5); (1, 4); (0, 3); (6, 10); (5, 9); (4, 8); (3, 7); (2, 6);
 (1, 5); (0, 4); (5, 10); (4, 9); (3, 8); (2, 7); (1, 6); (0, 5); (4, 10);
 (3, 9); (2, 8); (1, 7); (0, 6); (3, 10); (2, 9); (1, 8); (0, 7); (2, 10);
 (1, 9); (0, 8); (1, 10); (0, 9); (0, 10)]
```

## Documentation

You can consult the [online documentation](https://c-cube.github.io/datalog)

## License

The code is distributed under the [bsd_license](http://opensource.org/licenses/BSD-2-Clause)
See the `LICENSE` file.

Build
=====

It is recommended to use opam: `opam install datalog`.

Manual build:

You need **OCaml >= 4.02** with [dune](https://github.com/ocaml/dune). Just type in the root directory:

```sh non-deterministic
$ make
```

## How to use it

There are two ways to use `datalog`:

- With the command line tool, `datalog_cli.exe`, or `datalog_cli` if you
  installed it on your system; just type in

```sh non-deterministic
$ datalog_cli <problem_file>
```

- The libraries `datalog`, `datalog.top_down`, `datalog.unix`, `datalog.caml_interface`.
  See the `.mli` files for documentation, or the online documentation.
  For both `Datalog_top_down` and `Datalog.BottomUp`, functors are
  provided to use your own datatype for symbols (constants);
  however, a default implementation with strings as symbols is available as
  `Datalog.Default` (which is used by the parser `Datalog.Parser`)
  for bottom-up and in `Datalog_top_down.Default` for top-down.

A few example files, suffixed with `.pl`, can be found in `tests/`. For instance, you
can try:

```sh
$ cat tests/clique10.pl
% generate problem of size 10
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
$ datalog_cli tests/clique10.pl --pattern 'same_clique(1,X)'
% start datalog
% parse file tests/clique10.pl
% process 15 clauses
% computing fixpoint...
% done.
% facts matching pattern same_clique(1, X0):
  same_clique(1, 4).
  same_clique(1, 5).
  same_clique(1, 3).
  same_clique(1, 2).
  same_clique(1, 1).
  same_clique(1, 0).
...
```

Or

```sh
$ datalog_cli tests/graph200.pl --size --sum reachable
% start datalog
% parse file tests/graph200.pl
% process 205 clauses
% computing fixpoint...
% done.
% size of saturated set: 41209
% number of fact with head reachable: 40401
...
```

Or

```sh
$ datalog_cli tests/graph10.pl --goal 'increasing(3,7)' --pattern 'increasing(3,X)'
% start datalog
% parse file tests/graph10.pl
% process 15 clauses
% computing fixpoint...
% done.
% facts matching pattern increasing(3, X0):
  increasing(3, 10).
  increasing(3, 7).
  increasing(3, 6).
  increasing(3, 4).
  increasing(3, 5).
  increasing(3, 8).
  increasing(3, 9).
...
```

Or

```sh
$ datalog_cli tests/small.pl --query '(X,Y) :- ancestor(X,john), father(X,Y), not mother(Y,Z)'
% start datalog
% parse file tests/small.pl
% process 12 clauses
% computing fixpoint...
% done.
% query plan: (match[0] ancestor(X0, john) |><| match[1,0] father(X0, X1)) |> match[2,1] mother(X1, X2)
% query answer:
    'jean-jacques', alphonse
    brad, john
...
```

## Aggregates in top-down:

```prolog
$ cat test.pl
foo(a, 1).
foo(a, 2).
foo(b, 10).
foo(b, 11).
foo(c, 0).

bar(A, S) :- S := sum B : foo(A, B).

$ topDownCli -load foo.pl -builtin '(X,Y) :- bar(X,Y)'
  (a, 3).
  (b, 21).
  (c, 0).
```

## TODOs/ideas

- Goal subsumption
- Clause subsumption (when selected lit is ground)
- Clause retraction
- Library of standard interpreted predicates
