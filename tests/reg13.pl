% Test case for issue #13: Top down query has incorrect/differing behavior
% https://github.com/c-cube/datalog/issues/13

foo(a1, a3).
foo(a2, a3).

fooAlso(X, Y) :- foo(X, Y).

bar(a1, a2).

baz(a3, X) :- fooAlso(X, a3), fooAlso(Y, a3), bar(X, Y).

result(a3, X) :- fooAlso(X, a3), ~baz(a3, X).
