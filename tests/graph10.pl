% generate problem of size 10
reachable(X,Y) :- edge(X,Y).
reachable(X,Y) :- edge(X,Z), reachable(Z,Y).
increasing(X,Y) :- edge(X,Y), lt(X,Y).
increasing(X,Y) :- edge(X,Z), lt(X,Z), increasing(Z,Y).
edge(0, 1).
edge(1, 2).
edge(2, 3).
edge(3, 4).
edge(4, 5).
edge(5, 6).
edge(6, 7).
edge(7, 8).
edge(8, 9).
edge(9, 10).
edge(10, 0).
