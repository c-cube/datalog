% generate problem of size 10
reachable(X,Y) :- edge(X,Y).
reachable(X,Y) :- edge(X,Z), reachable(Z,Y).
edge(e0, e1).
edge(e1, e2).
edge(e2, e3).
edge(e3, e4).
edge(e4, e5).
edge(e5, e6).
edge(e6, e7).
edge(e7, e8).
edge(e8, e9).
edge(e9, e10).
edge(e10, e0).
