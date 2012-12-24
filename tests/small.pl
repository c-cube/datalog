% small example file

mother(claudette, ann).
mother(jeannette, bill).
father(john, ann).
father(john, bill).
father('jean-jacques', alphonse).

parent(X,Y) :- mother(X,Y).
parent(X,Y) :- father(X,Y).

ancestor(X,Y) :- parent(X,Y).
ancestor(X,Y) :- parent(X,Z), ancestor(Z,Y).
