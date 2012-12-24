% generate problem of size 10
p(n1) :- p(n0), q(n1).
q(n1) :- p(n0), q(n0).
p(n2) :- p(n1), q(n2).
q(n2) :- p(n1), q(n1).
p(n3) :- p(n2), q(n3).
q(n3) :- p(n2), q(n2).
p(n4) :- p(n3), q(n4).
q(n4) :- p(n3), q(n3).
p(n5) :- p(n4), q(n5).
q(n5) :- p(n4), q(n4).
p(n6) :- p(n5), q(n6).
q(n6) :- p(n5), q(n5).
p(n7) :- p(n6), q(n7).
q(n7) :- p(n6), q(n6).
p(n8) :- p(n7), q(n8).
q(n8) :- p(n7), q(n7).
p(n9) :- p(n8), q(n9).
q(n9) :- p(n8), q(n8).
p(n10) :- p(n9), q(n10).
q(n10) :- p(n9), q(n9).
p(n0).
q(n0).
