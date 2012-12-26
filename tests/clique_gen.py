#!/usr/bin/env python2

"""Generate a graph example of given size. It produces a cyclic graph
with vertices in [0...size-1] and edges from i to i+1 mod size. The
single rule computes a transitive closure of the graph, the predicate
reachable() describes a clique of size size.
"""

import sys

def generate(size):
    print "reachable(X,Y) :- edge(X,Y)."
    print "reachable(X,Y) :- edge(X,Z), reachable(Z,Y)."
    print "same_clique(X,Y) :- reachable(X,Y), reachable(Y,X)."
    for i in xrange(size):
        print "edge(%d, %d)." % (i, i+1)
    print "edge(%d, %d)." % (size, 0)

if __name__ == '__main__':
    size = int(sys.argv[1]) if len(sys.argv) > 1 else 10
    print "%% generate problem of size %d" % size
    generate(size)
