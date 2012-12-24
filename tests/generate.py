#!/usr/bin/env python2

"""Generate a graph example of given size"""

import sys

def generate(size):
    print "reachable(X,Y) :- edge(X,Y)."
    print "reachable(X,Y) :- edge(X,Z), reachable(Z,Y)."
    for i in xrange(size):
        print "edge(e%d, e%d)." % (i, i+1)
    print "edge(e%d, e%d)." % (size, 0)

if __name__ == '__main__':
    size = int(sys.argv[1]) if len(sys.argv) > 1 else 10
    print "%% generate problem of size %d" % size
    generate(size)
