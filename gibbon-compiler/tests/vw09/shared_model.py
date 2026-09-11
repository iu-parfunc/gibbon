#!/usr/bin/env python3
"""Independent model for tests/vw09/SharedOwnership.hs.

The output's Int32 field is bumped by 3; every other field, and the whole
input, is unchanged.  Reading the input after the output must therefore give
the ORIGINAL Int32 sum, not the bumped one -- that difference is what makes
this an ownership test rather than a smoke test.
"""
import sys


def wrap(v, bits):
    m = 1 << bits
    v &= m - 1
    return v - m if v >= (m >> 1) else v


def answer(n):
    ns = []
    for k in range(n, 0, -1):
        if k % 4 == 0:
            ns.append(('S', wrap(k + 5000, 32)))
        else:
            ns.append(('N', wrap(k % 7, 8), wrap(k + 300, 16),
                       wrap(k + 40000, 32), wrap(k + 500000, 64)))
    o = [('N', x[1], x[2], wrap(x[3] + 3, 32), x[4]) if x[0] == 'N' else x for x in ns]
    g = lambda xs, i: wrap(sum(x[i] for x in xs if x[0] == 'N'), 64)
    sS = lambda xs: wrap(sum(x[1] for x in xs if x[0] == 'S'), 64)
    return "'#(%d %d %d %d %d %d %d %d %d)" % (
        len(o), g(o, 1), g(o, 2), g(o, 3), g(o, 4), sS(o),
        g(ns, 3), g(ns, 1), sS(ns))


if __name__ == '__main__':
    print(answer(int(sys.argv[1]) if len(sys.argv) > 1 else 40))
