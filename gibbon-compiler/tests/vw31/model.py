#!/usr/bin/env python3
"""Independent model for the tests/vw31 fixtures.

Each expected value is computed from the source definition in this file.  None
of it is recorded from Gibbon, and the observables were chosen so that the ways
VW-31 could go wrong are distinguishable rather than aliased:

  length  -- a lost head or tail, or a traversal truncated after the first node
  sumA    -- the A arm's field, so a clobbered tag moves value out of this sum
  sumB    -- the B (and C) arms' fields, so a clobbered tag moves value INTO it
"""
import sys


def build(stem, n):
    if stem in ('sharedTail', 'dupCall'):
        return [('A', k) if k < 3 else ('B', k + 200) for k in range(n, 0, -1)]
    if stem == 'revOrder':
        return [('B', k + 200) if k >= 3 else ('A', k) for k in range(n, 0, -1)]
    if stem == 'threeWay':
        out = []
        for k in range(n, 0, -1):
            if k < 3:
                out.append(('A', k))
            elif k < 6:
                out.append(('B', k + 200))
            else:
                out.append(('B', k + 500))   # C is summed with B
        return out
    if stem == 'oneArm':
        # the B arm discards the tail, so the list ends at the first B
        out = []
        for k in range(n, 0, -1):
            if k < 3:
                out.append(('A', k))
            else:
                out.append(('B', k + 200))
                break
        return out
    if stem == 'oneCtor':
        return [('A', k) for k in range(n, 0, -1)]
    if stem == 'gap16':
        return [('A', k + 2 * k + 3 * k) if k < 3 else ('B', k + 200)
                for k in range(n, 0, -1)]
    if stem == 'gap8':
        return [('A', k + 2 * k) if k < 3 else ('B', k + 200)
                for k in range(n, 0, -1)]
    raise SystemExit('unknown fixture ' + stem)


if __name__ == '__main__':
    stem, n = sys.argv[1], int(sys.argv[2])
    ns = build(stem, n)
    if stem == 'oneCtor':
        print("'#(%d %d 0)" % (len(ns), sum(v for _, v in ns)))
    else:
        print("'#(%d %d %d)" % (len(ns),
                                sum(v for c, v in ns if c == 'A'),
                                sum(v for c, v in ns if c == 'B')))
