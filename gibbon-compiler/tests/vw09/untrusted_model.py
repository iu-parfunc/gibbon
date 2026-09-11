#!/usr/bin/env python3
"""Independent model for tests/vw09/UntrustedMetadata.hs.

`mkP 40` builds PC k (k+100) for k = 40..1; `bumpP` adds 1 and 2.  The program
prints the sum before and after, so a traversal that silently produced an empty
result -- which is what trusting an untouched footer would do -- shows up as a
second component collapsing toward zero.
"""
ns = [(k, k + 100) for k in range(40, 0, -1)]
b = [(a + 1, c + 2) for a, c in ns]
print("'#(%d %d)" % (sum(a + c for a, c in ns), sum(a + c for a, c in b)))
