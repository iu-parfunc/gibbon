#!/usr/bin/env python3
"""Independent model for tests/vw09/SkewedWidths.hs.

Nothing here reads Gibbon output to decide what is correct: the expected
program value and the expected per-buffer element totals are computed from the
source's own definition, in Python, with explicit two's-complement wrapping at
each declared width.

`check` additionally reconciles the two instrumented streams the harness emits:

  VW09PROD <buffer> <chunk> <elements>   physical element writes, counted at the
                                         producer's bump call sites and reset at
                                         every region growth
  VW09CONS <buf> cc=<n> written=<b> rem=<r> ok|OVERRUN
                                         the STORED footer count the loopified
                                         consumer read back, and the bytes it
                                         actually wrote for that chunk

The producer stream is ground truth; the consumer stream is the metadata under
test.  They must agree per chunk, and the byte relationship must hold.
"""
import sys
from collections import defaultdict


def wrap(v, bits):
    m = 1 << bits
    v &= m - 1
    return v - m if v >= (m >> 1) else v


def nodes(n):
    out = []
    for k in range(n, 0, -1):
        if k % 4 == 0:
            out.append(('S', wrap(k + 5000, 32)))
        else:
            out.append(('N', wrap(k % 7, 8), wrap(k + 300, 16),
                        wrap(k + 40000, 32), wrap(k + 500000, 64)))
    return out


def bumped(ns):
    r = []
    for x in ns:
        if x[0] == 'N':
            r.append(('N', wrap(x[1] + 1, 8), wrap(x[2] + 2, 16),
                      wrap(x[3] + 3, 32), wrap(x[4] + 4, 64)))
        else:
            r.append(('S', wrap(x[1] + 9, 32)))
    return r


def answer(n):
    b = bumped(nodes(n))
    f = lambda i: wrap(sum(x[i] for x in b if x[0] == 'N'), 64)
    return "'#(%d %d %d %d %d %d)" % (len(b), f(1), f(2), f(3), f(4),
                                      wrap(sum(x[1] for x in b if x[0] == 'S'), 64))


def totals(n):
    """Elements each *scalar* buffer must receive, plus the tag stream.

    The tag buffer counts every constructor occurrence including the final
    `End`; N's four buffers each get one element per N; S's one per S.
    """
    ns = nodes(n)
    nN = sum(1 for x in ns if x[0] == 'N')
    nS = sum(1 for x in ns if x[0] == 'S')
    return {'tags': len(ns) + 1, 'N': nN, 'S': nS}


def check(n, lines):
    prod = defaultdict(lambda: defaultdict(int))   # buffer -> chunk -> elements
    cons = defaultdict(list)                       # buffer -> [(cc, written, rem, flag)]
    for ln in lines:
        f = ln.split()
        if not f:
            continue
        if f[0] == 'VW09PROD' and len(f) == 4:
            prod[int(f[1])][int(f[2])] += int(f[3])
        elif f[0] == 'VW09CONS' and len(f) >= 6:
            buf = f[1]
            cc = int(f[2].split('=')[1])
            wr = int(f[3].split('=')[1])
            rem = int(f[4].split('=')[1])
            cons[buf].append((cc, wr, rem, f[5]))

    t = totals(n)
    problems = []

    # 1. Physical totals per buffer must match the model, summed over chunks.
    #    One site is the tag stream, four are N's fields, one is S's.  The
    #    multiset is compared rather than the index order: which producer site
    #    is which buffer is a fact about the generated code, not something this
    #    model is entitled to assume.
    got = sorted(sum(ch.values()) for ch in prod.values())
    want = sorted([t['tags']] + [t['N']] * 4 + [t['S']])
    if n > 0 and got != want:
        problems.append("physical totals %s != model %s" % (got, want))

    # 2. Every chunk the consumer walked must have written exactly
    #    chunk_count elements' worth of bytes, within capacity.
    for buf, rows in cons.items():
        for cc, wr, rem, flag in rows:
            if flag != 'ok':
                problems.append("%s overran: wrote %d of %d" % (buf, wr, rem))
            if cc > 0 and wr % cc != 0:
                problems.append("%s wrote %d bytes for %d elements (not a whole width)"
                                % (buf, wr, cc))
            if wr > rem:
                problems.append("%s wrote %d > capacity %d" % (buf, wr, rem))

    # 3. The stored per-chunk counts the consumer read must match the
    #    producer's physical per-chunk counts, chunk for chunk.
    #
    #    The two streams are matched by buffer TOTAL rather than by index:
    #    the producer's site order and the consumer's buffer numbering are
    #    independent facts about the generated code, and assuming they agree
    #    would smuggle an unproven claim into the check.  Per-chunk sequences
    #    are compared as multisets because the footer chain is cyclic -- a
    #    non-final footer holds the NEXT chunk's count and the final one holds
    #    the first's -- so the consumer walks a rotation of the producer's
    #    order, not the identity.
    pseq = {}
    for b, ch in prod.items():
        pseq[b] = [ch[k] for k in sorted(ch)]
    unclaimed = dict(pseq)
    for buf, rows in sorted(cons.items()):
        seq = sorted(cc for cc, _, _, _ in rows)
        tot = sum(seq)
        hit = None
        for b, ps in unclaimed.items():
            if sum(ps) == tot and sorted(ps) == seq:
                hit = b
                break
        if hit is None:
            cands = {b: sorted(ps) for b, ps in unclaimed.items() if sum(ps) == tot}
            problems.append("%s stored %s; no unclaimed producer buffer wrote that "
                            "(same-total candidates: %s)" % (buf, seq, cands or "none"))
        else:
            del unclaimed[hit]
    return "OK" if not problems else "; ".join(problems)


if __name__ == '__main__':
    mode = sys.argv[1]
    n = int(sys.argv[2])
    if mode == 'answer':
        print(answer(n))
    elif mode == 'check':
        print(check(n, sys.stdin.read().splitlines()))
    elif mode == 'totals':
        print(totals(n))
    elif mode == 'table':
        # Human-readable per-buffer / per-chunk view of the same two streams.
        prod = defaultdict(lambda: defaultdict(int))
        cons = defaultdict(list)
        for ln in sys.stdin.read().splitlines():
            f = ln.split()
            if not f:
                continue
            if f[0] == 'VW09PROD' and len(f) == 4:
                prod[int(f[1])][int(f[2])] += int(f[3])
            elif f[0] == 'VW09CONS' and len(f) >= 6:
                cons[f[1]].append((int(f[2].split('=')[1]), int(f[3].split('=')[1]),
                                   int(f[4].split('=')[1]), f[5]))
        print("n=%d  model totals: %s" % (n, totals(n)))
        print("producer (physical element writes, per buffer per chunk)")
        for b in sorted(prod):
            ch = prod[b]
            print("  site %-2d total=%-6d chunks=%s" %
                  (b, sum(ch.values()), [ch[k] for k in sorted(ch)]))
        print("consumer (stored footer count / bytes written / capacity left)")
        for b in sorted(cons):
            rows = cons[b]
            print("  %-28s total=%-6d %s" %
                  (b.split('_')[-1], sum(r[0] for r in rows),
                   [(cc, wr, rem, fl) for cc, wr, rem, fl in rows]))
    else:
        sys.exit("unknown mode " + mode)
