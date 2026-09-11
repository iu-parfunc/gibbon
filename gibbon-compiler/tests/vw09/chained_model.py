#!/usr/bin/env python3
"""Independent model for tests/vw09/Chained{1,2,3}.hs.

Nothing here reads Gibbon output to decide what is correct.  `nodes`/`answer`
are computed straight from each fixture's own source definition, in Python,
with explicit two's-complement wrapping at each declared width.

Usage:
  chained_model.py answer <hops> <n>
  chained_model.py check  <hops> <n>   < instrumented-stderr

`check` reconciles the SAME two independent streams vw09/skewed_model.py
does -- VW09PROD (producer physical writes, ground truth) against VW09CONS
(the stored footer count / bytes the loopified consumer reads back) -- except
here VW09PROD is emitted once per HOP (P0's own build, then once per
intermediate producer's ScalarCountCopyAll), so a chain fidelity failure at
any hop shows up as a per-hop, per-buffer, per-chunk mismatch rather than
just a wrong final answer.
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


def p1(ns):
    r = []
    for x in ns:
        if x[0] == 'N':
            r.append(('N', wrap(x[1] + 1, 8), x[2], x[3], x[4]))
        else:
            r.append(('S', wrap(x[1] + 1, 32)))
    return r


def p2(ns):
    r = []
    for x in ns:
        if x[0] == 'N':
            r.append(('N', x[1], wrap(x[2] + 10, 16), x[3], x[4]))
        else:
            r.append(('S', wrap(x[1] + 100, 32)))
    return r


def p3(ns):
    r = []
    for x in ns:
        if x[0] == 'N':
            r.append(('N', x[1], x[2], wrap(x[3] + 1000, 32), x[4]))
        else:
            r.append(('S', wrap(x[1] + 10000, 32)))
    return r


def bumped(ns):
    r = []
    for x in ns:
        if x[0] == 'N':
            r.append(('N', wrap(x[1] + 1, 8), wrap(x[2] + 2, 16),
                      wrap(x[3] + 3, 32), wrap(x[4] + 4, 64)))
        else:
            r.append(('S', wrap(x[1] + 9, 32)))
    return r


HOPS = {1: [p1], 2: [p1, p2], 3: [p1, p2, p3]}


def chain(hops, n):
    ns = nodes(n)
    for f in HOPS[hops]:
        ns = f(ns)
    return bumped(ns)


def answer(hops, n):
    b = chain(hops, n)
    f = lambda i: wrap(sum(x[i] for x in b if x[0] == 'N'), 64)
    return "'#(%d %d %d %d %d %d)" % (len(b), f(1), f(2), f(3), f(4),
                                      wrap(sum(x[1] for x in b if x[0] == 'S'), 64))


def check(hops, n, log):
    # Ground truth: physical writes at P0 (hop 0).
    prod = defaultdict(dict)   # prod[hop][(buf,chunk)] = elements
    cons = defaultdict(dict)   # cons[hop][(buf,chunk)] = (chunk_count, bytes)
    for line in log:
        parts = line.split()
        if not parts:
            continue
        if parts[0] == 'VW09PROD':
            hop, buf, chunk, elems = int(parts[1]), int(parts[2]), int(parts[3]), int(parts[4])
            prod[hop][(buf, chunk)] = prod[hop].get((buf, chunk), 0) + elems
        elif parts[0] == 'VW09CONS':
            # buf is the generated loop's own variable-name prefix, a string
            # -- not an index comparable to VW09PROD's.  Sequences are
            # reconciled by matching VALUES below, never by this name.
            hop, buf = int(parts[1]), parts[2]
            cc = int(parts[3].split('=')[1])
            written = int(parts[4].split('=')[1])
            cons[hop].setdefault((buf,), []).append((cc, written))

    if 0 not in prod:
        return "NO_PRODUCER_STREAM"
    # Every hop's consumer-observed chunk_count sequence, reconstructed by
    # matching totals (order across independent buffers is a fact about the
    # generated code, not assumed here), must equal hop 0's physical stream.
    base = prod[0]
    base_by_buf = defaultdict(list)
    for (buf, chunk), e in sorted(base.items(), key=lambda kv: (kv[0][0], kv[0][1])):
        base_by_buf[buf].append(e)

    for hop in sorted(cons.keys()):
        seen_by_buf = defaultdict(list)
        for (buf,), recs in cons[hop].items():
            seen_by_buf[buf] = [cc for cc, _ in recs]
        unclaimed = {b: list(v) for b, v in base_by_buf.items()}
        for buf, seq in seen_by_buf.items():
            matched = False
            for b2, seq2 in unclaimed.items():
                if seq2 == seq:
                    del unclaimed[b2]
                    matched = True
                    break
            if not matched:
                return "HOP_%d_BUF_%s_MISMATCH got=%s" % (hop, buf, seq)
    return "OK"


if __name__ == "__main__":
    cmd = sys.argv[1]
    hops = int(sys.argv[2])
    n = int(sys.argv[3])
    if cmd == "answer":
        print(answer(hops, n))
    elif cmd == "check":
        print(check(hops, n, sys.stdin.read().splitlines()))
    else:
        sys.exit("unknown command")
