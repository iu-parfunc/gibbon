#!/usr/bin/env python3
"""Independent Python oracle for Trie.hs.

Unlike DomTree/MonoTree/ObjectGraph, `buildTrie`'s left and right children
receive DIFFERENT seeds (`mixSeed seed 23` / `mixSeed seed 29`), so no two
subtrees are field-identical and memoization by depth alone does not apply
-- this is a direct, single-pass transliteration of the actual recursion.

`seed`'s arithmetic (`mixSeed s salt = s*1103 + salt*97 + 13`) legitimately
overflows/wraps Int64 within a handful of levels (1103^7 already exceeds
Int64's ~9.22e18 range), exactly as it did before this migration (`seed` was
already bare `Int` = Int64).  `wrap64` reproduces C's two's-complement
wraparound with ctypes.c_int64, which is what the compiled RTS actually
does; ordinary Python big-int arithmetic is used up to that point since
modular reduction commutes with + and * regardless of when it is applied.
"""
import sys
import ctypes
sys.setrecursionlimit(10000)

SIZE_PARAM = 0
D0 = SIZE_PARAM + 22
SEED0 = 17


def wrap64(x: int) -> int:
    return ctypes.c_int64(x).value


def mix_seed(s: int, salt: int) -> int:
    return wrap64(s * 1103 + salt * 97 + 13)


def abs_i(x: int) -> int:
    return -x if x < 0 else x


def visit(d: int, seed: int):
    """Returns (sumPrefixFreq, countTerminals, sumSubtreeHints,
    autocompleteTopKProxy(40), countLazyNodes(4), decayedFreq) for the
    subtree rooted here -- one pass instead of six."""
    if d == 0:
        term = 1 + (abs_i(mix_seed(seed, 2)) % 3)
        scr = 5 + (abs_i(mix_seed(seed, 5)) % 95)
        meta = abs_i(mix_seed(seed, 7)) % 16
        topk = term * scr if scr >= 40 else 0
        lazy = 1 if meta < 4 else 0
        return (0, term, 0, topk, lazy, 0)
    c = abs_i(mix_seed(seed, 11)) % 26  # unused by any reduction below
    pf = 1 + (abs_i(mix_seed(seed, 13)) % 120)
    sc = 2 * (1 + (abs_i(mix_seed(seed, 17)) % 80))
    fl = abs_i(mix_seed(seed, 19)) % 4
    lf, lt, ls, lk, ll, ld = visit(d - 1, mix_seed(seed, 23))
    rf, rt, rs, rk, rl, rd = visit(d - 1, mix_seed(seed, 29))
    lazy_here = 1 if fl == 0 else 0
    f2 = (pf * 9) // 10  # k=9, always non-negative -> floor == truncate
    return (pf + lf + rf, lt + rt, sc + ls + rs, pf + lk + rk,
           lazy_here + ll + rl, f2 + ld + rd)


def expected():
    tot_freq, tot_terms, hint_sum, topk, lazy_n, decayed_freq = visit(D0, SEED0)
    reset_freq = decayed_freq  # resetTraversalState never touches `f`
    return "'#(%d %d %d %d %d %d %d)" % (
        tot_freq, tot_terms, hint_sum, topk, lazy_n, decayed_freq, reset_freq)


if __name__ == "__main__":
    print(expected())
