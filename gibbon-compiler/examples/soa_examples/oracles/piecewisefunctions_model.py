#!/usr/bin/env python3
"""Independent Python oracle for PiecewiseFunctions.hs.

Like Trie.hs, `buildPW`'s left/right children get different seeds
(`mixSeed seed 1` / `mixSeed seed 2`), so this is a direct single-pass
transliteration, not a depth-memoized closed form.  `seed`'s Int64
arithmetic overflow/wraparound is reproduced with ctypes.c_int64, exactly as
in trie_model.py -- see that file's docstring for why big-int arithmetic
followed by a single wrap is equivalent to wrapping at every step.

`norm2Estimate` is an Int32 field (overflow alone does not justify an
Int64 exception). Its true total (4,404,055,778 at the committed d=23)
exceeds Int32 and wraps; `wrap32` is applied once to the
final accumulated `norm` value in `expected()` below, valid because this is
a pure addition tree of individually-Int32-safe per-leaf terms (each leaf's
own `c*c + (d*d)/(s+1)` never overflows on its own: c<=33, d<=39).
"""
import functools
import sys
import ctypes
sys.setrecursionlimit(10000)

SIZE_PARAM = 0
D0 = SIZE_PARAM + 23
SEED0 = 17
TOL = 18
CUT = 500
SHIFT = 10


def wrap64(x: int) -> int:
    return ctypes.c_int64(x).value


def mix_seed(s: int, salt: int) -> int:
    return wrap64(s * 1103 + salt * 97 + 13)


def abs_i(x: int) -> int:
    return -x if x < 0 else x


def visit(d: int, seed: int):
    """Returns (norm2, tolViol, mass, maxLvl, pmapCuts, loadW, massShift,
    massDiff) for the subtree rooted here -- one pass instead of eight,
    including the addConstPW(10)+diffPW(.)+compressMass composition."""
    if d == 0:
        coeff = 5 + (abs_i(mix_seed(seed, 3)) % 29)
        scale = 1 + (abs_i(mix_seed(seed, 5)) % 12)
        detail = abs_i(mix_seed(seed, 7)) % 40
        norm2 = coeff * coeff + (detail * detail) // (scale + 1)
        tol_viol = 1 if detail > TOL else 0
        mass = abs_i(coeff)
        max_lvl = scale  # autorefineMaxLevel reads the 2nd Leaf field
        load_w = (scale + 1) * (1 + detail // 8)
        shifted_coeff = coeff + SHIFT
        mass_shift = abs_i(shifted_coeff)
        # diffPW: scale is always >=1 here, so the sc==0 branch never fires.
        diffed_coeff = shifted_coeff * scale
        mass_diff = abs_i(diffed_coeff)
        return (norm2, tol_viol, mass, max_lvl, 0, load_w, mass_shift, mass_diff)
    dim = abs_i(mix_seed(seed, 11)) % 3
    split = abs_i(mix_seed(seed, 13)) % 1000
    lvl = d
    (ln2, ltv, lm, lml, lpc, llw, lms, lmd) = visit(d - 1, mix_seed(seed, 1))
    (rn2, rtv, rm, rml, rpc, rlw, rms, rmd) = visit(d - 1, mix_seed(seed, 2))
    pmap_here = (dim + 1) if split > CUT else 0
    return (
        ln2 + rn2,
        ltv + rtv,
        lm + rm,
        max(lvl, max(lml, rml)),
        pmap_here + lpc + rpc,
        lvl + 1 + llw + rlw,
        lms + rms,
        lmd + rmd,
    )


def expected():
    """The combined program's 8-tuple. No shipped program prints this shape
    any more (the benchmark is one executable per timed pass since
    2026-09-06), but it stays the single source of every split file's
    expected value -- see SPLIT_PASSES/expected_pass below -- so the
    committed number is still re-derivable exactly as before."""
    norm, refine_cnt, mass, max_lvl, pmap_cuts, load_w, mass_shift, mass_diff = visit(D0, SEED0)
    return "'#(%d %d %d %d %d %d %d %d)" % (
        wrap64(norm), refine_cnt, mass, max_lvl, pmap_cuts, load_w, mass_shift, mass_diff)


# PiecewiseFunctions_<pass>.hs -> its index in the combined 8-tuple, in the
# order the combined program's gibbon_main computed them. The two map
# passes return `compressMass` of their own output (the combined program's
# `massShift`/`massDiff`), which is what makes them checkable at all: a PW
# tree is not printable, a reduction over it is.
SPLIT_PASSES = [
    ("norm2Estimate", 0), ("truncateTolViolations", 1), ("compressMass", 2),
    ("autorefineMaxLevel", 3), ("pmapCutHistogram", 4), ("lbDeuxLoadProxy", 5),
    ("addConstPW", 6), ("diffPW", 7),
]


@functools.lru_cache(maxsize=None)
def _tuple_values():
    """Cached: `visit` walks the full depth-23 tree (8.4M leaves), so the
    eight split files' values come off ONE traversal, not eight."""
    norm, refine_cnt, mass, max_lvl, pmap_cuts, load_w, mass_shift, mass_diff = visit(D0, SEED0)
    return (wrap64(norm), refine_cnt, mass, max_lvl, pmap_cuts, load_w,
            mass_shift, mass_diff)


def expected_pass(pass_name):
    """The scalar PiecewiseFunctions_<pass_name>.hs prints."""
    index = dict(SPLIT_PASSES)[pass_name]
    return str(_tuple_values()[index])


if __name__ == "__main__":
    print("combined (superseded): %s" % expected())
    for name, _ in SPLIT_PASSES:
        print("  PiecewiseFunctions_%-22s %s" % (name, expected_pass(name)))
