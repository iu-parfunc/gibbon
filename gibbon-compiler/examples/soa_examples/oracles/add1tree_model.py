#!/usr/bin/env python3
"""Independent Python oracle for Add1TreeIntN.hs.

Never invokes Gibbon. Constructs the same Fibonacci-shaped tree the real
source builds (`mkTree d seed`: `d-1` on the left branch, `d-2` on the
right, base case `d <= 0`), applies the same `add1Tree` map, and folds the
same `checksumTree` (`h*31+x`, left-then-right), independently for each of
the four widths -- native modular wraparound per width, using `ctypes` to
match Gibbon's two's-complement semantics exactly.

Committed size: depth 33, seed 1 -- 9,227,465 leaves (a Fibonacci number).
Raised from depth 20 (17,711 leaves) by the benchmark owner on 2026-09-06 so
the reported per-pass times land in a legible range; the four expected
values in oracles/manifest.json were re-derived here for the new depth.

At 9.2M leaves an explicit tree costs gigabytes, so `expected` no longer
builds one. It folds the SAME recurrence in closed form instead: a subtree
contributes (n, S) where n is its leaf count and S = sum_i v_i * 31^(n-i)
mod 2^width, and a node combines its children as S = S_L * 31^(n_R) + S_R.
Arithmetic mod 2^width is a ring, so this is exactly equal to the
sequential `h*31+x` fold, not an approximation. Memoized on (depth, seed
mod 64) -- a leaf's value depends only on `seed mod 64`, and both child
seed recurrences (2s+1, 2s+3) are closed over mod 64 -- so the whole thing
is a few thousand states regardless of depth. `expected_explicit` keeps the
literal tree-building version; test_add1tree_widths.py asserts the two
agree at every width on a depth small enough to build.

WHY a Fibonacci-shaped tree instead of a perfectly balanced one: see
`programs/AOS/Add1TreeInt32.hs`'s header comment for the full account.
Briefly, two designs were tried on a perfectly-balanced 2^20-leaf tree and
independently disproven by direct construction before this shape was
adopted:
  (1) a plain rolling hash over a balanced tree is provably insensitive to
      the initial accumulator at Int8/Int16 once the leaf count is a
      multiple of the relevant modulus's multiplicative-group exponent
      (65536 is, at both widths, for ANY odd multiplier) -- it cannot even
      detect "map never ran";
  (2) threading an explicit position counter through the traversal's
      RETURN value fixes that, and works in AoS, but fails to compile
      against genuine Factored SoA (a real, newly-discovered compiler
      limitation -- BUGS.md's VW-35).
Breaking the tree's structural self-similarity (asymmetric recursion,
giving Fibonacci-shaped subtrees whose sizes differ meaningfully at every
sibling pair) fixes both problems while keeping `checksumTree` a plain
single-accumulator fold -- proven, by direct construction of each
corrupted variant below (`verify_design`), to distinguish all six required
failure classes from a correct run, at every one of the four widths.
"""
import ctypes
import functools
import re
import sys
from pathlib import Path

# Must match `mkTree <DEPTH0> <SEED0>` in every programs/{AOS,SOA}/Add1TreeIntN.hs.
# source_depths() below reads the sources back, and
# test_add1tree_widths.py fails if they ever disagree with this constant --
# the drift that silently invalidated the depth-20 values once already.
DEPTH0 = 33
SEED0 = 1


def wrap(bits):
    ctype = {8: ctypes.c_int8, 16: ctypes.c_int16,
             32: ctypes.c_int32, 64: ctypes.c_int64}[bits]
    return lambda x: ctype(x).value


def tdiv(a, b):
    q = abs(a) // abs(b)
    return -q if (a < 0) != (b < 0) else q


def tmod(a, b):
    return a - b * tdiv(a, b)


def build(d, seed):
    """('L', value) | ('N', left, right) -- mirrors mkTree exactly."""
    if d <= 0:
        return ('L', tmod(seed, 64) - 32)
    return ('N', build(d - 1, seed * 2 + 1), build(d - 2, seed * 2 + 3))


def add1(t, w):
    if t[0] == 'L':
        return ('L', w(t[1] + 1))
    return ('N', add1(t[1], w), add1(t[2], w))


def checksum(t, h, w):
    if t[0] == 'L':
        return w(w(h * 31) + t[1])
    hl = checksum(t[1], h, w)
    return checksum(t[2], hl, w)


def expected_explicit(width, depth=DEPTH0, seed=SEED0):
    """The literal definition: build the tree, map it, fold it. Exact but
    O(leaves) in time AND memory, so it is usable only at small depths --
    it exists to cross-check `expected`, which is what the manifest uses."""
    w = wrap(width)
    tree = build(depth, seed)
    mapped = add1(tree, w)
    return checksum(mapped, 0, w)


def leaf_count(depth=DEPTH0, seed=SEED0):
    return _fold(64, depth, seed)[0]


def _fold(width, depth, seed):
    """(leaf_count, S mod 2^width) for the add1-mapped subtree, folded in
    closed form -- see the module docstring for why this equals the
    sequential h*31+x fold exactly."""
    modulus = 1 << width

    @functools.lru_cache(maxsize=None)
    def state(d, s):
        if d <= 0:
            # Leaf value is `mod seed 64 - 32`; seed is always positive here
            # (1, then 2s+1 / 2s+3), so Haskell's `mod` is Python's `%`.
            # add1Tree then adds one, hence the -31.
            return (1, (s - 31) % modulus)
        n_l, s_l = state(d - 1, (2 * s + 1) % 64)
        n_r, s_r = state(d - 2, (2 * s + 3) % 64)
        return (n_l + n_r, (s_l * pow(31, n_r, modulus) + s_r) % modulus)

    return state(depth, seed % 64)


def expected(width, depth=DEPTH0, seed=SEED0):
    modulus = 1 << width
    _, acc = _fold(width, depth, seed)
    # Initial accumulator is 0, so the fold's result IS S; reinterpret the
    # residue as two's complement at this width.
    return acc - modulus if acc >= (modulus >> 1) else acc


_SOURCE_DEPTH_RE = re.compile(r"mkTree\s+(\d+)\s+(\d+)")


def source_depths(programs_dir=None):
    """{path: (depth, seed)} parsed out of every Add1TreeIntN.hs the suite
    ships, so a size edit to the sources cannot silently diverge from the
    committed oracle values again."""
    base = (Path(programs_dir) if programs_dir is not None
            else Path(__file__).resolve().parent.parent / "programs")
    found = {}
    for layout in ("AOS", "SOA"):
        for width in (8, 16, 32, 64):
            src = base / layout / ("Add1TreeInt%d.hs" % width)
            if not src.exists():
                continue
            for line in src.read_text().splitlines():
                stripped = line.strip()
                # The call in gibbon_main, not mkTree's own recursive body
                # (`mkTree (d - 1) ...`, which the regex cannot match anyway).
                if stripped.startswith("tree = mkTree"):
                    m = _SOURCE_DEPTH_RE.search(stripped)
                    if m:
                        found[str(src)] = (int(m.group(1)), int(m.group(2)))
                    break
    return found


def expected_str(width, depth=DEPTH0, seed=SEED0):
    return "Running program Add1Tree Int%d: \n%d" % (width, expected(width, depth, seed))


# ---------------------------------------------------------------------------
# Design verification: proves, by direct construction of each corrupted
# tree (not by assumption), that the committed (depth, width) checksum
# distinguishes all six required failure classes from a correct run. Run as
# a script (`python3 add1tree_model.py --verify`) or imported by
# test_add1tree_widths.py.
# ---------------------------------------------------------------------------
def _count(t):
    if t[0] == 'L':
        return 1
    return _count(t[1]) + _count(t[2])


# verify_design constructs six corrupted copies of the whole tree, so it
# runs at a depth small enough to build explicitly. It establishes that the
# CHECKSUM DESIGN separates the failure classes -- a property of the
# recurrence, not of the committed depth -- so it does not need, and at
# 9.2M leaves could not afford, DEPTH0.
VERIFY_DEPTH = 20


def verify_design(depth=VERIFY_DEPTH, seed=SEED0, widths=(8, 16, 32, 64)):
    """Returns a list of (width, failure_class) pairs that FAIL to differ
    from the correct checksum -- empty if the design is sound at every
    width tested."""
    tree = build(depth, seed)
    problems = []
    for width in widths:
        w = wrap(width)

        def add1_swapped(t):
            if t[0] == 'L':
                return ('L', w(t[1] + 1))
            return ('N', add1_swapped(t[2]), add1_swapped(t[1]))

        def add1_twice(t):
            if t[0] == 'L':
                return ('L', w(t[1] + 2))
            return ('N', add1_twice(t[1]), add1_twice(t[2]))

        def unmap_rightmost(mapped, orig):
            if mapped[0] == 'L':
                return orig
            return ('N', mapped[1], unmap_rightmost(mapped[2], orig[2]))

        def swap_first_two_values(t):
            vals = []

            def collect(node):
                if node[0] == 'L':
                    vals.append(node[1])
                else:
                    collect(node[1])
                    collect(node[2])
            collect(t)
            vals[0], vals[1] = vals[1], vals[0]
            it = iter(vals)

            def rebuild(node):
                if node[0] == 'L':
                    return ('L', next(it))
                return ('N', rebuild(node[1]), rebuild(node[2]))
            return rebuild(t)

        mapped = add1(tree, w)
        correct = checksum(mapped, 0, w)
        cases = {
            'map_never_ran': checksum(tree, 0, w),
            'add1_applied_twice': checksum(add1_twice(tree), 0, w),
            'left_right_exchanged': checksum(add1_swapped(tree), 0, w),
            'only_part_mapped': checksum(('N', add1(tree[1], w), tree[2]), 0, w),
            'scalar_tail_skipped': checksum(unmap_rightmost(mapped, tree), 0, w),
            'payload_order_corrupted': checksum(swap_first_two_values(mapped), 0, w),
        }
        for name, value in cases.items():
            if value == correct:
                problems.append((width, name))
        # Sensitivity to the initial accumulator (distinguishes "checksum
        # never actually depends on the data" from a coincidental match).
        if checksum(mapped, 9, w) == correct:
            problems.append((width, 'initial_accumulator_insensitive'))
    return problems


if __name__ == "__main__":
    if "--verify" in sys.argv:
        problems = verify_design()
        if problems:
            print("DESIGN VERIFICATION FAILED:")
            for width, name in problems:
                print("  width=%d: %s collides with correct checksum" % (width, name))
            sys.exit(1)
        print("Design verification OK: all six failure classes + accumulator "
              "sensitivity distinguished at every width (8/16/32/64).")
        sys.exit(0)
    for width in (8, 16, 32, 64):
        print(width, expected(width))
