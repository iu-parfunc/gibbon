#!/usr/bin/env python3
"""Independent Python oracle for ArithmeticIntensityIntN.hs. Never invokes
Gibbon.

Constructs the same Fibonacci-shaped tree ArithmeticIntensityIntN.hs's
`mkTree` builds (identical to Add1TreeIntN.hs's: `d-1` on the left branch,
`d-2` on the right, base case `d <= 0`), applies the same nonlinear
`arithKernel` mixing chain, and folds the same `checksumTree` (`h*31+x`,
left-then-right) -- native modular wraparound per width via `ctypes`,
applied AFTER EVERY ARITHMETIC OPERATION in the exact source-expression
order, never by computing in arbitrary precision and truncating only at
the end:

    a = wrapN(wrapN(x*x) + c1)
    b = wrapN(wrapN(a*a) - x)
    c = wrapN(wrapN(b*x) + a)
    d = wrapN(wrapN(c*c) - b)
    result = wrapN(d + x)

Committed size: depth 20, seed 1 -- 17,711 leaves (a Fibonacci number),
identical to Add1TreeIntN.hs's committed size (same tree-building formula,
same reason: see Add1TreeInt32.hs's header comment for why a perfectly
balanced tree defeats an order-sensitive rolling-hash checksum at narrow
widths, and why threading an explicit position counter through the
checksum's own return type fails to compile against Factored SoA -- see
BUGS.md VW-35).

Design verification (`verify_design()`): proves, by direct construction of
each corrupted variant (not by assumption), that the committed checksum
distinguishes all eight required failure classes, at every width: kernel
not executed; one arithmetic stage omitted; one stage executed twice;
multiplication replaced with addition; operands reordered; traversal
order corrupted; scalar tail omitted; partial tree/list processing.
"""
import ctypes
import functools
import re
import sys
from pathlib import Path

# Must match `mkTree <DEPTH0> <SEED0>` in every
# programs/{AOS,SOA}/ArithmeticIntensityIntN.hs. source_depths() below reads
# the sources back and test_arithintensity_widths.py fails if they ever
# disagree -- the drift this constant is guarding against actually happened:
# the SoA sources were raised to 35 while AoS stayed at 20, so every SoA
# configuration scored WRONG against a depth-20 oracle while silently
# benchmarking a 24x larger tree.
DEPTH0 = 35
SEED0 = 1
C1 = 17


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


def kernel(x, w):
    """The high-intensity mixing chain, wrapped after EVERY operation in
    exact source-expression order.

    415 operations per element (was 8): 8 independent chain-pairs of 6
    rounds each, seeded from the leaf value and summed. Every design choice
    here was forced by a measured failure, not chosen for elegance --
    see programs/AOS/ArithmeticIntensityInt32.hs and the notes below:

      * INDEPENDENT chains, because one dependent chain is latency-bound:
        measured 7.08 vs 50.10 Gop/s at identical op counts.
      * Both multipliers of a round are formed from the OLD a and b, so the
        round's two multiplies issue in PARALLEL. Deriving tb from the NEW a
        put them in series (mul->add->mul->add, ~22 cycles a round with a
        10-cycle packed multiply): Int32 then vectorized to 0.81x -- SLOWER
        than scalar -- while executing 2.4x fewer instructions. Making them
        independent took Int32 to 1.76x and Int16 from 2.76x to 4.56x.
      * Multiply density is NOT reduced by padding with add/sub. That was
        tried: runs of `a = a+b+c; b = b-a+c` are affine, so the C compiler
        composes them and deletes the work -- measured 0.38 instructions per
        source operation against a healthy ~0.75-1.0. With only + - * in the
        subset, multiplies are the only non-affine ingredient, so any
        multiply-free run collapses.
      * DATA-DEPENDENT multiplier (2b+1), because `a*m + c` with constant m
        is linear, so N rounds fold into one multiply-add -- the compiler
        deleted the work and "achieved" 163.5 Gop/s against a 59.6 ceiling.
      * ODD multiplier, because multiplication by an odd number is a
        bijection mod 2^k. Squaring instead (`a*a + c`) collapsed 64 leaf
        values to 2 distinct outputs at Int8/Int16.

    verify_design() confirms all eight corruption classes remain
    distinguishable at every width with this kernel.
    """
    a0 = w(x + 29)
    b0 = w(x + 31)
    a1 = w(x + 37)
    b1 = w(x + 41)
    a2 = w(x + 43)
    b2 = w(x + 47)
    a3 = w(x + 53)
    b3 = w(x + 59)
    a4 = w(x + 29)
    b4 = w(x + 31)
    a5 = w(x + 37)
    b5 = w(x + 41)
    a6 = w(x + 43)
    b6 = w(x + 47)
    a7 = w(x + 53)
    b7 = w(x + 59)
    ta0 = w(w(b0 + b0) + 1)
    tb0 = w(w(a0 + a0) + 1)
    a0 = w(w(a0 * ta0) + 29)
    b0 = w(w(b0 * tb0) + 41)
    ta1 = w(w(b1 + b1) + 1)
    tb1 = w(w(a1 + a1) + 1)
    a1 = w(w(a1 * ta1) + 31)
    b1 = w(w(b1 * tb1) + 43)
    ta2 = w(w(b2 + b2) + 1)
    tb2 = w(w(a2 + a2) + 1)
    a2 = w(w(a2 * ta2) + 37)
    b2 = w(w(b2 * tb2) + 47)
    ta3 = w(w(b3 + b3) + 1)
    tb3 = w(w(a3 + a3) + 1)
    a3 = w(w(a3 * ta3) + 41)
    b3 = w(w(b3 * tb3) + 53)
    ta4 = w(w(b4 + b4) + 1)
    tb4 = w(w(a4 + a4) + 1)
    a4 = w(w(a4 * ta4) + 43)
    b4 = w(w(b4 * tb4) + 59)
    ta5 = w(w(b5 + b5) + 1)
    tb5 = w(w(a5 + a5) + 1)
    a5 = w(w(a5 * ta5) + 47)
    b5 = w(w(b5 * tb5) + 29)
    ta6 = w(w(b6 + b6) + 1)
    tb6 = w(w(a6 + a6) + 1)
    a6 = w(w(a6 * ta6) + 53)
    b6 = w(w(b6 * tb6) + 31)
    ta7 = w(w(b7 + b7) + 1)
    tb7 = w(w(a7 + a7) + 1)
    a7 = w(w(a7 * ta7) + 59)
    b7 = w(w(b7 * tb7) + 37)
    ta0 = w(w(b0 + b0) + 1)
    tb0 = w(w(a0 + a0) + 1)
    a0 = w(w(a0 * ta0) + 31)
    b0 = w(w(b0 * tb0) + 43)
    ta1 = w(w(b1 + b1) + 1)
    tb1 = w(w(a1 + a1) + 1)
    a1 = w(w(a1 * ta1) + 37)
    b1 = w(w(b1 * tb1) + 47)
    ta2 = w(w(b2 + b2) + 1)
    tb2 = w(w(a2 + a2) + 1)
    a2 = w(w(a2 * ta2) + 41)
    b2 = w(w(b2 * tb2) + 53)
    ta3 = w(w(b3 + b3) + 1)
    tb3 = w(w(a3 + a3) + 1)
    a3 = w(w(a3 * ta3) + 43)
    b3 = w(w(b3 * tb3) + 59)
    ta4 = w(w(b4 + b4) + 1)
    tb4 = w(w(a4 + a4) + 1)
    a4 = w(w(a4 * ta4) + 47)
    b4 = w(w(b4 * tb4) + 29)
    ta5 = w(w(b5 + b5) + 1)
    tb5 = w(w(a5 + a5) + 1)
    a5 = w(w(a5 * ta5) + 53)
    b5 = w(w(b5 * tb5) + 31)
    ta6 = w(w(b6 + b6) + 1)
    tb6 = w(w(a6 + a6) + 1)
    a6 = w(w(a6 * ta6) + 59)
    b6 = w(w(b6 * tb6) + 37)
    ta7 = w(w(b7 + b7) + 1)
    tb7 = w(w(a7 + a7) + 1)
    a7 = w(w(a7 * ta7) + 29)
    b7 = w(w(b7 * tb7) + 41)
    ta0 = w(w(b0 + b0) + 1)
    tb0 = w(w(a0 + a0) + 1)
    a0 = w(w(a0 * ta0) + 37)
    b0 = w(w(b0 * tb0) + 47)
    ta1 = w(w(b1 + b1) + 1)
    tb1 = w(w(a1 + a1) + 1)
    a1 = w(w(a1 * ta1) + 41)
    b1 = w(w(b1 * tb1) + 53)
    ta2 = w(w(b2 + b2) + 1)
    tb2 = w(w(a2 + a2) + 1)
    a2 = w(w(a2 * ta2) + 43)
    b2 = w(w(b2 * tb2) + 59)
    ta3 = w(w(b3 + b3) + 1)
    tb3 = w(w(a3 + a3) + 1)
    a3 = w(w(a3 * ta3) + 47)
    b3 = w(w(b3 * tb3) + 29)
    ta4 = w(w(b4 + b4) + 1)
    tb4 = w(w(a4 + a4) + 1)
    a4 = w(w(a4 * ta4) + 53)
    b4 = w(w(b4 * tb4) + 31)
    ta5 = w(w(b5 + b5) + 1)
    tb5 = w(w(a5 + a5) + 1)
    a5 = w(w(a5 * ta5) + 59)
    b5 = w(w(b5 * tb5) + 37)
    ta6 = w(w(b6 + b6) + 1)
    tb6 = w(w(a6 + a6) + 1)
    a6 = w(w(a6 * ta6) + 29)
    b6 = w(w(b6 * tb6) + 41)
    ta7 = w(w(b7 + b7) + 1)
    tb7 = w(w(a7 + a7) + 1)
    a7 = w(w(a7 * ta7) + 31)
    b7 = w(w(b7 * tb7) + 43)
    ta0 = w(w(b0 + b0) + 1)
    tb0 = w(w(a0 + a0) + 1)
    a0 = w(w(a0 * ta0) + 41)
    b0 = w(w(b0 * tb0) + 53)
    ta1 = w(w(b1 + b1) + 1)
    tb1 = w(w(a1 + a1) + 1)
    a1 = w(w(a1 * ta1) + 43)
    b1 = w(w(b1 * tb1) + 59)
    ta2 = w(w(b2 + b2) + 1)
    tb2 = w(w(a2 + a2) + 1)
    a2 = w(w(a2 * ta2) + 47)
    b2 = w(w(b2 * tb2) + 29)
    ta3 = w(w(b3 + b3) + 1)
    tb3 = w(w(a3 + a3) + 1)
    a3 = w(w(a3 * ta3) + 53)
    b3 = w(w(b3 * tb3) + 31)
    ta4 = w(w(b4 + b4) + 1)
    tb4 = w(w(a4 + a4) + 1)
    a4 = w(w(a4 * ta4) + 59)
    b4 = w(w(b4 * tb4) + 37)
    ta5 = w(w(b5 + b5) + 1)
    tb5 = w(w(a5 + a5) + 1)
    a5 = w(w(a5 * ta5) + 29)
    b5 = w(w(b5 * tb5) + 41)
    ta6 = w(w(b6 + b6) + 1)
    tb6 = w(w(a6 + a6) + 1)
    a6 = w(w(a6 * ta6) + 31)
    b6 = w(w(b6 * tb6) + 43)
    ta7 = w(w(b7 + b7) + 1)
    tb7 = w(w(a7 + a7) + 1)
    a7 = w(w(a7 * ta7) + 37)
    b7 = w(w(b7 * tb7) + 47)
    ta0 = w(w(b0 + b0) + 1)
    tb0 = w(w(a0 + a0) + 1)
    a0 = w(w(a0 * ta0) + 43)
    b0 = w(w(b0 * tb0) + 59)
    ta1 = w(w(b1 + b1) + 1)
    tb1 = w(w(a1 + a1) + 1)
    a1 = w(w(a1 * ta1) + 47)
    b1 = w(w(b1 * tb1) + 29)
    ta2 = w(w(b2 + b2) + 1)
    tb2 = w(w(a2 + a2) + 1)
    a2 = w(w(a2 * ta2) + 53)
    b2 = w(w(b2 * tb2) + 31)
    ta3 = w(w(b3 + b3) + 1)
    tb3 = w(w(a3 + a3) + 1)
    a3 = w(w(a3 * ta3) + 59)
    b3 = w(w(b3 * tb3) + 37)
    ta4 = w(w(b4 + b4) + 1)
    tb4 = w(w(a4 + a4) + 1)
    a4 = w(w(a4 * ta4) + 29)
    b4 = w(w(b4 * tb4) + 41)
    ta5 = w(w(b5 + b5) + 1)
    tb5 = w(w(a5 + a5) + 1)
    a5 = w(w(a5 * ta5) + 31)
    b5 = w(w(b5 * tb5) + 43)
    ta6 = w(w(b6 + b6) + 1)
    tb6 = w(w(a6 + a6) + 1)
    a6 = w(w(a6 * ta6) + 37)
    b6 = w(w(b6 * tb6) + 47)
    ta7 = w(w(b7 + b7) + 1)
    tb7 = w(w(a7 + a7) + 1)
    a7 = w(w(a7 * ta7) + 41)
    b7 = w(w(b7 * tb7) + 53)
    ta0 = w(w(b0 + b0) + 1)
    tb0 = w(w(a0 + a0) + 1)
    a0 = w(w(a0 * ta0) + 47)
    b0 = w(w(b0 * tb0) + 29)
    ta1 = w(w(b1 + b1) + 1)
    tb1 = w(w(a1 + a1) + 1)
    a1 = w(w(a1 * ta1) + 53)
    b1 = w(w(b1 * tb1) + 31)
    ta2 = w(w(b2 + b2) + 1)
    tb2 = w(w(a2 + a2) + 1)
    a2 = w(w(a2 * ta2) + 59)
    b2 = w(w(b2 * tb2) + 37)
    ta3 = w(w(b3 + b3) + 1)
    tb3 = w(w(a3 + a3) + 1)
    a3 = w(w(a3 * ta3) + 29)
    b3 = w(w(b3 * tb3) + 41)
    ta4 = w(w(b4 + b4) + 1)
    tb4 = w(w(a4 + a4) + 1)
    a4 = w(w(a4 * ta4) + 31)
    b4 = w(w(b4 * tb4) + 43)
    ta5 = w(w(b5 + b5) + 1)
    tb5 = w(w(a5 + a5) + 1)
    a5 = w(w(a5 * ta5) + 37)
    b5 = w(w(b5 * tb5) + 47)
    ta6 = w(w(b6 + b6) + 1)
    tb6 = w(w(a6 + a6) + 1)
    a6 = w(w(a6 * ta6) + 41)
    b6 = w(w(b6 * tb6) + 53)
    ta7 = w(w(b7 + b7) + 1)
    tb7 = w(w(a7 + a7) + 1)
    a7 = w(w(a7 * ta7) + 43)
    b7 = w(w(b7 * tb7) + 59)
    return w(w(w(w(w(w(w(w(w(w(w(w(w(w(w(a0 + b0) + a1) + b1) + a2) + b2) + a3) + b3) + a4) + b4) + a5) + b5) + a6) + b6) + a7) + b7)

def apply_kernel(t, fn, w):
    if t[0] == 'L':
        return ('L', fn(t[1], w))
    return ('N', apply_kernel(t[1], fn, w), apply_kernel(t[2], fn, w))


def checksum(t, h, w):
    if t[0] == 'L':
        return w(w(h * 31) + t[1])
    hl = checksum(t[1], h, w)
    return checksum(t[2], hl, w)


def expected_explicit(width, depth=DEPTH0, seed=SEED0):
    """The literal definition: build the tree, apply the kernel, fold it.
    Exact but O(leaves) in time AND memory, so usable only at small depths
    -- it exists to cross-check `expected`, which is what the manifest
    uses."""
    w = wrap(width)
    tree = build(depth, seed)
    mapped = apply_kernel(tree, kernel, w)
    return checksum(mapped, 0, w)


def _fold(width, depth, seed):
    """(leaf_count, S mod 2^width) for the kernel-mapped subtree.

    At DEPTH0 the tree has 24,157,817 leaves, so it is never built. The
    checksum `h*31 + x` is a linear recurrence: a subtree contributes
    (n, S) with S = sum_i k_i * 31^(n-i), and a node combines its children
    as S = S_L * 31^(n_R) + S_R. Arithmetic mod 2^width is a ring, so this
    equals the sequential fold exactly (test_arithintensity_widths.py pins
    it against expected_explicit at buildable depths).

    Memoized on (depth, seed mod 64): a leaf's value is
    `tmod(seed, 64) - 32` and both child seed recurrences (2s+1, 2s+3) are
    closed over mod 64, so the whole thing is a few thousand states."""
    modulus = 1 << width
    w = wrap(width)

    @functools.lru_cache(maxsize=None)
    def state(d, s):
        if d <= 0:
            # `seed` is always positive here (1, then 2s+1 / 2s+3), so
            # Gibbon's truncated `mod` agrees with Python's `%`.
            return (1, kernel(s - 32, w) % modulus)
        n_l, s_l = state(d - 1, (2 * s + 1) % 64)
        n_r, s_r = state(d - 2, (2 * s + 3) % 64)
        return (n_l + n_r, (s_l * pow(31, n_r, modulus) + s_r) % modulus)

    return state(depth, seed % 64)


def leaf_count(depth=DEPTH0, seed=SEED0):
    return _fold(64, depth, seed)[0]


def expected(width, depth=DEPTH0, seed=SEED0):
    modulus = 1 << width
    _n, acc = _fold(width, depth, seed)
    # Initial accumulator is 0, so the fold's result IS S; reinterpret the
    # residue as two's complement at this width.
    return acc - modulus if acc >= (modulus >> 1) else acc


_SOURCE_DEPTH_RE = re.compile(r"mkTree\s+(\d+)\s+(\d+)")


def source_depths(programs_dir=None):
    """{path: (depth, seed)} parsed out of every ArithmeticIntensityIntN.hs
    the suite ships, so a size edit cannot silently diverge from the
    committed oracle values again."""
    base = (Path(programs_dir) if programs_dir is not None
            else Path(__file__).resolve().parent.parent / "programs")
    found = {}
    for layout in ("AOS", "SOA"):
        for width in (8, 16, 32, 64):
            src = base / layout / ("ArithmeticIntensityInt%d.hs" % width)
            if not src.exists():
                continue
            for line in src.read_text().splitlines():
                stripped = line.strip()
                if stripped.startswith("tree = mkTree"):
                    m = _SOURCE_DEPTH_RE.search(stripped)
                    if m:
                        found[str(src)] = (int(m.group(1)), int(m.group(2)))
                    break
    return found


# ---------------------------------------------------------------------------
# Design verification: constructs each of the eight required corrupted
# variants directly (not by assumption) and proves the committed checksum
# distinguishes every one of them from a correct run, at every width.
# ---------------------------------------------------------------------------
def _kernel_stage_omitted(x, w):
    """Skips stage c entirely -- d is computed from b instead of c."""
    a = w(w(x * x) + C1)
    b = w(w(a * a) - x)
    d = w(w(b * b) - b)
    return w(d + x)


def _kernel_stage_twice(x, w):
    """Stage a's computation is applied twice before continuing."""
    a = w(w(x * x) + C1)
    a2 = w(w(a * a) + C1)
    b = w(w(a2 * a2) - x)
    c = w(w(b * x) + a2)
    d = w(w(c * c) - b)
    return w(d + x)


def _kernel_mul_to_add(x, w):
    """Stage c's multiply (b*x) becomes an add (b+x)."""
    a = w(w(x * x) + C1)
    b = w(w(a * a) - x)
    c = w(w(b + x) + a)
    d = w(w(c * c) - b)
    return w(d + x)


def _kernel_operands_reordered(x, w):
    """Stage c's operand order and operator are corrupted (x*b instead of
    b*x, combined with an operator flip) -- a structurally different
    computation, not just a relabeling (multiplication is commutative, so
    a pure operand swap on `*` alone is not observable; the corruption
    must actually change what is computed)."""
    a = w(w(x * x) + C1)
    b = w(w(a * a) - x)
    c = w(w(x * b) - a)
    d = w(w(c * c) - b)
    return w(d + x)


# verify_design constructs corrupted copies of the whole tree, so it runs
# at a depth small enough to build explicitly. It establishes that the
# CHECKSUM DESIGN separates the failure classes -- a property of the
# recurrence, not of the committed depth -- so it does not need, and at
# 24.2M leaves could not afford, DEPTH0.
VERIFY_DEPTH = 20


def verify_design(depth=VERIFY_DEPTH, seed=SEED0, widths=(8, 16, 32, 64)):
    """Returns a list of (width, failure_class) pairs that FAIL to differ
    from the correct checksum -- empty if the design is sound at every
    width tested."""
    tree = build(depth, seed)
    problems = []
    for width in widths:
        w = wrap(width)

        def unmap_rightmost(mapped, orig):
            if mapped[0] == 'L':
                return orig
            return ('N', mapped[1], unmap_rightmost(mapped[2], orig[2]))

        mapped = apply_kernel(tree, kernel, w)
        correct = checksum(mapped, 0, w)
        cases = {
            'kernel_not_executed': checksum(tree, 0, w),
            'stage_omitted': checksum(apply_kernel(tree, _kernel_stage_omitted, w), 0, w),
            'stage_executed_twice': checksum(apply_kernel(tree, _kernel_stage_twice, w), 0, w),
            'multiplication_replaced_with_addition':
                checksum(apply_kernel(tree, _kernel_mul_to_add, w), 0, w),
            'operands_reordered':
                checksum(apply_kernel(tree, _kernel_operands_reordered, w), 0, w),
            'traversal_order_corrupted':
                checksum(('N', apply_kernel(tree[2], kernel, w), apply_kernel(tree[1], kernel, w)), 0, w),
            'scalar_tail_omitted': checksum(unmap_rightmost(mapped, tree), 0, w),
            'partial_tree_processing':
                checksum(('N', apply_kernel(tree[1], kernel, w), tree[2]), 0, w),
        }
        for name, value in cases.items():
            if value == correct:
                problems.append((width, name))
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
        print("Design verification OK: all eight failure classes + accumulator "
              "sensitivity distinguished at every width (8/16/32/64).")
        sys.exit(0)
    for width in (8, 16, 32, 64):
        print(width, expected(width))
