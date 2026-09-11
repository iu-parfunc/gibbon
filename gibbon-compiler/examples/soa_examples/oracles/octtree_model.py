#!/usr/bin/env python3
"""Independent Python oracle for the OctTree_*.hs family, built on the
shared OctTreeBase.hs module. Serves all 8 DEFAULT_PROGRAMS rows that
import it: OctTree_sumMass, OctTree_sumEnergy, OctTree_countActive,
OctTree_countParticles, OctTree_barnesHutPotential, OctTree_fmmPotential,
OctTree_scaleEnergy, OctTree_clearFlags. Every OctTreeBase.hs field and
semantic function is Int64.

`buildOctree d seed center half` recurses into 8 children, each with a
DIFFERENT seed (`mixSeed seed 1` .. `mixSeed seed 8`) -- unlike KDTree.hs's
two IDENTICAL children, there is no shared-subtree collapse available here,
so this model materializes the real tree once (as nested Python tuples,
~19M nodes at d=8: 8^8=16777216 leaves plus ~2.4M internal cells) and runs
each reduction as a direct traversal over it, exactly mirroring
OctTreeBase.hs's case-of-constructor structure.

`wrap64` is applied after EVERY Int64 add/sub/mul in this model, matching
Gibbon's own generated C evaluation order exactly (an overflowing
multiply/add is wrapped BEFORE any subsequent division consumes it) -- not
merely truncating each function's final return value. This is deliberate
even though most of this family's true values turn out to fit comfortably
inside Int64 (mass up to ~5e7, so most products stay far below 2**63): the
independent oracle models operation-by-operation two's-complement Int64
wrapping as the reference value regardless, rather than assuming in
advance which sub-computations do or don't need it. Because
`weightedPos` feeds `wTot`, which determines `com` at every non-leaf node,
and `com` is read by `sumEnergy`/`countActive`/`barnesHutPotential`/
`fmmPotential`'s distance calculations, any wraparound that DOES occur
would propagate through the whole family; the full tree materialization
(rather than a closed-form estimate) is what lets this model capture that
propagation exactly, node by node, the same way the real compiled program
does, at whichever width.

Benchmark-campaign note: the real `OctTree_*.hs` executables are compiled
with `--c-arithmetic=unsafe`, which supplies NO `-fwrapv` -- unsafe mode
gives no language-level guarantee of
two's-complement wraparound on overflow. If a compiled unsafe-mode result
matches this oracle's wrap64-modeled value, that is an OBSERVED toolchain
result for the current GCC/Clang build, not a semantic guarantee of unsafe
mode; see BUGS.md for the full caveat. This oracle's own reference value
does not depend on that toolchain behavior -- it is independently derived
from the source's specified arithmetic operations.

`com` (buildOctree's center-of-mass field) and `scaleEnergy`'s `mom'`/`v'`
are the only spots that divide a value which can be NEGATIVE (weightedPos
sums can go negative since position can; momentum/velocity fields can be
negative) by a POSITIVE divisor -- Gibbon's `/` truncates toward zero
(C-style), so these need the `tdiv` helper; every other division in this
module has non-negative operands throughout (every dividend here is either
an abs_i result, a sum-of-squares, or dist*dist/d*d -- all non-negative)
and Python's floor `//` agrees with truncation there. `mixSeed` (Int64
build-side infra, never stored) keeps its original `wrap64`-based model.

See BUGS.md VW-24: OctTree_barnesHutPotential, OctTree_fmmPotential,
OctTree_scaleEnergy and OctTree_clearFlags are known to produce WRONG
results in the loopify/selective/vectorize *optimized* compile modes
(BUGS.md). This oracle's `expected` values are for the PLAIN
--packed --use-mutable-cursors build only -- they must never be used to
claim those programs are correct under the optimized modes.
"""
import sys
import ctypes
sys.setrecursionlimit(10000)

D0 = 0 + 8
SEED0 = 17
CENTER0 = 0
HALF0 = 64

CELL, PARTICLE, EMPTY = "C", "P", "E"


def abs_i(x):
    return -x if x < 0 else x


def abs_i64(x):
    return wrap64(-x) if x < 0 else x


def max_i(a, b):
    return a if a > b else b


def wrap64(x):
    return ctypes.c_int64(x).value


def mix_seed(s, salt):
    return wrap64(s * 1103 + salt * 97 + 13)


def tdiv(a, b):
    """C-style truncating division (sign follows the dividend)."""
    q = abs(a) // abs(b)
    return -q if (a < 0) != (b < 0) else q


def tmod(a, b):
    """C-style truncating remainder (sign follows the dividend), needed
    wherever a `mixSeed` result can be negative (after Int64 wraparound)
    and is used directly by `mod` without an enclosing `absI` -- namely
    buildOctree's `p` and `v` leaf fields."""
    return a - b * tdiv(a, b)


def build(d, seed, center, half):
    """Build-side (`d`, `seed`, `center`, `half`, `stride`, offsets) is
    Int64 infra; the constructed Octree payload fields
    (mass/com/count/momentum) and this function's own internal aggregation
    (mTot/wTot/nTot/pTot/com) are Int64 as well."""
    if d == 0:
        m = 1 + (abs_i(seed) % 5)
        p = center + tmod(mix_seed(seed, 3), 3) - 1
        v = tmod(mix_seed(seed, 11), 11) - 5
        return (PARTICLE, wrap64(m), wrap64(p), wrap64(v))
    half2 = max_i(1, half // 2)
    stride = max_i(1, half // 4)
    offs = [-(stride * 7), -(stride * 5), -(stride * 3), -stride,
            stride, stride * 3, stride * 5, stride * 7]
    kids = [build(d - 1, mix_seed(seed, k + 1), center + offs[k], half2)
            for k in range(8)]

    def mass_of(t):
        return t[1] if t[0] != EMPTY else 0

    def wpos_of(t):
        # weightedPos: `m*c` (Cell) / `m*p` (Particle), Int64 -- wrapped
        # after the multiply per this model's wrap64 discipline.
        if t[0] == CELL:
            return wrap64(t[1] * t[2])
        elif t[0] == PARTICLE:
            return wrap64(t[1] * t[2])
        return 0

    def count_of(t):
        if t[0] == CELL:
            return t[3]
        elif t[0] == PARTICLE:
            return 1
        return 0

    def mom_of(t):
        if t[0] == CELL:
            return t[5]
        elif t[0] == PARTICLE:
            return wrap64(t[1] * t[3])
        return 0

    # sum8i64: a+b+...+h, each individual add wrapping at Int64 in the real
    # source; wrapping the exact bignum sum once is provably identical
    # (Int64 addition mod 2**64 is associative/commutative) for any of
    # these 8-way sums throughout this model.
    m_tot = wrap64(sum(mass_of(c) for c in kids))
    w_tot = wrap64(sum(wpos_of(c) for c in kids))
    n_tot = wrap64(sum(count_of(c) for c in kids))
    p_tot = wrap64(sum(mom_of(c) for c in kids))
    # com: Int64 division (`wTot / mTot`).
    com = wrap64(center) if m_tot == 0 else tdiv(w_tot, m_tot)
    # (mass, com, count, halfSize, momentum, children)
    return (CELL, m_tot, com, n_tot, wrap64(half), p_tot, kids)


def sum_mass(t):
    if t[0] == PARTICLE:
        return t[1]
    elif t[0] == CELL:
        return wrap64(sum(sum_mass(c) for c in t[6]))
    return 0


def sum_energy(t):
    """`bulk = (m*mom*mom)/(m*m+1)` and `pot = (m*s*50)/dist` compute in
    Int64 throughout, per this model's operation-by-operation wrap64
    discipline: each product is wrapped BEFORE the division that follows
    it, matching Gibbon's own evaluation order (see BUGS.md VW-34 for the
    width history behind this discipline)."""
    if t[0] == PARTICLE:
        _, m, _, v = t
        return tdiv(wrap64(wrap64(m * v) * v), 2)
    elif t[0] == CELL:
        _, m, c, _, s, mom, kids = t
        dist = wrap64(abs_i64(c) + 1)
        bulk = tdiv(wrap64(wrap64(m * mom) * mom), wrap64(wrap64(m * m) + 1))
        pot = tdiv(wrap64(wrap64(m * s) * 50), dist)
        return wrap64(wrap64(bulk + pot) + sum(sum_energy(k) for k in kids))
    return 0


def count_active(t, theta):
    if t[0] == PARTICLE:
        return 0
    elif t[0] == CELL:
        _, _, c, _, s, _, kids = t
        probe = 0
        dist = wrap64(abs_i64(c - probe) + 1)
        open_lhs = wrap64(s * 100)
        open_rhs = wrap64(theta * dist)
        refine = 1 if open_lhs >= open_rhs else 0
        return wrap64(refine + sum(count_active(k, theta) for k in kids))
    return 0


def count_particles(t):
    if t[0] == PARTICLE:
        return 1
    elif t[0] == CELL:
        return wrap64(sum(count_particles(k) for k in t[6]))
    return 0


def barnes_hut_potential(t, probe, theta):
    """`probe`/`theta` are Int64 parameters (call-site values 21/60 are
    tiny); `m*1000` (m up to 50,331,648) is wrapped at Int64 before the
    division is applied, per this model's wrap64 discipline."""
    if t[0] == PARTICLE:
        _, m, p, _ = t
        dist = wrap64(abs_i64(p - probe) + 1)
        return tdiv(wrap64(m * 1000), wrap64(dist * dist))
    elif t[0] == CELL:
        _, m, c, n, s, _, kids = t
        dist = wrap64(abs_i64(c - probe) + 1)
        open_lhs = wrap64(s * 100)
        open_rhs = wrap64(theta * dist)
        approx = 0 if n == 0 else tdiv(wrap64(m * 1000), wrap64(dist * dist))
        if open_lhs < open_rhs:
            return approx
        return wrap64(sum(barnes_hut_potential(k, probe, theta) for k in kids))
    return 0


def fmm_up_series(m, dip, order):
    """Int64 throughout, per the module's wrap64 discipline: `m*100` (m up
    to 50,331,648) is wrapped before `corr` is accumulated on top of it."""
    if order <= 0:
        return wrap64(m * 100)
    prev = fmm_up_series(m, dip, order - 1)
    corr = tdiv(abs_i64(dip), wrap64(order * 20 + 1))
    return wrap64(prev + corr)


def fmm_down_series(m, mom, s, dist, order):
    if order <= 0:
        return tdiv(wrap64(m * 100), dist)
    prev = fmm_down_series(m, mom, s, dist, order - 1)
    d = wrap64(dist + order)
    corr = tdiv(wrap64(abs_i64(mom) + wrap64(s * order)), wrap64(wrap64(d * d) + 1))
    return wrap64(prev + corr)


def fmm_potential(t, probe, order, eta):
    """`fmmUpSeries`/`fmmDownSeries` and this function's own locals are
    Int64 throughout, per the module's wrap64 discipline -- `m*c`/`m*100`
    are wrapped right after the multiplication, matching Gibbon's own
    evaluation order, before any subsequent addition/division consumes the
    wrapped product."""
    if t[0] == PARTICLE:
        _, m, p, v = t
        dist = wrap64(abs_i64(p - probe) + 1)
        up = fmm_up_series(m, wrap64(m * p), order)
        return wrap64(tdiv(up, wrap64(dist + 1)) +
                       tdiv(wrap64(wrap64(m * 100) + abs_i64(v)), dist))
    elif t[0] == CELL:
        _, m, c, _, s, mom, kids = t
        dist = wrap64(abs_i64(c - probe) + 1)
        far_lhs = wrap64(s * 100)
        far_rhs = wrap64(eta * dist)
        up_moment = fmm_up_series(m, wrap64(m * c), order)
        down_approx = fmm_down_series(m, mom, s, dist, order)
        approx = wrap64(tdiv(up_moment, wrap64(dist + 1)) + down_approx)
        if far_lhs < far_rhs:
            return approx
        return wrap64(sum(fmm_potential(k, probe, order, eta) for k in kids))
    return 0


def scale_energy(t, k):
    """`mom * k` and `v * k` are wrapped at 64 bits per this model's
    operation-by-operation Int64 wrapping discipline (see module
    docstring); kept even though these products are not expected to
    actually overflow at Int64, since omitting the wrap here would drop
    coverage of any future width change (see BUGS.md VW-34)."""
    if t[0] == PARTICLE:
        _, m, p, v = t
        return (PARTICLE, m, p, tdiv(wrap64(v * k), 10))
    elif t[0] == CELL:
        _, m, c, n, s, mom, kids = t
        mom2 = tdiv(wrap64(mom * k), s + 1)
        return (CELL, m, c, n, s, mom2, [scale_energy(kk, k) for kk in kids])
    return t


def clear_flags(t):
    if t[0] == PARTICLE:
        return t
    elif t[0] == CELL:
        _, m, c, _, s, mom, kids = t
        return (CELL, m, c, 0, s, mom, [clear_flags(kk) for kk in kids])
    return t


def build_tree():
    return build(D0, SEED0, CENTER0, HALF0)


EXPECTED = {
    "OctTree_sumMass": lambda t: sum_mass(t),
    "OctTree_sumEnergy": lambda t: sum_energy(t),
    "OctTree_countActive": lambda t: count_active(t, 60),
    "OctTree_countParticles": lambda t: count_particles(t),
    "OctTree_barnesHutPotential": lambda t: barnes_hut_potential(t, 21, 60),
    "OctTree_fmmPotential": lambda t: fmm_potential(t, 21, 4, 70),
    "OctTree_scaleEnergy": lambda t: sum_energy(scale_energy(t, 9)),
    "OctTree_clearFlags": lambda t: count_active(clear_flags(t), 60),
}


if __name__ == "__main__":
    stem = sys.argv[1] if len(sys.argv) > 1 else None
    tree = build_tree()
    if stem:
        print(EXPECTED[stem](tree))
    else:
        for name, fn in EXPECTED.items():
            print(name, "=", fn(tree))
