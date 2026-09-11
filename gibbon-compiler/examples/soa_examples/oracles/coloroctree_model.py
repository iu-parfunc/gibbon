#!/usr/bin/env python3
"""Independent Python oracle for ColorOctree.hs. Every semantic integer in
this program is Int64. Same structural shape as the OctTree_* family
(buildColorOctree gives each of its 8 children a different seed, so the
model materializes the real ~19M-node tree once, as oracles/octtree_model.py
does), plus the same `mixSeed`-wraps-Int64 and
`mod`-can-be-truncated-on-a-negative-operand concerns as that family
(`tmod`/`wrap64`, applied to buildColorOctree's `r`/`g`/`b`/`flags`
computations, all of which apply `mod` after `absI` so in practice never
see a negative dividend here -- confirmed by inspection, kept anyway for
defensiveness/consistency with the sibling models).

`wrap64` is applied after EVERY Int64 add/subtract/multiply, exactly where
Gibbon's own generated C would wrap -- not merely truncating the final
tuple, keeping this model's operation-by-operation wrapping discipline
regardless of whether a given step turns out to need it. At Int64 this is
an identity at every step for this program (root-level sums reach only
~2.1e9, ~0.02% of Int64's range). Division (`tdiv`) itself needs no
additional wrapping -- Gibbon's Int64 division result is already the
correctly-narrow value; only the add/sub/mul operands feeding INTO a
division are wrapped first, matching the source's own evaluation order.
"""
import sys
import ctypes
sys.setrecursionlimit(10000)

DEPTH0 = 0 + 8
LEVEL0 = 0
SEED0 = 31

NODE, PIXEL = "N", "PX"


def wrap64(x):
    return ctypes.c_int64(x).value


def abs_i(x):
    return -x if x < 0 else x


def abs_i64(x):
    return wrap64(-x) if x < 0 else x


def mix_seed(s, salt):
    return wrap64(s * 1103 + salt * 97 + 13)


def tdiv(a, b):
    q = abs(a) // abs(b)
    return -q if (a < 0) != (b < 0) else q


def tmod(a, b):
    return a - b * tdiv(a, b)


def build(depth, level, seed):
    if depth == 0:
        r = tmod(abs_i(mix_seed(seed, 3)), 256)
        g = tmod(abs_i(mix_seed(seed, 5)), 256)
        b = tmod(abs_i(mix_seed(seed, 7)), 256)
        return (PIXEL, r, g, b)
    kids = [build(depth - 1, level + 1, mix_seed(seed, k + 1)) for k in range(8)]

    def sr_of(t):
        return t[1]

    def sg_of(t):
        return t[2]

    def sb_of(t):
        return t[3]

    def cnt_of(t):
        return t[4] if t[0] == NODE else 1

    # Pure addition trees of individually-Int32-safe terms: wrapping the
    # exact bignum total once is provably identical to wrapping every
    # individual addition (associative/commutative mod 2**32).
    sr = wrap64(sum(sr_of(k) for k in kids))
    sg = wrap64(sum(sg_of(k) for k in kids))
    sb = wrap64(sum(sb_of(k) for k in kids))
    cnt = wrap64(sum(cnt_of(k) for k in kids))
    r_mean = 0 if cnt == 0 else tdiv(sr, cnt)
    g_mean = 0 if cnt == 0 else tdiv(sg, cnt)
    b_mean = 0 if cnt == 0 else tdiv(sb, cnt)
    min_r = wrap64(r_mean - 20) if r_mean > 20 else 0
    min_g = wrap64(g_mean - 20) if g_mean > 20 else 0
    min_b = wrap64(b_mean - 20) if b_mean > 20 else 0
    max_r = wrap64(r_mean + 20) if wrap64(r_mean + 20) < 255 else 255
    max_g = wrap64(g_mean + 20) if wrap64(g_mean + 20) < 255 else 255
    max_b = wrap64(b_mean + 20) if wrap64(b_mean + 20) < 255 else 255
    spread = wrap64(wrap64(abs_i64(max_r - min_r) + abs_i64(max_g - min_g)) + abs_i64(max_b - min_b))
    var_p = wrap64(spread * (1 + tmod(level, 3)))
    # `sr+sg+sb` genuinely overflows Int32 (owner policy: wrap it in native
    # Int32 arithmetic, exactly like a division after an overflowing
    # multiply -- the addition wraps BEFORE the division is applied).
    energy = tdiv(wrap64(wrap64(sr + sg) + sb), 1 + cnt)
    flags = tmod(abs_i(mix_seed(seed, 29)), 8)
    return (NODE, sr, sg, sb, cnt, level, min_r, min_g, min_b, max_r, max_g, max_b,
            var_p, energy, flags, kids)


def palette_entries_quantized(t, max_depth, theta):
    if t[0] == PIXEL:
        return 1
    (_, _, _, _, cnt, lvl, min_r, min_g, min_b, max_r, max_g, max_b,
     var_p, energy, flags, kids) = t
    compact = wrap64(wrap64(wrap64(abs_i64(max_r - min_r) + abs_i64(max_g - min_g)) + abs_i64(max_b - min_b)) + (var_p // 4))
    threshold = wrap64(theta * (lvl + 1) + (flags * 2))
    approx = 1 if (lvl >= max_depth or energy < 12) else 0
    if wrap64(compact * (1 + cnt // 16)) < threshold:
        return wrap64(1 + approx)
    return wrap64(sum(palette_entries_quantized(k, max_depth, theta) for k in kids))


def quantization_error_proxy(t, max_depth, eta, weight):
    if t[0] == PIXEL:
        _, r, g, b = t
        return wrap64(wrap64(abs_i64(r - g) + abs_i64(g - b)) + abs_i64(b - r))
    (_, sr, sg, sb, cnt, lvl, _, _, _, _, _, _, _, _, _, kids) = t
    depth_term = lvl + 1
    far_lhs = cnt * 10
    far_rhs = eta * depth_term
    r = 0 if cnt == 0 else tdiv(sr, cnt)
    g0 = 0 if cnt == 0 else tdiv(sg, cnt)
    b0 = 0 if cnt == 0 else tdiv(sb, cnt)
    approx = wrap64(wrap64(abs_i64(r - g0) + abs_i64(g0 - b0)) + abs_i64(b0 - r)) * weight
    approx = wrap64(approx)
    if lvl >= max_depth or far_lhs < far_rhs:
        return approx
    return wrap64(sum(quantization_error_proxy(k, max_depth, eta, weight) for k in kids))


def build_tree():
    return build(DEPTH0, LEVEL0, SEED0)


def expected():
    t = build_tree()
    palette = palette_entries_quantized(t, 4, 12)
    quant_error = quantization_error_proxy(t, 4, 11, 3)
    return "'#(%d %d)" % (palette, quant_error)


if __name__ == "__main__":
    print(expected())
