#!/usr/bin/env python3
"""Independent Python oracle for DBQuery.hs. Every semantic integer field
is Int64.

`buildQuery` branches into Join (two children: d-1 and, if d>1, d-2, else
0) or Filter (one child: d-1) based on a seed-derived tag, and every seed
diverges per child -- a direct single-pass transliteration, matching
Trie.hs/PiecewiseFunctions.hs's approach. `seed`'s Int64 wraparound is
reproduced with ctypes.c_int64, as in those models (buildQuery's own
locals are Int64 build-side infra). All mod/division operands inside
`visit`'s build-side arithmetic are non-negative by construction (`d`
never goes negative -- the base case is exactly `d == 0`), so Python's
floor division agrees with Gibbon's C-style truncation there.

The six reduction functions (sumCost, sumRows, countJoins, sumMemory,
hashJoinPressure, filterSelectivitySkew) are modeled with `wrap64` applied
after EVERY Int64 add/sub/mul, matching Gibbon's own generated-C
evaluation order (an overflowing multiply is wrapped BEFORE the division
that follows it consumes it) -- not merely truncating each function's
final return value. At the committed sizes every one of these totals
fits comfortably inside Int64 (the largest, `mapCost1`/`mapCost2`'s
10x-scaled `sumCost` -- computed by re-running the same `visit`-derived
`sum_cost` traversal over a 10x-scaled cost tree, see `expected()` below
-- reaches 19,340,852,180), so `wrap64` is an identity throughout; it is
kept for rigor/consistency with the sibling oracle models rather than
because any value here actually overflows.
"""
import sys
import ctypes
sys.setrecursionlimit(10000)

SIZE_PARAM = 0
D0 = SIZE_PARAM + 75
SEED0 = 17


def wrap64(x: int) -> int:
    return ctypes.c_int64(x).value


def mix_seed(s: int, salt: int) -> int:
    return wrap64(s * 1103 + salt * 97 + 13)


def abs_i(x: int) -> int:
    return -x if x < 0 else x


def abs_i64(x: int) -> int:
    return wrap64(-x) if x < 0 else x


def max_i(a, b):
    return a if a > b else b


def tdiv(a: int, b: int) -> int:
    """C-style truncating division (sign follows the dividend), needed once
    an operand can be a wrapped (possibly negative) Int64 value."""
    q = abs(a) // abs(b)
    if (a < 0) != (b < 0):
        q = -q
    return q


def build(d: int, seed: int):
    """Materializes the real Query tree as nested Python tuples, mirroring
    OctTreeBase.hs's approach -- needed (rather than a single fused
    reduction pass) because `sumCost` must be re-run over a `scaleCosts`-10x
    variant of the SAME tree for mapCost1/mapCost2, and both must be
    Int64-wrapped independently."""
    if d == 0:
        table_id = abs_i(seed) % 17
        rows = 2000 + (abs_i(mix_seed(seed, 3)) % 6000)
        cost = 20 + rows // 16
        width = 24 + (abs_i(mix_seed(seed, 7)) % 120)
        return ("Scan", wrap64(table_id), wrap64(rows), wrap64(cost), wrap64(width))
    tag = abs_i(mix_seed(seed, 11)) % 4
    if tag < 2:
        l = build(d - 1, mix_seed(seed, 1))
        r_depth = d - 2 if d > 1 else 0
        r = build(r_depth, mix_seed(seed, 2))
        join_ty = abs_i(mix_seed(seed, 13)) % 3
        l_rows = 1200 + d * 20 + (abs_i(mix_seed(seed, 17)) % 2000)
        r_rows = 1000 + d * 15 + (abs_i(mix_seed(seed, 19)) % 1700)
        sel = 60 + (abs_i(mix_seed(seed, 23)) % 260)
        out_rows = max_i(1, (l_rows * r_rows) // (sel * 10 + 1))
        if join_ty == 0:
            join_cpu = (l_rows * r_rows) // 2400
        elif join_ty == 1:
            join_cpu = (l_rows + r_rows) // 7
        else:
            join_cpu = (l_rows + r_rows) // 9
        total = 30 + join_cpu + out_rows // 20
        mem = (r_rows // 2) if join_ty == 1 else (out_rows // 8)
        return ("Join", wrap64(join_ty), wrap64(out_rows), wrap64(total), wrap64(mem), l, r)
    else:
        s = build(d - 1, mix_seed(seed, 3))
        pred_id = abs_i(mix_seed(seed, 29)) % 31
        sel = 120 + (abs_i(mix_seed(seed, 31)) % 760)
        cpu = 4 + (abs_i(mix_seed(seed, 37)) % 40)
        flags = abs_i(mix_seed(seed, 41)) % 8
        return ("Filter", wrap64(pred_id), wrap64(sel), wrap64(cpu), wrap64(flags), s)


def sum_cost(q):
    if q[0] == "Join":
        _, _, _, c, _, l, r = q
        return wrap64(wrap64(c + sum_cost(l)) + sum_cost(r))
    elif q[0] == "Filter":
        _, _, _, c, _, s = q
        return wrap64(c + sum_cost(s))
    elif q[0] == "Scan":
        _, _, _, c, _ = q
        return c
    return 0


def sum_rows(q):
    if q[0] == "Join":
        _, _, r, _, _, l, s = q
        return wrap64(wrap64(r + sum_rows(l)) + sum_rows(s))
    elif q[0] == "Filter":
        _, _, sel, _, _, s = q
        child_rows = sum_rows(s)
        out_rows = max_i(1, tdiv(wrap64(child_rows * sel), 1000))
        return wrap64(out_rows + child_rows)
    elif q[0] == "Scan":
        _, _, r, _, _ = q
        return r
    return 0


def count_joins(q):
    if q[0] == "Join":
        _, _, _, _, _, l, r = q
        return wrap64(wrap64(1 + count_joins(l)) + count_joins(r))
    elif q[0] == "Filter":
        _, _, _, _, _, s = q
        return count_joins(s)
    return 0


def sum_memory(q):
    if q[0] == "Join":
        _, _, _, _, m, l, r = q
        return wrap64(wrap64(m + sum_memory(l)) + sum_memory(r))
    elif q[0] == "Filter":
        _, _, _, c, _, s = q
        return wrap64(c + sum_memory(s))
    elif q[0] == "Scan":
        _, _, _, _, w = q
        return w
    return 0


def hash_join_pressure(q):
    if q[0] == "Join":
        _, jt, _, _, m, l, r = q
        mine = m if jt == 1 else 0
        return wrap64(wrap64(mine + hash_join_pressure(l)) + hash_join_pressure(r))
    elif q[0] == "Filter":
        _, _, _, _, _, s = q
        return hash_join_pressure(s)
    return 0


def filter_selectivity_skew(q):
    if q[0] == "Filter":
        _, _, sel, _, _, s = q
        return wrap64(abs_i64(sel - 500) + filter_selectivity_skew(s))
    elif q[0] == "Join":
        _, _, _, _, _, l, r = q
        return wrap64(filter_selectivity_skew(l) + filter_selectivity_skew(r))
    return 0


def scale_costs(q, k):
    if q[0] == "Join":
        _, t, r, c, m, l, s = q
        return ("Join", t, r, wrap64(c * k), m, scale_costs(l, k), scale_costs(s, k))
    elif q[0] == "Filter":
        _, p, sel, c, f, s = q
        return ("Filter", p, sel, wrap64(c * k), f, scale_costs(s, k))
    elif q[0] == "Scan":
        _, t, r, c, w = q
        return ("Scan", t, r, wrap64(c * k), w)
    return q


def build_tree():
    return build(D0, SEED0)


def expected():
    t = build_tree()
    tot_cost = sum_cost(t)
    tot_rows = sum_rows(t)
    tot_joins = count_joins(t)
    tot_mem = sum_memory(t)
    hash_pressure = hash_join_pressure(t)
    sel_skew = filter_selectivity_skew(t)
    # gibbon_main: scaleCosts 10, then clearQueryFlags (never touches cost),
    # so mapCost1 == mapCost2 == sumCost(scaleCosts(t, 10)) exactly.
    t_scaled = scale_costs(t, 10)
    map_cost = sum_cost(t_scaled)
    return "Running Data base Query Pass:\n'#(%d %d %d %d %d %d %d %d)" % (
        tot_cost, tot_rows, tot_joins, tot_mem, hash_pressure, sel_skew,
        map_cost, map_cost)


if __name__ == "__main__":
    print(expected())
