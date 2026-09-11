#!/usr/bin/env python3
"""Independent Python oracle for DecisionTree.hs.

`buildTree d` is a pure function of `d` (a FULL binary tree: left child
d-1, right child d-2, every `Node` has exactly two children), so every
node's OWN field values, and every purely structural reduction over the
subtree rooted at `d`, depend only on `d` -- memoized here exactly as the
Haskell recursion defines them. `classify`/`classifyDepth` are simulated
directly (a single root-to-leaf walk per query vector), which is what the
source itself does; there is no way to memoize a walk that depends on the
query vector's contents.

Depth-to-size identity (full binary tree: #internal = #leaves - 1):
  leaves(d) = Fib(d+2)  (Fib(1)=Fib(2)=1);  nodes(d) = 2*leaves(d) - 1.

Every ADT field and semantic function is Int64 (`wrap64`); at depth 35,
`sum_impurity(35)` = 24,094,570,052 and `sum_path_lengths(0, 35)` =
379,975,140, both of which exceed Int32 range but fit Int64.

The committed benchmark depth (`D0`) is `SIZE_PARAM + 14` rather than
`SIZE_PARAM + 35`, to keep runtime bounded under mandatory `--packed`
(see BUGS.md VW-38); the query/batch counts passed to
`classify_depth_batch`/`classify_batch` below are unchanged
(250,000/1,000,000). At depth 14, `sum_impurity(14)` = 983,433 and
`sum_path_lengths(0, 14)` = 1,907,500, both far inside Int32 range, so
`wrap64` is a no-op at this size -- it is kept so the model stays
correct if the committed depth is ever raised again.
"""
import sys
import ctypes
from functools import lru_cache


def wrap64(x: int) -> int:
    return ctypes.c_int64(x).value

SIZE_PARAM = 0

# The benchmark was one program (one tree, one depth) until 2026-09-06. It is
# now two, because the two halves need opposite sizes: measured at the old
# shared depth 14, every structural/ML fold ran 12-23 MICROseconds while
# classifyDepthBatch alone took 0.60s. See programs/AOS/DecisionTree.hs's
# header for the full account (including the SoA + --use-mutable-cursors
# compiler bug that rules out two trees inside one gibbon_main).
#
# DecisionTree.hs -- the nine folds, on a tree big enough to time.
FOLD_D0 = SIZE_PARAM + 32
# DecisionTreeClassify.hs -- batched inference, on the UNCHANGED committed
# depth: its two values are bit-for-bit the last two entries of the old
# combined tuple, so nothing about this half's workload moved.
CLASSIFY_D0 = SIZE_PARAM + 14

# Back-compat alias for the pre-split single depth. Retained because
# `D0` names the depth the CLASSIFICATION half still runs at.
D0 = CLASSIFY_D0


def tmod(a: int, b: int) -> int:
    """Gibbon's `mod` is C-style truncated remainder (sign follows the
    dividend), NOT Haskell's/Python's floored `mod`/`%` -- confirmed by
    cross-checking this exact model against the compiled program's output at
    a small depth, where `d` goes negative (see BUGS.md VW-12: Gibbon's
    div/mod deliberately do not floor)."""
    q = abs(a) // abs(b)
    if (a < 0) != (b < 0):
        q = -q
    return a - q * b


def leaf_fields(d):
    return (tmod(d, 3), 1 + tmod(d, 10))  # (label, samples)


def node_fields(d):
    return (tmod(d, 16), tmod(d, 100), 1000 - d)  # (feature, threshold, impurity)


@lru_cache(maxsize=None)
def count_nodes(d):
    if d <= 0:
        return 1
    return 1 + count_nodes(d - 1) + count_nodes(d - 2)


@lru_cache(maxsize=None)
def count_leaves(d):
    if d <= 0:
        return 1
    return count_leaves(d - 1) + count_leaves(d - 2)


@lru_cache(maxsize=None)
def tree_depth(d):
    if d <= 0:
        return 1
    return 1 + max(tree_depth(d - 1), tree_depth(d - 2))


@lru_cache(maxsize=None)
def sum_impurity(d):
    if d <= 0:
        return 0
    _, _, imp = node_fields(d)
    return imp + sum_impurity(d - 1) + sum_impurity(d - 2)


@lru_cache(maxsize=None)
def sum_samples(d):
    if d <= 0:
        _, samples = leaf_fields(d)
        return samples
    return sum_samples(d - 1) + sum_samples(d - 2)


@lru_cache(maxsize=None)
def count_feature_uses(fid, d):
    if d <= 0:
        return 0
    f, _, _ = node_fields(d)
    here = 1 if f == fid else 0
    return here + count_feature_uses(fid, d - 1) + count_feature_uses(fid, d - 2)


@lru_cache(maxsize=None)
def count_small_leaves(thresh, d):
    if d <= 0:
        _, samples = leaf_fields(d)
        return 1 if samples < thresh else 0
    return count_small_leaves(thresh, d - 1) + count_small_leaves(thresh, d - 2)


@lru_cache(maxsize=None)
def inference_cost(d):
    if d <= 0:
        return 0
    return 1 + max(inference_cost(d - 1), inference_cost(d - 2))


@lru_cache(maxsize=None)
def sum_path_lengths(depth, d):
    if d <= 0:
        _, samples = leaf_fields(d)
        return depth * samples
    return sum_path_lengths(depth + 1, d - 1) + sum_path_lengths(depth + 1, d - 2)


def gen_fv(fv_size, seed):
    return [tmod(j * 3 + seed + SIZE_PARAM, 100) for j in range(fv_size)]


def classify(d, fv):
    while d > 0:
        feature, threshold, _ = node_fields(d)
        val = fv[feature]
        d = (d - 1) if val <= threshold else (d - 2)
    label, _ = leaf_fields(d)
    return label


def classify_depth(d, fv):
    depth = 0
    while d > 0:
        feature, threshold, _ = node_fields(d)
        val = fv[feature]
        d = (d - 1) if val <= threshold else (d - 2)
        depth += 1
    return depth


def classify_depth_batch(d0, fv_size, n):
    total = 0
    for i in range(n, 0, -1):
        fv = gen_fv(fv_size, i)
        total += classify_depth(d0, fv)
    return total


def classify_batch(d0, fv_size, n):
    total = 0
    for i in range(n, 0, -1):
        fv = gen_fv(fv_size, i)
        total += classify(d0, fv)
    return total


def expected_folds(d=None):
    """DecisionTree.hs -- the nine structural/ML folds, in source order."""
    d = FOLD_D0 if d is None else d
    return "'#(%d %d %d %d %d %d %d %d %d)" % (
        count_nodes(d), count_leaves(d), tree_depth(d), wrap64(sum_impurity(d)),
        sum_samples(d), count_feature_uses(0, d), count_small_leaves(5, d),
        inference_cost(d), sum_path_lengths(0, d))


def expected_classify(d=None):
    """DecisionTreeClassify.hs -- classifyDepthBatch then classifyBatch."""
    d = CLASSIFY_D0 if d is None else d
    return "'#(%d %d)" % (classify_depth_batch(d, 32, 250000),
                          classify_batch(d, 32, 1000000))


def expected_combined(d=None):
    """The pre-split 11-tuple: all nine folds AND both classify passes at a
    single depth. Kept so the historical committed value stays re-derivable
    (`expected_combined(14)` reproduces it exactly); no shipped program
    prints this shape any more."""
    d = D0 if d is None else d
    return "'#(%d %d %d %d %d %d %d %d %d %d %d)" % (
        count_nodes(d), count_leaves(d), tree_depth(d), wrap64(sum_impurity(d)),
        sum_samples(d), count_feature_uses(0, d), count_small_leaves(5, d),
        inference_cost(d), sum_path_lengths(0, d),
        classify_depth_batch(d, 32, 250000), classify_batch(d, 32, 1000000))


if __name__ == "__main__":
    print("DecisionTree         (d=%d): %s" % (FOLD_D0, expected_folds()))
    print("DecisionTreeClassify (d=%d): %s" % (CLASSIFY_D0, expected_classify()))
