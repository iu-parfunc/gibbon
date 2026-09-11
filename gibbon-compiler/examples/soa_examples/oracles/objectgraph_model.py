#!/usr/bin/env python3
"""Independent Python oracle for ObjectGraph.hs.

`buildHeap d` always recurses with (d-1) on BOTH children, so every node's
field values, and every purely structural reduction, depend only on the
remaining depth `d` -- memoized here exactly as the Haskell recursion
defines them.  `d` is always >=0 in this program (base case is `d==0`), so
Gibbon's C-style truncated `mod`/`/` agree with Python's for every value
actually reached here (unlike DecisionTree.hs, where `d` goes negative).
"""
import sys
from functools import lru_cache

SIZE_PARAM = 0
D0 = SIZE_PARAM + 23


def id_of(d):
    return d


def size_of(d):
    return d * 10


def mark_of(d):
    return d - (d // 2) * 2


@lru_cache(maxsize=None)
def total_heap_size(d):
    if d == 0:
        return 0
    return size_of(d) + 2 * total_heap_size(d - 1)


@lru_cache(maxsize=None)
def count_marked(d):
    if d == 0:
        return 0
    here = 1 if mark_of(d) == 1 else 0
    return here + 2 * count_marked(d - 1)


@lru_cache(maxsize=None)
def count_large(d, limit):
    if d == 0:
        return 0
    here = 1 if size_of(d) > limit else 0
    return here + 2 * count_large(d - 1, limit)


@lru_cache(maxsize=None)
def live_bytes(d):
    if d == 0:
        return 0
    here = size_of(d) if mark_of(d) == 1 else 0
    return here + 2 * live_bytes(d - 1)


@lru_cache(maxsize=None)
def dead_bytes(d):
    if d == 0:
        return 0
    here = size_of(d) if mark_of(d) == 0 else 0
    return here + 2 * dead_bytes(d - 1)


@lru_cache(maxsize=None)
def count_survivors(d, max_size):
    if d == 0:
        return 0
    here = 1 if (mark_of(d) == 1 and size_of(d) <= max_size) else 0
    return here + 2 * count_survivors(d - 1, max_size)


@lru_cache(maxsize=None)
def sum_obj_ids(d):
    if d == 0:
        return 0
    return id_of(d) + 2 * sum_obj_ids(d - 1)


def live_bytes_swept():
    # sweepUnmarked sets mark=0 on EVERY node, so liveBytes (which only
    # counts mark==1 nodes) over the swept tree is always 0.
    return 0


def size_hot(d, stride, delta):
    return size_of(d) + delta if (id_of(d) % stride) == 0 else size_of(d)


def mark_hot(d, stride):
    return 1 if (id_of(d) % stride) == 0 else mark_of(d)


@lru_cache(maxsize=None)
def live_bytes_hot(d, stride, delta):
    if d == 0:
        return 0
    here = size_hot(d, stride, delta) if mark_hot(d, stride) == 1 else 0
    return here + 2 * live_bytes_hot(d - 1, stride, delta)


def expected():
    d = D0
    heap_size = total_heap_size(d)
    marked = count_marked(d)
    large = count_large(d, 100)
    live = live_bytes(d)
    dead = dead_bytes(d)
    surv = count_survivors(d, 120)
    obj_ids = sum_obj_ids(d)
    live_swept = live_bytes_swept()
    live_hot = live_bytes_hot(d, 4, 12)
    return "'#(%d %d %d %d %d %d %d %d %d)" % (
        heap_size, marked, large, live, dead, surv, obj_ids, live_swept, live_hot)


if __name__ == "__main__":
    print(expected())
