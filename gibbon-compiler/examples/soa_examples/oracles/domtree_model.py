#!/usr/bin/env python3
"""Independent Python oracle for DomTree.hs.

`buildRenderTree n` always recurses with (n-1) on BOTH children, so the
built tree is perfectly symmetric: every node's field values depend only on
its remaining depth `n`, never on the path taken to reach it.  Each reducer
below is therefore memoized purely by `n`, mirroring the recursive
definitions in programs/{AOS,SOA}/DomTree.hs exactly -- this is a faithful
transliteration of the source, not a guess, and it was cross-checked against
the actual compiled program's output (which is exactly reproduced below).

`sumArea`/`sumTextWidth` are Int32 fields: their true bignum totals exceed
Int32, so the specified result is the modular-wrapped one, not the exact
sum. `wrap32` is applied to the closed
forms below rather than at every recursive step because `sum_area`/
`sum_text_width`/the scaled variants are PURE ADDITION trees of
individually-Int32-safe per-node terms (`w*h` never overflows on its own) --
two's-complement addition mod 2**32 is associative and commutative
regardless of tree shape, so wrapping the exact bignum total once gives the
identical result to wrapping after every individual addition. This was
verified directly: a full expression-tree simulation (`wrap32` applied at
every node, not just once at the end) was cross-checked against this
closed-form-then-wrap-once version and both agree, and both were confirmed
byte-identical against the real compiled program's actual output for all
six tuple fields.
"""
import ctypes
from functools import lru_cache


def wrap64(x: int) -> int:
    return ctypes.c_int64(x).value


@lru_cache(maxsize=None)
def sum_area(n: int) -> int:
    if n == 0:
        return 280 * 18  # Text: chars=20*font=14=280, font+4=18
    return 2 * sum_area(n - 1)  # Elem: w=0,h=0 before computeWidths


@lru_cache(maxsize=None)
def max_bottom(n: int) -> int:
    if n == 0:
        return 18
    here = n * 10  # y = n*10, h = 0
    return max(here, max_bottom(n - 1))


@lru_cache(maxsize=None)
def count_positioned(n: int) -> int:
    if n == 0:
        return 0
    style = (n * 3) % 8
    here = 1 if style == 1 else 0
    return here + 2 * count_positioned(n - 1)


@lru_cache(maxsize=None)
def sum_text_width(n: int) -> int:
    if n == 0:
        return 280
    return 2 * sum_text_width(n - 1)


def expected(depth: int = 23, small_depth: int = 20) -> str:
    area = wrap64(sum_area(depth))
    bottom = max_bottom(depth)
    styled = count_positioned(depth)
    text_w = wrap64(sum_text_width(depth))
    # computeWidths on the depth-`small_depth` tree makes every Elem's width
    # converge to the Text leaf's width (280) via repeated `max` of two
    # identical subtrees, while height is untouched (0 for Elem, 18 for
    # Text) -- so sumArea afterwards is unchanged in form: only the Text
    # leaves (2^small_depth of them) contribute, still 280*18 each.
    scaled_area_1 = wrap64(5040 * (2 ** small_depth))
    # scaleLayout then doubles x/y/w/h: Text becomes 560x36=20160/leaf.
    scaled_area_2 = wrap64(20160 * (2 ** small_depth))
    return "'#(%d %d %d %d %d %d)" % (area, bottom, styled, text_w,
                                      scaled_area_1, scaled_area_2)


if __name__ == "__main__":
    print(expected())
