#!/usr/bin/env python3
"""Independent Python oracle for KDTree.hs.

KDTree.hs is the designated explicit-Int64 program: its ADT fields are
declared Int64 explicitly rather than left as bare `Int` (which already
meant Int64), so no value changes -- this oracle exists purely to keep
KDTree.hs on the same independent-oracle footing as every other curated
program.

`buildKD d axis` recurses with the SAME `(d - 1, nextAxis)` on both
children (`l = buildKD (d-1) nextAxis; r = buildKD (d-1) nextAxis`), so the
left and right subtrees are always structurally and value-identical. Every
reduction pass here computes both `l`/`r` results unconditionally (a source
comment says so explicitly: "computed unconditionally for Gibbon compiler
stability"), so cl == cr (or dl == dr, ml == mr, hl == hr) at every single
node -- meaning each pass collapses to ONE recursive call per level, not
two. This model exploits that collapse for a direct depth-22 recursion
(memoized by (d, axis[, phases, rays, seed, radius]) for extra safety, but
the state space is tiny either way -- no exponential blowup, unlike a
naive two-branch simulation).

Leaves are built only at d == 0, where `buildKD` ignores `axis` entirely
(`KDLeaf d (d+1) (d+2) (d*3) d`), so every leaf in the tree is identical:
point (0, 1, 2), mass 0, objectId 0.

All divisions in `photonMappingBenchmark` (the only pass that divides) have
provably non-negative operands throughout the recursion (`rays`, `seed`,
`radius`, `splitVal` all stay >= 0 by construction/clamping), so Python's
floor division agrees with Gibbon's C-style truncation there -- no `tdiv`
helper needed, unlike DecisionTree.hs/DBQuery.hs.
"""
import sys
from functools import lru_cache

sys.setrecursionlimit(10000)

SIZE_PARAM = 0
D0 = SIZE_PARAM + 22
AXIS0 = 0

LEAF_X, LEAF_Y, LEAF_Z, LEAF_MASS, LEAF_OID = 0, 1, 2, 0, 0


def coord_at(axis, x, y, z):
    return x if axis == 0 else (y if axis == 1 else z)


def dist3(x1, y1, z1, x2, y2, z2):
    return abs(x1 - x2) + abs(y1 - y2) + abs(z1 - z2)


def axis_lower_bound(q, lo, hi):
    if q < lo:
        return lo - q
    elif q > hi:
        return q - hi
    else:
        return 0


def bbox_lower_bound(minX, minY, minZ, maxX, maxY, maxZ, qx, qy, qz):
    return (axis_lower_bound(qx, minX, maxX)
            + axis_lower_bound(qy, minY, maxY)
            + axis_lower_bound(qz, minZ, maxZ))


def bbox_upper_bound(minX, minY, minZ, maxX, maxY, maxZ, qx, qy, qz):
    return (max(abs(qx - minX), abs(qx - maxX))
            + max(abs(qy - minY), abs(qy - maxY))
            + max(abs(qz - minZ), abs(qz - maxZ)))


def point_in_box(x, y, z, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ):
    return (qMinX <= x <= qMaxX) and (qMinY <= y <= qMaxY) and (qMinZ <= z <= qMaxZ)


def bbox_disjoint(minX, minY, minZ, maxX, maxY, maxZ, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ):
    return (maxX < qMinX or minX > qMaxX
            or maxY < qMinY or minY > qMaxY
            or maxZ < qMinZ or minZ > qMaxZ)


@lru_cache(maxsize=None)
def nearest(d, axis, qx, qy, qz):
    if d == 0:
        return dist3(LEAF_X, LEAF_Y, LEAF_Z, qx, qy, qz)
    next_axis = (axis + 1) % 3
    split_val = d * 11 + axis
    q_coord = coord_at(axis, qx, qy, qz)
    plane_dist = abs(q_coord - split_val)
    box_dist = bbox_lower_bound(-d, -d, -d, d, d, d, qx, qy, qz)
    c = nearest(d - 1, next_axis, qx, qy, qz)
    near, far = c, c
    if box_dist >= near:
        return near
    elif plane_dist < near:
        return min(near, far)
    else:
        return near


@lru_cache(maxsize=None)
def count_in_range(d, axis, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ):
    if d == 0:
        return 1 if point_in_box(LEAF_X, LEAF_Y, LEAF_Z, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ) else 0
    next_axis = (axis + 1) % 3
    split_val = d * 11 + axis
    disjoint = bbox_disjoint(-d, -d, -d, d, d, d, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ)
    q_lo = coord_at(axis, qMinX, qMinY, qMinZ)
    q_hi = coord_at(axis, qMaxX, qMaxY, qMaxZ)
    c = count_in_range(d - 1, next_axis, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ)
    if disjoint:
        return 0
    elif q_hi < split_val:
        return c
    elif q_lo > split_val:
        return c
    else:
        return c + c


@lru_cache(maxsize=None)
def sum_mass_in_range(d, axis, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ):
    if d == 0:
        return LEAF_MASS if point_in_box(LEAF_X, LEAF_Y, LEAF_Z, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ) else 0
    next_axis = (axis + 1) % 3
    split_val = d * 11 + axis
    disjoint = bbox_disjoint(-d, -d, -d, d, d, d, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ)
    q_lo = coord_at(axis, qMinX, qMinY, qMinZ)
    q_hi = coord_at(axis, qMaxX, qMaxY, qMaxZ)
    m = sum_mass_in_range(d - 1, next_axis, qMinX, qMaxX, qMinY, qMaxY, qMinZ, qMaxZ)
    if disjoint:
        return 0
    elif q_hi < split_val:
        return m
    elif q_lo > split_val:
        return m
    else:
        return m + m


@lru_cache(maxsize=None)
def two_point_correlation(d, axis, qx, qy, qz, r_lo, r_hi):
    if d == 0:
        dd = dist3(LEAF_X, LEAF_Y, LEAF_Z, qx, qy, qz)
        if dd < r_lo:
            return 0
        elif dd > r_hi:
            return 0
        else:
            return 1
    next_axis = (axis + 1) % 3
    d_min = bbox_lower_bound(-d, -d, -d, d, d, d, qx, qy, qz)
    d_max = bbox_upper_bound(-d, -d, -d, d, d, d, qx, qy, qz)
    c = two_point_correlation(d - 1, next_axis, qx, qy, qz, r_lo, r_hi)
    if d_min > r_hi:
        return 0
    elif d_max < r_lo:
        return 0
    else:
        return c + c


@lru_cache(maxsize=None)
def point_cloud_neighborhood(d, axis, qx, qy, qz, radius):
    if d == 0:
        dd = dist3(LEAF_X, LEAF_Y, LEAF_Z, qx, qy, qz)
        return 1 if dd <= radius else 0
    next_axis = (axis + 1) % 3
    d_min = bbox_lower_bound(-d, -d, -d, d, d, d, qx, qy, qz)
    c = point_cloud_neighborhood(d - 1, next_axis, qx, qy, qz, radius)
    if d_min > radius:
        return 0
    else:
        return c + c


@lru_cache(maxsize=None)
def photon(d, axis, phases, rays, seed, radius):
    active = 0 if phases == 0 else (0 if rays == 0 else 1)
    ox = (seed * 13) - (phases * 7)
    oy = (seed * 5) + (rays * 3)
    oz = (seed * 11) - rays
    if d == 0:
        dd = dist3(LEAF_X, LEAF_Y, LEAF_Z, ox, oy, oz)
        m_hit = 1 if dd <= radius else 0
        return active * m_hit * rays
    next_axis = (axis + 1) % 3
    split_val = d * 11 + axis
    dx = (seed * 3) - (phases * 2)
    dy = (seed * 7) - rays
    dz = (seed * 5) - (phases + rays)
    o_coord = coord_at(axis, ox, oy, oz)
    plane_dist = abs(o_coord - split_val)
    box_dist = bbox_lower_bound(-d, -d, -d, d, d, d, ox, oy, oz)
    reflected = rays // 2
    ior_i = 2 + axis
    ior_t = 1 + (split_val - (split_val // 3) * 3)
    tir = 1 if (ior_i > ior_t and (plane_dist * ior_i) > (radius * ior_t)) else 0
    refracted = 0 if tir == 1 else rays // 3
    next_rays = active * (reflected + refracted)
    next_phase = phases - 1 if phases > 0 else 0
    next_seed = seed + 17
    next_radius = radius - 3 if radius > 3 else 3
    c = photon(d - 1, next_axis, next_phase, next_rays, next_seed, next_radius)
    near, far = c, c
    m_box = 0 if box_dist > radius else 1
    m_plane = 1 if plane_dist <= radius else 0
    local = m_box * rays
    k_reflect = 2 + (axis - (axis // 2) * 2)
    k_refract = 0 if tir == 1 else 1 + (ior_t // 2)
    reflected_term = (k_reflect * near) // 3
    refracted_term = (k_refract * m_plane * far) // 3
    return active * (local + reflected_term + refracted_term)


def expected():
    dist = nearest(D0, AXIS0, 1, 2, 3)
    in_range_count = count_in_range(D0, AXIS0, -20, 20, -12, 12, -7, 7)
    mass_in_range = sum_mass_in_range(D0, AXIS0, -25, 25, -20, 20, -15, 15)
    corr_count = two_point_correlation(D0, AXIS0, 0, 0, 0, 8, 16)
    cloud_count = point_cloud_neighborhood(D0, AXIS0, 0, 0, 0, 24)
    photon_hits = photon(D0, AXIS0, 5, 16, 7, 18)
    return "'#(%d %d %d %d %d %d)" % (
        dist, in_range_count, mass_in_range, corr_count, cloud_count, photon_hits)


if __name__ == "__main__":
    print(expected())
