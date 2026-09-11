#!/usr/bin/env python3
"""Independent expected-value oracle for the VW-24 fixtures. Every fixture
is a symmetric tree (all children built identically), so each has a closed
form -- derived directly from the fixture source, never from Gibbon's own
output.

  oneChild/oneChildSoA/oneChildInt64: `agg` is a pure identity chain
    (agg = getAgg c0 at every level, leaf = 7), so the root's agg is always
    7, at any depth.
  twoChild: agg(d) = 2 * agg(d-1), agg(0) = 7  =>  agg(d) = 7 * 2**d.
  eightChild: agg(d) = 8 * agg(d-1), agg(0) = 7  =>  agg(d) = 7 * 8**d.
  leafOnlyControl: sumLeaves never reads the cached `agg` field at all, so
    it is always exactly the leaf value (7), at any depth -- the pre-fix
    and post-fix behavior must be identical here.
"""
import sys

STEM = sys.argv[1]
D = int(sys.argv[2])

if STEM in ("oneChild", "oneChildSoA", "oneChildInt64", "leafOnlyControl"):
    print(7)
elif STEM == "twoChild":
    print(7 * (2 ** D))
elif STEM == "eightChild":
    print(7 * (8 ** D))
else:
    sys.exit("model.py: unknown fixture stem %r" % STEM)
