#!/usr/bin/env bash
# VW-24: a parent-node scalar field computed by reading an already-built
# CHILD value (via a getter call) and then re-embedding that SAME child as
# a constructor argument.
#
#     let c0 = buildT (d - 1)
#         agg = getAgg c0
#      in Node ctl agg c0
#
# Under `--use-mutable-cursors`, Cursorize's `cursorizeLet` (the
# `isPackedTy`, non-empty-`locs` case) recovers `c0`'s start-of-value cursor
# by searching `MutableLocPtsToEnv` for an entry named after `c0`'s location
# and returning the BUCKET's own key, `l`. A derived, fixed-offset location
# (an `AfterConstantLE` location, e.g. `loc_195 = loc_194 + 4`) is registered
# by *appending its own entry into the bucket of whichever location it was
# computed from* (`cursorizeLocExp`'s `AfterConstantLE` case), cascading
# back to the enclosing function's own output-mutable location -- so `l` can
# come back as that ENCLOSING location, not `c0`'s own. The old code then
# unconditionally trusted `M.lookup l m2'` for `c0`'s start value, silently
# substituting the PARENT's own pre-write cursor. A later read through it
# landed on the parent's not-yet-written scalar fields: zeroed/uninitialized
# memory. See Note [Self-registration is not aliasing] in Cursorize.hs.
#
# Scope: this fixture set tests and fixes `--no-ran` only (RAN-enabled
# behavior is explicitly deferred future work; the CLI has no `--use-ran`
# flag to begin with). Every mode below passes `--no-ran` explicitly.
#
# Every expected value comes from tests/vw24/model.py, a closed-form
# derivation from each fixture's own source, never from Gibbon's own output.
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIBBON="${GIBBON_EXE:-$(find "$GIBBONDIR/dist-newstyle" -name gibbon -type f -path '*x/gibbon/build*' 2>/dev/null | head -1)}"
[ -x "$GIBBON" ] || { echo "FATAL: no gibbon executable (set GIBBON_EXE)"; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
pass=0; fail=0
chk () { if [ "$2" == "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "  FAIL  $1: got [$2] want [$3]"; fi }

NORAN="--packed --no-ran"
MUT="--packed --no-ran --use-mutable-cursors"
LOOP="--packed --no-ran --use-mutable-cursors --store-scalar-field-counts --opt-loopification --auto-loopification"
SEL="$LOOP --opt-selective-buffer-sharing"
VEC="$SEL --opt-vectorization"
# Names and flag strings are kept in parallel arrays: the flag strings
# contain spaces, so a single "name:flags" list would word-split into
# nonsense modes.
MODE_NAMES=(packed mut loopify selective vectorize)
MODE_FLAGS=("$NORAN" "$MUT" "$LOOP" "$SEL" "$VEC")

# $1 = fixture stem, $2 = space-separated depth list
run_fixture () {
  local stem=$1 depths=$2 i mode flags d got exp
  for i in "${!MODE_NAMES[@]}"; do
    mode=${MODE_NAMES[$i]}; flags=${MODE_FLAGS[$i]}
    for d in $depths; do
      sed "s/@D@/$d/" "$HERE/vw24/$stem.hs.in" > "$TMP/$stem.$d.hs"
      if "$GIBBON" $flags --no-gcc-vectorize --to-exe --cfile="$TMP/$stem.$mode.c" \
           --exefile="$TMP/$stem.$mode.exe" "$TMP/$stem.$d.hs" >"$TMP/$stem.$mode.build" 2>&1; then
        got=$(timeout 60 "$TMP/$stem.$mode.exe" --size-param 0 --iterate 1 </dev/null 2>&1 | tail -1)
        exp=$(python3 "$HERE/vw24/model.py" "$stem" "$d")
        chk "$stem/$mode d=$d" "$got" "$exp"
      else
        fail=$((fail+1)); echo "  FAIL  $stem/$mode d=$d: compile failed"
        head -5 "$TMP/$stem.$mode.build" | sed 's/^/        /'
      fi
    done
  done
}

echo "== VW-24: repeated child read-then-reconstruct, under --no-ran =="
run_fixture oneChild        "0 1 2 3 6"     # depth>=1 is the reproducer; depth 0 is a leaf (trivially safe)
run_fixture twoChild        "0 1 2 3 8"
run_fixture eightChild      "0 1 2 3 5"     # 8^5 = 32768 nodes: a genuinely larger, still-fast depth
run_fixture oneChildSoA     "0 1 2 3 6"     # Factored/SoA layout
run_fixture oneChildInt64   "0 1 2 3 6"     # bare Int (Int64) fields, pre-BW-03 style
run_fixture leafOnlyControl "0 1 2 3 6"     # never reads the cached field -- must stay correct

echo
echo "== VW-24: $pass passed, $fail failed =="
[ "$fail" -eq 0 ]
