#!/usr/bin/env bash
# VW-09 step 5: a genuinely-zero count versus metadata that was never
# established, end to end.
#
# Case A -- GENUINE ZERO.  `SkewedWidths.hs` with a small initial chunk size
#   produces a first chunk that really holds zero elements for every buffer:
#   the first node does not fit, so the region grows before anything is
#   written.  The count is established and is 0, and the loopified consumer
#   must run zero iterations for that chunk and still produce the right answer.
#   n=0 is the other genuine zero: an empty value.
#
# Case B -- UNTRUSTED.  `UntrustedMetadata.hs` maps over a value returned by
#   `readPackedFile`.  Nothing established that value's footers, and an
#   untouched footer reads back as 0 through `gib_scalar_count_footer_get` --
#   indistinguishable, at the value, from case A.  The distinction is made at
#   the loopification GATE: `countGuaranteedTyCons` reports only types whose
#   every producer establishes counts, and `unattributedPackedTyCons` makes a
#   `readPackedFile` result disqualify its type.
#
#   That gate is covered as a unit test
#   (`case_vw09_readPackedFile_producer_disqualifies_the_type` in
#   tests/ScalarCountPropagation.hs), NOT end to end, because case B cannot be
#   compiled at all today: `readPackedFile` does not survive `InferRegionScope`
#   in the packed backend, which fails with an internal
#   "Map lookup failed on key: MMapR" for AoS and SoA alike -- see VW-32.  The
#   in-tree example `examples/test_writepacked.hs` fails the same way and is
#   not registered in the suite, which is why nothing caught it.
#
#   What this script asserts for case B is therefore the honest, currently
#   observable property: the compiler REFUSES IT LOUDLY rather than silently
#   producing an empty result.  The check is written so that it changes shape
#   the day VW-32 is fixed -- which is exactly when the end-to-end gate
#   evidence becomes constructible and must be demanded instead.
#
# Case A is checked end to end: a chunk loop is emitted, a genuinely zero
# chunk is walked, and the answer matches an independent model.
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIBBON="${GIBBON_EXE:-$(find "$GIBBONDIR/dist-newstyle" -name gibbon -type f -path '*x/gibbon/build*' 2>/dev/null | head -1)}"
[ -x "$GIBBON" ] || { echo "FATAL: no gibbon executable (set GIBBON_EXE)"; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
pass=0; fail=0
chk () { if [ "$2" == "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "  FAIL  $1: got [$2] want [$3]"; fi }
L="--packed --use-mutable-cursors --store-scalar-field-counts --opt-loopification --auto-loopification"

echo "== VW-09: genuine zero vs untouched metadata =="

# ---- Case A: counts established, some of them genuinely zero.
"$GIBBON" $L --no-gcc-vectorize --to-exe --cfile="$TMP/a.c" --exefile="$TMP/a.exe" \
    "$HERE/vw09/SkewedWidths.hs" >"$TMP/a.build" 2>&1 || { echo "FATAL: case A compile"; exit 2; }
wa=$(grep -c 'while (' "$TMP/a.c")
if [ "$wa" -gt 0 ]; then pass=$((pass+1)); echo "  ok    case A loopified: $wa chunk loops"
else fail=$((fail+1)); echo "  FAIL  case A did not loopify -- the control is not a control"; fi
for n in 0 1 7 64; do
  chk "case A n=$n (first chunk holds zero elements)" \
      "$(timeout 60 "$TMP/a.exe" --size-param "$n" --inf-buffer-size 64 </dev/null 2>&1)" \
      "$(python3 "$HERE/vw09/skewed_model.py" answer "$n")"
done

# ---- Case B: counts never established.
#
# Today this cannot reach the loopification gate at all (VW-32).  Assert the
# loud refusal, and assert that no C was produced -- so nothing silently ran on
# untouched metadata.  If compilation ever succeeds, VW-32 has been fixed and
# the branch below demands the end-to-end gate evidence the fixture was written
# for instead.
cd "$TMP"
if "$GIBBON" $L --no-gcc-vectorize --to-exe --cfile="$TMP/b.c" --exefile="$TMP/b.exe" \
      "$HERE/vw09/UntrustedMetadata.hs" >"$TMP/b.build" 2>&1; then
  echo "  NOTE  case B now compiles -- VW-32 appears fixed; demanding the gate."
  wb=$(grep -c 'while (' "$TMP/b.c")
  scb=$(grep -c 'gib_scalar_count_footer_set' "$TMP/b.c")
  chk "case B emits no synthesized chunk loop" "$wb" "0"
  chk "case B stamps no output chunk count"    "$scb" "0"
  chk "case B value (recursive traversal)" \
      "$(timeout 60 "$TMP/b.exe" </dev/null 2>&1 | grep -oE "^.#\\(.*\\)$" | head -1)" \
      "$(python3 "$HERE/vw09/untrusted_model.py")"
  if [ "$wa" -gt 0 ] && [ "$wb" -eq 0 ]; then
    pass=$((pass+1)); echo "  ok    the gate separates the two cases: A loopifies, B does not"
  else
    fail=$((fail+1)); echo "  FAIL  the two cases were treated alike (A=$wa loops, B=$wb loops)"
  fi
else
  chk "case B is refused loudly, before codegen (VW-32)" \
      "$(grep -c 'Map lookup failed on key: MMapR' "$TMP/b.build")" "1"
  chk "case B produced no C" "$([ -s "$TMP/b.c" ] && echo nonempty || echo empty)" "empty"
  echo "  ok    case B cannot reach the loopification gate: readPackedFile fails"
  echo "        in InferRegionScope (VW-32).  The gate itself is covered by"
  echo "        case_vw09_readPackedFile_producer_disqualifies_the_type."
fi

echo "vw09_untouched_vs_zero: pass=$pass fail=$fail"
[ "$fail" -eq 0 ]
