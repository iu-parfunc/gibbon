#!/usr/bin/env bash
# VW-09 / historical B22: end-to-end regression for loopified chunk-loop fusion.
#
# `--opt-loop-fusion` is not part of any of the four example-suite run modes,
# so nothing in the example harness exercises
# 'Gibbon.Passes.LoopifiedTraversalFusion' at all.  This script is that
# coverage.  See Note [What makes fusing two chunk loops sound] in that module.
#
# Three fixtures, each checked against an independent Python model rather than
# against recorded Gibbon output:
#
#   FusionKeyCollision.hs  two DISTINCT constructors, A' and A_, whose names
#                          differ only in a character the loop-name encoder used
#                          to collapse.  Their frequencies are 1:3, so fusing
#                          them runs the A_ buffer for the number of A'
#                          elements.  Must NOT fuse.
#   FusionKeyControl.hs    byte-identical program with the constructors renamed
#                          Ap/Aq.  Same shape, same frequencies, no name
#                          collision -- the control that shows the collision,
#                          not the shape, was the cause.
#   SkewedWidths.hs        one constructor with four different field widths
#                          beside a second constructor of a different
#                          frequency.  Fusion SHOULD fire here, over the four
#                          same-constructor buffers, and must stay correct
#                          across repeated region growth.
#
# Both the answers and the structure are checked: a fixture that merely
# produced the right number by not transforming at all would pass the first and
# fail the second.
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
S="$L --opt-selective-buffer-sharing"
V="$S --opt-vectorization"

build () { # $1=label $2=src $3=flags...
  local label=$1 src=$2; shift 2
  "$GIBBON" "$@" --no-gcc-vectorize --to-exe --cfile="$TMP/$label.c" \
      --exefile="$TMP/$label.exe" "$src" >"$TMP/$label.build" 2>&1 \
    || { echo "  FAIL  $label: compile"; fail=$((fail+1)); return 1; }
}
whiles () { grep -c 'while (' "$TMP/$1.c"; }

# ---------------------------------------------------------------- collision
echo "== VW-09 B22: a non-injective constructor key must not fuse two constructors =="
for pair in "collide:$HERE/vw09/FusionKeyCollision.hs" "control:$HERE/vw09/FusionKeyControl.hs"; do
  name=${pair%%:*}; src=${pair#*:}
  exp=$(python3 "$HERE/vw09/fusion_model.py" 40)
  for m in "loopify:$L" "loopify_fuse:$L --opt-loop-fusion" \
           "selective:$S" "selective_fuse:$S --opt-loop-fusion" \
           "vectorize:$V" "vectorize_fuse:$V --opt-loop-fusion"; do
    mode=${m%%:*}; flags=${m#*:}
    build "${name}_${mode}" "$src" $flags || continue
    chk "$name/$mode value" "$(timeout 60 "$TMP/${name}_${mode}.exe" </dev/null 2>&1)" "$exp"
  done
  # Structure: enabling fusion must not change the loop count for EITHER
  # fixture, because both hold two loops of two different constructors.
  for base in loopify selective vectorize; do
    chk "$name/$base fusion must not merge different constructors" \
        "$(whiles "${name}_${base}_fuse")" "$(whiles "${name}_${base}")"
  done
done

# ------------------------------------------------------------------- skewed
echo "== VW-09 B22: same-constructor buffers of different widths must still fuse =="
build skew_l "$HERE/vw09/SkewedWidths.hs" $L || exit 1
build skew_lf "$HERE/vw09/SkewedWidths.hs" $L --opt-loop-fusion || exit 1
wl=$(whiles skew_l); wlf=$(whiles skew_lf)
if [ "$wlf" -lt "$wl" ]; then pass=$((pass+1)); echo "  ok    fusion fired: $wl chunk loops -> $wlf"
else fail=$((fail+1)); echo "  FAIL  fusion did not fire on a legal group ($wl -> $wlf)"; fi
for n in 0 1 7 64 65 300 1000; do
  for bs in 64 1024; do
    exp=$(python3 "$HERE/vw09/skewed_model.py" answer "$n")
    chk "skew n=$n bs=$bs fused" \
        "$(timeout 60 "$TMP/skew_lf.exe" --size-param "$n" --inf-buffer-size "$bs" </dev/null 2>&1)" "$exp"
  done
done

echo "vw09_fusion: pass=$pass fail=$fail"
[ "$fail" -eq 0 ]
