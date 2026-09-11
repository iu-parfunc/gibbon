#!/usr/bin/env bash
# VW-09 step 3: who owns a scalar-count footer chain after selective buffer
# sharing, and does anything mutate metadata the input still owns.
#
# `SharedOwnership.hs` changes exactly one of N's four scalar buffers.  The
# other three, S's buffer and the dcon stream are pure copies, so
# 'Gibbon.Passes.SelectiveBufferSharing' replaces each of their chunk loops
# with one `WriteCursorSelectiveIndirection`.  The program then reads the
# output AND, afterwards, the still-live input, so a shared buffer whose count
# metadata had been rewritten in place would show up as a wrong input sum.
#
# What is checked:
#   * the value, against an independent Python model, in every mode
#   * that sharing actually happened (indirections present, loops removed)
#   * that a shared buffer gets NO fresh count stamped -- structurally, the
#     ScalarCountSet and GrowRegion for a shared buffer are dropped rather than
#     redirected (`shouldDropLoopBind` in SelectiveBufferSharing), so the count
#     the consumer reads is the input's own, reached through the indirection
#   * that the fresh buffer keeps one, and only one, count sequence
#   * ASan + UBSan clean, since sharing is where a use-after-free or an
#     out-of-bounds read of a peer buffer would appear
#   * a non-selective control, so "correct" is not just "nothing happened"
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIBBON="${GIBBON_EXE:-$(find "$GIBBONDIR/dist-newstyle" -name gibbon -type f -path '*x/gibbon/build*' 2>/dev/null | head -1)}"
[ -x "$GIBBON" ] || { echo "FATAL: no gibbon executable (set GIBBON_EXE)"; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
SRC="$HERE/vw09/SharedOwnership.hs"
pass=0; fail=0
chk () { if [ "$2" == "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "  FAIL  $1: got [$2] want [$3]"; fi }

L="--packed --use-mutable-cursors --store-scalar-field-counts --opt-loopification --auto-loopification"
S="$L --opt-selective-buffer-sharing"
V="$S --opt-vectorization"
build () { local label=$1; shift
  "$GIBBON" "$@" --no-gcc-vectorize --to-exe --cfile="$TMP/$label.c" \
      --exefile="$TMP/$label.exe" "$SRC" >"$TMP/$label.build" 2>&1 \
    || { echo "  FAIL  $label: compile"; fail=$((fail+1)); return 1; }
}

echo "== VW-09 selective-sharing count ownership =="
for m in "loopify:$L" "selective:$S" "selective_fuse:$S --opt-loop-fusion" \
         "vectorize:$V" "vectorize_fuse:$V --opt-loop-fusion"; do
  mode=${m%%:*}; flags=${m#*:}
  build "$mode" $flags || continue
  for n in 0 1 7 64 300 1000; do
    for bs in 64 1024; do
      chk "$mode n=$n bs=$bs" \
          "$(timeout 60 "$TMP/$mode.exe" --size-param "$n" --inf-buffer-size "$bs" </dev/null 2>&1)" \
          "$(python3 "$HERE/vw09/shared_model.py" "$n")"
    done
  done
done

# Sharing must actually have fired, and must have removed loops rather than
# left them beside an indirection.
ind=$(grep -c 'gib_write_selective_indirection\|SELECTIVE' "$TMP/selective.c" || true)
wl=$(grep -c 'while (' "$TMP/loopify.c"); ws=$(grep -c 'while (' "$TMP/selective.c")
if [ "$ind" -gt 0 ]; then pass=$((pass+1)); echo "  ok    selective indirections emitted: $ind"
else fail=$((fail+1)); echo "  FAIL  no selective indirection emitted -- sharing did not fire"; fi
if [ "$ws" -lt "$wl" ]; then pass=$((pass+1)); echo "  ok    shared buffers' chunk loops removed: $wl -> $ws"
else fail=$((fail+1)); echo "  FAIL  sharing left every chunk loop in place ($wl -> $ws)"; fi
# One count stamp per surviving loop: a shared buffer must not also stamp one.
sets=$(grep -c 'gib_scalar_count_footer_set' "$TMP/selective.c")
chk "one count stamp per surviving chunk loop" "$sets" "$ws"

# Sanitizers, where an ownership mistake would surface as a use-after-free or
# an out-of-bounds read of a peer buffer.
#
# AddressSanitizer must be silent outright: a use-after-free or an
# out-of-bounds read of a peer buffer is exactly what a count-ownership mistake
# would look like, and neither is acceptable.
#
# UBSan is checked by CLASS rather than by count.  Gibbon's packed
# representation stores and loads multi-byte fields at byte-granular cursor
# offsets, so `misaligned address` findings appear in every packed build --
# already in a plain `--packed` one, before loopification or sharing exist.
# Selective sharing adds misaligned *stores* on top of the plain build's
# misaligned *loads*, because `WriteCursorSelectiveIndirection` writes a tag
# plus two pointers plus a mask at `dst + 1`; that is the same known property
# of the layout, not new behaviour.  Requiring zero would mean deleting a real
# check or keeping a permanently red one, so the check is: no ASan finding at
# all, and no UBSan finding outside the misaligned-access class.  A genuinely
# new kind of undefined behaviour still fails.
sanbuild () { # $1=label  $2..=gibbon flags
  local label=$1; shift
  "$GIBBON" "$@" --no-gcc-vectorize --to-exe --cfile="$TMP/$label.c" \
      --exefile="$TMP/$label.tmp" "$SRC" >/dev/null 2>&1
  gcc -O1 -g -fsanitize=address,undefined -I "$GIBBONDIR/gibbon-rts/build" \
      -o "$TMP/$label.exe" "$TMP/$label.c" "$GIBBONDIR/gibbon-rts/build/gibbon_rts.o" \
      -L "$GIBBONDIR/gibbon-rts/build" -Wl,-rpath="$GIBBONDIR/gibbon-rts/build" \
      -lgibbon_rts_ng -lm 2>"$TMP/$label.cc"
}
kinds () { grep -oE 'runtime error: [a-z ]+' "$1" | sort -u; }

if sanbuild sanctl --packed && sanbuild sanshr $V; then
  for n in 7 300 1000; do
    for label in sanctl sanshr; do
      ASAN_OPTIONS=detect_leaks=0 "$TMP/$label.exe" --size-param "$n" \
        --inf-buffer-size 64 >"$TMP/$label.$n.out" 2>"$TMP/$label.$n.err"
    done
    chk "sanitizer n=$n value" "$(grep -oE "^'#\(.*\)$" "$TMP/sanshr.$n.out" | head -1)" \
        "$(python3 "$HERE/vw09/shared_model.py" "$n")"
    chk "sanitizer n=$n no AddressSanitizer finding" \
        "$(grep -c AddressSanitizer "$TMP/sanshr.$n.err")" "0"
    extra=$(kinds "$TMP/sanshr.$n.err" | grep -v 'misaligned address' || true)
    chk "sanitizer n=$n no UB outside the known misaligned-access class" "$extra" ""
    # ...and the plain --packed control shows the same class, so this is not a
    # check that would pass by the program having done nothing.
    chk "sanitizer n=$n control shows the same known class" \
        "$(kinds "$TMP/sanctl.$n.err" | grep -c 'misaligned address')" "1"
  done
else
  echo "  FAIL  sanitizer build"; fail=$((fail+1))
fi

echo "vw09_sharing_ownership: pass=$pass fail=$fail"
[ "$fail" -eq 0 ]
