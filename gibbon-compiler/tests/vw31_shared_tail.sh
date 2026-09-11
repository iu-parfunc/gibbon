#!/usr/bin/env bash
# VW-31: a let-bound recursive packed tail consumed by more than one
# constructor alternative.
#
#     let rst = mkT (n - 1)
#      in if p then A x rst else B y rst
#
# The recursive call is written ONCE.  Two things used to go wrong, and this
# script pins both plus the boundary of what the representation can express.
#
#   1. `reorderScalarWrites` lifted BOTH arms' tag writes out of the
#      conditional and sequenced them, so the second arm's tag always won.
#      See Note [A hoisted write must be control-independent].
#   2. `removeCopies` turned an IDENTITY copy (source location == destination)
#      into an indirection pointing at itself, which destroys the value and
#      makes the RTS write barrier spin.
#      See Note [An identity copy is not an indirection].
#
# What the representation genuinely cannot do -- re-place a value onto storage
# that overlaps the value itself -- is now refused before code generation.
# See Note [A value cannot be re-placed onto itself].  The `gap16` fixture is
# the control for that boundary: identical program, but the alternatives are
# far enough apart that an indirection fits, and it compiles and runs.
#
# Every expected value comes from tests/vw31/model.py, derived from the source
# definitions, never recorded from Gibbon.
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIBBON="${GIBBON_EXE:-$(find "$GIBBONDIR/dist-newstyle" -name gibbon -type f -path '*x/gibbon/build*' 2>/dev/null | head -1)}"
[ -x "$GIBBON" ] || { echo "FATAL: no gibbon executable (set GIBBON_EXE)"; exit 2; }
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
pass=0; fail=0
chk () { if [ "$2" == "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "  FAIL  $1: got [$2] want [$3]"; fi }

P="--packed"
MUT="--packed --use-mutable-cursors"
NORAN="--packed --no-ran"
LOOP="--packed --use-mutable-cursors --store-scalar-field-counts --opt-loopification --auto-loopification"
VEC="$LOOP --opt-selective-buffer-sharing --opt-vectorization"
# Names and flag strings are kept in parallel arrays: the flag strings contain
# spaces, so a single "name:flags" list would word-split into nonsense modes.
MODE_NAMES=(packed mut noran loopify vectorize)
MODE_FLAGS=("$P" "$MUT" "$NORAN" "$LOOP" "$VEC")

# $1 = fixture stem
# $2 = expected outcome:
#        ok        -- must compile and match the model in every mode
#        reject    -- must be refused before code generation in every mode
#        indirect  -- supported only where the copy becomes an indirection, i.e.
#                     without --use-mutable-cursors; refused where the copy is
#                     kept, because that ABI does not implement a same-region
#                     re-placement correctly (measured: it returns a value with
#                     an unknown tag).
run_fixture () {
  local stem=$1 spec=$2 want i mode flags n got exp
  for i in "${!MODE_NAMES[@]}"; do
    mode=${MODE_NAMES[$i]}; flags=${MODE_FLAGS[$i]}
    case "$spec" in
      indirect) case "$mode" in packed|noran) want=ok ;; *) want=reject ;; esac ;;
      *)        want=$spec ;;
    esac
    for n in 0 1 2 3 5 9; do
      sed "s/@N@/$n/" "$HERE/vw31/$stem.hs.in" > "$TMP/$stem.$n.hs"
      if "$GIBBON" $flags --no-gcc-vectorize --to-exe --cfile="$TMP/$stem.c" \
           --exefile="$TMP/$stem.exe" "$TMP/$stem.$n.hs" >"$TMP/$stem.build" 2>&1; then
        if [ "$want" = reject ]; then
          fail=$((fail+1)); echo "  FAIL  $stem/$mode n=$n: compiled, expected a refusal"
          continue
        fi
        got=$(timeout 30 "$TMP/$stem.exe" </dev/null 2>&1)
        exp=$(python3 "$HERE/vw31/model.py" "$stem" "$n")
        chk "$stem/$mode n=$n" "$got" "$exp"
      else
        if [ "$want" = reject ]; then
          chk "$stem/$mode n=$n refused before codegen" \
              "$(grep -c 'Gibbon cannot re-place this packed value' "$TMP/$stem.build")" "1"
        else
          fail=$((fail+1)); echo "  FAIL  $stem/$mode n=$n: compile failed"
          head -3 "$TMP/$stem.build" | sed 's/^/        /'
        fi
      fi
    done
  done
}

echo "== VW-31: shared let-bound tail, alternatives that agree on the field layout =="
run_fixture sharedTail ok        # the canonical reproducer
run_fixture revOrder   ok        # branch order reversed
run_fixture threeWay   ok        # three constructor alternatives
run_fixture oneArm     ok        # only one arm consumes the binding
run_fixture oneCtor    ok        # the passing one-constructor control

echo "== VW-31: alternatives that disagree, so the value must be re-placed =="
run_fixture gap16      indirect  # far enough apart that an indirection fits
run_fixture gap8       reject    # 8 bytes apart: an indirection would overlap

# Evaluation count: the producing call must appear ONCE in the generated C for
# the shared-tail fixture and TWICE for the duplicated-call control, and both
# must build a list of exactly n nodes -- so "one call site" is not being
# confused with "one call".
echo "== VW-31: the recursive producer is not duplicated =="
sed "s/@N@/9/" "$HERE/vw31/sharedTail.hs.in" > "$TMP/ec_shared.hs"
sed "s/@N@/9/" "$HERE/vw31/dupCall.hs.in"    > "$TMP/ec_dup.hs"
for v in shared dup; do
  "$GIBBON" $P --no-gcc-vectorize --to-exe --cfile="$TMP/ec_$v.c" \
      --exefile="$TMP/ec_$v.exe" "$TMP/ec_$v.hs" >"$TMP/ec_$v.build" 2>&1 \
    || { echo "  FAIL  evalcount/$v: compile"; fail=$((fail+1)); continue; }
done
# Recursive call sites = every mention of mkT( in the generated C, minus the
# two declarations (prototype and definition) and minus the one call from
# gibbon_main.  Counting mentions alone would report 4 and 5.
reccalls () {
  local f=$1 total decls main
  total=$(grep -c 'mkT(' "$f")
  decls=$(grep -cE '^[A-Za-z].*mkT\(' "$f")
  main=$(grep -cE '= *mkT\(' "$f")
  echo $((total - decls - main))
}
chk "one recursive call site in the shared-tail form" "$(reccalls "$TMP/ec_shared.c")" "1"
chk "two recursive call sites in the duplicated-call control" "$(reccalls "$TMP/ec_dup.c")" "2"
chk "and both build the same 9-node list" \
    "$(timeout 30 "$TMP/ec_shared.exe" </dev/null 2>&1)" \
    "$(timeout 30 "$TMP/ec_dup.exe" </dev/null 2>&1)"
chk "which is what the model says" \
    "$(timeout 30 "$TMP/ec_shared.exe" </dev/null 2>&1)" \
    "$(python3 "$HERE/vw31/model.py" sharedTail 9)"

echo "vw31_shared_tail: pass=$pass fail=$fail"
[ "$fail" -eq 0 ]
