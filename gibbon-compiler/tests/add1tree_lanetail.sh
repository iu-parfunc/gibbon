#!/usr/bin/env bash
# BW-01: lane-tail and chunk-boundary correctness for
# add1Tree's kernel shape (x+1, MayVectorize, order-sensitive checksum).
#
# Add1TreeIntN.hs's own committed workload is a Fibonacci-shaped tree,
# whose leaf counts are restricted to the Fibonacci sequence and so cannot
# hit an exact lane boundary. This script instead compiles
# tests/add1tree/Add1ListChunks.hs.in (list-shaped, same map/checksum
# shape, exact node count controllable via --size-param) at every width
# and exercises node counts around that width's own SIMD lane boundary:
# lanes-1, lanes, lanes+1, 2*lanes-1, 2*lanes, 2*lanes+1, plus the shared
# 0/1 edge cases -- proving vector_elements + scalar_tail_elements equals
# the logical element count at every one of those sizes, checked against
# an independent Python oracle, not inferred from the final output alone
# (a wrong final value would already prove a bug; passing at every size
# individually is the stronger claim).
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
ROOT="$GIBBONDIR/gibbon-compiler"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
MODE="--use-mutable-cursors --store-scalar-field-counts --opt-loopification --auto-loopification --opt-vectorization"
pass=0; fail=0

ORACLE='
import sys, ctypes
w, n = int(sys.argv[1]), int(sys.argv[2])
ctype = {8: ctypes.c_int8, 16: ctypes.c_int16, 32: ctypes.c_int32, 64: ctypes.c_int64}[w]
def wrap(x): return ctype(x).value
def tdiv(a, b):
    q = abs(a) // abs(b)
    return -q if (a < 0) != (b < 0) else q
def tmod(a, b):
    return a - b * tdiv(a, b)
seed = 1
h = 0
for _ in range(n):
    x = tmod(seed, 64) - 32
    seed = seed * 2 + 1
    h = wrap(wrap(h * 31) + wrap(x + 1))
print(h)
'

run_one () { # $1=width $2=n
  local w="$1"
  local n="$2"
  local label="W${w}_n${n}"
  local src="$TMP/${label}.hs"
  sed "s/@W@/$w/g" "$ROOT/tests/add1tree/Add1ListChunks.hs.in" > "$src"
  gibbon --to-exe --packed $MODE \
        --cfile="$TMP/$label.c" --exefile="$TMP/$label.exe" "$src" >"$TMP/$label.build" 2>&1
  if [ ! -x "$TMP/$label.exe" ]; then
    echo "  FAIL  $label: compile failed"; cat "$TMP/$label.build" | tail -5; fail=$((fail+1)); return
  fi
  local exp out val
  exp=$(python3 -c "$ORACLE" "$w" "$n")
  out=$("$TMP/$label.exe" --size-param "$n" 2>&1)
  val=$(echo "$out" | tail -1)
  if [ "$val" = "$exp" ]; then
    echo "  PASS  $label  value=$val"; pass=$((pass+1))
  else
    echo "  FAIL  $label  value=$val (expected $exp)"; fail=$((fail+1))
  fi
}

for W in 8 16 32 64; do
  case $W in
    8)  LANES=16 ;;
    16) LANES=8  ;;
    32) LANES=4  ;;
    64) LANES=2  ;;
  esac
  for N in 0 1 $((LANES-1)) $LANES $((LANES+1)) $((2*LANES-1)) $((2*LANES)) $((2*LANES+1)); do
    run_one "$W" "$N"
  done
done

echo "add1tree lane-tail/chunk-boundary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
