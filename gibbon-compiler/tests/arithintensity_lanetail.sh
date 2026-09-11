#!/usr/bin/env bash
# BW-02: lane-tail and chunk-boundary correctness for
# ArithmeticIntensityIntN.hs's nonlinear mixing kernel (a=x*x+c1; b=a*a-x;
# c=b*x+a; d=c*c-b; result=d+x), MayVectorize, order-sensitive checksum.
#
# Same rationale as add1tree_lanetail.sh (the committed workload's
# Fibonacci-shaped tree cannot hit an exact lane boundary): this script
# compiles tests/add1tree/ArithIntensityListChunks.hs.in (list-shaped,
# same kernel/checksum shape, exact node count controllable via
# --size-param) at W8/W16/W32 (genuine packed multiply widths) and W64
# (scalar-only -- Gibbon SIMD is never enabled here, matching this
# benchmark's policy of not exercising W64 through the legacy
# lane-spilling multiply helper), exercising node counts around each
# width's own SIMD lane boundary: lanes-1, lanes, lanes+1, 2*lanes-1,
# 2*lanes, 2*lanes+1, plus the shared 0/1 edge cases -- proving
# lanes*vector_iterations + scalar_tail_iterations equals the logical
# element count at every one of those sizes, checked against an
# independent Python oracle that wraps after every operation in exact
# source-expression order.
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
ROOT="$GIBBONDIR/gibbon-compiler"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
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
C1 = 17
def kernel(x):
    a = wrap(wrap(x * x) + C1)
    b = wrap(wrap(a * a) - x)
    c = wrap(wrap(b * x) + a)
    d = wrap(wrap(c * c) - b)
    return wrap(d + x)
seed = 1
h = 0
for _ in range(n):
    x = tmod(seed, 64) - 32
    seed = seed * 2 + 1
    h = wrap(wrap(h * 31) + kernel(x))
print(h)
'

run_one () { # $1=width $2=n $3=mode-flags
  local w="$1"
  local n="$2"
  local mode="$3"
  local label="W${w}_n${n}"
  local src="$TMP/${label}.hs"
  sed "s/@W@/$w/g" "$ROOT/tests/add1tree/ArithIntensityListChunks.hs.in" > "$src"
  gibbon --to-exe --packed --use-mutable-cursors $mode \
        --cfile="$TMP/$label.c" --exefile="$TMP/$label.exe" "$src" >"$TMP/$label.build" 2>&1
  if [ ! -x "$TMP/$label.exe" ]; then
    echo "  FAIL  $label: compile failed"; tail -5 "$TMP/$label.build"; fail=$((fail+1)); return
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

# W8/16/32: genuine SIMD widths -- loopified + Gibbon vectorization enabled.
SIMD_MODE="--store-scalar-field-counts --opt-loopification --auto-loopification --opt-vectorization"
for W in 8 16 32; do
  case $W in
    8)  LANES=16 ;;
    16) LANES=8  ;;
    32) LANES=4  ;;
  esac
  for N in 0 1 $((LANES-1)) $LANES $((LANES+1)) $((2*LANES-1)) $((2*LANES)) $((2*LANES+1)); do
    run_one "$W" "$N" "$SIMD_MODE"
  done
done

# W64: scalar-loopify ONLY -- --opt-vectorization deliberately NEVER
# passed here, per this benchmark's policy of not exercising W64 through
# the legacy scalar-spilling multiply helper (BUGS.md's W64-multiply
# note; see also VW-18/VW-19's adjacent deferred packed-W64 gaps).
SCALAR_MODE="--store-scalar-field-counts --opt-loopification --auto-loopification --no-gcc-vectorize"
for N in 0 1 1 2 3 3 4 5; do
  run_one "64" "$N" "$SCALAR_MODE"
done

echo "arithintensity lane-tail/chunk-boundary: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
