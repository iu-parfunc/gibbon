#!/usr/bin/env bash
# VW-07: re-measure the output-capacity invariant of synthesized chunk loops.
#
# See Note [Output capacity in synthesized chunk loops] in
# src/Gibbon/Passes/LoopifyTraversals.hs.  Loopification removes the per-node
# BoundsCheck and replaces it with a per-output-buffer chunk discipline whose
# trip count comes from the INPUT footer while the capacity comes from the
# OUTPUT region.  Nothing tests one against the other at runtime, so this script
# measures the relationship directly:
#
#   for every output buffer, in every chunk:
#       bytes actually written  ==  chunk_count * width(buffer)
#                               <=  usable capacity of the output chunk
#
# It instruments the GENERATED C (never the compiler or the RTS) by recording
# the output cursor before and after each chunk's inner loops, so the byte count
# is measured rather than assumed.  Values are also checked against an
# independent Python oracle.
#
# Usage: gibbon-compiler/tests/vw07_output_capacity.sh [sizes...]
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
ROOT="$GIBBONDIR/gibbon-compiler"
SIZES=${*:-"1 17 33 1025 3000"}
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
MODE="--use-mutable-cursors --store-scalar-field-counts --opt-loopification --auto-loopification --opt-selective-buffer-sharing --opt-vectorization"
pass=0; fail=0

instrument () { # $1 = generated .c   $2 = out .c
python3 - "$1" "$2" <<'PY'
import re,sys
src=open(sys.argv[1]).read()
bufs=[]
a=re.compile(r'gib_scalar_count_footer_set\((\w+)_current_out_end,\s*\n?\s*(\w+)_chunk_count\);')
def repA(m):
    buf=m.group(1); bufs.append(buf)
    return (m.group(0) + '\n long _cc_%s=(long)%s_chunk_count;'
            ' long _rem_%s=(long)(*%s_out_end_loc - *%s_out_loc);'
            ' char *_pre_%s=*%s_out_loc;' % (buf,m.group(2),buf,buf,buf,buf,buf))
out=a.sub(repA,src)
for b in bufs:
    anchor='if (%s_is_last_chunk) {' % b
    probe=('{ long _w=(long)(*%s_out_loc - _pre_%s);'
           ' fprintf(stderr,"VW07CAP %s cc=%%ld written=%%ld rem=%%ld %%s\\n",'
           ' _cc_%s,_w,_rem_%s,(_w>_rem_%s)?"OVERRUN":"ok"); }\n ' % (b,b,b,b,b,b))+anchor
    if anchor in out: out=out.replace(anchor,probe,1)
open(sys.argv[2],'w').write(out)
print(len(bufs))
PY
}

run_case () { # $1=label  $2=source  $3=oracle-python
  local label=$1 src=$2 oracle=$3
  gibbon --to-exe --packed $MODE --optc " -O1 -g " \
        --cfile="$TMP/$label.c" --exefile="$TMP/$label.exe" "$src" >"$TMP/$label.build" 2>&1
  if [ ! -s "$TMP/$label.c" ]; then echo "  FAIL  $label: compile failed"; fail=$((fail+1)); return; fi
  local nb; nb=$(instrument "$TMP/$label.c" "$TMP/${label}i.c")
  if [ "${nb:-0}" -eq 0 ]; then
    echo "  FAIL  $label: no synthesized chunk loop found -- did loopification stop firing?"
    fail=$((fail+1)); return
  fi
  gcc -O1 -g -fsanitize=address -I "$GIBBONDIR/gibbon-rts/build" -o "$TMP/${label}i.exe" \
      "$TMP/${label}i.c" "$GIBBONDIR/gibbon-rts/build/gibbon_rts.o" \
      -L "$GIBBONDIR/gibbon-rts/build" -Wl,-rpath="$GIBBONDIR/gibbon-rts/build" \
      -lgibbon_rts_ng -lm 2>"$TMP/$label.cc" || { echo "  FAIL  $label: cc"; fail=$((fail+1)); return; }
  for n in $SIZES; do
    local exp out val over asan
    exp=$(python3 -c "$oracle" "$n")
    out=$(ASAN_OPTIONS=detect_leaks=0 "$TMP/${label}i.exe" --size-param "$n" 2>&1)
    val=$(echo "$out" | grep -oE '^-?[0-9]+$' | head -1)
    over=$(echo "$out" | grep -c OVERRUN)
    asan=$(echo "$out" | grep -c AddressSanitizer)
    if [ "$val" = "$exp" ] && [ "$over" -eq 0 ] && [ "$asan" -eq 0 ]; then
      echo "  PASS  $label n=$n  buffers=$nb  value=$val  no overrun"; pass=$((pass+1))
    else
      echo "  FAIL  $label n=$n  value=$val (expected $exp) overruns=$over asan=$asan"
      echo "$out" | grep -E 'OVERRUN|AddressSanitizer' | head -3
      fail=$((fail+1))
    fi
  done
}

SINGLE_ORACLE='import sys;n=int(sys.argv[1]);print(sum((k%50)+1 for k in range(1,n+1)))'
MIXED_ORACLE='import sys
n=int(sys.argv[1]);t=0
for k in range(1,n+1):
    a=(k%50+3)&0xff;  a=a-256 if a>127 else a
    b=(k%300-2)&0xffff; b=b-65536 if b>32767 else b
    c=(k%1000*2)&0xffffffff; c=c-(1<<32) if c>=(1<<31) else c
    t+=a+b+c+(k%7+1)
print(t)'

for W in 8 16 32 64; do
  sed "s/@W@/$W/g" "$ROOT/tests/vw07/SingleWidthChunks.hs.in" > "$TMP/W$W.hs"
  run_case "W$W" "$TMP/W$W.hs" "$SINGLE_ORACLE"
done
run_case "mixed" "$ROOT/tests/vw07/MixedWidthChunks.hs" "$MIXED_ORACLE"

echo "VW-07 output-capacity: $pass passed, $fail failed"
[ "$fail" -eq 0 ]
