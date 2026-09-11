#!/usr/bin/env bash
# VW-09: chained ScalarCountCopyAll propagation.
#
# tests/vw09/Chained{1,2,3}.hs each chain P0 (mkW, OPT:StoreScalarCounts)
# through N plain (non-annotated, non-loopified) shape-preserving producers
# into C (bumpW, OPT:MayVectorize, loopified).  Two independent facts are
# checked, neither derived from the other and neither derived from Gibbon's
# own runtime output:
#
#   copy count     exactly N `gib_scalar_count_copy_all` call sites must
#                  appear in the generated C -- one per intermediate producer,
#                  no more (no duplicate at a call site) and no fewer (no
#                  producer call left uncovered).
#   end-to-end     P0's own physical per-buffer per-chunk element writes
#   fidelity       (ground truth, instrumented at mkW's bump call sites) must
#                  equal the chunk_count the FINAL loopified consumer (bumpW)
#                  reads back, per buffer per chunk -- proving fidelity
#                  survived every hop, not just that the final program value
#                  happens to be right.
#
# See:
#   Note [Coverage must mirror emission]  (ScalarCountPropagation)
#
# Usage: gibbon-compiler/tests/vw09_chained_propagation.sh [sizes...]
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIBBON="${GIBBON_EXE:-$(find "$GIBBONDIR/dist-newstyle" -name gibbon -type f -path '*x/gibbon/build*' 2>/dev/null | head -1)}"
[ -x "$GIBBON" ] || { echo "FATAL: no gibbon executable (set GIBBON_EXE)"; exit 2; }
SIZES=${*:-"0 1 2 7 8 9 64 300 1000"}
BUFSIZES="64 128 1024"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
# Deliberately NOT --auto-loopification: p1/p2/p3 carry no OPT:MayVectorize
# annotation, so with auto-loopification they would each become their own
# self-sufficient loopified producer (writing their own output counts) and
# ScalarCountPropagation would have nothing to do -- this harness exists to
# exercise the copy path itself, so only the annotated bumpW may loopify.
MODE="--packed --use-mutable-cursors --store-scalar-field-counts --opt-loopification"
pass=0; fail=0
chk () { if [ "$2" == "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "  FAIL  $1: got [$2] want [$3]"; fi }

instrument () { # $1 = generated .c   $2 = out .c   -> prints producer buffer count
python3 - "$1" "$2" <<'PY'
import re,sys
src=open(sys.argv[1]).read()
start=None
for m in re.finditer(r'^unsigned char mkW\(', src, re.M):
    b=src.index('{', m.start()); sc=src.find(';', m.start())
    if sc == -1 or b < sc: start=m; break
if start is None: print(0); sys.exit(0)
i=src.index('{', start.start()); d=0; j=i
while True:
    if src[j]=='{': d+=1
    elif src[j]=='}':
        d-=1
        if d==0: break
    j+=1
body=src[i:j+1]

args=[]
for a in re.findall(r'gib_scalar_count_footer_bump\((\w+)\);', body):
    if a not in args: args.append(a)
if not args: print(0); sys.exit(0)
idx={a:k for k,a in enumerate(args)}
nb=len(args)
prologue = ('\nstatic long _vw09_cur[%d];\nstatic long _vw09_chunk;\n'
            'static void _vw09_flush(void){int k;for(k=0;k<%d;k++){'
            'fprintf(stderr,"VW09PROD 0 %%d %%ld %%ld\\n",k,_vw09_chunk,_vw09_cur[k]);_vw09_cur[k]=0;}'
            '_vw09_chunk++;}\n'
            '__attribute__((destructor)) static void _vw09_final(void){_vw09_flush();}\n'
            ) % (nb, nb)

nb_body = re.sub(r'gib_scalar_count_footer_bump\((\w+)\);',
                 lambda mm: '{_vw09_cur[%d]++; gib_scalar_count_footer_bump(%s);}'
                            % (idx[mm.group(1)], mm.group(1)),
                 body)
nb_body = re.sub(r'(\n\s*)(gib_grow_region\()', r'\1_vw09_flush(); \2', nb_body, count=1)
out = src[:i] + nb_body + src[j+1:]

bufs=[]
def repA(mm):
    b=mm.group(1); bufs.append(b)
    return (mm.group(0) +
            '\n long _cc_%s=(long)%s_chunk_count;'
            ' long _rem_%s=(long)(*%s_out_end_loc - *%s_out_loc);'
            ' char *_pre_%s=*%s_out_loc;' % (b,mm.group(2),b,b,b,b,b))
out = re.sub(r'gib_scalar_count_footer_set\((\w+)_current_out_end,\s*\n?\s*(\w+)_chunk_count\);',
             repA, out)
for b in bufs:
    anchor='if (%s_is_last_chunk) {' % b
    probe=('{ long _w=(long)(*%s_out_loc - _pre_%s);'
           ' fprintf(stderr,"VW09CONS 99 %s cc=%%ld written=%%ld rem=%%ld %%s\\n",'
           ' _cc_%s,_w,_rem_%s,(_w>_rem_%s)?"OVERRUN":"ok"); }\n ' % (b,b,b,b,b,b))+anchor
    if anchor in out: out=out.replace(anchor,probe,1)

k=out.index('\n', out.index('#include'))
out = out[:k] + prologue + out[k:]
open(sys.argv[2],'w').write(out)
print(nb)
PY
}

run_fixture () { # $1 = label  $2 = stem (Chained1/2/3)  $3 = hops  $4.. = flags
  local label=$1 stem=$2 hops=$3; shift 3
  local src="$HERE/vw09/$stem.hs"
  echo "== VW-09 chained propagation: $label ($hops hops) =="
  "$GIBBON" "$@" --no-gcc-vectorize --to-exe --cfile="$TMP/$label.c" \
      --exefile="$TMP/$label.exe" "$src" >"$TMP/$label.build" 2>&1
  if [ ! -s "$TMP/$label.c" ]; then
    echo "  FAIL  $label: compile"; fail=$((fail+1)); sed -n '1,10p' "$TMP/$label.build"; return
  fi
  local ncopies
  ncopies=$(grep -c 'gib_scalar_count_copy_all(' "$TMP/$label.c")
  chk "$label copy count" "$ncopies" "$hops"
  local stem="$label"

  local nb; nb=$(instrument "$TMP/$stem.c" "$TMP/${stem}i.c")
  if [ "${nb:-0}" -eq 0 ]; then
    echo "  FAIL  $stem: no producer bump sites"; fail=$((fail+1)); return
  fi
  gcc -O1 -g ${VW09_SAN:-} -I "$GIBBONDIR/gibbon-rts/build" -o "$TMP/${stem}i.exe" \
      "$TMP/${stem}i.c" "$GIBBONDIR/gibbon-rts/build/gibbon_rts.o" \
      -L "$GIBBONDIR/gibbon-rts/build" -Wl,-rpath="$GIBBONDIR/gibbon-rts/build" \
      -lgibbon_rts_ng -lm 2>"$TMP/$stem.cc" \
    || { echo "  FAIL  $stem: instrumented build"; fail=$((fail+1))
         sed -n '1,10p' "$TMP/$stem.cc"; return; }
  local n bs exp res
  for n in $SIZES; do
    for bs in $BUFSIZES; do
      ASAN_OPTIONS=detect_leaks=0 "$TMP/${stem}i.exe" --size-param "$n" \
        --inf-buffer-size "$bs" >"$TMP/$stem.out" 2>"$TMP/$stem.err"
      exp=$(python3 "$HERE/vw09/chained_model.py" answer "$hops" "$n")
      chk "$stem n=$n bs=$bs value" \
          "$(grep -oE "^'#\(.*\)$" "$TMP/$stem.out" | head -1)" "$exp"
      res=$(python3 "$HERE/vw09/chained_model.py" check "$hops" "$n" < "$TMP/$stem.err")
      chk "$stem n=$n bs=$bs fidelity" "$res" "OK"
    done
  done
}

run_fixture Chained1 Chained1 1 $MODE
run_fixture Chained2 Chained2 2 $MODE
run_fixture Chained3 Chained3 3 $MODE

# Gate D: the same 2-hop chain under every optimization interaction that
# might reorder, drop, redirect or reinterpret a copied footer sequence.
SELECTIVE="$MODE --opt-selective-buffer-sharing"
VECTORIZE="$SELECTIVE --opt-vectorization"
run_fixture Chained2_loopify_fuse   Chained2 2 $MODE --opt-loop-fusion
run_fixture Chained2_selective      Chained2 2 $SELECTIVE
run_fixture Chained2_vectorize      Chained2 2 $VECTORIZE
run_fixture Chained2_vectorize_fuse Chained2 2 $VECTORIZE --opt-loop-fusion

echo "vw09_chained_propagation: pass=$pass fail=$fail"
[ "$fail" -eq 0 ]
