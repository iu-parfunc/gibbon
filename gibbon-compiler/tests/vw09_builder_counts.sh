#!/usr/bin/env bash
# VW-09 / historical B19-B22: measure scalar-count footer metadata against the
# physical writes that produced it, per buffer and per chunk, on a real
# Factored builder with skewed constructor frequencies and mixed field widths.
#
# See:
#   Note [The loop-name constructor key must be injective]  (LoopifyTraversals)
#   Note [What makes fusing two chunk loops sound]          (LoopifiedTraversalFusion)
#   Note [Output capacity in synthesized chunk loops]       (LoopifyTraversals, VW-07)
#
# Two INDEPENDENT mechanisms are compared, so neither is derived from the other:
#
#   producer side  every `gib_scalar_count_footer_bump` call site in the
#                  GENERATED C is one physical element written to one buffer.
#                  Counting those calls, reset at every region growth, yields
#                  the true number of elements each buffer received in each
#                  physical chunk.
#   consumer side  the loopified traversal reads `chunk_count` back out of the
#                  input footer chain, one value per chunk per buffer.  That is
#                  the STORED metadata.
#
# The check is  stored[buffer][chunk] == physical[buffer][chunk]  for every
# buffer and chunk, plus the VW-07 byte relationship
#   bytes_written == chunk_count * width <= capacity
# measured from the output cursor rather than assumed.
#
# Only the generated C is instrumented; the compiler and the RTS are untouched.
#
# Usage: gibbon-compiler/tests/vw09_builder_counts.sh [sizes...]
set -u
: "${GIBBONDIR:=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)}"
export GIBBONDIR
HERE="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
GIBBON="${GIBBON_EXE:-$(find "$GIBBONDIR/dist-newstyle" -name gibbon -type f -path '*x/gibbon/build*' 2>/dev/null | head -1)}"
[ -x "$GIBBON" ] || { echo "FATAL: no gibbon executable (set GIBBON_EXE)"; exit 2; }
SIZES=${*:-"0 1 2 7 8 9 64 65 300 1000"}
BUFSIZES="64 128 1024"
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
SRC="$HERE/vw09/SkewedWidths.hs"
MODE="--packed --use-mutable-cursors --store-scalar-field-counts --opt-loopification --auto-loopification"
pass=0; fail=0
chk () { if [ "$2" == "$3" ]; then pass=$((pass+1)); else fail=$((fail+1)); echo "  FAIL  $1: got [$2] want [$3]"; fi }

# ---------------------------------------------------------------- instrument
instrument () { # $1 = generated .c   $2 = out .c   -> prints buffer count
python3 - "$1" "$2" <<'PY'
import re,sys
src=open(sys.argv[1]).read()

# Locate the producer function body (mkW) by brace matching.
# The name appears twice: once as a prototype, once as the definition.  Take
# the occurrence whose header is closed by '{' rather than ';'.
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

# Map each bump argument to a stable buffer id, in first-appearance order.
args=[]
for a in re.findall(r'gib_scalar_count_footer_bump\((\w+)\);', body):
    if a not in args: args.append(a)
if not args: print(0); sys.exit(0)
idx={a:k for k,a in enumerate(args)}

nb=len(args)
# Every site is reported every chunk, including zero.  A chunk really can hold
# zero elements for a buffer -- the first chunk does whenever the initial chunk
# size cannot fit the first node, and a constructor that never occurs writes
# nothing anywhere -- and dropping those would hide exactly the genuine-zero
# case this harness exists to distinguish from untouched metadata.
prologue = ('\nstatic long _vw09_cur[%d];\nstatic long _vw09_chunk;\n'
            'static void _vw09_flush(void){int k;for(k=0;k<%d;k++){'
            'fprintf(stderr,"VW09PROD %%d %%ld %%ld\\n",k,_vw09_chunk,_vw09_cur[k]);_vw09_cur[k]=0;}'
            '_vw09_chunk++;}\n'
            '__attribute__((destructor)) static void _vw09_final(void){_vw09_flush();}\n'
            ) % (nb, nb)

# 1. count each physical element write
nb_body = re.sub(r'gib_scalar_count_footer_bump\((\w+)\);',
                 lambda mm: '{_vw09_cur[%d]++; gib_scalar_count_footer_bump(%s);}'
                            % (idx[mm.group(1)], mm.group(1)),
                 body)
# 2. a region growth ends the current physical chunk for every buffer at once
nb_body = re.sub(r'(\n\s*)(gib_grow_region\()', r'\1_vw09_flush(); \2', nb_body, count=1)

out = src[:i] + nb_body + src[j+1:]
# 3. the final (partial) chunk is flushed by the destructor in the prologue --
#    the generated C has no end-of-build marker to hang it on.

# 4. the loopified consumer reports the STORED count and the bytes it writes
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
           ' fprintf(stderr,"VW09CONS %s cc=%%ld written=%%ld rem=%%ld %%s\\n",'
           ' _cc_%s,_w,_rem_%s,(_w>_rem_%s)?"OVERRUN":"ok"); }\n ' % (b,b,b,b,b,b))+anchor
    if anchor in out: out=out.replace(anchor,probe,1)

k=out.index('\n', out.index('#include'))
out = out[:k] + prologue + out[k:]
open(sys.argv[2],'w').write(out)
print(nb)
PY
}

# The same measurement is repeated with sharing, fusion and vectorization
# enabled, because VW-07's byte relationship has to hold for each of them:
# fusion changes which loop reads the count, sharing removes some loops
# entirely, and vectorization splits the inner loop into a vector body plus a
# scalar tail.
SELECTIVE="$MODE --opt-selective-buffer-sharing"
VECTORIZE="$SELECTIVE --opt-vectorization"

run_mode () { # $1 = label   $2.. = gibbon flags
  local label=$1; shift
  echo "== VW-09 builder metadata, per buffer per chunk: $label =="
  "$GIBBON" "$@" --no-gcc-vectorize --to-exe --cfile="$TMP/$label.c" \
      --exefile="$TMP/$label.exe" "$SRC" >"$TMP/$label.build" 2>&1
  if [ ! -s "$TMP/$label.c" ]; then
    echo "  FAIL  $label: compile"; fail=$((fail+1)); sed -n '1,10p' "$TMP/$label.build"; return
  fi
  local nb; nb=$(instrument "$TMP/$label.c" "$TMP/${label}i.c")
  if [ "${nb:-0}" -eq 0 ]; then
    echo "  FAIL  $label: no producer bump sites -- did StoreScalarCounts stop firing?"
    fail=$((fail+1)); return
  fi
  echo "   producer scalar-count buffers instrumented: $nb"
  gcc -O1 -g ${VW09_SAN:-} -I "$GIBBONDIR/gibbon-rts/build" -o "$TMP/${label}i.exe" \
      "$TMP/${label}i.c" "$GIBBONDIR/gibbon-rts/build/gibbon_rts.o" \
      -L "$GIBBONDIR/gibbon-rts/build" -Wl,-rpath="$GIBBONDIR/gibbon-rts/build" \
      -lgibbon_rts_ng -lm 2>"$TMP/$label.cc" \
    || { echo "  FAIL  $label: instrumented build"; fail=$((fail+1))
         sed -n '1,10p' "$TMP/$label.cc"; return; }
  local n bs exp res
  for n in $SIZES; do
    for bs in $BUFSIZES; do
      ASAN_OPTIONS=detect_leaks=0 "$TMP/${label}i.exe" --size-param "$n" \
        --inf-buffer-size "$bs" >"$TMP/$label.out" 2>"$TMP/$label.err"
      exp=$(python3 "$HERE/vw09/skewed_model.py" answer "$n")
      chk "$label n=$n bs=$bs value" \
          "$(grep -oE "^'#\(.*\)$" "$TMP/$label.out" | head -1)" "$exp"
      res=$(python3 "$HERE/vw09/skewed_model.py" check "$n" < "$TMP/$label.err")
      chk "$label n=$n bs=$bs metadata" "$res" "OK"
    done
  done
}

run_mode loopify        $MODE
run_mode loopify_fuse   $MODE --opt-loop-fusion
run_mode selective      $SELECTIVE
run_mode vectorize      $VECTORIZE
run_mode vectorize_fuse $VECTORIZE --opt-loop-fusion

echo "vw09_builder_counts: pass=$pass fail=$fail"
[ "$fail" -eq 0 ]
