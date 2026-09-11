#!/usr/bin/env bash
# Peak RSS of a benchmark loop must be FLAT in the iteration count, not linear.
#
# Without --reclaim-iterate-regions, each `iterate` iteration rewinds to its
# output region's first chunk, re-grows it, and strands the previous
# iteration's chunk chain.  Memory therefore grows by one whole output value
# per iteration: measured 12 KB/iter at mkList 1000, 9.3 MB/iter at 1e6, and
# 929 MB/iter at 1e8 -- the last of which OOM-kills a 94 GB machine at
# --iterate 101.
#
# Usage: region_iterate_rss.sh [gibbon-binary]
#
# Everything runs under `ulimit -v` so a failure is a clean allocation error
# rather than an OOM kill that takes the machine down with it.
set -u
ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
GIB=${1:-$ROOT/dist-newstyle/build/x86_64-linux/ghc-9.10.1/gibbon-0.3/x/gibbon/build/gibbon/gibbon}
export GIBBONDIR=$ROOT
TMP=$(mktemp -d); trap 'rm -rf "$TMP"' EXIT
MEM_KB=8000000          # 8 GB address-space cap for every child
rc=0

# A list whose output comfortably exceeds GIB_INIT_CHUNK_SIZE (1024 B), so the
# region actually grows -- that is the only case that leaks.
cat > "$TMP/Leaky.hs" <<'HS'
data List = Cons Int64 List | Nil

mkList :: Int64 -> List
mkList n = if n <= 0 then Nil else let r = mkList (n - 1) in Cons n r

add1 :: List -> List
add1 lst = case lst of
             Nil -> Nil
             Cons i rst -> let j = i + 1 in Cons j (add1 rst)

gibbon_main = let lst = mkList 100000
                  lst' = iterate (add1 lst)
               in 0
HS

peak_rss () {   # $1=exe $2=iters  -> peak RSS in KB, or "FAIL"
    local out
    out=$( ulimit -v $MEM_KB; /usr/bin/time -f "%M" "$1" --iterate "$2" 2>&1 >/dev/null | tail -1 )
    case "$out" in (*[!0-9]*|"") echo FAIL ;; (*) echo "$out" ;; esac
}

build () {      # $1=label  $2...=extra gibbon flags
    local label=$1; shift
    ( ulimit -v 12000000
      "$GIB" --packed --to-exe --c-arithmetic unsafe "$@" \
             --cfile "$TMP/$label.c" --exefile "$TMP/$label.exe" "$TMP/Leaky.hs" ) \
        >"$TMP/$label.build.log" 2>&1
    if [ ! -x "$TMP/$label.exe" ]; then
        echo "FAIL: could not build $label"; tail -5 "$TMP/$label.build.log"; return 1
    fi
}

check () {      # $1=label  $2=expect-flat(yes/no)
    local label=$1 expect=$2
    local r1 r100
    r1=$(peak_rss "$TMP/$label.exe" 1)
    r100=$(peak_rss "$TMP/$label.exe" 100)
    if [ "$r1" = FAIL ] || [ "$r100" = FAIL ]; then
        echo "  $label: FAIL (a run did not complete)"; return 1
    fi
    # Flat means iteration 100 costs no more than a small constant over 1.
    local limit=$(( r1 * 2 + 20000 ))
    printf "  %-28s iters=1 %8s KB   iters=100 %8s KB" "$label" "$r1" "$r100"
    if [ "$expect" = yes ]; then
        if [ "$r100" -le "$limit" ]; then echo "   FLAT  ok"; else
            echo "   GREW  FAIL (expected flat)"; return 1; fi
    else
        if [ "$r100" -gt "$limit" ]; then echo "   grew  (expected, feature off)"; else
            echo "   FLAT  FAIL (control did not reproduce the leak)"; return 1; fi
    fi
}

echo "Region reclaim: peak RSS vs iteration count"
for mode_label in "aos_imm:" "aos_mut:--use-mutable-cursors" \
                  "soa_mut:--use-mutable-cursors --store-scalar-field-counts"; do
    label=${mode_label%%:*}; flags=${mode_label#*:}
    # shellcheck disable=SC2086
    build "off_$label"  $flags                              || { rc=1; continue; }
    # shellcheck disable=SC2086
    build "on_$label"   $flags --reclaim-iterate-regions     || { rc=1; continue; }
    check "off_$label" no  || rc=1     # control: the leak must still be there
    check "on_$label"  yes || rc=1     # the fix
done
exit $rc
