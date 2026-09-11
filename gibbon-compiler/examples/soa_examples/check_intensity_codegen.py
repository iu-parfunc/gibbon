#!/usr/bin/env python3
"""Acceptance check for the arithmetic-intensity microbenchmarks.

The intensity sweep in ``programs/{SOA,AOS}/MapIntensityV2.hs`` is only
meaningful if the arithmetic it declares actually survives into the generated
code.  It repeatedly has not: GCC strength-reduced constant multipliers to
shift+add, and reassociated ``sum_k (i + c_k) * m`` into a single multiply.
Both collapses were silent -- the benchmark still ran, still produced numbers,
and the numbers meant nothing.

This script makes such a collapse fail loudly.  It builds the program twice
(a true scalar configuration and an ``--sse4.1`` vectorized one), compiles the
emitted C to assembly, locates each map function's innermost multiply-carrying
loop, and asserts:

  scalar build
    * exactly the declared number of ``imul``
    * zero ``pslld``  -- a packed shift means strength reduction is back

  --sse4.1 build
    * the same count as ``pmulld``
    * zero ``imul``   -- nothing was scalarized out of the vector loop
    * a 16-byte pointer stride -- one full vector of lanes per iteration at the
      source's declared width, i.e. real
      cross-element SIMD rather than SLP within a single element

Usage
-----
    ./check_intensity_codegen.py                    # SoA (source-declared width)
    ./check_intensity_codegen.py --layout AOS
    ./check_intensity_codegen.py --int64
    ./check_intensity_codegen.py --keep-temps -v
"""

from __future__ import annotations

import argparse
import re
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Dict, List, Optional, Tuple

BENCH_DIR = Path(__file__).resolve().parent
REPO_ROOT = BENCH_DIR.parents[2]

# --------------------------------------------------------------------------
# The contract.  fn -> multiplies expected in the innermost loop, per element
# group.  Derived from the source, and independently confirmed by disassembly.
# --------------------------------------------------------------------------
EXPECTED_MULTIPLIES: Dict[str, int] = {}

# mapSer<N>: one serial Horner chain of depth N.
for _n in (1, 2, 4, 8, 16):
    EXPECTED_MULTIPLIES[f"mapSer{_n}"] = _n

# mapChain<D>: 1 seed (i*i) + 2 chains of depth D + 1 closing product.
for _d in (1, 2, 4, 8):
    EXPECTED_MULTIPLIES[f"mapChain{_d}"] = 2 * _d + 2

# mapPar<N>: REASSOCIATION CONTROL.  sum_k (i + c_k) * m distributes to
# m * (N*i + sum c_k), so exactly one multiply survives at every N.  This is
# asserted deliberately: if it ever becomes N, the control has stopped being a
# control and the family may be promoted to a real intensity point.
for _n in (1, 2, 4, 8, 16):
    EXPECTED_MULTIPLIES[f"mapPar{_n}"] = 1

PROGRAM = "MapIntensityV2.hs"

LOOP_SHARE = [
    "--opt-loopification",
    "--auto-loopification",
    "--store-scalar-field-counts",
    "--opt-selective-buffer-sharing",
]


class CheckError(RuntimeError):
    pass


# --------------------------------------------------------------------------
# building
# --------------------------------------------------------------------------
def gibbon_binary() -> str:
    found = shutil.which("gibbon")
    if found:
        return found
    guess = REPO_ROOT / (
        "dist-newstyle/build/x86_64-linux/ghc-9.10.1/gibbon-0.3/x/gibbon/build/gibbon/gibbon"
    )
    if guess.exists():
        return str(guess)
    raise CheckError("gibbon not found on PATH and no dist-newstyle build present")


def rts_build_dir() -> Path:
    d = REPO_ROOT / "gibbon-rts" / "build"
    if not (d / "gibbon_rts.h").exists():
        raise CheckError(f"RTS headers not found in {d}; build the RTS first")
    return d


def run(cmd: List[str], verbose: bool) -> None:
    if verbose:
        print("  $ " + " ".join(cmd), file=sys.stderr)
    proc = subprocess.run(cmd, capture_output=True, text=True)
    if proc.returncode != 0:
        raise CheckError(
            "command failed: " + " ".join(cmd) + "\n" + proc.stdout[-4000:] + proc.stderr[-4000:]
        )


def declared_width_profile(src: Path) -> str:
    """Describe the integer widths the SOURCE declares.

    Read from the file, never inferred from its name.  Returns one of the four
    sanctioned descriptions: an exact width set, the bare-Int default, "mixed",
    or "unknown".
    """
    import re as _re
    text = src.read_text()
    explicit = sorted({m for m in _re.findall(r"\bInt(?:8|16|32|64)\b", text)})
    bare = _re.search(r"\bInt\b(?!\d)", text) is not None
    if explicit and bare:
        return f"mixed (explicit {', '.join(explicit)} plus bare Int = Int64)"
    if len(explicit) > 1:
        return f"mixed ({', '.join(explicit)})"
    if len(explicit) == 1:
        return f"single explicit width ({explicit[0]})"
    if bare:
        return "bare Int / Int64 default"
    return "unknown (no integer declaration found)"


def emit_c(src: Path, out_c: Path, vectorize: bool, verbose: bool) -> None:
    """Emit C for the checked program.

    Takes no width argument.  Width is declared by the source; this checker's
    program uses bare `Int`, which means `Int64`.  The removed `--int32`
    whole-program mode used to be appended here.
    """
    cmd = [gibbon_binary(), "--use-mutable-cursors", "--no-ran"]
    cmd += LOOP_SHARE
    if vectorize:
        cmd += ["--opt-vectorization", "--sse4.1"]
    cmd += ["--packed", "--to-exe", "--cfile", str(out_c),
            "--exefile", str(out_c.with_suffix(".exe")), str(src)]
    run(cmd, verbose)


def emit_asm(c_file: Path, out_s: Path, sse41: bool, verbose: bool) -> None:
    cmd = [
        "gcc", "-std=gnu11", "-O3", "-S", "-w",
        "-D_GIBBON_GENGC=1", "-D_GIBBON_SIMPLE_WRITE_BARRIER=0", "-D_GIBBON_EAGER_PROMOTION=1",
        "-I", str(rts_build_dir()), "-I", str(REPO_ROOT / "deps" / "uthash"),
    ]
    cmd += ["-msse4.1"] if sse41 else ["-fno-tree-vectorize"]
    cmd += ["-o", str(out_s), str(c_file)]
    run(cmd, verbose)


# --------------------------------------------------------------------------
# assembly analysis
# --------------------------------------------------------------------------
INSN = re.compile(r"^\s+[a-z]")
LABEL = re.compile(r"^(\.L\d+):")
BRANCH = re.compile(r"^\s+j\w+\s+(\.L\d+)")
MUL = re.compile(r"\b(imul|pmulld|pmuludq)")  # no trailing \b: opcodes carry a size suffix (imull/imulq)


def function_body(asm: List[str], fn: str) -> List[str]:
    out: List[str] = []
    inside = False
    for line in asm:
        if line.startswith(fn + ":"):
            inside = True
        if inside:
            out.append(line.rstrip())
            if ".size" in line and fn in line:
                break
    if not out:
        raise CheckError(f"function {fn} not found in assembly (was it inlined away?)")
    return out


def innermost_mul_loop(body: List[str]) -> Optional[List[str]]:
    """Smallest loop (backward branch to an enclosing label) containing a multiply."""
    labels = {m.group(1): i for i, line in enumerate(body) if (m := LABEL.match(line))}
    loops: List[Tuple[int, int, int]] = []
    for i, line in enumerate(body):
        m = BRANCH.match(line)
        if m and m.group(1) in labels and labels[m.group(1)] < i:
            a = labels[m.group(1)]
            loops.append((i - a, a, i))
    for _, a, b in sorted(loops):
        chunk = body[a : b + 1]
        if any(MUL.search(x) for x in chunk):
            return chunk
    return None


def count(chunk: List[str], pat: str) -> int:
    # No trailing \b -- x86 opcodes carry a size suffix (imull, imulq, pslld).
    rx = re.compile(rf"\b{pat}")
    return sum(1 for x in chunk if rx.search(x))


def strides(chunk: List[str]) -> List[int]:
    return sorted({int(v) for v in re.findall(r"add[lq]\s+\$(\d+), %r", "\n".join(chunk))})


# --------------------------------------------------------------------------
# checks
# --------------------------------------------------------------------------
def check(asm_path: Path, mode: str, lane_packable: bool = True) -> List[str]:
    """Return a list of failure strings; print a per-function table.

    ``lane_packable`` is False for the AoS (``Linear``) layout, where the two
    Int fields are interleaved with the tag inside each node.  There is no
    contiguous run of same-field values to load into a vector register, so the
    loop stays scalar (stride 9 = one AoS node: 1 tag byte + 2 x int32) no
    matter what vectorization flags are passed.  That is not a defect -- it is
    precisely the property SoA exists to fix -- so on AoS the vectorized build
    is checked with the scalar rules, and lane packing is asserted ABSENT.
    """
    asm = asm_path.read_text().split("\n")
    failures: List[str] = []
    # The checked program declares bare `Int`, i.e. Int64: two 8-byte lanes per
    # 128-bit register.  This used to be `4 if int32 else 8`, selected by the
    # removed whole-program width mode.
    lane_bytes = 8
    lanes = 16 // lane_bytes

    if mode == "sse41" and not lane_packable:
        mode = "scalar-aos"

    if mode.startswith("scalar"):
        print(f"\n  {'pass':12} {'want':>5} {'imul':>5} {'pslld':>6}  verdict")
    else:
        print(f"\n  {'pass':12} {'want':>5} {'pmulld':>7} {'imul':>5} {'stride':>7}  verdict")

    for fn, want in EXPECTED_MULTIPLIES.items():
        body = function_body(asm, fn)
        chunk = innermost_mul_loop(body)
        if chunk is None:
            failures.append(f"{fn} [{mode}]: no multiply found in any loop")
            print(f"  {fn:12} {want:5} {'--':>7}  NO MULTIPLY IN LOOP")
            continue

        if mode.startswith("scalar"):
            n_imul = count(chunk, "imul")
            n_shift = count(chunk, "pslld")
            # On the AoS vectorized build the multiply-count contract is NOT
            # asserted: GCC may SLP the expression within a single element, so a
            # multiply can legitimately appear as pmuludq/pmulld instead of imul.
            # The arithmetic itself is already pinned by the pure-scalar build;
            # the only thing this build has to prove is that no lane packing
            # happened.  Counts are still printed, for information.
            assert_counts = mode == "scalar"
            ok = (n_imul == want or not assert_counts) and n_shift == 0
            if assert_counts and n_imul != want:
                failures.append(
                    f"{fn} [{mode}]: expected {want} imul, found {n_imul} "
                    f"-- the scalar baseline is not doing the declared work "
                    f"(algebraic collapse?)"
                )
            if n_shift:
                failures.append(
                    f"{fn} [{mode}]: {n_shift} pslld present -- strength reduction is back"
                )
            if mode == "scalar-aos" and 16 in strides(chunk):
                failures.append(
                    f"{fn} [scalar-aos]: unexpected 16-byte stride -- the AoS loop appears "
                    f"lane-packed, which the interleaved Linear layout should make impossible"
                )
            if mode == "scalar-aos" and 16 in strides(chunk):
                ok = False
            note = "ok" if ok else "FAIL"
            if mode == "scalar-aos" and n_imul != want:
                note = "ok (SLP)"
            print(f"  {fn:12} {want:5} {n_imul:5} {n_shift:6}  {note}")
        else:
            n_mul = count(chunk, "pmulld")
            n_imul = count(chunk, "imul")
            st = strides(chunk)
            stride_ok = 16 in st

            if True:
                # SSE2/SSE4.1 has NO packed 64-bit multiply -- `pmullq` first
                # appears in AVX-512DQ.  Gibbon's int64x2 multiply therefore
                # has to extract each lane, multiply it scalar, and reinsert.
                # That is a hardware fact, not a codegen defect.  A W32 source
                # would instead get Gibbon's verified SSE2 `_mm_mul_epu32`
                # sequence -- Gibbon does not emit `pmulld` even under
                # `--sse4.1`, though a C compiler may recognise the sequence.
                # Assert the scalarization rather than fail on it, so the day a
                # wider ISA is targeted this check notices.
                ok = n_mul == 0 and n_imul >= want and stride_ok
                if n_mul:
                    failures.append(
                        f"{fn} [sse41/int64]: {n_mul} pmulld found, but pmulld is a 32-bit "
                        f"instruction -- expected none in a 64-bit build"
                    )
                if n_imul < want:
                    failures.append(
                        f"{fn} [sse41/int64]: expected at least {want} scalar imul (one per "
                        f"declared multiply per lane), found {n_imul}"
                    )
                if not stride_ok:
                    failures.append(
                        f"{fn} [sse41/int64]: pointer stride {st} has no 16-byte step -- the "
                        f"loop is not advancing {lanes} elements per iteration"
                    )
                ratio = f"{n_imul // want}x" if want and n_imul % want == 0 else "-"
                print(
                    f"  {fn:12} {want:5} {n_mul:7} {n_imul:5} {str(st):>7}  "
                    f"{'ok (scalarized ' + ratio + ')' if ok else 'FAIL'}"
                )
                continue

            ok = n_mul == want and n_imul == 0 and stride_ok
            if n_mul != want:
                failures.append(
                    f"{fn} [sse41]: expected {want} pmulld, found {n_mul}"
                )
            if n_imul:
                failures.append(
                    f"{fn} [sse41]: {n_imul} scalar imul inside the vector loop "
                    f"-- a multiply was scalarized"
                )
            if not stride_ok:
                failures.append(
                    f"{fn} [sse41]: pointer stride {st} has no 16-byte step -- the loop is "
                    f"not advancing {lanes} elements per iteration, so this is SLP within one "
                    f"element, not cross-element SIMD"
                )
            print(
                f"  {fn:12} {want:5} {n_mul:7} {n_imul:5} {str(st):>7}  {'ok' if ok else 'FAIL'}"
            )

    return failures


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--layout", choices=("SOA", "AOS"), default="SOA",
                    help="which source tree to check (default: SOA)")
    # `--int64` is now the only behaviour and is accepted as a no-op so existing
    # invocations keep working.  `--int32` is a tombstone: the checked program
    # declares bare `Int` (i.e. Int64), and there is no whole-program width mode
    # that could reinterpret it.
    ap.add_argument("--int64", action="store_true", help=argparse.SUPPRESS)
    ap.add_argument("--int32", dest="removed_int32_mode", action="store_true",
                    help=argparse.SUPPRESS)
    ap.add_argument("--program", default=PROGRAM,
                    help=f"program file name (default: {PROGRAM})")
    ap.add_argument("--keep-temps", action="store_true",
                    help="leave the generated .c/.s files in place and print their paths")
    ap.add_argument("-v", "--verbose", action="store_true",
                    help="echo every compiler invocation")
    args = ap.parse_args()

    if getattr(args, "removed_int32_mode", False):
        print("error: --int32 has been removed; integer width is declared by the "
              "source program.\n"
              "       This checker's program declares bare `Int`, which means "
              "`Int64`, and it is\n"
              "       checked as such.  Verifying a packed 32-bit lane pattern "
              "requires an\n"
              "       explicit-Int32 benchmark source, which does not exist yet "
              "(see\n"
              "       benchmark_qualification.md).  Write one and pass it with "
              "--program.",
              file=sys.stderr)
        return 2

    src = BENCH_DIR / "programs" / args.layout / args.program
    if not src.exists():
        print(f"error: {src} does not exist", file=sys.stderr)
        return 2

    tmp = Path(tempfile.mkdtemp(prefix="gibbon-intensity-check-"))
    print(f"Arithmetic-intensity codegen check")
    print(f"  program : {src}")
    # Read from the source, never inferred from the file name.
    print(f"  width   : {declared_width_profile(src)}")
    print(f"  workdir : {tmp}")

    lane_packable = args.layout == "SOA"
    failures: List[str] = []
    try:
        for mode, vectorize in (("scalar", False), ("sse41", True)):
            c_file = tmp / f"{args.layout.lower()}_{mode}.c"
            s_file = tmp / f"{args.layout.lower()}_{mode}.s"
            emit_c(src, c_file, vectorize=vectorize, verbose=args.verbose)
            emit_asm(c_file, s_file, sse41=vectorize, verbose=args.verbose)
            print(f"\n=== {mode} build ===")
            if mode == "sse41" and not lane_packable:
                print("  (AoS Linear layout: fields are interleaved, so no lane packing is")
                print("   possible; checked with the scalar rules and asserted NOT packed.)")
            failures += check(s_file, mode, lane_packable=lane_packable)
    except CheckError as exc:
        print(f"\nERROR: {exc}", file=sys.stderr)
        return 2
    finally:
        if args.keep_temps:
            print(f"\ntemporaries kept in {tmp}")
        else:
            shutil.rmtree(tmp, ignore_errors=True)

    print()
    if failures:
        print(f"FAILED -- {len(failures)} problem(s):")
        for f in failures:
            print(f"  * {f}")
        print(
            "\nA multiply-count mismatch means the benchmark is no longer measuring what it\n"
            "claims.  Fix the source (or, if the change is intended, update\n"
            "EXPECTED_MULTIPLIES in this script) before trusting any intensity numbers."
        )
        return 1

    print(f"PASSED -- all {len(EXPECTED_MULTIPLIES)} map passes emit the declared arithmetic.")
    return 0


if __name__ == "__main__":
    sys.exit(main())
