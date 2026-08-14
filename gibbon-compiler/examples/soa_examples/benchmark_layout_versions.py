#!/usr/bin/env python3
"""Run side-by-side Gibbon AoS/SoA layout optimization comparisons.

This is a thin parent script around the sibling `gibbon_benchmark.py` in this
same directory.  It runs the benchmark configurations needed to compare the
built-in layout/optimization variants and combines their JSON outputs into one
Markdown report:

AoS:
  1. immutable cursors, recursive
  2. mutable cursors, recursive
  3. mutable cursors, loopified

SoA:
  1. immutable cursors, recursive
  2. mutable cursors, recursive
  3. mutable cursors, loopified
  4. mutable cursors, loopified + selective buffer sharing
  5. mutable cursors, loopified + selective buffer sharing + SIMD vectorization

Int width:
  The default run uses the normal 64-bit GibInt backend.  Pass --32-bit (or
  --int32) to generate the same comparison matrix with 32-bit GibInt instead.
"""

from __future__ import annotations

import argparse
import json
import queue
import shutil
import subprocess
import sys
import textwrap
import threading
import time
import math
from dataclasses import dataclass
from pathlib import Path
from typing import Dict, Iterable, List, Optional, Sequence, Tuple


# This script lives in gibbon-compiler/examples/soa_examples/ alongside the
# gibbon_benchmark.py it drives.  BENCH_DIR is therefore simply its own
# directory, and REPO_ROOT is three levels up
# (soa_examples -> examples -> gibbon-compiler -> repo root).
BENCH_DIR = Path(__file__).resolve().parent
REPO_ROOT = BENCH_DIR.parents[2]
BENCH_SCRIPT = BENCH_DIR / "gibbon_benchmark.py"


@dataclass(frozen=True)
class Version:
    key: str
    title: str
    run_key: str
    json_variant: str
    description: str


@dataclass(frozen=True)
class RunConfig:
    key: str
    args: Tuple[str, ...]
    versions: Tuple[Version, ...]


VERSIONS: Tuple[Version, ...] = (
    Version("aos_imm_recursive", "AoS imm recursive", "recursive", "aos_imm",
            "AoS, immutable/non-mutable cursors, recursive traversal"),
    Version("aos_mut_recursive", "AoS mut recursive", "recursive", "aos",
            "AoS, mutable cursors, recursive traversal"),
    Version("aos_mut_loopified", "AoS mut loopified", "loopified", "aos",
            "AoS, mutable cursors, OPT:CanVectorize loopification"),
    Version("soa_imm_recursive", "SoA imm recursive", "recursive", "soa_imm",
            "SoA, immutable/non-mutable cursors, recursive traversal"),
    Version("soa_mut_recursive", "SoA mut recursive", "recursive", "soa",
            "SoA, mutable cursors, recursive traversal"),
    Version("soa_mut_loopified", "SoA mut loopified", "loopified", "soa",
            "SoA, mutable cursors, scalar-counted loopification"),
    Version("soa_mut_loopified_selective", "SoA mut loop+share", "selective", "soa",
            "SoA, mutable cursors, loopification plus selective buffer sharing"),
    Version("soa_mut_loopified_selective_vectorized", "SoA mut loop+share+vec", "vectorized", "soa",
            "SoA, mutable cursors, loopification plus selective buffer sharing plus SIMD vectorization"),
)


RUN_CONFIGS: Tuple[RunConfig, ...] = (
    RunConfig(
        "recursive",
        ("--benchmark-immutable",),
        tuple(v for v in VERSIONS if v.run_key == "recursive"),
    ),
    RunConfig(
        "loopified",
        ("--enable-loopification", "--auto-loopification", "--store-scalar-field-counts"),
        tuple(v for v in VERSIONS if v.run_key == "loopified"),
    ),
    RunConfig(
        "selective",
        ("--enable-loopification", "--auto-loopification", "--store-scalar-field-counts",
         "--enable-selective-buffer-sharing"),
        tuple(v for v in VERSIONS if v.run_key == "selective"),
    ),
    RunConfig(
        "vectorized",
        ("--enable-loopification", "--auto-loopification", "--store-scalar-field-counts",
         "--enable-selective-buffer-sharing", "--enable-vectorization"),
        tuple(v for v in VERSIONS if v.run_key == "vectorized"),
    ),
)


def short_status_line(line: str, limit: int = 86) -> str:
    line = " ".join(line.strip().split())
    if len(line) <= limit:
        return line
    return line[: max(0, limit - 3)] + "..."


def render_status(config_idx: int, config_count: int, config: RunConfig,
                  elapsed: float, line_count: int, latest: str) -> str:
    bar_width = 24
    # The child benchmark script does the real work; this bar shows wrapper-level
    # progress while the live message reports fine-grained child progress.
    filled = int(bar_width * (config_idx - 1) / max(config_count, 1))
    if filled < bar_width and line_count > 0:
        filled = min(bar_width, filled + 1)
    bar = "#" * filled + "-" * (bar_width - filled)
    return (f"\r[{bar}] {config_idx}/{config_count} {config.key} "
            f"elapsed={elapsed:6.1f}s lines={line_count:5d} "
            f"{short_status_line(latest)}")


def run_child_with_progress(cmd: Sequence[str], cwd: Path, run_dir: Path,
                            config: RunConfig, config_idx: int,
                            config_count: int, verbose: bool) -> int:
    stdout_path = run_dir / "stdout.txt"
    stderr_path = run_dir / "stderr.txt"
    start = time.time()
    latest = "starting"
    line_count = 0
    q: "queue.Queue[Optional[str]]" = queue.Queue()

    proc = subprocess.Popen(
        cmd,
        cwd=cwd,
        stdout=subprocess.PIPE,
        stderr=subprocess.STDOUT,
        text=True,
        bufsize=1,
    )

    def reader() -> None:
        assert proc.stdout is not None
        try:
            for line in proc.stdout:
                q.put(line)
        finally:
            q.put(None)

    thread = threading.Thread(target=reader, daemon=True)
    thread.start()

    def print_status(force_newline: bool = False) -> None:
        if verbose:
            return
        msg = render_status(
            config_idx,
            config_count,
            config,
            time.time() - start,
            line_count,
            latest,
        )
        sys.stdout.write(msg)
        if force_newline:
            sys.stdout.write("\n")
        sys.stdout.flush()

    with stdout_path.open("w", encoding="utf-8") as stdout_log, \
         stderr_path.open("w", encoding="utf-8") as stderr_log:
        stderr_log.write("stderr was merged into stdout for live progress streaming.\n")
        print_status()
        reader_done = False
        while not reader_done:
            try:
                item = q.get(timeout=0.2)
            except queue.Empty:
                print_status()
                continue
            if item is None:
                reader_done = True
                break
            stdout_log.write(item)
            stdout_log.flush()
            line_count += 1
            stripped = item.rstrip("\n")
            if stripped:
                latest = stripped
            if verbose:
                sys.stdout.write(f"[{config.key}] {item}")
                sys.stdout.flush()
            else:
                print_status()

    returncode = proc.wait()
    thread.join(timeout=1.0)
    if verbose:
        print(f"[{config.key}] finished exit={returncode} elapsed={time.time() - start:.1f}s")
    else:
        latest = f"finished exit={returncode}"
        print_status(force_newline=True)
    return returncode


def md_escape(text: str) -> str:
    return text.replace("|", "\\|")


def highlight_color(speedup: Optional[float]) -> str:
    """Color the best cell by the size of the win over the row baseline."""
    if speedup is None:
        return "#d73a49"
    if speedup <= 1.05:
        return "#d73a49"  # red: essentially a tie, 0-5% faster
    if speedup <= 1.25:
        return "#0969da"  # blue: moderate, 5-25% faster
    return "#1a7f37"      # green: large, >25% faster


def highlight(text: str, enabled: bool, speedup: Optional[float] = None,
              colorize: bool = False) -> str:
    if not enabled or text in {"--", "FAIL", "missing", "fail"}:
        return text
    if not colorize:
        return f"**{text}**"
    color = highlight_color(speedup)
    return f'<span style="color: {color}; font-weight: 700;">{text}</span>'


MAP_ENTRY_BORDERS = {
    "soa_mut_loopified": "2px dotted #0969da",
    "soa_mut_loopified_selective": "2px solid #8250df",
    "soa_mut_loopified_selective_vectorized": "2px solid #1a7f37",
}


def version_title(version: Version, use_int32: bool) -> str:
    if use_int32 and version.key == "soa_mut_loopified_selective_vectorized":
        return f"{version.title} (32-bit vec)"
    return version.title


def version_description(version: Version, use_int32: bool) -> str:
    if use_int32 and version.key == "soa_mut_loopified_selective_vectorized":
        return f"{version.description} using 32-bit vector lanes"
    return version.description


def map_entry_cell(version: Version, text: str, kind: Optional[str], colorize: bool) -> str:
    if (not colorize or kind != "map" or version.key not in MAP_ENTRY_BORDERS
            or text in {"--", "FAIL", "missing", "fail"}):
        return text
    border = MAP_ENTRY_BORDERS[version.key]
    return (
        f'<span style="border: {border}; border-radius: 999px; '
        'padding: 0.05rem 0.35rem; font-weight: 700; display: inline-block; '
        'white-space: nowrap;">'
        f'{text}</span>'
    )


def min_keys(values: Dict[str, Optional[float]]) -> set:
    present = {k: v for k, v in values.items() if isinstance(v, (int, float))}
    if not present:
        return set()
    best = min(present.values())
    return {k for k, v in present.items() if v == best}


def max_keys(values: Dict[str, Optional[float]]) -> set:
    present = {k: v for k, v in values.items() if isinstance(v, (int, float))}
    if not present:
        return set()
    best = max(present.values())
    return {k for k, v in present.items() if v == best}



_T_CRIT_975 = {
    1: 12.706, 2: 4.303, 3: 3.182, 4: 2.776, 5: 2.571,
    6: 2.447, 7: 2.365, 8: 2.306, 9: 2.262, 10: 2.228,
    11: 2.201, 12: 2.179, 13: 2.160, 14: 2.145, 15: 2.131,
    16: 2.120, 17: 2.110, 18: 2.101, 19: 2.093, 20: 2.086,
    21: 2.080, 22: 2.074, 23: 2.069, 24: 2.064, 25: 2.060,
    26: 2.056, 27: 2.052, 28: 2.048, 29: 2.045, 30: 2.042,
}


def t_crit_975(n: Optional[int]) -> Optional[float]:
    if not isinstance(n, int) or n < 2:
        return None
    df = n - 1
    return _T_CRIT_975.get(df, 1.96)


def ci95_from_stderr(mean: float, stderr: Optional[float], n: Optional[int]) -> Optional[float]:
    if not isinstance(stderr, (int, float)):
        return None
    tcrit = t_crit_975(n)
    return None if tcrit is None else tcrit * float(stderr)


def fmt_ci(value: Optional[float]) -> str:
    if value is None:
        return "--"
    if value < 1e-3:
        return f"{value:.6f}"
    return f"{value:.4f}"


def pass_summary(variant: Optional[Dict], pass_name: str) -> Optional[Dict[str, Optional[float]]]:
    if not variant or not variant.get("run_success"):
        return None
    pdata = (variant.get("passes") or {}).get(pass_name)
    if not pdata:
        return None
    med = pdata.get("median_time")
    if not isinstance(med, (int, float)):
        return None
    mean = pdata.get("mean_time", med)
    if not isinstance(mean, (int, float)):
        mean = med
    n = pdata.get("n")
    ci = pdata.get("ci95_abs")
    if ci is None:
        ci = ci95_from_stderr(float(mean), pdata.get("stderr"), n)
    return {
        "median": float(med),
        "mean": float(mean),
        "ci95_abs": float(ci) if isinstance(ci, (int, float)) else None,
        "n": n if isinstance(n, int) else None,
    }


def total_pass_summary(variant: Optional[Dict]) -> Optional[Dict[str, Optional[float]]]:
    if not variant or not variant.get("run_success"):
        return None
    summaries = []
    for pname in (variant.get("passes") or {}):
        sm = pass_summary(variant, pname)
        if sm is not None:
            summaries.append(sm)
    if not summaries:
        return None
    median = sum(sm["median"] or 0.0 for sm in summaries)
    mean = sum((sm["mean"] if sm["mean"] is not None else sm["median"]) or 0.0 for sm in summaries)
    # Approximate CI for the sum of per-pass means by adding independent
    # standard errors in quadrature.  This is conservative enough for the
    # report while preserving backwards compatibility with existing JSON.
    n_vals = [sm.get("n") for sm in summaries if isinstance(sm.get("n"), int)]
    n_min = min(n_vals) if n_vals else None
    stderr_sq = 0.0
    have_stderr = False
    for sm in summaries:
        ci = sm.get("ci95_abs")
        n = sm.get("n")
        tcrit = t_crit_975(n)
        if isinstance(ci, (int, float)) and tcrit:
            stderr_sq += (float(ci) / tcrit) ** 2
            have_stderr = True
    total_ci = None
    if have_stderr:
        tcrit_total = t_crit_975(n_min) or 1.96
        total_ci = tcrit_total * math.sqrt(stderr_sq)
    return {"median": median, "mean": mean, "ci95_abs": total_ci, "n": n_min}


def fmt_runtime_cell(summary: Optional[Dict[str, Optional[float]]], colorize: bool) -> str:
    if summary is None:
        return "--"
    med = fmt_time(summary.get("median"))
    mean = fmt_time(summary.get("mean"))
    ci = fmt_ci(summary.get("ci95_abs"))
    n = summary.get("n")
    n_s = f", n={n}" if isinstance(n, int) else ""
    if colorize:
        return f"med {med}<br><small>mean {mean} ±{ci}{n_s}</small>"
    return f"med {med}; mean {mean} ±{ci}{n_s}"


def runtime_stat_value(summary: Optional[Dict[str, Optional[float]]], stat: str) -> str:
    if summary is None:
        return "--"
    if stat == "median":
        return fmt_time(summary.get("median"))
    if stat == "mean":
        return fmt_time(summary.get("mean"))
    if stat == "error":
        return f"±{fmt_ci(summary.get('ci95_abs'))}"
    raise ValueError(f"unknown runtime stat row: {stat}")


def runtime_stat_rows(row_prefix: Sequence[str],
                      summaries: Dict[str, Optional[Dict[str, Optional[float]]]],
                      best_keys: set,
                      speedups: Dict[str, Optional[float]],
                      colorize_best: bool,
                      kind: Optional[str] = None) -> List[List[str]]:
    rows: List[List[str]] = []
    for idx, stat in enumerate(["median", "mean", "error"]):
        prefix = list(row_prefix) if idx == 0 else [""] * len(row_prefix)
        prefix.append(stat)
        highlight_best = stat == "median"
        rows.append(
            prefix +
            [map_entry_cell(
                v,
                highlight(runtime_stat_value(summaries[v.key], stat), highlight_best and v.key in best_keys,
                          speedups.get(v.key), colorize_best),
                kind,
                colorize_best,
             )
             for v in VERSIONS]
        )
    return rows


def fmt_time(value: Optional[float]) -> str:
    if value is None:
        return "--"
    if value < 1e-3:
        return f"{value:.6f}"
    return f"{value:.4f}"


def fmt_total_cell(variant: Optional[Dict], colorize: bool = False) -> str:
    if variant is None:
        return "--"
    if not variant.get("run_success"):
        return "FAIL"
    return fmt_runtime_cell(total_pass_summary(variant), colorize)


def fmt_speedup(base: Optional[float], value: Optional[float]) -> str:
    if base is None or value is None or value <= 0:
        return "--"
    return f"{base / value:.3f}x"


def total_pass_time(variant: Optional[Dict]) -> Optional[float]:
    if not variant or not variant.get("run_success"):
        return None
    passes = variant.get("passes") or {}
    if not passes:
        return None
    total = 0.0
    saw_time = False
    for pdata in passes.values():
        med = pdata.get("median_time")
        if isinstance(med, (int, float)):
            total += float(med)
            saw_time = True
    return total if saw_time else None


def pass_time(variant: Optional[Dict], pass_name: str) -> Optional[float]:
    if not variant or not variant.get("run_success"):
        return None
    pdata = (variant.get("passes") or {}).get(pass_name)
    med = pdata.get("median_time") if pdata else None
    return float(med) if isinstance(med, (int, float)) else None


def load_json(path: Path) -> List[Dict]:
    with path.open("r", encoding="utf-8") as f:
        data = json.load(f)
    if not isinstance(data, list):
        raise ValueError(f"Expected a list in {path}")
    return data


def run_benchmark(config: RunConfig, args: argparse.Namespace) -> Path:
    width_dir = "int32" if args.use_int32 else "int64"
    run_dir = args.output_dir / width_dir / config.key
    json_path = run_dir / "results.json"
    report_path = run_dir / "benchmark_report.txt"
    bench_output_dir = run_dir / "benchmark_output"
    run_dir.mkdir(parents=True, exist_ok=True)

    cmd = [
        sys.executable,
        str(BENCH_SCRIPT.name),
        "--iterations", str(args.iterations),
        "--warmup-runs", str(args.warmup_runs),
        "--warmup-iterations", str(args.warmup_iterations),
        "--cooldown-seconds", str(args.cooldown_seconds),
        "--output-dir", str(bench_output_dir),
        "--json", str(json_path),
        "--report", str(report_path),
    ]
    if args.clean:
        cmd.append("--clean")
    if args.dump_raw:
        cmd.append("--dump-raw")
    if args.programs:
        cmd.append("--programs")
        cmd.extend(args.programs)
    if args.use_int32:
        cmd.append("--int32")
    cmd.extend(config.args)

    print(f"\n[{config.key}] {' '.join(cmd)}")
    returncode = run_child_with_progress(
        cmd, BENCH_DIR, run_dir, config, args._config_index,
        len(RUN_CONFIGS), args.verbose,
    )

    if returncode != 0:
        raise RuntimeError(
            f"benchmark config '{config.key}' failed with exit code {returncode}; "
            f"see {run_dir / 'stdout.txt'} and {run_dir / 'stderr.txt'}"
        )
    if not json_path.exists():
        raise RuntimeError(f"benchmark config '{config.key}' did not write {json_path}")
    return json_path


def collect_results(json_paths: Dict[str, Path]) -> Dict[str, Dict[str, Optional[Dict]]]:
    combined: Dict[str, Dict[str, Optional[Dict]]] = {}
    for config in RUN_CONFIGS:
        rows_by_program = {row["program"]: row for row in load_json(json_paths[config.key])}
        for program, row in rows_by_program.items():
            combined.setdefault(program, {})
            for version in config.versions:
                combined[program][version.key] = row.get(version.json_variant)
    return combined


def collect_pass_names(version_rows: Dict[str, Optional[Dict]]) -> List[str]:
    names = set()
    for variant in version_rows.values():
        if variant and variant.get("run_success"):
            names.update((variant.get("passes") or {}).keys())
    return sorted(names)


def pass_kind(version_rows: Dict[str, Optional[Dict]], pass_name: str) -> str:
    for variant in version_rows.values():
        if not variant:
            continue
        pdata = (variant.get("passes") or {}).get(pass_name)
        if pdata and pdata.get("pass_type"):
            return str(pdata["pass_type"])
    return "unknown"


def speedup_value(base: Optional[float], value: Optional[float]) -> Optional[float]:
    if base is None or value is None or value <= 0:
        return None
    return base / value


def table(headers: Sequence[str], rows: Sequence[Sequence[str]]) -> str:
    header = "| " + " | ".join(md_escape(h) for h in headers) + " |"
    sep = "| " + " | ".join("---" for _ in headers) + " |"
    body = ["| " + " | ".join(md_escape(cell) for cell in row) + " |" for row in rows]
    return "\n".join([header, sep] + body)


def render_report(combined: Dict[str, Dict[str, Optional[Dict]]],
                  json_paths: Dict[str, Path],
                  args: argparse.Namespace,
                  colorize_best: bool) -> str:
    version_by_key = {v.key: v for v in VERSIONS}

    lines: List[str] = []
    lines.append("# Gibbon AoS/SoA Layout Optimization Comparison")
    lines.append("")
    lines.append(f"Timed iterations per executable run: {args.iterations}")
    lines.append(f"Warmup: {args.warmup_runs} run(s) x --iterate {args.warmup_iterations}")
    lines.append(f"Cooldown between variants: {args.cooldown_seconds:g}s")
    lines.append(f"Int width: {'32-bit GibInt' if args.use_int32 else '64-bit GibInt'}")
    lines.append(f"Programs: {' '.join(args.programs) if args.programs else 'default benchmark set'}")
    width_dir = "int32" if args.use_int32 else "int64"
    lines.append(f"Output directory root: {args.output_dir}")
    lines.append(f"Output namespace: {width_dir}")
    lines.append("")
    if args.use_int32:
        lines.append("> **Reading these 32-bit numbers.** Output-equality checks in this report")
        lines.append("> compare int32 against int32 only. A program whose result exceeds 32 bits")
        lines.append("> legitimately prints a different answer than the 64-bit run; check it")
        lines.append("> against `x mod 2**32` read as signed before calling it a bug.")
        lines.append(">")
        lines.append("> Do **not** read a 32-bit wall-clock win as a vectorization win. Narrowing")
        lines.append("> `Int` to 4 bytes halves memory traffic *and* doubles the SSE2 lane count,")
        lines.append("> and those are separate effects. To isolate them, compare the marginal")
        lines.append("> `+share -> +share+vec` gain *within* each width. Measured on this suite:")
        lines.append("> narrowing alone ~1.93x, the extra SIMD lanes ~1.01x.")
        lines.append(">")
        lines.append("> `DBQuery.hs` is **not work-equivalent** across widths -- it branches on a")
        lines.append("> seed-derived value that wraps at 32 bits and so builds a roughly half-size")
        lines.append("> tree under `--int32`. Exclude it from any cross-width comparison.")
        lines.append("")
    lines.append("## Benchmark Runs")
    for config in RUN_CONFIGS:
        lines.append(f"- {config.key}: {json_paths[config.key]}")
    lines.append("")
    lines.append("## Compared Versions")
    for version in VERSIONS:
        lines.append(f"- {version_title(version, args.use_int32)}: {version_description(version, args.use_int32)}")
    lines.append("")
    if colorize_best:
        lines.append("Best entries are colored by speedup over the row baseline: red = 0-5%, blue = 5-25%, green = >25%.")
        lines.append("For map pass entries only, SoA mut loopified uses a dotted blue border, SoA mut loop+share uses a solid purple border, and SoA mut loop+share+vec uses a solid green border.")
    else:
        lines.append("Bold entries mark the lowest runtime or highest speedup in that row.")
    lines.append("")

    runtime_headers = ["Program", "Statistic"] + [version_title(v, args.use_int32) for v in VERSIONS]
    headers = ["Program"] + [version_title(v, args.use_int32) for v in VERSIONS]
    rows: List[List[str]] = []
    speed_rows: List[List[str]] = []
    status_rows: List[List[str]] = []
    failures: List[str] = []

    for program in sorted(combined):
        version_rows = combined[program]
        summaries = {v.key: total_pass_summary(version_rows.get(v.key)) for v in VERSIONS}
        totals = {v.key: (summaries[v.key] or {}).get("median") for v in VERSIONS}
        base = totals.get("aos_imm_recursive") or totals.get("aos_mut_recursive")
        total_best = min_keys(totals)
        speedups = {v.key: speedup_value(base, totals[v.key]) for v in VERSIONS}
        speed_best = max_keys(speedups)
        rows.extend(runtime_stat_rows([program], summaries, total_best, speedups, colorize_best))
        speed_rows.append(
            [program] +
            [highlight(fmt_speedup(base, totals[v.key]), v.key in speed_best,
                       speedups.get(v.key), colorize_best)
             for v in VERSIONS]
        )
        status_cells: List[str] = []
        for v in VERSIONS:
            variant = version_rows.get(v.key)
            if variant is None:
                status_cells.append("missing")
            elif variant.get("run_success"):
                status_cells.append("ok")
            else:
                status_cells.append("fail")
                failures.append(
                    f"- {program}, {version_title(v, args.use_int32)}: {variant.get('error') or 'run failed'}"
                )
        status_rows.append([program] + status_cells)

    lines.append("## Total Timed Pass Runtime")
    lines.append("")
    lines.append("Seconds. Each benchmark expands into median, mean, and error sub-rows. Error is the two-sided 95% confidence interval for the mean.")
    lines.append("")
    lines.append(table(runtime_headers, rows))
    lines.append("")
    lines.append("## Speedup Vs AoS Recursive Baseline")
    lines.append("")
    lines.append("Uses AoS immutable recursive when available, otherwise AoS mutable recursive.")
    lines.append("")
    lines.append(table(headers, speed_rows))
    lines.append("")
    lines.append("## Run Status")
    lines.append("")
    lines.append(table(headers, status_rows))
    lines.append("")
    if failures:
        lines.append("## Failure Details")
        lines.append("")
        lines.extend(failures)
        lines.append("")

    lines.append("## Per-Program Pass Timings")
    for program in sorted(combined):
        version_rows = combined[program]
        pass_names = collect_pass_names(version_rows)
        if not pass_names:
            lines.append("")
            lines.append(f"{program}: no successful pass timings")
            continue
        lines.append("")
        lines.append(f"### {program}")
        lines.append("")
        pass_rows: List[List[str]] = []
        pass_speed_rows: List[List[str]] = []
        for pname in pass_names:
            pass_summaries = {v.key: pass_summary(version_rows.get(v.key), pname) for v in VERSIONS}
            times = {v.key: (pass_summaries[v.key] or {}).get("median") for v in VERSIONS}
            best_times = min_keys(times)
            base_time = times.get("aos_imm_recursive") or times.get("aos_mut_recursive")
            speedups = {v.key: speedup_value(base_time, times[v.key]) for v in VERSIONS}
            best_speedups = max_keys(speedups)
            kind = pass_kind(version_rows, pname)
            pass_rows.extend(runtime_stat_rows([pname, kind], pass_summaries, best_times, speedups, colorize_best, kind=kind))
            pass_speed_rows.append(
                [pname, kind] +
                [map_entry_cell(
                    v,
                    highlight(fmt_speedup(base_time, times[v.key]), v.key in best_speedups,
                              speedups.get(v.key), colorize_best),
                    kind,
                    colorize_best,
                 )
                 for v in VERSIONS]
            )
        pass_runtime_headers = ["Pass", "Kind", "Statistic"] + [version_title(v, args.use_int32) for v in VERSIONS]
        pass_headers = ["Pass", "Kind"] + [version_title(v, args.use_int32) for v in VERSIONS]
        lines.append(table(pass_runtime_headers, pass_rows))
        lines.append("")
        lines.append("")
        lines.append(f"### {program} Pass Speedups Vs AoS Recursive Baseline")
        lines.append("")
        lines.append("Uses AoS immutable recursive when available, otherwise AoS mutable recursive.")
        lines.append("")
        lines.append(table(pass_headers, pass_speed_rows))

    return "\n".join(lines) + "\n"


def write_report(combined: Dict[str, Dict[str, Optional[Dict]]],
                 json_paths: Dict[str, Path],
                 args: argparse.Namespace) -> None:
    args.results_file.parent.mkdir(parents=True, exist_ok=True)
    requested_is_md = args.results_file.suffix.lower() == ".md"
    report = render_report(combined, json_paths, args, colorize_best=requested_is_md)
    args.results_file.write_text(report, encoding="utf-8")
    print(f"\nWrote combined Markdown comparison: {args.results_file}")

    # Keep the Markdown companion fresh even when a run is explicitly pointed at
    # the old .txt filename.  The companion gets colorized best-cell highlights,
    # while the requested .txt report stays plain text/Markdown.
    if not requested_is_md:
        md_path = args.results_file.with_suffix(".md")
        md_path.parent.mkdir(parents=True, exist_ok=True)
        md_report = render_report(combined, json_paths, args, colorize_best=True)
        md_path.write_text(md_report, encoding="utf-8")
        print(f"Wrote Markdown companion: {md_path}")


def parse_args(argv: Optional[Sequence[str]] = None) -> argparse.Namespace:
    ap = argparse.ArgumentParser(
        description="Run AoS/SoA recursive, loopified, selective-sharing, and vectorized benchmark comparisons.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog=textwrap.dedent(
            """\
            This script lives in gibbon-compiler/examples/soa_examples/, next to the
            gibbon_benchmark.py it drives.  It can be run from anywhere; output paths
            default to the repo root regardless of the working directory.

            Examples (from soa_examples/):
              ./benchmark_layout_versions.py --clean --iterations 20 --programs List.hs MonoTree.hs
              ./benchmark_layout_versions.py --32-bit --iterations 20 --results-file layout_comparison_int32.md
              ./benchmark_layout_versions.py --iterations 20 --results-file layout_comparison.md

            Or from the repo root:
              python3 gibbon-compiler/examples/soa_examples/benchmark_layout_versions.py --clean

            By default this runs the normal 64-bit GibInt backend.  Use --32-bit
            to run the same comparison matrix with 32-bit GibInt.

            Reading a --32-bit run:
              * Outputs land under <output-dir>/int32/ (64-bit uses int64/), so the
                two widths never reuse each other's executables.
              * Output-equality checks compare int32 against int32 only.  A program
                whose result exceeds 32 bits will legitimately print a different
                answer than the 64-bit run; verify it against `x mod 2**32` read as
                signed before treating a difference as a bug.
              * To isolate the SIMD-lane effect from the memory-traffic effect, run
                BOTH widths and compare the *marginal* vectorization gain
                (+share -> +share+vec) within each width.  Comparing 32-bit
                wall-clock directly against 64-bit conflates the two: measured on
                this suite, narrowing alone is ~1.93x while the extra SIMD lanes
                contribute ~1.01x.
              * DBQuery.hs is NOT work-equivalent across widths -- it branches on a
                seed-derived value that wraps at 32 bits, so it builds a roughly
                half-size tree under --int32.  Exclude it from cross-width
                comparisons.
            """
        ),
    )
    ap.add_argument("--programs", nargs="+",
                    help="Subset of benchmark source files, e.g. List.hs MonoTree.hs.")
    ap.add_argument("--iterations", type=int, default=20,
                    help="Timed iterations passed to gibbon_benchmark.py. Default: 20.")
    ap.add_argument("--warmup-runs", type=int, default=1,
                    help="Untimed warmup runs before each measured variant. Default: 1.")
    ap.add_argument("--warmup-iterations", type=int, default=1,
                    help="--iterate value for each warmup run. Default: 1.")
    ap.add_argument("--cooldown-seconds", type=float, default=3.0,
                    help="Sleep between variant runs in the child benchmark. Default: 3.0.")
    ap.add_argument("--clean", action="store_true",
                    help="Force recompilation in each benchmark run.")
    ap.add_argument("--dump-raw", action="store_true",
                    help="Ask gibbon_benchmark.py to save raw executable stdout.")
    ap.add_argument("--32-bit", "--int32", dest="use_int32", action="store_true",
                    help="Run every Gibbon layout/optimization variant (AoS/SoA, recursive, "
                         "loopified, +selective sharing, +vectorized) with 32-bit GibInt instead "
                         "of 64-bit. Appends --int32 to every Gibbon compile. Under 32-bit, Int "
                         "vectorization uses 4 SSE2 lanes per group instead of 2. Outputs are "
                         "namespaced under <output-dir>/int32/ so 64-bit executables are never "
                         "reused. Correctness checking stays int32-vs-int32: results that exceed "
                         "32 bits legitimately differ from the 64-bit run by two's-complement "
                         "wraparound and must NOT be compared across widths. Default: 64-bit GibInt.")
    ap.add_argument("--verbose", "-v", action="store_true",
                    help="Stream child benchmark output in real time instead of the compact status bar.")
    # Defaults are anchored to the repo root rather than the current working
    # directory, so results land in the same place whether this is invoked from
    # the repo root or from soa_examples/.  Relative paths given on the command
    # line still resolve against the CWD, as usual.
    ap.add_argument("--output-dir", type=Path,
                    default=REPO_ROOT / "layout_version_benchmark_output",
                    help="Directory for the per-configuration benchmark outputs. "
                         "Default: <repo-root>/layout_version_benchmark_output.")
    ap.add_argument("--results-file", type=Path,
                    default=REPO_ROOT / "layout_version_comparison.md",
                    help="Combined side-by-side Markdown report to write. "
                         "Default: <repo-root>/layout_version_comparison.md.")
    return ap.parse_args(argv)


def main(argv: Optional[Sequence[str]] = None) -> int:
    args = parse_args(argv)
    if not BENCH_SCRIPT.exists():
        print(f"Missing benchmark script: {BENCH_SCRIPT}", file=sys.stderr)
        return 2

    args.output_dir = args.output_dir.resolve()
    args.results_file = args.results_file.resolve()

    json_paths: Dict[str, Path] = {}
    try:
        for idx, config in enumerate(RUN_CONFIGS, start=1):
            args._config_index = idx
            json_paths[config.key] = run_benchmark(config, args)
        combined = collect_results(json_paths)
        write_report(combined, json_paths, args)
    except Exception as exc:
        print(f"error: {exc}", file=sys.stderr)
        return 1
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
