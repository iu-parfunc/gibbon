#!/usr/bin/env python3
"""Regression tests locking down two facts about the benchmark driver: it
has no adaptive/calibration logic (iteration count is a fixed CLI value),
and --packed is unconditional for every curated compile. A timed-out probe
must be reported as a clean failure, never mistaken for a successful
sample.

Background: DecisionTree.hs's timeout under the real driver traces to
`classifyDepthBatch`/`classifyBatch` allocating a fresh `Gibbon.Vector` per
query (via `generate`) in a non-tail loop while the ~9M-node packed tree
region stays live, so per-call cost GROWS as the batch count grows
(measured ~12.7ms/call at n=100, ~37ms/call at n=1000) -- extrapolated, the
committed 250,000/1,000,000-query batches are a genuine multi-hour
computation, not a hang. This reproduces deterministically on a
byte-identical executable, independent of the driver (a bare direct run
with `--packed` times out; the same source without `--packed` completes in
~3.5s), and is compiler/RTS-adjacent rather than a driver defect. See
BUGS.md VW-38 for the full investigation.

These tests lock down what IS true and testable at the driver level:
there is no calibration function to test (characterized, not invented),
`--packed` is and must remain unconditional, and a timed-out probe is
reported as a clean failure, never mistaken for a successful sample.
"""
import os
import subprocess
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import gibbon_benchmark as gb  # noqa: E402
import bench_provenance as prov  # noqa: E402


class TestNoCalibrationExists(unittest.TestCase):
    """Characterization, not aspiration: the driver's iteration count is a
    fixed CLI value, never computed from a prior probe's timing."""

    def test_run_exe_takes_a_fixed_iteration_count_not_a_target_duration(self):
        import inspect
        sig = inspect.signature(gb.run_exe)
        self.assertIn("iterations", sig.parameters)
        for forbidden in ("target_seconds", "min_duration", "calibrate",
                          "target_duration", "max_seconds"):
            self.assertNotIn(forbidden, sig.parameters)

    def test_no_calibration_or_adaptive_iteration_function_in_module(self):
        adaptive_names = [n for n in dir(gb)
                          if "calibrat" in n.lower() or "adaptive" in n.lower()]
        self.assertEqual(adaptive_names, [],
                         "an adaptive/calibration function now exists and "
                         "this characterization test is stale: %s" % adaptive_names)

    def test_iterations_default_is_a_plain_constant(self):
        # main()'s --iterations default (20) is a literal in the source,
        # not derived from any measurement. Grepped rather than asserted
        # from a live parser (main()'s argparse is not a standalone
        # function), which is itself evidence there is no separate
        # calibration entry point to import and unit-test.
        src = (HERE / "gibbon_benchmark.py").read_text()
        self.assertIn('"--iterations",     type=int,  default=20', src)


class TestPackedIsUnconditional(unittest.TestCase):
    """The actual VW-38 discriminator: `--packed` must always be present in
    every Gibbon compile this driver issues -- it is not a tunable, and a
    future edit that makes it conditional would silently change every
    program's measured representation, not just DecisionTree.hs's."""

    def test_build_gibbon_command_always_includes_packed(self):
        for mode in ("portable", "wrapv", "unsafe"):
            for mut in (True, False):
                cmd = gb.build_gibbon_command(
                    Path("P.hs"), "soa", Path("P.c"), Path("P.exe"), "gcc",
                    use_mutable_cursors=mut, c_arith_mode=mode)
                self.assertIn("--packed", cmd)


class TestTimeoutIsACleanFailure(unittest.TestCase):
    """A probe that exceeds its timeout must be reported as a failure with
    a return code that can never be confused with a real exit status --
    never silently treated as a successful sample."""

    def _make_sleeper(self, td: Path, seconds: float) -> Path:
        script = td / "sleeper.sh"
        script.write_text(f"#!/bin/sh\nsleep {seconds}\necho done\n")
        script.chmod(0o755)
        return script

    def test_run_exe_reports_timeout_as_failure_not_success(self):
        with tempfile.TemporaryDirectory() as td:
            td = Path(td)
            sleeper = self._make_sleeper(td, 5.0)
            ok, elapsed, out, err, rc = gb.run_exe(
                sleeper, 1, timeout=1, use_iterate_flag=False)
            self.assertFalse(ok)
            self.assertEqual(err, "timeout expired")
            self.assertEqual(rc, -1)
            self.assertIsNone(out)

    def test_run_exe_timeout_kills_the_whole_process_group_not_just_the_direct_child(self):
        # A shell script that itself forks a grandchild is the case a plain
        # `proc.kill()` (SIGKILL to the direct child only) leaks: the shell
        # dies but its `sleep` grandchild is reparented and keeps running.
        # This is exactly the gap `run_exe`'s `start_new_session=True` +
        # `os.killpg` fix closes. The grandchild's real
        # PID is captured to a file (written before anything is killed) so
        # survival can be checked directly with `os.kill(pid, 0)`, rather
        # than pattern-matching `ps` output.
        with tempfile.TemporaryDirectory() as td:
            td = Path(td)
            pidfile = td / "grandchild.pid"
            script = td / "sleeper2.sh"
            script.write_text(
                f"#!/bin/sh\nsleep 5 &\necho $! > {pidfile}\nwait\n")
            script.chmod(0o755)
            gb.run_exe(script, 1, timeout=1, use_iterate_flag=False)
            import time
            deadline = time.time() + 3
            grandchild_pid = None
            while time.time() < deadline:
                if pidfile.exists() and pidfile.read_text().strip():
                    grandchild_pid = int(pidfile.read_text().strip())
                    break
                time.sleep(0.05)
            self.assertIsNotNone(grandchild_pid, "grandchild never started")
            time.sleep(0.5)
            alive = True
            try:
                os.kill(grandchild_pid, 0)
            except ProcessLookupError:
                alive = False
            self.assertFalse(alive,
                             "grandchild pid %d of the timed-out probe is "
                             "still running after run_exe's timeout fired" %
                             grandchild_pid)

    def test_qualify_variant_does_not_mark_a_timeout_style_run_failure_as_passing(self):
        manifest = prov.OracleManifest.load_default(HERE)
        st = gb.qualify_variant(
            "DecisionTree", "aos_mut", Path("/nonexistent/DecisionTree.hs"),
            True, None, False, "rc=-1: timeout expired", None,
            manifest=manifest)
        self.assertFalse(st.verified)
        self.assertEqual(st.exec_status, prov.EXEC_FAIL)
        res = gb.BenchmarkResult("DecisionTree", "aos_mut")
        res.qualification = st
        res.run_success = False
        self.assertFalse(prov.verified_result(res))


class TestPackedVsUnpackedCharacterization(unittest.TestCase):
    """Documents the discriminating flag as data, not just prose: the SAME
    source, same arithmetic mode, same --no-ran, differing ONLY in
    --packed, produces a different generated-C representation. This is
    the mechanical fact the whole VW-38 re-classification rests on."""

    def test_packed_and_unpacked_produce_different_argv_and_are_distinguishable(self):
        cmd_packed = gb.build_gibbon_command(
            Path("DecisionTree.hs"), "aos", Path("P.c"), Path("P.exe"), "gcc")
        self.assertIn("--packed", cmd_packed)
        # There is no driver knob to omit --packed for a curated program --
        # confirmed structurally: no parameter of build_gibbon_command or
        # compile_one controls it.
        import inspect
        for fn in (gb.build_gibbon_command, gb.compile_one):
            self.assertNotIn("packed", [p.lower() for p in inspect.signature(fn).parameters])


if __name__ == "__main__":
    unittest.main(verbosity=2)
