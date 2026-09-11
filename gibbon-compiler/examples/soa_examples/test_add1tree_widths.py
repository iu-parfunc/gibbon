#!/usr/bin/env python3
"""Permanent regression tests: Add1TreeIntN.hs source policy, the
independent oracle's design verification, and the 'Integer-width add1Tree
vectorization' table's eligibility gating.

Three independent layers:
  - TestAdd1TreeOracleModel: the Python oracle (oracles/add1tree_model.py)
    is internally sound -- verify_design() proves all six failure classes
    are distinguished at every width -- and its committed-size values are
    pinned against a hardcoded regression value (caught by a fresh
    independent re-derivation, not copied from Gibbon's own output).
  - TestAdd1TreeSourceWidths: static source checks over the eight
    Add1TreeIntN.hs files (AoS+SoA), reusing this suite's own
    parse_data_decl/parse_function_signatures helpers -- payload width
    correctness, no bare Int in a semantic signature, AoS/SoA algorithm
    equivalence, no avoidable toInt* inside add1Tree, program/oracle
    registration.
  - TestAdd1TreeWidthTable: the gibbon_benchmark.py table renderer,
    exercised ONLY with synthetic BenchmarkResult/QualificationStatus
    fixtures (no real timing) -- proves an unverified, missing-oracle,
    failed or empty-output variant can never reach a numeric cell, total,
    or speedup.
"""
import re
import sys
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
PROGRAMS_AOS = HERE / "programs" / "AOS"
PROGRAMS_SOA = HERE / "programs" / "SOA"
sys.path.insert(0, str(HERE))
sys.path.insert(0, str(HERE / "oracles"))

import add1tree_model as model  # noqa: E402
import gibbon_benchmark as gb  # noqa: E402
import bench_provenance as prov  # noqa: E402
from test_width_migration import parse_data_decl, parse_function_signatures  # noqa: E402

WIDTHS = (8, 16, 32, 64)
LANES = {8: 16, 16: 8, 32: 4, 64: 2}


class TestAdd1TreeOracleModel(unittest.TestCase):
    def test_design_verification_passes_at_every_width(self):
        problems = model.verify_design()
        self.assertEqual(problems, [],
                         "add1tree_model design no longer distinguishes: %s" % problems)

    def test_committed_size_values_are_pinned(self):
        # Regression pin: independently re-derive, then compare against the
        # value recorded from that independent derivation
        # (oracles/manifest.json) -- never copied from Gibbon.
        # Depth 33 values (owner raised the size on 2026-09-06); the
        # superseded depth-20 values were
        # {8: -52, 16: 4044, 32: -1583607860, 64: 5210117493961265100}.
        expected = {8: -78, 16: -31310, 32: -195656270,
                    64: 2014808968409810354}
        for width, exp in expected.items():
            self.assertEqual(model.expected(width), exp,
                             "width %d oracle value drifted" % width)

    def test_leaf_count_matches_documented_fibonacci_shape(self):
        # Fib(35). Read from the closed form, not by building the tree --
        # at depth 33 an explicit tree is 9.2M nodes.
        self.assertEqual(model.leaf_count(), 9227465)

    def test_closed_form_agrees_with_the_literal_build_map_fold(self):
        # `expected` folds the recurrence in closed form so DEPTH0 is
        # affordable; this pins it against the literal implementation at
        # depths small enough to build both ways.
        for depth in (0, 1, 5, 12, 20):
            for width in WIDTHS:
                self.assertEqual(
                    model.expected(width, depth, model.SEED0),
                    model.expected_explicit(width, depth, model.SEED0),
                    "closed form differs from explicit fold at depth %d, "
                    "width %d" % (depth, width))

    def test_model_depth_matches_every_source_file(self):
        # The failure this exists to catch: an owner edits the input size in
        # the .hs sources and the committed oracle values silently keep
        # describing the old tree. Every Add1TreeIntN.hs in both layouts
        # must build exactly the tree the model scores.
        depths = model.source_depths()
        self.assertEqual(len(depths), 8,
                         "expected 8 Add1TreeIntN.hs sources, found %d" % len(depths))
        for path, (depth, seed) in depths.items():
            self.assertEqual((depth, seed), (model.DEPTH0, model.SEED0),
                             "%s builds mkTree %d %d but the oracle models "
                             "mkTree %d %d" % (path, depth, seed,
                                               model.DEPTH0, model.SEED0))

    def test_manifest_matches_the_model(self):
        import json
        manifest = json.loads(
            (HERE / "oracles" / "manifest.json").read_text())["oracles"]
        for width in WIDTHS:
            self.assertEqual(manifest["Add1TreeInt%d" % width]["expected"],
                             str(model.expected(width)),
                             "manifest Add1TreeInt%d is stale" % width)


class TestAdd1TreeSourceWidths(unittest.TestCase):
    """Fails loudly on the same regression classes test_width_migration.py
    guards for the curated 22, applied to the four Add1TreeIntN.hs files."""

    def _path(self, variant_dir, width):
        return variant_dir / ("Add1TreeInt%d.hs" % width)

    def test_every_file_exists_both_layouts(self):
        for width in WIDTHS:
            for d in (PROGRAMS_AOS, PROGRAMS_SOA):
                self.assertTrue(self._path(d, width).exists(), "%s missing" % self._path(d, width))

    def test_tree_payload_field_matches_declared_width(self):
        for width in WIDTHS:
            for d in (PROGRAMS_AOS, PROGRAMS_SOA):
                text = self._path(d, width).read_text()
                ctors = parse_data_decl(text, "Tree")
                self.assertEqual(ctors["Leaf"], ["Int%d" % width],
                                 "%s: Leaf field is %r, want Int%d" %
                                 (self._path(d, width), ctors["Leaf"], width))

    def test_no_bare_int_in_semantic_signatures(self):
        """add1Tree/checksumTree must be exactly this file's own width --
        never bare Int (Int64) -- in both argument and return position."""
        bare_int = re.compile(r"\bInt\b")
        sig_re = re.compile(r"^([A-Za-z_][A-Za-z0-9_']*)\s*::\s*(.+)$", re.MULTILINE)
        failures = []
        for width in WIDTHS:
            for d in (PROGRAMS_AOS, PROGRAMS_SOA):
                text = self._path(d, width).read_text()
                for m in sig_re.finditer(text):
                    name, rhs = m.group(1), m.group(2)
                    if name not in ("add1Tree", "checksumTree"):
                        continue
                    if bare_int.search(rhs):
                        failures.append("%s %s :: %s" % (self._path(d, width).name, name, rhs.strip()))
        self.assertEqual(failures, [], "\n".join(failures))

    def test_add1tree_body_has_no_avoidable_conversion(self):
        toint_re = re.compile(r"toInt\d+")
        for width in WIDTHS:
            for d in (PROGRAMS_AOS, PROGRAMS_SOA):
                text = self._path(d, width).read_text()
                m = re.search(r"^add1Tree ::.*?\n(?:add1Tree .*\n(?:  .*\n)*)+", text, re.MULTILINE)
                self.assertIsNotNone(m, "%s: could not isolate add1Tree body" % self._path(d, width))
                body = m.group(0)
                self.assertNotIn("toInt", body,
                                 "%s: avoidable conversion inside add1Tree: %s" %
                                 (self._path(d, width), toint_re.findall(body)))

    def test_aos_and_soa_agree_on_every_function_signature(self):
        mismatches = []
        for width in WIDTHS:
            aos_sigs = parse_function_signatures(self._path(PROGRAMS_AOS, width).read_text())
            soa_sigs = parse_function_signatures(self._path(PROGRAMS_SOA, width).read_text())
            for name in sorted(set(aos_sigs) & set(soa_sigs)):
                if aos_sigs[name] != soa_sigs[name]:
                    mismatches.append("Int%d.%s: AOS=%r SOA=%r" %
                                      (width, name, aos_sigs[name], soa_sigs[name]))
        self.assertEqual(mismatches, [])

    def test_soa_declares_factored_and_aos_declares_linear(self):
        for width in WIDTHS:
            aos_text = self._path(PROGRAMS_AOS, width).read_text()
            soa_text = self._path(PROGRAMS_SOA, width).read_text()
            self.assertIn('{-# ANN type Tree "Linear" #-}', aos_text)
            self.assertIn('{-# ANN type Tree "Factored" #-}', soa_text)

    def test_add1tree_is_mayvectorize_annotated_both_layouts(self):
        for width in WIDTHS:
            for d in (PROGRAMS_AOS, PROGRAMS_SOA):
                text = self._path(d, width).read_text()
                self.assertIn('{-# ANN add1Tree "OPT:MayVectorize" #-}', text,
                             "%s missing MayVectorize annotation" % self._path(d, width))

    def test_no_toint64_toint32_round_trip(self):
        pattern = re.compile(r"toInt64\s*\(\s*toInt32|toInt32\s*\(\s*toInt64|"
                             r"toInt\d+\s*\(\s*toInt\d+")
        for width in WIDTHS:
            for d in (PROGRAMS_AOS, PROGRAMS_SOA):
                text = self._path(d, width).read_text()
                self.assertIsNone(pattern.search(text),
                                  "%s: width round trip found" % self._path(d, width))

    def test_registered_in_gibbon_benchmark(self):
        for width in WIDTHS:
            self.assertIn("Add1TreeInt%d.hs" % width, gb.ADD1TREE_WIDTH_PROGRAMS)
        # Deliberately NOT in DEFAULT_PROGRAMS -- opt-in only.
        for width in WIDTHS:
            self.assertNotIn("Add1TreeInt%d.hs" % width, gb.DEFAULT_PROGRAMS)

    def test_registered_in_oracle_manifest(self):
        import json
        manifest = json.loads((HERE / "oracles" / "manifest.json").read_text())["oracles"]
        for width in WIDTHS:
            key = "Add1TreeInt%d" % width
            self.assertIn(key, manifest, "%s missing from oracles/manifest.json" % key)
            entry = manifest[key]
            self.assertIn(entry.get("provenance"), prov.ORACLE_PROVENANCES)
            self.assertEqual(entry["expected"], str(model.expected(width)))


# ---------------------------------------------------------------------------
# Synthetic BenchmarkResult/QualificationStatus fixtures -- no real timing
# is performed here, so the table renderer is exercised only against
# fabricated data, explicitly labeled as test fixtures.
# ---------------------------------------------------------------------------
def _make_result(program, variant, verified, median_time=None, oracle_status=None):
    # NOTE: `median_time if median_time is not None else 1.0`, NOT
    # `median_time or 1.0` -- the latter would silently replace an
    # intentional 0.0 (used by
    # test_speedup_never_computed_from_non_positive_metric) with 1.0,
    # since 0.0 is falsy in Python.
    t = median_time if median_time is not None else 1.0
    res = gb.BenchmarkResult(program, variant)
    st = prov.QualificationStatus(variant, program)
    if verified:
        st.compile_status = prov.COMPILE_OK
        st.exec_status = prov.EXEC_OK
        st.oracle_status = prov.ORACLE_PASS
        st.semantic_output = "42"
        res.passes = {"add1Tree": {"median_time": t, "pass_type": "map"}}
    else:
        st.compile_status = prov.COMPILE_OK
        st.exec_status = prov.EXEC_OK
        st.oracle_status = oracle_status or prov.ORACLE_FAIL
        st.oracle_detail = "synthetic test fixture: deliberately unverified"
        st.semantic_output = "42" if oracle_status != prov.ORACLE_MISSING else None
        res.passes = {"add1Tree": {"median_time": t, "pass_type": "map"}}
    res.compile_success = True
    res.run_success = True
    res.qualification = st
    return res


class TestAdd1TreeWidthTable(unittest.TestCase):
    """The new table (gibbon_benchmark._table_add1tree_widths) must render
    every unverified/missing cell as N/A and must never let an unverified
    metric reach a speedup, exactly like every other table in this file --
    exercised here with synthetic fixtures only (labeled as such), since
    no real timing is performed in this suite."""

    def _render(self, results_by_width):
        import io
        buf = io.StringIO()
        gb._table_add1tree_widths(buf, results_by_width)
        return buf.getvalue()

    def test_all_verified_renders_numeric_cells_and_speedups(self):
        results = {32: {
            "aos_mut": _make_result("Add1TreeInt32.hs", "aos_mut", True, median_time=2.0),
            "soa_mut": _make_result("Add1TreeInt32.hs", "soa_mut", True, median_time=1.0),
            "soa_loopify": _make_result("Add1TreeInt32.hs", "soa_loopify", True, median_time=1.0),
            "soa_simd": _make_result("Add1TreeInt32.hs", "soa_simd", True, median_time=0.5),
        }}
        out = self._render(results)
        self.assertIn("Int32", out)
        self.assertIn("2.000000", out)   # aos raw
        self.assertIn("1.000000", out)   # soa raw / loopify raw
        self.assertIn("0.500000", out)   # simd raw
        self.assertIn(r"$\times$", out)  # a speedup was rendered
        self.assertNotIn("N/A", out)

    def test_unverified_variant_renders_na_not_a_number(self):
        results = {8: {
            "aos_mut": _make_result("Add1TreeInt8.hs", "aos_mut", True, median_time=1.0),
            "soa_mut": _make_result("Add1TreeInt8.hs", "soa_mut", False),  # oracle FAIL
            "soa_loopify": _make_result("Add1TreeInt8.hs", "soa_loopify", True, median_time=1.0),
            "soa_simd": _make_result("Add1TreeInt8.hs", "soa_simd", True, median_time=0.5),
        }}
        out = self._render(results)
        lines = [l for l in out.splitlines() if l.startswith("Int8")]
        self.assertEqual(len(lines), 1)
        row = lines[0]
        # SoA raw must be N/A (unverified), and any speedup that depends on
        # SoA (aos/soa comparison) must also be N/A, never a number derived
        # from the unverified 1.0 that was stashed in soa_mut's passes dict.
        self.assertIn("N/A", row)
        cells = row.split("&")
        soa_raw_cell = cells[2].strip()
        self.assertEqual(soa_raw_cell, "N/A")
        aos_over_soa_cell = cells[3].strip()
        self.assertTrue(aos_over_soa_cell.startswith("N/A"))

    def test_missing_config_renders_na_not_crash(self):
        results = {16: {
            "aos_mut": _make_result("Add1TreeInt16.hs", "aos_mut", True, median_time=1.0),
            # soa_mut, soa_loopify, soa_simd deliberately absent
        }}
        out = self._render(results)
        self.assertIn("Int16", out)
        self.assertIn("N/A", out)

    def test_speedup_never_computed_from_non_positive_metric(self):
        # safe_speedup's contract (bench_provenance.py) is `numerator /
        # denominator`, defined only when the DENOMINATOR is positive (a
        # zero numerator is a legitimate, if degenerate, "no measurable
        # time" result, not an error) -- so the non-positive metric belongs
        # on the denominator (soa_simd) here, not the numerator.
        results = {64: {
            "soa_loopify": _make_result("Add1TreeInt64.hs", "soa_loopify", True, median_time=1.0),
            "soa_simd": _make_result("Add1TreeInt64.hs", "soa_simd", True, median_time=0.0),
        }}
        spd, reason = prov.safe_speedup(
            results[64]["soa_loopify"], results[64]["soa_simd"], gb.total_pass_time)
        self.assertIsNone(spd)
        self.assertIn("not positive", reason)

    def test_empty_output_result_is_not_verified(self):
        res = _make_result("Add1TreeInt32.hs", "soa_mut", False, oracle_status=prov.ORACLE_MISSING)
        self.assertFalse(prov.verified_result(res))

    def test_missing_oracle_result_is_not_verified(self):
        res = gb.BenchmarkResult("Add1TreeInt32.hs", "soa_mut")
        st = prov.QualificationStatus("soa_mut", "Add1TreeInt32.hs")
        st.compile_status = prov.COMPILE_OK
        st.exec_status = prov.EXEC_OK
        st.oracle_status = prov.ORACLE_MISSING
        res.qualification = st
        self.assertFalse(prov.verified_result(res))


# ---------------------------------------------------------------------------
# Mutation tests: representative source-width, vectorization-annotation and
# eligibility regressions, applied to real files (with restore) and to the
# synthetic-fixture path, exactly mirroring test_width_migration.py's own
# mutation-testing discipline for the curated 22.
# ---------------------------------------------------------------------------
class TestAdd1TreeMutations(unittest.TestCase):
    def test_wrong_payload_width_is_caught(self):
        path = PROGRAMS_AOS / "Add1TreeInt32.hs"
        backup = path.read_text()
        try:
            mutated = backup.replace("data Tree = Leaf Int32", "data Tree = Leaf Int64", 1)
            path.write_text(mutated)
            ctors = parse_data_decl(path.read_text(), "Tree")
            self.assertNotEqual(ctors["Leaf"], ["Int32"])
        finally:
            path.write_text(backup)
        self.assertEqual(path.read_text(), backup)

    def test_aos_soa_disagreement_is_caught(self):
        path = PROGRAMS_AOS / "Add1TreeInt16.hs"
        backup = path.read_text()
        try:
            mutated = backup.replace(
                "checksumTree :: Tree -> Int16 -> Int16",
                "checksumTree :: Tree -> Int16 -> Int", 1)
            path.write_text(mutated)
            aos_sigs = parse_function_signatures(path.read_text())
            soa_sigs = parse_function_signatures((PROGRAMS_SOA / "Add1TreeInt16.hs").read_text())
            self.assertNotEqual(aos_sigs.get("checksumTree"), soa_sigs.get("checksumTree"))
        finally:
            path.write_text(backup)
        self.assertEqual(path.read_text(), backup)

    def test_unverified_result_cannot_reach_table_as_a_number(self):
        """A deliberately WRONG oracle status (simulating a speedup
        calculated with a missing/non-positive metric) must still render
        N/A, never a fabricated number."""
        results = {32: {
            "aos_mut": _make_result("Add1TreeInt32.hs", "aos_mut", False, oracle_status=prov.ORACLE_FAIL),
            "soa_mut": _make_result("Add1TreeInt32.hs", "soa_mut", False, oracle_status=prov.ORACLE_FAIL),
        }}
        import io
        buf = io.StringIO()
        gb._table_add1tree_widths(buf, results)
        out = buf.getvalue()
        row = [l for l in out.splitlines() if l.startswith("Int32")][0]
        cells = [c.strip() for c in row.split("&")]
        self.assertEqual(cells[1], "N/A")
        self.assertEqual(cells[2], "N/A")


if __name__ == "__main__":
    unittest.main()
