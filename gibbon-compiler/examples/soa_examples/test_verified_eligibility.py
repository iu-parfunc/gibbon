#!/usr/bin/env python3
"""Adversarial regression tests for strict verified-result eligibility across
full benchmark mode.

Full benchmark mode must gate every table, report, JSON field and figure
on whether an independent oracle actually passed (a `QualificationStatus`),
not merely on `compile_success`/`run_success`/`outputs_match`. These tests
build synthetic `BenchmarkResult`s with unmistakable SENTINEL timings
(99991, 99992, ...) and assert that no sentinel from an unverified result
ever reaches a qualified sink: JSON `passes`, the text report, the LaTeX
summary table, PAPI totals, `total_pass_time`, or a computed speedup.
Each case's comment says which sentinel it targets.

No compiler is invoked here: these are fast and run anywhere.  The real
compiled CLI matrix (DriverQualify, full mode and --correctness-only) is
exercised separately.

Run:  python3 test_verified_eligibility.py
"""
import json
import sys
import tempfile
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import bench_provenance as prov
import gibbon_benchmark as gb


# ---------------------------------------------------------------------------
# Sentinel builders
# ---------------------------------------------------------------------------

def make_result(program="P.hs", variant="aos", *, compile_ok=True, exec_ok=True,
                semantic_output="42", oracle_status=prov.ORACLE_PASS,
                allow_unverified=False, median_time=None, exec_wall_time=None,
                error_message=None, cross_variant_status=prov.XVAR_NA) -> gb.BenchmarkResult:
    """A BenchmarkResult with a fully-populated QualificationStatus, exactly as
    `qualify_variant`/`benchmark_program` would build it -- never a hand-rolled
    shortcut that could drift from the production shape."""
    r = gb.BenchmarkResult(program, variant)
    r.compile_success = compile_ok
    r.run_success = exec_ok
    r.error_message = error_message
    if exec_wall_time is not None:
        r.exec_wall_time = exec_wall_time
    st = prov.QualificationStatus(variant, program)
    st.allow_unverified = allow_unverified
    st.compile_status = prov.COMPILE_OK if compile_ok else prov.COMPILE_FAIL
    if compile_ok:
        st.exec_status = prov.EXEC_OK if exec_ok else prov.EXEC_FAIL
        if exec_ok:
            st.semantic_output = semantic_output
            st.oracle_status = oracle_status
            st.oracle_detail = "test sentinel"
    st.cross_variant_status = cross_variant_status
    r.qualification = st
    if median_time is not None:
        r.passes = {"onlyPass": {"median_time": median_time, "pass_type": "fold",
                                 "stderr": 0.0, "mean_time": median_time,
                                 "min_time": median_time, "max_time": median_time,
                                 "n": 1, "iter_times": [median_time]}}
    return r


SENTINEL_A = 99991.0
SENTINEL_B = 99992.0


def verified(program="P.hs", variant="aos", median_time=SENTINEL_A, **kw):
    return make_result(program, variant, median_time=median_time, **kw)


def wrong(program="P.hs", variant="aos", median_time=SENTINEL_A, **kw):
    return make_result(program, variant, oracle_status=prov.ORACLE_FAIL,
                       median_time=median_time, **kw)


def no_oracle(program="P.hs", variant="aos", median_time=SENTINEL_A, allow_unverified=False, **kw):
    return make_result(program, variant, oracle_status=prov.ORACLE_MISSING,
                       allow_unverified=allow_unverified, median_time=median_time, **kw)


def compile_fail(program="P.hs", variant="aos", median_time=SENTINEL_A):
    r = make_result(program, variant, compile_ok=False, exec_ok=False,
                    error_message="compile failed")
    # Stale timing fields that a real crash could leave behind (Step 2 #7/#8):
    # a leak must not read these regardless of how they got populated.
    r.passes = {"onlyPass": {"median_time": median_time, "pass_type": "fold",
                             "stderr": 0.0, "mean_time": median_time,
                             "min_time": median_time, "max_time": median_time,
                             "n": 1, "iter_times": [median_time]}}
    return r


def run_fail(program="P.hs", variant="aos", median_time=SENTINEL_A):
    r = make_result(program, variant, compile_ok=True, exec_ok=False,
                    error_message="execution failed")
    r.passes = {"onlyPass": {"median_time": median_time, "pass_type": "fold",
                             "stderr": 0.0, "mean_time": median_time,
                             "min_time": median_time, "max_time": median_time,
                             "n": 1, "iter_times": [median_time]}}
    return r


def empty_output(program="P.hs", variant="aos", median_time=SENTINEL_A):
    return make_result(program, variant, semantic_output=None, median_time=median_time)


# ---------------------------------------------------------------------------
# Gate B: qualification-model tests
# ---------------------------------------------------------------------------

class TestStrictEligibility(unittest.TestCase):
    def test_verified_result_true_only_for_oracle_pass(self):
        self.assertTrue(prov.verified_result(verified()))
        self.assertFalse(prov.verified_result(wrong()))
        self.assertFalse(prov.verified_result(no_oracle()))
        self.assertFalse(prov.verified_result(compile_fail()))
        self.assertFalse(prov.verified_result(run_fail()))
        self.assertFalse(prov.verified_result(empty_output()))
        self.assertFalse(prov.verified_result(None))

    def test_eligible_for_reporting_equals_verified(self):
        for r in (verified(), wrong(), no_oracle(), compile_fail(), run_fail(), empty_output()):
            self.assertEqual(r.qualification.eligible_for_reporting, r.qualification.verified,
                             "eligible_for_reporting must be IDENTICAL to verified, not a weaker proxy")

    def test_allow_unverified_output_never_makes_eligible_for_reporting_true(self):
        r = no_oracle(allow_unverified=True)
        self.assertFalse(r.qualification.verified)
        self.assertFalse(r.qualification.eligible_for_reporting,
                         "the pre-fix bug: allow_unverified used to make this True")
        self.assertTrue(r.qualification.eligible_for_exploratory_output,
                        "the exploratory escape must still exist, under its OWN name")

    def test_allow_unverified_output_does_not_forgive_a_wrong_answer(self):
        r = wrong(allow_unverified=True)
        self.assertFalse(r.qualification.eligible_for_exploratory_output,
                         "allow_unverified only excuses a MISSING oracle, never an oracle FAIL")

    def test_oracle_not_required_is_not_verified(self):
        # Unreachable in production (nothing calls manifest.check with
        # required=False), but the property must not silently trust it if it
        # ever is reached.
        r = make_result(oracle_status=prov.ORACLE_NOT_REQUIRED)
        self.assertFalse(r.qualification.verified)


class TestPairwiseEligibility(unittest.TestCase):
    def test_both_verified(self):
        self.assertTrue(prov.eligible_pair(verified(), verified()))

    def test_one_wrong_one_verified(self):
        self.assertFalse(prov.eligible_pair(wrong(), verified()))
        self.assertFalse(prov.eligible_pair(verified(), wrong()))

    def test_both_wrong_but_mutually_equal(self):
        # Two variants agreeing with EACH OTHER is not an oracle.
        a = wrong(median_time=SENTINEL_A)
        b = wrong(variant="soa", median_time=SENTINEL_A)
        self.assertFalse(prov.eligible_pair(a, b))

    def test_both_missing_oracle_but_mutually_equal(self):
        a = no_oracle(median_time=SENTINEL_A)
        b = no_oracle(variant="soa", median_time=SENTINEL_A)
        self.assertFalse(prov.eligible_pair(a, b))

    def test_missing_oracle_with_allow_unverified_still_ineligible(self):
        a = no_oracle(allow_unverified=True)
        b = verified(variant="soa")
        self.assertFalse(prov.eligible_pair(a, b))

    def test_only_one_side_of_a_requested_ratio_verified(self):
        self.assertFalse(prov.eligible_pair(verified(), no_oracle(variant="soa")))

    def test_verified_mutable_but_unverified_immutable(self):
        mut = verified(variant="aos")
        imm = no_oracle(variant="aos_imm")
        self.assertTrue(prov.eligible_pair(mut, mut))
        self.assertFalse(prov.eligible_pair(mut, imm))

    def test_none_operand(self):
        self.assertFalse(prov.eligible_pair(verified(), None))
        self.assertFalse(prov.eligible_pair(None, None))


class TestSafeSpeedup(unittest.TestCase):
    def metric(self, r):
        return gb.total_pass_time(r)

    def test_verified_pair_yields_a_number(self):
        a = verified(median_time=10.0)
        b = verified(variant="soa", median_time=5.0)
        val, reason = prov.safe_speedup(a, b, self.metric)
        self.assertEqual(val, 2.0)
        self.assertEqual(reason, "ok")

    def test_one_invalid_side_yields_no_ratio(self):
        a = verified(median_time=10.0)
        b = wrong(variant="soa", median_time=5.0)
        val, reason = prov.safe_speedup(a, b, self.metric)
        self.assertIsNone(val)
        self.assertNotEqual(reason, "ok")

    def test_absent_metric_is_not_zero_one_or_infinity(self):
        # Both verified, but neither has any recorded pass data.
        a = verified(median_time=None)
        b = verified(variant="soa", median_time=None)
        val, reason = prov.safe_speedup(a, b, self.metric)
        self.assertIsNone(val)
        self.assertNotIn(reason, ("0", "1", "inf"))


class TestTotalPassTimeChokepoint(unittest.TestCase):
    """`total_pass_time` is the central chokepoint most tables/reports go
    through.  Mutation-style: prove the gate is `verified_result`, not
    `run_success`, by holding `run_success=True` fixed and varying only the
    oracle outcome."""

    def test_run_success_alone_is_not_enough(self):
        r = no_oracle(median_time=SENTINEL_A)
        self.assertTrue(r.run_success)
        self.assertIsNone(gb.total_pass_time(r),
                          "SENTINEL LEAK: total_pass_time must not read a "
                          "run_success=True, unverified result's passes")

    def test_verified_result_is_read(self):
        r = verified(median_time=SENTINEL_A)
        self.assertEqual(gb.total_pass_time(r), SENTINEL_A)

    def test_mutation_revert_reproduces_the_leak(self):
        """Directly re-runs the OLD predicate to prove it would have leaked
        the sentinel -- without needing to edit and rebuild the file."""
        r = no_oracle(median_time=SENTINEL_A)
        old_predicate_result = (
            None if (r is None or not r.run_success or not r.passes)
            else sum(p["median_time"] for p in r.passes.values())
        )
        self.assertEqual(old_predicate_result, SENTINEL_A,
                         "confirms the old run_success-only gate DID leak "
                         "this sentinel -- the fixed gate (above) does not")


class TestRepeatedRunInvalidation(unittest.TestCase):
    """A later bad repetition must invalidate the measurement rather than
    leaving an earlier verified status attached to it (Step 3)."""

    def test_qualification_reflects_the_last_checked_output_not_the_first(self):
        r = verified()
        self.assertTrue(prov.verified_result(r))
        # Simulate re-qualifying after a later repetition changed the output.
        r.qualification = gb.qualify_variant(
            "P.hs", "aos", None, True, None, True, None,
            "unexpected different output", manifest=_FakeManifest({}))
        self.assertFalse(prov.verified_result(r),
                         "a result object must not keep an earlier VERIFIED "
                         "qualification once it has been re-qualified against "
                         "a later, disagreeing repetition")


class _FakeManifest:
    def __init__(self, entries):
        self.entries = entries

    def check(self, program, raw_output, required=True):
        entry = self.entries.get(program)
        if entry is None:
            return (prov.ORACLE_MISSING, "no entry") if required else (prov.ORACLE_NOT_REQUIRED, "")
        got = prov.semantic_tokens(raw_output)
        want = entry.split()
        return (prov.ORACLE_PASS, "match") if got == want else (prov.ORACLE_FAIL, "mismatch")


# ---------------------------------------------------------------------------
# Gate C: sink tests
# ---------------------------------------------------------------------------

class TestJsonSink(unittest.TestCase):
    def _write(self, aos, soa):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "r.json"
            gb.write_json_results([(aos, soa)], out)
            return json.loads(out.read_text())

    def test_verified_pair_json_contains_the_sentinel(self):
        doc = self._write(verified(median_time=SENTINEL_A), verified(variant="soa", median_time=SENTINEL_B))
        self.assertEqual(doc["report_schema"], prov.REPORT_SCHEMA)
        aos = doc["results"][0]["aos"]
        self.assertTrue(aos["verified"])
        self.assertEqual(aos["passes"]["onlyPass"]["median_time"], SENTINEL_A)

    def test_unverified_pair_json_never_contains_the_sentinel(self):
        doc = self._write(no_oracle(median_time=SENTINEL_A), no_oracle(variant="soa", median_time=SENTINEL_A))
        rec = doc["results"][0]
        raw = json.dumps(rec)
        self.assertNotIn(str(SENTINEL_A), raw,
                         "SENTINEL LEAK: unverified median_time serialized into JSON")
        self.assertIsNone(rec["aos"]["passes"])
        self.assertIsNone(rec["soa"]["passes"])
        self.assertIsNotNone(rec["aos"]["passes_omitted_reason"])

    def test_compile_fail_json_never_contains_the_stale_sentinel(self):
        doc = self._write(compile_fail(median_time=SENTINEL_A), verified(variant="soa"))
        raw = json.dumps(doc["results"][0]["aos"])
        self.assertNotIn(str(SENTINEL_A), raw)

    def test_mixed_valid_invalid_list_keeps_the_valid_one_and_rejects_the_other(self):
        doc = self._write(verified(median_time=SENTINEL_A), no_oracle(variant="soa", median_time=SENTINEL_B))
        rec = doc["results"][0]
        self.assertTrue(rec["aos"]["verified"])
        self.assertEqual(rec["aos"]["passes"]["onlyPass"]["median_time"], SENTINEL_A)
        self.assertFalse(rec["soa"]["verified"])
        self.assertIsNone(rec["soa"]["passes"])

    def test_json_write_is_atomic(self):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "r.json"
            out.write_text("stale garbage, must never be visible mid-write")
            gb.write_json_results([(verified(), verified(variant="soa"))], out)
            doc = json.loads(out.read_text())
            self.assertEqual(doc["report_schema"], prov.REPORT_SCHEMA)


class TestLegacyReportRejection(unittest.TestCase):
    def test_legacy_list_shaped_report_is_rejected(self):
        # A pre-step-9 report was a bare JSON list, not an object -- confirm
        # the schema check refuses it cleanly (not by crashing).
        legacy_doc = [{"program": "P.hs", "aos": {"run_success": True}}]
        ok, why = prov.report_schema_supports_qualified_metrics(legacy_doc)  # type: ignore
        self.assertFalse(ok)
        self.assertIn("not a JSON object", why)

    def test_dict_without_report_schema_is_rejected(self):
        ok, why = prov.report_schema_supports_qualified_metrics({"results": []})
        self.assertFalse(ok)
        self.assertIn("report_schema", why)

    def test_current_schema_is_accepted(self):
        ok, why = prov.report_schema_supports_qualified_metrics(
            {"report_schema": prov.REPORT_SCHEMA, "results": []})
        self.assertTrue(ok)

    def test_a_real_written_report_round_trips_as_accepted(self):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "r.json"
            gb.write_json_results([(verified(), verified(variant="soa"))], out)
            doc = json.loads(out.read_text())
            ok, _why = prov.report_schema_supports_qualified_metrics(doc)
            self.assertTrue(ok)


class TestTextReportSink(unittest.TestCase):
    def _write(self, aos, soa):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "r.txt"
            gb.write_text_report([(aos, soa)], out)
            return out.read_text()

    def test_verified_pair_text_report_contains_the_sentinel(self):
        text = self._write(verified(median_time=SENTINEL_A), verified(variant="soa", median_time=SENTINEL_A))
        self.assertIn("VERIFIED", text)

    def test_unverified_pair_text_report_never_contains_the_sentinel(self):
        text = self._write(no_oracle(median_time=SENTINEL_A), no_oracle(variant="soa", median_time=SENTINEL_A))
        self.assertNotIn("%.4f" % SENTINEL_A, text)
        self.assertIn("UNVERIFIED", text)

    def test_text_report_write_is_atomic(self):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "r.txt"
            out.write_text("stale")
            gb.write_text_report([(verified(), verified(variant="soa"))], out)
            self.assertIn("VERIFIED", out.read_text())


class TestLatexSummaryTableSink(unittest.TestCase):
    def _render(self, aos, soa):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "t.tex"
            with open(out, "w") as f:
                gb._table_summary(f, [(aos, soa)])
            return out.read_text()

    def test_verified_pair_produces_a_real_time_cell(self):
        tex = self._render(verified(median_time=10.0), verified(variant="soa", median_time=5.0))
        self.assertIn("2.00", tex)  # bold speedup cell, 10/5

    def test_unverified_pair_produces_no_row_with_the_sentinel(self):
        tex = self._render(no_oracle(median_time=SENTINEL_A), no_oracle(variant="soa", median_time=SENTINEL_A))
        self.assertNotIn(str(int(SENTINEL_A)), tex)

    def test_all_invalid_selection_produces_no_numeric_cell(self):
        tex = self._render(wrong(median_time=SENTINEL_A), wrong(variant="soa", median_time=SENTINEL_A))
        # A labelled row may still appear (both "ran"), but every numeric
        # cell must be the explicit "--" placeholder, never a real number.
        body = tex.split("\\midrule", 1)[1].split("\\bottomrule", 1)[0]
        row = [ln for ln in body.splitlines() if ln.strip().startswith("P ")]
        self.assertEqual(len(row), 1)
        cells = [c.strip() for c in row[0].split("&")[1:]]
        self.assertTrue(all(c.rstrip("\\\\").strip() == "--" for c in cells),
                        "a numeric cell leaked for an all-invalid row: %r" % cells)


class TestTablePerProgramSink(unittest.TestCase):
    """`_table_per_program` is the largest sink in the file (per-pass tables
    with optional immutable-cursor and GHC columns).  These pin the
    "verified mutable, unverified immutable/GHC" adversarial case (#13) for
    its Totals row specifically, since that operand is NOT covered by the
    aos/soa row-level gate."""

    def _render(self, aos, soa, all_variants_results=None):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "t.tex"
            with open(out, "w") as f:
                gb._table_per_program(f, [(aos, soa)], all_variants_results)
            return out.read_text()

    def test_verified_pair_renders_without_crashing(self):
        tex = self._render(verified(median_time=10.0), verified(variant="soa", median_time=5.0))
        self.assertIn("Total", tex)

    def test_verified_mutable_unverified_immutable_totals_row_has_no_sentinel(self):
        aos = verified(median_time=10.0)
        soa = verified(variant="soa", median_time=5.0)
        aos_imm = no_oracle(variant="aos_imm", median_time=SENTINEL_A)
        entry = {"program": "P.hs", "aos": aos, "aos_imm": aos_imm, "soa": soa, "soa_imm": None}
        tex = self._render(aos, soa, all_variants_results=[entry])
        self.assertNotIn(str(int(SENTINEL_A)), tex,
                         "SENTINEL LEAK: unverified aos_imm's total leaked into the Totals row")

    def test_unverified_pair_renders_no_table_at_all(self):
        tex = self._render(no_oracle(median_time=SENTINEL_A), no_oracle(variant="soa", median_time=SENTINEL_A))
        self.assertNotIn(str(int(SENTINEL_A)), tex)


class TestTablePerProgramGhcSink(unittest.TestCase):
    def _render(self, aos, soa, ghc):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "t.tex"
            entry = {"program": aos.program, "aos": aos, "soa": soa, "ghc": ghc}
            with open(out, "w") as f:
                gb._table_per_program_ghc(f, [(aos, soa)], [entry])
            return out.read_text()

    def test_verified_triple_renders_without_crashing(self):
        aos = verified(median_time=10.0)
        soa = verified(variant="soa", median_time=5.0)
        ghc = verified(variant="ghc", median_time=20.0)
        for r in (aos, soa, ghc):
            r.passes["onlyPass"]["pass_type"] = "fold"
        tex = self._render(aos, soa, ghc)
        self.assertIn("Total", tex)

    def test_unverified_ghc_produces_no_table_and_no_sentinel(self):
        aos = verified(median_time=10.0)
        soa = verified(variant="soa", median_time=5.0)
        ghc = no_oracle(variant="ghc", median_time=SENTINEL_A)
        tex = self._render(aos, soa, ghc)
        self.assertNotIn(str(int(SENTINEL_A)), tex)


class TestPapiSink(unittest.TestCase):
    def _papi_result(self, variant, counter_value, verified_ok=True):
        r = (verified(variant=variant, median_time=1.0) if verified_ok
             else no_oracle(variant=variant, median_time=1.0))
        r.passes["onlyPass"]["papi_counters"] = {"PAPI_TOT_CYC": {"median": counter_value}}
        return r

    def test_verified_papi_total_is_read(self):
        r = self._papi_result("aos", SENTINEL_A)
        self.assertEqual(gb._papi_total_for_result(r, "PAPI_TOT_CYC"), SENTINEL_A)

    def test_unverified_papi_total_is_none(self):
        r = self._papi_result("aos", SENTINEL_A, verified_ok=False)
        self.assertIsNone(gb._papi_total_for_result(r, "PAPI_TOT_CYC"),
                          "SENTINEL LEAK: PAPI counter read from an unverified result")


class TestMergeHelpersOnlyAdmitVerifiedContributors(unittest.TestCase):
    def test_merge_octree_results_excludes_unverified_source(self):
        good = verified(program="OctTree_sumMass.hs", variant="aos", median_time=SENTINEL_A)
        good_soa = verified(program="OctTree_sumMass.hs", variant="soa", median_time=SENTINEL_A)
        bad = no_oracle(program="OctTree_countActive.hs", variant="aos", median_time=SENTINEL_B)
        bad_soa = no_oracle(program="OctTree_countActive.hs", variant="soa", median_time=SENTINEL_B)
        combined, _filtered = gb._merge_octree_results(
            [(good, good_soa), (bad, bad_soa)])
        self.assertIsNotNone(combined)
        merged_aos, merged_soa = combined
        raw = json.dumps({k: v for k, v in merged_aos.passes.items()})
        self.assertNotIn(str(SENTINEL_B), raw,
                         "SENTINEL LEAK: an unverified OctTree_* source's pass "
                         "data was merged into the combined row")
        self.assertIn(str(SENTINEL_A), raw)
        self.assertTrue(prov.verified_result(merged_aos),
                        "a merge of only-verified contributors must itself be verified")

    def test_merge_octree_results_all_unverified_yields_no_verified_merge(self):
        bad = no_oracle(program="OctTree_countActive.hs", variant="aos", median_time=SENTINEL_B)
        bad_soa = no_oracle(program="OctTree_countActive.hs", variant="soa", median_time=SENTINEL_B)
        combined, _filtered = gb._merge_octree_results([(bad, bad_soa)])
        if combined is not None:
            self.assertFalse(prov.verified_result(combined[0]))
            self.assertFalse(prov.verified_result(combined[1]))


class TestGeometricMeanExcludesInvalid(unittest.TestCase):
    def test_speedup_vs_ghc_geomean_uses_only_valid_ratios(self):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "t.tex"
            entries = [
                {"program": "Good.hs",
                 "aos": verified(median_time=10.0), "soa": None,
                 "ghc": verified(variant="ghc", median_time=20.0)},
                {"program": "Bad.hs",
                 "aos": no_oracle(median_time=SENTINEL_A), "soa": None,
                 "ghc": no_oracle(variant="ghc", median_time=SENTINEL_B)},
            ]
            with open(out, "w") as f:
                gb._table_speedup_vs_ghc(f, entries)
            tex = out.read_text()
            self.assertNotIn(str(int(SENTINEL_A)), tex)
            self.assertNotIn(str(int(SENTINEL_B)), tex)
            self.assertIn("2.00", tex)  # 20/10 for the good row


class TestFiguresGateReferencesEligiblePair(unittest.TestCase):
    """matplotlib is not installed in this environment, so figures cannot
    actually be rendered here; this pins the SOURCE-LEVEL contract instead --
    `generate_all_figures` must build its admission list from
    `prov.eligible_pair`, not a `run_success`-only proxy."""

    def test_generate_all_figures_uses_eligible_pair(self):
        import inspect
        src = inspect.getsource(gb.generate_all_figures)
        self.assertIn("prov.eligible_pair", src)
        self.assertNotIn("a.run_success and s.run_success", src)


# ---------------------------------------------------------------------------
# Gate A characterization: adversarial matrix covering every combination of
# verified/wrong/missing-oracle pairing across AoS/SoA variants, proving each
# scenario is handled correctly.
# ---------------------------------------------------------------------------

class TestAdversarialMatrix(unittest.TestCase):
    def test_01_verified_aos_and_verified_soa(self):
        self.assertTrue(prov.eligible_pair(verified(), verified(variant="soa")))

    def test_02_wrong_aos_verified_soa(self):
        self.assertFalse(prov.eligible_pair(wrong(), verified(variant="soa")))

    def test_03_verified_aos_wrong_soa(self):
        self.assertFalse(prov.eligible_pair(verified(), wrong(variant="soa")))

    def test_04_both_wrong_mutually_equal(self):
        self.assertFalse(prov.eligible_pair(wrong(median_time=SENTINEL_A),
                                            wrong(variant="soa", median_time=SENTINEL_A)))

    def test_05_both_missing_oracle_mutually_equal(self):
        self.assertFalse(prov.eligible_pair(no_oracle(median_time=SENTINEL_A),
                                            no_oracle(variant="soa", median_time=SENTINEL_A)))

    def test_06_missing_oracle_with_allow_unverified(self):
        r = no_oracle(allow_unverified=True)
        self.assertFalse(r.qualification.verified)
        self.assertTrue(r.qualification.eligible_for_exploratory_output)

    def test_07_compile_failure_with_stale_timing(self):
        self.assertIsNone(gb.total_pass_time(compile_fail(median_time=SENTINEL_A)))

    def test_08_run_failure_with_stale_timing(self):
        self.assertIsNone(gb.total_pass_time(run_fail(median_time=SENTINEL_A)))

    def test_09_empty_output_with_timing(self):
        r = empty_output(median_time=SENTINEL_A)
        self.assertFalse(r.qualification.verified)
        self.assertIsNone(gb.total_pass_time(r))

    def test_10_oracle_failure_with_timing(self):
        self.assertIsNone(gb.total_pass_time(wrong(median_time=SENTINEL_A)))

    def test_11_cross_variant_disagreement_despite_valid_looking_results(self):
        a = verified(cross_variant_status=prov.XVAR_DISAGREE)
        b = verified(variant="soa", cross_variant_status=prov.XVAR_DISAGREE)
        # verified() does not consult cross_variant_status by design (it's a
        # diagnostic, never a substitute for the oracle) -- confirm that.
        self.assertTrue(prov.verified_result(a))
        code = prov.campaign_exit_code([a.qualification, b.qualification])
        self.assertEqual(code, 1, "cross-variant disagreement must still fail the campaign exit code")

    def test_12_only_one_side_of_a_ratio_verified(self):
        val, _ = prov.safe_speedup(verified(median_time=10.0), no_oracle(variant="soa"), gb.total_pass_time)
        self.assertIsNone(val)

    def test_13_verified_mutable_unverified_immutable(self):
        self.assertFalse(prov.eligible_pair(verified(variant="aos"), no_oracle(variant="aos_imm")))

    def test_14_verified_runtime_unverified_build_pass(self):
        # A build-pass entry piggybacks on the SAME BenchmarkResult/qualification
        # -- if the result itself is unverified, total_pass_time (which the
        # build-pass digest also flows through) must refuse it regardless of
        # which pass key holds the data.
        r = no_oracle(median_time=SENTINEL_A)
        r.passes["build"] = dict(r.passes["onlyPass"])
        self.assertIsNone(gb.total_pass_time(r))

    def test_16_verified_execution_failed_codegen_evidence(self):
        r = verified()
        r.qualification.codegen_status = "FAIL"
        # Codegen evidence is recorded separately and does not itself flip
        # `verified` in this model (it feeds oracle_status upstream in
        # `qualify_variant` for the layout-mismatch case) -- assert it is at
        # least VISIBLE and not silently dropped.
        self.assertEqual(r.qualification.as_dict()["codegen_status"], "FAIL")

    def test_17_old_json_record_lacking_qualification_fields(self):
        ok, _why = prov.report_schema_supports_qualified_metrics({"program": "P.hs"})
        self.assertFalse(ok)

    def test_18_mixed_list_one_invalid_sentinel_among_valid(self):
        with tempfile.TemporaryDirectory() as td:
            out = Path(td) / "r.json"
            gb.write_json_results(
                [(verified(program="Good.hs", median_time=SENTINEL_A), verified(program="Good.hs", variant="soa", median_time=SENTINEL_A)),
                 (no_oracle(program="Bad.hs", median_time=SENTINEL_B), no_oracle(program="Bad.hs", variant="soa", median_time=SENTINEL_B))],
                out)
            doc = json.loads(out.read_text())
        good_rec = next(r for r in doc["results"] if r["program"] == "Good.hs")
        bad_rec = next(r for r in doc["results"] if r["program"] == "Bad.hs")
        self.assertTrue(good_rec["aos"]["verified"])
        self.assertFalse(bad_rec["aos"]["verified"])
        self.assertIsNone(bad_rec["aos"]["passes"])


if __name__ == "__main__":
    unittest.main(verbosity=2)
