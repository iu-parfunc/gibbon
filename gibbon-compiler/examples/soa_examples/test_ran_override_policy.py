#!/usr/bin/env python3
"""Regression tests for two provenance defects found while auditing the
generated benchmark tables (VW-36, VW-37 investigation):

  * VW-36: `PROGRAM_COMPILE_OVERRIDES` used to carry a silent
    `use_no_ran=False` entry for reduceNestedList.hs, defeating the
    campaign-wide `--no-ran` policy for every variant of that one program
    with no ledger authorization.  `_validate_no_ran_overrides()` now makes
    that class of defect fail loudly instead of silently.

  * The `ad.get("dead_ratio") or sd.get("dead_ratio")` idiom (and the sibling
    `uses` idiom) treats a real `0.0`/`0` value as "absent" because of
    Python's `or`-is-falsy-on-zero semantics, so a program with a genuine 0%
    dead ratio (or 0 uses) on the AoS side would silently display the SoA
    side's value instead.  `_first_present()` replaces it.

No compiler is invoked: these are fast, pure-logic tests.

Run:  python3 test_ran_override_policy.py
"""
import sys
import unittest
from pathlib import Path

HERE = Path(__file__).resolve().parent
sys.path.insert(0, str(HERE))

import gibbon_benchmark as gb


class TestFirstPresentCoalescing(unittest.TestCase):
    """`_first_present` must treat 0 / 0.0 / "" as present, unlike `or`."""

    def test_zero_float_from_first_arg_is_kept(self):
        # A program whose AoS side is genuinely 0% dead must not fall through
        # to the SoA side's value.
        self.assertEqual(gb._first_present(0.0, 0.5), 0.0)

    def test_zero_int_from_first_arg_is_kept(self):
        self.assertEqual(gb._first_present(0, 7), 0)

    def test_none_falls_through_to_next_present_value(self):
        self.assertEqual(gb._first_present(None, 0.33), 0.33)
        self.assertEqual(gb._first_present(None, None, 5), 5)

    def test_all_none_returns_none(self):
        self.assertIsNone(gb._first_present(None, None))

    def test_matches_or_semantics_when_first_is_truthy(self):
        self.assertEqual(gb._first_present(0.82, 0.1), 0.82)

    def test_diverges_from_or_semantics_exactly_at_falsy_zero(self):
        # This is the actual bug: `0.0 or 0.5 == 0.5`, but the correct answer
        # (AoS really did measure 0% dead) is 0.0.
        self.assertNotEqual(0.0 or 0.5, 0.0)
        self.assertEqual(gb._first_present(0.0, 0.5), 0.0)


class TestDeadRatioAndUsesUseCoalescing(unittest.TestCase):
    """The two call sites that render OctTree dead-ratio/uses cells must use
    `_first_present`, not the falsy-zero-prone `or` idiom."""

    def test_no_or_based_dead_ratio_coalescing_remains(self):
        src = (HERE / "gibbon_benchmark.py").read_text()
        self.assertNotIn(
            'ad.get("dead_ratio") or sd.get("dead_ratio")', src,
            "the falsy-zero-unsafe dead_ratio idiom must not reappear")
        self.assertNotIn(
            'ad.get("uses") or sd.get("uses")', src,
            "the falsy-zero-unsafe uses idiom must not reappear")

    def test_first_present_is_used_for_dead_ratio_and_uses(self):
        src = (HERE / "gibbon_benchmark.py").read_text()
        self.assertIn('_first_present(ad.get("dead_ratio")', src)
        self.assertIn('_first_present(ad.get("uses")', src)


class TestNoRanOverridePolicy(unittest.TestCase):
    """VW-36: no PROGRAM_COMPILE_OVERRIDES entry may weaken --no-ran, and the
    validator must actually be wired into the compile path."""

    def test_shipped_overrides_table_has_no_violations(self):
        # Must not raise.
        gb._validate_no_ran_overrides()

    def test_reduce_nested_list_has_no_override_entry(self):
        # The specific program the defect was found on must be clean, not
        # merely "not currently violating" -- it should carry no override at
        # all, since none was ever legitimately needed.
        self.assertNotIn("reduceNestedList.hs", gb.PROGRAM_COMPILE_OVERRIDES)

    def test_synthetic_use_no_ran_false_is_rejected(self):
        bad = {"Fake.hs": {"aos": {"use_no_ran": False}}}
        with self.assertRaises(RuntimeError) as ctx:
            gb._validate_no_ran_overrides(bad)
        self.assertIn("VW-36", str(ctx.exception))
        self.assertIn("Fake.hs[aos]", str(ctx.exception))

    def test_synthetic_use_no_ran_true_is_accepted(self):
        # Explicitly re-affirming --no-ran (a no-op) must not be treated as a
        # violation -- only weakening it is disallowed.
        ok = {"Fake.hs": {"aos": {"use_no_ran": True}}}
        gb._validate_no_ran_overrides(ok)  # must not raise

    def test_synthetic_override_without_use_no_ran_key_is_accepted(self):
        # An override that only touches an unrelated knob (e.g.
        # use_mutable_cursors) must not be flagged.
        ok = {"Fake.hs": {"aos": {"use_mutable_cursors": False}}}
        gb._validate_no_ran_overrides(ok)  # must not raise

    def test_multiple_violations_are_all_reported(self):
        bad = {
            "Fake.hs": {"aos": {"use_no_ran": False},
                        "soa": {"use_no_ran": False}},
        }
        with self.assertRaises(RuntimeError) as ctx:
            gb._validate_no_ran_overrides(bad)
        msg = str(ctx.exception)
        self.assertIn("Fake.hs[aos]", msg)
        self.assertIn("Fake.hs[soa]", msg)

    def test_validator_runs_at_benchmark_program_entry(self):
        # benchmark_program's docstring records the VW-36 defense-in-depth
        # call; assert the source actually calls it as the first statement in
        # the function body, not just in the docstring.
        import inspect
        src = inspect.getsource(gb.benchmark_program)
        # Strip the docstring so a mention of the call inside prose doesn't
        # count; find the first real statement after the closing triple-quote.
        after_doc = src.split('"""', 2)[2]
        first_stmt = after_doc.strip().splitlines()[0].strip()
        self.assertEqual(first_stmt, "_validate_no_ran_overrides()")

    def test_default_variant_use_no_ran_eff_is_true_for_all_curated_programs(self):
        # Reproduce benchmark_program's own use_no_ran_eff computation for
        # every (prog, variant) pair under default (non --use-ran,
        # non --benchmark-ghc) settings, and assert it is always True: no
        # override may flip it, because none currently exist.
        use_ran = False
        benchmark_ghc = False
        for var in ("aos", "aos_imm", "soa", "soa_imm"):
            is_gibbon_variant = var.startswith("aos") or var.startswith("soa")
            use_no_ran = not ((use_ran or benchmark_ghc) and is_gibbon_variant)
            self.assertTrue(use_no_ran)
            for prog in list(gb.PROGRAM_COMPILE_OVERRIDES.keys()) + ["reduceNestedList.hs"]:
                override = gb.PROGRAM_COMPILE_OVERRIDES.get(prog, {}).get(var, {})
                use_no_ran_eff = override.get("use_no_ran", use_no_ran)
                self.assertTrue(
                    use_no_ran_eff,
                    f"{prog}[{var}] must compile with --no-ran by default")


if __name__ == "__main__":
    unittest.main(verbosity=2)
