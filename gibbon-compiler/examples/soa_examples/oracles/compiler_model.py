#!/usr/bin/env python3
"""Independent Python oracle for Compiler.hs.

`buildIR_validPhi_go n pendingPhi` builds a SINGLY-recursive (non-branching)
IR chain: at each step it either emits a PHI Instr (draining `pendingPhi`
without touching `n`), a BlockEnd (decrementing `n`, resetting `pendingPhi`
to 2), or a normal Instr (decrementing `n`, `pendingPhi` forced to 0). `n`
starts at sizeParam+5000000 and only ever decreases, staying >=0 -- unlike
DecisionTree.hs's `d`, it never goes negative, so Python's floor `%` agrees
with Gibbon's C-style truncation throughout (no `tmod` needed here).

Every reduction pass in the program (instCountPass, blockCountPass,
castInstCountPass, memoryOpStatsPass, branchStatsPass, latencyModelPass,
throughputModelPass, goHasCycle, verifyPhiPlacement_IO) is a structural fold
that walks the tree from the OUTERMOST constructor (the first-built node,
n=n_max) inward to End (n=0) -- exactly the order `n` decreases in the
build recursion. So this model does ONE forward iterative pass (not
recursion -- n_max is ~6.4M steps including PHI insertions, well past
Python's default recursion limit) mirroring that exact order, instead of
materializing the tree.

`targetRetunePass`/`stripSideEffectsPass` are structure-preserving maps (they
rewrite fields, never add/remove nodes), so `instCountPass ir'` and
`instCountPass ir''` both equal the same `insts` count computed here --
confirmed by the real compiled output, where both trailing count fields are
identical to the first.

`op` is forced to 0 whenever the natural encoding would collide with 6 (the
PHI opcode) in the *normal* Instr branch (`op = if op0==6 then 0 else op0`),
so opcode 6 only ever occurs in the PHI-emission branch, which only fires
immediately after a BlockEnd resets `seenNonPhi` to 0 for exactly the next
two instructions -- so `verifyPhiPlacement_IO`'s misplaced-PHI count is
provably always 0, confirmed both analytically and by direct simulation
here (not just by copying the observed 0 from the compiled program).
"""
import sys

SIZE_PARAM = 0
N0 = SIZE_PARAM + 5000000


def expected():
    n = N0
    pending_phi = 0

    insts = 0
    blocks = 0
    memops = 0
    brs = 0
    cast_instrs = 0
    lat_sum = 0
    thr_sum = 0
    has_cycle = False
    cur_block = 0
    seen_non_phi = 0
    bad_phis = 0

    while True:
        if n <= 0:
            break
        elif pending_phi > 0:
            op, flags, src1, lat, thr = 6, 0, 0, 1, 1
            pending_phi -= 1
            # -- Instr fields --
            insts += 1
            if flags == 1 or flags == 2:
                memops += 1
            if flags == 4:
                brs += 1
            if op == 7:
                cast_instrs += 1
            lat_sum += lat
            thr_sum += thr
            if op == 4 and src1 < cur_block:
                has_cycle = True
            if op == 6:
                if seen_non_phi == 1:
                    bad_phis += 1
            else:
                seen_non_phi = 1
        elif n % 7 == 0:
            blocks += 1
            n -= 1
            pending_phi = 2
            seen_non_phi = 0
            cur_block += 1
        else:
            op0 = n % 8
            op = 0 if op0 == 6 else op0
            flags = (n * 3) % 16
            lat = 1 + n % 5
            thr = 1 + n % 3
            src1 = n - 1
            # -- Instr fields --
            insts += 1
            if flags == 1 or flags == 2:
                memops += 1
            if flags == 4:
                brs += 1
            if op == 7:
                cast_instrs += 1
            lat_sum += lat
            thr_sum += thr
            if op == 4 and src1 < cur_block:
                has_cycle = True
            if op == 6:
                if seen_non_phi == 1:
                    bad_phis += 1
            else:
                seen_non_phi = 1
            n -= 1
            pending_phi = 0

    return (insts, blocks, memops, brs, lat_sum, has_cycle, thr_sum,
            insts, insts, bad_phis, cast_instrs)


def format_expected():
    (insts, blocks, memops, brs, lat_sum, has_cycle, thr_sum,
     insts2, insts3, bad_phis, cast_instrs) = expected()
    hc = "#t" if has_cycle else "#f"
    return "'#(%d %d %d %d %d %s %d %d %d %d %d)" % (
        insts, blocks, memops, brs, lat_sum, hc, thr_sum,
        insts2, insts3, bad_phis, cast_instrs)


if __name__ == "__main__":
    print(format_expected())
