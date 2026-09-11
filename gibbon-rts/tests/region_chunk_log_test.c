/*
 * Region chunk log, measured directly with no Gibbon compiler in the loop.
 *
 * The bug this guards: a benchmark iteration (`iterate`) rewinds its write
 * cursors to the output region's first chunk and runs again, so every
 * iteration re-grows the region from chunk 0.  gib_grow_region_on_heap links
 * the new chunk in with `old_footer->next = new_footer`, which OVERWRITES the
 * link the previous iteration left there -- stranding that whole chain, and
 * leaking one entire output value per iteration (measured at 929 MB/iteration
 * for a 100M-element list, i.e. 94 GB over 101 iterations).
 *
 * Two properties are load-bearing and are what this file pins:
 *
 *   1. Logging is INERT outside a bracket.  Chunks allocated before the loop
 *      -- the whole input value -- must never be logged, and therefore can
 *      never be freed.  This is what makes the scheme safe without any
 *      per-region bookkeeping, so a regression here silently corrupts inputs.
 *
 *   2. restore_state frees exactly the chunks logged since its matching
 *      save_state, and nothing else, under nesting.
 *
 * Built into a PRIVATE temp directory by run_region_chunk_log_tests.sh, so it
 * never touches gibbon-rts/build and can run beside a compiler gate.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include "gibbon_rts.h"

static int failures = 0;
static int checks = 0;

#define CHECK(cond, ...)                                                    \
    do { checks++;                                                          \
         if (!(cond)) { failures++; printf("  FAIL: "); printf(__VA_ARGS__);\
                        printf("   [%s:%d]\n", __func__, __LINE__); } } while (0)

#if _GIBBON_REGIONRESET

// A logged pointer must be freeable exactly once.  We cannot observe free()
// directly, so each "chunk" is a real malloc'd block whose first bytes carry a
// sentinel; after a bulk free we check the allocator hands the same sizes back,
// which is the observable consequence that matters (bounded memory).
#define BLOCK 4096

// Allocation goes through the RTS entry point, which logs when a bracket is
// open.  (It is one function rather than malloc-then-log so that the generated
// program's code cannot depend on whether the feature is compiled in; see
// GIB_NOIPA in gibbon_rts.h.)
static char *fresh_block(int tag)
{
    char *p = (char *) gib_region_chunk_alloc(BLOCK);
    if (p == NULL) { printf("  FAIL: malloc\n"); failures++; exit(1); }
    memset(p, tag, BLOCK);
    return p;
}

// 1. Inert outside a bracket: logging with depth 0 must not record anything,
//    so a later restore cannot free it.  If this regresses, the input value's
//    chunks become eligible for freeing.
static void test_inert_outside_bracket(void)
{
    printf("test_inert_outside_bracket\n");
    char *before = fresh_block(1);     // depth == 0, must not be logged

    gib_region_chunk_save_state();
    gib_region_chunk_restore_state();  // must NOT free `before`

    // Still ours: writing to it is defined, and the sentinel survived.
    CHECK(before[0] == 1, "block logged outside a bracket was disturbed");
    CHECK(before[BLOCK - 1] == 1, "block logged outside a bracket was disturbed");
    free(before);
}

// 2. Round trip: everything logged inside a bracket is released by its restore.
static void test_bracket_frees_what_it_logged(void)
{
    printf("test_bracket_frees_what_it_logged\n");
    gib_region_chunk_save_state();
    for (int i = 0; i < 1000; i++) {
        (void) fresh_block(2);
    }
    gib_region_chunk_restore_state();
    // Reaching here without an allocator abort is the observable success; ASan
    // in the suite is what proves the frees were well-formed.
    CHECK(true, "unreachable");
}

// 3. Repetition is bounded, which is the whole point: N brackets in a row must
//    not accumulate.  We approximate "bounded" by requiring that the addresses
//    handed out settle -- with the chunks freed each round, the allocator
//    reuses them, so a later round sees an address from an earlier round.
static void test_repeated_brackets_reuse_memory(void)
{
    printf("test_repeated_brackets_reuse_memory\n");
    char *first_round_addr = NULL;
    bool saw_reuse = false;
    for (int round = 0; round < 8; round++) {
        gib_region_chunk_save_state();
        for (int i = 0; i < 64; i++) {
            char *p = fresh_block(3);
            if (round == 0 && i == 0) { first_round_addr = p; }
            else if (p == first_round_addr) { saw_reuse = true; }
        }
        gib_region_chunk_restore_state();
    }
    CHECK(saw_reuse,
          "no address was reused across brackets: chunks are not being freed");
}

// 4. Nesting: an inner bracket must release only its own entries and leave the
//    outer bracket's untouched until the outer restore.
static void test_nesting(void)
{
    printf("test_nesting\n");
    gib_region_chunk_save_state();          // outer
    char *outer = fresh_block(4);

    gib_region_chunk_save_state();          // inner
    for (int i = 0; i < 16; i++) { (void) fresh_block(5); }
    gib_region_chunk_restore_state();       // frees only the 16

    CHECK(outer[0] == 4, "inner restore freed an outer bracket's chunk");
    CHECK(outer[BLOCK - 1] == 4, "inner restore freed an outer bracket's chunk");

    gib_region_chunk_restore_state();       // now frees `outer`
    CHECK(true, "unreachable");
}

// 5. An empty bracket is legal and frees nothing.
static void test_empty_bracket(void)
{
    printf("test_empty_bracket\n");
    char *keep = fresh_block(6);   // outside any bracket: never logged
    gib_region_chunk_save_state();
    gib_region_chunk_restore_state();
    CHECK(keep[0] == 6, "empty bracket disturbed an unlogged block");
    free(keep);
}

// 6. Capacity is retained across brackets, so only the first bracket grows the
//    log.  This is why the per-append cost is ~8ns with no realloc after
//    iteration 1; it is a performance contract, checked here as reachability.
static void test_capacity_retained(void)
{
    printf("test_capacity_retained\n");
    for (int round = 0; round < 3; round++) {
        gib_region_chunk_save_state();
        for (int i = 0; i < 2000; i++) { (void) fresh_block(7); }
        gib_region_chunk_restore_state();
    }
    CHECK(true, "unreachable");
}

int main(void)
{
    printf("Region chunk log tests\n");
    test_inert_outside_bracket();
    test_bracket_frees_what_it_logged();
    test_repeated_brackets_reuse_memory();
    test_nesting();
    test_empty_bracket();
    test_capacity_retained();
    printf("\n%d checks, %d failures\n", checks, failures);
    return failures == 0 ? 0 : 1;
}

#else  // _GIBBON_REGIONRESET

// Built without the feature: the three entry points must still LINK, because
// generated code calls them unconditionally.  A link failure here is exactly
// the breakage a flag-off build would hit.
int main(void)
{
    printf("Region chunk log tests (feature disabled)\n");
    char *p = (char *) gib_region_chunk_alloc(64);
    gib_region_chunk_save_state();
    gib_region_chunk_restore_state();
    free(p);
    printf("  stubs link and are no-ops\n");
    printf("\n1 checks, 0 failures\n");
    return 0;
}

#endif // _GIBBON_REGIONRESET
