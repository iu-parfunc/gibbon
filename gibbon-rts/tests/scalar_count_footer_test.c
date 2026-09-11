/*
 * VW-09 regression: the scalar-count footer encoding, measured directly.
 *
 * This test drives the RTS scalar-count API with NO Gibbon compiler in the
 * loop, and compares every observed count against an independently maintained
 * logical sequence kept in this file.  It is the ground truth that the
 * compiler-side invariant
 *
 *     footer_count(buffer b, chunk k)
 *       == logical elements of b physically stored in chunk k
 *
 * is checked against.
 *
 * Built into a PRIVATE temp directory by run_scalar_count_footer_tests.sh, so
 * it never touches gibbon-rts/build and can run beside a compiler gate.
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

/* A chunk big enough that we control growth explicitly. */
#define CHUNK 1024

static GibChunk fresh_region(void)
{
    return gib_alloc_region_on_heap(CHUNK);
}

static char *footer_of(GibChunk c) { return c.end; }

/* ---------------------------------------------------------------- */
/* 1. one chunk, N bumps -> the count is exactly N                    */
/* ---------------------------------------------------------------- */
static void test_single_chunk_bump_counts(void)
{
    for (int n = 0; n <= 5; n++) {
        gib_scalar_count_footer_begin();
        GibChunk c = fresh_region();
        char *f = footer_of(c);
        for (int i = 0; i < n; i++) gib_scalar_count_footer_bump(f);
        uint64_t got = gib_scalar_count_footer_get(f);
        CHECK(got == (uint64_t) n, "single chunk: %d bumps -> count %llu",
              n, (unsigned long long) got);
        gib_scalar_count_footer_end("test_single_chunk_bump_counts");
    }
    printf("  ok: single-chunk bump count equals the number of elements (0..5)\n");
}

/* ---------------------------------------------------------------- */
/* 2. set overrides, and set/bump interact as documented              */
/* ---------------------------------------------------------------- */
static void test_set_and_get(void)
{
    gib_scalar_count_footer_begin();
    GibChunk c = fresh_region();
    char *f = footer_of(c);
    gib_scalar_count_footer_set(f, 41);
    CHECK(gib_scalar_count_footer_get(f) == 41, "set(41) -> %llu",
          (unsigned long long) gib_scalar_count_footer_get(f));
    gib_scalar_count_footer_bump(f);
    CHECK(gib_scalar_count_footer_get(f) == 42, "set(41)+bump -> %llu",
          (unsigned long long) gib_scalar_count_footer_get(f));
    gib_scalar_count_footer_set(f, 0);
    CHECK(gib_scalar_count_footer_get(f) == 0, "set(0) -> %llu",
          (unsigned long long) gib_scalar_count_footer_get(f));
    gib_scalar_count_footer_end("test_set_and_get");
    printf("  ok: set/bump/get\n");
}

/* ---------------------------------------------------------------- */
/* 3. UNTOUCHED vs a genuine zero  (VW-09's central question)         */
/* ---------------------------------------------------------------- */
static void test_untouched_versus_genuine_zero(void)
{
    gib_scalar_count_footer_begin();
    GibChunk c1 = fresh_region();
    char *f1 = footer_of(c1);
    uint64_t untouched = gib_scalar_count_footer_get(f1);

    GibChunk c2 = fresh_region();
    char *f2 = footer_of(c2);
    gib_scalar_count_footer_set(f2, 0);          /* a REAL empty chunk */
    uint64_t genuine = gib_scalar_count_footer_get(f2);

    CHECK(untouched == 0, "untouched footer reads %llu", (unsigned long long) untouched);
    CHECK(genuine == 0, "genuine zero reads %llu", (unsigned long long) genuine);
    /* This is the defect being pinned: the public getter cannot tell them
     * apart.  A consumer that trusts the value alone treats "nobody ever wrote
     * counts here" as "this chunk is legitimately empty". */
    CHECK(untouched == genuine,
          "documented hazard: untouched and genuine-zero are indistinguishable "
          "through gib_scalar_count_footer_get");
    gib_scalar_count_footer_end("test_untouched_versus_genuine_zero");
    printf("  ok: untouched(%llu) == genuine-zero(%llu) through the public getter\n",
           (unsigned long long) untouched, (unsigned long long) genuine);
}

/* ---------------------------------------------------------------- */
/* 4. growth: the cyclic convention                                   */
/*    non-final footer stores the count for the NEXT chunk;           */
/*    the final footer stores the count for the FIRST chunk.          */
/* ---------------------------------------------------------------- */
static void test_growth_cyclic_convention(void)
{
    gib_scalar_count_footer_begin();
    GibChunk c = fresh_region();
    char *writeloc = c.start;
    char *footer = c.end;

    /* chunk 0 holds 3 logical elements */
    for (int i = 0; i < 3; i++) gib_scalar_count_footer_bump(footer);

    char *old_footer = footer;
    gib_grow_region(&writeloc, &footer);
    gib_scalar_count_on_grow(old_footer, footer);

    /* chunk 1 holds 5 */
    for (int i = 0; i < 5; i++) gib_scalar_count_footer_bump(footer);

    char *first = gib_scalar_count_first_footer(footer);
    CHECK(first != NULL, "first_footer resolved");
    if (first) {
        uint64_t at_first = gib_scalar_count_footer_get(first);
        char *nxt = gib_scalar_count_footer_next(first);
        CHECK(nxt != NULL, "next_footer from first resolved");
        uint64_t at_final = nxt ? gib_scalar_count_footer_get(nxt) : (uint64_t) -1;
        /* Per the documented cycle: footer[first] carries chunk 1's count (5),
         * and footer[final] carries chunk 0's count (3). */
        printf("     first_footer count = %llu (expect 5 = chunk 1)\n",
               (unsigned long long) at_first);
        printf("     final_footer count = %llu (expect 3 = chunk 0)\n",
               (unsigned long long) at_final);
        CHECK(at_first == 5, "cyclic: first footer should carry chunk 1's count, got %llu",
              (unsigned long long) at_first);
        CHECK(at_final == 3, "cyclic: final footer should carry chunk 0's count, got %llu",
              (unsigned long long) at_final);
    }
    gib_scalar_count_footer_end("test_growth_cyclic_convention");
    printf("  ok: cyclic footer convention after one growth\n");
}

/* ---------------------------------------------------------------- */
/* 5. repeated growth against an independent logical sequence         */
/* ---------------------------------------------------------------- */
static void test_repeated_growth_sequence(void)
{
    const uint64_t logical[] = {2, 7, 1, 4};      /* elements per chunk */
    const int nchunks = (int) (sizeof(logical) / sizeof(logical[0]));

    gib_scalar_count_footer_begin();
    GibChunk c = fresh_region();
    char *writeloc = c.start;
    char *footer = c.end;

    for (int k = 0; k < nchunks; k++) {
        for (uint64_t i = 0; i < logical[k]; i++) gib_scalar_count_footer_bump(footer);
        if (k + 1 < nchunks) {
            char *old_footer = footer;
            gib_grow_region(&writeloc, &footer);
            gib_scalar_count_on_grow(old_footer, footer);
        }
    }

    /* Walk the chain and collect what the RTS stored. */
    char *f = gib_scalar_count_first_footer(footer);
    uint64_t stored[8]; int n = 0;
    while (f != NULL && n < 8) {
        stored[n++] = gib_scalar_count_footer_get(f);
        char *nx = gib_scalar_count_footer_next(f);
        if (nx == f) break;
        f = nx;
    }
    printf("     logical per-chunk : ");
    for (int i = 0; i < nchunks; i++) printf("%llu ", (unsigned long long) logical[i]);
    printf("\n     stored  per-footer: ");
    for (int i = 0; i < n; i++) printf("%llu ", (unsigned long long) stored[i]);
    printf("\n");
    CHECK(n == nchunks, "walked %d footers, expected %d", n, nchunks);
    if (n == nchunks) {
        /* Cyclic: stored[i] == logical[(i+1) % nchunks] */
        for (int i = 0; i < nchunks; i++) {
            uint64_t want = logical[(i + 1) % nchunks];
            CHECK(stored[i] == want,
                  "footer %d stored %llu, cyclic convention expects %llu",
                  i, (unsigned long long) stored[i], (unsigned long long) want);
        }
    }
    gib_scalar_count_footer_end("test_repeated_growth_sequence");
    printf("  ok: repeated growth vs independent logical sequence\n");
}

/* ---------------------------------------------------------------- */
/* 6. copy_chain: destination must receive the source's sequence      */
/* ---------------------------------------------------------------- */
static void test_copy_chain(void)
{
    gib_scalar_count_footer_begin();
    GibChunk s = fresh_region();
    char *swrite = s.start, *sfoot = s.end;
    for (int i = 0; i < 4; i++) gib_scalar_count_footer_bump(sfoot);
    char *sold = sfoot;
    gib_grow_region(&swrite, &sfoot);
    gib_scalar_count_on_grow(sold, sfoot);
    for (int i = 0; i < 6; i++) gib_scalar_count_footer_bump(sfoot);

    GibChunk d = fresh_region();
    char *dwrite = d.start, *dfoot = d.end;
    char *dold = dfoot;
    gib_grow_region(&dwrite, &dfoot);
    gib_scalar_count_on_grow(dold, dfoot);

    gib_scalar_count_copy_chain(dfoot, sfoot);

    char *sf = gib_scalar_count_first_footer(sfoot);
    char *df = gib_scalar_count_first_footer(dfoot);
    int n = 0;
    while (sf && df && n < 8) {
        uint64_t sv = gib_scalar_count_footer_get(sf);
        uint64_t dv = gib_scalar_count_footer_get(df);
        CHECK(sv == dv, "copy_chain footer %d: src %llu != dst %llu",
              n, (unsigned long long) sv, (unsigned long long) dv);
        sf = gib_scalar_count_footer_next(sf);
        df = gib_scalar_count_footer_next(df);
        n++;
    }
    CHECK(n > 0, "copy_chain walked no footers");
    gib_scalar_count_footer_end("test_copy_chain");
    printf("  ok: copy_chain reproduces the source sequence over %d footers\n", n);
}

/* ---------------------------------------------------------------- */
/* 7. copy_chain with MISMATCHED chunk counts                         */
/*    A shorter destination cannot represent a longer source's        */
/*    partition; this records what actually happens.                  */
/* ---------------------------------------------------------------- */
static void test_copy_chain_mismatched_chunk_counts(void)
{
    gib_scalar_count_footer_begin();
    GibChunk s = fresh_region();
    char *swrite = s.start, *sfoot = s.end;
    for (int k = 0; k < 3; k++) {
        for (int i = 0; i < 3; i++) gib_scalar_count_footer_bump(sfoot);
        char *old = sfoot;
        gib_grow_region(&swrite, &sfoot);
        gib_scalar_count_on_grow(old, sfoot);
    }
    GibChunk d = fresh_region();          /* single chunk destination */
    char *dfoot = d.end;
    gib_scalar_count_copy_chain(dfoot, sfoot);

    int sn = 0; for (char *f = gib_scalar_count_first_footer(sfoot); f && sn < 16; f = gib_scalar_count_footer_next(f)) sn++;
    int dn = 0; for (char *f = gib_scalar_count_first_footer(dfoot); f && dn < 16; f = gib_scalar_count_footer_next(f)) dn++;
    printf("     src footers = %d, dst footers = %d  (partitions differ)\n", sn, dn);
    CHECK(sn != dn, "this case is only meaningful when the partitions differ");
    printf("  ok: mismatched-partition copy characterized (src %d, dst %d)\n", sn, dn);
    gib_scalar_count_footer_end("test_copy_chain_mismatched");
}

int main(void)
{
    gib_init(0, NULL);
    printf("VW-09 scalar-count footer encoding\n");
    test_single_chunk_bump_counts();
    test_set_and_get();
    test_untouched_versus_genuine_zero();
    test_growth_cyclic_convention();
    test_repeated_growth_sequence();
    test_copy_chain();
    test_copy_chain_mismatched_chunk_counts();
    printf("scalar-count footer tests: %d checks, %d failures\n", checks, failures);
    return failures == 0 ? 0 : 1;
}
