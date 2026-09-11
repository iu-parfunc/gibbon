/*
 * VW-04 regression: the GibVector range/indexing contract.
 *
 * See Note [GibVector range and indexing contract] in rts-c/gibbon_rts.c.
 * This test is self-contained: it is compiled together with gibbon_rts.c and
 * -D_GIBBON_BOUNDSCHECK by run_vector_bounds_tests.sh, so it also proves that
 * the checked configuration still COMPILES -- it did not, for as long as the
 * check existed, because it called `fprintf(stdderr, ..)`.
 *
 * Invalid accesses exit(1) from inside the RTS, so each one is run as its own
 * process by the driver script; this binary takes a case name in argv[1].
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "gibbon_rts.h"

static GibVector *filled(int64_t n)
{
    GibVector *v = gib_vector_alloc(n, sizeof(int64_t));
    for (int64_t k = 0; k < n; k++) {
        int64_t e = 1000 + k;
        gib_vector_inplace_update(v, k, &e);
    }
    return v;
}

#define CHECK(cond, ...)                                                \
    do { if (!(cond)) { printf("FAIL: "); printf(__VA_ARGS__);          \
                        printf("\n"); return 1; } } while (0)

/* Valid indexing at every length of interest, first and last element. */
static int case_valid(void)
{
    static const int64_t lens[] = {1, 8, 16, 17, 24, 32};
    for (unsigned k = 0; k < sizeof(lens)/sizeof(lens[0]); k++) {
        int64_t n = lens[k];
        GibVector *v = filled(n);
        CHECK(gib_vector_length(v) == n, "length(%ld) = %ld", (long)n,
              (long)gib_vector_length(v));
        CHECK(v->lower == 0 && v->upper == n, "alloc(%ld) range [%ld,%ld)",
              (long)n, (long)v->lower, (long)v->upper);
        for (int64_t i = 0; i < n; i++) {
            int64_t got = *(int64_t *) gib_vector_nth(v, i);
            CHECK(got == 1000 + i, "nth(%ld,%ld) = %ld", (long)n, (long)i, (long)got);
            /* the address must land inside the backing allocation */
            char *addr = (char *) gib_vector_nth(v, i);
            CHECK(addr >= (char *) v->data
                  && addr + sizeof(int64_t) <= (char *) v->data + n * sizeof(int64_t),
                  "nth(%ld,%ld) address outside the allocation", (long)n, (long)i);
        }
    }
    printf("ok: valid indexing 0..n-1 at n in {1,8,16,17,24,32}\n");
    return 0;
}

/* `generate`'s write loop writes exactly n elements: canaries either side. */
static int case_generate_writes_exactly_n(void)
{
    int64_t n = 16;
    GibVector *before = filled(4);
    GibVector *v      = gib_vector_alloc(n, sizeof(int64_t));
    GibVector *after  = filled(4);
    for (int64_t k = 0; k < n; k++) { int64_t e = 55; gib_vector_inplace_update(v, k, &e); }
    for (int64_t k = 0; k < n; k++)
        CHECK(*(int64_t *) gib_vector_nth(v, k) == 55, "element %ld not written", (long)k);
    for (int64_t k = 0; k < 4; k++) {
        CHECK(*(int64_t *) gib_vector_nth(before, k) == 1000 + k, "canary before clobbered");
        CHECK(*(int64_t *) gib_vector_nth(after,  k) == 1000 + k, "canary after clobbered");
    }
    printf("ok: %ld writes stayed inside the allocation\n", (long)n);
    return 0;
}

/* A slice's indices are RELATIVE and its range is half-open. */
static int case_slice_relative(void)
{
    GibVector *v = filled(8);
    GibVector *s = gib_vector_slice(3, 3, v);
    CHECK(s->lower == 3 && s->upper == 6, "slice range [%ld,%ld)",
          (long)s->lower, (long)s->upper);
    CHECK(gib_vector_length(s) == 3, "slice length %ld", (long)gib_vector_length(s));
    for (int64_t i = 0; i < 3; i++) {
        int64_t got = *(int64_t *) gib_vector_nth(s, i);
        CHECK(got == 1003 + i, "nth(slice,%ld) = %ld, expected %ld",
              (long)i, (long)got, (long)(1003 + i));
    }
    /* empty vector and empty slice at the end are both legal */
    GibVector *z = gib_vector_alloc(0, sizeof(int64_t));
    CHECK(gib_vector_length(z) == 0, "alloc(0) length %ld", (long)gib_vector_length(z));
    GibVector *e = gib_vector_slice(8, 0, v);
    CHECK(gib_vector_length(e) == 0 && e->lower == 8 && e->upper == 8,
          "empty end slice [%ld,%ld)", (long)e->lower, (long)e->upper);
    printf("ok: slice indices are relative, ranges are half-open\n");
    return 0;
}

/* Each of these must exit(1) from the RTS under -D_GIBBON_BOUNDSCHECK. */
static int case_bad_read(int64_t n, int64_t i)
{
    GibVector *v = filled(n);
    volatile int64_t sink = *(int64_t *) gib_vector_nth(v, i);
    printf("FAIL: nth(%ld,%ld) was not rejected (read %ld)\n",
           (long)n, (long)i, (long)sink);
    return 1;
}

static int case_bad_update(int64_t n, int64_t i)
{
    GibVector *v = filled(n);
    int64_t e = 7;
    gib_vector_inplace_update(v, i, &e);
    printf("FAIL: inplace_update(%ld,%ld) was not rejected\n", (long)n, (long)i);
    return 1;
}

static int case_bad_slice_index(void)
{
    GibVector *v = filled(8);
    GibVector *s = gib_vector_slice(3, 3, v);   /* valid relative range is 0..2 */
    volatile int64_t sink = *(int64_t *) gib_vector_nth(s, 3);
    printf("FAIL: nth(slice,3) was not rejected (read %ld)\n", (long)sink);
    return 1;
}

static int case_negative_slice(int64_t lo, int64_t len)
{
    GibVector *v = filled(8);
    gib_vector_slice(lo, len, v);
    printf("FAIL: slice(%ld,%ld) was not rejected\n", (long)lo, (long)len);
    return 1;
}

int main(int argc, char **argv)
{
    gib_init(0, NULL);
    if (argc < 2) { printf("usage: %s <case>\n", argv[0]); return 2; }
    const char *c = argv[1];
    if (!strcmp(c, "valid"))          return case_valid();
    if (!strcmp(c, "generate"))       return case_generate_writes_exactly_n();
    if (!strcmp(c, "slice"))          return case_slice_relative();
    if (!strcmp(c, "bad-read"))       return case_bad_read(atoll(argv[2]), atoll(argv[3]));
    if (!strcmp(c, "bad-update"))     return case_bad_update(atoll(argv[2]), atoll(argv[3]));
    if (!strcmp(c, "bad-slice-index")) return case_bad_slice_index();
    if (!strcmp(c, "negative-slice")) return case_negative_slice(atoll(argv[2]), atoll(argv[3]));
    printf("unknown case %s\n", c);
    return 2;
}
