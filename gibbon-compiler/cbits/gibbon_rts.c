// The imports here must be kept in sync with
// 'hashIncludes' in Gibbon.Passes.Codegen.

#include "gibbon.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <alloca.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdarg.h>
#include <errno.h>
#include <uthash.h>

#ifdef _WIN64
#include <windows.h>
#endif

#ifdef _GIBBON_POINTER
#include <gc.h>
#endif

#ifdef _GIBBON_PARALLEL
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#endif


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Globals and their accessors
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


// Chunk sizes of buffers, see GitHub #79 and #110.
static uint64_t gib_global_biginf_init_chunk_size = 4 * GB;
static uint64_t gib_global_inf_init_chunk_size = 1 * KB;
static const uint64_t gib_global_max_chunk_size = (1 * GB);

// Runtime arguments, values updated by the flags parser.
static GibInt gib_global_size_param = 1;
static GibInt gib_global_iters_param = 1;
static char *gib_global_bench_prog_param = (char *) NULL;
static char *gib_global_benchfile_param = (char *) NULL;
static char *gib_global_arrayfile_param = (char *) NULL;
static int64_t gib_global_arrayfile_length_param = -1;

// Number of regions allocated.
static int64_t gib_global_region_count = 0;

// Invariant: should always be equal to max(sym_table_keys).
static GibSym gib_global_gensym_counter = 0;



inline uint64_t gib_get_biginf_init_chunk_size(void)
{
    return gib_global_biginf_init_chunk_size;
}

inline uint64_t gib_get_inf_init_chunk_size(void)
{
    return gib_global_inf_init_chunk_size;
}

inline GibInt gib_get_size_param(void)
{
    return gib_global_size_param;
}

inline GibInt gib_get_iters_param(void)
{
    return gib_global_iters_param;
}

char *gib_read_bench_prog_param(void)
{
    if (gib_global_bench_prog_param == NULL) {
        fprintf(stderr, "gib_read_bench_prog_param: benchmark program was not set! Set using --bench-prog.\n");
        exit(1);
    } else {
        return gib_global_bench_prog_param;
    }
}

char *gib_read_benchfile_param(void)
{
    if (gib_global_benchfile_param == NULL) {
        fprintf(stderr, "gib_read_benchfile_param: benchmark input file was not set! Set using --bench-input.\n");
        exit(1);
    } else {
        return gib_global_benchfile_param;
    }
}

char *gib_read_arrayfile_param(void)
{
    if (gib_global_arrayfile_param == NULL) {
        fprintf(stderr, "gib_read_arrayfile_param: array input file was not set! Set using --array-input.\n");
        exit(1);
    } else {
        return gib_global_arrayfile_param;
    }
}

int64_t gib_read_arrayfile_length_param(void)
{
    if (gib_global_arrayfile_length_param == -1) {
        fprintf(stderr, "gib_read_arrayfile_length_param: array input file length was not set! Set using --array-input-length.\n");
        exit(1);
    } else {
        return gib_global_arrayfile_length_param;
    }
}

inline int64_t gib_read_region_count(void)
{
    return gib_global_region_count;
}

inline GibSym gib_read_gensym_counter(void)
{
    return gib_global_gensym_counter;
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Allocators
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


/*
 * If parallelism is enabled, we always use a nursery/malloc based allocator
 * since Boehm GC is not thread-safe in its default configuration. It can be
 * made thread-safe by building it with appropriate flags, but we don't do that.
 * Presently, all parallel pointer-based programs will leak memory.
 */

#ifdef _GIBBON_POINTER
#ifndef _GIBBON_PARALLEL
inline void *gib_alloc(size_t n) { return GC_MALLOC(n); }
#else
inline void *gib_alloc(size_t n) { return malloc(n); }
#endif // ifndef _GIBBON_PARALLEL
#else
inline void *gib_alloc(size_t n) { return malloc(n); }
#endif // ifdef _GIBBON_POINTER

inline void *gib_counted_alloc(size_t n) {
    gib_bump_global_region_count();
    return gib_alloc(n);
}

// Could try alloca() here.  Better yet, we could keep our own,
// separate stack and insert our own code to restore the pointer
// before any function that (may have) called gib_scoped_alloc returns.
inline void *gib_scoped_alloc(size_t n) { return alloca(n); }

// Stack allocation is either too small or blows our stack.
// We need a way to make a giant stack if we want to use alloca.

// Our global pointer.  No parallelism.
// static char* stack_scoped_region;
// char* alloc_scoped() { return stack_scoped_region; }



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Arenas
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

inline GibArena *gib_alloc_arena(void)
{
    GibArena *ar = gib_alloc(sizeof(GibArena));
    ar->ind = 0;
    ar->mem = (char *) gib_alloc(gib_global_max_chunk_size);
    ar->reflist = 0;
    return ar;
}

inline void gib_free_arena(GibArena *ar)
{
    free(ar->mem);
    // TODO(vollmerm): free everything in ar->reflist
    free(ar);
    return;
}

inline GibCursor gib_extend_arena(GibArena *ar, int size)
{
    GibCursor ret = ar->mem + ar->ind;
    ar->ind += size;
    return ret;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Arena-based dictionaries
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


inline GibSymDict *gib_dict_alloc(GibArena *ar)
{
    return (GibSymDict *) gib_extend_arena(ar, sizeof(GibSymDict)); // gib_alloc(sizeof(GibSymDict));
}

inline GibSymDict *gib_dict_insert_ptr(GibArena *ar, GibSymDict *ptr, GibSym key, GibPtr val)
{
    GibSymDict *ret = gib_dict_alloc(ar);
    ret->key = key;
    ret->ptrval = val;
    ret->next = ptr;
    return ret;
}

inline GibPtr gib_dict_lookup_ptr(GibSymDict *ptr, GibSym key)
{
    while (ptr != 0) {
        if (ptr->key == key) {
            return ptr->ptrval;
        } else {
            ptr = ptr->next;
        }
    }
    fprintf(stderr, "Error, key %" PRId64 " not found!\n",key);
    exit(1);
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Sets
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


inline GibSymSet *gib_empty_set(void)
{
    return (GibSymSet *) NULL;
}

inline GibSymSet *gib_insert_set(GibSymSet *set, int sym)
{
    GibSymSet *s;
    HASH_FIND_INT(set, &sym, s);  /* sym already in the hash? */
    if (s==NULL) {
        s = (GibSymSet *) gib_alloc(sizeof(GibSymSet));
        s->val = sym;
        HASH_ADD_INT(set,val,s);
    }
    return set;
}

inline GibBool gib_contains_set(GibSymSet *set, int sym)
{
    GibSymSet *s;
    HASH_FIND_INT(set, &sym, s);
    return (s!=NULL);
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Sym Hash
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


inline GibSymHash *gib_empty_hash(void)
{
    return (GibSymHash *) NULL;
}

inline GibSymHash *gib_insert_hash(GibSymHash *hash, int k, int v)
{
    GibSymHash *s;
    // NOTE: not checking for duplicates!
    s = (GibSymHash *) gib_alloc(sizeof(GibSymHash));
    s->val = v;
    s->key = k;
    HASH_ADD_INT(hash,key,s);

    return hash;
}

inline GibSym gib_lookup_hash(GibSymHash *hash, int k)
{
    GibSymHash *s;
    HASH_FIND_INT(hash,&k,s);
    if (s==NULL) {
        return k; // NOTE: return original key if val not found
        // TODO(vollmerm): come up with something better to do here
    } else {
        return s->val;
    }
}

inline GibBool gib_contains_hash(GibSymHash *hash, int sym)
{
    GibSymHash *s;
    HASH_FIND_INT(hash,&sym,s);
    return (s!=NULL);
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Symbol table
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

static GibSym newline_symbol = -1;
static GibSym space_symbol = -1;
static GibSym comma_symbol = -1;
static GibSym leftparen_symbol = -1;
static GibSym rightparen_symbol = -1;


// important! initialize to NULL
static GibSymtable *global_sym_table = (GibSymtable *) NULL;

void gib_add_symbol(GibSym idx, char *value)
{
    GibSymtable *s;
    s = (GibSymtable *) gib_alloc(sizeof(GibSymtable));
    s->idx = idx;
    strcpy(s->value, value);
    HASH_ADD(hh, global_sym_table, idx, sizeof(GibSym), s);
    if (idx > gib_global_gensym_counter) {
        gib_global_gensym_counter = idx;
    }
    return;
}

inline void gib_set_newline(GibSym idx)
{
    newline_symbol = idx;
    gib_add_symbol(idx,"NEWLINE");
    return;
}

inline void gib_set_space(GibSym idx)
{
    space_symbol = idx;
    gib_add_symbol(idx,"SPACE");
    return;
}

inline void gib_set_comma(GibSym idx)
{
    comma_symbol = idx;
    gib_add_symbol(idx,"COMMA");
    return;
}

inline void gib_set_leftparen(GibSym idx)
{
    leftparen_symbol = idx;
    gib_add_symbol(idx,"LEFTPAREN");
    return;
}

inline void gib_set_rightparen(GibSym idx)
{
    rightparen_symbol = idx;
    gib_add_symbol(idx,"RIGHTPAREN");
    return;
}

inline int gib_print_symbol(GibSym idx)
{
    if (idx == comma_symbol) {
        return printf(",");
    } else if (idx == newline_symbol) {
        return printf("\n");
    } else if (idx == space_symbol) {
        return printf(" ");
    } else if (idx == leftparen_symbol) {
        return printf("(");
    } else if (idx == rightparen_symbol) {
        return printf(")");
    } else {
        GibSymtable *s;
        HASH_FIND(hh, global_sym_table, &idx, sizeof(GibSym), s);
        if (s == NULL) {
            return printf("%" PRId64, idx);
        } else {
            return printf("%s", s->value);
        }

    }
}

GibSym gib_gensym(void)
{
    GibSym idx;
#ifdef _GIBBON_PARALLEL
    idx = __atomic_add_fetch(&gib_global_gensym_counter, 1, __ATOMIC_SEQ_CST);
#else
    gib_global_gensym_counter += 1;
    idx = gib_global_gensym_counter;
#endif
    return idx;
}


void gib_free_symtable(void)
{
    GibSymtable *elt, *tmp;
    HASH_ITER(hh, global_sym_table, elt, tmp) {
        HASH_DEL(global_sym_table,elt);
    }
    free(elt);
    free(tmp);
    return;
}



/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Vectors
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

GibVector *gib_vector_alloc(GibInt num, size_t elt_size)
{
    GibVector *vec = (GibVector *) gib_alloc(sizeof(GibVector));
    if (vec == NULL) {
        fprintf(stderr, "alloc_vector: gib_alloc failed: %ld", sizeof(GibVector));
        exit(1);
    }
    void *data = (void *) gib_alloc(num * elt_size);
    if (data == NULL) {
        fprintf(stderr, "alloc_vector: gib_alloc failed: %ld", sizeof(num * elt_size));
        exit(1);
    }
    vec->vec_lower = 0;
    vec->vec_upper = num;
    vec->vec_elt_size = elt_size;
    vec->vec_data = data;
    return vec;
}

inline GibInt gib_vector_length(GibVector *vec)
{
    return (vec->vec_upper - vec->vec_lower);
}

inline GibBool gib_vector_is_empty(GibVector *vec)
{
    return (gib_vector_length(vec) == 0);
}

GibVector *gib_vector_slice(GibInt i, GibInt n, GibVector *vec)
{
    GibInt lower = vec->vec_lower + i;
    GibInt upper = vec->vec_lower + i + n;
    if ((lower > vec->vec_upper)) {
        fprintf(stderr, "gib_vector_slice: lower out of bounds, %" PRId64 " > %" PRId64, lower, vec->vec_upper);
        exit(1);
    }
    if ((upper > vec->vec_upper)) {
        fprintf(stderr, "gib_vector_slice: upper out of bounds, %" PRId64 " > %" PRId64, upper, vec->vec_upper);
        exit(1);
    }
    GibVector *vec2 = (GibVector *) gib_alloc(sizeof(GibVector));
    if (vec == NULL) {
        fprintf(stderr, "gib_vector_slice: gib_alloc failed: %ld", sizeof(GibVector));
        exit(1);
    }
    vec2->vec_lower = lower;
    vec2->vec_upper = upper;
    vec2->vec_elt_size = vec->vec_elt_size;
    vec2->vec_data = vec->vec_data;
    return vec2;
}

// The callers must cast the return value.
inline void *gib_vector_nth(GibVector *vec, GibInt i)
{
#ifdef _GIBBON_BOUNDSCHECK
    if (i < vec->lower || i > vec->upper) {
        fprintf(stdderr, "gib_vector_nth index out of bounds: %lld (%lld,%lld) \n", i, vec->vec_lower, vec->vec_upper);
        exit(1);
    }
#endif
    return ((char*)vec->vec_data + (vec->vec_elt_size * (vec->vec_lower + i)));
}

inline GibVector *gib_vector_inplace_update(GibVector *vec, GibInt i, void* elt)
{
    void* dst = gib_vector_nth(vec, i);
    memcpy(dst, elt, vec->vec_elt_size);
    return vec;
}

inline GibVector *gib_vector_copy(GibVector *vec)
{
    GibInt len = gib_vector_length(vec);
    void *start = gib_vector_nth(vec, 0);
    GibVector *vec2 = gib_vector_alloc(len, vec->vec_elt_size);
    memcpy(vec2->vec_data, start, len * vec->vec_elt_size);
    return vec2;
}

inline GibVector *gib_vector_inplace_sort(GibVector *vec, int (*compar)(const void *, const void*))
{
    void *start = gib_vector_nth(vec, 0);
    qsort(start, gib_vector_length(vec), vec->vec_elt_size, compar);
    return vec;
}

inline GibVector *gib_vector_sort(GibVector *vec, int (*compar)(const void *, const void*))
{
    GibVector *vec2 = gib_vector_copy(vec);
    gib_vector_inplace_sort(vec2, compar);
    return vec2;
}

GibVector *gib_vector_concat(GibVector *vec)
{
    // Length of the input vector.
    GibInt len = gib_vector_length(vec);
    // Length of the concatenated vector.
    GibInt result_len = 0;
    // Size of each element in the concatenated vector.
    GibInt result_elt_size = 0;
    GibVector **elt_ref, *elt;
    for (GibInt i = 0; i < len; i++) {
        elt_ref = gib_vector_nth(vec, i);
        elt = *elt_ref;
        result_elt_size = elt->vec_elt_size;
        result_len += gib_vector_length(elt);
    }

    // Concatenated vector.
    GibVector *result = gib_vector_alloc(result_len, result_elt_size);
    GibInt elt_len;
    // A counter that tracks the index of elements in 'result'.
    GibInt k = 0;
    for (GibInt i = 0; i < len; i++) {
        elt_ref = gib_vector_nth(vec, i);
        elt = *elt_ref;
        elt_len = gib_vector_length(elt);

        for (GibInt j = 0; j < elt_len; j++) {
            void* k_elt = gib_vector_nth(elt, j);
            gib_vector_inplace_update(result, k, k_elt);
            k++;
        }
    }

    return result;
}

inline void gib_vector_free(GibVector *vec)
{
    free(vec->vec_data);
    free(vec);
    return;
}

GibVector *gib_vector_merge(GibVector *vec1, GibVector *vec2)
{
    if (vec1->vec_upper != vec2->vec_lower) {
        fprintf(stderr,"gib_vector_merge: non-contiguous slices, (%" PRId64 ",%" PRId64 "), (%" PRId64 ",%" PRId64 ")",
               vec1->vec_lower, vec1->vec_upper, vec2->vec_lower, vec2->vec_upper);
        exit(1);
    }
    GibVector *merged = (GibVector *) gib_alloc(sizeof(GibVector));
    if (merged == NULL) {
        fprintf(stderr, "gib_vector_merge: gib_alloc failed: %ld", sizeof(GibVector));
        exit(1);
    }
    merged->vec_lower = vec1->vec_lower;
    merged->vec_upper = vec2->vec_upper;
    merged->vec_elt_size = vec1->vec_elt_size;
    merged->vec_data = vec1->vec_data;
    return merged;
}

void gib_print_timing_array(GibVector *times) {
    printf("ITER TIMES: [");
    double *d;
    GibInt n = gib_vector_length(times);
    for(GibInt i = 0; i < n; i++) {
        d = gib_vector_nth(times, i);
        if (i == (n-1)) {
            printf("%f",*d);
        }
        else {
            printf("%f, ",*d);
        }
    }
    printf("]\n");
    return;
}

double gib_sum_timing_array(GibVector *times)
{
    double *d;
    double acc = 0;
    for(int i = 0; i < gib_vector_length(times); i++) {
        d = gib_vector_nth(times, i);
        acc += *d;
    }
    return acc;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Linked lists
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


#ifdef _BUMPALLOC
// #define _GIBBON_DEBUG
#warning "Using bump allocator."

static __thread char *gib_global_bumpalloc_heap_ptr = (char *) NULL;
static __thread char *gib_global_bumpalloc_heap_ptr_end = (char *) NULL;
static char *gib_global_saved_heap_ptr_stack[100];
static int gib_global_num_saved_heap_ptr = 0;

// For simplicity just use a single large slab:
inline void gib_init_bumpalloc(void)
{
    gib_global_bumpalloc_heap_ptr = (char*)gib_alloc(gib_global_biginf_init_chunk_size);
    gib_global_bumpalloc_heap_ptr_end = gib_global_bumpalloc_heap_ptr + gib_global_biginf_init_chunk_size;
#ifdef _GIBBON_DEBUG
    printf("Arena size for bump alloc: %lld\n", gib_global_biginf_init_chunk_size);
    printf("gib_list_bumpalloc/gib_init_bumpalloc DONE: heap_ptr = %p\n", gib_global_bumpalloc_heap_ptr);
#endif
}

void *gib_list_bumpalloc(int64_t n)
{
    if (! gib_global_bumpalloc_heap_ptr) {
        gib_init_bumpalloc();
    }
    if (gib_global_bumpalloc_heap_ptr + n < gib_global_bumpalloc_heap_ptr_end) {
        char* old= gib_global_bumpalloc_heap_ptr;
        gib_global_bumpalloc_heap_ptr += n;
        return old;
    } else {
        fprintf(stderr, "Warning: bump allocator ran out of memory.");
        exit(1);
    }
}

// Snapshot the current heap pointer value across all threads.
void gib_list_save_alloc_state(void)
{
#ifdef _GIBBON_DEBUG
    printf("Saving(%p): pos %d", heap_ptr, gib_global_num_saved_heap_ptr);
#endif
    gib_global_saved_heap_ptr_stack[gib_global_num_saved_heap_ptr] = heap_ptr;
    gib_global_num_saved_heap_ptr++;
#ifdef _GIBBON_DEBUG
    printf("\n");
#endif
}

void gib_list_restore_alloc_state(void)
{
    if(gib_global_num_saved_heap_ptr <= 0) {
        fprintf(stderr, "Bad call to gib_list_restore_alloc_state!  Saved stack empty!\ne");
        exit(1);
    }
    gib_global_num_saved_heap_ptr--;
#ifdef _GIBBON_DEBUG
    printf("Restoring(%p): pos %d, discarding %p",
           gib_global_saved_heap_ptr_stack[gib_global_num_saved_heap_ptr],
           gib_global_num_saved_heap_ptr,
           gib_global_bumpalloc_heap_ptr);
#endif
    gib_global_bumpalloc_heap_ptr =
        gib_global_saved_heap_ptr_stack[gib_global_num_saved_heap_ptr];
}

#else
// Regular malloc mode:
void gib_init_bumpalloc(void) {}
void *gib_list_bumpalloc(int64_t n) { return gib_alloc(n); }
void gib_list_save_alloc_state(void) {}
void gib_list_restore_alloc_state(void) {}

#endif // BUMPALLOC


// List API.

inline GibList *gib_list_alloc(size_t data_size)
{
    // GibList *ls = gib_alloc(sizeof(GibList));
    GibList *ls = gib_list_bumpalloc(sizeof(GibList));
    ls->ll_data_size = data_size;
    ls->ll_data = (void *) NULL;
    ls->ll_next = (GibList *) NULL;
    return ls;
}

inline GibBool gib_list_is_empty(GibList *ls)
{
    return ls->ll_next == NULL;
}

inline GibList *gib_list_cons(void *elt, GibList *ls)
{
    // void* data = gib_alloc(ls->data_size);
    void* data = gib_list_bumpalloc(ls->ll_data_size);
    if (data == NULL) {
        fprintf(stderr, "gib_list_cons: gib_alloc failed: %ld", ls->ll_data_size);
        exit(1);
    }
    memcpy(data, elt, ls->ll_data_size);
    // GibList *res = gib_alloc(sizeof(GibList));
    GibList *res = gib_list_bumpalloc(sizeof(GibList));
    res->ll_data_size = ls->ll_data_size;
    res->ll_data = data;
    res->ll_next = (GibList*) ls;
    return res;
}

inline void *gib_list_head(GibList *ls)
{
    return ls->ll_data;
}

inline GibList* gib_list_tail(GibList *ls)
{
    return ls->ll_next;
}

inline void gib_list_free(GibList *ls)
{
    free(ls->ll_data);
    free(ls);
    return;
}

inline GibList *gib_list_copy(GibList *ls)
{
    GibList *ls2 = gib_list_alloc(ls->ll_data_size);
    if (ls->ll_data != NULL) {
        void* data = gib_list_bumpalloc(ls->ll_data_size);
        memcpy(data, ls->ll_data, ls->ll_data_size);
        ls2->ll_data = data;
    }
    ls2->ll_next = ls->ll_next;
    return ls2;
}

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Ppm images
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

// Example: writePpm("gibbon_rgb_1000.ppm", 1000, 1000, pixels);
void gib_write_ppm(char* filename, GibInt width, GibInt height, GibVector *pixels)
{
    FILE *fp;
    fp = fopen(filename, "w+");
    fprintf(fp, "P3\n");
    // fprintf(fp, "%lld %lld\n255\n", width, height);
    fprintf(fp, "%" PRId64 " %" PRId64 "\n255\n", width, height);
    GibInt len = gib_vector_length(pixels);
    gib_write_ppm_loop(fp, 0, len, pixels);
    fclose(fp);
    return;
}

void gib_write_ppm_loop(FILE *fp, GibInt idx, GibInt end, GibVector *pixels)
{
    bool fltIf_5768_6575 = idx == end;

    if (fltIf_5768_6575) {
        return;
    } else {
        GibPixel *tmp_112;
        tmp_112 = (GibPixel *) gib_vector_nth(pixels, idx);
        GibPixel tup = *tmp_112;
        GibInt x = tup.field0;
        GibInt y = tup.field1;
        GibInt z = tup.field2;
        // write to file.
        // fprintf(fp, "%lld %lld %lld\n", x, y, z);
        fprintf(fp, "%" PRId64 " %" PRId64 " %" PRId64 "\n", x, y, z);
        gib_write_ppm_loop(fp, (idx+1), end, pixels);
    }
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Memory Management; regions, chunks, GC etc.
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

/*

  Gibbon has "growing regions" i.e each logical region is backed by a doubly
  linked-list of smaller chunks which grows as required. In addition to actual
  data, each chunk stores some additional metadata (GibRegionFooter) to chain
  the chunks together in a list and for garbage collection. The footer:

  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  data | rf_reg_metadata_ptr | rf_seq_no | rf_size | rf_next | rf_prev
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  The metadata after the serialized data serves various purposes:

  - rf_reg_metadata_ptr: A pointer to a GibRegionMeta struct that contains
  various metadata. Of particular interest to us are the fields:

  = reg_id: A unique identifier for a region.

  = refcount and outset: Whenever an inter-region indirection is created, we
    record that information using these two fields. Suppose we have an
    indirection from region A that points to some chunk in region B. Then A's
    outset will store a pointer to that chunk's footer, and B's refcount will
    be bumped by 1. Note that all there's only 1 refcount cell, and 1 outset
    per logical region, and chunks only store a pointer to them.

  - rf_seq_no: The index of this particular chunk in the list.

  - rf_size: Used during bounds checking to calculate the size of the next
    region in the linked list.

  - rf_next / rf_prev: Point to the next and previous chunk respectively.


  There are two ways in which a region may be freed:

  (1) Whenever it goes out of scope

  The RTS tries to free a region whenever it goes out of scope. But this doesn't
  always succeed as regions sometimes contain values that "escape". One reason
  why this'll happen is if there's an indirection from A->B, and A lives longer
  than B. In such a case, when B goes out of scope it's refcount won't be 0,
  and the RTS won't free it. This brings us to (2).

  (2)

  When the RTS successfully frees a region, it decrements the refcounts of all
  the regions it points to (via the outset). At the same time, if it encounters
  a region in the outset whoose refcount becomes 0 after the decrement, it
  calls gib_free_region on that. This way we can be sure that all regions will
  eventually be garbage collected before the program exits.



  Why is it a doubly linked-list?
  ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  Due to way that bounds-checking works, the pointers in the outset may
  actually point to any arbitrary chunk in the chain. However, we must call
  gib_free_region on the first one to ensure that all of them are GC'd.
  So we need pointers to traverse backward get to the first one.
  'gib_trav_to_first_chunk' accomplishes this.

*/


typedef struct gib_region_meta {
    GibSym reg_id;
    uint16_t reg_refcount;
    uint16_t reg_outset_len;
    GibCursor reg_outset[MAX_OUTSET_LENGTH];
} GibRegionMeta;

typedef struct gib_region_footer {
    GibRegionMeta *rf_reg_metadata_ptr;

    uint16_t rf_seq_no;
    uint64_t rf_size;
    struct gib_region_footer *rf_next;
    struct gib_region_footer *rf_prev;
} GibRegionFooter;

void gib_insert_into_outset(GibCursor ptr, GibRegionMeta *reg);
void gib_remove_from_outset(GibCursor ptr, GibRegionMeta *reg);
GibRegionFooter *gib_trav_to_first_chunk(GibRegionFooter *footer);
uint16_t gib_get_ref_count(GibCursor end_ptr);


GibChunk *gib_alloc_region(uint64_t size)
{
    // Allocate the region metadata.
    GibRegionMeta *reg_meta = gib_alloc(sizeof(GibRegionMeta));
    if (reg_meta == NULL) {
        fprintf(stderr, "gib_alloc_region: allocation failed: %ld",
                sizeof(GibRegionMeta));
        exit(1);
    }

    // Allocate the first chunk.
    int64_t total_size = size + sizeof(GibRegionFooter);
    GibCursor heap_start = gib_alloc(total_size);
    if (heap_start == NULL) {
        fprintf(stderr, "gib_alloc_region: gib_alloc failed: %" PRId64,
                total_size);
        exit(1);
    }
    // Not start+total_size, since we must keep space for the footer.
    GibCursor heap_end = heap_start + size;

    // Initialize metadata fields.
    reg_meta->reg_id = gib_gensym();
    reg_meta->reg_refcount = 1;
    reg_meta->reg_outset_len = 0;

#ifdef _GIBBON_DEBUG
    printf("Allocated a region(%lld): %lld bytes.\n", reg->reg_id, size);
#endif

    // Write the footer.
    GibRegionFooter *footer = (GibRegionFooter *) heap_end;
    footer->rf_reg_metadata_ptr = reg_meta;
    footer->rf_seq_no = 1;
    footer->rf_size = size;
    footer->rf_next = (GibRegionFooter *) NULL;
    footer->rf_prev = (GibRegionFooter *) NULL;

    GibChunk *region = (GibChunk *) gib_alloc(sizeof(GibChunk));
    region->c_start = heap_start;
    region->c_end = heap_end;
    return region;
}

GibChunk gib_alloc_chunk(GibCursor footer_ptr)
{
    // Get size from current footer.
    GibRegionFooter *footer = (GibRegionFooter *) footer_ptr;
    uint64_t newsize = footer->rf_size * 2;
    // See #110.
    if (newsize > gib_global_max_chunk_size) {
        newsize = gib_global_max_chunk_size;
    }
    int64_t total_size = newsize + sizeof(GibRegionFooter);

    // Allocate.
    GibCursor start = (char *) gib_alloc(total_size);
    if (start == NULL) {
        fprintf(stderr, "gib_alloc_chunk: gib_alloc failed: %" PRId64,
                total_size);
        exit(1);
    }
    GibCursor end = start + newsize;

    // Link the next chunk's footer.
    footer->rf_next = (GibRegionFooter *) end;

    // Write the footer.
    GibRegionFooter *new_footer = (GibRegionFooter *) end;
    new_footer->rf_reg_metadata_ptr = footer->rf_reg_metadata_ptr;
    new_footer->rf_seq_no = footer->rf_seq_no + 1;
    new_footer->rf_size = newsize;
    new_footer->rf_next = (GibRegionFooter *) NULL;
    new_footer->rf_prev = footer;

#ifdef _GIBBON_DEBUG
    GibRegionMeta *reg = (GibRegionMeta*) new_footer->rf_reg_metadata_ptr;
    printf("gib_alloc_chunk: allocated %lld bytes for region %lld.\n",
           total_size,
           reg->reg_id);
#endif

    return (GibChunk) {start , end};
}

// B is the pointer, and A is the pointee (i.e B -> A).
// Bump A's refcount and update B's outset.
inline void gib_bump_refcount(GibCursor end_b, GibCursor end_a)
{
    if (end_a == end_b) {
#ifdef _GIBBON_DEBUG
        printf("gib_bump_refcount: indirection within a region found, refcount not bumped. TODO.\n");
#endif
        return;
    }
    // Grab footers.
    GibRegionFooter *footer_a = (GibRegionFooter *) end_a;
    GibRegionFooter *footer_b = (GibRegionFooter *) end_b;

    // Grab metadata.
    GibRegionMeta *reg_a = (GibRegionMeta *) footer_a->rf_reg_metadata_ptr;
    GibRegionMeta *reg_b = (GibRegionMeta *) footer_b->rf_reg_metadata_ptr;

    // Bump A's refcount.
    uint16_t current_refcount, new_refcount;
    current_refcount = reg_a->reg_refcount;
    new_refcount = current_refcount + 1;
    reg_a->reg_refcount = new_refcount;

#ifdef _GIBBON_DEBUG
    printf("gib_bump_refcount: %lld -> %lld\n", reg_b->reg_id, reg_a->reg_id);
    printf("gib_bump_refcount: old-refcount=%d, old-outset-len=%d:\n",
           current_refcount, reg_b->reg_outset_len);
    assert(current_refcount == reg_b->reg_outset_len+1);
#endif

    // Add A to B's outset.
    gib_insert_into_outset(end_a, reg_b);

#ifdef _GIBBON_DEBUG
    // printf("gib_bump_refcount: Added %p to %lld's outset, %p.\n", end_a, reg_b->reg_id, reg_b);
    printf("gib_bump_refcount: new-refcount=%d, new-outset-len=%d\n",
           new_refcount, reg_b->reg_outset_len);
    assert(new_refcount == reg_b->reg_outset_len+1);
#endif

    return;
}

void gib_free_region(GibCursor end_reg) {
    // Grab footer and the metadata.
    GibRegionFooter *footer = (GibRegionFooter *) end_reg;
    GibRegionMeta *reg = (GibRegionMeta *) footer->rf_reg_metadata_ptr;

    //
    GibRegionFooter *first_chunk_footer, *next_chunk_footer;
    GibCursor first_chunk, next_chunk;

    // Decrement current reference count.
    uint16_t current_refcount, new_refcount;
    current_refcount = reg->reg_refcount;
    new_refcount = 0;
    if (current_refcount != 0) {
        new_refcount = current_refcount - 1;
        reg->reg_refcount = new_refcount;
    }

#ifdef _GIBBON_DEBUG
    printf("gib_free_region(%lld): refcounts (1): old-refcount=%d, new-refcount=%d:\n", reg->reg_id, current_refcount, new_refcount);
#endif


    // Free this region recount is 0.
    if (new_refcount == 0) {

#ifdef _GIBBON_DEBUG
        printf("gib_free_region(%lld): outset length: %d\n",
               reg->reg_id, reg->reg_outset_len);
#endif

        // Decrement refcounts, free regions with refcount==0 and also free
        // elements of the outset.
        if (reg->reg_outset_len != 0) {
            uint16_t outset_len = reg->reg_outset_len;
            GibCursor *outset = reg->reg_outset;
            GibRegionFooter *elt_footer;
            GibRegionMeta *elt_reg;
            uint16_t elt_current_refcount, elt_new_refcount;
            GibCursor to_be_removed[MAX_OUTSET_LENGTH];
            uint16_t to_be_removed_idx = 0;
            for (uint16_t i = 0; i < outset_len; i++) {
                elt_footer = (GibRegionFooter *) outset[i];
                elt_reg = (GibRegionMeta *) elt_footer->rf_reg_metadata_ptr;
                elt_current_refcount = elt_reg->reg_refcount;
                elt_new_refcount = elt_current_refcount - 1;
                elt_reg->reg_refcount = elt_new_refcount;
#ifdef _GIBBON_DEBUG
                printf("gib_free_region(%lld): old-refcount=%d, new-refcount=%d:\n",
                       elt_reg->reg_id,
                       elt_current_refcount,
                       elt_reg->reg_refcount);
#endif
                if (elt_new_refcount == 0) {
                    // See [Why is it a doubly linked-list?] above
                    first_chunk_footer = gib_trav_to_first_chunk(elt_footer);
                    if (first_chunk_footer != NULL) {
                        gib_free_region((GibCursor) first_chunk_footer);
                    }
                }
                to_be_removed[to_be_removed_idx] = outset[i];
                to_be_removed_idx++;
            }
            // Remove elements from the outset.
            for (uint16_t i = 0; i < to_be_removed_idx; i++) {
                gib_remove_from_outset(to_be_removed[i], reg);
            }
        }


#ifdef _GIBBON_DEBUG
        // Bookkeeping
        int64_t num_freed_chunks = 0, total_bytesize = 0;
#endif

        // Free the chunks in this region.
        first_chunk = end_reg - footer->rf_size;
        first_chunk_footer = footer;
        next_chunk = (char*) footer->rf_next;

#ifdef _GIBBON_DEBUG
        num_freed_chunks++;
        total_bytesize = total_bytesize + first_chunk_footer->rf_size;
#endif
        free(first_chunk);

        while (next_chunk != NULL) {
            next_chunk_footer = (GibRegionFooter *) next_chunk;
#ifdef _GIBBON_DEBUG
            num_freed_chunks++;
            total_bytesize = total_bytesize + next_chunk_footer->rf_size;
#endif
            free(next_chunk - next_chunk_footer->rf_size);
            next_chunk = (char*) next_chunk_footer->rf_next;
        }

#ifdef _GIBBON_DEBUG
        printf("gib_free_region(%lld): Freed %lld bytes across %lld chunks.\n",
               reg->reg_id, total_bytesize, num_freed_chunks);
#endif

        // Free the metadata.
        free(reg);

    } else {
#ifdef _GIBBON_DEBUG
        printf("gib_free_region(%lld): non-zero refcount: %d.\n",
               reg->reg_id, reg->reg_refcount);
#endif
    }
}

void gib_insert_into_outset(GibCursor ptr, GibRegionMeta *reg)
{
    uint16_t outset_len = reg->reg_outset_len;
    // Check for duplicates.
    for (uint16_t i = 0; i < outset_len; i++) {
        if (ptr == reg->reg_outset[i]) {
            return;
        }
    }
    // Otherwise, insert into the outset.
    reg->reg_outset[outset_len] = ptr;
    reg->reg_outset_len = outset_len + 1;
    return;
}

void gib_remove_from_outset(GibCursor ptr, GibRegionMeta *reg)
{
    uint16_t outset_len = reg->reg_outset_len;
    GibCursor *outset = reg->reg_outset;
    uint16_t i;
    if (outset_len == 0) {
        fprintf(stderr, "gib_remove_from_outset: empty outset\n");
        exit(1);
    }
    // Position of 'ptr' in the outset.
    int elt_idx = -1;
    for (i = 0; i < outset_len; i++) {
        if (ptr == outset[i]) {
            elt_idx = i;
        }
    }
    if (elt_idx == -1) {
        fprintf(stderr, "gib_remove_from_outset: element not found\n");
        exit(1);
    }
    // Move all elements ahead of 'elt_idx' back by one position.
    for (i = elt_idx; i < outset_len; i++) {
        outset[i] = outset[i+1];
    }
    return;
}

GibRegionFooter *gib_trav_to_first_chunk(GibRegionFooter *footer)
{
    if (footer->rf_seq_no == 1) {
        return footer;
    } else if (footer->rf_prev == NULL) {
        fprintf(stderr, "No previous chunk found at rf_seq_no: %" PRIu16,
                footer->rf_seq_no);
        return (GibRegionFooter *) NULL;
    } else {
        gib_trav_to_first_chunk((GibRegionFooter *) footer->rf_prev);
    }
    return (GibRegionFooter *) NULL;
}

inline uint16_t gib_get_ref_count(GibCursor end_ptr)
{
    GibRegionFooter *footer = (GibRegionFooter *) end_ptr;
    GibRegionMeta *reg = (GibRegionMeta *) footer->rf_reg_metadata_ptr;
    return reg->reg_refcount;
}


// Functions related to counting the number of allocated regions.

inline GibChunk *gib_alloc_counted_region(int64_t size)
{
    // Bump the count.
    gib_bump_global_region_count();
    return gib_alloc_region(size);
}

void gib_bump_global_region_count(void)
{
#ifdef _GIBBON_PARALLEL
    __atomic_add_fetch(&gib_global_region_count, 1, __ATOMIC_SEQ_CST);
    return;
#else
    gib_global_region_count++;
#endif
    return;
}

void gib_print_global_region_count(void)
{
    printf("REGION_COUNT: %" PRId64 "\n", gib_global_region_count);
    return;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Generational GC
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

// 4 megabytes for each semi-space.
// #define NURSERY_SIZE (4 * MB)

// 4KB for debugging.
#define NURSERY_SIZE (4 * KB)

// If a region is over this size, alloc to refcounted heap directly.
#define NURSERY_REGION_MAX_SIZE (NURSERY_SIZE / 2)

#define NUM_GENERATIONS 1

// TODO(ckopaprkar): The shadow stack doesn't grow and we don't check for
// overflows at the moment. But this stack probably wouldn't overflow since
// each stack frame is only 16 bytes.
#define SHADOWSTACK_SIZE (sizeof(GibShadowstackFrame) * 4 * 1024 * 1024)


// Array of nurseries, indexed by thread_id.
GibNursery *gib_global_nurseries = (GibNursery *) NULL;

// Array of all generations.
GibGeneration *gib_global_generations = (GibGeneration *) NULL;
// For convenience.
GibGeneration *gib_global_gen0 = (GibGeneration *) NULL;
GibGeneration *gib_global_oldest_gen = (GibGeneration *) NULL;

// Shadow stacks for readable and writeable locations respectively,
// indexed by thread_id.
//
// TODO(ckoparkar): not clear how shadow stacks would be when we have
// parallel mutators.. These arrays are abstract enough for now.
GibShadowstack *gib_global_read_shadowstacks = (GibShadowstack *) NULL;
GibShadowstack *gib_global_write_shadowstacks = (GibShadowstack *) NULL;

// Initialize nurseries, shadow stacks and generations.
void gib_storage_initialize(void);
void gib_nursery_initialize(GibNursery *nursery);
void gib_generation_initialize(GibGeneration *gen, uint8_t gen_no);
void gib_shadowstack_initialize(GibShadowstack *stack);

// Initialize nurseries, shadow stacks and generations.
void gib_storage_initialize(void)
{
    if (gib_global_nurseries != NULL) {
        return;
    }

    // Initialize nurseries.
    uint64_t n;
    gib_global_nurseries = (GibNursery *) gib_alloc(gib_global_num_threads *
                                                    sizeof(GibNursery));
    for (n = 0; n < gib_global_num_threads; n++) {
        gib_nursery_initialize(&(gib_global_nurseries[n]));
     }

    // Initialize generations.
    int g;
    gib_global_generations = (GibGeneration *) gib_alloc(NUM_GENERATIONS *
                                                         sizeof(GibGeneration));
    for (g = 0; g < NUM_GENERATIONS; g++) {
        gib_generation_initialize(&(gib_global_generations[g]), g);
    }
    gib_global_gen0 = &(gib_global_generations[0]);
    gib_global_oldest_gen = &(gib_global_generations[NUM_GENERATIONS-1]);
    // Set up destination pointers in each generation.
    for (g = 0; g < NUM_GENERATIONS-1; g++) {
        gib_global_generations[g].g_dest = &(gib_global_generations[g+1]);
    }
    gib_global_oldest_gen->g_dest = gib_global_oldest_gen;

    // Initialize shadow stacks.
    uint64_t ss;
    gib_global_read_shadowstacks =
            (GibShadowstack *) gib_alloc(gib_global_num_threads *
                                         sizeof(GibShadowstack));
    gib_global_write_shadowstacks =
            (GibShadowstack *) gib_alloc(gib_global_num_threads *
                                         sizeof(GibShadowstack));
    for (ss = 0; ss < gib_global_num_threads; ss++) {
        gib_shadowstack_initialize(&(gib_global_read_shadowstacks[ss]));
        gib_shadowstack_initialize(&(gib_global_write_shadowstacks[ss]));
     }

    return;
}

// Initialize a nursery.
void gib_nursery_initialize(GibNursery *nursery)
{
    nursery->n_step = 0;
    nursery->n_heap_size = NURSERY_SIZE;
    nursery->n_heap_start = (char *) gib_alloc(NURSERY_SIZE);
    if (nursery->n_heap_start == NULL) {
        fprintf(stderr, "gib_nursery_initialize: gib_alloc failed: %ld",
                NURSERY_SIZE);
        exit(1);
    }
    nursery->n_heap_end = nursery->n_heap_start + NURSERY_SIZE;
    nursery->n_alloc = nursery->n_heap_start;
    nursery->n_initialized = true;
    nursery->n_rem_set = (void *) NULL;

    return;
}

// Initialize a generation.
void gib_generation_initialize(GibGeneration *gen, uint8_t gen_no)
{
    gen->g_no = gen_no;
    gen->g_dest = (GibGeneration *) NULL;
    gen->g_mem_allocated = 0;
    // Initialize the heap if it's not the oldest generation.
    if (gen == gib_global_oldest_gen) {
        gib_global_oldest_gen->g_refcounted = true;
        gen->g_heap_size = 0;
        gen->g_heap_start = (char *) NULL;
        gen->g_heap_end = (char *) NULL;
        gen->g_alloc = (char *) NULL;
    } else {
        gen->g_refcounted = false;
        gen->g_heap_size = ((gen_no+1) * 256 * MB);
        gen->g_heap_start = (char *) gib_alloc(gen->g_heap_size);
        if (gen->g_heap_start == NULL) {
            fprintf(stderr, "gib_generation_initialize: gib_alloc failed: %ld",
                    gen->g_heap_size);
        }
        gen->g_heap_end = gen->g_heap_start + gen->g_heap_size;
        gen->g_alloc = gen->g_heap_start;
    }
    gen->g_rem_set = (void *) NULL;
    gen->g_zct = (void *) NULL;

    return;
}

// Initialize a shadow stack.
void gib_shadowstack_initialize(GibShadowstack* stack)
{
    stack->ss_start = (char*) gib_alloc(SHADOWSTACK_SIZE);
    if (stack->ss_start == NULL) {
        fprintf(stderr, "gib_shadowstack_initialize: gib_alloc failed: %ld",
                SHADOWSTACK_SIZE);
        exit(1);
    }
    stack->ss_end = stack->ss_start + SHADOWSTACK_SIZE;
    stack->ss_alloc = stack->ss_start;
    stack->ss_initialized = true;
    return;
}

void gib_shadowstack_push(GibShadowstack *stack, char *ptr, uint32_t datatype)
{
    assert(stack->ss_initialized);
    char *stack_alloc_ptr = stack->ss_alloc;
    char *stack_end = stack->ss_end;
    char **stack_alloc_ptr_addr = &(stack->ss_alloc);
    size_t size = sizeof(GibShadowstackFrame);
    if (stack_alloc_ptr + size > stack_end) {
        fprintf(stderr, "gib_shadowstack_push: out of memory");
        exit(1);
    }
    GibShadowstackFrame *frame = (GibShadowstackFrame *) stack_alloc_ptr;
    frame->ssf_ptr = ptr;
    frame->ssf_datatype = datatype;
    (*stack_alloc_ptr_addr) += size;
    return;
}

GibShadowstackFrame *gib_shadowstack_pop(GibShadowstack *stack)
{
    assert(stack->ss_initialized);
    char *stack_alloc_ptr = stack->ss_alloc;
    char *stack_start = stack->ss_start;
    char **stack_alloc_ptr_addr = &(stack->ss_alloc);
    size_t size = sizeof(GibShadowstackFrame);
    if (stack_alloc_ptr - size < stack_start) {
        fprintf(stderr, "gib_shadowstack_pop: stack empty");
        exit(1);
    }
    (*stack_alloc_ptr_addr) -= size;
    GibShadowstackFrame *frame = (GibShadowstackFrame *) (*stack_alloc_ptr_addr);
    return frame;
}

int32_t gib_shadowstack_length(GibShadowstack *stack)
{
    assert(stack->ss_initialized);
    char *stack_alloc_ptr = stack->ss_alloc;
    char *stack_start = stack->ss_start;
    return ( (stack_alloc_ptr - stack_start) / sizeof(GibShadowstackFrame) );
}

void gib_shadowstack_print_all(GibShadowstack *stack)
{
    char *run_ptr = stack->ss_start;
    char *end_ptr = stack->ss_alloc;
    GibShadowstackFrame *frame;
    while (run_ptr < end_ptr) {
        frame = (GibShadowstackFrame *) run_ptr;
        printf("ptr=%p, datatype=%d\n", frame->ssf_ptr, frame->ssf_datatype);
        run_ptr += sizeof(GibShadowstackFrame);
    }
    return;
}


GibChunk *gib_alloc_region_on_heap(uint64_t size);
GibChunk *gib_alloc_region_in_nursery(uint64_t size);

GibChunk *gib_alloc_region2(uint64_t size)
{
    if (size > NURSERY_REGION_MAX_SIZE) {
        return gib_alloc_region_on_heap(size);
    } else {
        return gib_alloc_region_in_nursery(size);
    }
}

GibChunk *gib_alloc_region_on_heap(uint64_t size)
{
    char *heap_start = gib_alloc(size);
    if (heap_start == NULL) {
        fprintf(stderr, "gib_alloc_region_on_heap: gib_alloc failed: %" PRId64, size);
        exit(1);
    }
    char *heap_end = heap_start + size;
    GibChunk *region = gib_alloc(sizeof(GibChunk));
    region->c_start = heap_start;
    region->c_end = heap_end;
    return region;
}

GibChunk *gib_alloc_region_in_nursery(uint64_t size)
{
    GibNursery *nursery = &(gib_global_nurseries[0]);
    GibShadowstack *rstack = &(gib_global_read_shadowstacks[0]);
    GibShadowstack *wstack = &(gib_global_write_shadowstacks[0]);
    GibGeneration *generations = gib_global_generations;
    assert(nursery->n_initialized);
    bool garbage_collected = false;

try_alloc: ; // empty statement to appease C
    char *old = nursery->n_alloc;
    char *bump = old + size;
    if (bump < nursery->n_heap_end) {
        nursery->n_alloc = bump;
        GibChunk *region = gib_alloc(sizeof(GibChunk));
        region->c_start = old;
        region->c_end = bump;
        return region;
    }
    if (garbage_collected) {
        fprintf(stderr, "Couldn't free space after garbage collection.\n");
        exit(1);
    }
    int error = gib_garbage_collect(rstack, wstack, nursery, generations, false);
    if (error < 0) {
        fprintf(stderr, "Couldn't garbage collect minor gen, errorno=%d.",
                error);
        exit(1);
    }
    garbage_collected = true;
    goto try_alloc;
}

void gib_free_region2(GibChunk *region)
{
    printf("gib_free_region2: TODO.\n");
    return;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Helpers
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */


void gib_show_usage(char** argv)
{
    printf("\n");
    printf("This binary was generated by the Gibbon compiler.\n");
    printf("\n");
    printf("Usage: %s [OPTS] [size] [iters]\n", argv[0]);

    printf("\n");
    printf("Options:\n");
    printf(" --buffer-size <bytes>      Set the buffer size (default %" PRId64 ").\n", gib_global_biginf_init_chunk_size);
    printf(" --bench-input <path>       Set the input file read for benchmarking. Applies only\n");
    printf("                            IF the program was *compiled* with --bench-fun. \n");
    return;
}

double gib_avg(const double* arr, int n)
{
    double sum = 0.0;
    for(int i=0; i<n; i++) sum += arr[i];
    return sum / (double)n;
}

double gib_difftimespecs(struct timespec* t0, struct timespec* t1)
{
    return (double)(t1->tv_sec - t0->tv_sec)
            + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
}

int gib_compare_doubles(const void *a, const void *b)
{
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}

// Exponentiation
GibInt gib_expll(GibInt base, GibInt pow)
{
    if (base == 2) {
        return (1 << pow);
    } else {
        GibInt i, result = 1;
        for (i = 0; i < pow; i++)
            result *= base;
        return result;
    }
}

// https://www.cprogramming.com/snippets/source-code/find-the-number-of-cpu-cores-for-windows-mac-or-linux
inline GibInt gib_get_num_processors(void)
{
#ifdef _WIN64
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
#else
    return sysconf(_SC_NPROCESSORS_ONLN);
#endif
}

// Requires -std=gnu11
int dbgprintf(const char *format, ...)
{
    int code = 0;
    va_list args;
    va_start(args, format);
#ifdef _GIBBON_DEBUG
    code = vprintf(format, args);
#endif
    va_end(args);
    return code;
}


/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Main functions
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

int main(int argc, char **argv)
{
    // parameters to parse:
    //
    //   num iterations: How many times to repeat a benchmark.
    //   tree size: An integer passes to `build_tree()`.

    struct rlimit lim;
    int code;
    if ( (code = getrlimit(RLIMIT_STACK, &lim)) ) {
        fprintf(stderr, " [gibbon rts] failed to getrlimit, code %d\n", code);
        exit(1);
    }

    // lim.rlim_cur = 1024LU * 1024LU * 1024LU; // 1GB stack.
    lim.rlim_cur = 512LU * 1024LU * 1024LU; // 500MB stack.
    // lim.rlim_max = lim.rlim_cur; // Normal users may only be able to decrease this.

    // WARNING: Haven't yet figured out why this doesn't work on MacOS...
#ifndef __APPLE__
    code = setrlimit(RLIMIT_STACK, &lim);
    while (code) {
        fprintf(stderr, " [gibbon rts] Failed to set stack size to %lu, code %d\n",
                (uint64_t)lim.rlim_cur, code);
        lim.rlim_cur /= 2;
        // lim.rlim_max /= 2;
        if(lim.rlim_cur < 100 * 1024) {
            fprintf(stderr, " [gibbon rts] Failed setrlimit stack size to something reasonable; giving up.\n");
            break; // abort();
        }
        int code = setrlimit(RLIMIT_STACK, &lim);
    }
#endif

    int got_numargs = 0; // How many numeric arguments have we got.

    int i;
    for (i = 1; i < argc; ++i)
    {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            gib_show_usage(argv);
            exit(0);
        }
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1)
        {
            gib_global_biginf_init_chunk_size = atoll(argv[i + 1]);
            i++;
        }
        else if (strcmp(argv[i], "--inf-buffer-size") == 0 && i < argc - 1)
        {
            gib_global_inf_init_chunk_size = atoll(argv[i + 1]);
            i++;
        }
        else if ((strcmp(argv[i], "--bench-input") == 0)) {
            if (i+1 >= argc) {
                fprintf(stderr, "Not enough arguments after --bench-input, expected <file>.\n");
                gib_show_usage(argv);
                exit(1);
            }
            gib_global_benchfile_param = argv[i+1];
            i++;
        }
        else if ((strcmp(argv[i], "--array-input") == 0)) {
            if (i+1 >= argc) {
                fprintf(stderr, "Not enough arguments after --array-input, expected <file>.\n");
                gib_show_usage(argv);
                exit(1);
            }
            gib_global_arrayfile_param = argv[i+1];
            i++;
        }
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
            gib_global_arrayfile_length_param = atoll(argv[i+1]);
            i++;
        }
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
            int len = strlen(argv[i+1]);
            gib_global_bench_prog_param = (char*) gib_alloc((len+1)*sizeof(char));
            strncpy(gib_global_bench_prog_param,argv[i+1],len);
            i++;
        }
        // If present, we expect the two arguments to be <size> <iters>
        else if (got_numargs >= 2) {
            fprintf(stderr, "Extra arguments left over: ");
            for(; i < argc; i++) fprintf(stderr, "%s ", argv[i]);
            gib_show_usage(argv);
            exit(1);
        } else {
            if (got_numargs == 0) {
                gib_global_size_param  = atoll(argv[i]);
                got_numargs ++;
            } else {
                gib_global_iters_param = atoll(argv[i]);
            }
        }
    }

    // Initialize gib_global_bench_prog_param to an empty string in case
    // the runtime argument --bench-prog isn't passed.
    if (gib_global_bench_prog_param == NULL) {
        gib_global_bench_prog_param = (char*) gib_alloc(1*sizeof(char));
        *gib_global_bench_prog_param = '\n';
    }

    // Initialize the nursery and shadow stack.
    gib_storage_initialize();

    // Run the program.
    gib_main_expr();

    return 0;
}