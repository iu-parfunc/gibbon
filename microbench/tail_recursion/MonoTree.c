#pragma GCC optimize("O3", "inline")
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <alloca.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
// #include <sys/sysinfo.h>
#ifdef _WIN64
#include <windows.h>
#endif
#include <unistd.h>
#include <fcntl.h>
#include <stdarg.h> // For va_start etc
#include <errno.h>
#include <utlist.h>
#include <uthash.h>
#include <utarray.h>
#ifdef _POINTER
#include <gc.h>
#endif
#ifdef _PARALLEL
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#endif

#define KB 1024lu
#define MB (KB * 1000lu)
#define GB (MB * 1000lu)

#define REDIRECTION_TAG 255
#define INDIRECTION_TAG 254

#define stack_size 10000000

// Initial size of BigInfinite buffers
static long long global_init_biginf_buf_size = (4 * GB);

// Initial size of Infinite buffers
static long long global_init_inf_buf_size = 1 * KB;

// Maximum size of a chunk, see GitHub #110.
static long long global_inf_buf_max_chunk_size = 1 * GB;

static long long global_size_param = 1;
static long long global_iters_param = 1;

static char* global_benchfile_param = NULL;
static char* global_arrayfile_param = NULL;
// Number of lines in the arrayfile
static long long global_arrayfile_length_param = -1;

// Sequential for now:
static const int num_workers = 1;

// Count the number of regions allocated.
static long long global_region_count = 0;
static bool global_region_count_flag = false;

#ifdef _PARALLEL
static inline void bump_global_region_count() {
    __atomic_add_fetch(&global_region_count, 1, __ATOMIC_SEQ_CST);
    return;
}
#else
static inline void bump_global_region_count() {
    global_region_count++;
    return;
}
#endif

static inline void print_global_region_count() {
    printf("REGION_COUNT: %lld\n", global_region_count);
    return;
}

#define REDIRECTION_NODE_SIZE 9
#define MAX(a,b) (((a)>(b))?(a):(b))
#define MIN(a,b) (((a)<(b))?(a):(b))

// https://www.cprogramming.com/snippets/source-code/find-the-number-of-cpu-cores-for-windows-mac-or-linux
static int get_num_processors() {
#ifdef _WIN64
    SYSTEM_INFO sysinfo;
    GetSystemInfo(&sysinfo);
    return sysinfo.dwNumberOfProcessors;
#else
    return sysconf(_SC_NPROCESSORS_ONLN);
#endif
}

// Requires -std=gnu11
int dbgprintf(const char *format, ...) {
    int code = 0;
    va_list args;
    va_start(args, format);
#ifdef _DEBUG
    code = vprintf(format, args);
#endif
    va_end(args);
    return code;
}


// -----------------------------------------------------------------------------
// Allocators
// -----------------------------------------------------------------------------


// -------------------------------------
// Bump allocation for linked-lists
// -------------------------------------


#ifdef _BUMPALLOC
// #define _DEBUG
#warning "Using bump allocator."

__thread char* bumpalloc_heap_ptr = (char*)NULL;
__thread char* bumpalloc_heap_ptr_end = (char*)NULL;

char* saved_heap_ptr_stack[100];
int num_saved_heap_ptr = 0;

// For simplicity just use a single large slab:
static inline void INITBUMPALLOC() {
      bumpalloc_heap_ptr = (char*)malloc(global_init_biginf_buf_size);
      bumpalloc_heap_ptr_end = bumpalloc_heap_ptr + global_init_biginf_buf_size;
#ifdef _DEBUG
      printf("Arena size for bump alloc: %lld\n", global_init_biginf_buf_size);
      printf("BUMPALLOC/INITBUMPALLOC DONE: heap_ptr = %p\n", bumpalloc_heap_ptr);
#endif
}

static inline void* BUMPALLOC(long long n) {
      if (! bumpalloc_heap_ptr) {
          INITBUMPALLOC();
      }
      if (bumpalloc_heap_ptr + n < bumpalloc_heap_ptr_end) {
          char* old= bumpalloc_heap_ptr;
          bumpalloc_heap_ptr += n;
          return old;
      } else {
          fprintf(stderr, "Warning: bump allocator ran out of memory.");
          exit(1);
      }
}

// Snapshot the current heap pointer value across all threads.
void save_alloc_state() {
  dbgprintf("Saving(%p): pos %d", heap_ptr, num_saved_heap_ptr);
  saved_heap_ptr_stack[num_saved_heap_ptr] = heap_ptr;
  num_saved_heap_ptr++;
  dbgprintf("\n");
}

void restore_alloc_state() {
  if(num_saved_heap_ptr <= 0) {
    fprintf(stderr, "Bad call to restore_alloc_state!  Saved stack empty!\ne");
    exit(1);
  }
  num_saved_heap_ptr--;
  dbgprintf("Restoring(%p): pos %d, discarding %p",
            saved_heap_ptr_stack[num_saved_heap_ptr], num_saved_heap_ptr, bumpalloc_heap_ptr);
  bumpalloc_heap_ptr = saved_heap_ptr_stack[num_saved_heap_ptr];
}


#else
// Regular malloc mode:
void INITBUMPALLOC() {}
void save_alloc_state() {}
void restore_alloc_state() {}

#define BUMPALLOC(n) malloc(n)

#endif // BUMPALLOC


// -------------------------------------
// ALLOC and ALLOC_PACKED macros
// -------------------------------------


/*

If parallelism is enabled, we always use a malloc based allocator
since Boehm GC is not thread-safe in its default configuration. It can be
made thread-safe by building it with appropriate flags, but we don't do that.
Presently, all parallel pointer-based programs will leak memory.

*/

#ifdef _PARALLEL
#define ALLOC(n) malloc(n)
#define ALLOC_PACKED_BIG(n) malloc(n)
char *ALLOC_COUNTED(size_t size) {
    bump_global_region_count();
    return ALLOC(size);
}
#else
  #ifdef _POINTER
#define ALLOC(n) GC_MALLOC(n)
#define ALLOC_PACKED_BIG(n) GC_MALLOC(n)
char *ALLOC_COUNTED(size_t size) {
    bump_global_region_count();
    return GC_MALLOC(size);
}
  #else
#define ALLOC(n) malloc(n)
#define ALLOC_PACKED_BIG(n) malloc(n)
char *ALLOC_COUNTED(size_t size) {
    bump_global_region_count();
    return ALLOC(size);
}
  #endif // _POINTER
#endif // _PARALLEL


// Could try alloca() here.  Better yet, we could keep our own,
// separate stack and insert our own code to restore the pointer
// before any function that (may have) called ALLOC_SCOPED returns.

// #define ALLOC_SCOPED() alloca(1024)
#define ALLOC_SCOPED(n) alloca(n)
// #define ALLOC_SCOPED() alloc_scoped()

// Stack allocation is either too small or blows our stack.
// We need a way to make a giant stack if we want to use alloca.
// #define ALLOC_SCOPED() ALLOC(global_init_biginf_buf_size)

// Our global pointer.  No parallelism.
// static char* stack_scoped_region;
// char* alloc_scoped() { return stack_scoped_region; }



// -------------------------------------
// Basic types
// -------------------------------------

// Must be consistent with sizeOfTy defined in Gibbon.Language.Syntax.

typedef unsigned char TagTyPacked;
typedef unsigned char TagTyBoxed;
typedef long long IntTy;
typedef char CharTy;
typedef float FloatTy;
typedef unsigned long long SymTy;
typedef bool BoolTy;
typedef char* PtrTy;
typedef char* CursorTy;

// -------------------------------------
// Arenas and dictionaries
// -------------------------------------

typedef struct mem_arena {
  int ind;
  char* mem; // TODO(vollmerm): make this a list of chunks?
  void* reflist;
} mem_arena_t;

typedef mem_arena_t* ArenaTy;

ArenaTy alloc_arena() {
  ArenaTy ar = ALLOC(sizeof(mem_arena_t));
  ar->ind = 0;
  ar->mem = malloc(global_inf_buf_max_chunk_size);
  ar->reflist = 0;
  return ar;
}

void free_arena(ArenaTy ar) {
  free(ar->mem);
  // TODO(vollmerm): free everything in ar->reflist
  free(ar);
}

CursorTy extend_arena(ArenaTy ar, int size) {
  CursorTy ret = ar->mem + ar->ind;
  ar->ind += size;
  return ret;
}

typedef struct dict_item {
  struct dict_item * next;
  int key;
  void * ptrval;
} dict_item_t;

dict_item_t * dict_alloc(ArenaTy ar) {
  return (dict_item_t *) extend_arena(ar, sizeof(dict_item_t)); // ALLOC(sizeof(dict_item_t));
}

dict_item_t *dict_insert_ptr(ArenaTy ar, dict_item_t *ptr, SymTy key, PtrTy val) {
  dict_item_t *ret = dict_alloc(ar);
  ret->key = key;
  ret->ptrval = val;
  ret->next = ptr;
  return ret;
}

PtrTy dict_lookup_ptr(dict_item_t *ptr, SymTy key) {
  while (ptr != 0) {
    if (ptr->key == key) {
      return ptr->ptrval;
    } else {
      ptr = ptr->next;
    }
  }
  printf("Error, key %lld not found!\n",key);
  exit(1);
}

// -------------------------------------
// Sets
// -------------------------------------


struct set_elem {
  int val;
  UT_hash_handle hh;
};

typedef struct set_elem* SymSetTy;

SymSetTy empty_set() {
  return NULL;
}

SymSetTy insert_set(SymSetTy set, int sym) {
  SymSetTy s;
  HASH_FIND_INT(set, &sym, s);  /* sym already in the hash? */
  if (s==NULL) {
    s = malloc(sizeof(struct set_elem));
    s->val = sym;
    HASH_ADD_INT(set,val,s);
  }
  return set;
}

BoolTy contains_set(SymSetTy set, int sym) {
  SymSetTy s;
  HASH_FIND_INT(set, &sym, s);
  return (s!=NULL);
}

// -------------------------------------
// Sym Hash
// -------------------------------------

struct sym_hash_elem {
  int key;
  int val;
  UT_hash_handle hh;
};

typedef struct sym_hash_elem* SymHashTy;

typedef struct sym_hash_elem* IntHashTy;

SymHashTy empty_hash() {
  return NULL;
}

SymHashTy insert_hash(SymHashTy hash, int k, int v) {
  SymHashTy s;
  // NOTE: not checking for duplicates!
  // s = malloc(sizeof(struct sym_hash_elem));
  s = ALLOC(sizeof(struct sym_hash_elem));
  s->val = v;
  s->key = k;
  HASH_ADD_INT(hash,key,s);

  return hash;
}

IntTy lookup_hash(SymHashTy hash, int k) {
  SymHashTy s;
  HASH_FIND_INT(hash,&k,s);
  if (s==NULL) {
    return k; // NOTE: return original key if val not found
              // TODO(vollmerm): come up with something better to do here
  } else {
    return s->val;
  }
}

BoolTy contains_hash(SymHashTy hash, int sym) {
  SymHashTy s;
  HASH_FIND_INT(hash,&sym,s);
  return (s!=NULL);
}

// -------------------------------------
// Helpers
// -------------------------------------

char* read_benchfile_param() {
  if (global_benchfile_param == NULL) {
    fprintf(stderr, "read_benchfile_param: benchmark input file was not set! Set using --bench-input.\n");
    exit(1);
  } else
    return global_benchfile_param;
}

char* read_arrayfile_param() {
  if (global_arrayfile_param == NULL) {
    fprintf(stderr, "read_arrayfile_param: array input file was not set! Set using --array-input.\n");
    exit(1);
  } else
    return global_arrayfile_param;
}

IntTy read_arrayfile_length_param() {
  if (global_arrayfile_length_param == -1) {
    fprintf(stderr, "read_arrayfile_length_param: array input file length was not set! Set using --array-input-length.\n");
    exit(1);
  } else
    return global_arrayfile_length_param;
}


// fun fact: __ prefix is actually reserved and this is an undefined behavior.
// These functions must be provided by the code generator.
int __main_expr();


void show_usage(char** argv)
{
    printf("\n");
    printf("This binary was generated by the Gibbon compiler.\n");
    printf("\n");
    printf("Usage: %s [OPTS] [size] [iters]\n", argv[0]);

    printf("\n");
    printf("Options:\n");
    printf(" --buffer-size <bytes>      Set the buffer size (default %lld).\n", global_init_biginf_buf_size);
    printf(" --bench-input <path>       Set the input file read for benchmarking. Applies only\n");
    printf("                            IF the program was *compiled* with --bench-fun. \n");
    return;
}

double avg(const double* arr, int n)
{
    double sum = 0.0;
    for(int i=0; i<n; i++) sum += arr[i];
    return sum / (double)n;
}

double difftimespecs(struct timespec* t0, struct timespec* t1)
{
    return (double)(t1->tv_sec - t0->tv_sec)
      + ((double)(t1->tv_nsec - t0->tv_nsec) / 1000000000.0);
}

int compare_doubles(const void *a, const void *b)
{
    const double *da = (const double *) a;
    const double *db = (const double *) b;
    return (*da > *db) - (*da < *db);
}

// Exponentiation
IntTy expll(IntTy base, IntTy pow) {
    if (base == 2) {
        return (1 << pow);
    } else {
        IntTy i, result = 1;
        for (i = 0; i < pow; i++)
            result *= base;
        return result;
    }
 }

// -------------------------------------
// Symbol table
// -------------------------------------

#define global_max_symbol_len 256

// Invariant: should always be equal to max(sym_table_keys)
static SymTy global_gensym_counter = 0;

// Its value is updated by the flags parser.
static char *global_bench_prog_param;

static SymTy newline_symbol = -1;
static SymTy space_symbol = -1;
static SymTy comma_symbol = -1;
static SymTy leftparen_symbol = -1;
static SymTy rightparen_symbol = -1;

typedef struct SymTable_elem {
    SymTy idx;                 /* key */
    char value[global_max_symbol_len];
    UT_hash_handle hh;         /* makes this structure hashable */
} SymTable_elem;

// important! initialize to NULL
SymTable_elem *global_sym_table = NULL;

void add_symbol(SymTy idx, char *value) {
    struct SymTable_elem *s;
    s = ALLOC(sizeof(struct SymTable_elem));
    s->idx = idx;
    strcpy(s->value, value);
    HASH_ADD(hh, global_sym_table, idx, sizeof(IntTy), s);
    if (idx > global_gensym_counter) {
        global_gensym_counter = idx;
    }
}

void set_newline(SymTy idx) {
  newline_symbol = idx;
  add_symbol(idx,"NEWLINE");
}

void set_space(SymTy idx) {
  space_symbol = idx;
  add_symbol(idx,"SPACE");
}

void set_comma(SymTy idx) {
  comma_symbol = idx;
  add_symbol(idx,"COMMA");
}

void set_leftparen(SymTy idx) {
  leftparen_symbol = idx;
  add_symbol(idx,"LEFTPAREN");
}

void set_rightparen(SymTy idx) {
  rightparen_symbol = idx;
  add_symbol(idx,"RIGHTPAREN");
}

IntTy print_symbol(SymTy idx) {
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
    struct SymTable_elem *s;
    HASH_FIND(hh, global_sym_table, &idx, sizeof(SymTy), s);
    if (s == NULL) {
        return printf("%lld", idx);
    } else {
        return printf("%s", s->value);
    }

  }
}

#ifdef _PARALLEL
SymTy gensym() {
    SymTy idx = __atomic_add_fetch(&global_gensym_counter, 1, __ATOMIC_SEQ_CST);
    return idx;
}
#else
SymTy gensym() {
    global_gensym_counter += 1;
    SymTy idx = global_gensym_counter;
    return idx;
}
#endif

void free_symtable() {
    struct SymTable_elem *elt, *tmp;
    HASH_ITER(hh, global_sym_table, elt, tmp) {
        HASH_DEL(global_sym_table,elt);
    }
    free(elt);
    free(tmp);
}

/*

----------------------------------------
Garbage collection
----------------------------------------

   Gibbon has "growing regions" i.e each logical region is backed by a doubly linked-list
   of smaller chunks which grows as required. In addition to actual data, each chunk
   stores some additional metadata (RegionFooter) to chain the chunks together in a list
   and for garbage collection. The footer:

   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
   serialized data | rf_reg_metadata_ptr | rf_seq_no | rf_size | rf_next | rf_prev
   ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

   The metadata after the serialized data serves various purposes:

   - rf_reg_metadata_ptr: A pointer to a RegionTy struct that contains various metadata.
     Of particular interest to us are the fields:

     = reg_id: A unique identifier for a region.

     = refcount and outset: Whenever an inter-region indirection is created, we record that information
       using these two fields. Suppose we have an indirection from region A that points to some chunk
       in region B. Then A's outset will store a pointer to that chunk's footer, and B's refcount will
       be bumped by 1. Note that all there's only 1 refcount cell, and 1 outset per logical region,
       and chunks only store a pointer to them.

   - rf_seq_no: The index of this particular chunk in the list.

   - rf_size: Used during bounds checking to calculate the size of the next region in
     the linked list.

   - rf_next / rf_prev: Point to the next and previous chunk respectively.


There are two ways in which a region may be freed:

(1) Whenever it goes out of scope

  The RTS tries to free a region whenever it goes out of scope. But this doesn't always succeed as
  regions sometimes contain values that "escape". One reason why this'll happen is if there's an
  indirection from A->B, and A lives longer than B.
  In such a case, when B goes out of scope it's refcount won't be 0, and the RTS won't free it.
  This brings us to (2).

(2)

  When the RTS successfully frees a region, it decrements the refcounts of all the regions it
  points to (via the outset). At the same time, if it encounters a region in the outset whoose
  refcount becomes 0 after the decrement, it calls free_region on that. This way we can be sure
  that all regions will eventually be garbage collected before the program exits.



Why is it a doubly linked-list?
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Due to way that bounds-checking works, the pointers in the outset may actually point to any
arbitrary chunk in the chain. However, we must call free_region on the first one to ensure that
all of them are GC'd. So we need pointers to traverse backward get to the first one.
'trav_to_first_chunk' accomplishes this.

 */

#define MAX_OUTSET_LENGTH 10

typedef struct RegionTy_struct {
    SymTy reg_id;
    uint reg_refcount;
    CursorTy reg_heap;
    uint reg_outset_len;
    CursorTy reg_outset[MAX_OUTSET_LENGTH];
} RegionTy;

typedef struct RegionFooter_struct {
    RegionTy *rf_reg_metadata_ptr;

    IntTy rf_seq_no;
    IntTy rf_size;
    struct RegionFooter_struct *rf_next;
    struct RegionFooter_struct *rf_prev;
} RegionFooter;

typedef struct ChunkTy_struct {
    CursorTy chunk_start;
    CursorTy chunk_end;
} ChunkTy;

static inline void insert_into_outset(CursorTy ptr, RegionTy *reg) {
    uint outset_len = reg->reg_outset_len;
    // Check for duplicates.
    for (uint i = 0; i < outset_len; i++) {
        if (ptr == reg->reg_outset[i]) {
            return;
        }
    }
    // Otherwise, insert into the outset.
    reg->reg_outset[outset_len] = ptr;
    reg->reg_outset_len = outset_len + 1;
    return;
}

static inline void remove_from_outset(CursorTy ptr, RegionTy *reg) {
    uint outset_len = reg->reg_outset_len;
    CursorTy *outset = reg->reg_outset;
    int i;
    if (outset_len == 0) {
        fprintf(stderr, "remove_from_outset: empty outset\n");
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
        fprintf(stderr, "remove_from_outset: element not found\n");
        exit(1);
    }
    // Move all elements ahead of 'elt_idx' back by one position.
    for (i = elt_idx; i < outset_len; i++) {
        outset[i] = outset[i+1];
    }
    return;
}

RegionTy *alloc_region(IntTy size) {
    // Allocate the region metadata.
    RegionTy *reg = ALLOC(sizeof(RegionTy));
    if (reg == NULL) {
        printf("alloc_region: allocation failed: %ld", sizeof(RegionTy));
        exit(1);
    }

    // Allocate the first chunk.
    IntTy total_size = size + sizeof(RegionFooter);
    CursorTy heap = ALLOC_PACKED_BIG(total_size);
    if (heap == NULL) {
        printf("alloc_region: malloc failed: %lld", total_size);
        exit(1);
    }
    // Not heap+total_size, since we must keep space for the footer.
    CursorTy heap_end = heap + size;

    // Initialize metadata fields.
    reg->reg_id = gensym();
    reg->reg_refcount = 1;
    reg->reg_heap = heap;
    reg->reg_outset_len = 0;

#ifdef _DEBUG
    printf("Allocated a region(%lld): %lld bytes.\n", reg->reg_id, size);
#endif

    // Write the footer.
    RegionFooter *footer = (RegionFooter *) heap_end;
    footer->rf_reg_metadata_ptr = reg;
    footer->rf_seq_no = 1;
    footer->rf_size = size;
    footer->rf_next = NULL;
    footer->rf_prev = NULL;

    return reg;
}

RegionTy *alloc_counted_region(IntTy size) {
    // Bump the count.
    bump_global_region_count();
    return alloc_region(size);
}

ChunkTy alloc_chunk(CursorTy end_old_chunk) {
    // Get size from current footer.
    RegionFooter *footer = (RegionFooter *) end_old_chunk;
    IntTy newsize = footer->rf_size * 2;
    // See #110.
    if (newsize > global_inf_buf_max_chunk_size) {
        newsize = global_inf_buf_max_chunk_size;
    }
    IntTy total_size = newsize + sizeof(RegionFooter);

    // Allocate.
    CursorTy start = ALLOC_PACKED_BIG(total_size);
    if (start == NULL) {
        printf("alloc_chunk: malloc failed: %lld", total_size);
        exit(1);
    }
    CursorTy end = start + newsize;

    // Link the next chunk's footer.
    footer->rf_next = (RegionFooter *) end;

    // Write the footer.
    RegionFooter* new_footer = (RegionFooter *) end;
    new_footer->rf_reg_metadata_ptr = footer->rf_reg_metadata_ptr;
    new_footer->rf_seq_no = footer->rf_seq_no + 1;
    new_footer->rf_size = newsize;
    new_footer->rf_next = NULL;
    new_footer->rf_prev = footer;

#ifdef _DEBUG
    RegionTy *reg = (RegionTy*) new_footer->rf_reg_metadata_ptr;
    printf("alloc_chunk: allocated %lld bytes for region %lld.\n", total_size, reg->reg_id);
#endif

    return (ChunkTy) {start , end};
}

RegionFooter* trav_to_first_chunk(RegionFooter *footer) {
    if (footer->rf_seq_no == 1) {
        return footer;
    } else if (footer->rf_prev == NULL) {
        fprintf(stderr, "No previous chunk found at rf_seq_no: %lld", footer->rf_seq_no);
        return NULL;
    } else {
        trav_to_first_chunk((RegionFooter *) footer->rf_prev);
    }
    return NULL;
}

uint get_ref_count(CursorTy end_ptr) {
    RegionFooter *footer = (RegionFooter *) end_ptr;
    RegionTy *reg = (RegionTy *) footer->rf_reg_metadata_ptr;
    return reg->reg_refcount;
}

// B is the pointer, and A is the pointee (i.e B -> A).
// Bump A's refcount and update B's outset.
static inline void bump_ref_count(CursorTy end_b, CursorTy end_a) {
    // Grab footers.
    RegionFooter *footer_a = (RegionFooter *) end_a;
    RegionFooter *footer_b = (RegionFooter *) end_b;

    // Grab metadata.
    RegionTy *reg_a = (RegionTy *) footer_a->rf_reg_metadata_ptr;
    RegionTy *reg_b = (RegionTy *) footer_b->rf_reg_metadata_ptr;

    // Bump A's refcount.
    uint current_refcount, new_refcount;
    current_refcount = reg_a->reg_refcount;
    new_refcount = current_refcount + 1;
    reg_a->reg_refcount = new_refcount;

#ifdef _DEBUG
    printf("bump_ref_count: %lld -> %lld\n", reg_b->reg_id, reg_a->reg_id);
    printf("bump_ref_count: old-refcount=%d, old-outset-len=%d:\n", current_refcount, reg_b->reg_outset_len);
    assert(current_refcount == reg_b->reg_outset_len+1);
#endif

    // Add A to B's outset.
    insert_into_outset(end_a, reg_b);

#ifdef _DEBUG
    // printf("bump_ref_count: Added %p to %lld's outset, %p.\n", end_a, reg_b->reg_id, reg_b);
    printf("bump_ref_count: new-refcount=%d, new-outset-len=%d\n", new_refcount, reg_b->reg_outset_len);
    assert(new_refcount == reg_b->reg_outset_len+1);
#endif

    return;
}

void free_region(CursorTy end_reg) {
    // Grab footer and the metadata.
    RegionFooter *footer = (RegionFooter *) end_reg;
    RegionTy *reg = (RegionTy *) footer->rf_reg_metadata_ptr;

    //
    RegionFooter *first_chunk_footer, *next_chunk_footer;
    CursorTy first_chunk, next_chunk;

    // Decrement current reference count.
    uint current_refcount, new_refcount;
    current_refcount = reg->reg_refcount;
    new_refcount = 0;
    if (current_refcount != 0) {
        new_refcount = current_refcount - 1;
        reg->reg_refcount = new_refcount;
    }

#ifdef _DEBUG
    printf("free_region(%lld): refcounts (1): old-refcount=%d, new-refcount=%d:\n", reg->reg_id, current_refcount, new_refcount);
#endif


    // Free this region recount is 0.
    if (new_refcount == 0) {

#ifdef _DEBUG
        printf("free_region(%lld): outset length: %d\n", reg->reg_id, reg->reg_outset_len);
#endif

        // Decrement refcounts, free regions with refcount==0 and also free
        // elements of the outset.
        if (reg->reg_outset_len != 0) {
            uint outset_len = reg->reg_outset_len;
            CursorTy *outset = reg->reg_outset;
            RegionFooter *elt_footer;
            RegionTy *elt_reg;
            uint elt_current_refcount, elt_new_refcount;
            CursorTy to_be_removed[MAX_OUTSET_LENGTH];
            uint to_be_removed_idx = 0;
            for (int i = 0; i < outset_len; i++) {
                elt_footer = (RegionFooter *) outset[i];
                elt_reg = (RegionTy *) elt_footer->rf_reg_metadata_ptr;
#ifdef _DEBUG
                elt_current_refcount = elt_reg->reg_refcount;
#endif
                elt_new_refcount = elt_current_refcount - 1;
                elt_reg->reg_refcount = elt_new_refcount;
#ifdef _DEBUG
                printf("free_region(%lld): old-refcount=%d, new-refcount=%d:\n",
                       elt_reg->reg_id, elt_current_refcount, elt_reg->reg_refcount);
#endif
                if (elt_new_refcount == 0) {
                    // See [Why is it a doubly linked-list?] above
                    first_chunk_footer = trav_to_first_chunk(elt_footer);
                    if (first_chunk_footer != NULL) {
                        free_region((CursorTy) first_chunk_footer);
                    }
                }
                to_be_removed[to_be_removed_idx] = outset[i];
                to_be_removed_idx++;
            }
            // Remove elements from the outset.
            for (uint i = 0; i < to_be_removed_idx; i++) {
                remove_from_outset(to_be_removed[i], reg);
            }
        }


#ifdef _DEBUG
        // Bookkeeping
        IntTy num_freed_chunks = 0, total_bytesize = 0;
#endif

        // Free the chunks in this region.
        first_chunk = end_reg - footer->rf_size;
        first_chunk_footer = footer;
        next_chunk = (char*) footer->rf_next;

#ifdef _DEBUG
        printf("free_region(%lld)\n", reg->reg_id);
#endif

#ifdef _DEBUG
        num_freed_chunks++;
        total_bytesize = total_bytesize + first_chunk_footer->rf_size;
#endif
        free(first_chunk);

        while (next_chunk != NULL) {
            next_chunk_footer = (RegionFooter *) next_chunk;
            #ifdef _DEBUG
            num_freed_chunks++;
            total_bytesize = total_bytesize + next_chunk_footer->rf_size;
            #endif
            free(next_chunk - next_chunk_footer->rf_size);
            next_chunk = (char*) next_chunk_footer->rf_next;
        }

#ifdef _DEBUG
        printf("free_region(%lld): Freed %lld bytes across %lld chunks.\n",
               reg->reg_id, total_bytesize, num_freed_chunks);
#endif

        // Free the metadata.
        free(reg);

    } else {
#ifdef _DEBUG
        printf("free_region(%lld): non-zero refcount: %d.\n",
               reg->reg_id, reg->reg_refcount);
#endif
    }
}

// Assume that all nodes with size information have tags >= 150.
BoolTy is_big(IntTy i, CursorTy cur) {
    TagTyPacked tag = *(TagTyPacked *) cur;
    if (tag >= 150) {
        cur += 1;
        IntTy size = *(IntTy *) cur;
        if (size >= i) {
            return true;
        } else {
            return false;
        }
    }
    return false;
}

// -------------------------------------
// Vectors
// -------------------------------------

typedef struct VectorTy_struct {
    // Bounds on the vector.
    IntTy vec_lower, vec_upper;

    // Size of each element.
    IntTy vec_elt_size;

    // Actual elements of the vector.
    void* vec_data;
} VectorTy;

VectorTy* vector_alloc(IntTy num, IntTy elt_size) {
    VectorTy *vec = ALLOC(sizeof(VectorTy));
    if (vec == NULL) {
        printf("alloc_vector: malloc failed: %ld", sizeof(VectorTy));
        exit(1);
    }
    void* data = ALLOC(num * elt_size);
    if (data == NULL) {
        printf("alloc_vector: malloc failed: %ld", sizeof(num * elt_size));
        exit(1);
    }
    vec->vec_lower = 0;
    vec->vec_upper = num;
    vec->vec_elt_size = elt_size;
    vec->vec_data = data;
    return vec;
}

IntTy vector_length(VectorTy *vec) {
    return (vec->vec_upper - vec->vec_lower);
}

BoolTy vector_is_empty(VectorTy *vec) {
    return (vector_length(vec) == 0);
}

VectorTy* vector_slice(IntTy i, IntTy n, VectorTy *vec) {
    IntTy lower = vec->vec_lower + i;
    IntTy upper = vec->vec_lower + i + n;
    if ((lower > vec->vec_upper)) {
        printf("vector_slice: lower out of bounds, %lld > %lld", lower, vec->vec_upper);
        exit(1);
    }
    if ((upper > vec->vec_upper)) {
        printf("vector_slice: upper out of bounds: %lld > %lld", upper, vec->vec_upper);
        exit(1);
    }
    VectorTy *vec2 = ALLOC(sizeof(VectorTy));
    if (vec == NULL) {
        printf("vector_slice: malloc failed: %ld", sizeof(VectorTy));
        exit(1);
    }
    vec2->vec_lower = lower;
    vec2->vec_upper = upper;
    vec2->vec_elt_size = vec->vec_elt_size;
    vec2->vec_data = vec->vec_data;
    return vec2;
}

// The callers must cast the return value.
static inline void* vector_nth(VectorTy *vec, IntTy i) {
    // if (i < vec->lower || i > vec->upper) {
    //     printf("vector_nth index out of bounds: %lld (%lld,%lld) \n", i, vec->vec_lower, vec->vec_upper);
    //     exit(1);
    // }
    return (vec->vec_data + (vec->vec_elt_size * (vec->vec_lower + i)));
}

static inline VectorTy* vector_inplace_update(VectorTy *vec, IntTy i, void* elt) {
    void* dst = vector_nth(vec, i);
    memcpy(dst, elt, vec->vec_elt_size);
    return vec;
}

static inline VectorTy* vector_copy(VectorTy *vec) {
    IntTy len = vector_length(vec);
    void *start = vector_nth(vec, 0);
    VectorTy *vec2 = vector_alloc(len, vec->vec_elt_size);
    memcpy(vec2->vec_data, start, len * vec->vec_elt_size);
    return vec2;
}

static inline VectorTy* vector_inplace_sort(VectorTy *vec, int (*compar)(const void *, const void*)) {
    void *start = vector_nth(vec, 0);
    qsort(start, vector_length(vec), vec->vec_elt_size, compar);
    return vec;
}

static inline VectorTy* vector_sort(VectorTy *vec, int (*compar)(const void *, const void*)) {
    VectorTy *vec2 = vector_copy(vec);
    vector_inplace_sort(vec2, compar);
    return vec2;
}

static inline VectorTy* vector_concat(VectorTy *vec) {
    // Length of the input vector.
    IntTy len = vector_length(vec);
    // Length of the concatenated vector.
    IntTy result_len = 0;
    // Size of each element in the concatenated vector.
    IntTy result_elt_size = 0;
    VectorTy **elt_ref, *elt;
    for (IntTy i = 0; i < len; i++) {
        elt_ref = vector_nth(vec, i);
        elt = *elt_ref;
        result_elt_size = elt->vec_elt_size;
        result_len += vector_length(elt);
    }

    // Concatenated vector.
    VectorTy *result = vector_alloc(result_len, result_elt_size);
    IntTy elt_len;
    // A counter that tracks the index of elements in 'result'.
    IntTy k = 0;
    for (IntTy i = 0; i < len; i++) {
        elt_ref = vector_nth(vec, i);
        elt = *elt_ref;
        elt_len = vector_length(elt);

        for (IntTy j = 0; j < elt_len; j++) {
            void* k_elt = vector_nth(elt, j);
            vector_inplace_update(result, k, k_elt);
            k++;
        }
    }

    return result;
}

static inline void vector_free(VectorTy *vec) {
    free(vec->vec_data);
    free(vec);
    return;
}

static inline VectorTy* vector_merge(VectorTy *vec1, VectorTy *vec2) {
    if (vec1->vec_upper != vec2->vec_lower) {
        printf("vector_merge: non-contiguous slices, (%lld,%lld), (%lld,%lld).",
               vec1->vec_lower, vec1->vec_upper, vec2->vec_lower, vec2->vec_upper);
        exit(1);
    }
    VectorTy *merged = ALLOC(sizeof(VectorTy));
    if (merged == NULL) {
        printf("vector_merge: malloc failed: %ld", sizeof(VectorTy));
        exit(1);
    }
    merged->vec_lower = vec1->vec_lower;
    merged->vec_upper = vec2->vec_upper;
    merged->vec_elt_size = vec1->vec_elt_size;
    merged->vec_data = vec1->vec_data;
    return merged;
}

void print_timing_array(VectorTy *times) {
    printf("TIMES: [");
    double *d;
    IntTy n = vector_length(times);
    for(int i = 0; i < n; i++) {
        d = vector_nth(times, i);
        if (i == (n-1)) {
            printf("%f",*d);
        }
        else {
            printf("%f, ",*d);
        }
    }
    printf("]\n");
}

double sum_timing_array(VectorTy *times) {
    double *d;
    double acc = 0;
    for(int i = 0; i < vector_length(times); i++) {
        d = vector_nth(times, i);
        acc += *d;
    }
    return acc;
}

// -------------------------------------
// Linked lists
// -------------------------------------

typedef struct ListTy_struct {
    IntTy ll_data_size;
    void* ll_data;
    struct ListTy_struct* ll_next;
} ListTy;

static inline ListTy* list_alloc(IntTy data_size) {
    // ListTy *ls = ALLOC(sizeof(ListTy));
    ListTy *ls = BUMPALLOC(sizeof(ListTy));
    ls->ll_data_size = data_size;
    ls->ll_data = NULL;
    ls->ll_next = NULL;
    return ls;
}

static inline BoolTy list_is_empty(ListTy *ls) {
    return ls->ll_next == NULL;
}

static inline ListTy* list_cons(void* elt, ListTy *ls) {
    // void* data = ALLOC(ls->data_size);
    void* data = BUMPALLOC(ls->ll_data_size);
    if (data == NULL) {
        printf("list_cons: malloc failed: %lld", ls->ll_data_size);
        exit(1);
    }
    memcpy(data, elt, ls->ll_data_size);
    // ListTy *res = ALLOC(sizeof(ListTy));
    ListTy *res = BUMPALLOC(sizeof(ListTy));
    res->ll_data_size = ls->ll_data_size;
    res->ll_data = data;
    res->ll_next = (ListTy*) ls;
    return res;
}

static inline void* list_head(ListTy *ls) {
    return ls->ll_data;
}

static inline ListTy* list_tail(ListTy *ls) {
    return ls->ll_next;
}

static inline void list_free(ListTy *ls) {
    free(ls->ll_data);
    free(ls);
    return;
}

static inline ListTy* list_copy(ListTy *ls) {
    ListTy *ls2 = list_alloc(ls->ll_data_size);
    if (ls->ll_data != NULL) {
        void* data = BUMPALLOC(ls->ll_data_size);
        memcpy(data, ls->ll_data, ls->ll_data_size);
        ls2->ll_data = data;
    }
    ls2->ll_next = ls->ll_next;
    return ls2;
}

// -------------------------------------
// Ppm Images
// -------------------------------------

typedef struct __Pixel_struct {
    IntTy field0;
    IntTy field1;
    IntTy field2;
} __Pixel;

void writePpm(char* filename, IntTy width, IntTy height, VectorTy *pixels);
void writePpm_loop(FILE *fp, IntTy idx, IntTy end, VectorTy *pixels);

// Example: writePpm("gibbon_rgb_1000.ppm", 1000, 1000, pixels);
void writePpm(char* filename, IntTy width, IntTy height, VectorTy *pixels)
{
    FILE *fp;
    fp = fopen(filename, "w+");
    fprintf(fp, "P3\n");
    fprintf(fp, "%lld %lld\n255\n", width, height);
    IntTy len = vector_length(pixels);
    writePpm_loop(fp, 0, len, pixels);
    fclose(fp);
    return;
}

void writePpm_loop(FILE *fp, IntTy idx, IntTy end, VectorTy *pixels)
{
    BoolTy fltIf_5768_6575 = idx == end;

    if (fltIf_5768_6575) {
        return;
    } else {
        __Pixel *tmp_112;
        tmp_112 = (__Pixel *) vector_nth(pixels, idx);
        __Pixel tup = *tmp_112;
        IntTy x = tup.field0;
        IntTy y = tup.field1;
        IntTy z = tup.field2;
        // write to file.
        fprintf(fp, "%lld %lld %lld\n", x, y, z);
        writePpm_loop(fp, (idx+1), end, pixels);
    }
}

/* -------------------------------------------------------------------------------- */

int main(int argc, char** argv)
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
      fprintf(stderr, " [gibbon rts] Failed to set stack size to %llu, code %d\n", (unsigned long long)lim.rlim_cur, code);
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
          show_usage(argv);
          exit(0);
        }
        else if (strcmp(argv[i], "--biginf-buffer-size") == 0 && i < argc - 1)
        {
            global_init_biginf_buf_size = atoll(argv[i + 1]);
            i++;
        }
        else if (strcmp(argv[i], "--inf-buffer-size") == 0 && i < argc - 1)
        {
            global_init_inf_buf_size = atoll(argv[i + 1]);
            i++;
        }
        else if ((strcmp(argv[i], "--bench-input") == 0)) {
          if (i+1 >= argc) {
            fprintf(stderr, "Not enough arguments after --bench-input, expected <file>.\n");
            show_usage(argv);
            exit(1);
          }
          global_benchfile_param = argv[i+1];
          i++;
        }
        else if ((strcmp(argv[i], "--array-input") == 0)) {
          if (i+1 >= argc) {
            fprintf(stderr, "Not enough arguments after --array-input, expected <file>.\n");
            show_usage(argv);
            exit(1);
          }
          global_arrayfile_param = argv[i+1];
          i++;
        }
        else if (strcmp(argv[i], "--array-input-length") == 0 && i < argc - 1) {
            global_arrayfile_length_param = atoll(argv[i+1]);
            i++;
        }
        else if (strcmp(argv[i], "--bench-prog") == 0 && i < argc - 1) {
            int len = strlen(argv[i+1]);
            global_bench_prog_param = (char*) malloc((len+1)*sizeof(char));
            strncpy(global_bench_prog_param,argv[i+1],len);
            i++;
        }
        // If present, we expect the two arguments to be <size> <iters>
        else if (got_numargs >= 2) {
            fprintf(stderr, "Extra arguments left over: ");
            for(; i < argc; i++) fprintf(stderr, "%s ", argv[i]);
            show_usage(argv);
            exit(1);
        } else {
          if (got_numargs == 0) {
            global_size_param  = atoll(argv[i]);
            got_numargs ++;
          } else {
            global_iters_param = atoll(argv[i]);
          }
        }
    }

    // Initialize global_bench_prog_param to an empty string in case
    // the runtime argument --bench-prog isn't passed.
    if (global_bench_prog_param == NULL) {
        global_bench_prog_param = (char*) malloc(1*sizeof(char));
        *global_bench_prog_param = '\n';
    }

    __main_expr();

    return 0;
}



// -----------------------------------------------------------------------------
// Program starts here
// -----------------------------------------------------------------------------

typedef struct Prod_struct { } Prod;
typedef struct Int64Prod_struct {
            IntTy field0;
        } Int64Prod;
typedef struct Int64CursorProd_struct {
            IntTy field0;
            CursorTy field1;
        } Int64CursorProd;
typedef struct BoolProd_struct {
            BoolTy field0;
        } BoolProd;
typedef struct TagCursorProd_struct {
            TagTyPacked field0;
            CursorTy field1;
        } TagCursorProd;
typedef struct CursorProd_struct {
            CursorTy field0;
        } CursorProd;
typedef struct CursorInt64Prod_struct {
            CursorTy field0;
            IntTy field1;
        } CursorInt64Prod;
typedef struct CursorCursorProd_struct {
            CursorTy field0;
            CursorTy field1;
        } CursorCursorProd;
typedef struct CursorCursorCursorProd_struct {
            CursorTy field0;
            CursorTy field1;
            CursorTy field2;
        } CursorCursorCursorProd;
typedef struct CursorCursorCursorCursorProd_struct {
            CursorTy field0;
            CursorTy field1;
            CursorTy field2;
            CursorTy field3;
        } CursorCursorCursorCursorProd;

// -----------------------------------------------------------------------------
// Stack related functions 
// -----------------------------------------------------------------------------

typedef struct stackValue{

  uintptr_t pointers[5]; 
  bool visitedPreOrder;
  bool visitedInOrder;
  bool visitedPostOrder; 

  CursorInt64Prod returnVal;

} stackValue; 

stackValue * newStackNode(){

  stackValue * newNode = (stackValue*) malloc(sizeof(stackValue));
  newNode->visitedPreOrder = false;
  newNode->visitedInOrder = false;
  newNode->visitedPostOrder = false;

  return newNode;

}

// void display(int* top, stackValue* stack)
// {
//     if(*top>=0)
//     {
//         printf("\n The elements in STACK \n");
//         for(int i=*top; i>=0; i--)
//             printf("\n%ld\n",stack[i]);
//     }
//     else
//     {
//         printf("\n The STACK is empty");
//     }
   
// }

static inline void push(stackValue *pointer, int* top, stackValue* stack){

    if ((void*)pointer == NULL)
        return;

    if(*top < stack_size - 1 ){
        (*top)++;
        stack[*top] = *pointer;
    }
    else{
        printf("Stack Overflow!!\n");
    }
}

static inline stackValue* pop(int* top, stackValue* stack){

    if(*top > -1){
        stackValue* pointer = &stack[*top];
        (*top)--; 
        return pointer;
    }
    else{
        printf("Stack Underflow\n"); 
        exit(1);
    }

}

static inline stackValue* _top(int* top, stackValue* stack){

    if(*top > -1){
        return &stack[*top];
    }
    else{
        printf("Stack Underflow\n");
        exit(1); 
    }

}

static inline void _pop(int* top){

    if(*top > -1){
        (*top)--; 
        return;
    }
    else{
        printf("Stack Underflow\n");
        return;
    }

}

static inline bool isEmptyStack(int* top){

    if (*top > -1){
        return false;
    }
    
    return true;
    
}

stackValue* newStack(){

  stackValue* newstack = (stackValue*) malloc(sizeof(stackValue) * stack_size);
  return newstack;

}


CursorCursorCursorProd mkTree(CursorTy end_r_335, CursorTy loc_334,
                              IntTy d_14_94_151);
void sumTree(CursorTy end_r_337, CursorTy tr_15_95_157, CursorInt64Prod* returnValPreOrder, stackValue* stack);
CursorCursorCursorCursorProd add1Tree(CursorTy end_r_340, CursorTy end_r_341,
                                      CursorTy loc_339, CursorTy t_19_99_163);
CursorCursorCursorCursorProd _copy_Tree(CursorTy end_r_344, CursorTy end_r_345,
                                        CursorTy loc_343,
                                        CursorTy arg_59_103_170);
CursorCursorCursorCursorProd _copy_without_ptrs_Tree(CursorTy end_r_348,
                                                     CursorTy end_r_349,
                                                     CursorTy loc_347,
                                                     CursorTy arg_66_110_177);
CursorProd _traverse_Tree(CursorTy end_r_351, CursorTy arg_73_117_184);
CursorProd _print_Tree(CursorTy end_r_353, CursorTy arg_80_123_190);
CursorCursorCursorCursorProd _add_size_and_rel_offsets_Tree(CursorTy end_r_356,
                                                            CursorTy end_r_357,
                                                            CursorTy loc_355,
                                                            CursorTy arg_325);
CursorCursorCursorProd mkTree(CursorTy end_r_335, CursorTy loc_334,
                              IntTy d_14_94_151)
{
    if (loc_334 + 32 > end_r_335) {
        ChunkTy new_chunk_2 = alloc_chunk(end_r_335);
        CursorTy chunk_start_3 = new_chunk_2.chunk_start;
        CursorTy chunk_end_4 = new_chunk_2.chunk_end;
        
        end_r_335 = chunk_end_4;
        *(TagTyPacked *) loc_334 = 255;
        
        CursorTy redir = loc_334 + 1;
        
        *(CursorTy *) redir = chunk_start_3;
        loc_334 = chunk_start_3;
    }
    
    BoolTy fltIf_139_152 = d_14_94_151 == 0;
    
    if (fltIf_139_152) {
        *(TagTyPacked *) loc_334 = 0;
        
        CursorTy writetag_577 = loc_334 + 1;
        
        *(IntTy *) writetag_577 = 1;
        
        CursorTy writecur_578 = writetag_577 + sizeof(IntTy);
        
        return (CursorCursorCursorProd) {end_r_335, loc_334, writecur_578};
    } else {
        IntTy fltAppE_141_153 = d_14_94_151 - 1;
        CursorTy loc_371 = loc_334 + 1;
        CursorCursorCursorProd tmp_struct_0 =
                                mkTree(end_r_335, loc_371, fltAppE_141_153);
        CursorTy pvrtmp_899 = tmp_struct_0.field0;
        CursorTy pvrtmp_900 = tmp_struct_0.field1;
        CursorTy pvrtmp_901 = tmp_struct_0.field2;
        IntTy fltAppE_143_155 = d_14_94_151 - 1;
        CursorCursorCursorProd tmp_struct_1 =
                                mkTree(pvrtmp_899, pvrtmp_901, fltAppE_143_155);
        CursorTy pvrtmp_906 = tmp_struct_1.field0;
        CursorTy pvrtmp_907 = tmp_struct_1.field1;
        CursorTy pvrtmp_908 = tmp_struct_1.field2;
        
        *(TagTyPacked *) loc_334 = 1;
        
        CursorTy writetag_582 = loc_334 + 1;
        
        return (CursorCursorCursorProd) {pvrtmp_906, loc_334, pvrtmp_908};
    }
}
void sumTree(CursorTy end_r_337, CursorTy tr_15_95_157, CursorInt64Prod* returnedVal, stackValue* stack)
{
    int top = -1;

    stackValue* root = newStackNode();
    root->pointers[0] = (uintptr_t) tr_15_95_157;
    root->visitedInOrder   = false;
    root->visitedPostOrder = false;

    push(root, &top, stack);

    IntTy pvrtmp_922;
    CursorTy pvrtmp_921;
    CursorTy pvrtmp_923;
    IntTy pvrtmp_924; 

    returnedVal->field1 = 0;

  while(!isEmptyStack(&top)){

     stackValue* currNode = _top(&top, stack);
     tr_15_95_157 = (CursorTy) currNode->pointers[0];

     TagTyPacked tmpval_917 = *(TagTyPacked *) tr_15_95_157;
     CursorTy tmpcur_918 = tr_15_95_157 + 1;

  switch_933:
    ;
    switch (tmpval_917) {
        
      case 0:
        {   
            IntTy tmpval_919 = *(IntTy *) tmpcur_918;
            CursorTy jump_477 = tmpcur_918 + 8;
            
            returnedVal->field0 = jump_477; 
            returnedVal->field1 += tmpval_919; 
            _pop(&top);
            break;
        }
        
      case 1:
        {   

           if(currNode->visitedInOrder){
            pvrtmp_921 = returnedVal->field0;
            pvrtmp_922 = returnedVal->field1;
            
            stackValue right;
            right.pointers[0] = (uintptr_t) pvrtmp_921;
            right.visitedInOrder   = false;
            right.visitedPostOrder = false;
            
            push(&right, &top, stack);
            currNode->visitedInOrder   = false;
            currNode->visitedPostOrder = true;
            continue;

           }   
           
           if (currNode->visitedPostOrder){

            //pvrtmp_923 = returnedVal->field0;
            //pvrtmp_924 = returnedVal->field1;
            //IntTy tailprim_480  = pvrtmp_922 + pvrtmp_924;
            
            //returnedVal->field0 = pvrtmp_923; 
            //returnedVal->field1 = tailprim_480;
            
            _pop(&top);
            continue;

           }

           

            stackValue left;
            left.pointers[0] = (uintptr_t) tmpcur_918;
            left.visitedInOrder   = false;
            left.visitedPostOrder = false;

            push(&left, &top, stack);
            currNode->visitedInOrder = true;


            //sumTree(end_r_337, tmpcur_918, returnedVal);
            //push(tmpcur_918, &top, stack);
            //if (maskVal == 0){
            //  push((uintptr_t) returnedVal, &top, stack);
            //  push((uintptr_t) tmpcur_918, &top, stack);
            //  continue;
            //}
            //else if (maskVal == 1)
            //   maskVal = 2;
            
            // CursorTy pvrtmp_921 = returnedVal->field0;
            // IntTy pvrtmp_922    = returnedVal->field1;

            //sumTree(end_r_337, pvrtmp_921, returnedVal);
            //if (maskVal == 0){
            //  push((uintptr_t) returnedVal, &top, stack);
            //  push((uintptr_t) pvrtmp_921, &top, stack);
            //  continue; 
            //}
            //else if (maskVal == 3)
          
            // CursorTy pvrtmp_923 = returnedVal->field0;
            // IntTy pvrtmp_924    = returnedVal->field1;
            // IntTy tailprim_480  = pvrtmp_922 + pvrtmp_924;
            
            // returnedVal->field0 = pvrtmp_923; 
            // returnedVal->field1 = tailprim_480;

            break;
           
        }
        
      case INDIRECTION_TAG:
        {   
            //printf("INDIRECTION CASE\n");
            CursorTy tmpcur_925 = *(CursorTy *) tmpcur_918;
            CursorTy tmpaftercur_926 = tmpcur_918 + 8;
            CursorTy jump_517 = tmpcur_918 + 8;

            
            _pop(&top);

            stackValue toPush; 
            toPush.pointers[0] = (uintptr_t) tmpcur_925;
            toPush.visitedInOrder = false;
            toPush.visitedPostOrder = false;

            push(&toPush, &top, stack);


        }
        
      case REDIRECTION_TAG:
        {   
            //printf("REDIRECTION_TAG\n"); 
            CursorTy tmpcur_929 = *(CursorTy *) tmpcur_918;
            CursorTy tmpaftercur_930 = tmpcur_918 + 8;
            
            _pop(&top);

            stackValue toPush; 
            toPush.pointers[0] = (uintptr_t) tmpcur_929;
            toPush.visitedInOrder = false;
            toPush.visitedPostOrder = false;

            push(&toPush, &top, stack);
            
            //currNode->visitedInOrder = true;
            //currNode->visitedPostOrder = true;
            //sumTree(end_r_337, tmpcur_929, returnedVal);
            //break;
            
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_917");
            exit(1);
        }
    }
  }
}

CursorCursorCursorCursorProd add1Tree(CursorTy end_r_340, CursorTy end_r_341,
                                      CursorTy loc_339, CursorTy t_19_99_163)
{
    if (loc_339 + 32 > end_r_341) {
        ChunkTy new_chunk_13 = alloc_chunk(end_r_341);
        CursorTy chunk_start_14 = new_chunk_13.chunk_start;
        CursorTy chunk_end_15 = new_chunk_13.chunk_end;
        
        end_r_341 = chunk_end_15;
        *(TagTyPacked *) loc_339 = 255;
        
        CursorTy redir = loc_339 + 1;
        
        *(CursorTy *) redir = chunk_start_14;
        loc_339 = chunk_start_14;
    }
    
    TagTyPacked tmpval_934 = *(TagTyPacked *) t_19_99_163;
    CursorTy tmpcur_935 = t_19_99_163 + 1;
    
    
  switch_986:
    ;
    switch (tmpval_934) {
        
      case 0:
        {
            IntTy tmpval_936 = *(IntTy *) tmpcur_935;
            CursorTy tmpcur_937 = tmpcur_935 + sizeof(IntTy);
            CursorTy jump_481 = tmpcur_935 + 8;
            IntTy fltPkd_146_165 = tmpval_936 + 1;
            
            *(TagTyPacked *) loc_339 = 0;
            
            CursorTy writetag_599 = loc_339 + 1;
            
            *(IntTy *) writetag_599 = fltPkd_146_165;
            
            CursorTy writecur_600 = writetag_599 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {end_r_341, jump_481, loc_339,
                                                   writecur_600};
            break;
        }
        
      case 1:
        {
            CursorTy loc_398 = loc_339 + 1;
            CursorCursorCursorCursorProd tmp_struct_9 =
                                          add1Tree(end_r_340, end_r_341, loc_398, tmpcur_935);
            CursorTy pvrtmp_942 = tmp_struct_9.field0;
            CursorTy pvrtmp_943 = tmp_struct_9.field1;
            CursorTy pvrtmp_944 = tmp_struct_9.field2;
            CursorTy pvrtmp_945 = tmp_struct_9.field3;
            CursorCursorCursorCursorProd tmp_struct_10 =
                                          add1Tree(end_r_340, pvrtmp_942, pvrtmp_945, pvrtmp_943);
            CursorTy pvrtmp_950 = tmp_struct_10.field0;
            CursorTy pvrtmp_951 = tmp_struct_10.field1;
            CursorTy pvrtmp_952 = tmp_struct_10.field2;
            CursorTy pvrtmp_953 = tmp_struct_10.field3;
            
            *(TagTyPacked *) loc_339 = 1;
            
            CursorTy writetag_605 = loc_339 + 1;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_950, pvrtmp_951,
                                                   loc_339, pvrtmp_953};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_962 = *(CursorTy *) tmpcur_935;
            CursorTy tmpaftercur_963 = tmpcur_935 + 8;
            CursorTy jump_523 = tmpcur_935 + 8;
            CursorCursorCursorCursorProd tmp_struct_11 =
                                          add1Tree(end_r_340, end_r_341, loc_339, tmpcur_962);
            CursorTy pvrtmp_964 = tmp_struct_11.field0;
            CursorTy pvrtmp_965 = tmp_struct_11.field1;
            CursorTy pvrtmp_966 = tmp_struct_11.field2;
            CursorTy pvrtmp_967 = tmp_struct_11.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_964, jump_523,
                                                   pvrtmp_966, pvrtmp_967};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_974 = *(CursorTy *) tmpcur_935;
            CursorTy tmpaftercur_975 = tmpcur_935 + 8;
            CursorCursorCursorCursorProd tmp_struct_12 =
                                          add1Tree(end_r_340, end_r_341, loc_339, tmpcur_974);
            CursorTy pvrtmp_976 = tmp_struct_12.field0;
            CursorTy pvrtmp_977 = tmp_struct_12.field1;
            CursorTy pvrtmp_978 = tmp_struct_12.field2;
            CursorTy pvrtmp_979 = tmp_struct_12.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_976, pvrtmp_977,
                                                   pvrtmp_978, pvrtmp_979};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_934");
            exit(1);
        }
    }
}
CursorCursorCursorCursorProd _copy_Tree(CursorTy end_r_344, CursorTy end_r_345,
                                        CursorTy loc_343,
                                        CursorTy arg_59_103_170)
{
    if (loc_343 + 32 > end_r_345) {
        ChunkTy new_chunk_20 = alloc_chunk(end_r_345);
        CursorTy chunk_start_21 = new_chunk_20.chunk_start;
        CursorTy chunk_end_22 = new_chunk_20.chunk_end;
        
        end_r_345 = chunk_end_22;
        *(TagTyPacked *) loc_343 = 255;
        
        CursorTy redir = loc_343 + 1;
        
        *(CursorTy *) redir = chunk_start_21;
        loc_343 = chunk_start_21;
    }
    
    TagTyPacked tmpval_987 = *(TagTyPacked *) arg_59_103_170;
    CursorTy tmpcur_988 = arg_59_103_170 + 1;
    
    
  switch_1039:
    ;
    switch (tmpval_987) {
        
      case 0:
        {
            IntTy tmpval_989 = *(IntTy *) tmpcur_988;
            CursorTy tmpcur_990 = tmpcur_988 + sizeof(IntTy);
            CursorTy jump_486 = tmpcur_988 + 8;
            
            *(TagTyPacked *) loc_343 = 0;
            
            CursorTy writetag_617 = loc_343 + 1;
            
            *(IntTy *) writetag_617 = tmpval_989;
            
            CursorTy writecur_618 = writetag_617 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {end_r_345, jump_486, loc_343,
                                                   writecur_618};
            break;
        }
        
      case 1:
        {
            CursorTy loc_416 = loc_343 + 1;
            CursorCursorCursorCursorProd tmp_struct_16 =
                                          _copy_Tree(end_r_344, end_r_345, loc_416, tmpcur_988);
            CursorTy pvrtmp_995 = tmp_struct_16.field0;
            CursorTy pvrtmp_996 = tmp_struct_16.field1;
            CursorTy pvrtmp_997 = tmp_struct_16.field2;
            CursorTy pvrtmp_998 = tmp_struct_16.field3;
            CursorCursorCursorCursorProd tmp_struct_17 =
                                          _copy_Tree(end_r_344, pvrtmp_995, pvrtmp_998, pvrtmp_996);
            CursorTy pvrtmp_1003 = tmp_struct_17.field0;
            CursorTy pvrtmp_1004 = tmp_struct_17.field1;
            CursorTy pvrtmp_1005 = tmp_struct_17.field2;
            CursorTy pvrtmp_1006 = tmp_struct_17.field3;
            
            *(TagTyPacked *) loc_343 = 1;
            
            CursorTy writetag_623 = loc_343 + 1;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1003, pvrtmp_1004,
                                                   loc_343, pvrtmp_1006};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1015 = *(CursorTy *) tmpcur_988;
            CursorTy tmpaftercur_1016 = tmpcur_988 + 8;
            CursorTy jump_529 = tmpcur_988 + 8;
            CursorCursorCursorCursorProd tmp_struct_18 =
                                          _copy_Tree(end_r_344, end_r_345, loc_343, tmpcur_1015);
            CursorTy pvrtmp_1017 = tmp_struct_18.field0;
            CursorTy pvrtmp_1018 = tmp_struct_18.field1;
            CursorTy pvrtmp_1019 = tmp_struct_18.field2;
            CursorTy pvrtmp_1020 = tmp_struct_18.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1017, jump_529,
                                                   pvrtmp_1019, pvrtmp_1020};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1027 = *(CursorTy *) tmpcur_988;
            CursorTy tmpaftercur_1028 = tmpcur_988 + 8;
            CursorCursorCursorCursorProd tmp_struct_19 =
                                          _copy_Tree(end_r_344, end_r_345, loc_343, tmpcur_1027);
            CursorTy pvrtmp_1029 = tmp_struct_19.field0;
            CursorTy pvrtmp_1030 = tmp_struct_19.field1;
            CursorTy pvrtmp_1031 = tmp_struct_19.field2;
            CursorTy pvrtmp_1032 = tmp_struct_19.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1029, pvrtmp_1030,
                                                   pvrtmp_1031, pvrtmp_1032};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_987");
            exit(1);
        }
    }
}
CursorCursorCursorCursorProd _copy_without_ptrs_Tree(CursorTy end_r_348,
                                                     CursorTy end_r_349,
                                                     CursorTy loc_347,
                                                     CursorTy arg_66_110_177)
{
    TagTyPacked tmpval_1040 = *(TagTyPacked *) arg_66_110_177;
    CursorTy tmpcur_1041 = arg_66_110_177 + 1;
    
    
  switch_1092:
    ;
    switch (tmpval_1040) {
        
      case 0:
        {
            IntTy tmpval_1042 = *(IntTy *) tmpcur_1041;
            CursorTy tmpcur_1043 = tmpcur_1041 + sizeof(IntTy);
            CursorTy jump_491 = tmpcur_1041 + 8;
            
            *(TagTyPacked *) loc_347 = 0;
            
            CursorTy writetag_635 = loc_347 + 1;
            
            *(IntTy *) writetag_635 = tmpval_1042;
            
            CursorTy writecur_636 = writetag_635 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {end_r_349, jump_491, loc_347,
                                                   writecur_636};
            break;
        }
        
      case 1:
        {
            CursorTy loc_434 = loc_347 + 1;
            CursorCursorCursorCursorProd tmp_struct_23 =
                                          _copy_without_ptrs_Tree(end_r_348, end_r_349, loc_434, tmpcur_1041);
            CursorTy pvrtmp_1048 = tmp_struct_23.field0;
            CursorTy pvrtmp_1049 = tmp_struct_23.field1;
            CursorTy pvrtmp_1050 = tmp_struct_23.field2;
            CursorTy pvrtmp_1051 = tmp_struct_23.field3;
            CursorCursorCursorCursorProd tmp_struct_24 =
                                          _copy_without_ptrs_Tree(end_r_348, pvrtmp_1048, pvrtmp_1051, pvrtmp_1049);
            CursorTy pvrtmp_1056 = tmp_struct_24.field0;
            CursorTy pvrtmp_1057 = tmp_struct_24.field1;
            CursorTy pvrtmp_1058 = tmp_struct_24.field2;
            CursorTy pvrtmp_1059 = tmp_struct_24.field3;
            
            *(TagTyPacked *) loc_347 = 1;
            
            CursorTy writetag_641 = loc_347 + 1;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1056, pvrtmp_1057,
                                                   loc_347, pvrtmp_1059};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1068 = *(CursorTy *) tmpcur_1041;
            CursorTy tmpaftercur_1069 = tmpcur_1041 + 8;
            CursorTy jump_535 = tmpcur_1041 + 8;
            CursorCursorCursorCursorProd tmp_struct_25 =
                                          _copy_without_ptrs_Tree(end_r_348, end_r_349, loc_347, tmpcur_1068);
            CursorTy pvrtmp_1070 = tmp_struct_25.field0;
            CursorTy pvrtmp_1071 = tmp_struct_25.field1;
            CursorTy pvrtmp_1072 = tmp_struct_25.field2;
            CursorTy pvrtmp_1073 = tmp_struct_25.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1070, jump_535,
                                                   pvrtmp_1072, pvrtmp_1073};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1080 = *(CursorTy *) tmpcur_1041;
            CursorTy tmpaftercur_1081 = tmpcur_1041 + 8;
            CursorCursorCursorCursorProd tmp_struct_26 =
                                          _copy_without_ptrs_Tree(end_r_348, end_r_349, loc_347, tmpcur_1080);
            CursorTy pvrtmp_1082 = tmp_struct_26.field0;
            CursorTy pvrtmp_1083 = tmp_struct_26.field1;
            CursorTy pvrtmp_1084 = tmp_struct_26.field2;
            CursorTy pvrtmp_1085 = tmp_struct_26.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1082, pvrtmp_1083,
                                                   pvrtmp_1084, pvrtmp_1085};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1040");
            exit(1);
        }
    }
}
CursorProd _traverse_Tree(CursorTy end_r_351, CursorTy arg_73_117_184)
{
    TagTyPacked tmpval_1093 = *(TagTyPacked *) arg_73_117_184;
    CursorTy tmpcur_1094 = arg_73_117_184 + 1;
    
    
  switch_1105:
    ;
    switch (tmpval_1093) {
        
      case 0:
        {
            IntTy tmpval_1095 = *(IntTy *) tmpcur_1094;
            CursorTy tmpcur_1096 = tmpcur_1094 + sizeof(IntTy);
            CursorTy jump_496 = tmpcur_1094 + 8;
            
            return (CursorProd) {jump_496};
            break;
        }
        
      case 1:
        {
            CursorProd tmp_struct_27 =  _traverse_Tree(end_r_351, tmpcur_1094);
            CursorTy pvrtmp_1097 = tmp_struct_27.field0;
            CursorProd tmp_struct_28 =  _traverse_Tree(end_r_351, pvrtmp_1097);
            CursorTy pvrtmp_1098 = tmp_struct_28.field0;
            
            return (CursorProd) {pvrtmp_1098};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1099 = *(CursorTy *) tmpcur_1094;
            CursorTy tmpaftercur_1100 = tmpcur_1094 + 8;
            CursorTy jump_541 = tmpcur_1094 + 8;
            CursorProd tmp_struct_29 =  _traverse_Tree(end_r_351, tmpcur_1099);
            CursorTy pvrtmp_1101 = tmp_struct_29.field0;
            
            return (CursorProd) {jump_541};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1102 = *(CursorTy *) tmpcur_1094;
            CursorTy tmpaftercur_1103 = tmpcur_1094 + 8;
            CursorProd tmp_struct_30 =  _traverse_Tree(end_r_351, tmpcur_1102);
            CursorTy pvrtmp_1104 = tmp_struct_30.field0;
            
            return (CursorProd) {pvrtmp_1104};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1093");
            exit(1);
        }
    }
}
CursorProd _print_Tree(CursorTy end_r_353, CursorTy arg_80_123_190)
{
    TagTyPacked tmpval_1106 = *(TagTyPacked *) arg_80_123_190;
    CursorTy tmpcur_1107 = arg_80_123_190 + 1;
    
    
  switch_1118:
    ;
    switch (tmpval_1106) {
        
      case 0:
        {
            IntTy tmpval_1108 = *(IntTy *) tmpcur_1107;
            CursorTy tmpcur_1109 = tmpcur_1107 + sizeof(IntTy);
            CursorTy jump_501 = tmpcur_1107 + 8;
            unsigned char wildcard_83_125_192 = print_symbol(870);
            unsigned char wildcard_85_126_193 = print_symbol(873);
            unsigned char y_82_127_194 = printf("%lld", tmpval_1108);
            unsigned char wildcard_84_128_195 = print_symbol(868);
            
            return (CursorProd) {jump_501};
            break;
        }
        
      case 1:
        {
            unsigned char wildcard_90_131_198 = print_symbol(869);
            unsigned char wildcard_93_132_199 = print_symbol(873);
            CursorProd tmp_struct_31 =  _print_Tree(end_r_353, tmpcur_1107);
            CursorTy pvrtmp_1110 = tmp_struct_31.field0;
            unsigned char wildcard_92_134_201 = print_symbol(873);
            CursorProd tmp_struct_32 =  _print_Tree(end_r_353, pvrtmp_1110);
            CursorTy pvrtmp_1111 = tmp_struct_32.field0;
            unsigned char wildcard_91_136_203 = print_symbol(868);
            
            return (CursorProd) {pvrtmp_1111};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1112 = *(CursorTy *) tmpcur_1107;
            CursorTy tmpaftercur_1113 = tmpcur_1107 + 8;
            CursorTy jump_547 = tmpcur_1107 + 8;
            unsigned char wildcard_550 = print_symbol(872);
            CursorProd tmp_struct_33 =  _print_Tree(end_r_353, tmpcur_1112);
            CursorTy pvrtmp_1114 = tmp_struct_33.field0;
            
            return (CursorProd) {jump_547};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1115 = *(CursorTy *) tmpcur_1107;
            CursorTy tmpaftercur_1116 = tmpcur_1107 + 8;
            unsigned char wildcard_550 = print_symbol(871);
            CursorProd tmp_struct_34 =  _print_Tree(end_r_353, tmpcur_1115);
            CursorTy pvrtmp_1117 = tmp_struct_34.field0;
            
            return (CursorProd) {pvrtmp_1117};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1106");
            exit(1);
        }
    }
}
CursorCursorCursorCursorProd _add_size_and_rel_offsets_Tree(CursorTy end_r_356,
                                                            CursorTy end_r_357,
                                                            CursorTy loc_355,
                                                            CursorTy arg_325)
{
    if (loc_355 + 32 > end_r_357) {
        ChunkTy new_chunk_39 = alloc_chunk(end_r_357);
        CursorTy chunk_start_40 = new_chunk_39.chunk_start;
        CursorTy chunk_end_41 = new_chunk_39.chunk_end;
        
        end_r_357 = chunk_end_41;
        *(TagTyPacked *) loc_355 = 255;
        
        CursorTy redir = loc_355 + 1;
        
        *(CursorTy *) redir = chunk_start_40;
        loc_355 = chunk_start_40;
    }
    
    TagTyPacked tmpval_1119 = *(TagTyPacked *) arg_325;
    CursorTy tmpcur_1120 = arg_325 + 1;
    
    
  switch_1171:
    ;
    switch (tmpval_1119) {
        
      case 0:
        {
            IntTy tmpval_1121 = *(IntTy *) tmpcur_1120;
            CursorTy tmpcur_1122 = tmpcur_1120 + sizeof(IntTy);
            CursorTy jump_506 = tmpcur_1120 + 8;
            
            *(TagTyPacked *) loc_355 = 0;
            
            CursorTy writetag_675 = loc_355 + 1;
            
            *(IntTy *) writetag_675 = tmpval_1121;
            
            CursorTy writecur_676 = writetag_675 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {end_r_357, jump_506, loc_355,
                                                   writecur_676};
            break;
        }
        
      case 1:
        {
            CursorTy loc_470 = loc_355 + 1;
            CursorCursorCursorCursorProd tmp_struct_35 =
                                          _add_size_and_rel_offsets_Tree(end_r_356, end_r_357, loc_470, tmpcur_1120);
            CursorTy pvrtmp_1127 = tmp_struct_35.field0;
            CursorTy pvrtmp_1128 = tmp_struct_35.field1;
            CursorTy pvrtmp_1129 = tmp_struct_35.field2;
            CursorTy pvrtmp_1130 = tmp_struct_35.field3;
            CursorCursorCursorCursorProd tmp_struct_36 =
                                          _add_size_and_rel_offsets_Tree(end_r_356, pvrtmp_1127, pvrtmp_1130, pvrtmp_1128);
            CursorTy pvrtmp_1135 = tmp_struct_36.field0;
            CursorTy pvrtmp_1136 = tmp_struct_36.field1;
            CursorTy pvrtmp_1137 = tmp_struct_36.field2;
            CursorTy pvrtmp_1138 = tmp_struct_36.field3;
            
            *(TagTyPacked *) loc_355 = 1;
            
            CursorTy writetag_681 = loc_355 + 1;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1135, pvrtmp_1136,
                                                   loc_355, pvrtmp_1138};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1147 = *(CursorTy *) tmpcur_1120;
            CursorTy tmpaftercur_1148 = tmpcur_1120 + 8;
            CursorTy jump_553 = tmpcur_1120 + 8;
            CursorCursorCursorCursorProd tmp_struct_37 =
                                          _add_size_and_rel_offsets_Tree(end_r_356, end_r_357, loc_355, tmpcur_1147);
            CursorTy pvrtmp_1149 = tmp_struct_37.field0;
            CursorTy pvrtmp_1150 = tmp_struct_37.field1;
            CursorTy pvrtmp_1151 = tmp_struct_37.field2;
            CursorTy pvrtmp_1152 = tmp_struct_37.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1149, jump_553,
                                                   pvrtmp_1151, pvrtmp_1152};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1159 = *(CursorTy *) tmpcur_1120;
            CursorTy tmpaftercur_1160 = tmpcur_1120 + 8;
            CursorCursorCursorCursorProd tmp_struct_38 =
                                          _add_size_and_rel_offsets_Tree(end_r_356, end_r_357, loc_355, tmpcur_1159);
            CursorTy pvrtmp_1161 = tmp_struct_38.field0;
            CursorTy pvrtmp_1162 = tmp_struct_38.field1;
            CursorTy pvrtmp_1163 = tmp_struct_38.field2;
            CursorTy pvrtmp_1164 = tmp_struct_38.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1161, pvrtmp_1162,
                                                   pvrtmp_1163, pvrtmp_1164};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1119");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(868, ")");
    add_symbol(869, "(Node");
    add_symbol(870, "(Leaf");
    add_symbol(871, " ->r ");
    add_symbol(872, " ->i ");
    add_symbol(873, " ");

    clock_t start, end;
    double cpu_time_used;

    
    RegionTy *region_874 = alloc_region(global_init_inf_buf_size);
    CursorTy r_367 = region_874->reg_heap;
    IntTy sizeof_end_r_367_875 = global_init_inf_buf_size;
    CursorTy end_r_367 = r_367 + sizeof_end_r_367_875;
    RegionTy *region_876 = alloc_region(global_init_inf_buf_size);
    CursorTy r_366 = region_876->reg_heap;
    IntTy sizeof_end_r_366_877 = global_init_inf_buf_size;
    CursorTy end_r_366 = r_366 + sizeof_end_r_366_877;
    CursorCursorCursorProd tmp_struct_42 =  mkTree(end_r_367, r_367, 25);
    CursorTy pvrtmp_878 = tmp_struct_42.field0;
    CursorTy pvrtmp_879 = tmp_struct_42.field1;
    CursorTy pvrtmp_880 = tmp_struct_42.field2;
    CursorCursorCursorCursorProd tmp_struct_43 =
                                  add1Tree(pvrtmp_878, end_r_366, r_366, pvrtmp_879);
    CursorTy pvrtmp_885 = tmp_struct_43.field0;
    CursorTy pvrtmp_886 = tmp_struct_43.field1;
    CursorTy pvrtmp_887 = tmp_struct_43.field2;
    CursorTy pvrtmp_888 = tmp_struct_43.field3;
    CursorInt64Prod tmp_struct_44;

    stackValue* stack = newStack();

    start = clock(); 
    sumTree(pvrtmp_885, pvrtmp_887, &tmp_struct_44, stack);
    end = clock(); 

    cpu_time_used = ((double) (end - start)) / CLOCKS_PER_SEC;

    printf("Execution time: %lf\n", cpu_time_used);

    CursorTy pvrtmp_893 = tmp_struct_44.field0;
    IntTy pvrtmp_894 = tmp_struct_44.field1;
    
    printf("%lld", pvrtmp_894);
    printf("\n");
    free_symtable();
    return 0;
}