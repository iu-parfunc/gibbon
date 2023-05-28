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

#include <emmintrin.h>
#include <immintrin.h>

#define KB 1024lu
#define MB (KB * 1000lu)
#define GB (MB * 1000lu)

#define REDIRECTION_TAG 255
#define INDIRECTION_TAG 254

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
CursorCursorCursorCursorProd add1(CursorTy end_r_545, CursorTy end_r_546,
                                  CursorTy loc_544, CursorTy lst_34_146_246);
CursorCursorCursorProd mkList(CursorTy end_r_548, CursorTy loc_547,
                              IntTy len_42_154_261);
CursorCursorCursorCursorProd _copy_List(CursorTy end_r_551, CursorTy end_r_552,
                                        CursorTy loc_550,
                                        CursorTy arg_67_155_272);
CursorCursorCursorCursorProd _copy_without_ptrs_List(CursorTy end_r_555,
                                                     CursorTy end_r_556,
                                                     CursorTy loc_554,
                                                     CursorTy arg_82_170_287);
CursorProd _traverse_List(CursorTy end_r_558, CursorTy arg_97_185_302);
CursorProd _print_List(CursorTy end_r_560, CursorTy arg_112_195_312);
CursorCursorCursorCursorProd _add_size_and_rel_offsets_List(CursorTy end_r_563,
                                                            CursorTy end_r_564,
                                                            CursorTy loc_562,
                                                            CursorTy arg_526);
CursorCursorCursorCursorProd add1(CursorTy end_r_545, CursorTy end_r_546,
                                  CursorTy loc_544, CursorTy lst_34_146_246)
{
    if (loc_544 + 33 > end_r_546) {
        ChunkTy new_chunk_4 = alloc_chunk(end_r_546);
        CursorTy chunk_start_5 = new_chunk_4.chunk_start;
        CursorTy chunk_end_6 = new_chunk_4.chunk_end;
        
        end_r_546 = chunk_end_6;
        *(TagTyPacked *) loc_544 = 255;
        
        CursorTy redir = loc_544 + 1;
        
        *(CursorTy *) redir = chunk_start_5;
        loc_544 = chunk_start_5;
    }
    
    CursorTy loc_587 = loc_544 + 1;
    CursorTy loc_588 = loc_587 + 8;
    CursorTy loc_589 = loc_588 + 8;
    CursorTy loc_590 = loc_589 + 8;
    CursorTy loc_591 = loc_590 + 8;
    TagTyPacked tmpval_1281 = *(TagTyPacked *) lst_34_146_246;
    CursorTy tmpcur_1282 = lst_34_146_246 + 1;
    
    
  switch_1345:
    ;
    switch (tmpval_1281) {
        
      case 1:
        {    
            
            //Load 64 bit long long into two __m128i vector registers. 
            //Limited by the architecture i am working on which only supports 128 bit vector registers. 
            __m128i one_two   = _mm_loadu_si128((__m128i const*)tmpcur_1282);
            __m128i two_three = _mm_loadu_si128((__m128i const*)(tmpcur_1282 + 2*sizeof(IntTy)));
            
            __m128i ones      = _mm_set_epi64x ((__int64_t)1, (__int64_t)1);
            
            __m128i one_two_1    = _mm_add_epi64 (one_two, ones);
            __m128i two_three_1  = _mm_add_epi64 (two_three, ones);
            
            //DEBUG: Print the mask vector
            //printf("Print the masked bit vector\n");
            //IntTy *res1 = (IntTy*) &one_two_1; 
            //for (int j=0; j < 2; j++){
            //   printf("Value is %lld\n", res1[2 - j - 1]);
            //}
            //printf("\n");
            ////////////////////////////////////////////////////////////////////////////////////////
            
            //DEBUG: Print the mask vector
            //printf("Print the masked bit vector\n");
            //IntTy *res2 = (IntTy*) &two_three_1; 
            //for (int j=0; j < 2; j++){
            //   printf("Value is %lld\n", res2[2 - j - 1]);
            //}
            //printf("\n");
            ////////////////////////////////////////////////////////////////////////////////////////
            
            //IntTy tmpval_1283 = *(IntTy *) tmpcur_1282;
            //CursorTy tmpcur_1284 = tmpcur_1282 + sizeof(IntTy);
            //IntTy tmpval_1285 = *(IntTy *) tmpcur_1284;
            //CursorTy tmpcur_1286 = tmpcur_1284 + sizeof(IntTy);
            //IntTy tmpval_1287 = *(IntTy *) tmpcur_1286;
            //CursorTy tmpcur_1288 = tmpcur_1286 + sizeof(IntTy);
            //IntTy tmpval_1289 = *(IntTy *) tmpcur_1288;
            //CursorTy tmpcur_1290 = tmpcur_1288 + sizeof(IntTy);
            //CursorTy jump_769 = tmpcur_1288 + 8;
            //CursorTy jump_768 = tmpcur_1286 + 8;
            //CursorTy jump_767 = tmpcur_1284 + 8;
            //CursorTy jump_766 = tmpcur_1282 + 8;
            //IntTy fltPkd_223_252 = tmpval_1283 + 1;
            //IntTy fltPkd_224_253 = tmpval_1285 + 1;
            //IntTy fltPkd_225_254 = tmpval_1287 + 1;
            //IntTy fltPkd_226_255 = tmpval_1289 + 1;
            CursorCursorCursorCursorProd tmp_struct_0 =
                                          add1(end_r_545, end_r_546, loc_591, (tmpcur_1282  + 4*sizeof(IntTy)) );
            CursorTy pvrtmp_1291 = tmp_struct_0.field0;
            CursorTy pvrtmp_1292 = tmp_struct_0.field1;
            CursorTy pvrtmp_1293 = tmp_struct_0.field2;
            CursorTy pvrtmp_1294 = tmp_struct_0.field3;
            
            *(TagTyPacked *) loc_544 = 1;
            
            CursorTy writetag_902 = loc_544 + 1;
            
            _mm_storeu_si128 ((__m128i*) writetag_902, one_two_1);
            
            _mm_storeu_si128 ((__m128i*) (writetag_902 + 2*sizeof(IntTy)), two_three_1);
            
            //*(IntTy *) writetag_902 = fltPkd_223_252;
            
            //CursorTy writecur_903 = writetag_902 + sizeof(IntTy);
            
            //*(IntTy *) writecur_903 = fltPkd_224_253;
            
            //CursorTy writecur_904 = writecur_903 + sizeof(IntTy);
            
            //*(IntTy *) writecur_904 = fltPkd_225_254;
            
            //CursorTy writecur_905 = writecur_904 + sizeof(IntTy);
            
            //*(IntTy *) writecur_905 = fltPkd_226_255;
            
            //CursorTy writecur_906 = writecur_905 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1291, pvrtmp_1292,
                                                   loc_544, pvrtmp_1294};
            break;
        }
        
      case 0:
        {
            IntTy tmpval_1303 = *(IntTy *) tmpcur_1282;
            CursorTy tmpcur_1304 = tmpcur_1282 + sizeof(IntTy);
            CursorTy jump_772 = tmpcur_1282 + 8;
            IntTy fltPkd_228_259 = tmpval_1303 + 1;
            CursorCursorCursorCursorProd tmp_struct_1 =
                                          add1(end_r_545, end_r_546, loc_588, tmpcur_1304);
            CursorTy pvrtmp_1305 = tmp_struct_1.field0;
            CursorTy pvrtmp_1306 = tmp_struct_1.field1;
            CursorTy pvrtmp_1307 = tmp_struct_1.field2;
            CursorTy pvrtmp_1308 = tmp_struct_1.field3;
            
            *(TagTyPacked *) loc_544 = 0;
            
            CursorTy writetag_912 = loc_544 + 1;
            
            *(IntTy *) writetag_912 = fltPkd_228_259;
            
            CursorTy writecur_913 = writetag_912 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1305, pvrtmp_1306,
                                                   loc_544, pvrtmp_1308};
            break;
        }
        
      case 2:
        {
            CursorTy jump_775 = lst_34_146_246 + 1;
            
            *(TagTyPacked *) loc_544 = 2;
            
            CursorTy writetag_917 = loc_544 + 1;
            
            return (CursorCursorCursorCursorProd) {end_r_546, jump_775, loc_544,
                                                   writetag_917};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1321 = *(CursorTy *) tmpcur_1282;
            CursorTy tmpaftercur_1322 = tmpcur_1282 + 8;
            CursorTy jump_842 = tmpcur_1282 + 8;
            CursorCursorCursorCursorProd tmp_struct_2 =
                                          add1(end_r_545, end_r_546, loc_544, tmpcur_1321);
            CursorTy pvrtmp_1323 = tmp_struct_2.field0;
            CursorTy pvrtmp_1324 = tmp_struct_2.field1;
            CursorTy pvrtmp_1325 = tmp_struct_2.field2;
            CursorTy pvrtmp_1326 = tmp_struct_2.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1323, jump_842,
                                                   pvrtmp_1325, pvrtmp_1326};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1333 = *(CursorTy *) tmpcur_1282;
            CursorTy tmpaftercur_1334 = tmpcur_1282 + 8;
            CursorCursorCursorCursorProd tmp_struct_3 =
                                          add1(end_r_545, end_r_546, loc_544, tmpcur_1333);
            CursorTy pvrtmp_1335 = tmp_struct_3.field0;
            CursorTy pvrtmp_1336 = tmp_struct_3.field1;
            CursorTy pvrtmp_1337 = tmp_struct_3.field2;
            CursorTy pvrtmp_1338 = tmp_struct_3.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1335, pvrtmp_1336,
                                                   pvrtmp_1337, pvrtmp_1338};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1281");
            exit(1);
        }
    }
}
CursorCursorCursorProd mkList(CursorTy end_r_548, CursorTy loc_547,
                              IntTy len_42_154_261)
{
    if (loc_547 + 33 > end_r_548) {
        ChunkTy new_chunk_9 = alloc_chunk(end_r_548);
        CursorTy chunk_start_10 = new_chunk_9.chunk_start;
        CursorTy chunk_end_11 = new_chunk_9.chunk_end;
        
        end_r_548 = chunk_end_11;
        *(TagTyPacked *) loc_547 = 255;
        
        CursorTy redir = loc_547 + 1;
        
        *(CursorTy *) redir = chunk_start_10;
        loc_547 = chunk_start_10;
    }
    
    CursorTy loc_614 = loc_547 + 1;
    CursorTy loc_615 = loc_614 + 8;
    CursorTy loc_616 = loc_615 + 8;
    CursorTy loc_617 = loc_616 + 8;
    CursorTy loc_618 = loc_617 + 8;
    BoolTy fltIf_230_262 = len_42_154_261 <= 0;
    
    if (fltIf_230_262) {
        *(TagTyPacked *) loc_547 = 2;
        
        CursorTy writetag_925 = loc_547 + 1;
        
        return (CursorCursorCursorProd) {end_r_548, loc_547, writetag_925};
    } else {
        IntTy fltPrm_232_263 = len_42_154_261 % 4;
        BoolTy fltIf_231_264 = fltPrm_232_263 == 0;
        
        if (fltIf_231_264) {
            IntTy fltPkd_233_265 = len_42_154_261 - 1;
            IntTy fltPkd_234_266 = len_42_154_261 - 2;
            IntTy fltPkd_235_267 = len_42_154_261 - 3;
            IntTy fltAppE_237_268 = len_42_154_261 - 4;
            CursorCursorCursorProd tmp_struct_7 =
                                    mkList(end_r_548, loc_618, fltAppE_237_268);
            CursorTy pvrtmp_1350 = tmp_struct_7.field0;
            CursorTy pvrtmp_1351 = tmp_struct_7.field1;
            CursorTy pvrtmp_1352 = tmp_struct_7.field2;
            
            *(TagTyPacked *) loc_547 = 1;
            
            CursorTy writetag_928 = loc_547 + 1;
            
            *(IntTy *) writetag_928 = len_42_154_261;
            
            CursorTy writecur_929 = writetag_928 + sizeof(IntTy);
            
            *(IntTy *) writecur_929 = fltPkd_233_265;
            
            CursorTy writecur_930 = writecur_929 + sizeof(IntTy);
            
            *(IntTy *) writecur_930 = fltPkd_234_266;
            
            CursorTy writecur_931 = writecur_930 + sizeof(IntTy);
            
            *(IntTy *) writecur_931 = fltPkd_235_267;
            
            CursorTy writecur_932 = writecur_931 + sizeof(IntTy);
            
            return (CursorCursorCursorProd) {pvrtmp_1350, loc_547, pvrtmp_1352};
        } else {
            IntTy fltAppE_239_270 = len_42_154_261 - 1;
            CursorCursorCursorProd tmp_struct_8 =
                                    mkList(end_r_548, loc_615, fltAppE_239_270);
            CursorTy pvrtmp_1361 = tmp_struct_8.field0;
            CursorTy pvrtmp_1362 = tmp_struct_8.field1;
            CursorTy pvrtmp_1363 = tmp_struct_8.field2;
            
            *(TagTyPacked *) loc_547 = 0;
            
            CursorTy writetag_936 = loc_547 + 1;
            
            *(IntTy *) writetag_936 = len_42_154_261;
            
            CursorTy writecur_937 = writetag_936 + sizeof(IntTy);
            
            return (CursorCursorCursorProd) {pvrtmp_1361, loc_547, pvrtmp_1363};
        }
    }
}
CursorCursorCursorCursorProd _copy_List(CursorTy end_r_551, CursorTy end_r_552,
                                        CursorTy loc_550,
                                        CursorTy arg_67_155_272)
{
    if (loc_550 + 33 > end_r_552) {
        ChunkTy new_chunk_16 = alloc_chunk(end_r_552);
        CursorTy chunk_start_17 = new_chunk_16.chunk_start;
        CursorTy chunk_end_18 = new_chunk_16.chunk_end;
        
        end_r_552 = chunk_end_18;
        *(TagTyPacked *) loc_550 = 255;
        
        CursorTy redir = loc_550 + 1;
        
        *(CursorTy *) redir = chunk_start_17;
        loc_550 = chunk_start_17;
    }
    
    CursorTy loc_641 = loc_550 + 1;
    CursorTy loc_642 = loc_641 + 8;
    CursorTy loc_656 = loc_642 + 8;
    CursorTy loc_657 = loc_656 + 8;
    CursorTy loc_658 = loc_657 + 8;
    TagTyPacked tmpval_1372 = *(TagTyPacked *) arg_67_155_272;
    CursorTy tmpcur_1373 = arg_67_155_272 + 1;
    
    
  switch_1436:
    ;
    switch (tmpval_1372) {
        
      case 0:
        {
            IntTy tmpval_1374 = *(IntTy *) tmpcur_1373;
            CursorTy tmpcur_1375 = tmpcur_1373 + sizeof(IntTy);
            CursorTy jump_780 = tmpcur_1373 + 8;
            CursorCursorCursorCursorProd tmp_struct_12 =
                                          _copy_List(end_r_551, end_r_552, loc_642, tmpcur_1375);
            CursorTy pvrtmp_1376 = tmp_struct_12.field0;
            CursorTy pvrtmp_1377 = tmp_struct_12.field1;
            CursorTy pvrtmp_1378 = tmp_struct_12.field2;
            CursorTy pvrtmp_1379 = tmp_struct_12.field3;
            
            *(TagTyPacked *) loc_550 = 0;
            
            CursorTy writetag_943 = loc_550 + 1;
            
            *(IntTy *) writetag_943 = tmpval_1374;
            
            CursorTy writecur_944 = writetag_943 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1376, pvrtmp_1377,
                                                   loc_550, pvrtmp_1379};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_1388 = *(IntTy *) tmpcur_1373;
            CursorTy tmpcur_1389 = tmpcur_1373 + sizeof(IntTy);
            IntTy tmpval_1390 = *(IntTy *) tmpcur_1389;
            CursorTy tmpcur_1391 = tmpcur_1389 + sizeof(IntTy);
            IntTy tmpval_1392 = *(IntTy *) tmpcur_1391;
            CursorTy tmpcur_1393 = tmpcur_1391 + sizeof(IntTy);
            IntTy tmpval_1394 = *(IntTy *) tmpcur_1393;
            CursorTy tmpcur_1395 = tmpcur_1393 + sizeof(IntTy);
            CursorTy jump_786 = tmpcur_1393 + 8;
            CursorTy jump_785 = tmpcur_1391 + 8;
            CursorTy jump_784 = tmpcur_1389 + 8;
            CursorTy jump_783 = tmpcur_1373 + 8;
            CursorCursorCursorCursorProd tmp_struct_13 =
                                          _copy_List(end_r_551, end_r_552, loc_658, tmpcur_1395);
            CursorTy pvrtmp_1396 = tmp_struct_13.field0;
            CursorTy pvrtmp_1397 = tmp_struct_13.field1;
            CursorTy pvrtmp_1398 = tmp_struct_13.field2;
            CursorTy pvrtmp_1399 = tmp_struct_13.field3;
            
            *(TagTyPacked *) loc_550 = 1;
            
            CursorTy writetag_953 = loc_550 + 1;
            
            *(IntTy *) writetag_953 = tmpval_1388;
            
            CursorTy writecur_954 = writetag_953 + sizeof(IntTy);
            
            *(IntTy *) writecur_954 = tmpval_1390;
            
            CursorTy writecur_955 = writecur_954 + sizeof(IntTy);
            
            *(IntTy *) writecur_955 = tmpval_1392;
            
            CursorTy writecur_956 = writecur_955 + sizeof(IntTy);
            
            *(IntTy *) writecur_956 = tmpval_1394;
            
            CursorTy writecur_957 = writecur_956 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1396, pvrtmp_1397,
                                                   loc_550, pvrtmp_1399};
            break;
        }
        
      case 2:
        {
            CursorTy jump_789 = arg_67_155_272 + 1;
            
            *(TagTyPacked *) loc_550 = 2;
            
            CursorTy writetag_961 = loc_550 + 1;
            
            return (CursorCursorCursorCursorProd) {end_r_552, jump_789, loc_550,
                                                   writetag_961};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1412 = *(CursorTy *) tmpcur_1373;
            CursorTy tmpaftercur_1413 = tmpcur_1373 + 8;
            CursorTy jump_848 = tmpcur_1373 + 8;
            CursorCursorCursorCursorProd tmp_struct_14 =
                                          _copy_List(end_r_551, end_r_552, loc_550, tmpcur_1412);
            CursorTy pvrtmp_1414 = tmp_struct_14.field0;
            CursorTy pvrtmp_1415 = tmp_struct_14.field1;
            CursorTy pvrtmp_1416 = tmp_struct_14.field2;
            CursorTy pvrtmp_1417 = tmp_struct_14.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1414, jump_848,
                                                   pvrtmp_1416, pvrtmp_1417};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1424 = *(CursorTy *) tmpcur_1373;
            CursorTy tmpaftercur_1425 = tmpcur_1373 + 8;
            CursorCursorCursorCursorProd tmp_struct_15 =
                                          _copy_List(end_r_551, end_r_552, loc_550, tmpcur_1424);
            CursorTy pvrtmp_1426 = tmp_struct_15.field0;
            CursorTy pvrtmp_1427 = tmp_struct_15.field1;
            CursorTy pvrtmp_1428 = tmp_struct_15.field2;
            CursorTy pvrtmp_1429 = tmp_struct_15.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1426, pvrtmp_1427,
                                                   pvrtmp_1428, pvrtmp_1429};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1372");
            exit(1);
        }
    }
}
CursorCursorCursorCursorProd _copy_without_ptrs_List(CursorTy end_r_555,
                                                     CursorTy end_r_556,
                                                     CursorTy loc_554,
                                                     CursorTy arg_82_170_287)
{
    CursorTy loc_676 = loc_554 + 1;
    CursorTy loc_677 = loc_676 + 8;
    CursorTy loc_691 = loc_677 + 8;
    CursorTy loc_692 = loc_691 + 8;
    CursorTy loc_693 = loc_692 + 8;
    TagTyPacked tmpval_1437 = *(TagTyPacked *) arg_82_170_287;
    CursorTy tmpcur_1438 = arg_82_170_287 + 1;
    
    
  switch_1501:
    ;
    switch (tmpval_1437) {
        
      case 0:
        {
            IntTy tmpval_1439 = *(IntTy *) tmpcur_1438;
            CursorTy tmpcur_1440 = tmpcur_1438 + sizeof(IntTy);
            CursorTy jump_791 = tmpcur_1438 + 8;
            CursorCursorCursorCursorProd tmp_struct_19 =
                                          _copy_without_ptrs_List(end_r_555, end_r_556, loc_677, tmpcur_1440);
            CursorTy pvrtmp_1441 = tmp_struct_19.field0;
            CursorTy pvrtmp_1442 = tmp_struct_19.field1;
            CursorTy pvrtmp_1443 = tmp_struct_19.field2;
            CursorTy pvrtmp_1444 = tmp_struct_19.field3;
            
            *(TagTyPacked *) loc_554 = 0;
            
            CursorTy writetag_972 = loc_554 + 1;
            
            *(IntTy *) writetag_972 = tmpval_1439;
            
            CursorTy writecur_973 = writetag_972 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1441, pvrtmp_1442,
                                                   loc_554, pvrtmp_1444};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_1453 = *(IntTy *) tmpcur_1438;
            CursorTy tmpcur_1454 = tmpcur_1438 + sizeof(IntTy);
            IntTy tmpval_1455 = *(IntTy *) tmpcur_1454;
            CursorTy tmpcur_1456 = tmpcur_1454 + sizeof(IntTy);
            IntTy tmpval_1457 = *(IntTy *) tmpcur_1456;
            CursorTy tmpcur_1458 = tmpcur_1456 + sizeof(IntTy);
            IntTy tmpval_1459 = *(IntTy *) tmpcur_1458;
            CursorTy tmpcur_1460 = tmpcur_1458 + sizeof(IntTy);
            CursorTy jump_797 = tmpcur_1458 + 8;
            CursorTy jump_796 = tmpcur_1456 + 8;
            CursorTy jump_795 = tmpcur_1454 + 8;
            CursorTy jump_794 = tmpcur_1438 + 8;
            CursorCursorCursorCursorProd tmp_struct_20 =
                                          _copy_without_ptrs_List(end_r_555, end_r_556, loc_693, tmpcur_1460);
            CursorTy pvrtmp_1461 = tmp_struct_20.field0;
            CursorTy pvrtmp_1462 = tmp_struct_20.field1;
            CursorTy pvrtmp_1463 = tmp_struct_20.field2;
            CursorTy pvrtmp_1464 = tmp_struct_20.field3;
            
            *(TagTyPacked *) loc_554 = 1;
            
            CursorTy writetag_982 = loc_554 + 1;
            
            *(IntTy *) writetag_982 = tmpval_1453;
            
            CursorTy writecur_983 = writetag_982 + sizeof(IntTy);
            
            *(IntTy *) writecur_983 = tmpval_1455;
            
            CursorTy writecur_984 = writecur_983 + sizeof(IntTy);
            
            *(IntTy *) writecur_984 = tmpval_1457;
            
            CursorTy writecur_985 = writecur_984 + sizeof(IntTy);
            
            *(IntTy *) writecur_985 = tmpval_1459;
            
            CursorTy writecur_986 = writecur_985 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1461, pvrtmp_1462,
                                                   loc_554, pvrtmp_1464};
            break;
        }
        
      case 2:
        {
            CursorTy jump_800 = arg_82_170_287 + 1;
            
            *(TagTyPacked *) loc_554 = 2;
            
            CursorTy writetag_990 = loc_554 + 1;
            
            return (CursorCursorCursorCursorProd) {end_r_556, jump_800, loc_554,
                                                   writetag_990};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1477 = *(CursorTy *) tmpcur_1438;
            CursorTy tmpaftercur_1478 = tmpcur_1438 + 8;
            CursorTy jump_854 = tmpcur_1438 + 8;
            CursorCursorCursorCursorProd tmp_struct_21 =
                                          _copy_without_ptrs_List(end_r_555, end_r_556, loc_554, tmpcur_1477);
            CursorTy pvrtmp_1479 = tmp_struct_21.field0;
            CursorTy pvrtmp_1480 = tmp_struct_21.field1;
            CursorTy pvrtmp_1481 = tmp_struct_21.field2;
            CursorTy pvrtmp_1482 = tmp_struct_21.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1479, jump_854,
                                                   pvrtmp_1481, pvrtmp_1482};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1489 = *(CursorTy *) tmpcur_1438;
            CursorTy tmpaftercur_1490 = tmpcur_1438 + 8;
            CursorCursorCursorCursorProd tmp_struct_22 =
                                          _copy_without_ptrs_List(end_r_555, end_r_556, loc_554, tmpcur_1489);
            CursorTy pvrtmp_1491 = tmp_struct_22.field0;
            CursorTy pvrtmp_1492 = tmp_struct_22.field1;
            CursorTy pvrtmp_1493 = tmp_struct_22.field2;
            CursorTy pvrtmp_1494 = tmp_struct_22.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1491, pvrtmp_1492,
                                                   pvrtmp_1493, pvrtmp_1494};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1437");
            exit(1);
        }
    }
}
CursorProd _traverse_List(CursorTy end_r_558, CursorTy arg_97_185_302)
{
    TagTyPacked tmpval_1502 = *(TagTyPacked *) arg_97_185_302;
    CursorTy tmpcur_1503 = arg_97_185_302 + 1;
    
    
  switch_1522:
    ;
    switch (tmpval_1502) {
        
      case 0:
        {
            IntTy tmpval_1504 = *(IntTy *) tmpcur_1503;
            CursorTy tmpcur_1505 = tmpcur_1503 + sizeof(IntTy);
            CursorTy jump_802 = tmpcur_1503 + 8;
            CursorProd tmp_struct_23 =  _traverse_List(end_r_558, tmpcur_1505);
            CursorTy pvrtmp_1506 = tmp_struct_23.field0;
            
            return (CursorProd) {pvrtmp_1506};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_1507 = *(IntTy *) tmpcur_1503;
            CursorTy tmpcur_1508 = tmpcur_1503 + sizeof(IntTy);
            IntTy tmpval_1509 = *(IntTy *) tmpcur_1508;
            CursorTy tmpcur_1510 = tmpcur_1508 + sizeof(IntTy);
            IntTy tmpval_1511 = *(IntTy *) tmpcur_1510;
            CursorTy tmpcur_1512 = tmpcur_1510 + sizeof(IntTy);
            IntTy tmpval_1513 = *(IntTy *) tmpcur_1512;
            CursorTy tmpcur_1514 = tmpcur_1512 + sizeof(IntTy);
            CursorTy jump_808 = tmpcur_1512 + 8;
            CursorTy jump_807 = tmpcur_1510 + 8;
            CursorTy jump_806 = tmpcur_1508 + 8;
            CursorTy jump_805 = tmpcur_1503 + 8;
            CursorProd tmp_struct_24 =  _traverse_List(end_r_558, tmpcur_1514);
            CursorTy pvrtmp_1515 = tmp_struct_24.field0;
            
            return (CursorProd) {pvrtmp_1515};
            break;
        }
        
      case 2:
        {
            CursorTy jump_811 = arg_97_185_302 + 1;
            
            return (CursorProd) {jump_811};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1516 = *(CursorTy *) tmpcur_1503;
            CursorTy tmpaftercur_1517 = tmpcur_1503 + 8;
            CursorTy jump_860 = tmpcur_1503 + 8;
            CursorProd tmp_struct_25 =  _traverse_List(end_r_558, tmpcur_1516);
            CursorTy pvrtmp_1518 = tmp_struct_25.field0;
            
            return (CursorProd) {jump_860};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1519 = *(CursorTy *) tmpcur_1503;
            CursorTy tmpaftercur_1520 = tmpcur_1503 + 8;
            CursorProd tmp_struct_26 =  _traverse_List(end_r_558, tmpcur_1519);
            CursorTy pvrtmp_1521 = tmp_struct_26.field0;
            
            return (CursorProd) {pvrtmp_1521};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1502");
            exit(1);
        }
    }
}
CursorProd _print_List(CursorTy end_r_560, CursorTy arg_112_195_312)
{
    TagTyPacked tmpval_1523 = *(TagTyPacked *) arg_112_195_312;
    CursorTy tmpcur_1524 = arg_112_195_312 + 1;
    
    
  switch_1543:
    ;
    switch (tmpval_1523) {
        
      case 0:
        {
            IntTy tmpval_1525 = *(IntTy *) tmpcur_1524;
            CursorTy tmpcur_1526 = tmpcur_1524 + sizeof(IntTy);
            CursorTy jump_813 = tmpcur_1524 + 8;
            unsigned char wildcard_117_198_315 = print_symbol(1255);
            unsigned char wildcard_120_199_316 = print_symbol(1258);
            unsigned char y_115_200_317 = printf("%lld", tmpval_1525);
            unsigned char wildcard_119_201_318 = print_symbol(1258);
            CursorProd tmp_struct_27 =  _print_List(end_r_560, tmpcur_1526);
            CursorTy pvrtmp_1527 = tmp_struct_27.field0;
            unsigned char wildcard_118_203_320 = print_symbol(1252);
            
            return (CursorProd) {pvrtmp_1527};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_1528 = *(IntTy *) tmpcur_1524;
            CursorTy tmpcur_1529 = tmpcur_1524 + sizeof(IntTy);
            IntTy tmpval_1530 = *(IntTy *) tmpcur_1529;
            CursorTy tmpcur_1531 = tmpcur_1529 + sizeof(IntTy);
            IntTy tmpval_1532 = *(IntTy *) tmpcur_1531;
            CursorTy tmpcur_1533 = tmpcur_1531 + sizeof(IntTy);
            IntTy tmpval_1534 = *(IntTy *) tmpcur_1533;
            CursorTy tmpcur_1535 = tmpcur_1533 + sizeof(IntTy);
            CursorTy jump_819 = tmpcur_1533 + 8;
            CursorTy jump_818 = tmpcur_1531 + 8;
            CursorTy jump_817 = tmpcur_1529 + 8;
            CursorTy jump_816 = tmpcur_1524 + 8;
            unsigned char wildcard_131_209_326 = print_symbol(1254);
            unsigned char wildcard_137_210_327 = print_symbol(1258);
            unsigned char y_126_211_328 = printf("%lld", tmpval_1528);
            unsigned char wildcard_136_212_329 = print_symbol(1258);
            unsigned char y_127_213_330 = printf("%lld", tmpval_1530);
            unsigned char wildcard_135_214_331 = print_symbol(1258);
            unsigned char y_128_215_332 = printf("%lld", tmpval_1532);
            unsigned char wildcard_134_216_333 = print_symbol(1258);
            unsigned char y_129_217_334 = printf("%lld", tmpval_1534);
            unsigned char wildcard_133_218_335 = print_symbol(1258);
            CursorProd tmp_struct_28 =  _print_List(end_r_560, tmpcur_1535);
            CursorTy pvrtmp_1536 = tmp_struct_28.field0;
            unsigned char wildcard_132_220_337 = print_symbol(1252);
            
            return (CursorProd) {pvrtmp_1536};
            break;
        }
        
      case 2:
        {
            CursorTy jump_822 = arg_112_195_312 + 1;
            unsigned char wildcard_138_221_338 = print_symbol(1253);
            unsigned char wildcard_139_222_339 = print_symbol(1252);
            
            return (CursorProd) {jump_822};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1537 = *(CursorTy *) tmpcur_1524;
            CursorTy tmpaftercur_1538 = tmpcur_1524 + 8;
            CursorTy jump_866 = tmpcur_1524 + 8;
            unsigned char wildcard_869 = print_symbol(1257);
            CursorProd tmp_struct_29 =  _print_List(end_r_560, tmpcur_1537);
            CursorTy pvrtmp_1539 = tmp_struct_29.field0;
            
            return (CursorProd) {jump_866};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1540 = *(CursorTy *) tmpcur_1524;
            CursorTy tmpaftercur_1541 = tmpcur_1524 + 8;
            unsigned char wildcard_869 = print_symbol(1256);
            CursorProd tmp_struct_30 =  _print_List(end_r_560, tmpcur_1540);
            CursorTy pvrtmp_1542 = tmp_struct_30.field0;
            
            return (CursorProd) {pvrtmp_1542};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1523");
            exit(1);
        }
    }
}
CursorCursorCursorCursorProd _add_size_and_rel_offsets_List(CursorTy end_r_563,
                                                            CursorTy end_r_564,
                                                            CursorTy loc_562,
                                                            CursorTy arg_526)
{
    if (loc_562 + 33 > end_r_564) {
        ChunkTy new_chunk_35 = alloc_chunk(end_r_564);
        CursorTy chunk_start_36 = new_chunk_35.chunk_start;
        CursorTy chunk_end_37 = new_chunk_35.chunk_end;
        
        end_r_564 = chunk_end_37;
        *(TagTyPacked *) loc_562 = 255;
        
        CursorTy redir = loc_562 + 1;
        
        *(CursorTy *) redir = chunk_start_36;
        loc_562 = chunk_start_36;
    }
    
    CursorTy loc_737 = loc_562 + 1;
    CursorTy loc_738 = loc_737 + 8;
    CursorTy loc_752 = loc_738 + 8;
    CursorTy loc_753 = loc_752 + 8;
    CursorTy loc_754 = loc_753 + 8;
    TagTyPacked tmpval_1544 = *(TagTyPacked *) arg_526;
    CursorTy tmpcur_1545 = arg_526 + 1;
    
    
  switch_1608:
    ;
    switch (tmpval_1544) {
        
      case 0:
        {
            IntTy tmpval_1546 = *(IntTy *) tmpcur_1545;
            CursorTy tmpcur_1547 = tmpcur_1545 + sizeof(IntTy);
            CursorTy jump_824 = tmpcur_1545 + 8;
            CursorCursorCursorCursorProd tmp_struct_31 =
                                          _add_size_and_rel_offsets_List(end_r_563, end_r_564, loc_738, tmpcur_1547);
            CursorTy pvrtmp_1548 = tmp_struct_31.field0;
            CursorTy pvrtmp_1549 = tmp_struct_31.field1;
            CursorTy pvrtmp_1550 = tmp_struct_31.field2;
            CursorTy pvrtmp_1551 = tmp_struct_31.field3;
            
            *(TagTyPacked *) loc_562 = 0;
            
            CursorTy writetag_1033 = loc_562 + 1;
            
            *(IntTy *) writetag_1033 = tmpval_1546;
            
            CursorTy writecur_1034 = writetag_1033 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1548, pvrtmp_1549,
                                                   loc_562, pvrtmp_1551};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_1560 = *(IntTy *) tmpcur_1545;
            CursorTy tmpcur_1561 = tmpcur_1545 + sizeof(IntTy);
            IntTy tmpval_1562 = *(IntTy *) tmpcur_1561;
            CursorTy tmpcur_1563 = tmpcur_1561 + sizeof(IntTy);
            IntTy tmpval_1564 = *(IntTy *) tmpcur_1563;
            CursorTy tmpcur_1565 = tmpcur_1563 + sizeof(IntTy);
            IntTy tmpval_1566 = *(IntTy *) tmpcur_1565;
            CursorTy tmpcur_1567 = tmpcur_1565 + sizeof(IntTy);
            CursorTy jump_830 = tmpcur_1565 + 8;
            CursorTy jump_829 = tmpcur_1563 + 8;
            CursorTy jump_828 = tmpcur_1561 + 8;
            CursorTy jump_827 = tmpcur_1545 + 8;
            CursorCursorCursorCursorProd tmp_struct_32 =
                                          _add_size_and_rel_offsets_List(end_r_563, end_r_564, loc_754, tmpcur_1567);
            CursorTy pvrtmp_1568 = tmp_struct_32.field0;
            CursorTy pvrtmp_1569 = tmp_struct_32.field1;
            CursorTy pvrtmp_1570 = tmp_struct_32.field2;
            CursorTy pvrtmp_1571 = tmp_struct_32.field3;
            
            *(TagTyPacked *) loc_562 = 1;
            
            CursorTy writetag_1043 = loc_562 + 1;
            
            *(IntTy *) writetag_1043 = tmpval_1560;
            
            CursorTy writecur_1044 = writetag_1043 + sizeof(IntTy);
            
            *(IntTy *) writecur_1044 = tmpval_1562;
            
            CursorTy writecur_1045 = writecur_1044 + sizeof(IntTy);
            
            *(IntTy *) writecur_1045 = tmpval_1564;
            
            CursorTy writecur_1046 = writecur_1045 + sizeof(IntTy);
            
            *(IntTy *) writecur_1046 = tmpval_1566;
            
            CursorTy writecur_1047 = writecur_1046 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1568, pvrtmp_1569,
                                                   loc_562, pvrtmp_1571};
            break;
        }
        
      case 2:
        {
            CursorTy jump_833 = arg_526 + 1;
            
            *(TagTyPacked *) loc_562 = 2;
            
            CursorTy writetag_1051 = loc_562 + 1;
            
            return (CursorCursorCursorCursorProd) {end_r_564, jump_833, loc_562,
                                                   writetag_1051};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_1584 = *(CursorTy *) tmpcur_1545;
            CursorTy tmpaftercur_1585 = tmpcur_1545 + 8;
            CursorTy jump_872 = tmpcur_1545 + 8;
            CursorCursorCursorCursorProd tmp_struct_33 =
                                          _add_size_and_rel_offsets_List(end_r_563, end_r_564, loc_562, tmpcur_1584);
            CursorTy pvrtmp_1586 = tmp_struct_33.field0;
            CursorTy pvrtmp_1587 = tmp_struct_33.field1;
            CursorTy pvrtmp_1588 = tmp_struct_33.field2;
            CursorTy pvrtmp_1589 = tmp_struct_33.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1586, jump_872,
                                                   pvrtmp_1588, pvrtmp_1589};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_1596 = *(CursorTy *) tmpcur_1545;
            CursorTy tmpaftercur_1597 = tmpcur_1545 + 8;
            CursorCursorCursorCursorProd tmp_struct_34 =
                                          _add_size_and_rel_offsets_List(end_r_563, end_r_564, loc_562, tmpcur_1596);
            CursorTy pvrtmp_1598 = tmp_struct_34.field0;
            CursorTy pvrtmp_1599 = tmp_struct_34.field1;
            CursorTy pvrtmp_1600 = tmp_struct_34.field2;
            CursorTy pvrtmp_1601 = tmp_struct_34.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_1598, pvrtmp_1599,
                                                   pvrtmp_1600, pvrtmp_1601};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1544");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1252, ")");
    add_symbol(1253, "(Nil");
    add_symbol(1254, "(Cons4");
    add_symbol(1255, "(Cons");
    add_symbol(1256, " ->r ");
    add_symbol(1257, " ->i ");
    add_symbol(1258, " ");
    add_symbol(1259, "\n");
    
    RegionTy *region_1260 = alloc_region(global_init_inf_buf_size);
    CursorTy r_576 = region_1260->reg_heap;
    IntTy sizeof_end_r_576_1261 = global_init_inf_buf_size;
    CursorTy end_r_576 = r_576 + sizeof_end_r_576_1261;
    RegionTy *region_1262 = alloc_region(global_init_inf_buf_size);
    CursorTy r_575 = region_1262->reg_heap;
    IntTy sizeof_end_r_575_1263 = global_init_inf_buf_size;
    CursorTy end_r_575 = r_575 + sizeof_end_r_575_1263;
    CursorCursorCursorProd tmp_struct_38 =  mkList(end_r_576, r_576, 1000000);
    CursorTy pvrtmp_1264 = tmp_struct_38.field0;
    CursorTy pvrtmp_1265 = tmp_struct_38.field1;
    CursorTy pvrtmp_1266 = tmp_struct_38.field2;
    //CursorProd tmp_struct_39 =  _print_List(pvrtmp_1264, pvrtmp_1265);
    //CursorTy pvrtmp_1271 = tmp_struct_39.field0;
    unsigned char wildcard__21_30_142_242 = print_symbol(1259);
    CursorCursorCursorCursorProd tmp_struct_40 =
                                  add1(pvrtmp_1264, end_r_575, r_575, pvrtmp_1265);
    CursorTy pvrtmp_1272 = tmp_struct_40.field0;
    CursorTy pvrtmp_1273 = tmp_struct_40.field1;
    CursorTy pvrtmp_1274 = tmp_struct_40.field2;
    CursorTy pvrtmp_1275 = tmp_struct_40.field3;
    //CursorProd tmp_struct_41 =  _print_List(pvrtmp_1272, pvrtmp_1274);
    //CursorTy pvrtmp_1280 = tmp_struct_41.field0;
    unsigned char wildcard__15_33_145_245 = print_symbol(1259);
    
    printf("'#()");
    printf("\n");
    free_symtable();
    return 0;
}
