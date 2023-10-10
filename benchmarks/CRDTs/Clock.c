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
typedef struct Int64Int64Prod_struct {
            IntTy field0;
            IntTy field1;
        } Int64Int64Prod;
typedef struct Int64Int64Int64Prod_struct {
            IntTy field0;
            IntTy field1;
            IntTy field2;
        } Int64Int64Int64Prod;
typedef struct Int64Int64Int64Int64CursorCursorProd_struct {
            IntTy field0;
            IntTy field1;
            IntTy field2;
            IntTy field3;
            CursorTy field4;
            CursorTy field5;
        } Int64Int64Int64Int64CursorCursorProd;
typedef struct Int64Int64Int64CursorCursorProd_struct {
            IntTy field0;
            IntTy field1;
            IntTy field2;
            CursorTy field3;
            CursorTy field4;
        } Int64Int64Int64CursorCursorProd;
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
CursorTy init(IntTy uid_12_685_948);
CursorTy stamp(IntTy uid_13_686_949, CursorTy clk_14_687_950);
CursorTy step(IntTy uid_16_689_953, CursorTy clk_17_690_954);
IntTy ratio();
IntTy delta();
IntTy size_273(CursorTy m_128_692_958);
CursorTy singleL_280(IntTy k1_84_698_964, IntTy x1_85_699_965,
                     CursorTy t1_86_700_966, CursorTy m_87_701_967);
CursorTy doubleL_281(IntTy k1_55_707_974, IntTy x1_56_708_975,
                     CursorTy t1_57_709_976, CursorTy m0_58_710_977);
CursorTy rotateL_274(IntTy k_109_716_983, IntTy x_110_717_984,
                     CursorTy l_111_718_985, CursorTy r_112_719_986);
CursorTy bin_279(IntTy k_94_725_997, IntTy x_95_726_998, CursorTy l_96_727_999,
                 CursorTy r_97_728_1000);
CursorTy singleR_276(IntTy k1_74_729_1005, IntTy x1_75_730_1006,
                     CursorTy m_76_731_1007, CursorTy t3_77_732_1008);
CursorTy doubleR_277(IntTy k1_40_738_1015, IntTy x1_41_739_1016,
                     CursorTy m0_42_740_1017, CursorTy t4_43_741_1018);
CursorTy empty_278();
CursorTy rotateR_275(IntTy k_99_747_1024, IntTy x_100_748_1025,
                     CursorTy l_101_749_1026, CursorTy r_102_750_1027);
CursorTy balance_272(IntTy k_119_756_1038, IntTy x_120_757_1039,
                     CursorTy l_121_758_1040, CursorTy r_122_759_1041);
CursorTy insert_271(IntTy kx_30_760_1062, IntTy x_31_761_1063,
                    CursorTy m_32_762_1064);
CursorTy lookup_270(IntTy k_22_768_1074, CursorTy m_23_769_1075);
CursorTy singleton_268(IntTy k_19_775_1083, IntTy x_20_776_1084);
CursorTy _copy_without_ptrs_Timestamp(CursorTy arg_580_777_1087);
CursorTy _copy_Timestamp(CursorTy arg_575_782_1092);
unsigned char _traverse_Timestamp(CursorTy arg_585_787_1097);
unsigned char _print_Timestamp(CursorTy arg_590_790_1100);
CursorTy _copy_Maybe_v_269(CursorTy arg_599_799_1109);
CursorTy _copy_without_ptrs_Maybe_v_269(CursorTy arg_602_802_1112);
unsigned char _traverse_Maybe_v_269(CursorTy arg_605_805_1115);
unsigned char _print_Maybe_v_269(CursorTy arg_608_807_1117);
CursorTy _copy_Map_v_267(CursorTy arg_616_815_1125);
CursorTy _copy_without_ptrs_Map_v_267(CursorTy arg_627_826_1136);
unsigned char _traverse_Map_v_267(CursorTy arg_638_837_1147);
unsigned char _print_Map_v_267(CursorTy arg_649_845_1155);
CursorTy caseFn_669(IntTy x1_56_670_865_1175, IntTy k1_55_671_866_1176,
                    CursorTy t1_57_672_867_1177, CursorTy m1_63_673_868_1178,
                    IntTy k2_61_674_869_1179, IntTy x2_62_675_870_1180,
                    CursorTy t4_64_676_871_1181);
CursorTy caseFn_677(IntTy x1_41_678_877_1189, IntTy k1_40_679_878_1190,
                    CursorTy t4_43_680_879_1191, CursorTy m1_49_681_880_1192,
                    IntTy k2_46_682_881_1193, IntTy x2_47_683_882_1194,
                    CursorTy t1_48_684_883_1195);
CursorTy init(IntTy uid_12_685_948)
{
    return singleton_268(uid_12_685_948, 0);
}
CursorTy stamp(IntTy uid_13_686_949, CursorTy clk_14_687_950)
{
    CursorTy fltCse_893_951 =  lookup_270(uid_13_686_949, clk_14_687_950);
    TagTyPacked tag_1211 = *(TagTyPacked *) fltCse_893_951;
    CursorTy tail_1212 = fltCse_893_951 + sizeof(IntTy);
    
    
  switch_1215:
    ;
    switch (tag_1211) {
        
      case 1:
        {
            IntTy v_15_688_952 = ((Int64Prod *) tail_1212)->field0;
            PtrTy tailift_1213 = ALLOC(sizeof(Int64Int64Int64Prod));
            
            ((Int64Int64Int64Prod *) tailift_1213)->field0 = 0;
            ((Int64Int64Int64Prod *) tailift_1213)->field1 = uid_13_686_949;
            ((Int64Int64Int64Prod *) tailift_1213)->field2 = v_15_688_952;
            return tailift_1213;
            break;
        }
        
      case 0:
        {
            PtrTy tailift_1214 = ALLOC(sizeof(Int64Int64Int64Prod));
            
            ((Int64Int64Int64Prod *) tailift_1214)->field0 = 0;
            ((Int64Int64Int64Prod *) tailift_1214)->field1 = uid_13_686_949;
            ((Int64Int64Int64Prod *) tailift_1214)->field2 = 0;
            return tailift_1214;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1211");
            exit(1);
        }
    }
}
CursorTy step(IntTy uid_16_689_953, CursorTy clk_17_690_954)
{
    CursorTy fltCse_894_955 =  lookup_270(uid_16_689_953, clk_17_690_954);
    TagTyPacked tag_1216 = *(TagTyPacked *) fltCse_894_955;
    CursorTy tail_1217 = fltCse_894_955 + sizeof(IntTy);
    
    
  switch_1218:
    ;
    switch (tag_1216) {
        
      case 1:
        {
            IntTy v_18_691_956 = ((Int64Prod *) tail_1217)->field0;
            IntTy fltAppE_895_957 = v_18_691_956 + 1;
            
            return insert_271(uid_16_689_953, fltAppE_895_957, clk_17_690_954);
            break;
        }
        
      case 0:
        {
            return insert_271(uid_16_689_953, 1, clk_17_690_954);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1216");
            exit(1);
        }
    }
}
IntTy ratio()
{
    return 2;
}
IntTy delta()
{
    return 4;
}
IntTy size_273(CursorTy m_128_692_958)
{
    TagTyPacked tag_1219 = *(TagTyPacked *) m_128_692_958;
    CursorTy tail_1220 = m_128_692_958 + sizeof(IntTy);
    
    
  switch_1221:
    ;
    switch (tag_1219) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy sz_130_693_959 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1220)->field0;
            IntTy wildcard__18_131_694_960 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1220)->field1;
            IntTy wildcard__19_132_695_961 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1220)->field2;
            CursorTy wildcard__20_133_696_962 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1220)->field3;
            CursorTy wildcard__21_134_697_963 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1220)->field4;
            
            return sz_130_693_959;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1219");
            exit(1);
        }
    }
}
CursorTy singleL_280(IntTy k1_84_698_964, IntTy x1_85_699_965,
                     CursorTy t1_86_700_966, CursorTy m_87_701_967)
{
    TagTyPacked tag_1222 = *(TagTyPacked *) m_87_701_967;
    CursorTy tail_1223 = m_87_701_967 + sizeof(IntTy);
    
    
  switch_1224:
    ;
    switch (tag_1222) {
        
      case 1:
        {
            IntTy wildcard__89_89_702_968 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1223)->field0;
            IntTy k2_90_703_969 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1223)->field1;
            IntTy x2_91_704_970 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1223)->field2;
            CursorTy t2_92_705_971 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1223)->field3;
            CursorTy t3_93_706_972 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1223)->field4;
            CursorTy fltAppE_896_973 =
                      bin_279(k1_84_698_964, x1_85_699_965, t1_86_700_966, t2_92_705_971);
            
            return bin_279(k2_90_703_969, x2_91_704_970, fltAppE_896_973,
                           t3_93_706_972);
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1222");
            exit(1);
        }
    }
}
CursorTy doubleL_281(IntTy k1_55_707_974, IntTy x1_56_708_975,
                     CursorTy t1_57_709_976, CursorTy m0_58_710_977)
{
    TagTyPacked tag_1225 = *(TagTyPacked *) m0_58_710_977;
    CursorTy tail_1226 = m0_58_710_977 + sizeof(IntTy);
    
    
  switch_1227:
    ;
    switch (tag_1225) {
        
      case 1:
        {
            IntTy wildcard__109_60_711_978 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1226)->field0;
            IntTy k2_61_712_979 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1226)->field1;
            IntTy x2_62_713_980 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1226)->field2;
            CursorTy m1_63_714_981 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1226)->field3;
            CursorTy t4_64_715_982 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1226)->field4;
            
            return caseFn_669(x1_56_708_975, k1_55_707_974, t1_57_709_976,
                              m1_63_714_981, k2_61_712_979, x2_62_713_980,
                              t4_64_715_982);
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1225");
            exit(1);
        }
    }
}
CursorTy rotateL_274(IntTy k_109_716_983, IntTy x_110_717_984,
                     CursorTy l_111_718_985, CursorTy r_112_719_986)
{
    TagTyPacked tag_1228 = *(TagTyPacked *) r_112_719_986;
    CursorTy tail_1229 = r_112_719_986 + sizeof(IntTy);
    
    
  switch_1230:
    ;
    switch (tag_1228) {
        
      case 1:
        {
            IntTy wildcard__60_114_720_987 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1229)->field0;
            IntTy wildcard__61_115_721_988 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1229)->field1;
            IntTy wildcard__62_116_722_989 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1229)->field2;
            CursorTy ly_117_723_990 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1229)->field3;
            CursorTy ry_118_724_991 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1229)->field4;
            IntTy fltPrm_898_992 =  size_273(ly_117_723_990);
            IntTy fltPrm_900_993 =  ratio();
            IntTy fltPrm_901_994 =  size_273(ry_118_724_991);
            IntTy fltPrm_899_995 = fltPrm_900_993 * fltPrm_901_994;
            BoolTy fltIf_897_996 = fltPrm_898_992 < fltPrm_899_995;
            
            if (fltIf_897_996) {
                return singleL_280(k_109_716_983, x_110_717_984, l_111_718_985,
                                   r_112_719_986);
            } else {
                return doubleL_281(k_109_716_983, x_110_717_984, l_111_718_985,
                                   r_112_719_986);
            }
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1228");
            exit(1);
        }
    }
}
CursorTy bin_279(IntTy k_94_725_997, IntTy x_95_726_998, CursorTy l_96_727_999,
                 CursorTy r_97_728_1000)
{
    IntTy fltPrm_904_1001 =  size_273(l_96_727_999);
    IntTy fltPrm_905_1002 =  size_273(r_97_728_1000);
    IntTy fltPrm_903_1003 = fltPrm_904_1001 + fltPrm_905_1002;
    IntTy fltPkd_902_1004 = fltPrm_903_1003 + 1;
    PtrTy tailift_1231 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field1 =
        fltPkd_902_1004;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field2 =
        k_94_725_997;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field3 =
        x_95_726_998;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field4 =
        l_96_727_999;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field5 =
        r_97_728_1000;
    return tailift_1231;
}
CursorTy singleR_276(IntTy k1_74_729_1005, IntTy x1_75_730_1006,
                     CursorTy m_76_731_1007, CursorTy t3_77_732_1008)
{
    TagTyPacked tag_1232 = *(TagTyPacked *) m_76_731_1007;
    CursorTy tail_1233 = m_76_731_1007 + sizeof(IntTy);
    
    
  switch_1234:
    ;
    switch (tag_1232) {
        
      case 1:
        {
            IntTy wildcard__99_79_733_1009 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1233)->field0;
            IntTy k2_80_734_1010 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1233)->field1;
            IntTy x2_81_735_1011 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1233)->field2;
            CursorTy t1_82_736_1012 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1233)->field3;
            CursorTy t2_83_737_1013 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1233)->field4;
            CursorTy fltAppE_906_1014 =
                      bin_279(k1_74_729_1005, x1_75_730_1006, t2_83_737_1013, t3_77_732_1008);
            
            return bin_279(k2_80_734_1010, x2_81_735_1011, t1_82_736_1012,
                           fltAppE_906_1014);
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1232");
            exit(1);
        }
    }
}
CursorTy doubleR_277(IntTy k1_40_738_1015, IntTy x1_41_739_1016,
                     CursorTy m0_42_740_1017, CursorTy t4_43_741_1018)
{
    TagTyPacked tag_1235 = *(TagTyPacked *) m0_42_740_1017;
    CursorTy tail_1236 = m0_42_740_1017 + sizeof(IntTy);
    
    
  switch_1237:
    ;
    switch (tag_1235) {
        
      case 1:
        {
            IntTy wildcard__133_45_742_1019 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1236)->field0;
            IntTy k2_46_743_1020 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1236)->field1;
            IntTy x2_47_744_1021 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1236)->field2;
            CursorTy t1_48_745_1022 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1236)->field3;
            CursorTy m1_49_746_1023 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1236)->field4;
            
            return caseFn_677(x1_41_739_1016, k1_40_738_1015, t4_43_741_1018,
                              m1_49_746_1023, k2_46_743_1020, x2_47_744_1021,
                              t1_48_745_1022);
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1235");
            exit(1);
        }
    }
}
CursorTy empty_278()
{
    PtrTy tailift_1238 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1238)->field0 = 0;
    return tailift_1238;
}
CursorTy rotateR_275(IntTy k_99_747_1024, IntTy x_100_748_1025,
                     CursorTy l_101_749_1026, CursorTy r_102_750_1027)
{
    TagTyPacked tag_1239 = *(TagTyPacked *) l_101_749_1026;
    CursorTy tail_1240 = l_101_749_1026 + sizeof(IntTy);
    
    
  switch_1241:
    ;
    switch (tag_1239) {
        
      case 1:
        {
            IntTy wildcard__72_104_751_1028 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1240)->field0;
            IntTy wildcard__73_105_752_1029 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1240)->field1;
            IntTy wildcard__74_106_753_1030 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1240)->field2;
            CursorTy ly_107_754_1031 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1240)->field3;
            CursorTy ry_108_755_1032 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1240)->field4;
            IntTy fltPrm_908_1033 =  size_273(ry_108_755_1032);
            IntTy fltPrm_910_1034 =  ratio();
            IntTy fltPrm_911_1035 =  size_273(ly_107_754_1031);
            IntTy fltPrm_909_1036 = fltPrm_910_1034 * fltPrm_911_1035;
            BoolTy fltIf_907_1037 = fltPrm_908_1033 < fltPrm_909_1036;
            
            if (fltIf_907_1037) {
                return singleR_276(k_99_747_1024, x_100_748_1025,
                                   l_101_749_1026, r_102_750_1027);
            } else {
                return doubleR_277(k_99_747_1024, x_100_748_1025,
                                   l_101_749_1026, r_102_750_1027);
            }
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1239");
            exit(1);
        }
    }
}
CursorTy balance_272(IntTy k_119_756_1038, IntTy x_120_757_1039,
                     CursorTy l_121_758_1040, CursorTy r_122_759_1041)
{
    IntTy fltPrm_914_1042 =  size_273(l_121_758_1040);
    IntTy fltPrm_915_1043 =  size_273(r_122_759_1041);
    IntTy fltPrm_913_1044 = fltPrm_914_1042 + fltPrm_915_1043;
    BoolTy fltIf_912_1045 = fltPrm_913_1044 <= 1;
    
    if (fltIf_912_1045) {
        IntTy fltPrm_917_1046 =  size_273(l_121_758_1040);
        IntTy fltPrm_918_1047 =  size_273(r_122_759_1041);
        IntTy fltPkd_916_1048 = fltPrm_917_1046 + fltPrm_918_1047;
        PtrTy tailift_1242 =
              ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
        
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1242)->field0 = 1;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1242)->field1 =
            fltPkd_916_1048;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1242)->field2 =
            k_119_756_1038;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1242)->field3 =
            x_120_757_1039;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1242)->field4 =
            l_121_758_1040;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1242)->field5 =
            r_122_759_1041;
        return tailift_1242;
    } else {
        IntTy fltPrm_920_1049 =  size_273(r_122_759_1041);
        IntTy fltPrm_922_1050 =  delta();
        IntTy fltPrm_923_1051 =  size_273(l_121_758_1040);
        IntTy fltPrm_921_1052 = fltPrm_922_1050 * fltPrm_923_1051;
        BoolTy fltIf_919_1053 = fltPrm_920_1049 >= fltPrm_921_1052;
        
        if (fltIf_919_1053) {
            return rotateL_274(k_119_756_1038, x_120_757_1039, l_121_758_1040,
                               r_122_759_1041);
        } else {
            IntTy fltPrm_925_1054 =  size_273(l_121_758_1040);
            IntTy fltPrm_927_1055 =  delta();
            IntTy fltPrm_928_1056 =  size_273(r_122_759_1041);
            IntTy fltPrm_926_1057 = fltPrm_927_1055 * fltPrm_928_1056;
            BoolTy fltIf_924_1058 = fltPrm_925_1054 >= fltPrm_926_1057;
            
            if (fltIf_924_1058) {
                return rotateR_275(k_119_756_1038, x_120_757_1039,
                                   l_121_758_1040, r_122_759_1041);
            } else {
                IntTy fltPrm_930_1059 =  size_273(l_121_758_1040);
                IntTy fltPrm_931_1060 =  size_273(r_122_759_1041);
                IntTy fltPkd_929_1061 = fltPrm_930_1059 + fltPrm_931_1060;
                PtrTy tailift_1243 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1243)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1243)->field1 =
                    fltPkd_929_1061;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1243)->field2 =
                    k_119_756_1038;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1243)->field3 =
                    x_120_757_1039;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1243)->field4 =
                    l_121_758_1040;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1243)->field5 =
                    r_122_759_1041;
                return tailift_1243;
            }
        }
    }
}
CursorTy insert_271(IntTy kx_30_760_1062, IntTy x_31_761_1063,
                    CursorTy m_32_762_1064)
{
    TagTyPacked tag_1244 = *(TagTyPacked *) m_32_762_1064;
    CursorTy tail_1245 = m_32_762_1064 + sizeof(IntTy);
    
    
  switch_1247:
    ;
    switch (tag_1244) {
        
      case 0:
        {
            return singleton_268(kx_30_760_1062, x_31_761_1063);
            break;
        }
        
      case 1:
        {
            IntTy sz_34_763_1065 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1245)->field0;
            IntTy k_35_764_1066 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1245)->field1;
            IntTy v_36_765_1067 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1245)->field2;
            CursorTy l_37_766_1068 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1245)->field3;
            CursorTy r_38_767_1069 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1245)->field4;
            BoolTy fltIf_932_1070 = kx_30_760_1062 == k_35_764_1066;
            
            if (fltIf_932_1070) {
                PtrTy tailift_1246 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1246)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1246)->field1 =
                    sz_34_763_1065;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1246)->field2 =
                    k_35_764_1066;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1246)->field3 =
                    x_31_761_1063;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1246)->field4 =
                    l_37_766_1068;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1246)->field5 =
                    r_38_767_1069;
                return tailift_1246;
            } else {
                BoolTy fltIf_933_1071 = kx_30_760_1062 <= k_35_764_1066;
                
                if (fltIf_933_1071) {
                    CursorTy fltAppE_934_1072 =
                              insert_271(kx_30_760_1062, x_31_761_1063, l_37_766_1068);
                    
                    return balance_272(k_35_764_1066, v_36_765_1067,
                                       fltAppE_934_1072, r_38_767_1069);
                } else {
                    CursorTy fltAppE_935_1073 =
                              insert_271(kx_30_760_1062, x_31_761_1063, r_38_767_1069);
                    
                    return balance_272(k_35_764_1066, v_36_765_1067,
                                       l_37_766_1068, fltAppE_935_1073);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1244");
            exit(1);
        }
    }
}
CursorTy lookup_270(IntTy k_22_768_1074, CursorTy m_23_769_1075)
{
    TagTyPacked tag_1248 = *(TagTyPacked *) m_23_769_1075;
    CursorTy tail_1249 = m_23_769_1075 + sizeof(IntTy);
    
    
  switch_1252:
    ;
    switch (tag_1248) {
        
      case 0:
        {
            PtrTy tailift_1250 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1250)->field0 = 0;
            return tailift_1250;
            break;
        }
        
      case 1:
        {
            IntTy wildcard__30_25_770_1076 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1249)->field0;
            IntTy kx_26_771_1077 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1249)->field1;
            IntTy v_27_772_1078 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1249)->field2;
            CursorTy l_28_773_1079 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1249)->field3;
            CursorTy r_29_774_1080 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1249)->field4;
            BoolTy fltIf_936_1081 = k_22_768_1074 < kx_26_771_1077;
            
            if (fltIf_936_1081) {
                return lookup_270(k_22_768_1074, l_28_773_1079);
            } else {
                BoolTy fltIf_937_1082 = k_22_768_1074 > kx_26_771_1077;
                
                if (fltIf_937_1082) {
                    return lookup_270(k_22_768_1074, r_29_774_1080);
                } else {
                    PtrTy tailift_1251 = ALLOC(sizeof(Int64Int64Prod));
                    
                    ((Int64Int64Prod *) tailift_1251)->field0 = 1;
                    ((Int64Int64Prod *) tailift_1251)->field1 = v_27_772_1078;
                    return tailift_1251;
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1248");
            exit(1);
        }
    }
}
CursorTy singleton_268(IntTy k_19_775_1083, IntTy x_20_776_1084)
{
    PtrTy fltPkd_938_1085 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_938_1085)->field0 = 0;
    
    PtrTy fltPkd_939_1086 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_939_1086)->field0 = 0;
    
    PtrTy tailift_1253 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1253)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1253)->field1 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1253)->field2 =
        k_19_775_1083;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1253)->field3 =
        x_20_776_1084;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1253)->field4 =
        fltPkd_938_1085;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1253)->field5 =
        fltPkd_939_1086;
    return tailift_1253;
}
CursorTy _copy_without_ptrs_Timestamp(CursorTy arg_580_777_1087)
{
    IntTy tag_1254 = ((Int64Int64Int64Prod *) arg_580_777_1087)->field0;
    IntTy x_581_778_1088 = ((Int64Int64Int64Prod *) arg_580_777_1087)->field1;
    IntTy x_582_779_1089 = ((Int64Int64Int64Prod *) arg_580_777_1087)->field2;
    PtrTy tailift_1255 = ALLOC(sizeof(Int64Int64Int64Prod));
    
    ((Int64Int64Int64Prod *) tailift_1255)->field0 = 0;
    ((Int64Int64Int64Prod *) tailift_1255)->field1 = x_581_778_1088;
    ((Int64Int64Int64Prod *) tailift_1255)->field2 = x_582_779_1089;
    return tailift_1255;
}
CursorTy _copy_Timestamp(CursorTy arg_575_782_1092)
{
    IntTy tag_1256 = ((Int64Int64Int64Prod *) arg_575_782_1092)->field0;
    IntTy x_576_783_1093 = ((Int64Int64Int64Prod *) arg_575_782_1092)->field1;
    IntTy x_577_784_1094 = ((Int64Int64Int64Prod *) arg_575_782_1092)->field2;
    PtrTy tailift_1257 = ALLOC(sizeof(Int64Int64Int64Prod));
    
    ((Int64Int64Int64Prod *) tailift_1257)->field0 = 0;
    ((Int64Int64Int64Prod *) tailift_1257)->field1 = x_576_783_1093;
    ((Int64Int64Int64Prod *) tailift_1257)->field2 = x_577_784_1094;
    return tailift_1257;
}
unsigned char _traverse_Timestamp(CursorTy arg_585_787_1097)
{
    IntTy tag_1258 = ((Int64Int64Int64Prod *) arg_585_787_1097)->field0;
    IntTy x_586_788_1098 = ((Int64Int64Int64Prod *) arg_585_787_1097)->field1;
    IntTy x_587_789_1099 = ((Int64Int64Int64Prod *) arg_585_787_1097)->field2;
    
    return 0;
}
unsigned char _print_Timestamp(CursorTy arg_590_790_1100)
{
    IntTy tag_1259 = ((Int64Int64Int64Prod *) arg_590_790_1100)->field0;
    IntTy x_591_791_1101 = ((Int64Int64Int64Prod *) arg_590_790_1100)->field1;
    IntTy x_592_792_1102 = ((Int64Int64Int64Prod *) arg_590_790_1100)->field2;
    unsigned char wildcard_595_793_1103 = print_symbol(1206);
    unsigned char wildcard_598_794_1104 = print_symbol(1210);
    unsigned char y_593_795_1105 = printf("%lld", x_591_791_1101);
    unsigned char wildcard_597_796_1106 = print_symbol(1210);
    unsigned char y_594_797_1107 = printf("%lld", x_592_792_1102);
    unsigned char wildcard_596_798_1108 = print_symbol(1204);
    
    return 0;
}
CursorTy _copy_Maybe_v_269(CursorTy arg_599_799_1109)
{
    TagTyPacked tag_1260 = *(TagTyPacked *) arg_599_799_1109;
    CursorTy tail_1261 = arg_599_799_1109 + sizeof(IntTy);
    
    
  switch_1264:
    ;
    switch (tag_1260) {
        
      case 0:
        {
            PtrTy tailift_1262 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1262)->field0 = 0;
            return tailift_1262;
            break;
        }
        
      case 1:
        {
            IntTy x_600_800_1110 = ((Int64Prod *) tail_1261)->field0;
            PtrTy tailift_1263 = ALLOC(sizeof(Int64Int64Prod));
            
            ((Int64Int64Prod *) tailift_1263)->field0 = 1;
            ((Int64Int64Prod *) tailift_1263)->field1 = x_600_800_1110;
            return tailift_1263;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1260");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Maybe_v_269(CursorTy arg_602_802_1112)
{
    TagTyPacked tag_1265 = *(TagTyPacked *) arg_602_802_1112;
    CursorTy tail_1266 = arg_602_802_1112 + sizeof(IntTy);
    
    
  switch_1269:
    ;
    switch (tag_1265) {
        
      case 0:
        {
            PtrTy tailift_1267 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1267)->field0 = 0;
            return tailift_1267;
            break;
        }
        
      case 1:
        {
            IntTy x_603_803_1113 = ((Int64Prod *) tail_1266)->field0;
            PtrTy tailift_1268 = ALLOC(sizeof(Int64Int64Prod));
            
            ((Int64Int64Prod *) tailift_1268)->field0 = 1;
            ((Int64Int64Prod *) tailift_1268)->field1 = x_603_803_1113;
            return tailift_1268;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1265");
            exit(1);
        }
    }
}
unsigned char _traverse_Maybe_v_269(CursorTy arg_605_805_1115)
{
    TagTyPacked tag_1270 = *(TagTyPacked *) arg_605_805_1115;
    CursorTy tail_1271 = arg_605_805_1115 + sizeof(IntTy);
    
    
  switch_1272:
    ;
    switch (tag_1270) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_606_806_1116 = ((Int64Prod *) tail_1271)->field0;
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1270");
            exit(1);
        }
    }
}
unsigned char _print_Maybe_v_269(CursorTy arg_608_807_1117)
{
    TagTyPacked tag_1273 = *(TagTyPacked *) arg_608_807_1117;
    CursorTy tail_1274 = arg_608_807_1117 + sizeof(IntTy);
    
    
  switch_1275:
    ;
    switch (tag_1273) {
        
      case 0:
        {
            unsigned char wildcard_609_808_1118 = print_symbol(1207);
            unsigned char wildcard_610_809_1119 = print_symbol(1204);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_611_810_1120 = ((Int64Prod *) tail_1274)->field0;
            unsigned char wildcard_613_811_1121 = print_symbol(1208);
            unsigned char wildcard_615_812_1122 = print_symbol(1210);
            unsigned char y_612_813_1123 = printf("%lld", x_611_810_1120);
            unsigned char wildcard_614_814_1124 = print_symbol(1204);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1273");
            exit(1);
        }
    }
}
CursorTy _copy_Map_v_267(CursorTy arg_616_815_1125)
{
    TagTyPacked tag_1276 = *(TagTyPacked *) arg_616_815_1125;
    CursorTy tail_1277 = arg_616_815_1125 + sizeof(IntTy);
    
    
  switch_1280:
    ;
    switch (tag_1276) {
        
      case 0:
        {
            PtrTy tailift_1278 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1278)->field0 = 0;
            return tailift_1278;
            break;
        }
        
      case 1:
        {
            IntTy x_617_816_1126 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1277)->field0;
            IntTy x_618_817_1127 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1277)->field1;
            IntTy x_619_818_1128 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1277)->field2;
            CursorTy x_620_819_1129 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1277)->field3;
            CursorTy x_621_820_1130 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1277)->field4;
            CursorTy y_625_824_1134 =  _copy_Map_v_267(x_620_819_1129);
            CursorTy y_626_825_1135 =  _copy_Map_v_267(x_621_820_1130);
            PtrTy tailift_1279 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1279)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1279)->field1 =
                x_617_816_1126;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1279)->field2 =
                x_618_817_1127;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1279)->field3 =
                x_619_818_1128;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1279)->field4 =
                y_625_824_1134;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1279)->field5 =
                y_626_825_1135;
            return tailift_1279;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1276");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Map_v_267(CursorTy arg_627_826_1136)
{
    TagTyPacked tag_1281 = *(TagTyPacked *) arg_627_826_1136;
    CursorTy tail_1282 = arg_627_826_1136 + sizeof(IntTy);
    
    
  switch_1285:
    ;
    switch (tag_1281) {
        
      case 0:
        {
            PtrTy tailift_1283 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1283)->field0 = 0;
            return tailift_1283;
            break;
        }
        
      case 1:
        {
            IntTy x_628_827_1137 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1282)->field0;
            IntTy x_629_828_1138 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1282)->field1;
            IntTy x_630_829_1139 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1282)->field2;
            CursorTy x_631_830_1140 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1282)->field3;
            CursorTy x_632_831_1141 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1282)->field4;
            CursorTy y_636_835_1145 =
                      _copy_without_ptrs_Map_v_267(x_631_830_1140);
            CursorTy y_637_836_1146 =
                      _copy_without_ptrs_Map_v_267(x_632_831_1141);
            PtrTy tailift_1284 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1284)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1284)->field1 =
                x_628_827_1137;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1284)->field2 =
                x_629_828_1138;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1284)->field3 =
                x_630_829_1139;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1284)->field4 =
                y_636_835_1145;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1284)->field5 =
                y_637_836_1146;
            return tailift_1284;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1281");
            exit(1);
        }
    }
}
unsigned char _traverse_Map_v_267(CursorTy arg_638_837_1147)
{
    TagTyPacked tag_1286 = *(TagTyPacked *) arg_638_837_1147;
    CursorTy tail_1287 = arg_638_837_1147 + sizeof(IntTy);
    
    
  switch_1288:
    ;
    switch (tag_1286) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_639_838_1148 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1287)->field0;
            IntTy x_640_839_1149 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1287)->field1;
            IntTy x_641_840_1150 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1287)->field2;
            CursorTy x_642_841_1151 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1287)->field3;
            CursorTy x_643_842_1152 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1287)->field4;
            unsigned char y_647_843_1153 =  _traverse_Map_v_267(x_642_841_1151);
            unsigned char y_648_844_1154 =  _traverse_Map_v_267(x_643_842_1152);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1286");
            exit(1);
        }
    }
}
unsigned char _print_Map_v_267(CursorTy arg_649_845_1155)
{
    TagTyPacked tag_1289 = *(TagTyPacked *) arg_649_845_1155;
    CursorTy tail_1290 = arg_649_845_1155 + sizeof(IntTy);
    
    
  switch_1291:
    ;
    switch (tag_1289) {
        
      case 0:
        {
            unsigned char wildcard_650_846_1156 = print_symbol(1205);
            unsigned char wildcard_651_847_1157 = print_symbol(1204);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_652_848_1158 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1290)->field0;
            IntTy x_653_849_1159 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1290)->field1;
            IntTy x_654_850_1160 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1290)->field2;
            CursorTy x_655_851_1161 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1290)->field3;
            CursorTy x_656_852_1162 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1290)->field4;
            unsigned char wildcard_662_853_1163 = print_symbol(1209);
            unsigned char wildcard_668_854_1164 = print_symbol(1210);
            unsigned char y_657_855_1165 = printf("%lld", x_652_848_1158);
            unsigned char wildcard_667_856_1166 = print_symbol(1210);
            unsigned char y_658_857_1167 = printf("%lld", x_653_849_1159);
            unsigned char wildcard_666_858_1168 = print_symbol(1210);
            unsigned char y_659_859_1169 = printf("%lld", x_654_850_1160);
            unsigned char wildcard_665_860_1170 = print_symbol(1210);
            unsigned char y_660_861_1171 =  _print_Map_v_267(x_655_851_1161);
            unsigned char wildcard_664_862_1172 = print_symbol(1210);
            unsigned char y_661_863_1173 =  _print_Map_v_267(x_656_852_1162);
            unsigned char wildcard_663_864_1174 = print_symbol(1204);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1289");
            exit(1);
        }
    }
}
CursorTy caseFn_669(IntTy x1_56_670_865_1175, IntTy k1_55_671_866_1176,
                    CursorTy t1_57_672_867_1177, CursorTy m1_63_673_868_1178,
                    IntTy k2_61_674_869_1179, IntTy x2_62_675_870_1180,
                    CursorTy t4_64_676_871_1181)
{
    TagTyPacked tag_1292 = *(TagTyPacked *) m1_63_673_868_1178;
    CursorTy tail_1293 = m1_63_673_868_1178 + sizeof(IntTy);
    
    
  switch_1294:
    ;
    switch (tag_1292) {
        
      case 1:
        {
            IntTy wildcard__110_65_872_1182 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1293)->field0;
            IntTy k3_66_873_1183 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1293)->field1;
            IntTy x3_67_874_1184 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1293)->field2;
            CursorTy t2_68_875_1185 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1293)->field3;
            CursorTy t3_69_876_1186 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1293)->field4;
            CursorTy fltAppE_940_1187 =
                      bin_279(k1_55_671_866_1176, x1_56_670_865_1175, t1_57_672_867_1177, t2_68_875_1185);
            CursorTy fltAppE_941_1188 =
                      bin_279(k2_61_674_869_1179, x2_62_675_870_1180, t3_69_876_1186, t4_64_676_871_1181);
            
            return bin_279(k3_66_873_1183, x3_67_874_1184, fltAppE_940_1187,
                           fltAppE_941_1188);
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1292");
            exit(1);
        }
    }
}
CursorTy caseFn_677(IntTy x1_41_678_877_1189, IntTy k1_40_679_878_1190,
                    CursorTy t4_43_680_879_1191, CursorTy m1_49_681_880_1192,
                    IntTy k2_46_682_881_1193, IntTy x2_47_683_882_1194,
                    CursorTy t1_48_684_883_1195)
{
    TagTyPacked tag_1295 = *(TagTyPacked *) m1_49_681_880_1192;
    CursorTy tail_1296 = m1_49_681_880_1192 + sizeof(IntTy);
    
    
  switch_1297:
    ;
    switch (tag_1295) {
        
      case 1:
        {
            IntTy wildcard__134_50_884_1196 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1296)->field0;
            IntTy k3_51_885_1197 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1296)->field1;
            IntTy x3_52_886_1198 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1296)->field2;
            CursorTy t2_53_887_1199 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1296)->field3;
            CursorTy t3_54_888_1200 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1296)->field4;
            CursorTy fltAppE_942_1201 =
                      bin_279(k2_46_682_881_1193, x2_47_683_882_1194, t1_48_684_883_1195, t2_53_887_1199);
            CursorTy fltAppE_943_1202 =
                      bin_279(k1_40_679_878_1190, x1_41_678_877_1189, t3_54_888_1200, t4_43_680_879_1191);
            
            return bin_279(k3_51_885_1197, x3_52_886_1198, fltAppE_942_1201,
                           fltAppE_943_1202);
            break;
        }
        
      case 0:
        {
            return empty_278();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1295");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1204, ")");
    add_symbol(1205, "(Tip_v_267");
    add_symbol(1206, "(Timestamp");
    add_symbol(1207, "(Nothing_v_269");
    add_symbol(1208, "(Just_v_269");
    add_symbol(1209, "(Bin_v_267");
    add_symbol(1210, " ");
    
    CursorTy fltAppE_892_944 =  init(0);
    CursorTy fltAppE_891_945 =  step(0, fltAppE_892_944);
    CursorTy fltAppE_890_946 =  step(1, fltAppE_891_945);
    CursorTy fltAppE_889_947 =  step(0, fltAppE_890_946);
    CursorTy tmp_app_1203 =  stamp(1, fltAppE_889_947);
    
     _print_Timestamp(tmp_app_1203);
    printf("\n");
    free_symtable();
    return 0;
}