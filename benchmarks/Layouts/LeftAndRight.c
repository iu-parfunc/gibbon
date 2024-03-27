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
IntTy sumRight(CursorTy m_48_719_958);
CursorTy build(IntTy x_54_725_967, IntTy sz_55_726_968, CursorTy m_56_727_969);
IntTy sumLeft(CursorTy m_57_728_979);
IntTy ratio();
IntTy delta();
IntTy size_338(CursorTy m_193_734_988);
CursorTy singleL_345(IntTy k1_120_740_994, IntTy x1_121_741_995,
                     CursorTy t1_122_742_996, CursorTy m_123_743_997);
CursorTy doubleL_346(IntTy k1_91_749_1004, IntTy x1_92_750_1005,
                     CursorTy t1_93_751_1006, CursorTy m0_94_752_1007);
CursorTy rotateL_339(IntTy k_145_758_1013, IntTy x_146_759_1014,
                     CursorTy l_147_760_1015, CursorTy r_148_761_1016);
CursorTy bin_344(IntTy k_130_767_1027, IntTy x_131_768_1028,
                 CursorTy l_132_769_1029, CursorTy r_133_770_1030);
CursorTy singleR_341(IntTy k1_110_771_1035, IntTy x1_111_772_1036,
                     CursorTy m_112_773_1037, CursorTy t3_113_774_1038);
CursorTy doubleR_342(IntTy k1_76_780_1045, IntTy x1_77_781_1046,
                     CursorTy m0_78_782_1047, CursorTy t4_79_783_1048);
CursorTy empty_343();
CursorTy rotateR_340(IntTy k_135_789_1054, IntTy x_136_790_1055,
                     CursorTy l_137_791_1056, CursorTy r_138_792_1057);
CursorTy balance_337(IntTy k_155_798_1068, IntTy x_156_799_1069,
                     CursorTy l_157_800_1070, CursorTy r_158_801_1071);
CursorTy singleton_334(IntTy k_72_802_1092, IntTy x_73_803_1093);
CursorTy insert_336(IntTy kx_63_804_1096, IntTy x_64_805_1097,
                    CursorTy m_65_806_1098);
CursorTy _copy_without_ptrs_Map_v_335(CursorTy arg_652_812_1108);
CursorTy _copy_Map_v_335(CursorTy arg_641_823_1119);
unsigned char _traverse_Map_v_335(CursorTy arg_663_834_1130);
unsigned char _print_Map_v_335(CursorTy arg_674_842_1138);
CursorTy caseFn_694(IntTy x1_92_695_862_1158, IntTy k1_91_696_863_1159,
                    CursorTy t1_93_697_864_1160, CursorTy m1_99_698_865_1161,
                    IntTy k2_97_699_866_1162, IntTy x2_98_700_867_1163,
                    CursorTy t4_100_701_868_1164);
CursorTy caseFn_702(IntTy x1_77_703_874_1172, IntTy k1_76_704_875_1173,
                    CursorTy t4_79_705_876_1174, CursorTy m1_85_706_877_1175,
                    IntTy k2_82_707_878_1176, IntTy x2_83_708_879_1177,
                    CursorTy t1_84_709_880_1178);
IntTy sumRight(CursorTy m_48_719_958)
{
    TagTyPacked tag_1195 = *(TagTyPacked *) m_48_719_958;
    CursorTy tail_1196 = m_48_719_958 + sizeof(IntTy);
    
    
  switch_1198:
    ;
    switch (tag_1195) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy wildcard__1_49_720_959 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1196)->field0;
            IntTy wildcard__2_50_721_960 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1196)->field1;
            IntTy v_51_722_961 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1196)->field2;
            CursorTy l_52_723_962 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1196)->field3;
            CursorTy r_53_724_963 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1196)->field4;
            IntTy fltPrm_888_964 =  sumRight(r_53_724_963);
            IntTy fltPrm_887_965 = fltPrm_888_964 + v_51_722_961;
            IntTy fltPrm_889_966 =  sumRight(r_53_724_963);
            IntTy flt_1197 = fltPrm_887_965 + fltPrm_889_966;
            
            return flt_1197;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1195");
            exit(1);
        }
    }
}
CursorTy build(IntTy x_54_725_967, IntTy sz_55_726_968, CursorTy m_56_727_969)
{
    BoolTy fltIf_890_970 = sz_55_726_968 == 0;
    
    if (fltIf_890_970) {
        return m_56_727_969;
    } else {
        IntTy fltPrm_892_971 = sz_55_726_968 / 2;
        IntTy fltAppE_891_972 = x_54_725_967 - fltPrm_892_971;
        IntTy fltAppE_893_973 = sz_55_726_968 / 2;
        IntTy fltPrm_896_974 = sz_55_726_968 / 2;
        IntTy fltAppE_895_975 = x_54_725_967 + fltPrm_896_974;
        IntTy fltAppE_897_976 = sz_55_726_968 / 2;
        CursorTy fltAppE_898_977 =
                  insert_336(x_54_725_967, x_54_725_967, m_56_727_969);
        CursorTy fltAppE_894_978 =
                  build(fltAppE_895_975, fltAppE_897_976, fltAppE_898_977);
        
        return build(fltAppE_891_972, fltAppE_893_973, fltAppE_894_978);
    }
}
IntTy sumLeft(CursorTy m_57_728_979)
{
    TagTyPacked tag_1199 = *(TagTyPacked *) m_57_728_979;
    CursorTy tail_1200 = m_57_728_979 + sizeof(IntTy);
    
    
  switch_1202:
    ;
    switch (tag_1199) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy wildcard__9_58_729_980 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1200)->field0;
            IntTy wildcard__10_59_730_981 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1200)->field1;
            IntTy v_60_731_982 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1200)->field2;
            CursorTy l_61_732_983 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1200)->field3;
            CursorTy r_62_733_984 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1200)->field4;
            IntTy fltPrm_900_985 =  sumLeft(l_61_732_983);
            IntTy fltPrm_899_986 = fltPrm_900_985 + v_60_731_982;
            IntTy fltPrm_901_987 =  sumLeft(r_62_733_984);
            IntTy flt_1201 = fltPrm_899_986 + fltPrm_901_987;
            
            return flt_1201;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1199");
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
IntTy size_338(CursorTy m_193_734_988)
{
    TagTyPacked tag_1203 = *(TagTyPacked *) m_193_734_988;
    CursorTy tail_1204 = m_193_734_988 + sizeof(IntTy);
    
    
  switch_1205:
    ;
    switch (tag_1203) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy sz_195_735_989 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1204)->field0;
            IntTy wildcard__18_196_736_990 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1204)->field1;
            IntTy wildcard__19_197_737_991 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1204)->field2;
            CursorTy wildcard__20_198_738_992 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1204)->field3;
            CursorTy wildcard__21_199_739_993 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1204)->field4;
            
            return sz_195_735_989;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1203");
            exit(1);
        }
    }
}
CursorTy singleL_345(IntTy k1_120_740_994, IntTy x1_121_741_995,
                     CursorTy t1_122_742_996, CursorTy m_123_743_997)
{
    TagTyPacked tag_1206 = *(TagTyPacked *) m_123_743_997;
    CursorTy tail_1207 = m_123_743_997 + sizeof(IntTy);
    
    
  switch_1208:
    ;
    switch (tag_1206) {
        
      case 1:
        {
            IntTy wildcard__123_125_744_998 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1207)->field0;
            IntTy k2_126_745_999 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1207)->field1;
            IntTy x2_127_746_1000 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1207)->field2;
            CursorTy t2_128_747_1001 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1207)->field3;
            CursorTy t3_129_748_1002 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1207)->field4;
            CursorTy fltAppE_902_1003 =
                      bin_344(k1_120_740_994, x1_121_741_995, t1_122_742_996, t2_128_747_1001);
            
            return bin_344(k2_126_745_999, x2_127_746_1000, fltAppE_902_1003,
                           t3_129_748_1002);
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1206");
            exit(1);
        }
    }
}
CursorTy doubleL_346(IntTy k1_91_749_1004, IntTy x1_92_750_1005,
                     CursorTy t1_93_751_1006, CursorTy m0_94_752_1007)
{
    TagTyPacked tag_1209 = *(TagTyPacked *) m0_94_752_1007;
    CursorTy tail_1210 = m0_94_752_1007 + sizeof(IntTy);
    
    
  switch_1211:
    ;
    switch (tag_1209) {
        
      case 1:
        {
            IntTy wildcard__143_96_753_1008 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1210)->field0;
            IntTy k2_97_754_1009 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1210)->field1;
            IntTy x2_98_755_1010 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1210)->field2;
            CursorTy m1_99_756_1011 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1210)->field3;
            CursorTy t4_100_757_1012 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1210)->field4;
            
            return caseFn_694(x1_92_750_1005, k1_91_749_1004, t1_93_751_1006,
                              m1_99_756_1011, k2_97_754_1009, x2_98_755_1010,
                              t4_100_757_1012);
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1209");
            exit(1);
        }
    }
}
CursorTy rotateL_339(IntTy k_145_758_1013, IntTy x_146_759_1014,
                     CursorTy l_147_760_1015, CursorTy r_148_761_1016)
{
    TagTyPacked tag_1212 = *(TagTyPacked *) r_148_761_1016;
    CursorTy tail_1213 = r_148_761_1016 + sizeof(IntTy);
    
    
  switch_1214:
    ;
    switch (tag_1212) {
        
      case 1:
        {
            IntTy wildcard__94_150_762_1017 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1213)->field0;
            IntTy wildcard__95_151_763_1018 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1213)->field1;
            IntTy wildcard__96_152_764_1019 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1213)->field2;
            CursorTy ly_153_765_1020 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1213)->field3;
            CursorTy ry_154_766_1021 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1213)->field4;
            IntTy fltPrm_904_1022 =  size_338(ly_153_765_1020);
            IntTy fltPrm_906_1023 =  ratio();
            IntTy fltPrm_907_1024 =  size_338(ry_154_766_1021);
            IntTy fltPrm_905_1025 = fltPrm_906_1023 * fltPrm_907_1024;
            BoolTy fltIf_903_1026 = fltPrm_904_1022 < fltPrm_905_1025;
            
            if (fltIf_903_1026) {
                return singleL_345(k_145_758_1013, x_146_759_1014,
                                   l_147_760_1015, r_148_761_1016);
            } else {
                return doubleL_346(k_145_758_1013, x_146_759_1014,
                                   l_147_760_1015, r_148_761_1016);
            }
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1212");
            exit(1);
        }
    }
}
CursorTy bin_344(IntTy k_130_767_1027, IntTy x_131_768_1028,
                 CursorTy l_132_769_1029, CursorTy r_133_770_1030)
{
    IntTy fltPrm_910_1031 =  size_338(l_132_769_1029);
    IntTy fltPrm_911_1032 =  size_338(r_133_770_1030);
    IntTy fltPrm_909_1033 = fltPrm_910_1031 + fltPrm_911_1032;
    IntTy fltPkd_908_1034 = fltPrm_909_1033 + 1;
    PtrTy tailift_1215 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field1 =
        fltPkd_908_1034;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field2 =
        k_130_767_1027;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field3 =
        x_131_768_1028;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field4 =
        l_132_769_1029;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field5 =
        r_133_770_1030;
    return tailift_1215;
}
CursorTy singleR_341(IntTy k1_110_771_1035, IntTy x1_111_772_1036,
                     CursorTy m_112_773_1037, CursorTy t3_113_774_1038)
{
    TagTyPacked tag_1216 = *(TagTyPacked *) m_112_773_1037;
    CursorTy tail_1217 = m_112_773_1037 + sizeof(IntTy);
    
    
  switch_1218:
    ;
    switch (tag_1216) {
        
      case 1:
        {
            IntTy wildcard__133_115_775_1039 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1217)->field0;
            IntTy k2_116_776_1040 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1217)->field1;
            IntTy x2_117_777_1041 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1217)->field2;
            CursorTy t1_118_778_1042 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1217)->field3;
            CursorTy t2_119_779_1043 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1217)->field4;
            CursorTy fltAppE_912_1044 =
                      bin_344(k1_110_771_1035, x1_111_772_1036, t2_119_779_1043, t3_113_774_1038);
            
            return bin_344(k2_116_776_1040, x2_117_777_1041, t1_118_778_1042,
                           fltAppE_912_1044);
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1216");
            exit(1);
        }
    }
}
CursorTy doubleR_342(IntTy k1_76_780_1045, IntTy x1_77_781_1046,
                     CursorTy m0_78_782_1047, CursorTy t4_79_783_1048)
{
    TagTyPacked tag_1219 = *(TagTyPacked *) m0_78_782_1047;
    CursorTy tail_1220 = m0_78_782_1047 + sizeof(IntTy);
    
    
  switch_1221:
    ;
    switch (tag_1219) {
        
      case 1:
        {
            IntTy wildcard__167_81_784_1049 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1220)->field0;
            IntTy k2_82_785_1050 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1220)->field1;
            IntTy x2_83_786_1051 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1220)->field2;
            CursorTy t1_84_787_1052 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1220)->field3;
            CursorTy m1_85_788_1053 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1220)->field4;
            
            return caseFn_702(x1_77_781_1046, k1_76_780_1045, t4_79_783_1048,
                              m1_85_788_1053, k2_82_785_1050, x2_83_786_1051,
                              t1_84_787_1052);
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1219");
            exit(1);
        }
    }
}
CursorTy empty_343()
{
    PtrTy tailift_1222 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1222)->field0 = 0;
    return tailift_1222;
}
CursorTy rotateR_340(IntTy k_135_789_1054, IntTy x_136_790_1055,
                     CursorTy l_137_791_1056, CursorTy r_138_792_1057)
{
    TagTyPacked tag_1223 = *(TagTyPacked *) l_137_791_1056;
    CursorTy tail_1224 = l_137_791_1056 + sizeof(IntTy);
    
    
  switch_1225:
    ;
    switch (tag_1223) {
        
      case 1:
        {
            IntTy wildcard__106_140_793_1058 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1224)->field0;
            IntTy wildcard__107_141_794_1059 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1224)->field1;
            IntTy wildcard__108_142_795_1060 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1224)->field2;
            CursorTy ly_143_796_1061 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1224)->field3;
            CursorTy ry_144_797_1062 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1224)->field4;
            IntTy fltPrm_914_1063 =  size_338(ry_144_797_1062);
            IntTy fltPrm_916_1064 =  ratio();
            IntTy fltPrm_917_1065 =  size_338(ly_143_796_1061);
            IntTy fltPrm_915_1066 = fltPrm_916_1064 * fltPrm_917_1065;
            BoolTy fltIf_913_1067 = fltPrm_914_1063 < fltPrm_915_1066;
            
            if (fltIf_913_1067) {
                return singleR_341(k_135_789_1054, x_136_790_1055,
                                   l_137_791_1056, r_138_792_1057);
            } else {
                return doubleR_342(k_135_789_1054, x_136_790_1055,
                                   l_137_791_1056, r_138_792_1057);
            }
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1223");
            exit(1);
        }
    }
}
CursorTy balance_337(IntTy k_155_798_1068, IntTy x_156_799_1069,
                     CursorTy l_157_800_1070, CursorTy r_158_801_1071)
{
    IntTy fltPrm_920_1072 =  size_338(l_157_800_1070);
    IntTy fltPrm_921_1073 =  size_338(r_158_801_1071);
    IntTy fltPrm_919_1074 = fltPrm_920_1072 + fltPrm_921_1073;
    BoolTy fltIf_918_1075 = fltPrm_919_1074 <= 1;
    
    if (fltIf_918_1075) {
        IntTy fltPrm_923_1076 =  size_338(l_157_800_1070);
        IntTy fltPrm_924_1077 =  size_338(r_158_801_1071);
        IntTy fltPkd_922_1078 = fltPrm_923_1076 + fltPrm_924_1077;
        PtrTy tailift_1226 =
              ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
        
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1226)->field0 = 1;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1226)->field1 =
            fltPkd_922_1078;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1226)->field2 =
            k_155_798_1068;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1226)->field3 =
            x_156_799_1069;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1226)->field4 =
            l_157_800_1070;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1226)->field5 =
            r_158_801_1071;
        return tailift_1226;
    } else {
        IntTy fltPrm_926_1079 =  size_338(r_158_801_1071);
        IntTy fltPrm_928_1080 =  delta();
        IntTy fltPrm_929_1081 =  size_338(l_157_800_1070);
        IntTy fltPrm_927_1082 = fltPrm_928_1080 * fltPrm_929_1081;
        BoolTy fltIf_925_1083 = fltPrm_926_1079 >= fltPrm_927_1082;
        
        if (fltIf_925_1083) {
            return rotateL_339(k_155_798_1068, x_156_799_1069, l_157_800_1070,
                               r_158_801_1071);
        } else {
            IntTy fltPrm_931_1084 =  size_338(l_157_800_1070);
            IntTy fltPrm_933_1085 =  delta();
            IntTy fltPrm_934_1086 =  size_338(r_158_801_1071);
            IntTy fltPrm_932_1087 = fltPrm_933_1085 * fltPrm_934_1086;
            BoolTy fltIf_930_1088 = fltPrm_931_1084 >= fltPrm_932_1087;
            
            if (fltIf_930_1088) {
                return rotateR_340(k_155_798_1068, x_156_799_1069,
                                   l_157_800_1070, r_158_801_1071);
            } else {
                IntTy fltPrm_936_1089 =  size_338(l_157_800_1070);
                IntTy fltPrm_937_1090 =  size_338(r_158_801_1071);
                IntTy fltPkd_935_1091 = fltPrm_936_1089 + fltPrm_937_1090;
                PtrTy tailift_1227 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1227)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1227)->field1 =
                    fltPkd_935_1091;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1227)->field2 =
                    k_155_798_1068;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1227)->field3 =
                    x_156_799_1069;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1227)->field4 =
                    l_157_800_1070;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1227)->field5 =
                    r_158_801_1071;
                return tailift_1227;
            }
        }
    }
}
CursorTy singleton_334(IntTy k_72_802_1092, IntTy x_73_803_1093)
{
    PtrTy fltPkd_938_1094 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_938_1094)->field0 = 0;
    
    PtrTy fltPkd_939_1095 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_939_1095)->field0 = 0;
    
    PtrTy tailift_1228 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1228)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1228)->field1 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1228)->field2 =
        k_72_802_1092;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1228)->field3 =
        x_73_803_1093;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1228)->field4 =
        fltPkd_938_1094;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1228)->field5 =
        fltPkd_939_1095;
    return tailift_1228;
}
CursorTy insert_336(IntTy kx_63_804_1096, IntTy x_64_805_1097,
                    CursorTy m_65_806_1098)
{
    TagTyPacked tag_1229 = *(TagTyPacked *) m_65_806_1098;
    CursorTy tail_1230 = m_65_806_1098 + sizeof(IntTy);
    
    
  switch_1232:
    ;
    switch (tag_1229) {
        
      case 0:
        {
            return singleton_334(kx_63_804_1096, x_64_805_1097);
            break;
        }
        
      case 1:
        {
            IntTy sz_67_807_1099 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1230)->field0;
            IntTy k_68_808_1100 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1230)->field1;
            IntTy v_69_809_1101 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1230)->field2;
            CursorTy l_70_810_1102 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1230)->field3;
            CursorTy r_71_811_1103 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1230)->field4;
            BoolTy fltIf_940_1104 = kx_63_804_1096 == k_68_808_1100;
            
            if (fltIf_940_1104) {
                PtrTy tailift_1231 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field1 =
                    sz_67_807_1099;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field2 =
                    k_68_808_1100;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field3 =
                    x_64_805_1097;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field4 =
                    l_70_810_1102;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1231)->field5 =
                    r_71_811_1103;
                return tailift_1231;
            } else {
                BoolTy fltIf_941_1105 = kx_63_804_1096 <= k_68_808_1100;
                
                if (fltIf_941_1105) {
                    CursorTy fltAppE_942_1106 =
                              insert_336(kx_63_804_1096, x_64_805_1097, l_70_810_1102);
                    
                    return balance_337(k_68_808_1100, v_69_809_1101,
                                       fltAppE_942_1106, r_71_811_1103);
                } else {
                    CursorTy fltAppE_943_1107 =
                              insert_336(kx_63_804_1096, x_64_805_1097, r_71_811_1103);
                    
                    return balance_337(k_68_808_1100, v_69_809_1101,
                                       l_70_810_1102, fltAppE_943_1107);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1229");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Map_v_335(CursorTy arg_652_812_1108)
{
    TagTyPacked tag_1233 = *(TagTyPacked *) arg_652_812_1108;
    CursorTy tail_1234 = arg_652_812_1108 + sizeof(IntTy);
    
    
  switch_1237:
    ;
    switch (tag_1233) {
        
      case 0:
        {
            PtrTy tailift_1235 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1235)->field0 = 0;
            return tailift_1235;
            break;
        }
        
      case 1:
        {
            IntTy x_653_813_1109 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1234)->field0;
            IntTy x_654_814_1110 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1234)->field1;
            IntTy x_655_815_1111 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1234)->field2;
            CursorTy x_656_816_1112 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1234)->field3;
            CursorTy x_657_817_1113 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1234)->field4;
            CursorTy y_661_821_1117 =
                      _copy_without_ptrs_Map_v_335(x_656_816_1112);
            CursorTy y_662_822_1118 =
                      _copy_without_ptrs_Map_v_335(x_657_817_1113);
            PtrTy tailift_1236 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1236)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1236)->field1 =
                x_653_813_1109;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1236)->field2 =
                x_654_814_1110;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1236)->field3 =
                x_655_815_1111;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1236)->field4 =
                y_661_821_1117;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1236)->field5 =
                y_662_822_1118;
            return tailift_1236;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1233");
            exit(1);
        }
    }
}
CursorTy _copy_Map_v_335(CursorTy arg_641_823_1119)
{
    TagTyPacked tag_1238 = *(TagTyPacked *) arg_641_823_1119;
    CursorTy tail_1239 = arg_641_823_1119 + sizeof(IntTy);
    
    
  switch_1242:
    ;
    switch (tag_1238) {
        
      case 0:
        {
            PtrTy tailift_1240 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1240)->field0 = 0;
            return tailift_1240;
            break;
        }
        
      case 1:
        {
            IntTy x_642_824_1120 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1239)->field0;
            IntTy x_643_825_1121 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1239)->field1;
            IntTy x_644_826_1122 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1239)->field2;
            CursorTy x_645_827_1123 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1239)->field3;
            CursorTy x_646_828_1124 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1239)->field4;
            CursorTy y_650_832_1128 =  _copy_Map_v_335(x_645_827_1123);
            CursorTy y_651_833_1129 =  _copy_Map_v_335(x_646_828_1124);
            PtrTy tailift_1241 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1241)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1241)->field1 =
                x_642_824_1120;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1241)->field2 =
                x_643_825_1121;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1241)->field3 =
                x_644_826_1122;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1241)->field4 =
                y_650_832_1128;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1241)->field5 =
                y_651_833_1129;
            return tailift_1241;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1238");
            exit(1);
        }
    }
}
unsigned char _traverse_Map_v_335(CursorTy arg_663_834_1130)
{
    TagTyPacked tag_1243 = *(TagTyPacked *) arg_663_834_1130;
    CursorTy tail_1244 = arg_663_834_1130 + sizeof(IntTy);
    
    
  switch_1245:
    ;
    switch (tag_1243) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_664_835_1131 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1244)->field0;
            IntTy x_665_836_1132 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1244)->field1;
            IntTy x_666_837_1133 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1244)->field2;
            CursorTy x_667_838_1134 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1244)->field3;
            CursorTy x_668_839_1135 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1244)->field4;
            unsigned char y_672_840_1136 =  _traverse_Map_v_335(x_667_838_1134);
            unsigned char y_673_841_1137 =  _traverse_Map_v_335(x_668_839_1135);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1243");
            exit(1);
        }
    }
}
unsigned char _print_Map_v_335(CursorTy arg_674_842_1138)
{
    TagTyPacked tag_1246 = *(TagTyPacked *) arg_674_842_1138;
    CursorTy tail_1247 = arg_674_842_1138 + sizeof(IntTy);
    
    
  switch_1248:
    ;
    switch (tag_1246) {
        
      case 0:
        {
            unsigned char wildcard_675_843_1139 = print_symbol(1191);
            unsigned char wildcard_676_844_1140 = print_symbol(1190);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_677_845_1141 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1247)->field0;
            IntTy x_678_846_1142 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1247)->field1;
            IntTy x_679_847_1143 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1247)->field2;
            CursorTy x_680_848_1144 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1247)->field3;
            CursorTy x_681_849_1145 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1247)->field4;
            unsigned char wildcard_687_850_1146 = print_symbol(1192);
            unsigned char wildcard_693_851_1147 = print_symbol(1193);
            unsigned char y_682_852_1148 = printf("%lld", x_677_845_1141);
            unsigned char wildcard_692_853_1149 = print_symbol(1193);
            unsigned char y_683_854_1150 = printf("%lld", x_678_846_1142);
            unsigned char wildcard_691_855_1151 = print_symbol(1193);
            unsigned char y_684_856_1152 = printf("%lld", x_679_847_1143);
            unsigned char wildcard_690_857_1153 = print_symbol(1193);
            unsigned char y_685_858_1154 =  _print_Map_v_335(x_680_848_1144);
            unsigned char wildcard_689_859_1155 = print_symbol(1193);
            unsigned char y_686_860_1156 =  _print_Map_v_335(x_681_849_1145);
            unsigned char wildcard_688_861_1157 = print_symbol(1190);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1246");
            exit(1);
        }
    }
}
CursorTy caseFn_694(IntTy x1_92_695_862_1158, IntTy k1_91_696_863_1159,
                    CursorTy t1_93_697_864_1160, CursorTy m1_99_698_865_1161,
                    IntTy k2_97_699_866_1162, IntTy x2_98_700_867_1163,
                    CursorTy t4_100_701_868_1164)
{
    TagTyPacked tag_1249 = *(TagTyPacked *) m1_99_698_865_1161;
    CursorTy tail_1250 = m1_99_698_865_1161 + sizeof(IntTy);
    
    
  switch_1251:
    ;
    switch (tag_1249) {
        
      case 1:
        {
            IntTy wildcard__144_101_869_1165 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1250)->field0;
            IntTy k3_102_870_1166 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1250)->field1;
            IntTy x3_103_871_1167 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1250)->field2;
            CursorTy t2_104_872_1168 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1250)->field3;
            CursorTy t3_105_873_1169 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1250)->field4;
            CursorTy fltAppE_944_1170 =
                      bin_344(k1_91_696_863_1159, x1_92_695_862_1158, t1_93_697_864_1160, t2_104_872_1168);
            CursorTy fltAppE_945_1171 =
                      bin_344(k2_97_699_866_1162, x2_98_700_867_1163, t3_105_873_1169, t4_100_701_868_1164);
            
            return bin_344(k3_102_870_1166, x3_103_871_1167, fltAppE_944_1170,
                           fltAppE_945_1171);
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1249");
            exit(1);
        }
    }
}
CursorTy caseFn_702(IntTy x1_77_703_874_1172, IntTy k1_76_704_875_1173,
                    CursorTy t4_79_705_876_1174, CursorTy m1_85_706_877_1175,
                    IntTy k2_82_707_878_1176, IntTy x2_83_708_879_1177,
                    CursorTy t1_84_709_880_1178)
{
    TagTyPacked tag_1252 = *(TagTyPacked *) m1_85_706_877_1175;
    CursorTy tail_1253 = m1_85_706_877_1175 + sizeof(IntTy);
    
    
  switch_1254:
    ;
    switch (tag_1252) {
        
      case 1:
        {
            IntTy wildcard__168_86_881_1179 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1253)->field0;
            IntTy k3_87_882_1180 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1253)->field1;
            IntTy x3_88_883_1181 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1253)->field2;
            CursorTy t2_89_884_1182 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1253)->field3;
            CursorTy t3_90_885_1183 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1253)->field4;
            CursorTy fltAppE_946_1184 =
                      bin_344(k2_82_707_878_1176, x2_83_708_879_1177, t1_84_709_880_1178, t2_89_884_1182);
            CursorTy fltAppE_947_1185 =
                      bin_344(k1_76_704_875_1173, x1_77_703_874_1172, t3_90_885_1183, t4_79_705_876_1174);
            
            return bin_344(k3_87_882_1180, x3_88_883_1181, fltAppE_946_1184,
                           fltAppE_947_1185);
            break;
        }
        
      case 0:
        {
            return empty_343();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1252");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1188, "SUM RIGHT: ");
    add_symbol(1189, "SUM LEFT: ");
    add_symbol(1190, ")");
    add_symbol(1191, "(Tip_v_335");
    add_symbol(1192, "(Bin_v_335");
    add_symbol(1193, " ");
    add_symbol(1194, "\n");
    
    CursorTy fltAppE_886_948 =  singleton_334(0, 0);
    CursorTy m_37_710_949 =  build(0, 10, fltAppE_886_948);
    unsigned char wildcard__33_38_711_950 = print_symbol(1188);
    IntTy timed_1186;
    VectorTy *times_3 = vector_alloc(global_iters_param, sizeof(double));
    struct timespec begin_timed_1186;
    struct timespec end_timed_1186;
    
    for (long long iters_timed_1186 = 0; iters_timed_1186 < global_iters_param;
         iters_timed_1186++) {
        if (iters_timed_1186 != global_iters_param - 1)
            save_alloc_state();
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1186);
        
        IntTy timed_1186hack =  sumRight(m_37_710_949);
        
        timed_1186 = timed_1186hack;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1186);
        if (iters_timed_1186 != global_iters_param - 1)
            restore_alloc_state();
        
        double itertime_0 = difftimespecs(&begin_timed_1186, &end_timed_1186);
        
        vector_inplace_update(times_3, iters_timed_1186, &itertime_0);
    }
    vector_inplace_sort(times_3, compare_doubles);
    
    double *tmp_4 = (double *) vector_nth(times_3, global_iters_param / 2);
    double selftimed_2 = *tmp_4;
    double batchtime_1 = sum_timing_array(times_3);
    
    print_timing_array(times_3);
    printf("ITERS: %lld\n", global_iters_param);
    printf("SIZE: %lld\n", global_size_param);
    printf("BATCHTIME: %e\n", batchtime_1);
    printf("SELFTIMED: %e\n", selftimed_2);
    
    unsigned char wildcard__29_40_713_952 = printf("%lld", timed_1186);
    unsigned char wildcard__27_41_714_953 = print_symbol(1194);
    unsigned char wildcard__25_42_715_954 = print_symbol(1189);
    IntTy timed_1187;
    VectorTy *times_8 = vector_alloc(global_iters_param, sizeof(double));
    struct timespec begin_timed_1187;
    struct timespec end_timed_1187;
    
    for (long long iters_timed_1187 = 0; iters_timed_1187 < global_iters_param;
         iters_timed_1187++) {
        if (iters_timed_1187 != global_iters_param - 1)
            save_alloc_state();
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1187);
        
        IntTy timed_1187hack =  sumLeft(m_37_710_949);
        
        timed_1187 = timed_1187hack;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1187);
        if (iters_timed_1187 != global_iters_param - 1)
            restore_alloc_state();
        
        double itertime_5 = difftimespecs(&begin_timed_1187, &end_timed_1187);
        
        vector_inplace_update(times_8, iters_timed_1187, &itertime_5);
    }
    vector_inplace_sort(times_8, compare_doubles);
    
    double *tmp_9 = (double *) vector_nth(times_8, global_iters_param / 2);
    double selftimed_7 = *tmp_9;
    double batchtime_6 = sum_timing_array(times_8);
    
    print_timing_array(times_8);
    printf("ITERS: %lld\n", global_iters_param);
    printf("SIZE: %lld\n", global_size_param);
    printf("BATCHTIME: %e\n", batchtime_6);
    printf("SELFTIMED: %e\n", selftimed_7);
    
    unsigned char wildcard__21_44_717_956 = printf("%lld", timed_1187);
    unsigned char wildcard__19_45_718_957 = print_symbol(1194);
    
    printf("'#()");
    printf("\n");
    free_symtable();
    return 0;
}