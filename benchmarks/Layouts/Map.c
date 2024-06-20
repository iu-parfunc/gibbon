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
typedef struct Int64Int64Int64VectorCursorCursorProd_struct {
            IntTy field0;
            IntTy field1;
            IntTy field2;
            VectorTy *field3;
            CursorTy field4;
            CursorTy field5;
        } Int64Int64Int64VectorCursorCursorProd;
typedef struct Int64Int64VectorCursorCursorProd_struct {
            IntTy field0;
            IntTy field1;
            VectorTy *field2;
            CursorTy field3;
            CursorTy field4;
        } Int64Int64VectorCursorCursorProd;
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
typedef struct VectorProd_struct {
            VectorTy *field0;
        } VectorProd;
IntTy ratio();
IntTy delta();
CursorTy singleton_480(IntTy k_344_858_1098, VectorTy *x_345_859_1099);
IntTy size_482(CursorTy m_330_860_1102);
CursorTy singleL_488(IntTy k1_248_866_1108, VectorTy *x1_249_867_1109,
                     CursorTy t1_250_868_1110, CursorTy m_251_869_1111);
CursorTy doubleL_489(IntTy k1_219_875_1118, VectorTy *x1_220_876_1119,
                     CursorTy t1_221_877_1120, CursorTy m0_222_878_1121);
CursorTy rotateL_483(IntTy k_273_884_1127, VectorTy *x_274_885_1128,
                     CursorTy l_275_886_1129, CursorTy r_276_887_1130);
CursorTy bin_487(IntTy k_258_893_1141, VectorTy *x_259_894_1142,
                 CursorTy l_260_895_1143, CursorTy r_261_896_1144);
CursorTy singleR_485(IntTy k1_238_897_1149, VectorTy *x1_239_898_1150,
                     CursorTy m_240_899_1151, CursorTy t3_241_900_1152);
CursorTy doubleR_486(IntTy k1_204_906_1159, VectorTy *x1_205_907_1160,
                     CursorTy m0_206_908_1161, CursorTy t4_207_909_1162);
CursorTy rotateR_484(IntTy k_263_915_1168, VectorTy *x_264_916_1169,
                     CursorTy l_265_917_1170, CursorTy r_266_918_1171);
CursorTy balance_481(IntTy k_283_924_1182, VectorTy *x_284_925_1183,
                     CursorTy l_285_926_1184, CursorTy r_286_927_1185);
CursorTy insert_478(IntTy kx_288_928_1206, VectorTy *x_289_929_1207,
                    CursorTy m_290_930_1208);
CursorTy empty_477();
CursorTy _copy_without_ptrs_Map_v_479(CursorTy arg_786_936_1218);
CursorTy _copy_Map_v_479(CursorTy arg_775_947_1229);
unsigned char _traverse_Map_v_479(CursorTy arg_797_958_1240);
unsigned char _print_Map_v_479(CursorTy arg_808_966_1248);
CursorTy caseFn_828(VectorTy *x1_220_829_986_1268, IntTy k1_219_830_987_1269,
                    CursorTy t1_221_831_988_1270, CursorTy m1_227_832_989_1271,
                    IntTy k2_225_833_990_1272, VectorTy *x2_226_834_991_1273,
                    CursorTy t4_228_835_992_1274);
CursorTy caseFn_836(VectorTy *x1_205_837_998_1282, IntTy k1_204_838_999_1283,
                    CursorTy t4_207_839_1000_1284,
                    CursorTy m1_213_840_1001_1285, IntTy k2_210_841_1002_1286,
                    VectorTy *x2_211_842_1003_1287,
                    CursorTy t1_212_843_1004_1288);
IntTy ratio()
{
    return 2;
}
IntTy delta()
{
    return 4;
}
CursorTy singleton_480(IntTy k_344_858_1098, VectorTy *x_345_859_1099)
{
    PtrTy fltPkd_1024_1100 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_1024_1100)->field0 = 0;
    
    PtrTy fltPkd_1025_1101 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_1025_1101)->field0 = 0;
    
    PtrTy tailift_1302 = ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
    
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1302)->field0 = 1;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1302)->field1 = 1;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1302)->field2 =
        k_344_858_1098;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1302)->field3 =
        x_345_859_1099;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1302)->field4 =
        fltPkd_1024_1100;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1302)->field5 =
        fltPkd_1025_1101;
    return tailift_1302;
}
IntTy size_482(CursorTy m_330_860_1102)
{
    TagTyPacked tag_1303 = *(TagTyPacked *) m_330_860_1102;
    CursorTy tail_1304 = m_330_860_1102 + sizeof(IntTy);
    
    
  switch_1305:
    ;
    switch (tag_1303) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy sz_332_861_1103 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1304)->field0;
            IntTy wildcard__18_333_862_1104 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1304)->field1;
            VectorTy *wildcard__19_334_863_1105 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1304)->field2;
            CursorTy wildcard__20_335_864_1106 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1304)->field3;
            CursorTy wildcard__21_336_865_1107 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1304)->field4;
            
            return sz_332_861_1103;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1303");
            exit(1);
        }
    }
}
CursorTy singleL_488(IntTy k1_248_866_1108, VectorTy *x1_249_867_1109,
                     CursorTy t1_250_868_1110, CursorTy m_251_869_1111)
{
    TagTyPacked tag_1306 = *(TagTyPacked *) m_251_869_1111;
    CursorTy tail_1307 = m_251_869_1111 + sizeof(IntTy);
    
    
  switch_1308:
    ;
    switch (tag_1306) {
        
      case 1:
        {
            IntTy wildcard__123_253_870_1112 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1307)->field0;
            IntTy k2_254_871_1113 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1307)->field1;
            VectorTy *x2_255_872_1114 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1307)->field2;
            CursorTy t2_256_873_1115 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1307)->field3;
            CursorTy t3_257_874_1116 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1307)->field4;
            CursorTy fltAppE_1026_1117 =
                      bin_487(k1_248_866_1108, x1_249_867_1109, t1_250_868_1110, t2_256_873_1115);
            
            return bin_487(k2_254_871_1113, x2_255_872_1114, fltAppE_1026_1117,
                           t3_257_874_1116);
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1306");
            exit(1);
        }
    }
}
CursorTy doubleL_489(IntTy k1_219_875_1118, VectorTy *x1_220_876_1119,
                     CursorTy t1_221_877_1120, CursorTy m0_222_878_1121)
{
    TagTyPacked tag_1309 = *(TagTyPacked *) m0_222_878_1121;
    CursorTy tail_1310 = m0_222_878_1121 + sizeof(IntTy);
    
    
  switch_1311:
    ;
    switch (tag_1309) {
        
      case 1:
        {
            IntTy wildcard__143_224_879_1122 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1310)->field0;
            IntTy k2_225_880_1123 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1310)->field1;
            VectorTy *x2_226_881_1124 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1310)->field2;
            CursorTy m1_227_882_1125 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1310)->field3;
            CursorTy t4_228_883_1126 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1310)->field4;
            
            return caseFn_828(x1_220_876_1119, k1_219_875_1118, t1_221_877_1120,
                              m1_227_882_1125, k2_225_880_1123, x2_226_881_1124,
                              t4_228_883_1126);
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1309");
            exit(1);
        }
    }
}
CursorTy rotateL_483(IntTy k_273_884_1127, VectorTy *x_274_885_1128,
                     CursorTy l_275_886_1129, CursorTy r_276_887_1130)
{
    TagTyPacked tag_1312 = *(TagTyPacked *) r_276_887_1130;
    CursorTy tail_1313 = r_276_887_1130 + sizeof(IntTy);
    
    
  switch_1314:
    ;
    switch (tag_1312) {
        
      case 1:
        {
            IntTy wildcard__94_278_888_1131 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1313)->field0;
            IntTy wildcard__95_279_889_1132 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1313)->field1;
            VectorTy *wildcard__96_280_890_1133 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1313)->field2;
            CursorTy ly_281_891_1134 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1313)->field3;
            CursorTy ry_282_892_1135 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1313)->field4;
            IntTy fltPrm_1028_1136 =  size_482(ly_281_891_1134);
            IntTy fltPrm_1030_1137 =  ratio();
            IntTy fltPrm_1031_1138 =  size_482(ry_282_892_1135);
            IntTy fltPrm_1029_1139 = fltPrm_1030_1137 * fltPrm_1031_1138;
            BoolTy fltIf_1027_1140 = fltPrm_1028_1136 < fltPrm_1029_1139;
            
            if (fltIf_1027_1140) {
                return singleL_488(k_273_884_1127, x_274_885_1128,
                                   l_275_886_1129, r_276_887_1130);
            } else {
                return doubleL_489(k_273_884_1127, x_274_885_1128,
                                   l_275_886_1129, r_276_887_1130);
            }
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1312");
            exit(1);
        }
    }
}
CursorTy bin_487(IntTy k_258_893_1141, VectorTy *x_259_894_1142,
                 CursorTy l_260_895_1143, CursorTy r_261_896_1144)
{
    IntTy fltPrm_1034_1145 =  size_482(l_260_895_1143);
    IntTy fltPrm_1035_1146 =  size_482(r_261_896_1144);
    IntTy fltPrm_1033_1147 = fltPrm_1034_1145 + fltPrm_1035_1146;
    IntTy fltPkd_1032_1148 = fltPrm_1033_1147 + 1;
    PtrTy tailift_1315 = ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
    
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1315)->field0 = 1;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1315)->field1 =
        fltPkd_1032_1148;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1315)->field2 =
        k_258_893_1141;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1315)->field3 =
        x_259_894_1142;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1315)->field4 =
        l_260_895_1143;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1315)->field5 =
        r_261_896_1144;
    return tailift_1315;
}
CursorTy singleR_485(IntTy k1_238_897_1149, VectorTy *x1_239_898_1150,
                     CursorTy m_240_899_1151, CursorTy t3_241_900_1152)
{
    TagTyPacked tag_1316 = *(TagTyPacked *) m_240_899_1151;
    CursorTy tail_1317 = m_240_899_1151 + sizeof(IntTy);
    
    
  switch_1318:
    ;
    switch (tag_1316) {
        
      case 1:
        {
            IntTy wildcard__133_243_901_1153 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1317)->field0;
            IntTy k2_244_902_1154 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1317)->field1;
            VectorTy *x2_245_903_1155 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1317)->field2;
            CursorTy t1_246_904_1156 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1317)->field3;
            CursorTy t2_247_905_1157 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1317)->field4;
            CursorTy fltAppE_1036_1158 =
                      bin_487(k1_238_897_1149, x1_239_898_1150, t2_247_905_1157, t3_241_900_1152);
            
            return bin_487(k2_244_902_1154, x2_245_903_1155, t1_246_904_1156,
                           fltAppE_1036_1158);
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1316");
            exit(1);
        }
    }
}
CursorTy doubleR_486(IntTy k1_204_906_1159, VectorTy *x1_205_907_1160,
                     CursorTy m0_206_908_1161, CursorTy t4_207_909_1162)
{
    TagTyPacked tag_1319 = *(TagTyPacked *) m0_206_908_1161;
    CursorTy tail_1320 = m0_206_908_1161 + sizeof(IntTy);
    
    
  switch_1321:
    ;
    switch (tag_1319) {
        
      case 1:
        {
            IntTy wildcard__167_209_910_1163 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1320)->field0;
            IntTy k2_210_911_1164 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1320)->field1;
            VectorTy *x2_211_912_1165 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1320)->field2;
            CursorTy t1_212_913_1166 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1320)->field3;
            CursorTy m1_213_914_1167 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1320)->field4;
            
            return caseFn_836(x1_205_907_1160, k1_204_906_1159, t4_207_909_1162,
                              m1_213_914_1167, k2_210_911_1164, x2_211_912_1165,
                              t1_212_913_1166);
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1319");
            exit(1);
        }
    }
}
CursorTy rotateR_484(IntTy k_263_915_1168, VectorTy *x_264_916_1169,
                     CursorTy l_265_917_1170, CursorTy r_266_918_1171)
{
    TagTyPacked tag_1322 = *(TagTyPacked *) l_265_917_1170;
    CursorTy tail_1323 = l_265_917_1170 + sizeof(IntTy);
    
    
  switch_1324:
    ;
    switch (tag_1322) {
        
      case 1:
        {
            IntTy wildcard__106_268_919_1172 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1323)->field0;
            IntTy wildcard__107_269_920_1173 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1323)->field1;
            VectorTy *wildcard__108_270_921_1174 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1323)->field2;
            CursorTy ly_271_922_1175 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1323)->field3;
            CursorTy ry_272_923_1176 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1323)->field4;
            IntTy fltPrm_1038_1177 =  size_482(ry_272_923_1176);
            IntTy fltPrm_1040_1178 =  ratio();
            IntTy fltPrm_1041_1179 =  size_482(ly_271_922_1175);
            IntTy fltPrm_1039_1180 = fltPrm_1040_1178 * fltPrm_1041_1179;
            BoolTy fltIf_1037_1181 = fltPrm_1038_1177 < fltPrm_1039_1180;
            
            if (fltIf_1037_1181) {
                return singleR_485(k_263_915_1168, x_264_916_1169,
                                   l_265_917_1170, r_266_918_1171);
            } else {
                return doubleR_486(k_263_915_1168, x_264_916_1169,
                                   l_265_917_1170, r_266_918_1171);
            }
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1322");
            exit(1);
        }
    }
}
CursorTy balance_481(IntTy k_283_924_1182, VectorTy *x_284_925_1183,
                     CursorTy l_285_926_1184, CursorTy r_286_927_1185)
{
    IntTy fltPrm_1044_1186 =  size_482(l_285_926_1184);
    IntTy fltPrm_1045_1187 =  size_482(r_286_927_1185);
    IntTy fltPrm_1043_1188 = fltPrm_1044_1186 + fltPrm_1045_1187;
    BoolTy fltIf_1042_1189 = fltPrm_1043_1188 <= 1;
    
    if (fltIf_1042_1189) {
        IntTy fltPrm_1047_1190 =  size_482(l_285_926_1184);
        IntTy fltPrm_1048_1191 =  size_482(r_286_927_1185);
        IntTy fltPkd_1046_1192 = fltPrm_1047_1190 + fltPrm_1048_1191;
        PtrTy tailift_1325 =
              ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
        
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1325)->field0 = 1;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1325)->field1 =
            fltPkd_1046_1192;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1325)->field2 =
            k_283_924_1182;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1325)->field3 =
            x_284_925_1183;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1325)->field4 =
            l_285_926_1184;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1325)->field5 =
            r_286_927_1185;
        return tailift_1325;
    } else {
        IntTy fltPrm_1050_1193 =  size_482(r_286_927_1185);
        IntTy fltPrm_1052_1194 =  delta();
        IntTy fltPrm_1053_1195 =  size_482(l_285_926_1184);
        IntTy fltPrm_1051_1196 = fltPrm_1052_1194 * fltPrm_1053_1195;
        BoolTy fltIf_1049_1197 = fltPrm_1050_1193 >= fltPrm_1051_1196;
        
        if (fltIf_1049_1197) {
            return rotateL_483(k_283_924_1182, x_284_925_1183, l_285_926_1184,
                               r_286_927_1185);
        } else {
            IntTy fltPrm_1055_1198 =  size_482(l_285_926_1184);
            IntTy fltPrm_1057_1199 =  delta();
            IntTy fltPrm_1058_1200 =  size_482(r_286_927_1185);
            IntTy fltPrm_1056_1201 = fltPrm_1057_1199 * fltPrm_1058_1200;
            BoolTy fltIf_1054_1202 = fltPrm_1055_1198 >= fltPrm_1056_1201;
            
            if (fltIf_1054_1202) {
                return rotateR_484(k_283_924_1182, x_284_925_1183,
                                   l_285_926_1184, r_286_927_1185);
            } else {
                IntTy fltPrm_1060_1203 =  size_482(l_285_926_1184);
                IntTy fltPrm_1061_1204 =  size_482(r_286_927_1185);
                IntTy fltPkd_1059_1205 = fltPrm_1060_1203 + fltPrm_1061_1204;
                PtrTy tailift_1326 =
                      ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
                
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1326)->field0 =
                    1;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1326)->field1 =
                    fltPkd_1059_1205;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1326)->field2 =
                    k_283_924_1182;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1326)->field3 =
                    x_284_925_1183;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1326)->field4 =
                    l_285_926_1184;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1326)->field5 =
                    r_286_927_1185;
                return tailift_1326;
            }
        }
    }
}
CursorTy insert_478(IntTy kx_288_928_1206, VectorTy *x_289_929_1207,
                    CursorTy m_290_930_1208)
{
    TagTyPacked tag_1327 = *(TagTyPacked *) m_290_930_1208;
    CursorTy tail_1328 = m_290_930_1208 + sizeof(IntTy);
    
    
  switch_1330:
    ;
    switch (tag_1327) {
        
      case 0:
        {
            return singleton_480(kx_288_928_1206, x_289_929_1207);
            break;
        }
        
      case 1:
        {
            IntTy sz_292_931_1209 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1328)->field0;
            IntTy k_293_932_1210 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1328)->field1;
            VectorTy *v_294_933_1211 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1328)->field2;
            CursorTy l_295_934_1212 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1328)->field3;
            CursorTy r_296_935_1213 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1328)->field4;
            BoolTy fltIf_1062_1214 = kx_288_928_1206 == k_293_932_1210;
            
            if (fltIf_1062_1214) {
                PtrTy tailift_1329 =
                      ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
                
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1329)->field0 =
                    1;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1329)->field1 =
                    sz_292_931_1209;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1329)->field2 =
                    k_293_932_1210;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1329)->field3 =
                    x_289_929_1207;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1329)->field4 =
                    l_295_934_1212;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1329)->field5 =
                    r_296_935_1213;
                return tailift_1329;
            } else {
                BoolTy fltIf_1063_1215 = kx_288_928_1206 <= k_293_932_1210;
                
                if (fltIf_1063_1215) {
                    CursorTy fltAppE_1064_1216 =
                              insert_478(kx_288_928_1206, x_289_929_1207, l_295_934_1212);
                    
                    return balance_481(k_293_932_1210, v_294_933_1211,
                                       fltAppE_1064_1216, r_296_935_1213);
                } else {
                    CursorTy fltAppE_1065_1217 =
                              insert_478(kx_288_928_1206, x_289_929_1207, r_296_935_1213);
                    
                    return balance_481(k_293_932_1210, v_294_933_1211,
                                       l_295_934_1212, fltAppE_1065_1217);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1327");
            exit(1);
        }
    }
}
CursorTy empty_477()
{
    PtrTy tailift_1331 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1331)->field0 = 0;
    return tailift_1331;
}
CursorTy _copy_without_ptrs_Map_v_479(CursorTy arg_786_936_1218)
{
    TagTyPacked tag_1332 = *(TagTyPacked *) arg_786_936_1218;
    CursorTy tail_1333 = arg_786_936_1218 + sizeof(IntTy);
    
    
  switch_1336:
    ;
    switch (tag_1332) {
        
      case 0:
        {
            PtrTy tailift_1334 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1334)->field0 = 0;
            return tailift_1334;
            break;
        }
        
      case 1:
        {
            IntTy x_787_937_1219 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1333)->field0;
            IntTy x_788_938_1220 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1333)->field1;
            VectorTy *x_789_939_1221 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1333)->field2;
            CursorTy x_790_940_1222 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1333)->field3;
            CursorTy x_791_941_1223 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1333)->field4;
            CursorTy y_795_945_1227 =
                      _copy_without_ptrs_Map_v_479(x_790_940_1222);
            CursorTy y_796_946_1228 =
                      _copy_without_ptrs_Map_v_479(x_791_941_1223);
            PtrTy tailift_1335 =
                  ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
            
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1335)->field0 =
                1;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1335)->field1 =
                x_787_937_1219;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1335)->field2 =
                x_788_938_1220;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1335)->field3 =
                x_789_939_1221;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1335)->field4 =
                y_795_945_1227;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1335)->field5 =
                y_796_946_1228;
            return tailift_1335;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1332");
            exit(1);
        }
    }
}
CursorTy _copy_Map_v_479(CursorTy arg_775_947_1229)
{
    TagTyPacked tag_1337 = *(TagTyPacked *) arg_775_947_1229;
    CursorTy tail_1338 = arg_775_947_1229 + sizeof(IntTy);
    
    
  switch_1341:
    ;
    switch (tag_1337) {
        
      case 0:
        {
            PtrTy tailift_1339 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1339)->field0 = 0;
            return tailift_1339;
            break;
        }
        
      case 1:
        {
            IntTy x_776_948_1230 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1338)->field0;
            IntTy x_777_949_1231 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1338)->field1;
            VectorTy *x_778_950_1232 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1338)->field2;
            CursorTy x_779_951_1233 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1338)->field3;
            CursorTy x_780_952_1234 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1338)->field4;
            CursorTy y_784_956_1238 =  _copy_Map_v_479(x_779_951_1233);
            CursorTy y_785_957_1239 =  _copy_Map_v_479(x_780_952_1234);
            PtrTy tailift_1340 =
                  ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
            
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1340)->field0 =
                1;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1340)->field1 =
                x_776_948_1230;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1340)->field2 =
                x_777_949_1231;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1340)->field3 =
                x_778_950_1232;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1340)->field4 =
                y_784_956_1238;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1340)->field5 =
                y_785_957_1239;
            return tailift_1340;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1337");
            exit(1);
        }
    }
}
unsigned char _traverse_Map_v_479(CursorTy arg_797_958_1240)
{
    TagTyPacked tag_1342 = *(TagTyPacked *) arg_797_958_1240;
    CursorTy tail_1343 = arg_797_958_1240 + sizeof(IntTy);
    
    
  switch_1344:
    ;
    switch (tag_1342) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_798_959_1241 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1343)->field0;
            IntTy x_799_960_1242 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1343)->field1;
            VectorTy *x_800_961_1243 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1343)->field2;
            CursorTy x_801_962_1244 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1343)->field3;
            CursorTy x_802_963_1245 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1343)->field4;
            unsigned char y_806_964_1246 =  _traverse_Map_v_479(x_801_962_1244);
            unsigned char y_807_965_1247 =  _traverse_Map_v_479(x_802_963_1245);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1342");
            exit(1);
        }
    }
}
unsigned char _print_Map_v_479(CursorTy arg_808_966_1248)
{
    TagTyPacked tag_1345 = *(TagTyPacked *) arg_808_966_1248;
    CursorTy tail_1346 = arg_808_966_1248 + sizeof(IntTy);
    
    
  switch_1347:
    ;
    switch (tag_1345) {
        
      case 0:
        {
            unsigned char wildcard_809_967_1249 = print_symbol(1299);
            unsigned char wildcard_810_968_1250 = print_symbol(1298);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_811_969_1251 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1346)->field0;
            IntTy x_812_970_1252 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1346)->field1;
            VectorTy *x_813_971_1253 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1346)->field2;
            CursorTy x_814_972_1254 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1346)->field3;
            CursorTy x_815_973_1255 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1346)->field4;
            unsigned char wildcard_821_974_1256 = print_symbol(1300);
            unsigned char wildcard_827_975_1257 = print_symbol(1301);
            unsigned char y_816_976_1258 = printf("%lld", x_811_969_1251);
            unsigned char wildcard_826_977_1259 = print_symbol(1301);
            unsigned char y_817_978_1260 = printf("%lld", x_812_970_1252);
            unsigned char wildcard_825_979_1261 = print_symbol(1301);
            unsigned char y_818_980_1262 = print_symbol(1297);
            unsigned char wildcard_824_981_1263 = print_symbol(1301);
            unsigned char y_819_982_1264 =  _print_Map_v_479(x_814_972_1254);
            unsigned char wildcard_823_983_1265 = print_symbol(1301);
            unsigned char y_820_984_1266 =  _print_Map_v_479(x_815_973_1255);
            unsigned char wildcard_822_985_1267 = print_symbol(1298);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1345");
            exit(1);
        }
    }
}
CursorTy caseFn_828(VectorTy *x1_220_829_986_1268, IntTy k1_219_830_987_1269,
                    CursorTy t1_221_831_988_1270, CursorTy m1_227_832_989_1271,
                    IntTy k2_225_833_990_1272, VectorTy *x2_226_834_991_1273,
                    CursorTy t4_228_835_992_1274)
{
    TagTyPacked tag_1348 = *(TagTyPacked *) m1_227_832_989_1271;
    CursorTy tail_1349 = m1_227_832_989_1271 + sizeof(IntTy);
    
    
  switch_1350:
    ;
    switch (tag_1348) {
        
      case 1:
        {
            IntTy wildcard__144_229_993_1275 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1349)->field0;
            IntTy k3_230_994_1276 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1349)->field1;
            VectorTy *x3_231_995_1277 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1349)->field2;
            CursorTy t2_232_996_1278 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1349)->field3;
            CursorTy t3_233_997_1279 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1349)->field4;
            CursorTy fltAppE_1066_1280 =
                      bin_487(k1_219_830_987_1269, x1_220_829_986_1268, t1_221_831_988_1270, t2_232_996_1278);
            CursorTy fltAppE_1067_1281 =
                      bin_487(k2_225_833_990_1272, x2_226_834_991_1273, t3_233_997_1279, t4_228_835_992_1274);
            
            return bin_487(k3_230_994_1276, x3_231_995_1277, fltAppE_1066_1280,
                           fltAppE_1067_1281);
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1348");
            exit(1);
        }
    }
}
CursorTy caseFn_836(VectorTy *x1_205_837_998_1282, IntTy k1_204_838_999_1283,
                    CursorTy t4_207_839_1000_1284,
                    CursorTy m1_213_840_1001_1285, IntTy k2_210_841_1002_1286,
                    VectorTy *x2_211_842_1003_1287,
                    CursorTy t1_212_843_1004_1288)
{
    TagTyPacked tag_1351 = *(TagTyPacked *) m1_213_840_1001_1285;
    CursorTy tail_1352 = m1_213_840_1001_1285 + sizeof(IntTy);
    
    
  switch_1353:
    ;
    switch (tag_1351) {
        
      case 1:
        {
            IntTy wildcard__168_214_1005_1289 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1352)->field0;
            IntTy k3_215_1006_1290 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1352)->field1;
            VectorTy *x3_216_1007_1291 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1352)->field2;
            CursorTy t2_217_1008_1292 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1352)->field3;
            CursorTy t3_218_1009_1293 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1352)->field4;
            CursorTy fltAppE_1068_1294 =
                      bin_487(k2_210_841_1002_1286, x2_211_842_1003_1287, t1_212_843_1004_1288, t2_217_1008_1292);
            CursorTy fltAppE_1069_1295 =
                      bin_487(k1_204_838_999_1283, x1_205_837_998_1282, t3_218_1009_1293, t4_207_839_1000_1284);
            
            return bin_487(k3_215_1006_1290, x3_216_1007_1291,
                           fltAppE_1068_1294, fltAppE_1069_1295);
            break;
        }
        
      case 0:
        {
            return empty_477();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1351");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1297, "Vector");
    add_symbol(1298, ")");
    add_symbol(1299, "(Tip_v_479");
    add_symbol(1300, "(Bin_v_479");
    add_symbol(1301, " ");
    
    IntTy tmp_13 = sizeof(CharTy);
    VectorTy *vec_179_187_844_1070 = vector_alloc(1, tmp_13);
    CharTy tmp_12 = 'f';
    VectorTy *__188_845_1071 = vector_inplace_update(vec_179_187_844_1070, 0,
                                                     &tmp_12);
    IntTy tmp_11 = sizeof(CharTy);
    VectorTy *vec_180_189_846_1073 = vector_alloc(1, tmp_11);
    CharTy tmp_10 = 'f';
    VectorTy *__190_847_1074 = vector_inplace_update(vec_180_189_846_1073, 0,
                                                     &tmp_10);
    IntTy tmp_9 = sizeof(CharTy);
    VectorTy *vec_181_191_848_1076 = vector_alloc(1, tmp_9);
    CharTy tmp_8 = 'e';
    VectorTy *__192_849_1077 = vector_inplace_update(vec_181_191_848_1076, 0,
                                                     &tmp_8);
    IntTy tmp_7 = sizeof(CharTy);
    VectorTy *vec_182_193_850_1079 = vector_alloc(1, tmp_7);
    CharTy tmp_6 = 'd';
    VectorTy *__194_851_1080 = vector_inplace_update(vec_182_193_850_1079, 0,
                                                     &tmp_6);
    IntTy tmp_5 = sizeof(CharTy);
    VectorTy *vec_183_195_852_1082 = vector_alloc(1, tmp_5);
    CharTy tmp_4 = 'c';
    VectorTy *__196_853_1083 = vector_inplace_update(vec_183_195_852_1082, 0,
                                                     &tmp_4);
    IntTy tmp_3 = sizeof(CharTy);
    VectorTy *vec_184_197_854_1085 = vector_alloc(1, tmp_3);
    CharTy tmp_2 = 'b';
    VectorTy *__198_855_1086 = vector_inplace_update(vec_184_197_854_1085, 0,
                                                     &tmp_2);
    IntTy tmp_1 = sizeof(CharTy);
    VectorTy *vec_185_199_856_1088 = vector_alloc(1, tmp_1);
    CharTy tmp_0 = 'a';
    VectorTy *__200_857_1089 = vector_inplace_update(vec_185_199_856_1088, 0,
                                                     &tmp_0);
    CursorTy fltAppE_1023_1091 =  empty_477();
    CursorTy fltAppE_1021_1092 =
              insert_478(0, vec_185_199_856_1088, fltAppE_1023_1091);
    CursorTy fltAppE_1019_1093 =
              insert_478(1, vec_184_197_854_1085, fltAppE_1021_1092);
    CursorTy fltAppE_1017_1094 =
              insert_478(2, vec_183_195_852_1082, fltAppE_1019_1093);
    CursorTy fltAppE_1015_1095 =
              insert_478(3, vec_182_193_850_1079, fltAppE_1017_1094);
    CursorTy fltAppE_1013_1096 =
              insert_478(4, vec_181_191_848_1076, fltAppE_1015_1095);
    CursorTy fltAppE_1011_1097 =
              insert_478(5, vec_180_189_846_1073, fltAppE_1013_1096);
    CursorTy tmp_app_1296 =
              insert_478(0, vec_179_187_844_1070, fltAppE_1011_1097);
    
     _print_Map_v_479(tmp_app_1296);
    printf("\n");
    free_symtable();
    return 0;
}