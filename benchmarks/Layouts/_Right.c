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
IntTy sumRight(CursorTy m_34_689_917);
CursorTy build(IntTy x_40_695_926, IntTy sz_41_696_927, CursorTy m_42_697_928);
IntTy ratio();
IntTy delta();
IntTy size_316(CursorTy m_173_698_938);
CursorTy singleL_323(IntTy k1_100_704_944, IntTy x1_101_705_945,
                     CursorTy t1_102_706_946, CursorTy m_103_707_947);
CursorTy doubleL_324(IntTy k1_71_713_954, IntTy x1_72_714_955,
                     CursorTy t1_73_715_956, CursorTy m0_74_716_957);
CursorTy rotateL_317(IntTy k_125_722_963, IntTy x_126_723_964,
                     CursorTy l_127_724_965, CursorTy r_128_725_966);
CursorTy bin_322(IntTy k_110_731_977, IntTy x_111_732_978,
                 CursorTy l_112_733_979, CursorTy r_113_734_980);
CursorTy singleR_319(IntTy k1_90_735_985, IntTy x1_91_736_986,
                     CursorTy m_92_737_987, CursorTy t3_93_738_988);
CursorTy doubleR_320(IntTy k1_56_744_995, IntTy x1_57_745_996,
                     CursorTy m0_58_746_997, CursorTy t4_59_747_998);
CursorTy empty_321();
CursorTy rotateR_318(IntTy k_115_753_1004, IntTy x_116_754_1005,
                     CursorTy l_117_755_1006, CursorTy r_118_756_1007);
CursorTy balance_315(IntTy k_135_762_1018, IntTy x_136_763_1019,
                     CursorTy l_137_764_1020, CursorTy r_138_765_1021);
CursorTy singleton_312(IntTy k_52_766_1042, IntTy x_53_767_1043);
CursorTy insert_314(IntTy kx_43_768_1046, IntTy x_44_769_1047,
                    CursorTy m_45_770_1048);
CursorTy _copy_without_ptrs_Map_v_313(CursorTy arg_624_776_1058);
CursorTy _copy_Map_v_313(CursorTy arg_613_787_1069);
unsigned char _traverse_Map_v_313(CursorTy arg_635_798_1080);
unsigned char _print_Map_v_313(CursorTy arg_646_806_1088);
CursorTy caseFn_666(IntTy x1_72_667_826_1108, IntTy k1_71_668_827_1109,
                    CursorTy t1_73_669_828_1110, CursorTy m1_79_670_829_1111,
                    IntTy k2_77_671_830_1112, IntTy x2_78_672_831_1113,
                    CursorTy t4_80_673_832_1114);
CursorTy caseFn_674(IntTy x1_57_675_838_1122, IntTy k1_56_676_839_1123,
                    CursorTy t4_59_677_840_1124, CursorTy m1_65_678_841_1125,
                    IntTy k2_62_679_842_1126, IntTy x2_63_680_843_1127,
                    CursorTy t1_64_681_844_1128);
IntTy sumRight(CursorTy m_34_689_917)
{
    TagTyPacked tag_1143 = *(TagTyPacked *) m_34_689_917;
    CursorTy tail_1144 = m_34_689_917 + sizeof(IntTy);
    
    
  switch_1146:
    ;
    switch (tag_1143) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy wildcard__1_35_690_918 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1144)->field0;
            IntTy wildcard__2_36_691_919 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1144)->field1;
            IntTy v_37_692_920 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1144)->field2;
            CursorTy l_38_693_921 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1144)->field3;
            CursorTy r_39_694_922 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1144)->field4;
            IntTy fltPrm_852_923 =  sumRight(r_39_694_922);
            IntTy fltPrm_851_924 = fltPrm_852_923 + v_37_692_920;
            IntTy fltPrm_853_925 =  sumRight(l_38_693_921);
            IntTy flt_1145 = fltPrm_851_924 + fltPrm_853_925;
            
            return flt_1145;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1143");
            exit(1);
        }
    }
}
CursorTy build(IntTy x_40_695_926, IntTy sz_41_696_927, CursorTy m_42_697_928)
{
    BoolTy fltIf_854_929 = sz_41_696_927 == 0;
    
    if (fltIf_854_929) {
        return m_42_697_928;
    } else {
        IntTy fltPrm_856_930 = sz_41_696_927 / 2;
        IntTy fltAppE_855_931 = x_40_695_926 - fltPrm_856_930;
        IntTy fltAppE_857_932 = sz_41_696_927 / 2;
        IntTy fltPrm_860_933 = sz_41_696_927 / 2;
        IntTy fltAppE_859_934 = x_40_695_926 + fltPrm_860_933;
        IntTy fltAppE_861_935 = sz_41_696_927 / 2;
        CursorTy fltAppE_862_936 =
                  insert_314(x_40_695_926, x_40_695_926, m_42_697_928);
        CursorTy fltAppE_858_937 =
                  build(fltAppE_859_934, fltAppE_861_935, fltAppE_862_936);
        
        return build(fltAppE_855_931, fltAppE_857_932, fltAppE_858_937);
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
IntTy size_316(CursorTy m_173_698_938)
{
    TagTyPacked tag_1147 = *(TagTyPacked *) m_173_698_938;
    CursorTy tail_1148 = m_173_698_938 + sizeof(IntTy);
    
    
  switch_1149:
    ;
    switch (tag_1147) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy sz_175_699_939 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1148)->field0;
            IntTy wildcard__18_176_700_940 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1148)->field1;
            IntTy wildcard__19_177_701_941 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1148)->field2;
            CursorTy wildcard__20_178_702_942 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1148)->field3;
            CursorTy wildcard__21_179_703_943 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1148)->field4;
            
            return sz_175_699_939;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1147");
            exit(1);
        }
    }
}
CursorTy singleL_323(IntTy k1_100_704_944, IntTy x1_101_705_945,
                     CursorTy t1_102_706_946, CursorTy m_103_707_947)
{
    TagTyPacked tag_1150 = *(TagTyPacked *) m_103_707_947;
    CursorTy tail_1151 = m_103_707_947 + sizeof(IntTy);
    
    
  switch_1152:
    ;
    switch (tag_1150) {
        
      case 1:
        {
            IntTy wildcard__123_105_708_948 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1151)->field0;
            IntTy k2_106_709_949 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1151)->field1;
            IntTy x2_107_710_950 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1151)->field2;
            CursorTy t2_108_711_951 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1151)->field3;
            CursorTy t3_109_712_952 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1151)->field4;
            CursorTy fltAppE_863_953 =
                      bin_322(k1_100_704_944, x1_101_705_945, t1_102_706_946, t2_108_711_951);
            
            return bin_322(k2_106_709_949, x2_107_710_950, fltAppE_863_953,
                           t3_109_712_952);
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1150");
            exit(1);
        }
    }
}
CursorTy doubleL_324(IntTy k1_71_713_954, IntTy x1_72_714_955,
                     CursorTy t1_73_715_956, CursorTy m0_74_716_957)
{
    TagTyPacked tag_1153 = *(TagTyPacked *) m0_74_716_957;
    CursorTy tail_1154 = m0_74_716_957 + sizeof(IntTy);
    
    
  switch_1155:
    ;
    switch (tag_1153) {
        
      case 1:
        {
            IntTy wildcard__143_76_717_958 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1154)->field0;
            IntTy k2_77_718_959 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1154)->field1;
            IntTy x2_78_719_960 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1154)->field2;
            CursorTy m1_79_720_961 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1154)->field3;
            CursorTy t4_80_721_962 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1154)->field4;
            
            return caseFn_666(x1_72_714_955, k1_71_713_954, t1_73_715_956,
                              m1_79_720_961, k2_77_718_959, x2_78_719_960,
                              t4_80_721_962);
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1153");
            exit(1);
        }
    }
}
CursorTy rotateL_317(IntTy k_125_722_963, IntTy x_126_723_964,
                     CursorTy l_127_724_965, CursorTy r_128_725_966)
{
    TagTyPacked tag_1156 = *(TagTyPacked *) r_128_725_966;
    CursorTy tail_1157 = r_128_725_966 + sizeof(IntTy);
    
    
  switch_1158:
    ;
    switch (tag_1156) {
        
      case 1:
        {
            IntTy wildcard__94_130_726_967 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1157)->field0;
            IntTy wildcard__95_131_727_968 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1157)->field1;
            IntTy wildcard__96_132_728_969 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1157)->field2;
            CursorTy ly_133_729_970 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1157)->field3;
            CursorTy ry_134_730_971 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1157)->field4;
            IntTy fltPrm_865_972 =  size_316(ly_133_729_970);
            IntTy fltPrm_867_973 =  ratio();
            IntTy fltPrm_868_974 =  size_316(ry_134_730_971);
            IntTy fltPrm_866_975 = fltPrm_867_973 * fltPrm_868_974;
            BoolTy fltIf_864_976 = fltPrm_865_972 < fltPrm_866_975;
            
            if (fltIf_864_976) {
                return singleL_323(k_125_722_963, x_126_723_964, l_127_724_965,
                                   r_128_725_966);
            } else {
                return doubleL_324(k_125_722_963, x_126_723_964, l_127_724_965,
                                   r_128_725_966);
            }
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1156");
            exit(1);
        }
    }
}
CursorTy bin_322(IntTy k_110_731_977, IntTy x_111_732_978,
                 CursorTy l_112_733_979, CursorTy r_113_734_980)
{
    IntTy fltPrm_871_981 =  size_316(l_112_733_979);
    IntTy fltPrm_872_982 =  size_316(r_113_734_980);
    IntTy fltPrm_870_983 = fltPrm_871_981 + fltPrm_872_982;
    IntTy fltPkd_869_984 = fltPrm_870_983 + 1;
    PtrTy tailift_1159 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1159)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1159)->field1 =
        fltPkd_869_984;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1159)->field2 =
        k_110_731_977;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1159)->field3 =
        x_111_732_978;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1159)->field4 =
        l_112_733_979;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1159)->field5 =
        r_113_734_980;
    return tailift_1159;
}
CursorTy singleR_319(IntTy k1_90_735_985, IntTy x1_91_736_986,
                     CursorTy m_92_737_987, CursorTy t3_93_738_988)
{
    TagTyPacked tag_1160 = *(TagTyPacked *) m_92_737_987;
    CursorTy tail_1161 = m_92_737_987 + sizeof(IntTy);
    
    
  switch_1162:
    ;
    switch (tag_1160) {
        
      case 1:
        {
            IntTy wildcard__133_95_739_989 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1161)->field0;
            IntTy k2_96_740_990 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1161)->field1;
            IntTy x2_97_741_991 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1161)->field2;
            CursorTy t1_98_742_992 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1161)->field3;
            CursorTy t2_99_743_993 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1161)->field4;
            CursorTy fltAppE_873_994 =
                      bin_322(k1_90_735_985, x1_91_736_986, t2_99_743_993, t3_93_738_988);
            
            return bin_322(k2_96_740_990, x2_97_741_991, t1_98_742_992,
                           fltAppE_873_994);
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1160");
            exit(1);
        }
    }
}
CursorTy doubleR_320(IntTy k1_56_744_995, IntTy x1_57_745_996,
                     CursorTy m0_58_746_997, CursorTy t4_59_747_998)
{
    TagTyPacked tag_1163 = *(TagTyPacked *) m0_58_746_997;
    CursorTy tail_1164 = m0_58_746_997 + sizeof(IntTy);
    
    
  switch_1165:
    ;
    switch (tag_1163) {
        
      case 1:
        {
            IntTy wildcard__167_61_748_999 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1164)->field0;
            IntTy k2_62_749_1000 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1164)->field1;
            IntTy x2_63_750_1001 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1164)->field2;
            CursorTy t1_64_751_1002 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1164)->field3;
            CursorTy m1_65_752_1003 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1164)->field4;
            
            return caseFn_674(x1_57_745_996, k1_56_744_995, t4_59_747_998,
                              m1_65_752_1003, k2_62_749_1000, x2_63_750_1001,
                              t1_64_751_1002);
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1163");
            exit(1);
        }
    }
}
CursorTy empty_321()
{
    PtrTy tailift_1166 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1166)->field0 = 0;
    return tailift_1166;
}
CursorTy rotateR_318(IntTy k_115_753_1004, IntTy x_116_754_1005,
                     CursorTy l_117_755_1006, CursorTy r_118_756_1007)
{
    TagTyPacked tag_1167 = *(TagTyPacked *) l_117_755_1006;
    CursorTy tail_1168 = l_117_755_1006 + sizeof(IntTy);
    
    
  switch_1169:
    ;
    switch (tag_1167) {
        
      case 1:
        {
            IntTy wildcard__106_120_757_1008 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1168)->field0;
            IntTy wildcard__107_121_758_1009 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1168)->field1;
            IntTy wildcard__108_122_759_1010 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1168)->field2;
            CursorTy ly_123_760_1011 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1168)->field3;
            CursorTy ry_124_761_1012 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1168)->field4;
            IntTy fltPrm_875_1013 =  size_316(ry_124_761_1012);
            IntTy fltPrm_877_1014 =  ratio();
            IntTy fltPrm_878_1015 =  size_316(ly_123_760_1011);
            IntTy fltPrm_876_1016 = fltPrm_877_1014 * fltPrm_878_1015;
            BoolTy fltIf_874_1017 = fltPrm_875_1013 < fltPrm_876_1016;
            
            if (fltIf_874_1017) {
                return singleR_319(k_115_753_1004, x_116_754_1005,
                                   l_117_755_1006, r_118_756_1007);
            } else {
                return doubleR_320(k_115_753_1004, x_116_754_1005,
                                   l_117_755_1006, r_118_756_1007);
            }
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1167");
            exit(1);
        }
    }
}
CursorTy balance_315(IntTy k_135_762_1018, IntTy x_136_763_1019,
                     CursorTy l_137_764_1020, CursorTy r_138_765_1021)
{
    IntTy fltPrm_881_1022 =  size_316(l_137_764_1020);
    IntTy fltPrm_882_1023 =  size_316(r_138_765_1021);
    IntTy fltPrm_880_1024 = fltPrm_881_1022 + fltPrm_882_1023;
    BoolTy fltIf_879_1025 = fltPrm_880_1024 <= 1;
    
    if (fltIf_879_1025) {
        IntTy fltPrm_884_1026 =  size_316(l_137_764_1020);
        IntTy fltPrm_885_1027 =  size_316(r_138_765_1021);
        IntTy fltPkd_883_1028 = fltPrm_884_1026 + fltPrm_885_1027;
        PtrTy tailift_1170 =
              ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
        
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1170)->field0 = 1;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1170)->field1 =
            fltPkd_883_1028;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1170)->field2 =
            k_135_762_1018;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1170)->field3 =
            x_136_763_1019;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1170)->field4 =
            l_137_764_1020;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1170)->field5 =
            r_138_765_1021;
        return tailift_1170;
    } else {
        IntTy fltPrm_887_1029 =  size_316(r_138_765_1021);
        IntTy fltPrm_889_1030 =  delta();
        IntTy fltPrm_890_1031 =  size_316(l_137_764_1020);
        IntTy fltPrm_888_1032 = fltPrm_889_1030 * fltPrm_890_1031;
        BoolTy fltIf_886_1033 = fltPrm_887_1029 >= fltPrm_888_1032;
        
        if (fltIf_886_1033) {
            return rotateL_317(k_135_762_1018, x_136_763_1019, l_137_764_1020,
                               r_138_765_1021);
        } else {
            IntTy fltPrm_892_1034 =  size_316(l_137_764_1020);
            IntTy fltPrm_894_1035 =  delta();
            IntTy fltPrm_895_1036 =  size_316(r_138_765_1021);
            IntTy fltPrm_893_1037 = fltPrm_894_1035 * fltPrm_895_1036;
            BoolTy fltIf_891_1038 = fltPrm_892_1034 >= fltPrm_893_1037;
            
            if (fltIf_891_1038) {
                return rotateR_318(k_135_762_1018, x_136_763_1019,
                                   l_137_764_1020, r_138_765_1021);
            } else {
                IntTy fltPrm_897_1039 =  size_316(l_137_764_1020);
                IntTy fltPrm_898_1040 =  size_316(r_138_765_1021);
                IntTy fltPkd_896_1041 = fltPrm_897_1039 + fltPrm_898_1040;
                PtrTy tailift_1171 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1171)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1171)->field1 =
                    fltPkd_896_1041;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1171)->field2 =
                    k_135_762_1018;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1171)->field3 =
                    x_136_763_1019;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1171)->field4 =
                    l_137_764_1020;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1171)->field5 =
                    r_138_765_1021;
                return tailift_1171;
            }
        }
    }
}
CursorTy singleton_312(IntTy k_52_766_1042, IntTy x_53_767_1043)
{
    PtrTy fltPkd_899_1044 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_899_1044)->field0 = 0;
    
    PtrTy fltPkd_900_1045 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_900_1045)->field0 = 0;
    
    PtrTy tailift_1172 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1172)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1172)->field1 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1172)->field2 =
        k_52_766_1042;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1172)->field3 =
        x_53_767_1043;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1172)->field4 =
        fltPkd_899_1044;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1172)->field5 =
        fltPkd_900_1045;
    return tailift_1172;
}
CursorTy insert_314(IntTy kx_43_768_1046, IntTy x_44_769_1047,
                    CursorTy m_45_770_1048)
{
    TagTyPacked tag_1173 = *(TagTyPacked *) m_45_770_1048;
    CursorTy tail_1174 = m_45_770_1048 + sizeof(IntTy);
    
    
  switch_1176:
    ;
    switch (tag_1173) {
        
      case 0:
        {
            return singleton_312(kx_43_768_1046, x_44_769_1047);
            break;
        }
        
      case 1:
        {
            IntTy sz_47_771_1049 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1174)->field0;
            IntTy k_48_772_1050 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1174)->field1;
            IntTy v_49_773_1051 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1174)->field2;
            CursorTy l_50_774_1052 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1174)->field3;
            CursorTy r_51_775_1053 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1174)->field4;
            BoolTy fltIf_901_1054 = kx_43_768_1046 == k_48_772_1050;
            
            if (fltIf_901_1054) {
                PtrTy tailift_1175 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1175)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1175)->field1 =
                    sz_47_771_1049;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1175)->field2 =
                    k_48_772_1050;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1175)->field3 =
                    x_44_769_1047;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1175)->field4 =
                    l_50_774_1052;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1175)->field5 =
                    r_51_775_1053;
                return tailift_1175;
            } else {
                BoolTy fltIf_902_1055 = kx_43_768_1046 <= k_48_772_1050;
                
                if (fltIf_902_1055) {
                    CursorTy fltAppE_903_1056 =
                              insert_314(kx_43_768_1046, x_44_769_1047, l_50_774_1052);
                    
                    return balance_315(k_48_772_1050, v_49_773_1051,
                                       fltAppE_903_1056, r_51_775_1053);
                } else {
                    CursorTy fltAppE_904_1057 =
                              insert_314(kx_43_768_1046, x_44_769_1047, r_51_775_1053);
                    
                    return balance_315(k_48_772_1050, v_49_773_1051,
                                       l_50_774_1052, fltAppE_904_1057);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1173");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Map_v_313(CursorTy arg_624_776_1058)
{
    TagTyPacked tag_1177 = *(TagTyPacked *) arg_624_776_1058;
    CursorTy tail_1178 = arg_624_776_1058 + sizeof(IntTy);
    
    
  switch_1181:
    ;
    switch (tag_1177) {
        
      case 0:
        {
            PtrTy tailift_1179 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1179)->field0 = 0;
            return tailift_1179;
            break;
        }
        
      case 1:
        {
            IntTy x_625_777_1059 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1178)->field0;
            IntTy x_626_778_1060 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1178)->field1;
            IntTy x_627_779_1061 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1178)->field2;
            CursorTy x_628_780_1062 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1178)->field3;
            CursorTy x_629_781_1063 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1178)->field4;
            CursorTy y_633_785_1067 =
                      _copy_without_ptrs_Map_v_313(x_628_780_1062);
            CursorTy y_634_786_1068 =
                      _copy_without_ptrs_Map_v_313(x_629_781_1063);
            PtrTy tailift_1180 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1180)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1180)->field1 =
                x_625_777_1059;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1180)->field2 =
                x_626_778_1060;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1180)->field3 =
                x_627_779_1061;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1180)->field4 =
                y_633_785_1067;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1180)->field5 =
                y_634_786_1068;
            return tailift_1180;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1177");
            exit(1);
        }
    }
}
CursorTy _copy_Map_v_313(CursorTy arg_613_787_1069)
{
    TagTyPacked tag_1182 = *(TagTyPacked *) arg_613_787_1069;
    CursorTy tail_1183 = arg_613_787_1069 + sizeof(IntTy);
    
    
  switch_1186:
    ;
    switch (tag_1182) {
        
      case 0:
        {
            PtrTy tailift_1184 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1184)->field0 = 0;
            return tailift_1184;
            break;
        }
        
      case 1:
        {
            IntTy x_614_788_1070 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1183)->field0;
            IntTy x_615_789_1071 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1183)->field1;
            IntTy x_616_790_1072 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1183)->field2;
            CursorTy x_617_791_1073 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1183)->field3;
            CursorTy x_618_792_1074 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1183)->field4;
            CursorTy y_622_796_1078 =  _copy_Map_v_313(x_617_791_1073);
            CursorTy y_623_797_1079 =  _copy_Map_v_313(x_618_792_1074);
            PtrTy tailift_1185 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1185)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1185)->field1 =
                x_614_788_1070;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1185)->field2 =
                x_615_789_1071;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1185)->field3 =
                x_616_790_1072;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1185)->field4 =
                y_622_796_1078;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1185)->field5 =
                y_623_797_1079;
            return tailift_1185;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1182");
            exit(1);
        }
    }
}
unsigned char _traverse_Map_v_313(CursorTy arg_635_798_1080)
{
    TagTyPacked tag_1187 = *(TagTyPacked *) arg_635_798_1080;
    CursorTy tail_1188 = arg_635_798_1080 + sizeof(IntTy);
    
    
  switch_1189:
    ;
    switch (tag_1187) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_636_799_1081 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1188)->field0;
            IntTy x_637_800_1082 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1188)->field1;
            IntTy x_638_801_1083 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1188)->field2;
            CursorTy x_639_802_1084 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1188)->field3;
            CursorTy x_640_803_1085 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1188)->field4;
            unsigned char y_644_804_1086 =  _traverse_Map_v_313(x_639_802_1084);
            unsigned char y_645_805_1087 =  _traverse_Map_v_313(x_640_803_1085);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1187");
            exit(1);
        }
    }
}
unsigned char _print_Map_v_313(CursorTy arg_646_806_1088)
{
    TagTyPacked tag_1190 = *(TagTyPacked *) arg_646_806_1088;
    CursorTy tail_1191 = arg_646_806_1088 + sizeof(IntTy);
    
    
  switch_1192:
    ;
    switch (tag_1190) {
        
      case 0:
        {
            unsigned char wildcard_647_807_1089 = print_symbol(1139);
            unsigned char wildcard_648_808_1090 = print_symbol(1138);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_649_809_1091 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1191)->field0;
            IntTy x_650_810_1092 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1191)->field1;
            IntTy x_651_811_1093 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1191)->field2;
            CursorTy x_652_812_1094 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1191)->field3;
            CursorTy x_653_813_1095 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1191)->field4;
            unsigned char wildcard_659_814_1096 = print_symbol(1140);
            unsigned char wildcard_665_815_1097 = print_symbol(1141);
            unsigned char y_654_816_1098 = printf("%lld", x_649_809_1091);
            unsigned char wildcard_664_817_1099 = print_symbol(1141);
            unsigned char y_655_818_1100 = printf("%lld", x_650_810_1092);
            unsigned char wildcard_663_819_1101 = print_symbol(1141);
            unsigned char y_656_820_1102 = printf("%lld", x_651_811_1093);
            unsigned char wildcard_662_821_1103 = print_symbol(1141);
            unsigned char y_657_822_1104 =  _print_Map_v_313(x_652_812_1094);
            unsigned char wildcard_661_823_1105 = print_symbol(1141);
            unsigned char y_658_824_1106 =  _print_Map_v_313(x_653_813_1095);
            unsigned char wildcard_660_825_1107 = print_symbol(1138);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1190");
            exit(1);
        }
    }
}
CursorTy caseFn_666(IntTy x1_72_667_826_1108, IntTy k1_71_668_827_1109,
                    CursorTy t1_73_669_828_1110, CursorTy m1_79_670_829_1111,
                    IntTy k2_77_671_830_1112, IntTy x2_78_672_831_1113,
                    CursorTy t4_80_673_832_1114)
{
    TagTyPacked tag_1193 = *(TagTyPacked *) m1_79_670_829_1111;
    CursorTy tail_1194 = m1_79_670_829_1111 + sizeof(IntTy);
    
    
  switch_1195:
    ;
    switch (tag_1193) {
        
      case 1:
        {
            IntTy wildcard__144_81_833_1115 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1194)->field0;
            IntTy k3_82_834_1116 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1194)->field1;
            IntTy x3_83_835_1117 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1194)->field2;
            CursorTy t2_84_836_1118 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1194)->field3;
            CursorTy t3_85_837_1119 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1194)->field4;
            CursorTy fltAppE_905_1120 =
                      bin_322(k1_71_668_827_1109, x1_72_667_826_1108, t1_73_669_828_1110, t2_84_836_1118);
            CursorTy fltAppE_906_1121 =
                      bin_322(k2_77_671_830_1112, x2_78_672_831_1113, t3_85_837_1119, t4_80_673_832_1114);
            
            return bin_322(k3_82_834_1116, x3_83_835_1117, fltAppE_905_1120,
                           fltAppE_906_1121);
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1193");
            exit(1);
        }
    }
}
CursorTy caseFn_674(IntTy x1_57_675_838_1122, IntTy k1_56_676_839_1123,
                    CursorTy t4_59_677_840_1124, CursorTy m1_65_678_841_1125,
                    IntTy k2_62_679_842_1126, IntTy x2_63_680_843_1127,
                    CursorTy t1_64_681_844_1128)
{
    TagTyPacked tag_1196 = *(TagTyPacked *) m1_65_678_841_1125;
    CursorTy tail_1197 = m1_65_678_841_1125 + sizeof(IntTy);
    
    
  switch_1198:
    ;
    switch (tag_1196) {
        
      case 1:
        {
            IntTy wildcard__168_66_845_1129 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1197)->field0;
            IntTy k3_67_846_1130 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1197)->field1;
            IntTy x3_68_847_1131 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1197)->field2;
            CursorTy t2_69_848_1132 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1197)->field3;
            CursorTy t3_70_849_1133 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1197)->field4;
            CursorTy fltAppE_907_1134 =
                      bin_322(k2_62_679_842_1126, x2_63_680_843_1127, t1_64_681_844_1128, t2_69_848_1132);
            CursorTy fltAppE_908_1135 =
                      bin_322(k1_56_676_839_1123, x1_57_675_838_1122, t3_70_849_1133, t4_59_677_840_1124);
            
            return bin_322(k3_67_846_1130, x3_68_847_1131, fltAppE_907_1134,
                           fltAppE_908_1135);
            break;
        }
        
      case 0:
        {
            return empty_321();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1196");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1138, ")");
    add_symbol(1139, "(Tip_v_313");
    add_symbol(1140, "(Bin_v_313");
    add_symbol(1141, " ");
    add_symbol(1142, "\n");
    
    CursorTy fltAppE_850_909 =  singleton_312(0, 0);
    CursorTy m_25_682_910 =  build(0, 512, fltAppE_850_909);
    IntTy timed_1136;
    VectorTy *times_3 = vector_alloc(global_iters_param, sizeof(double));
    struct timespec begin_timed_1136;
    struct timespec end_timed_1136;
    
    for (long long iters_timed_1136 = 0; iters_timed_1136 < global_iters_param;
         iters_timed_1136++) {
        if (iters_timed_1136 != global_iters_param - 1)
            save_alloc_state();
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1136);
        
        IntTy timed_1136hack =  sumRight(m_25_682_910);
        
        timed_1136 = timed_1136hack;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1136);
        if (iters_timed_1136 != global_iters_param - 1)
            restore_alloc_state();
        
        double itertime_0 = difftimespecs(&begin_timed_1136, &end_timed_1136);
        
        vector_inplace_update(times_3, iters_timed_1136, &itertime_0);
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
    
    unsigned char wildcard__19_27_684_912 = printf("%lld", timed_1136);
    unsigned char wildcard__17_28_685_913 = print_symbol(1142);
    IntTy timed_1137;
    VectorTy *times_8 = vector_alloc(global_iters_param, sizeof(double));
    struct timespec begin_timed_1137;
    struct timespec end_timed_1137;
    
    for (long long iters_timed_1137 = 0; iters_timed_1137 < global_iters_param;
         iters_timed_1137++) {
        if (iters_timed_1137 != global_iters_param - 1)
            save_alloc_state();
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1137);
        
        IntTy timed_1137hack =  sumRight(m_25_682_910);
        
        timed_1137 = timed_1137hack;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1137);
        if (iters_timed_1137 != global_iters_param - 1)
            restore_alloc_state();
        
        double itertime_5 = difftimespecs(&begin_timed_1137, &end_timed_1137);
        
        vector_inplace_update(times_8, iters_timed_1137, &itertime_5);
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
    
    unsigned char wildcard__13_30_687_915 = printf("%lld", timed_1137);
    unsigned char wildcard__11_31_688_916 = print_symbol(1142);
    
    printf("'#()");
    printf("\n");
    free_symtable();
    return 0;
}