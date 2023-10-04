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
IntTy sumRight(CursorTy m_42_711_948);
CursorTy build(IntTy x_48_717_957, IntTy sz_49_718_958, CursorTy m_50_719_959);
IntTy sumLeft(CursorTy m_51_720_969);
IntTy ratio();
IntTy delta();
IntTy size_332(CursorTy m_187_726_978);
CursorTy singleL_339(IntTy k1_114_732_984, IntTy x1_115_733_985,
                     CursorTy t1_116_734_986, CursorTy m_117_735_987);
CursorTy doubleL_340(IntTy k1_85_741_994, IntTy x1_86_742_995,
                     CursorTy t1_87_743_996, CursorTy m0_88_744_997);
CursorTy rotateL_333(IntTy k_139_750_1003, IntTy x_140_751_1004,
                     CursorTy l_141_752_1005, CursorTy r_142_753_1006);
CursorTy bin_338(IntTy k_124_759_1017, IntTy x_125_760_1018,
                 CursorTy l_126_761_1019, CursorTy r_127_762_1020);
CursorTy singleR_335(IntTy k1_104_763_1025, IntTy x1_105_764_1026,
                     CursorTy m_106_765_1027, CursorTy t3_107_766_1028);
CursorTy doubleR_336(IntTy k1_70_772_1035, IntTy x1_71_773_1036,
                     CursorTy m0_72_774_1037, CursorTy t4_73_775_1038);
CursorTy empty_337();
CursorTy rotateR_334(IntTy k_129_781_1044, IntTy x_130_782_1045,
                     CursorTy l_131_783_1046, CursorTy r_132_784_1047);
CursorTy balance_331(IntTy k_149_790_1058, IntTy x_150_791_1059,
                     CursorTy l_151_792_1060, CursorTy r_152_793_1061);
CursorTy singleton_328(IntTy k_66_794_1082, IntTy x_67_795_1083);
CursorTy insert_330(IntTy kx_57_796_1086, IntTy x_58_797_1087,
                    CursorTy m_59_798_1088);
CursorTy _copy_without_ptrs_Map_v_329(CursorTy arg_646_804_1098);
CursorTy _copy_Map_v_329(CursorTy arg_635_815_1109);
unsigned char _traverse_Map_v_329(CursorTy arg_657_826_1120);
unsigned char _print_Map_v_329(CursorTy arg_668_834_1128);
CursorTy caseFn_688(IntTy x1_86_689_854_1148, IntTy k1_85_690_855_1149,
                    CursorTy t1_87_691_856_1150, CursorTy m1_93_692_857_1151,
                    IntTy k2_91_693_858_1152, IntTy x2_92_694_859_1153,
                    CursorTy t4_94_695_860_1154);
CursorTy caseFn_696(IntTy x1_71_697_866_1162, IntTy k1_70_698_867_1163,
                    CursorTy t4_73_699_868_1164, CursorTy m1_79_700_869_1165,
                    IntTy k2_76_701_870_1166, IntTy x2_77_702_871_1167,
                    CursorTy t1_78_703_872_1168);
IntTy sumRight(CursorTy m_42_711_948)
{
    TagTyPacked tag_1183 = *(TagTyPacked *) m_42_711_948;
    CursorTy tail_1184 = m_42_711_948 + sizeof(IntTy);
    
    
  switch_1186:
    ;
    switch (tag_1183) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy wildcard__1_43_712_949 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1184)->field0;
            IntTy wildcard__2_44_713_950 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1184)->field1;
            IntTy v_45_714_951 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1184)->field2;
            CursorTy l_46_715_952 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1184)->field3;
            CursorTy r_47_716_953 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1184)->field4;
            IntTy fltPrm_880_954 =  sumRight(r_47_716_953);
            IntTy fltPrm_879_955 = fltPrm_880_954 + v_45_714_951;
            IntTy fltPrm_881_956 =  sumRight(l_46_715_952);
            IntTy flt_1185 = fltPrm_879_955 + fltPrm_881_956;
            
            return flt_1185;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1183");
            exit(1);
        }
    }
}
CursorTy build(IntTy x_48_717_957, IntTy sz_49_718_958, CursorTy m_50_719_959)
{
    BoolTy fltIf_882_960 = sz_49_718_958 == 0;
    
    if (fltIf_882_960) {
        return m_50_719_959;
    } else {
        IntTy fltPrm_884_961 = sz_49_718_958 / 2;
        IntTy fltAppE_883_962 = x_48_717_957 - fltPrm_884_961;
        IntTy fltAppE_885_963 = sz_49_718_958 / 2;
        IntTy fltPrm_888_964 = sz_49_718_958 / 2;
        IntTy fltAppE_887_965 = x_48_717_957 + fltPrm_888_964;
        IntTy fltAppE_889_966 = sz_49_718_958 / 2;
        CursorTy fltAppE_890_967 =
                  insert_330(x_48_717_957, x_48_717_957, m_50_719_959);
        CursorTy fltAppE_886_968 =
                  build(fltAppE_887_965, fltAppE_889_966, fltAppE_890_967);
        
        return build(fltAppE_883_962, fltAppE_885_963, fltAppE_886_968);
    }
}
IntTy sumLeft(CursorTy m_51_720_969)
{
    TagTyPacked tag_1187 = *(TagTyPacked *) m_51_720_969;
    CursorTy tail_1188 = m_51_720_969 + sizeof(IntTy);
    
    
  switch_1190:
    ;
    switch (tag_1187) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy wildcard__9_52_721_970 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1188)->field0;
            IntTy wildcard__10_53_722_971 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1188)->field1;
            IntTy v_54_723_972 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1188)->field2;
            CursorTy l_55_724_973 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1188)->field3;
            CursorTy r_56_725_974 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1188)->field4;
            IntTy fltPrm_892_975 =  sumLeft(l_55_724_973);
            IntTy fltPrm_891_976 = fltPrm_892_975 + v_54_723_972;
            IntTy fltPrm_893_977 =  sumLeft(r_56_725_974);
            IntTy flt_1189 = fltPrm_891_976 + fltPrm_893_977;
            
            return flt_1189;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1187");
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
IntTy size_332(CursorTy m_187_726_978)
{
    TagTyPacked tag_1191 = *(TagTyPacked *) m_187_726_978;
    CursorTy tail_1192 = m_187_726_978 + sizeof(IntTy);
    
    
  switch_1193:
    ;
    switch (tag_1191) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy sz_189_727_979 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1192)->field0;
            IntTy wildcard__18_190_728_980 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1192)->field1;
            IntTy wildcard__19_191_729_981 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1192)->field2;
            CursorTy wildcard__20_192_730_982 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1192)->field3;
            CursorTy wildcard__21_193_731_983 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1192)->field4;
            
            return sz_189_727_979;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1191");
            exit(1);
        }
    }
}
CursorTy singleL_339(IntTy k1_114_732_984, IntTy x1_115_733_985,
                     CursorTy t1_116_734_986, CursorTy m_117_735_987)
{
    TagTyPacked tag_1194 = *(TagTyPacked *) m_117_735_987;
    CursorTy tail_1195 = m_117_735_987 + sizeof(IntTy);
    
    
  switch_1196:
    ;
    switch (tag_1194) {
        
      case 1:
        {
            IntTy wildcard__123_119_736_988 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1195)->field0;
            IntTy k2_120_737_989 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1195)->field1;
            IntTy x2_121_738_990 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1195)->field2;
            CursorTy t2_122_739_991 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1195)->field3;
            CursorTy t3_123_740_992 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1195)->field4;
            CursorTy fltAppE_894_993 =
                      bin_338(k1_114_732_984, x1_115_733_985, t1_116_734_986, t2_122_739_991);
            
            return bin_338(k2_120_737_989, x2_121_738_990, fltAppE_894_993,
                           t3_123_740_992);
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1194");
            exit(1);
        }
    }
}
CursorTy doubleL_340(IntTy k1_85_741_994, IntTy x1_86_742_995,
                     CursorTy t1_87_743_996, CursorTy m0_88_744_997)
{
    TagTyPacked tag_1197 = *(TagTyPacked *) m0_88_744_997;
    CursorTy tail_1198 = m0_88_744_997 + sizeof(IntTy);
    
    
  switch_1199:
    ;
    switch (tag_1197) {
        
      case 1:
        {
            IntTy wildcard__143_90_745_998 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1198)->field0;
            IntTy k2_91_746_999 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1198)->field1;
            IntTy x2_92_747_1000 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1198)->field2;
            CursorTy m1_93_748_1001 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1198)->field3;
            CursorTy t4_94_749_1002 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1198)->field4;
            
            return caseFn_688(x1_86_742_995, k1_85_741_994, t1_87_743_996,
                              m1_93_748_1001, k2_91_746_999, x2_92_747_1000,
                              t4_94_749_1002);
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1197");
            exit(1);
        }
    }
}
CursorTy rotateL_333(IntTy k_139_750_1003, IntTy x_140_751_1004,
                     CursorTy l_141_752_1005, CursorTy r_142_753_1006)
{
    TagTyPacked tag_1200 = *(TagTyPacked *) r_142_753_1006;
    CursorTy tail_1201 = r_142_753_1006 + sizeof(IntTy);
    
    
  switch_1202:
    ;
    switch (tag_1200) {
        
      case 1:
        {
            IntTy wildcard__94_144_754_1007 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1201)->field0;
            IntTy wildcard__95_145_755_1008 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1201)->field1;
            IntTy wildcard__96_146_756_1009 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1201)->field2;
            CursorTy ly_147_757_1010 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1201)->field3;
            CursorTy ry_148_758_1011 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1201)->field4;
            IntTy fltPrm_896_1012 =  size_332(ly_147_757_1010);
            IntTy fltPrm_898_1013 =  ratio();
            IntTy fltPrm_899_1014 =  size_332(ry_148_758_1011);
            IntTy fltPrm_897_1015 = fltPrm_898_1013 * fltPrm_899_1014;
            BoolTy fltIf_895_1016 = fltPrm_896_1012 < fltPrm_897_1015;
            
            if (fltIf_895_1016) {
                return singleL_339(k_139_750_1003, x_140_751_1004,
                                   l_141_752_1005, r_142_753_1006);
            } else {
                return doubleL_340(k_139_750_1003, x_140_751_1004,
                                   l_141_752_1005, r_142_753_1006);
            }
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1200");
            exit(1);
        }
    }
}
CursorTy bin_338(IntTy k_124_759_1017, IntTy x_125_760_1018,
                 CursorTy l_126_761_1019, CursorTy r_127_762_1020)
{
    IntTy fltPrm_902_1021 =  size_332(l_126_761_1019);
    IntTy fltPrm_903_1022 =  size_332(r_127_762_1020);
    IntTy fltPrm_901_1023 = fltPrm_902_1021 + fltPrm_903_1022;
    IntTy fltPkd_900_1024 = fltPrm_901_1023 + 1;
    PtrTy tailift_1203 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1203)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1203)->field1 =
        fltPkd_900_1024;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1203)->field2 =
        k_124_759_1017;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1203)->field3 =
        x_125_760_1018;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1203)->field4 =
        l_126_761_1019;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1203)->field5 =
        r_127_762_1020;
    return tailift_1203;
}
CursorTy singleR_335(IntTy k1_104_763_1025, IntTy x1_105_764_1026,
                     CursorTy m_106_765_1027, CursorTy t3_107_766_1028)
{
    TagTyPacked tag_1204 = *(TagTyPacked *) m_106_765_1027;
    CursorTy tail_1205 = m_106_765_1027 + sizeof(IntTy);
    
    
  switch_1206:
    ;
    switch (tag_1204) {
        
      case 1:
        {
            IntTy wildcard__133_109_767_1029 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1205)->field0;
            IntTy k2_110_768_1030 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1205)->field1;
            IntTy x2_111_769_1031 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1205)->field2;
            CursorTy t1_112_770_1032 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1205)->field3;
            CursorTy t2_113_771_1033 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1205)->field4;
            CursorTy fltAppE_904_1034 =
                      bin_338(k1_104_763_1025, x1_105_764_1026, t2_113_771_1033, t3_107_766_1028);
            
            return bin_338(k2_110_768_1030, x2_111_769_1031, t1_112_770_1032,
                           fltAppE_904_1034);
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1204");
            exit(1);
        }
    }
}
CursorTy doubleR_336(IntTy k1_70_772_1035, IntTy x1_71_773_1036,
                     CursorTy m0_72_774_1037, CursorTy t4_73_775_1038)
{
    TagTyPacked tag_1207 = *(TagTyPacked *) m0_72_774_1037;
    CursorTy tail_1208 = m0_72_774_1037 + sizeof(IntTy);
    
    
  switch_1209:
    ;
    switch (tag_1207) {
        
      case 1:
        {
            IntTy wildcard__167_75_776_1039 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1208)->field0;
            IntTy k2_76_777_1040 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1208)->field1;
            IntTy x2_77_778_1041 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1208)->field2;
            CursorTy t1_78_779_1042 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1208)->field3;
            CursorTy m1_79_780_1043 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1208)->field4;
            
            return caseFn_696(x1_71_773_1036, k1_70_772_1035, t4_73_775_1038,
                              m1_79_780_1043, k2_76_777_1040, x2_77_778_1041,
                              t1_78_779_1042);
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1207");
            exit(1);
        }
    }
}
CursorTy empty_337()
{
    PtrTy tailift_1210 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1210)->field0 = 0;
    return tailift_1210;
}
CursorTy rotateR_334(IntTy k_129_781_1044, IntTy x_130_782_1045,
                     CursorTy l_131_783_1046, CursorTy r_132_784_1047)
{
    TagTyPacked tag_1211 = *(TagTyPacked *) l_131_783_1046;
    CursorTy tail_1212 = l_131_783_1046 + sizeof(IntTy);
    
    
  switch_1213:
    ;
    switch (tag_1211) {
        
      case 1:
        {
            IntTy wildcard__106_134_785_1048 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1212)->field0;
            IntTy wildcard__107_135_786_1049 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1212)->field1;
            IntTy wildcard__108_136_787_1050 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1212)->field2;
            CursorTy ly_137_788_1051 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1212)->field3;
            CursorTy ry_138_789_1052 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1212)->field4;
            IntTy fltPrm_906_1053 =  size_332(ry_138_789_1052);
            IntTy fltPrm_908_1054 =  ratio();
            IntTy fltPrm_909_1055 =  size_332(ly_137_788_1051);
            IntTy fltPrm_907_1056 = fltPrm_908_1054 * fltPrm_909_1055;
            BoolTy fltIf_905_1057 = fltPrm_906_1053 < fltPrm_907_1056;
            
            if (fltIf_905_1057) {
                return singleR_335(k_129_781_1044, x_130_782_1045,
                                   l_131_783_1046, r_132_784_1047);
            } else {
                return doubleR_336(k_129_781_1044, x_130_782_1045,
                                   l_131_783_1046, r_132_784_1047);
            }
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1211");
            exit(1);
        }
    }
}
CursorTy balance_331(IntTy k_149_790_1058, IntTy x_150_791_1059,
                     CursorTy l_151_792_1060, CursorTy r_152_793_1061)
{
    IntTy fltPrm_912_1062 =  size_332(l_151_792_1060);
    IntTy fltPrm_913_1063 =  size_332(r_152_793_1061);
    IntTy fltPrm_911_1064 = fltPrm_912_1062 + fltPrm_913_1063;
    BoolTy fltIf_910_1065 = fltPrm_911_1064 <= 1;
    
    if (fltIf_910_1065) {
        IntTy fltPrm_915_1066 =  size_332(l_151_792_1060);
        IntTy fltPrm_916_1067 =  size_332(r_152_793_1061);
        IntTy fltPkd_914_1068 = fltPrm_915_1066 + fltPrm_916_1067;
        PtrTy tailift_1214 =
              ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
        
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1214)->field0 = 1;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1214)->field1 =
            fltPkd_914_1068;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1214)->field2 =
            k_149_790_1058;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1214)->field3 =
            x_150_791_1059;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1214)->field4 =
            l_151_792_1060;
        ((Int64Int64Int64Int64CursorCursorProd *) tailift_1214)->field5 =
            r_152_793_1061;
        return tailift_1214;
    } else {
        IntTy fltPrm_918_1069 =  size_332(r_152_793_1061);
        IntTy fltPrm_920_1070 =  delta();
        IntTy fltPrm_921_1071 =  size_332(l_151_792_1060);
        IntTy fltPrm_919_1072 = fltPrm_920_1070 * fltPrm_921_1071;
        BoolTy fltIf_917_1073 = fltPrm_918_1069 >= fltPrm_919_1072;
        
        if (fltIf_917_1073) {
            return rotateL_333(k_149_790_1058, x_150_791_1059, l_151_792_1060,
                               r_152_793_1061);
        } else {
            IntTy fltPrm_923_1074 =  size_332(l_151_792_1060);
            IntTy fltPrm_925_1075 =  delta();
            IntTy fltPrm_926_1076 =  size_332(r_152_793_1061);
            IntTy fltPrm_924_1077 = fltPrm_925_1075 * fltPrm_926_1076;
            BoolTy fltIf_922_1078 = fltPrm_923_1074 >= fltPrm_924_1077;
            
            if (fltIf_922_1078) {
                return rotateR_334(k_149_790_1058, x_150_791_1059,
                                   l_151_792_1060, r_152_793_1061);
            } else {
                IntTy fltPrm_928_1079 =  size_332(l_151_792_1060);
                IntTy fltPrm_929_1080 =  size_332(r_152_793_1061);
                IntTy fltPkd_927_1081 = fltPrm_928_1079 + fltPrm_929_1080;
                PtrTy tailift_1215 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field1 =
                    fltPkd_927_1081;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field2 =
                    k_149_790_1058;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field3 =
                    x_150_791_1059;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field4 =
                    l_151_792_1060;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1215)->field5 =
                    r_152_793_1061;
                return tailift_1215;
            }
        }
    }
}
CursorTy singleton_328(IntTy k_66_794_1082, IntTy x_67_795_1083)
{
    PtrTy fltPkd_930_1084 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_930_1084)->field0 = 0;
    
    PtrTy fltPkd_931_1085 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_931_1085)->field0 = 0;
    
    PtrTy tailift_1216 = ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1216)->field0 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1216)->field1 = 1;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1216)->field2 =
        k_66_794_1082;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1216)->field3 =
        x_67_795_1083;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1216)->field4 =
        fltPkd_930_1084;
    ((Int64Int64Int64Int64CursorCursorProd *) tailift_1216)->field5 =
        fltPkd_931_1085;
    return tailift_1216;
}
CursorTy insert_330(IntTy kx_57_796_1086, IntTy x_58_797_1087,
                    CursorTy m_59_798_1088)
{
    TagTyPacked tag_1217 = *(TagTyPacked *) m_59_798_1088;
    CursorTy tail_1218 = m_59_798_1088 + sizeof(IntTy);
    
    
  switch_1220:
    ;
    switch (tag_1217) {
        
      case 0:
        {
            return singleton_328(kx_57_796_1086, x_58_797_1087);
            break;
        }
        
      case 1:
        {
            IntTy sz_61_799_1089 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1218)->field0;
            IntTy k_62_800_1090 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1218)->field1;
            IntTy v_63_801_1091 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1218)->field2;
            CursorTy l_64_802_1092 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1218)->field3;
            CursorTy r_65_803_1093 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1218)->field4;
            BoolTy fltIf_932_1094 = kx_57_796_1086 == k_62_800_1090;
            
            if (fltIf_932_1094) {
                PtrTy tailift_1219 =
                      ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1219)->field0 =
                    1;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1219)->field1 =
                    sz_61_799_1089;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1219)->field2 =
                    k_62_800_1090;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1219)->field3 =
                    x_58_797_1087;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1219)->field4 =
                    l_64_802_1092;
                ((Int64Int64Int64Int64CursorCursorProd *) tailift_1219)->field5 =
                    r_65_803_1093;
                return tailift_1219;
            } else {
                BoolTy fltIf_933_1095 = kx_57_796_1086 <= k_62_800_1090;
                
                if (fltIf_933_1095) {
                    CursorTy fltAppE_934_1096 =
                              insert_330(kx_57_796_1086, x_58_797_1087, l_64_802_1092);
                    
                    return balance_331(k_62_800_1090, v_63_801_1091,
                                       fltAppE_934_1096, r_65_803_1093);
                } else {
                    CursorTy fltAppE_935_1097 =
                              insert_330(kx_57_796_1086, x_58_797_1087, r_65_803_1093);
                    
                    return balance_331(k_62_800_1090, v_63_801_1091,
                                       l_64_802_1092, fltAppE_935_1097);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1217");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Map_v_329(CursorTy arg_646_804_1098)
{
    TagTyPacked tag_1221 = *(TagTyPacked *) arg_646_804_1098;
    CursorTy tail_1222 = arg_646_804_1098 + sizeof(IntTy);
    
    
  switch_1225:
    ;
    switch (tag_1221) {
        
      case 0:
        {
            PtrTy tailift_1223 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1223)->field0 = 0;
            return tailift_1223;
            break;
        }
        
      case 1:
        {
            IntTy x_647_805_1099 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1222)->field0;
            IntTy x_648_806_1100 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1222)->field1;
            IntTy x_649_807_1101 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1222)->field2;
            CursorTy x_650_808_1102 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1222)->field3;
            CursorTy x_651_809_1103 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1222)->field4;
            CursorTy y_655_813_1107 =
                      _copy_without_ptrs_Map_v_329(x_650_808_1102);
            CursorTy y_656_814_1108 =
                      _copy_without_ptrs_Map_v_329(x_651_809_1103);
            PtrTy tailift_1224 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1224)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1224)->field1 =
                x_647_805_1099;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1224)->field2 =
                x_648_806_1100;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1224)->field3 =
                x_649_807_1101;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1224)->field4 =
                y_655_813_1107;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1224)->field5 =
                y_656_814_1108;
            return tailift_1224;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1221");
            exit(1);
        }
    }
}
CursorTy _copy_Map_v_329(CursorTy arg_635_815_1109)
{
    TagTyPacked tag_1226 = *(TagTyPacked *) arg_635_815_1109;
    CursorTy tail_1227 = arg_635_815_1109 + sizeof(IntTy);
    
    
  switch_1230:
    ;
    switch (tag_1226) {
        
      case 0:
        {
            PtrTy tailift_1228 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1228)->field0 = 0;
            return tailift_1228;
            break;
        }
        
      case 1:
        {
            IntTy x_636_816_1110 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1227)->field0;
            IntTy x_637_817_1111 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1227)->field1;
            IntTy x_638_818_1112 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1227)->field2;
            CursorTy x_639_819_1113 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1227)->field3;
            CursorTy x_640_820_1114 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1227)->field4;
            CursorTy y_644_824_1118 =  _copy_Map_v_329(x_639_819_1113);
            CursorTy y_645_825_1119 =  _copy_Map_v_329(x_640_820_1114);
            PtrTy tailift_1229 =
                  ALLOC(sizeof(Int64Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1229)->field0 = 1;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1229)->field1 =
                x_636_816_1110;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1229)->field2 =
                x_637_817_1111;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1229)->field3 =
                x_638_818_1112;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1229)->field4 =
                y_644_824_1118;
            ((Int64Int64Int64Int64CursorCursorProd *) tailift_1229)->field5 =
                y_645_825_1119;
            return tailift_1229;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1226");
            exit(1);
        }
    }
}
unsigned char _traverse_Map_v_329(CursorTy arg_657_826_1120)
{
    TagTyPacked tag_1231 = *(TagTyPacked *) arg_657_826_1120;
    CursorTy tail_1232 = arg_657_826_1120 + sizeof(IntTy);
    
    
  switch_1233:
    ;
    switch (tag_1231) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_658_827_1121 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1232)->field0;
            IntTy x_659_828_1122 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1232)->field1;
            IntTy x_660_829_1123 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1232)->field2;
            CursorTy x_661_830_1124 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1232)->field3;
            CursorTy x_662_831_1125 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1232)->field4;
            unsigned char y_666_832_1126 =  _traverse_Map_v_329(x_661_830_1124);
            unsigned char y_667_833_1127 =  _traverse_Map_v_329(x_662_831_1125);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1231");
            exit(1);
        }
    }
}
unsigned char _print_Map_v_329(CursorTy arg_668_834_1128)
{
    TagTyPacked tag_1234 = *(TagTyPacked *) arg_668_834_1128;
    CursorTy tail_1235 = arg_668_834_1128 + sizeof(IntTy);
    
    
  switch_1236:
    ;
    switch (tag_1234) {
        
      case 0:
        {
            unsigned char wildcard_669_835_1129 = print_symbol(1179);
            unsigned char wildcard_670_836_1130 = print_symbol(1178);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_671_837_1131 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1235)->field0;
            IntTy x_672_838_1132 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1235)->field1;
            IntTy x_673_839_1133 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1235)->field2;
            CursorTy x_674_840_1134 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1235)->field3;
            CursorTy x_675_841_1135 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1235)->field4;
            unsigned char wildcard_681_842_1136 = print_symbol(1180);
            unsigned char wildcard_687_843_1137 = print_symbol(1181);
            unsigned char y_676_844_1138 = printf("%lld", x_671_837_1131);
            unsigned char wildcard_686_845_1139 = print_symbol(1181);
            unsigned char y_677_846_1140 = printf("%lld", x_672_838_1132);
            unsigned char wildcard_685_847_1141 = print_symbol(1181);
            unsigned char y_678_848_1142 = printf("%lld", x_673_839_1133);
            unsigned char wildcard_684_849_1143 = print_symbol(1181);
            unsigned char y_679_850_1144 =  _print_Map_v_329(x_674_840_1134);
            unsigned char wildcard_683_851_1145 = print_symbol(1181);
            unsigned char y_680_852_1146 =  _print_Map_v_329(x_675_841_1135);
            unsigned char wildcard_682_853_1147 = print_symbol(1178);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1234");
            exit(1);
        }
    }
}
CursorTy caseFn_688(IntTy x1_86_689_854_1148, IntTy k1_85_690_855_1149,
                    CursorTy t1_87_691_856_1150, CursorTy m1_93_692_857_1151,
                    IntTy k2_91_693_858_1152, IntTy x2_92_694_859_1153,
                    CursorTy t4_94_695_860_1154)
{
    TagTyPacked tag_1237 = *(TagTyPacked *) m1_93_692_857_1151;
    CursorTy tail_1238 = m1_93_692_857_1151 + sizeof(IntTy);
    
    
  switch_1239:
    ;
    switch (tag_1237) {
        
      case 1:
        {
            IntTy wildcard__144_95_861_1155 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1238)->field0;
            IntTy k3_96_862_1156 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1238)->field1;
            IntTy x3_97_863_1157 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1238)->field2;
            CursorTy t2_98_864_1158 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1238)->field3;
            CursorTy t3_99_865_1159 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1238)->field4;
            CursorTy fltAppE_936_1160 =
                      bin_338(k1_85_690_855_1149, x1_86_689_854_1148, t1_87_691_856_1150, t2_98_864_1158);
            CursorTy fltAppE_937_1161 =
                      bin_338(k2_91_693_858_1152, x2_92_694_859_1153, t3_99_865_1159, t4_94_695_860_1154);
            
            return bin_338(k3_96_862_1156, x3_97_863_1157, fltAppE_936_1160,
                           fltAppE_937_1161);
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1237");
            exit(1);
        }
    }
}
CursorTy caseFn_696(IntTy x1_71_697_866_1162, IntTy k1_70_698_867_1163,
                    CursorTy t4_73_699_868_1164, CursorTy m1_79_700_869_1165,
                    IntTy k2_76_701_870_1166, IntTy x2_77_702_871_1167,
                    CursorTy t1_78_703_872_1168)
{
    TagTyPacked tag_1240 = *(TagTyPacked *) m1_79_700_869_1165;
    CursorTy tail_1241 = m1_79_700_869_1165 + sizeof(IntTy);
    
    
  switch_1242:
    ;
    switch (tag_1240) {
        
      case 1:
        {
            IntTy wildcard__168_80_873_1169 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1241)->field0;
            IntTy k3_81_874_1170 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1241)->field1;
            IntTy x3_82_875_1171 =
                  ((Int64Int64Int64CursorCursorProd *) tail_1241)->field2;
            CursorTy t2_83_876_1172 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1241)->field3;
            CursorTy t3_84_877_1173 =
                     ((Int64Int64Int64CursorCursorProd *) tail_1241)->field4;
            CursorTy fltAppE_938_1174 =
                      bin_338(k2_76_701_870_1166, x2_77_702_871_1167, t1_78_703_872_1168, t2_83_876_1172);
            CursorTy fltAppE_939_1175 =
                      bin_338(k1_70_698_867_1163, x1_71_697_866_1162, t3_84_877_1173, t4_73_699_868_1164);
            
            return bin_338(k3_81_874_1170, x3_82_875_1171, fltAppE_938_1174,
                           fltAppE_939_1175);
            break;
        }
        
      case 0:
        {
            return empty_337();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1240");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1178, ")");
    add_symbol(1179, "(Tip_v_329");
    add_symbol(1180, "(Bin_v_329");
    add_symbol(1181, " ");
    add_symbol(1182, "\n");
    
    CursorTy fltAppE_878_940 =  singleton_328(0, 0);
    CursorTy m_33_704_941 =  build(0, 512, fltAppE_878_940);
    IntTy timed_1176;
    VectorTy *times_3 = vector_alloc(global_iters_param, sizeof(double));
    struct timespec begin_timed_1176;
    struct timespec end_timed_1176;
    
    for (long long iters_timed_1176 = 0; iters_timed_1176 < global_iters_param;
         iters_timed_1176++) {
        if (iters_timed_1176 != global_iters_param - 1)
            save_alloc_state();
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1176);
        
        IntTy timed_1176hack =  sumLeft(m_33_704_941);
        
        timed_1176 = timed_1176hack;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1176);
        if (iters_timed_1176 != global_iters_param - 1)
            restore_alloc_state();
        
        double itertime_0 = difftimespecs(&begin_timed_1176, &end_timed_1176);
        
        vector_inplace_update(times_3, iters_timed_1176, &itertime_0);
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
    
    unsigned char wildcard__27_35_706_943 = printf("%lld", timed_1176);
    unsigned char wildcard__25_36_707_944 = print_symbol(1182);
    IntTy timed_1177;
    VectorTy *times_8 = vector_alloc(global_iters_param, sizeof(double));
    struct timespec begin_timed_1177;
    struct timespec end_timed_1177;
    
    for (long long iters_timed_1177 = 0; iters_timed_1177 < global_iters_param;
         iters_timed_1177++) {
        if (iters_timed_1177 != global_iters_param - 1)
            save_alloc_state();
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1177);
        
        IntTy timed_1177hack =  sumRight(m_33_704_941);
        
        timed_1177 = timed_1177hack;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1177);
        if (iters_timed_1177 != global_iters_param - 1)
            restore_alloc_state();
        
        double itertime_5 = difftimespecs(&begin_timed_1177, &end_timed_1177);
        
        vector_inplace_update(times_8, iters_timed_1177, &itertime_5);
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
    
    unsigned char wildcard__21_38_709_946 = printf("%lld", timed_1177);
    unsigned char wildcard__19_39_710_947 = print_symbol(1182);
    
    printf("'#()");
    printf("\n");
    free_symtable();
    return 0;
}