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
CursorTy singleton_421(IntTy k_289_795_1035, VectorTy *x_290_796_1036);
IntTy size_423(CursorTy m_275_797_1039);
CursorTy singleL_429(IntTy k1_214_803_1045, VectorTy *x1_215_804_1046,
                     CursorTy t1_216_805_1047, CursorTy m_217_806_1048);
CursorTy doubleL_430(IntTy k1_185_812_1055, VectorTy *x1_186_813_1056,
                     CursorTy t1_187_814_1057, CursorTy m0_188_815_1058);
CursorTy rotateL_424(IntTy k_239_821_1064, VectorTy *x_240_822_1065,
                     CursorTy l_241_823_1066, CursorTy r_242_824_1067);
CursorTy bin_428(IntTy k_224_830_1078, VectorTy *x_225_831_1079,
                 CursorTy l_226_832_1080, CursorTy r_227_833_1081);
CursorTy singleR_426(IntTy k1_204_834_1086, VectorTy *x1_205_835_1087,
                     CursorTy m_206_836_1088, CursorTy t3_207_837_1089);
CursorTy doubleR_427(IntTy k1_170_843_1096, VectorTy *x1_171_844_1097,
                     CursorTy m0_172_845_1098, CursorTy t4_173_846_1099);
CursorTy rotateR_425(IntTy k_229_852_1105, VectorTy *x_230_853_1106,
                     CursorTy l_231_854_1107, CursorTy r_232_855_1108);
CursorTy balance_422(IntTy k_249_861_1119, VectorTy *x_250_862_1120,
                     CursorTy l_251_863_1121, CursorTy r_252_864_1122);
CursorTy insert_419(IntTy kx_254_865_1143, VectorTy *x_255_866_1144,
                    CursorTy m_256_867_1145);
CursorTy empty_418();
CursorTy _copy_without_ptrs_GibbonMap_v_420(CursorTy arg_723_873_1155);
CursorTy _copy_GibbonMap_v_420(CursorTy arg_712_884_1166);
unsigned char _traverse_GibbonMap_v_420(CursorTy arg_734_895_1177);
unsigned char _print_GibbonMap_v_420(CursorTy arg_745_903_1185);
CursorTy caseFn_765(VectorTy *x1_186_766_923_1205, IntTy k1_185_767_924_1206,
                    CursorTy t1_187_768_925_1207, CursorTy m1_193_769_926_1208,
                    IntTy k2_191_770_927_1209, VectorTy *x2_192_771_928_1210,
                    CursorTy t4_194_772_929_1211);
CursorTy caseFn_773(VectorTy *x1_171_774_935_1219, IntTy k1_170_775_936_1220,
                    CursorTy t4_173_776_937_1221, CursorTy m1_179_777_938_1222,
                    IntTy k2_176_778_939_1223, VectorTy *x2_177_779_940_1224,
                    CursorTy t1_178_780_941_1225);
IntTy ratio()
{
    return 2;
}
IntTy delta()
{
    return 4;
}
CursorTy singleton_421(IntTy k_289_795_1035, VectorTy *x_290_796_1036)
{
    PtrTy fltPkd_961_1037 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_961_1037)->field0 = 0;
    
    PtrTy fltPkd_962_1038 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_962_1038)->field0 = 0;
    
    PtrTy tailift_1239 = ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
    
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1239)->field0 = 1;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1239)->field1 = 1;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1239)->field2 =
        k_289_795_1035;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1239)->field3 =
        x_290_796_1036;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1239)->field4 =
        fltPkd_961_1037;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1239)->field5 =
        fltPkd_962_1038;
    return tailift_1239;
}
IntTy size_423(CursorTy m_275_797_1039)
{
    TagTyPacked tag_1240 = *(TagTyPacked *) m_275_797_1039;
    CursorTy tail_1241 = m_275_797_1039 + sizeof(IntTy);
    
    
  switch_1242:
    ;
    switch (tag_1240) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy sz_277_798_1040 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1241)->field0;
            IntTy wildcard__18_278_799_1041 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1241)->field1;
            VectorTy *wildcard__19_279_800_1042 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1241)->field2;
            CursorTy wildcard__20_280_801_1043 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1241)->field3;
            CursorTy wildcard__21_281_802_1044 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1241)->field4;
            
            return sz_277_798_1040;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1240");
            exit(1);
        }
    }
}
CursorTy singleL_429(IntTy k1_214_803_1045, VectorTy *x1_215_804_1046,
                     CursorTy t1_216_805_1047, CursorTy m_217_806_1048)
{
    TagTyPacked tag_1243 = *(TagTyPacked *) m_217_806_1048;
    CursorTy tail_1244 = m_217_806_1048 + sizeof(IntTy);
    
    
  switch_1245:
    ;
    switch (tag_1243) {
        
      case 1:
        {
            IntTy wildcard__89_219_807_1049 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1244)->field0;
            IntTy k2_220_808_1050 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1244)->field1;
            VectorTy *x2_221_809_1051 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1244)->field2;
            CursorTy t2_222_810_1052 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1244)->field3;
            CursorTy t3_223_811_1053 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1244)->field4;
            CursorTy fltAppE_963_1054 =
                      bin_428(k1_214_803_1045, x1_215_804_1046, t1_216_805_1047, t2_222_810_1052);
            
            return bin_428(k2_220_808_1050, x2_221_809_1051, fltAppE_963_1054,
                           t3_223_811_1053);
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1243");
            exit(1);
        }
    }
}
CursorTy doubleL_430(IntTy k1_185_812_1055, VectorTy *x1_186_813_1056,
                     CursorTy t1_187_814_1057, CursorTy m0_188_815_1058)
{
    TagTyPacked tag_1246 = *(TagTyPacked *) m0_188_815_1058;
    CursorTy tail_1247 = m0_188_815_1058 + sizeof(IntTy);
    
    
  switch_1248:
    ;
    switch (tag_1246) {
        
      case 1:
        {
            IntTy wildcard__109_190_816_1059 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1247)->field0;
            IntTy k2_191_817_1060 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1247)->field1;
            VectorTy *x2_192_818_1061 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1247)->field2;
            CursorTy m1_193_819_1062 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1247)->field3;
            CursorTy t4_194_820_1063 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1247)->field4;
            
            return caseFn_765(x1_186_813_1056, k1_185_812_1055, t1_187_814_1057,
                              m1_193_819_1062, k2_191_817_1060, x2_192_818_1061,
                              t4_194_820_1063);
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1246");
            exit(1);
        }
    }
}
CursorTy rotateL_424(IntTy k_239_821_1064, VectorTy *x_240_822_1065,
                     CursorTy l_241_823_1066, CursorTy r_242_824_1067)
{
    TagTyPacked tag_1249 = *(TagTyPacked *) r_242_824_1067;
    CursorTy tail_1250 = r_242_824_1067 + sizeof(IntTy);
    
    
  switch_1251:
    ;
    switch (tag_1249) {
        
      case 1:
        {
            IntTy wildcard__60_244_825_1068 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1250)->field0;
            IntTy wildcard__61_245_826_1069 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1250)->field1;
            VectorTy *wildcard__62_246_827_1070 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1250)->field2;
            CursorTy ly_247_828_1071 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1250)->field3;
            CursorTy ry_248_829_1072 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1250)->field4;
            IntTy fltPrm_965_1073 =  size_423(ly_247_828_1071);
            IntTy fltPrm_967_1074 =  ratio();
            IntTy fltPrm_968_1075 =  size_423(ry_248_829_1072);
            IntTy fltPrm_966_1076 = fltPrm_967_1074 * fltPrm_968_1075;
            BoolTy fltIf_964_1077 = fltPrm_965_1073 < fltPrm_966_1076;
            
            if (fltIf_964_1077) {
                return singleL_429(k_239_821_1064, x_240_822_1065,
                                   l_241_823_1066, r_242_824_1067);
            } else {
                return doubleL_430(k_239_821_1064, x_240_822_1065,
                                   l_241_823_1066, r_242_824_1067);
            }
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1249");
            exit(1);
        }
    }
}
CursorTy bin_428(IntTy k_224_830_1078, VectorTy *x_225_831_1079,
                 CursorTy l_226_832_1080, CursorTy r_227_833_1081)
{
    IntTy fltPrm_971_1082 =  size_423(l_226_832_1080);
    IntTy fltPrm_972_1083 =  size_423(r_227_833_1081);
    IntTy fltPrm_970_1084 = fltPrm_971_1082 + fltPrm_972_1083;
    IntTy fltPkd_969_1085 = fltPrm_970_1084 + 1;
    PtrTy tailift_1252 = ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
    
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1252)->field0 = 1;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1252)->field1 =
        fltPkd_969_1085;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1252)->field2 =
        k_224_830_1078;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1252)->field3 =
        x_225_831_1079;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1252)->field4 =
        l_226_832_1080;
    ((Int64Int64Int64VectorCursorCursorProd *) tailift_1252)->field5 =
        r_227_833_1081;
    return tailift_1252;
}
CursorTy singleR_426(IntTy k1_204_834_1086, VectorTy *x1_205_835_1087,
                     CursorTy m_206_836_1088, CursorTy t3_207_837_1089)
{
    TagTyPacked tag_1253 = *(TagTyPacked *) m_206_836_1088;
    CursorTy tail_1254 = m_206_836_1088 + sizeof(IntTy);
    
    
  switch_1255:
    ;
    switch (tag_1253) {
        
      case 1:
        {
            IntTy wildcard__99_209_838_1090 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1254)->field0;
            IntTy k2_210_839_1091 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1254)->field1;
            VectorTy *x2_211_840_1092 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1254)->field2;
            CursorTy t1_212_841_1093 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1254)->field3;
            CursorTy t2_213_842_1094 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1254)->field4;
            CursorTy fltAppE_973_1095 =
                      bin_428(k1_204_834_1086, x1_205_835_1087, t2_213_842_1094, t3_207_837_1089);
            
            return bin_428(k2_210_839_1091, x2_211_840_1092, t1_212_841_1093,
                           fltAppE_973_1095);
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1253");
            exit(1);
        }
    }
}
CursorTy doubleR_427(IntTy k1_170_843_1096, VectorTy *x1_171_844_1097,
                     CursorTy m0_172_845_1098, CursorTy t4_173_846_1099)
{
    TagTyPacked tag_1256 = *(TagTyPacked *) m0_172_845_1098;
    CursorTy tail_1257 = m0_172_845_1098 + sizeof(IntTy);
    
    
  switch_1258:
    ;
    switch (tag_1256) {
        
      case 1:
        {
            IntTy wildcard__133_175_847_1100 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1257)->field0;
            IntTy k2_176_848_1101 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1257)->field1;
            VectorTy *x2_177_849_1102 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1257)->field2;
            CursorTy t1_178_850_1103 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1257)->field3;
            CursorTy m1_179_851_1104 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1257)->field4;
            
            return caseFn_773(x1_171_844_1097, k1_170_843_1096, t4_173_846_1099,
                              m1_179_851_1104, k2_176_848_1101, x2_177_849_1102,
                              t1_178_850_1103);
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1256");
            exit(1);
        }
    }
}
CursorTy rotateR_425(IntTy k_229_852_1105, VectorTy *x_230_853_1106,
                     CursorTy l_231_854_1107, CursorTy r_232_855_1108)
{
    TagTyPacked tag_1259 = *(TagTyPacked *) l_231_854_1107;
    CursorTy tail_1260 = l_231_854_1107 + sizeof(IntTy);
    
    
  switch_1261:
    ;
    switch (tag_1259) {
        
      case 1:
        {
            IntTy wildcard__72_234_856_1109 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1260)->field0;
            IntTy wildcard__73_235_857_1110 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1260)->field1;
            VectorTy *wildcard__74_236_858_1111 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1260)->field2;
            CursorTy ly_237_859_1112 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1260)->field3;
            CursorTy ry_238_860_1113 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1260)->field4;
            IntTy fltPrm_975_1114 =  size_423(ry_238_860_1113);
            IntTy fltPrm_977_1115 =  ratio();
            IntTy fltPrm_978_1116 =  size_423(ly_237_859_1112);
            IntTy fltPrm_976_1117 = fltPrm_977_1115 * fltPrm_978_1116;
            BoolTy fltIf_974_1118 = fltPrm_975_1114 < fltPrm_976_1117;
            
            if (fltIf_974_1118) {
                return singleR_426(k_229_852_1105, x_230_853_1106,
                                   l_231_854_1107, r_232_855_1108);
            } else {
                return doubleR_427(k_229_852_1105, x_230_853_1106,
                                   l_231_854_1107, r_232_855_1108);
            }
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1259");
            exit(1);
        }
    }
}
CursorTy balance_422(IntTy k_249_861_1119, VectorTy *x_250_862_1120,
                     CursorTy l_251_863_1121, CursorTy r_252_864_1122)
{
    IntTy fltPrm_981_1123 =  size_423(l_251_863_1121);
    IntTy fltPrm_982_1124 =  size_423(r_252_864_1122);
    IntTy fltPrm_980_1125 = fltPrm_981_1123 + fltPrm_982_1124;
    BoolTy fltIf_979_1126 = fltPrm_980_1125 <= 1;
    
    if (fltIf_979_1126) {
        IntTy fltPrm_984_1127 =  size_423(l_251_863_1121);
        IntTy fltPrm_985_1128 =  size_423(r_252_864_1122);
        IntTy fltPkd_983_1129 = fltPrm_984_1127 + fltPrm_985_1128;
        PtrTy tailift_1262 =
              ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
        
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1262)->field0 = 1;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1262)->field1 =
            fltPkd_983_1129;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1262)->field2 =
            k_249_861_1119;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1262)->field3 =
            x_250_862_1120;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1262)->field4 =
            l_251_863_1121;
        ((Int64Int64Int64VectorCursorCursorProd *) tailift_1262)->field5 =
            r_252_864_1122;
        return tailift_1262;
    } else {
        IntTy fltPrm_987_1130 =  size_423(r_252_864_1122);
        IntTy fltPrm_989_1131 =  delta();
        IntTy fltPrm_990_1132 =  size_423(l_251_863_1121);
        IntTy fltPrm_988_1133 = fltPrm_989_1131 * fltPrm_990_1132;
        BoolTy fltIf_986_1134 = fltPrm_987_1130 >= fltPrm_988_1133;
        
        if (fltIf_986_1134) {
            return rotateL_424(k_249_861_1119, x_250_862_1120, l_251_863_1121,
                               r_252_864_1122);
        } else {
            IntTy fltPrm_992_1135 =  size_423(l_251_863_1121);
            IntTy fltPrm_994_1136 =  delta();
            IntTy fltPrm_995_1137 =  size_423(r_252_864_1122);
            IntTy fltPrm_993_1138 = fltPrm_994_1136 * fltPrm_995_1137;
            BoolTy fltIf_991_1139 = fltPrm_992_1135 >= fltPrm_993_1138;
            
            if (fltIf_991_1139) {
                return rotateR_425(k_249_861_1119, x_250_862_1120,
                                   l_251_863_1121, r_252_864_1122);
            } else {
                IntTy fltPrm_997_1140 =  size_423(l_251_863_1121);
                IntTy fltPrm_998_1141 =  size_423(r_252_864_1122);
                IntTy fltPkd_996_1142 = fltPrm_997_1140 + fltPrm_998_1141;
                PtrTy tailift_1263 =
                      ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
                
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1263)->field0 =
                    1;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1263)->field1 =
                    fltPkd_996_1142;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1263)->field2 =
                    k_249_861_1119;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1263)->field3 =
                    x_250_862_1120;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1263)->field4 =
                    l_251_863_1121;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1263)->field5 =
                    r_252_864_1122;
                return tailift_1263;
            }
        }
    }
}
CursorTy insert_419(IntTy kx_254_865_1143, VectorTy *x_255_866_1144,
                    CursorTy m_256_867_1145)
{
    TagTyPacked tag_1264 = *(TagTyPacked *) m_256_867_1145;
    CursorTy tail_1265 = m_256_867_1145 + sizeof(IntTy);
    
    
  switch_1267:
    ;
    switch (tag_1264) {
        
      case 0:
        {
            return singleton_421(kx_254_865_1143, x_255_866_1144);
            break;
        }
        
      case 1:
        {
            IntTy sz_258_868_1146 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1265)->field0;
            IntTy k_259_869_1147 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1265)->field1;
            VectorTy *v_260_870_1148 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1265)->field2;
            CursorTy l_261_871_1149 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1265)->field3;
            CursorTy r_262_872_1150 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1265)->field4;
            BoolTy fltIf_999_1151 = kx_254_865_1143 == k_259_869_1147;
            
            if (fltIf_999_1151) {
                PtrTy tailift_1266 =
                      ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
                
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1266)->field0 =
                    1;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1266)->field1 =
                    sz_258_868_1146;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1266)->field2 =
                    k_259_869_1147;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1266)->field3 =
                    x_255_866_1144;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1266)->field4 =
                    l_261_871_1149;
                ((Int64Int64Int64VectorCursorCursorProd *) tailift_1266)->field5 =
                    r_262_872_1150;
                return tailift_1266;
            } else {
                BoolTy fltIf_1000_1152 = kx_254_865_1143 <= k_259_869_1147;
                
                if (fltIf_1000_1152) {
                    CursorTy fltAppE_1001_1153 =
                              insert_419(kx_254_865_1143, x_255_866_1144, l_261_871_1149);
                    
                    return balance_422(k_259_869_1147, v_260_870_1148,
                                       fltAppE_1001_1153, r_262_872_1150);
                } else {
                    CursorTy fltAppE_1002_1154 =
                              insert_419(kx_254_865_1143, x_255_866_1144, r_262_872_1150);
                    
                    return balance_422(k_259_869_1147, v_260_870_1148,
                                       l_261_871_1149, fltAppE_1002_1154);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1264");
            exit(1);
        }
    }
}
CursorTy empty_418()
{
    PtrTy tailift_1268 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1268)->field0 = 0;
    return tailift_1268;
}
CursorTy _copy_without_ptrs_GibbonMap_v_420(CursorTy arg_723_873_1155)
{
    TagTyPacked tag_1269 = *(TagTyPacked *) arg_723_873_1155;
    CursorTy tail_1270 = arg_723_873_1155 + sizeof(IntTy);
    
    
  switch_1273:
    ;
    switch (tag_1269) {
        
      case 0:
        {
            PtrTy tailift_1271 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1271)->field0 = 0;
            return tailift_1271;
            break;
        }
        
      case 1:
        {
            IntTy x_724_874_1156 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1270)->field0;
            IntTy x_725_875_1157 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1270)->field1;
            VectorTy *x_726_876_1158 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1270)->field2;
            CursorTy x_727_877_1159 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1270)->field3;
            CursorTy x_728_878_1160 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1270)->field4;
            CursorTy y_732_882_1164 =
                      _copy_without_ptrs_GibbonMap_v_420(x_727_877_1159);
            CursorTy y_733_883_1165 =
                      _copy_without_ptrs_GibbonMap_v_420(x_728_878_1160);
            PtrTy tailift_1272 =
                  ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
            
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1272)->field0 =
                1;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1272)->field1 =
                x_724_874_1156;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1272)->field2 =
                x_725_875_1157;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1272)->field3 =
                x_726_876_1158;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1272)->field4 =
                y_732_882_1164;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1272)->field5 =
                y_733_883_1165;
            return tailift_1272;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1269");
            exit(1);
        }
    }
}
CursorTy _copy_GibbonMap_v_420(CursorTy arg_712_884_1166)
{
    TagTyPacked tag_1274 = *(TagTyPacked *) arg_712_884_1166;
    CursorTy tail_1275 = arg_712_884_1166 + sizeof(IntTy);
    
    
  switch_1278:
    ;
    switch (tag_1274) {
        
      case 0:
        {
            PtrTy tailift_1276 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1276)->field0 = 0;
            return tailift_1276;
            break;
        }
        
      case 1:
        {
            IntTy x_713_885_1167 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1275)->field0;
            IntTy x_714_886_1168 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1275)->field1;
            VectorTy *x_715_887_1169 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1275)->field2;
            CursorTy x_716_888_1170 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1275)->field3;
            CursorTy x_717_889_1171 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1275)->field4;
            CursorTy y_721_893_1175 =  _copy_GibbonMap_v_420(x_716_888_1170);
            CursorTy y_722_894_1176 =  _copy_GibbonMap_v_420(x_717_889_1171);
            PtrTy tailift_1277 =
                  ALLOC(sizeof(Int64Int64Int64VectorCursorCursorProd));
            
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1277)->field0 =
                1;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1277)->field1 =
                x_713_885_1167;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1277)->field2 =
                x_714_886_1168;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1277)->field3 =
                x_715_887_1169;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1277)->field4 =
                y_721_893_1175;
            ((Int64Int64Int64VectorCursorCursorProd *) tailift_1277)->field5 =
                y_722_894_1176;
            return tailift_1277;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1274");
            exit(1);
        }
    }
}
unsigned char _traverse_GibbonMap_v_420(CursorTy arg_734_895_1177)
{
    TagTyPacked tag_1279 = *(TagTyPacked *) arg_734_895_1177;
    CursorTy tail_1280 = arg_734_895_1177 + sizeof(IntTy);
    
    
  switch_1281:
    ;
    switch (tag_1279) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_735_896_1178 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1280)->field0;
            IntTy x_736_897_1179 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1280)->field1;
            VectorTy *x_737_898_1180 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1280)->field2;
            CursorTy x_738_899_1181 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1280)->field3;
            CursorTy x_739_900_1182 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1280)->field4;
            unsigned char y_743_901_1183 =
                           _traverse_GibbonMap_v_420(x_738_899_1181);
            unsigned char y_744_902_1184 =
                           _traverse_GibbonMap_v_420(x_739_900_1182);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1279");
            exit(1);
        }
    }
}
unsigned char _print_GibbonMap_v_420(CursorTy arg_745_903_1185)
{
    TagTyPacked tag_1282 = *(TagTyPacked *) arg_745_903_1185;
    CursorTy tail_1283 = arg_745_903_1185 + sizeof(IntTy);
    
    
  switch_1284:
    ;
    switch (tag_1282) {
        
      case 0:
        {
            unsigned char wildcard_746_904_1186 = print_symbol(1236);
            unsigned char wildcard_747_905_1187 = print_symbol(1235);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_748_906_1188 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1283)->field0;
            IntTy x_749_907_1189 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1283)->field1;
            VectorTy *x_750_908_1190 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1283)->field2;
            CursorTy x_751_909_1191 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1283)->field3;
            CursorTy x_752_910_1192 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1283)->field4;
            unsigned char wildcard_758_911_1193 = print_symbol(1237);
            unsigned char wildcard_764_912_1194 = print_symbol(1238);
            unsigned char y_753_913_1195 = printf("%lld", x_748_906_1188);
            unsigned char wildcard_763_914_1196 = print_symbol(1238);
            unsigned char y_754_915_1197 = printf("%lld", x_749_907_1189);
            unsigned char wildcard_762_916_1198 = print_symbol(1238);
            unsigned char y_755_917_1199 = print_symbol(1234);
            unsigned char wildcard_761_918_1200 = print_symbol(1238);
            unsigned char y_756_919_1201 =
                           _print_GibbonMap_v_420(x_751_909_1191);
            unsigned char wildcard_760_920_1202 = print_symbol(1238);
            unsigned char y_757_921_1203 =
                           _print_GibbonMap_v_420(x_752_910_1192);
            unsigned char wildcard_759_922_1204 = print_symbol(1235);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1282");
            exit(1);
        }
    }
}
CursorTy caseFn_765(VectorTy *x1_186_766_923_1205, IntTy k1_185_767_924_1206,
                    CursorTy t1_187_768_925_1207, CursorTy m1_193_769_926_1208,
                    IntTy k2_191_770_927_1209, VectorTy *x2_192_771_928_1210,
                    CursorTy t4_194_772_929_1211)
{
    TagTyPacked tag_1285 = *(TagTyPacked *) m1_193_769_926_1208;
    CursorTy tail_1286 = m1_193_769_926_1208 + sizeof(IntTy);
    
    
  switch_1287:
    ;
    switch (tag_1285) {
        
      case 1:
        {
            IntTy wildcard__110_195_930_1212 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1286)->field0;
            IntTy k3_196_931_1213 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1286)->field1;
            VectorTy *x3_197_932_1214 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1286)->field2;
            CursorTy t2_198_933_1215 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1286)->field3;
            CursorTy t3_199_934_1216 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1286)->field4;
            CursorTy fltAppE_1003_1217 =
                      bin_428(k1_185_767_924_1206, x1_186_766_923_1205, t1_187_768_925_1207, t2_198_933_1215);
            CursorTy fltAppE_1004_1218 =
                      bin_428(k2_191_770_927_1209, x2_192_771_928_1210, t3_199_934_1216, t4_194_772_929_1211);
            
            return bin_428(k3_196_931_1213, x3_197_932_1214, fltAppE_1003_1217,
                           fltAppE_1004_1218);
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1285");
            exit(1);
        }
    }
}
CursorTy caseFn_773(VectorTy *x1_171_774_935_1219, IntTy k1_170_775_936_1220,
                    CursorTy t4_173_776_937_1221, CursorTy m1_179_777_938_1222,
                    IntTy k2_176_778_939_1223, VectorTy *x2_177_779_940_1224,
                    CursorTy t1_178_780_941_1225)
{
    TagTyPacked tag_1288 = *(TagTyPacked *) m1_179_777_938_1222;
    CursorTy tail_1289 = m1_179_777_938_1222 + sizeof(IntTy);
    
    
  switch_1290:
    ;
    switch (tag_1288) {
        
      case 1:
        {
            IntTy wildcard__134_180_942_1226 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1289)->field0;
            IntTy k3_181_943_1227 =
                  ((Int64Int64VectorCursorCursorProd *) tail_1289)->field1;
            VectorTy *x3_182_944_1228 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1289)->field2;
            CursorTy t2_183_945_1229 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1289)->field3;
            CursorTy t3_184_946_1230 =
                     ((Int64Int64VectorCursorCursorProd *) tail_1289)->field4;
            CursorTy fltAppE_1005_1231 =
                      bin_428(k2_176_778_939_1223, x2_177_779_940_1224, t1_178_780_941_1225, t2_183_945_1229);
            CursorTy fltAppE_1006_1232 =
                      bin_428(k1_170_775_936_1220, x1_171_774_935_1219, t3_184_946_1230, t4_173_776_937_1221);
            
            return bin_428(k3_181_943_1227, x3_182_944_1228, fltAppE_1005_1231,
                           fltAppE_1006_1232);
            break;
        }
        
      case 0:
        {
            return empty_418();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1288");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1234, "Vector");
    add_symbol(1235, ")");
    add_symbol(1236, "(Tip_v_420");
    add_symbol(1237, "(Bin_v_420");
    add_symbol(1238, " ");
    
    IntTy tmp_13 = sizeof(CharTy);
    VectorTy *vec_145_153_781_1007 = vector_alloc(1, tmp_13);
    CharTy tmp_12 = 'f';
    VectorTy *__154_782_1008 = vector_inplace_update(vec_145_153_781_1007, 0,
                                                     &tmp_12);
    IntTy tmp_11 = sizeof(CharTy);
    VectorTy *vec_146_155_783_1010 = vector_alloc(1, tmp_11);
    CharTy tmp_10 = 'f';
    VectorTy *__156_784_1011 = vector_inplace_update(vec_146_155_783_1010, 0,
                                                     &tmp_10);
    IntTy tmp_9 = sizeof(CharTy);
    VectorTy *vec_147_157_785_1013 = vector_alloc(1, tmp_9);
    CharTy tmp_8 = 'e';
    VectorTy *__158_786_1014 = vector_inplace_update(vec_147_157_785_1013, 0,
                                                     &tmp_8);
    IntTy tmp_7 = sizeof(CharTy);
    VectorTy *vec_148_159_787_1016 = vector_alloc(1, tmp_7);
    CharTy tmp_6 = 'd';
    VectorTy *__160_788_1017 = vector_inplace_update(vec_148_159_787_1016, 0,
                                                     &tmp_6);
    IntTy tmp_5 = sizeof(CharTy);
    VectorTy *vec_149_161_789_1019 = vector_alloc(1, tmp_5);
    CharTy tmp_4 = 'c';
    VectorTy *__162_790_1020 = vector_inplace_update(vec_149_161_789_1019, 0,
                                                     &tmp_4);
    IntTy tmp_3 = sizeof(CharTy);
    VectorTy *vec_150_163_791_1022 = vector_alloc(1, tmp_3);
    CharTy tmp_2 = 'b';
    VectorTy *__164_792_1023 = vector_inplace_update(vec_150_163_791_1022, 0,
                                                     &tmp_2);
    IntTy tmp_1 = sizeof(CharTy);
    VectorTy *vec_151_165_793_1025 = vector_alloc(1, tmp_1);
    CharTy tmp_0 = 'a';
    VectorTy *__166_794_1026 = vector_inplace_update(vec_151_165_793_1025, 0,
                                                     &tmp_0);
    CursorTy fltAppE_960_1028 =  empty_418();
    CursorTy fltAppE_958_1029 =
              insert_419(0, vec_151_165_793_1025, fltAppE_960_1028);
    CursorTy fltAppE_956_1030 =
              insert_419(1, vec_150_163_791_1022, fltAppE_958_1029);
    CursorTy fltAppE_954_1031 =
              insert_419(2, vec_149_161_789_1019, fltAppE_956_1030);
    CursorTy fltAppE_952_1032 =
              insert_419(3, vec_148_159_787_1016, fltAppE_954_1031);
    CursorTy fltAppE_950_1033 =
              insert_419(4, vec_147_157_785_1013, fltAppE_952_1032);
    CursorTy fltAppE_948_1034 =
              insert_419(5, vec_146_155_783_1010, fltAppE_950_1033);
    CursorTy tmp_app_1233 =
              insert_419(0, vec_145_153_781_1007, fltAppE_948_1034);
    
     _print_GibbonMap_v_420(tmp_app_1233);
    printf("\n");
    free_symtable();
    return 0;
}