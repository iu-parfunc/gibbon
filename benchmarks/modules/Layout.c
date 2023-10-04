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
typedef struct Int64Int64Int64CursorCursorProd_struct {
            IntTy field0;
            IntTy field1;
            IntTy field2;
            CursorTy field3;
            CursorTy field4;
        } Int64Int64Int64CursorCursorProd;
typedef struct Int64Int64CursorCursorProd_struct {
            IntTy field0;
            IntTy field1;
            CursorTy field2;
            CursorTy field3;
        } Int64Int64CursorCursorProd;
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
CursorTy build(IntTy x_23_469_822, IntTy sz_24_470_823);
IntTy eval(IntTy x_25_471_829, CursorTy m_26_472_830);
CursorTy singleton(IntTy x_35_481_841);
CursorTy insert(IntTy x_36_482_844, CursorTy s_37_483_845);
IntTy depth(IntTy x_44_490_854, IntTy c_45_491_855, CursorTy s_46_492_856);
CursorTy right(CursorTy x_58_504_865);
CursorTy left(CursorTy x_63_509_870);
IntTy val(CursorTy x_68_514_875);
IntTy size(CursorTy s_73_519_880);
CursorTy balanceR(IntTy x_89_535_885, CursorTy l_90_536_886,
                  CursorTy r_91_537_887);
CursorTy balanceL(IntTy x_116_542_892, CursorTy l_117_543_893,
                  CursorTy r_118_544_894);
CursorTy _copy_without_ptrs_IntSet(CursorTy arg_380_549_899);
CursorTy _copy_IntSet(CursorTy arg_371_558_908);
unsigned char _traverse_IntSet(CursorTy arg_389_567_917);
unsigned char _print_IntSet(CursorTy arg_398_574_924);
CursorTy caseFn_415(IntTy x_89_416_591_941, CursorTy rr_95_417_592_942,
                    IntTy rx_93_418_593_943, IntTy rs_92_419_594_944,
                    CursorTy rl_94_420_595_945);
CursorTy caseFn_421(IntTy x_89_422_600_977, CursorTy r_91_423_601_978,
                    IntTy rx_93_424_602_979, CursorTy rl_94_425_603_980);
CursorTy caseFn_426(IntTy x_89_427_608_993, CursorTy r_91_428_609_994,
                    CursorTy rr_95_429_610_995, IntTy rx_93_430_611_996,
                    IntTy rs_92_431_612_997, CursorTy rl_94_432_613_998);
CursorTy caseFn_433(IntTy x_89_434_618_1003, CursorTy r_91_435_619_1004);
CursorTy caseFn_436(CursorTy l_90_437_624_1011, IntTy x_89_438_625_1012,
                    CursorTy r_91_439_626_1013, IntTy ls_108_440_627_1014);
CursorTy caseFn_441(IntTy x_116_442_632_1051, CursorTy ll_121_443_633_1052,
                    IntTy lx_120_444_634_1053, IntTy ls_119_445_635_1054,
                    CursorTy lr_122_446_636_1055, IntTy lls_123_447_637_1056);
CursorTy caseFn_448(CursorTy l_117_449_642_1079, IntTy x_116_450_643_1080,
                    IntTy lx_120_451_644_1081, CursorTy lr_122_452_645_1082);
CursorTy caseFn_453(CursorTy l_117_454_650_1094, IntTy x_116_455_651_1095,
                    CursorTy ll_121_456_652_1096, IntTy lx_120_457_653_1097,
                    IntTy ls_119_458_654_1098, CursorTy lr_122_459_655_1099);
CursorTy caseFn_460(CursorTy l_117_461_660_1104, IntTy x_116_462_661_1105);
CursorTy caseFn_463(CursorTy r_118_464_666_1112, CursorTy l_117_465_667_1113,
                    IntTy x_116_466_668_1114, IntTy rs_135_467_669_1115);
CursorTy build(IntTy x_23_469_822, IntTy sz_24_470_823)
{
    BoolTy fltIf_676_824 = x_23_469_822 == 0;
    
    if (fltIf_676_824) {
        return singleton(0);
    } else {
        IntTy fltPrm_678_825 = sz_24_470_823 / 2;
        IntTy fltAppE_677_826 = x_23_469_822 - fltPrm_678_825;
        IntTy fltAppE_680_827 = x_23_469_822 - 1;
        CursorTy fltAppE_679_828 =  build(fltAppE_680_827, sz_24_470_823);
        
        return insert(fltAppE_677_826, fltAppE_679_828);
    }
}
IntTy eval(IntTy x_25_471_829, CursorTy m_26_472_830)
{
    unsigned char wildcard__18_27_473_831 = print_symbol(1157);
    unsigned char wildcard__16_28_474_832 = print_symbol(1155);
    unsigned char wildcard__14_29_475_833 = printf("%lld", x_25_471_829);
    unsigned char wildcard__12_30_476_834 = print_symbol(1154);
    IntTy timed_1153;
    VectorTy *times_3 = vector_alloc(global_iters_param, sizeof(double));
    struct timespec begin_timed_1153;
    struct timespec end_timed_1153;
    
    for (long long iters_timed_1153 = 0; iters_timed_1153 < global_iters_param;
         iters_timed_1153++) {
        if (iters_timed_1153 != global_iters_param - 1)
            save_alloc_state();
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_1153);
        
        IntTy timed_1153hack =  depth(x_25_471_829, 0, m_26_472_830);
        
        timed_1153 = timed_1153hack;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_1153);
        if (iters_timed_1153 != global_iters_param - 1)
            restore_alloc_state();
        
        double itertime_0 = difftimespecs(&begin_timed_1153, &end_timed_1153);
        
        vector_inplace_update(times_3, iters_timed_1153, &itertime_0);
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
    
    unsigned char wildcard__8_32_478_836 = print_symbol(1156);
    unsigned char wildcard__6_33_479_837 = printf("%lld", timed_1153);
    unsigned char wildcard__4_34_480_838 = print_symbol(1154);
    BoolTy fltIf_681_839 = timed_1153 < 0;
    
    if (fltIf_681_839) {
        return 0;
    } else {
        IntTy fltAppE_682_840 = x_25_471_829 - 1;
        
        return eval(fltAppE_682_840, m_26_472_830);
    }
}
CursorTy singleton(IntTy x_35_481_841)
{
    PtrTy fltPkd_683_842 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_683_842)->field0 = 1;
    
    PtrTy fltPkd_684_843 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_684_843)->field0 = 1;
    
    PtrTy tailift_1162 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64CursorCursorProd *) tailift_1162)->field0 = 0;
    ((Int64Int64Int64CursorCursorProd *) tailift_1162)->field1 = 1;
    ((Int64Int64Int64CursorCursorProd *) tailift_1162)->field2 = x_35_481_841;
    ((Int64Int64Int64CursorCursorProd *) tailift_1162)->field3 = fltPkd_683_842;
    ((Int64Int64Int64CursorCursorProd *) tailift_1162)->field4 = fltPkd_684_843;
    return tailift_1162;
}
CursorTy insert(IntTy x_36_482_844, CursorTy s_37_483_845)
{
    TagTyPacked tag_1163 = *(TagTyPacked *) s_37_483_845;
    CursorTy tail_1164 = s_37_483_845 + sizeof(IntTy);
    
    
  switch_1165:
    ;
    switch (tag_1163) {
        
      case 1:
        {
            return singleton(x_36_482_844);
            break;
        }
        
      case 0:
        {
            IntTy sz_38_484_846 =
                  ((Int64Int64CursorCursorProd *) tail_1164)->field0;
            IntTy v_39_485_847 =
                  ((Int64Int64CursorCursorProd *) tail_1164)->field1;
            CursorTy l_40_486_848 =
                     ((Int64Int64CursorCursorProd *) tail_1164)->field2;
            CursorTy r_41_487_849 =
                     ((Int64Int64CursorCursorProd *) tail_1164)->field3;
            BoolTy fltIf_685_850 = x_36_482_844 == v_39_485_847;
            
            if (fltIf_685_850) {
                return s_37_483_845;
            } else {
                BoolTy fltIf_686_851 = x_36_482_844 <= v_39_485_847;
                
                if (fltIf_686_851) {
                    CursorTy nl_42_488_852 =
                              insert(x_36_482_844, l_40_486_848);
                    
                    return balanceL(v_39_485_847, nl_42_488_852, r_41_487_849);
                } else {
                    CursorTy nr_43_489_853 =
                              insert(x_36_482_844, r_41_487_849);
                    
                    return balanceR(v_39_485_847, l_40_486_848, nr_43_489_853);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1163");
            exit(1);
        }
    }
}
IntTy depth(IntTy x_44_490_854, IntTy c_45_491_855, CursorTy s_46_492_856)
{
    TagTyPacked tag_1166 = *(TagTyPacked *) s_46_492_856;
    CursorTy tail_1167 = s_46_492_856 + sizeof(IntTy);
    
    
  switch_1170:
    ;
    switch (tag_1166) {
        
      case 1:
        {
            IntTy flt_1168 = 0 - 1;
            
            return flt_1168;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__154_47_493_857 =
                  ((Int64Int64CursorCursorProd *) tail_1167)->field0;
            IntTy v_48_494_858 =
                  ((Int64Int64CursorCursorProd *) tail_1167)->field1;
            CursorTy l_49_495_859 =
                     ((Int64Int64CursorCursorProd *) tail_1167)->field2;
            CursorTy r_50_496_860 =
                     ((Int64Int64CursorCursorProd *) tail_1167)->field3;
            BoolTy fltIf_687_861 = x_44_490_854 == v_48_494_858;
            
            if (fltIf_687_861) {
                IntTy flt_1169 = c_45_491_855 + 1;
                
                return flt_1169;
            } else {
                BoolTy fltIf_688_862 = x_44_490_854 <= v_48_494_858;
                
                if (fltIf_688_862) {
                    IntTy fltAppE_689_863 = c_45_491_855 + 1;
                    
                    return depth(x_44_490_854, fltAppE_689_863, l_49_495_859);
                } else {
                    IntTy fltAppE_690_864 = c_45_491_855 + 1;
                    
                    return depth(x_44_490_854, fltAppE_690_864, r_50_496_860);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1166");
            exit(1);
        }
    }
}
CursorTy right(CursorTy x_58_504_865)
{
    TagTyPacked tag_1171 = *(TagTyPacked *) x_58_504_865;
    CursorTy tail_1172 = x_58_504_865 + sizeof(IntTy);
    
    
  switch_1174:
    ;
    switch (tag_1171) {
        
      case 1:
        {
            PtrTy tailift_1173 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1173)->field0 = 1;
            return tailift_1173;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__186_59_505_866 =
                  ((Int64Int64CursorCursorProd *) tail_1172)->field0;
            IntTy wildcard__187_60_506_867 =
                  ((Int64Int64CursorCursorProd *) tail_1172)->field1;
            CursorTy wildcard__188_61_507_868 =
                     ((Int64Int64CursorCursorProd *) tail_1172)->field2;
            CursorTy r_62_508_869 =
                     ((Int64Int64CursorCursorProd *) tail_1172)->field3;
            
            return r_62_508_869;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1171");
            exit(1);
        }
    }
}
CursorTy left(CursorTy x_63_509_870)
{
    TagTyPacked tag_1175 = *(TagTyPacked *) x_63_509_870;
    CursorTy tail_1176 = x_63_509_870 + sizeof(IntTy);
    
    
  switch_1178:
    ;
    switch (tag_1175) {
        
      case 1:
        {
            PtrTy tailift_1177 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1177)->field0 = 1;
            return tailift_1177;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__177_64_510_871 =
                  ((Int64Int64CursorCursorProd *) tail_1176)->field0;
            IntTy wildcard__178_65_511_872 =
                  ((Int64Int64CursorCursorProd *) tail_1176)->field1;
            CursorTy l_66_512_873 =
                     ((Int64Int64CursorCursorProd *) tail_1176)->field2;
            CursorTy wildcard__179_67_513_874 =
                     ((Int64Int64CursorCursorProd *) tail_1176)->field3;
            
            return l_66_512_873;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1175");
            exit(1);
        }
    }
}
IntTy val(CursorTy x_68_514_875)
{
    TagTyPacked tag_1179 = *(TagTyPacked *) x_68_514_875;
    CursorTy tail_1180 = x_68_514_875 + sizeof(IntTy);
    
    
  switch_1181:
    ;
    switch (tag_1179) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__168_69_515_876 =
                  ((Int64Int64CursorCursorProd *) tail_1180)->field0;
            IntTy v_70_516_877 =
                  ((Int64Int64CursorCursorProd *) tail_1180)->field1;
            CursorTy wildcard__169_71_517_878 =
                     ((Int64Int64CursorCursorProd *) tail_1180)->field2;
            CursorTy wildcard__170_72_518_879 =
                     ((Int64Int64CursorCursorProd *) tail_1180)->field3;
            
            return v_70_516_877;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1179");
            exit(1);
        }
    }
}
IntTy size(CursorTy s_73_519_880)
{
    TagTyPacked tag_1182 = *(TagTyPacked *) s_73_519_880;
    CursorTy tail_1183 = s_73_519_880 + sizeof(IntTy);
    
    
  switch_1184:
    ;
    switch (tag_1182) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy sz_74_520_881 =
                  ((Int64Int64CursorCursorProd *) tail_1183)->field0;
            IntTy wildcard__160_75_521_882 =
                  ((Int64Int64CursorCursorProd *) tail_1183)->field1;
            CursorTy wildcard__161_76_522_883 =
                     ((Int64Int64CursorCursorProd *) tail_1183)->field2;
            CursorTy wildcard__162_77_523_884 =
                     ((Int64Int64CursorCursorProd *) tail_1183)->field3;
            
            return sz_74_520_881;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1182");
            exit(1);
        }
    }
}
CursorTy balanceR(IntTy x_89_535_885, CursorTy l_90_536_886,
                  CursorTy r_91_537_887)
{
    TagTyPacked tag_1185 = *(TagTyPacked *) l_90_536_886;
    CursorTy tail_1186 = l_90_536_886 + sizeof(IntTy);
    
    
  switch_1187:
    ;
    switch (tag_1185) {
        
      case 1:
        {
            return caseFn_433(x_89_535_885, r_91_537_887);
            break;
        }
        
      case 0:
        {
            IntTy ls_108_538_888 =
                  ((Int64Int64CursorCursorProd *) tail_1186)->field0;
            IntTy wildcard__116_109_539_889 =
                  ((Int64Int64CursorCursorProd *) tail_1186)->field1;
            CursorTy wildcard__117_110_540_890 =
                     ((Int64Int64CursorCursorProd *) tail_1186)->field2;
            CursorTy wildcard__118_111_541_891 =
                     ((Int64Int64CursorCursorProd *) tail_1186)->field3;
            
            return caseFn_436(l_90_536_886, x_89_535_885, r_91_537_887,
                              ls_108_538_888);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1185");
            exit(1);
        }
    }
}
CursorTy balanceL(IntTy x_116_542_892, CursorTy l_117_543_893,
                  CursorTy r_118_544_894)
{
    TagTyPacked tag_1188 = *(TagTyPacked *) r_118_544_894;
    CursorTy tail_1189 = r_118_544_894 + sizeof(IntTy);
    
    
  switch_1190:
    ;
    switch (tag_1188) {
        
      case 1:
        {
            return caseFn_460(l_117_543_893, x_116_542_892);
            break;
        }
        
      case 0:
        {
            IntTy rs_135_545_895 =
                  ((Int64Int64CursorCursorProd *) tail_1189)->field0;
            IntTy wildcard__55_136_546_896 =
                  ((Int64Int64CursorCursorProd *) tail_1189)->field1;
            CursorTy wildcard__56_137_547_897 =
                     ((Int64Int64CursorCursorProd *) tail_1189)->field2;
            CursorTy wildcard__57_138_548_898 =
                     ((Int64Int64CursorCursorProd *) tail_1189)->field3;
            
            return caseFn_463(r_118_544_894, l_117_543_893, x_116_542_892,
                              rs_135_545_895);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1188");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_IntSet(CursorTy arg_380_549_899)
{
    TagTyPacked tag_1191 = *(TagTyPacked *) arg_380_549_899;
    CursorTy tail_1192 = arg_380_549_899 + sizeof(IntTy);
    
    
  switch_1195:
    ;
    switch (tag_1191) {
        
      case 0:
        {
            IntTy x_381_550_900 =
                  ((Int64Int64CursorCursorProd *) tail_1192)->field0;
            IntTy x_382_551_901 =
                  ((Int64Int64CursorCursorProd *) tail_1192)->field1;
            CursorTy x_383_552_902 =
                     ((Int64Int64CursorCursorProd *) tail_1192)->field2;
            CursorTy x_384_553_903 =
                     ((Int64Int64CursorCursorProd *) tail_1192)->field3;
            CursorTy y_387_556_906 =  _copy_without_ptrs_IntSet(x_383_552_902);
            CursorTy y_388_557_907 =  _copy_without_ptrs_IntSet(x_384_553_903);
            PtrTy tailift_1193 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1193)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1193)->field1 =
                x_381_550_900;
            ((Int64Int64Int64CursorCursorProd *) tailift_1193)->field2 =
                x_382_551_901;
            ((Int64Int64Int64CursorCursorProd *) tailift_1193)->field3 =
                y_387_556_906;
            ((Int64Int64Int64CursorCursorProd *) tailift_1193)->field4 =
                y_388_557_907;
            return tailift_1193;
            break;
        }
        
      case 1:
        {
            PtrTy tailift_1194 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1194)->field0 = 1;
            return tailift_1194;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1191");
            exit(1);
        }
    }
}
CursorTy _copy_IntSet(CursorTy arg_371_558_908)
{
    TagTyPacked tag_1196 = *(TagTyPacked *) arg_371_558_908;
    CursorTy tail_1197 = arg_371_558_908 + sizeof(IntTy);
    
    
  switch_1200:
    ;
    switch (tag_1196) {
        
      case 0:
        {
            IntTy x_372_559_909 =
                  ((Int64Int64CursorCursorProd *) tail_1197)->field0;
            IntTy x_373_560_910 =
                  ((Int64Int64CursorCursorProd *) tail_1197)->field1;
            CursorTy x_374_561_911 =
                     ((Int64Int64CursorCursorProd *) tail_1197)->field2;
            CursorTy x_375_562_912 =
                     ((Int64Int64CursorCursorProd *) tail_1197)->field3;
            CursorTy y_378_565_915 =  _copy_IntSet(x_374_561_911);
            CursorTy y_379_566_916 =  _copy_IntSet(x_375_562_912);
            PtrTy tailift_1198 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1198)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1198)->field1 =
                x_372_559_909;
            ((Int64Int64Int64CursorCursorProd *) tailift_1198)->field2 =
                x_373_560_910;
            ((Int64Int64Int64CursorCursorProd *) tailift_1198)->field3 =
                y_378_565_915;
            ((Int64Int64Int64CursorCursorProd *) tailift_1198)->field4 =
                y_379_566_916;
            return tailift_1198;
            break;
        }
        
      case 1:
        {
            PtrTy tailift_1199 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1199)->field0 = 1;
            return tailift_1199;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1196");
            exit(1);
        }
    }
}
unsigned char _traverse_IntSet(CursorTy arg_389_567_917)
{
    TagTyPacked tag_1201 = *(TagTyPacked *) arg_389_567_917;
    CursorTy tail_1202 = arg_389_567_917 + sizeof(IntTy);
    
    
  switch_1203:
    ;
    switch (tag_1201) {
        
      case 0:
        {
            IntTy x_390_568_918 =
                  ((Int64Int64CursorCursorProd *) tail_1202)->field0;
            IntTy x_391_569_919 =
                  ((Int64Int64CursorCursorProd *) tail_1202)->field1;
            CursorTy x_392_570_920 =
                     ((Int64Int64CursorCursorProd *) tail_1202)->field2;
            CursorTy x_393_571_921 =
                     ((Int64Int64CursorCursorProd *) tail_1202)->field3;
            unsigned char y_396_572_922 =  _traverse_IntSet(x_392_570_920);
            unsigned char y_397_573_923 =  _traverse_IntSet(x_393_571_921);
            
            return 0;
            break;
        }
        
      case 1:
        {
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1201");
            exit(1);
        }
    }
}
unsigned char _print_IntSet(CursorTy arg_398_574_924)
{
    TagTyPacked tag_1204 = *(TagTyPacked *) arg_398_574_924;
    CursorTy tail_1205 = arg_398_574_924 + sizeof(IntTy);
    
    
  switch_1206:
    ;
    switch (tag_1204) {
        
      case 0:
        {
            IntTy x_399_575_925 =
                  ((Int64Int64CursorCursorProd *) tail_1205)->field0;
            IntTy x_400_576_926 =
                  ((Int64Int64CursorCursorProd *) tail_1205)->field1;
            CursorTy x_401_577_927 =
                     ((Int64Int64CursorCursorProd *) tail_1205)->field2;
            CursorTy x_402_578_928 =
                     ((Int64Int64CursorCursorProd *) tail_1205)->field3;
            unsigned char wildcard_407_579_929 = print_symbol(1159);
            unsigned char wildcard_412_580_930 = print_symbol(1161);
            unsigned char y_403_581_931 = printf("%lld", x_399_575_925);
            unsigned char wildcard_411_582_932 = print_symbol(1161);
            unsigned char y_404_583_933 = printf("%lld", x_400_576_926);
            unsigned char wildcard_410_584_934 = print_symbol(1161);
            unsigned char y_405_585_935 =  _print_IntSet(x_401_577_927);
            unsigned char wildcard_409_586_936 = print_symbol(1161);
            unsigned char y_406_587_937 =  _print_IntSet(x_402_578_928);
            unsigned char wildcard_408_588_938 = print_symbol(1158);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard_413_589_939 = print_symbol(1160);
            unsigned char wildcard_414_590_940 = print_symbol(1158);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1204");
            exit(1);
        }
    }
}
CursorTy caseFn_415(IntTy x_89_416_591_941, CursorTy rr_95_417_592_942,
                    IntTy rx_93_418_593_943, IntTy rs_92_419_594_944,
                    CursorTy rl_94_420_595_945)
{
    TagTyPacked tag_1207 = *(TagTyPacked *) rl_94_420_595_945;
    CursorTy tail_1208 = rl_94_420_595_945 + sizeof(IntTy);
    
    
  switch_1212:
    ;
    switch (tag_1207) {
        
      case 0:
        {
            IntTy rls_100_596_946 =
                  ((Int64Int64CursorCursorProd *) tail_1208)->field0;
            IntTy rlx_101_597_947 =
                  ((Int64Int64CursorCursorProd *) tail_1208)->field1;
            CursorTy rll_102_598_948 =
                     ((Int64Int64CursorCursorProd *) tail_1208)->field2;
            CursorTy rlr_103_599_949 =
                     ((Int64Int64CursorCursorProd *) tail_1208)->field3;
            IntTy fltPrm_692_950 =  size(rl_94_420_595_945);
            IntTy fltPrm_694_951 =  size(rr_95_417_592_942);
            IntTy fltPrm_693_952 = 2 * fltPrm_694_951;
            BoolTy fltIf_691_953 = fltPrm_692_950 < fltPrm_693_952;
            
            if (fltIf_691_953) {
                IntTy fltPkd_695_954 = 1 + rs_92_419_594_944;
                IntTy fltPrm_698_955 =  size(rl_94_420_595_945);
                IntTy fltPkd_697_956 = 1 + fltPrm_698_955;
                PtrTy fltPkd_699_957 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_699_957)->field0 = 1;
                
                PtrTy fltPkd_696_958 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_696_958)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_696_958)->field1 =
                    fltPkd_697_956;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_696_958)->field2 =
                    x_89_416_591_941;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_696_958)->field3 =
                    fltPkd_699_957;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_696_958)->field4 =
                    rl_94_420_595_945;
                
                PtrTy tailift_1209 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1209)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1209)->field1 =
                    fltPkd_695_954;
                ((Int64Int64Int64CursorCursorProd *) tailift_1209)->field2 =
                    rx_93_418_593_943;
                ((Int64Int64Int64CursorCursorProd *) tailift_1209)->field3 =
                    fltPkd_696_958;
                ((Int64Int64Int64CursorCursorProd *) tailift_1209)->field4 =
                    rr_95_417_592_942;
                return tailift_1209;
            } else {
                IntTy fltPkd_700_959 = 1 + rs_92_419_594_944;
                IntTy fltPkd_701_960 =  val(rl_94_420_595_945);
                CursorTy fltAppE_705_961 =  left(rl_94_420_595_945);
                IntTy fltPrm_704_962 =  size(fltAppE_705_961);
                IntTy fltPkd_703_963 = 1 + fltPrm_704_962;
                PtrTy fltPkd_706_964 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_706_964)->field0 = 1;
                
                CursorTy fltPkd_707_965 =  left(rl_94_420_595_945);
                PtrTy fltPkd_702_966 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_702_966)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_702_966)->field1 =
                    fltPkd_703_963;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_702_966)->field2 =
                    x_89_416_591_941;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_702_966)->field3 =
                    fltPkd_706_964;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_702_966)->field4 =
                    fltPkd_707_965;
                
                IntTy fltPrm_711_967 =  size(rr_95_417_592_942);
                IntTy fltPrm_710_968 = 1 + fltPrm_711_967;
                CursorTy fltAppE_713_969 =  right(rl_94_420_595_945);
                IntTy fltPrm_712_970 =  size(fltAppE_713_969);
                IntTy fltPkd_709_971 = fltPrm_710_968 + fltPrm_712_970;
                CursorTy fltPkd_714_972 =  right(rl_94_420_595_945);
                PtrTy fltPkd_708_973 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_708_973)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_708_973)->field1 =
                    fltPkd_709_971;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_708_973)->field2 =
                    rx_93_418_593_943;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_708_973)->field3 =
                    fltPkd_714_972;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_708_973)->field4 =
                    rr_95_417_592_942;
                
                PtrTy tailift_1210 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1210)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1210)->field1 =
                    fltPkd_700_959;
                ((Int64Int64Int64CursorCursorProd *) tailift_1210)->field2 =
                    fltPkd_701_960;
                ((Int64Int64Int64CursorCursorProd *) tailift_1210)->field3 =
                    fltPkd_702_966;
                ((Int64Int64Int64CursorCursorProd *) tailift_1210)->field4 =
                    fltPkd_708_973;
                return tailift_1210;
            }
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_716_974 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_716_974)->field0 = 1;
            
            PtrTy fltPkd_717_975 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_717_975)->field0 = 1;
            
            PtrTy fltPkd_715_976 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_715_976)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_715_976)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_715_976)->field2 =
                x_89_416_591_941;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_715_976)->field3 =
                fltPkd_716_974;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_715_976)->field4 =
                fltPkd_717_975;
            
            PtrTy tailift_1211 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1211)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1211)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1211)->field2 =
                rx_93_418_593_943;
            ((Int64Int64Int64CursorCursorProd *) tailift_1211)->field3 =
                fltPkd_715_976;
            ((Int64Int64Int64CursorCursorProd *) tailift_1211)->field4 =
                rr_95_417_592_942;
            return tailift_1211;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1207");
            exit(1);
        }
    }
}
CursorTy caseFn_421(IntTy x_89_422_600_977, CursorTy r_91_423_601_978,
                    IntTy rx_93_424_602_979, CursorTy rl_94_425_603_980)
{
    TagTyPacked tag_1213 = *(TagTyPacked *) rl_94_425_603_980;
    CursorTy tail_1214 = rl_94_425_603_980 + sizeof(IntTy);
    
    
  switch_1217:
    ;
    switch (tag_1213) {
        
      case 0:
        {
            IntTy rls_104_604_981 =
                  ((Int64Int64CursorCursorProd *) tail_1214)->field0;
            IntTy rlx_105_605_982 =
                  ((Int64Int64CursorCursorProd *) tail_1214)->field1;
            CursorTy rll_106_606_983 =
                     ((Int64Int64CursorCursorProd *) tail_1214)->field2;
            CursorTy rlr_107_607_984 =
                     ((Int64Int64CursorCursorProd *) tail_1214)->field3;
            IntTy fltPkd_718_985 =  val(rl_94_425_603_980);
            PtrTy fltPkd_720_986 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_720_986)->field0 = 1;
            
            PtrTy fltPkd_721_987 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_721_987)->field0 = 1;
            
            PtrTy fltPkd_719_988 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_719_988)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_719_988)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_719_988)->field2 =
                x_89_422_600_977;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_719_988)->field3 =
                fltPkd_720_986;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_719_988)->field4 =
                fltPkd_721_987;
            
            PtrTy fltPkd_723_989 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_723_989)->field0 = 1;
            
            PtrTy fltPkd_724_990 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_724_990)->field0 = 1;
            
            PtrTy fltPkd_722_991 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_722_991)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_722_991)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_722_991)->field2 =
                rx_93_424_602_979;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_722_991)->field3 =
                fltPkd_723_989;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_722_991)->field4 =
                fltPkd_724_990;
            
            PtrTy tailift_1215 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1215)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1215)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1215)->field2 =
                fltPkd_718_985;
            ((Int64Int64Int64CursorCursorProd *) tailift_1215)->field3 =
                fltPkd_719_988;
            ((Int64Int64Int64CursorCursorProd *) tailift_1215)->field4 =
                fltPkd_722_991;
            return tailift_1215;
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_725_992 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_725_992)->field0 = 1;
            
            PtrTy tailift_1216 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1216)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1216)->field1 = 2;
            ((Int64Int64Int64CursorCursorProd *) tailift_1216)->field2 =
                x_89_422_600_977;
            ((Int64Int64Int64CursorCursorProd *) tailift_1216)->field3 =
                fltPkd_725_992;
            ((Int64Int64Int64CursorCursorProd *) tailift_1216)->field4 =
                r_91_423_601_978;
            return tailift_1216;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1213");
            exit(1);
        }
    }
}
CursorTy caseFn_426(IntTy x_89_427_608_993, CursorTy r_91_428_609_994,
                    CursorTy rr_95_429_610_995, IntTy rx_93_430_611_996,
                    IntTy rs_92_431_612_997, CursorTy rl_94_432_613_998)
{
    TagTyPacked tag_1218 = *(TagTyPacked *) rr_95_429_610_995;
    CursorTy tail_1219 = rr_95_429_610_995 + sizeof(IntTy);
    
    
  switch_1220:
    ;
    switch (tag_1218) {
        
      case 0:
        {
            IntTy rrs_96_614_999 =
                  ((Int64Int64CursorCursorProd *) tail_1219)->field0;
            IntTy rrx_97_615_1000 =
                  ((Int64Int64CursorCursorProd *) tail_1219)->field1;
            CursorTy rrl_98_616_1001 =
                     ((Int64Int64CursorCursorProd *) tail_1219)->field2;
            CursorTy rrr_99_617_1002 =
                     ((Int64Int64CursorCursorProd *) tail_1219)->field3;
            
            return caseFn_415(x_89_427_608_993, rr_95_429_610_995,
                              rx_93_430_611_996, rs_92_431_612_997,
                              rl_94_432_613_998);
            break;
        }
        
      case 1:
        {
            return caseFn_421(x_89_427_608_993, r_91_428_609_994,
                              rx_93_430_611_996, rl_94_432_613_998);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1218");
            exit(1);
        }
    }
}
CursorTy caseFn_433(IntTy x_89_434_618_1003, CursorTy r_91_435_619_1004)
{
    TagTyPacked tag_1221 = *(TagTyPacked *) r_91_435_619_1004;
    CursorTy tail_1222 = r_91_435_619_1004 + sizeof(IntTy);
    
    
  switch_1224:
    ;
    switch (tag_1221) {
        
      case 1:
        {
            PtrTy fltPkd_726_1005 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_726_1005)->field0 = 1;
            
            PtrTy fltPkd_727_1006 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_727_1006)->field0 = 1;
            
            PtrTy tailift_1223 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1223)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1223)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) tailift_1223)->field2 =
                x_89_434_618_1003;
            ((Int64Int64Int64CursorCursorProd *) tailift_1223)->field3 =
                fltPkd_726_1005;
            ((Int64Int64Int64CursorCursorProd *) tailift_1223)->field4 =
                fltPkd_727_1006;
            return tailift_1223;
            break;
        }
        
      case 0:
        {
            IntTy rs_92_620_1007 =
                  ((Int64Int64CursorCursorProd *) tail_1222)->field0;
            IntTy rx_93_621_1008 =
                  ((Int64Int64CursorCursorProd *) tail_1222)->field1;
            CursorTy rl_94_622_1009 =
                     ((Int64Int64CursorCursorProd *) tail_1222)->field2;
            CursorTy rr_95_623_1010 =
                     ((Int64Int64CursorCursorProd *) tail_1222)->field3;
            
            return caseFn_426(x_89_434_618_1003, r_91_435_619_1004,
                              rr_95_623_1010, rx_93_621_1008, rs_92_620_1007,
                              rl_94_622_1009);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1221");
            exit(1);
        }
    }
}
CursorTy caseFn_436(CursorTy l_90_437_624_1011, IntTy x_89_438_625_1012,
                    CursorTy r_91_439_626_1013, IntTy ls_108_440_627_1014)
{
    TagTyPacked tag_1225 = *(TagTyPacked *) r_91_439_626_1013;
    CursorTy tail_1226 = r_91_439_626_1013 + sizeof(IntTy);
    
    
  switch_1231:
    ;
    switch (tag_1225) {
        
      case 1:
        {
            IntTy fltPkd_728_1015 = 1 + ls_108_440_627_1014;
            PtrTy fltPkd_729_1016 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_729_1016)->field0 = 1;
            
            PtrTy tailift_1227 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1227)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1227)->field1 =
                fltPkd_728_1015;
            ((Int64Int64Int64CursorCursorProd *) tailift_1227)->field2 =
                x_89_438_625_1012;
            ((Int64Int64Int64CursorCursorProd *) tailift_1227)->field3 =
                l_90_437_624_1011;
            ((Int64Int64Int64CursorCursorProd *) tailift_1227)->field4 =
                fltPkd_729_1016;
            return tailift_1227;
            break;
        }
        
      case 0:
        {
            IntTy rs_112_628_1017 =
                  ((Int64Int64CursorCursorProd *) tail_1226)->field0;
            IntTy rx_113_629_1018 =
                  ((Int64Int64CursorCursorProd *) tail_1226)->field1;
            CursorTy rl_114_630_1019 =
                     ((Int64Int64CursorCursorProd *) tail_1226)->field2;
            CursorTy rr_115_631_1020 =
                     ((Int64Int64CursorCursorProd *) tail_1226)->field3;
            IntTy fltPrm_731_1021 = 3 * ls_108_440_627_1014;
            BoolTy fltIf_730_1022 = rs_112_628_1017 > fltPrm_731_1021;
            
            if (fltIf_730_1022) {
                IntTy fltPrm_733_1023 =  size(rl_114_630_1019);
                IntTy fltPrm_735_1024 =  size(rr_115_631_1020);
                IntTy fltPrm_734_1025 = 2 * fltPrm_735_1024;
                BoolTy fltIf_732_1026 = fltPrm_733_1023 < fltPrm_734_1025;
                
                if (fltIf_732_1026) {
                    IntTy fltPrm_737_1027 = 1 + ls_108_440_627_1014;
                    IntTy fltPkd_736_1028 = fltPrm_737_1027 + rs_112_628_1017;
                    IntTy fltPrm_740_1029 = 1 + ls_108_440_627_1014;
                    IntTy fltPrm_741_1030 =  size(rl_114_630_1019);
                    IntTy fltPkd_739_1031 = fltPrm_740_1029 + fltPrm_741_1030;
                    PtrTy fltPkd_738_1032 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_738_1032)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_738_1032)->field1 =
                        fltPkd_739_1031;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_738_1032)->field2 =
                        x_89_438_625_1012;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_738_1032)->field3 =
                        l_90_437_624_1011;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_738_1032)->field4 =
                        rl_114_630_1019;
                    
                    PtrTy tailift_1228 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1228)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1228)->field1 =
                        fltPkd_736_1028;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1228)->field2 =
                        rx_113_629_1018;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1228)->field3 =
                        fltPkd_738_1032;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1228)->field4 =
                        rr_115_631_1020;
                    return tailift_1228;
                } else {
                    IntTy fltPrm_743_1033 = 1 + ls_108_440_627_1014;
                    IntTy fltPkd_742_1034 = fltPrm_743_1033 + rs_112_628_1017;
                    IntTy fltPkd_744_1035 =  val(rl_114_630_1019);
                    IntTy fltPrm_747_1036 = 1 + ls_108_440_627_1014;
                    CursorTy fltAppE_749_1037 =  left(rl_114_630_1019);
                    IntTy fltPrm_748_1038 =  size(fltAppE_749_1037);
                    IntTy fltPkd_746_1039 = fltPrm_747_1036 + fltPrm_748_1038;
                    CursorTy fltPkd_750_1040 =  left(rl_114_630_1019);
                    PtrTy fltPkd_745_1041 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_745_1041)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_745_1041)->field1 =
                        fltPkd_746_1039;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_745_1041)->field2 =
                        x_89_438_625_1012;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_745_1041)->field3 =
                        l_90_437_624_1011;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_745_1041)->field4 =
                        fltPkd_750_1040;
                    
                    IntTy fltPrm_754_1042 =  size(rr_115_631_1020);
                    IntTy fltPrm_753_1043 = 1 + fltPrm_754_1042;
                    CursorTy fltAppE_756_1044 =  right(rl_114_630_1019);
                    IntTy fltPrm_755_1045 =  size(fltAppE_756_1044);
                    IntTy fltPkd_752_1046 = fltPrm_753_1043 + fltPrm_755_1045;
                    CursorTy fltPkd_757_1047 =  right(rl_114_630_1019);
                    PtrTy fltPkd_751_1048 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_751_1048)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_751_1048)->field1 =
                        fltPkd_752_1046;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_751_1048)->field2 =
                        rx_113_629_1018;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_751_1048)->field3 =
                        fltPkd_757_1047;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_751_1048)->field4 =
                        rr_115_631_1020;
                    
                    PtrTy tailift_1229 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1229)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1229)->field1 =
                        fltPkd_742_1034;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1229)->field2 =
                        fltPkd_744_1035;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1229)->field3 =
                        fltPkd_745_1041;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1229)->field4 =
                        fltPkd_751_1048;
                    return tailift_1229;
                }
            } else {
                IntTy fltPrm_759_1049 = 1 + ls_108_440_627_1014;
                IntTy fltPkd_758_1050 = fltPrm_759_1049 + rs_112_628_1017;
                PtrTy tailift_1230 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1230)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1230)->field1 =
                    fltPkd_758_1050;
                ((Int64Int64Int64CursorCursorProd *) tailift_1230)->field2 =
                    x_89_438_625_1012;
                ((Int64Int64Int64CursorCursorProd *) tailift_1230)->field3 =
                    l_90_437_624_1011;
                ((Int64Int64Int64CursorCursorProd *) tailift_1230)->field4 =
                    r_91_439_626_1013;
                return tailift_1230;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1225");
            exit(1);
        }
    }
}
CursorTy caseFn_441(IntTy x_116_442_632_1051, CursorTy ll_121_443_633_1052,
                    IntTy lx_120_444_634_1053, IntTy ls_119_445_635_1054,
                    CursorTy lr_122_446_636_1055, IntTy lls_123_447_637_1056)
{
    TagTyPacked tag_1232 = *(TagTyPacked *) lr_122_446_636_1055;
    CursorTy tail_1233 = lr_122_446_636_1055 + sizeof(IntTy);
    
    
  switch_1237:
    ;
    switch (tag_1232) {
        
      case 0:
        {
            IntTy lrs_127_638_1057 =
                  ((Int64Int64CursorCursorProd *) tail_1233)->field0;
            IntTy lrx_128_639_1058 =
                  ((Int64Int64CursorCursorProd *) tail_1233)->field1;
            CursorTy lrl_129_640_1059 =
                     ((Int64Int64CursorCursorProd *) tail_1233)->field2;
            CursorTy lrr_130_641_1060 =
                     ((Int64Int64CursorCursorProd *) tail_1233)->field3;
            IntTy fltPrm_761_1061 = 2 * lls_123_447_637_1056;
            BoolTy fltIf_760_1062 = lrs_127_638_1057 < fltPrm_761_1061;
            
            if (fltIf_760_1062) {
                IntTy fltPkd_762_1063 = 1 + ls_119_445_635_1054;
                IntTy fltPkd_764_1064 = 1 + lrs_127_638_1057;
                PtrTy fltPkd_765_1065 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_765_1065)->field0 = 1;
                
                PtrTy fltPkd_763_1066 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_763_1066)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_763_1066)->field1 =
                    fltPkd_764_1064;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_763_1066)->field2 =
                    x_116_442_632_1051;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_763_1066)->field3 =
                    lr_122_446_636_1055;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_763_1066)->field4 =
                    fltPkd_765_1065;
                
                PtrTy tailift_1234 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1234)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1234)->field1 =
                    fltPkd_762_1063;
                ((Int64Int64Int64CursorCursorProd *) tailift_1234)->field2 =
                    lx_120_444_634_1053;
                ((Int64Int64Int64CursorCursorProd *) tailift_1234)->field3 =
                    ll_121_443_633_1052;
                ((Int64Int64Int64CursorCursorProd *) tailift_1234)->field4 =
                    fltPkd_763_1066;
                return tailift_1234;
            } else {
                IntTy fltPkd_766_1067 = 1 + ls_119_445_635_1054;
                IntTy fltPrm_769_1068 = 1 + lls_123_447_637_1056;
                IntTy fltPrm_770_1069 =  size(lrl_129_640_1059);
                IntTy fltPkd_768_1070 = fltPrm_769_1068 + fltPrm_770_1069;
                PtrTy fltPkd_767_1071 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_767_1071)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_767_1071)->field1 =
                    fltPkd_768_1070;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_767_1071)->field2 =
                    lx_120_444_634_1053;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_767_1071)->field3 =
                    ll_121_443_633_1052;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_767_1071)->field4 =
                    lrl_129_640_1059;
                
                IntTy fltPrm_773_1072 =  size(lrr_130_641_1060);
                IntTy fltPkd_772_1073 = 1 + fltPrm_773_1072;
                PtrTy fltPkd_774_1074 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_774_1074)->field0 = 1;
                
                PtrTy fltPkd_771_1075 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_771_1075)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_771_1075)->field1 =
                    fltPkd_772_1073;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_771_1075)->field2 =
                    x_116_442_632_1051;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_771_1075)->field3 =
                    lrr_130_641_1060;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_771_1075)->field4 =
                    fltPkd_774_1074;
                
                PtrTy tailift_1235 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1235)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1235)->field1 =
                    fltPkd_766_1067;
                ((Int64Int64Int64CursorCursorProd *) tailift_1235)->field2 =
                    lrx_128_639_1058;
                ((Int64Int64Int64CursorCursorProd *) tailift_1235)->field3 =
                    fltPkd_767_1071;
                ((Int64Int64Int64CursorCursorProd *) tailift_1235)->field4 =
                    fltPkd_771_1075;
                return tailift_1235;
            }
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_776_1076 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_776_1076)->field0 = 1;
            
            PtrTy fltPkd_777_1077 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_777_1077)->field0 = 1;
            
            PtrTy fltPkd_775_1078 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_775_1078)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_775_1078)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_775_1078)->field2 =
                x_116_442_632_1051;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_775_1078)->field3 =
                fltPkd_776_1076;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_775_1078)->field4 =
                fltPkd_777_1077;
            
            PtrTy tailift_1236 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1236)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1236)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1236)->field2 =
                lx_120_444_634_1053;
            ((Int64Int64Int64CursorCursorProd *) tailift_1236)->field3 =
                ll_121_443_633_1052;
            ((Int64Int64Int64CursorCursorProd *) tailift_1236)->field4 =
                fltPkd_775_1078;
            return tailift_1236;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1232");
            exit(1);
        }
    }
}
CursorTy caseFn_448(CursorTy l_117_449_642_1079, IntTy x_116_450_643_1080,
                    IntTy lx_120_451_644_1081, CursorTy lr_122_452_645_1082)
{
    TagTyPacked tag_1238 = *(TagTyPacked *) lr_122_452_645_1082;
    CursorTy tail_1239 = lr_122_452_645_1082 + sizeof(IntTy);
    
    
  switch_1242:
    ;
    switch (tag_1238) {
        
      case 0:
        {
            IntTy lrs_131_646_1083 =
                  ((Int64Int64CursorCursorProd *) tail_1239)->field0;
            IntTy lrx_132_647_1084 =
                  ((Int64Int64CursorCursorProd *) tail_1239)->field1;
            CursorTy lrl_133_648_1085 =
                     ((Int64Int64CursorCursorProd *) tail_1239)->field2;
            CursorTy lrr_134_649_1086 =
                     ((Int64Int64CursorCursorProd *) tail_1239)->field3;
            PtrTy fltPkd_779_1087 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_779_1087)->field0 = 1;
            
            PtrTy fltPkd_780_1088 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_780_1088)->field0 = 1;
            
            PtrTy fltPkd_778_1089 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_778_1089)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_778_1089)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_778_1089)->field2 =
                lx_120_451_644_1081;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_778_1089)->field3 =
                fltPkd_779_1087;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_778_1089)->field4 =
                fltPkd_780_1088;
            
            PtrTy fltPkd_782_1090 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_782_1090)->field0 = 1;
            
            PtrTy fltPkd_783_1091 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_783_1091)->field0 = 1;
            
            PtrTy fltPkd_781_1092 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_781_1092)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_781_1092)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_781_1092)->field2 =
                x_116_450_643_1080;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_781_1092)->field3 =
                fltPkd_782_1090;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_781_1092)->field4 =
                fltPkd_783_1091;
            
            PtrTy tailift_1240 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1240)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1240)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1240)->field2 =
                lrx_132_647_1084;
            ((Int64Int64Int64CursorCursorProd *) tailift_1240)->field3 =
                fltPkd_778_1089;
            ((Int64Int64Int64CursorCursorProd *) tailift_1240)->field4 =
                fltPkd_781_1092;
            return tailift_1240;
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_784_1093 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_784_1093)->field0 = 1;
            
            PtrTy tailift_1241 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1241)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1241)->field1 = 2;
            ((Int64Int64Int64CursorCursorProd *) tailift_1241)->field2 =
                x_116_450_643_1080;
            ((Int64Int64Int64CursorCursorProd *) tailift_1241)->field3 =
                l_117_449_642_1079;
            ((Int64Int64Int64CursorCursorProd *) tailift_1241)->field4 =
                fltPkd_784_1093;
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
CursorTy caseFn_453(CursorTy l_117_454_650_1094, IntTy x_116_455_651_1095,
                    CursorTy ll_121_456_652_1096, IntTy lx_120_457_653_1097,
                    IntTy ls_119_458_654_1098, CursorTy lr_122_459_655_1099)
{
    TagTyPacked tag_1243 = *(TagTyPacked *) ll_121_456_652_1096;
    CursorTy tail_1244 = ll_121_456_652_1096 + sizeof(IntTy);
    
    
  switch_1245:
    ;
    switch (tag_1243) {
        
      case 0:
        {
            IntTy lls_123_656_1100 =
                  ((Int64Int64CursorCursorProd *) tail_1244)->field0;
            IntTy llx_124_657_1101 =
                  ((Int64Int64CursorCursorProd *) tail_1244)->field1;
            CursorTy lll_125_658_1102 =
                     ((Int64Int64CursorCursorProd *) tail_1244)->field2;
            CursorTy llr_126_659_1103 =
                     ((Int64Int64CursorCursorProd *) tail_1244)->field3;
            
            return caseFn_441(x_116_455_651_1095, ll_121_456_652_1096,
                              lx_120_457_653_1097, ls_119_458_654_1098,
                              lr_122_459_655_1099, lls_123_656_1100);
            break;
        }
        
      case 1:
        {
            return caseFn_448(l_117_454_650_1094, x_116_455_651_1095,
                              lx_120_457_653_1097, lr_122_459_655_1099);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1243");
            exit(1);
        }
    }
}
CursorTy caseFn_460(CursorTy l_117_461_660_1104, IntTy x_116_462_661_1105)
{
    TagTyPacked tag_1246 = *(TagTyPacked *) l_117_461_660_1104;
    CursorTy tail_1247 = l_117_461_660_1104 + sizeof(IntTy);
    
    
  switch_1249:
    ;
    switch (tag_1246) {
        
      case 1:
        {
            PtrTy fltPkd_785_1106 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_785_1106)->field0 = 1;
            
            PtrTy fltPkd_786_1107 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_786_1107)->field0 = 1;
            
            PtrTy tailift_1248 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1248)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1248)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) tailift_1248)->field2 =
                x_116_462_661_1105;
            ((Int64Int64Int64CursorCursorProd *) tailift_1248)->field3 =
                fltPkd_785_1106;
            ((Int64Int64Int64CursorCursorProd *) tailift_1248)->field4 =
                fltPkd_786_1107;
            return tailift_1248;
            break;
        }
        
      case 0:
        {
            IntTy ls_119_662_1108 =
                  ((Int64Int64CursorCursorProd *) tail_1247)->field0;
            IntTy lx_120_663_1109 =
                  ((Int64Int64CursorCursorProd *) tail_1247)->field1;
            CursorTy ll_121_664_1110 =
                     ((Int64Int64CursorCursorProd *) tail_1247)->field2;
            CursorTy lr_122_665_1111 =
                     ((Int64Int64CursorCursorProd *) tail_1247)->field3;
            
            return caseFn_453(l_117_461_660_1104, x_116_462_661_1105,
                              ll_121_664_1110, lx_120_663_1109, ls_119_662_1108,
                              lr_122_665_1111);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1246");
            exit(1);
        }
    }
}
CursorTy caseFn_463(CursorTy r_118_464_666_1112, CursorTy l_117_465_667_1113,
                    IntTy x_116_466_668_1114, IntTy rs_135_467_669_1115)
{
    TagTyPacked tag_1250 = *(TagTyPacked *) l_117_465_667_1113;
    CursorTy tail_1251 = l_117_465_667_1113 + sizeof(IntTy);
    
    
  switch_1256:
    ;
    switch (tag_1250) {
        
      case 1:
        {
            IntTy fltPkd_787_1116 = 1 + rs_135_467_669_1115;
            PtrTy fltPkd_788_1117 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_788_1117)->field0 = 1;
            
            PtrTy tailift_1252 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1252)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1252)->field1 =
                fltPkd_787_1116;
            ((Int64Int64Int64CursorCursorProd *) tailift_1252)->field2 =
                x_116_466_668_1114;
            ((Int64Int64Int64CursorCursorProd *) tailift_1252)->field3 =
                fltPkd_788_1117;
            ((Int64Int64Int64CursorCursorProd *) tailift_1252)->field4 =
                r_118_464_666_1112;
            return tailift_1252;
            break;
        }
        
      case 0:
        {
            IntTy ls_139_670_1118 =
                  ((Int64Int64CursorCursorProd *) tail_1251)->field0;
            IntTy lx_140_671_1119 =
                  ((Int64Int64CursorCursorProd *) tail_1251)->field1;
            CursorTy ll_141_672_1120 =
                     ((Int64Int64CursorCursorProd *) tail_1251)->field2;
            CursorTy lr_142_673_1121 =
                     ((Int64Int64CursorCursorProd *) tail_1251)->field3;
            IntTy fltPrm_790_1122 = 3 * rs_135_467_669_1115;
            BoolTy fltIf_789_1123 = ls_139_670_1118 > fltPrm_790_1122;
            
            if (fltIf_789_1123) {
                IntTy fltPrm_792_1124 =  size(lr_142_673_1121);
                IntTy fltPrm_794_1125 =  size(ll_141_672_1120);
                IntTy fltPrm_793_1126 = 2 * fltPrm_794_1125;
                BoolTy fltIf_791_1127 = fltPrm_792_1124 < fltPrm_793_1126;
                
                if (fltIf_791_1127) {
                    IntTy fltPrm_796_1128 = 1 + ls_139_670_1118;
                    IntTy fltPkd_795_1129 = fltPrm_796_1128 +
                          rs_135_467_669_1115;
                    IntTy fltPrm_799_1130 = 1 + rs_135_467_669_1115;
                    IntTy fltPrm_800_1131 =  size(lr_142_673_1121);
                    IntTy fltPkd_798_1132 = fltPrm_799_1130 + fltPrm_800_1131;
                    PtrTy fltPkd_797_1133 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_797_1133)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_797_1133)->field1 =
                        fltPkd_798_1132;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_797_1133)->field2 =
                        x_116_466_668_1114;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_797_1133)->field3 =
                        lr_142_673_1121;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_797_1133)->field4 =
                        r_118_464_666_1112;
                    
                    PtrTy tailift_1253 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1253)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1253)->field1 =
                        fltPkd_795_1129;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1253)->field2 =
                        lx_140_671_1119;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1253)->field3 =
                        ll_141_672_1120;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1253)->field4 =
                        fltPkd_797_1133;
                    return tailift_1253;
                } else {
                    IntTy fltPrm_802_1134 = 1 + ls_139_670_1118;
                    IntTy fltPkd_801_1135 = fltPrm_802_1134 +
                          rs_135_467_669_1115;
                    IntTy fltPkd_803_1136 =  val(lr_142_673_1121);
                    IntTy fltPrm_807_1137 =  size(ll_141_672_1120);
                    IntTy fltPrm_806_1138 = 1 + fltPrm_807_1137;
                    CursorTy fltAppE_809_1139 =  left(lr_142_673_1121);
                    IntTy fltPrm_808_1140 =  size(fltAppE_809_1139);
                    IntTy fltPkd_805_1141 = fltPrm_806_1138 + fltPrm_808_1140;
                    CursorTy fltPkd_810_1142 =  left(lr_142_673_1121);
                    PtrTy fltPkd_804_1143 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1143)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1143)->field1 =
                        fltPkd_805_1141;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1143)->field2 =
                        lx_140_671_1119;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1143)->field3 =
                        ll_141_672_1120;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1143)->field4 =
                        fltPkd_810_1142;
                    
                    IntTy fltPrm_813_1144 = 1 + rs_135_467_669_1115;
                    CursorTy fltAppE_815_1145 =  right(lr_142_673_1121);
                    IntTy fltPrm_814_1146 =  size(fltAppE_815_1145);
                    IntTy fltPkd_812_1147 = fltPrm_813_1144 + fltPrm_814_1146;
                    CursorTy fltPkd_816_1148 =  right(lr_142_673_1121);
                    PtrTy fltPkd_811_1149 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_811_1149)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_811_1149)->field1 =
                        fltPkd_812_1147;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_811_1149)->field2 =
                        x_116_466_668_1114;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_811_1149)->field3 =
                        fltPkd_816_1148;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_811_1149)->field4 =
                        r_118_464_666_1112;
                    
                    PtrTy tailift_1254 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1254)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1254)->field1 =
                        fltPkd_801_1135;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1254)->field2 =
                        fltPkd_803_1136;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1254)->field3 =
                        fltPkd_804_1143;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1254)->field4 =
                        fltPkd_811_1149;
                    return tailift_1254;
                }
            } else {
                IntTy fltPrm_818_1150 = 1 + ls_139_670_1118;
                IntTy fltPkd_817_1151 = fltPrm_818_1150 + rs_135_467_669_1115;
                PtrTy tailift_1255 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1255)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1255)->field1 =
                    fltPkd_817_1151;
                ((Int64Int64Int64CursorCursorProd *) tailift_1255)->field2 =
                    x_116_466_668_1114;
                ((Int64Int64Int64CursorCursorProd *) tailift_1255)->field3 =
                    l_117_465_667_1113;
                ((Int64Int64Int64CursorCursorProd *) tailift_1255)->field4 =
                    r_118_464_666_1112;
                return tailift_1255;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1250");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1154, "]\n");
    add_symbol(1155, "ID: [");
    add_symbol(1156, "DEPTH: [");
    add_symbol(1157, "---\n");
    add_symbol(1158, ")");
    add_symbol(1159, "(PureSet");
    add_symbol(1160, "(EmptySet");
    add_symbol(1161, " ");
    
    IntTy fltAppE_674_820 = 16 / 2;
    CursorTy fltAppE_675_821 =  build(16, 16);
    IntTy tmp_app_1152 =  eval(fltAppE_674_820, fltAppE_675_821);
    
    printf("%lld", tmp_app_1152);
    printf("\n");
    free_symtable();
    return 0;
}