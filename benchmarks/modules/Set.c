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
CursorTy empty();
IntTy sum(CursorTy s_194_596_921);
CursorTy right(CursorTy x_201_603_929);
CursorTy left(CursorTy x_206_608_934);
IntTy val(CursorTy x_211_613_939);
IntTy size(CursorTy s_216_618_944);
CursorTy balanceR(IntTy x_232_634_949, CursorTy l_233_635_950,
                  CursorTy r_234_636_951);
CursorTy balanceL(IntTy x_259_641_956, CursorTy l_260_642_957,
                  CursorTy r_261_643_958);
CursorTy insert(IntTy x_286_648_963, CursorTy s_287_649_964);
CursorTy singleton(IntTy x_294_656_973);
CursorTy _copy_without_ptrs_IntSet(CursorTy arg_508_657_976);
CursorTy _copy_IntSet(CursorTy arg_499_666_985);
unsigned char _traverse_IntSet(CursorTy arg_517_675_994);
unsigned char _print_IntSet(CursorTy arg_526_682_1001);
CursorTy caseFn_543(IntTy x_232_544_699_1018, CursorTy rr_238_545_700_1019,
                    IntTy rx_236_546_701_1020, IntTy rs_235_547_702_1021,
                    CursorTy rl_237_548_703_1022);
CursorTy caseFn_549(IntTy x_232_550_708_1054, CursorTy r_234_551_709_1055,
                    IntTy rx_236_552_710_1056, CursorTy rl_237_553_711_1057);
CursorTy caseFn_554(IntTy x_232_555_716_1070, CursorTy r_234_556_717_1071,
                    CursorTy rr_238_557_718_1072, IntTy rx_236_558_719_1073,
                    IntTy rs_235_559_720_1074, CursorTy rl_237_560_721_1075);
CursorTy caseFn_561(IntTy x_232_562_726_1080, CursorTy r_234_563_727_1081);
CursorTy caseFn_564(CursorTy l_233_565_732_1088, IntTy x_232_566_733_1089,
                    CursorTy r_234_567_734_1090, IntTy ls_251_568_735_1091);
CursorTy caseFn_569(IntTy x_259_570_740_1128, CursorTy ll_264_571_741_1129,
                    IntTy lx_263_572_742_1130, IntTy ls_262_573_743_1131,
                    CursorTy lr_265_574_744_1132, IntTy lls_266_575_745_1133);
CursorTy caseFn_576(CursorTy l_260_577_750_1156, IntTy x_259_578_751_1157,
                    IntTy lx_263_579_752_1158, CursorTy lr_265_580_753_1159);
CursorTy caseFn_581(CursorTy l_260_582_758_1171, IntTy x_259_583_759_1172,
                    CursorTy ll_264_584_760_1173, IntTy lx_263_585_761_1174,
                    IntTy ls_262_586_762_1175, CursorTy lr_265_587_763_1176);
CursorTy caseFn_588(CursorTy l_260_589_768_1181, IntTy x_259_590_769_1182);
CursorTy caseFn_591(CursorTy r_261_592_774_1189, CursorTy l_260_593_775_1190,
                    IntTy x_259_594_776_1191, IntTy rs_278_595_777_1192);
CursorTy empty()
{
    PtrTy tailift_1234 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1234)->field0 = 1;
    return tailift_1234;
}
IntTy sum(CursorTy s_194_596_921)
{
    TagTyPacked tag_1235 = *(TagTyPacked *) s_194_596_921;
    CursorTy tail_1236 = s_194_596_921 + sizeof(IntTy);
    
    
  switch_1238:
    ;
    switch (tag_1235) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__188_195_597_922 =
                  ((Int64Int64CursorCursorProd *) tail_1236)->field0;
            IntTy v_196_598_923 =
                  ((Int64Int64CursorCursorProd *) tail_1236)->field1;
            CursorTy l_197_599_924 =
                     ((Int64Int64CursorCursorProd *) tail_1236)->field2;
            CursorTy r_198_600_925 =
                     ((Int64Int64CursorCursorProd *) tail_1236)->field3;
            IntTy fltPrm_785_926 =  sum(l_197_599_924);
            IntTy fltPrm_784_927 = v_196_598_923 + fltPrm_785_926;
            IntTy fltPrm_786_928 =  sum(r_198_600_925);
            IntTy flt_1237 = fltPrm_784_927 + fltPrm_786_928;
            
            return flt_1237;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1235");
            exit(1);
        }
    }
}
CursorTy right(CursorTy x_201_603_929)
{
    TagTyPacked tag_1239 = *(TagTyPacked *) x_201_603_929;
    CursorTy tail_1240 = x_201_603_929 + sizeof(IntTy);
    
    
  switch_1242:
    ;
    switch (tag_1239) {
        
      case 1:
        {
            PtrTy tailift_1241 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1241)->field0 = 1;
            return tailift_1241;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__178_202_604_930 =
                  ((Int64Int64CursorCursorProd *) tail_1240)->field0;
            IntTy wildcard__179_203_605_931 =
                  ((Int64Int64CursorCursorProd *) tail_1240)->field1;
            CursorTy wildcard__180_204_606_932 =
                     ((Int64Int64CursorCursorProd *) tail_1240)->field2;
            CursorTy r_205_607_933 =
                     ((Int64Int64CursorCursorProd *) tail_1240)->field3;
            
            return r_205_607_933;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1239");
            exit(1);
        }
    }
}
CursorTy left(CursorTy x_206_608_934)
{
    TagTyPacked tag_1243 = *(TagTyPacked *) x_206_608_934;
    CursorTy tail_1244 = x_206_608_934 + sizeof(IntTy);
    
    
  switch_1246:
    ;
    switch (tag_1243) {
        
      case 1:
        {
            PtrTy tailift_1245 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1245)->field0 = 1;
            return tailift_1245;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__169_207_609_935 =
                  ((Int64Int64CursorCursorProd *) tail_1244)->field0;
            IntTy wildcard__170_208_610_936 =
                  ((Int64Int64CursorCursorProd *) tail_1244)->field1;
            CursorTy l_209_611_937 =
                     ((Int64Int64CursorCursorProd *) tail_1244)->field2;
            CursorTy wildcard__171_210_612_938 =
                     ((Int64Int64CursorCursorProd *) tail_1244)->field3;
            
            return l_209_611_937;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1243");
            exit(1);
        }
    }
}
IntTy val(CursorTy x_211_613_939)
{
    TagTyPacked tag_1247 = *(TagTyPacked *) x_211_613_939;
    CursorTy tail_1248 = x_211_613_939 + sizeof(IntTy);
    
    
  switch_1249:
    ;
    switch (tag_1247) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__160_212_614_940 =
                  ((Int64Int64CursorCursorProd *) tail_1248)->field0;
            IntTy v_213_615_941 =
                  ((Int64Int64CursorCursorProd *) tail_1248)->field1;
            CursorTy wildcard__161_214_616_942 =
                     ((Int64Int64CursorCursorProd *) tail_1248)->field2;
            CursorTy wildcard__162_215_617_943 =
                     ((Int64Int64CursorCursorProd *) tail_1248)->field3;
            
            return v_213_615_941;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1247");
            exit(1);
        }
    }
}
IntTy size(CursorTy s_216_618_944)
{
    TagTyPacked tag_1250 = *(TagTyPacked *) s_216_618_944;
    CursorTy tail_1251 = s_216_618_944 + sizeof(IntTy);
    
    
  switch_1252:
    ;
    switch (tag_1250) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy sz_217_619_945 =
                  ((Int64Int64CursorCursorProd *) tail_1251)->field0;
            IntTy wildcard__152_218_620_946 =
                  ((Int64Int64CursorCursorProd *) tail_1251)->field1;
            CursorTy wildcard__153_219_621_947 =
                     ((Int64Int64CursorCursorProd *) tail_1251)->field2;
            CursorTy wildcard__154_220_622_948 =
                     ((Int64Int64CursorCursorProd *) tail_1251)->field3;
            
            return sz_217_619_945;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1250");
            exit(1);
        }
    }
}
CursorTy balanceR(IntTy x_232_634_949, CursorTy l_233_635_950,
                  CursorTy r_234_636_951)
{
    TagTyPacked tag_1253 = *(TagTyPacked *) l_233_635_950;
    CursorTy tail_1254 = l_233_635_950 + sizeof(IntTy);
    
    
  switch_1255:
    ;
    switch (tag_1253) {
        
      case 1:
        {
            return caseFn_561(x_232_634_949, r_234_636_951);
            break;
        }
        
      case 0:
        {
            IntTy ls_251_637_952 =
                  ((Int64Int64CursorCursorProd *) tail_1254)->field0;
            IntTy wildcard__116_252_638_953 =
                  ((Int64Int64CursorCursorProd *) tail_1254)->field1;
            CursorTy wildcard__117_253_639_954 =
                     ((Int64Int64CursorCursorProd *) tail_1254)->field2;
            CursorTy wildcard__118_254_640_955 =
                     ((Int64Int64CursorCursorProd *) tail_1254)->field3;
            
            return caseFn_564(l_233_635_950, x_232_634_949, r_234_636_951,
                              ls_251_637_952);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1253");
            exit(1);
        }
    }
}
CursorTy balanceL(IntTy x_259_641_956, CursorTy l_260_642_957,
                  CursorTy r_261_643_958)
{
    TagTyPacked tag_1256 = *(TagTyPacked *) r_261_643_958;
    CursorTy tail_1257 = r_261_643_958 + sizeof(IntTy);
    
    
  switch_1258:
    ;
    switch (tag_1256) {
        
      case 1:
        {
            return caseFn_588(l_260_642_957, x_259_641_956);
            break;
        }
        
      case 0:
        {
            IntTy rs_278_644_959 =
                  ((Int64Int64CursorCursorProd *) tail_1257)->field0;
            IntTy wildcard__55_279_645_960 =
                  ((Int64Int64CursorCursorProd *) tail_1257)->field1;
            CursorTy wildcard__56_280_646_961 =
                     ((Int64Int64CursorCursorProd *) tail_1257)->field2;
            CursorTy wildcard__57_281_647_962 =
                     ((Int64Int64CursorCursorProd *) tail_1257)->field3;
            
            return caseFn_591(r_261_643_958, l_260_642_957, x_259_641_956,
                              rs_278_644_959);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1256");
            exit(1);
        }
    }
}
CursorTy insert(IntTy x_286_648_963, CursorTy s_287_649_964)
{
    TagTyPacked tag_1259 = *(TagTyPacked *) s_287_649_964;
    CursorTy tail_1260 = s_287_649_964 + sizeof(IntTy);
    
    
  switch_1261:
    ;
    switch (tag_1259) {
        
      case 1:
        {
            return singleton(x_286_648_963);
            break;
        }
        
      case 0:
        {
            IntTy sz_288_650_965 =
                  ((Int64Int64CursorCursorProd *) tail_1260)->field0;
            IntTy v_289_651_966 =
                  ((Int64Int64CursorCursorProd *) tail_1260)->field1;
            CursorTy l_290_652_967 =
                     ((Int64Int64CursorCursorProd *) tail_1260)->field2;
            CursorTy r_291_653_968 =
                     ((Int64Int64CursorCursorProd *) tail_1260)->field3;
            BoolTy fltIf_787_969 = x_286_648_963 == v_289_651_966;
            
            if (fltIf_787_969) {
                return s_287_649_964;
            } else {
                BoolTy fltIf_788_970 = x_286_648_963 <= v_289_651_966;
                
                if (fltIf_788_970) {
                    CursorTy nl_292_654_971 =
                              insert(x_286_648_963, l_290_652_967);
                    
                    return balanceL(v_289_651_966, nl_292_654_971,
                                    r_291_653_968);
                } else {
                    CursorTy nr_293_655_972 =
                              insert(x_286_648_963, r_291_653_968);
                    
                    return balanceR(v_289_651_966, l_290_652_967,
                                    nr_293_655_972);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1259");
            exit(1);
        }
    }
}
CursorTy singleton(IntTy x_294_656_973)
{
    PtrTy fltPkd_789_974 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_789_974)->field0 = 1;
    
    PtrTy fltPkd_790_975 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_790_975)->field0 = 1;
    
    PtrTy tailift_1262 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64CursorCursorProd *) tailift_1262)->field0 = 0;
    ((Int64Int64Int64CursorCursorProd *) tailift_1262)->field1 = 1;
    ((Int64Int64Int64CursorCursorProd *) tailift_1262)->field2 = x_294_656_973;
    ((Int64Int64Int64CursorCursorProd *) tailift_1262)->field3 = fltPkd_789_974;
    ((Int64Int64Int64CursorCursorProd *) tailift_1262)->field4 = fltPkd_790_975;
    return tailift_1262;
}
CursorTy _copy_without_ptrs_IntSet(CursorTy arg_508_657_976)
{
    TagTyPacked tag_1263 = *(TagTyPacked *) arg_508_657_976;
    CursorTy tail_1264 = arg_508_657_976 + sizeof(IntTy);
    
    
  switch_1267:
    ;
    switch (tag_1263) {
        
      case 0:
        {
            IntTy x_509_658_977 =
                  ((Int64Int64CursorCursorProd *) tail_1264)->field0;
            IntTy x_510_659_978 =
                  ((Int64Int64CursorCursorProd *) tail_1264)->field1;
            CursorTy x_511_660_979 =
                     ((Int64Int64CursorCursorProd *) tail_1264)->field2;
            CursorTy x_512_661_980 =
                     ((Int64Int64CursorCursorProd *) tail_1264)->field3;
            CursorTy y_515_664_983 =  _copy_without_ptrs_IntSet(x_511_660_979);
            CursorTy y_516_665_984 =  _copy_without_ptrs_IntSet(x_512_661_980);
            PtrTy tailift_1265 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1265)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1265)->field1 =
                x_509_658_977;
            ((Int64Int64Int64CursorCursorProd *) tailift_1265)->field2 =
                x_510_659_978;
            ((Int64Int64Int64CursorCursorProd *) tailift_1265)->field3 =
                y_515_664_983;
            ((Int64Int64Int64CursorCursorProd *) tailift_1265)->field4 =
                y_516_665_984;
            return tailift_1265;
            break;
        }
        
      case 1:
        {
            PtrTy tailift_1266 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1266)->field0 = 1;
            return tailift_1266;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1263");
            exit(1);
        }
    }
}
CursorTy _copy_IntSet(CursorTy arg_499_666_985)
{
    TagTyPacked tag_1268 = *(TagTyPacked *) arg_499_666_985;
    CursorTy tail_1269 = arg_499_666_985 + sizeof(IntTy);
    
    
  switch_1272:
    ;
    switch (tag_1268) {
        
      case 0:
        {
            IntTy x_500_667_986 =
                  ((Int64Int64CursorCursorProd *) tail_1269)->field0;
            IntTy x_501_668_987 =
                  ((Int64Int64CursorCursorProd *) tail_1269)->field1;
            CursorTy x_502_669_988 =
                     ((Int64Int64CursorCursorProd *) tail_1269)->field2;
            CursorTy x_503_670_989 =
                     ((Int64Int64CursorCursorProd *) tail_1269)->field3;
            CursorTy y_506_673_992 =  _copy_IntSet(x_502_669_988);
            CursorTy y_507_674_993 =  _copy_IntSet(x_503_670_989);
            PtrTy tailift_1270 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1270)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1270)->field1 =
                x_500_667_986;
            ((Int64Int64Int64CursorCursorProd *) tailift_1270)->field2 =
                x_501_668_987;
            ((Int64Int64Int64CursorCursorProd *) tailift_1270)->field3 =
                y_506_673_992;
            ((Int64Int64Int64CursorCursorProd *) tailift_1270)->field4 =
                y_507_674_993;
            return tailift_1270;
            break;
        }
        
      case 1:
        {
            PtrTy tailift_1271 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1271)->field0 = 1;
            return tailift_1271;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1268");
            exit(1);
        }
    }
}
unsigned char _traverse_IntSet(CursorTy arg_517_675_994)
{
    TagTyPacked tag_1273 = *(TagTyPacked *) arg_517_675_994;
    CursorTy tail_1274 = arg_517_675_994 + sizeof(IntTy);
    
    
  switch_1275:
    ;
    switch (tag_1273) {
        
      case 0:
        {
            IntTy x_518_676_995 =
                  ((Int64Int64CursorCursorProd *) tail_1274)->field0;
            IntTy x_519_677_996 =
                  ((Int64Int64CursorCursorProd *) tail_1274)->field1;
            CursorTy x_520_678_997 =
                     ((Int64Int64CursorCursorProd *) tail_1274)->field2;
            CursorTy x_521_679_998 =
                     ((Int64Int64CursorCursorProd *) tail_1274)->field3;
            unsigned char y_524_680_999 =  _traverse_IntSet(x_520_678_997);
            unsigned char y_525_681_1000 =  _traverse_IntSet(x_521_679_998);
            
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
            printf("%s\n", "Unknown tag in: tag_1273");
            exit(1);
        }
    }
}
unsigned char _print_IntSet(CursorTy arg_526_682_1001)
{
    TagTyPacked tag_1276 = *(TagTyPacked *) arg_526_682_1001;
    CursorTy tail_1277 = arg_526_682_1001 + sizeof(IntTy);
    
    
  switch_1278:
    ;
    switch (tag_1276) {
        
      case 0:
        {
            IntTy x_527_683_1002 =
                  ((Int64Int64CursorCursorProd *) tail_1277)->field0;
            IntTy x_528_684_1003 =
                  ((Int64Int64CursorCursorProd *) tail_1277)->field1;
            CursorTy x_529_685_1004 =
                     ((Int64Int64CursorCursorProd *) tail_1277)->field2;
            CursorTy x_530_686_1005 =
                     ((Int64Int64CursorCursorProd *) tail_1277)->field3;
            unsigned char wildcard_535_687_1006 = print_symbol(1231);
            unsigned char wildcard_540_688_1007 = print_symbol(1233);
            unsigned char y_531_689_1008 = printf("%lld", x_527_683_1002);
            unsigned char wildcard_539_690_1009 = print_symbol(1233);
            unsigned char y_532_691_1010 = printf("%lld", x_528_684_1003);
            unsigned char wildcard_538_692_1011 = print_symbol(1233);
            unsigned char y_533_693_1012 =  _print_IntSet(x_529_685_1004);
            unsigned char wildcard_537_694_1013 = print_symbol(1233);
            unsigned char y_534_695_1014 =  _print_IntSet(x_530_686_1005);
            unsigned char wildcard_536_696_1015 = print_symbol(1230);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard_541_697_1016 = print_symbol(1232);
            unsigned char wildcard_542_698_1017 = print_symbol(1230);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1276");
            exit(1);
        }
    }
}
CursorTy caseFn_543(IntTy x_232_544_699_1018, CursorTy rr_238_545_700_1019,
                    IntTy rx_236_546_701_1020, IntTy rs_235_547_702_1021,
                    CursorTy rl_237_548_703_1022)
{
    TagTyPacked tag_1279 = *(TagTyPacked *) rl_237_548_703_1022;
    CursorTy tail_1280 = rl_237_548_703_1022 + sizeof(IntTy);
    
    
  switch_1284:
    ;
    switch (tag_1279) {
        
      case 0:
        {
            IntTy rls_243_704_1023 =
                  ((Int64Int64CursorCursorProd *) tail_1280)->field0;
            IntTy rlx_244_705_1024 =
                  ((Int64Int64CursorCursorProd *) tail_1280)->field1;
            CursorTy rll_245_706_1025 =
                     ((Int64Int64CursorCursorProd *) tail_1280)->field2;
            CursorTy rlr_246_707_1026 =
                     ((Int64Int64CursorCursorProd *) tail_1280)->field3;
            IntTy fltPrm_792_1027 =  size(rl_237_548_703_1022);
            IntTy fltPrm_794_1028 =  size(rr_238_545_700_1019);
            IntTy fltPrm_793_1029 = 2 * fltPrm_794_1028;
            BoolTy fltIf_791_1030 = fltPrm_792_1027 < fltPrm_793_1029;
            
            if (fltIf_791_1030) {
                IntTy fltPkd_795_1031 = 1 + rs_235_547_702_1021;
                IntTy fltPrm_798_1032 =  size(rl_237_548_703_1022);
                IntTy fltPkd_797_1033 = 1 + fltPrm_798_1032;
                PtrTy fltPkd_799_1034 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_799_1034)->field0 = 1;
                
                PtrTy fltPkd_796_1035 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_796_1035)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_796_1035)->field1 =
                    fltPkd_797_1033;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_796_1035)->field2 =
                    x_232_544_699_1018;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_796_1035)->field3 =
                    fltPkd_799_1034;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_796_1035)->field4 =
                    rl_237_548_703_1022;
                
                PtrTy tailift_1281 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1281)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1281)->field1 =
                    fltPkd_795_1031;
                ((Int64Int64Int64CursorCursorProd *) tailift_1281)->field2 =
                    rx_236_546_701_1020;
                ((Int64Int64Int64CursorCursorProd *) tailift_1281)->field3 =
                    fltPkd_796_1035;
                ((Int64Int64Int64CursorCursorProd *) tailift_1281)->field4 =
                    rr_238_545_700_1019;
                return tailift_1281;
            } else {
                IntTy fltPkd_800_1036 = 1 + rs_235_547_702_1021;
                IntTy fltPkd_801_1037 =  val(rl_237_548_703_1022);
                CursorTy fltAppE_805_1038 =  left(rl_237_548_703_1022);
                IntTy fltPrm_804_1039 =  size(fltAppE_805_1038);
                IntTy fltPkd_803_1040 = 1 + fltPrm_804_1039;
                PtrTy fltPkd_806_1041 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_806_1041)->field0 = 1;
                
                CursorTy fltPkd_807_1042 =  left(rl_237_548_703_1022);
                PtrTy fltPkd_802_1043 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_802_1043)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_802_1043)->field1 =
                    fltPkd_803_1040;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_802_1043)->field2 =
                    x_232_544_699_1018;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_802_1043)->field3 =
                    fltPkd_806_1041;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_802_1043)->field4 =
                    fltPkd_807_1042;
                
                IntTy fltPrm_811_1044 =  size(rr_238_545_700_1019);
                IntTy fltPrm_810_1045 = 1 + fltPrm_811_1044;
                CursorTy fltAppE_813_1046 =  right(rl_237_548_703_1022);
                IntTy fltPrm_812_1047 =  size(fltAppE_813_1046);
                IntTy fltPkd_809_1048 = fltPrm_810_1045 + fltPrm_812_1047;
                CursorTy fltPkd_814_1049 =  right(rl_237_548_703_1022);
                PtrTy fltPkd_808_1050 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_808_1050)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_808_1050)->field1 =
                    fltPkd_809_1048;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_808_1050)->field2 =
                    rx_236_546_701_1020;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_808_1050)->field3 =
                    fltPkd_814_1049;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_808_1050)->field4 =
                    rr_238_545_700_1019;
                
                PtrTy tailift_1282 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1282)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1282)->field1 =
                    fltPkd_800_1036;
                ((Int64Int64Int64CursorCursorProd *) tailift_1282)->field2 =
                    fltPkd_801_1037;
                ((Int64Int64Int64CursorCursorProd *) tailift_1282)->field3 =
                    fltPkd_802_1043;
                ((Int64Int64Int64CursorCursorProd *) tailift_1282)->field4 =
                    fltPkd_808_1050;
                return tailift_1282;
            }
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_816_1051 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_816_1051)->field0 = 1;
            
            PtrTy fltPkd_817_1052 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_817_1052)->field0 = 1;
            
            PtrTy fltPkd_815_1053 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_815_1053)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_815_1053)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_815_1053)->field2 =
                x_232_544_699_1018;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_815_1053)->field3 =
                fltPkd_816_1051;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_815_1053)->field4 =
                fltPkd_817_1052;
            
            PtrTy tailift_1283 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1283)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1283)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1283)->field2 =
                rx_236_546_701_1020;
            ((Int64Int64Int64CursorCursorProd *) tailift_1283)->field3 =
                fltPkd_815_1053;
            ((Int64Int64Int64CursorCursorProd *) tailift_1283)->field4 =
                rr_238_545_700_1019;
            return tailift_1283;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1279");
            exit(1);
        }
    }
}
CursorTy caseFn_549(IntTy x_232_550_708_1054, CursorTy r_234_551_709_1055,
                    IntTy rx_236_552_710_1056, CursorTy rl_237_553_711_1057)
{
    TagTyPacked tag_1285 = *(TagTyPacked *) rl_237_553_711_1057;
    CursorTy tail_1286 = rl_237_553_711_1057 + sizeof(IntTy);
    
    
  switch_1289:
    ;
    switch (tag_1285) {
        
      case 0:
        {
            IntTy rls_247_712_1058 =
                  ((Int64Int64CursorCursorProd *) tail_1286)->field0;
            IntTy rlx_248_713_1059 =
                  ((Int64Int64CursorCursorProd *) tail_1286)->field1;
            CursorTy rll_249_714_1060 =
                     ((Int64Int64CursorCursorProd *) tail_1286)->field2;
            CursorTy rlr_250_715_1061 =
                     ((Int64Int64CursorCursorProd *) tail_1286)->field3;
            IntTy fltPkd_818_1062 =  val(rl_237_553_711_1057);
            PtrTy fltPkd_820_1063 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_820_1063)->field0 = 1;
            
            PtrTy fltPkd_821_1064 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_821_1064)->field0 = 1;
            
            PtrTy fltPkd_819_1065 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_819_1065)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_819_1065)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_819_1065)->field2 =
                x_232_550_708_1054;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_819_1065)->field3 =
                fltPkd_820_1063;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_819_1065)->field4 =
                fltPkd_821_1064;
            
            PtrTy fltPkd_823_1066 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_823_1066)->field0 = 1;
            
            PtrTy fltPkd_824_1067 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_824_1067)->field0 = 1;
            
            PtrTy fltPkd_822_1068 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_822_1068)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_822_1068)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_822_1068)->field2 =
                rx_236_552_710_1056;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_822_1068)->field3 =
                fltPkd_823_1066;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_822_1068)->field4 =
                fltPkd_824_1067;
            
            PtrTy tailift_1287 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1287)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1287)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1287)->field2 =
                fltPkd_818_1062;
            ((Int64Int64Int64CursorCursorProd *) tailift_1287)->field3 =
                fltPkd_819_1065;
            ((Int64Int64Int64CursorCursorProd *) tailift_1287)->field4 =
                fltPkd_822_1068;
            return tailift_1287;
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_825_1069 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_825_1069)->field0 = 1;
            
            PtrTy tailift_1288 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1288)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1288)->field1 = 2;
            ((Int64Int64Int64CursorCursorProd *) tailift_1288)->field2 =
                x_232_550_708_1054;
            ((Int64Int64Int64CursorCursorProd *) tailift_1288)->field3 =
                fltPkd_825_1069;
            ((Int64Int64Int64CursorCursorProd *) tailift_1288)->field4 =
                r_234_551_709_1055;
            return tailift_1288;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1285");
            exit(1);
        }
    }
}
CursorTy caseFn_554(IntTy x_232_555_716_1070, CursorTy r_234_556_717_1071,
                    CursorTy rr_238_557_718_1072, IntTy rx_236_558_719_1073,
                    IntTy rs_235_559_720_1074, CursorTy rl_237_560_721_1075)
{
    TagTyPacked tag_1290 = *(TagTyPacked *) rr_238_557_718_1072;
    CursorTy tail_1291 = rr_238_557_718_1072 + sizeof(IntTy);
    
    
  switch_1292:
    ;
    switch (tag_1290) {
        
      case 0:
        {
            IntTy rrs_239_722_1076 =
                  ((Int64Int64CursorCursorProd *) tail_1291)->field0;
            IntTy rrx_240_723_1077 =
                  ((Int64Int64CursorCursorProd *) tail_1291)->field1;
            CursorTy rrl_241_724_1078 =
                     ((Int64Int64CursorCursorProd *) tail_1291)->field2;
            CursorTy rrr_242_725_1079 =
                     ((Int64Int64CursorCursorProd *) tail_1291)->field3;
            
            return caseFn_543(x_232_555_716_1070, rr_238_557_718_1072,
                              rx_236_558_719_1073, rs_235_559_720_1074,
                              rl_237_560_721_1075);
            break;
        }
        
      case 1:
        {
            return caseFn_549(x_232_555_716_1070, r_234_556_717_1071,
                              rx_236_558_719_1073, rl_237_560_721_1075);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1290");
            exit(1);
        }
    }
}
CursorTy caseFn_561(IntTy x_232_562_726_1080, CursorTy r_234_563_727_1081)
{
    TagTyPacked tag_1293 = *(TagTyPacked *) r_234_563_727_1081;
    CursorTy tail_1294 = r_234_563_727_1081 + sizeof(IntTy);
    
    
  switch_1296:
    ;
    switch (tag_1293) {
        
      case 1:
        {
            PtrTy fltPkd_826_1082 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_826_1082)->field0 = 1;
            
            PtrTy fltPkd_827_1083 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_827_1083)->field0 = 1;
            
            PtrTy tailift_1295 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field2 =
                x_232_562_726_1080;
            ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field3 =
                fltPkd_826_1082;
            ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field4 =
                fltPkd_827_1083;
            return tailift_1295;
            break;
        }
        
      case 0:
        {
            IntTy rs_235_728_1084 =
                  ((Int64Int64CursorCursorProd *) tail_1294)->field0;
            IntTy rx_236_729_1085 =
                  ((Int64Int64CursorCursorProd *) tail_1294)->field1;
            CursorTy rl_237_730_1086 =
                     ((Int64Int64CursorCursorProd *) tail_1294)->field2;
            CursorTy rr_238_731_1087 =
                     ((Int64Int64CursorCursorProd *) tail_1294)->field3;
            
            return caseFn_554(x_232_562_726_1080, r_234_563_727_1081,
                              rr_238_731_1087, rx_236_729_1085, rs_235_728_1084,
                              rl_237_730_1086);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1293");
            exit(1);
        }
    }
}
CursorTy caseFn_564(CursorTy l_233_565_732_1088, IntTy x_232_566_733_1089,
                    CursorTy r_234_567_734_1090, IntTy ls_251_568_735_1091)
{
    TagTyPacked tag_1297 = *(TagTyPacked *) r_234_567_734_1090;
    CursorTy tail_1298 = r_234_567_734_1090 + sizeof(IntTy);
    
    
  switch_1303:
    ;
    switch (tag_1297) {
        
      case 1:
        {
            IntTy fltPkd_828_1092 = 1 + ls_251_568_735_1091;
            PtrTy fltPkd_829_1093 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_829_1093)->field0 = 1;
            
            PtrTy tailift_1299 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1299)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1299)->field1 =
                fltPkd_828_1092;
            ((Int64Int64Int64CursorCursorProd *) tailift_1299)->field2 =
                x_232_566_733_1089;
            ((Int64Int64Int64CursorCursorProd *) tailift_1299)->field3 =
                l_233_565_732_1088;
            ((Int64Int64Int64CursorCursorProd *) tailift_1299)->field4 =
                fltPkd_829_1093;
            return tailift_1299;
            break;
        }
        
      case 0:
        {
            IntTy rs_255_736_1094 =
                  ((Int64Int64CursorCursorProd *) tail_1298)->field0;
            IntTy rx_256_737_1095 =
                  ((Int64Int64CursorCursorProd *) tail_1298)->field1;
            CursorTy rl_257_738_1096 =
                     ((Int64Int64CursorCursorProd *) tail_1298)->field2;
            CursorTy rr_258_739_1097 =
                     ((Int64Int64CursorCursorProd *) tail_1298)->field3;
            IntTy fltPrm_831_1098 = 3 * ls_251_568_735_1091;
            BoolTy fltIf_830_1099 = rs_255_736_1094 > fltPrm_831_1098;
            
            if (fltIf_830_1099) {
                IntTy fltPrm_833_1100 =  size(rl_257_738_1096);
                IntTy fltPrm_835_1101 =  size(rr_258_739_1097);
                IntTy fltPrm_834_1102 = 2 * fltPrm_835_1101;
                BoolTy fltIf_832_1103 = fltPrm_833_1100 < fltPrm_834_1102;
                
                if (fltIf_832_1103) {
                    IntTy fltPrm_837_1104 = 1 + ls_251_568_735_1091;
                    IntTy fltPkd_836_1105 = fltPrm_837_1104 + rs_255_736_1094;
                    IntTy fltPrm_840_1106 = 1 + ls_251_568_735_1091;
                    IntTy fltPrm_841_1107 =  size(rl_257_738_1096);
                    IntTy fltPkd_839_1108 = fltPrm_840_1106 + fltPrm_841_1107;
                    PtrTy fltPkd_838_1109 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_838_1109)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_838_1109)->field1 =
                        fltPkd_839_1108;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_838_1109)->field2 =
                        x_232_566_733_1089;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_838_1109)->field3 =
                        l_233_565_732_1088;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_838_1109)->field4 =
                        rl_257_738_1096;
                    
                    PtrTy tailift_1300 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1300)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1300)->field1 =
                        fltPkd_836_1105;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1300)->field2 =
                        rx_256_737_1095;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1300)->field3 =
                        fltPkd_838_1109;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1300)->field4 =
                        rr_258_739_1097;
                    return tailift_1300;
                } else {
                    IntTy fltPrm_843_1110 = 1 + ls_251_568_735_1091;
                    IntTy fltPkd_842_1111 = fltPrm_843_1110 + rs_255_736_1094;
                    IntTy fltPkd_844_1112 =  val(rl_257_738_1096);
                    IntTy fltPrm_847_1113 = 1 + ls_251_568_735_1091;
                    CursorTy fltAppE_849_1114 =  left(rl_257_738_1096);
                    IntTy fltPrm_848_1115 =  size(fltAppE_849_1114);
                    IntTy fltPkd_846_1116 = fltPrm_847_1113 + fltPrm_848_1115;
                    CursorTy fltPkd_850_1117 =  left(rl_257_738_1096);
                    PtrTy fltPkd_845_1118 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_845_1118)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_845_1118)->field1 =
                        fltPkd_846_1116;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_845_1118)->field2 =
                        x_232_566_733_1089;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_845_1118)->field3 =
                        l_233_565_732_1088;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_845_1118)->field4 =
                        fltPkd_850_1117;
                    
                    IntTy fltPrm_854_1119 =  size(rr_258_739_1097);
                    IntTy fltPrm_853_1120 = 1 + fltPrm_854_1119;
                    CursorTy fltAppE_856_1121 =  right(rl_257_738_1096);
                    IntTy fltPrm_855_1122 =  size(fltAppE_856_1121);
                    IntTy fltPkd_852_1123 = fltPrm_853_1120 + fltPrm_855_1122;
                    CursorTy fltPkd_857_1124 =  right(rl_257_738_1096);
                    PtrTy fltPkd_851_1125 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_851_1125)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_851_1125)->field1 =
                        fltPkd_852_1123;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_851_1125)->field2 =
                        rx_256_737_1095;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_851_1125)->field3 =
                        fltPkd_857_1124;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_851_1125)->field4 =
                        rr_258_739_1097;
                    
                    PtrTy tailift_1301 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field1 =
                        fltPkd_842_1111;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field2 =
                        fltPkd_844_1112;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field3 =
                        fltPkd_845_1118;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field4 =
                        fltPkd_851_1125;
                    return tailift_1301;
                }
            } else {
                IntTy fltPrm_859_1126 = 1 + ls_251_568_735_1091;
                IntTy fltPkd_858_1127 = fltPrm_859_1126 + rs_255_736_1094;
                PtrTy tailift_1302 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field1 =
                    fltPkd_858_1127;
                ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field2 =
                    x_232_566_733_1089;
                ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field3 =
                    l_233_565_732_1088;
                ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field4 =
                    r_234_567_734_1090;
                return tailift_1302;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1297");
            exit(1);
        }
    }
}
CursorTy caseFn_569(IntTy x_259_570_740_1128, CursorTy ll_264_571_741_1129,
                    IntTy lx_263_572_742_1130, IntTy ls_262_573_743_1131,
                    CursorTy lr_265_574_744_1132, IntTy lls_266_575_745_1133)
{
    TagTyPacked tag_1304 = *(TagTyPacked *) lr_265_574_744_1132;
    CursorTy tail_1305 = lr_265_574_744_1132 + sizeof(IntTy);
    
    
  switch_1309:
    ;
    switch (tag_1304) {
        
      case 0:
        {
            IntTy lrs_270_746_1134 =
                  ((Int64Int64CursorCursorProd *) tail_1305)->field0;
            IntTy lrx_271_747_1135 =
                  ((Int64Int64CursorCursorProd *) tail_1305)->field1;
            CursorTy lrl_272_748_1136 =
                     ((Int64Int64CursorCursorProd *) tail_1305)->field2;
            CursorTy lrr_273_749_1137 =
                     ((Int64Int64CursorCursorProd *) tail_1305)->field3;
            IntTy fltPrm_861_1138 = 2 * lls_266_575_745_1133;
            BoolTy fltIf_860_1139 = lrs_270_746_1134 < fltPrm_861_1138;
            
            if (fltIf_860_1139) {
                IntTy fltPkd_862_1140 = 1 + ls_262_573_743_1131;
                IntTy fltPkd_864_1141 = 1 + lrs_270_746_1134;
                PtrTy fltPkd_865_1142 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_865_1142)->field0 = 1;
                
                PtrTy fltPkd_863_1143 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_863_1143)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_863_1143)->field1 =
                    fltPkd_864_1141;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_863_1143)->field2 =
                    x_259_570_740_1128;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_863_1143)->field3 =
                    lr_265_574_744_1132;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_863_1143)->field4 =
                    fltPkd_865_1142;
                
                PtrTy tailift_1306 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1306)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1306)->field1 =
                    fltPkd_862_1140;
                ((Int64Int64Int64CursorCursorProd *) tailift_1306)->field2 =
                    lx_263_572_742_1130;
                ((Int64Int64Int64CursorCursorProd *) tailift_1306)->field3 =
                    ll_264_571_741_1129;
                ((Int64Int64Int64CursorCursorProd *) tailift_1306)->field4 =
                    fltPkd_863_1143;
                return tailift_1306;
            } else {
                IntTy fltPkd_866_1144 = 1 + ls_262_573_743_1131;
                IntTy fltPrm_869_1145 = 1 + lls_266_575_745_1133;
                IntTy fltPrm_870_1146 =  size(lrl_272_748_1136);
                IntTy fltPkd_868_1147 = fltPrm_869_1145 + fltPrm_870_1146;
                PtrTy fltPkd_867_1148 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_867_1148)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_867_1148)->field1 =
                    fltPkd_868_1147;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_867_1148)->field2 =
                    lx_263_572_742_1130;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_867_1148)->field3 =
                    ll_264_571_741_1129;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_867_1148)->field4 =
                    lrl_272_748_1136;
                
                IntTy fltPrm_873_1149 =  size(lrr_273_749_1137);
                IntTy fltPkd_872_1150 = 1 + fltPrm_873_1149;
                PtrTy fltPkd_874_1151 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_874_1151)->field0 = 1;
                
                PtrTy fltPkd_871_1152 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1152)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1152)->field1 =
                    fltPkd_872_1150;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1152)->field2 =
                    x_259_570_740_1128;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1152)->field3 =
                    lrr_273_749_1137;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1152)->field4 =
                    fltPkd_874_1151;
                
                PtrTy tailift_1307 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1307)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1307)->field1 =
                    fltPkd_866_1144;
                ((Int64Int64Int64CursorCursorProd *) tailift_1307)->field2 =
                    lrx_271_747_1135;
                ((Int64Int64Int64CursorCursorProd *) tailift_1307)->field3 =
                    fltPkd_867_1148;
                ((Int64Int64Int64CursorCursorProd *) tailift_1307)->field4 =
                    fltPkd_871_1152;
                return tailift_1307;
            }
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_876_1153 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_876_1153)->field0 = 1;
            
            PtrTy fltPkd_877_1154 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_877_1154)->field0 = 1;
            
            PtrTy fltPkd_875_1155 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1155)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1155)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1155)->field2 =
                x_259_570_740_1128;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1155)->field3 =
                fltPkd_876_1153;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1155)->field4 =
                fltPkd_877_1154;
            
            PtrTy tailift_1308 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1308)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1308)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1308)->field2 =
                lx_263_572_742_1130;
            ((Int64Int64Int64CursorCursorProd *) tailift_1308)->field3 =
                ll_264_571_741_1129;
            ((Int64Int64Int64CursorCursorProd *) tailift_1308)->field4 =
                fltPkd_875_1155;
            return tailift_1308;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1304");
            exit(1);
        }
    }
}
CursorTy caseFn_576(CursorTy l_260_577_750_1156, IntTy x_259_578_751_1157,
                    IntTy lx_263_579_752_1158, CursorTy lr_265_580_753_1159)
{
    TagTyPacked tag_1310 = *(TagTyPacked *) lr_265_580_753_1159;
    CursorTy tail_1311 = lr_265_580_753_1159 + sizeof(IntTy);
    
    
  switch_1314:
    ;
    switch (tag_1310) {
        
      case 0:
        {
            IntTy lrs_274_754_1160 =
                  ((Int64Int64CursorCursorProd *) tail_1311)->field0;
            IntTy lrx_275_755_1161 =
                  ((Int64Int64CursorCursorProd *) tail_1311)->field1;
            CursorTy lrl_276_756_1162 =
                     ((Int64Int64CursorCursorProd *) tail_1311)->field2;
            CursorTy lrr_277_757_1163 =
                     ((Int64Int64CursorCursorProd *) tail_1311)->field3;
            PtrTy fltPkd_879_1164 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_879_1164)->field0 = 1;
            
            PtrTy fltPkd_880_1165 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_880_1165)->field0 = 1;
            
            PtrTy fltPkd_878_1166 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_878_1166)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_878_1166)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_878_1166)->field2 =
                lx_263_579_752_1158;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_878_1166)->field3 =
                fltPkd_879_1164;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_878_1166)->field4 =
                fltPkd_880_1165;
            
            PtrTy fltPkd_882_1167 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_882_1167)->field0 = 1;
            
            PtrTy fltPkd_883_1168 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_883_1168)->field0 = 1;
            
            PtrTy fltPkd_881_1169 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_881_1169)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_881_1169)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_881_1169)->field2 =
                x_259_578_751_1157;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_881_1169)->field3 =
                fltPkd_882_1167;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_881_1169)->field4 =
                fltPkd_883_1168;
            
            PtrTy tailift_1312 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1312)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1312)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1312)->field2 =
                lrx_275_755_1161;
            ((Int64Int64Int64CursorCursorProd *) tailift_1312)->field3 =
                fltPkd_878_1166;
            ((Int64Int64Int64CursorCursorProd *) tailift_1312)->field4 =
                fltPkd_881_1169;
            return tailift_1312;
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_884_1170 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_884_1170)->field0 = 1;
            
            PtrTy tailift_1313 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field1 = 2;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field2 =
                x_259_578_751_1157;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field3 =
                l_260_577_750_1156;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field4 =
                fltPkd_884_1170;
            return tailift_1313;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1310");
            exit(1);
        }
    }
}
CursorTy caseFn_581(CursorTy l_260_582_758_1171, IntTy x_259_583_759_1172,
                    CursorTy ll_264_584_760_1173, IntTy lx_263_585_761_1174,
                    IntTy ls_262_586_762_1175, CursorTy lr_265_587_763_1176)
{
    TagTyPacked tag_1315 = *(TagTyPacked *) ll_264_584_760_1173;
    CursorTy tail_1316 = ll_264_584_760_1173 + sizeof(IntTy);
    
    
  switch_1317:
    ;
    switch (tag_1315) {
        
      case 0:
        {
            IntTy lls_266_764_1177 =
                  ((Int64Int64CursorCursorProd *) tail_1316)->field0;
            IntTy llx_267_765_1178 =
                  ((Int64Int64CursorCursorProd *) tail_1316)->field1;
            CursorTy lll_268_766_1179 =
                     ((Int64Int64CursorCursorProd *) tail_1316)->field2;
            CursorTy llr_269_767_1180 =
                     ((Int64Int64CursorCursorProd *) tail_1316)->field3;
            
            return caseFn_569(x_259_583_759_1172, ll_264_584_760_1173,
                              lx_263_585_761_1174, ls_262_586_762_1175,
                              lr_265_587_763_1176, lls_266_764_1177);
            break;
        }
        
      case 1:
        {
            return caseFn_576(l_260_582_758_1171, x_259_583_759_1172,
                              lx_263_585_761_1174, lr_265_587_763_1176);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1315");
            exit(1);
        }
    }
}
CursorTy caseFn_588(CursorTy l_260_589_768_1181, IntTy x_259_590_769_1182)
{
    TagTyPacked tag_1318 = *(TagTyPacked *) l_260_589_768_1181;
    CursorTy tail_1319 = l_260_589_768_1181 + sizeof(IntTy);
    
    
  switch_1321:
    ;
    switch (tag_1318) {
        
      case 1:
        {
            PtrTy fltPkd_885_1183 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_885_1183)->field0 = 1;
            
            PtrTy fltPkd_886_1184 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_886_1184)->field0 = 1;
            
            PtrTy tailift_1320 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field2 =
                x_259_590_769_1182;
            ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field3 =
                fltPkd_885_1183;
            ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field4 =
                fltPkd_886_1184;
            return tailift_1320;
            break;
        }
        
      case 0:
        {
            IntTy ls_262_770_1185 =
                  ((Int64Int64CursorCursorProd *) tail_1319)->field0;
            IntTy lx_263_771_1186 =
                  ((Int64Int64CursorCursorProd *) tail_1319)->field1;
            CursorTy ll_264_772_1187 =
                     ((Int64Int64CursorCursorProd *) tail_1319)->field2;
            CursorTy lr_265_773_1188 =
                     ((Int64Int64CursorCursorProd *) tail_1319)->field3;
            
            return caseFn_581(l_260_589_768_1181, x_259_590_769_1182,
                              ll_264_772_1187, lx_263_771_1186, ls_262_770_1185,
                              lr_265_773_1188);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1318");
            exit(1);
        }
    }
}
CursorTy caseFn_591(CursorTy r_261_592_774_1189, CursorTy l_260_593_775_1190,
                    IntTy x_259_594_776_1191, IntTy rs_278_595_777_1192)
{
    TagTyPacked tag_1322 = *(TagTyPacked *) l_260_593_775_1190;
    CursorTy tail_1323 = l_260_593_775_1190 + sizeof(IntTy);
    
    
  switch_1328:
    ;
    switch (tag_1322) {
        
      case 1:
        {
            IntTy fltPkd_887_1193 = 1 + rs_278_595_777_1192;
            PtrTy fltPkd_888_1194 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_888_1194)->field0 = 1;
            
            PtrTy tailift_1324 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1324)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1324)->field1 =
                fltPkd_887_1193;
            ((Int64Int64Int64CursorCursorProd *) tailift_1324)->field2 =
                x_259_594_776_1191;
            ((Int64Int64Int64CursorCursorProd *) tailift_1324)->field3 =
                fltPkd_888_1194;
            ((Int64Int64Int64CursorCursorProd *) tailift_1324)->field4 =
                r_261_592_774_1189;
            return tailift_1324;
            break;
        }
        
      case 0:
        {
            IntTy ls_282_778_1195 =
                  ((Int64Int64CursorCursorProd *) tail_1323)->field0;
            IntTy lx_283_779_1196 =
                  ((Int64Int64CursorCursorProd *) tail_1323)->field1;
            CursorTy ll_284_780_1197 =
                     ((Int64Int64CursorCursorProd *) tail_1323)->field2;
            CursorTy lr_285_781_1198 =
                     ((Int64Int64CursorCursorProd *) tail_1323)->field3;
            IntTy fltPrm_890_1199 = 3 * rs_278_595_777_1192;
            BoolTy fltIf_889_1200 = ls_282_778_1195 > fltPrm_890_1199;
            
            if (fltIf_889_1200) {
                IntTy fltPrm_892_1201 =  size(lr_285_781_1198);
                IntTy fltPrm_894_1202 =  size(ll_284_780_1197);
                IntTy fltPrm_893_1203 = 2 * fltPrm_894_1202;
                BoolTy fltIf_891_1204 = fltPrm_892_1201 < fltPrm_893_1203;
                
                if (fltIf_891_1204) {
                    IntTy fltPrm_896_1205 = 1 + ls_282_778_1195;
                    IntTy fltPkd_895_1206 = fltPrm_896_1205 +
                          rs_278_595_777_1192;
                    IntTy fltPrm_899_1207 = 1 + rs_278_595_777_1192;
                    IntTy fltPrm_900_1208 =  size(lr_285_781_1198);
                    IntTy fltPkd_898_1209 = fltPrm_899_1207 + fltPrm_900_1208;
                    PtrTy fltPkd_897_1210 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_897_1210)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_897_1210)->field1 =
                        fltPkd_898_1209;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_897_1210)->field2 =
                        x_259_594_776_1191;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_897_1210)->field3 =
                        lr_285_781_1198;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_897_1210)->field4 =
                        r_261_592_774_1189;
                    
                    PtrTy tailift_1325 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1325)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1325)->field1 =
                        fltPkd_895_1206;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1325)->field2 =
                        lx_283_779_1196;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1325)->field3 =
                        ll_284_780_1197;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1325)->field4 =
                        fltPkd_897_1210;
                    return tailift_1325;
                } else {
                    IntTy fltPrm_902_1211 = 1 + ls_282_778_1195;
                    IntTy fltPkd_901_1212 = fltPrm_902_1211 +
                          rs_278_595_777_1192;
                    IntTy fltPkd_903_1213 =  val(lr_285_781_1198);
                    IntTy fltPrm_907_1214 =  size(ll_284_780_1197);
                    IntTy fltPrm_906_1215 = 1 + fltPrm_907_1214;
                    CursorTy fltAppE_909_1216 =  left(lr_285_781_1198);
                    IntTy fltPrm_908_1217 =  size(fltAppE_909_1216);
                    IntTy fltPkd_905_1218 = fltPrm_906_1215 + fltPrm_908_1217;
                    CursorTy fltPkd_910_1219 =  left(lr_285_781_1198);
                    PtrTy fltPkd_904_1220 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_904_1220)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_904_1220)->field1 =
                        fltPkd_905_1218;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_904_1220)->field2 =
                        lx_283_779_1196;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_904_1220)->field3 =
                        ll_284_780_1197;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_904_1220)->field4 =
                        fltPkd_910_1219;
                    
                    IntTy fltPrm_913_1221 = 1 + rs_278_595_777_1192;
                    CursorTy fltAppE_915_1222 =  right(lr_285_781_1198);
                    IntTy fltPrm_914_1223 =  size(fltAppE_915_1222);
                    IntTy fltPkd_912_1224 = fltPrm_913_1221 + fltPrm_914_1223;
                    CursorTy fltPkd_916_1225 =  right(lr_285_781_1198);
                    PtrTy fltPkd_911_1226 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_911_1226)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_911_1226)->field1 =
                        fltPkd_912_1224;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_911_1226)->field2 =
                        x_259_594_776_1191;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_911_1226)->field3 =
                        fltPkd_916_1225;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_911_1226)->field4 =
                        r_261_592_774_1189;
                    
                    PtrTy tailift_1326 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field1 =
                        fltPkd_901_1212;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field2 =
                        fltPkd_903_1213;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field3 =
                        fltPkd_904_1220;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field4 =
                        fltPkd_911_1226;
                    return tailift_1326;
                }
            } else {
                IntTy fltPrm_918_1227 = 1 + ls_282_778_1195;
                IntTy fltPkd_917_1228 = fltPrm_918_1227 + rs_278_595_777_1192;
                PtrTy tailift_1327 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field1 =
                    fltPkd_917_1228;
                ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field2 =
                    x_259_594_776_1191;
                ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field3 =
                    l_260_593_775_1190;
                ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field4 =
                    r_261_592_774_1189;
                return tailift_1327;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1322");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1230, ")");
    add_symbol(1231, "(PureSet");
    add_symbol(1232, "(EmptySet");
    add_symbol(1233, " ");
    
    CursorTy fltAppE_783_919 =  empty();
    CursorTy fltAppE_782_920 =  insert(0, fltAppE_783_919);
    IntTy tmp_app_1229 =  sum(fltAppE_782_920);
    
    printf("%lld", tmp_app_1229);
    printf("\n");
    free_symtable();
    return 0;
}