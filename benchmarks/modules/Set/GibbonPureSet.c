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
IntTy sum(CursorTy s_194_600_930);
CursorTy insert_num(IntTy x_199_605_938, CursorTy s_200_606_939);
CursorTy right(CursorTy x_201_607_943);
CursorTy left(CursorTy x_206_612_948);
IntTy val(CursorTy x_211_617_953);
IntTy size(CursorTy s_216_622_958);
CursorTy balanceR(IntTy x_232_638_963, CursorTy l_233_639_964,
                  CursorTy r_234_640_965);
CursorTy balanceL(IntTy x_259_645_970, CursorTy l_260_646_971,
                  CursorTy r_261_647_972);
CursorTy insert(IntTy x_286_652_977, CursorTy s_287_653_978);
CursorTy singleton(IntTy x_294_660_987);
CursorTy _copy_without_ptrs_IntSet(CursorTy arg_512_661_990);
CursorTy _copy_IntSet(CursorTy arg_503_670_999);
unsigned char _traverse_IntSet(CursorTy arg_521_679_1008);
unsigned char _print_IntSet(CursorTy arg_530_686_1015);
CursorTy caseFn_547(IntTy x_232_548_703_1032, CursorTy rr_238_549_704_1033,
                    IntTy rx_236_550_705_1034, IntTy rs_235_551_706_1035,
                    CursorTy rl_237_552_707_1036);
CursorTy caseFn_553(IntTy x_232_554_712_1068, CursorTy r_234_555_713_1069,
                    IntTy rx_236_556_714_1070, CursorTy rl_237_557_715_1071);
CursorTy caseFn_558(IntTy x_232_559_720_1084, CursorTy r_234_560_721_1085,
                    CursorTy rr_238_561_722_1086, IntTy rx_236_562_723_1087,
                    IntTy rs_235_563_724_1088, CursorTy rl_237_564_725_1089);
CursorTy caseFn_565(IntTy x_232_566_730_1094, CursorTy r_234_567_731_1095);
CursorTy caseFn_568(CursorTy l_233_569_736_1102, IntTy x_232_570_737_1103,
                    CursorTy r_234_571_738_1104, IntTy ls_251_572_739_1105);
CursorTy caseFn_573(IntTy x_259_574_744_1142, CursorTy ll_264_575_745_1143,
                    IntTy lx_263_576_746_1144, IntTy ls_262_577_747_1145,
                    CursorTy lr_265_578_748_1146, IntTy lls_266_579_749_1147);
CursorTy caseFn_580(CursorTy l_260_581_754_1170, IntTy x_259_582_755_1171,
                    IntTy lx_263_583_756_1172, CursorTy lr_265_584_757_1173);
CursorTy caseFn_585(CursorTy l_260_586_762_1185, IntTy x_259_587_763_1186,
                    CursorTy ll_264_588_764_1187, IntTy lx_263_589_765_1188,
                    IntTy ls_262_590_766_1189, CursorTy lr_265_591_767_1190);
CursorTy caseFn_592(CursorTy l_260_593_772_1195, IntTy x_259_594_773_1196);
CursorTy caseFn_595(CursorTy r_261_596_778_1203, CursorTy l_260_597_779_1204,
                    IntTy x_259_598_780_1205, IntTy rs_278_599_781_1206);
CursorTy empty()
{
    PtrTy tailift_1248 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) tailift_1248)->field0 = 1;
    return tailift_1248;
}
IntTy sum(CursorTy s_194_600_930)
{
    TagTyPacked tag_1249 = *(TagTyPacked *) s_194_600_930;
    CursorTy tail_1250 = s_194_600_930 + sizeof(IntTy);
    
    
  switch_1252:
    ;
    switch (tag_1249) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__188_195_601_931 =
                  ((Int64Int64CursorCursorProd *) tail_1250)->field0;
            IntTy v_196_602_932 =
                  ((Int64Int64CursorCursorProd *) tail_1250)->field1;
            CursorTy l_197_603_933 =
                     ((Int64Int64CursorCursorProd *) tail_1250)->field2;
            CursorTy r_198_604_934 =
                     ((Int64Int64CursorCursorProd *) tail_1250)->field3;
            IntTy fltPrm_790_935 =  sum(l_197_603_933);
            IntTy fltPrm_789_936 = v_196_602_932 + fltPrm_790_935;
            IntTy fltPrm_791_937 =  sum(r_198_604_934);
            IntTy flt_1251 = fltPrm_789_936 + fltPrm_791_937;
            
            return flt_1251;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1249");
            exit(1);
        }
    }
}
CursorTy insert_num(IntTy x_199_605_938, CursorTy s_200_606_939)
{
    BoolTy fltIf_792_940 = x_199_605_938 == 0;
    
    if (fltIf_792_940) {
        return insert(x_199_605_938, s_200_606_939);
    } else {
        IntTy fltAppE_794_941 = x_199_605_938 - 1;
        CursorTy fltAppE_793_942 =  insert_num(fltAppE_794_941, s_200_606_939);
        
        return insert(x_199_605_938, fltAppE_793_942);
    }
}
CursorTy right(CursorTy x_201_607_943)
{
    TagTyPacked tag_1253 = *(TagTyPacked *) x_201_607_943;
    CursorTy tail_1254 = x_201_607_943 + sizeof(IntTy);
    
    
  switch_1256:
    ;
    switch (tag_1253) {
        
      case 1:
        {
            PtrTy tailift_1255 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1255)->field0 = 1;
            return tailift_1255;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__178_202_608_944 =
                  ((Int64Int64CursorCursorProd *) tail_1254)->field0;
            IntTy wildcard__179_203_609_945 =
                  ((Int64Int64CursorCursorProd *) tail_1254)->field1;
            CursorTy wildcard__180_204_610_946 =
                     ((Int64Int64CursorCursorProd *) tail_1254)->field2;
            CursorTy r_205_611_947 =
                     ((Int64Int64CursorCursorProd *) tail_1254)->field3;
            
            return r_205_611_947;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1253");
            exit(1);
        }
    }
}
CursorTy left(CursorTy x_206_612_948)
{
    TagTyPacked tag_1257 = *(TagTyPacked *) x_206_612_948;
    CursorTy tail_1258 = x_206_612_948 + sizeof(IntTy);
    
    
  switch_1260:
    ;
    switch (tag_1257) {
        
      case 1:
        {
            PtrTy tailift_1259 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1259)->field0 = 1;
            return tailift_1259;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__169_207_613_949 =
                  ((Int64Int64CursorCursorProd *) tail_1258)->field0;
            IntTy wildcard__170_208_614_950 =
                  ((Int64Int64CursorCursorProd *) tail_1258)->field1;
            CursorTy l_209_615_951 =
                     ((Int64Int64CursorCursorProd *) tail_1258)->field2;
            CursorTy wildcard__171_210_616_952 =
                     ((Int64Int64CursorCursorProd *) tail_1258)->field3;
            
            return l_209_615_951;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1257");
            exit(1);
        }
    }
}
IntTy val(CursorTy x_211_617_953)
{
    TagTyPacked tag_1261 = *(TagTyPacked *) x_211_617_953;
    CursorTy tail_1262 = x_211_617_953 + sizeof(IntTy);
    
    
  switch_1263:
    ;
    switch (tag_1261) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy wildcard__160_212_618_954 =
                  ((Int64Int64CursorCursorProd *) tail_1262)->field0;
            IntTy v_213_619_955 =
                  ((Int64Int64CursorCursorProd *) tail_1262)->field1;
            CursorTy wildcard__161_214_620_956 =
                     ((Int64Int64CursorCursorProd *) tail_1262)->field2;
            CursorTy wildcard__162_215_621_957 =
                     ((Int64Int64CursorCursorProd *) tail_1262)->field3;
            
            return v_213_619_955;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1261");
            exit(1);
        }
    }
}
IntTy size(CursorTy s_216_622_958)
{
    TagTyPacked tag_1264 = *(TagTyPacked *) s_216_622_958;
    CursorTy tail_1265 = s_216_622_958 + sizeof(IntTy);
    
    
  switch_1266:
    ;
    switch (tag_1264) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            IntTy sz_217_623_959 =
                  ((Int64Int64CursorCursorProd *) tail_1265)->field0;
            IntTy wildcard__152_218_624_960 =
                  ((Int64Int64CursorCursorProd *) tail_1265)->field1;
            CursorTy wildcard__153_219_625_961 =
                     ((Int64Int64CursorCursorProd *) tail_1265)->field2;
            CursorTy wildcard__154_220_626_962 =
                     ((Int64Int64CursorCursorProd *) tail_1265)->field3;
            
            return sz_217_623_959;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1264");
            exit(1);
        }
    }
}
CursorTy balanceR(IntTy x_232_638_963, CursorTy l_233_639_964,
                  CursorTy r_234_640_965)
{
    TagTyPacked tag_1267 = *(TagTyPacked *) l_233_639_964;
    CursorTy tail_1268 = l_233_639_964 + sizeof(IntTy);
    
    
  switch_1269:
    ;
    switch (tag_1267) {
        
      case 1:
        {
            return caseFn_565(x_232_638_963, r_234_640_965);
            break;
        }
        
      case 0:
        {
            IntTy ls_251_641_966 =
                  ((Int64Int64CursorCursorProd *) tail_1268)->field0;
            IntTy wildcard__116_252_642_967 =
                  ((Int64Int64CursorCursorProd *) tail_1268)->field1;
            CursorTy wildcard__117_253_643_968 =
                     ((Int64Int64CursorCursorProd *) tail_1268)->field2;
            CursorTy wildcard__118_254_644_969 =
                     ((Int64Int64CursorCursorProd *) tail_1268)->field3;
            
            return caseFn_568(l_233_639_964, x_232_638_963, r_234_640_965,
                              ls_251_641_966);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1267");
            exit(1);
        }
    }
}
CursorTy balanceL(IntTy x_259_645_970, CursorTy l_260_646_971,
                  CursorTy r_261_647_972)
{
    TagTyPacked tag_1270 = *(TagTyPacked *) r_261_647_972;
    CursorTy tail_1271 = r_261_647_972 + sizeof(IntTy);
    
    
  switch_1272:
    ;
    switch (tag_1270) {
        
      case 1:
        {
            return caseFn_592(l_260_646_971, x_259_645_970);
            break;
        }
        
      case 0:
        {
            IntTy rs_278_648_973 =
                  ((Int64Int64CursorCursorProd *) tail_1271)->field0;
            IntTy wildcard__55_279_649_974 =
                  ((Int64Int64CursorCursorProd *) tail_1271)->field1;
            CursorTy wildcard__56_280_650_975 =
                     ((Int64Int64CursorCursorProd *) tail_1271)->field2;
            CursorTy wildcard__57_281_651_976 =
                     ((Int64Int64CursorCursorProd *) tail_1271)->field3;
            
            return caseFn_595(r_261_647_972, l_260_646_971, x_259_645_970,
                              rs_278_648_973);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1270");
            exit(1);
        }
    }
}
CursorTy insert(IntTy x_286_652_977, CursorTy s_287_653_978)
{
    TagTyPacked tag_1273 = *(TagTyPacked *) s_287_653_978;
    CursorTy tail_1274 = s_287_653_978 + sizeof(IntTy);
    
    
  switch_1275:
    ;
    switch (tag_1273) {
        
      case 1:
        {
            return singleton(x_286_652_977);
            break;
        }
        
      case 0:
        {
            IntTy sz_288_654_979 =
                  ((Int64Int64CursorCursorProd *) tail_1274)->field0;
            IntTy v_289_655_980 =
                  ((Int64Int64CursorCursorProd *) tail_1274)->field1;
            CursorTy l_290_656_981 =
                     ((Int64Int64CursorCursorProd *) tail_1274)->field2;
            CursorTy r_291_657_982 =
                     ((Int64Int64CursorCursorProd *) tail_1274)->field3;
            BoolTy fltIf_795_983 = x_286_652_977 == v_289_655_980;
            
            if (fltIf_795_983) {
                return s_287_653_978;
            } else {
                BoolTy fltIf_796_984 = x_286_652_977 <= v_289_655_980;
                
                if (fltIf_796_984) {
                    CursorTy nl_292_658_985 =
                              insert(x_286_652_977, l_290_656_981);
                    
                    return balanceL(v_289_655_980, nl_292_658_985,
                                    r_291_657_982);
                } else {
                    CursorTy nr_293_659_986 =
                              insert(x_286_652_977, r_291_657_982);
                    
                    return balanceR(v_289_655_980, l_290_656_981,
                                    nr_293_659_986);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1273");
            exit(1);
        }
    }
}
CursorTy singleton(IntTy x_294_660_987)
{
    PtrTy fltPkd_797_988 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_797_988)->field0 = 1;
    
    PtrTy fltPkd_798_989 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltPkd_798_989)->field0 = 1;
    
    PtrTy tailift_1276 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
    
    ((Int64Int64Int64CursorCursorProd *) tailift_1276)->field0 = 0;
    ((Int64Int64Int64CursorCursorProd *) tailift_1276)->field1 = 1;
    ((Int64Int64Int64CursorCursorProd *) tailift_1276)->field2 = x_294_660_987;
    ((Int64Int64Int64CursorCursorProd *) tailift_1276)->field3 = fltPkd_797_988;
    ((Int64Int64Int64CursorCursorProd *) tailift_1276)->field4 = fltPkd_798_989;
    return tailift_1276;
}
CursorTy _copy_without_ptrs_IntSet(CursorTy arg_512_661_990)
{
    TagTyPacked tag_1277 = *(TagTyPacked *) arg_512_661_990;
    CursorTy tail_1278 = arg_512_661_990 + sizeof(IntTy);
    
    
  switch_1281:
    ;
    switch (tag_1277) {
        
      case 0:
        {
            IntTy x_513_662_991 =
                  ((Int64Int64CursorCursorProd *) tail_1278)->field0;
            IntTy x_514_663_992 =
                  ((Int64Int64CursorCursorProd *) tail_1278)->field1;
            CursorTy x_515_664_993 =
                     ((Int64Int64CursorCursorProd *) tail_1278)->field2;
            CursorTy x_516_665_994 =
                     ((Int64Int64CursorCursorProd *) tail_1278)->field3;
            CursorTy y_519_668_997 =  _copy_without_ptrs_IntSet(x_515_664_993);
            CursorTy y_520_669_998 =  _copy_without_ptrs_IntSet(x_516_665_994);
            PtrTy tailift_1279 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1279)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1279)->field1 =
                x_513_662_991;
            ((Int64Int64Int64CursorCursorProd *) tailift_1279)->field2 =
                x_514_663_992;
            ((Int64Int64Int64CursorCursorProd *) tailift_1279)->field3 =
                y_519_668_997;
            ((Int64Int64Int64CursorCursorProd *) tailift_1279)->field4 =
                y_520_669_998;
            return tailift_1279;
            break;
        }
        
      case 1:
        {
            PtrTy tailift_1280 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1280)->field0 = 1;
            return tailift_1280;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1277");
            exit(1);
        }
    }
}
CursorTy _copy_IntSet(CursorTy arg_503_670_999)
{
    TagTyPacked tag_1282 = *(TagTyPacked *) arg_503_670_999;
    CursorTy tail_1283 = arg_503_670_999 + sizeof(IntTy);
    
    
  switch_1286:
    ;
    switch (tag_1282) {
        
      case 0:
        {
            IntTy x_504_671_1000 =
                  ((Int64Int64CursorCursorProd *) tail_1283)->field0;
            IntTy x_505_672_1001 =
                  ((Int64Int64CursorCursorProd *) tail_1283)->field1;
            CursorTy x_506_673_1002 =
                     ((Int64Int64CursorCursorProd *) tail_1283)->field2;
            CursorTy x_507_674_1003 =
                     ((Int64Int64CursorCursorProd *) tail_1283)->field3;
            CursorTy y_510_677_1006 =  _copy_IntSet(x_506_673_1002);
            CursorTy y_511_678_1007 =  _copy_IntSet(x_507_674_1003);
            PtrTy tailift_1284 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1284)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1284)->field1 =
                x_504_671_1000;
            ((Int64Int64Int64CursorCursorProd *) tailift_1284)->field2 =
                x_505_672_1001;
            ((Int64Int64Int64CursorCursorProd *) tailift_1284)->field3 =
                y_510_677_1006;
            ((Int64Int64Int64CursorCursorProd *) tailift_1284)->field4 =
                y_511_678_1007;
            return tailift_1284;
            break;
        }
        
      case 1:
        {
            PtrTy tailift_1285 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1285)->field0 = 1;
            return tailift_1285;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1282");
            exit(1);
        }
    }
}
unsigned char _traverse_IntSet(CursorTy arg_521_679_1008)
{
    TagTyPacked tag_1287 = *(TagTyPacked *) arg_521_679_1008;
    CursorTy tail_1288 = arg_521_679_1008 + sizeof(IntTy);
    
    
  switch_1289:
    ;
    switch (tag_1287) {
        
      case 0:
        {
            IntTy x_522_680_1009 =
                  ((Int64Int64CursorCursorProd *) tail_1288)->field0;
            IntTy x_523_681_1010 =
                  ((Int64Int64CursorCursorProd *) tail_1288)->field1;
            CursorTy x_524_682_1011 =
                     ((Int64Int64CursorCursorProd *) tail_1288)->field2;
            CursorTy x_525_683_1012 =
                     ((Int64Int64CursorCursorProd *) tail_1288)->field3;
            unsigned char y_528_684_1013 =  _traverse_IntSet(x_524_682_1011);
            unsigned char y_529_685_1014 =  _traverse_IntSet(x_525_683_1012);
            
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
            printf("%s\n", "Unknown tag in: tag_1287");
            exit(1);
        }
    }
}
unsigned char _print_IntSet(CursorTy arg_530_686_1015)
{
    TagTyPacked tag_1290 = *(TagTyPacked *) arg_530_686_1015;
    CursorTy tail_1291 = arg_530_686_1015 + sizeof(IntTy);
    
    
  switch_1292:
    ;
    switch (tag_1290) {
        
      case 0:
        {
            IntTy x_531_687_1016 =
                  ((Int64Int64CursorCursorProd *) tail_1291)->field0;
            IntTy x_532_688_1017 =
                  ((Int64Int64CursorCursorProd *) tail_1291)->field1;
            CursorTy x_533_689_1018 =
                     ((Int64Int64CursorCursorProd *) tail_1291)->field2;
            CursorTy x_534_690_1019 =
                     ((Int64Int64CursorCursorProd *) tail_1291)->field3;
            unsigned char wildcard_539_691_1020 = print_symbol(1245);
            unsigned char wildcard_544_692_1021 = print_symbol(1247);
            unsigned char y_535_693_1022 = printf("%lld", x_531_687_1016);
            unsigned char wildcard_543_694_1023 = print_symbol(1247);
            unsigned char y_536_695_1024 = printf("%lld", x_532_688_1017);
            unsigned char wildcard_542_696_1025 = print_symbol(1247);
            unsigned char y_537_697_1026 =  _print_IntSet(x_533_689_1018);
            unsigned char wildcard_541_698_1027 = print_symbol(1247);
            unsigned char y_538_699_1028 =  _print_IntSet(x_534_690_1019);
            unsigned char wildcard_540_700_1029 = print_symbol(1244);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard_545_701_1030 = print_symbol(1246);
            unsigned char wildcard_546_702_1031 = print_symbol(1244);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1290");
            exit(1);
        }
    }
}
CursorTy caseFn_547(IntTy x_232_548_703_1032, CursorTy rr_238_549_704_1033,
                    IntTy rx_236_550_705_1034, IntTy rs_235_551_706_1035,
                    CursorTy rl_237_552_707_1036)
{
    TagTyPacked tag_1293 = *(TagTyPacked *) rl_237_552_707_1036;
    CursorTy tail_1294 = rl_237_552_707_1036 + sizeof(IntTy);
    
    
  switch_1298:
    ;
    switch (tag_1293) {
        
      case 0:
        {
            IntTy rls_243_708_1037 =
                  ((Int64Int64CursorCursorProd *) tail_1294)->field0;
            IntTy rlx_244_709_1038 =
                  ((Int64Int64CursorCursorProd *) tail_1294)->field1;
            CursorTy rll_245_710_1039 =
                     ((Int64Int64CursorCursorProd *) tail_1294)->field2;
            CursorTy rlr_246_711_1040 =
                     ((Int64Int64CursorCursorProd *) tail_1294)->field3;
            IntTy fltPrm_800_1041 =  size(rl_237_552_707_1036);
            IntTy fltPrm_802_1042 =  size(rr_238_549_704_1033);
            IntTy fltPrm_801_1043 = 2 * fltPrm_802_1042;
            BoolTy fltIf_799_1044 = fltPrm_800_1041 < fltPrm_801_1043;
            
            if (fltIf_799_1044) {
                IntTy fltPkd_803_1045 = 1 + rs_235_551_706_1035;
                IntTy fltPrm_806_1046 =  size(rl_237_552_707_1036);
                IntTy fltPkd_805_1047 = 1 + fltPrm_806_1046;
                PtrTy fltPkd_807_1048 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_807_1048)->field0 = 1;
                
                PtrTy fltPkd_804_1049 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1049)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1049)->field1 =
                    fltPkd_805_1047;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1049)->field2 =
                    x_232_548_703_1032;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1049)->field3 =
                    fltPkd_807_1048;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_804_1049)->field4 =
                    rl_237_552_707_1036;
                
                PtrTy tailift_1295 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field1 =
                    fltPkd_803_1045;
                ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field2 =
                    rx_236_550_705_1034;
                ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field3 =
                    fltPkd_804_1049;
                ((Int64Int64Int64CursorCursorProd *) tailift_1295)->field4 =
                    rr_238_549_704_1033;
                return tailift_1295;
            } else {
                IntTy fltPkd_808_1050 = 1 + rs_235_551_706_1035;
                IntTy fltPkd_809_1051 =  val(rl_237_552_707_1036);
                CursorTy fltAppE_813_1052 =  left(rl_237_552_707_1036);
                IntTy fltPrm_812_1053 =  size(fltAppE_813_1052);
                IntTy fltPkd_811_1054 = 1 + fltPrm_812_1053;
                PtrTy fltPkd_814_1055 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_814_1055)->field0 = 1;
                
                CursorTy fltPkd_815_1056 =  left(rl_237_552_707_1036);
                PtrTy fltPkd_810_1057 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_810_1057)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_810_1057)->field1 =
                    fltPkd_811_1054;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_810_1057)->field2 =
                    x_232_548_703_1032;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_810_1057)->field3 =
                    fltPkd_814_1055;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_810_1057)->field4 =
                    fltPkd_815_1056;
                
                IntTy fltPrm_819_1058 =  size(rr_238_549_704_1033);
                IntTy fltPrm_818_1059 = 1 + fltPrm_819_1058;
                CursorTy fltAppE_821_1060 =  right(rl_237_552_707_1036);
                IntTy fltPrm_820_1061 =  size(fltAppE_821_1060);
                IntTy fltPkd_817_1062 = fltPrm_818_1059 + fltPrm_820_1061;
                CursorTy fltPkd_822_1063 =  right(rl_237_552_707_1036);
                PtrTy fltPkd_816_1064 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_816_1064)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_816_1064)->field1 =
                    fltPkd_817_1062;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_816_1064)->field2 =
                    rx_236_550_705_1034;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_816_1064)->field3 =
                    fltPkd_822_1063;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_816_1064)->field4 =
                    rr_238_549_704_1033;
                
                PtrTy tailift_1296 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1296)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1296)->field1 =
                    fltPkd_808_1050;
                ((Int64Int64Int64CursorCursorProd *) tailift_1296)->field2 =
                    fltPkd_809_1051;
                ((Int64Int64Int64CursorCursorProd *) tailift_1296)->field3 =
                    fltPkd_810_1057;
                ((Int64Int64Int64CursorCursorProd *) tailift_1296)->field4 =
                    fltPkd_816_1064;
                return tailift_1296;
            }
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_824_1065 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_824_1065)->field0 = 1;
            
            PtrTy fltPkd_825_1066 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_825_1066)->field0 = 1;
            
            PtrTy fltPkd_823_1067 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_823_1067)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_823_1067)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_823_1067)->field2 =
                x_232_548_703_1032;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_823_1067)->field3 =
                fltPkd_824_1065;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_823_1067)->field4 =
                fltPkd_825_1066;
            
            PtrTy tailift_1297 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1297)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1297)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1297)->field2 =
                rx_236_550_705_1034;
            ((Int64Int64Int64CursorCursorProd *) tailift_1297)->field3 =
                fltPkd_823_1067;
            ((Int64Int64Int64CursorCursorProd *) tailift_1297)->field4 =
                rr_238_549_704_1033;
            return tailift_1297;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1293");
            exit(1);
        }
    }
}
CursorTy caseFn_553(IntTy x_232_554_712_1068, CursorTy r_234_555_713_1069,
                    IntTy rx_236_556_714_1070, CursorTy rl_237_557_715_1071)
{
    TagTyPacked tag_1299 = *(TagTyPacked *) rl_237_557_715_1071;
    CursorTy tail_1300 = rl_237_557_715_1071 + sizeof(IntTy);
    
    
  switch_1303:
    ;
    switch (tag_1299) {
        
      case 0:
        {
            IntTy rls_247_716_1072 =
                  ((Int64Int64CursorCursorProd *) tail_1300)->field0;
            IntTy rlx_248_717_1073 =
                  ((Int64Int64CursorCursorProd *) tail_1300)->field1;
            CursorTy rll_249_718_1074 =
                     ((Int64Int64CursorCursorProd *) tail_1300)->field2;
            CursorTy rlr_250_719_1075 =
                     ((Int64Int64CursorCursorProd *) tail_1300)->field3;
            IntTy fltPkd_826_1076 =  val(rl_237_557_715_1071);
            PtrTy fltPkd_828_1077 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_828_1077)->field0 = 1;
            
            PtrTy fltPkd_829_1078 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_829_1078)->field0 = 1;
            
            PtrTy fltPkd_827_1079 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_827_1079)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_827_1079)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_827_1079)->field2 =
                x_232_554_712_1068;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_827_1079)->field3 =
                fltPkd_828_1077;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_827_1079)->field4 =
                fltPkd_829_1078;
            
            PtrTy fltPkd_831_1080 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_831_1080)->field0 = 1;
            
            PtrTy fltPkd_832_1081 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_832_1081)->field0 = 1;
            
            PtrTy fltPkd_830_1082 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_830_1082)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_830_1082)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_830_1082)->field2 =
                rx_236_556_714_1070;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_830_1082)->field3 =
                fltPkd_831_1080;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_830_1082)->field4 =
                fltPkd_832_1081;
            
            PtrTy tailift_1301 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field2 =
                fltPkd_826_1076;
            ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field3 =
                fltPkd_827_1079;
            ((Int64Int64Int64CursorCursorProd *) tailift_1301)->field4 =
                fltPkd_830_1082;
            return tailift_1301;
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_833_1083 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_833_1083)->field0 = 1;
            
            PtrTy tailift_1302 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field1 = 2;
            ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field2 =
                x_232_554_712_1068;
            ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field3 =
                fltPkd_833_1083;
            ((Int64Int64Int64CursorCursorProd *) tailift_1302)->field4 =
                r_234_555_713_1069;
            return tailift_1302;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1299");
            exit(1);
        }
    }
}
CursorTy caseFn_558(IntTy x_232_559_720_1084, CursorTy r_234_560_721_1085,
                    CursorTy rr_238_561_722_1086, IntTy rx_236_562_723_1087,
                    IntTy rs_235_563_724_1088, CursorTy rl_237_564_725_1089)
{
    TagTyPacked tag_1304 = *(TagTyPacked *) rr_238_561_722_1086;
    CursorTy tail_1305 = rr_238_561_722_1086 + sizeof(IntTy);
    
    
  switch_1306:
    ;
    switch (tag_1304) {
        
      case 0:
        {
            IntTy rrs_239_726_1090 =
                  ((Int64Int64CursorCursorProd *) tail_1305)->field0;
            IntTy rrx_240_727_1091 =
                  ((Int64Int64CursorCursorProd *) tail_1305)->field1;
            CursorTy rrl_241_728_1092 =
                     ((Int64Int64CursorCursorProd *) tail_1305)->field2;
            CursorTy rrr_242_729_1093 =
                     ((Int64Int64CursorCursorProd *) tail_1305)->field3;
            
            return caseFn_547(x_232_559_720_1084, rr_238_561_722_1086,
                              rx_236_562_723_1087, rs_235_563_724_1088,
                              rl_237_564_725_1089);
            break;
        }
        
      case 1:
        {
            return caseFn_553(x_232_559_720_1084, r_234_560_721_1085,
                              rx_236_562_723_1087, rl_237_564_725_1089);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1304");
            exit(1);
        }
    }
}
CursorTy caseFn_565(IntTy x_232_566_730_1094, CursorTy r_234_567_731_1095)
{
    TagTyPacked tag_1307 = *(TagTyPacked *) r_234_567_731_1095;
    CursorTy tail_1308 = r_234_567_731_1095 + sizeof(IntTy);
    
    
  switch_1310:
    ;
    switch (tag_1307) {
        
      case 1:
        {
            PtrTy fltPkd_834_1096 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_834_1096)->field0 = 1;
            
            PtrTy fltPkd_835_1097 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_835_1097)->field0 = 1;
            
            PtrTy tailift_1309 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1309)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1309)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) tailift_1309)->field2 =
                x_232_566_730_1094;
            ((Int64Int64Int64CursorCursorProd *) tailift_1309)->field3 =
                fltPkd_834_1096;
            ((Int64Int64Int64CursorCursorProd *) tailift_1309)->field4 =
                fltPkd_835_1097;
            return tailift_1309;
            break;
        }
        
      case 0:
        {
            IntTy rs_235_732_1098 =
                  ((Int64Int64CursorCursorProd *) tail_1308)->field0;
            IntTy rx_236_733_1099 =
                  ((Int64Int64CursorCursorProd *) tail_1308)->field1;
            CursorTy rl_237_734_1100 =
                     ((Int64Int64CursorCursorProd *) tail_1308)->field2;
            CursorTy rr_238_735_1101 =
                     ((Int64Int64CursorCursorProd *) tail_1308)->field3;
            
            return caseFn_558(x_232_566_730_1094, r_234_567_731_1095,
                              rr_238_735_1101, rx_236_733_1099, rs_235_732_1098,
                              rl_237_734_1100);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1307");
            exit(1);
        }
    }
}
CursorTy caseFn_568(CursorTy l_233_569_736_1102, IntTy x_232_570_737_1103,
                    CursorTy r_234_571_738_1104, IntTy ls_251_572_739_1105)
{
    TagTyPacked tag_1311 = *(TagTyPacked *) r_234_571_738_1104;
    CursorTy tail_1312 = r_234_571_738_1104 + sizeof(IntTy);
    
    
  switch_1317:
    ;
    switch (tag_1311) {
        
      case 1:
        {
            IntTy fltPkd_836_1106 = 1 + ls_251_572_739_1105;
            PtrTy fltPkd_837_1107 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_837_1107)->field0 = 1;
            
            PtrTy tailift_1313 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field1 =
                fltPkd_836_1106;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field2 =
                x_232_570_737_1103;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field3 =
                l_233_569_736_1102;
            ((Int64Int64Int64CursorCursorProd *) tailift_1313)->field4 =
                fltPkd_837_1107;
            return tailift_1313;
            break;
        }
        
      case 0:
        {
            IntTy rs_255_740_1108 =
                  ((Int64Int64CursorCursorProd *) tail_1312)->field0;
            IntTy rx_256_741_1109 =
                  ((Int64Int64CursorCursorProd *) tail_1312)->field1;
            CursorTy rl_257_742_1110 =
                     ((Int64Int64CursorCursorProd *) tail_1312)->field2;
            CursorTy rr_258_743_1111 =
                     ((Int64Int64CursorCursorProd *) tail_1312)->field3;
            IntTy fltPrm_839_1112 = 3 * ls_251_572_739_1105;
            BoolTy fltIf_838_1113 = rs_255_740_1108 > fltPrm_839_1112;
            
            if (fltIf_838_1113) {
                IntTy fltPrm_841_1114 =  size(rl_257_742_1110);
                IntTy fltPrm_843_1115 =  size(rr_258_743_1111);
                IntTy fltPrm_842_1116 = 2 * fltPrm_843_1115;
                BoolTy fltIf_840_1117 = fltPrm_841_1114 < fltPrm_842_1116;
                
                if (fltIf_840_1117) {
                    IntTy fltPrm_845_1118 = 1 + ls_251_572_739_1105;
                    IntTy fltPkd_844_1119 = fltPrm_845_1118 + rs_255_740_1108;
                    IntTy fltPrm_848_1120 = 1 + ls_251_572_739_1105;
                    IntTy fltPrm_849_1121 =  size(rl_257_742_1110);
                    IntTy fltPkd_847_1122 = fltPrm_848_1120 + fltPrm_849_1121;
                    PtrTy fltPkd_846_1123 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_846_1123)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_846_1123)->field1 =
                        fltPkd_847_1122;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_846_1123)->field2 =
                        x_232_570_737_1103;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_846_1123)->field3 =
                        l_233_569_736_1102;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_846_1123)->field4 =
                        rl_257_742_1110;
                    
                    PtrTy tailift_1314 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1314)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1314)->field1 =
                        fltPkd_844_1119;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1314)->field2 =
                        rx_256_741_1109;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1314)->field3 =
                        fltPkd_846_1123;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1314)->field4 =
                        rr_258_743_1111;
                    return tailift_1314;
                } else {
                    IntTy fltPrm_851_1124 = 1 + ls_251_572_739_1105;
                    IntTy fltPkd_850_1125 = fltPrm_851_1124 + rs_255_740_1108;
                    IntTy fltPkd_852_1126 =  val(rl_257_742_1110);
                    IntTy fltPrm_855_1127 = 1 + ls_251_572_739_1105;
                    CursorTy fltAppE_857_1128 =  left(rl_257_742_1110);
                    IntTy fltPrm_856_1129 =  size(fltAppE_857_1128);
                    IntTy fltPkd_854_1130 = fltPrm_855_1127 + fltPrm_856_1129;
                    CursorTy fltPkd_858_1131 =  left(rl_257_742_1110);
                    PtrTy fltPkd_853_1132 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_853_1132)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_853_1132)->field1 =
                        fltPkd_854_1130;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_853_1132)->field2 =
                        x_232_570_737_1103;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_853_1132)->field3 =
                        l_233_569_736_1102;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_853_1132)->field4 =
                        fltPkd_858_1131;
                    
                    IntTy fltPrm_862_1133 =  size(rr_258_743_1111);
                    IntTy fltPrm_861_1134 = 1 + fltPrm_862_1133;
                    CursorTy fltAppE_864_1135 =  right(rl_257_742_1110);
                    IntTy fltPrm_863_1136 =  size(fltAppE_864_1135);
                    IntTy fltPkd_860_1137 = fltPrm_861_1134 + fltPrm_863_1136;
                    CursorTy fltPkd_865_1138 =  right(rl_257_742_1110);
                    PtrTy fltPkd_859_1139 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_859_1139)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_859_1139)->field1 =
                        fltPkd_860_1137;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_859_1139)->field2 =
                        rx_256_741_1109;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_859_1139)->field3 =
                        fltPkd_865_1138;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_859_1139)->field4 =
                        rr_258_743_1111;
                    
                    PtrTy tailift_1315 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1315)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1315)->field1 =
                        fltPkd_850_1125;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1315)->field2 =
                        fltPkd_852_1126;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1315)->field3 =
                        fltPkd_853_1132;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1315)->field4 =
                        fltPkd_859_1139;
                    return tailift_1315;
                }
            } else {
                IntTy fltPrm_867_1140 = 1 + ls_251_572_739_1105;
                IntTy fltPkd_866_1141 = fltPrm_867_1140 + rs_255_740_1108;
                PtrTy tailift_1316 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1316)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1316)->field1 =
                    fltPkd_866_1141;
                ((Int64Int64Int64CursorCursorProd *) tailift_1316)->field2 =
                    x_232_570_737_1103;
                ((Int64Int64Int64CursorCursorProd *) tailift_1316)->field3 =
                    l_233_569_736_1102;
                ((Int64Int64Int64CursorCursorProd *) tailift_1316)->field4 =
                    r_234_571_738_1104;
                return tailift_1316;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1311");
            exit(1);
        }
    }
}
CursorTy caseFn_573(IntTy x_259_574_744_1142, CursorTy ll_264_575_745_1143,
                    IntTy lx_263_576_746_1144, IntTy ls_262_577_747_1145,
                    CursorTy lr_265_578_748_1146, IntTy lls_266_579_749_1147)
{
    TagTyPacked tag_1318 = *(TagTyPacked *) lr_265_578_748_1146;
    CursorTy tail_1319 = lr_265_578_748_1146 + sizeof(IntTy);
    
    
  switch_1323:
    ;
    switch (tag_1318) {
        
      case 0:
        {
            IntTy lrs_270_750_1148 =
                  ((Int64Int64CursorCursorProd *) tail_1319)->field0;
            IntTy lrx_271_751_1149 =
                  ((Int64Int64CursorCursorProd *) tail_1319)->field1;
            CursorTy lrl_272_752_1150 =
                     ((Int64Int64CursorCursorProd *) tail_1319)->field2;
            CursorTy lrr_273_753_1151 =
                     ((Int64Int64CursorCursorProd *) tail_1319)->field3;
            IntTy fltPrm_869_1152 = 2 * lls_266_579_749_1147;
            BoolTy fltIf_868_1153 = lrs_270_750_1148 < fltPrm_869_1152;
            
            if (fltIf_868_1153) {
                IntTy fltPkd_870_1154 = 1 + ls_262_577_747_1145;
                IntTy fltPkd_872_1155 = 1 + lrs_270_750_1148;
                PtrTy fltPkd_873_1156 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_873_1156)->field0 = 1;
                
                PtrTy fltPkd_871_1157 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1157)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1157)->field1 =
                    fltPkd_872_1155;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1157)->field2 =
                    x_259_574_744_1142;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1157)->field3 =
                    lr_265_578_748_1146;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_871_1157)->field4 =
                    fltPkd_873_1156;
                
                PtrTy tailift_1320 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field1 =
                    fltPkd_870_1154;
                ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field2 =
                    lx_263_576_746_1144;
                ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field3 =
                    ll_264_575_745_1143;
                ((Int64Int64Int64CursorCursorProd *) tailift_1320)->field4 =
                    fltPkd_871_1157;
                return tailift_1320;
            } else {
                IntTy fltPkd_874_1158 = 1 + ls_262_577_747_1145;
                IntTy fltPrm_877_1159 = 1 + lls_266_579_749_1147;
                IntTy fltPrm_878_1160 =  size(lrl_272_752_1150);
                IntTy fltPkd_876_1161 = fltPrm_877_1159 + fltPrm_878_1160;
                PtrTy fltPkd_875_1162 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1162)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1162)->field1 =
                    fltPkd_876_1161;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1162)->field2 =
                    lx_263_576_746_1144;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1162)->field3 =
                    ll_264_575_745_1143;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_875_1162)->field4 =
                    lrl_272_752_1150;
                
                IntTy fltPrm_881_1163 =  size(lrr_273_753_1151);
                IntTy fltPkd_880_1164 = 1 + fltPrm_881_1163;
                PtrTy fltPkd_882_1165 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) fltPkd_882_1165)->field0 = 1;
                
                PtrTy fltPkd_879_1166 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) fltPkd_879_1166)->field0 =
                    0;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_879_1166)->field1 =
                    fltPkd_880_1164;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_879_1166)->field2 =
                    x_259_574_744_1142;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_879_1166)->field3 =
                    lrr_273_753_1151;
                ((Int64Int64Int64CursorCursorProd *) fltPkd_879_1166)->field4 =
                    fltPkd_882_1165;
                
                PtrTy tailift_1321 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1321)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1321)->field1 =
                    fltPkd_874_1158;
                ((Int64Int64Int64CursorCursorProd *) tailift_1321)->field2 =
                    lrx_271_751_1149;
                ((Int64Int64Int64CursorCursorProd *) tailift_1321)->field3 =
                    fltPkd_875_1162;
                ((Int64Int64Int64CursorCursorProd *) tailift_1321)->field4 =
                    fltPkd_879_1166;
                return tailift_1321;
            }
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_884_1167 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_884_1167)->field0 = 1;
            
            PtrTy fltPkd_885_1168 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_885_1168)->field0 = 1;
            
            PtrTy fltPkd_883_1169 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_883_1169)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_883_1169)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_883_1169)->field2 =
                x_259_574_744_1142;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_883_1169)->field3 =
                fltPkd_884_1167;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_883_1169)->field4 =
                fltPkd_885_1168;
            
            PtrTy tailift_1322 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1322)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1322)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1322)->field2 =
                lx_263_576_746_1144;
            ((Int64Int64Int64CursorCursorProd *) tailift_1322)->field3 =
                ll_264_575_745_1143;
            ((Int64Int64Int64CursorCursorProd *) tailift_1322)->field4 =
                fltPkd_883_1169;
            return tailift_1322;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1318");
            exit(1);
        }
    }
}
CursorTy caseFn_580(CursorTy l_260_581_754_1170, IntTy x_259_582_755_1171,
                    IntTy lx_263_583_756_1172, CursorTy lr_265_584_757_1173)
{
    TagTyPacked tag_1324 = *(TagTyPacked *) lr_265_584_757_1173;
    CursorTy tail_1325 = lr_265_584_757_1173 + sizeof(IntTy);
    
    
  switch_1328:
    ;
    switch (tag_1324) {
        
      case 0:
        {
            IntTy lrs_274_758_1174 =
                  ((Int64Int64CursorCursorProd *) tail_1325)->field0;
            IntTy lrx_275_759_1175 =
                  ((Int64Int64CursorCursorProd *) tail_1325)->field1;
            CursorTy lrl_276_760_1176 =
                     ((Int64Int64CursorCursorProd *) tail_1325)->field2;
            CursorTy lrr_277_761_1177 =
                     ((Int64Int64CursorCursorProd *) tail_1325)->field3;
            PtrTy fltPkd_887_1178 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_887_1178)->field0 = 1;
            
            PtrTy fltPkd_888_1179 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_888_1179)->field0 = 1;
            
            PtrTy fltPkd_886_1180 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_886_1180)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_886_1180)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_886_1180)->field2 =
                lx_263_583_756_1172;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_886_1180)->field3 =
                fltPkd_887_1178;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_886_1180)->field4 =
                fltPkd_888_1179;
            
            PtrTy fltPkd_890_1181 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_890_1181)->field0 = 1;
            
            PtrTy fltPkd_891_1182 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_891_1182)->field0 = 1;
            
            PtrTy fltPkd_889_1183 =
                  ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) fltPkd_889_1183)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_889_1183)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_889_1183)->field2 =
                x_259_582_755_1171;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_889_1183)->field3 =
                fltPkd_890_1181;
            ((Int64Int64Int64CursorCursorProd *) fltPkd_889_1183)->field4 =
                fltPkd_891_1182;
            
            PtrTy tailift_1326 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field1 = 3;
            ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field2 =
                lrx_275_759_1175;
            ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field3 =
                fltPkd_886_1180;
            ((Int64Int64Int64CursorCursorProd *) tailift_1326)->field4 =
                fltPkd_889_1183;
            return tailift_1326;
            break;
        }
        
      case 1:
        {
            PtrTy fltPkd_892_1184 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_892_1184)->field0 = 1;
            
            PtrTy tailift_1327 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field1 = 2;
            ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field2 =
                x_259_582_755_1171;
            ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field3 =
                l_260_581_754_1170;
            ((Int64Int64Int64CursorCursorProd *) tailift_1327)->field4 =
                fltPkd_892_1184;
            return tailift_1327;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1324");
            exit(1);
        }
    }
}
CursorTy caseFn_585(CursorTy l_260_586_762_1185, IntTy x_259_587_763_1186,
                    CursorTy ll_264_588_764_1187, IntTy lx_263_589_765_1188,
                    IntTy ls_262_590_766_1189, CursorTy lr_265_591_767_1190)
{
    TagTyPacked tag_1329 = *(TagTyPacked *) ll_264_588_764_1187;
    CursorTy tail_1330 = ll_264_588_764_1187 + sizeof(IntTy);
    
    
  switch_1331:
    ;
    switch (tag_1329) {
        
      case 0:
        {
            IntTy lls_266_768_1191 =
                  ((Int64Int64CursorCursorProd *) tail_1330)->field0;
            IntTy llx_267_769_1192 =
                  ((Int64Int64CursorCursorProd *) tail_1330)->field1;
            CursorTy lll_268_770_1193 =
                     ((Int64Int64CursorCursorProd *) tail_1330)->field2;
            CursorTy llr_269_771_1194 =
                     ((Int64Int64CursorCursorProd *) tail_1330)->field3;
            
            return caseFn_573(x_259_587_763_1186, ll_264_588_764_1187,
                              lx_263_589_765_1188, ls_262_590_766_1189,
                              lr_265_591_767_1190, lls_266_768_1191);
            break;
        }
        
      case 1:
        {
            return caseFn_580(l_260_586_762_1185, x_259_587_763_1186,
                              lx_263_589_765_1188, lr_265_591_767_1190);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1329");
            exit(1);
        }
    }
}
CursorTy caseFn_592(CursorTy l_260_593_772_1195, IntTy x_259_594_773_1196)
{
    TagTyPacked tag_1332 = *(TagTyPacked *) l_260_593_772_1195;
    CursorTy tail_1333 = l_260_593_772_1195 + sizeof(IntTy);
    
    
  switch_1335:
    ;
    switch (tag_1332) {
        
      case 1:
        {
            PtrTy fltPkd_893_1197 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_893_1197)->field0 = 1;
            
            PtrTy fltPkd_894_1198 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_894_1198)->field0 = 1;
            
            PtrTy tailift_1334 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1334)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1334)->field1 = 1;
            ((Int64Int64Int64CursorCursorProd *) tailift_1334)->field2 =
                x_259_594_773_1196;
            ((Int64Int64Int64CursorCursorProd *) tailift_1334)->field3 =
                fltPkd_893_1197;
            ((Int64Int64Int64CursorCursorProd *) tailift_1334)->field4 =
                fltPkd_894_1198;
            return tailift_1334;
            break;
        }
        
      case 0:
        {
            IntTy ls_262_774_1199 =
                  ((Int64Int64CursorCursorProd *) tail_1333)->field0;
            IntTy lx_263_775_1200 =
                  ((Int64Int64CursorCursorProd *) tail_1333)->field1;
            CursorTy ll_264_776_1201 =
                     ((Int64Int64CursorCursorProd *) tail_1333)->field2;
            CursorTy lr_265_777_1202 =
                     ((Int64Int64CursorCursorProd *) tail_1333)->field3;
            
            return caseFn_585(l_260_593_772_1195, x_259_594_773_1196,
                              ll_264_776_1201, lx_263_775_1200, ls_262_774_1199,
                              lr_265_777_1202);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1332");
            exit(1);
        }
    }
}
CursorTy caseFn_595(CursorTy r_261_596_778_1203, CursorTy l_260_597_779_1204,
                    IntTy x_259_598_780_1205, IntTy rs_278_599_781_1206)
{
    TagTyPacked tag_1336 = *(TagTyPacked *) l_260_597_779_1204;
    CursorTy tail_1337 = l_260_597_779_1204 + sizeof(IntTy);
    
    
  switch_1342:
    ;
    switch (tag_1336) {
        
      case 1:
        {
            IntTy fltPkd_895_1207 = 1 + rs_278_599_781_1206;
            PtrTy fltPkd_896_1208 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) fltPkd_896_1208)->field0 = 1;
            
            PtrTy tailift_1338 = ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
            
            ((Int64Int64Int64CursorCursorProd *) tailift_1338)->field0 = 0;
            ((Int64Int64Int64CursorCursorProd *) tailift_1338)->field1 =
                fltPkd_895_1207;
            ((Int64Int64Int64CursorCursorProd *) tailift_1338)->field2 =
                x_259_598_780_1205;
            ((Int64Int64Int64CursorCursorProd *) tailift_1338)->field3 =
                fltPkd_896_1208;
            ((Int64Int64Int64CursorCursorProd *) tailift_1338)->field4 =
                r_261_596_778_1203;
            return tailift_1338;
            break;
        }
        
      case 0:
        {
            IntTy ls_282_782_1209 =
                  ((Int64Int64CursorCursorProd *) tail_1337)->field0;
            IntTy lx_283_783_1210 =
                  ((Int64Int64CursorCursorProd *) tail_1337)->field1;
            CursorTy ll_284_784_1211 =
                     ((Int64Int64CursorCursorProd *) tail_1337)->field2;
            CursorTy lr_285_785_1212 =
                     ((Int64Int64CursorCursorProd *) tail_1337)->field3;
            IntTy fltPrm_898_1213 = 3 * rs_278_599_781_1206;
            BoolTy fltIf_897_1214 = ls_282_782_1209 > fltPrm_898_1213;
            
            if (fltIf_897_1214) {
                IntTy fltPrm_900_1215 =  size(lr_285_785_1212);
                IntTy fltPrm_902_1216 =  size(ll_284_784_1211);
                IntTy fltPrm_901_1217 = 2 * fltPrm_902_1216;
                BoolTy fltIf_899_1218 = fltPrm_900_1215 < fltPrm_901_1217;
                
                if (fltIf_899_1218) {
                    IntTy fltPrm_904_1219 = 1 + ls_282_782_1209;
                    IntTy fltPkd_903_1220 = fltPrm_904_1219 +
                          rs_278_599_781_1206;
                    IntTy fltPrm_907_1221 = 1 + rs_278_599_781_1206;
                    IntTy fltPrm_908_1222 =  size(lr_285_785_1212);
                    IntTy fltPkd_906_1223 = fltPrm_907_1221 + fltPrm_908_1222;
                    PtrTy fltPkd_905_1224 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_905_1224)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_905_1224)->field1 =
                        fltPkd_906_1223;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_905_1224)->field2 =
                        x_259_598_780_1205;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_905_1224)->field3 =
                        lr_285_785_1212;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_905_1224)->field4 =
                        r_261_596_778_1203;
                    
                    PtrTy tailift_1339 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1339)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1339)->field1 =
                        fltPkd_903_1220;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1339)->field2 =
                        lx_283_783_1210;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1339)->field3 =
                        ll_284_784_1211;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1339)->field4 =
                        fltPkd_905_1224;
                    return tailift_1339;
                } else {
                    IntTy fltPrm_910_1225 = 1 + ls_282_782_1209;
                    IntTy fltPkd_909_1226 = fltPrm_910_1225 +
                          rs_278_599_781_1206;
                    IntTy fltPkd_911_1227 =  val(lr_285_785_1212);
                    IntTy fltPrm_915_1228 =  size(ll_284_784_1211);
                    IntTy fltPrm_914_1229 = 1 + fltPrm_915_1228;
                    CursorTy fltAppE_917_1230 =  left(lr_285_785_1212);
                    IntTy fltPrm_916_1231 =  size(fltAppE_917_1230);
                    IntTy fltPkd_913_1232 = fltPrm_914_1229 + fltPrm_916_1231;
                    CursorTy fltPkd_918_1233 =  left(lr_285_785_1212);
                    PtrTy fltPkd_912_1234 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_912_1234)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_912_1234)->field1 =
                        fltPkd_913_1232;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_912_1234)->field2 =
                        lx_283_783_1210;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_912_1234)->field3 =
                        ll_284_784_1211;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_912_1234)->field4 =
                        fltPkd_918_1233;
                    
                    IntTy fltPrm_921_1235 = 1 + rs_278_599_781_1206;
                    CursorTy fltAppE_923_1236 =  right(lr_285_785_1212);
                    IntTy fltPrm_922_1237 =  size(fltAppE_923_1236);
                    IntTy fltPkd_920_1238 = fltPrm_921_1235 + fltPrm_922_1237;
                    CursorTy fltPkd_924_1239 =  right(lr_285_785_1212);
                    PtrTy fltPkd_919_1240 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_919_1240)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_919_1240)->field1 =
                        fltPkd_920_1238;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_919_1240)->field2 =
                        x_259_598_780_1205;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_919_1240)->field3 =
                        fltPkd_924_1239;
                    ((Int64Int64Int64CursorCursorProd *) fltPkd_919_1240)->field4 =
                        r_261_596_778_1203;
                    
                    PtrTy tailift_1340 =
                          ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                    
                    ((Int64Int64Int64CursorCursorProd *) tailift_1340)->field0 =
                        0;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1340)->field1 =
                        fltPkd_909_1226;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1340)->field2 =
                        fltPkd_911_1227;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1340)->field3 =
                        fltPkd_912_1234;
                    ((Int64Int64Int64CursorCursorProd *) tailift_1340)->field4 =
                        fltPkd_919_1240;
                    return tailift_1340;
                }
            } else {
                IntTy fltPrm_926_1241 = 1 + ls_282_782_1209;
                IntTy fltPkd_925_1242 = fltPrm_926_1241 + rs_278_599_781_1206;
                PtrTy tailift_1341 =
                      ALLOC(sizeof(Int64Int64Int64CursorCursorProd));
                
                ((Int64Int64Int64CursorCursorProd *) tailift_1341)->field0 = 0;
                ((Int64Int64Int64CursorCursorProd *) tailift_1341)->field1 =
                    fltPkd_925_1242;
                ((Int64Int64Int64CursorCursorProd *) tailift_1341)->field2 =
                    x_259_598_780_1205;
                ((Int64Int64Int64CursorCursorProd *) tailift_1341)->field3 =
                    l_260_597_779_1204;
                ((Int64Int64Int64CursorCursorProd *) tailift_1341)->field4 =
                    r_261_596_778_1203;
                return tailift_1341;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1336");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1244, ")");
    add_symbol(1245, "(PureSet");
    add_symbol(1246, "(EmptySet");
    add_symbol(1247, " ");
    
    CursorTy fltAppE_788_927 =  empty();
    CursorTy fltAppE_787_928 =  insert_num(100, fltAppE_788_927);
    CursorTy fltAppE_786_929 =  insert_num(100, fltAppE_787_928);
    IntTy tmp_app_1243 =  sum(fltAppE_786_929);
    
    printf("%lld", tmp_app_1243);
    printf("\n");
    free_symtable();
    return 0;
}