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
typedef struct Int64CursorProd_struct {
            IntTy field0;
            CursorTy field1;
        } Int64CursorProd;
typedef struct BoolCursorProd_struct {
            BoolTy field0;
            CursorTy field1;
        } BoolCursorProd;
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
typedef struct CursorCursorCursorCursorProd_struct {
            CursorTy field0;
            CursorTy field1;
            CursorTy field2;
            CursorTy field3;
        } CursorCursorCursorCursorProd;
CursorCursorCursorCursorProd _copy_without_ptrs_Tree_v_41(CursorTy end_r_334,
                                                          CursorTy end_r_335,
                                                          CursorTy loc_333,
                                                          CursorTy arg_76_130_180);
CursorCursorCursorCursorProd _copy_Tree_v_41(CursorTy end_r_338,
                                             CursorTy end_r_339,
                                             CursorTy loc_337,
                                             CursorTy arg_65_141_191);
CursorProd _traverse_Tree_v_41(CursorTy end_r_341, CursorTy arg_87_152_202);
CursorProd _print_Tree_v_41(CursorTy end_r_343, CursorTy arg_98_160_210);
CursorCursorCursorCursorProd
_add_size_and_rel_offsets_Tree_v_41(CursorTy end_r_346, CursorTy end_r_347,
                                    CursorTy loc_345, CursorTy arg_319);
CursorCursorCursorCursorProd _copy_without_ptrs_Tree_v_41(CursorTy end_r_334,
                                                          CursorTy end_r_335,
                                                          CursorTy loc_333,
                                                          CursorTy arg_76_130_180)
{
    CursorTy loc_367 = loc_333 + 1;
    CursorTy loc_368 = loc_367 + 8;
    TagTyPacked tmpval_733 = *(TagTyPacked *) arg_76_130_180;
    CursorTy tmpcur_734 = arg_76_130_180 + 1;
    
    
  switch_789:
    ;
    switch (tmpval_733) {
        
      case 0:
        {
            IntTy tmpval_735 = *(IntTy *) tmpcur_734;
            CursorTy tmpcur_736 = tmpcur_734 + sizeof(IntTy);
            BoolTy tmpval_737 = *(BoolTy *) tmpcur_736;
            CursorTy tmpcur_738 = tmpcur_736 + sizeof(BoolTy);
            CursorTy jump_450 = tmpcur_736 + 1;
            CursorTy jump_449 = tmpcur_734 + 8;
            
            *(TagTyPacked *) loc_333 = 0;
            
            CursorTy writetag_530 = loc_333 + 1;
            
            *(IntTy *) writetag_530 = tmpval_735;
            
            CursorTy writecur_531 = writetag_530 + sizeof(IntTy);
            
            *(BoolTy *) writecur_531 = tmpval_737;
            
            CursorTy writecur_532 = writecur_531 + sizeof(BoolTy);
            
            return (CursorCursorCursorCursorProd) {end_r_335, jump_450, loc_333,
                                                   writecur_532};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_743 = *(IntTy *) tmpcur_734;
            CursorTy tmpcur_744 = tmpcur_734 + sizeof(IntTy);
            CursorTy jump_452 = tmpcur_734 + 8;
            CursorCursorCursorCursorProd tmp_struct_0 =
                                          _copy_without_ptrs_Tree_v_41(end_r_334, end_r_335, loc_368, tmpcur_744);
            CursorTy pvrtmp_745 = tmp_struct_0.field0;
            CursorTy pvrtmp_746 = tmp_struct_0.field1;
            CursorTy pvrtmp_747 = tmp_struct_0.field2;
            CursorTy pvrtmp_748 = tmp_struct_0.field3;
            CursorCursorCursorCursorProd tmp_struct_1 =
                                          _copy_without_ptrs_Tree_v_41(end_r_334, pvrtmp_745, pvrtmp_748, pvrtmp_746);
            CursorTy pvrtmp_753 = tmp_struct_1.field0;
            CursorTy pvrtmp_754 = tmp_struct_1.field1;
            CursorTy pvrtmp_755 = tmp_struct_1.field2;
            CursorTy pvrtmp_756 = tmp_struct_1.field3;
            
            *(TagTyPacked *) loc_333 = 1;
            
            CursorTy writetag_538 = loc_333 + 1;
            
            *(IntTy *) writetag_538 = tmpval_743;
            
            CursorTy writecur_539 = writetag_538 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_753, pvrtmp_754,
                                                   loc_333, pvrtmp_756};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_765 = *(CursorTy *) tmpcur_734;
            CursorTy tmpaftercur_766 = tmpcur_734 + 8;
            CursorTy jump_487 = tmpcur_734 + 8;
            CursorCursorCursorCursorProd tmp_struct_2 =
                                          _copy_without_ptrs_Tree_v_41(end_r_334, end_r_335, loc_333, tmpcur_765);
            CursorTy pvrtmp_767 = tmp_struct_2.field0;
            CursorTy pvrtmp_768 = tmp_struct_2.field1;
            CursorTy pvrtmp_769 = tmp_struct_2.field2;
            CursorTy pvrtmp_770 = tmp_struct_2.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_767, jump_487,
                                                   pvrtmp_769, pvrtmp_770};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_777 = *(CursorTy *) tmpcur_734;
            CursorTy tmpaftercur_778 = tmpcur_734 + 8;
            CursorCursorCursorCursorProd tmp_struct_3 =
                                          _copy_without_ptrs_Tree_v_41(end_r_334, end_r_335, loc_333, tmpcur_777);
            CursorTy pvrtmp_779 = tmp_struct_3.field0;
            CursorTy pvrtmp_780 = tmp_struct_3.field1;
            CursorTy pvrtmp_781 = tmp_struct_3.field2;
            CursorTy pvrtmp_782 = tmp_struct_3.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_779, pvrtmp_780,
                                                   pvrtmp_781, pvrtmp_782};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_733");
            exit(1);
        }
    }
}
CursorCursorCursorCursorProd _copy_Tree_v_41(CursorTy end_r_338,
                                             CursorTy end_r_339,
                                             CursorTy loc_337,
                                             CursorTy arg_65_141_191)
{
    if (loc_337 + 32 > end_r_339) {
        ChunkTy new_chunk_8 = alloc_chunk(end_r_339);
        CursorTy chunk_start_9 = new_chunk_8.chunk_start;
        CursorTy chunk_end_10 = new_chunk_8.chunk_end;
        
        end_r_339 = chunk_end_10;
        *(TagTyPacked *) loc_337 = 255;
        
        CursorTy redir = loc_337 + 1;
        
        *(CursorTy *) redir = chunk_start_9;
        loc_337 = chunk_start_9;
    }
    
    CursorTy loc_393 = loc_337 + 1;
    CursorTy loc_394 = loc_393 + 8;
    TagTyPacked tmpval_790 = *(TagTyPacked *) arg_65_141_191;
    CursorTy tmpcur_791 = arg_65_141_191 + 1;
    
    
  switch_846:
    ;
    switch (tmpval_790) {
        
      case 0:
        {
            IntTy tmpval_792 = *(IntTy *) tmpcur_791;
            CursorTy tmpcur_793 = tmpcur_791 + sizeof(IntTy);
            BoolTy tmpval_794 = *(BoolTy *) tmpcur_793;
            CursorTy tmpcur_795 = tmpcur_793 + sizeof(BoolTy);
            CursorTy jump_457 = tmpcur_793 + 1;
            CursorTy jump_456 = tmpcur_791 + 8;
            
            *(TagTyPacked *) loc_337 = 0;
            
            CursorTy writetag_552 = loc_337 + 1;
            
            *(IntTy *) writetag_552 = tmpval_792;
            
            CursorTy writecur_553 = writetag_552 + sizeof(IntTy);
            
            *(BoolTy *) writecur_553 = tmpval_794;
            
            CursorTy writecur_554 = writecur_553 + sizeof(BoolTy);
            
            return (CursorCursorCursorCursorProd) {end_r_339, jump_457, loc_337,
                                                   writecur_554};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_800 = *(IntTy *) tmpcur_791;
            CursorTy tmpcur_801 = tmpcur_791 + sizeof(IntTy);
            CursorTy jump_459 = tmpcur_791 + 8;
            CursorCursorCursorCursorProd tmp_struct_4 =
                                          _copy_Tree_v_41(end_r_338, end_r_339, loc_394, tmpcur_801);
            CursorTy pvrtmp_802 = tmp_struct_4.field0;
            CursorTy pvrtmp_803 = tmp_struct_4.field1;
            CursorTy pvrtmp_804 = tmp_struct_4.field2;
            CursorTy pvrtmp_805 = tmp_struct_4.field3;
            CursorCursorCursorCursorProd tmp_struct_5 =
                                          _copy_Tree_v_41(end_r_338, pvrtmp_802, pvrtmp_805, pvrtmp_803);
            CursorTy pvrtmp_810 = tmp_struct_5.field0;
            CursorTy pvrtmp_811 = tmp_struct_5.field1;
            CursorTy pvrtmp_812 = tmp_struct_5.field2;
            CursorTy pvrtmp_813 = tmp_struct_5.field3;
            
            *(TagTyPacked *) loc_337 = 1;
            
            CursorTy writetag_560 = loc_337 + 1;
            
            *(IntTy *) writetag_560 = tmpval_800;
            
            CursorTy writecur_561 = writetag_560 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_810, pvrtmp_811,
                                                   loc_337, pvrtmp_813};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_822 = *(CursorTy *) tmpcur_791;
            CursorTy tmpaftercur_823 = tmpcur_791 + 8;
            CursorTy jump_493 = tmpcur_791 + 8;
            CursorCursorCursorCursorProd tmp_struct_6 =
                                          _copy_Tree_v_41(end_r_338, end_r_339, loc_337, tmpcur_822);
            CursorTy pvrtmp_824 = tmp_struct_6.field0;
            CursorTy pvrtmp_825 = tmp_struct_6.field1;
            CursorTy pvrtmp_826 = tmp_struct_6.field2;
            CursorTy pvrtmp_827 = tmp_struct_6.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_824, jump_493,
                                                   pvrtmp_826, pvrtmp_827};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_834 = *(CursorTy *) tmpcur_791;
            CursorTy tmpaftercur_835 = tmpcur_791 + 8;
            CursorCursorCursorCursorProd tmp_struct_7 =
                                          _copy_Tree_v_41(end_r_338, end_r_339, loc_337, tmpcur_834);
            CursorTy pvrtmp_836 = tmp_struct_7.field0;
            CursorTy pvrtmp_837 = tmp_struct_7.field1;
            CursorTy pvrtmp_838 = tmp_struct_7.field2;
            CursorTy pvrtmp_839 = tmp_struct_7.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_836, pvrtmp_837,
                                                   pvrtmp_838, pvrtmp_839};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_790");
            exit(1);
        }
    }
}
CursorProd _traverse_Tree_v_41(CursorTy end_r_341, CursorTy arg_87_152_202)
{
    TagTyPacked tmpval_847 = *(TagTyPacked *) arg_87_152_202;
    CursorTy tmpcur_848 = arg_87_152_202 + 1;
    
    
  switch_863:
    ;
    switch (tmpval_847) {
        
      case 0:
        {
            IntTy tmpval_849 = *(IntTy *) tmpcur_848;
            CursorTy tmpcur_850 = tmpcur_848 + sizeof(IntTy);
            BoolTy tmpval_851 = *(BoolTy *) tmpcur_850;
            CursorTy tmpcur_852 = tmpcur_850 + sizeof(BoolTy);
            CursorTy jump_464 = tmpcur_850 + 1;
            CursorTy jump_463 = tmpcur_848 + 8;
            
            return (CursorProd) {jump_464};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_853 = *(IntTy *) tmpcur_848;
            CursorTy tmpcur_854 = tmpcur_848 + sizeof(IntTy);
            CursorTy jump_466 = tmpcur_848 + 8;
            CursorProd tmp_struct_11 =
                        _traverse_Tree_v_41(end_r_341, tmpcur_854);
            CursorTy pvrtmp_855 = tmp_struct_11.field0;
            CursorProd tmp_struct_12 =
                        _traverse_Tree_v_41(end_r_341, pvrtmp_855);
            CursorTy pvrtmp_856 = tmp_struct_12.field0;
            
            return (CursorProd) {pvrtmp_856};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_857 = *(CursorTy *) tmpcur_848;
            CursorTy tmpaftercur_858 = tmpcur_848 + 8;
            CursorTy jump_499 = tmpcur_848 + 8;
            CursorProd tmp_struct_13 =
                        _traverse_Tree_v_41(end_r_341, tmpcur_857);
            CursorTy pvrtmp_859 = tmp_struct_13.field0;
            
            return (CursorProd) {jump_499};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_860 = *(CursorTy *) tmpcur_848;
            CursorTy tmpaftercur_861 = tmpcur_848 + 8;
            CursorProd tmp_struct_14 =
                        _traverse_Tree_v_41(end_r_341, tmpcur_860);
            CursorTy pvrtmp_862 = tmp_struct_14.field0;
            
            return (CursorProd) {pvrtmp_862};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_847");
            exit(1);
        }
    }
}
CursorProd _print_Tree_v_41(CursorTy end_r_343, CursorTy arg_98_160_210)
{
    TagTyPacked tmpval_864 = *(TagTyPacked *) arg_98_160_210;
    CursorTy tmpcur_865 = arg_98_160_210 + 1;
    
    
  switch_880:
    ;
    switch (tmpval_864) {
        
      case 0:
        {
            IntTy tmpval_866 = *(IntTy *) tmpcur_865;
            CursorTy tmpcur_867 = tmpcur_865 + sizeof(IntTy);
            BoolTy tmpval_868 = *(BoolTy *) tmpcur_867;
            CursorTy tmpcur_869 = tmpcur_867 + sizeof(BoolTy);
            CursorTy jump_471 = tmpcur_867 + 1;
            CursorTy jump_470 = tmpcur_865 + 8;
            unsigned char wildcard_103_163_213 = print_symbol(729);
            unsigned char wildcard_106_164_214 = print_symbol(732);
            unsigned char y_101_165_215 = printf("%lld", tmpval_866);
            unsigned char wildcard_105_166_216 = print_symbol(732);
            unsigned char y_102_167_217 = printf("%d", tmpval_868);
            unsigned char wildcard_104_168_218 = print_symbol(727);
            
            return (CursorProd) {jump_471};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_870 = *(IntTy *) tmpcur_865;
            CursorTy tmpcur_871 = tmpcur_865 + sizeof(IntTy);
            CursorTy jump_473 = tmpcur_865 + 8;
            unsigned char wildcard_113_172_222 = print_symbol(728);
            unsigned char wildcard_117_173_223 = print_symbol(732);
            unsigned char y_110_174_224 = printf("%lld", tmpval_870);
            unsigned char wildcard_116_175_225 = print_symbol(732);
            CursorProd tmp_struct_15 =  _print_Tree_v_41(end_r_343, tmpcur_871);
            CursorTy pvrtmp_872 = tmp_struct_15.field0;
            unsigned char wildcard_115_177_227 = print_symbol(732);
            CursorProd tmp_struct_16 =  _print_Tree_v_41(end_r_343, pvrtmp_872);
            CursorTy pvrtmp_873 = tmp_struct_16.field0;
            unsigned char wildcard_114_179_229 = print_symbol(727);
            
            return (CursorProd) {pvrtmp_873};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_874 = *(CursorTy *) tmpcur_865;
            CursorTy tmpaftercur_875 = tmpcur_865 + 8;
            CursorTy jump_505 = tmpcur_865 + 8;
            unsigned char wildcard_508 = print_symbol(731);
            CursorProd tmp_struct_17 =  _print_Tree_v_41(end_r_343, tmpcur_874);
            CursorTy pvrtmp_876 = tmp_struct_17.field0;
            
            return (CursorProd) {jump_505};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_877 = *(CursorTy *) tmpcur_865;
            CursorTy tmpaftercur_878 = tmpcur_865 + 8;
            unsigned char wildcard_508 = print_symbol(730);
            CursorProd tmp_struct_18 =  _print_Tree_v_41(end_r_343, tmpcur_877);
            CursorTy pvrtmp_879 = tmp_struct_18.field0;
            
            return (CursorProd) {pvrtmp_879};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_864");
            exit(1);
        }
    }
}
CursorCursorCursorCursorProd _add_size_and_rel_offsets_Tree_v_41(CursorTy end_r_346,
                                                                 CursorTy end_r_347,
                                                                 CursorTy loc_345,
                                                                 CursorTy arg_319)
{
    if (loc_345 + 32 > end_r_347) {
        ChunkTy new_chunk_23 = alloc_chunk(end_r_347);
        CursorTy chunk_start_24 = new_chunk_23.chunk_start;
        CursorTy chunk_end_25 = new_chunk_23.chunk_end;
        
        end_r_347 = chunk_end_25;
        *(TagTyPacked *) loc_345 = 255;
        
        CursorTy redir = loc_345 + 1;
        
        *(CursorTy *) redir = chunk_start_24;
        loc_345 = chunk_start_24;
    }
    
    CursorTy loc_441 = loc_345 + 1;
    CursorTy loc_442 = loc_441 + 8;
    TagTyPacked tmpval_881 = *(TagTyPacked *) arg_319;
    CursorTy tmpcur_882 = arg_319 + 1;
    
    
  switch_937:
    ;
    switch (tmpval_881) {
        
      case 0:
        {
            IntTy tmpval_883 = *(IntTy *) tmpcur_882;
            CursorTy tmpcur_884 = tmpcur_882 + sizeof(IntTy);
            BoolTy tmpval_885 = *(BoolTy *) tmpcur_884;
            CursorTy tmpcur_886 = tmpcur_884 + sizeof(BoolTy);
            CursorTy jump_478 = tmpcur_884 + 1;
            CursorTy jump_477 = tmpcur_882 + 8;
            
            *(TagTyPacked *) loc_345 = 0;
            
            CursorTy writetag_600 = loc_345 + 1;
            
            *(IntTy *) writetag_600 = tmpval_883;
            
            CursorTy writecur_601 = writetag_600 + sizeof(IntTy);
            
            *(BoolTy *) writecur_601 = tmpval_885;
            
            CursorTy writecur_602 = writecur_601 + sizeof(BoolTy);
            
            return (CursorCursorCursorCursorProd) {end_r_347, jump_478, loc_345,
                                                   writecur_602};
            break;
        }
        
      case 1:
        {
            IntTy tmpval_891 = *(IntTy *) tmpcur_882;
            CursorTy tmpcur_892 = tmpcur_882 + sizeof(IntTy);
            CursorTy jump_480 = tmpcur_882 + 8;
            CursorCursorCursorCursorProd tmp_struct_19 =
                                          _add_size_and_rel_offsets_Tree_v_41(end_r_346, end_r_347, loc_442, tmpcur_892);
            CursorTy pvrtmp_893 = tmp_struct_19.field0;
            CursorTy pvrtmp_894 = tmp_struct_19.field1;
            CursorTy pvrtmp_895 = tmp_struct_19.field2;
            CursorTy pvrtmp_896 = tmp_struct_19.field3;
            CursorCursorCursorCursorProd tmp_struct_20 =
                                          _add_size_and_rel_offsets_Tree_v_41(end_r_346, pvrtmp_893, pvrtmp_896, pvrtmp_894);
            CursorTy pvrtmp_901 = tmp_struct_20.field0;
            CursorTy pvrtmp_902 = tmp_struct_20.field1;
            CursorTy pvrtmp_903 = tmp_struct_20.field2;
            CursorTy pvrtmp_904 = tmp_struct_20.field3;
            
            *(TagTyPacked *) loc_345 = 1;
            
            CursorTy writetag_608 = loc_345 + 1;
            
            *(IntTy *) writetag_608 = tmpval_891;
            
            CursorTy writecur_609 = writetag_608 + sizeof(IntTy);
            
            return (CursorCursorCursorCursorProd) {pvrtmp_901, pvrtmp_902,
                                                   loc_345, pvrtmp_904};
            break;
        }
        
      case INDIRECTION_TAG:
        {
            CursorTy tmpcur_913 = *(CursorTy *) tmpcur_882;
            CursorTy tmpaftercur_914 = tmpcur_882 + 8;
            CursorTy jump_511 = tmpcur_882 + 8;
            CursorCursorCursorCursorProd tmp_struct_21 =
                                          _add_size_and_rel_offsets_Tree_v_41(end_r_346, end_r_347, loc_345, tmpcur_913);
            CursorTy pvrtmp_915 = tmp_struct_21.field0;
            CursorTy pvrtmp_916 = tmp_struct_21.field1;
            CursorTy pvrtmp_917 = tmp_struct_21.field2;
            CursorTy pvrtmp_918 = tmp_struct_21.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_915, jump_511,
                                                   pvrtmp_917, pvrtmp_918};
            break;
        }
        
      case REDIRECTION_TAG:
        {
            CursorTy tmpcur_925 = *(CursorTy *) tmpcur_882;
            CursorTy tmpaftercur_926 = tmpcur_882 + 8;
            CursorCursorCursorCursorProd tmp_struct_22 =
                                          _add_size_and_rel_offsets_Tree_v_41(end_r_346, end_r_347, loc_345, tmpcur_925);
            CursorTy pvrtmp_927 = tmp_struct_22.field0;
            CursorTy pvrtmp_928 = tmp_struct_22.field1;
            CursorTy pvrtmp_929 = tmp_struct_22.field2;
            CursorTy pvrtmp_930 = tmp_struct_22.field3;
            
            return (CursorCursorCursorCursorProd) {pvrtmp_927, pvrtmp_928,
                                                   pvrtmp_929, pvrtmp_930};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_881");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(727, ")");
    add_symbol(728, "(Node_v_41");
    add_symbol(729, "(Leaf_v_41");
    add_symbol(730, " ->r ");
    add_symbol(731, " ->i ");
    add_symbol(732, " ");
}