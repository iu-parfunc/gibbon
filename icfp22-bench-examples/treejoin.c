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
// Bump allocated nursery for regions
// -------------------------------------

// See https://github.com/iu-parfunc/gibbon/issues/122.

__thread char* nursery_heap_ptr = (char*)NULL;
__thread char* nursery_heap_ptr_end = (char*)NULL;

#define NURSERY_SIZE 0
// #define NURSERY_SIZE global_init_biginf_buf_size
#define NURSERY_ALLOC_UPPER_BOUND 1024

static inline void init_nursery() {
    nursery_heap_ptr = (char*)malloc(NURSERY_SIZE);
    if (nursery_heap_ptr == NULL) {
      printf("init_region: malloc failed: %d", NURSERY_SIZE);
      exit(1);
    }
    nursery_heap_ptr_end = nursery_heap_ptr + NURSERY_SIZE;
#ifdef _DEBUG
    printf("init_nursery: DONE, heap_ptr = %p\n", nursery_heap_ptr);
#endif
}

static inline void* alloc_in_nursery(long long n) {
    if (! nursery_heap_ptr) {
        init_nursery();
    }
    if (nursery_heap_ptr + n < nursery_heap_ptr_end) {
        char* old = nursery_heap_ptr;
        nursery_heap_ptr += n;
#ifdef _DEBUG
        printf("alloc_in_nursery: DONE, %lld\n", n);
#endif
        return old;
    } else {
        return NULL;
    }
}

// -------------------------------------
// ALLOC and ALLOC_PACKED macros
// -------------------------------------


/*

If parallelism is enabled, we always use a nursery/malloc based allocator
since Boehm GC is not thread-safe in its default configuration. It can be
made thread-safe by building it with appropriate flags, but we don't do that.
Presently, all parallel pointer-based programs will leak memory.

*/

#ifdef _PARALLEL
#define ALLOC(n) malloc(n)
#define ALLOC_PACKED_SMALL(n) alloc_in_nursery(n)
#define ALLOC_PACKED_BIG(n) malloc(n)
char *ALLOC_COUNTED(size_t size) {
    bump_global_region_count();
    return ALLOC(size);
}
#else
  #ifdef _POINTER
#define ALLOC(n) GC_MALLOC(n)
#define ALLOC_PACKED_SMALL(n) GC_MALLOC(n)
#define ALLOC_PACKED_BIG(n) GC_MALLOC(n)
char *ALLOC_COUNTED(size_t size) {
    bump_global_region_count();
    return GC_MALLOC(size);
}
  #else
#define ALLOC(n) malloc(n)
#define ALLOC_PACKED_SMALL(n) alloc_in_nursery(n)
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
   serialized data | rf_reg_metadata_ptr | rf_seq_no | rf_nursery_allocated | rf_size | rf_next | rf_prev
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

   - rf_nursery_allocated: Whether this chunk was allocated in a nursery.

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
    bool rf_nursery_allocated;
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
    CursorTy heap;
    bool nursery_allocated = true;
    if (size <= NURSERY_ALLOC_UPPER_BOUND) {
        heap = ALLOC_PACKED_SMALL(total_size);
        if (heap == NULL) {
            heap = malloc(total_size);
            nursery_allocated = false;
        }
    } else {
        heap = ALLOC_PACKED_BIG(total_size);
        nursery_allocated = false;
    }
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
    printf("Allocated a region(%lld): %lld bytes, nursery=%d.\n", reg->reg_id, size, nursery_allocated);
#endif

    // Write the footer.
    RegionFooter *footer = (RegionFooter *) heap_end;
    footer->rf_reg_metadata_ptr = reg;
    footer->rf_seq_no = 1;
    footer->rf_nursery_allocated = nursery_allocated;
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
    new_footer->rf_nursery_allocated = false;
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
        printf("free_region(%lld): first chunk in nursery: %d\n",
               reg->reg_id,
               first_chunk_footer->rf_nursery_allocated);
#endif

        if (! first_chunk_footer->rf_nursery_allocated) {
            #ifdef _DEBUG
            num_freed_chunks++;
            total_bytesize = total_bytesize + first_chunk_footer->rf_size;
            #endif
            free(first_chunk);
        }

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
typedef struct Int64Int64Int64Prod_struct {
            IntTy field0;
            IntTy field1;
            IntTy field2;
        } Int64Int64Int64Prod;
typedef struct Int64Int64Int64Int64Int64Prod_struct {
            IntTy field0;
            IntTy field1;
            IntTy field2;
            IntTy field3;
            IntTy field4;
        } Int64Int64Int64Int64Int64Prod;
typedef struct Int64Int64PtrPtrProd_struct {
            IntTy field0;
            IntTy field1;
            PtrTy field2;
            PtrTy field3;
        } Int64Int64PtrPtrProd;
typedef struct Int64Int64ProdInt64Int64Int64Prod_struct {
            IntTy field0;
            IntTy field1;
            Int64Int64Int64Prod field2;
        } Int64Int64ProdInt64Int64Int64Prod;
typedef struct Int64Int64ProdInt64Int64Int64Int64Int64Prod_struct {
            IntTy field0;
            IntTy field1;
            Int64Int64Int64Int64Int64Prod field2;
        } Int64Int64ProdInt64Int64Int64Int64Int64Prod;
typedef struct Int64PtrProd_struct {
            IntTy field0;
            PtrTy field1;
        } Int64PtrProd;
typedef struct Int64PtrPtrProd_struct {
            IntTy field0;
            PtrTy field1;
            PtrTy field2;
        } Int64PtrPtrProd;
typedef struct Int64ProdInt64Int64Int64Prod_struct {
            IntTy field0;
            Int64Int64Int64Prod field1;
        } Int64ProdInt64Int64Int64Prod;
typedef struct Int64ProdInt64Int64Int64Int64Int64Prod_struct {
            IntTy field0;
            Int64Int64Int64Int64Int64Prod field1;
        } Int64ProdInt64Int64Int64Int64Int64Prod;
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
typedef struct ProdInt64Int64Int64Prod_struct {
            Int64Int64Int64Prod field0;
        } ProdInt64Int64Int64Prod;
typedef struct VectorProd_struct {
            VectorTy *field0;
        } VectorProd;
unsigned char bench_main();
CursorTy join(CursorTy t1_119_969_1247, CursorTy t2_120_970_1248,
              CursorTy j_121_971_1249);
IntTy maxInt(IntTy a_392_996_1277, IntTy b_393_997_1278);
IntTy minInt(IntTy a_394_998_1280, IntTy b_395_999_1281);
CursorTy lookupT_607(IntTy k_152_1028_1288, CursorTy n_153_1029_1289);
CursorTy insertT_609(IntTy k_96_1035_1297,
                     Int64Int64Int64Int64Int64Prod e_97_1036_1298,
                     CursorTy t_98_1037_1299);
CursorTy insertT_612(IntTy k_96_1043_1314, Int64Int64Int64Prod e_97_1044_1315,
                     CursorTy t_98_1045_1316);
CursorTy mkTree_810(VectorTy *pts_147_1055_1331, CursorTy t_148_1056_1332);
CursorTy _copy_Tree_v_606(CursorTy arg_840_1060_1348);
CursorTy _copy_without_ptrs_Tree_v_606(CursorTy arg_851_1071_1359);
unsigned char _traverse_Tree_v_606(CursorTy arg_862_1082_1370);
unsigned char _print_Tree_v_606(CursorTy arg_873_1090_1378);
CursorTy _copy_Tree_v_605(CursorTy arg_892_1108_1396);
CursorTy _copy_without_ptrs_Tree_v_605(CursorTy arg_903_1119_1407);
unsigned char _traverse_Tree_v_605(CursorTy arg_914_1130_1418);
unsigned char _print_Tree_v_605(CursorTy arg_925_1138_1426);
CursorTy _copy_Maybe_v_608(CursorTy arg_944_1156_1444);
CursorTy _copy_without_ptrs_Maybe_v_608(CursorTy arg_947_1159_1447);
unsigned char _traverse_Maybe_v_608(CursorTy arg_950_1162_1450);
unsigned char _print_Maybe_v_608(CursorTy arg_953_1164_1452);
unsigned char bench_main()
{
    VectorTy *f_105_960_1225 = vector_alloc(read_arrayfile_length_param(),
                                            sizeof(Int64Int64Int64Prod));
    Int64Int64Int64Prod arr_elem_0;
    
    FILE * fp_1;
    
    char *line_2 = NULL;
    
    size_t(len_3);
    len_3 = 0;
    ssize_t(read_4);
    fp_1 = fopen(read_arrayfile_param(), "r");
    if (fp_1 == NULL) {
        fprintf(stderr, "fopen failed\n");
        abort();
    }
    
    IntTy tmp_6;
    IntTy tmp_7;
    IntTy tmp_8;
    IntTy i_5 = 0;
    
    while ((read_4 = getline(&line_2, &len_3, fp_1)) != -1) {
        int xxxx = sscanf(line_2, "%lld %lld %lld", &tmp_6, &tmp_7, &tmp_8);
        
        arr_elem_0.field0 = tmp_6;
        arr_elem_0.field1 = tmp_7;
        arr_elem_0.field2 = tmp_8;
        vector_inplace_update(f_105_960_1225, i_5, &arr_elem_0);
        i_5++;
    }
    
    IntTy len_106_961_1227 = vector_length(f_105_960_1225);
    IntTy half_107_962_1228 = len_106_961_1227 / 2;
    IntTy len_169_1021_1173_1231 = vector_length(f_105_960_1225);
    IntTy n__170_1022_1174_1232 =  maxInt(half_107_962_1228, 0);
    IntTy m_171_1023_1175_1233 =
           minInt(n__170_1022_1174_1232, len_169_1021_1173_1231);
    IntTy fltAppE_1189_1234 = len_169_1021_1173_1231 - n__170_1022_1174_1232;
    IntTy m__172_1024_1176_1235 =  maxInt(0, fltAppE_1189_1234);
    VectorTy *fltPrd_1190_1236 = vector_slice(0, m_171_1023_1175_1233,
                                              f_105_960_1225);
    VectorTy *fltPrd_1191_1237 = vector_slice(m_171_1023_1175_1233,
                                              m__172_1024_1176_1235,
                                              f_105_960_1225);
    VectorTy *pvrtmp_1476 = (VectorTy *) fltPrd_1191_1237;
    VectorTy *pvrtmp_1475 = (VectorTy *) fltPrd_1190_1236;
    VectorTy *f1_109_964_1239 = (VectorTy *) pvrtmp_1475;
    VectorTy *f2_110_965_1240 = (VectorTy *) pvrtmp_1476;
    PtrTy fltAppE_1192_1241 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltAppE_1192_1241)->field0 = 2;
    
    CursorTy t1_116_966_1242 =  mkTree_810(f1_109_964_1239, fltAppE_1192_1241);
    PtrTy fltAppE_1193_1243 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltAppE_1193_1243)->field0 = 2;
    
    CursorTy t2_117_967_1244 =  mkTree_810(f2_110_965_1240, fltAppE_1193_1243);
    PtrTy fltAppE_1194_1245 = ALLOC(sizeof(Int64Prod));
    
    ((Int64Prod *) fltAppE_1194_1245)->field0 = 2;
    
    CursorTy wildcard__77_118_968_1246 =
              join(t1_116_966_1242, t2_117_967_1244, fltAppE_1194_1245);
    
    return 0;
}
CursorTy join(CursorTy t1_119_969_1247, CursorTy t2_120_970_1248,
              CursorTy j_121_971_1249)
{
    TagTyPacked tag_1477 = *(TagTyPacked *) t1_119_969_1247;
    CursorTy tail_1478 = t1_119_969_1247 + sizeof(IntTy);
    
    
  switch_1500:
    ;
    switch (tag_1477) {
        
      case 3:
        {
            PtrTy tailift_1479 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1479)->field0 = 3;
            return tailift_1479;
            break;
        }
        
      case 2:
        {
            return j_121_971_1249;
            break;
        }
        
      case 1:
        {
            IntTy k_122_972_1250 =
                  ((Int64ProdInt64Int64Int64Prod *) tail_1478)->field0;
            Int64Int64Int64Prod v_123_973_1251 =
                                ((Int64ProdInt64Int64Int64Prod *) tail_1478)->field1;
            IntTy a_125_975_1253 = (IntTy) v_123_973_1251.field0;
            IntTy b_126_976_1254 = (IntTy) v_123_973_1251.field1;
            IntTy c_127_977_1255 = (IntTy) v_123_973_1251.field2;
            CursorTy t2__128_978_1256 =
                      lookupT_607(c_127_977_1255, t2_120_970_1248);
            TagTyPacked tag_1480 = *(TagTyPacked *) t2__128_978_1256;
            CursorTy tail_1481 = t2__128_978_1256 + sizeof(IntTy);
            
            
          switch_1495:
            ;
            switch (tag_1480) {
                
              case 0:
                {
                    CursorTy res_134_984_1263 = (CursorTy) j_121_971_1249;
                    TagTyPacked tag_1482 = *(TagTyPacked *) t2_120_970_1248;
                    CursorTy tail_1483 = t2_120_970_1248 + sizeof(IntTy);
                    
                    
                  switch_1485:
                    ;
                    switch (tag_1482) {
                        
                      case 2:
                        {
                            return j_121_971_1249;
                            break;
                        }
                        
                      case 0:
                        {
                            IntTy wildcard__40_135_985_1264 =
                                  ((Int64PtrProd *) tail_1483)->field0;
                            PtrTy wildcard__41_136_986_1265 =
                                  ((Int64PtrProd *) tail_1483)->field1;
                            
                            return res_134_984_1263;
                            break;
                        }
                        
                      case 1:
                        {
                            IntTy wildcard__44_137_987_1266 =
                                  ((Int64ProdInt64Int64Int64Prod *) tail_1483)->field0;
                            Int64Int64Int64Prod wildcard__45_138_988_1267 =
                                                ((Int64ProdInt64Int64Int64Prod *) tail_1483)->field1;
                            
                            return res_134_984_1263;
                            break;
                        }
                        
                      case 3:
                        {
                            PtrTy tailift_1484 = ALLOC(sizeof(Int64Prod));
                            
                            ((Int64Prod *) tailift_1484)->field0 = 3;
                            return tailift_1484;
                            break;
                        }
                        
                      default:
                        {
                            printf("%s\n", "Unknown tag in: tag_1482");
                            exit(1);
                        }
                    }
                    break;
                }
                
              case 1:
                {
                    Int64Int64Int64Prod w_129_979_1257 =
                                        ((ProdInt64Int64Int64Prod *) tail_1481)->field0;
                    IntTy d_131_981_1259 = (IntTy) w_129_979_1257.field0;
                    IntTy e_132_982_1260 = (IntTy) w_129_979_1257.field1;
                    IntTy f_133_983_1261 = (IntTy) w_129_979_1257.field2;
                    IntTy pvrtmp_1490 = (IntTy) e_132_982_1260;
                    IntTy pvrtmp_1489 = (IntTy) d_131_981_1259;
                    IntTy pvrtmp_1488 = (IntTy) c_127_977_1255;
                    IntTy pvrtmp_1487 = (IntTy) b_126_976_1254;
                    IntTy pvrtmp_1486 = (IntTy) a_125_975_1253;
                    CursorTy res_134_984_1263 =
                              insertT_609(c_127_977_1255, (Int64Int64Int64Int64Int64Prod) {pvrtmp_1486, pvrtmp_1487, pvrtmp_1488, pvrtmp_1489, pvrtmp_1490}, j_121_971_1249);
                    TagTyPacked tag_1491 = *(TagTyPacked *) t2_120_970_1248;
                    CursorTy tail_1492 = t2_120_970_1248 + sizeof(IntTy);
                    
                    
                  switch_1494:
                    ;
                    switch (tag_1491) {
                        
                      case 2:
                        {
                            return j_121_971_1249;
                            break;
                        }
                        
                      case 0:
                        {
                            IntTy wildcard__40_135_985_1264 =
                                  ((Int64PtrProd *) tail_1492)->field0;
                            PtrTy wildcard__41_136_986_1265 =
                                  ((Int64PtrProd *) tail_1492)->field1;
                            
                            return res_134_984_1263;
                            break;
                        }
                        
                      case 1:
                        {
                            IntTy wildcard__44_137_987_1266 =
                                  ((Int64ProdInt64Int64Int64Prod *) tail_1492)->field0;
                            Int64Int64Int64Prod wildcard__45_138_988_1267 =
                                                ((Int64ProdInt64Int64Int64Prod *) tail_1492)->field1;
                            
                            return res_134_984_1263;
                            break;
                        }
                        
                      case 3:
                        {
                            PtrTy tailift_1493 = ALLOC(sizeof(Int64Prod));
                            
                            ((Int64Prod *) tailift_1493)->field0 = 3;
                            return tailift_1493;
                            break;
                        }
                        
                      default:
                        {
                            printf("%s\n", "Unknown tag in: tag_1491");
                            exit(1);
                        }
                    }
                    break;
                }
                
              default:
                {
                    printf("%s\n", "Unknown tag in: tag_1480");
                    exit(1);
                }
            }
            break;
        }
        
      case 0:
        {
            IntTy k_139_989_1268 = ((Int64PtrPtrProd *) tail_1478)->field0;
            PtrTy l_140_990_1269 = ((Int64PtrPtrProd *) tail_1478)->field1;
            PtrTy r_141_991_1270 = ((Int64PtrPtrProd *) tail_1478)->field2;
            TagTyPacked tag_1496 = *(TagTyPacked *) t2_120_970_1248;
            CursorTy tail_1497 = t2_120_970_1248 + sizeof(IntTy);
            
            
          switch_1499:
            ;
            switch (tag_1496) {
                
              case 2:
                {
                    return j_121_971_1249;
                    break;
                }
                
              case 0:
                {
                    IntTy wildcard__64_142_992_1271 =
                          ((Int64PtrProd *) tail_1497)->field0;
                    PtrTy wildcard__65_143_993_1272 =
                          ((Int64PtrProd *) tail_1497)->field1;
                    CursorTy fltAppE_1196_1273 =
                              join(r_141_991_1270, t2_120_970_1248, j_121_971_1249);
                    
                    return join(l_140_990_1269, t2_120_970_1248,
                                fltAppE_1196_1273);
                    break;
                }
                
              case 1:
                {
                    IntTy wildcard__68_144_994_1274 =
                          ((Int64ProdInt64Int64Int64Prod *) tail_1497)->field0;
                    Int64Int64Int64Prod wildcard__69_145_995_1275 =
                                        ((Int64ProdInt64Int64Int64Prod *) tail_1497)->field1;
                    CursorTy fltAppE_1197_1276 =
                              join(r_141_991_1270, t2_120_970_1248, j_121_971_1249);
                    
                    return join(l_140_990_1269, t2_120_970_1248,
                                fltAppE_1197_1276);
                    break;
                }
                
              case 3:
                {
                    PtrTy tailift_1498 = ALLOC(sizeof(Int64Prod));
                    
                    ((Int64Prod *) tailift_1498)->field0 = 3;
                    return tailift_1498;
                    break;
                }
                
              default:
                {
                    printf("%s\n", "Unknown tag in: tag_1496");
                    exit(1);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1477");
            exit(1);
        }
    }
}
IntTy maxInt(IntTy a_392_996_1277, IntTy b_393_997_1278)
{
    BoolTy fltIf_1198_1279 = a_392_996_1277 > b_393_997_1278;
    
    if (fltIf_1198_1279) {
        return a_392_996_1277;
    } else {
        return b_393_997_1278;
    }
}
IntTy minInt(IntTy a_394_998_1280, IntTy b_395_999_1281)
{
    BoolTy fltIf_1199_1282 = a_394_998_1280 < b_395_999_1281;
    
    if (fltIf_1199_1282) {
        return a_394_998_1280;
    } else {
        return b_395_999_1281;
    }
}
CursorTy lookupT_607(IntTy k_152_1028_1288, CursorTy n_153_1029_1289)
{
    TagTyPacked tag_1501 = *(TagTyPacked *) n_153_1029_1289;
    CursorTy tail_1502 = n_153_1029_1289 + sizeof(IntTy);
    
    
  switch_1507:
    ;
    switch (tag_1501) {
        
      case 0:
        {
            IntTy k__155_1030_1290 = ((Int64PtrPtrProd *) tail_1502)->field0;
            PtrTy l_156_1031_1291 = ((Int64PtrPtrProd *) tail_1502)->field1;
            PtrTy r_157_1032_1292 = ((Int64PtrPtrProd *) tail_1502)->field2;
            BoolTy fltIf_1200_1293 = k_152_1028_1288 <= k__155_1030_1290;
            
            if (fltIf_1200_1293) {
                return lookupT_607(k_152_1028_1288, l_156_1031_1291);
            } else {
                return lookupT_607(k_152_1028_1288, r_157_1032_1292);
            }
            break;
        }
        
      case 1:
        {
            IntTy k__158_1033_1294 =
                  ((Int64ProdInt64Int64Int64Prod *) tail_1502)->field0;
            Int64Int64Int64Prod e_159_1034_1295 =
                                ((Int64ProdInt64Int64Int64Prod *) tail_1502)->field1;
            BoolTy fltIf_1201_1296 = k_152_1028_1288 == k__158_1033_1294;
            
            if (fltIf_1201_1296) {
                PtrTy tailift_1503 =
                      ALLOC(sizeof(Int64ProdInt64Int64Int64Prod));
                
                ((Int64ProdInt64Int64Int64Prod *) tailift_1503)->field0 = 1;
                ((Int64ProdInt64Int64Int64Prod *) tailift_1503)->field1 =
                    e_159_1034_1295;
                return tailift_1503;
            } else {
                PtrTy tailift_1504 = ALLOC(sizeof(Int64Prod));
                
                ((Int64Prod *) tailift_1504)->field0 = 0;
                return tailift_1504;
            }
            break;
        }
        
      case 2:
        {
            PtrTy tailift_1505 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1505)->field0 = 0;
            return tailift_1505;
            break;
        }
        
      case 3:
        {
            PtrTy tailift_1506 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1506)->field0 = 0;
            return tailift_1506;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1501");
            exit(1);
        }
    }
}
CursorTy insertT_609(IntTy k_96_1035_1297,
                     Int64Int64Int64Int64Int64Prod e_97_1036_1298,
                     CursorTy t_98_1037_1299)
{
    TagTyPacked tag_1508 = *(TagTyPacked *) t_98_1037_1299;
    CursorTy tail_1509 = t_98_1037_1299 + sizeof(IntTy);
    
    
  switch_1517:
    ;
    switch (tag_1508) {
        
      case 0:
        {
            IntTy k__100_1038_1300 = ((Int64PtrPtrProd *) tail_1509)->field0;
            PtrTy l_101_1039_1301 = ((Int64PtrPtrProd *) tail_1509)->field1;
            PtrTy r_102_1040_1302 = ((Int64PtrPtrProd *) tail_1509)->field2;
            BoolTy fltIf_1202_1303 = k_96_1035_1297 <= k__100_1038_1300;
            
            if (fltIf_1202_1303) {
                CursorTy fltPkd_1203_1304 =
                          insertT_609(k_96_1035_1297, e_97_1036_1298, l_101_1039_1301);
                PtrTy tailift_1510 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                
                ((Int64Int64PtrPtrProd *) tailift_1510)->field0 = 0;
                ((Int64Int64PtrPtrProd *) tailift_1510)->field1 =
                    k__100_1038_1300;
                ((Int64Int64PtrPtrProd *) tailift_1510)->field2 =
                    fltPkd_1203_1304;
                ((Int64Int64PtrPtrProd *) tailift_1510)->field3 =
                    r_102_1040_1302;
                return tailift_1510;
            } else {
                CursorTy fltPkd_1204_1305 =
                          insertT_609(k_96_1035_1297, e_97_1036_1298, r_102_1040_1302);
                PtrTy tailift_1511 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                
                ((Int64Int64PtrPtrProd *) tailift_1511)->field0 = 0;
                ((Int64Int64PtrPtrProd *) tailift_1511)->field1 =
                    k__100_1038_1300;
                ((Int64Int64PtrPtrProd *) tailift_1511)->field2 =
                    l_101_1039_1301;
                ((Int64Int64PtrPtrProd *) tailift_1511)->field3 =
                    fltPkd_1204_1305;
                return tailift_1511;
            }
            break;
        }
        
      case 1:
        {
            IntTy k__103_1041_1306 =
                  ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1509)->field0;
            Int64Int64Int64Int64Int64Prod v_104_1042_1307 =
                                          ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1509)->field1;
            BoolTy fltIf_1205_1308 = k_96_1035_1297 < k__103_1041_1306;
            
            if (fltIf_1205_1308) {
                PtrTy fltPkd_1206_1309 =
                      ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Int64Int64Prod));
                
                ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1206_1309)->field0 =
                    1;
                ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1206_1309)->field1 =
                    k_96_1035_1297;
                ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1206_1309)->field2 =
                    e_97_1036_1298;
                
                PtrTy fltPkd_1207_1310 =
                      ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Int64Int64Prod));
                
                ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1207_1310)->field0 =
                    1;
                ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1207_1310)->field1 =
                    k__103_1041_1306;
                ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1207_1310)->field2 =
                    v_104_1042_1307;
                
                PtrTy tailift_1512 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                
                ((Int64Int64PtrPtrProd *) tailift_1512)->field0 = 0;
                ((Int64Int64PtrPtrProd *) tailift_1512)->field1 =
                    k_96_1035_1297;
                ((Int64Int64PtrPtrProd *) tailift_1512)->field2 =
                    fltPkd_1206_1309;
                ((Int64Int64PtrPtrProd *) tailift_1512)->field3 =
                    fltPkd_1207_1310;
                return tailift_1512;
            } else {
                BoolTy fltIf_1208_1311 = k_96_1035_1297 > k__103_1041_1306;
                
                if (fltIf_1208_1311) {
                    PtrTy fltPkd_1209_1312 =
                          ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Int64Int64Prod));
                    
                    ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1209_1312)->field0 =
                        1;
                    ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1209_1312)->field1 =
                        k__103_1041_1306;
                    ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1209_1312)->field2 =
                        v_104_1042_1307;
                    
                    PtrTy fltPkd_1210_1313 =
                          ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Int64Int64Prod));
                    
                    ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1210_1313)->field0 =
                        1;
                    ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1210_1313)->field1 =
                        k__103_1041_1306;
                    ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) fltPkd_1210_1313)->field2 =
                        e_97_1036_1298;
                    
                    PtrTy tailift_1513 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                    
                    ((Int64Int64PtrPtrProd *) tailift_1513)->field0 = 0;
                    ((Int64Int64PtrPtrProd *) tailift_1513)->field1 =
                        k__103_1041_1306;
                    ((Int64Int64PtrPtrProd *) tailift_1513)->field2 =
                        fltPkd_1209_1312;
                    ((Int64Int64PtrPtrProd *) tailift_1513)->field3 =
                        fltPkd_1210_1313;
                    return tailift_1513;
                } else {
                    PtrTy tailift_1514 = ALLOC(sizeof(Int64Prod));
                    
                    ((Int64Prod *) tailift_1514)->field0 = 3;
                    return tailift_1514;
                }
            }
            break;
        }
        
      case 2:
        {
            PtrTy tailift_1515 =
                  ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Int64Int64Prod));
            
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1515)->field0 =
                1;
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1515)->field1 =
                k_96_1035_1297;
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1515)->field2 =
                e_97_1036_1298;
            return tailift_1515;
            break;
        }
        
      case 3:
        {
            PtrTy tailift_1516 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1516)->field0 = 3;
            return tailift_1516;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1508");
            exit(1);
        }
    }
}
CursorTy insertT_612(IntTy k_96_1043_1314, Int64Int64Int64Prod e_97_1044_1315,
                     CursorTy t_98_1045_1316)
{
    TagTyPacked tag_1518 = *(TagTyPacked *) t_98_1045_1316;
    CursorTy tail_1519 = t_98_1045_1316 + sizeof(IntTy);
    
    
  switch_1527:
    ;
    switch (tag_1518) {
        
      case 0:
        {
            IntTy k__100_1046_1317 = ((Int64PtrPtrProd *) tail_1519)->field0;
            PtrTy l_101_1047_1318 = ((Int64PtrPtrProd *) tail_1519)->field1;
            PtrTy r_102_1048_1319 = ((Int64PtrPtrProd *) tail_1519)->field2;
            BoolTy fltIf_1211_1320 = k_96_1043_1314 <= k__100_1046_1317;
            
            if (fltIf_1211_1320) {
                CursorTy fltPkd_1212_1321 =
                          insertT_612(k_96_1043_1314, e_97_1044_1315, l_101_1047_1318);
                PtrTy tailift_1520 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                
                ((Int64Int64PtrPtrProd *) tailift_1520)->field0 = 0;
                ((Int64Int64PtrPtrProd *) tailift_1520)->field1 =
                    k__100_1046_1317;
                ((Int64Int64PtrPtrProd *) tailift_1520)->field2 =
                    fltPkd_1212_1321;
                ((Int64Int64PtrPtrProd *) tailift_1520)->field3 =
                    r_102_1048_1319;
                return tailift_1520;
            } else {
                CursorTy fltPkd_1213_1322 =
                          insertT_612(k_96_1043_1314, e_97_1044_1315, r_102_1048_1319);
                PtrTy tailift_1521 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                
                ((Int64Int64PtrPtrProd *) tailift_1521)->field0 = 0;
                ((Int64Int64PtrPtrProd *) tailift_1521)->field1 =
                    k__100_1046_1317;
                ((Int64Int64PtrPtrProd *) tailift_1521)->field2 =
                    l_101_1047_1318;
                ((Int64Int64PtrPtrProd *) tailift_1521)->field3 =
                    fltPkd_1213_1322;
                return tailift_1521;
            }
            break;
        }
        
      case 1:
        {
            IntTy k__103_1049_1323 =
                  ((Int64ProdInt64Int64Int64Prod *) tail_1519)->field0;
            Int64Int64Int64Prod v_104_1050_1324 =
                                ((Int64ProdInt64Int64Int64Prod *) tail_1519)->field1;
            BoolTy fltIf_1214_1325 = k_96_1043_1314 < k__103_1049_1323;
            
            if (fltIf_1214_1325) {
                PtrTy fltPkd_1215_1326 =
                      ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Prod));
                
                ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1215_1326)->field0 =
                    1;
                ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1215_1326)->field1 =
                    k_96_1043_1314;
                ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1215_1326)->field2 =
                    e_97_1044_1315;
                
                PtrTy fltPkd_1216_1327 =
                      ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Prod));
                
                ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1216_1327)->field0 =
                    1;
                ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1216_1327)->field1 =
                    k__103_1049_1323;
                ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1216_1327)->field2 =
                    v_104_1050_1324;
                
                PtrTy tailift_1522 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                
                ((Int64Int64PtrPtrProd *) tailift_1522)->field0 = 0;
                ((Int64Int64PtrPtrProd *) tailift_1522)->field1 =
                    k_96_1043_1314;
                ((Int64Int64PtrPtrProd *) tailift_1522)->field2 =
                    fltPkd_1215_1326;
                ((Int64Int64PtrPtrProd *) tailift_1522)->field3 =
                    fltPkd_1216_1327;
                return tailift_1522;
            } else {
                BoolTy fltIf_1217_1328 = k_96_1043_1314 > k__103_1049_1323;
                
                if (fltIf_1217_1328) {
                    PtrTy fltPkd_1218_1329 =
                          ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Prod));
                    
                    ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1218_1329)->field0 =
                        1;
                    ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1218_1329)->field1 =
                        k__103_1049_1323;
                    ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1218_1329)->field2 =
                        v_104_1050_1324;
                    
                    PtrTy fltPkd_1219_1330 =
                          ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Prod));
                    
                    ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1219_1330)->field0 =
                        1;
                    ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1219_1330)->field1 =
                        k__103_1049_1323;
                    ((Int64Int64ProdInt64Int64Int64Prod *) fltPkd_1219_1330)->field2 =
                        e_97_1044_1315;
                    
                    PtrTy tailift_1523 = ALLOC(sizeof(Int64Int64PtrPtrProd));
                    
                    ((Int64Int64PtrPtrProd *) tailift_1523)->field0 = 0;
                    ((Int64Int64PtrPtrProd *) tailift_1523)->field1 =
                        k__103_1049_1323;
                    ((Int64Int64PtrPtrProd *) tailift_1523)->field2 =
                        fltPkd_1218_1329;
                    ((Int64Int64PtrPtrProd *) tailift_1523)->field3 =
                        fltPkd_1219_1330;
                    return tailift_1523;
                } else {
                    PtrTy tailift_1524 = ALLOC(sizeof(Int64Prod));
                    
                    ((Int64Prod *) tailift_1524)->field0 = 3;
                    return tailift_1524;
                }
            }
            break;
        }
        
      case 2:
        {
            PtrTy tailift_1525 =
                  ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Prod));
            
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1525)->field0 = 1;
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1525)->field1 =
                k_96_1043_1314;
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1525)->field2 =
                e_97_1044_1315;
            return tailift_1525;
            break;
        }
        
      case 3:
        {
            PtrTy tailift_1526 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1526)->field0 = 3;
            return tailift_1526;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1518");
            exit(1);
        }
    }
}
CursorTy mkTree_810(VectorTy *pts_147_1055_1331, CursorTy t_148_1056_1332)
{
    IntTy fltPrm_1221_1334 = vector_length(pts_147_1055_1331);
    BoolTy fltIf_1220_1335 = fltPrm_1221_1334 == 0;
    
    if (fltIf_1220_1335) {
        return t_148_1056_1332;
    } else {
        Int64Int64Int64Prod *tmp_9;
        
        tmp_9 = (Int64Int64Int64Prod *) vector_nth(pts_147_1055_1331, 0);
        
        Int64Int64Int64Prod pt_149_1057_1337 = *tmp_9;
        IntTy fltPrm_1223_1339 = vector_length(pts_147_1055_1331);
        IntTy fltAppE_1222_1340 = fltPrm_1223_1339 - 1;
        VectorTy *pts__150_1058_1341 = vector_slice(1, fltAppE_1222_1340,
                                                    pts_147_1055_1331);
        IntTy x_112_1052_1186_1343 = (IntTy) pt_149_1057_1337.field0;
        IntTy wildcard__84_113_1053_1187_1344 = (IntTy) pt_149_1057_1337.field1;
        IntTy wildcard__86_114_1054_1188_1345 = (IntTy) pt_149_1057_1337.field2;
        CursorTy fltAppE_1224_1347 =
                  insertT_612(x_112_1052_1186_1343, pt_149_1057_1337, t_148_1056_1332);
        
        return mkTree_810(pts__150_1058_1341, fltAppE_1224_1347);
    }
}
CursorTy _copy_Tree_v_606(CursorTy arg_840_1060_1348)
{
    TagTyPacked tag_1528 = *(TagTyPacked *) arg_840_1060_1348;
    CursorTy tail_1529 = arg_840_1060_1348 + sizeof(IntTy);
    
    
  switch_1534:
    ;
    switch (tag_1528) {
        
      case 0:
        {
            IntTy x_841_1061_1349 = ((Int64PtrPtrProd *) tail_1529)->field0;
            PtrTy x_842_1062_1350 = ((Int64PtrPtrProd *) tail_1529)->field1;
            PtrTy x_843_1063_1351 = ((Int64PtrPtrProd *) tail_1529)->field2;
            CursorTy y_845_1065_1353 =  _copy_Tree_v_606(x_842_1062_1350);
            CursorTy y_846_1066_1354 =  _copy_Tree_v_606(x_843_1063_1351);
            PtrTy tailift_1530 = ALLOC(sizeof(Int64Int64PtrPtrProd));
            
            ((Int64Int64PtrPtrProd *) tailift_1530)->field0 = 0;
            ((Int64Int64PtrPtrProd *) tailift_1530)->field1 = x_841_1061_1349;
            ((Int64Int64PtrPtrProd *) tailift_1530)->field2 = y_845_1065_1353;
            ((Int64Int64PtrPtrProd *) tailift_1530)->field3 = y_846_1066_1354;
            return tailift_1530;
            break;
        }
        
      case 1:
        {
            IntTy x_847_1067_1355 =
                  ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1529)->field0;
            Int64Int64Int64Int64Int64Prod x_848_1068_1356 =
                                          ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1529)->field1;
            PtrTy tailift_1531 =
                  ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Int64Int64Prod));
            
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1531)->field0 =
                1;
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1531)->field1 =
                x_847_1067_1355;
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1531)->field2 =
                x_848_1068_1356;
            return tailift_1531;
            break;
        }
        
      case 2:
        {
            PtrTy tailift_1532 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1532)->field0 = 2;
            return tailift_1532;
            break;
        }
        
      case 3:
        {
            PtrTy tailift_1533 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1533)->field0 = 3;
            return tailift_1533;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1528");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Tree_v_606(CursorTy arg_851_1071_1359)
{
    TagTyPacked tag_1535 = *(TagTyPacked *) arg_851_1071_1359;
    CursorTy tail_1536 = arg_851_1071_1359 + sizeof(IntTy);
    
    
  switch_1541:
    ;
    switch (tag_1535) {
        
      case 0:
        {
            IntTy x_852_1072_1360 = ((Int64PtrPtrProd *) tail_1536)->field0;
            PtrTy x_853_1073_1361 = ((Int64PtrPtrProd *) tail_1536)->field1;
            PtrTy x_854_1074_1362 = ((Int64PtrPtrProd *) tail_1536)->field2;
            CursorTy y_856_1076_1364 =
                      _copy_without_ptrs_Tree_v_606(x_853_1073_1361);
            CursorTy y_857_1077_1365 =
                      _copy_without_ptrs_Tree_v_606(x_854_1074_1362);
            PtrTy tailift_1537 = ALLOC(sizeof(Int64Int64PtrPtrProd));
            
            ((Int64Int64PtrPtrProd *) tailift_1537)->field0 = 0;
            ((Int64Int64PtrPtrProd *) tailift_1537)->field1 = x_852_1072_1360;
            ((Int64Int64PtrPtrProd *) tailift_1537)->field2 = y_856_1076_1364;
            ((Int64Int64PtrPtrProd *) tailift_1537)->field3 = y_857_1077_1365;
            return tailift_1537;
            break;
        }
        
      case 1:
        {
            IntTy x_858_1078_1366 =
                  ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1536)->field0;
            Int64Int64Int64Int64Int64Prod x_859_1079_1367 =
                                          ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1536)->field1;
            PtrTy tailift_1538 =
                  ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Int64Int64Prod));
            
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1538)->field0 =
                1;
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1538)->field1 =
                x_858_1078_1366;
            ((Int64Int64ProdInt64Int64Int64Int64Int64Prod *) tailift_1538)->field2 =
                x_859_1079_1367;
            return tailift_1538;
            break;
        }
        
      case 2:
        {
            PtrTy tailift_1539 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1539)->field0 = 2;
            return tailift_1539;
            break;
        }
        
      case 3:
        {
            PtrTy tailift_1540 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1540)->field0 = 3;
            return tailift_1540;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1535");
            exit(1);
        }
    }
}
unsigned char _traverse_Tree_v_606(CursorTy arg_862_1082_1370)
{
    TagTyPacked tag_1542 = *(TagTyPacked *) arg_862_1082_1370;
    CursorTy tail_1543 = arg_862_1082_1370 + sizeof(IntTy);
    
    
  switch_1544:
    ;
    switch (tag_1542) {
        
      case 0:
        {
            IntTy x_863_1083_1371 = ((Int64PtrPtrProd *) tail_1543)->field0;
            PtrTy x_864_1084_1372 = ((Int64PtrPtrProd *) tail_1543)->field1;
            PtrTy x_865_1085_1373 = ((Int64PtrPtrProd *) tail_1543)->field2;
            unsigned char y_867_1086_1374 =
                           _traverse_Tree_v_606(x_864_1084_1372);
            unsigned char y_868_1087_1375 =
                           _traverse_Tree_v_606(x_865_1085_1373);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_869_1088_1376 =
                  ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1543)->field0;
            Int64Int64Int64Int64Int64Prod x_870_1089_1377 =
                                          ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1543)->field1;
            
            return 0;
            break;
        }
        
      case 2:
        {
            return 0;
            break;
        }
        
      case 3:
        {
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1542");
            exit(1);
        }
    }
}
unsigned char _print_Tree_v_606(CursorTy arg_873_1090_1378)
{
    TagTyPacked tag_1545 = *(TagTyPacked *) arg_873_1090_1378;
    CursorTy tail_1546 = arg_873_1090_1378 + sizeof(IntTy);
    
    
  switch_1547:
    ;
    switch (tag_1545) {
        
      case 0:
        {
            IntTy x_874_1091_1379 = ((Int64PtrPtrProd *) tail_1546)->field0;
            PtrTy x_875_1092_1380 = ((Int64PtrPtrProd *) tail_1546)->field1;
            PtrTy x_876_1093_1381 = ((Int64PtrPtrProd *) tail_1546)->field2;
            unsigned char wildcard_880_1094_1382 = print_symbol(1466);
            unsigned char y_877_1095_1383 = printf("%lld", x_874_1091_1379);
            unsigned char y_878_1096_1384 =  _print_Tree_v_606(x_875_1092_1380);
            unsigned char y_879_1097_1385 =  _print_Tree_v_606(x_876_1093_1381);
            unsigned char wildcard_881_1098_1386 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_882_1099_1387 =
                  ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1546)->field0;
            Int64Int64Int64Int64Int64Prod x_883_1100_1388 =
                                          ((Int64ProdInt64Int64Int64Int64Int64Prod *) tail_1546)->field1;
            unsigned char wildcard_886_1101_1389 = print_symbol(1468);
            unsigned char y_884_1102_1390 = printf("%lld", x_882_1099_1387);
            unsigned char wildcard_887_1103_1391 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard_888_1104_1392 = print_symbol(1473);
            unsigned char wildcard_889_1105_1393 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard_890_1106_1394 = print_symbol(1471);
            unsigned char wildcard_891_1107_1395 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1545");
            exit(1);
        }
    }
}
CursorTy _copy_Tree_v_605(CursorTy arg_892_1108_1396)
{
    TagTyPacked tag_1548 = *(TagTyPacked *) arg_892_1108_1396;
    CursorTy tail_1549 = arg_892_1108_1396 + sizeof(IntTy);
    
    
  switch_1554:
    ;
    switch (tag_1548) {
        
      case 0:
        {
            IntTy x_893_1109_1397 = ((Int64PtrPtrProd *) tail_1549)->field0;
            PtrTy x_894_1110_1398 = ((Int64PtrPtrProd *) tail_1549)->field1;
            PtrTy x_895_1111_1399 = ((Int64PtrPtrProd *) tail_1549)->field2;
            CursorTy y_897_1113_1401 =  _copy_Tree_v_605(x_894_1110_1398);
            CursorTy y_898_1114_1402 =  _copy_Tree_v_605(x_895_1111_1399);
            PtrTy tailift_1550 = ALLOC(sizeof(Int64Int64PtrPtrProd));
            
            ((Int64Int64PtrPtrProd *) tailift_1550)->field0 = 0;
            ((Int64Int64PtrPtrProd *) tailift_1550)->field1 = x_893_1109_1397;
            ((Int64Int64PtrPtrProd *) tailift_1550)->field2 = y_897_1113_1401;
            ((Int64Int64PtrPtrProd *) tailift_1550)->field3 = y_898_1114_1402;
            return tailift_1550;
            break;
        }
        
      case 1:
        {
            IntTy x_899_1115_1403 =
                  ((Int64ProdInt64Int64Int64Prod *) tail_1549)->field0;
            Int64Int64Int64Prod x_900_1116_1404 =
                                ((Int64ProdInt64Int64Int64Prod *) tail_1549)->field1;
            PtrTy tailift_1551 =
                  ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Prod));
            
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1551)->field0 = 1;
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1551)->field1 =
                x_899_1115_1403;
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1551)->field2 =
                x_900_1116_1404;
            return tailift_1551;
            break;
        }
        
      case 2:
        {
            PtrTy tailift_1552 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1552)->field0 = 2;
            return tailift_1552;
            break;
        }
        
      case 3:
        {
            PtrTy tailift_1553 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1553)->field0 = 3;
            return tailift_1553;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1548");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Tree_v_605(CursorTy arg_903_1119_1407)
{
    TagTyPacked tag_1555 = *(TagTyPacked *) arg_903_1119_1407;
    CursorTy tail_1556 = arg_903_1119_1407 + sizeof(IntTy);
    
    
  switch_1561:
    ;
    switch (tag_1555) {
        
      case 0:
        {
            IntTy x_904_1120_1408 = ((Int64PtrPtrProd *) tail_1556)->field0;
            PtrTy x_905_1121_1409 = ((Int64PtrPtrProd *) tail_1556)->field1;
            PtrTy x_906_1122_1410 = ((Int64PtrPtrProd *) tail_1556)->field2;
            CursorTy y_908_1124_1412 =
                      _copy_without_ptrs_Tree_v_605(x_905_1121_1409);
            CursorTy y_909_1125_1413 =
                      _copy_without_ptrs_Tree_v_605(x_906_1122_1410);
            PtrTy tailift_1557 = ALLOC(sizeof(Int64Int64PtrPtrProd));
            
            ((Int64Int64PtrPtrProd *) tailift_1557)->field0 = 0;
            ((Int64Int64PtrPtrProd *) tailift_1557)->field1 = x_904_1120_1408;
            ((Int64Int64PtrPtrProd *) tailift_1557)->field2 = y_908_1124_1412;
            ((Int64Int64PtrPtrProd *) tailift_1557)->field3 = y_909_1125_1413;
            return tailift_1557;
            break;
        }
        
      case 1:
        {
            IntTy x_910_1126_1414 =
                  ((Int64ProdInt64Int64Int64Prod *) tail_1556)->field0;
            Int64Int64Int64Prod x_911_1127_1415 =
                                ((Int64ProdInt64Int64Int64Prod *) tail_1556)->field1;
            PtrTy tailift_1558 =
                  ALLOC(sizeof(Int64Int64ProdInt64Int64Int64Prod));
            
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1558)->field0 = 1;
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1558)->field1 =
                x_910_1126_1414;
            ((Int64Int64ProdInt64Int64Int64Prod *) tailift_1558)->field2 =
                x_911_1127_1415;
            return tailift_1558;
            break;
        }
        
      case 2:
        {
            PtrTy tailift_1559 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1559)->field0 = 2;
            return tailift_1559;
            break;
        }
        
      case 3:
        {
            PtrTy tailift_1560 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1560)->field0 = 3;
            return tailift_1560;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1555");
            exit(1);
        }
    }
}
unsigned char _traverse_Tree_v_605(CursorTy arg_914_1130_1418)
{
    TagTyPacked tag_1562 = *(TagTyPacked *) arg_914_1130_1418;
    CursorTy tail_1563 = arg_914_1130_1418 + sizeof(IntTy);
    
    
  switch_1564:
    ;
    switch (tag_1562) {
        
      case 0:
        {
            IntTy x_915_1131_1419 = ((Int64PtrPtrProd *) tail_1563)->field0;
            PtrTy x_916_1132_1420 = ((Int64PtrPtrProd *) tail_1563)->field1;
            PtrTy x_917_1133_1421 = ((Int64PtrPtrProd *) tail_1563)->field2;
            unsigned char y_919_1134_1422 =
                           _traverse_Tree_v_605(x_916_1132_1420);
            unsigned char y_920_1135_1423 =
                           _traverse_Tree_v_605(x_917_1133_1421);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_921_1136_1424 =
                  ((Int64ProdInt64Int64Int64Prod *) tail_1563)->field0;
            Int64Int64Int64Prod x_922_1137_1425 =
                                ((Int64ProdInt64Int64Int64Prod *) tail_1563)->field1;
            
            return 0;
            break;
        }
        
      case 2:
        {
            return 0;
            break;
        }
        
      case 3:
        {
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1562");
            exit(1);
        }
    }
}
unsigned char _print_Tree_v_605(CursorTy arg_925_1138_1426)
{
    TagTyPacked tag_1565 = *(TagTyPacked *) arg_925_1138_1426;
    CursorTy tail_1566 = arg_925_1138_1426 + sizeof(IntTy);
    
    
  switch_1567:
    ;
    switch (tag_1565) {
        
      case 0:
        {
            IntTy x_926_1139_1427 = ((Int64PtrPtrProd *) tail_1566)->field0;
            PtrTy x_927_1140_1428 = ((Int64PtrPtrProd *) tail_1566)->field1;
            PtrTy x_928_1141_1429 = ((Int64PtrPtrProd *) tail_1566)->field2;
            unsigned char wildcard_932_1142_1430 = print_symbol(1467);
            unsigned char y_929_1143_1431 = printf("%lld", x_926_1139_1427);
            unsigned char y_930_1144_1432 =  _print_Tree_v_605(x_927_1140_1428);
            unsigned char y_931_1145_1433 =  _print_Tree_v_605(x_928_1141_1429);
            unsigned char wildcard_933_1146_1434 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      case 1:
        {
            IntTy x_934_1147_1435 =
                  ((Int64ProdInt64Int64Int64Prod *) tail_1566)->field0;
            Int64Int64Int64Prod x_935_1148_1436 =
                                ((Int64ProdInt64Int64Int64Prod *) tail_1566)->field1;
            unsigned char wildcard_938_1149_1437 = print_symbol(1469);
            unsigned char y_936_1150_1438 = printf("%lld", x_934_1147_1435);
            unsigned char wildcard_939_1151_1439 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard_940_1152_1440 = print_symbol(1474);
            unsigned char wildcard_941_1153_1441 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard_942_1154_1442 = print_symbol(1472);
            unsigned char wildcard_943_1155_1443 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1565");
            exit(1);
        }
    }
}
CursorTy _copy_Maybe_v_608(CursorTy arg_944_1156_1444)
{
    TagTyPacked tag_1568 = *(TagTyPacked *) arg_944_1156_1444;
    CursorTy tail_1569 = arg_944_1156_1444 + sizeof(IntTy);
    
    
  switch_1572:
    ;
    switch (tag_1568) {
        
      case 0:
        {
            PtrTy tailift_1570 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1570)->field0 = 0;
            return tailift_1570;
            break;
        }
        
      case 1:
        {
            Int64Int64Int64Prod x_945_1157_1445 =
                                ((ProdInt64Int64Int64Prod *) tail_1569)->field0;
            PtrTy tailift_1571 = ALLOC(sizeof(Int64ProdInt64Int64Int64Prod));
            
            ((Int64ProdInt64Int64Int64Prod *) tailift_1571)->field0 = 1;
            ((Int64ProdInt64Int64Int64Prod *) tailift_1571)->field1 =
                x_945_1157_1445;
            return tailift_1571;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1568");
            exit(1);
        }
    }
}
CursorTy _copy_without_ptrs_Maybe_v_608(CursorTy arg_947_1159_1447)
{
    TagTyPacked tag_1573 = *(TagTyPacked *) arg_947_1159_1447;
    CursorTy tail_1574 = arg_947_1159_1447 + sizeof(IntTy);
    
    
  switch_1577:
    ;
    switch (tag_1573) {
        
      case 0:
        {
            PtrTy tailift_1575 = ALLOC(sizeof(Int64Prod));
            
            ((Int64Prod *) tailift_1575)->field0 = 0;
            return tailift_1575;
            break;
        }
        
      case 1:
        {
            Int64Int64Int64Prod x_948_1160_1448 =
                                ((ProdInt64Int64Int64Prod *) tail_1574)->field0;
            PtrTy tailift_1576 = ALLOC(sizeof(Int64ProdInt64Int64Int64Prod));
            
            ((Int64ProdInt64Int64Int64Prod *) tailift_1576)->field0 = 1;
            ((Int64ProdInt64Int64Int64Prod *) tailift_1576)->field1 =
                x_948_1160_1448;
            return tailift_1576;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1573");
            exit(1);
        }
    }
}
unsigned char _traverse_Maybe_v_608(CursorTy arg_950_1162_1450)
{
    TagTyPacked tag_1578 = *(TagTyPacked *) arg_950_1162_1450;
    CursorTy tail_1579 = arg_950_1162_1450 + sizeof(IntTy);
    
    
  switch_1580:
    ;
    switch (tag_1578) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            Int64Int64Int64Prod x_951_1163_1451 =
                                ((ProdInt64Int64Int64Prod *) tail_1579)->field0;
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1578");
            exit(1);
        }
    }
}
unsigned char _print_Maybe_v_608(CursorTy arg_953_1164_1452)
{
    TagTyPacked tag_1581 = *(TagTyPacked *) arg_953_1164_1452;
    CursorTy tail_1582 = arg_953_1164_1452 + sizeof(IntTy);
    
    
  switch_1583:
    ;
    switch (tag_1581) {
        
      case 0:
        {
            unsigned char wildcard_954_1165_1453 = print_symbol(1465);
            unsigned char wildcard_955_1166_1454 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      case 1:
        {
            Int64Int64Int64Prod x_956_1167_1455 =
                                ((ProdInt64Int64Int64Prod *) tail_1582)->field0;
            unsigned char wildcard_958_1168_1456 = print_symbol(1470);
            unsigned char wildcard_959_1169_1457 = print_symbol(1464);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag_1581");
            exit(1);
        }
    }
}
int __main_expr()
{
    add_symbol(1464, ")");
    add_symbol(1465, "(Nothing_v_608 ");
    add_symbol(1466, "(Node_v_606 ");
    add_symbol(1467, "(Node_v_605 ");
    add_symbol(1468, "(Leaf_v_606 ");
    add_symbol(1469, "(Leaf_v_605 ");
    add_symbol(1470, "(Just_v_608 ");
    add_symbol(1471, "(ErrorNode_v_606 ");
    add_symbol(1472, "(ErrorNode_v_605 ");
    add_symbol(1473, "(Empty_v_606 ");
    add_symbol(1474, "(Empty_v_605 ");
    
    unsigned char tmp_app_1463 =  bench_main();
    
    printf("'#()");
    printf("\n");
    free_symtable();
    return 0;
}