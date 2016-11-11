#include <assert.h>
#include <stdio.h>
// #include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <alloca.h>

#include <sys/resource.h>

#define ALLOC malloc
#define ALLOC_PACKED ALLOC

#define SIZE 1000

// 10MB default:
#define DEFAULT_BUF_SIZE 10000000

static long long global_size_param = 1;
static long long  global_iters_param = 1;

typedef char TagTy;
typedef long long IntTy;
typedef IntTy SymTy;

typedef struct dict_item {
  struct dict_item * next;
  int key;
  union {
    int intval;
    void * ptrval;
  };
} dict_item_t;

dict_item_t * dict_alloc() {
  return ALLOC(sizeof(dict_item_t));
}

dict_item_t *dict_insert_int(dict_item_t *ptr, SymTy key, IntTy val) {
  dict_item_t *ret = dict_alloc();
  ret->key = key;
  ret->intval = val;
  ret->next = ptr;
  return ret;
}

IntTy dict_lookup_int(dict_item_t *ptr, SymTy key) {
  while (ptr != 0) {
    if (ptr->key == key) {
      return ptr->intval;
    } else {
      ptr = ptr->next;
    }
  }
  printf("Error, key %lld not found!\n",key);
  exit(1);
}

// Could try alloca() here.  Better yet, we could keep our own,
// separate stack and insert our own code to restore the pointer
// before any function that (may have) called ALLOC_SCOPED returns.

// #define ALLOC_SCOPED() alloca(1024)
#define ALLOC_SCOPED() alloca(100LU*1024LU)
// #define ALLOC_SCOPED() alloc_scoped()

// Stack allocation is either too small or blows our stack.
// We need a way to make a giant stack if we want to use alloca.
// #define ALLOC_SCOPED() ALLOC(DEFAULT_BUF_SIZE)


// Our global pointer.  No parallelism.
// static char* stack_scoped_region;
// char* alloc_scoped() { return stack_scoped_region; }



// fun fact: __ prefix is actually reserved and this is an undefined behavior.
// These functions must be provided by the code generator.
void __fn_to_bench(char* in, char* out);
IntTy __main_expr();
void __build_tree(IntTy tree_size, char* buffer);

void show_usage()
{
    // TODO
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

void bench(IntTy num_iterations, int tree_size, int buffer_size)
{
    printf("Generating initial tree...\n");
    char* initial_buffer = (char*)malloc(buffer_size);
    assert(initial_buffer);
    __build_tree(tree_size, initial_buffer);

    printf("Benchmarking. Iteration count: %lld\n", num_iterations);
    char* bench_buffer = (char*)malloc(buffer_size);
    assert(bench_buffer);

    double trials[num_iterations];
    struct timespec begin, end;

    for (int i = 0; i < num_iterations; ++i)
    {
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin);
        __fn_to_bench(initial_buffer, bench_buffer);
        clock_gettime(CLOCK_MONOTONIC_RAW, &end);
        trials[i] = difftimespecs(&begin, &end);
    }

    qsort(trials, num_iterations, sizeof(double), compare_doubles);
    printf("\nMINTIME: %lf\n",  trials[0]);
    printf("MEDIANTIME: %lf\n", trials[num_iterations / 2]);
    printf("MAXTIME: %lf\n",    trials[num_iterations - 1]);
    printf("AVGTIME: %lf\n",    avg(trials, num_iterations));
}

void run()
{
    printf("%lld\n", __main_expr());
}

int main(int argc, char** argv)
{
    // parameters to parse:
    //
    //   num iterations: How many times to repeat a benchmark. Default: 10.
    //   tree size: An integer passes to `build_tree()`. Default: 10.
    //   buffer size: Default 10M.

    struct rlimit lim;
    lim.rlim_cur = 1024LU * 1024LU * 1024LU; // 1GB stack.
    lim.rlim_max = lim.rlim_cur;
    int code = setrlimit(RLIMIT_STACK, &lim);
    if (code) {
      fprintf(stderr, "Failed to set stack size to %lu, code %d\n", lim.rlim_cur, code);
    }
  
    IntTy num_iterations = 10;
    int tree_size = 10;
    // We COULD make this larger than 4GB:
    IntTy buffer_size = DEFAULT_BUF_SIZE; // 10M

    // test by default
    int benchmark = 0;

    // TODO: atoi() error checking

    for (int i = 1; i < argc; ++i)
    {
      /* 
        if (strcmp(argv[i], "-num-iterations") == 0 && i < argc - 1)
        {
            num_iterations = atoi(argv[i + 1]);
            ++i;
        }
        else if (strcmp(argv[i], "-tree-size") == 0 && i < argc - 1)
        {
            tree_size = atoi(argv[i + 1]);
            ++i;
        }
        else*/
        if (strcmp(argv[i], "-buffer-size") == 0 && i < argc - 1)
        {
            buffer_size = atoll(argv[i + 1]);
            ++i;
        }
        else if ((strcmp(argv[i], "-benchmark") == 0) || (strcmp(argv[i], "-bench") == 0))
        {
          // benchmark = 1;
          if (i+2 >= argc) {
            fprintf(stderr, "Not enough arguments after -benchmark, expected <size> <iters>.\n");
            show_usage();
            exit(1);
          }
          // In this mode, we expect the last two arguments to be 
          global_size_param  = atoll(argv[i + 1]);
          global_iters_param = atoll(argv[i + 2]);
          break;
        }
        else
        {
            fprintf(stderr, "Can't parse argument: \"%s\"\n", argv[i]);
            show_usage();
            exit(1);
        }
    }

    // printf("\nTREEDEPTH: %d\nITERS: %d\n", tree_size, num_iterations);

    // Initialization:
    // stack_scoped_region = (char*)malloc(DEFAULT_BUF_SIZE);
    
    if (benchmark)
      // RRN: This will become the harness for runnin on mmap'd data:
        bench(num_iterations, tree_size, buffer_size);
    else
        run();

    return 0;
}
