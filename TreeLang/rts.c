#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <alloca.h>

#define ALLOC malloc
#define ALLOC_PACKED ALLOC

#define SIZE 1000

// 10MB default:
#define DEFAULT_BUF_SIZE 10000000

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

dict_item_t *dict_insert_int(dict_item_t *ptr, int key, int val) {
  dict_item_t *ret = dict_alloc();
  ret->key = key;
  ret->intval = val;
  ret->next = ptr;
  return ret;
}

int dict_lookup_int(dict_item_t *ptr, int key) {
  while (ptr != 0) {
    if (ptr->key == key) {
      return ptr->intval;
    } else {
      ptr = ptr->next;
    }
  }
  printf("Error, key %d not found!\n",key);
  exit(1);
}

// Could try alloca() here.  Better yet, we could keep our own,
// separate stack and insert our own code to restore the pointer
// before any function that (may have) called ALLOC_SCOPED returns.
#define ALLOC_SCOPED() alloca(1024)
// #define ALLOC_SCOPED() alloc_scoped()

// Our global pointer.  No parallelism.
// static char* stack_scoped_region;
// char* alloc_scoped() { return stack_scoped_region; }



// fun fact: __ prefix is actually reserved and this is an undefined behavior.
// These functions must be provided by the code generator.
void __fn_to_bench(char* in, char* out);
int __main_expr();
void __build_tree(int tree_size, char* buffer);

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

void bench(int num_iterations, int tree_size, int buffer_size)
{
    printf("Generating initial tree...\n");
    char* initial_buffer = (char*)malloc(buffer_size);
    assert(initial_buffer);
    __build_tree(tree_size, initial_buffer);

    printf("Benchmarking. Iteration count: %d\n", num_iterations);
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
    printf("%d\n", __main_expr());
}

int main(int argc, char** argv)
{
    // parameters to parse:
    //
    //   num iterations: How many times to repeat a benchmark. Default: 10.
    //   tree size: An integer passes to `build_tree()`. Default: 10.
    //   buffer size: Default 10M.

    int num_iterations = 10;
    int tree_size = 10;
    int buffer_size = DEFAULT_BUF_SIZE; // 10M

    // test by default
    int benchmark = 0;

    // TODO: atoi() error checking

    for (int i = 1; i < argc; ++i)
    {
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
        else if (strcmp(argv[i], "-buffer-size") == 0 && i < argc - 1)
        {
            buffer_size = atoi(argv[i + 1]);
            ++i;
        }
        else if ((strcmp(argv[i], "-benchmark") == 0) || (strcmp(argv[i], "-bench") == 0))
        {
            benchmark = 1;
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
        bench(num_iterations, tree_size, buffer_size);
    else
        run();

    return 0;
}
