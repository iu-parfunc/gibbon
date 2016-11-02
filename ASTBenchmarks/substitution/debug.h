
#define ERROR(fmt, ...)\
    printf(fmt " %s %d\n", ##__VA_ARGS__, __FILE__, __LINE__);\
  assert(0);
//exit(EXIT_FAILURE)


