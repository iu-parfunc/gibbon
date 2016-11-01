
#define ERROR(fmt, ...)\
    printf(fmt " %s %d\n", ##__VA_ARGS__, __FILE__, __LINE__);\
  exit(EXIT_FAILURE)


