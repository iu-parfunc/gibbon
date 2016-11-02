
#ifndef _DEBUG_H_
#define _DEBUG_H_

#include "debug.h"
#include <assert.h>

#define ERROR(fmt, ...)\
    printf(fmt " %s %d\n", ##__VA_ARGS__, __FILE__, __LINE__);\
  assert(0);
//exit(EXIT_FAILURE)

#endif /* _DEBUG_H_ */
