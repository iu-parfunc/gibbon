/* Gibbon program. */

#include "gibbon_rts.h"

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <stdbool.h>
#include <string.h>
#include <time.h>
#include <alloca.h>
#include <sys/mman.h>
#include <sys/resource.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdarg.h>
#include <errno.h>
#include <uthash.h>

#ifdef _WIN64
#include <windows.h>
#endif

#ifdef _GIBBON_POINTER
#include <gc.h>
#endif

#ifdef _GIBBON_PARALLEL
#include <cilk/cilk.h>
#include <cilk/cilk_api.h>
#endif

/* ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 * Program starts here
 * ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 */

typedef struct GibIntProd_struct {
            GibInt field0;
        } GibIntProd;
GibInt add1(GibInt x_1_10_13);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(7);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[0];
    
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{ }
GibInt add1(GibInt x_1_10_13)
{
    GibInt flt_15 = x_1_10_13 + 1;
    
    return flt_15;
}
int main(int argc, char **argv)
{
    int init_0 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibInt fltAppE_11_12 =  add1(0);
    GibInt tmp_app_14 =  add1(fltAppE_11_12);
    
    printf("%ld", tmp_app_14);
    printf("\n");
    return 0;
    
    int exit_1 = gib_exit();
    
    return exit_1;
}