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
typedef struct GibPackedTagGibCursorProd_struct {
            GibPackedTag field0;
            GibCursor field1;
        } GibPackedTagGibCursorProd;
GibCursor _copy_Ord9(GibCursor arg203547);
GibCursor _copy_without_ptrs_Ord9(GibCursor arg213648);
unsigned char _traverse_Ord9(GibCursor arg223749);
unsigned char _print_Ord9(GibCursor arg233850);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Ord9_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(8);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[0];
    
    error = gib_info_table_insert_packed_dcon(Ord9_T, 3, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord9_T, 3);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord9_T, 2, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord9_T, 2);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord9_T, 1, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord9_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord9_T, 0, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord9_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(59, ")");
    gib_add_symbol(60, "(Lt10");
    gib_add_symbol(61, "(Gt11");
    gib_add_symbol(62, "(Eq12");
    gib_add_symbol(63, "(Cc13");
}
GibCursor _copy_Ord9(GibCursor arg203547)
{
    GibPackedTag tag64 = *(GibPackedTag *) arg203547;
    GibCursor tail65 = arg203547 + sizeof(GibInt);
    
    
  switch70:
    ;
    switch (tag64) {
        
      case 0:
        {
            GibPtr tailift66 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift66)->field0 = 0;
            return tailift66;
            break;
        }
        
      case 1:
        {
            GibPtr tailift67 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift67)->field0 = 1;
            return tailift67;
            break;
        }
        
      case 2:
        {
            GibPtr tailift68 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift68)->field0 = 2;
            return tailift68;
            break;
        }
        
      case 3:
        {
            GibPtr tailift69 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift69)->field0 = 3;
            return tailift69;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag64");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Ord9(GibCursor arg213648)
{
    GibPackedTag tag71 = *(GibPackedTag *) arg213648;
    GibCursor tail72 = arg213648 + sizeof(GibInt);
    
    
  switch77:
    ;
    switch (tag71) {
        
      case 0:
        {
            GibPtr tailift73 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift73)->field0 = 0;
            return tailift73;
            break;
        }
        
      case 1:
        {
            GibPtr tailift74 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift74)->field0 = 1;
            return tailift74;
            break;
        }
        
      case 2:
        {
            GibPtr tailift75 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift75)->field0 = 2;
            return tailift75;
            break;
        }
        
      case 3:
        {
            GibPtr tailift76 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift76)->field0 = 3;
            return tailift76;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag71");
            exit(1);
        }
    }
}
unsigned char _traverse_Ord9(GibCursor arg223749)
{
    GibPackedTag tag78 = *(GibPackedTag *) arg223749;
    GibCursor tail79 = arg223749 + sizeof(GibInt);
    
    
  switch80:
    ;
    switch (tag78) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
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
            printf("%s\n", "Unknown tag in: tag78");
            exit(1);
        }
    }
}
unsigned char _print_Ord9(GibCursor arg233850)
{
    GibPackedTag tag81 = *(GibPackedTag *) arg233850;
    GibCursor tail82 = arg233850 + sizeof(GibInt);
    
    
  switch83:
    ;
    switch (tag81) {
        
      case 0:
        {
            unsigned char wildcard243951 = gib_print_symbol(60);
            unsigned char wildcard254052 = gib_print_symbol(59);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard264153 = gib_print_symbol(61);
            unsigned char wildcard274254 = gib_print_symbol(59);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard284355 = gib_print_symbol(62);
            unsigned char wildcard294456 = gib_print_symbol(59);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard304557 = gib_print_symbol(63);
            unsigned char wildcard314658 = gib_print_symbol(59);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag81");
            exit(1);
        }
    }
}
int main(int argc, char **argv)
{
    int init0 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    int exit1 = gib_exit();
    
    return exit1;
}