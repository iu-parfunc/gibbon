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
typedef struct GibIntGibIntGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibCursor field2;
        } GibIntGibIntGibCursorProd;
typedef struct GibIntGibCursorProd_struct {
            GibInt field0;
            GibCursor field1;
        } GibIntGibCursorProd;
typedef struct GibBoolProd_struct {
            GibBool field0;
        } GibBoolProd;
typedef struct GibPackedTagGibCursorProd_struct {
            GibPackedTag field0;
            GibCursor field1;
        } GibPackedTagGibCursorProd;
typedef struct GibCursorProd_struct {
            GibCursor field0;
        } GibCursorProd;
GibCursor insert2062(GibInt idx27122168, GibInt x28123169, GibCursor s29124170);
GibCursor singleton1861(GibInt x25127177);
GibCursor _copy_Node21_v63(GibCursor arg96128179);
GibCursor _copy_without_ptrs_Node21_v63(GibCursor arg101133184);
unsigned char _traverse_Node21_v63(GibCursor arg106138189);
unsigned char _print_Node21_v63(GibCursor arg111142193);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Node21_v63_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(8);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[1];
    
    error = gib_info_table_insert_packed_dcon(Node21_v63_T, 1, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Node21_v63_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Node21_v63_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Node21_v63_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(205, ")");
    gib_add_symbol(206, "(Tip22_v63");
    gib_add_symbol(207, "(Bin23_v63");
    gib_add_symbol(208, " ");
}
GibCursor insert2062(GibInt idx27122168, GibInt x28123169, GibCursor s29124170)
{
    GibPackedTag tag209 = *(GibPackedTag *) s29124170;
    GibCursor tail210 = s29124170 + sizeof(GibInt);
    
    
  switch214:
    ;
    switch (tag209) {
        
      case 1:
        {
            GibInt v31125171 = ((GibIntGibCursorProd *) tail210)->field0;
            GibCursor n32126172 = ((GibIntGibCursorProd *) tail210)->field1;
            GibBool fltIf158173 = idx27122168 == 0;
            
            if (fltIf158173) {
                GibPtr tailift211 =
                       gib_alloc(sizeof(GibIntGibIntGibCursorProd));
                
                ((GibIntGibIntGibCursorProd *) tailift211)->field0 = 1;
                ((GibIntGibIntGibCursorProd *) tailift211)->field1 = x28123169;
                ((GibIntGibIntGibCursorProd *) tailift211)->field2 = s29124170;
                return tailift211;
            } else {
                GibInt fltAppE160174 = idx27122168 - 1;
                GibCursor fltPkd159175 =
                           insert2062(fltAppE160174, x28123169, n32126172);
                GibPtr tailift212 =
                       gib_alloc(sizeof(GibIntGibIntGibCursorProd));
                
                ((GibIntGibIntGibCursorProd *) tailift212)->field0 = 1;
                ((GibIntGibIntGibCursorProd *) tailift212)->field1 = v31125171;
                ((GibIntGibIntGibCursorProd *) tailift212)->field2 =
                    fltPkd159175;
                return tailift212;
            }
            break;
        }
        
      case 0:
        {
            GibPtr fltPkd161176 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd161176)->field0 = 0;
            
            GibPtr tailift213 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift213)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift213)->field1 = x28123169;
            ((GibIntGibIntGibCursorProd *) tailift213)->field2 = fltPkd161176;
            return tailift213;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag209");
            exit(1);
        }
    }
}
GibCursor singleton1861(GibInt x25127177)
{
    GibPtr fltPkd162178 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd162178)->field0 = 0;
    
    GibPtr tailift215 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift215)->field0 = 1;
    ((GibIntGibIntGibCursorProd *) tailift215)->field1 = x25127177;
    ((GibIntGibIntGibCursorProd *) tailift215)->field2 = fltPkd162178;
    return tailift215;
}
GibCursor _copy_Node21_v63(GibCursor arg96128179)
{
    GibPackedTag tag216 = *(GibPackedTag *) arg96128179;
    GibCursor tail217 = arg96128179 + sizeof(GibInt);
    
    
  switch220:
    ;
    switch (tag216) {
        
      case 0:
        {
            GibPtr tailift218 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift218)->field0 = 0;
            return tailift218;
            break;
        }
        
      case 1:
        {
            GibInt x97129180 = ((GibIntGibCursorProd *) tail217)->field0;
            GibCursor x98130181 = ((GibIntGibCursorProd *) tail217)->field1;
            GibCursor y100132183 =  _copy_Node21_v63(x98130181);
            GibPtr tailift219 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift219)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift219)->field1 = x97129180;
            ((GibIntGibIntGibCursorProd *) tailift219)->field2 = y100132183;
            return tailift219;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag216");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Node21_v63(GibCursor arg101133184)
{
    GibPackedTag tag221 = *(GibPackedTag *) arg101133184;
    GibCursor tail222 = arg101133184 + sizeof(GibInt);
    
    
  switch225:
    ;
    switch (tag221) {
        
      case 0:
        {
            GibPtr tailift223 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift223)->field0 = 0;
            return tailift223;
            break;
        }
        
      case 1:
        {
            GibInt x102134185 = ((GibIntGibCursorProd *) tail222)->field0;
            GibCursor x103135186 = ((GibIntGibCursorProd *) tail222)->field1;
            GibCursor y105137188 =  _copy_without_ptrs_Node21_v63(x103135186);
            GibPtr tailift224 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift224)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift224)->field1 = x102134185;
            ((GibIntGibIntGibCursorProd *) tailift224)->field2 = y105137188;
            return tailift224;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag221");
            exit(1);
        }
    }
}
unsigned char _traverse_Node21_v63(GibCursor arg106138189)
{
    GibPackedTag tag226 = *(GibPackedTag *) arg106138189;
    GibCursor tail227 = arg106138189 + sizeof(GibInt);
    
    
  switch228:
    ;
    switch (tag226) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x107139190 = ((GibIntGibCursorProd *) tail227)->field0;
            GibCursor x108140191 = ((GibIntGibCursorProd *) tail227)->field1;
            unsigned char y110141192 =  _traverse_Node21_v63(x108140191);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag226");
            exit(1);
        }
    }
}
unsigned char _print_Node21_v63(GibCursor arg111142193)
{
    GibPackedTag tag229 = *(GibPackedTag *) arg111142193;
    GibCursor tail230 = arg111142193 + sizeof(GibInt);
    
    
  switch231:
    ;
    switch (tag229) {
        
      case 0:
        {
            unsigned char wildcard112143194 = gib_print_symbol(206);
            unsigned char wildcard113144195 = gib_print_symbol(205);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x114145196 = ((GibIntGibCursorProd *) tail230)->field0;
            GibCursor x115146197 = ((GibIntGibCursorProd *) tail230)->field1;
            unsigned char wildcard118147198 = gib_print_symbol(207);
            unsigned char wildcard121148199 = gib_print_symbol(208);
            unsigned char y116149200 = printf("%ld", x114145196);
            unsigned char wildcard120150201 = gib_print_symbol(208);
            unsigned char y117151202 =  _print_Node21_v63(x115146197);
            unsigned char wildcard119152203 = gib_print_symbol(205);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag229");
            exit(1);
        }
    }
}
int main(int argc, char **argv)
{
    int init0 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibCursor fltAppE157163 =  singleton1861(5);
    GibCursor fltAppE156164 =  insert2062(1, 4, fltAppE157163);
    GibCursor fltAppE155165 =  insert2062(2, 3, fltAppE156164);
    GibCursor fltAppE154166 =  insert2062(3, 2, fltAppE155165);
    GibCursor fltAppE153167 =  insert2062(4, 1, fltAppE154166);
    GibCursor tmp_app204 =  insert2062(5, 0, fltAppE153167);
    
     _print_Node21_v63(tmp_app204);
    printf("\n");
    return 0;
    
    int exit1 = gib_exit();
    
    return exit1;
}