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
GibCursor insert2085(GibInt idx27145191, GibInt x28146192, GibCursor s29147193);
GibCursor singleton1884(GibInt x25150200);
GibCursor _copy_Node21_v86(GibCursor arg119151202);
GibCursor _copy_without_ptrs_Node21_v86(GibCursor arg124156207);
unsigned char _traverse_Node21_v86(GibCursor arg129161212);
unsigned char _print_Node21_v86(GibCursor arg134165216);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Node21_v86_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(8);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[1];
    
    error = gib_info_table_insert_packed_dcon(Node21_v86_T, 1, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Node21_v86_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Node21_v86_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Node21_v86_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(228, ")");
    gib_add_symbol(229, "(Tip22_v86");
    gib_add_symbol(230, "(Bin23_v86");
    gib_add_symbol(231, " ");
}
GibCursor insert2085(GibInt idx27145191, GibInt x28146192, GibCursor s29147193)
{
    GibPackedTag tag232 = *(GibPackedTag *) s29147193;
    GibCursor tail233 = s29147193 + sizeof(GibInt);
    
    
  switch237:
    ;
    switch (tag232) {
        
      case 1:
        {
            GibInt v31148194 = ((GibIntGibCursorProd *) tail233)->field0;
            GibCursor n32149195 = ((GibIntGibCursorProd *) tail233)->field1;
            GibBool fltIf181196 = idx27145191 == 0;
            
            if (fltIf181196) {
                GibPtr tailift234 =
                       gib_alloc(sizeof(GibIntGibIntGibCursorProd));
                
                ((GibIntGibIntGibCursorProd *) tailift234)->field0 = 1;
                ((GibIntGibIntGibCursorProd *) tailift234)->field1 = x28146192;
                ((GibIntGibIntGibCursorProd *) tailift234)->field2 = s29147193;
                return tailift234;
            } else {
                GibInt fltAppE183197 = idx27145191 - 1;
                GibCursor fltPkd182198 =
                           insert2085(fltAppE183197, x28146192, n32149195);
                GibPtr tailift235 =
                       gib_alloc(sizeof(GibIntGibIntGibCursorProd));
                
                ((GibIntGibIntGibCursorProd *) tailift235)->field0 = 1;
                ((GibIntGibIntGibCursorProd *) tailift235)->field1 = v31148194;
                ((GibIntGibIntGibCursorProd *) tailift235)->field2 =
                    fltPkd182198;
                return tailift235;
            }
            break;
        }
        
      case 0:
        {
            GibPtr fltPkd184199 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd184199)->field0 = 0;
            
            GibPtr tailift236 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift236)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift236)->field1 = x28146192;
            ((GibIntGibIntGibCursorProd *) tailift236)->field2 = fltPkd184199;
            return tailift236;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag232");
            exit(1);
        }
    }
}
GibCursor singleton1884(GibInt x25150200)
{
    GibPtr fltPkd185201 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd185201)->field0 = 0;
    
    GibPtr tailift238 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift238)->field0 = 1;
    ((GibIntGibIntGibCursorProd *) tailift238)->field1 = x25150200;
    ((GibIntGibIntGibCursorProd *) tailift238)->field2 = fltPkd185201;
    return tailift238;
}
GibCursor _copy_Node21_v86(GibCursor arg119151202)
{
    GibPackedTag tag239 = *(GibPackedTag *) arg119151202;
    GibCursor tail240 = arg119151202 + sizeof(GibInt);
    
    
  switch243:
    ;
    switch (tag239) {
        
      case 0:
        {
            GibPtr tailift241 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift241)->field0 = 0;
            return tailift241;
            break;
        }
        
      case 1:
        {
            GibInt x120152203 = ((GibIntGibCursorProd *) tail240)->field0;
            GibCursor x121153204 = ((GibIntGibCursorProd *) tail240)->field1;
            GibCursor y123155206 =  _copy_Node21_v86(x121153204);
            GibPtr tailift242 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift242)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift242)->field1 = x120152203;
            ((GibIntGibIntGibCursorProd *) tailift242)->field2 = y123155206;
            return tailift242;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag239");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Node21_v86(GibCursor arg124156207)
{
    GibPackedTag tag244 = *(GibPackedTag *) arg124156207;
    GibCursor tail245 = arg124156207 + sizeof(GibInt);
    
    
  switch248:
    ;
    switch (tag244) {
        
      case 0:
        {
            GibPtr tailift246 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift246)->field0 = 0;
            return tailift246;
            break;
        }
        
      case 1:
        {
            GibInt x125157208 = ((GibIntGibCursorProd *) tail245)->field0;
            GibCursor x126158209 = ((GibIntGibCursorProd *) tail245)->field1;
            GibCursor y128160211 =  _copy_without_ptrs_Node21_v86(x126158209);
            GibPtr tailift247 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift247)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift247)->field1 = x125157208;
            ((GibIntGibIntGibCursorProd *) tailift247)->field2 = y128160211;
            return tailift247;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag244");
            exit(1);
        }
    }
}
unsigned char _traverse_Node21_v86(GibCursor arg129161212)
{
    GibPackedTag tag249 = *(GibPackedTag *) arg129161212;
    GibCursor tail250 = arg129161212 + sizeof(GibInt);
    
    
  switch251:
    ;
    switch (tag249) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x130162213 = ((GibIntGibCursorProd *) tail250)->field0;
            GibCursor x131163214 = ((GibIntGibCursorProd *) tail250)->field1;
            unsigned char y133164215 =  _traverse_Node21_v86(x131163214);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag249");
            exit(1);
        }
    }
}
unsigned char _print_Node21_v86(GibCursor arg134165216)
{
    GibPackedTag tag252 = *(GibPackedTag *) arg134165216;
    GibCursor tail253 = arg134165216 + sizeof(GibInt);
    
    
  switch254:
    ;
    switch (tag252) {
        
      case 0:
        {
            unsigned char wildcard135166217 = gib_print_symbol(229);
            unsigned char wildcard136167218 = gib_print_symbol(228);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x137168219 = ((GibIntGibCursorProd *) tail253)->field0;
            GibCursor x138169220 = ((GibIntGibCursorProd *) tail253)->field1;
            unsigned char wildcard141170221 = gib_print_symbol(230);
            unsigned char wildcard144171222 = gib_print_symbol(231);
            unsigned char y139172223 = printf("%ld", x137168219);
            unsigned char wildcard143173224 = gib_print_symbol(231);
            unsigned char y140174225 =  _print_Node21_v86(x138169220);
            unsigned char wildcard142175226 = gib_print_symbol(228);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag252");
            exit(1);
        }
    }
}
int main(int argc, char **argv)
{
    int init0 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibCursor fltAppE180186 =  singleton1884(5);
    GibCursor fltAppE179187 =  insert2085(1, 4, fltAppE180186);
    GibCursor fltAppE178188 =  insert2085(2, 3, fltAppE179187);
    GibCursor fltAppE177189 =  insert2085(3, 2, fltAppE178188);
    GibCursor fltAppE176190 =  insert2085(4, 1, fltAppE177189);
    GibCursor tmp_app227 =  insert2085(5, 0, fltAppE176190);
    
     _print_Node21_v86(tmp_app227);
    printf("\n");
    
    int exit1 = gib_exit();
    
    return exit1;
}