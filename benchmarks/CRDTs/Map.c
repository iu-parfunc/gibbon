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
typedef struct GibIntGibIntGibIntGibIntGibCursorGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibInt field2;
            GibInt field3;
            GibCursor field4;
            GibCursor field5;
        } GibIntGibIntGibIntGibIntGibCursorGibCursorProd;
typedef struct GibIntGibIntGibIntGibCursorGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibInt field2;
            GibCursor field3;
            GibCursor field4;
        } GibIntGibIntGibIntGibCursorGibCursorProd;
typedef struct GibIntGibIntGibIntGibVectorGibCursorGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibInt field2;
            GibVector *field3;
            GibCursor field4;
            GibCursor field5;
        } GibIntGibIntGibIntGibVectorGibCursorGibCursorProd;
typedef struct GibIntGibIntGibVectorGibCursorGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibVector *field2;
            GibCursor field3;
            GibCursor field4;
        } GibIntGibIntGibVectorGibCursorGibCursorProd;
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
typedef struct GibVectorProd_struct {
            GibVector *field0;
        } GibVectorProd;
GibInt ratio232();
GibInt delta233();
GibCursor singleton244776(GibInt k44612571559, GibVector *x44712581560);
GibInt size242778(GibCursor m43212591563);
GibCursor singleL227784(GibInt k132912651569, GibVector *x133012661570,
                        GibCursor t133112671571, GibCursor m33212681572);
GibCursor doubleL225785(GibInt k130412741579, GibVector *x130512751580,
                        GibCursor t130612761581, GibCursor m030712771582);
GibCursor rotateL230779(GibInt k35412831588, GibVector *x35512841589,
                        GibCursor l35612851590, GibCursor r35712861591);
GibCursor bin228783(GibInt k33912921602, GibVector *x34012931603,
                    GibCursor l34112941604, GibCursor r34212951605);
GibCursor singleR226781(GibInt k131912961610, GibVector *x132012971611,
                        GibCursor m32112981612, GibCursor t332212991613);
GibCursor doubleR224782(GibInt k128913051620, GibVector *x129013061621,
                        GibCursor m029113071622, GibCursor t429213081623);
GibCursor rotateR229780(GibInt k34413141629, GibVector *x34513151630,
                        GibCursor l34613161631, GibCursor r34713171632);
GibCursor balance231777(GibInt k36413231643, GibVector *x36513241644,
                        GibCursor l36613251645, GibCursor r36713261646);
GibCursor insert238773(GibInt kx27413271667, GibVector *x27513281668,
                       GibCursor m27613291669);
GibCursor empty222772();
GibCursor _copy_Ord250(GibCursor arg110013351679);
GibCursor _copy_without_ptrs_Ord250(GibCursor arg110113361680);
unsigned char _traverse_Ord250(GibCursor arg110213371681);
unsigned char _print_Ord250(GibCursor arg110313381682);
GibCursor _copy_Map245_v775(GibCursor arg111213471691);
GibCursor _copy_without_ptrs_Map245_v775(GibCursor arg112313581702);
unsigned char _traverse_Map245_v775(GibCursor arg113413691713);
unsigned char _print_Map245_v775(GibCursor arg114513771721);
GibCursor _copy_Map245_v774(GibCursor arg116513971741);
GibCursor _copy_without_ptrs_Map245_v774(GibCursor arg117614081752);
unsigned char _traverse_Map245_v774(GibCursor arg118714191763);
unsigned char _print_Map245_v774(GibCursor arg119814271771);
GibCursor caseFn1218(GibInt k1304121914471791, GibVector *x1305122014481792,
                     GibCursor t1306122114491793, GibCursor m1312122214501794,
                     GibInt k2310122314511795, GibVector *x2311122414521796,
                     GibCursor t4313122514531797);
GibCursor caseFn1226(GibInt k1289122714591805, GibVector *x1290122814601806,
                     GibCursor t4292122914611807, GibCursor m1298123014621808,
                     GibInt k2295123114631809, GibVector *x2296123214641810,
                     GibCursor t1297123314651811);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Map245_v774_T,
            Map245_v775_T,
            Ord250_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(10);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[3];
    
    error = gib_info_table_insert_packed_dcon(Map245_v774_T, 1, 24, 2, 3, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map245_v774_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map245_v774_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map245_v774_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map245_v775_T, 1, 24, 2, 3, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map245_v775_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map245_v775_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map245_v775_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord250_T, 3, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord250_T, 3);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord250_T, 2, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord250_T, 2);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord250_T, 1, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord250_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord250_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord250_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(1820, "Vector");
    gib_add_symbol(1821, ")");
    gib_add_symbol(1822, "(Tip246_v775");
    gib_add_symbol(1823, "(Tip246_v774");
    gib_add_symbol(1824, "(Lt251");
    gib_add_symbol(1825, "(Gt252");
    gib_add_symbol(1826, "(Eq253");
    gib_add_symbol(1827, "(Cc254");
    gib_add_symbol(1828, "(Bin247_v775");
    gib_add_symbol(1829, "(Bin247_v774");
    gib_add_symbol(1830, " ");
}
GibInt ratio232()
{
    return 2;
}
GibInt delta233()
{
    return 4;
}
GibCursor singleton244776(GibInt k44612571559, GibVector *x44712581560)
{
    GibPtr fltPkd14851561 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd14851561)->field0 = 0;
    
    GibPtr fltPkd14861562 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd14861562)->field0 = 0;
    
    GibPtr tailift1831 =
           gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
    
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1831)->field0 =
        1;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1831)->field1 =
        1;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1831)->field2 =
        k44612571559;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1831)->field3 =
        x44712581560;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1831)->field4 =
        fltPkd14851561;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1831)->field5 =
        fltPkd14861562;
    return tailift1831;
}
GibInt size242778(GibCursor m43212591563)
{
    GibPackedTag tag1832 = *(GibPackedTag *) m43212591563;
    GibCursor tail1833 = m43212591563 + sizeof(GibInt);
    
    
  switch1834:
    ;
    switch (tag1832) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt sz43412601564 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1833)->field0;
            GibInt wildcard_1843512611565 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1833)->field1;
            GibVector *wildcard_1943612621566 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1833)->field2;
            GibCursor wildcard_2043712631567 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1833)->field3;
            GibCursor wildcard_2143812641568 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1833)->field4;
            
            return sz43412601564;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1832");
            exit(1);
        }
    }
}
GibCursor singleL227784(GibInt k132912651569, GibVector *x133012661570,
                        GibCursor t133112671571, GibCursor m33212681572)
{
    GibPackedTag tag1835 = *(GibPackedTag *) m33212681572;
    GibCursor tail1836 = m33212681572 + sizeof(GibInt);
    
    
  switch1837:
    ;
    switch (tag1835) {
        
      case 1:
        {
            GibInt wildcard_15933412691573 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1836)->field0;
            GibInt k233512701574 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1836)->field1;
            GibVector *x233612711575 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1836)->field2;
            GibCursor t233712721576 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1836)->field3;
            GibCursor t333812731577 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1836)->field4;
            GibCursor fltAppE14871578 =
                       bin228783(k132912651569, x133012661570, t133112671571, t233712721576);
            
            return bin228783(k233512701574, x233612711575, fltAppE14871578,
                             t333812731577);
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1835");
            exit(1);
        }
    }
}
GibCursor doubleL225785(GibInt k130412741579, GibVector *x130512751580,
                        GibCursor t130612761581, GibCursor m030712771582)
{
    GibPackedTag tag1838 = *(GibPackedTag *) m030712771582;
    GibCursor tail1839 = m030712771582 + sizeof(GibInt);
    
    
  switch1840:
    ;
    switch (tag1838) {
        
      case 1:
        {
            GibInt wildcard_17930912781583 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1839)->field0;
            GibInt k231012791584 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1839)->field1;
            GibVector *x231112801585 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1839)->field2;
            GibCursor m131212811586 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1839)->field3;
            GibCursor t431312821587 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1839)->field4;
            
            return caseFn1218(k130412741579, x130512751580, t130612761581,
                              m131212811586, k231012791584, x231112801585,
                              t431312821587);
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1838");
            exit(1);
        }
    }
}
GibCursor rotateL230779(GibInt k35412831588, GibVector *x35512841589,
                        GibCursor l35612851590, GibCursor r35712861591)
{
    GibPackedTag tag1841 = *(GibPackedTag *) r35712861591;
    GibCursor tail1842 = r35712861591 + sizeof(GibInt);
    
    
  switch1843:
    ;
    switch (tag1841) {
        
      case 1:
        {
            GibInt wildcard_13035912871592 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1842)->field0;
            GibInt wildcard_13136012881593 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1842)->field1;
            GibVector *wildcard_13236112891594 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1842)->field2;
            GibCursor ly36212901595 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1842)->field3;
            GibCursor ry36312911596 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1842)->field4;
            GibInt fltPrm14891597 =  size242778(ly36212901595);
            GibInt fltPrm14911598 =  ratio232();
            GibInt fltPrm14921599 =  size242778(ry36312911596);
            GibInt fltPrm14901600 = fltPrm14911598 * fltPrm14921599;
            GibBool fltIf14881601 = fltPrm14891597 < fltPrm14901600;
            
            if (fltIf14881601) {
                return singleL227784(k35412831588, x35512841589, l35612851590,
                                     r35712861591);
            } else {
                return doubleL225785(k35412831588, x35512841589, l35612851590,
                                     r35712861591);
            }
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1841");
            exit(1);
        }
    }
}
GibCursor bin228783(GibInt k33912921602, GibVector *x34012931603,
                    GibCursor l34112941604, GibCursor r34212951605)
{
    GibInt fltPrm14951606 =  size242778(l34112941604);
    GibInt fltPrm14961607 =  size242778(r34212951605);
    GibInt fltPrm14941608 = fltPrm14951606 + fltPrm14961607;
    GibInt fltPkd14931609 = fltPrm14941608 + 1;
    GibPtr tailift1844 =
           gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
    
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1844)->field0 =
        1;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1844)->field1 =
        fltPkd14931609;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1844)->field2 =
        k33912921602;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1844)->field3 =
        x34012931603;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1844)->field4 =
        l34112941604;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1844)->field5 =
        r34212951605;
    return tailift1844;
}
GibCursor singleR226781(GibInt k131912961610, GibVector *x132012971611,
                        GibCursor m32112981612, GibCursor t332212991613)
{
    GibPackedTag tag1845 = *(GibPackedTag *) m32112981612;
    GibCursor tail1846 = m32112981612 + sizeof(GibInt);
    
    
  switch1847:
    ;
    switch (tag1845) {
        
      case 1:
        {
            GibInt wildcard_16932413001614 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1846)->field0;
            GibInt k232513011615 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1846)->field1;
            GibVector *x232613021616 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1846)->field2;
            GibCursor t132713031617 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1846)->field3;
            GibCursor t232813041618 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1846)->field4;
            GibCursor fltAppE14971619 =
                       bin228783(k131912961610, x132012971611, t232813041618, t332212991613);
            
            return bin228783(k232513011615, x232613021616, t132713031617,
                             fltAppE14971619);
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1845");
            exit(1);
        }
    }
}
GibCursor doubleR224782(GibInt k128913051620, GibVector *x129013061621,
                        GibCursor m029113071622, GibCursor t429213081623)
{
    GibPackedTag tag1848 = *(GibPackedTag *) m029113071622;
    GibCursor tail1849 = m029113071622 + sizeof(GibInt);
    
    
  switch1850:
    ;
    switch (tag1848) {
        
      case 1:
        {
            GibInt wildcard_19529413091624 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1849)->field0;
            GibInt k229513101625 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1849)->field1;
            GibVector *x229613111626 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1849)->field2;
            GibCursor t129713121627 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1849)->field3;
            GibCursor m129813131628 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1849)->field4;
            
            return caseFn1226(k128913051620, x129013061621, t429213081623,
                              m129813131628, k229513101625, x229613111626,
                              t129713121627);
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1848");
            exit(1);
        }
    }
}
GibCursor rotateR229780(GibInt k34413141629, GibVector *x34513151630,
                        GibCursor l34613161631, GibCursor r34713171632)
{
    GibPackedTag tag1851 = *(GibPackedTag *) l34613161631;
    GibCursor tail1852 = l34613161631 + sizeof(GibInt);
    
    
  switch1853:
    ;
    switch (tag1851) {
        
      case 1:
        {
            GibInt wildcard_14234913181633 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1852)->field0;
            GibInt wildcard_14335013191634 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1852)->field1;
            GibVector *wildcard_14435113201635 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1852)->field2;
            GibCursor ly35213211636 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1852)->field3;
            GibCursor ry35313221637 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1852)->field4;
            GibInt fltPrm14991638 =  size242778(ry35313221637);
            GibInt fltPrm15011639 =  ratio232();
            GibInt fltPrm15021640 =  size242778(ly35213211636);
            GibInt fltPrm15001641 = fltPrm15011639 * fltPrm15021640;
            GibBool fltIf14981642 = fltPrm14991638 < fltPrm15001641;
            
            if (fltIf14981642) {
                return singleR226781(k34413141629, x34513151630, l34613161631,
                                     r34713171632);
            } else {
                return doubleR224782(k34413141629, x34513151630, l34613161631,
                                     r34713171632);
            }
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1851");
            exit(1);
        }
    }
}
GibCursor balance231777(GibInt k36413231643, GibVector *x36513241644,
                        GibCursor l36613251645, GibCursor r36713261646)
{
    GibInt fltPrm15051647 =  size242778(l36613251645);
    GibInt fltPrm15061648 =  size242778(r36713261646);
    GibInt fltPrm15041649 = fltPrm15051647 + fltPrm15061648;
    GibBool fltIf15031650 = fltPrm15041649 <= 1;
    
    if (fltIf15031650) {
        GibInt fltPrm15081651 =  size242778(l36613251645);
        GibInt fltPrm15091652 =  size242778(r36713261646);
        GibInt fltPkd15071653 = fltPrm15081651 + fltPrm15091652;
        GibPtr tailift1854 =
               gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
        
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1854)->field0 =
            1;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1854)->field1 =
            fltPkd15071653;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1854)->field2 =
            k36413231643;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1854)->field3 =
            x36513241644;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1854)->field4 =
            l36613251645;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1854)->field5 =
            r36713261646;
        return tailift1854;
    } else {
        GibInt fltPrm15111654 =  size242778(r36713261646);
        GibInt fltPrm15131655 =  delta233();
        GibInt fltPrm15141656 =  size242778(l36613251645);
        GibInt fltPrm15121657 = fltPrm15131655 * fltPrm15141656;
        GibBool fltIf15101658 = fltPrm15111654 >= fltPrm15121657;
        
        if (fltIf15101658) {
            return rotateL230779(k36413231643, x36513241644, l36613251645,
                                 r36713261646);
        } else {
            GibInt fltPrm15161659 =  size242778(l36613251645);
            GibInt fltPrm15181660 =  delta233();
            GibInt fltPrm15191661 =  size242778(r36713261646);
            GibInt fltPrm15171662 = fltPrm15181660 * fltPrm15191661;
            GibBool fltIf15151663 = fltPrm15161659 >= fltPrm15171662;
            
            if (fltIf15151663) {
                return rotateR229780(k36413231643, x36513241644, l36613251645,
                                     r36713261646);
            } else {
                GibInt fltPrm15211664 =  size242778(l36613251645);
                GibInt fltPrm15221665 =  size242778(r36713261646);
                GibInt fltPkd15201666 = fltPrm15211664 + fltPrm15221665;
                GibPtr tailift1855 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1855)->field0 =
                    1;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1855)->field1 =
                    fltPkd15201666;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1855)->field2 =
                    k36413231643;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1855)->field3 =
                    x36513241644;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1855)->field4 =
                    l36613251645;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1855)->field5 =
                    r36713261646;
                return tailift1855;
            }
        }
    }
}
GibCursor insert238773(GibInt kx27413271667, GibVector *x27513281668,
                       GibCursor m27613291669)
{
    GibPackedTag tag1856 = *(GibPackedTag *) m27613291669;
    GibCursor tail1857 = m27613291669 + sizeof(GibInt);
    
    
  switch1859:
    ;
    switch (tag1856) {
        
      case 0:
        {
            return singleton244776(kx27413271667, x27513281668);
            break;
        }
        
      case 1:
        {
            GibInt sz27813301670 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1857)->field0;
            GibInt k27913311671 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1857)->field1;
            GibVector *v28013321672 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1857)->field2;
            GibCursor l28113331673 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1857)->field3;
            GibCursor r28213341674 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1857)->field4;
            GibBool fltIf15231675 = kx27413271667 == k27913311671;
            
            if (fltIf15231675) {
                GibPtr tailift1858 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1858)->field0 =
                    1;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1858)->field1 =
                    sz27813301670;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1858)->field2 =
                    k27913311671;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1858)->field3 =
                    x27513281668;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1858)->field4 =
                    l28113331673;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1858)->field5 =
                    r28213341674;
                return tailift1858;
            } else {
                GibBool fltIf15241676 = kx27413271667 <= k27913311671;
                
                if (fltIf15241676) {
                    GibCursor fltAppE15251677 =
                               insert238773(kx27413271667, x27513281668, l28113331673);
                    
                    return balance231777(k27913311671, v28013321672,
                                         fltAppE15251677, r28213341674);
                } else {
                    GibCursor fltAppE15261678 =
                               insert238773(kx27413271667, x27513281668, r28213341674);
                    
                    return balance231777(k27913311671, v28013321672,
                                         l28113331673, fltAppE15261678);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1856");
            exit(1);
        }
    }
}
GibCursor empty222772()
{
    GibPtr tailift1860 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) tailift1860)->field0 = 0;
    return tailift1860;
}
GibCursor _copy_Ord250(GibCursor arg110013351679)
{
    GibPackedTag tag1861 = *(GibPackedTag *) arg110013351679;
    GibCursor tail1862 = arg110013351679 + sizeof(GibInt);
    
    
  switch1867:
    ;
    switch (tag1861) {
        
      case 0:
        {
            GibPtr tailift1863 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1863)->field0 = 0;
            return tailift1863;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1864 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1864)->field0 = 1;
            return tailift1864;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1865 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1865)->field0 = 2;
            return tailift1865;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1866 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1866)->field0 = 3;
            return tailift1866;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1861");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Ord250(GibCursor arg110113361680)
{
    GibPackedTag tag1868 = *(GibPackedTag *) arg110113361680;
    GibCursor tail1869 = arg110113361680 + sizeof(GibInt);
    
    
  switch1874:
    ;
    switch (tag1868) {
        
      case 0:
        {
            GibPtr tailift1870 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1870)->field0 = 0;
            return tailift1870;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1871 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1871)->field0 = 1;
            return tailift1871;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1872 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1872)->field0 = 2;
            return tailift1872;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1873 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1873)->field0 = 3;
            return tailift1873;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1868");
            exit(1);
        }
    }
}
unsigned char _traverse_Ord250(GibCursor arg110213371681)
{
    GibPackedTag tag1875 = *(GibPackedTag *) arg110213371681;
    GibCursor tail1876 = arg110213371681 + sizeof(GibInt);
    
    
  switch1877:
    ;
    switch (tag1875) {
        
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
            printf("%s\n", "Unknown tag in: tag1875");
            exit(1);
        }
    }
}
unsigned char _print_Ord250(GibCursor arg110313381682)
{
    GibPackedTag tag1878 = *(GibPackedTag *) arg110313381682;
    GibCursor tail1879 = arg110313381682 + sizeof(GibInt);
    
    
  switch1880:
    ;
    switch (tag1878) {
        
      case 0:
        {
            unsigned char wildcard110413391683 = gib_print_symbol(1824);
            unsigned char wildcard110513401684 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard110613411685 = gib_print_symbol(1825);
            unsigned char wildcard110713421686 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard110813431687 = gib_print_symbol(1826);
            unsigned char wildcard110913441688 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard111013451689 = gib_print_symbol(1827);
            unsigned char wildcard111113461690 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1878");
            exit(1);
        }
    }
}
GibCursor _copy_Map245_v775(GibCursor arg111213471691)
{
    GibPackedTag tag1881 = *(GibPackedTag *) arg111213471691;
    GibCursor tail1882 = arg111213471691 + sizeof(GibInt);
    
    
  switch1885:
    ;
    switch (tag1881) {
        
      case 0:
        {
            GibPtr tailift1883 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1883)->field0 = 0;
            return tailift1883;
            break;
        }
        
      case 1:
        {
            GibInt x111313481692 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1882)->field0;
            GibInt x111413491693 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1882)->field1;
            GibVector *x111513501694 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1882)->field2;
            GibCursor x111613511695 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1882)->field3;
            GibCursor x111713521696 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1882)->field4;
            GibCursor y112113561700 =  _copy_Map245_v775(x111613511695);
            GibCursor y112213571701 =  _copy_Map245_v775(x111713521696);
            GibPtr tailift1884 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1884)->field0 =
                1;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1884)->field1 =
                x111313481692;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1884)->field2 =
                x111413491693;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1884)->field3 =
                x111513501694;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1884)->field4 =
                y112113561700;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1884)->field5 =
                y112213571701;
            return tailift1884;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1881");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Map245_v775(GibCursor arg112313581702)
{
    GibPackedTag tag1886 = *(GibPackedTag *) arg112313581702;
    GibCursor tail1887 = arg112313581702 + sizeof(GibInt);
    
    
  switch1890:
    ;
    switch (tag1886) {
        
      case 0:
        {
            GibPtr tailift1888 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1888)->field0 = 0;
            return tailift1888;
            break;
        }
        
      case 1:
        {
            GibInt x112413591703 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1887)->field0;
            GibInt x112513601704 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1887)->field1;
            GibVector *x112613611705 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1887)->field2;
            GibCursor x112713621706 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1887)->field3;
            GibCursor x112813631707 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1887)->field4;
            GibCursor y113213671711 =
                       _copy_without_ptrs_Map245_v775(x112713621706);
            GibCursor y113313681712 =
                       _copy_without_ptrs_Map245_v775(x112813631707);
            GibPtr tailift1889 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1889)->field0 =
                1;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1889)->field1 =
                x112413591703;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1889)->field2 =
                x112513601704;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1889)->field3 =
                x112613611705;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1889)->field4 =
                y113213671711;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1889)->field5 =
                y113313681712;
            return tailift1889;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1886");
            exit(1);
        }
    }
}
unsigned char _traverse_Map245_v775(GibCursor arg113413691713)
{
    GibPackedTag tag1891 = *(GibPackedTag *) arg113413691713;
    GibCursor tail1892 = arg113413691713 + sizeof(GibInt);
    
    
  switch1893:
    ;
    switch (tag1891) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x113513701714 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1892)->field0;
            GibInt x113613711715 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1892)->field1;
            GibVector *x113713721716 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1892)->field2;
            GibCursor x113813731717 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1892)->field3;
            GibCursor x113913741718 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1892)->field4;
            unsigned char y114313751719 =  _traverse_Map245_v775(x113813731717);
            unsigned char y114413761720 =  _traverse_Map245_v775(x113913741718);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1891");
            exit(1);
        }
    }
}
unsigned char _print_Map245_v775(GibCursor arg114513771721)
{
    GibPackedTag tag1894 = *(GibPackedTag *) arg114513771721;
    GibCursor tail1895 = arg114513771721 + sizeof(GibInt);
    
    
  switch1896:
    ;
    switch (tag1894) {
        
      case 0:
        {
            unsigned char wildcard114613781722 = gib_print_symbol(1822);
            unsigned char wildcard114713791723 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x114813801724 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1895)->field0;
            GibInt x114913811725 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1895)->field1;
            GibVector *x115013821726 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1895)->field2;
            GibCursor x115113831727 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1895)->field3;
            GibCursor x115213841728 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1895)->field4;
            unsigned char wildcard115813851729 = gib_print_symbol(1828);
            unsigned char wildcard116413861730 = gib_print_symbol(1830);
            unsigned char y115313871731 = printf("%ld", x114813801724);
            unsigned char wildcard116313881732 = gib_print_symbol(1830);
            unsigned char y115413891733 = printf("%ld", x114913811725);
            unsigned char wildcard116213901734 = gib_print_symbol(1830);
            unsigned char y115513911735 = gib_print_symbol(1820);
            unsigned char wildcard116113921736 = gib_print_symbol(1830);
            unsigned char y115613931737 =  _print_Map245_v775(x115113831727);
            unsigned char wildcard116013941738 = gib_print_symbol(1830);
            unsigned char y115713951739 =  _print_Map245_v775(x115213841728);
            unsigned char wildcard115913961740 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1894");
            exit(1);
        }
    }
}
GibCursor _copy_Map245_v774(GibCursor arg116513971741)
{
    GibPackedTag tag1897 = *(GibPackedTag *) arg116513971741;
    GibCursor tail1898 = arg116513971741 + sizeof(GibInt);
    
    
  switch1901:
    ;
    switch (tag1897) {
        
      case 0:
        {
            GibPtr tailift1899 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1899)->field0 = 0;
            return tailift1899;
            break;
        }
        
      case 1:
        {
            GibInt x116613981742 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1898)->field0;
            GibInt x116713991743 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1898)->field1;
            GibInt x116814001744 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1898)->field2;
            GibCursor x116914011745 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1898)->field3;
            GibCursor x117014021746 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1898)->field4;
            GibCursor y117414061750 =  _copy_Map245_v774(x116914011745);
            GibCursor y117514071751 =  _copy_Map245_v774(x117014021746);
            GibPtr tailift1900 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1900)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1900)->field1 =
                x116613981742;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1900)->field2 =
                x116713991743;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1900)->field3 =
                x116814001744;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1900)->field4 =
                y117414061750;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1900)->field5 =
                y117514071751;
            return tailift1900;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1897");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Map245_v774(GibCursor arg117614081752)
{
    GibPackedTag tag1902 = *(GibPackedTag *) arg117614081752;
    GibCursor tail1903 = arg117614081752 + sizeof(GibInt);
    
    
  switch1906:
    ;
    switch (tag1902) {
        
      case 0:
        {
            GibPtr tailift1904 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1904)->field0 = 0;
            return tailift1904;
            break;
        }
        
      case 1:
        {
            GibInt x117714091753 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1903)->field0;
            GibInt x117814101754 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1903)->field1;
            GibInt x117914111755 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1903)->field2;
            GibCursor x118014121756 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1903)->field3;
            GibCursor x118114131757 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1903)->field4;
            GibCursor y118514171761 =
                       _copy_without_ptrs_Map245_v774(x118014121756);
            GibCursor y118614181762 =
                       _copy_without_ptrs_Map245_v774(x118114131757);
            GibPtr tailift1905 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1905)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1905)->field1 =
                x117714091753;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1905)->field2 =
                x117814101754;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1905)->field3 =
                x117914111755;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1905)->field4 =
                y118514171761;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1905)->field5 =
                y118614181762;
            return tailift1905;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1902");
            exit(1);
        }
    }
}
unsigned char _traverse_Map245_v774(GibCursor arg118714191763)
{
    GibPackedTag tag1907 = *(GibPackedTag *) arg118714191763;
    GibCursor tail1908 = arg118714191763 + sizeof(GibInt);
    
    
  switch1909:
    ;
    switch (tag1907) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x118814201764 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1908)->field0;
            GibInt x118914211765 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1908)->field1;
            GibInt x119014221766 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1908)->field2;
            GibCursor x119114231767 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1908)->field3;
            GibCursor x119214241768 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1908)->field4;
            unsigned char y119614251769 =  _traverse_Map245_v774(x119114231767);
            unsigned char y119714261770 =  _traverse_Map245_v774(x119214241768);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1907");
            exit(1);
        }
    }
}
unsigned char _print_Map245_v774(GibCursor arg119814271771)
{
    GibPackedTag tag1910 = *(GibPackedTag *) arg119814271771;
    GibCursor tail1911 = arg119814271771 + sizeof(GibInt);
    
    
  switch1912:
    ;
    switch (tag1910) {
        
      case 0:
        {
            unsigned char wildcard119914281772 = gib_print_symbol(1823);
            unsigned char wildcard120014291773 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x120114301774 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1911)->field0;
            GibInt x120214311775 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1911)->field1;
            GibInt x120314321776 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1911)->field2;
            GibCursor x120414331777 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1911)->field3;
            GibCursor x120514341778 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1911)->field4;
            unsigned char wildcard121114351779 = gib_print_symbol(1829);
            unsigned char wildcard121714361780 = gib_print_symbol(1830);
            unsigned char y120614371781 = printf("%ld", x120114301774);
            unsigned char wildcard121614381782 = gib_print_symbol(1830);
            unsigned char y120714391783 = printf("%ld", x120214311775);
            unsigned char wildcard121514401784 = gib_print_symbol(1830);
            unsigned char y120814411785 = printf("%ld", x120314321776);
            unsigned char wildcard121414421786 = gib_print_symbol(1830);
            unsigned char y120914431787 =  _print_Map245_v774(x120414331777);
            unsigned char wildcard121314441788 = gib_print_symbol(1830);
            unsigned char y121014451789 =  _print_Map245_v774(x120514341778);
            unsigned char wildcard121214461790 = gib_print_symbol(1821);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1910");
            exit(1);
        }
    }
}
GibCursor caseFn1218(GibInt k1304121914471791, GibVector *x1305122014481792,
                     GibCursor t1306122114491793, GibCursor m1312122214501794,
                     GibInt k2310122314511795, GibVector *x2311122414521796,
                     GibCursor t4313122514531797)
{
    GibPackedTag tag1913 = *(GibPackedTag *) m1312122214501794;
    GibCursor tail1914 = m1312122214501794 + sizeof(GibInt);
    
    
  switch1915:
    ;
    switch (tag1913) {
        
      case 1:
        {
            GibInt wildcard_18031414541798 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1914)->field0;
            GibInt k331514551799 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1914)->field1;
            GibVector *x331614561800 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1914)->field2;
            GibCursor t231714571801 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1914)->field3;
            GibCursor t331814581802 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1914)->field4;
            GibCursor fltAppE15271803 =
                       bin228783(k1304121914471791, x1305122014481792, t1306122114491793, t231714571801);
            GibCursor fltAppE15281804 =
                       bin228783(k2310122314511795, x2311122414521796, t331814581802, t4313122514531797);
            
            return bin228783(k331514551799, x331614561800, fltAppE15271803,
                             fltAppE15281804);
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1913");
            exit(1);
        }
    }
}
GibCursor caseFn1226(GibInt k1289122714591805, GibVector *x1290122814601806,
                     GibCursor t4292122914611807, GibCursor m1298123014621808,
                     GibInt k2295123114631809, GibVector *x2296123214641810,
                     GibCursor t1297123314651811)
{
    GibPackedTag tag1916 = *(GibPackedTag *) m1298123014621808;
    GibCursor tail1917 = m1298123014621808 + sizeof(GibInt);
    
    
  switch1918:
    ;
    switch (tag1916) {
        
      case 1:
        {
            GibInt wildcard_19629914661812 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1917)->field0;
            GibInt k330014671813 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1917)->field1;
            GibVector *x330114681814 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1917)->field2;
            GibCursor t230214691815 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1917)->field3;
            GibCursor t330314701816 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1917)->field4;
            GibCursor fltAppE15291817 =
                       bin228783(k2295123114631809, x2296123214641810, t1297123314651811, t230214691815);
            GibCursor fltAppE15301818 =
                       bin228783(k1289122714591805, x1290122814601806, t330314701816, t4292122914611807);
            
            return bin228783(k330014671813, x330114681814, fltAppE15291817,
                             fltAppE15301818);
            break;
        }
        
      case 0:
        {
            return empty222772();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1916");
            exit(1);
        }
    }
}
int main(int argc, char **argv)
{
    int init14 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibInt tmp13 = sizeof(GibChar);
    GibVector *vec21425712341531 = gib_vector_alloc(1, tmp13);
    GibChar tmp12 = 'f';
    GibVector *_25812351532 = gib_vector_inplace_update(vec21425712341531, 0,
                                                        &tmp12);
    GibInt tmp11 = sizeof(GibChar);
    GibVector *vec21525912361534 = gib_vector_alloc(1, tmp11);
    GibChar tmp10 = 'f';
    GibVector *_26012371535 = gib_vector_inplace_update(vec21525912361534, 0,
                                                        &tmp10);
    GibInt tmp9 = sizeof(GibChar);
    GibVector *vec21626112381537 = gib_vector_alloc(1, tmp9);
    GibChar tmp8 = 'e';
    GibVector *_26212391538 = gib_vector_inplace_update(vec21626112381537, 0,
                                                        &tmp8);
    GibInt tmp7 = sizeof(GibChar);
    GibVector *vec21726312401540 = gib_vector_alloc(1, tmp7);
    GibChar tmp6 = 'd';
    GibVector *_26412411541 = gib_vector_inplace_update(vec21726312401540, 0,
                                                        &tmp6);
    GibInt tmp5 = sizeof(GibChar);
    GibVector *vec21826512421543 = gib_vector_alloc(1, tmp5);
    GibChar tmp4 = 'c';
    GibVector *_26612431544 = gib_vector_inplace_update(vec21826512421543, 0,
                                                        &tmp4);
    GibInt tmp3 = sizeof(GibChar);
    GibVector *vec21926712441546 = gib_vector_alloc(1, tmp3);
    GibChar tmp2 = 'b';
    GibVector *_26812451547 = gib_vector_inplace_update(vec21926712441546, 0,
                                                        &tmp2);
    GibInt tmp1 = sizeof(GibChar);
    GibVector *vec22026912461549 = gib_vector_alloc(1, tmp1);
    GibChar tmp0 = 'a';
    GibVector *_27012471550 = gib_vector_inplace_update(vec22026912461549, 0,
                                                        &tmp0);
    GibCursor fltAppE14841552 =  empty222772();
    GibCursor fltAppE14821553 =
               insert238773(0, vec22026912461549, fltAppE14841552);
    GibCursor fltAppE14801554 =
               insert238773(1, vec21926712441546, fltAppE14821553);
    GibCursor fltAppE14781555 =
               insert238773(2, vec21826512421543, fltAppE14801554);
    GibCursor fltAppE14761556 =
               insert238773(3, vec21726312401540, fltAppE14781555);
    GibCursor fltAppE14741557 =
               insert238773(4, vec21626112381537, fltAppE14761556);
    GibCursor fltAppE14721558 =
               insert238773(5, vec21525912361534, fltAppE14741557);
    GibCursor tmp_app1819 =
               insert238773(0, vec21425712341531, fltAppE14721558);
    
     _print_Map245_v775(tmp_app1819);
    printf("\n");
    
    int exit15 = gib_exit();
    
    return exit15;
}