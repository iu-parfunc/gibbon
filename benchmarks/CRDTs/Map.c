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
GibInt ratio224();
GibInt delta225();
GibCursor singleton236604(GibInt k43210201272, GibVector *x43310211273);
GibInt size234606(GibCursor m41810221276);
GibCursor singleL219612(GibInt k131510281282, GibVector *x131610291283,
                        GibCursor t131710301284, GibCursor m31810311285);
GibCursor doubleL217613(GibInt k129010371292, GibVector *x129110381293,
                        GibCursor t129210391294, GibCursor m029310401295);
GibCursor rotateL222607(GibInt k34010461301, GibVector *x34110471302,
                        GibCursor l34210481303, GibCursor r34310491304);
GibCursor bin220611(GibInt k32510551315, GibVector *x32610561316,
                    GibCursor l32710571317, GibCursor r32810581318);
GibCursor singleR218609(GibInt k130510591323, GibVector *x130610601324,
                        GibCursor m30710611325, GibCursor t330810621326);
GibCursor doubleR216610(GibInt k127510681333, GibVector *x127610691334,
                        GibCursor m027710701335, GibCursor t427810711336);
GibCursor rotateR221608(GibInt k33010771342, GibVector *x33110781343,
                        GibCursor l33210791344, GibCursor r33310801345);
GibCursor balance223605(GibInt k35010861356, GibVector *x35110871357,
                        GibCursor l35210881358, GibCursor r35310891359);
GibCursor insert230602(GibInt kx26610901380, GibVector *x26710911381,
                       GibCursor m26810921382);
GibCursor empty215601();
GibCursor _copy_Ord242(GibCursor arg92210981392);
GibCursor _copy_without_ptrs_Ord242(GibCursor arg92310991393);
unsigned char _traverse_Ord242(GibCursor arg92411001394);
unsigned char _print_Ord242(GibCursor arg92511011395);
GibCursor _copy_Map237_v603(GibCursor arg93411101404);
GibCursor _copy_without_ptrs_Map237_v603(GibCursor arg94511211415);
unsigned char _traverse_Map237_v603(GibCursor arg95611321426);
unsigned char _print_Map237_v603(GibCursor arg96711401434);
GibCursor caseFn987(GibInt k129098811601454, GibVector *x129198911611455,
                    GibCursor t129299011621456, GibCursor m129899111631457,
                    GibInt k229699211641458, GibVector *x229799311651459,
                    GibCursor t429999411661460);
GibCursor caseFn995(GibInt k127599611721468, GibVector *x127699711731469,
                    GibCursor t427899811741470, GibCursor m128499911751471,
                    GibInt k2281100011761472, GibVector *x2282100111771473,
                    GibCursor t1283100211781474);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Map237_v603_T,
            Ord242_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(9);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[3];
    
    error = gib_info_table_insert_packed_dcon(Map237_v603_T, 1, 24, 2, 3, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map237_v603_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map237_v603_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map237_v603_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord242_T, 3, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord242_T, 3);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord242_T, 2, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord242_T, 2);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord242_T, 1, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord242_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord242_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord242_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(1483, "Vector");
    gib_add_symbol(1484, ")");
    gib_add_symbol(1485, "(Tip238_v603");
    gib_add_symbol(1486, "(Lt243");
    gib_add_symbol(1487, "(Gt244");
    gib_add_symbol(1488, "(Eq245");
    gib_add_symbol(1489, "(Cc246");
    gib_add_symbol(1490, "(Bin239_v603");
    gib_add_symbol(1491, " ");
}
GibInt ratio224()
{
    return 2;
}
GibInt delta225()
{
    return 4;
}
GibCursor singleton236604(GibInt k43210201272, GibVector *x43310211273)
{
    GibPtr fltPkd11981274 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd11981274)->field0 = 0;
    
    GibPtr fltPkd11991275 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd11991275)->field0 = 0;
    
    GibPtr tailift1492 =
           gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
    
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1492)->field0 =
        1;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1492)->field1 =
        1;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1492)->field2 =
        k43210201272;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1492)->field3 =
        x43310211273;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1492)->field4 =
        fltPkd11981274;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1492)->field5 =
        fltPkd11991275;
    return tailift1492;
}
GibInt size234606(GibCursor m41810221276)
{
    GibPackedTag tag1493 = *(GibPackedTag *) m41810221276;
    GibCursor tail1494 = m41810221276 + sizeof(GibInt);
    
    
  switch1495:
    ;
    switch (tag1493) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt sz42010231277 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1494)->field0;
            GibInt wildcard_1842110241278 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1494)->field1;
            GibVector *wildcard_1942210251279 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1494)->field2;
            GibCursor wildcard_2042310261280 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1494)->field3;
            GibCursor wildcard_2142410271281 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1494)->field4;
            
            return sz42010231277;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1493");
            exit(1);
        }
    }
}
GibCursor singleL219612(GibInt k131510281282, GibVector *x131610291283,
                        GibCursor t131710301284, GibCursor m31810311285)
{
    GibPackedTag tag1496 = *(GibPackedTag *) m31810311285;
    GibCursor tail1497 = m31810311285 + sizeof(GibInt);
    
    
  switch1498:
    ;
    switch (tag1496) {
        
      case 1:
        {
            GibInt wildcard_15932010321286 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1497)->field0;
            GibInt k232110331287 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1497)->field1;
            GibVector *x232210341288 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1497)->field2;
            GibCursor t232310351289 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1497)->field3;
            GibCursor t332410361290 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1497)->field4;
            GibCursor fltAppE12001291 =
                       bin220611(k131510281282, x131610291283, t131710301284, t232310351289);
            
            return bin220611(k232110331287, x232210341288, fltAppE12001291,
                             t332410361290);
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1496");
            exit(1);
        }
    }
}
GibCursor doubleL217613(GibInt k129010371292, GibVector *x129110381293,
                        GibCursor t129210391294, GibCursor m029310401295)
{
    GibPackedTag tag1499 = *(GibPackedTag *) m029310401295;
    GibCursor tail1500 = m029310401295 + sizeof(GibInt);
    
    
  switch1501:
    ;
    switch (tag1499) {
        
      case 1:
        {
            GibInt wildcard_17929510411296 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1500)->field0;
            GibInt k229610421297 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1500)->field1;
            GibVector *x229710431298 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1500)->field2;
            GibCursor m129810441299 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1500)->field3;
            GibCursor t429910451300 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1500)->field4;
            
            return caseFn987(k129010371292, x129110381293, t129210391294,
                             m129810441299, k229610421297, x229710431298,
                             t429910451300);
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1499");
            exit(1);
        }
    }
}
GibCursor rotateL222607(GibInt k34010461301, GibVector *x34110471302,
                        GibCursor l34210481303, GibCursor r34310491304)
{
    GibPackedTag tag1502 = *(GibPackedTag *) r34310491304;
    GibCursor tail1503 = r34310491304 + sizeof(GibInt);
    
    
  switch1504:
    ;
    switch (tag1502) {
        
      case 1:
        {
            GibInt wildcard_13034510501305 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1503)->field0;
            GibInt wildcard_13134610511306 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1503)->field1;
            GibVector *wildcard_13234710521307 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1503)->field2;
            GibCursor ly34810531308 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1503)->field3;
            GibCursor ry34910541309 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1503)->field4;
            GibInt fltPrm12021310 =  size234606(ly34810531308);
            GibInt fltPrm12041311 =  ratio224();
            GibInt fltPrm12051312 =  size234606(ry34910541309);
            GibInt fltPrm12031313 = fltPrm12041311 * fltPrm12051312;
            GibBool fltIf12011314 = fltPrm12021310 < fltPrm12031313;
            
            if (fltIf12011314) {
                return singleL219612(k34010461301, x34110471302, l34210481303,
                                     r34310491304);
            } else {
                return doubleL217613(k34010461301, x34110471302, l34210481303,
                                     r34310491304);
            }
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1502");
            exit(1);
        }
    }
}
GibCursor bin220611(GibInt k32510551315, GibVector *x32610561316,
                    GibCursor l32710571317, GibCursor r32810581318)
{
    GibInt fltPrm12081319 =  size234606(l32710571317);
    GibInt fltPrm12091320 =  size234606(r32810581318);
    GibInt fltPrm12071321 = fltPrm12081319 + fltPrm12091320;
    GibInt fltPkd12061322 = fltPrm12071321 + 1;
    GibPtr tailift1505 =
           gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
    
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1505)->field0 =
        1;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1505)->field1 =
        fltPkd12061322;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1505)->field2 =
        k32510551315;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1505)->field3 =
        x32610561316;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1505)->field4 =
        l32710571317;
    ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1505)->field5 =
        r32810581318;
    return tailift1505;
}
GibCursor singleR218609(GibInt k130510591323, GibVector *x130610601324,
                        GibCursor m30710611325, GibCursor t330810621326)
{
    GibPackedTag tag1506 = *(GibPackedTag *) m30710611325;
    GibCursor tail1507 = m30710611325 + sizeof(GibInt);
    
    
  switch1508:
    ;
    switch (tag1506) {
        
      case 1:
        {
            GibInt wildcard_16931010631327 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1507)->field0;
            GibInt k231110641328 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1507)->field1;
            GibVector *x231210651329 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1507)->field2;
            GibCursor t131310661330 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1507)->field3;
            GibCursor t231410671331 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1507)->field4;
            GibCursor fltAppE12101332 =
                       bin220611(k130510591323, x130610601324, t231410671331, t330810621326);
            
            return bin220611(k231110641328, x231210651329, t131310661330,
                             fltAppE12101332);
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1506");
            exit(1);
        }
    }
}
GibCursor doubleR216610(GibInt k127510681333, GibVector *x127610691334,
                        GibCursor m027710701335, GibCursor t427810711336)
{
    GibPackedTag tag1509 = *(GibPackedTag *) m027710701335;
    GibCursor tail1510 = m027710701335 + sizeof(GibInt);
    
    
  switch1511:
    ;
    switch (tag1509) {
        
      case 1:
        {
            GibInt wildcard_19528010721337 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1510)->field0;
            GibInt k228110731338 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1510)->field1;
            GibVector *x228210741339 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1510)->field2;
            GibCursor t128310751340 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1510)->field3;
            GibCursor m128410761341 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1510)->field4;
            
            return caseFn995(k127510681333, x127610691334, t427810711336,
                             m128410761341, k228110731338, x228210741339,
                             t128310751340);
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1509");
            exit(1);
        }
    }
}
GibCursor rotateR221608(GibInt k33010771342, GibVector *x33110781343,
                        GibCursor l33210791344, GibCursor r33310801345)
{
    GibPackedTag tag1512 = *(GibPackedTag *) l33210791344;
    GibCursor tail1513 = l33210791344 + sizeof(GibInt);
    
    
  switch1514:
    ;
    switch (tag1512) {
        
      case 1:
        {
            GibInt wildcard_14233510811346 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1513)->field0;
            GibInt wildcard_14333610821347 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1513)->field1;
            GibVector *wildcard_14433710831348 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1513)->field2;
            GibCursor ly33810841349 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1513)->field3;
            GibCursor ry33910851350 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1513)->field4;
            GibInt fltPrm12121351 =  size234606(ry33910851350);
            GibInt fltPrm12141352 =  ratio224();
            GibInt fltPrm12151353 =  size234606(ly33810841349);
            GibInt fltPrm12131354 = fltPrm12141352 * fltPrm12151353;
            GibBool fltIf12111355 = fltPrm12121351 < fltPrm12131354;
            
            if (fltIf12111355) {
                return singleR218609(k33010771342, x33110781343, l33210791344,
                                     r33310801345);
            } else {
                return doubleR216610(k33010771342, x33110781343, l33210791344,
                                     r33310801345);
            }
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1512");
            exit(1);
        }
    }
}
GibCursor balance223605(GibInt k35010861356, GibVector *x35110871357,
                        GibCursor l35210881358, GibCursor r35310891359)
{
    GibInt fltPrm12181360 =  size234606(l35210881358);
    GibInt fltPrm12191361 =  size234606(r35310891359);
    GibInt fltPrm12171362 = fltPrm12181360 + fltPrm12191361;
    GibBool fltIf12161363 = fltPrm12171362 <= 1;
    
    if (fltIf12161363) {
        GibInt fltPrm12211364 =  size234606(l35210881358);
        GibInt fltPrm12221365 =  size234606(r35310891359);
        GibInt fltPkd12201366 = fltPrm12211364 + fltPrm12221365;
        GibPtr tailift1515 =
               gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
        
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1515)->field0 =
            1;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1515)->field1 =
            fltPkd12201366;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1515)->field2 =
            k35010861356;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1515)->field3 =
            x35110871357;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1515)->field4 =
            l35210881358;
        ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1515)->field5 =
            r35310891359;
        return tailift1515;
    } else {
        GibInt fltPrm12241367 =  size234606(r35310891359);
        GibInt fltPrm12261368 =  delta225();
        GibInt fltPrm12271369 =  size234606(l35210881358);
        GibInt fltPrm12251370 = fltPrm12261368 * fltPrm12271369;
        GibBool fltIf12231371 = fltPrm12241367 >= fltPrm12251370;
        
        if (fltIf12231371) {
            return rotateL222607(k35010861356, x35110871357, l35210881358,
                                 r35310891359);
        } else {
            GibInt fltPrm12291372 =  size234606(l35210881358);
            GibInt fltPrm12311373 =  delta225();
            GibInt fltPrm12321374 =  size234606(r35310891359);
            GibInt fltPrm12301375 = fltPrm12311373 * fltPrm12321374;
            GibBool fltIf12281376 = fltPrm12291372 >= fltPrm12301375;
            
            if (fltIf12281376) {
                return rotateR221608(k35010861356, x35110871357, l35210881358,
                                     r35310891359);
            } else {
                GibInt fltPrm12341377 =  size234606(l35210881358);
                GibInt fltPrm12351378 =  size234606(r35310891359);
                GibInt fltPkd12331379 = fltPrm12341377 + fltPrm12351378;
                GibPtr tailift1516 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1516)->field0 =
                    1;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1516)->field1 =
                    fltPkd12331379;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1516)->field2 =
                    k35010861356;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1516)->field3 =
                    x35110871357;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1516)->field4 =
                    l35210881358;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1516)->field5 =
                    r35310891359;
                return tailift1516;
            }
        }
    }
}
GibCursor insert230602(GibInt kx26610901380, GibVector *x26710911381,
                       GibCursor m26810921382)
{
    GibPackedTag tag1517 = *(GibPackedTag *) m26810921382;
    GibCursor tail1518 = m26810921382 + sizeof(GibInt);
    
    
  switch1520:
    ;
    switch (tag1517) {
        
      case 0:
        {
            return singleton236604(kx26610901380, x26710911381);
            break;
        }
        
      case 1:
        {
            GibInt sz27010931383 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1518)->field0;
            GibInt k27110941384 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1518)->field1;
            GibVector *v27210951385 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1518)->field2;
            GibCursor l27310961386 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1518)->field3;
            GibCursor r27410971387 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1518)->field4;
            GibBool fltIf12361388 = kx26610901380 == k27110941384;
            
            if (fltIf12361388) {
                GibPtr tailift1519 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1519)->field0 =
                    1;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1519)->field1 =
                    sz27010931383;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1519)->field2 =
                    k27110941384;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1519)->field3 =
                    x26710911381;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1519)->field4 =
                    l27310961386;
                ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1519)->field5 =
                    r27410971387;
                return tailift1519;
            } else {
                GibBool fltIf12371389 = kx26610901380 <= k27110941384;
                
                if (fltIf12371389) {
                    GibCursor fltAppE12381390 =
                               insert230602(kx26610901380, x26710911381, l27310961386);
                    
                    return balance223605(k27110941384, v27210951385,
                                         fltAppE12381390, r27410971387);
                } else {
                    GibCursor fltAppE12391391 =
                               insert230602(kx26610901380, x26710911381, r27410971387);
                    
                    return balance223605(k27110941384, v27210951385,
                                         l27310961386, fltAppE12391391);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1517");
            exit(1);
        }
    }
}
GibCursor empty215601()
{
    GibPtr tailift1521 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) tailift1521)->field0 = 0;
    return tailift1521;
}
GibCursor _copy_Ord242(GibCursor arg92210981392)
{
    GibPackedTag tag1522 = *(GibPackedTag *) arg92210981392;
    GibCursor tail1523 = arg92210981392 + sizeof(GibInt);
    
    
  switch1528:
    ;
    switch (tag1522) {
        
      case 0:
        {
            GibPtr tailift1524 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1524)->field0 = 0;
            return tailift1524;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1525 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1525)->field0 = 1;
            return tailift1525;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1526 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1526)->field0 = 2;
            return tailift1526;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1527 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1527)->field0 = 3;
            return tailift1527;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1522");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Ord242(GibCursor arg92310991393)
{
    GibPackedTag tag1529 = *(GibPackedTag *) arg92310991393;
    GibCursor tail1530 = arg92310991393 + sizeof(GibInt);
    
    
  switch1535:
    ;
    switch (tag1529) {
        
      case 0:
        {
            GibPtr tailift1531 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1531)->field0 = 0;
            return tailift1531;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1532 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1532)->field0 = 1;
            return tailift1532;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1533 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1533)->field0 = 2;
            return tailift1533;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1534 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1534)->field0 = 3;
            return tailift1534;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1529");
            exit(1);
        }
    }
}
unsigned char _traverse_Ord242(GibCursor arg92411001394)
{
    GibPackedTag tag1536 = *(GibPackedTag *) arg92411001394;
    GibCursor tail1537 = arg92411001394 + sizeof(GibInt);
    
    
  switch1538:
    ;
    switch (tag1536) {
        
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
            printf("%s\n", "Unknown tag in: tag1536");
            exit(1);
        }
    }
}
unsigned char _print_Ord242(GibCursor arg92511011395)
{
    GibPackedTag tag1539 = *(GibPackedTag *) arg92511011395;
    GibCursor tail1540 = arg92511011395 + sizeof(GibInt);
    
    
  switch1541:
    ;
    switch (tag1539) {
        
      case 0:
        {
            unsigned char wildcard92611021396 = gib_print_symbol(1486);
            unsigned char wildcard92711031397 = gib_print_symbol(1484);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard92811041398 = gib_print_symbol(1487);
            unsigned char wildcard92911051399 = gib_print_symbol(1484);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard93011061400 = gib_print_symbol(1488);
            unsigned char wildcard93111071401 = gib_print_symbol(1484);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard93211081402 = gib_print_symbol(1489);
            unsigned char wildcard93311091403 = gib_print_symbol(1484);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1539");
            exit(1);
        }
    }
}
GibCursor _copy_Map237_v603(GibCursor arg93411101404)
{
    GibPackedTag tag1542 = *(GibPackedTag *) arg93411101404;
    GibCursor tail1543 = arg93411101404 + sizeof(GibInt);
    
    
  switch1546:
    ;
    switch (tag1542) {
        
      case 0:
        {
            GibPtr tailift1544 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1544)->field0 = 0;
            return tailift1544;
            break;
        }
        
      case 1:
        {
            GibInt x93511111405 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1543)->field0;
            GibInt x93611121406 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1543)->field1;
            GibVector *x93711131407 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1543)->field2;
            GibCursor x93811141408 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1543)->field3;
            GibCursor x93911151409 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1543)->field4;
            GibCursor y94311191413 =  _copy_Map237_v603(x93811141408);
            GibCursor y94411201414 =  _copy_Map237_v603(x93911151409);
            GibPtr tailift1545 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1545)->field0 =
                1;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1545)->field1 =
                x93511111405;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1545)->field2 =
                x93611121406;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1545)->field3 =
                x93711131407;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1545)->field4 =
                y94311191413;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1545)->field5 =
                y94411201414;
            return tailift1545;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1542");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Map237_v603(GibCursor arg94511211415)
{
    GibPackedTag tag1547 = *(GibPackedTag *) arg94511211415;
    GibCursor tail1548 = arg94511211415 + sizeof(GibInt);
    
    
  switch1551:
    ;
    switch (tag1547) {
        
      case 0:
        {
            GibPtr tailift1549 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1549)->field0 = 0;
            return tailift1549;
            break;
        }
        
      case 1:
        {
            GibInt x94611221416 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1548)->field0;
            GibInt x94711231417 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1548)->field1;
            GibVector *x94811241418 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1548)->field2;
            GibCursor x94911251419 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1548)->field3;
            GibCursor x95011261420 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1548)->field4;
            GibCursor y95411301424 =
                       _copy_without_ptrs_Map237_v603(x94911251419);
            GibCursor y95511311425 =
                       _copy_without_ptrs_Map237_v603(x95011261420);
            GibPtr tailift1550 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibVectorGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1550)->field0 =
                1;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1550)->field1 =
                x94611221416;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1550)->field2 =
                x94711231417;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1550)->field3 =
                x94811241418;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1550)->field4 =
                y95411301424;
            ((GibIntGibIntGibIntGibVectorGibCursorGibCursorProd *) tailift1550)->field5 =
                y95511311425;
            return tailift1550;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1547");
            exit(1);
        }
    }
}
unsigned char _traverse_Map237_v603(GibCursor arg95611321426)
{
    GibPackedTag tag1552 = *(GibPackedTag *) arg95611321426;
    GibCursor tail1553 = arg95611321426 + sizeof(GibInt);
    
    
  switch1554:
    ;
    switch (tag1552) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x95711331427 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1553)->field0;
            GibInt x95811341428 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1553)->field1;
            GibVector *x95911351429 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1553)->field2;
            GibCursor x96011361430 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1553)->field3;
            GibCursor x96111371431 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1553)->field4;
            unsigned char y96511381432 =  _traverse_Map237_v603(x96011361430);
            unsigned char y96611391433 =  _traverse_Map237_v603(x96111371431);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1552");
            exit(1);
        }
    }
}
unsigned char _print_Map237_v603(GibCursor arg96711401434)
{
    GibPackedTag tag1555 = *(GibPackedTag *) arg96711401434;
    GibCursor tail1556 = arg96711401434 + sizeof(GibInt);
    
    
  switch1557:
    ;
    switch (tag1555) {
        
      case 0:
        {
            unsigned char wildcard96811411435 = gib_print_symbol(1485);
            unsigned char wildcard96911421436 = gib_print_symbol(1484);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x97011431437 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1556)->field0;
            GibInt x97111441438 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1556)->field1;
            GibVector *x97211451439 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1556)->field2;
            GibCursor x97311461440 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1556)->field3;
            GibCursor x97411471441 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1556)->field4;
            unsigned char wildcard98011481442 = gib_print_symbol(1490);
            unsigned char wildcard98611491443 = gib_print_symbol(1491);
            unsigned char y97511501444 = printf("%ld", x97011431437);
            unsigned char wildcard98511511445 = gib_print_symbol(1491);
            unsigned char y97611521446 = printf("%ld", x97111441438);
            unsigned char wildcard98411531447 = gib_print_symbol(1491);
            unsigned char y97711541448 = gib_print_symbol(1483);
            unsigned char wildcard98311551449 = gib_print_symbol(1491);
            unsigned char y97811561450 =  _print_Map237_v603(x97311461440);
            unsigned char wildcard98211571451 = gib_print_symbol(1491);
            unsigned char y97911581452 =  _print_Map237_v603(x97411471441);
            unsigned char wildcard98111591453 = gib_print_symbol(1484);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1555");
            exit(1);
        }
    }
}
GibCursor caseFn987(GibInt k129098811601454, GibVector *x129198911611455,
                    GibCursor t129299011621456, GibCursor m129899111631457,
                    GibInt k229699211641458, GibVector *x229799311651459,
                    GibCursor t429999411661460)
{
    GibPackedTag tag1558 = *(GibPackedTag *) m129899111631457;
    GibCursor tail1559 = m129899111631457 + sizeof(GibInt);
    
    
  switch1560:
    ;
    switch (tag1558) {
        
      case 1:
        {
            GibInt wildcard_18030011671461 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1559)->field0;
            GibInt k330111681462 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1559)->field1;
            GibVector *x330211691463 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1559)->field2;
            GibCursor t230311701464 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1559)->field3;
            GibCursor t330411711465 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1559)->field4;
            GibCursor fltAppE12401466 =
                       bin220611(k129098811601454, x129198911611455, t129299011621456, t230311701464);
            GibCursor fltAppE12411467 =
                       bin220611(k229699211641458, x229799311651459, t330411711465, t429999411661460);
            
            return bin220611(k330111681462, x330211691463, fltAppE12401466,
                             fltAppE12411467);
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1558");
            exit(1);
        }
    }
}
GibCursor caseFn995(GibInt k127599611721468, GibVector *x127699711731469,
                    GibCursor t427899811741470, GibCursor m128499911751471,
                    GibInt k2281100011761472, GibVector *x2282100111771473,
                    GibCursor t1283100211781474)
{
    GibPackedTag tag1561 = *(GibPackedTag *) m128499911751471;
    GibCursor tail1562 = m128499911751471 + sizeof(GibInt);
    
    
  switch1563:
    ;
    switch (tag1561) {
        
      case 1:
        {
            GibInt wildcard_19628511791475 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1562)->field0;
            GibInt k328611801476 =
                   ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1562)->field1;
            GibVector *x328711811477 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1562)->field2;
            GibCursor t228811821478 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1562)->field3;
            GibCursor t328911831479 =
                      ((GibIntGibIntGibVectorGibCursorGibCursorProd *) tail1562)->field4;
            GibCursor fltAppE12421480 =
                       bin220611(k2281100011761472, x2282100111771473, t1283100211781474, t228811821478);
            GibCursor fltAppE12431481 =
                       bin220611(k127599611721468, x127699711731469, t328911831479, t427899811741470);
            
            return bin220611(k328611801476, x328711811477, fltAppE12421480,
                             fltAppE12431481);
            break;
        }
        
      case 0:
        {
            return empty215601();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1561");
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
    GibVector *vec20724910031244 = gib_vector_alloc(1, tmp13);
    GibChar tmp12 = 'f';
    GibVector *_25010041245 = gib_vector_inplace_update(vec20724910031244, 0,
                                                        &tmp12);
    GibInt tmp11 = sizeof(GibChar);
    GibVector *vec20825110051247 = gib_vector_alloc(1, tmp11);
    GibChar tmp10 = 'f';
    GibVector *_25210061248 = gib_vector_inplace_update(vec20825110051247, 0,
                                                        &tmp10);
    GibInt tmp9 = sizeof(GibChar);
    GibVector *vec20925310071250 = gib_vector_alloc(1, tmp9);
    GibChar tmp8 = 'e';
    GibVector *_25410081251 = gib_vector_inplace_update(vec20925310071250, 0,
                                                        &tmp8);
    GibInt tmp7 = sizeof(GibChar);
    GibVector *vec21025510091253 = gib_vector_alloc(1, tmp7);
    GibChar tmp6 = 'd';
    GibVector *_25610101254 = gib_vector_inplace_update(vec21025510091253, 0,
                                                        &tmp6);
    GibInt tmp5 = sizeof(GibChar);
    GibVector *vec21125710111256 = gib_vector_alloc(1, tmp5);
    GibChar tmp4 = 'c';
    GibVector *_25810121257 = gib_vector_inplace_update(vec21125710111256, 0,
                                                        &tmp4);
    GibInt tmp3 = sizeof(GibChar);
    GibVector *vec21225910131259 = gib_vector_alloc(1, tmp3);
    GibChar tmp2 = 'b';
    GibVector *_26010141260 = gib_vector_inplace_update(vec21225910131259, 0,
                                                        &tmp2);
    GibInt tmp1 = sizeof(GibChar);
    GibVector *vec21326110151262 = gib_vector_alloc(1, tmp1);
    GibChar tmp0 = 'a';
    GibVector *_26210161263 = gib_vector_inplace_update(vec21326110151262, 0,
                                                        &tmp0);
    GibCursor fltAppE11971265 =  empty215601();
    GibCursor fltAppE11951266 =
               insert230602(0, vec21326110151262, fltAppE11971265);
    GibCursor fltAppE11931267 =
               insert230602(1, vec21225910131259, fltAppE11951266);
    GibCursor fltAppE11911268 =
               insert230602(2, vec21125710111256, fltAppE11931267);
    GibCursor fltAppE11891269 =
               insert230602(3, vec21025510091253, fltAppE11911268);
    GibCursor fltAppE11871270 =
               insert230602(4, vec20925310071250, fltAppE11891269);
    GibCursor fltAppE11851271 =
               insert230602(5, vec20825110051247, fltAppE11871270);
    GibCursor tmp_app1482 =
               insert230602(0, vec20724910031244, fltAppE11851271);
    
     _print_Map237_v603(tmp_app1482);
    printf("\n");
    return 0;
    
    int exit15 = gib_exit();
    
    return exit15;
}