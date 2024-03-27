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
typedef struct GibIntGibIntProd_struct {
            GibInt field0;
            GibInt field1;
        } GibIntGibIntProd;
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
typedef struct GibIntGibIntGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibCursor field2;
        } GibIntGibIntGibCursorProd;
typedef struct GibPackedTagGibCursorProd_struct {
            GibPackedTag field0;
            GibCursor field1;
        } GibPackedTagGibCursorProd;
typedef struct GibCursorProd_struct {
            GibCursor field0;
        } GibCursorProd;
GibCursor _copy_Timestamp75(GibCursor arg84911221278);
GibCursor _copy_without_ptrs_Timestamp75(GibCursor arg85411271283);
unsigned char _traverse_Timestamp75(GibCursor arg85911321288);
unsigned char _print_Timestamp75(GibCursor arg86411361292);
GibCursor _copy_Ord79(GibCursor arg87311451301);
GibCursor _copy_without_ptrs_Ord79(GibCursor arg87411461302);
unsigned char _traverse_Ord79(GibCursor arg87511471303);
unsigned char _print_Ord79(GibCursor arg87611481304);
GibCursor _copy_Maybe78_v492(GibCursor arg88511571313);
GibCursor _copy_without_ptrs_Maybe78_v492(GibCursor arg88811601316);
unsigned char _traverse_Maybe78_v492(GibCursor arg89111631319);
unsigned char _print_Maybe78_v492(GibCursor arg89411651321);
GibCursor _copy_Map108_v490(GibCursor arg90211731329);
GibCursor _copy_without_ptrs_Map108_v490(GibCursor arg91311841340);
unsigned char _traverse_Map108_v490(GibCursor arg92411951351);
unsigned char _print_Map108_v490(GibCursor arg93512031359);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Map108_v490_T,
            Maybe78_v492_T,
            Ord79_T,
            Timestamp75_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(11);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[3];
    
    error = gib_info_table_insert_packed_dcon(Map108_v490_T, 1, 24, 2, 3, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map108_v490_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map108_v490_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map108_v490_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe78_v492_T, 1, 8, 0, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe78_v492_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe78_v492_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe78_v492_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord79_T, 3, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord79_T, 3);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord79_T, 2, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord79_T, 2);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord79_T, 1, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord79_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord79_T, 0, 0, 0, 0, 0, field_tys,
                                              0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord79_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Timestamp75_T, 0, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Timestamp75_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(1379, ")");
    gib_add_symbol(1380, "(Tip109_v490");
    gib_add_symbol(1381, "(Timestamp76");
    gib_add_symbol(1382, "(Nothing84_v492");
    gib_add_symbol(1383, "(Lt80");
    gib_add_symbol(1384, "(Just85_v492");
    gib_add_symbol(1385, "(Gt81");
    gib_add_symbol(1386, "(Eq82");
    gib_add_symbol(1387, "(Cc83");
    gib_add_symbol(1388, "(Bin110_v490");
    gib_add_symbol(1389, " ");
}
GibCursor _copy_Timestamp75(GibCursor arg84911221278)
{
    GibInt tag1390 = ((GibIntGibIntGibCursorProd *) arg84911221278)->field0;
    GibInt x85011231279 =
           ((GibIntGibIntGibCursorProd *) arg84911221278)->field1;
    GibCursor x85111241280 =
              ((GibIntGibIntGibCursorProd *) arg84911221278)->field2;
    GibCursor y85311261282 =  _copy_Map108_v490(x85111241280);
    GibPtr tailift1391 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1391)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1391)->field1 = x85011231279;
    ((GibIntGibIntGibCursorProd *) tailift1391)->field2 = y85311261282;
    return tailift1391;
}
GibCursor _copy_without_ptrs_Timestamp75(GibCursor arg85411271283)
{
    GibInt tag1392 = ((GibIntGibIntGibCursorProd *) arg85411271283)->field0;
    GibInt x85511281284 =
           ((GibIntGibIntGibCursorProd *) arg85411271283)->field1;
    GibCursor x85611291285 =
              ((GibIntGibIntGibCursorProd *) arg85411271283)->field2;
    GibCursor y85811311287 =  _copy_without_ptrs_Map108_v490(x85611291285);
    GibPtr tailift1393 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1393)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1393)->field1 = x85511281284;
    ((GibIntGibIntGibCursorProd *) tailift1393)->field2 = y85811311287;
    return tailift1393;
}
unsigned char _traverse_Timestamp75(GibCursor arg85911321288)
{
    GibInt tag1394 = ((GibIntGibIntGibCursorProd *) arg85911321288)->field0;
    GibInt x86011331289 =
           ((GibIntGibIntGibCursorProd *) arg85911321288)->field1;
    GibCursor x86111341290 =
              ((GibIntGibIntGibCursorProd *) arg85911321288)->field2;
    unsigned char y86311351291 =  _traverse_Map108_v490(x86111341290);
    
    return 0;
}
unsigned char _print_Timestamp75(GibCursor arg86411361292)
{
    GibInt tag1395 = ((GibIntGibIntGibCursorProd *) arg86411361292)->field0;
    GibInt x86511371293 =
           ((GibIntGibIntGibCursorProd *) arg86411361292)->field1;
    GibCursor x86611381294 =
              ((GibIntGibIntGibCursorProd *) arg86411361292)->field2;
    unsigned char wildcard86911391295 = gib_print_symbol(1381);
    unsigned char wildcard87211401296 = gib_print_symbol(1389);
    unsigned char y86711411297 = printf("%ld", x86511371293);
    unsigned char wildcard87111421298 = gib_print_symbol(1389);
    unsigned char y86811431299 =  _print_Map108_v490(x86611381294);
    unsigned char wildcard87011441300 = gib_print_symbol(1379);
    
    return 0;
}
GibCursor _copy_Ord79(GibCursor arg87311451301)
{
    GibPackedTag tag1396 = *(GibPackedTag *) arg87311451301;
    GibCursor tail1397 = arg87311451301 + sizeof(GibInt);
    
    
  switch1402:
    ;
    switch (tag1396) {
        
      case 0:
        {
            GibPtr tailift1398 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1398)->field0 = 0;
            return tailift1398;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1399 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1399)->field0 = 1;
            return tailift1399;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1400 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1400)->field0 = 2;
            return tailift1400;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1401 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1401)->field0 = 3;
            return tailift1401;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1396");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Ord79(GibCursor arg87411461302)
{
    GibPackedTag tag1403 = *(GibPackedTag *) arg87411461302;
    GibCursor tail1404 = arg87411461302 + sizeof(GibInt);
    
    
  switch1409:
    ;
    switch (tag1403) {
        
      case 0:
        {
            GibPtr tailift1405 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1405)->field0 = 0;
            return tailift1405;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1406 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1406)->field0 = 1;
            return tailift1406;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1407 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1407)->field0 = 2;
            return tailift1407;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1408 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1408)->field0 = 3;
            return tailift1408;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1403");
            exit(1);
        }
    }
}
unsigned char _traverse_Ord79(GibCursor arg87511471303)
{
    GibPackedTag tag1410 = *(GibPackedTag *) arg87511471303;
    GibCursor tail1411 = arg87511471303 + sizeof(GibInt);
    
    
  switch1412:
    ;
    switch (tag1410) {
        
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
            printf("%s\n", "Unknown tag in: tag1410");
            exit(1);
        }
    }
}
unsigned char _print_Ord79(GibCursor arg87611481304)
{
    GibPackedTag tag1413 = *(GibPackedTag *) arg87611481304;
    GibCursor tail1414 = arg87611481304 + sizeof(GibInt);
    
    
  switch1415:
    ;
    switch (tag1413) {
        
      case 0:
        {
            unsigned char wildcard87711491305 = gib_print_symbol(1383);
            unsigned char wildcard87811501306 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard87911511307 = gib_print_symbol(1385);
            unsigned char wildcard88011521308 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard88111531309 = gib_print_symbol(1386);
            unsigned char wildcard88211541310 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard88311551311 = gib_print_symbol(1387);
            unsigned char wildcard88411561312 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1413");
            exit(1);
        }
    }
}
GibCursor _copy_Maybe78_v492(GibCursor arg88511571313)
{
    GibPackedTag tag1416 = *(GibPackedTag *) arg88511571313;
    GibCursor tail1417 = arg88511571313 + sizeof(GibInt);
    
    
  switch1420:
    ;
    switch (tag1416) {
        
      case 0:
        {
            GibPtr tailift1418 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1418)->field0 = 0;
            return tailift1418;
            break;
        }
        
      case 1:
        {
            GibInt x88611581314 = ((GibIntProd *) tail1417)->field0;
            GibPtr tailift1419 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift1419)->field0 = 1;
            ((GibIntGibIntProd *) tailift1419)->field1 = x88611581314;
            return tailift1419;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1416");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Maybe78_v492(GibCursor arg88811601316)
{
    GibPackedTag tag1421 = *(GibPackedTag *) arg88811601316;
    GibCursor tail1422 = arg88811601316 + sizeof(GibInt);
    
    
  switch1425:
    ;
    switch (tag1421) {
        
      case 0:
        {
            GibPtr tailift1423 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1423)->field0 = 0;
            return tailift1423;
            break;
        }
        
      case 1:
        {
            GibInt x88911611317 = ((GibIntProd *) tail1422)->field0;
            GibPtr tailift1424 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift1424)->field0 = 1;
            ((GibIntGibIntProd *) tailift1424)->field1 = x88911611317;
            return tailift1424;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1421");
            exit(1);
        }
    }
}
unsigned char _traverse_Maybe78_v492(GibCursor arg89111631319)
{
    GibPackedTag tag1426 = *(GibPackedTag *) arg89111631319;
    GibCursor tail1427 = arg89111631319 + sizeof(GibInt);
    
    
  switch1428:
    ;
    switch (tag1426) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x89211641320 = ((GibIntProd *) tail1427)->field0;
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1426");
            exit(1);
        }
    }
}
unsigned char _print_Maybe78_v492(GibCursor arg89411651321)
{
    GibPackedTag tag1429 = *(GibPackedTag *) arg89411651321;
    GibCursor tail1430 = arg89411651321 + sizeof(GibInt);
    
    
  switch1431:
    ;
    switch (tag1429) {
        
      case 0:
        {
            unsigned char wildcard89511661322 = gib_print_symbol(1382);
            unsigned char wildcard89611671323 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x89711681324 = ((GibIntProd *) tail1430)->field0;
            unsigned char wildcard89911691325 = gib_print_symbol(1384);
            unsigned char wildcard90111701326 = gib_print_symbol(1389);
            unsigned char y89811711327 = printf("%ld", x89711681324);
            unsigned char wildcard90011721328 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1429");
            exit(1);
        }
    }
}
GibCursor _copy_Map108_v490(GibCursor arg90211731329)
{
    GibPackedTag tag1432 = *(GibPackedTag *) arg90211731329;
    GibCursor tail1433 = arg90211731329 + sizeof(GibInt);
    
    
  switch1436:
    ;
    switch (tag1432) {
        
      case 0:
        {
            GibPtr tailift1434 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1434)->field0 = 0;
            return tailift1434;
            break;
        }
        
      case 1:
        {
            GibInt x90311741330 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1433)->field0;
            GibInt x90411751331 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1433)->field1;
            GibInt x90511761332 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1433)->field2;
            GibCursor x90611771333 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1433)->field3;
            GibCursor x90711781334 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1433)->field4;
            GibCursor y91111821338 =  _copy_Map108_v490(x90611771333);
            GibCursor y91211831339 =  _copy_Map108_v490(x90711781334);
            GibPtr tailift1435 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1435)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1435)->field1 =
                x90311741330;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1435)->field2 =
                x90411751331;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1435)->field3 =
                x90511761332;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1435)->field4 =
                y91111821338;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1435)->field5 =
                y91211831339;
            return tailift1435;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1432");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Map108_v490(GibCursor arg91311841340)
{
    GibPackedTag tag1437 = *(GibPackedTag *) arg91311841340;
    GibCursor tail1438 = arg91311841340 + sizeof(GibInt);
    
    
  switch1441:
    ;
    switch (tag1437) {
        
      case 0:
        {
            GibPtr tailift1439 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1439)->field0 = 0;
            return tailift1439;
            break;
        }
        
      case 1:
        {
            GibInt x91411851341 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1438)->field0;
            GibInt x91511861342 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1438)->field1;
            GibInt x91611871343 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1438)->field2;
            GibCursor x91711881344 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1438)->field3;
            GibCursor x91811891345 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1438)->field4;
            GibCursor y92211931349 =
                       _copy_without_ptrs_Map108_v490(x91711881344);
            GibCursor y92311941350 =
                       _copy_without_ptrs_Map108_v490(x91811891345);
            GibPtr tailift1440 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1440)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1440)->field1 =
                x91411851341;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1440)->field2 =
                x91511861342;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1440)->field3 =
                x91611871343;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1440)->field4 =
                y92211931349;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1440)->field5 =
                y92311941350;
            return tailift1440;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1437");
            exit(1);
        }
    }
}
unsigned char _traverse_Map108_v490(GibCursor arg92411951351)
{
    GibPackedTag tag1442 = *(GibPackedTag *) arg92411951351;
    GibCursor tail1443 = arg92411951351 + sizeof(GibInt);
    
    
  switch1444:
    ;
    switch (tag1442) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x92511961352 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1443)->field0;
            GibInt x92611971353 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1443)->field1;
            GibInt x92711981354 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1443)->field2;
            GibCursor x92811991355 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1443)->field3;
            GibCursor x92912001356 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1443)->field4;
            unsigned char y93312011357 =  _traverse_Map108_v490(x92811991355);
            unsigned char y93412021358 =  _traverse_Map108_v490(x92912001356);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1442");
            exit(1);
        }
    }
}
unsigned char _print_Map108_v490(GibCursor arg93512031359)
{
    GibPackedTag tag1445 = *(GibPackedTag *) arg93512031359;
    GibCursor tail1446 = arg93512031359 + sizeof(GibInt);
    
    
  switch1447:
    ;
    switch (tag1445) {
        
      case 0:
        {
            unsigned char wildcard93612041360 = gib_print_symbol(1380);
            unsigned char wildcard93712051361 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x93812061362 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1446)->field0;
            GibInt x93912071363 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1446)->field1;
            GibInt x94012081364 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1446)->field2;
            GibCursor x94112091365 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1446)->field3;
            GibCursor x94212101366 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1446)->field4;
            unsigned char wildcard94812111367 = gib_print_symbol(1388);
            unsigned char wildcard95412121368 = gib_print_symbol(1389);
            unsigned char y94312131369 = printf("%ld", x93812061362);
            unsigned char wildcard95312141370 = gib_print_symbol(1389);
            unsigned char y94412151371 = printf("%ld", x93912071363);
            unsigned char wildcard95212161372 = gib_print_symbol(1389);
            unsigned char y94512171373 = printf("%ld", x94012081364);
            unsigned char wildcard95112181374 = gib_print_symbol(1389);
            unsigned char y94612191375 =  _print_Map108_v490(x94112091365);
            unsigned char wildcard95012201376 = gib_print_symbol(1389);
            unsigned char y94712211377 =  _print_Map108_v490(x94212101366);
            unsigned char wildcard94912221378 = gib_print_symbol(1379);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1445");
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