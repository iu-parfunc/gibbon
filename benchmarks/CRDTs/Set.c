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
typedef struct GibIntGibIntGibIntGibCursorGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibInt field2;
            GibCursor field3;
            GibCursor field4;
        } GibIntGibIntGibIntGibCursorGibCursorProd;
typedef struct GibIntGibIntGibCursorGibCursorProd_struct {
            GibInt field0;
            GibInt field1;
            GibCursor field2;
            GibCursor field3;
        } GibIntGibIntGibCursorGibCursorProd;
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
GibCursor empty202();
GibCursor insert214(GibInt x219636968, GibCursor s220637969);
GibInt sum203(GibCursor s227644978);
GibCursor right205(GibCursor x234651986);
GibCursor left206(GibCursor x239656991);
GibInt val207(GibCursor x244661996);
GibInt size208(GibCursor s2496661001);
GibCursor balanceR212(GibInt x2726891006, GibCursor l2736901007,
                      GibCursor r2746911008);
GibCursor balanceL213(GibInt x2996961013, GibCursor l3006971014,
                      GibCursor r3016981015);
GibCursor singleton215(GibInt x3267031020);
GibCursor _copy_IntSet216(GibCursor arg5397041023);
GibCursor _copy_without_ptrs_IntSet216(GibCursor arg5487131032);
unsigned char _traverse_IntSet216(GibCursor arg5577221041);
unsigned char _print_IntSet216(GibCursor arg5667291048);
GibCursor caseFn583(GibInt x2725847461065, GibCursor rr2785857471066,
                    GibInt rs2755867481067, GibInt rx2765877491068,
                    GibCursor rl2775887501069);
GibCursor caseFn589(GibInt x2725907551101, GibCursor r2745917561102,
                    GibInt rx2765927571103, GibCursor rl2775937581104);
GibCursor caseFn594(GibInt x2725957631117, GibCursor r2745967641118,
                    GibCursor rr2785977651119, GibInt rs2755987661120,
                    GibInt rx2765997671121, GibCursor rl2776007681122);
GibCursor caseFn601(GibInt x2726027731127, GibCursor r2746037741128);
GibCursor caseFn604(GibCursor l2736057791135, GibInt x2726067801136,
                    GibCursor r2746077811137, GibInt ls2916087821138);
GibCursor caseFn609(GibInt x2996107871175, GibCursor ll3046117881176,
                    GibInt ls3026127891177, GibInt lx3036137901178,
                    GibCursor lr3056147911179, GibInt lls3066157921180);
GibCursor caseFn616(GibInt x2996177971203, GibCursor l3006187981204,
                    GibInt lx3036197991205, GibCursor lr3056208001206);
GibCursor caseFn621(GibInt x2996228051218, GibCursor l3006238061219,
                    GibCursor ll3046248071220, GibInt ls3026258081221,
                    GibInt lx3036268091222, GibCursor lr3056278101223);
GibCursor caseFn628(GibInt x2996298151228, GibCursor l3006308161229);
GibCursor caseFn631(GibCursor r3016328211236, GibInt x2996338221237,
                    GibCursor l3006348231238, GibInt rs3186358241239);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            IntSet216_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(8);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[2];
    
    error = gib_info_table_insert_packed_dcon(IntSet216_T, 1, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, IntSet216_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(IntSet216_T, 0, 16, 2, 2, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, IntSet216_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(1277, ")");
    gib_add_symbol(1278, "(PureSet217");
    gib_add_symbol(1279, "(EmptySet218");
    gib_add_symbol(1280, " ");
}
GibCursor empty202()
{
    GibPtr tailift1281 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) tailift1281)->field0 = 1;
    return tailift1281;
}
GibCursor insert214(GibInt x219636968, GibCursor s220637969)
{
    GibPackedTag tag1282 = *(GibPackedTag *) s220637969;
    GibCursor tail1283 = s220637969 + sizeof(GibInt);
    
    
  switch1284:
    ;
    switch (tag1282) {
        
      case 1:
        {
            return singleton215(x219636968);
            break;
        }
        
      case 0:
        {
            GibInt sz221638970 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1283)->field0;
            GibInt v222639971 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1283)->field1;
            GibCursor l223640972 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1283)->field2;
            GibCursor r224641973 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1283)->field3;
            GibBool fltIf831974 = x219636968 == v222639971;
            
            if (fltIf831974) {
                return s220637969;
            } else {
                GibBool fltIf832975 = x219636968 <= v222639971;
                
                if (fltIf832975) {
                    GibCursor nl225642976 =  insert214(x219636968, l223640972);
                    
                    return balanceL213(v222639971, nl225642976, r224641973);
                } else {
                    GibCursor nr226643977 =  insert214(x219636968, r224641973);
                    
                    return balanceR212(v222639971, l223640972, nr226643977);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1282");
            exit(1);
        }
    }
}
GibInt sum203(GibCursor s227644978)
{
    GibPackedTag tag1285 = *(GibPackedTag *) s227644978;
    GibCursor tail1286 = s227644978 + sizeof(GibInt);
    
    
  switch1288:
    ;
    switch (tag1285) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            GibInt wildcard_196228645979 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1286)->field0;
            GibInt v229646980 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1286)->field1;
            GibCursor l230647981 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1286)->field2;
            GibCursor r231648982 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1286)->field3;
            GibInt fltPrm834983 =  sum203(l230647981);
            GibInt fltPrm833984 = v229646980 + fltPrm834983;
            GibInt fltPrm835985 =  sum203(r231648982);
            GibInt flt1287 = fltPrm833984 + fltPrm835985;
            
            return flt1287;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1285");
            exit(1);
        }
    }
}
GibCursor right205(GibCursor x234651986)
{
    GibPackedTag tag1289 = *(GibPackedTag *) x234651986;
    GibCursor tail1290 = x234651986 + sizeof(GibInt);
    
    
  switch1292:
    ;
    switch (tag1289) {
        
      case 1:
        {
            GibPtr tailift1291 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1291)->field0 = 1;
            return tailift1291;
            break;
        }
        
      case 0:
        {
            GibInt wildcard_186235652987 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1290)->field0;
            GibInt wildcard_187236653988 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1290)->field1;
            GibCursor wildcard_188237654989 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1290)->field2;
            GibCursor r238655990 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1290)->field3;
            
            return r238655990;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1289");
            exit(1);
        }
    }
}
GibCursor left206(GibCursor x239656991)
{
    GibPackedTag tag1293 = *(GibPackedTag *) x239656991;
    GibCursor tail1294 = x239656991 + sizeof(GibInt);
    
    
  switch1296:
    ;
    switch (tag1293) {
        
      case 1:
        {
            GibPtr tailift1295 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1295)->field0 = 1;
            return tailift1295;
            break;
        }
        
      case 0:
        {
            GibInt wildcard_177240657992 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1294)->field0;
            GibInt wildcard_178241658993 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1294)->field1;
            GibCursor l242659994 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1294)->field2;
            GibCursor wildcard_179243660995 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1294)->field3;
            
            return l242659994;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1293");
            exit(1);
        }
    }
}
GibInt val207(GibCursor x244661996)
{
    GibPackedTag tag1297 = *(GibPackedTag *) x244661996;
    GibCursor tail1298 = x244661996 + sizeof(GibInt);
    
    
  switch1299:
    ;
    switch (tag1297) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            GibInt wildcard_168245662997 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1298)->field0;
            GibInt v246663998 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1298)->field1;
            GibCursor wildcard_169247664999 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1298)->field2;
            GibCursor wildcard_1702486651000 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1298)->field3;
            
            return v246663998;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1297");
            exit(1);
        }
    }
}
GibInt size208(GibCursor s2496661001)
{
    GibPackedTag tag1300 = *(GibPackedTag *) s2496661001;
    GibCursor tail1301 = s2496661001 + sizeof(GibInt);
    
    
  switch1302:
    ;
    switch (tag1300) {
        
      case 1:
        {
            return 0;
            break;
        }
        
      case 0:
        {
            GibInt sz2506671002 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1301)->field0;
            GibInt wildcard_1602516681003 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1301)->field1;
            GibCursor wildcard_1612526691004 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1301)->field2;
            GibCursor wildcard_1622536701005 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1301)->field3;
            
            return sz2506671002;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1300");
            exit(1);
        }
    }
}
GibCursor balanceR212(GibInt x2726891006, GibCursor l2736901007,
                      GibCursor r2746911008)
{
    GibPackedTag tag1303 = *(GibPackedTag *) l2736901007;
    GibCursor tail1304 = l2736901007 + sizeof(GibInt);
    
    
  switch1305:
    ;
    switch (tag1303) {
        
      case 1:
        {
            return caseFn601(x2726891006, r2746911008);
            break;
        }
        
      case 0:
        {
            GibInt ls2916921009 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1304)->field0;
            GibInt wildcard_1162926931010 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1304)->field1;
            GibCursor wildcard_1172936941011 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1304)->field2;
            GibCursor wildcard_1182946951012 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1304)->field3;
            
            return caseFn604(l2736901007, x2726891006, r2746911008,
                             ls2916921009);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1303");
            exit(1);
        }
    }
}
GibCursor balanceL213(GibInt x2996961013, GibCursor l3006971014,
                      GibCursor r3016981015)
{
    GibPackedTag tag1306 = *(GibPackedTag *) r3016981015;
    GibCursor tail1307 = r3016981015 + sizeof(GibInt);
    
    
  switch1308:
    ;
    switch (tag1306) {
        
      case 1:
        {
            return caseFn628(x2996961013, l3006971014);
            break;
        }
        
      case 0:
        {
            GibInt rs3186991016 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1307)->field0;
            GibInt wildcard_553197001017 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1307)->field1;
            GibCursor wildcard_563207011018 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1307)->field2;
            GibCursor wildcard_573217021019 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1307)->field3;
            
            return caseFn631(r3016981015, x2996961013, l3006971014,
                             rs3186991016);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1306");
            exit(1);
        }
    }
}
GibCursor singleton215(GibInt x3267031020)
{
    GibPtr fltPkd8361021 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd8361021)->field0 = 1;
    
    GibPtr fltPkd8371022 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd8371022)->field0 = 1;
    
    GibPtr tailift1309 =
           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
    
    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1309)->field0 = 0;
    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1309)->field1 = 1;
    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1309)->field2 =
        x3267031020;
    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1309)->field3 =
        fltPkd8361021;
    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1309)->field4 =
        fltPkd8371022;
    return tailift1309;
}
GibCursor _copy_IntSet216(GibCursor arg5397041023)
{
    GibPackedTag tag1310 = *(GibPackedTag *) arg5397041023;
    GibCursor tail1311 = arg5397041023 + sizeof(GibInt);
    
    
  switch1314:
    ;
    switch (tag1310) {
        
      case 0:
        {
            GibInt x5407051024 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1311)->field0;
            GibInt x5417061025 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1311)->field1;
            GibCursor x5427071026 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1311)->field2;
            GibCursor x5437081027 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1311)->field3;
            GibCursor y5467111030 =  _copy_IntSet216(x5427071026);
            GibCursor y5477121031 =  _copy_IntSet216(x5437081027);
            GibPtr tailift1312 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1312)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1312)->field1 =
                x5407051024;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1312)->field2 =
                x5417061025;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1312)->field3 =
                y5467111030;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1312)->field4 =
                y5477121031;
            return tailift1312;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1313 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1313)->field0 = 1;
            return tailift1313;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1310");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_IntSet216(GibCursor arg5487131032)
{
    GibPackedTag tag1315 = *(GibPackedTag *) arg5487131032;
    GibCursor tail1316 = arg5487131032 + sizeof(GibInt);
    
    
  switch1319:
    ;
    switch (tag1315) {
        
      case 0:
        {
            GibInt x5497141033 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1316)->field0;
            GibInt x5507151034 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1316)->field1;
            GibCursor x5517161035 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1316)->field2;
            GibCursor x5527171036 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1316)->field3;
            GibCursor y5557201039 =  _copy_without_ptrs_IntSet216(x5517161035);
            GibCursor y5567211040 =  _copy_without_ptrs_IntSet216(x5527171036);
            GibPtr tailift1317 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1317)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1317)->field1 =
                x5497141033;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1317)->field2 =
                x5507151034;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1317)->field3 =
                y5557201039;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1317)->field4 =
                y5567211040;
            return tailift1317;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1318 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1318)->field0 = 1;
            return tailift1318;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1315");
            exit(1);
        }
    }
}
unsigned char _traverse_IntSet216(GibCursor arg5577221041)
{
    GibPackedTag tag1320 = *(GibPackedTag *) arg5577221041;
    GibCursor tail1321 = arg5577221041 + sizeof(GibInt);
    
    
  switch1322:
    ;
    switch (tag1320) {
        
      case 0:
        {
            GibInt x5587231042 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1321)->field0;
            GibInt x5597241043 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1321)->field1;
            GibCursor x5607251044 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1321)->field2;
            GibCursor x5617261045 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1321)->field3;
            unsigned char y5647271046 =  _traverse_IntSet216(x5607251044);
            unsigned char y5657281047 =  _traverse_IntSet216(x5617261045);
            
            return 0;
            break;
        }
        
      case 1:
        {
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1320");
            exit(1);
        }
    }
}
unsigned char _print_IntSet216(GibCursor arg5667291048)
{
    GibPackedTag tag1323 = *(GibPackedTag *) arg5667291048;
    GibCursor tail1324 = arg5667291048 + sizeof(GibInt);
    
    
  switch1325:
    ;
    switch (tag1323) {
        
      case 0:
        {
            GibInt x5677301049 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1324)->field0;
            GibInt x5687311050 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1324)->field1;
            GibCursor x5697321051 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1324)->field2;
            GibCursor x5707331052 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1324)->field3;
            unsigned char wildcard5757341053 = gib_print_symbol(1278);
            unsigned char wildcard5807351054 = gib_print_symbol(1280);
            unsigned char y5717361055 = printf("%ld", x5677301049);
            unsigned char wildcard5797371056 = gib_print_symbol(1280);
            unsigned char y5727381057 = printf("%ld", x5687311050);
            unsigned char wildcard5787391058 = gib_print_symbol(1280);
            unsigned char y5737401059 =  _print_IntSet216(x5697321051);
            unsigned char wildcard5777411060 = gib_print_symbol(1280);
            unsigned char y5747421061 =  _print_IntSet216(x5707331052);
            unsigned char wildcard5767431062 = gib_print_symbol(1277);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard5817441063 = gib_print_symbol(1279);
            unsigned char wildcard5827451064 = gib_print_symbol(1277);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1323");
            exit(1);
        }
    }
}
GibCursor caseFn583(GibInt x2725847461065, GibCursor rr2785857471066,
                    GibInt rs2755867481067, GibInt rx2765877491068,
                    GibCursor rl2775887501069)
{
    GibPackedTag tag1326 = *(GibPackedTag *) rl2775887501069;
    GibCursor tail1327 = rl2775887501069 + sizeof(GibInt);
    
    
  switch1331:
    ;
    switch (tag1326) {
        
      case 0:
        {
            GibInt rls2837511070 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1327)->field0;
            GibInt rlx2847521071 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1327)->field1;
            GibCursor rll2857531072 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1327)->field2;
            GibCursor rlr2867541073 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1327)->field3;
            GibInt fltPrm8391074 =  size208(rl2775887501069);
            GibInt fltPrm8411075 =  size208(rr2785857471066);
            GibInt fltPrm8401076 = 2 * fltPrm8411075;
            GibBool fltIf8381077 = fltPrm8391074 < fltPrm8401076;
            
            if (fltIf8381077) {
                GibInt fltPkd8421078 = 1 + rs2755867481067;
                GibInt fltPrm8451079 =  size208(rl2775887501069);
                GibInt fltPkd8441080 = 1 + fltPrm8451079;
                GibPtr fltPkd8461081 = gib_alloc(sizeof(GibIntProd));
                
                ((GibIntProd *) fltPkd8461081)->field0 = 1;
                
                GibPtr fltPkd8431082 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8431082)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8431082)->field1 =
                    fltPkd8441080;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8431082)->field2 =
                    x2725847461065;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8431082)->field3 =
                    fltPkd8461081;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8431082)->field4 =
                    rl2775887501069;
                
                GibPtr tailift1328 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1328)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1328)->field1 =
                    fltPkd8421078;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1328)->field2 =
                    rx2765877491068;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1328)->field3 =
                    fltPkd8431082;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1328)->field4 =
                    rr2785857471066;
                return tailift1328;
            } else {
                GibInt fltPkd8471083 = 1 + rs2755867481067;
                GibInt fltPkd8481084 =  val207(rl2775887501069);
                GibCursor fltAppE8521085 =  left206(rl2775887501069);
                GibInt fltPrm8511086 =  size208(fltAppE8521085);
                GibInt fltPkd8501087 = 1 + fltPrm8511086;
                GibPtr fltPkd8531088 = gib_alloc(sizeof(GibIntProd));
                
                ((GibIntProd *) fltPkd8531088)->field0 = 1;
                
                GibCursor fltPkd8541089 =  left206(rl2775887501069);
                GibPtr fltPkd8491090 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8491090)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8491090)->field1 =
                    fltPkd8501087;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8491090)->field2 =
                    x2725847461065;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8491090)->field3 =
                    fltPkd8531088;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8491090)->field4 =
                    fltPkd8541089;
                
                GibInt fltPrm8581091 =  size208(rr2785857471066);
                GibInt fltPrm8571092 = 1 + fltPrm8581091;
                GibCursor fltAppE8601093 =  right205(rl2775887501069);
                GibInt fltPrm8591094 =  size208(fltAppE8601093);
                GibInt fltPkd8561095 = fltPrm8571092 + fltPrm8591094;
                GibCursor fltPkd8611096 =  right205(rl2775887501069);
                GibPtr fltPkd8551097 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8551097)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8551097)->field1 =
                    fltPkd8561095;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8551097)->field2 =
                    rx2765877491068;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8551097)->field3 =
                    fltPkd8611096;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8551097)->field4 =
                    rr2785857471066;
                
                GibPtr tailift1329 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1329)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1329)->field1 =
                    fltPkd8471083;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1329)->field2 =
                    fltPkd8481084;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1329)->field3 =
                    fltPkd8491090;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1329)->field4 =
                    fltPkd8551097;
                return tailift1329;
            }
            break;
        }
        
      case 1:
        {
            GibPtr fltPkd8631098 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8631098)->field0 = 1;
            
            GibPtr fltPkd8641099 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8641099)->field0 = 1;
            
            GibPtr fltPkd8621100 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8621100)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8621100)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8621100)->field2 =
                x2725847461065;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8621100)->field3 =
                fltPkd8631098;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8621100)->field4 =
                fltPkd8641099;
            
            GibPtr tailift1330 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1330)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1330)->field1 =
                3;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1330)->field2 =
                rx2765877491068;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1330)->field3 =
                fltPkd8621100;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1330)->field4 =
                rr2785857471066;
            return tailift1330;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1326");
            exit(1);
        }
    }
}
GibCursor caseFn589(GibInt x2725907551101, GibCursor r2745917561102,
                    GibInt rx2765927571103, GibCursor rl2775937581104)
{
    GibPackedTag tag1332 = *(GibPackedTag *) rl2775937581104;
    GibCursor tail1333 = rl2775937581104 + sizeof(GibInt);
    
    
  switch1336:
    ;
    switch (tag1332) {
        
      case 0:
        {
            GibInt rls2877591105 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1333)->field0;
            GibInt rlx2887601106 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1333)->field1;
            GibCursor rll2897611107 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1333)->field2;
            GibCursor rlr2907621108 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1333)->field3;
            GibInt fltPkd8651109 =  val207(rl2775937581104);
            GibPtr fltPkd8671110 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8671110)->field0 = 1;
            
            GibPtr fltPkd8681111 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8681111)->field0 = 1;
            
            GibPtr fltPkd8661112 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8661112)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8661112)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8661112)->field2 =
                x2725907551101;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8661112)->field3 =
                fltPkd8671110;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8661112)->field4 =
                fltPkd8681111;
            
            GibPtr fltPkd8701113 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8701113)->field0 = 1;
            
            GibPtr fltPkd8711114 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8711114)->field0 = 1;
            
            GibPtr fltPkd8691115 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8691115)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8691115)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8691115)->field2 =
                rx2765927571103;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8691115)->field3 =
                fltPkd8701113;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8691115)->field4 =
                fltPkd8711114;
            
            GibPtr tailift1334 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1334)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1334)->field1 =
                3;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1334)->field2 =
                fltPkd8651109;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1334)->field3 =
                fltPkd8661112;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1334)->field4 =
                fltPkd8691115;
            return tailift1334;
            break;
        }
        
      case 1:
        {
            GibPtr fltPkd8721116 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8721116)->field0 = 1;
            
            GibPtr tailift1335 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1335)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1335)->field1 =
                2;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1335)->field2 =
                x2725907551101;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1335)->field3 =
                fltPkd8721116;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1335)->field4 =
                r2745917561102;
            return tailift1335;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1332");
            exit(1);
        }
    }
}
GibCursor caseFn594(GibInt x2725957631117, GibCursor r2745967641118,
                    GibCursor rr2785977651119, GibInt rs2755987661120,
                    GibInt rx2765997671121, GibCursor rl2776007681122)
{
    GibPackedTag tag1337 = *(GibPackedTag *) rr2785977651119;
    GibCursor tail1338 = rr2785977651119 + sizeof(GibInt);
    
    
  switch1339:
    ;
    switch (tag1337) {
        
      case 0:
        {
            GibInt rrs2797691123 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1338)->field0;
            GibInt rrx2807701124 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1338)->field1;
            GibCursor rrl2817711125 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1338)->field2;
            GibCursor rrr2827721126 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1338)->field3;
            
            return caseFn583(x2725957631117, rr2785977651119, rs2755987661120,
                             rx2765997671121, rl2776007681122);
            break;
        }
        
      case 1:
        {
            return caseFn589(x2725957631117, r2745967641118, rx2765997671121,
                             rl2776007681122);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1337");
            exit(1);
        }
    }
}
GibCursor caseFn601(GibInt x2726027731127, GibCursor r2746037741128)
{
    GibPackedTag tag1340 = *(GibPackedTag *) r2746037741128;
    GibCursor tail1341 = r2746037741128 + sizeof(GibInt);
    
    
  switch1343:
    ;
    switch (tag1340) {
        
      case 1:
        {
            GibPtr fltPkd8731129 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8731129)->field0 = 1;
            
            GibPtr fltPkd8741130 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8741130)->field0 = 1;
            
            GibPtr tailift1342 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1342)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1342)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1342)->field2 =
                x2726027731127;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1342)->field3 =
                fltPkd8731129;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1342)->field4 =
                fltPkd8741130;
            return tailift1342;
            break;
        }
        
      case 0:
        {
            GibInt rs2757751131 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1341)->field0;
            GibInt rx2767761132 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1341)->field1;
            GibCursor rl2777771133 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1341)->field2;
            GibCursor rr2787781134 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1341)->field3;
            
            return caseFn594(x2726027731127, r2746037741128, rr2787781134,
                             rs2757751131, rx2767761132, rl2777771133);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1340");
            exit(1);
        }
    }
}
GibCursor caseFn604(GibCursor l2736057791135, GibInt x2726067801136,
                    GibCursor r2746077811137, GibInt ls2916087821138)
{
    GibPackedTag tag1344 = *(GibPackedTag *) r2746077811137;
    GibCursor tail1345 = r2746077811137 + sizeof(GibInt);
    
    
  switch1350:
    ;
    switch (tag1344) {
        
      case 1:
        {
            GibInt fltPkd8751139 = 1 + ls2916087821138;
            GibPtr fltPkd8761140 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd8761140)->field0 = 1;
            
            GibPtr tailift1346 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1346)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1346)->field1 =
                fltPkd8751139;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1346)->field2 =
                x2726067801136;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1346)->field3 =
                l2736057791135;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1346)->field4 =
                fltPkd8761140;
            return tailift1346;
            break;
        }
        
      case 0:
        {
            GibInt rs2957831141 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1345)->field0;
            GibInt rx2967841142 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1345)->field1;
            GibCursor rl2977851143 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1345)->field2;
            GibCursor rr2987861144 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1345)->field3;
            GibInt fltPrm8781145 = 3 * ls2916087821138;
            GibBool fltIf8771146 = rs2957831141 > fltPrm8781145;
            
            if (fltIf8771146) {
                GibInt fltPrm8801147 =  size208(rl2977851143);
                GibInt fltPrm8821148 =  size208(rr2987861144);
                GibInt fltPrm8811149 = 2 * fltPrm8821148;
                GibBool fltIf8791150 = fltPrm8801147 < fltPrm8811149;
                
                if (fltIf8791150) {
                    GibInt fltPrm8841151 = 1 + ls2916087821138;
                    GibInt fltPkd8831152 = fltPrm8841151 + rs2957831141;
                    GibInt fltPrm8871153 = 1 + ls2916087821138;
                    GibInt fltPrm8881154 =  size208(rl2977851143);
                    GibInt fltPkd8861155 = fltPrm8871153 + fltPrm8881154;
                    GibPtr fltPkd8851156 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8851156)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8851156)->field1 =
                        fltPkd8861155;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8851156)->field2 =
                        x2726067801136;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8851156)->field3 =
                        l2736057791135;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8851156)->field4 =
                        rl2977851143;
                    
                    GibPtr tailift1347 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1347)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1347)->field1 =
                        fltPkd8831152;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1347)->field2 =
                        rx2967841142;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1347)->field3 =
                        fltPkd8851156;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1347)->field4 =
                        rr2987861144;
                    return tailift1347;
                } else {
                    GibInt fltPrm8901157 = 1 + ls2916087821138;
                    GibInt fltPkd8891158 = fltPrm8901157 + rs2957831141;
                    GibInt fltPkd8911159 =  val207(rl2977851143);
                    GibInt fltPrm8941160 = 1 + ls2916087821138;
                    GibCursor fltAppE8961161 =  left206(rl2977851143);
                    GibInt fltPrm8951162 =  size208(fltAppE8961161);
                    GibInt fltPkd8931163 = fltPrm8941160 + fltPrm8951162;
                    GibCursor fltPkd8971164 =  left206(rl2977851143);
                    GibPtr fltPkd8921165 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8921165)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8921165)->field1 =
                        fltPkd8931163;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8921165)->field2 =
                        x2726067801136;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8921165)->field3 =
                        l2736057791135;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8921165)->field4 =
                        fltPkd8971164;
                    
                    GibInt fltPrm9011166 =  size208(rr2987861144);
                    GibInt fltPrm9001167 = 1 + fltPrm9011166;
                    GibCursor fltAppE9031168 =  right205(rl2977851143);
                    GibInt fltPrm9021169 =  size208(fltAppE9031168);
                    GibInt fltPkd8991170 = fltPrm9001167 + fltPrm9021169;
                    GibCursor fltPkd9041171 =  right205(rl2977851143);
                    GibPtr fltPkd8981172 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8981172)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8981172)->field1 =
                        fltPkd8991170;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8981172)->field2 =
                        rx2967841142;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8981172)->field3 =
                        fltPkd9041171;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd8981172)->field4 =
                        rr2987861144;
                    
                    GibPtr tailift1348 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1348)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1348)->field1 =
                        fltPkd8891158;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1348)->field2 =
                        fltPkd8911159;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1348)->field3 =
                        fltPkd8921165;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1348)->field4 =
                        fltPkd8981172;
                    return tailift1348;
                }
            } else {
                GibInt fltPrm9061173 = 1 + ls2916087821138;
                GibInt fltPkd9051174 = fltPrm9061173 + rs2957831141;
                GibPtr tailift1349 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1349)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1349)->field1 =
                    fltPkd9051174;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1349)->field2 =
                    x2726067801136;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1349)->field3 =
                    l2736057791135;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1349)->field4 =
                    r2746077811137;
                return tailift1349;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1344");
            exit(1);
        }
    }
}
GibCursor caseFn609(GibInt x2996107871175, GibCursor ll3046117881176,
                    GibInt ls3026127891177, GibInt lx3036137901178,
                    GibCursor lr3056147911179, GibInt lls3066157921180)
{
    GibPackedTag tag1351 = *(GibPackedTag *) lr3056147911179;
    GibCursor tail1352 = lr3056147911179 + sizeof(GibInt);
    
    
  switch1356:
    ;
    switch (tag1351) {
        
      case 0:
        {
            GibInt lrs3107931181 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1352)->field0;
            GibInt lrx3117941182 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1352)->field1;
            GibCursor lrl3127951183 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1352)->field2;
            GibCursor lrr3137961184 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1352)->field3;
            GibInt fltPrm9081185 = 2 * lls3066157921180;
            GibBool fltIf9071186 = lrs3107931181 < fltPrm9081185;
            
            if (fltIf9071186) {
                GibInt fltPkd9091187 = 1 + ls3026127891177;
                GibInt fltPkd9111188 = 1 + lrs3107931181;
                GibPtr fltPkd9121189 = gib_alloc(sizeof(GibIntProd));
                
                ((GibIntProd *) fltPkd9121189)->field0 = 1;
                
                GibPtr fltPkd9101190 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9101190)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9101190)->field1 =
                    fltPkd9111188;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9101190)->field2 =
                    x2996107871175;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9101190)->field3 =
                    lr3056147911179;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9101190)->field4 =
                    fltPkd9121189;
                
                GibPtr tailift1353 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1353)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1353)->field1 =
                    fltPkd9091187;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1353)->field2 =
                    lx3036137901178;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1353)->field3 =
                    ll3046117881176;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1353)->field4 =
                    fltPkd9101190;
                return tailift1353;
            } else {
                GibInt fltPkd9131191 = 1 + ls3026127891177;
                GibInt fltPrm9161192 = 1 + lls3066157921180;
                GibInt fltPrm9171193 =  size208(lrl3127951183);
                GibInt fltPkd9151194 = fltPrm9161192 + fltPrm9171193;
                GibPtr fltPkd9141195 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9141195)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9141195)->field1 =
                    fltPkd9151194;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9141195)->field2 =
                    lx3036137901178;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9141195)->field3 =
                    ll3046117881176;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9141195)->field4 =
                    lrl3127951183;
                
                GibInt fltPrm9201196 =  size208(lrr3137961184);
                GibInt fltPkd9191197 = 1 + fltPrm9201196;
                GibPtr fltPkd9211198 = gib_alloc(sizeof(GibIntProd));
                
                ((GibIntProd *) fltPkd9211198)->field0 = 1;
                
                GibPtr fltPkd9181199 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9181199)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9181199)->field1 =
                    fltPkd9191197;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9181199)->field2 =
                    x2996107871175;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9181199)->field3 =
                    lrr3137961184;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9181199)->field4 =
                    fltPkd9211198;
                
                GibPtr tailift1354 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1354)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1354)->field1 =
                    fltPkd9131191;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1354)->field2 =
                    lrx3117941182;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1354)->field3 =
                    fltPkd9141195;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1354)->field4 =
                    fltPkd9181199;
                return tailift1354;
            }
            break;
        }
        
      case 1:
        {
            GibPtr fltPkd9231200 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9231200)->field0 = 1;
            
            GibPtr fltPkd9241201 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9241201)->field0 = 1;
            
            GibPtr fltPkd9221202 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9221202)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9221202)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9221202)->field2 =
                x2996107871175;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9221202)->field3 =
                fltPkd9231200;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9221202)->field4 =
                fltPkd9241201;
            
            GibPtr tailift1355 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1355)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1355)->field1 =
                3;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1355)->field2 =
                lx3036137901178;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1355)->field3 =
                ll3046117881176;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1355)->field4 =
                fltPkd9221202;
            return tailift1355;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1351");
            exit(1);
        }
    }
}
GibCursor caseFn616(GibInt x2996177971203, GibCursor l3006187981204,
                    GibInt lx3036197991205, GibCursor lr3056208001206)
{
    GibPackedTag tag1357 = *(GibPackedTag *) lr3056208001206;
    GibCursor tail1358 = lr3056208001206 + sizeof(GibInt);
    
    
  switch1361:
    ;
    switch (tag1357) {
        
      case 0:
        {
            GibInt lrs3148011207 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1358)->field0;
            GibInt lrx3158021208 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1358)->field1;
            GibCursor lrl3168031209 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1358)->field2;
            GibCursor lrr3178041210 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1358)->field3;
            GibPtr fltPkd9261211 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9261211)->field0 = 1;
            
            GibPtr fltPkd9271212 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9271212)->field0 = 1;
            
            GibPtr fltPkd9251213 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9251213)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9251213)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9251213)->field2 =
                lx3036197991205;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9251213)->field3 =
                fltPkd9261211;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9251213)->field4 =
                fltPkd9271212;
            
            GibPtr fltPkd9291214 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9291214)->field0 = 1;
            
            GibPtr fltPkd9301215 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9301215)->field0 = 1;
            
            GibPtr fltPkd9281216 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9281216)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9281216)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9281216)->field2 =
                x2996177971203;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9281216)->field3 =
                fltPkd9291214;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9281216)->field4 =
                fltPkd9301215;
            
            GibPtr tailift1359 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1359)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1359)->field1 =
                3;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1359)->field2 =
                lrx3158021208;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1359)->field3 =
                fltPkd9251213;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1359)->field4 =
                fltPkd9281216;
            return tailift1359;
            break;
        }
        
      case 1:
        {
            GibPtr fltPkd9311217 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9311217)->field0 = 1;
            
            GibPtr tailift1360 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1360)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1360)->field1 =
                2;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1360)->field2 =
                x2996177971203;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1360)->field3 =
                l3006187981204;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1360)->field4 =
                fltPkd9311217;
            return tailift1360;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1357");
            exit(1);
        }
    }
}
GibCursor caseFn621(GibInt x2996228051218, GibCursor l3006238061219,
                    GibCursor ll3046248071220, GibInt ls3026258081221,
                    GibInt lx3036268091222, GibCursor lr3056278101223)
{
    GibPackedTag tag1362 = *(GibPackedTag *) ll3046248071220;
    GibCursor tail1363 = ll3046248071220 + sizeof(GibInt);
    
    
  switch1364:
    ;
    switch (tag1362) {
        
      case 0:
        {
            GibInt lls3068111224 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1363)->field0;
            GibInt llx3078121225 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1363)->field1;
            GibCursor lll3088131226 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1363)->field2;
            GibCursor llr3098141227 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1363)->field3;
            
            return caseFn609(x2996228051218, ll3046248071220, ls3026258081221,
                             lx3036268091222, lr3056278101223, lls3068111224);
            break;
        }
        
      case 1:
        {
            return caseFn616(x2996228051218, l3006238061219, lx3036268091222,
                             lr3056278101223);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1362");
            exit(1);
        }
    }
}
GibCursor caseFn628(GibInt x2996298151228, GibCursor l3006308161229)
{
    GibPackedTag tag1365 = *(GibPackedTag *) l3006308161229;
    GibCursor tail1366 = l3006308161229 + sizeof(GibInt);
    
    
  switch1368:
    ;
    switch (tag1365) {
        
      case 1:
        {
            GibPtr fltPkd9321230 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9321230)->field0 = 1;
            
            GibPtr fltPkd9331231 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9331231)->field0 = 1;
            
            GibPtr tailift1367 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1367)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1367)->field1 =
                1;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1367)->field2 =
                x2996298151228;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1367)->field3 =
                fltPkd9321230;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1367)->field4 =
                fltPkd9331231;
            return tailift1367;
            break;
        }
        
      case 0:
        {
            GibInt ls3028171232 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1366)->field0;
            GibInt lx3038181233 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1366)->field1;
            GibCursor ll3048191234 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1366)->field2;
            GibCursor lr3058201235 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1366)->field3;
            
            return caseFn621(x2996298151228, l3006308161229, ll3048191234,
                             ls3028171232, lx3038181233, lr3058201235);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1365");
            exit(1);
        }
    }
}
GibCursor caseFn631(GibCursor r3016328211236, GibInt x2996338221237,
                    GibCursor l3006348231238, GibInt rs3186358241239)
{
    GibPackedTag tag1369 = *(GibPackedTag *) l3006348231238;
    GibCursor tail1370 = l3006348231238 + sizeof(GibInt);
    
    
  switch1375:
    ;
    switch (tag1369) {
        
      case 1:
        {
            GibInt fltPkd9341240 = 1 + rs3186358241239;
            GibPtr fltPkd9351241 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd9351241)->field0 = 1;
            
            GibPtr tailift1371 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1371)->field0 =
                0;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1371)->field1 =
                fltPkd9341240;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1371)->field2 =
                x2996338221237;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1371)->field3 =
                fltPkd9351241;
            ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1371)->field4 =
                r3016328211236;
            return tailift1371;
            break;
        }
        
      case 0:
        {
            GibInt ls3228251242 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1370)->field0;
            GibInt lx3238261243 =
                   ((GibIntGibIntGibCursorGibCursorProd *) tail1370)->field1;
            GibCursor ll3248271244 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1370)->field2;
            GibCursor lr3258281245 =
                      ((GibIntGibIntGibCursorGibCursorProd *) tail1370)->field3;
            GibInt fltPrm9371246 = 3 * rs3186358241239;
            GibBool fltIf9361247 = ls3228251242 > fltPrm9371246;
            
            if (fltIf9361247) {
                GibInt fltPrm9391248 =  size208(lr3258281245);
                GibInt fltPrm9411249 =  size208(ll3248271244);
                GibInt fltPrm9401250 = 2 * fltPrm9411249;
                GibBool fltIf9381251 = fltPrm9391248 < fltPrm9401250;
                
                if (fltIf9381251) {
                    GibInt fltPrm9431252 = 1 + ls3228251242;
                    GibInt fltPkd9421253 = fltPrm9431252 + rs3186358241239;
                    GibInt fltPrm9461254 = 1 + rs3186358241239;
                    GibInt fltPrm9471255 =  size208(lr3258281245);
                    GibInt fltPkd9451256 = fltPrm9461254 + fltPrm9471255;
                    GibPtr fltPkd9441257 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9441257)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9441257)->field1 =
                        fltPkd9451256;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9441257)->field2 =
                        x2996338221237;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9441257)->field3 =
                        lr3258281245;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9441257)->field4 =
                        r3016328211236;
                    
                    GibPtr tailift1372 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1372)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1372)->field1 =
                        fltPkd9421253;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1372)->field2 =
                        lx3238261243;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1372)->field3 =
                        ll3248271244;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1372)->field4 =
                        fltPkd9441257;
                    return tailift1372;
                } else {
                    GibInt fltPrm9491258 = 1 + ls3228251242;
                    GibInt fltPkd9481259 = fltPrm9491258 + rs3186358241239;
                    GibInt fltPkd9501260 =  val207(lr3258281245);
                    GibInt fltPrm9541261 =  size208(ll3248271244);
                    GibInt fltPrm9531262 = 1 + fltPrm9541261;
                    GibCursor fltAppE9561263 =  left206(lr3258281245);
                    GibInt fltPrm9551264 =  size208(fltAppE9561263);
                    GibInt fltPkd9521265 = fltPrm9531262 + fltPrm9551264;
                    GibCursor fltPkd9571266 =  left206(lr3258281245);
                    GibPtr fltPkd9511267 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9511267)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9511267)->field1 =
                        fltPkd9521265;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9511267)->field2 =
                        lx3238261243;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9511267)->field3 =
                        ll3248271244;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9511267)->field4 =
                        fltPkd9571266;
                    
                    GibInt fltPrm9601268 = 1 + rs3186358241239;
                    GibCursor fltAppE9621269 =  right205(lr3258281245);
                    GibInt fltPrm9611270 =  size208(fltAppE9621269);
                    GibInt fltPkd9591271 = fltPrm9601268 + fltPrm9611270;
                    GibCursor fltPkd9631272 =  right205(lr3258281245);
                    GibPtr fltPkd9581273 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9581273)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9581273)->field1 =
                        fltPkd9591271;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9581273)->field2 =
                        x2996338221237;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9581273)->field3 =
                        fltPkd9631272;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) fltPkd9581273)->field4 =
                        r3016328211236;
                    
                    GibPtr tailift1373 =
                           gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                    
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1373)->field0 =
                        0;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1373)->field1 =
                        fltPkd9481259;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1373)->field2 =
                        fltPkd9501260;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1373)->field3 =
                        fltPkd9511267;
                    ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1373)->field4 =
                        fltPkd9581273;
                    return tailift1373;
                }
            } else {
                GibInt fltPrm9651274 = 1 + ls3228251242;
                GibInt fltPkd9641275 = fltPrm9651274 + rs3186358241239;
                GibPtr tailift1374 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1374)->field0 =
                    0;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1374)->field1 =
                    fltPkd9641275;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1374)->field2 =
                    x2996338221237;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1374)->field3 =
                    l3006348231238;
                ((GibIntGibIntGibIntGibCursorGibCursorProd *) tailift1374)->field4 =
                    r3016328211236;
                return tailift1374;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1369");
            exit(1);
        }
    }
}
int main(int argc, char **argv)
{
    int init0 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibCursor fltAppE830966 =  empty202();
    GibCursor fltAppE829967 =  insert214(0, fltAppE830966);
    GibInt tmp_app1276 =  sum203(fltAppE829967);
    
    printf("%ld", tmp_app1276);
    printf("\n");
    return 0;
    
    int exit1 = gib_exit();
    
    return exit1;
}