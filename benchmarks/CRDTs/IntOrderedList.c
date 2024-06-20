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
typedef struct GibIntGibCursorProd_struct {
            GibInt field0;
            GibCursor field1;
        } GibIntGibCursorProd;
typedef struct GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd_struct {
            GibInt field0;
            GibCursor field1;
            GibInt field2;
            GibCursor field3;
            GibCursor field4;
            GibCursor field5;
        } GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd;
typedef struct GibIntGibCursorGibCursorProd_struct {
            GibInt field0;
            GibCursor field1;
            GibCursor field2;
        } GibIntGibCursorGibCursorProd;
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
typedef struct GibCursorGibIntGibCursorGibCursorGibCursorProd_struct {
            GibCursor field0;
            GibInt field1;
            GibCursor field2;
            GibCursor field3;
            GibCursor field4;
        } GibCursorGibIntGibCursorGibCursorGibCursorProd;
GibCursor compare170(GibCursor a27624343183, GibCursor b27724353184);
GibCursor stamp171(GibInt uid29824413191, GibCursor clk29924423192);
GibCursor step172(GibInt uid30024433193, GibCursor clk30124443194);
GibCursor clockmap173(GibCursor x30724463197);
GibCursor lookup174(GibInt k31024493200, GibCursor clk31124503201);
GibCursor init175(GibInt uid31424533204);
GibInt author176(GibCursor t31524543206);
GibCursor clock177(GibCursor t31824573209);
GibCursor compareInt154(GibInt a33424603212, GibInt b33524613213);
GibInt ratio141();
GibInt delta142();
GibCursor cursor_1151249(GibInt i21724693218, GibCursor x21824703219);
GibCursor value_1131252(GibCursor x21024763228);
GibCursor getHead1111248(GibCursor list20624823235);
GibCursor value1121240(GibCursor x20424853238);
GibCursor insert1181239(GibInt uid19724863240, GibInt x19824873241,
                        GibCursor l19924883242, GibCursor r20024893243,
                        GibCursor list20124903244);
GibCursor cursor1141238(GibInt t19424923250, GibCursor x19524933251);
GibCursor singleton1191237(GibInt uid18924943253, GibInt x19024953254);
GibInt size1491257(GibCursor m49924983259);
GibInt key1481241(GibCursor s49225043265);
GibCursor singleL1361264(GibInt k140425103271, GibInt x140525113272,
                         GibCursor t140625123273, GibCursor m40725133274);
GibCursor doubleL1341265(GibInt k137925193281, GibInt x138025203282,
                         GibCursor t138125213283, GibCursor m038225223284);
GibCursor rotateL1391258(GibInt k42925283290, GibInt x43025293291,
                         GibCursor l43125303292, GibCursor r43225313293);
GibCursor bin1371263(GibInt k41425373304, GibInt x41525383305,
                     GibCursor l41625393306, GibCursor r41725403307);
GibCursor singleR1351260(GibInt k139425413312, GibInt x139525423313,
                         GibCursor m39625433314, GibCursor t339725443315);
GibCursor doubleR1331261(GibInt k136425503322, GibInt x136525513323,
                         GibCursor m036625523324, GibCursor t436725533325);
GibCursor empty1311262();
GibCursor rotateR1381259(GibInt k41925593331, GibInt x42025603332,
                         GibCursor l42125613333, GibCursor r42225623334);
GibCursor balance1401256(GibInt k43925683345, GibInt x44025693346,
                         GibCursor l44125703347, GibCursor r44225713348);
GibCursor lookup1301244(GibInt k34925723369, GibCursor m35025733370);
GibCursor singleton1291245(GibInt k34625793378, GibInt x34725803379);
GibCursor insert1281243(GibInt kx33725813382, GibInt x33825823383,
                        GibCursor m33925833384);
GibCursor getClock1221250(GibCursor list26725893394);
GibCursor getStamp1211254(GibCursor x26025923397);
GibCursor place_1171255(GibCursor x24025983406, GibCursor acc24125993407);
GibCursor place1161251(GibInt uid22526053413, GibCursor clk22626063414,
                       GibInt x22726073415, GibCursor s22826083416,
                       GibCursor l22926093417, GibCursor r23026103418);
GibCursor _copy_Clock178(GibCursor arg206826123420);
GibCursor _copy_without_ptrs_Clock178(GibCursor arg207326173425);
unsigned char _traverse_Clock178(GibCursor arg207826223430);
unsigned char _print_Clock178(GibCursor arg208326263434);
GibCursor _copy_Timestamp179(GibCursor arg209226353443);
GibCursor _copy_without_ptrs_Timestamp179(GibCursor arg209726403448);
unsigned char _traverse_Timestamp179(GibCursor arg210226453453);
unsigned char _print_Timestamp179(GibCursor arg210726493457);
GibCursor _copy_Ord156(GibCursor arg211626583466);
GibCursor _copy_without_ptrs_Ord156(GibCursor arg211726593467);
unsigned char _traverse_Ord156(GibCursor arg211826603468);
unsigned char _print_Ord156(GibCursor arg211926613469);
GibCursor _copy_OrderedNode124_v1246(GibCursor arg212826703478);
GibCursor _copy_without_ptrs_OrderedNode124_v1246(GibCursor arg213926813489);
unsigned char _traverse_OrderedNode124_v1246(GibCursor arg215026923500);
unsigned char _print_OrderedNode124_v1246(GibCursor arg216127023510);
GibCursor _copy_OrderedList123_v1247(GibCursor arg218127223530);
GibCursor _copy_without_ptrs_OrderedList123_v1247(GibCursor arg218627273535);
unsigned char _traverse_OrderedList123_v1247(GibCursor arg219127323540);
unsigned char _print_OrderedList123_v1247(GibCursor arg219627373545);
GibCursor _copy_Node166_v1253(GibCursor arg220527463554);
GibCursor _copy_without_ptrs_Node166_v1253(GibCursor arg221027513559);
unsigned char _traverse_Node166_v1253(GibCursor arg221527563564);
unsigned char _print_Node166_v1253(GibCursor arg222027603568);
GibCursor _copy_Maybe155_v1242(GibCursor arg223127713579);
GibCursor _copy_without_ptrs_Maybe155_v1242(GibCursor arg223427743582);
unsigned char _traverse_Maybe155_v1242(GibCursor arg223727773585);
unsigned char _print_Maybe155_v1242(GibCursor arg224027793587);
GibCursor _copy_Map151_v1236(GibCursor arg224827873595);
GibCursor _copy_without_ptrs_Map151_v1236(GibCursor arg225927983606);
unsigned char _traverse_Map151_v1236(GibCursor arg227028093617);
unsigned char _print_Map151_v1236(GibCursor arg228128173625);
GibCursor caseFn2304(GibCursor b277230528413645);
GibCursor caseFn2306(GibCursor b277230728473652, GibInt k293230828483653);
GibCursor caseFn2309(GibCursor b277231028503656, GibInt k293231128513657,
                     GibInt ax295231228523658);
GibCursor caseFn2313(GibCursor a276231428543661, GibCursor b277231528553662,
                     GibInt k293231628563663);
GibCursor caseFn2317(GibCursor diff297231828583666);
GibCursor caseFn2319(GibCursor diff297232028593667);
GibCursor caseFn2321(GibCursor diff297232228603668);
GibCursor caseFn2323(GibCursor diff297232428613669);
GibCursor caseFn2325(GibCursor a276232628623670, GibCursor b277232728633671);
GibCursor caseFn2328(GibInt uid300232928713681, GibCursor clk301233028723682,
                     GibInt v302233128733683);
GibCursor caseFn2332(GibInt uid300233328763688, GibCursor clk301233428773689);
GibCursor caseFn2335(GibInt i217233628803693, GibCursor n224233728813694,
                     GibCursor t220233828823695);
GibCursor caseFn2339(GibInt k1379234028883703, GibInt x1380234128893704,
                     GibCursor t1381234228903705, GibCursor m1387234328913706,
                     GibInt k2385234428923707, GibInt x2386234528933708,
                     GibCursor t4388234628943709);
GibCursor caseFn2347(GibInt k1364234829003717, GibInt x1365234929013718,
                     GibCursor t4367235029023719, GibCursor m1373235129033720,
                     GibInt k2370235229043721, GibInt x2371235329053722,
                     GibCursor t1372235429063723);
GibCursor caseFn2355(GibCursor x240235629123731, GibCursor acc241235729133732,
                     GibCursor tx243235829143733, GibInt vx244235929153734,
                     GibCursor lx245236029163735, GibCursor rx246236129173736);
GibCursor caseFn2362(GibInt uid225236329233743, GibCursor clk226236429243744,
                     GibInt x227236529253745, GibCursor l229236629263746,
                     GibCursor r230236729273747, GibCursor t235236829283748,
                     GibInt v236236929293749, GibCursor n239237029303750);
GibCursor caseFn2371(GibInt uid225237229313779, GibCursor clk226237329323780,
                     GibInt x227237429333781, GibCursor s228237529343782,
                     GibCursor l229237629353783, GibCursor r230237729363784,
                     GibCursor t232237829373785);
GibCursor caseFn2379(GibInt uid225238029433793, GibCursor clk226238129443794,
                     GibInt x227238229453795, GibCursor l229238329463796,
                     GibCursor r230238429473797, GibCursor t235238529483798,
                     GibInt v236238629493799, GibCursor n239238729503800);
GibCursor caseFn2388(GibInt uid225238929513829, GibCursor clk226239029523830,
                     GibInt x227239129533831, GibCursor s228239229543832,
                     GibCursor l229239329553833, GibCursor r230239429563834,
                     GibCursor t232239529573835);
GibCursor caseFn2396(GibInt uid225239729633843, GibCursor clk226239829643844,
                     GibInt x227239929653845, GibCursor l229240029663846,
                     GibCursor r230240129673847, GibCursor t235240229683848,
                     GibInt v236240329693849, GibCursor n239240429703850);
GibCursor caseFn2405(GibInt uid225240629713879, GibCursor clk226240729723880,
                     GibInt x227240829733881, GibCursor s228240929743882,
                     GibCursor l229241029753883, GibCursor r230241129763884,
                     GibCursor t232241229773885);
GibCursor caseFn2413(GibInt uid225241429833893, GibCursor clk226241529843894,
                     GibInt x227241629853895, GibCursor s228241729863896,
                     GibCursor l229241829873897, GibCursor r230241929883898,
                     GibCursor t232242029893899);
GibCursor caseFn2421(GibInt uid225242229903905, GibCursor clk226242329913906,
                     GibInt x227242429923907, GibCursor s228242529933908,
                     GibCursor l229242629943909, GibCursor r230242729953910,
                     GibCursor t232242829963911);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Clock178_T,
            Map151_v1236_T,
            Maybe155_v1242_T,
            Node166_v1253_T,
            Ord156_T,
            OrderedList123_v1247_T,
            OrderedNode124_v1246_T,
            Timestamp179_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(15);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[3];
    
    error = gib_info_table_insert_packed_dcon(Clock178_T, 0, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Clock178_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map151_v1236_T, 1, 24, 2, 3, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map151_v1236_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map151_v1236_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map151_v1236_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe155_v1242_T, 1, 8, 0, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe155_v1242_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe155_v1242_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe155_v1242_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Node166_v1253_T, 1, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Node166_v1253_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Node166_v1253_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Node166_v1253_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord156_T, 3, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord156_T, 3);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord156_T, 2, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord156_T, 2);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord156_T, 1, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord156_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord156_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord156_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(OrderedList123_v1247_T, 0, 0, 2,
                                              0, 0, field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, OrderedList123_v1247_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(OrderedNode124_v1246_T, 0, 8, 4,
                                              1, 0, field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, OrderedNode124_v1246_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(OrderedNode124_v1246_T, 1, 0, 0,
                                              0, 0, field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, OrderedNode124_v1246_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Timestamp179_T, 0, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Timestamp179_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(3917, ")");
    gib_add_symbol(3918, "(Tip167_v1253");
    gib_add_symbol(3919, "(Tip152_v1236");
    gib_add_symbol(3920, "(Tip126_v1246");
    gib_add_symbol(3921, "(Timestamp180");
    gib_add_symbol(3922, "(OrderedList127_v1247");
    gib_add_symbol(3923, "(Nothing161_v1242");
    gib_add_symbol(3924, "(Lt157");
    gib_add_symbol(3925, "(Just162_v1242");
    gib_add_symbol(3926, "(Gt158");
    gib_add_symbol(3927, "(Eq159");
    gib_add_symbol(3928, "(Clk181");
    gib_add_symbol(3929, "(Cc160");
    gib_add_symbol(3930, "(Bin168_v1253");
    gib_add_symbol(3931, "(Bin153_v1236");
    gib_add_symbol(3932, "(Bin125_v1246");
    gib_add_symbol(3933, " ");
}
GibCursor compare170(GibCursor a27624343183, GibCursor b27724353184)
{
    GibCursor fltCse30013185 =  clockmap173(a27624343183);
    GibPackedTag tag3934 = *(GibPackedTag *) fltCse30013185;
    GibCursor tail3935 = fltCse30013185 + sizeof(GibInt);
    
    
  switch3936:
    ;
    switch (tag3934) {
        
      case 0:
        {
            return caseFn2304(b27724353184);
            break;
        }
        
      case 1:
        {
            GibInt wildcard_5328324363186 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3935)->field0;
            GibInt wildcard_5428424373187 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3935)->field1;
            GibInt wildcard_5528524383188 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3935)->field2;
            GibCursor wildcard_5628624393189 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3935)->field3;
            GibCursor wildcard_5728724403190 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3935)->field4;
            
            return caseFn2325(a27624343183, b27724353184);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3934");
            exit(1);
        }
    }
}
GibCursor stamp171(GibInt uid29824413191, GibCursor clk29924423192)
{
    GibPtr tailift3937 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift3937)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift3937)->field1 = uid29824413191;
    ((GibIntGibIntGibCursorProd *) tailift3937)->field2 = clk29924423192;
    return tailift3937;
}
GibCursor step172(GibInt uid30024433193, GibCursor clk30124443194)
{
    GibCursor fltCse30023195 =  lookup174(uid30024433193, clk30124443194);
    GibPackedTag tag3938 = *(GibPackedTag *) fltCse30023195;
    GibCursor tail3939 = fltCse30023195 + sizeof(GibInt);
    
    
  switch3940:
    ;
    switch (tag3938) {
        
      case 1:
        {
            GibInt v30224453196 = ((GibIntProd *) tail3939)->field0;
            
            return caseFn2328(uid30024433193, clk30124443194, v30224453196);
            break;
        }
        
      case 0:
        {
            return caseFn2332(uid30024433193, clk30124443194);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3938");
            exit(1);
        }
    }
}
GibCursor clockmap173(GibCursor x30724463197)
{
    GibInt tag3941 = ((GibIntGibIntGibCursorProd *) x30724463197)->field0;
    GibInt wildcard_2230824473198 =
           ((GibIntGibIntGibCursorProd *) x30724463197)->field1;
    GibCursor y30924483199 =
              ((GibIntGibIntGibCursorProd *) x30724463197)->field2;
    
    return y30924483199;
}
GibCursor lookup174(GibInt k31024493200, GibCursor clk31124503201)
{
    GibInt tag3942 = ((GibIntGibIntGibCursorProd *) clk31124503201)->field0;
    GibInt wildcard_1831224513202 =
           ((GibIntGibIntGibCursorProd *) clk31124503201)->field1;
    GibCursor m31324523203 =
              ((GibIntGibIntGibCursorProd *) clk31124503201)->field2;
    
    return lookup1301244(k31024493200, m31324523203);
}
GibCursor init175(GibInt uid31424533204)
{
    GibCursor fltPkd30033205 =  singleton1291245(uid31424533204, 0);
    GibPtr tailift3943 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift3943)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift3943)->field1 = 0;
    ((GibIntGibIntGibCursorProd *) tailift3943)->field2 = fltPkd30033205;
    return tailift3943;
}
GibInt author176(GibCursor t31524543206)
{
    GibInt tag3944 = ((GibIntGibIntGibCursorProd *) t31524543206)->field0;
    GibInt a31624553207 = ((GibIntGibIntGibCursorProd *) t31524543206)->field1;
    GibCursor wildcard_1131724563208 =
              ((GibIntGibIntGibCursorProd *) t31524543206)->field2;
    
    return a31624553207;
}
GibCursor clock177(GibCursor t31824573209)
{
    GibInt tag3945 = ((GibIntGibIntGibCursorProd *) t31824573209)->field0;
    GibInt wildcard_731924583210 =
           ((GibIntGibIntGibCursorProd *) t31824573209)->field1;
    GibCursor c32024593211 =
              ((GibIntGibIntGibCursorProd *) t31824573209)->field2;
    
    return c32024593211;
}
GibCursor compareInt154(GibInt a33424603212, GibInt b33524613213)
{
    GibInt sub33624623214 = a33424603212 - b33524613213;
    GibBool fltIf30043215 = sub33624623214 < 0;
    
    if (fltIf30043215) {
        GibPtr tailift3946 = gib_alloc(sizeof(GibIntProd));
        
        ((GibIntProd *) tailift3946)->field0 = 0;
        return tailift3946;
    } else {
        GibBool fltIf30053216 = sub33624623214 > 0;
        
        if (fltIf30053216) {
            GibPtr tailift3947 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift3947)->field0 = 1;
            return tailift3947;
        } else {
            GibBool fltIf30063217 = sub33624623214 == 0;
            
            if (fltIf30063217) {
                GibPtr tailift3948 = gib_alloc(sizeof(GibIntProd));
                
                ((GibIntProd *) tailift3948)->field0 = 2;
                return tailift3948;
            } else {
                GibPtr tailift3949 = gib_alloc(sizeof(GibIntProd));
                
                ((GibIntProd *) tailift3949)->field0 = 3;
                return tailift3949;
            }
        }
    }
}
GibInt ratio141()
{
    return 2;
}
GibInt delta142()
{
    return 4;
}
GibCursor cursor_1151249(GibInt i21724693218, GibCursor x21824703219)
{
    GibPackedTag tag3950 = *(GibPackedTag *) x21824703219;
    GibCursor tail3951 = x21824703219 + sizeof(GibInt);
    
    
  switch3952:
    ;
    switch (tag3950) {
        
      case 1:
        {
            GibInt fltAppE30073220 = 0 - 1;
            GibInt fltAppE30093221 = 0 - 1;
            GibCursor fltAppE30083222 =  init175(fltAppE30093221);
            
            return stamp171(fltAppE30073220, fltAppE30083222);
            break;
        }
        
      case 0:
        {
            GibCursor t22024713223 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3951)->field0;
            GibInt wildcard_8622124723224 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3951)->field1;
            GibCursor wildcard_8722224733225 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3951)->field2;
            GibCursor wildcard_8822324743226 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3951)->field3;
            GibCursor n22424753227 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3951)->field4;
            
            return caseFn2335(i21724693218, n22424753227, t22024713223);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3950");
            exit(1);
        }
    }
}
GibCursor value_1131252(GibCursor x21024763228)
{
    GibPackedTag tag3953 = *(GibPackedTag *) x21024763228;
    GibCursor tail3954 = x21024763228 + sizeof(GibInt);
    
    
  switch3957:
    ;
    switch (tag3953) {
        
      case 1:
        {
            GibPtr tailift3955 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift3955)->field0 = 0;
            return tailift3955;
            break;
        }
        
      case 0:
        {
            GibCursor wildcard_9821224773229 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3954)->field0;
            GibInt v21324783230 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3954)->field1;
            GibCursor wildcard_9921424793231 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3954)->field2;
            GibCursor wildcard_10021524803232 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3954)->field3;
            GibCursor n21624813233 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail3954)->field4;
            GibCursor fltPkd30103234 =  value_1131252(n21624813233);
            GibPtr tailift3956 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift3956)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift3956)->field1 = v21324783230;
            ((GibIntGibIntGibCursorProd *) tailift3956)->field2 =
                fltPkd30103234;
            return tailift3956;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3953");
            exit(1);
        }
    }
}
GibCursor getHead1111248(GibCursor list20624823235)
{
    GibInt tag3958 = ((GibIntGibCursorGibCursorProd *) list20624823235)->field0;
    GibCursor wildcard_120824833236 =
              ((GibIntGibCursorGibCursorProd *) list20624823235)->field1;
    GibCursor node20924843237 =
              ((GibIntGibCursorGibCursorProd *) list20624823235)->field2;
    
    return node20924843237;
}
GibCursor value1121240(GibCursor x20424853238)
{
    GibCursor fltAppE30113239 =  getHead1111248(x20424853238);
    
    return value_1131252(fltAppE30113239);
}
GibCursor insert1181239(GibInt uid19724863240, GibInt x19824873241,
                        GibCursor l19924883242, GibCursor r20024893243,
                        GibCursor list20124903244)
{
    GibCursor fltAppE30123245 =  getClock1221250(list20124903244);
    GibCursor clk20324913246 =  step172(uid19724863240, fltAppE30123245);
    GibCursor fltAppE30143247 =  stamp171(uid19724863240, clk20324913246);
    GibCursor fltAppE30153248 =  getHead1111248(list20124903244);
    GibCursor fltPkd30133249 =
               place1161251(uid19724863240, fltAppE30143247, x19824873241, fltAppE30153248, l19924883242, r20024893243);
    GibPtr tailift3959 = gib_alloc(sizeof(GibIntGibCursorGibCursorProd));
    
    ((GibIntGibCursorGibCursorProd *) tailift3959)->field0 = 0;
    ((GibIntGibCursorGibCursorProd *) tailift3959)->field1 = clk20324913246;
    ((GibIntGibCursorGibCursorProd *) tailift3959)->field2 = fltPkd30133249;
    return tailift3959;
}
GibCursor cursor1141238(GibInt t19424923250, GibCursor x19524933251)
{
    GibCursor fltAppE30163252 =  getHead1111248(x19524933251);
    
    return cursor_1151249(t19424923250, fltAppE30163252);
}
GibCursor singleton1191237(GibInt uid18924943253, GibInt x19024953254)
{
    GibCursor clk19224963255 =  init175(uid18924943253);
    GibCursor t19324973256 =  stamp171(uid18924943253, clk19224963255);
    GibPtr fltPkd30183257 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd30183257)->field0 = 1;
    
    GibPtr fltPkd30173258 =
           gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
    
    ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30173258)->field0 =
        0;
    ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30173258)->field1 =
        t19324973256;
    ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30173258)->field2 =
        x19024953254;
    ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30173258)->field3 =
        t19324973256;
    ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30173258)->field4 =
        t19324973256;
    ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30173258)->field5 =
        fltPkd30183257;
    
    GibPtr tailift3960 = gib_alloc(sizeof(GibIntGibCursorGibCursorProd));
    
    ((GibIntGibCursorGibCursorProd *) tailift3960)->field0 = 0;
    ((GibIntGibCursorGibCursorProd *) tailift3960)->field1 = clk19224963255;
    ((GibIntGibCursorGibCursorProd *) tailift3960)->field2 = fltPkd30173258;
    return tailift3960;
}
GibInt size1491257(GibCursor m49924983259)
{
    GibPackedTag tag3961 = *(GibPackedTag *) m49924983259;
    GibCursor tail3962 = m49924983259 + sizeof(GibInt);
    
    
  switch3963:
    ;
    switch (tag3961) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt sz50124993260 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3962)->field0;
            GibInt wildcard_1850225003261 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3962)->field1;
            GibInt wildcard_1950325013262 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3962)->field2;
            GibCursor wildcard_2050425023263 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3962)->field3;
            GibCursor wildcard_2150525033264 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3962)->field4;
            
            return sz50124993260;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3961");
            exit(1);
        }
    }
}
GibInt key1481241(GibCursor s49225043265)
{
    GibPackedTag tag3964 = *(GibPackedTag *) s49225043265;
    GibCursor tail3965 = s49225043265 + sizeof(GibInt);
    
    
  switch3966:
    ;
    switch (tag3964) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt wildcard_2849425053266 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3965)->field0;
            GibInt k49525063267 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3965)->field1;
            GibInt wildcard_2949625073268 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3965)->field2;
            GibCursor wildcard_3049725083269 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3965)->field3;
            GibCursor wildcard_3149825093270 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3965)->field4;
            
            return k49525063267;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3964");
            exit(1);
        }
    }
}
GibCursor singleL1361264(GibInt k140425103271, GibInt x140525113272,
                         GibCursor t140625123273, GibCursor m40725133274)
{
    GibPackedTag tag3967 = *(GibPackedTag *) m40725133274;
    GibCursor tail3968 = m40725133274 + sizeof(GibInt);
    
    
  switch3969:
    ;
    switch (tag3967) {
        
      case 1:
        {
            GibInt wildcard_15940925143275 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3968)->field0;
            GibInt k241025153276 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3968)->field1;
            GibInt x241125163277 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3968)->field2;
            GibCursor t241225173278 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3968)->field3;
            GibCursor t341325183279 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3968)->field4;
            GibCursor fltAppE30193280 =
                       bin1371263(k140425103271, x140525113272, t140625123273, t241225173278);
            
            return bin1371263(k241025153276, x241125163277, fltAppE30193280,
                              t341325183279);
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3967");
            exit(1);
        }
    }
}
GibCursor doubleL1341265(GibInt k137925193281, GibInt x138025203282,
                         GibCursor t138125213283, GibCursor m038225223284)
{
    GibPackedTag tag3970 = *(GibPackedTag *) m038225223284;
    GibCursor tail3971 = m038225223284 + sizeof(GibInt);
    
    
  switch3972:
    ;
    switch (tag3970) {
        
      case 1:
        {
            GibInt wildcard_17938425233285 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3971)->field0;
            GibInt k238525243286 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3971)->field1;
            GibInt x238625253287 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3971)->field2;
            GibCursor m138725263288 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3971)->field3;
            GibCursor t438825273289 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3971)->field4;
            
            return caseFn2339(k137925193281, x138025203282, t138125213283,
                              m138725263288, k238525243286, x238625253287,
                              t438825273289);
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3970");
            exit(1);
        }
    }
}
GibCursor rotateL1391258(GibInt k42925283290, GibInt x43025293291,
                         GibCursor l43125303292, GibCursor r43225313293)
{
    GibPackedTag tag3973 = *(GibPackedTag *) r43225313293;
    GibCursor tail3974 = r43225313293 + sizeof(GibInt);
    
    
  switch3975:
    ;
    switch (tag3973) {
        
      case 1:
        {
            GibInt wildcard_13043425323294 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3974)->field0;
            GibInt wildcard_13143525333295 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3974)->field1;
            GibInt wildcard_13243625343296 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3974)->field2;
            GibCursor ly43725353297 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3974)->field3;
            GibCursor ry43825363298 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3974)->field4;
            GibInt fltPrm30213299 =  size1491257(ly43725353297);
            GibInt fltPrm30233300 =  ratio141();
            GibInt fltPrm30243301 =  size1491257(ry43825363298);
            GibInt fltPrm30223302 = fltPrm30233300 * fltPrm30243301;
            GibBool fltIf30203303 = fltPrm30213299 < fltPrm30223302;
            
            if (fltIf30203303) {
                return singleL1361264(k42925283290, x43025293291, l43125303292,
                                      r43225313293);
            } else {
                return doubleL1341265(k42925283290, x43025293291, l43125303292,
                                      r43225313293);
            }
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3973");
            exit(1);
        }
    }
}
GibCursor bin1371263(GibInt k41425373304, GibInt x41525383305,
                     GibCursor l41625393306, GibCursor r41725403307)
{
    GibInt fltPrm30273308 =  size1491257(l41625393306);
    GibInt fltPrm30283309 =  size1491257(r41725403307);
    GibInt fltPrm30263310 = fltPrm30273308 + fltPrm30283309;
    GibInt fltPkd30253311 = fltPrm30263310 + 1;
    GibPtr tailift3976 =
           gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
    
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3976)->field0 =
        1;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3976)->field1 =
        fltPkd30253311;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3976)->field2 =
        k41425373304;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3976)->field3 =
        x41525383305;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3976)->field4 =
        l41625393306;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3976)->field5 =
        r41725403307;
    return tailift3976;
}
GibCursor singleR1351260(GibInt k139425413312, GibInt x139525423313,
                         GibCursor m39625433314, GibCursor t339725443315)
{
    GibPackedTag tag3977 = *(GibPackedTag *) m39625433314;
    GibCursor tail3978 = m39625433314 + sizeof(GibInt);
    
    
  switch3979:
    ;
    switch (tag3977) {
        
      case 1:
        {
            GibInt wildcard_16939925453316 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3978)->field0;
            GibInt k240025463317 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3978)->field1;
            GibInt x240125473318 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3978)->field2;
            GibCursor t140225483319 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3978)->field3;
            GibCursor t240325493320 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3978)->field4;
            GibCursor fltAppE30293321 =
                       bin1371263(k139425413312, x139525423313, t240325493320, t339725443315);
            
            return bin1371263(k240025463317, x240125473318, t140225483319,
                              fltAppE30293321);
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3977");
            exit(1);
        }
    }
}
GibCursor doubleR1331261(GibInt k136425503322, GibInt x136525513323,
                         GibCursor m036625523324, GibCursor t436725533325)
{
    GibPackedTag tag3980 = *(GibPackedTag *) m036625523324;
    GibCursor tail3981 = m036625523324 + sizeof(GibInt);
    
    
  switch3982:
    ;
    switch (tag3980) {
        
      case 1:
        {
            GibInt wildcard_19536925543326 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3981)->field0;
            GibInt k237025553327 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3981)->field1;
            GibInt x237125563328 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3981)->field2;
            GibCursor t137225573329 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3981)->field3;
            GibCursor m137325583330 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3981)->field4;
            
            return caseFn2347(k136425503322, x136525513323, t436725533325,
                              m137325583330, k237025553327, x237125563328,
                              t137225573329);
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3980");
            exit(1);
        }
    }
}
GibCursor empty1311262()
{
    GibPtr tailift3983 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) tailift3983)->field0 = 0;
    return tailift3983;
}
GibCursor rotateR1381259(GibInt k41925593331, GibInt x42025603332,
                         GibCursor l42125613333, GibCursor r42225623334)
{
    GibPackedTag tag3984 = *(GibPackedTag *) l42125613333;
    GibCursor tail3985 = l42125613333 + sizeof(GibInt);
    
    
  switch3986:
    ;
    switch (tag3984) {
        
      case 1:
        {
            GibInt wildcard_14242425633335 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3985)->field0;
            GibInt wildcard_14342525643336 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3985)->field1;
            GibInt wildcard_14442625653337 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3985)->field2;
            GibCursor ly42725663338 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3985)->field3;
            GibCursor ry42825673339 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3985)->field4;
            GibInt fltPrm30313340 =  size1491257(ry42825673339);
            GibInt fltPrm30333341 =  ratio141();
            GibInt fltPrm30343342 =  size1491257(ly42725663338);
            GibInt fltPrm30323343 = fltPrm30333341 * fltPrm30343342;
            GibBool fltIf30303344 = fltPrm30313340 < fltPrm30323343;
            
            if (fltIf30303344) {
                return singleR1351260(k41925593331, x42025603332, l42125613333,
                                      r42225623334);
            } else {
                return doubleR1331261(k41925593331, x42025603332, l42125613333,
                                      r42225623334);
            }
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3984");
            exit(1);
        }
    }
}
GibCursor balance1401256(GibInt k43925683345, GibInt x44025693346,
                         GibCursor l44125703347, GibCursor r44225713348)
{
    GibInt fltPrm30373349 =  size1491257(l44125703347);
    GibInt fltPrm30383350 =  size1491257(r44225713348);
    GibInt fltPrm30363351 = fltPrm30373349 + fltPrm30383350;
    GibBool fltIf30353352 = fltPrm30363351 <= 1;
    
    if (fltIf30353352) {
        GibInt fltPrm30403353 =  size1491257(l44125703347);
        GibInt fltPrm30413354 =  size1491257(r44225713348);
        GibInt fltPkd30393355 = fltPrm30403353 + fltPrm30413354;
        GibPtr tailift3987 =
               gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
        
        ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3987)->field0 =
            1;
        ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3987)->field1 =
            fltPkd30393355;
        ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3987)->field2 =
            k43925683345;
        ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3987)->field3 =
            x44025693346;
        ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3987)->field4 =
            l44125703347;
        ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3987)->field5 =
            r44225713348;
        return tailift3987;
    } else {
        GibInt fltPrm30433356 =  size1491257(r44225713348);
        GibInt fltPrm30453357 =  delta142();
        GibInt fltPrm30463358 =  size1491257(l44125703347);
        GibInt fltPrm30443359 = fltPrm30453357 * fltPrm30463358;
        GibBool fltIf30423360 = fltPrm30433356 >= fltPrm30443359;
        
        if (fltIf30423360) {
            return rotateL1391258(k43925683345, x44025693346, l44125703347,
                                  r44225713348);
        } else {
            GibInt fltPrm30483361 =  size1491257(l44125703347);
            GibInt fltPrm30503362 =  delta142();
            GibInt fltPrm30513363 =  size1491257(r44225713348);
            GibInt fltPrm30493364 = fltPrm30503362 * fltPrm30513363;
            GibBool fltIf30473365 = fltPrm30483361 >= fltPrm30493364;
            
            if (fltIf30473365) {
                return rotateR1381259(k43925683345, x44025693346, l44125703347,
                                      r44225713348);
            } else {
                GibInt fltPrm30533366 =  size1491257(l44125703347);
                GibInt fltPrm30543367 =  size1491257(r44225713348);
                GibInt fltPkd30523368 = fltPrm30533366 + fltPrm30543367;
                GibPtr tailift3988 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3988)->field0 =
                    1;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3988)->field1 =
                    fltPkd30523368;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3988)->field2 =
                    k43925683345;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3988)->field3 =
                    x44025693346;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3988)->field4 =
                    l44125703347;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3988)->field5 =
                    r44225713348;
                return tailift3988;
            }
        }
    }
}
GibCursor lookup1301244(GibInt k34925723369, GibCursor m35025733370)
{
    GibPackedTag tag3989 = *(GibPackedTag *) m35025733370;
    GibCursor tail3990 = m35025733370 + sizeof(GibInt);
    
    
  switch3993:
    ;
    switch (tag3989) {
        
      case 0:
        {
            GibPtr tailift3991 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift3991)->field0 = 0;
            return tailift3991;
            break;
        }
        
      case 1:
        {
            GibInt wildcard_4035225743371 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3990)->field0;
            GibInt kx35325753372 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3990)->field1;
            GibInt v35425763373 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3990)->field2;
            GibCursor l35525773374 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3990)->field3;
            GibCursor r35625783375 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3990)->field4;
            GibBool fltIf30553376 = k34925723369 < kx35325753372;
            
            if (fltIf30553376) {
                return lookup1301244(k34925723369, l35525773374);
            } else {
                GibBool fltIf30563377 = k34925723369 > kx35325753372;
                
                if (fltIf30563377) {
                    return lookup1301244(k34925723369, r35625783375);
                } else {
                    GibPtr tailift3992 = gib_alloc(sizeof(GibIntGibIntProd));
                    
                    ((GibIntGibIntProd *) tailift3992)->field0 = 1;
                    ((GibIntGibIntProd *) tailift3992)->field1 = v35425763373;
                    return tailift3992;
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3989");
            exit(1);
        }
    }
}
GibCursor singleton1291245(GibInt k34625793378, GibInt x34725803379)
{
    GibPtr fltPkd30573380 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd30573380)->field0 = 0;
    
    GibPtr fltPkd30583381 = gib_alloc(sizeof(GibIntProd));
    
    ((GibIntProd *) fltPkd30583381)->field0 = 0;
    
    GibPtr tailift3994 =
           gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
    
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3994)->field0 =
        1;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3994)->field1 =
        1;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3994)->field2 =
        k34625793378;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3994)->field3 =
        x34725803379;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3994)->field4 =
        fltPkd30573380;
    ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3994)->field5 =
        fltPkd30583381;
    return tailift3994;
}
GibCursor insert1281243(GibInt kx33725813382, GibInt x33825823383,
                        GibCursor m33925833384)
{
    GibPackedTag tag3995 = *(GibPackedTag *) m33925833384;
    GibCursor tail3996 = m33925833384 + sizeof(GibInt);
    
    
  switch3998:
    ;
    switch (tag3995) {
        
      case 0:
        {
            return singleton1291245(kx33725813382, x33825823383);
            break;
        }
        
      case 1:
        {
            GibInt sz34125843385 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3996)->field0;
            GibInt k34225853386 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3996)->field1;
            GibInt v34325863387 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3996)->field2;
            GibCursor l34425873388 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3996)->field3;
            GibCursor r34525883389 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail3996)->field4;
            GibBool fltIf30593390 = kx33725813382 == k34225853386;
            
            if (fltIf30593390) {
                GibPtr tailift3997 =
                       gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
                
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3997)->field0 =
                    1;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3997)->field1 =
                    sz34125843385;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3997)->field2 =
                    k34225853386;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3997)->field3 =
                    x33825823383;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3997)->field4 =
                    l34425873388;
                ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift3997)->field5 =
                    r34525883389;
                return tailift3997;
            } else {
                GibBool fltIf30603391 = kx33725813382 <= k34225853386;
                
                if (fltIf30603391) {
                    GibCursor fltAppE30613392 =
                               insert1281243(kx33725813382, x33825823383, l34425873388);
                    
                    return balance1401256(k34225853386, v34325863387,
                                          fltAppE30613392, r34525883389);
                } else {
                    GibCursor fltAppE30623393 =
                               insert1281243(kx33725813382, x33825823383, r34525883389);
                    
                    return balance1401256(k34225853386, v34325863387,
                                          l34425873388, fltAppE30623393);
                }
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag3995");
            exit(1);
        }
    }
}
GibCursor getClock1221250(GibCursor list26725893394)
{
    GibInt tag3999 = ((GibIntGibCursorGibCursorProd *) list26725893394)->field0;
    GibCursor c26925903395 =
              ((GibIntGibCursorGibCursorProd *) list26725893394)->field1;
    GibCursor wildcard_527025913396 =
              ((GibIntGibCursorGibCursorProd *) list26725893394)->field2;
    
    return c26925903395;
}
GibCursor getStamp1211254(GibCursor x26025923397)
{
    GibPackedTag tag4000 = *(GibPackedTag *) x26025923397;
    GibCursor tail4001 = x26025923397 + sizeof(GibInt);
    
    
  switch4003:
    ;
    switch (tag4000) {
        
      case 0:
        {
            GibCursor t26225933398 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4001)->field0;
            GibInt wildcard_926325943399 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4001)->field1;
            GibCursor wildcard_1026425953400 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4001)->field2;
            GibCursor wildcard_1126525963401 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4001)->field3;
            GibCursor wildcard_1226625973402 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4001)->field4;
            
            return t26225933398;
            break;
        }
        
      case 1:
        {
            GibInt fltPkd30633403 = 0 - 1;
            GibInt fltAppE30653404 = 0 - 1;
            GibCursor fltPkd30643405 =  init175(fltAppE30653404);
            GibPtr tailift4002 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift4002)->field0 = 0;
            ((GibIntGibIntGibCursorProd *) tailift4002)->field1 =
                fltPkd30633403;
            ((GibIntGibIntGibCursorProd *) tailift4002)->field2 =
                fltPkd30643405;
            return tailift4002;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4000");
            exit(1);
        }
    }
}
GibCursor place_1171255(GibCursor x24025983406, GibCursor acc24125993407)
{
    GibPackedTag tag4004 = *(GibPackedTag *) x24025983406;
    GibCursor tail4005 = x24025983406 + sizeof(GibInt);
    
    
  switch4006:
    ;
    switch (tag4004) {
        
      case 1:
        {
            return acc24125993407;
            break;
        }
        
      case 0:
        {
            GibCursor tx24326003408 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4005)->field0;
            GibInt vx24426013409 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4005)->field1;
            GibCursor lx24526023410 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4005)->field2;
            GibCursor rx24626033411 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4005)->field3;
            GibCursor wildcard_4624726043412 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4005)->field4;
            
            return caseFn2355(x24025983406, acc24125993407, tx24326003408,
                              vx24426013409, lx24526023410, rx24626033411);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4004");
            exit(1);
        }
    }
}
GibCursor place1161251(GibInt uid22526053413, GibCursor clk22626063414,
                       GibInt x22726073415, GibCursor s22826083416,
                       GibCursor l22926093417, GibCursor r23026103418)
{
    GibCursor t23226113419 =  getStamp1211254(s22826083416);
    
    return caseFn2421(uid22526053413, clk22626063414, x22726073415,
                      s22826083416, l22926093417, r23026103418, t23226113419);
}
GibCursor _copy_Clock178(GibCursor arg206826123420)
{
    GibInt tag4007 = ((GibIntGibIntGibCursorProd *) arg206826123420)->field0;
    GibInt x206926133421 =
           ((GibIntGibIntGibCursorProd *) arg206826123420)->field1;
    GibCursor x207026143422 =
              ((GibIntGibIntGibCursorProd *) arg206826123420)->field2;
    GibCursor y207226163424 =  _copy_Map151_v1236(x207026143422);
    GibPtr tailift4008 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift4008)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4008)->field1 = x206926133421;
    ((GibIntGibIntGibCursorProd *) tailift4008)->field2 = y207226163424;
    return tailift4008;
}
GibCursor _copy_without_ptrs_Clock178(GibCursor arg207326173425)
{
    GibInt tag4009 = ((GibIntGibIntGibCursorProd *) arg207326173425)->field0;
    GibInt x207426183426 =
           ((GibIntGibIntGibCursorProd *) arg207326173425)->field1;
    GibCursor x207526193427 =
              ((GibIntGibIntGibCursorProd *) arg207326173425)->field2;
    GibCursor y207726213429 =  _copy_without_ptrs_Map151_v1236(x207526193427);
    GibPtr tailift4010 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift4010)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4010)->field1 = x207426183426;
    ((GibIntGibIntGibCursorProd *) tailift4010)->field2 = y207726213429;
    return tailift4010;
}
unsigned char _traverse_Clock178(GibCursor arg207826223430)
{
    GibInt tag4011 = ((GibIntGibIntGibCursorProd *) arg207826223430)->field0;
    GibInt x207926233431 =
           ((GibIntGibIntGibCursorProd *) arg207826223430)->field1;
    GibCursor x208026243432 =
              ((GibIntGibIntGibCursorProd *) arg207826223430)->field2;
    unsigned char y208226253433 =  _traverse_Map151_v1236(x208026243432);
    
    return 0;
}
unsigned char _print_Clock178(GibCursor arg208326263434)
{
    GibInt tag4012 = ((GibIntGibIntGibCursorProd *) arg208326263434)->field0;
    GibInt x208426273435 =
           ((GibIntGibIntGibCursorProd *) arg208326263434)->field1;
    GibCursor x208526283436 =
              ((GibIntGibIntGibCursorProd *) arg208326263434)->field2;
    unsigned char wildcard208826293437 = gib_print_symbol(3928);
    unsigned char wildcard209126303438 = gib_print_symbol(3933);
    unsigned char y208626313439 = printf("%ld", x208426273435);
    unsigned char wildcard209026323440 = gib_print_symbol(3933);
    unsigned char y208726333441 =  _print_Map151_v1236(x208526283436);
    unsigned char wildcard208926343442 = gib_print_symbol(3917);
    
    return 0;
}
GibCursor _copy_Timestamp179(GibCursor arg209226353443)
{
    GibInt tag4013 = ((GibIntGibIntGibCursorProd *) arg209226353443)->field0;
    GibInt x209326363444 =
           ((GibIntGibIntGibCursorProd *) arg209226353443)->field1;
    GibCursor x209426373445 =
              ((GibIntGibIntGibCursorProd *) arg209226353443)->field2;
    GibCursor y209626393447 =  _copy_Clock178(x209426373445);
    GibPtr tailift4014 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift4014)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4014)->field1 = x209326363444;
    ((GibIntGibIntGibCursorProd *) tailift4014)->field2 = y209626393447;
    return tailift4014;
}
GibCursor _copy_without_ptrs_Timestamp179(GibCursor arg209726403448)
{
    GibInt tag4015 = ((GibIntGibIntGibCursorProd *) arg209726403448)->field0;
    GibInt x209826413449 =
           ((GibIntGibIntGibCursorProd *) arg209726403448)->field1;
    GibCursor x209926423450 =
              ((GibIntGibIntGibCursorProd *) arg209726403448)->field2;
    GibCursor y210126443452 =  _copy_without_ptrs_Clock178(x209926423450);
    GibPtr tailift4016 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift4016)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4016)->field1 = x209826413449;
    ((GibIntGibIntGibCursorProd *) tailift4016)->field2 = y210126443452;
    return tailift4016;
}
unsigned char _traverse_Timestamp179(GibCursor arg210226453453)
{
    GibInt tag4017 = ((GibIntGibIntGibCursorProd *) arg210226453453)->field0;
    GibInt x210326463454 =
           ((GibIntGibIntGibCursorProd *) arg210226453453)->field1;
    GibCursor x210426473455 =
              ((GibIntGibIntGibCursorProd *) arg210226453453)->field2;
    unsigned char y210626483456 =  _traverse_Clock178(x210426473455);
    
    return 0;
}
unsigned char _print_Timestamp179(GibCursor arg210726493457)
{
    GibInt tag4018 = ((GibIntGibIntGibCursorProd *) arg210726493457)->field0;
    GibInt x210826503458 =
           ((GibIntGibIntGibCursorProd *) arg210726493457)->field1;
    GibCursor x210926513459 =
              ((GibIntGibIntGibCursorProd *) arg210726493457)->field2;
    unsigned char wildcard211226523460 = gib_print_symbol(3921);
    unsigned char wildcard211526533461 = gib_print_symbol(3933);
    unsigned char y211026543462 = printf("%ld", x210826503458);
    unsigned char wildcard211426553463 = gib_print_symbol(3933);
    unsigned char y211126563464 =  _print_Clock178(x210926513459);
    unsigned char wildcard211326573465 = gib_print_symbol(3917);
    
    return 0;
}
GibCursor _copy_Ord156(GibCursor arg211626583466)
{
    GibPackedTag tag4019 = *(GibPackedTag *) arg211626583466;
    GibCursor tail4020 = arg211626583466 + sizeof(GibInt);
    
    
  switch4025:
    ;
    switch (tag4019) {
        
      case 0:
        {
            GibPtr tailift4021 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4021)->field0 = 0;
            return tailift4021;
            break;
        }
        
      case 1:
        {
            GibPtr tailift4022 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4022)->field0 = 1;
            return tailift4022;
            break;
        }
        
      case 2:
        {
            GibPtr tailift4023 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4023)->field0 = 2;
            return tailift4023;
            break;
        }
        
      case 3:
        {
            GibPtr tailift4024 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4024)->field0 = 3;
            return tailift4024;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4019");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Ord156(GibCursor arg211726593467)
{
    GibPackedTag tag4026 = *(GibPackedTag *) arg211726593467;
    GibCursor tail4027 = arg211726593467 + sizeof(GibInt);
    
    
  switch4032:
    ;
    switch (tag4026) {
        
      case 0:
        {
            GibPtr tailift4028 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4028)->field0 = 0;
            return tailift4028;
            break;
        }
        
      case 1:
        {
            GibPtr tailift4029 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4029)->field0 = 1;
            return tailift4029;
            break;
        }
        
      case 2:
        {
            GibPtr tailift4030 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4030)->field0 = 2;
            return tailift4030;
            break;
        }
        
      case 3:
        {
            GibPtr tailift4031 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4031)->field0 = 3;
            return tailift4031;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4026");
            exit(1);
        }
    }
}
unsigned char _traverse_Ord156(GibCursor arg211826603468)
{
    GibPackedTag tag4033 = *(GibPackedTag *) arg211826603468;
    GibCursor tail4034 = arg211826603468 + sizeof(GibInt);
    
    
  switch4035:
    ;
    switch (tag4033) {
        
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
            printf("%s\n", "Unknown tag in: tag4033");
            exit(1);
        }
    }
}
unsigned char _print_Ord156(GibCursor arg211926613469)
{
    GibPackedTag tag4036 = *(GibPackedTag *) arg211926613469;
    GibCursor tail4037 = arg211926613469 + sizeof(GibInt);
    
    
  switch4038:
    ;
    switch (tag4036) {
        
      case 0:
        {
            unsigned char wildcard212026623470 = gib_print_symbol(3924);
            unsigned char wildcard212126633471 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard212226643472 = gib_print_symbol(3926);
            unsigned char wildcard212326653473 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard212426663474 = gib_print_symbol(3927);
            unsigned char wildcard212526673475 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard212626683476 = gib_print_symbol(3929);
            unsigned char wildcard212726693477 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4036");
            exit(1);
        }
    }
}
GibCursor _copy_OrderedNode124_v1246(GibCursor arg212826703478)
{
    GibPackedTag tag4039 = *(GibPackedTag *) arg212826703478;
    GibCursor tail4040 = arg212826703478 + sizeof(GibInt);
    
    
  switch4043:
    ;
    switch (tag4039) {
        
      case 0:
        {
            GibCursor x212926713479 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4040)->field0;
            GibInt x213026723480 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4040)->field1;
            GibCursor x213126733481 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4040)->field2;
            GibCursor x213226743482 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4040)->field3;
            GibCursor x213326753483 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4040)->field4;
            GibCursor y213426763484 =  _copy_Timestamp179(x212926713479);
            GibCursor y213626783486 =  _copy_Timestamp179(x213126733481);
            GibCursor y213726793487 =  _copy_Timestamp179(x213226743482);
            GibCursor y213826803488 =
                       _copy_OrderedNode124_v1246(x213326753483);
            GibPtr tailift4041 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4041)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4041)->field1 =
                y213426763484;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4041)->field2 =
                x213026723480;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4041)->field3 =
                y213626783486;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4041)->field4 =
                y213726793487;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4041)->field5 =
                y213826803488;
            return tailift4041;
            break;
        }
        
      case 1:
        {
            GibPtr tailift4042 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4042)->field0 = 1;
            return tailift4042;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4039");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_OrderedNode124_v1246(GibCursor arg213926813489)
{
    GibPackedTag tag4044 = *(GibPackedTag *) arg213926813489;
    GibCursor tail4045 = arg213926813489 + sizeof(GibInt);
    
    
  switch4048:
    ;
    switch (tag4044) {
        
      case 0:
        {
            GibCursor x214026823490 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4045)->field0;
            GibInt x214126833491 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4045)->field1;
            GibCursor x214226843492 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4045)->field2;
            GibCursor x214326853493 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4045)->field3;
            GibCursor x214426863494 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4045)->field4;
            GibCursor y214526873495 =
                       _copy_without_ptrs_Timestamp179(x214026823490);
            GibCursor y214726893497 =
                       _copy_without_ptrs_Timestamp179(x214226843492);
            GibCursor y214826903498 =
                       _copy_without_ptrs_Timestamp179(x214326853493);
            GibCursor y214926913499 =
                       _copy_without_ptrs_OrderedNode124_v1246(x214426863494);
            GibPtr tailift4046 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4046)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4046)->field1 =
                y214526873495;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4046)->field2 =
                x214126833491;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4046)->field3 =
                y214726893497;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4046)->field4 =
                y214826903498;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4046)->field5 =
                y214926913499;
            return tailift4046;
            break;
        }
        
      case 1:
        {
            GibPtr tailift4047 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4047)->field0 = 1;
            return tailift4047;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4044");
            exit(1);
        }
    }
}
unsigned char _traverse_OrderedNode124_v1246(GibCursor arg215026923500)
{
    GibPackedTag tag4049 = *(GibPackedTag *) arg215026923500;
    GibCursor tail4050 = arg215026923500 + sizeof(GibInt);
    
    
  switch4051:
    ;
    switch (tag4049) {
        
      case 0:
        {
            GibCursor x215126933501 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4050)->field0;
            GibInt x215226943502 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4050)->field1;
            GibCursor x215326953503 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4050)->field2;
            GibCursor x215426963504 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4050)->field3;
            GibCursor x215526973505 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4050)->field4;
            unsigned char y215626983506 =
                           _traverse_Timestamp179(x215126933501);
            unsigned char y215826993507 =
                           _traverse_Timestamp179(x215326953503);
            unsigned char y215927003508 =
                           _traverse_Timestamp179(x215426963504);
            unsigned char y216027013509 =
                           _traverse_OrderedNode124_v1246(x215526973505);
            
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
            printf("%s\n", "Unknown tag in: tag4049");
            exit(1);
        }
    }
}
unsigned char _print_OrderedNode124_v1246(GibCursor arg216127023510)
{
    GibPackedTag tag4052 = *(GibPackedTag *) arg216127023510;
    GibCursor tail4053 = arg216127023510 + sizeof(GibInt);
    
    
  switch4054:
    ;
    switch (tag4052) {
        
      case 0:
        {
            GibCursor x216227033511 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4053)->field0;
            GibInt x216327043512 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4053)->field1;
            GibCursor x216427053513 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4053)->field2;
            GibCursor x216527063514 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4053)->field3;
            GibCursor x216627073515 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4053)->field4;
            unsigned char wildcard217227083516 = gib_print_symbol(3932);
            unsigned char wildcard217827093517 = gib_print_symbol(3933);
            unsigned char y216727103518 =  _print_Timestamp179(x216227033511);
            unsigned char wildcard217727113519 = gib_print_symbol(3933);
            unsigned char y216827123520 = printf("%ld", x216327043512);
            unsigned char wildcard217627133521 = gib_print_symbol(3933);
            unsigned char y216927143522 =  _print_Timestamp179(x216427053513);
            unsigned char wildcard217527153523 = gib_print_symbol(3933);
            unsigned char y217027163524 =  _print_Timestamp179(x216527063514);
            unsigned char wildcard217427173525 = gib_print_symbol(3933);
            unsigned char y217127183526 =
                           _print_OrderedNode124_v1246(x216627073515);
            unsigned char wildcard217327193527 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard217927203528 = gib_print_symbol(3920);
            unsigned char wildcard218027213529 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4052");
            exit(1);
        }
    }
}
GibCursor _copy_OrderedList123_v1247(GibCursor arg218127223530)
{
    GibInt tag4055 = ((GibIntGibCursorGibCursorProd *) arg218127223530)->field0;
    GibCursor x218227233531 =
              ((GibIntGibCursorGibCursorProd *) arg218127223530)->field1;
    GibCursor x218327243532 =
              ((GibIntGibCursorGibCursorProd *) arg218127223530)->field2;
    GibCursor y218427253533 =  _copy_Clock178(x218227233531);
    GibCursor y218527263534 =  _copy_OrderedNode124_v1246(x218327243532);
    GibPtr tailift4056 = gib_alloc(sizeof(GibIntGibCursorGibCursorProd));
    
    ((GibIntGibCursorGibCursorProd *) tailift4056)->field0 = 0;
    ((GibIntGibCursorGibCursorProd *) tailift4056)->field1 = y218427253533;
    ((GibIntGibCursorGibCursorProd *) tailift4056)->field2 = y218527263534;
    return tailift4056;
}
GibCursor _copy_without_ptrs_OrderedList123_v1247(GibCursor arg218627273535)
{
    GibInt tag4057 = ((GibIntGibCursorGibCursorProd *) arg218627273535)->field0;
    GibCursor x218727283536 =
              ((GibIntGibCursorGibCursorProd *) arg218627273535)->field1;
    GibCursor x218827293537 =
              ((GibIntGibCursorGibCursorProd *) arg218627273535)->field2;
    GibCursor y218927303538 =  _copy_without_ptrs_Clock178(x218727283536);
    GibCursor y219027313539 =
               _copy_without_ptrs_OrderedNode124_v1246(x218827293537);
    GibPtr tailift4058 = gib_alloc(sizeof(GibIntGibCursorGibCursorProd));
    
    ((GibIntGibCursorGibCursorProd *) tailift4058)->field0 = 0;
    ((GibIntGibCursorGibCursorProd *) tailift4058)->field1 = y218927303538;
    ((GibIntGibCursorGibCursorProd *) tailift4058)->field2 = y219027313539;
    return tailift4058;
}
unsigned char _traverse_OrderedList123_v1247(GibCursor arg219127323540)
{
    GibInt tag4059 = ((GibIntGibCursorGibCursorProd *) arg219127323540)->field0;
    GibCursor x219227333541 =
              ((GibIntGibCursorGibCursorProd *) arg219127323540)->field1;
    GibCursor x219327343542 =
              ((GibIntGibCursorGibCursorProd *) arg219127323540)->field2;
    unsigned char y219427353543 =  _traverse_Clock178(x219227333541);
    unsigned char y219527363544 =
                   _traverse_OrderedNode124_v1246(x219327343542);
    
    return 0;
}
unsigned char _print_OrderedList123_v1247(GibCursor arg219627373545)
{
    GibInt tag4060 = ((GibIntGibCursorGibCursorProd *) arg219627373545)->field0;
    GibCursor x219727383546 =
              ((GibIntGibCursorGibCursorProd *) arg219627373545)->field1;
    GibCursor x219827393547 =
              ((GibIntGibCursorGibCursorProd *) arg219627373545)->field2;
    unsigned char wildcard220127403548 = gib_print_symbol(3922);
    unsigned char wildcard220427413549 = gib_print_symbol(3933);
    unsigned char y219927423550 =  _print_Clock178(x219727383546);
    unsigned char wildcard220327433551 = gib_print_symbol(3933);
    unsigned char y220027443552 =  _print_OrderedNode124_v1246(x219827393547);
    unsigned char wildcard220227453553 = gib_print_symbol(3917);
    
    return 0;
}
GibCursor _copy_Node166_v1253(GibCursor arg220527463554)
{
    GibPackedTag tag4061 = *(GibPackedTag *) arg220527463554;
    GibCursor tail4062 = arg220527463554 + sizeof(GibInt);
    
    
  switch4065:
    ;
    switch (tag4061) {
        
      case 0:
        {
            GibPtr tailift4063 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4063)->field0 = 0;
            return tailift4063;
            break;
        }
        
      case 1:
        {
            GibInt x220627473555 = ((GibIntGibCursorProd *) tail4062)->field0;
            GibCursor x220727483556 =
                      ((GibIntGibCursorProd *) tail4062)->field1;
            GibCursor y220927503558 =  _copy_Node166_v1253(x220727483556);
            GibPtr tailift4064 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift4064)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift4064)->field1 = x220627473555;
            ((GibIntGibIntGibCursorProd *) tailift4064)->field2 = y220927503558;
            return tailift4064;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4061");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Node166_v1253(GibCursor arg221027513559)
{
    GibPackedTag tag4066 = *(GibPackedTag *) arg221027513559;
    GibCursor tail4067 = arg221027513559 + sizeof(GibInt);
    
    
  switch4070:
    ;
    switch (tag4066) {
        
      case 0:
        {
            GibPtr tailift4068 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4068)->field0 = 0;
            return tailift4068;
            break;
        }
        
      case 1:
        {
            GibInt x221127523560 = ((GibIntGibCursorProd *) tail4067)->field0;
            GibCursor x221227533561 =
                      ((GibIntGibCursorProd *) tail4067)->field1;
            GibCursor y221427553563 =
                       _copy_without_ptrs_Node166_v1253(x221227533561);
            GibPtr tailift4069 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
            
            ((GibIntGibIntGibCursorProd *) tailift4069)->field0 = 1;
            ((GibIntGibIntGibCursorProd *) tailift4069)->field1 = x221127523560;
            ((GibIntGibIntGibCursorProd *) tailift4069)->field2 = y221427553563;
            return tailift4069;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4066");
            exit(1);
        }
    }
}
unsigned char _traverse_Node166_v1253(GibCursor arg221527563564)
{
    GibPackedTag tag4071 = *(GibPackedTag *) arg221527563564;
    GibCursor tail4072 = arg221527563564 + sizeof(GibInt);
    
    
  switch4073:
    ;
    switch (tag4071) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x221627573565 = ((GibIntGibCursorProd *) tail4072)->field0;
            GibCursor x221727583566 =
                      ((GibIntGibCursorProd *) tail4072)->field1;
            unsigned char y221927593567 =
                           _traverse_Node166_v1253(x221727583566);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4071");
            exit(1);
        }
    }
}
unsigned char _print_Node166_v1253(GibCursor arg222027603568)
{
    GibPackedTag tag4074 = *(GibPackedTag *) arg222027603568;
    GibCursor tail4075 = arg222027603568 + sizeof(GibInt);
    
    
  switch4076:
    ;
    switch (tag4074) {
        
      case 0:
        {
            unsigned char wildcard222127613569 = gib_print_symbol(3918);
            unsigned char wildcard222227623570 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x222327633571 = ((GibIntGibCursorProd *) tail4075)->field0;
            GibCursor x222427643572 =
                      ((GibIntGibCursorProd *) tail4075)->field1;
            unsigned char wildcard222727653573 = gib_print_symbol(3930);
            unsigned char wildcard223027663574 = gib_print_symbol(3933);
            unsigned char y222527673575 = printf("%ld", x222327633571);
            unsigned char wildcard222927683576 = gib_print_symbol(3933);
            unsigned char y222627693577 =  _print_Node166_v1253(x222427643572);
            unsigned char wildcard222827703578 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4074");
            exit(1);
        }
    }
}
GibCursor _copy_Maybe155_v1242(GibCursor arg223127713579)
{
    GibPackedTag tag4077 = *(GibPackedTag *) arg223127713579;
    GibCursor tail4078 = arg223127713579 + sizeof(GibInt);
    
    
  switch4081:
    ;
    switch (tag4077) {
        
      case 0:
        {
            GibPtr tailift4079 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4079)->field0 = 0;
            return tailift4079;
            break;
        }
        
      case 1:
        {
            GibInt x223227723580 = ((GibIntProd *) tail4078)->field0;
            GibPtr tailift4080 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift4080)->field0 = 1;
            ((GibIntGibIntProd *) tailift4080)->field1 = x223227723580;
            return tailift4080;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4077");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Maybe155_v1242(GibCursor arg223427743582)
{
    GibPackedTag tag4082 = *(GibPackedTag *) arg223427743582;
    GibCursor tail4083 = arg223427743582 + sizeof(GibInt);
    
    
  switch4086:
    ;
    switch (tag4082) {
        
      case 0:
        {
            GibPtr tailift4084 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4084)->field0 = 0;
            return tailift4084;
            break;
        }
        
      case 1:
        {
            GibInt x223527753583 = ((GibIntProd *) tail4083)->field0;
            GibPtr tailift4085 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift4085)->field0 = 1;
            ((GibIntGibIntProd *) tailift4085)->field1 = x223527753583;
            return tailift4085;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4082");
            exit(1);
        }
    }
}
unsigned char _traverse_Maybe155_v1242(GibCursor arg223727773585)
{
    GibPackedTag tag4087 = *(GibPackedTag *) arg223727773585;
    GibCursor tail4088 = arg223727773585 + sizeof(GibInt);
    
    
  switch4089:
    ;
    switch (tag4087) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x223827783586 = ((GibIntProd *) tail4088)->field0;
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4087");
            exit(1);
        }
    }
}
unsigned char _print_Maybe155_v1242(GibCursor arg224027793587)
{
    GibPackedTag tag4090 = *(GibPackedTag *) arg224027793587;
    GibCursor tail4091 = arg224027793587 + sizeof(GibInt);
    
    
  switch4092:
    ;
    switch (tag4090) {
        
      case 0:
        {
            unsigned char wildcard224127803588 = gib_print_symbol(3923);
            unsigned char wildcard224227813589 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x224327823590 = ((GibIntProd *) tail4091)->field0;
            unsigned char wildcard224527833591 = gib_print_symbol(3925);
            unsigned char wildcard224727843592 = gib_print_symbol(3933);
            unsigned char y224427853593 = printf("%ld", x224327823590);
            unsigned char wildcard224627863594 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4090");
            exit(1);
        }
    }
}
GibCursor _copy_Map151_v1236(GibCursor arg224827873595)
{
    GibPackedTag tag4093 = *(GibPackedTag *) arg224827873595;
    GibCursor tail4094 = arg224827873595 + sizeof(GibInt);
    
    
  switch4097:
    ;
    switch (tag4093) {
        
      case 0:
        {
            GibPtr tailift4095 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4095)->field0 = 0;
            return tailift4095;
            break;
        }
        
      case 1:
        {
            GibInt x224927883596 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4094)->field0;
            GibInt x225027893597 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4094)->field1;
            GibInt x225127903598 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4094)->field2;
            GibCursor x225227913599 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4094)->field3;
            GibCursor x225327923600 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4094)->field4;
            GibCursor y225727963604 =  _copy_Map151_v1236(x225227913599);
            GibCursor y225827973605 =  _copy_Map151_v1236(x225327923600);
            GibPtr tailift4096 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4096)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4096)->field1 =
                x224927883596;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4096)->field2 =
                x225027893597;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4096)->field3 =
                x225127903598;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4096)->field4 =
                y225727963604;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4096)->field5 =
                y225827973605;
            return tailift4096;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4093");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Map151_v1236(GibCursor arg225927983606)
{
    GibPackedTag tag4098 = *(GibPackedTag *) arg225927983606;
    GibCursor tail4099 = arg225927983606 + sizeof(GibInt);
    
    
  switch4102:
    ;
    switch (tag4098) {
        
      case 0:
        {
            GibPtr tailift4100 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4100)->field0 = 0;
            return tailift4100;
            break;
        }
        
      case 1:
        {
            GibInt x226027993607 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4099)->field0;
            GibInt x226128003608 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4099)->field1;
            GibInt x226228013609 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4099)->field2;
            GibCursor x226328023610 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4099)->field3;
            GibCursor x226428033611 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4099)->field4;
            GibCursor y226828073615 =
                       _copy_without_ptrs_Map151_v1236(x226328023610);
            GibCursor y226928083616 =
                       _copy_without_ptrs_Map151_v1236(x226428033611);
            GibPtr tailift4101 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4101)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4101)->field1 =
                x226027993607;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4101)->field2 =
                x226128003608;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4101)->field3 =
                x226228013609;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4101)->field4 =
                y226828073615;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift4101)->field5 =
                y226928083616;
            return tailift4101;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4098");
            exit(1);
        }
    }
}
unsigned char _traverse_Map151_v1236(GibCursor arg227028093617)
{
    GibPackedTag tag4103 = *(GibPackedTag *) arg227028093617;
    GibCursor tail4104 = arg227028093617 + sizeof(GibInt);
    
    
  switch4105:
    ;
    switch (tag4103) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x227128103618 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4104)->field0;
            GibInt x227228113619 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4104)->field1;
            GibInt x227328123620 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4104)->field2;
            GibCursor x227428133621 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4104)->field3;
            GibCursor x227528143622 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4104)->field4;
            unsigned char y227928153623 =
                           _traverse_Map151_v1236(x227428133621);
            unsigned char y228028163624 =
                           _traverse_Map151_v1236(x227528143622);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4103");
            exit(1);
        }
    }
}
unsigned char _print_Map151_v1236(GibCursor arg228128173625)
{
    GibPackedTag tag4106 = *(GibPackedTag *) arg228128173625;
    GibCursor tail4107 = arg228128173625 + sizeof(GibInt);
    
    
  switch4108:
    ;
    switch (tag4106) {
        
      case 0:
        {
            unsigned char wildcard228228183626 = gib_print_symbol(3919);
            unsigned char wildcard228328193627 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x228428203628 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4107)->field0;
            GibInt x228528213629 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4107)->field1;
            GibInt x228628223630 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4107)->field2;
            GibCursor x228728233631 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4107)->field3;
            GibCursor x228828243632 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4107)->field4;
            unsigned char wildcard229428253633 = gib_print_symbol(3931);
            unsigned char wildcard230028263634 = gib_print_symbol(3933);
            unsigned char y228928273635 = printf("%ld", x228428203628);
            unsigned char wildcard229928283636 = gib_print_symbol(3933);
            unsigned char y229028293637 = printf("%ld", x228528213629);
            unsigned char wildcard229828303638 = gib_print_symbol(3933);
            unsigned char y229128313639 = printf("%ld", x228628223630);
            unsigned char wildcard229728323640 = gib_print_symbol(3933);
            unsigned char y229228333641 =  _print_Map151_v1236(x228728233631);
            unsigned char wildcard229628343642 = gib_print_symbol(3933);
            unsigned char y229328353643 =  _print_Map151_v1236(x228828243632);
            unsigned char wildcard229528363644 = gib_print_symbol(3917);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4106");
            exit(1);
        }
    }
}
GibCursor caseFn2304(GibCursor b277230528413645)
{
    GibCursor fltCse30663646 =  clockmap173(b277230528413645);
    GibPackedTag tag4109 = *(GibPackedTag *) fltCse30663646;
    GibCursor tail4110 = fltCse30663646 + sizeof(GibInt);
    
    
  switch4113:
    ;
    switch (tag4109) {
        
      case 0:
        {
            GibPtr tailift4111 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4111)->field0 = 2;
            return tailift4111;
            break;
        }
        
      case 1:
        {
            GibInt wildcard_4227828423647 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4110)->field0;
            GibInt wildcard_4327928433648 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4110)->field1;
            GibInt wildcard_4428028443649 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4110)->field2;
            GibCursor wildcard_4528128453650 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4110)->field3;
            GibCursor wildcard_4628228463651 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4110)->field4;
            GibPtr tailift4112 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4112)->field0 = 0;
            return tailift4112;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4109");
            exit(1);
        }
    }
}
GibCursor caseFn2306(GibCursor b277230728473652, GibInt k293230828483653)
{
    GibCursor fltCse30673654 =  lookup174(k293230828483653, b277230728473652);
    GibPackedTag tag4114 = *(GibPackedTag *) fltCse30673654;
    GibCursor tail4115 = fltCse30673654 + sizeof(GibInt);
    
    
  switch4118:
    ;
    switch (tag4114) {
        
      case 0:
        {
            GibPtr tailift4116 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4116)->field0 = 2;
            return tailift4116;
            break;
        }
        
      case 1:
        {
            GibInt wildcard_7429428493655 = ((GibIntProd *) tail4115)->field0;
            GibPtr tailift4117 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4117)->field0 = 0;
            return tailift4117;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4114");
            exit(1);
        }
    }
}
GibCursor caseFn2309(GibCursor b277231028503656, GibInt k293231128513657,
                     GibInt ax295231228523658)
{
    GibCursor fltCse30683659 =  lookup174(k293231128513657, b277231028503656);
    GibPackedTag tag4119 = *(GibPackedTag *) fltCse30683659;
    GibCursor tail4120 = fltCse30683659 + sizeof(GibInt);
    
    
  switch4122:
    ;
    switch (tag4119) {
        
      case 0:
        {
            GibPtr tailift4121 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4121)->field0 = 1;
            return tailift4121;
            break;
        }
        
      case 1:
        {
            GibInt bx29628533660 = ((GibIntProd *) tail4120)->field0;
            
            return compareInt154(ax295231228523658, bx29628533660);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4119");
            exit(1);
        }
    }
}
GibCursor caseFn2313(GibCursor a276231428543661, GibCursor b277231528553662,
                     GibInt k293231628563663)
{
    GibCursor fltCse30693664 =  lookup174(k293231628563663, a276231428543661);
    GibPackedTag tag4123 = *(GibPackedTag *) fltCse30693664;
    GibCursor tail4124 = fltCse30693664 + sizeof(GibInt);
    
    
  switch4125:
    ;
    switch (tag4123) {
        
      case 0:
        {
            return caseFn2306(b277231528553662, k293231628563663);
            break;
        }
        
      case 1:
        {
            GibInt ax29528573665 = ((GibIntProd *) tail4124)->field0;
            
            return caseFn2309(b277231528553662, k293231628563663,
                              ax29528573665);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4123");
            exit(1);
        }
    }
}
GibCursor caseFn2317(GibCursor diff297231828583666)
{
    GibPackedTag tag4126 = *(GibPackedTag *) diff297231828583666;
    GibCursor tail4127 = diff297231828583666 + sizeof(GibInt);
    
    
  switch4132:
    ;
    switch (tag4126) {
        
      case 0:
        {
            GibPtr tailift4128 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4128)->field0 = 0;
            return tailift4128;
            break;
        }
        
      case 2:
        {
            GibPtr tailift4129 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4129)->field0 = 0;
            return tailift4129;
            break;
        }
        
      case 1:
        {
            GibPtr tailift4130 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4130)->field0 = 3;
            return tailift4130;
            break;
        }
        
      case 3:
        {
            GibPtr tailift4131 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4131)->field0 = 3;
            return tailift4131;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4126");
            exit(1);
        }
    }
}
GibCursor caseFn2319(GibCursor diff297232028593667)
{
    GibPackedTag tag4133 = *(GibPackedTag *) diff297232028593667;
    GibCursor tail4134 = diff297232028593667 + sizeof(GibInt);
    
    
  switch4139:
    ;
    switch (tag4133) {
        
      case 1:
        {
            GibPtr tailift4135 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4135)->field0 = 1;
            return tailift4135;
            break;
        }
        
      case 2:
        {
            GibPtr tailift4136 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4136)->field0 = 1;
            return tailift4136;
            break;
        }
        
      case 0:
        {
            GibPtr tailift4137 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4137)->field0 = 3;
            return tailift4137;
            break;
        }
        
      case 3:
        {
            GibPtr tailift4138 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4138)->field0 = 3;
            return tailift4138;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4133");
            exit(1);
        }
    }
}
GibCursor caseFn2321(GibCursor diff297232228603668)
{
    GibPackedTag tag4140 = *(GibPackedTag *) diff297232228603668;
    GibCursor tail4141 = diff297232228603668 + sizeof(GibInt);
    
    
  switch4146:
    ;
    switch (tag4140) {
        
      case 2:
        {
            GibPtr tailift4142 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4142)->field0 = 2;
            return tailift4142;
            break;
        }
        
      case 0:
        {
            GibPtr tailift4143 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4143)->field0 = 3;
            return tailift4143;
            break;
        }
        
      case 1:
        {
            GibPtr tailift4144 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4144)->field0 = 3;
            return tailift4144;
            break;
        }
        
      case 3:
        {
            GibPtr tailift4145 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4145)->field0 = 3;
            return tailift4145;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4140");
            exit(1);
        }
    }
}
GibCursor caseFn2323(GibCursor diff297232428613669)
{
    GibPackedTag tag4147 = *(GibPackedTag *) diff297232428613669;
    GibCursor tail4148 = diff297232428613669 + sizeof(GibInt);
    
    
  switch4150:
    ;
    switch (tag4147) {
        
      case 0:
        {
            return caseFn2317(diff297232428613669);
            break;
        }
        
      case 1:
        {
            return caseFn2319(diff297232428613669);
            break;
        }
        
      case 2:
        {
            return caseFn2321(diff297232428613669);
            break;
        }
        
      case 3:
        {
            GibPtr tailift4149 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4149)->field0 = 3;
            return tailift4149;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4147");
            exit(1);
        }
    }
}
GibCursor caseFn2325(GibCursor a276232628623670, GibCursor b277232728633671)
{
    GibCursor fltCse30703672 =  clockmap173(b277232728633671);
    GibPackedTag tag4151 = *(GibPackedTag *) fltCse30703672;
    GibCursor tail4152 = fltCse30703672 + sizeof(GibInt);
    
    
  switch4154:
    ;
    switch (tag4151) {
        
      case 0:
        {
            GibPtr tailift4153 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift4153)->field0 = 1;
            return tailift4153;
            break;
        }
        
      case 1:
        {
            GibInt wildcard_5928828643673 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4152)->field0;
            GibInt wildcard_6028928653674 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4152)->field1;
            GibInt wildcard_6129028663675 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4152)->field2;
            GibCursor wildcard_6229128673676 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4152)->field3;
            GibCursor wildcard_6329228683677 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4152)->field4;
            GibCursor fltAppE30713678 =  clockmap173(a276232628623670);
            GibInt k29328693679 =  key1481241(fltAppE30713678);
            GibCursor diff29728703680 =
                       caseFn2313(a276232628623670, b277232728633671, k29328693679);
            
            return caseFn2323(diff29728703680);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4151");
            exit(1);
        }
    }
}
GibCursor caseFn2328(GibInt uid300232928713681, GibCursor clk301233028723682,
                     GibInt v302233128733683)
{
    GibInt tag4155 = ((GibIntGibIntGibCursorProd *) clk301233028723682)->field0;
    GibInt wildcard_2730328743684 =
           ((GibIntGibIntGibCursorProd *) clk301233028723682)->field1;
    GibCursor m30428753685 =
              ((GibIntGibIntGibCursorProd *) clk301233028723682)->field2;
    GibInt fltAppE30733686 = v302233128733683 + 1;
    GibCursor fltPkd30723687 =
               insert1281243(uid300232928713681, fltAppE30733686, m30428753685);
    GibPtr tailift4156 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift4156)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4156)->field1 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4156)->field2 = fltPkd30723687;
    return tailift4156;
}
GibCursor caseFn2332(GibInt uid300233328763688, GibCursor clk301233428773689)
{
    GibInt tag4157 = ((GibIntGibIntGibCursorProd *) clk301233428773689)->field0;
    GibInt wildcard_3230528783690 =
           ((GibIntGibIntGibCursorProd *) clk301233428773689)->field1;
    GibCursor m30628793691 =
              ((GibIntGibIntGibCursorProd *) clk301233428773689)->field2;
    GibCursor fltPkd30743692 =
               insert1281243(uid300233328763688, 1, m30628793691);
    GibPtr tailift4158 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift4158)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4158)->field1 = 0;
    ((GibIntGibIntGibCursorProd *) tailift4158)->field2 = fltPkd30743692;
    return tailift4158;
}
GibCursor caseFn2335(GibInt i217233628803693, GibCursor n224233728813694,
                     GibCursor t220233828823695)
{
    GibPackedTag tag4159 = *(GibPackedTag *) n224233728813694;
    GibCursor tail4160 = n224233728813694 + sizeof(GibInt);
    
    
  switch4161:
    ;
    switch (tag4159) {
        
      case 1:
        {
            return t220233828823695;
            break;
        }
        
      case 0:
        {
            GibCursor wildcard54128833696 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4160)->field0;
            GibInt wildcard54228843697 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4160)->field1;
            GibCursor wildcard54328853698 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4160)->field2;
            GibCursor wildcard54428863699 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4160)->field3;
            GibCursor wildcard54528873700 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4160)->field4;
            GibBool fltIf30753701 = i217233628803693 == 0;
            
            if (fltIf30753701) {
                return t220233828823695;
            } else {
                GibInt fltAppE30763702 = i217233628803693 - 1;
                
                return cursor_1151249(fltAppE30763702, n224233728813694);
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4159");
            exit(1);
        }
    }
}
GibCursor caseFn2339(GibInt k1379234028883703, GibInt x1380234128893704,
                     GibCursor t1381234228903705, GibCursor m1387234328913706,
                     GibInt k2385234428923707, GibInt x2386234528933708,
                     GibCursor t4388234628943709)
{
    GibPackedTag tag4162 = *(GibPackedTag *) m1387234328913706;
    GibCursor tail4163 = m1387234328913706 + sizeof(GibInt);
    
    
  switch4164:
    ;
    switch (tag4162) {
        
      case 1:
        {
            GibInt wildcard_18038928953710 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4163)->field0;
            GibInt k339028963711 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4163)->field1;
            GibInt x339128973712 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4163)->field2;
            GibCursor t239228983713 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4163)->field3;
            GibCursor t339328993714 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4163)->field4;
            GibCursor fltAppE30773715 =
                       bin1371263(k1379234028883703, x1380234128893704, t1381234228903705, t239228983713);
            GibCursor fltAppE30783716 =
                       bin1371263(k2385234428923707, x2386234528933708, t339328993714, t4388234628943709);
            
            return bin1371263(k339028963711, x339128973712, fltAppE30773715,
                              fltAppE30783716);
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4162");
            exit(1);
        }
    }
}
GibCursor caseFn2347(GibInt k1364234829003717, GibInt x1365234929013718,
                     GibCursor t4367235029023719, GibCursor m1373235129033720,
                     GibInt k2370235229043721, GibInt x2371235329053722,
                     GibCursor t1372235429063723)
{
    GibPackedTag tag4165 = *(GibPackedTag *) m1373235129033720;
    GibCursor tail4166 = m1373235129033720 + sizeof(GibInt);
    
    
  switch4167:
    ;
    switch (tag4165) {
        
      case 1:
        {
            GibInt wildcard_19637429073724 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4166)->field0;
            GibInt k337529083725 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4166)->field1;
            GibInt x337629093726 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4166)->field2;
            GibCursor t237729103727 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4166)->field3;
            GibCursor t337829113728 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail4166)->field4;
            GibCursor fltAppE30793729 =
                       bin1371263(k2370235229043721, x2371235329053722, t1372235429063723, t237729103727);
            GibCursor fltAppE30803730 =
                       bin1371263(k1364234829003717, x1365234929013718, t337829113728, t4367235029023719);
            
            return bin1371263(k337529083725, x337629093726, fltAppE30793729,
                              fltAppE30803730);
            break;
        }
        
      case 0:
        {
            return empty1311262();
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4165");
            exit(1);
        }
    }
}
GibCursor caseFn2355(GibCursor x240235629123731, GibCursor acc241235729133732,
                     GibCursor tx243235829143733, GibInt vx244235929153734,
                     GibCursor lx245236029163735, GibCursor rx246236129173736)
{
    GibPackedTag tag4168 = *(GibPackedTag *) acc241235729133732;
    GibCursor tail4169 = acc241235729133732 + sizeof(GibInt);
    
    
  switch4171:
    ;
    switch (tag4168) {
        
      case 1:
        {
            return x240235629123731;
            break;
        }
        
      case 0:
        {
            GibCursor tacc24829183737 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4169)->field0;
            GibInt vacc24929193738 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4169)->field1;
            GibCursor lacc25029203739 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4169)->field2;
            GibCursor racc25129213740 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4169)->field3;
            GibCursor nacc25229223741 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4169)->field4;
            GibPtr fltPkd30813742 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30813742)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30813742)->field1 =
                tx243235829143733;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30813742)->field2 =
                vx244235929153734;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30813742)->field3 =
                lx245236029163735;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30813742)->field4 =
                rx246236129173736;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30813742)->field5 =
                nacc25229223741;
            
            GibPtr tailift4170 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4170)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4170)->field1 =
                tacc24829183737;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4170)->field2 =
                vacc24929193738;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4170)->field3 =
                lacc25029203739;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4170)->field4 =
                racc25129213740;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4170)->field5 =
                fltPkd30813742;
            return tailift4170;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4168");
            exit(1);
        }
    }
}
GibCursor caseFn2362(GibInt uid225236329233743, GibCursor clk226236429243744,
                     GibInt x227236529253745, GibCursor l229236629263746,
                     GibCursor r230236729273747, GibCursor t235236829283748,
                     GibInt v236236929293749, GibCursor n239237029303750)
{
    GibCursor fltAppE30833751 =  clock177(t235236829283748);
    GibCursor fltAppE30843752 =  clock177(l229236629263746);
    GibCursor fltCse30823753 =  compare170(fltAppE30833751, fltAppE30843752);
    GibPackedTag tag4172 = *(GibPackedTag *) fltCse30823753;
    GibCursor tail4173 = fltCse30823753 + sizeof(GibInt);
    
    
  switch4181:
    ;
    switch (tag4172) {
        
      case 2:
        {
            GibCursor fltAppE30873754 =  clock177(clk226236429243744);
            GibCursor fltPkd30863755 =
                       stamp171(uid225236329233743, fltAppE30873754);
            GibCursor fltPkd30883756 =
                       place1161251(uid225236329233743, clk226236429243744, x227236529253745, n239237029303750, l229236629263746, r230236729273747);
            GibPtr fltPkd30853757 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30853757)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30853757)->field1 =
                fltPkd30863755;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30853757)->field2 =
                x227236529253745;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30853757)->field3 =
                l229236629263746;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30853757)->field4 =
                r230236729273747;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30853757)->field5 =
                fltPkd30883756;
            
            GibPtr tailift4174 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4174)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4174)->field1 =
                t235236829283748;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4174)->field2 =
                v236236929293749;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4174)->field3 =
                l229236629263746;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4174)->field4 =
                r230236729273747;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4174)->field5 =
                fltPkd30853757;
            return tailift4174;
            break;
        }
        
      case 0:
        {
            GibInt fltPrm30903758 =  author176(t235236829283748);
            GibBool fltIf30893759 = uid225236329233743 < fltPrm30903758;
            
            if (fltIf30893759) {
                GibCursor fltPkd30913760 =
                           place1161251(uid225236329233743, clk226236429243744, x227236529253745, n239237029303750, l229236629263746, r230236729273747);
                GibPtr tailift4175 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4175)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4175)->field1 =
                    t235236829283748;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4175)->field2 =
                    v236236929293749;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4175)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4175)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4175)->field5 =
                    fltPkd30913760;
                return tailift4175;
            } else {
                GibCursor fltAppE30943761 =  clock177(clk226236429243744);
                GibCursor fltPkd30933762 =
                           stamp171(uid225236329233743, fltAppE30943761);
                GibCursor fltPkd30953763 =
                           place1161251(uid225236329233743, clk226236429243744, x227236529253745, n239237029303750, l229236629263746, r230236729273747);
                GibPtr fltPkd30923764 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30923764)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30923764)->field1 =
                    fltPkd30933762;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30923764)->field2 =
                    x227236529253745;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30923764)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30923764)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30923764)->field5 =
                    fltPkd30953763;
                
                GibPtr tailift4176 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4176)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4176)->field1 =
                    t235236829283748;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4176)->field2 =
                    v236236929293749;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4176)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4176)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4176)->field5 =
                    fltPkd30923764;
                return tailift4176;
            }
            break;
        }
        
      case 1:
        {
            GibInt fltPrm30973765 =  author176(t235236829283748);
            GibBool fltIf30963766 = uid225236329233743 < fltPrm30973765;
            
            if (fltIf30963766) {
                GibCursor fltPkd30983767 =
                           place1161251(uid225236329233743, clk226236429243744, x227236529253745, n239237029303750, l229236629263746, r230236729273747);
                GibPtr tailift4177 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4177)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4177)->field1 =
                    t235236829283748;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4177)->field2 =
                    v236236929293749;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4177)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4177)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4177)->field5 =
                    fltPkd30983767;
                return tailift4177;
            } else {
                GibCursor fltAppE31013768 =  clock177(clk226236429243744);
                GibCursor fltPkd31003769 =
                           stamp171(uid225236329233743, fltAppE31013768);
                GibCursor fltPkd31023770 =
                           place1161251(uid225236329233743, clk226236429243744, x227236529253745, n239237029303750, l229236629263746, r230236729273747);
                GibPtr fltPkd30993771 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30993771)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30993771)->field1 =
                    fltPkd31003769;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30993771)->field2 =
                    x227236529253745;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30993771)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30993771)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd30993771)->field5 =
                    fltPkd31023770;
                
                GibPtr tailift4178 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4178)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4178)->field1 =
                    t235236829283748;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4178)->field2 =
                    v236236929293749;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4178)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4178)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4178)->field5 =
                    fltPkd30993771;
                return tailift4178;
            }
            break;
        }
        
      case 3:
        {
            GibInt fltPrm31043772 =  author176(t235236829283748);
            GibBool fltIf31033773 = uid225236329233743 < fltPrm31043772;
            
            if (fltIf31033773) {
                GibCursor fltPkd31053774 =
                           place1161251(uid225236329233743, clk226236429243744, x227236529253745, n239237029303750, l229236629263746, r230236729273747);
                GibPtr tailift4179 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4179)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4179)->field1 =
                    t235236829283748;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4179)->field2 =
                    v236236929293749;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4179)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4179)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4179)->field5 =
                    fltPkd31053774;
                return tailift4179;
            } else {
                GibCursor fltAppE31083775 =  clock177(clk226236429243744);
                GibCursor fltPkd31073776 =
                           stamp171(uid225236329233743, fltAppE31083775);
                GibCursor fltPkd31093777 =
                           place1161251(uid225236329233743, clk226236429243744, x227236529253745, n239237029303750, l229236629263746, r230236729273747);
                GibPtr fltPkd31063778 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31063778)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31063778)->field1 =
                    fltPkd31073776;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31063778)->field2 =
                    x227236529253745;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31063778)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31063778)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31063778)->field5 =
                    fltPkd31093777;
                
                GibPtr tailift4180 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4180)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4180)->field1 =
                    t235236829283748;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4180)->field2 =
                    v236236929293749;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4180)->field3 =
                    l229236629263746;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4180)->field4 =
                    r230236729273747;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4180)->field5 =
                    fltPkd31063778;
                return tailift4180;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4172");
            exit(1);
        }
    }
}
GibCursor caseFn2371(GibInt uid225237229313779, GibCursor clk226237329323780,
                     GibInt x227237429333781, GibCursor s228237529343782,
                     GibCursor l229237629353783, GibCursor r230237729363784,
                     GibCursor t232237829373785)
{
    GibPackedTag tag4182 = *(GibPackedTag *) s228237529343782;
    GibCursor tail4183 = s228237529343782 + sizeof(GibInt);
    
    
  switch4184:
    ;
    switch (tag4182) {
        
      case 1:
        {
            GibPtr fltPkd31113786 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd31113786)->field0 = 1;
            
            GibPtr fltAppE31103787 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31103787)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31103787)->field1 =
                t232237829373785;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31103787)->field2 =
                x227237429333781;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31103787)->field3 =
                l229237629353783;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31103787)->field4 =
                r230237729363784;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31103787)->field5 =
                fltPkd31113786;
            return place_1171255(fltAppE31103787, s228237529343782);
            break;
        }
        
      case 0:
        {
            GibCursor t23529383788 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4183)->field0;
            GibInt v23629393789 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4183)->field1;
            GibCursor ls23729403790 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4183)->field2;
            GibCursor rs23829413791 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4183)->field3;
            GibCursor n23929423792 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4183)->field4;
            
            return caseFn2362(uid225237229313779, clk226237329323780,
                              x227237429333781, l229237629353783,
                              r230237729363784, t23529383788, v23629393789,
                              n23929423792);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4182");
            exit(1);
        }
    }
}
GibCursor caseFn2379(GibInt uid225238029433793, GibCursor clk226238129443794,
                     GibInt x227238229453795, GibCursor l229238329463796,
                     GibCursor r230238429473797, GibCursor t235238529483798,
                     GibInt v236238629493799, GibCursor n239238729503800)
{
    GibCursor fltAppE31133801 =  clock177(t235238529483798);
    GibCursor fltAppE31143802 =  clock177(l229238329463796);
    GibCursor fltCse31123803 =  compare170(fltAppE31133801, fltAppE31143802);
    GibPackedTag tag4185 = *(GibPackedTag *) fltCse31123803;
    GibCursor tail4186 = fltCse31123803 + sizeof(GibInt);
    
    
  switch4194:
    ;
    switch (tag4185) {
        
      case 2:
        {
            GibCursor fltAppE31173804 =  clock177(clk226238129443794);
            GibCursor fltPkd31163805 =
                       stamp171(uid225238029433793, fltAppE31173804);
            GibCursor fltPkd31183806 =
                       place1161251(uid225238029433793, clk226238129443794, x227238229453795, n239238729503800, l229238329463796, r230238429473797);
            GibPtr fltPkd31153807 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31153807)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31153807)->field1 =
                fltPkd31163805;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31153807)->field2 =
                x227238229453795;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31153807)->field3 =
                l229238329463796;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31153807)->field4 =
                r230238429473797;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31153807)->field5 =
                fltPkd31183806;
            
            GibPtr tailift4187 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4187)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4187)->field1 =
                t235238529483798;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4187)->field2 =
                v236238629493799;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4187)->field3 =
                l229238329463796;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4187)->field4 =
                r230238429473797;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4187)->field5 =
                fltPkd31153807;
            return tailift4187;
            break;
        }
        
      case 0:
        {
            GibInt fltPrm31203808 =  author176(t235238529483798);
            GibBool fltIf31193809 = uid225238029433793 < fltPrm31203808;
            
            if (fltIf31193809) {
                GibCursor fltPkd31213810 =
                           place1161251(uid225238029433793, clk226238129443794, x227238229453795, n239238729503800, l229238329463796, r230238429473797);
                GibPtr tailift4188 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4188)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4188)->field1 =
                    t235238529483798;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4188)->field2 =
                    v236238629493799;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4188)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4188)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4188)->field5 =
                    fltPkd31213810;
                return tailift4188;
            } else {
                GibCursor fltAppE31243811 =  clock177(clk226238129443794);
                GibCursor fltPkd31233812 =
                           stamp171(uid225238029433793, fltAppE31243811);
                GibCursor fltPkd31253813 =
                           place1161251(uid225238029433793, clk226238129443794, x227238229453795, n239238729503800, l229238329463796, r230238429473797);
                GibPtr fltPkd31223814 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31223814)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31223814)->field1 =
                    fltPkd31233812;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31223814)->field2 =
                    x227238229453795;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31223814)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31223814)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31223814)->field5 =
                    fltPkd31253813;
                
                GibPtr tailift4189 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4189)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4189)->field1 =
                    t235238529483798;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4189)->field2 =
                    v236238629493799;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4189)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4189)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4189)->field5 =
                    fltPkd31223814;
                return tailift4189;
            }
            break;
        }
        
      case 1:
        {
            GibInt fltPrm31273815 =  author176(t235238529483798);
            GibBool fltIf31263816 = uid225238029433793 < fltPrm31273815;
            
            if (fltIf31263816) {
                GibCursor fltPkd31283817 =
                           place1161251(uid225238029433793, clk226238129443794, x227238229453795, n239238729503800, l229238329463796, r230238429473797);
                GibPtr tailift4190 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4190)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4190)->field1 =
                    t235238529483798;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4190)->field2 =
                    v236238629493799;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4190)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4190)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4190)->field5 =
                    fltPkd31283817;
                return tailift4190;
            } else {
                GibCursor fltAppE31313818 =  clock177(clk226238129443794);
                GibCursor fltPkd31303819 =
                           stamp171(uid225238029433793, fltAppE31313818);
                GibCursor fltPkd31323820 =
                           place1161251(uid225238029433793, clk226238129443794, x227238229453795, n239238729503800, l229238329463796, r230238429473797);
                GibPtr fltPkd31293821 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31293821)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31293821)->field1 =
                    fltPkd31303819;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31293821)->field2 =
                    x227238229453795;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31293821)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31293821)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31293821)->field5 =
                    fltPkd31323820;
                
                GibPtr tailift4191 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4191)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4191)->field1 =
                    t235238529483798;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4191)->field2 =
                    v236238629493799;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4191)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4191)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4191)->field5 =
                    fltPkd31293821;
                return tailift4191;
            }
            break;
        }
        
      case 3:
        {
            GibInt fltPrm31343822 =  author176(t235238529483798);
            GibBool fltIf31333823 = uid225238029433793 < fltPrm31343822;
            
            if (fltIf31333823) {
                GibCursor fltPkd31353824 =
                           place1161251(uid225238029433793, clk226238129443794, x227238229453795, n239238729503800, l229238329463796, r230238429473797);
                GibPtr tailift4192 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4192)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4192)->field1 =
                    t235238529483798;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4192)->field2 =
                    v236238629493799;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4192)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4192)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4192)->field5 =
                    fltPkd31353824;
                return tailift4192;
            } else {
                GibCursor fltAppE31383825 =  clock177(clk226238129443794);
                GibCursor fltPkd31373826 =
                           stamp171(uid225238029433793, fltAppE31383825);
                GibCursor fltPkd31393827 =
                           place1161251(uid225238029433793, clk226238129443794, x227238229453795, n239238729503800, l229238329463796, r230238429473797);
                GibPtr fltPkd31363828 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31363828)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31363828)->field1 =
                    fltPkd31373826;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31363828)->field2 =
                    x227238229453795;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31363828)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31363828)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31363828)->field5 =
                    fltPkd31393827;
                
                GibPtr tailift4193 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4193)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4193)->field1 =
                    t235238529483798;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4193)->field2 =
                    v236238629493799;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4193)->field3 =
                    l229238329463796;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4193)->field4 =
                    r230238429473797;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4193)->field5 =
                    fltPkd31363828;
                return tailift4193;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4185");
            exit(1);
        }
    }
}
GibCursor caseFn2388(GibInt uid225238929513829, GibCursor clk226239029523830,
                     GibInt x227239129533831, GibCursor s228239229543832,
                     GibCursor l229239329553833, GibCursor r230239429563834,
                     GibCursor t232239529573835)
{
    GibPackedTag tag4195 = *(GibPackedTag *) s228239229543832;
    GibCursor tail4196 = s228239229543832 + sizeof(GibInt);
    
    
  switch4197:
    ;
    switch (tag4195) {
        
      case 1:
        {
            GibPtr fltPkd31413836 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd31413836)->field0 = 1;
            
            GibPtr fltAppE31403837 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31403837)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31403837)->field1 =
                t232239529573835;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31403837)->field2 =
                x227239129533831;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31403837)->field3 =
                l229239329553833;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31403837)->field4 =
                r230239429563834;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31403837)->field5 =
                fltPkd31413836;
            return place_1171255(fltAppE31403837, s228239229543832);
            break;
        }
        
      case 0:
        {
            GibCursor t23529583838 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4196)->field0;
            GibInt v23629593839 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4196)->field1;
            GibCursor ls23729603840 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4196)->field2;
            GibCursor rs23829613841 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4196)->field3;
            GibCursor n23929623842 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4196)->field4;
            
            return caseFn2379(uid225238929513829, clk226239029523830,
                              x227239129533831, l229239329553833,
                              r230239429563834, t23529583838, v23629593839,
                              n23929623842);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4195");
            exit(1);
        }
    }
}
GibCursor caseFn2396(GibInt uid225239729633843, GibCursor clk226239829643844,
                     GibInt x227239929653845, GibCursor l229240029663846,
                     GibCursor r230240129673847, GibCursor t235240229683848,
                     GibInt v236240329693849, GibCursor n239240429703850)
{
    GibCursor fltAppE31433851 =  clock177(t235240229683848);
    GibCursor fltAppE31443852 =  clock177(l229240029663846);
    GibCursor fltCse31423853 =  compare170(fltAppE31433851, fltAppE31443852);
    GibPackedTag tag4198 = *(GibPackedTag *) fltCse31423853;
    GibCursor tail4199 = fltCse31423853 + sizeof(GibInt);
    
    
  switch4207:
    ;
    switch (tag4198) {
        
      case 2:
        {
            GibCursor fltAppE31473854 =  clock177(clk226239829643844);
            GibCursor fltPkd31463855 =
                       stamp171(uid225239729633843, fltAppE31473854);
            GibCursor fltPkd31483856 =
                       place1161251(uid225239729633843, clk226239829643844, x227239929653845, n239240429703850, l229240029663846, r230240129673847);
            GibPtr fltPkd31453857 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31453857)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31453857)->field1 =
                fltPkd31463855;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31453857)->field2 =
                x227239929653845;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31453857)->field3 =
                l229240029663846;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31453857)->field4 =
                r230240129673847;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31453857)->field5 =
                fltPkd31483856;
            
            GibPtr tailift4200 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4200)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4200)->field1 =
                t235240229683848;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4200)->field2 =
                v236240329693849;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4200)->field3 =
                l229240029663846;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4200)->field4 =
                r230240129673847;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4200)->field5 =
                fltPkd31453857;
            return tailift4200;
            break;
        }
        
      case 0:
        {
            GibInt fltPrm31503858 =  author176(t235240229683848);
            GibBool fltIf31493859 = uid225239729633843 < fltPrm31503858;
            
            if (fltIf31493859) {
                GibCursor fltPkd31513860 =
                           place1161251(uid225239729633843, clk226239829643844, x227239929653845, n239240429703850, l229240029663846, r230240129673847);
                GibPtr tailift4201 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4201)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4201)->field1 =
                    t235240229683848;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4201)->field2 =
                    v236240329693849;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4201)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4201)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4201)->field5 =
                    fltPkd31513860;
                return tailift4201;
            } else {
                GibCursor fltAppE31543861 =  clock177(clk226239829643844);
                GibCursor fltPkd31533862 =
                           stamp171(uid225239729633843, fltAppE31543861);
                GibCursor fltPkd31553863 =
                           place1161251(uid225239729633843, clk226239829643844, x227239929653845, n239240429703850, l229240029663846, r230240129673847);
                GibPtr fltPkd31523864 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31523864)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31523864)->field1 =
                    fltPkd31533862;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31523864)->field2 =
                    x227239929653845;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31523864)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31523864)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31523864)->field5 =
                    fltPkd31553863;
                
                GibPtr tailift4202 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4202)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4202)->field1 =
                    t235240229683848;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4202)->field2 =
                    v236240329693849;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4202)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4202)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4202)->field5 =
                    fltPkd31523864;
                return tailift4202;
            }
            break;
        }
        
      case 1:
        {
            GibInt fltPrm31573865 =  author176(t235240229683848);
            GibBool fltIf31563866 = uid225239729633843 < fltPrm31573865;
            
            if (fltIf31563866) {
                GibCursor fltPkd31583867 =
                           place1161251(uid225239729633843, clk226239829643844, x227239929653845, n239240429703850, l229240029663846, r230240129673847);
                GibPtr tailift4203 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4203)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4203)->field1 =
                    t235240229683848;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4203)->field2 =
                    v236240329693849;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4203)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4203)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4203)->field5 =
                    fltPkd31583867;
                return tailift4203;
            } else {
                GibCursor fltAppE31613868 =  clock177(clk226239829643844);
                GibCursor fltPkd31603869 =
                           stamp171(uid225239729633843, fltAppE31613868);
                GibCursor fltPkd31623870 =
                           place1161251(uid225239729633843, clk226239829643844, x227239929653845, n239240429703850, l229240029663846, r230240129673847);
                GibPtr fltPkd31593871 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31593871)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31593871)->field1 =
                    fltPkd31603869;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31593871)->field2 =
                    x227239929653845;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31593871)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31593871)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31593871)->field5 =
                    fltPkd31623870;
                
                GibPtr tailift4204 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4204)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4204)->field1 =
                    t235240229683848;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4204)->field2 =
                    v236240329693849;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4204)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4204)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4204)->field5 =
                    fltPkd31593871;
                return tailift4204;
            }
            break;
        }
        
      case 3:
        {
            GibInt fltPrm31643872 =  author176(t235240229683848);
            GibBool fltIf31633873 = uid225239729633843 < fltPrm31643872;
            
            if (fltIf31633873) {
                GibCursor fltPkd31653874 =
                           place1161251(uid225239729633843, clk226239829643844, x227239929653845, n239240429703850, l229240029663846, r230240129673847);
                GibPtr tailift4205 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4205)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4205)->field1 =
                    t235240229683848;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4205)->field2 =
                    v236240329693849;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4205)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4205)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4205)->field5 =
                    fltPkd31653874;
                return tailift4205;
            } else {
                GibCursor fltAppE31683875 =  clock177(clk226239829643844);
                GibCursor fltPkd31673876 =
                           stamp171(uid225239729633843, fltAppE31683875);
                GibCursor fltPkd31693877 =
                           place1161251(uid225239729633843, clk226239829643844, x227239929653845, n239240429703850, l229240029663846, r230240129673847);
                GibPtr fltPkd31663878 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31663878)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31663878)->field1 =
                    fltPkd31673876;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31663878)->field2 =
                    x227239929653845;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31663878)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31663878)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltPkd31663878)->field5 =
                    fltPkd31693877;
                
                GibPtr tailift4206 =
                       gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
                
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4206)->field0 =
                    0;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4206)->field1 =
                    t235240229683848;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4206)->field2 =
                    v236240329693849;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4206)->field3 =
                    l229240029663846;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4206)->field4 =
                    r230240129673847;
                ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4206)->field5 =
                    fltPkd31663878;
                return tailift4206;
            }
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4198");
            exit(1);
        }
    }
}
GibCursor caseFn2405(GibInt uid225240629713879, GibCursor clk226240729723880,
                     GibInt x227240829733881, GibCursor s228240929743882,
                     GibCursor l229241029753883, GibCursor r230241129763884,
                     GibCursor t232241229773885)
{
    GibPackedTag tag4208 = *(GibPackedTag *) s228240929743882;
    GibCursor tail4209 = s228240929743882 + sizeof(GibInt);
    
    
  switch4210:
    ;
    switch (tag4208) {
        
      case 1:
        {
            GibPtr fltPkd31713886 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd31713886)->field0 = 1;
            
            GibPtr fltAppE31703887 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31703887)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31703887)->field1 =
                t232241229773885;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31703887)->field2 =
                x227240829733881;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31703887)->field3 =
                l229241029753883;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31703887)->field4 =
                r230241129763884;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31703887)->field5 =
                fltPkd31713886;
            return place_1171255(fltAppE31703887, s228240929743882);
            break;
        }
        
      case 0:
        {
            GibCursor t23529783888 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4209)->field0;
            GibInt v23629793889 =
                   ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4209)->field1;
            GibCursor ls23729803890 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4209)->field2;
            GibCursor rs23829813891 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4209)->field3;
            GibCursor n23929823892 =
                      ((GibCursorGibIntGibCursorGibCursorGibCursorProd *) tail4209)->field4;
            
            return caseFn2396(uid225240629713879, clk226240729723880,
                              x227240829733881, l229241029753883,
                              r230241129763884, t23529783888, v23629793889,
                              n23929823892);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4208");
            exit(1);
        }
    }
}
GibCursor caseFn2413(GibInt uid225241429833893, GibCursor clk226241529843894,
                     GibInt x227241629853895, GibCursor s228241729863896,
                     GibCursor l229241829873897, GibCursor r230241929883898,
                     GibCursor t232242029893899)
{
    GibCursor fltAppE31733900 =  clock177(t232242029893899);
    GibCursor fltAppE31743901 =  clock177(r230241929883898);
    GibCursor fltCse31723902 =  compare170(fltAppE31733900, fltAppE31743901);
    GibPackedTag tag4211 = *(GibPackedTag *) fltCse31723902;
    GibCursor tail4212 = fltCse31723902 + sizeof(GibInt);
    
    
  switch4213:
    ;
    switch (tag4211) {
        
      case 2:
        {
            GibPtr fltPkd31763903 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) fltPkd31763903)->field0 = 1;
            
            GibPtr fltAppE31753904 =
                   gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
            
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31753904)->field0 =
                0;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31753904)->field1 =
                t232242029893899;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31753904)->field2 =
                x227241629853895;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31753904)->field3 =
                l229241829873897;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31753904)->field4 =
                r230241929883898;
            ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) fltAppE31753904)->field5 =
                fltPkd31763903;
            return place_1171255(fltAppE31753904, s228241729863896);
            break;
        }
        
      case 0:
        {
            return caseFn2371(uid225241429833893, clk226241529843894,
                              x227241629853895, s228241729863896,
                              l229241829873897, r230241929883898,
                              t232242029893899);
            break;
        }
        
      case 1:
        {
            return caseFn2388(uid225241429833893, clk226241529843894,
                              x227241629853895, s228241729863896,
                              l229241829873897, r230241929883898,
                              t232242029893899);
            break;
        }
        
      case 3:
        {
            return caseFn2405(uid225241429833893, clk226241529843894,
                              x227241629853895, s228241729863896,
                              l229241829873897, r230241929883898,
                              t232242029893899);
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag4211");
            exit(1);
        }
    }
}
GibCursor caseFn2421(GibInt uid225242229903905, GibCursor clk226242329913906,
                     GibInt x227242429923907, GibCursor s228242529933908,
                     GibCursor l229242629943909, GibCursor r230242729953910,
                     GibCursor t232242829963911)
{
    GibInt tag4214 = ((GibIntGibIntGibCursorProd *) t232242829963911)->field0;
    GibInt suid23329973912 =
           ((GibIntGibIntGibCursorProd *) t232242829963911)->field1;
    GibCursor sclk23429983913 =
              ((GibIntGibIntGibCursorProd *) t232242829963911)->field2;
    GibBool fltIf31773914 = suid23329973912 < 0;
    
    if (fltIf31773914) {
        GibPtr fltPkd31783915 = gib_alloc(sizeof(GibIntProd));
        
        ((GibIntProd *) fltPkd31783915)->field0 = 1;
        
        GibPtr tailift4215 =
               gib_alloc(sizeof(GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd));
        
        ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4215)->field0 =
            0;
        ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4215)->field1 =
            t232242829963911;
        ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4215)->field2 =
            x227242429923907;
        ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4215)->field3 =
            l229242629943909;
        ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4215)->field4 =
            r230242729953910;
        ((GibIntGibCursorGibIntGibCursorGibCursorGibCursorProd *) tailift4215)->field5 =
            fltPkd31783915;
        return tailift4215;
    } else {
        return caseFn2413(uid225242229903905, clk226242329913906,
                          x227242429923907, s228242529933908, l229242629943909,
                          r230242729953910, t232242829963911);
    }
}
int main(int argc, char **argv)
{
    int init0 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibCursor x18224293179 =  singleton1191237(0, 0);
    GibCursor fltAppE29993180 =  cursor1141238(0, x18224293179);
    GibCursor fltAppE30003181 =  cursor1141238(1, x18224293179);
    GibCursor x18324303182 =
               insert1181239(0, 1, fltAppE29993180, fltAppE30003181, x18224293179);
    GibCursor tmp_app3916 =  value1121240(x18324303182);
    
     _print_Node166_v1253(tmp_app3916);
    printf("\n");
    
    int exit1 = gib_exit();
    
    return exit1;
}