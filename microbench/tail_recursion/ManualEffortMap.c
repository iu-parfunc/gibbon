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
typedef struct GibCursorGibBoolProd_struct {
            GibCursor field0;
            GibBool field1;
        } GibCursorGibBoolProd;
typedef struct GibCursorGibCursorProd_struct {
            GibCursor field0;
            GibCursor field1;
        } GibCursorGibCursorProd;
typedef struct GibCursorGibCursorGibIntProd_struct {
            GibCursor field0;
            GibCursor field1;
            GibInt field2;
        } GibCursorGibCursorGibIntProd;
typedef struct GibCursorGibCursorGibBoolProd_struct {
            GibCursor field0;
            GibCursor field1;
            GibBool field2;
        } GibCursorGibCursorGibBoolProd;
typedef struct GibCursorGibCursorGibCursorProd_struct {
            GibCursor field0;
            GibCursor field1;
            GibCursor field2;
        } GibCursorGibCursorGibCursorProd;
typedef struct GibCursorGibCursorGibCursorGibCursorGibCursorProd_struct {
            GibCursor field0;
            GibCursor field1;
            GibCursor field2;
            GibCursor field3;
            GibCursor field4;
        } GibCursorGibCursorGibCursorGibCursorGibCursorProd;
GibCursorGibCursorGibCursorProd mkConsIntList(GibCursor end_r_335,
                                              GibCursor loc_334,
                                              GibInt len_25_104_158);
GibCursorGibCursorGibBoolProd checkMapAdd1(GibCursor end_r_338,
                                           GibCursor end_r_339,
                                           GibCursor lst_27_106_162,
                                           GibCursor lst__28_107_163);
GibInt add1(GibInt x_33_110_166);
GibCursorGibCursorGibCursorGibCursorGibCursorProd map_61(GibCursor end_r_342,
                                                         GibCursor end_r_343,
                                                         GibCursor *loc_341,
                                                         GibCursor lst_35_111_167);
GibCursorGibCursorGibCursorGibCursorGibCursorProd
_copy_ConsIntList(GibCursor end_r_346, GibCursor end_r_347, GibCursor loc_345,
                  GibCursor arg_69_114_172);
GibCursorGibCursorGibCursorGibCursorGibCursorProd
_copy_without_ptrs_ConsIntList(GibCursor end_r_350, GibCursor end_r_351,
                               GibCursor loc_349, GibCursor arg_74_119_177);
GibCursorGibCursorProd _traverse_ConsIntList(GibCursor end_r_353,
                                             GibCursor arg_79_124_182);
GibCursorGibCursorProd _print_ConsIntList(GibCursor end_r_355,
                                          GibCursor arg_84_128_186);
GibCursorGibBoolProd caseFn_95(GibCursor end_r_357,
                               GibCursor lst__28_96_139_197);
GibCursorGibCursorGibBoolProd caseFn_97(GibCursor end_r_360,
                                        GibCursor end_r_361,
                                        GibCursor lst__28_98_142_200,
                                        GibInt x_29_99_143_201,
                                        GibCursor rst_30_100_144_202);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            ConsIntList_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(8);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[2];
    
    field_tys[0] = ConsIntList_T;
    error = gib_info_table_insert_packed_dcon(ConsIntList_T, 0, 8, 0, 1, 1,
                                              field_tys, 1);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, ConsIntList_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(ConsIntList_T, 1, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, ConsIntList_T, 1);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(967, ")");
    gib_add_symbol(968, "(Nil");
    gib_add_symbol(969, "(Cons");
    gib_add_symbol(970, " ->r ");
    gib_add_symbol(971, " ->i ");
    gib_add_symbol(972, " ");
}
GibCursorGibCursorGibCursorProd mkConsIntList(GibCursor end_r_335,
                                              GibCursor loc_334,
                                              GibInt len_25_104_158)
{
    if (loc_334 + 18 > end_r_335) {
        gib_grow_region(&loc_334, &end_r_335);
    }
    
    GibBool fltIf_147_159 = len_25_104_158 <= 0;
    
    if (fltIf_147_159) {
        *(GibPackedTag *) loc_334 = 1;
        
        GibCursor writetag_649 = loc_334 + 1;
        GibCursor after_tag_650 = loc_334 + 1;
        
        return (GibCursorGibCursorGibCursorProd) {end_r_335, loc_334,
                                                  after_tag_650};
    } else {
        GibInt fltAppE_148_160 = len_25_104_158 - 1;
        GibCursor loc_379 = loc_334 + 9;
        
        *(GibPackedTag *) loc_334 = 0;
        
        GibCursor writetag_658 = loc_334 + 1;
        GibCursor after_tag_659 = loc_334 + 1;
        
        *(GibInt *) after_tag_659 = len_25_104_158;
        
        GibCursor writecur_663 = after_tag_659 + sizeof(GibInt);
        GibCursorGibCursorGibCursorProd tmp_struct_0 =
                                         mkConsIntList(end_r_335, loc_379, fltAppE_148_160);
        GibCursor pvrtmp_1010 = tmp_struct_0.field0;
        GibCursor pvrtmp_1011 = tmp_struct_0.field1;
        GibCursor pvrtmp_1012 = tmp_struct_0.field2;
        
        return (GibCursorGibCursorGibCursorProd) {pvrtmp_1010, loc_334,
                                                  pvrtmp_1012};
    }
}
GibCursorGibCursorGibBoolProd checkMapAdd1(GibCursor end_r_338,
                                           GibCursor end_r_339,
                                           GibCursor lst_27_106_162,
                                           GibCursor lst__28_107_163)
{
    GibPackedTag tmpval_1021 = *(GibPackedTag *) lst_27_106_162;
    GibCursor tmpcur_1022 = lst_27_106_162 + 1;
    
    
  switch_1042:
    ;
    switch (tmpval_1021) {
        
      case 1:
        {
            GibCursorGibBoolProd tmp_struct_4 =
                                  caseFn_95(end_r_339, lst__28_107_163);
            GibCursor pvrtmp_1023 = tmp_struct_4.field0;
            GibBool pvrtmp_1024 = tmp_struct_4.field1;
            
            return (GibCursorGibCursorGibBoolProd) {end_r_338, pvrtmp_1023,
                                                    pvrtmp_1024};
            break;
        }
        
      case 0:
        {
            GibInt tmpval_1025 = *(GibInt *) tmpcur_1022;
            GibCursor tmpcur_1026 = tmpcur_1022 + sizeof(GibInt);
            GibCursorGibCursorGibBoolProd tmp_struct_5 =
                                           caseFn_97(end_r_339, end_r_338, lst__28_107_163, tmpval_1025, tmpcur_1026);
            GibCursor pvrtmp_1027 = tmp_struct_5.field0;
            GibCursor pvrtmp_1028 = tmp_struct_5.field1;
            GibBool pvrtmp_1029 = tmp_struct_5.field2;
            
            return (GibCursorGibCursorGibBoolProd) {pvrtmp_1028, pvrtmp_1027,
                                                    pvrtmp_1029};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_7 = *(uintptr_t *) tmpcur_1022;
            GibCursor tmpcur_1030 = GIB_UNTAG(tagged_tmpcur_7);
            GibCursor tmpaftercur_1031 = tmpcur_1022 + 8;
            uint16_t tmptag_1032 = GIB_GET_TAG(tagged_tmpcur_7);
            GibCursor end_from_tagged_indr_510 = tmpcur_1030 + tmptag_1032;
            GibCursor jump_512 = tmpcur_1022 + 8;
            GibCursorGibCursorGibBoolProd tmp_struct_6 =
                                           checkMapAdd1(end_from_tagged_indr_510, end_r_339, tmpcur_1030, lst__28_107_163);
            GibCursor pvrtmp_1033 = tmp_struct_6.field0;
            GibCursor pvrtmp_1034 = tmp_struct_6.field1;
            GibBool pvrtmp_1035 = tmp_struct_6.field2;
            
            return (GibCursorGibCursorGibBoolProd) {end_r_338, pvrtmp_1034,
                                                    pvrtmp_1035};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_9 = *(uintptr_t *) tmpcur_1022;
            GibCursor tmpcur_1036 = GIB_UNTAG(tagged_tmpcur_9);
            GibCursor tmpaftercur_1037 = tmpcur_1022 + 8;
            uint16_t tmptag_1038 = GIB_GET_TAG(tagged_tmpcur_9);
            GibCursor end_from_tagged_indr_510 = tmpcur_1036 + tmptag_1038;
            GibCursorGibCursorGibBoolProd tmp_struct_8 =
                                           checkMapAdd1(end_from_tagged_indr_510, end_r_339, tmpcur_1036, lst__28_107_163);
            GibCursor pvrtmp_1039 = tmp_struct_8.field0;
            GibCursor pvrtmp_1040 = tmp_struct_8.field1;
            GibBool pvrtmp_1041 = tmp_struct_8.field2;
            
            return (GibCursorGibCursorGibBoolProd) {pvrtmp_1039, pvrtmp_1040,
                                                    pvrtmp_1041};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1021");
            exit(1);
        }
    }
}
GibInt add1(GibInt x_33_110_166)
{
    GibInt tailprim_471 = x_33_110_166 + 1;
    
    return tailprim_471;
}
GibCursorGibCursorGibCursorGibCursorGibCursorProd map_61(GibCursor end_r_342,
                                                         GibCursor end_r_343,
                                                         GibCursor *loc_341,
                                                         GibCursor lst_35_111_167)
{
    if (*loc_341 + 18 > end_r_343) {
        gib_grow_region(&(*loc_341), &end_r_343);
    }
    
    GibPackedTag tmpval_1043 = *(GibPackedTag *) lst_35_111_167;
    GibCursor tmpcur_1044 = lst_35_111_167 + 1;
    
    
  switch_1092:
    ;
    switch (tmpval_1043) {
        
      case 1:
        {
            GibCursor jump_472 = lst_35_111_167 + 1;
            
            *(GibPackedTag *) (*loc_341) = 1;
            
            GibCursor writetag_686 = *loc_341 + 1;
            GibCursor after_tag_687 = *loc_341 + 1;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_342,
                                                                        end_r_343,
                                                                        jump_472,
                                                                        *loc_341,
                                                                        after_tag_687};
            break;
        }
        
      case 0:
        {
            GibInt tmpval_1049 = *(GibInt *) tmpcur_1044;
            GibCursor tmpcur_1050 = tmpcur_1044 + sizeof(GibInt);
            GibInt fltPkd_149_170 =  add1(tmpval_1049);
            GibCursor loc_402 = *loc_341 + 9;
            
            *(GibPackedTag *) (*loc_341) = 0;
            
            GibCursor writetag_698 = *loc_341 + 1;
            GibCursor after_tag_699 = *loc_341 + 1;
            
            *(GibInt *) after_tag_699 = fltPkd_149_170;
            
            GibCursor writecur_703 = after_tag_699 + sizeof(GibInt);
            return map_61(end_r_342, end_r_343, &loc_402, tmpcur_1050);
            // GibCursor pvrtmp_1051 = tmp_struct_10.field0;
            // GibCursor pvrtmp_1052 = tmp_struct_10.field1;
            // GibCursor pvrtmp_1053 = tmp_struct_10.field2;
            // GibCursor pvrtmp_1054 = tmp_struct_10.field3;
            // GibCursor pvrtmp_1055 = tmp_struct_10.field4;
            
            // return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_1051,
            //                                                             pvrtmp_1052,
            //                                                             pvrtmp_1053,
            //                                                             loc_341,
            //                                                             pvrtmp_1055};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_12 = *(uintptr_t *) tmpcur_1044;
            GibCursor tmpcur_1064 = GIB_UNTAG(tagged_tmpcur_12);
            GibCursor tmpaftercur_1065 = tmpcur_1044 + 8;
            uint16_t tmptag_1066 = GIB_GET_TAG(tagged_tmpcur_12);
            GibCursor end_from_tagged_indr_515 = tmpcur_1064 + tmptag_1066;
            GibCursor jump_517 = tmpcur_1044 + 8;
            return map_61(end_from_tagged_indr_515, end_r_343, loc_341, tmpcur_1064);
            // GibCursor pvrtmp_1067 = tmp_struct_11.field0;
            // GibCursor pvrtmp_1068 = tmp_struct_11.field1;
            // GibCursor pvrtmp_1069 = tmp_struct_11.field2;
            // GibCursor pvrtmp_1070 = tmp_struct_11.field3;
            // GibCursor pvrtmp_1071 = tmp_struct_11.field4;
            
            // return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_342,
            //                                                             pvrtmp_1068,
            //                                                             jump_517,
            //                                                             pvrtmp_1070,
            //                                                             pvrtmp_1071};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_14 = *(uintptr_t *) tmpcur_1044;
            GibCursor tmpcur_1078 = GIB_UNTAG(tagged_tmpcur_14);
            GibCursor tmpaftercur_1079 = tmpcur_1044 + 8;
            uint16_t tmptag_1080 = GIB_GET_TAG(tagged_tmpcur_14);
            GibCursor end_from_tagged_indr_515 = tmpcur_1078 + tmptag_1080;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_13 =
                                                               map_61(end_from_tagged_indr_515, end_r_343, loc_341, tmpcur_1078);
            // GibCursor pvrtmp_1081 = tmp_struct_13.field0;
            // GibCursor pvrtmp_1082 = tmp_struct_13.field1;
            // GibCursor pvrtmp_1083 = tmp_struct_13.field2;
            // GibCursor pvrtmp_1084 = tmp_struct_13.field3;
            // GibCursor pvrtmp_1085 = tmp_struct_13.field4;
            
            // return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_1081,
            //                                                             pvrtmp_1082,
            //                                                             pvrtmp_1083,
            //                                                             pvrtmp_1084,
            //                                                             pvrtmp_1085};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1043");
            exit(1);
        }
    }
}
GibCursorGibCursorGibCursorGibCursorGibCursorProd _copy_ConsIntList(GibCursor end_r_346,
                                                                    GibCursor end_r_347,
                                                                    GibCursor loc_345,
                                                                    GibCursor arg_69_114_172)
{
    if (loc_345 + 18 > end_r_347) {
        gib_grow_region(&loc_345, &end_r_347);
    }
    
    GibPackedTag tmpval_1093 = *(GibPackedTag *) arg_69_114_172;
    GibCursor tmpcur_1094 = arg_69_114_172 + 1;
    
    
  switch_1142:
    ;
    switch (tmpval_1093) {
        
      case 0:
        {
            GibInt tmpval_1095 = *(GibInt *) tmpcur_1094;
            GibCursor tmpcur_1096 = tmpcur_1094 + sizeof(GibInt);
            GibCursor loc_414 = loc_345 + 9;
            
            *(GibPackedTag *) loc_345 = 0;
            
            GibCursor writetag_722 = loc_345 + 1;
            GibCursor after_tag_723 = loc_345 + 1;
            
            *(GibInt *) after_tag_723 = tmpval_1095;
            
            GibCursor writecur_727 = after_tag_723 + sizeof(GibInt);
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_18 =
                                                               _copy_ConsIntList(end_r_346, end_r_347, loc_414, tmpcur_1096);
            GibCursor pvrtmp_1097 = tmp_struct_18.field0;
            GibCursor pvrtmp_1098 = tmp_struct_18.field1;
            GibCursor pvrtmp_1099 = tmp_struct_18.field2;
            GibCursor pvrtmp_1100 = tmp_struct_18.field3;
            GibCursor pvrtmp_1101 = tmp_struct_18.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_1097,
                                                                        pvrtmp_1098,
                                                                        pvrtmp_1099,
                                                                        loc_345,
                                                                        pvrtmp_1101};
            break;
        }
        
      case 1:
        {
            GibCursor jump_480 = arg_69_114_172 + 1;
            
            *(GibPackedTag *) loc_345 = 1;
            
            GibCursor writetag_732 = loc_345 + 1;
            GibCursor after_tag_733 = loc_345 + 1;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_346,
                                                                        end_r_347,
                                                                        jump_480,
                                                                        loc_345,
                                                                        after_tag_733};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_20 = *(uintptr_t *) tmpcur_1094;
            GibCursor tmpcur_1114 = GIB_UNTAG(tagged_tmpcur_20);
            GibCursor tmpaftercur_1115 = tmpcur_1094 + 8;
            uint16_t tmptag_1116 = GIB_GET_TAG(tagged_tmpcur_20);
            GibCursor end_from_tagged_indr_521 = tmpcur_1114 + tmptag_1116;
            GibCursor jump_523 = tmpcur_1094 + 8;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_19 =
                                                               _copy_ConsIntList(end_from_tagged_indr_521, end_r_347, loc_345, tmpcur_1114);
            GibCursor pvrtmp_1117 = tmp_struct_19.field0;
            GibCursor pvrtmp_1118 = tmp_struct_19.field1;
            GibCursor pvrtmp_1119 = tmp_struct_19.field2;
            GibCursor pvrtmp_1120 = tmp_struct_19.field3;
            GibCursor pvrtmp_1121 = tmp_struct_19.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_346,
                                                                        pvrtmp_1118,
                                                                        jump_523,
                                                                        pvrtmp_1120,
                                                                        pvrtmp_1121};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_22 = *(uintptr_t *) tmpcur_1094;
            GibCursor tmpcur_1128 = GIB_UNTAG(tagged_tmpcur_22);
            GibCursor tmpaftercur_1129 = tmpcur_1094 + 8;
            uint16_t tmptag_1130 = GIB_GET_TAG(tagged_tmpcur_22);
            GibCursor end_from_tagged_indr_521 = tmpcur_1128 + tmptag_1130;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_21 =
                                                               _copy_ConsIntList(end_from_tagged_indr_521, end_r_347, loc_345, tmpcur_1128);
            GibCursor pvrtmp_1131 = tmp_struct_21.field0;
            GibCursor pvrtmp_1132 = tmp_struct_21.field1;
            GibCursor pvrtmp_1133 = tmp_struct_21.field2;
            GibCursor pvrtmp_1134 = tmp_struct_21.field3;
            GibCursor pvrtmp_1135 = tmp_struct_21.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_1131,
                                                                        pvrtmp_1132,
                                                                        pvrtmp_1133,
                                                                        pvrtmp_1134,
                                                                        pvrtmp_1135};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1093");
            exit(1);
        }
    }
}
GibCursorGibCursorGibCursorGibCursorGibCursorProd _copy_without_ptrs_ConsIntList(GibCursor end_r_350,
                                                                                 GibCursor end_r_351,
                                                                                 GibCursor loc_349,
                                                                                 GibCursor arg_74_119_177)
{
    GibPackedTag tmpval_1143 = *(GibPackedTag *) arg_74_119_177;
    GibCursor tmpcur_1144 = arg_74_119_177 + 1;
    
    
  switch_1192:
    ;
    switch (tmpval_1143) {
        
      case 0:
        {
            GibInt tmpval_1145 = *(GibInt *) tmpcur_1144;
            GibCursor tmpcur_1146 = tmpcur_1144 + sizeof(GibInt);
            GibCursor loc_427 = loc_349 + 9;
            
            *(GibPackedTag *) loc_349 = 0;
            
            GibCursor writetag_754 = loc_349 + 1;
            GibCursor after_tag_755 = loc_349 + 1;
            
            *(GibInt *) after_tag_755 = tmpval_1145;
            
            GibCursor writecur_759 = after_tag_755 + sizeof(GibInt);
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_26 =
                                                               _copy_without_ptrs_ConsIntList(end_r_350, end_r_351, loc_427, tmpcur_1146);
            GibCursor pvrtmp_1147 = tmp_struct_26.field0;
            GibCursor pvrtmp_1148 = tmp_struct_26.field1;
            GibCursor pvrtmp_1149 = tmp_struct_26.field2;
            GibCursor pvrtmp_1150 = tmp_struct_26.field3;
            GibCursor pvrtmp_1151 = tmp_struct_26.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_1147,
                                                                        pvrtmp_1148,
                                                                        pvrtmp_1149,
                                                                        loc_349,
                                                                        pvrtmp_1151};
            break;
        }
        
      case 1:
        {
            GibCursor jump_485 = arg_74_119_177 + 1;
            
            *(GibPackedTag *) loc_349 = 1;
            
            GibCursor writetag_764 = loc_349 + 1;
            GibCursor after_tag_765 = loc_349 + 1;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_350,
                                                                        end_r_351,
                                                                        jump_485,
                                                                        loc_349,
                                                                        after_tag_765};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_28 = *(uintptr_t *) tmpcur_1144;
            GibCursor tmpcur_1164 = GIB_UNTAG(tagged_tmpcur_28);
            GibCursor tmpaftercur_1165 = tmpcur_1144 + 8;
            uint16_t tmptag_1166 = GIB_GET_TAG(tagged_tmpcur_28);
            GibCursor end_from_tagged_indr_527 = tmpcur_1164 + tmptag_1166;
            GibCursor jump_529 = tmpcur_1144 + 8;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_27 =
                                                               _copy_without_ptrs_ConsIntList(end_from_tagged_indr_527, end_r_351, loc_349, tmpcur_1164);
            GibCursor pvrtmp_1167 = tmp_struct_27.field0;
            GibCursor pvrtmp_1168 = tmp_struct_27.field1;
            GibCursor pvrtmp_1169 = tmp_struct_27.field2;
            GibCursor pvrtmp_1170 = tmp_struct_27.field3;
            GibCursor pvrtmp_1171 = tmp_struct_27.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_350,
                                                                        pvrtmp_1168,
                                                                        jump_529,
                                                                        pvrtmp_1170,
                                                                        pvrtmp_1171};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_30 = *(uintptr_t *) tmpcur_1144;
            GibCursor tmpcur_1178 = GIB_UNTAG(tagged_tmpcur_30);
            GibCursor tmpaftercur_1179 = tmpcur_1144 + 8;
            uint16_t tmptag_1180 = GIB_GET_TAG(tagged_tmpcur_30);
            GibCursor end_from_tagged_indr_527 = tmpcur_1178 + tmptag_1180;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_29 =
                                                               _copy_without_ptrs_ConsIntList(end_from_tagged_indr_527, end_r_351, loc_349, tmpcur_1178);
            GibCursor pvrtmp_1181 = tmp_struct_29.field0;
            GibCursor pvrtmp_1182 = tmp_struct_29.field1;
            GibCursor pvrtmp_1183 = tmp_struct_29.field2;
            GibCursor pvrtmp_1184 = tmp_struct_29.field3;
            GibCursor pvrtmp_1185 = tmp_struct_29.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_1181,
                                                                        pvrtmp_1182,
                                                                        pvrtmp_1183,
                                                                        pvrtmp_1184,
                                                                        pvrtmp_1185};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1143");
            exit(1);
        }
    }
}
GibCursorGibCursorProd _traverse_ConsIntList(GibCursor end_r_353,
                                             GibCursor arg_79_124_182)
{
    GibPackedTag tmpval_1193 = *(GibPackedTag *) arg_79_124_182;
    GibCursor tmpcur_1194 = arg_79_124_182 + 1;
    
    
  switch_1209:
    ;
    switch (tmpval_1193) {
        
      case 0:
        {
            GibInt tmpval_1195 = *(GibInt *) tmpcur_1194;
            GibCursor tmpcur_1196 = tmpcur_1194 + sizeof(GibInt);
            GibCursorGibCursorProd tmp_struct_31 =
                                    _traverse_ConsIntList(end_r_353, tmpcur_1196);
            GibCursor pvrtmp_1197 = tmp_struct_31.field0;
            GibCursor pvrtmp_1198 = tmp_struct_31.field1;
            
            return (GibCursorGibCursorProd) {pvrtmp_1197, pvrtmp_1198};
            break;
        }
        
      case 1:
        {
            GibCursor jump_490 = arg_79_124_182 + 1;
            
            return (GibCursorGibCursorProd) {end_r_353, jump_490};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_33 = *(uintptr_t *) tmpcur_1194;
            GibCursor tmpcur_1199 = GIB_UNTAG(tagged_tmpcur_33);
            GibCursor tmpaftercur_1200 = tmpcur_1194 + 8;
            uint16_t tmptag_1201 = GIB_GET_TAG(tagged_tmpcur_33);
            GibCursor end_from_tagged_indr_533 = tmpcur_1199 + tmptag_1201;
            GibCursor jump_535 = tmpcur_1194 + 8;
            GibCursorGibCursorProd tmp_struct_32 =
                                    _traverse_ConsIntList(end_from_tagged_indr_533, tmpcur_1199);
            GibCursor pvrtmp_1202 = tmp_struct_32.field0;
            GibCursor pvrtmp_1203 = tmp_struct_32.field1;
            
            return (GibCursorGibCursorProd) {end_r_353, jump_535};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_35 = *(uintptr_t *) tmpcur_1194;
            GibCursor tmpcur_1204 = GIB_UNTAG(tagged_tmpcur_35);
            GibCursor tmpaftercur_1205 = tmpcur_1194 + 8;
            uint16_t tmptag_1206 = GIB_GET_TAG(tagged_tmpcur_35);
            GibCursor end_from_tagged_indr_533 = tmpcur_1204 + tmptag_1206;
            GibCursorGibCursorProd tmp_struct_34 =
                                    _traverse_ConsIntList(end_from_tagged_indr_533, tmpcur_1204);
            GibCursor pvrtmp_1207 = tmp_struct_34.field0;
            GibCursor pvrtmp_1208 = tmp_struct_34.field1;
            
            return (GibCursorGibCursorProd) {pvrtmp_1207, pvrtmp_1208};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1193");
            exit(1);
        }
    }
}
GibCursorGibCursorProd _print_ConsIntList(GibCursor end_r_355,
                                          GibCursor arg_84_128_186)
{
    GibPackedTag tmpval_1210 = *(GibPackedTag *) arg_84_128_186;
    GibCursor tmpcur_1211 = arg_84_128_186 + 1;
    
    
  switch_1226:
    ;
    switch (tmpval_1210) {
        
      case 0:
        {
            GibInt tmpval_1212 = *(GibInt *) tmpcur_1211;
            GibCursor tmpcur_1213 = tmpcur_1211 + sizeof(GibInt);
            unsigned char wildcard_89_131_189 = gib_print_symbol(969);
            unsigned char wildcard_92_132_190 = gib_print_symbol(972);
            unsigned char y_87_133_191 = printf("%ld", tmpval_1212);
            unsigned char wildcard_91_134_192 = gib_print_symbol(972);
            GibCursorGibCursorProd tmp_struct_36 =
                                    _print_ConsIntList(end_r_355, tmpcur_1213);
            GibCursor pvrtmp_1214 = tmp_struct_36.field0;
            GibCursor pvrtmp_1215 = tmp_struct_36.field1;
            unsigned char wildcard_90_136_194 = gib_print_symbol(967);
            
            return (GibCursorGibCursorProd) {pvrtmp_1214, pvrtmp_1215};
            break;
        }
        
      case 1:
        {
            GibCursor jump_495 = arg_84_128_186 + 1;
            unsigned char wildcard_93_137_195 = gib_print_symbol(968);
            unsigned char wildcard_94_138_196 = gib_print_symbol(967);
            
            return (GibCursorGibCursorProd) {end_r_355, jump_495};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_38 = *(uintptr_t *) tmpcur_1211;
            GibCursor tmpcur_1216 = GIB_UNTAG(tagged_tmpcur_38);
            GibCursor tmpaftercur_1217 = tmpcur_1211 + 8;
            uint16_t tmptag_1218 = GIB_GET_TAG(tagged_tmpcur_38);
            GibCursor end_from_tagged_indr_539 = tmpcur_1216 + tmptag_1218;
            GibCursor jump_541 = tmpcur_1211 + 8;
            unsigned char wildcard_544 = gib_print_symbol(971);
            GibCursorGibCursorProd tmp_struct_37 =
                                    _print_ConsIntList(end_from_tagged_indr_539, tmpcur_1216);
            GibCursor pvrtmp_1219 = tmp_struct_37.field0;
            GibCursor pvrtmp_1220 = tmp_struct_37.field1;
            
            return (GibCursorGibCursorProd) {end_r_355, jump_541};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_40 = *(uintptr_t *) tmpcur_1211;
            GibCursor tmpcur_1221 = GIB_UNTAG(tagged_tmpcur_40);
            GibCursor tmpaftercur_1222 = tmpcur_1211 + 8;
            uint16_t tmptag_1223 = GIB_GET_TAG(tagged_tmpcur_40);
            GibCursor end_from_tagged_indr_539 = tmpcur_1221 + tmptag_1223;
            unsigned char wildcard_544 = gib_print_symbol(970);
            GibCursorGibCursorProd tmp_struct_39 =
                                    _print_ConsIntList(end_from_tagged_indr_539, tmpcur_1221);
            GibCursor pvrtmp_1224 = tmp_struct_39.field0;
            GibCursor pvrtmp_1225 = tmp_struct_39.field1;
            
            return (GibCursorGibCursorProd) {pvrtmp_1224, pvrtmp_1225};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1210");
            exit(1);
        }
    }
}
GibCursorGibBoolProd caseFn_95(GibCursor end_r_357,
                               GibCursor lst__28_96_139_197)
{
    GibPackedTag tmpval_1227 = *(GibPackedTag *) lst__28_96_139_197;
    GibCursor tmpcur_1228 = lst__28_96_139_197 + 1;
    
    
  switch_1241:
    ;
    switch (tmpval_1227) {
        
      case 1:
        {
            return (GibCursorGibBoolProd) {end_r_357, true};
            break;
        }
        
      case 0:
        {
            GibInt tmpval_1229 = *(GibInt *) tmpcur_1228;
            GibCursor tmpcur_1230 = tmpcur_1228 + sizeof(GibInt);
            
            return (GibCursorGibBoolProd) {end_r_357, false};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_42 = *(uintptr_t *) tmpcur_1228;
            GibCursor tmpcur_1231 = GIB_UNTAG(tagged_tmpcur_42);
            GibCursor tmpaftercur_1232 = tmpcur_1228 + 8;
            uint16_t tmptag_1233 = GIB_GET_TAG(tagged_tmpcur_42);
            GibCursor end_from_tagged_indr_545 = tmpcur_1231 + tmptag_1233;
            GibCursor jump_547 = tmpcur_1228 + 8;
            GibCursorGibBoolProd tmp_struct_41 =
                                  caseFn_95(end_from_tagged_indr_545, tmpcur_1231);
            GibCursor pvrtmp_1234 = tmp_struct_41.field0;
            GibBool pvrtmp_1235 = tmp_struct_41.field1;
            
            return (GibCursorGibBoolProd) {end_r_357, pvrtmp_1235};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_44 = *(uintptr_t *) tmpcur_1228;
            GibCursor tmpcur_1236 = GIB_UNTAG(tagged_tmpcur_44);
            GibCursor tmpaftercur_1237 = tmpcur_1228 + 8;
            uint16_t tmptag_1238 = GIB_GET_TAG(tagged_tmpcur_44);
            GibCursor end_from_tagged_indr_545 = tmpcur_1236 + tmptag_1238;
            GibCursorGibBoolProd tmp_struct_43 =
                                  caseFn_95(end_from_tagged_indr_545, tmpcur_1236);
            GibCursor pvrtmp_1239 = tmp_struct_43.field0;
            GibBool pvrtmp_1240 = tmp_struct_43.field1;
            
            return (GibCursorGibBoolProd) {pvrtmp_1239, pvrtmp_1240};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1227");
            exit(1);
        }
    }
}
GibCursorGibCursorGibBoolProd caseFn_97(GibCursor end_r_360,
                                        GibCursor end_r_361,
                                        GibCursor lst__28_98_142_200,
                                        GibInt x_29_99_143_201,
                                        GibCursor rst_30_100_144_202)
{
    GibPackedTag tmpval_1242 = *(GibPackedTag *) lst__28_98_142_200;
    GibCursor tmpcur_1243 = lst__28_98_142_200 + 1;
    
    
  switch_1261:
    ;
    switch (tmpval_1242) {
        
      case 1:
        {
            return (GibCursorGibCursorGibBoolProd) {end_r_360, end_r_361,
                                                    false};
            break;
        }
        
      case 0:
        {
            GibInt tmpval_1244 = *(GibInt *) tmpcur_1243;
            GibCursor tmpcur_1245 = tmpcur_1243 + sizeof(GibInt);
            GibInt fltPrm_152_205 = x_29_99_143_201 + 1;
            GibBool fltIf_151_206 = tmpval_1244 == fltPrm_152_205;
            
            if (fltIf_151_206) {
                GibCursorGibCursorGibBoolProd tmp_struct_45 =
                                               checkMapAdd1(end_r_361, end_r_360, rst_30_100_144_202, tmpcur_1245);
                GibCursor pvrtmp_1246 = tmp_struct_45.field0;
                GibCursor pvrtmp_1247 = tmp_struct_45.field1;
                GibBool pvrtmp_1248 = tmp_struct_45.field2;
                GibBool tailprim_505 = true && pvrtmp_1248;
                
                return (GibCursorGibCursorGibBoolProd) {pvrtmp_1247,
                                                        pvrtmp_1246,
                                                        tailprim_505};
            } else {
                return (GibCursorGibCursorGibBoolProd) {end_r_360, end_r_361,
                                                        false};
            }
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_47 = *(uintptr_t *) tmpcur_1243;
            GibCursor tmpcur_1249 = GIB_UNTAG(tagged_tmpcur_47);
            GibCursor tmpaftercur_1250 = tmpcur_1243 + 8;
            uint16_t tmptag_1251 = GIB_GET_TAG(tagged_tmpcur_47);
            GibCursor end_from_tagged_indr_550 = tmpcur_1249 + tmptag_1251;
            GibCursor jump_552 = tmpcur_1243 + 8;
            GibCursorGibCursorGibBoolProd tmp_struct_46 =
                                           caseFn_97(end_from_tagged_indr_550, end_r_361, tmpcur_1249, x_29_99_143_201, rst_30_100_144_202);
            GibCursor pvrtmp_1252 = tmp_struct_46.field0;
            GibCursor pvrtmp_1253 = tmp_struct_46.field1;
            GibBool pvrtmp_1254 = tmp_struct_46.field2;
            
            return (GibCursorGibCursorGibBoolProd) {end_r_360, pvrtmp_1253,
                                                    pvrtmp_1254};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_49 = *(uintptr_t *) tmpcur_1243;
            GibCursor tmpcur_1255 = GIB_UNTAG(tagged_tmpcur_49);
            GibCursor tmpaftercur_1256 = tmpcur_1243 + 8;
            uint16_t tmptag_1257 = GIB_GET_TAG(tagged_tmpcur_49);
            GibCursor end_from_tagged_indr_550 = tmpcur_1255 + tmptag_1257;
            GibCursorGibCursorGibBoolProd tmp_struct_48 =
                                           caseFn_97(end_from_tagged_indr_550, end_r_361, tmpcur_1255, x_29_99_143_201, rst_30_100_144_202);
            GibCursor pvrtmp_1258 = tmp_struct_48.field0;
            GibCursor pvrtmp_1259 = tmp_struct_48.field1;
            GibBool pvrtmp_1260 = tmp_struct_48.field2;
            
            return (GibCursorGibCursorGibBoolProd) {pvrtmp_1258, pvrtmp_1259,
                                                    pvrtmp_1260};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_1242");
            exit(1);
        }
    }
}
int main(int argc, char **argv)
{
    int init_58 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibChunk region_973 =
             gib_alloc_region_on_heap(gib_get_inf_init_chunk_size());
    GibCursor r_375 = region_973.start;
    GibCursor end_r_375 = region_973.end;
    GibCursorGibCursorGibCursorProd tmp_struct_50 =
                                     mkConsIntList(end_r_375, r_375, 10000000);
    GibCursor pvrtmp_974 = tmp_struct_50.field0;
    GibCursor pvrtmp_975 = tmp_struct_50.field1;
    GibCursor pvrtmp_976 = tmp_struct_50.field2;
    GibChunk region_981 =
             gib_alloc_region_on_heap(gib_get_inf_init_chunk_size());
    GibCursor r_374 = region_981.start;
    GibCursor end_r_374 = region_981.end;
    GibCursor pvrtmp_993;
    GibCursor pvrtmp_994;
    GibCursor pvrtmp_995;
    GibVector *times_55 = gib_vector_alloc(gib_get_iters_param(),
                                           sizeof(double));
    struct timespec begin_pvrtmp_993;
    struct timespec end_pvrtmp_993;
    
    for (long long iters_pvrtmp_993 = 0; iters_pvrtmp_993 <
         gib_get_iters_param(); iters_pvrtmp_993++) {
        if (iters_pvrtmp_993 != gib_get_iters_param() - 1) {
            gib_list_bumpalloc_save_state();
            gib_ptr_bumpalloc_save_state();
        }
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_pvrtmp_993);
        
        GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_51 =
                                                           map_61(pvrtmp_974, end_r_374, &r_374, pvrtmp_975);
        GibCursor pvrtmp_982 = tmp_struct_51.field0;
        GibCursor pvrtmp_983 = tmp_struct_51.field1;
        GibCursor pvrtmp_984 = tmp_struct_51.field2;
        GibCursor pvrtmp_985 = r_374;
        GibCursor pvrtmp_986 = tmp_struct_51.field4;
        
        pvrtmp_993 = pvrtmp_983;
        pvrtmp_994 = pvrtmp_985;
        pvrtmp_995 = pvrtmp_986;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_pvrtmp_993);
        if (iters_pvrtmp_993 != gib_get_iters_param() - 1) {
            gib_list_bumpalloc_restore_state();
            gib_ptr_bumpalloc_restore_state();
        }
        
        double itertime_52 = gib_difftimespecs(&begin_pvrtmp_993,
                                               &end_pvrtmp_993);
        
        printf("itertime: %lf\n", itertime_52);
        gib_vector_inplace_update(times_55, iters_pvrtmp_993, &itertime_52);
    }
    gib_vector_inplace_sort(times_55, gib_compare_doubles);
    
    double *tmp_56 = (double *) gib_vector_nth(times_55, gib_get_iters_param() /
                                               2);
    double selftimed_54 = *tmp_56;
    double batchtime_53 = gib_sum_timing_array(times_55);
    
    gib_print_timing_array(times_55);
    gib_vector_free(times_55);
    printf("ITERS: %ld\n", gib_get_iters_param());
    printf("SIZE: %ld\n", gib_get_size_param());
    printf("BATCHTIME: %e\n", batchtime_53);
    printf("SELFTIMED: %e\n", selftimed_54);
    
    GibCursorGibCursorGibBoolProd tmp_struct_57 =
                                   checkMapAdd1(pvrtmp_974, end_r_374, pvrtmp_975, pvrtmp_994);
    GibCursor pvrtmp_1003 = tmp_struct_57.field0;
    GibCursor pvrtmp_1004 = tmp_struct_57.field1;
    GibBool pvrtmp_1005 = tmp_struct_57.field2;
    
    if (pvrtmp_1005) {
        printf("#t");
        printf("\n");
    } else {
        printf("#f");
        printf("\n");
    }
    
    int exit_59 = gib_exit();
    
    return exit_59;
}