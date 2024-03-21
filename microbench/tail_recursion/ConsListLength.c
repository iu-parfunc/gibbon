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
typedef struct GibCursorGibCursorProd_struct {
            GibCursor field0;
            GibCursor field1;
        } GibCursorGibCursorProd;
typedef struct GibCursorGibCursorGibIntProd_struct {
            GibCursor field0;
            GibCursor field1;
            GibInt field2;
        } GibCursorGibCursorGibIntProd;
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
GibCursorGibCursorGibCursorProd mkConsIntList(GibCursor end_r_215,
                                              GibCursor loc_214,
                                              GibInt len_16_67_104);
GibCursorGibCursorGibIntProd length(GibCursor end_r_217,
                                    GibCursor lst_18_69_108,
                                    GibInt accum_19_70_109);
GibCursorGibCursorGibCursorGibCursorGibCursorProd
_copy_ConsIntList(GibCursor end_r_220, GibCursor end_r_221, GibCursor loc_219,
                  GibCursor arg_38_73_113);
GibCursorGibCursorGibCursorGibCursorGibCursorProd
_copy_without_ptrs_ConsIntList(GibCursor end_r_224, GibCursor end_r_225,
                               GibCursor loc_223, GibCursor arg_43_78_118);
GibCursorGibCursorProd _traverse_ConsIntList(GibCursor end_r_227,
                                             GibCursor arg_48_83_123);
GibCursorGibCursorProd _print_ConsIntList(GibCursor end_r_229,
                                          GibCursor arg_53_87_127);
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
    gib_add_symbol(608, ")");
    gib_add_symbol(609, "(Nil");
    gib_add_symbol(610, "(Cons");
    gib_add_symbol(611, " ->r ");
    gib_add_symbol(612, " ->i ");
    gib_add_symbol(613, " ");
}
GibCursorGibCursorGibCursorProd mkConsIntList(GibCursor end_r_215,
                                              GibCursor loc_214,
                                              GibInt len_16_67_104)
{
    if (loc_214 + 18 > end_r_215) {
        gib_grow_region(&loc_214, &end_r_215);
    }
    
    GibBool fltIf_98_105 = len_16_67_104 <= 0;
    
    if (fltIf_98_105) {
        *(GibPackedTag *) loc_214 = 1;
        
        GibCursor writetag_404 = loc_214 + 1;
        GibCursor after_tag_405 = loc_214 + 1;
        
        return (GibCursorGibCursorGibCursorProd) {end_r_215, loc_214,
                                                  after_tag_405};
    } else {
        GibInt fltAppE_99_106 = len_16_67_104 - 1;
        GibCursor loc_239 = loc_214 + 9;
        
        *(GibPackedTag *) loc_214 = 0;
        
        GibCursor writetag_413 = loc_214 + 1;
        GibCursor after_tag_414 = loc_214 + 1;
        
        *(GibInt *) after_tag_414 = len_16_67_104;
        
        GibCursor writecur_418 = after_tag_414 + sizeof(GibInt);
        GibCursorGibCursorGibCursorProd tmp_struct_0 =
                                         mkConsIntList(end_r_215, loc_239, fltAppE_99_106);
        GibCursor pvrtmp_629 = tmp_struct_0.field0;
        GibCursor pvrtmp_630 = tmp_struct_0.field1;
        GibCursor pvrtmp_631 = tmp_struct_0.field2;
        
        return (GibCursorGibCursorGibCursorProd) {pvrtmp_629, loc_214,
                                                  pvrtmp_631};
    }
}
GibCursorGibCursorGibIntProd length(GibCursor end_r_217,
                                    GibCursor lst_18_69_108,
                                    GibInt accum_19_70_109)
{
    GibPackedTag tmpval_640 = *(GibPackedTag *) lst_18_69_108;
    GibCursor tmpcur_641 = lst_18_69_108 + 1;
    
    
  switch_659:
    ;
    switch (tmpval_640) {
        
      case 1:
        {
            GibCursor jump_296 = lst_18_69_108 + 1;
            
            return (GibCursorGibCursorGibIntProd) {end_r_217, jump_296,
                                                   accum_19_70_109};
            break;
        }
        
      case 0:
        {
            GibInt tmpval_642 = *(GibInt *) tmpcur_641;
            GibCursor tmpcur_643 = tmpcur_641 + sizeof(GibInt);
            GibInt fltAppE_100_112 = accum_19_70_109 + 1;
            GibCursorGibCursorGibIntProd tmp_struct_4 =
                                          length(end_r_217, tmpcur_643, fltAppE_100_112);
            GibCursor pvrtmp_644 = tmp_struct_4.field0;
            GibCursor pvrtmp_645 = tmp_struct_4.field1;
            GibInt pvrtmp_646 = tmp_struct_4.field2;
            
            return (GibCursorGibCursorGibIntProd) {pvrtmp_644, pvrtmp_645,
                                                   pvrtmp_646};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_6 = *(uintptr_t *) tmpcur_641;
            GibCursor tmpcur_647 = GIB_UNTAG(tagged_tmpcur_6);
            GibCursor tmpaftercur_648 = tmpcur_641 + 8;
            uint16_t tmptag_649 = GIB_GET_TAG(tagged_tmpcur_6);
            GibCursor end_from_tagged_indr_323 = tmpcur_647 + tmptag_649;
            GibCursor jump_325 = tmpcur_641 + 8;
            GibCursorGibCursorGibIntProd tmp_struct_5 =
                                          length(end_from_tagged_indr_323, tmpcur_647, accum_19_70_109);
            GibCursor pvrtmp_650 = tmp_struct_5.field0;
            GibCursor pvrtmp_651 = tmp_struct_5.field1;
            GibInt pvrtmp_652 = tmp_struct_5.field2;
            
            return (GibCursorGibCursorGibIntProd) {end_r_217, jump_325,
                                                   pvrtmp_652};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_8 = *(uintptr_t *) tmpcur_641;
            GibCursor tmpcur_653 = GIB_UNTAG(tagged_tmpcur_8);
            GibCursor tmpaftercur_654 = tmpcur_641 + 8;
            uint16_t tmptag_655 = GIB_GET_TAG(tagged_tmpcur_8);
            GibCursor end_from_tagged_indr_323 = tmpcur_653 + tmptag_655;
            GibCursorGibCursorGibIntProd tmp_struct_7 =
                                          length(end_from_tagged_indr_323, tmpcur_653, accum_19_70_109);
            GibCursor pvrtmp_656 = tmp_struct_7.field0;
            GibCursor pvrtmp_657 = tmp_struct_7.field1;
            GibInt pvrtmp_658 = tmp_struct_7.field2;
            
            return (GibCursorGibCursorGibIntProd) {pvrtmp_656, pvrtmp_657,
                                                   pvrtmp_658};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_640");
            exit(1);
        }
    }
}
GibCursorGibCursorGibCursorGibCursorGibCursorProd _copy_ConsIntList(GibCursor end_r_220,
                                                                    GibCursor end_r_221,
                                                                    GibCursor loc_219,
                                                                    GibCursor arg_38_73_113)
{
    if (loc_219 + 18 > end_r_221) {
        gib_grow_region(&loc_219, &end_r_221);
    }
    
    GibPackedTag tmpval_660 = *(GibPackedTag *) arg_38_73_113;
    GibCursor tmpcur_661 = arg_38_73_113 + 1;
    
    
  switch_709:
    ;
    switch (tmpval_660) {
        
      case 0:
        {
            GibInt tmpval_662 = *(GibInt *) tmpcur_661;
            GibCursor tmpcur_663 = tmpcur_661 + sizeof(GibInt);
            GibCursor loc_257 = loc_219 + 9;
            
            *(GibPackedTag *) loc_219 = 0;
            
            GibCursor writetag_440 = loc_219 + 1;
            GibCursor after_tag_441 = loc_219 + 1;
            
            *(GibInt *) after_tag_441 = tmpval_662;
            
            GibCursor writecur_445 = after_tag_441 + sizeof(GibInt);
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_9 =
                                                               _copy_ConsIntList(end_r_220, end_r_221, loc_257, tmpcur_663);
            GibCursor pvrtmp_664 = tmp_struct_9.field0;
            GibCursor pvrtmp_665 = tmp_struct_9.field1;
            GibCursor pvrtmp_666 = tmp_struct_9.field2;
            GibCursor pvrtmp_667 = tmp_struct_9.field3;
            GibCursor pvrtmp_668 = tmp_struct_9.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_664,
                                                                        pvrtmp_665,
                                                                        pvrtmp_666,
                                                                        loc_219,
                                                                        pvrtmp_668};
            break;
        }
        
      case 1:
        {
            GibCursor jump_303 = arg_38_73_113 + 1;
            
            *(GibPackedTag *) loc_219 = 1;
            
            GibCursor writetag_450 = loc_219 + 1;
            GibCursor after_tag_451 = loc_219 + 1;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_220,
                                                                        end_r_221,
                                                                        jump_303,
                                                                        loc_219,
                                                                        after_tag_451};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_11 = *(uintptr_t *) tmpcur_661;
            GibCursor tmpcur_681 = GIB_UNTAG(tagged_tmpcur_11);
            GibCursor tmpaftercur_682 = tmpcur_661 + 8;
            uint16_t tmptag_683 = GIB_GET_TAG(tagged_tmpcur_11);
            GibCursor end_from_tagged_indr_329 = tmpcur_681 + tmptag_683;
            GibCursor jump_331 = tmpcur_661 + 8;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_10 =
                                                               _copy_ConsIntList(end_from_tagged_indr_329, end_r_221, loc_219, tmpcur_681);
            GibCursor pvrtmp_684 = tmp_struct_10.field0;
            GibCursor pvrtmp_685 = tmp_struct_10.field1;
            GibCursor pvrtmp_686 = tmp_struct_10.field2;
            GibCursor pvrtmp_687 = tmp_struct_10.field3;
            GibCursor pvrtmp_688 = tmp_struct_10.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_220,
                                                                        pvrtmp_685,
                                                                        jump_331,
                                                                        pvrtmp_687,
                                                                        pvrtmp_688};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_13 = *(uintptr_t *) tmpcur_661;
            GibCursor tmpcur_695 = GIB_UNTAG(tagged_tmpcur_13);
            GibCursor tmpaftercur_696 = tmpcur_661 + 8;
            uint16_t tmptag_697 = GIB_GET_TAG(tagged_tmpcur_13);
            GibCursor end_from_tagged_indr_329 = tmpcur_695 + tmptag_697;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_12 =
                                                               _copy_ConsIntList(end_from_tagged_indr_329, end_r_221, loc_219, tmpcur_695);
            GibCursor pvrtmp_698 = tmp_struct_12.field0;
            GibCursor pvrtmp_699 = tmp_struct_12.field1;
            GibCursor pvrtmp_700 = tmp_struct_12.field2;
            GibCursor pvrtmp_701 = tmp_struct_12.field3;
            GibCursor pvrtmp_702 = tmp_struct_12.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_698,
                                                                        pvrtmp_699,
                                                                        pvrtmp_700,
                                                                        pvrtmp_701,
                                                                        pvrtmp_702};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_660");
            exit(1);
        }
    }
}
GibCursorGibCursorGibCursorGibCursorGibCursorProd _copy_without_ptrs_ConsIntList(GibCursor end_r_224,
                                                                                 GibCursor end_r_225,
                                                                                 GibCursor loc_223,
                                                                                 GibCursor arg_43_78_118)
{
    GibPackedTag tmpval_710 = *(GibPackedTag *) arg_43_78_118;
    GibCursor tmpcur_711 = arg_43_78_118 + 1;
    
    
  switch_759:
    ;
    switch (tmpval_710) {
        
      case 0:
        {
            GibInt tmpval_712 = *(GibInt *) tmpcur_711;
            GibCursor tmpcur_713 = tmpcur_711 + sizeof(GibInt);
            GibCursor loc_270 = loc_223 + 9;
            
            *(GibPackedTag *) loc_223 = 0;
            
            GibCursor writetag_472 = loc_223 + 1;
            GibCursor after_tag_473 = loc_223 + 1;
            
            *(GibInt *) after_tag_473 = tmpval_712;
            
            GibCursor writecur_477 = after_tag_473 + sizeof(GibInt);
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_17 =
                                                               _copy_without_ptrs_ConsIntList(end_r_224, end_r_225, loc_270, tmpcur_713);
            GibCursor pvrtmp_714 = tmp_struct_17.field0;
            GibCursor pvrtmp_715 = tmp_struct_17.field1;
            GibCursor pvrtmp_716 = tmp_struct_17.field2;
            GibCursor pvrtmp_717 = tmp_struct_17.field3;
            GibCursor pvrtmp_718 = tmp_struct_17.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_714,
                                                                        pvrtmp_715,
                                                                        pvrtmp_716,
                                                                        loc_223,
                                                                        pvrtmp_718};
            break;
        }
        
      case 1:
        {
            GibCursor jump_308 = arg_43_78_118 + 1;
            
            *(GibPackedTag *) loc_223 = 1;
            
            GibCursor writetag_482 = loc_223 + 1;
            GibCursor after_tag_483 = loc_223 + 1;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_224,
                                                                        end_r_225,
                                                                        jump_308,
                                                                        loc_223,
                                                                        after_tag_483};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_19 = *(uintptr_t *) tmpcur_711;
            GibCursor tmpcur_731 = GIB_UNTAG(tagged_tmpcur_19);
            GibCursor tmpaftercur_732 = tmpcur_711 + 8;
            uint16_t tmptag_733 = GIB_GET_TAG(tagged_tmpcur_19);
            GibCursor end_from_tagged_indr_335 = tmpcur_731 + tmptag_733;
            GibCursor jump_337 = tmpcur_711 + 8;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_18 =
                                                               _copy_without_ptrs_ConsIntList(end_from_tagged_indr_335, end_r_225, loc_223, tmpcur_731);
            GibCursor pvrtmp_734 = tmp_struct_18.field0;
            GibCursor pvrtmp_735 = tmp_struct_18.field1;
            GibCursor pvrtmp_736 = tmp_struct_18.field2;
            GibCursor pvrtmp_737 = tmp_struct_18.field3;
            GibCursor pvrtmp_738 = tmp_struct_18.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {end_r_224,
                                                                        pvrtmp_735,
                                                                        jump_337,
                                                                        pvrtmp_737,
                                                                        pvrtmp_738};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_21 = *(uintptr_t *) tmpcur_711;
            GibCursor tmpcur_745 = GIB_UNTAG(tagged_tmpcur_21);
            GibCursor tmpaftercur_746 = tmpcur_711 + 8;
            uint16_t tmptag_747 = GIB_GET_TAG(tagged_tmpcur_21);
            GibCursor end_from_tagged_indr_335 = tmpcur_745 + tmptag_747;
            GibCursorGibCursorGibCursorGibCursorGibCursorProd tmp_struct_20 =
                                                               _copy_without_ptrs_ConsIntList(end_from_tagged_indr_335, end_r_225, loc_223, tmpcur_745);
            GibCursor pvrtmp_748 = tmp_struct_20.field0;
            GibCursor pvrtmp_749 = tmp_struct_20.field1;
            GibCursor pvrtmp_750 = tmp_struct_20.field2;
            GibCursor pvrtmp_751 = tmp_struct_20.field3;
            GibCursor pvrtmp_752 = tmp_struct_20.field4;
            
            return (GibCursorGibCursorGibCursorGibCursorGibCursorProd) {pvrtmp_748,
                                                                        pvrtmp_749,
                                                                        pvrtmp_750,
                                                                        pvrtmp_751,
                                                                        pvrtmp_752};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_710");
            exit(1);
        }
    }
}
GibCursorGibCursorProd _traverse_ConsIntList(GibCursor end_r_227,
                                             GibCursor arg_48_83_123)
{
    GibPackedTag tmpval_760 = *(GibPackedTag *) arg_48_83_123;
    GibCursor tmpcur_761 = arg_48_83_123 + 1;
    
    
  switch_776:
    ;
    switch (tmpval_760) {
        
      case 0:
        {
            GibInt tmpval_762 = *(GibInt *) tmpcur_761;
            GibCursor tmpcur_763 = tmpcur_761 + sizeof(GibInt);
            GibCursorGibCursorProd tmp_struct_22 =
                                    _traverse_ConsIntList(end_r_227, tmpcur_763);
            GibCursor pvrtmp_764 = tmp_struct_22.field0;
            GibCursor pvrtmp_765 = tmp_struct_22.field1;
            
            return (GibCursorGibCursorProd) {pvrtmp_764, pvrtmp_765};
            break;
        }
        
      case 1:
        {
            GibCursor jump_313 = arg_48_83_123 + 1;
            
            return (GibCursorGibCursorProd) {end_r_227, jump_313};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_24 = *(uintptr_t *) tmpcur_761;
            GibCursor tmpcur_766 = GIB_UNTAG(tagged_tmpcur_24);
            GibCursor tmpaftercur_767 = tmpcur_761 + 8;
            uint16_t tmptag_768 = GIB_GET_TAG(tagged_tmpcur_24);
            GibCursor end_from_tagged_indr_341 = tmpcur_766 + tmptag_768;
            GibCursor jump_343 = tmpcur_761 + 8;
            GibCursorGibCursorProd tmp_struct_23 =
                                    _traverse_ConsIntList(end_from_tagged_indr_341, tmpcur_766);
            GibCursor pvrtmp_769 = tmp_struct_23.field0;
            GibCursor pvrtmp_770 = tmp_struct_23.field1;
            
            return (GibCursorGibCursorProd) {end_r_227, jump_343};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_26 = *(uintptr_t *) tmpcur_761;
            GibCursor tmpcur_771 = GIB_UNTAG(tagged_tmpcur_26);
            GibCursor tmpaftercur_772 = tmpcur_761 + 8;
            uint16_t tmptag_773 = GIB_GET_TAG(tagged_tmpcur_26);
            GibCursor end_from_tagged_indr_341 = tmpcur_771 + tmptag_773;
            GibCursorGibCursorProd tmp_struct_25 =
                                    _traverse_ConsIntList(end_from_tagged_indr_341, tmpcur_771);
            GibCursor pvrtmp_774 = tmp_struct_25.field0;
            GibCursor pvrtmp_775 = tmp_struct_25.field1;
            
            return (GibCursorGibCursorProd) {pvrtmp_774, pvrtmp_775};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_760");
            exit(1);
        }
    }
}
GibCursorGibCursorProd _print_ConsIntList(GibCursor end_r_229,
                                          GibCursor arg_53_87_127)
{
    GibPackedTag tmpval_777 = *(GibPackedTag *) arg_53_87_127;
    GibCursor tmpcur_778 = arg_53_87_127 + 1;
    
    
  switch_793:
    ;
    switch (tmpval_777) {
        
      case 0:
        {
            GibInt tmpval_779 = *(GibInt *) tmpcur_778;
            GibCursor tmpcur_780 = tmpcur_778 + sizeof(GibInt);
            unsigned char wildcard_58_90_130 = gib_print_symbol(610);
            unsigned char wildcard_61_91_131 = gib_print_symbol(613);
            unsigned char y_56_92_132 = printf("%ld", tmpval_779);
            unsigned char wildcard_60_93_133 = gib_print_symbol(613);
            GibCursorGibCursorProd tmp_struct_27 =
                                    _print_ConsIntList(end_r_229, tmpcur_780);
            GibCursor pvrtmp_781 = tmp_struct_27.field0;
            GibCursor pvrtmp_782 = tmp_struct_27.field1;
            unsigned char wildcard_59_95_135 = gib_print_symbol(608);
            
            return (GibCursorGibCursorProd) {pvrtmp_781, pvrtmp_782};
            break;
        }
        
      case 1:
        {
            GibCursor jump_318 = arg_53_87_127 + 1;
            unsigned char wildcard_62_96_136 = gib_print_symbol(609);
            unsigned char wildcard_63_97_137 = gib_print_symbol(608);
            
            return (GibCursorGibCursorProd) {end_r_229, jump_318};
            break;
        }
        
      case GIB_INDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_29 = *(uintptr_t *) tmpcur_778;
            GibCursor tmpcur_783 = GIB_UNTAG(tagged_tmpcur_29);
            GibCursor tmpaftercur_784 = tmpcur_778 + 8;
            uint16_t tmptag_785 = GIB_GET_TAG(tagged_tmpcur_29);
            GibCursor end_from_tagged_indr_347 = tmpcur_783 + tmptag_785;
            GibCursor jump_349 = tmpcur_778 + 8;
            unsigned char wildcard_352 = gib_print_symbol(612);
            GibCursorGibCursorProd tmp_struct_28 =
                                    _print_ConsIntList(end_from_tagged_indr_347, tmpcur_783);
            GibCursor pvrtmp_786 = tmp_struct_28.field0;
            GibCursor pvrtmp_787 = tmp_struct_28.field1;
            
            return (GibCursorGibCursorProd) {end_r_229, jump_349};
            break;
        }
        
      case GIB_REDIRECTION_TAG:
        {
            uintptr_t tagged_tmpcur_31 = *(uintptr_t *) tmpcur_778;
            GibCursor tmpcur_788 = GIB_UNTAG(tagged_tmpcur_31);
            GibCursor tmpaftercur_789 = tmpcur_778 + 8;
            uint16_t tmptag_790 = GIB_GET_TAG(tagged_tmpcur_31);
            GibCursor end_from_tagged_indr_347 = tmpcur_788 + tmptag_790;
            unsigned char wildcard_352 = gib_print_symbol(611);
            GibCursorGibCursorProd tmp_struct_30 =
                                    _print_ConsIntList(end_from_tagged_indr_347, tmpcur_788);
            GibCursor pvrtmp_791 = tmp_struct_30.field0;
            GibCursor pvrtmp_792 = tmp_struct_30.field1;
            
            return (GibCursorGibCursorProd) {pvrtmp_791, pvrtmp_792};
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tmpval_777");
            exit(1);
        }
    }
}
int main(int argc, char **argv)
{
    int init_39 = gib_init(argc, argv);
    
    info_table_initialize();
    symbol_table_initialize();
    
    GibInt n_13_64_101 = gib_get_size_param();
    GibChunk region_614 =
             gib_alloc_region_on_heap(gib_get_inf_init_chunk_size());
    GibCursor r_235 = region_614.start;
    GibCursor end_r_235 = region_614.end;
    GibCursorGibCursorGibCursorProd tmp_struct_32 =
                                     mkConsIntList(end_r_235, r_235, n_13_64_101);
    GibCursor pvrtmp_615 = tmp_struct_32.field0;
    GibCursor pvrtmp_616 = tmp_struct_32.field1;
    GibCursor pvrtmp_617 = tmp_struct_32.field2;
    GibInt timed_555;
    GibVector *times_37 = gib_vector_alloc(gib_get_iters_param(),
                                           sizeof(double));
    struct timespec begin_timed_555;
    struct timespec end_timed_555;
    
    for (long long iters_timed_555 = 0; iters_timed_555 < gib_get_iters_param();
         iters_timed_555++) {
        if (iters_timed_555 != gib_get_iters_param() - 1) {
            gib_list_bumpalloc_save_state();
            gib_ptr_bumpalloc_save_state();
        }
        clock_gettime(CLOCK_MONOTONIC_RAW, &begin_timed_555);
        
        GibCursorGibCursorGibIntProd tmp_struct_33 =
                                      length(pvrtmp_615, pvrtmp_616, 0);
        GibCursor pvrtmp_622 = tmp_struct_33.field0;
        GibCursor pvrtmp_623 = tmp_struct_33.field1;
        GibInt pvrtmp_624 = tmp_struct_33.field2;
        
        timed_555 = pvrtmp_624;
        clock_gettime(CLOCK_MONOTONIC_RAW, &end_timed_555);
        if (iters_timed_555 != gib_get_iters_param() - 1) {
            gib_list_bumpalloc_restore_state();
            gib_ptr_bumpalloc_restore_state();
        }
        
        double itertime_34 = gib_difftimespecs(&begin_timed_555,
                                               &end_timed_555);
        
        printf("itertime: %lf\n", itertime_34);
        gib_vector_inplace_update(times_37, iters_timed_555, &itertime_34);
    }
    gib_vector_inplace_sort(times_37, gib_compare_doubles);
    
    double *tmp_38 = (double *) gib_vector_nth(times_37, gib_get_iters_param() /
                                               2);
    double selftimed_36 = *tmp_38;
    double batchtime_35 = gib_sum_timing_array(times_37);
    
    gib_print_timing_array(times_37);
    gib_vector_free(times_37);
    printf("ITERS: %ld\n", gib_get_iters_param());
    printf("SIZE: %ld\n", gib_get_size_param());
    printf("BATCHTIME: %e\n", batchtime_35);
    printf("SELFTIMED: %e\n", selftimed_36);
    printf("%ld", timed_555);
    printf("\n");
    
    int exit_40 = gib_exit();
    
    return exit_40;
}