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
GibCursor _copy_Clock101(GibCursor arg105613651556);
GibCursor _copy_without_ptrs_Clock101(GibCursor arg106113701561);
unsigned char _traverse_Clock101(GibCursor arg106613751566);
unsigned char _print_Clock101(GibCursor arg107113791570);
GibCursor _copy_Timestamp102(GibCursor arg108013881579);
GibCursor _copy_without_ptrs_Timestamp102(GibCursor arg108513931584);
unsigned char _traverse_Timestamp102(GibCursor arg109013981589);
unsigned char _print_Timestamp102(GibCursor arg109514021593);
GibCursor _copy_Ord107(GibCursor arg110414111602);
GibCursor _copy_without_ptrs_Ord107(GibCursor arg110514121603);
unsigned char _traverse_Ord107(GibCursor arg110614131604);
unsigned char _print_Ord107(GibCursor arg110714141605);
GibCursor _copy_Maybe106_v701(GibCursor arg111614231614);
GibCursor _copy_without_ptrs_Maybe106_v701(GibCursor arg111914261617);
unsigned char _traverse_Maybe106_v701(GibCursor arg112214291620);
unsigned char _print_Maybe106_v701(GibCursor arg112514311622);
GibCursor _copy_Map137_v699(GibCursor arg113314391630);
GibCursor _copy_without_ptrs_Map137_v699(GibCursor arg114414501641);
unsigned char _traverse_Map137_v699(GibCursor arg115514611652);
unsigned char _print_Map137_v699(GibCursor arg116614691660);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Clock101_T,
            Map137_v699_T,
            Maybe106_v701_T,
            Ord107_T,
            Timestamp102_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(12);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[3];
    
    error = gib_info_table_insert_packed_dcon(Clock101_T, 0, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Clock101_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map137_v699_T, 1, 24, 2, 3, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map137_v699_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map137_v699_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map137_v699_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe106_v701_T, 1, 8, 0, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe106_v701_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe106_v701_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe106_v701_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord107_T, 3, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord107_T, 3);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord107_T, 2, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord107_T, 2);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord107_T, 1, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord107_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord107_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord107_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Timestamp102_T, 0, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Timestamp102_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(1680, ")");
    gib_add_symbol(1681, "(Tip138_v699");
    gib_add_symbol(1682, "(Timestamp103");
    gib_add_symbol(1683, "(Nothing112_v701");
    gib_add_symbol(1684, "(Lt108");
    gib_add_symbol(1685, "(Just113_v701");
    gib_add_symbol(1686, "(Gt109");
    gib_add_symbol(1687, "(Eq110");
    gib_add_symbol(1688, "(Clk104");
    gib_add_symbol(1689, "(Cc111");
    gib_add_symbol(1690, "(Bin139_v699");
    gib_add_symbol(1691, " ");
}
GibCursor _copy_Clock101(GibCursor arg105613651556)
{
    GibInt tag1692 = ((GibIntGibIntGibCursorProd *) arg105613651556)->field0;
    GibInt x105713661557 =
           ((GibIntGibIntGibCursorProd *) arg105613651556)->field1;
    GibCursor x105813671558 =
              ((GibIntGibIntGibCursorProd *) arg105613651556)->field2;
    GibCursor y106013691560 =  _copy_Map137_v699(x105813671558);
    GibPtr tailift1693 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1693)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1693)->field1 = x105713661557;
    ((GibIntGibIntGibCursorProd *) tailift1693)->field2 = y106013691560;
    return tailift1693;
}
GibCursor _copy_without_ptrs_Clock101(GibCursor arg106113701561)
{
    GibInt tag1694 = ((GibIntGibIntGibCursorProd *) arg106113701561)->field0;
    GibInt x106213711562 =
           ((GibIntGibIntGibCursorProd *) arg106113701561)->field1;
    GibCursor x106313721563 =
              ((GibIntGibIntGibCursorProd *) arg106113701561)->field2;
    GibCursor y106513741565 =  _copy_without_ptrs_Map137_v699(x106313721563);
    GibPtr tailift1695 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1695)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1695)->field1 = x106213711562;
    ((GibIntGibIntGibCursorProd *) tailift1695)->field2 = y106513741565;
    return tailift1695;
}
unsigned char _traverse_Clock101(GibCursor arg106613751566)
{
    GibInt tag1696 = ((GibIntGibIntGibCursorProd *) arg106613751566)->field0;
    GibInt x106713761567 =
           ((GibIntGibIntGibCursorProd *) arg106613751566)->field1;
    GibCursor x106813771568 =
              ((GibIntGibIntGibCursorProd *) arg106613751566)->field2;
    unsigned char y107013781569 =  _traverse_Map137_v699(x106813771568);
    
    return 0;
}
unsigned char _print_Clock101(GibCursor arg107113791570)
{
    GibInt tag1697 = ((GibIntGibIntGibCursorProd *) arg107113791570)->field0;
    GibInt x107213801571 =
           ((GibIntGibIntGibCursorProd *) arg107113791570)->field1;
    GibCursor x107313811572 =
              ((GibIntGibIntGibCursorProd *) arg107113791570)->field2;
    unsigned char wildcard107613821573 = gib_print_symbol(1688);
    unsigned char wildcard107913831574 = gib_print_symbol(1691);
    unsigned char y107413841575 = printf("%ld", x107213801571);
    unsigned char wildcard107813851576 = gib_print_symbol(1691);
    unsigned char y107513861577 =  _print_Map137_v699(x107313811572);
    unsigned char wildcard107713871578 = gib_print_symbol(1680);
    
    return 0;
}
GibCursor _copy_Timestamp102(GibCursor arg108013881579)
{
    GibInt tag1698 = ((GibIntGibIntGibCursorProd *) arg108013881579)->field0;
    GibInt x108113891580 =
           ((GibIntGibIntGibCursorProd *) arg108013881579)->field1;
    GibCursor x108213901581 =
              ((GibIntGibIntGibCursorProd *) arg108013881579)->field2;
    GibCursor y108413921583 =  _copy_Clock101(x108213901581);
    GibPtr tailift1699 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1699)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1699)->field1 = x108113891580;
    ((GibIntGibIntGibCursorProd *) tailift1699)->field2 = y108413921583;
    return tailift1699;
}
GibCursor _copy_without_ptrs_Timestamp102(GibCursor arg108513931584)
{
    GibInt tag1700 = ((GibIntGibIntGibCursorProd *) arg108513931584)->field0;
    GibInt x108613941585 =
           ((GibIntGibIntGibCursorProd *) arg108513931584)->field1;
    GibCursor x108713951586 =
              ((GibIntGibIntGibCursorProd *) arg108513931584)->field2;
    GibCursor y108913971588 =  _copy_without_ptrs_Clock101(x108713951586);
    GibPtr tailift1701 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1701)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1701)->field1 = x108613941585;
    ((GibIntGibIntGibCursorProd *) tailift1701)->field2 = y108913971588;
    return tailift1701;
}
unsigned char _traverse_Timestamp102(GibCursor arg109013981589)
{
    GibInt tag1702 = ((GibIntGibIntGibCursorProd *) arg109013981589)->field0;
    GibInt x109113991590 =
           ((GibIntGibIntGibCursorProd *) arg109013981589)->field1;
    GibCursor x109214001591 =
              ((GibIntGibIntGibCursorProd *) arg109013981589)->field2;
    unsigned char y109414011592 =  _traverse_Clock101(x109214001591);
    
    return 0;
}
unsigned char _print_Timestamp102(GibCursor arg109514021593)
{
    GibInt tag1703 = ((GibIntGibIntGibCursorProd *) arg109514021593)->field0;
    GibInt x109614031594 =
           ((GibIntGibIntGibCursorProd *) arg109514021593)->field1;
    GibCursor x109714041595 =
              ((GibIntGibIntGibCursorProd *) arg109514021593)->field2;
    unsigned char wildcard110014051596 = gib_print_symbol(1682);
    unsigned char wildcard110314061597 = gib_print_symbol(1691);
    unsigned char y109814071598 = printf("%ld", x109614031594);
    unsigned char wildcard110214081599 = gib_print_symbol(1691);
    unsigned char y109914091600 =  _print_Clock101(x109714041595);
    unsigned char wildcard110114101601 = gib_print_symbol(1680);
    
    return 0;
}
GibCursor _copy_Ord107(GibCursor arg110414111602)
{
    GibPackedTag tag1704 = *(GibPackedTag *) arg110414111602;
    GibCursor tail1705 = arg110414111602 + sizeof(GibInt);
    
    
  switch1710:
    ;
    switch (tag1704) {
        
      case 0:
        {
            GibPtr tailift1706 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1706)->field0 = 0;
            return tailift1706;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1707 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1707)->field0 = 1;
            return tailift1707;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1708 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1708)->field0 = 2;
            return tailift1708;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1709 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1709)->field0 = 3;
            return tailift1709;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1704");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Ord107(GibCursor arg110514121603)
{
    GibPackedTag tag1711 = *(GibPackedTag *) arg110514121603;
    GibCursor tail1712 = arg110514121603 + sizeof(GibInt);
    
    
  switch1717:
    ;
    switch (tag1711) {
        
      case 0:
        {
            GibPtr tailift1713 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1713)->field0 = 0;
            return tailift1713;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1714 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1714)->field0 = 1;
            return tailift1714;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1715 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1715)->field0 = 2;
            return tailift1715;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1716 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1716)->field0 = 3;
            return tailift1716;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1711");
            exit(1);
        }
    }
}
unsigned char _traverse_Ord107(GibCursor arg110614131604)
{
    GibPackedTag tag1718 = *(GibPackedTag *) arg110614131604;
    GibCursor tail1719 = arg110614131604 + sizeof(GibInt);
    
    
  switch1720:
    ;
    switch (tag1718) {
        
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
            printf("%s\n", "Unknown tag in: tag1718");
            exit(1);
        }
    }
}
unsigned char _print_Ord107(GibCursor arg110714141605)
{
    GibPackedTag tag1721 = *(GibPackedTag *) arg110714141605;
    GibCursor tail1722 = arg110714141605 + sizeof(GibInt);
    
    
  switch1723:
    ;
    switch (tag1721) {
        
      case 0:
        {
            unsigned char wildcard110814151606 = gib_print_symbol(1684);
            unsigned char wildcard110914161607 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard111014171608 = gib_print_symbol(1686);
            unsigned char wildcard111114181609 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard111214191610 = gib_print_symbol(1687);
            unsigned char wildcard111314201611 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard111414211612 = gib_print_symbol(1689);
            unsigned char wildcard111514221613 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1721");
            exit(1);
        }
    }
}
GibCursor _copy_Maybe106_v701(GibCursor arg111614231614)
{
    GibPackedTag tag1724 = *(GibPackedTag *) arg111614231614;
    GibCursor tail1725 = arg111614231614 + sizeof(GibInt);
    
    
  switch1728:
    ;
    switch (tag1724) {
        
      case 0:
        {
            GibPtr tailift1726 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1726)->field0 = 0;
            return tailift1726;
            break;
        }
        
      case 1:
        {
            GibInt x111714241615 = ((GibIntProd *) tail1725)->field0;
            GibPtr tailift1727 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift1727)->field0 = 1;
            ((GibIntGibIntProd *) tailift1727)->field1 = x111714241615;
            return tailift1727;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1724");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Maybe106_v701(GibCursor arg111914261617)
{
    GibPackedTag tag1729 = *(GibPackedTag *) arg111914261617;
    GibCursor tail1730 = arg111914261617 + sizeof(GibInt);
    
    
  switch1733:
    ;
    switch (tag1729) {
        
      case 0:
        {
            GibPtr tailift1731 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1731)->field0 = 0;
            return tailift1731;
            break;
        }
        
      case 1:
        {
            GibInt x112014271618 = ((GibIntProd *) tail1730)->field0;
            GibPtr tailift1732 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift1732)->field0 = 1;
            ((GibIntGibIntProd *) tailift1732)->field1 = x112014271618;
            return tailift1732;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1729");
            exit(1);
        }
    }
}
unsigned char _traverse_Maybe106_v701(GibCursor arg112214291620)
{
    GibPackedTag tag1734 = *(GibPackedTag *) arg112214291620;
    GibCursor tail1735 = arg112214291620 + sizeof(GibInt);
    
    
  switch1736:
    ;
    switch (tag1734) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x112314301621 = ((GibIntProd *) tail1735)->field0;
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1734");
            exit(1);
        }
    }
}
unsigned char _print_Maybe106_v701(GibCursor arg112514311622)
{
    GibPackedTag tag1737 = *(GibPackedTag *) arg112514311622;
    GibCursor tail1738 = arg112514311622 + sizeof(GibInt);
    
    
  switch1739:
    ;
    switch (tag1737) {
        
      case 0:
        {
            unsigned char wildcard112614321623 = gib_print_symbol(1683);
            unsigned char wildcard112714331624 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x112814341625 = ((GibIntProd *) tail1738)->field0;
            unsigned char wildcard113014351626 = gib_print_symbol(1685);
            unsigned char wildcard113214361627 = gib_print_symbol(1691);
            unsigned char y112914371628 = printf("%ld", x112814341625);
            unsigned char wildcard113114381629 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1737");
            exit(1);
        }
    }
}
GibCursor _copy_Map137_v699(GibCursor arg113314391630)
{
    GibPackedTag tag1740 = *(GibPackedTag *) arg113314391630;
    GibCursor tail1741 = arg113314391630 + sizeof(GibInt);
    
    
  switch1744:
    ;
    switch (tag1740) {
        
      case 0:
        {
            GibPtr tailift1742 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1742)->field0 = 0;
            return tailift1742;
            break;
        }
        
      case 1:
        {
            GibInt x113414401631 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1741)->field0;
            GibInt x113514411632 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1741)->field1;
            GibInt x113614421633 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1741)->field2;
            GibCursor x113714431634 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1741)->field3;
            GibCursor x113814441635 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1741)->field4;
            GibCursor y114214481639 =  _copy_Map137_v699(x113714431634);
            GibCursor y114314491640 =  _copy_Map137_v699(x113814441635);
            GibPtr tailift1743 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1743)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1743)->field1 =
                x113414401631;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1743)->field2 =
                x113514411632;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1743)->field3 =
                x113614421633;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1743)->field4 =
                y114214481639;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1743)->field5 =
                y114314491640;
            return tailift1743;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1740");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Map137_v699(GibCursor arg114414501641)
{
    GibPackedTag tag1745 = *(GibPackedTag *) arg114414501641;
    GibCursor tail1746 = arg114414501641 + sizeof(GibInt);
    
    
  switch1749:
    ;
    switch (tag1745) {
        
      case 0:
        {
            GibPtr tailift1747 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1747)->field0 = 0;
            return tailift1747;
            break;
        }
        
      case 1:
        {
            GibInt x114514511642 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1746)->field0;
            GibInt x114614521643 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1746)->field1;
            GibInt x114714531644 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1746)->field2;
            GibCursor x114814541645 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1746)->field3;
            GibCursor x114914551646 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1746)->field4;
            GibCursor y115314591650 =
                       _copy_without_ptrs_Map137_v699(x114814541645);
            GibCursor y115414601651 =
                       _copy_without_ptrs_Map137_v699(x114914551646);
            GibPtr tailift1748 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1748)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1748)->field1 =
                x114514511642;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1748)->field2 =
                x114614521643;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1748)->field3 =
                x114714531644;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1748)->field4 =
                y115314591650;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1748)->field5 =
                y115414601651;
            return tailift1748;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1745");
            exit(1);
        }
    }
}
unsigned char _traverse_Map137_v699(GibCursor arg115514611652)
{
    GibPackedTag tag1750 = *(GibPackedTag *) arg115514611652;
    GibCursor tail1751 = arg115514611652 + sizeof(GibInt);
    
    
  switch1752:
    ;
    switch (tag1750) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x115614621653 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1751)->field0;
            GibInt x115714631654 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1751)->field1;
            GibInt x115814641655 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1751)->field2;
            GibCursor x115914651656 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1751)->field3;
            GibCursor x116014661657 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1751)->field4;
            unsigned char y116414671658 =  _traverse_Map137_v699(x115914651656);
            unsigned char y116514681659 =  _traverse_Map137_v699(x116014661657);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1750");
            exit(1);
        }
    }
}
unsigned char _print_Map137_v699(GibCursor arg116614691660)
{
    GibPackedTag tag1753 = *(GibPackedTag *) arg116614691660;
    GibCursor tail1754 = arg116614691660 + sizeof(GibInt);
    
    
  switch1755:
    ;
    switch (tag1753) {
        
      case 0:
        {
            unsigned char wildcard116714701661 = gib_print_symbol(1681);
            unsigned char wildcard116814711662 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x116914721663 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1754)->field0;
            GibInt x117014731664 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1754)->field1;
            GibInt x117114741665 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1754)->field2;
            GibCursor x117214751666 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1754)->field3;
            GibCursor x117314761667 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1754)->field4;
            unsigned char wildcard117914771668 = gib_print_symbol(1690);
            unsigned char wildcard118514781669 = gib_print_symbol(1691);
            unsigned char y117414791670 = printf("%ld", x116914721663);
            unsigned char wildcard118414801671 = gib_print_symbol(1691);
            unsigned char y117514811672 = printf("%ld", x117014731664);
            unsigned char wildcard118314821673 = gib_print_symbol(1691);
            unsigned char y117614831674 = printf("%ld", x117114741665);
            unsigned char wildcard118214841675 = gib_print_symbol(1691);
            unsigned char y117714851676 =  _print_Map137_v699(x117214751666);
            unsigned char wildcard118114861677 = gib_print_symbol(1691);
            unsigned char y117814871678 =  _print_Map137_v699(x117314761667);
            unsigned char wildcard118014881679 = gib_print_symbol(1680);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1753");
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