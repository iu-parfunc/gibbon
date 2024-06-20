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
GibCursor _copy_Clock94(GibCursor arg102213191506);
GibCursor _copy_without_ptrs_Clock94(GibCursor arg102713241511);
unsigned char _traverse_Clock94(GibCursor arg103213291516);
unsigned char _print_Clock94(GibCursor arg103713331520);
GibCursor _copy_Timestamp95(GibCursor arg104613421529);
GibCursor _copy_without_ptrs_Timestamp95(GibCursor arg105113471534);
unsigned char _traverse_Timestamp95(GibCursor arg105613521539);
unsigned char _print_Timestamp95(GibCursor arg106113561543);
GibCursor _copy_Ord100(GibCursor arg107013651552);
GibCursor _copy_without_ptrs_Ord100(GibCursor arg107113661553);
unsigned char _traverse_Ord100(GibCursor arg107213671554);
unsigned char _print_Ord100(GibCursor arg107313681555);
GibCursor _copy_Maybe99_v676(GibCursor arg108213771564);
GibCursor _copy_without_ptrs_Maybe99_v676(GibCursor arg108513801567);
unsigned char _traverse_Maybe99_v676(GibCursor arg108813831570);
unsigned char _print_Maybe99_v676(GibCursor arg109113851572);
GibCursor _copy_Map129_v674(GibCursor arg109913931580);
GibCursor _copy_without_ptrs_Map129_v674(GibCursor arg111014041591);
unsigned char _traverse_Map129_v674(GibCursor arg112114151602);
unsigned char _print_Map129_v674(GibCursor arg113214231610);
typedef enum {
            GibInt_T,
            GibFloat_T,
            GibSym_T,
            GibBool_T,
            GibVector_T,
            GibList_T,
            GibCursor_T,
            Clock94_T,
            Map129_v674_T,
            Maybe99_v676_T,
            Ord100_T,
            Timestamp95_T,
        } GibDatatype;
void info_table_initialize(void)
{
    int error = gib_info_table_initialize(12);
    
    if (error < 0) {
        fprintf(stderr, "Couldn't initialize info table, errorno=%d", error);
        exit(1);
    }
    
    GibDatatype field_tys[3];
    
    error = gib_info_table_insert_packed_dcon(Clock94_T, 0, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Clock94_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map129_v674_T, 1, 24, 2, 3, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map129_v674_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Map129_v674_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Map129_v674_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe99_v676_T, 1, 8, 0, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe99_v676_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Maybe99_v676_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Maybe99_v676_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord100_T, 3, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord100_T, 3);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord100_T, 2, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord100_T, 2);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord100_T, 1, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord100_T, 1);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Ord100_T, 0, 0, 0, 0, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Ord100_T, 0);
        exit(1);
    }
    error = gib_info_table_insert_packed_dcon(Timestamp95_T, 0, 8, 1, 1, 0,
                                              field_tys, 0);
    if (error < 0) {
        fprintf(stderr,
                "Couldn't insert into info table, errorno=%d, tycon=%d, dcon=%d",
                error, Timestamp95_T, 0);
        exit(1);
    }
    gib_info_table_finalize();
}
void symbol_table_initialize(void)
{
    gib_add_symbol(1630, ")");
    gib_add_symbol(1631, "(Tip130_v674");
    gib_add_symbol(1632, "(Timestamp96");
    gib_add_symbol(1633, "(Nothing105_v676");
    gib_add_symbol(1634, "(Lt101");
    gib_add_symbol(1635, "(Just106_v676");
    gib_add_symbol(1636, "(Gt102");
    gib_add_symbol(1637, "(Eq103");
    gib_add_symbol(1638, "(Clk97");
    gib_add_symbol(1639, "(Cc104");
    gib_add_symbol(1640, "(Bin131_v674");
    gib_add_symbol(1641, " ");
}
GibCursor _copy_Clock94(GibCursor arg102213191506)
{
    GibInt tag1642 = ((GibIntGibIntGibCursorProd *) arg102213191506)->field0;
    GibInt x102313201507 =
           ((GibIntGibIntGibCursorProd *) arg102213191506)->field1;
    GibCursor x102413211508 =
              ((GibIntGibIntGibCursorProd *) arg102213191506)->field2;
    GibCursor y102613231510 =  _copy_Map129_v674(x102413211508);
    GibPtr tailift1643 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1643)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1643)->field1 = x102313201507;
    ((GibIntGibIntGibCursorProd *) tailift1643)->field2 = y102613231510;
    return tailift1643;
}
GibCursor _copy_without_ptrs_Clock94(GibCursor arg102713241511)
{
    GibInt tag1644 = ((GibIntGibIntGibCursorProd *) arg102713241511)->field0;
    GibInt x102813251512 =
           ((GibIntGibIntGibCursorProd *) arg102713241511)->field1;
    GibCursor x102913261513 =
              ((GibIntGibIntGibCursorProd *) arg102713241511)->field2;
    GibCursor y103113281515 =  _copy_without_ptrs_Map129_v674(x102913261513);
    GibPtr tailift1645 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1645)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1645)->field1 = x102813251512;
    ((GibIntGibIntGibCursorProd *) tailift1645)->field2 = y103113281515;
    return tailift1645;
}
unsigned char _traverse_Clock94(GibCursor arg103213291516)
{
    GibInt tag1646 = ((GibIntGibIntGibCursorProd *) arg103213291516)->field0;
    GibInt x103313301517 =
           ((GibIntGibIntGibCursorProd *) arg103213291516)->field1;
    GibCursor x103413311518 =
              ((GibIntGibIntGibCursorProd *) arg103213291516)->field2;
    unsigned char y103613321519 =  _traverse_Map129_v674(x103413311518);
    
    return 0;
}
unsigned char _print_Clock94(GibCursor arg103713331520)
{
    GibInt tag1647 = ((GibIntGibIntGibCursorProd *) arg103713331520)->field0;
    GibInt x103813341521 =
           ((GibIntGibIntGibCursorProd *) arg103713331520)->field1;
    GibCursor x103913351522 =
              ((GibIntGibIntGibCursorProd *) arg103713331520)->field2;
    unsigned char wildcard104213361523 = gib_print_symbol(1638);
    unsigned char wildcard104513371524 = gib_print_symbol(1641);
    unsigned char y104013381525 = printf("%ld", x103813341521);
    unsigned char wildcard104413391526 = gib_print_symbol(1641);
    unsigned char y104113401527 =  _print_Map129_v674(x103913351522);
    unsigned char wildcard104313411528 = gib_print_symbol(1630);
    
    return 0;
}
GibCursor _copy_Timestamp95(GibCursor arg104613421529)
{
    GibInt tag1648 = ((GibIntGibIntGibCursorProd *) arg104613421529)->field0;
    GibInt x104713431530 =
           ((GibIntGibIntGibCursorProd *) arg104613421529)->field1;
    GibCursor x104813441531 =
              ((GibIntGibIntGibCursorProd *) arg104613421529)->field2;
    GibCursor y105013461533 =  _copy_Clock94(x104813441531);
    GibPtr tailift1649 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1649)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1649)->field1 = x104713431530;
    ((GibIntGibIntGibCursorProd *) tailift1649)->field2 = y105013461533;
    return tailift1649;
}
GibCursor _copy_without_ptrs_Timestamp95(GibCursor arg105113471534)
{
    GibInt tag1650 = ((GibIntGibIntGibCursorProd *) arg105113471534)->field0;
    GibInt x105213481535 =
           ((GibIntGibIntGibCursorProd *) arg105113471534)->field1;
    GibCursor x105313491536 =
              ((GibIntGibIntGibCursorProd *) arg105113471534)->field2;
    GibCursor y105513511538 =  _copy_without_ptrs_Clock94(x105313491536);
    GibPtr tailift1651 = gib_alloc(sizeof(GibIntGibIntGibCursorProd));
    
    ((GibIntGibIntGibCursorProd *) tailift1651)->field0 = 0;
    ((GibIntGibIntGibCursorProd *) tailift1651)->field1 = x105213481535;
    ((GibIntGibIntGibCursorProd *) tailift1651)->field2 = y105513511538;
    return tailift1651;
}
unsigned char _traverse_Timestamp95(GibCursor arg105613521539)
{
    GibInt tag1652 = ((GibIntGibIntGibCursorProd *) arg105613521539)->field0;
    GibInt x105713531540 =
           ((GibIntGibIntGibCursorProd *) arg105613521539)->field1;
    GibCursor x105813541541 =
              ((GibIntGibIntGibCursorProd *) arg105613521539)->field2;
    unsigned char y106013551542 =  _traverse_Clock94(x105813541541);
    
    return 0;
}
unsigned char _print_Timestamp95(GibCursor arg106113561543)
{
    GibInt tag1653 = ((GibIntGibIntGibCursorProd *) arg106113561543)->field0;
    GibInt x106213571544 =
           ((GibIntGibIntGibCursorProd *) arg106113561543)->field1;
    GibCursor x106313581545 =
              ((GibIntGibIntGibCursorProd *) arg106113561543)->field2;
    unsigned char wildcard106613591546 = gib_print_symbol(1632);
    unsigned char wildcard106913601547 = gib_print_symbol(1641);
    unsigned char y106413611548 = printf("%ld", x106213571544);
    unsigned char wildcard106813621549 = gib_print_symbol(1641);
    unsigned char y106513631550 =  _print_Clock94(x106313581545);
    unsigned char wildcard106713641551 = gib_print_symbol(1630);
    
    return 0;
}
GibCursor _copy_Ord100(GibCursor arg107013651552)
{
    GibPackedTag tag1654 = *(GibPackedTag *) arg107013651552;
    GibCursor tail1655 = arg107013651552 + sizeof(GibInt);
    
    
  switch1660:
    ;
    switch (tag1654) {
        
      case 0:
        {
            GibPtr tailift1656 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1656)->field0 = 0;
            return tailift1656;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1657 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1657)->field0 = 1;
            return tailift1657;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1658 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1658)->field0 = 2;
            return tailift1658;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1659 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1659)->field0 = 3;
            return tailift1659;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1654");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Ord100(GibCursor arg107113661553)
{
    GibPackedTag tag1661 = *(GibPackedTag *) arg107113661553;
    GibCursor tail1662 = arg107113661553 + sizeof(GibInt);
    
    
  switch1667:
    ;
    switch (tag1661) {
        
      case 0:
        {
            GibPtr tailift1663 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1663)->field0 = 0;
            return tailift1663;
            break;
        }
        
      case 1:
        {
            GibPtr tailift1664 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1664)->field0 = 1;
            return tailift1664;
            break;
        }
        
      case 2:
        {
            GibPtr tailift1665 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1665)->field0 = 2;
            return tailift1665;
            break;
        }
        
      case 3:
        {
            GibPtr tailift1666 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1666)->field0 = 3;
            return tailift1666;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1661");
            exit(1);
        }
    }
}
unsigned char _traverse_Ord100(GibCursor arg107213671554)
{
    GibPackedTag tag1668 = *(GibPackedTag *) arg107213671554;
    GibCursor tail1669 = arg107213671554 + sizeof(GibInt);
    
    
  switch1670:
    ;
    switch (tag1668) {
        
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
            printf("%s\n", "Unknown tag in: tag1668");
            exit(1);
        }
    }
}
unsigned char _print_Ord100(GibCursor arg107313681555)
{
    GibPackedTag tag1671 = *(GibPackedTag *) arg107313681555;
    GibCursor tail1672 = arg107313681555 + sizeof(GibInt);
    
    
  switch1673:
    ;
    switch (tag1671) {
        
      case 0:
        {
            unsigned char wildcard107413691556 = gib_print_symbol(1634);
            unsigned char wildcard107513701557 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      case 1:
        {
            unsigned char wildcard107613711558 = gib_print_symbol(1636);
            unsigned char wildcard107713721559 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      case 2:
        {
            unsigned char wildcard107813731560 = gib_print_symbol(1637);
            unsigned char wildcard107913741561 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      case 3:
        {
            unsigned char wildcard108013751562 = gib_print_symbol(1639);
            unsigned char wildcard108113761563 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1671");
            exit(1);
        }
    }
}
GibCursor _copy_Maybe99_v676(GibCursor arg108213771564)
{
    GibPackedTag tag1674 = *(GibPackedTag *) arg108213771564;
    GibCursor tail1675 = arg108213771564 + sizeof(GibInt);
    
    
  switch1678:
    ;
    switch (tag1674) {
        
      case 0:
        {
            GibPtr tailift1676 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1676)->field0 = 0;
            return tailift1676;
            break;
        }
        
      case 1:
        {
            GibInt x108313781565 = ((GibIntProd *) tail1675)->field0;
            GibPtr tailift1677 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift1677)->field0 = 1;
            ((GibIntGibIntProd *) tailift1677)->field1 = x108313781565;
            return tailift1677;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1674");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Maybe99_v676(GibCursor arg108513801567)
{
    GibPackedTag tag1679 = *(GibPackedTag *) arg108513801567;
    GibCursor tail1680 = arg108513801567 + sizeof(GibInt);
    
    
  switch1683:
    ;
    switch (tag1679) {
        
      case 0:
        {
            GibPtr tailift1681 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1681)->field0 = 0;
            return tailift1681;
            break;
        }
        
      case 1:
        {
            GibInt x108613811568 = ((GibIntProd *) tail1680)->field0;
            GibPtr tailift1682 = gib_alloc(sizeof(GibIntGibIntProd));
            
            ((GibIntGibIntProd *) tailift1682)->field0 = 1;
            ((GibIntGibIntProd *) tailift1682)->field1 = x108613811568;
            return tailift1682;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1679");
            exit(1);
        }
    }
}
unsigned char _traverse_Maybe99_v676(GibCursor arg108813831570)
{
    GibPackedTag tag1684 = *(GibPackedTag *) arg108813831570;
    GibCursor tail1685 = arg108813831570 + sizeof(GibInt);
    
    
  switch1686:
    ;
    switch (tag1684) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x108913841571 = ((GibIntProd *) tail1685)->field0;
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1684");
            exit(1);
        }
    }
}
unsigned char _print_Maybe99_v676(GibCursor arg109113851572)
{
    GibPackedTag tag1687 = *(GibPackedTag *) arg109113851572;
    GibCursor tail1688 = arg109113851572 + sizeof(GibInt);
    
    
  switch1689:
    ;
    switch (tag1687) {
        
      case 0:
        {
            unsigned char wildcard109213861573 = gib_print_symbol(1633);
            unsigned char wildcard109313871574 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x109413881575 = ((GibIntProd *) tail1688)->field0;
            unsigned char wildcard109613891576 = gib_print_symbol(1635);
            unsigned char wildcard109813901577 = gib_print_symbol(1641);
            unsigned char y109513911578 = printf("%ld", x109413881575);
            unsigned char wildcard109713921579 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1687");
            exit(1);
        }
    }
}
GibCursor _copy_Map129_v674(GibCursor arg109913931580)
{
    GibPackedTag tag1690 = *(GibPackedTag *) arg109913931580;
    GibCursor tail1691 = arg109913931580 + sizeof(GibInt);
    
    
  switch1694:
    ;
    switch (tag1690) {
        
      case 0:
        {
            GibPtr tailift1692 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1692)->field0 = 0;
            return tailift1692;
            break;
        }
        
      case 1:
        {
            GibInt x110013941581 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1691)->field0;
            GibInt x110113951582 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1691)->field1;
            GibInt x110213961583 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1691)->field2;
            GibCursor x110313971584 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1691)->field3;
            GibCursor x110413981585 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1691)->field4;
            GibCursor y110814021589 =  _copy_Map129_v674(x110313971584);
            GibCursor y110914031590 =  _copy_Map129_v674(x110413981585);
            GibPtr tailift1693 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1693)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1693)->field1 =
                x110013941581;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1693)->field2 =
                x110113951582;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1693)->field3 =
                x110213961583;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1693)->field4 =
                y110814021589;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1693)->field5 =
                y110914031590;
            return tailift1693;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1690");
            exit(1);
        }
    }
}
GibCursor _copy_without_ptrs_Map129_v674(GibCursor arg111014041591)
{
    GibPackedTag tag1695 = *(GibPackedTag *) arg111014041591;
    GibCursor tail1696 = arg111014041591 + sizeof(GibInt);
    
    
  switch1699:
    ;
    switch (tag1695) {
        
      case 0:
        {
            GibPtr tailift1697 = gib_alloc(sizeof(GibIntProd));
            
            ((GibIntProd *) tailift1697)->field0 = 0;
            return tailift1697;
            break;
        }
        
      case 1:
        {
            GibInt x111114051592 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1696)->field0;
            GibInt x111214061593 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1696)->field1;
            GibInt x111314071594 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1696)->field2;
            GibCursor x111414081595 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1696)->field3;
            GibCursor x111514091596 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1696)->field4;
            GibCursor y111914131600 =
                       _copy_without_ptrs_Map129_v674(x111414081595);
            GibCursor y112014141601 =
                       _copy_without_ptrs_Map129_v674(x111514091596);
            GibPtr tailift1698 =
                   gib_alloc(sizeof(GibIntGibIntGibIntGibIntGibCursorGibCursorProd));
            
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1698)->field0 =
                1;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1698)->field1 =
                x111114051592;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1698)->field2 =
                x111214061593;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1698)->field3 =
                x111314071594;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1698)->field4 =
                y111914131600;
            ((GibIntGibIntGibIntGibIntGibCursorGibCursorProd *) tailift1698)->field5 =
                y112014141601;
            return tailift1698;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1695");
            exit(1);
        }
    }
}
unsigned char _traverse_Map129_v674(GibCursor arg112114151602)
{
    GibPackedTag tag1700 = *(GibPackedTag *) arg112114151602;
    GibCursor tail1701 = arg112114151602 + sizeof(GibInt);
    
    
  switch1702:
    ;
    switch (tag1700) {
        
      case 0:
        {
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x112214161603 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1701)->field0;
            GibInt x112314171604 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1701)->field1;
            GibInt x112414181605 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1701)->field2;
            GibCursor x112514191606 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1701)->field3;
            GibCursor x112614201607 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1701)->field4;
            unsigned char y113014211608 =  _traverse_Map129_v674(x112514191606);
            unsigned char y113114221609 =  _traverse_Map129_v674(x112614201607);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1700");
            exit(1);
        }
    }
}
unsigned char _print_Map129_v674(GibCursor arg113214231610)
{
    GibPackedTag tag1703 = *(GibPackedTag *) arg113214231610;
    GibCursor tail1704 = arg113214231610 + sizeof(GibInt);
    
    
  switch1705:
    ;
    switch (tag1703) {
        
      case 0:
        {
            unsigned char wildcard113314241611 = gib_print_symbol(1631);
            unsigned char wildcard113414251612 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      case 1:
        {
            GibInt x113514261613 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1704)->field0;
            GibInt x113614271614 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1704)->field1;
            GibInt x113714281615 =
                   ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1704)->field2;
            GibCursor x113814291616 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1704)->field3;
            GibCursor x113914301617 =
                      ((GibIntGibIntGibIntGibCursorGibCursorProd *) tail1704)->field4;
            unsigned char wildcard114514311618 = gib_print_symbol(1640);
            unsigned char wildcard115114321619 = gib_print_symbol(1641);
            unsigned char y114014331620 = printf("%ld", x113514261613);
            unsigned char wildcard115014341621 = gib_print_symbol(1641);
            unsigned char y114114351622 = printf("%ld", x113614271614);
            unsigned char wildcard114914361623 = gib_print_symbol(1641);
            unsigned char y114214371624 = printf("%ld", x113714281615);
            unsigned char wildcard114814381625 = gib_print_symbol(1641);
            unsigned char y114314391626 =  _print_Map129_v674(x113814291616);
            unsigned char wildcard114714401627 = gib_print_symbol(1641);
            unsigned char y114414411628 =  _print_Map129_v674(x113914301617);
            unsigned char wildcard114614421629 = gib_print_symbol(1630);
            
            return 0;
            break;
        }
        
      default:
        {
            printf("%s\n", "Unknown tag in: tag1703");
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