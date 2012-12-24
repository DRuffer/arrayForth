\	File:		$Id: Objects.fth,v 1.2 2003/01/31 23:14:49 druffer Exp $
\
\	Contains:	Object Tables
\
\	Copyright:	© 2003 by Apple Computer, Inc., all rights reserved.
\
\		$Log: Objects.fth,v $
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

\ The number of entries in the Object Table is supplied by the peObjects
\ field in the PE Header.  Entries in the Object Table are numbered
\ starting from one.  The Object Table immediately follows the PE
\ Header.  The code and data memory object entries are in the order
\ choosen by the linker.  The virtual addresses for objects must be
\ assigned by the linker such that they are in ascending order and
\ adjacent, and must be a multiple of Object Align in the PE Header.

40 1 peObjectTable STRUCTURE objHeader	\ holds the Object Table.

0  8 BYTES objObjectName		\ null padded ASCII string,
								\ or / followed by ASCII offset into string table.
      LONG objVirtualSize		\ size allocated when loaded,
								\ difference from Physical is zero filled.
      LONG objVirtualAddr		\ virtual address image is relocated to.
								\ Virtual address space must be dense.
      LONG objPhysicalSize		\ size of initialized data in the image.
								\ Less than or equal to Virtual Size.
      LONG objPhysicalOffset	\ offset from beginning of EXE file.
      LONG objRelocPtr			\ pointer to relocation entries.
      LONG objLinePtr			\ pointer to line number entries.
   NUMERIC objRelocs			\ number of relocation entries.
   NUMERIC objLines				\ number of line number entries.
      LONG objObjectFlags		\ object flags.

CONSTANT objTableSize			\ size of object table.

\ /OBJECTS set offset of number of Object records.

: /OBJECTS ( -- )
   MS-DOS-FILE IF
      peObjects NU@ objHeader LIM !  PEHEADER-PTR L@
      peObjectTable + objHeader ORG !
   THEN ;

' /OBJECTS <FILE

\ .POINTER display the given pointer.

: .POINTER ( a c-addr len n -- )
   CR  14 SPACES  5 U.R  SPACE  TYPE  ."  @ "  H. ;

HEX

IMAGE-CONSTANTS IMAGE_SCN

00000000 IMAGE-CONSTANT IMAGE_SCN_TYPE_REGULAR           ," Regular"
00000001 IMAGE-CONSTANT IMAGE_SCN_TYPE_DUMMY             ," Dummy"
00000002 IMAGE-CONSTANT IMAGE_SCN_TYPE_NO_LOAD           ," No Load"
00000004 IMAGE-CONSTANT IMAGE_SCN_TYPE_GROUPED           ," Grouped"
00000008 IMAGE-CONSTANT IMAGE_SCN_TYPE_NO_PAD            ," No Pad"
00000010 IMAGE-CONSTANT IMAGE_SCN_TYPE_COPY              ," Copy"
00000020 IMAGE-CONSTANT IMAGE_SCN_CNT_CODE               ," Code"
00000040 IMAGE-CONSTANT IMAGE_SCN_CNT_INITIALIZED_DATA   ," Initialized data"
00000080 IMAGE-CONSTANT IMAGE_SCN_CNT_UNINITIALIZED_DAT  ," Uninitialized data"
00000100 IMAGE-CONSTANT IMAGE_SCN_LNK_OTHER              ," Other"
00000200 IMAGE-CONSTANT IMAGE_SCN_LNK_INFO               ," Info"
00000400 IMAGE-CONSTANT IMAGE_SCN_LNK_OVERLAY            ," Overlay"
00000800 IMAGE-CONSTANT IMAGE_SCN_LNK_REMOVE             ," Remove"
00001000 IMAGE-CONSTANT IMAGE_SCN_LNK_COMDAT             ," Comm Data"
02000000 IMAGE-CONSTANT IMAGE_SCN_MEM_DISCARDABLE        ," Discardable"
04000000 IMAGE-CONSTANT IMAGE_SCN_MEM_NOT_CACHED         ," Not Cached"
08000000 IMAGE-CONSTANT IMAGE_SCN_MEM_NOT_PAGED          ," Not Pageable"
10000000 IMAGE-CONSTANT IMAGE_SCN_MEM_SHARED             ," Shared"
20000000 IMAGE-CONSTANT IMAGE_SCN_MEM_EXECUTE            ," Executable"
40000000 IMAGE-CONSTANT IMAGE_SCN_MEM_READ               ," Readable"
80000000 IMAGE-CONSTANT IMAGE_SCN_MEM_WRITE              ," Writeable"

DECIMAL DROP

\ .OBJECT-FLAGS display object flags.

: .OBJECT-FLAGS ( n -- )
   3 SPACES  objObjectFlags L@ IMAGE_SCN TEST-CONSTANTS ;

[R Object Table Entries \ ] CONSTANT OBJECTS-HEADER

\ .OBJECTS display Object Table entries.

: .OBJECTS ( -- )
   peObjects NU@ ?DUP 0= IF
      EXIT
   THEN  OBJECTS-HEADER DUP RPT !  +L  HEADING  0 DO
      objHeader I READ
      I 1+ 3 U.R  ."  is "  objObjectName B?
      ."  Virtual @ "  objVirtualSize L@ objVirtualAddr L@ .MEMORY
      CR  15 SPACES
      ."  Physical @ "  objPhysicalSize L@ objPhysicalOffset L@ .MEMORY
      objRelocPtr L@ ?DUP IF
         S" Relocations" objRelocs NU@ .POINTER
      THEN  objLinePtr L@ ?DUP IF
         S" Line Numbers" objLines NU@ .POINTER
      THEN  CR  .OBJECT-FLAGS  +L
   LOOP ;
