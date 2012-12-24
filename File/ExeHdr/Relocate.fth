\	File:		$Id: Relocate.fth,v 1.2 2003/01/31 23:14:49 druffer Exp $
\
\	Contains:	Relocation Table
\
\	Copyright:	© 2002 by Apple Computer, Inc., all rights reserved.
\
\		$Log: Relocate.fth,v $
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

\ The Fix-Up Table contains entries for all fixups in the image.  The
\ Total Fix-Up Data Size in the Optional Header is the number of bytes in
\ the fixup table. The fixup table is broken into blocks of fixups. Each
\ block represents the fixups for a 4K page. Each block must start on a
\ 32-bit boundary.

\ Fixups that are resolved by the linker do not need to be processed by
\ the loader, unless the load image can't be loaded at the Image Base
\ specified in the PE Header.

4 1024 * 2* 99 0 STRUCTURE relFixupData		\ holds Fixup Data Blocks.


0    LONG relPageRVA		\ offset into image of this page of fixups.
     LONG relBlockSize		\ total number of bytes in Fixup Block.
0 NUMERIC relType/Offset	\ type in upper 4 bits/Offset within page.
   1024 4 * COPIES
2DROP

\ /FIXUPS setup offset of Fixup blocks.

: /FIXUPS ( -- )
   MS-DOS-FILE IF
      peRelocate L@ ?DUP IF
         FILE-RVA> relFixupData ORG !  8 B/R !
   THEN THEN ;

' /FIXUPS <FILE

IMAGE-CONSTANTS IMAGE_REL		\ describes the relocation types.

0 IMAGE-CONSTANT IMAGE_REL_BASED_ABSOLUTE     ," Skip @ "
1 IMAGE-CONSTANT IMAGE_REL_BASED_HIGH         ," High @ "
2 IMAGE-CONSTANT IMAGE_REL_BASED_LOW          ," Low  @ "
3 IMAGE-CONSTANT IMAGE_REL_BASED_HIGHLOW      ," Pair @ "
4 IMAGE-CONSTANT IMAGE_REL_BASED_HIGHADJ      ," Full @ "
5 IMAGE-CONSTANT IMAGE_REL_BASED_MIPS_JMPADDR ," Jump @ "

DROP

\ .FIXUP display Relocation entry.

: .FIXUP ( flag1 offset type -- flag2 )
   DUP >R  IMAGE_REL COMPARE-CONSTANTS  relPageRVA L@ + H.
   R> IMAGE_REL_BASED_HIGHLOW = IF  IF
         ." hi  " 0
      ELSE  ." lo  " -1
   THEN  ELSE  4 SPACES
   THEN ;

[R Relocation Table Entries \ ] CONSTANT FIXUPS-HEADER

\ .FIXUPS display Relocation Entries.

: .FIXUPS ( -- )
   FIXUPS-HEADER RPT !  peRelocateSize L@ ?DUP IF
      RPT @ HEADING  /FIXUPS  relFixupData 0 READ  0 DO
         -1  relBlockSize L@ DUP relFixupData B/R ! 2/ 4 DO
         relFixupData I INDEX  relType/Offset NU@
            0 4096 UM/MOD .FIXUP
         LOOP  DROP relBlockSize L@ DUP relFixupData ORG +!
      +LOOP  CR
   THEN ;
