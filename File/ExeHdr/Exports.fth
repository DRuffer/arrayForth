\	File:		$Id: Exports.fth,v 1.2 2003/01/31 23:14:49 druffer Exp $
\
\	Contains:	Export Directory Table
\
\	Copyright:	© 2003 by Apple Computer, Inc., all rights reserved.
\
\		$Log: Exports.fth,v $
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

\ The export information begins with the Export Directory Table, which
\ describes the remainder of the export information. The Export Directory
\ Table contains address information that is used to resolve fix-up
\ references to the entry points within this image.

40 1 0 STRUCTURE expDirectory	\ holds the Export Directory Table.

0    LONG expFlags			\ flags currently set to zero.
     LONG expTime/Date		\ time and date export was created.
  NUMERIC expMajorVersion	\ major version number of DLL referenced.
  NUMERIC expMinorVersion	\ minor version number of DLL referenced.
     LONG expNameAddr		\ RVA of DLL's ASCII Name.
     LONG expOrdinalBase	\ starting ordinal number for these exports.
     LONG expAddrEntries	\ number of entries in Export Address Table.
     LONG expNameEntries	\ number of entries in Name Pointer Table.
     LONG expAddress		\ RVA of start of Export Address Table.
     LONG expNamePointers	\ RVA of start of Name Pointer Table.
     LONG expOrdinals		\ RVA of start of Ordinal Table.

CONSTANT expDirectorySize

\ /EXPORTS setup offset of export directory.

: /EXPORTS ( -- )
   MS-DOS-FILE IF
      peExport L@ ?DUP IF
         FILE-RVA> expDirectory ORG !
   THEN THEN ;

' /EXPORTS <FILE

\ .EXPORT-NAME display export directory name.

: .EXPORT-NAME ( -- )
   expDirectory 0 READ  expNameAddr L@ RVA> .ASCIZ
   ."  Created: " expTime/Date L@ .TIME/DATE
   ."  Version: " expMajorVersion NU@ expMinorVersion NU@ .MAJOR/MINOR
   ."  Flags: " expFlags L?  CR
   expNameEntries L@ 0 OVER expAddrEntries L@ - ?DUP IF
      ." ENTRIES MISMATCHED BY " .  CR
   THEN ;

\ .EXPORTED display export information.

: .EXPORTED ( num -- )
   expOrdinals L@ RVA> OVER 2* + 2 nC@ DUP
   expOrdinalBase L@ + 10 U.R  SPACE
   expAddress L@ RVA> SWAP CELLS + @ DUP
   peExport L@ DUP peExportSize L@ + WITHIN IF
      ." Forward to: " RVA> .ASCIZ ."  - "
   ELSE  .ADDRESS  SPACE
   THEN  expNamePointers L@ RVA> SWAP CELLS + @ RVA> .ASCIZ  CR ;

[R Export Table Entries \ ] CONSTANT EXPORTS-HEADER

\ .EXPORTS display Export Entries.

: .EXPORTS ( -- )
   EXPORTS-HEADER RPT !  peExportSize L@ IF
      RPT @ HEADING  .EXPORT-NAME  DO
         I .EXPORTED
   LOOP  CR  THEN ;

