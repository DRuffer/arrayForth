\	File:		$Id: Imports.fth,v 1.2 2003/01/31 23:14:49 druffer Exp $
\
\	Contains:	Import Directory Table
\
\	Copyright:	© 2003 by Apple Computer, Inc., all rights reserved.
\
\		$Log: Imports.fth,v $
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

\ The import information begins with the Import Directory Table which
\ describes the remainder of the import information.  The Import
\ Directory Table contains address information that is used to resolve
\ Relocate references to the entry points within a DLL image.  The Import
\ Directory Table consists of an array of Import Directory Entries, one
\ entry for each DLL this image references.  The last directory entry is
\ empty (Null) which indicates the end of the directory table.

20 99 0 STRUCTURE impDirectory	\ holds the Import Directory Table.

0     LONG impLookup			\ RVA of Import Lookup Table.
      LONG impTime/Date			\ is 0 until DLL has been bound.
DUP   LONG impForwarder DROP	\ new field replaces following two?
   NUMERIC impMajorVersion		\ major version number of DLL referenced.
   NUMERIC impMinorVersion		\ minor version number of DLL referenced.
      LONG impNameAddr			\ RVA of DLL's ASCII Name.
      LONG impAddress			\ RVA of start of import addresses.

CONSTANT impDirectorySize

\ /IMPORTS setup offset of import directory.

: /IMPORTS ( -- )
   MS-DOS-FILE IF
      peImport L@ FILE-RVA> impDirectory ORG !
   THEN ;

' /IMPORTS <FILE

\ If the import is by ordinal, the msb is on and it is an ordinal
\ number.  Otherwise, it points to the RVA of the Hint-Name Table. The
\ hint value is used to index the Export Name Table Pointers array,
\ allowing faster by-name imports.  If the hint is incorrect, then a
\ binary search is performed on the Export Name Pointer Table. The string
\ is case sensitive and is terminated by a null byte followed by another
\ byte if needed to align the next entry on an even boundry.

HEX

\ .ThisImport display an import table entry.

: .ThisImport ( a -- )
   DUP 0< IF
      7FFFFFFF AND  ."  Ordinal # "  .
   ELSE  BASE @ >R  HEX  RVA> DUP 2 nC@ 6 U.R
      ."  = "  2+ .ASCIZ  R> BASE !
   THEN  CR ;

\ The Import Lookup Table is an array of ordinal or hint/name RVA's for
\ each DLL.  The last entry is empty (Null) which indicates the end of
\ the table.

\ .TheseImports display imports for current DLL.

: .TheseImports ( -- )
   impAddress L@  impLookup L@ DUP IF
      SWAP
   THEN  BEGIN
      SWAP DUP RVA> 4 nC@ ?DUP WHILE
         ROT DUP impTime/Date L@ IF
            RVA> 4 nC@
         THEN  .ADDRESS  CELL+  ROT ROT .ThisImport  CELL+ SWAP
   REPEAT  2DROP  CR ;

[R Import Table Entries \ ] CONSTANT IMPORTS-HEADER

\ .IMPORTS display Import Table Entries.

: .IMPORTS ( -- )
   IMPORTS-HEADER RPT !  peImportSize L@ IF
      RPT @ HEADING  0  BEGIN
         impDirectory DUP READ  impNameAddr L@ ?DUP WHILE
            OVER .  impTime/Date L@ ?DUP IF
               ." Bound on " .TIME/DATE
            ELSE  ."  = Version "
               impMajorVersion NU@ impMinorVersion NU@ .MAJOR/MINOR
            THEN  ." of "  RVA> .ASCIZ  CR CR
            .TheseImports  1+
      REPEAT  DROP
   THEN ;

DECIMAL
