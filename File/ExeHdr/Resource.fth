\	File:		$Id: Resource.fth,v 1.2 2003/01/31 23:14:49 druffer Exp $
\
\	Contains:	Resource Directory Table
\
\	Copyright:	© 2003 by Apple Computer, Inc., all rights reserved.
\
\		$Log: Resource.fth,v $
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

16 1 0 STRUCTURE resDirectory	\ holds Resource Directory Table.

0    LONG resFlags			\ resource Flags, currently 0.
     LONG resTime/Date		\ time resource was created.
  NUMERIC resMajorVersion	\ major version number.
  NUMERIC resMinorVersion	\ minor version mumber.
  NUMERIC resNameEntries	\ number of string entries.
  NUMERIC resIDEntries		\ number of numeric entries.

DROP

8 99 0 STRUCTURE resEntry	\ holds Resource Directory Entries.

0    LONG resNameRVA		\ address of string identifier.
0    LONG resIntegerID		\ integer identifier.
     LONG resEntryRVA		\ high bit 0 = Address of Resource Data Entry,
							\ high bit 1 = Address of Resource Directory.

2DROP

16 1 0 STRUCTURE resData	\ holds Resource Data Entry.

0    LONG resDataRVA		\ address of the Resource Data.
     LONG resSize			\ size of the Resource Data.
     LONG resCodePage		\ code Page, typically Unicode.
     LONG resReserved		\ must be 0.

DROP

HEX

\ .RESOURCE-ENTRY display current Resource Table.

: .RESOURCE-ENTRY ( -- )
   ." Flags: " resFlags L?  resTime/Date L@ .TIME/DATE
   resMajorVersion NU@ resMinorVersion NU@ .MAJOR/MINOR
   resDirectory ORG @ B/R @ +
   resNameEntries NU@ DUP resIDEntries NU@ + 0 DO
      OVER resEntry ORG !
      resEntry I DUP READ  OVER < IF
         ." Name: " resNameRVA L@ 7FFFFFFF AND peResource L@ +
         RVA> UCOUNT UTYPE  CR
      ELSE  ." ID: " resIntegerID L@ H.  CR
      THEN  resEntryRVA L@ DUP 0< IF
         7FFFFFFF AND peResource L@ + FILE-RVA>
         resDirectory ORG !  RECURSE
      ELSE  peResource L@ + FILE-RVA> resData ORG !
         resDataRVA L@ H. ( FILE-RVA> ) resSize L@ H. ( FILE-DUMP )  CR CR
   THEN  LOOP  2DROP ;

DECIMAL

[R Resource Table Entries \ ] CONSTANT RESOURCES-HEADER

\ .RESOURCES display Resource Directory Table.

: .RESOURCES ( -- )
   RESOURCES-HEADER RPT !  peResourceSize L@ IF
      RPT @ HEADING  peResource L@ FILE-RVA> resDirectory ORG !
      resDirectory 0 READ  .RESOURCE-ENTRY
   THEN ;

