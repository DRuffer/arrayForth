\	File:		$Id: DosHdr.fth,v 1.3 2005/03/31 00:23:17 druffer Exp $
\
\	Contains:	MS-DOS 2.0 Header Structure
\
\	Copyright:	© 2003-2005 by Apple Computer, Inc., all rights reserved.
\
\		$Log: DosHdr.fth,v $
\		Revision 1.3  2005/03/31 00:23:17  druffer
\		Use B@ (S.) where the equivalent is being done
\		
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

64 1 0 STRUCTURE exeHeader	\ holds MS-DOS 2.0 relocatable program header.

0  2 BYTES exSignature	\ .EXE file signature, equal to the letters "MZ".
   NUMERIC exExtraBytes	\ number of bytes in last page of the file.
   NUMERIC exPages		\ number of 512 byte pages in the file.
   NUMERIC exRelocItems	\ number of pointers in the relocation table.
   NUMERIC exHeaderSize	\ number of 16 byte paragraphs in file header.
   NUMERIC exMinAlloc	\ minimum extra paragraphs required by program.
   NUMERIC exMaxAlloc	\ maximum extra paragraphs requested by program.
   NUMERIC exInitSS		\ initial relocatable value of the SS register.
   NUMERIC exInitSP		\ initial value of the SP register.
   NUMERIC exCheckSum	\ one's complement of sum of all 16-bit values.
   NUMERIC exInitIP		\ initial value of the IP register.
   NUMERIC exInitCS		\ initial relocatable value of the CS register.
   NUMERIC exRelocTable	\ offset in file to relocation table.
   NUMERIC exOverlay	\ zero if file contains main program.
DROP

60 LONG PEHEADER-PTR DROP	\ pointer to Portable Executable header.

[R MS-DOS 2.0 Relocatable Header: \ ] CONSTANT MS-DOS-HEADER

: .EXEHEADER ( -- )		\ display EXE header.
   MS-DOS-HEADER HEADING  exeHeader
   ." Signature = "  exSignature B?
   ."   Checksum = "  exCheckSum NU@ H.
   ."   Overlay = "  exOverlay NU?  CR
   ." Size of file = "  exPages NU@ 512 *  exExtraBytes NU@ + H.
   ."   Header = "  exHeaderSize NU@ 16 * H.  CR
   ." Relocation items = "  exRelocItems NU?
   ." at file offset " exRelocTable NU@ H.  BASE @ HEX  CR
   ." Initial Stack Pointer = "  exInitSS NU@ exInitSP NU@ 7 E.R "h"  CR
   ." Program entry point  = "  exInitCS NU@ exInitIP NU@ 7 E.R "h"  CR
   ." Minimum allocation = "  BASE !  exMinAlloc NU@ 16 * H.
   ."   Maximum = "  exMaxAlloc NU@ 16 * H.  CR CR ;

: MS-DOS-FILE ( -- ? )   exSignature 2DUP B@ (S.) " MZ" COMPARE 0= ;

\ /EXEHEADER size the EXE header, position the PE header pointer as the
\ last cell.

: /EXEHEADER ( -- )
   MS-DOS-FILE IF
      exHeaderSize NU@ 16 *  DUP exeHeader B/R !
      CELL- ['] PEHEADER-PTR >BODY !
   THEN ;

' /EXEHEADER <FILE

\ exeImage holds the MSDOS executable image.  This is usually just a stub
\ that tells the user that it doesn't run.

MAXIMAGE 1 64 STRUCTURE exeImage

\ /EXEIMAGE the exeImage starts right after the exeHeader and extends up
\ to the peHeader.

: /EXEIMAGE ( -- )
   MS-DOS-FILE IF
      PEHEADER-PTR L@  exHeaderSize NU@ 16 *  exeImage
      DUP ORG !  - B/R !
   THEN ;

' /EXEIMAGE <FILE

