\	File:		$Id: Images.fth,v 1.3 2005/03/24 00:12:46 druffer Exp $
\
\	Contains:	Image Pages
\
\	Copyright:	© 2003-2005 by Apple Computer, Inc., all rights reserved.
\
\		$Log: Images.fth,v $
\		Revision 1.3  2005/03/24 00:12:46  druffer
\		Change D# to DB# to prevent conflicts
\		Use SET-DATA for DB# !
\		
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

\ The Image Pages section contains all initialized data for all objects.
\ The seek offset for the first page in each object is specified in the
\ Object Table and is aligned on a File Align boundary.  The objects are
\ ordered by the RVA.  Every object begins on a multiple of Object Align.

CREATE IMAGES   MAXIMAGES CELLS ALLOT	\ array of pointers to IMAGE structures.

\ MAKE-IMAGES define IMAGE structures.

: MAKE-IMAGES ( -- )
   MAXIMAGES 0 DO
      MAXIMAGE 1 0  S" STRUCTURE IMAGE" EVALUATE
      DB# @ IMAGES I CELLS + !
   LOOP ;

MAKE-IMAGES

\ /IMAGES setup size and offset of IMAGE structures.

: /IMAGES ( -- )
   MS-DOS-FILE IF
      peObjects NU@ ?DUP 0= IF
         EXIT
      THEN  0  OVER MAXIMAGES > IF
         ."  Need "  NIP MAXIMAGES - .
         0 HERE !  1 ABORT" more images!"
      THEN  DO
         objHeader I READ
         objPhysicalOffset L@  objPhysicalSize L@  IMAGES I CELLS +
         @ ?DUP IF SET-DATA THEN  B/R !  ORG !
      LOOP
   THEN ;

' /IMAGES <FILE

HEX

\ RVA> convert RVA to address in object images.

: RVA> ( a -- a' )
   peObjects NU@ ?DUP 0= IF
      EXIT
   THEN  0 DO
      objHeader I READ
      DUP objVirtualAddr L@ objVirtualSize L@
      objPhysicalSize L@ MAX  BOUNDS SWAP WITHIN IF
         objVirtualAddr L@ -  DUP objPhysicalSize L@ >
         objObjectFlags L@ 80 AND  OR ABORT" Uninitialized data"
         IMAGES I CELLS + @ ?DUP IF SET-DATA THEN  0 RECORD +  UNLOOP  EXIT
   THEN  LOOP  0 HERE !  1 ABORT" Invalid address" ;

\ FILE-RVA> convert RVA to address in file.

: FILE-RVA> ( a -- a' )
   peObjects NU@ ?DUP 0= IF
      EXIT
   THEN  0 DO
      objHeader I READ
      DUP objVirtualAddr L@ objVirtualSize L@
      objPhysicalSize L@ MAX  BOUNDS SWAP WITHIN IF
         objVirtualAddr L@ -  DUP objPhysicalSize L@ >
         objObjectFlags L@ 80 AND  OR ABORT" Uninitialized data"
         objPhysicalOffset L@ +  UNLOOP  EXIT
   THEN  LOOP  0 HERE !  1 ABORT" Invalid address" ;

DECIMAL
