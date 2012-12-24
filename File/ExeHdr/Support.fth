\	File:		$Id: Support.fth,v 1.2 2003/01/31 23:14:49 druffer Exp $
\
\	Contains:	Portable Executable File Support
\
\	Copyright:	© 2002 by Apple Computer, Inc., all rights reserved.
\
\		$Log: Support.fth,v $
\		Revision 1.2  2003/01/31 23:14:49  druffer
\		Add file support to repository
\		

\ Constants and Variables

              8 CONSTANT MAXIMAGES	\ max number of object images.
      64 1024 * CONSTANT MAXIMAGE	\ max size of object images.
    8 60 * 60 * CONSTANT GMT		\ Greenwich Mean Time.
   60 60 * 24 * CONSTANT SEC/DAY	\ seconds per day.
 1 1 1970 D-M-Y CONSTANT FIRST-DAY	\ first Julian day.

FILE PEFILE.EXE		\ holder for structured file bindings.

\ Display support

\ ZCOUNT  measures an ASCIIZ string, and returns an address and length,
\ suitable for TYPE .

: ZCOUNT ( zaddr -- addr n )
   DUP DUP BEGIN COUNT 0= UNTIL 1- SWAP - ;

\ .ASCIZ display an ASCIZ string.

: .ASCIZ ( a -- )   ZCOUNT TYPE ;

\ UCOUNT get Unicode string count.

: UCOUNT ( a -- a n )   DUP 2 +  SWAP 2 nC@ ;

\ UTYPE type Unicode string.

: UTYPE ( a n -- )   ?DUP IF  0 DO  UCOUNT EMIT  LOOP  THEN  DROP ;

\ "h" display an "h" to indicate a hex number.

: "h" ( -- )   ." h" ;

\ H. display the given number, unsigned, in HEX base.

: H. ( n -- )   BASE @  HEX  SWAP 1 U.R "h"  SPACE  BASE ! ;

: E.R ( ea n -- )
   >R  0 <#
      #S 2DROP  [char] : HOLD 0 #S
   #>  R> OVER - SPACES  TYPE ;

\ .ADDRESS display an address.

: .ADDRESS ( a -- )   ." At "  BASE @ >R  HEX  5 U.R "h"  R> BASE ! ;

\ .MAJOR/MINOR display give version numbers.

: .MAJOR/MINOR ( major minor -- )
   SWAP   DUP 65535 = IF
      ." ?"  DROP
   ELSE  0 .R
   THEN  ." ."  DUP 65535 = IF
      ." ?"  DROP
   ELSE  0 .R
   THEN  SPACE ;

\ .TIME/DATE display time and date.

: .TIME/DATE ( n -- )
   GMT -  SEC/DAY /MOD  FIRST-DAY + .DATE  .TIME ;

\ TIME/DATE returns current time and date.

: TIME/DATE ( -- n )
   @DATE FIRST-DAY -  SEC/DAY *
   @TIME + GMT + ;

\ Constant list support

\ IMAGE-CONSTANTS create list of constants.

: IMAGE-CONSTANTS ( -- addr ) \ Usage: IMAGE-CONSTANTS <name>
   CREATE
      HERE 0 , ;

\ IMAGE-CONSTANT create linked image constant, its description follows.

: IMAGE-CONSTANT ( addr num -- addr ) \ Usage: IMAGE-CONSTANT <name>
   CREATE
      ,  DUP <LINK
   DOES> ( -- num )
      @ ;

\ COMPARE-CONSTANTS display description if equal.

: COMPARE-CONSTANTS ( num addr -- )
   BEGIN
      @ ?DUP WHILE
         2DUP CELL- @ = IF
            CELL+ COUNT TYPE  DROP EXIT
   THEN  REPEAT  ." Type = "  H. ;

: DISPLAY-CONSTANTS ( num addr -- flag num' )
   0 ROT ROT  BEGIN                   \ Assume no bits displayed
      @ ?DUP WHILE                    \ While entries in list
         2DUP CELL- @ AND ?DUP IF     \ If bit is set
            -1 XOR ROT AND SWAP       \ Remove bits from number
            ROT IF                    \ If we've displayed bits
               ." , "                 \ Seperate with a comma
            THEN  -1  OVER CELL+      \ Get size of description
            COUNT TYPE  ROT ROT       \ Display description
   THEN  REPEAT ;

\ TEST-CONSTANTS display description if bit is on.

: TEST-CONSTANTS ( num addr -- )
   ." Flags = " OVER 0= IF         \ If no bits are on
      ." Regular."  CR  EXIT       \ Describe it as regular
   THEN  DISPLAY-CONSTANTS ?DUP IF \ If any bits remaining
      ." Undefined:" H. 1+         \ Display them
   THEN  IF
      ." . "                       \ Closing period
   THEN  CR ;

\ File dump routines

\ DUMP-BUFFER holds bytes from the file being dumped.

CREATE DUMP-BUFFER   16 ALLOT

: /U.R ( b n -- )   7 AND  0> IF 3 ELSE 4 THEN  U.R ;

: .T ( a -- )
   HEX  CR  ."    Offset"  16 BOUNDS 2DUP DO
      I 15 AND  I /U.R
   LOOP  2 SPACES  DO
      I 15 AND  1 U.R
   LOOP ;

\ DUMP-HEADER display dump header.

: DUMP-HEADER ( a n -- a n )   OVER .T ;

[R+ DUMP-HEADER File dump \ ] CONSTANT FILE-DUMP-HEADER

\ FILE-DUMP display bytes from file in ASCII format.

: FILE-DUMP ( a n -- )
   BASE @ >R  FILE-DUMP-HEADER LAYOUT  OVER 0 FILE-HANDLE @
   REPOSITION-FILE THROW  BOUNDS OVER -ROT DO
      I 9 U.R  I 16 +  2DUP -  0< IF
         DROP DUP
      THEN  I - DUMP-BUFFER OVER FILE-HANDLE @
      READ-FILE THROW ?DUP IF
         0 DO
            DUMP-BUFFER I + C@ I J + /U.R
         1 +LOOP
      ELSE  0 HERE !  1 ABORT" end"
      THEN  2 SPACES  DUMP-BUFFER SWAP TYPE  CR
   16 +LOOP  DROP  R> BASE ! ;
