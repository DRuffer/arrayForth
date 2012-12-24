\ Customers.fth - Customers example for the Database Package

\ Copyright (c) 2009 FORTH, Inc.  Portions contributed by Dennis Ruffer.

\ Permission is hereby granted, free of charge, to any person obtaining a copy
\ of this software and associated documentation files (the "Software"), to deal
\ in the Software without restriction, including without limitation the rights
\ to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
\ copies of the Software, and to permit persons to whom the Software is
\ furnished to do so, subject to the following conditions:

\ The above copyright notice and this permission notice shall be included in
\ all copies or substantial portions of the Software.

\ THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
\ IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
\ FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
\ AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
\ LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
\ OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
\ THE SOFTWARE.

INCLUDE ../../File.fth  files  TRUE REVERSE !  LITTLE-ENDIAN

FILE Customers.dbf   FILE= Customers.dbf

70 500       0 BLOCK-DATA CUSTOMERS
46 500 +ORIGIN BLOCK-DATA SERIALS

( Customers and Serial Numbers)
( CUSTOMERS records:)
0  4 FILLER LINK  ( LINK to 1st serial#)
   20 BYTES COMPANY
   16 BYTES CONTACT
   30 BYTES STREET 
DROP

( SERIALS records:)
0  4 FILLER LINK  ( LINK to next serial#)
   10 BYTES SERIAL#
    NUMERIC PRODUCT  ( product code)
    NUMERIC OWNER  ( link to owner CUSTOMERS record)
DROP

( Customer/serial number file)
: edit   CR  ." Company name? "  COMPANY ASK
   CR  ." Contact? "  CONTACT ASK
   CR  ." Address? "  STREET ASK ;
: new   CUSTOMERS  SLOT DUP . READ  edit ; 

TRUE CONSTANT ALT \ Another approach

ALT [IF]
: VALID ( n - t)   0 OVER < DUP  IF
   SWAP READ  ELSE  SWAP DROP  THEN ;
: FIRST ( - t)   HEAD @ VALID ;
: NEXT ( - t)   LINK L@ VALID ;
: -LOCATE ( n - t)   FIRST IF BEGIN  DUP WHILE
      1-  NEXT 0= IF  DROP -1 EXIT  THEN
   REPEAT  ELSE DROP -1 THEN ;
[THEN]

( Assumes a serial# chain linked thru HEAD)
: (add)   CR  ." Serial# ? "   SERIAL# ASK ;

ALT [IF]
: add   LINK L@ HEAD !  SAVE  SERIALS
   FIRST IF  -1 CHAIN  ELSE ( no chain)
      SLOT DUP RESTORE  LINK L!  SAVE
      SERIALS READ  THEN
   (add)  RESTORE ;
[ELSE]
: add   SAVE  LINK L@ DUP 0< IF  ( empty chain) DROP
         SAVE  SERIALS SLOT  RESTORE  
         DUP LINK L!  SERIALS READ
      ELSE  HEAD !  SERIALS -1 CHAIN ( add at end)
  THEN (add)  RESTORE ;
[THEN]

: edit ( n)   SAVE SERIALS  1- -LOCATE ABORT" Can't"
   CR  Serial# B?  (add)  RESTORE ;

( Customer/serial number display)
: .company   COMPANY B?  CONTACT B?  STREET B? ;  

: .companies   CUSTOMERS  RECORDS DO
     CR  I .  I READ  .company  LOOP ;

ALT [IF]
: all-serials   LINK L@ HEAD !  SAVE  0
   SERIALS  FIRST BEGIN  WHILE  CR  1+ DUP .
      SERIAL# B?  NEXT REPEAT  RESTORE  DROP ;
[THEN]
: .serials   0  SERIALS  FIRST BEGIN  
  CR 1+ DUP .  Serial# B?  -NEXT UNTIL  DROP ;
: all-serials   LINK L@  0>  IF
   LINK L@  HEAD !  SAVE  .serials  RESTORE  THEN ;
[THEN]

: show ( n)   CUSTOMERS READ  CR .company  all-serials ;

\ Usage:
\ To enter a new customer:  new  then  add  as needed.
\ To edit an old one:  n show  then  add  or  n edit .
