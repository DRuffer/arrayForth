\ Sort.fth - Database sorting

\ Copyright (c) 2009 Dennis Ruffer

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

[defined] Sort.version 0= [IF]

forth definitions 2 constant Sort.version files

\ Multiple field sorts can be accomplished by sorting the least
\ significant field first, progressing up to the most significant one
\ last.  The intermediate order will be maintained each time.

\ Dynamic sorting file allocation

\ TEMP.DBF is a temporary file that is created while sorting and
\ destroyed afterwards.  The first 256 entries contain heads to chained
\ records for each "bin".  The other entries contain a link to the next
\ record in the chain and the temporary holding value of the sorted
\ index.

FILE TEMP.DBF

( Bytes  records  origin             name )
      1        1       0  BLOCK-DATA TEMP

CELL  1 BYTES TMP  DROP

\ INIT initialize the temporary sort file.  This is done at the begining
\ of each sort pass.  The links need a marker for the end of the chain
\ so the file is initialized to 0's with the first 256 records already
\ used.

: INIT ( -- )
   CR  ." Initialize file "  SAVE  TEMP.DBF  TEMP INITIALIZE
   256 AVAILABLE 4 nC!  TOUCH  RESTORE ;

\ SET-UP creates the temporary sort file with the correct number of
\ records and fields to handle the current file.

: SET-UP ( -- )
   CR  ." Building sort file for "  FILE-NAME COUNT TYPE
   B/R @  ['] TMP >BODY FLD# !  FIELD-SIZE !  LIM @ 256 +  B/R @ CELL+
   SAVE  TEMP.DBF  s" TEMP.DBF" >FILE  TEMP
   DUP 1024 OVER / * B/B !  B/R !  LIM !
   RESTORE ;

\ TAKE-DOWN removes the temporary file.

: TAKE-DOWN ( -- )
   CR  ." Deleting sort file "  SAVE
   TEMP.DBF  -FILE  DESTROY-FILE DROP
   RESTORE ;

\ Divide and Konker Sort

\ The Divide and Konker sort was submitted as a reply to FIG's Contest
\ of Sorts in Forth Demensions Sept/Oct 1989 by:
\   Dwight K. Elvey
\   1300 Warren Drive
\   Santa Cruz, CA  95060

\ 'ORDER holds the execution vector for the current sort order.

VARIABLE 'ORDER

\ ORDERS tanslates a byte into its proper "bin".

: ORDERS ( b -- n )   'ORDER @ ?DUP IF EXECUTE THEN ;

\ ASCENDING is the default sort order with A before Z.

: ASCENDING ( -- )   ['] 1+ 'ORDER ! ;  ASCENDING

\ DESCENDING an alternate sort order with Z before A.

: (DESCEND) ( b -- n )   256 SWAP - ;
: DESCENDING ( -- )   ['] (DESCEND) 'ORDER ! ;

\ REORDER starts at the top of each "bin", and pulls records out of the
\ TEMP file and laying them back down into the original file.

: REORDER ( -- )
   ." re-indexing "  1  257 1 DO
      I HEAD !  BEGIN
         SAVE  TEMP.DBF  TEMP  FIRST  -NEXT 0= WHILE
            R# @ HEAD !  TMP B@  RESTORE
            DUP READ  ENTIRE B!  1+
   REPEAT  RESTORE  LOOP  DROP ;

\ ORDER starts at the back of the index file, puts each record into its
\ appropriate "bin".  This way, when they are pulled back out, they will
\ maintain the original sequence within each "bin".

: ORDER ( abyte -- )
   ." sorting "  RECORDS DO
      AVAILABLE 4 nC@ 1+ I - READ
      DUP 1@  ORDERS HEAD !  ENTIRE B@  SAVE
      TEMP.DBF  TEMP  0 CHAIN  TMP B!  RESTORE
   LOOP  DROP ;

\ Field Sorting

\ BSWAP adjusts the given address to reference BYTES field characters on
\ byte swapped machines.

: BSWAP ( a n -- a' )   1 AND 2* 1- + ;

\ SORT given the address of a numeric field and its number of bytes, or
\ a BYTES field, which contains its size and 0, the field will be sorted
\ least significant byte to most significant byte.

: SORT ( a n | n a 0 -- )
   FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
   SET-UP  ?DUP IF
      0 DO
         INIT  LITTLE-ENDIAN? IF  DUP I +
         ELSE  DUP I' + 1- I -
         THEN  ORDER  REORDER
      LOOP
   ELSE
      OVER 0 DO
         INIT  2DUP SWAP I - 1- +  I BSWAP ORDER  REORDER
      LOOP  DROP
   THEN  DROP  R> IF
      -FILE
   THEN  TAKE-DOWN ;

\ SORTS defines sorting words for each class of field within the FILE
\ package.

: SORTS ( n -- ) \ Usage: SORTS <name>
   CREATE
      ,
   DOES> ( a | n a -- )
      STATE @ IF
         POSTPONE LITERAL  @ ,  POSTPONE SORT
      ELSE  @ SORT  THEN ;

0 SORTS BSORT   \ sorts a BYTES field.
1 SORTS 1SORT   \ sorts a 1BYTE field.
2 SORTS NSORT   \ sorts a NUMERIC field.
4 SORTS LSORT   \ sorts a LONG field.
8 SORTS DSORT   \ sorts a DOUBLE field.
[THEN]
