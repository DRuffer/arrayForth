\ Index.fth - Database Ordered Index Support

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

\ Ordered Index Updates

[defined] Index.version 0= [IF]

forth definitions 2 constant Index.version files

\ HEAD holds the head of current chain.

#USER CELL +USER HEAD TO #USER

\ RSWAP swaps the record in WORKING with the current record of the
\ current file.

: RSWAP ( n a ra -- n a )
   >R 2DUP R>  ROT 2/ 0 DO
      OVER 2 nC@  OVER 2 nC@  2OVER  >R 2 nC!  R> 2 nC!
      SWAP 2 +  SWAP 2 +
   LOOP  2DROP ;

\ DIRECTION ajusts AVAILABLE depending upon the parameter:  1 indicates
\ an insertion, -1 indicates a deletion.  It then returns the parameters
\ for the loop that will update the index.

: DIRECTION ( n -- n a rh rl )
   AVAILABLE 4 nC@ +
   AVAILABLE 4 nC! TOUCH  ENTIRE
   AVAILABLE 4 nC@ 2 +   SAFE R# @ ;

\ +ORDERED inserts the record in WORKING before the record indicated by
\ R# in an ordered index.

: +ORDERED ( -- )
   FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
   1 DIRECTION DO
      I RECORD  RSWAP TOUCH
   LOOP  2DROP  ORDERED RELEASE
   R> IF  -FILE  THEN ;

\ -ORDERED deletes the record to which R# points from an ordered index.

: -ORDERED ( -- )
   FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
   ENTIRE SWAP ERASE  -1 DIRECTION SWAP DO
      I RECORD RSWAP TOUCH
   -1 +LOOP  2DROP  ORDERED RELEASE
   R> IF  -FILE  THEN ;

\ Binary Search

\ The following words perform a binary search of an Ordered Index.

\ -BINARY performs a binary search on an ordered index, given:  BYTES
\ field parameters, length, addr, on the stack.  The field length must
\ be an even number to use -TEXT .  A key in  WORKING  in the field
\ specified.  F#  indicates the desired ordered index file.  -BINARY
\ returns  TRUE  if the record is not found, leaving the record pointer
\ positioned at the record before which the given key might be inserted;
\ it returns 0 otherwise, leaving R# set to the first occurrance of a
\ matching key.

: -BINARY ( n a -- f )
   FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
   SWAP  AVAILABLE 4 nC@ 2/ 1+ DUP READ  ORDERED GET  BEGIN
      DUP 1+ 2/  2OVER OVER ADDRESS  -TEXT 1- IF
         NEGATE
      THEN  R# +!  2/ DUP 0=
   UNTIL  DROP  2DUP OVER ADDRESS  -TEXT 0> ABS  R# +!
   OVER ADDRESS -TEXT  R> IF  -FILE  THEN ;

\ BINARY is like -BINARY , with the difference that, having performed
\ the search, it will abort if a match is not found.  Otherwise, it
\ returns the record number of the main file record associated with the
\ index record found.

: BINARY ( n a -- n )
   -BINARY  ORDERED RELEASE  ABORT" Unknown"
   LINK L@ ;

\ Chained Records

\ The following words support chains of records attached to a 'head'.
\ This facility corresponds roughly to CODASYL "sets".

\ SNATCH takes a field address and record number.  It fetches the record
\ number from that field, replacing it with the record number given.  It
\ is used to update chains.

: SNATCH ( a r -- r )   OVER L@  SWAP ROT L! ;

\ -NEXT reads the next record, assuming the chain is linked through LINK
\ returning 'true' if there is another.

: -NEXT ( -- t )
   LINK L@  DUP 0> IF
      READ 1
   THEN  1- ;

\ FIRST reads the `head' record.

: FIRST ( -- )   HEAD @ READ ;

\ -LOCATE searches a chain for the nth record, returning 'true' if the
\ chain isn't that long, in which case R# is at the actual end of the
\ chain;  otherwise, leaves R# set to the nth record in the chain and
\ returns 0.

: -LOCATE ( n -- r t )
   1+ FIRST  BEGIN
      1- DUP WHILE
      -NEXT IF  EXIT
   THEN  REPEAT ;

\ CHAIN inserts a new record at the nth position or the end.

: CHAIN ( n -- )
   -LOCATE DROP  ( nth record or end )
   SLOT LINK OVER  SNATCH  SWAP READ  LINK L! ;

\ UNCHAIN removes the nth record from the chain.

: UNCHAIN ( n -- )
   DUP 0= ABORT" Won't"  -LOCATE ABORT" Not found"
   SAVE  LINK L@ READ  LINK 0 SNATCH  RESTORE  LINK L! ;
[THEN]
