\ Struct.fth - Database Structure Support

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

[defined] Struct.version 0= [IF]

forth definitions 2 constant Struct.version files

\ This file contains an extension to FILE that supports structured files.

\ POSITION-FILE utility word to position the file bound to the current
\ structure to the given record in that structure.

: POSITION-FILE ( r -- d )   DUP #REC !  B/R @ UM*  ORG @ M+ ;

\ STRUCTURE-RECORD reads the record data from the file every time a
\ record is accessed.  It is assumed that the OS provides plenty of
\ caching already and it is not needed here.

: STRUCTURE-RECORD ( r -- a )   POSITION-FILE (RECORD) ;

\ STRUCTURE-TOUCH writes the record data to the file every time a
\ record is touched.  It is again assumed that the OS provides plenty
\ of caching already and it is not needed here.

: STRUCTURE-TOUCH ( -- )   #REC @ POSITION-FILE (TOUCH) ;

\ STRUCTURE creates disk based structures.  These structures do not use
\ the disk buffers to access the data.  Rather, each structure has a
\ record buffer that is used to hold the current record's data.  Fields
\ within a STRUCTURE are bound to it.

: (STRUCTURE) ( b r o _ -- )
    HERE DUP SET-DATA  #DATA DUP ALLOT ERASE
    ORG !  LIM !  0 #REC !  0 #INDEX !
    DUP B/R !  HERE DATA-BFR !  ALLOT
    ['] STRUCTURE-RECORD 'RECORD !
    ['] STRUCTURE-TOUCH 'TOUCH ! ;

: STRUCTURE ( b r o _ -- ) \ Usage: STRUCTURE <name>
   SAVE-INPUT  CREATE  RESTORE-INPUT THROW
   (STRUCTURE)  BL WORD COUNT NAME-DATA
   DOES> ( -- )   SET-DATA ;
[THEN]
