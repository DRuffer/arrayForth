\ Memory.fth - Memory based database support

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

[defined] Memory.version 0= [IF]

forth definitions 2 constant Memory.version files

: MEMORY-BIND ( -- handle )   2 B/R @ #B 1024 * \ 2 records minimum
    DUP FILE-HANDLE CELL+ !  DUP ALLOCATE THROW
    DUP ROT ERASE ;

: MEMORY-UNBIND ( handle -- )   FREE THROW ;

: MEMORY-READ ( d a n -- n' ior )   BIND-FILE
    2SWAP IF  DROP NIP -24 EXIT  THEN           \ invalid numeric argument, file too large
    FILE-HANDLE CELL+ @ 2DUP >                  \ limit record position to file size
    IF  SWAP  THEN  DROP SWAP OVER +
    FILE-HANDLE CELL+ @ 2DUP >                  \ limit record size to file size
    IF  SWAP  THEN  DROP OVER -
    DUP >R  SWAP FILE-HANDLE @ +
    ROT ROT MOVE  R> 0 ;

: MEMORY-WRITE ( d a n -- ior )   BIND-FILE
    2SWAP IF  DROP 2DROP -24 EXIT  THEN         \ invalid numeric argument, file too large
    SWAP OVER +  DUP FILE-HANDLE CELL+ @ >      \ record position + size > file size
    IF  FILE-HANDLE @ OVER RESIZE ?DUP
        IF  >R 2DROP 2DROP R> EXIT              \ can't resize the memory
        THEN  FILE-HANDLE !
        DUP FILE-HANDLE CELL+ !
    THEN  OVER - SWAP FILE-HANDLE @ +
    SWAP MOVE  0 ;

: >MEMORY ( addr len -- )   -FILE
    ['] MEMORY-BIND 'BIND-FILE !
    ['] MEMORY-UNBIND 'UNBIND-FILE !
    ['] MEMORY-READ 'READ-RECORD !
    ['] MEMORY-WRITE 'WRITE-RECORD !
    SWAP FILE-HANDLE 2!  BIND-FILE
    FILE-INIT CALLS ;
[THEN]
