\ lyx.fs - Load LyX file directly into gforth

: <?> ( n -- )   .S DROP S" <?>" TYPE CR ;    \ a fence post to isolate issues

VARIABLE nw-file-id

VARIABLE LAST-END  0 LAST-END !

VARIABLE file-buffer  0 file-buffer !
: last-file-buffer ( -- a )   file-buffer @ ;
: last-file-position ( -- a )   file-buffer @ CELL+ ;
: file-input-buffer ( -- a )   file-buffer @ 3 CELLS + ;

CREATE 'CREATE  40 ALLOT  HERE CONSTANT CREATE'

: push-buffer ( -- )  file-buffer @
    1024 3 CELLS + ALLOCATE THROW DUP file-buffer ! !
    nw-file-id @ FILE-POSITION THROW last-file-position 2! ;

: pop-buffer ( -- )   last-file-position 2@  last-file-buffer @
    file-buffer @ FREE THROW  file-buffer !
    nw-file-id @ REPOSITION-FILE THROW ;

: $CREATE ( a n -- )   S" CREATE " DUP >R  'CREATE SWAP MOVE
    'CREATE R@ + OVER >R  2DUP + CREATE' > ABORT" Name too long!"
    SWAP MOVE  'CREATE R> R> + EVALUATE
    DOES>  DUP 2 CELLS + 2@  ROT 2@
        2OVER 2OVER D< ABORT" Unfinished symbol"
        push-buffer  nw-file-id @ REPOSITION-FILE THROW  BEGIN
            file-input-buffer 1024 nw-file-id @ READ-LINE THROW
            0= ABORT" Truncated symbol"
            CR file-input-buffer OVER TYPE
            file-input-buffer SWAP EVALUATE
            2DUP nw-file-id @ FILE-POSITION THROW D<=
        UNTIL  R> DROP 2DROP  pop-buffer ;

: find-nw-symbols ( -- )
    BEGIN  nw-file-id @ FILE-POSITION THROW
        file-input-buffer 1024 nw-file-id @ READ-LINE THROW
        0= IF  DROP 2DROP EXIT  THEN  ?DUP IF  file-input-buffer SWAP
            S" <<" 2OVER DROP OVER COMPARE 0= IF
                OVER SWAP BL SCAN DROP OVER - DUP >R
                S" >>=" 2OVER + OVER - OVER COMPARE 0= IF
                    CR 2DUP TYPE  2OVER D.
                    2DUP 1- $CREATE  2OVER R@ M+ , ,
                    HERE LAST-END !
                    0 , 0 ,
                THEN  2DROP R> DROP
            ELSE
                S" @" file-input-buffer OVER COMPARE 0= IF
                    LAST-END @ ?DUP 0= ABORT" @ outside of scrap"
                    2@ OR ABORT" @ sequence error"
                    2OVER D. 2OVER LAST-END @ 2!
                    0 LAST-END !
                THEN  2DROP
            THEN
        THEN  2DROP
    AGAIN ;

: notangle ( -name.nw- ) \ Start parsing NoWeb file
    BL WORD COUNT R/O OPEN-FILE THROW  nw-file-id !
    push-buffer  find-nw-symbols  pop-buffer ;
