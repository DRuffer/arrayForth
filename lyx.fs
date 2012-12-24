\ lyx.fs - Load LyX file directly into gforth

: <?> ( n -- )   .S DROP S" <?>" TYPE CR ;    \ a fence post to isolate issues

VARIABLE nw-file-id

VARIABLE LAST-END  0 LAST-END !

CREATE file-input-buffer   1024 ALLOT

CREATE 'CREATE  40 ALLOT  HERE CONSTANT CREATE'

: $CREATE ( a n -- )   S" CREATE " DUP >R  'CREATE SWAP MOVE
    'CREATE R@ + OVER >R  2DUP + CREATE' > ABORT" Name too long!"
    SWAP MOVE  'CREATE R> R> + EVALUATE
    DOES>  DUP 2 CELLS + 2@  ROT 2@ 1 <?>
        2OVER 2OVER D< ABORT" Unfinished symbol"
        nw-file-id @ 2 <?> REPOSITION-FILE THROW  BEGIN
            file-input-buffer 1024 nw-file-id @ READ-LINE THROW
            0= ABORT" Truncated symbol"  >R CR file-input-buffer R@ TYPE
            nw-file-id @ FILE-POSITION THROW
            2OVER D< WHILE
                file-input-buffer R> EVALUATE
        REPEAT  R> DROP ;

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
    find-nw-symbols ;
