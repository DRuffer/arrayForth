[defined] 2+! 0= [IF] : 2+! ( d a -- )   DUP >R 2@ D+ R> 2! ; [THEN]

warnings @  0 warnings !
: append ( from len to -- )   2DUP >R >R  COUNT + SWAP MOVE
    R> R@ C@ + R> C! ;
: place ( from len to -- )   0 OVER C! SWAP 255 MIN SWAP APPEND ;
warnings !

: prepend ( from len to -- )   dup count  dup 1+ allocate throw  dup >r place
    dup >r place  r> r@ count rot append  r> free throw ;

: ,string ( str len -- a )   HERE DUP >R OVER 1+ ALLOT place R> ;
: $c, ( str len -- )   OVER + SWAP ?DO  I C@ C,  LOOP ;

: null? DUP 0= IF NIP DUP THEN ;

: left-parse-string ( str len char -- rstr rlen lstr llen ) \ IEEE 1275 parser from left
    OVER IF
        >R 2DUP R> ROT ROT OVER + SWAP 2DUP = IF  2DROP  ELSE  DO
            DUP I C@ = IF
                DROP 2DUP + I 1+ SWAP OVER - null?  ( rstr rlen | 0 0 )
                2SWAP DROP I OVER - null?           ( lstr llen | 0 0 )
                UNLOOP EXIT  THEN
        LOOP THEN  DROP 0 0 2SWAP
    ELSE  DROP 0 0 2SWAP  THEN ;

[defined] c!-le 0= [IF]
: 1c!-le ( x a n -- )   BEGIN ?DUP WHILE
        1- ROT DUP 8 RSHIFT SWAP 2OVER DROP C! ROT 1+ ROT
    REPEAT 2DROP ;
: c!-le ( ... a n -- )   CELL /MOD SWAP >R  ?DUP
    IF  0 DO  SWAP OVER CELL 1c!-le CELL+ LOOP
    THEN  R> ?DUP IF  1c!-le  ELSE DROP THEN ;
[THEN]
[defined] c!-be 0= [IF]
: 1c!-be ( x a n -- )   BEGIN ?DUP WHILE
        1- ROT DUP 8 RSHIFT SWAP 2OVER +    C! ROT    ROT
    REPEAT 2DROP ;
: c!-be ( ... a n -- )   CELL /MOD SWAP >R  ?DUP
    IF  0 DO  SWAP OVER CELL 1c!-be CELL+ LOOP
    THEN  R> ?DUP IF  1c!-be  ELSE DROP THEN ;
[THEN]
[defined] c@-le 0= [IF]
: 1c@-le ( a n -- x )   0 SWAP BEGIN ?DUP WHILE
        1- ROT 2DUP +      C@ >R ROT 8 LSHIFT R> + ROT
    REPEAT NIP ;
: c@-le ( a n -- ... )
    CELL /MOD ROT OVER CELLS + -ROT SWAP ?DUP
    IF  ROT DUP ROT 1c@-le -ROT SWAP  THEN  ?DUP
    IF  0 DO  1 CELLS - DUP CELL 1c@-le SWAP  LOOP
    THEN  DROP ;
[THEN]
[defined] c@-be 0= [IF]
: 1c@-be ( a n -- x )   0 SWAP BEGIN ?DUP WHILE
        1- ROT DUP 1+ SWAP C@ >R ROT 8 LSHIFT R> + ROT
    REPEAT NIP ;
: c@-be ( a n -- ... )
    CELL /MOD ROT OVER CELLS + -ROT SWAP ?DUP
    IF  ROT DUP ROT 1c@-be -ROT SWAP  THEN  ?DUP
    IF  0 DO  1 CELLS - DUP CELL 1c@-be SWAP  LOOP
    THEN  DROP ;
[THEN]

VARIABLE c@-xx
VARIABLE c!-xx

: nC@ ( a n -- u|d )   c@-xx @ EXECUTE ;
: nC! ( u|d a n -- )   c!-xx @ EXECUTE ;

: LITTLE-ENDIAN ( -- )   ['] c@-le c@-xx !  ['] c!-le c!-xx ! ;
:    BIG-ENDIAN ( -- )   ['] c@-be c@-xx !  ['] c!-be c!-xx ! ;

: LITTLE-ENDIAN? ( -- flag )   ['] c@-le c@-xx @ = ;

BIG-ENDIAN \ Default
: @LINK ( a -- a )   [defined] @REL [IF]
        @REL  [ELSE]  @  [THEN] ;
: !LINK ( a1 a2 -- )   [defined] !REL [IF]
        !REL  [ELSE]  !  [THEN] ;
: ,LINK ( a -- )   [defined] ,REL [IF]
        ,REL  [ELSE]  ,  [THEN] ;

: LINKS ( a -- a' )   BEGIN  DUP @LINK ?DUP WHILE  NIP  REPEAT ;

: >LINK ( a -- )   ALIGN HERE  OVER @LINK ,LINK  SWAP !LINK ;
: <LINK ( a -- )   LINKS  >LINK ;
: UNLINK ( a a' -- a )   @LINK OVER !LINK ;
: CALLS ( a -- )
    BEGIN
        @LINK ?DUP WHILE
        DUP >R  1 CELLS + @LINK EXECUTE  R>
    REPEAT ;

wordlist constant DBfiles-wordlist

: files   forth-wordlist  DBfiles-wordlist
    2 set-order  definitions ;  immediate
files

[defined] spin 0= [IF] : spin ; IMMEDIATE [THEN]
[defined] -spin 0= [IF] : -spin ; IMMEDIATE [THEN]
[defined] GET 0= [IF] : GET DROP ; [THEN]
[defined] GRAB 0= [IF] : GRAB DROP ; [THEN]
[defined] RELEASE 0= [IF] : RELEASE DROP ; [THEN]
256 VALUE #TB       \ #TB is the size of the text input buffer.

CREATE FILE-PAD   #TB ALLOT     \ Make a PAD for our exclusive use

: >TEXT ( a n -- )   FILE-PAD #TB BLANK  FILE-PAD SWAP MOVE ;
: TEXT ( c -- )   WORD COUNT  >TEXT ;
VARIABLE REVERSE   ( Default ) FALSE REVERSE !
VARIABLE KEEP-CLOSED   ( Default ) FALSE KEEP-CLOSED !
[defined] ForTimbre [IF]  TRUE KEEP-CLOSED !  [THEN]

: >MOVE< ( s d n -- )
    REVERSE @ IF
        2/ 0 ?DO
            >R  COUNT R@ 1+ C!  COUNT R@ C!  R> 2 +
        LOOP  2DROP
    ELSE  MOVE  THEN ;

: xTYPE ( a n -- )
    BOUNDS ?DO
        I C@ DUP BL < IF
            DROP SPACE
        ELSE  EMIT
    THEN  LOOP ;

: -TEXT ( a1 n a2 -- flag )
    REVERSE @ IF
        0  ROT 2/ 0 DO
            ROT DUP 2 nC@ >R 2 +
            ROT DUP 2 nC@ >R 2 +
            ROT R> R> - ?DUP IF
                NIP 0< IF
                    1  ELSE  -1
        THEN  LEAVE  THEN  LOOP  NIP NIP
    ELSE  OVER COMPARE  THEN ;
365 4 * 1+ CONSTANT D/Y
CREATE DAYS   -1 ,  0 ,  31 ,  59 ,  90 ,  120 ,  151 ,
        181 ,  212 ,  243 ,  273 ,  304 ,  334 ,  367 ,
: @MTH ( u1 -- u2 )   CELLS DAYS + @ ;
: D-M-Y ( d m y -- u )   >R  @MTH
    58 OVER < IF  R@ 3 AND 0= - THEN + 1-
    R> 1900 -  D/Y UM*  4 UM/MOD SWAP 0<> - + ;
VARIABLE YEAR

: A.D. ( n -- )   YEAR ! ;   TIME&DATE A.D. 2DROP 2DROP DROP
: MTH ( n _ -- )   CREATE , DOES> ( d -- n )   @ YEAR @ D-M-Y ;

1 MTH JAN       2 MTH FEB       3 MTH MAR       4 MTH APR
5 MTH MAY       6 MTH JUN       7 MTH JUL       8 MTH AUG
9 MTH SEP      10 MTH OCT      11 MTH NOV      12 MTH DEC
: M/D/Y ( ud -- u )   10000 UM/MOD  100 /MOD  ROT D-M-Y ;
: Y-DD ( u1 -- y u2 u3 )   4 UM* D/Y  UM/MOD 1900 +  SWAP 4 /MOD 1+
    DUP ROT 0= IF  DUP 60 > +  SWAP DUP 59 > +  THEN ;

: DM ( u1 u2 -- d m )   1 BEGIN  1+  2DUP @MTH > 0= UNTIL  1-
    SWAP DROP SWAP  OVER @MTH - SWAP ;
CREATE MTHS   s" JanFebMarAprMayJunJulAugSepOctNovDec" $c,

: DD-MMM-YYYY ( n -- a n )   BASE @ >R  DECIMAL  Y-DD
    ROT 0 <#  # # # #  2DROP  BL HOLD  DM 1- 3 * MTHS +
    3 + 3 0 DO  1- DUP C@ HOLD  LOOP  DROP
    BL HOLD  0 # #  #>  R> BASE ! ;
: MM/DD/YYYY ( u1 -- c-addr u2 )   BASE @ >R  DECIMAL  Y-DD
    ROT 0 <#  # # # #  2DROP  [char] / HOLD  DM SWAP
    0 # #  2DROP   [char] / HOLD  0 # #  #>  R> BASE ! ;

VARIABLE DATE-FORMAT   ' MM/DD/YYYY DATE-FORMAT !

: .DATE ( u -- )   DATE-FORMAT @ EXECUTE TYPE SPACE ;
: @DATE ( -- n )   TIME&DATE D-M-Y NIP NIP NIP ;

: DATE ( -- )   @DATE .DATE ;
: :00 ( ud1 -- ud2)   DECIMAL  #  6 BASE !  # [char] : HOLD ;
: (TIME) ( secs -- c-addr u)   BASE @ >R  0 <#  :00 :00
    DECIMAL # #  #>  R> BASE ! ;
: .TIME ( secs -- )   (TIME) TYPE SPACE ;

: @TIME ( -- secs )   TIME&DATE 2DROP DROP 60 * + 60 * + ;

: TIME ( -- )   @TIME .TIME ;
VARIABLE 'TITLE

: TITLE" ( -- )   CREATE  HERE 'TITLE !  ,"
    DOES> ( -- )   'TITLE ! ;
VARIABLE 'COMPANY

: COMPANY" ( -- )   HERE 'COMPANY !  ," ;

COMPANY" Software Specialists"
VARIABLE 'APP

: APP" ( -- )   HERE 'APP !  ," ;

APP"  "
[defined] #USER 0= [IF]  0 VALUE #USER  [THEN]
[defined] +USER 0= [IF]
    : +USER ( o n _ -- o+n )   CREATE DUP ALLOT + ;
[THEN]

VARIABLE ORDERED   0 ORDERED !
VARIABLE BIND-FIELDS   FALSE BIND-FIELDS !
#USER CELL +USER R#         \ holds the current record number.
      CELL +USER F#         \ holds the current file pointer.
      CELL +USER DB#        \ holds the current database pointer.
      CELL +USER FLD#       \ holds the current field pointer.
TO #USER 
#USER 1024 +USER WORKING  TO #USER
: SAVE ( -- ) ( R: -- f# db# r# )   R>  DB# @ >R F# @ >R R# @ >R  >R ;
\ NO-TAIL-RECURSION
: RESTORE ( -- ) ( R: f# db# r# -- )   R>  R> R# ! R> F# ! R> DB# !  >R ;
\ NO-TAIL-RECURSION
: (FILE) ( o n -- o+n ) \ Usage: (FILE) <name>
    CREATE  OVER , +  DOES> ( -- a )   @  F# @ + ;

0 VALUE #FILE                       \ holds offset of next FCB field.

#FILE 2 CELLS (FILE) FILE-HANDLE    \ holds OS/Memory handle for this file, if open.
        CELL  (FILE) FILE-INIT      \ holds initialization routines for this file.
        CELL  (FILE) 'NAME-FILE     \ holds routine to name the file
        CELL  (FILE) 'BIND-FILE     \ holds routine to bind the file to a path.
        CELL  (FILE) 'UNBIND-FILE   \ holds routine to unbind the file.
        CELL  (FILE) 'READ-RECORD   \ holds routine to read a record from the file.
        CELL  (FILE) 'WRITE-RECORD  \ holds routine to write a record to the file.
        CELL  (FILE) 'DESTROY-FILE  \ holds routine to delete the file.
        256   (FILE) FILE-NAME      \ holds path and name of the file.
TO #FILE

: SET-FILE ( addr -- )   F# ! ;
: SET-DATA ( addr -- )   DB# ! ;

: (NAME-FILE) ( str len -- )   FILE-NAME DUP 256 ERASE place ;

: NAME-FILE ( str len -- )   'NAME-FILE @ ?DUP IF  EXECUTE
    ELSE  2DROP  THEN ;

: READ-RECORD ( d a n -- n' ior )   'READ-RECORD @ ?DUP IF  EXECUTE
    ELSE  NIP NIP NIP 0  THEN ;

: WRITE-RECORD ( d a n -- ior )   'WRITE-RECORD @ ?DUP IF  EXECUTE
    ELSE  2DROP 2DROP 0  THEN ;

: DESTROY-FILE ( -- ior )   'DESTROY-FILE @ ?DUP IF EXECUTE ELSE 0 THEN ;

: BIND-FILE ( -- )   FILE-HANDLE @ 0=
    IF  'BIND-FILE @ ?DUP
        IF  EXECUTE
        THEN  FILE-HANDLE !
    THEN ;

: -FILE ( -- )   FILE-HANDLE @ ?DUP
    IF  'UNBIND-FILE @ ?DUP
        IF  EXECUTE
        THEN  0 FILE-HANDLE !
    THEN ;

: FILE-ALLOT ( -- )
    HERE DUP SET-FILE  #FILE DUP ALLOT ERASE
    ['] (NAME-FILE) 'NAME-FILE ! ;

: FILE ( -- ) \ Usage: FILE <name>
    SAVE-INPUT  CREATE  RESTORE-INPUT THROW
    FILE-ALLOT  BL WORD COUNT NAME-FILE
    DOES> ( -- )   SET-FILE ;

: FILES-NAME ( str len -- )
    [defined] ?FileName [IF]  0 TO ?FileName  ,ObjName
    [THEN]  (NAME-FILE) ;

: FILES-BIND ( -- handle )
    FILE-NAME COUNT 2DUP R/W OPEN-FILE
    IF  DROP R/W CREATE-FILE THROW
    ELSE  -ROT 2DROP
    THEN ;

: FILES-UNBIND ( handle -- )   CLOSE-FILE THROW ;

: FILES-READ ( d a n -- n' ior )
    FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
    2SWAP FILE-HANDLE @ REPOSITION-FILE ?DUP IF  ROT DROP
    ELSE  FILE-HANDLE @ READ-FILE
    THEN  R> IF  -FILE  THEN ;

: FILES-WRITE ( d a n -- ior )
    FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
    2SWAP FILE-HANDLE @ REPOSITION-FILE ?DUP IF  -ROT 2DROP
    ELSE  FILE-HANDLE @ WRITE-FILE
    THEN  R> IF  -FILE  THEN ;

: FILES-DESTROY ( -- ior )   FILE-NAME COUNT DELETE-FILE ;

: >FILE ( str len -- )   -FILE
    ['] FILES-NAME 'NAME-FILE !
    ['] FILES-BIND 'BIND-FILE !
    ['] FILES-UNBIND 'UNBIND-FILE !
    ['] FILES-READ 'READ-RECORD !
    ['] FILES-WRITE 'WRITE-RECORD !
    ['] FILES-DESTROY 'DESTROY-FILE !
    NAME-FILE  BIND-FILE
    FILE-INIT CALLS
    KEEP-CLOSED @ IF
        -FILE
    THEN  ;

: FILE= ( -- )   BL WORD COUNT  DUP 1+ ALLOCATE THROW
    DUP >R place R@ COUNT >FILE  R> FREE THROW ;

: <FILE ( xt -- )   FILE-INIT <LINK ,LINK ;
: (DATA) ( o n -- o+n ) \ Usage: (DATA) <name>
    CREATE  OVER , +  DOES> ( -- a )   @  DB# @ + ;

0 VALUE #DATA               \ holds offset of next DCB field.

#DATA CELL (DATA) ORG       \ holds offset to first record in the file.
      CELL (DATA) LIM       \ holds number of records in file.
      CELL (DATA) B/B       \ holds bytes used per block.
      CELL (DATA) B/R       \ holds bytes per record.
      CELL (DATA) DATA-BFR  \ holds address of record buffer.
      CELL (DATA) 'RECORD   \ holds the vector for record access.
      CELL (DATA) 'TOUCH    \ holds the vector for field update.
      CELL (DATA) #REC      \ holds the record number last accessed in this file.
                            \  This is used with BOUND-FIELDS.
      CELL (DATA) #INDEX    \ holds index of COPIES field.
       256 (DATA) DATA-NAME \ holds name of the database.
TO #DATA

: NAME-DATA ( str len -- )   DATA-NAME DUP 256 ERASE place ;

: BLOCK-POSITION ( n -- d )   DUP #REC !
    B/R @  B/B @ */MOD  1024 UM*  ROT M+
    ORG @ M+ ;

: (RECORD) ( d -- a )
    DATA-BFR @ B/R @ READ-RECORD IF
        DROP DATA-BFR @ B/R @ ERASE
    ELSE  B/R @ OVER - ?DUP IF
            DATA-BFR @ ROT + SWAP ERASE
        ELSE  DROP  THEN
    THEN  DATA-BFR @ ;

: (TOUCH) ( d -- )   DATA-BFR @ B/R @ WRITE-RECORD THROW ;

: BLOCK-RECORD ( n -- a )   BLOCK-POSITION (RECORD) ;
: BLOCK-TOUCH ( -- )   #REC @ BLOCK-POSITION (TOUCH) ;

: (BLOCK-DATA) ( b r o -- )
    HERE DUP SET-DATA  #DATA DUP ALLOT ERASE
    ORG !  LIM !  0 #REC !  0 #INDEX !
    1024 OVER / OVER *  B/B !  B/R !
    HERE DATA-BFR !  B/R @ ALLOT
    ['] BLOCK-RECORD 'RECORD !
    ['] BLOCK-TOUCH 'TOUCH ! ;

: BLOCK-DATA ( b r o _ -- ) \ Usage: BLOCK-DATA <name>
    SAVE-INPUT  CREATE  RESTORE-INPUT THROW
    (BLOCK-DATA)  BL WORD COUNT NAME-DATA
    DOES> ( -- )   SET-DATA ;
: SAFE ( n -- n )   DUP LIM @ U< 0= ABORT" Outside file " ;

: READ ( n -- )   SAFE DUP R# !  #REC ! ;
: RECORD ( n -- a )   'RECORD @ ?DUP IF EXECUTE THEN ;

: TOUCH ( -- )   'TOUCH @ ?DUP IF EXECUTE THEN ;

: ADDRESS ( a -- a' )   R# @ RECORD  WORKING -  + ;
: INDEX ( n -- )   #INDEX ! ;
: INDEXED ( -- n )   #INDEX @ ;
: #B ( nr b/r -- nb )   1024 SWAP /  DUP 1- ROT +  SWAP / ;

: #R ( nb b/r -- nr )   1024 SWAP / * ;

: +ORIGIN ( -- o )   LIM @ B/R @ B/B @ IF #B 1024 THEN * ORG @ + ;
: STOPPER ( -- )   1 RECORD CELL+  B/R @ CELL -  -1 FILL  TOUCH ;

: INITIALIZE ( -- )
\   CR ." Initializing "
    FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
\   DATA-NAME COUNT TYPE ." ..."
    LIM @ 0 DO
        I RECORD  B/R @  ERASE  TOUCH  spin
    LOOP  STOPPER  -spin
    R> IF  -FILE  THEN ;
: AVAILABLE ( -- a )   0 RECORD ;
: SLOT ( -- r )
    AVAILABLE 4 nC@ DUP  BEGIN
        1+  LIM @ MOD  2DUP = ABORT" File full"
        DUP RECORD  DUP @ WHILE  DROP
    REPEAT  DUP  B/R @ ERASE  -1 SWAP !  TOUCH
    DUP AVAILABLE 4 nC!  TOUCH  NIP ;

: SCRATCH ( r -- )   SAFE  RECORD  0 SWAP !  TOUCH ;
: RECORDS ( -- l f )   AVAILABLE 4 nC@ 1+  1 ;

: WHOLE ( -- l f )   LIM @ 1 ;
: S@ ( n a -- )   FILE-PAD ROT >MOVE< ;
: S! ( n a -- )   FILE-PAD SWAP ROT >MOVE< ;
: (S.) ( n a -- a' n' )   DROP FILE-PAD SWAP -TRAILING ;
: S. ( n a -- )   (S.) xTYPE ;
: NU@ ( a -- n )   ADDRESS 2 nC@ ;
: N@ ( a -- n )   ADDRESS 2 nC@
    DUP 32768 AND IF  -65536 OR  THEN ;
: N! ( n a -- )   ADDRESS 2 nC! TOUCH ; 
: LU@ ( a -- n )   ADDRESS 4 nC@ ;
: L@ ( a -- n )   ADDRESS 4 nC@
    DUP 2147483648 AND IF  -4294967295 OR  THEN ;
: L! ( n a -- )   ADDRESS 4 nC! TOUCH ; 
[DEFINED] SFALIGN [IF]

SFALIGN HERE 1 SFLOATS ALLOT CONSTANT DBFLOAT

: FL@ ( a -- r )   ADDRESS 4 nC@ DBFLOAT ! DBFLOAT SF@ ;
: FL! ( r a -- )   DBFLOAT SF! DBFLOAT @ SWAP ADDRESS 4 nC! TOUCH ;

: FL@-le ( a -- r )   ADDRESS 4 c@-le DBFLOAT ! DBFLOAT SF@ ;
: FL!-le ( r a -- )   DBFLOAT SF! DBFLOAT @ SWAP ADDRESS 4 c!-le TOUCH ;

: FL@-be ( a -- r )   ADDRESS 4 c@-be DBFLOAT ! DBFLOAT SF@ ;
: FL!-be ( r a -- )   DBFLOAT SF! DBFLOAT @ SWAP ADDRESS 4 c!-be TOUCH ;

: FL@+ ( a -- r )   ADDRESS SF@ ;   \ These are fast, but endian dependant
: FL!+ ( r a -- )   ADDRESS SF! TOUCH ;
[THEN]
: D@ ( a -- d )   ADDRESS  CELL CASE
        2 OF  4 nC@  ENDOF
        4 OF  8 nC@  ENDOF
        8 OF  8 nC@ 4294967296 /MOD SWAP  ENDOF
        DUP OF  1 ABORT" CELL size not supported"  ENDOF
    ENDCASE ;
: D! ( d a -- )   ADDRESS  CELL CASE
        2 OF  4 nC!  ENDOF
        4 OF  8 nC!  ENDOF
        8 OF  SWAP 4294967296 + 8 nC!  ENDOF
        DUP OF  1 ABORT" CELL size not supported"  ENDOF
    ENDCASE  TOUCH ;
: B@ ( n a -- )   ADDRESS S@ ;
: B! ( n a -- )   ADDRESS S! TOUCH ;
: 1@ ( a -- n )   ADDRESS C@ ;
: 1! ( n a -- )   ADDRESS C! TOUCH ; 
: PUT ( n a -- )   1 TEXT B! ;
: ASK ( n a -- )   FILE-PAD #TB 2DUP BLANK ACCEPT DROP B! ;
[defined] -warning [IF] -warning [THEN]
: FILLER ( o n _ -- o+n )   BL WORD DROP + ;
[defined] +warning [IF] +warning [THEN] 
: (FIELD) ( o n -- o+n )   CREATE  OVER , +  DOES> ( a -- a' )   @  FLD# @ + ;

0 VALUE #FIELD

#FIELD CELL (FIELD) FIELD-OFFSET \ holds offset within the record.
       CELL (FIELD) FIELD-SIZE   \ holds the size of the field.
       CELL (FIELD) FIELD-COPIES \ holds numbers of times this field is repeated.
       CELL (FIELD) BOUND-FILE   \ holds addr of FCB this record is bound to, or 0.
TO #FIELD

: CREATE-FIELD ( o n -- o+n )
    HERE DUP FLD# !  #FIELD DUP ALLOT  ERASE
    2DUP FIELD-SIZE !  FIELD-OFFSET !  +
    0 FIELD-COPIES !  BIND-FIELDS @ IF
        DB# @ BOUND-FILE !
    ELSE  0 BOUND-FILE !
    THEN ;

: FIELD-DOES ( a -- a' )
    FLD# !  WORKING FIELD-OFFSET @ +
    BOUND-FILE @ ?DUP IF
        SET-DATA  #REC @ R# !
    THEN  FIELD-COPIES @ IF
        #INDEX @  FIELD-SIZE @ * +
    THEN ;

: 1BYTE ( o _ -- o+1 ) \ 1BYTE fields occupy 8 bits.
    CREATE 1 CREATE-FIELD  DOES> ( -- a )   FIELD-DOES ;

: NUMERIC ( o _ -- o+2 ) \ NUMERIC fields occupy 16 bits.
    CREATE 2 CREATE-FIELD  DOES> ( -- a )   FIELD-DOES ;

: LONG ( o _ -- o+4 ) \ LONG fields occupy 32 bits.
    CREATE 4 CREATE-FIELD  DOES> ( -- a )   FIELD-DOES ;

[DEFINED] SFALIGN [IF]
: FLOAT ( o _ -- o+4 ) \ FLOAT fields occupy 32 bits.
    CREATE 4 CREATE-FIELD  DOES> ( -- a )   FIELD-DOES ;
[THEN]

: DOUBLE ( o _ -- o+4 ) \ DOUBLE fields occupy 64 bits.
    CREATE 8 CREATE-FIELD  DOES> ( -- a )   FIELD-DOES ;

: BYTES ( o n _ -- o+n ) \ BYTES fields occupy a specified number of bytes.
    CREATE CREATE-FIELD  DOES> ( -- n a )   FIELD-DOES  FIELD-SIZE @ SWAP ;
: COPIES ( o n -- o' )
    FIELD-COPIES @ ABORT" Invalid COPIES"  DUP
    FIELD-COPIES !  1- FIELD-SIZE @ * + ; 
: ENTIRE ( -- n a )   B/R @ WORKING ;
0 LONG LINK DROP
: N? ( a -- )   N@ . ;
: NU? ( a -- )   NU@ . ;
: L? ( a -- )   L@ . ;
[DEFINED] SFALIGN [IF]
: FL? ( a -- )   FL@ F. ;
[THEN]
: D? ( a -- )   D@ . ;
: 1? ( a -- )   1@ . ;
: B? ( n a -- )   2DUP B@ S.  SPACE ;   \ DataBase Support System
#USER CELL +USER RPT  \ holds the address of the current report title block.
      CELL +USER #COL \ holds the address of the current column width.
TO #USER

: 0COL ( -- )   RPT @ @ #COL ! ;
: +L ( -- )   -spin  CR  0COL ;
: COL ( -- n )   #COL @  DUP CELL+  #COL ! ;
: COLS ( -- n )    COL @  DUP 0< 0= IF  EXIT
    THEN  DROP +L  COL @ ;

: SKIP-COL ( -- )   COLS 1+ SPACES ;
: SKIP-COLS ( n -- )   0 DO  SKIP-COL  LOOP ;
#USER CELL +USER P# TO #USER

VARIABLE 'PAGE   ' PAGE 'PAGE !

: +PAGE ( -- )
    'PAGE @ EXECUTE  1 P# +!  +L  ." Page "  P# ?
    SPACE  DATE  SPACE  'APP @ COUNT TYPE
    +L  RPT @  CELL - @ ?DUP IF EXECUTE THEN ;
: REGISTER ( -- a )   WORKING 16 + ;
: TOTALS ( n -- )   2* CELLS REGISTER  OVER 3 * 2 CELLS + ERASE  REGISTER ! ;
: SUM ( d n -- )   2* CELLS REGISTER + 2+! ;
: REG ( -- a )   REGISTER 2@ MOD  2 CELLS + DUP
    REGISTER CELL+ !  REGISTER + ;

: FOOT ( d -- d )   2DUP REG 2+! ;
: SUB ( -- d )   REG  DUP >R  2@  2DUP REGISTER @ R@ + 2+!  0 0 R> 2! ; 
: TOTAL ( -- )    REGISTER DUP @ DUP 0 DO  >R
        2 CELLS + DUP DUP R@ + 2@  DUP 2OVER ROT ROT 2!
        DUP 2OVER ROT ROT R@ 2* + 2+!  DNEGATE ROT R@ + 2!  R>
    2 CELLS +LOOP  2DROP ;
: GRAND ( -- )   REGISTER DUP @ >R  2 CELLS + DUP R@ 2* +  SWAP R> MOVE ;
: HEADING ( a -- )    DUP RPT !
    CELL+  DUP >R  CELL+ COUNT TYPE
    R@ 3 CELLS - @ ?DUP IF EXECUTE THEN  +L
    R> @  COUNT TYPE +L ;
: TITLE ( -- )   RPT @ HEADING ;
: (R) ( -- a )   ['] TITLE ,  HERE  HERE 0 ,  HERE 0 ,
    [char] \ WORD COUNT ,string DROP  HERE SWAP !  [char] ] WORD COUNT ,string
    HERE DUP ALIGNED  SWAP ?DO  0 C,  LOOP
    HERE ROT !  COUNT  DUP ROT 0  ROT 0 DO
        OVER I + C@  [char] \ = IF
            I SWAP -  ,  BL OVER I + C!  I 1+
    THEN  LOOP  NIP - ,  -1 , ;

: [R+ ( -- a ) \ Usage: [R+ <name> <title> \ <col> \ ... \ <col> ]
    ' , (R) ;
: [R ( -- a ) \ Usage: [R <title> \ <col> \ ... \ <col> ]
    0 , (R) ;
: RIGHT ( a n -- )   COLS OVER -  SPACES TYPE  SPACE ;
: LEFT ( a n -- )   COLS OVER - >R  TYPE  R> 1+ SPACES ;
: CENTER ( a n -- )    COLS OVER - DUP 2/  DUP >R - SPACES
    TYPE  R> 1+ SPACES ;
: LAYOUT ( a -- )   RPT !  0 P# !  +PAGE ;
: (D.) ( d -- a n )
    SWAP OVER DUP 0< IF DNEGATE THEN
    <#  #S ROT SIGN  #> ;

: .N ( n -- )   DUP 0< (D.) RIGHT ;
: .L ( n -- )   DUP 0< (D.) RIGHT ;
[DEFINED] SFALIGN [IF]
: .FL ( F: r -- )   PAD 4 REPRESENT IF  <#
        >R 1- DUP ABS 0 #S 2DROP SIGN  [CHAR] e HOLD        \ exponent
        3 0 DO  PAD 3 + I - C@ HOLD  LOOP  [CHAR] . HOLD
        PAD C@ HOLD  R> IF  [CHAR] - HOLD  THEN  0 0 #>
    ELSE  S" ?.?e?"  THEN  RIGHT ;
[THEN]
: .D ( d -- )   (D.) RIGHT ;

: ?N ( a -- )   N@ .N ;
: ?L ( a -- )   L@ .L ;
[DEFINED] SFALIGN [IF]
: ?FL ( a -- )   FL@ .FL ;
[THEN]
: ?D ( a -- )   D@ .D ;
: ?1 ( a -- )   1@ .N ;
: ?S ( n a -- )   (S.) LEFT ;
: ?B ( n a -- )    2DUP B@  OVER FILE-PAD SWAP 4 MIN nC@ IF
        ?S  ELSE  2DROP SKIP-COL
    THEN ;

: .M/D/Y ( n -- )    ?DUP IF
        MM/DD/YYYY RIGHT  ELSE  SKIP-COL
    THEN ;
: .D-M-Y ( n -- )    ?DUP IF
        DD-MMM-YYYY RIGHT  ELSE  SKIP-COL
    THEN ;
: .WHEN ( n -- )    ?DUP IF
        BASE @ >R  0  <#
        DECIMAL #  6 BASE ! #  [char] : HOLD
        DECIMAL #  6 BASE ! #  [char] : HOLD
        DECIMAL # #  #>  R> BASE !
        RIGHT  ELSE  SKIP-COL
    THEN ;   \ Report Generator
: POSITION-FILE ( r -- d )   DUP #REC !  B/R @ UM*  ORG @ M+ ;

: STRUCTURE-RECORD ( r -- a )   POSITION-FILE (RECORD) ;
: STRUCTURE-TOUCH ( -- )   #REC @ POSITION-FILE (TOUCH) ;

: (STRUCTURE) ( b r o _ -- )
    HERE DUP SET-DATA  #DATA DUP ALLOT ERASE
    ORG !  LIM !  0 #REC !  0 #INDEX !
    DUP B/R !  HERE DATA-BFR !  ALLOT
    ['] STRUCTURE-RECORD 'RECORD !
    ['] STRUCTURE-TOUCH 'TOUCH ! ;

: STRUCTURE ( b r o _ -- ) \ Usage: STRUCTURE <name>
    SAVE-INPUT  CREATE  RESTORE-INPUT THROW
    (STRUCTURE)  BL WORD COUNT NAME-DATA
    DOES> ( -- )   SET-DATA ; \ Structured files
: MEMORY-BIND ( -- handle )   2 B/R @ #B 1024 * \ 2 records minimum
    DUP FILE-HANDLE CELL+ !  DUP ALLOCATE THROW
    DUP ROT ERASE ;

: MEMORY-UNBIND ( handle -- )   FREE THROW ;

: MEMORY-READ ( d a n -- n' ior )   BIND-FILE
    2SWAP IF  DROP NIP -24 EXIT  THEN      \ invalid numeric argument, file too large
    FILE-HANDLE CELL+ @ 2DUP >             \ limit record position to file size
    IF  SWAP  THEN  DROP SWAP OVER +
    FILE-HANDLE CELL+ @ 2DUP >             \ limit record size to file size
    IF  SWAP  THEN  DROP OVER -
    DUP >R  SWAP FILE-HANDLE @ +
    ROT ROT MOVE  R> 0 ;

: MEMORY-WRITE ( d a n -- ior )   BIND-FILE
    2SWAP IF  DROP 2DROP -24 EXIT  THEN    \ invalid numeric argument, file too large
    SWAP OVER +  DUP FILE-HANDLE CELL+ @ > \ record position + size > file size
    IF  FILE-HANDLE @ OVER RESIZE ?DUP
        IF  >R 2DROP 2DROP R> EXIT         \ can't resize the memory
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
    FILE-INIT CALLS ;    \ Memory based data
#USER CELL +USER HEAD TO #USER
: RSWAP ( n a ra -- n a )    >R 2DUP R>  ROT 2/ 0 DO
        OVER 2 nC@  OVER 2 nC@  2OVER  >R 2 nC!  R> 2 nC!
        SWAP 2 +  SWAP 2 +
    LOOP  2DROP ;

: DIRECTION ( n -- n a rh rl )
    AVAILABLE 4 nC@ +
    AVAILABLE 4 nC! TOUCH  ENTIRE
    AVAILABLE 4 nC@ 2 +
    SAFE R# @ ;

: +ORDERED ( -- )
    FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
    1 DIRECTION DO
        I RECORD  RSWAP TOUCH
    LOOP  2DROP  ORDERED RELEASE
    R> IF  -FILE  THEN ;
: -ORDERED ( -- )
    FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
    ENTIRE SWAP ERASE  -1 DIRECTION SWAP DO
        I RECORD RSWAP TOUCH
    -1 +LOOP  2DROP  ORDERED RELEASE
    R> IF  -FILE  THEN ;
: -BINARY ( n a -- f )    FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
    SWAP  AVAILABLE 4 nC@ 2/ 1+ DUP READ  ORDERED GET  BEGIN
        DUP 1+ 2/  2OVER OVER ADDRESS  -TEXT 1- IF
            NEGATE
        THEN  R# +!  2/ DUP 0=
    UNTIL  DROP  2DUP OVER ADDRESS  -TEXT 0> ABS  R# +!
    OVER ADDRESS -TEXT  R> IF  -FILE  THEN ;
: BINARY ( n a -- n )   -BINARY  ORDERED RELEASE  ABORT" Unknown"  LINK L@ ;
: SNATCH ( a r -- r )   OVER L@  SWAP ROT L! ;
: -NEXT ( -- t )   LINK L@  DUP 0> IF  READ 1  THEN  1- ;
: FIRST ( -- )   HEAD @ READ ;
: -LOCATE ( n -- r t )
    1+ FIRST  BEGIN
        1- DUP WHILE
        -NEXT IF  EXIT
    THEN  REPEAT ;
: CHAIN ( n -- )    -LOCATE DROP  ( nth record or end )
    SLOT LINK OVER  SNATCH  SWAP READ  LINK L! ;
: UNCHAIN ( n -- )    DUP 0= ABORT" Won't"  -LOCATE ABORT" Not found"
    SAVE  LINK L@ READ  LINK 0 SNATCH  RESTORE  LINK L! ;     \ Ordered Index
0  20 BYTES NAME   20 BYTES STREET
   14 BYTES CITY    2 BYTES STATE
    6 BYTES ZIP    14 BYTES PHONE   DROP 
FILE People.dbf

: /People ( -- ) \ Runtime file setup
    People.dbf  S" People.dbf" >FILE
    TRUE REVERSE !  LITTLE-ENDIAN ;

76 500 400 BLOCK-DATA PEOPLE
\ PEOPLE INITIALIZE
: enter-person   PEOPLE  SLOT READ
    CR ." Name? "     NAME ASK
    CR ." Address? "  STREET ASK
    CR ." City? "     CITY ASK
    CR ." State? "    STATE ASK
    CR ." Zip? "      ZIP ASK
    CR ." Phone? "    PHONE ASK ; 
: display-person   CR NAME B?  CR STREET B?  CR
    CITY B?  STATE B?  ZIP B?  PHONE B? ; 
: display-everyone   PEOPLE RECORDS DO CR
    I READ  display-person  LOOP ; 
: display.person   NAME ?B  STREET ?B  CITY ?B  STATE ?B  ZIP ?B ;

[R                 People\Name          \Address            \City         \St.\Zip ]
    CONSTANT PEOPLE-TITLE

: all-people   PEOPLE-TITLE LAYOUT  +L
    PEOPLE RECORDS ?DO  I READ  display.person  +L  LOOP ;    \ People example
: PERSON-HELP ( -- )
    CR ." PERSON-HELP   Display these PERSONNEL instructions."
    CR ." enter name    Enter a new person into the file with"
    CR ."                   access key of 'name'."
    CR
    CR ." remove name   Delete 'name' from the data base."
    CR
    CR ." fix name      Enter new information replacing all"
    CR ."                   current data for 'name'."
    CR
    CR ." view name     Display a person whose key is 'name'."
    CR
    CR ." v             Display current person."
    CR
    CR ." all           Display all records in the file."
    CR ;

FILE Personnel.dbf

: /Personnel ( -- ) \ Runtime file setup
    Personnel.dbf  S" Personnel.dbf" >FILE
    FALSE REVERSE !  BIG-ENDIAN ;

( Bytes  records   origin           name   )
     16      300        0 BLOCK-DATA (PERSONNEL)
    128      300  +ORIGIN BLOCK-DATA  PERSONNEL 

  4 ( LINK)
  12 BYTES NICKNAME      ( Nickname, used as the key.)
  32 BYTES FULL-NAME     ( Full name, first name first.)
  32 BYTES STREET-ADDR   ( Street addr. or PO Box, etc.)
  32 BYTES CITY-STATE
     LONG ZIP-CODE       ( Note: can only handle US zips)
     NUMERIC AREA-CODE
     LONG PHONE-NUMBER
\ .( Personnel.dbf records = ) DUP .  .( Bytes ) CR
CONSTANT |PERSONNEL|
: PERSON ( - n a)   1 TEXT  NICKNAME S!  (PERSONNEL) NICKNAME ;
: DIGITS ( - n)   QUERY  32 WORD COUNT  0 0 2SWAP
    BEGIN  >NUMBER  DUP
    WHILE  1 /STRING
    REPEAT  2DROP DROP ;
: !LABEL   CR ." Name: "   FULL-NAME ASK
     CR   ." Street: "   STREET-ADDR ASK
     CR   ." City, State: "   CITY-STATE ASK
     CR   ." Zip: "   DIGITS ZIP-CODE L!
     CR   ." Area: "   DIGITS AREA-CODE N!
          ." Phone: "   DIGITS PHONE-NUMBER L! ;
: enter   PERSON -BINARY IF  SAVE  PERSONNEL SLOT DUP
        READ  NICKNAME S@ NICKNAME B!  RESTORE  DUP LINK !
        +ORDERED  PERSONNEL READ !LABEL
    ELSE  ORDERED RELEASE  1 ABORT" Already known "
    THEN ;
: fix   PERSON BINARY  PERSONNEL READ !LABEL ;
: remove   PERSON  BINARY -ORDERED  PERSONNEL SCRATCH ;
: .PHONE   AREA-CODE N@ 0  <# 41 HOLD # # #  40 HOLD #>
    TYPE SPACE  PHONE-NUMBER L@ 0 <# # # # #  45 HOLD ( -)
    # # # #> TYPE ;
: .ZIP   ZIP-CODE L@ 0 <# # # # # # #>  TYPE ;
: .PERSON ( n)   PERSONNEL READ  CR  FULL-NAME B?  5 SPACES
    ." (" SPACE   NICKNAME B?  ." )"  CR   STREET-ADDR B?
    CR  CITY-STATE B? CR   .ZIP  10 SPACES  .PHONE  SPACE ;
: view   PERSON BINARY  .PERSON ;
: v   R# @ .PERSON ;
: all   (PERSONNEL) RECORDS DO
        I (PERSONNEL) READ  LINK L@ .PERSON  CR
    LOOP  SPACE ; \ Personnel example
FILE Customers.dbf

: /Customers ( -- ) \ Runtime file setup
    Customers.dbf  S" Customers.dbf" >FILE
    TRUE REVERSE !  LITTLE-ENDIAN ;

70 500       0 BLOCK-DATA CUSTOMERS
46 500 +ORIGIN BLOCK-DATA SERIALS

( CUSTOMERS records:)
0  4 FILLER LINK  ( LINK to 1st serial#)
   20 BYTES COMPANY
   16 BYTES CONTACT
   30 BYTES ADDR
DROP

( SERIALS records:)
0  4 FILLER LINK  ( LINK to next serial#)
   10 BYTES SERIAL#
    NUMERIC PRODUCT  ( product code)
    NUMERIC OWNER  ( link to owner CUSTOMERS record)
DROP

: edit-company   CR  ." Company name? "  COMPANY ASK
    CR  ." Contact? "  CONTACT ASK
    CR  ." Address? "  ADDR ASK ;
: new-company   CUSTOMERS  SLOT DUP . READ  edit-company ;

VARIABLE ALT  TRUE ALT ! \ Another approach

: VALID ( n - t)   0 OVER < DUP  IF
    SWAP READ  ELSE  SWAP DROP  THEN ;
: ALT-FIRST ( - t)   HEAD @ VALID ;
: ALT-NEXT ( - t)   LINK L@ VALID ;
: ALT-LOCATE ( n - t)   ALT-FIRST IF BEGIN  DUP WHILE
        1-  ALT-NEXT 0= IF  DROP -1 EXIT  THEN
    REPEAT  ELSE DROP -1 THEN ;

: (add)   CR  ." Serial# ? "   SERIAL# ASK ;

: add   ALT @ IF  LINK L@ HEAD !  SAVE  SERIALS
                      ALT-FIRST IF  -1 CHAIN  ELSE ( no chain)
                          SLOT DUP RESTORE  LINK L!  SAVE
                          SERIALS READ  THEN
                      (add)  RESTORE
    ELSE  SAVE  LINK L@ DUP 0< IF  ( empty chain) DROP
            SAVE  SERIALS SLOT  RESTORE
            DUP LINK L!  SERIALS READ
        ELSE  HEAD !  SERIALS -1 CHAIN ( add at end)
        THEN (add)  RESTORE
    THEN ;

: edit-serial ( n)   SAVE SERIALS  1- -LOCATE ABORT" Can't"
    CR  Serial# B?  (add)  RESTORE ;

: .company   COMPANY B?  CONTACT B?  ADDR B? ;

: .companies   CUSTOMERS  RECORDS ?DO
    CR  I .  I READ  .company  LOOP ;

: .serials   0  SERIALS  FIRST BEGIN
    CR 1+ DUP .  Serial# B?  -NEXT UNTIL  DROP ;
: all-serials   ALT @ IF  LINK L@ HEAD !  SAVE  0
                              SERIALS  ALT-FIRST BEGIN  WHILE  CR  1+ DUP .
                                  SERIAL# B?  ALT-NEXT REPEAT  RESTORE  DROP
    ELSE  LINK L@  0>  IF
            LINK L@  HEAD !  SAVE  .serials  RESTORE
    THEN  THEN ;

: show ( n)   CUSTOMERS READ  CR .company  all-serials ; \ Customers example
FILE Accounts.dbf

: /Accounts ( -- ) \ Runtime file setup
    Accounts.dbf  S" Accounts.dbf" >FILE
    TRUE REVERSE !  LITTLE-ENDIAN ;

76 500 0 BLOCK-DATA ACCOUNTS

0  10 BYTES NAMES   NUMERIC ACCT#   DOUBLE BALANCE   DROP

: (.$) ( d - a n)   SWAP OVER DABS
    <#  # #  46 HOLD  #S  SIGN  #> ;
: .ACCOUNT   ACCT# ?N  NAMES ?B
    BALANCE D@ (.$) RIGHT ;

[R Account Balances\   Account#\Name          \Balance]
    CONSTANT ACCOUNTS-TITLE

: balances   ACCOUNTS-TITLE LAYOUT
    ACCOUNTS RECORDS ?DO  I READ  .ACCOUNT  LOOP ;

: enter-bal ( n d)   ACCOUNTS  SLOT READ  BALANCE D!
    ACCT# N!  NAMES PUT ;  \ Accounts example
FILE Wines.dbf

: /Wines ( -- ) \ Runtime file setup
    Wines.dbf  S" Wines.dbf" >FILE
    TRUE REVERSE !  LITTLE-ENDIAN ;

28 500 0 BLOCK-DATA WINES

0  16 BYTES Location   NUMERIC Chablis   NUMERIC Rose
     NUMERIC Champagne   DROP

: .amounts   Location ?B  Chablis N@ S>D FOOT .D
    Rose N@ S>D FOOT .D  Champagne N@ S>D FOOT .D ;

: .subs   SUB .D  SUB .D  SUB .D  +L ;

[R Wine Inventory by Store\Location        \Chablis\  Rose\ Champagne]
    CONSTANT WINES-TITLE

: INVENTORY   WINES-TITLE LAYOUT  3 TOTALS  +L
    ." Northern California"  +L
    WINES RECORDS DO  I READ  .amounts  I 4 = IF  +L
        SKIP-COL .subs +L  ." Southern California " +L
    THEN LOOP  +L
    SKIP-COL  .subs  ." Grand Total:     " COLS DROP
    TOTAL .subs ;

: enter-wine ( Cablis Rose Champagne -- )   WINES  SLOT READ
    Champagne N!  Rose N!  Chablis N!  Location PUT ;     \ Wine Inventory example
: HELP ( S: -- ) ( G: This is the  GLOSSARY  help screen )
 CR PAGE ." HELP                 To display this  GLOSSARY  application"
      CR ."    This utility uses a disk file named Glossary.dbf"
      CR ." SUMMARY              Displays all words in the file."
      CR ." /VOCABULARY _____    Displays all words in vocabulary _____ ."
      CR ." VOCAB _____          Specifies vocabulary for entries & searches"
      CR ." n SOURCE 2 nC!       Specifies source block  n  for entries."
      CR ." NEW _____            Enters  ____  making it current."
      CR ."    Thereafter use  U  to enter descriptive text."
      CR ." FIND _____           Locates and shows _____ making it current."
      CR ." F                    Re-displays the current word."
      CR ." n AT                 Changes the current word's source to  n ."
      CR ." STACKS               Enter 'input' and 'output' stack arguments."
      CR ." l T  ( or  t)        Types line  l (0-n)  making it current."
      CR ." P  ( or  p ) _____   Puts  _____  on the current line."
      CR ." U  ( or  u ) _____   Inserts  _____  under the current line."
      CR ." The date used for today's entries will be "  DATE ;

FILE Glossary.dbf

: /Glossary ( -- ) \ Runtime file setup
    Glossary.dbf  S" Glossary.dbf" >FILE
    TRUE REVERSE !  LITTLE-ENDIAN ;

( Bytes  records  origin              name )
   28      432    0       BLOCK-DATA (GLOSSARY)
   64     2300   +ORIGIN  BLOCK-DATA  GLOSSARY

4 ( LINK )  24 BYTES NAME+VOC  DROP   ( Index )

4 ( LINK )  12 BYTES WORD-NAME
            12 BYTES VOC
            16 BYTES BEFORE
            16 BYTES AFTER
             NUMERIC SOURCE
             NUMERIC ENTERED  DROP

4 ( LINK )  60 BYTES PHRASE  DROP
: VOCAB ( -- )   BL TEXT  VOC S! ;
: (SHOW) ( -- )    FIRST  WORD-NAME ?B  VOC ?B  BEFORE ?B  AFTER ?B
    SOURCE ?N  ENTERED NU@  .M/D/Y  BEGIN
        +L  -NEXT 0= WHILE
            10 SPACES  PHRASE B?
    REPEAT ;

[R FORTH GLOSSARY\ NAME        \VOCABULARY  \INPUT           \OUTPUT          \BLK\      ENTERED]
    CONSTANT GLOSSARY-TITLE

VARIABLE #L

: F ( -- )    GLOSSARY-TITLE HEADING  FIRST (SHOW)  1 #L ! ;
: STACKS ( -- )   FIRST
    CR  ." Before: "  BEFORE ASK
    CR ." After: "  AFTER ASK  F ;
: (FIND) ( -- n a ) \ Usage: (FIND) <name>
    BL TEXT WORD-NAME S!  (GLOSSARY) NAME+VOC ;

: (ENTER) ( -- ) \ Usage: (ENTER) <name>
    (FIND) -BINARY IF
        SAVE  GLOSSARY SLOT  DUP HEAD !  LINK !  RESTORE
        NAME+VOC S@  +ORDERED  NAME+VOC S!
        GLOSSARY FIRST  NAME+VOC B!
        SOURCE 2 nC@  SOURCE N!
    ELSE  ORDERED RELEASE  LINK L@  GLOSSARY READ
    THEN  @DATE ENTERED N! ;

: NEW ( -- ) \ Usage: NEW <name>
    (ENTER)  STACKS  CR  64 SPACES  0 #L ! ;
: DELETE ( -- )    (FIND)  BINARY
    ORDERED GRAB  -ORDERED
    GLOSSARY  BEGIN
        READ  LINK 0 SNATCH  DUP 0<
    UNTIL  DROP ;
: AT ( n -- )   FIRST  SOURCE N! ;
: T ( n -- )    1+ DUP -LOCATE ABORT" Not there"  #L !  CR
    2 SPACES  PHRASE B? ;
: P ( -- ) \ Usage: P text
    1 TEXT  PHRASE B! ;
: U ( -- ) \ Usage: U text
    #L @  CHAIN P  1 #L +! ;
: X ( -- )   #L @ 1- UNCHAIN ;
: FIND ( -- ) \ Usage: FIND <name>
    (FIND) BINARY HEAD !  GLOSSARY F ;
: GLOSS ( n -- t )    (GLOSSARY) READ  LINK L@ DUP 0> IF
        HEAD !  GLOSSARY FIRST TRUE
    ELSE  DROP 0  THEN ;

: MOVED ( s d -- )    SWAP  (GLOSSARY) RECORDS DO
        I GLOSS DROP  SOURCE N@ OVER = IF
            OVER SOURCE N!
    THEN  LOOP  2DROP ;
: SUMMARY ( -- )    GLOSSARY-TITLE LAYOUT
    (GLOSSARY) RECORDS DO
        I GLOSS IF
            +L (SHOW)
    THEN  LOOP  SPACE ;
: /VOCABULARY ( -- ) \ Usage: \VOCABULARY <name>
    VOCAB  GLOSSARY-TITLE LAYOUT  (GLOSSARY) RECORDS DO
        I GLOSS IF
            VOC  SWAP OVER ADDRESS -TEXT 0= IF
                +L (SHOW)
    THEN  THEN  LOOP  SPACE ;  \ Sample FILE application for documenting Forth words

: fits ( n a -- flag )   WORKING - + B/R @ > 0= ;
: text= ( a1 n1 n2 a2 -- flag )   2DUP B@ (S.) COMPARE 0= ;
: -linked ( n -- flag )   READ LINK L@ 0< ;
: double-stacked ( a n -- flag )   READ >R DEPTH R> SWAP >R D@ 2DROP DEPTH R> - 0= ;

: test ( -- )
    /People assert( PEOPLE PHONE fits ) display-everyone
    /Personnel assert( PERSONNEL RECORDS - 4 = ) all
        2 READ assert( S" Greg" NICKNAME text= )
    /Customers FALSE ALT ! assert( SERIALS 2 -linked ) 1 show 2 show
        TRUE ALT ! assert( SERIALS 3 -linked ) 1 show 2 show
    /Accounts assert( ACCOUNTS BALANCE 1 double-stacked ) balances
    /Wines INVENTORY
    /Glossary SUMMARY
;      \ Test the examples

FORTH DEFINITIONS
