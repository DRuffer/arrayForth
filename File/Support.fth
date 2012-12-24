\ Support.fth - Data Base Support System (FILE)

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

[defined] Support.version 0= [IF]

forth definitions 2 constant Support.version files

\ FILE variables

VARIABLE ORDERED        \ a "facility" variable that controls access to ordered
       0 ORDERED !      \ indexes during search and updating operations.

VARIABLE BIND-FIELDS    \ true if the following fields are bound to the current
   FALSE BIND-FIELDS !  \ file.  It is reset when a new file is defined.

\ These user variables are required by the Data Base Support System.

#USER CELL +USER R#         \ holds the current record number.
      CELL +USER F#         \ holds the current file pointer.
      CELL +USER DB#        \ holds the current database pointer.
      CELL +USER FLD#       \ holds the current field pointer.
      1024 +USER WORKING    \ storage area used by Ordered Indexes and Subtotaling.
TO #USER

\ File parameters

\ SAVE and RESTORE allow temporary use of a different file & record
\ combination.  Since the Return Stack is used for temporary storage,
\ they must be used in the same definition and at the same level with
\ respect to any DO structure.

: SAVE ( -- ) ( R: -- f# db# r# )   R>  R# @ >R DB# @ >R F# @ >R  >R ; \ NO-TAIL-RECURSION
: RESTORE ( -- ) ( R: f# db# r# -- )   R>  R> F# ! R> DB# ! R> R# !  >R ; \ NO-TAIL-RECURSION

\ (FILE) defines words that return the addresses of parameters for the
\ current file (the one selected by the use of its name, whose address
\ is in F# .

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

: NAME-FILE ( str len -- )   'NAME-FILE @ ?DUP IF EXECUTE THEN ;

: READ-RECORD ( d a n -- n' ior )   'READ-RECORD @ ?DUP IF EXECUTE ELSE NIP NIP NIP 0 THEN ;

: WRITE-RECORD ( d a n -- ior )   'WRITE-RECORD @ ?DUP IF EXECUTE ELSE 2DROP 2DROP 0 THEN ;

: DESTROY-FILE ( -- ior )   'DESTROY-FILE @ ?DUP IF EXECUTE ELSE 0 THEN ;

\ FILE binding

: BIND-FILE ( -- )   FILE-HANDLE @ 0=
    IF  'BIND-FILE @ ?DUP
        IF  EXECUTE
        THEN  FILE-HANDLE !
    THEN ;

\ -FILE if the file is bound, close the bound file and unbind it.
\ This is done when changing files.

: -FILE ( -- )   FILE-HANDLE @ ?DUP
    IF  'UNBIND-FILE @ ?DUP
        IF  EXECUTE
        THEN  0 FILE-HANDLE !
    THEN ;

: FILE-ALLOT ( -- )
    HERE DUP SET-FILE  #FILE DUP ALLOT ERASE
    ['] (NAME-FILE) 'NAME-FILE ! ;

\ FILE defines a FILE file given its name.

: FILE ( -- ) \ Usage: FILE <name>
    SAVE-INPUT  CREATE  RESTORE-INPUT THROW
    FILE-ALLOT  BL WORD COUNT NAME-FILE
    DOES> ( -- )   SET-FILE ;

\ This is also dependent on the environment setup by Forth2OF.fth
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

\ FILE= open the following file and bind it to the current data.

: FILE= ( -- )   BL WORD COUNT  DUP 1+ ALLOCATE THROW           \ Usage: FILE= <name>
    DUP >R place R@ COUNT >FILE  R> FREE THROW ;

\ <FILE extends the chain of definitions that adjust the locations of
\ various pieces of a structure.  It is extended as needed, where it
\ is needed.

: <FILE ( xt -- )   FILE-INIT <LINK ,LINK ;

\ Block Data definition and access

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

\ BLOCK-DATA defines a FILE database given bytes per record, number of records
\ and byte offset within the file.

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

\ READ selects given record, doesn't perform an actual disk operation.

: READ ( n -- )   SAFE DUP R# !  #REC ! ;

\ RECORD returns the address of the file data in a block buffer.

: RECORD ( n -- a )   'RECORD @ ?DUP IF EXECUTE THEN ;

\ TOUCH updates the most recently accessed field.

: TOUCH ( -- )   'TOUCH @ ?DUP IF EXECUTE THEN ;

\ INDEX set the current field index.  Used to select one of the fields
\ in a COPIES field.

: INDEX ( n -- )   #INDEX ! ;

\ INDEXED return the current field index.

: INDEXED ( -- n )   #INDEX @ ;

\ File initialization

\ #B and #R are useful in determining how many blocks a file will
\ require and how many records will fit in a range of blocks.  Note that
\ they are the converse of each other.

: #B ( nr b/r -- nb )   1024 SWAP /  DUP 1- ROT +  SWAP / ;

: #R ( nb b/r -- nr )   1024 SWAP / * ;

\ +ORIGIN returns the offset for the next data set

: +ORIGIN ( -- o )   LIM @ B/R @ B/B @ IF #B 1024 THEN * ORG @ + ;

\ STOPPER initializes a record to all "high values".  This is required
\ for Ordered Indexes, but is harmless for other files since the first
\ SLOT will clear the record to zeroes.

: STOPPER ( -- )   1 RECORD CELL+  B/R @ CELL -  -1 FILL  TOUCH ;

\ INITIALIZE erases the entire file to zeroes, except for the STOPPER in
\ record 1.

: INITIALIZE ( -- )
\   CR ." Initializing "
   FILE-HANDLE @ 0= DUP >R IF  BIND-FILE  THEN
\   DATA-NAME COUNT TYPE ." ..."
   LIM @ 0 DO
      I RECORD  B/R @  ERASE  TOUCH  spin
   LOOP  STOPPER  -spin
   R> IF  -FILE  THEN ;

\ Record allocation

\ AVAILABLE contains the last assigned record#.  It will be accurate for
\ files managed by SLOT and +ORDERED .

: AVAILABLE ( -- a )   0 RECORD ;

\ These words follow the convention that a record is "available" if its
\ 1st cell contains 0.

\ SLOT is used for allocating single records.  The assigned record is
\ cleared to zeroes.

: SLOT ( -- r )
   AVAILABLE 4 nC@ DUP  BEGIN
      1+  LIM @ MOD  2DUP = ABORT" File full"
      DUP RECORD  DUP @ WHILE  DROP
   REPEAT  DUP  B/R @ ERASE  -1 SWAP !  TOUCH
   DUP AVAILABLE 4 nC!  TOUCH  NIP ;

\ SCRATCH de-allocates the record, but doesn't destroy its contents
\ except for the 1st cell.

: SCRATCH ( r -- )   SAFE  RECORD  0 SWAP !  TOUCH ;

\ RECORDS returns loop values for an Ordered Index or other file which
\ has never "wrapped around".

: RECORDS ( -- l f )   AVAILABLE 4 nC@ 1+  1 ;

\ WHOLE returns loop values for the entire file.

: WHOLE ( -- l f )   LIM @ 1 ;

\ Field operators

\ These words access data in named fields in the current record.

\ ADDRESS converts a field address in WORKING to the address of that
\ field in the current record of the current file.

: ADDRESS ( a -- a' )   R# @ RECORD  WORKING -  + ;

\ S@ , S! , and S. move strings between FILE-PAD and a given address usually
\ in  WORKING or the equivalent field in the file.

: S@ ( n a -- )   FILE-PAD ROT >MOVE< ;
: S! ( n a -- )   FILE-PAD SWAP ROT >MOVE< ;
: (S.) ( n a -- a' n' )   DROP FILE-PAD SWAP -TRAILING ;
: S. ( n a -- )   (S.) xTYPE ;

\ NU@ is the unsigned version of N@ .

: NU@ ( a -- n )   ADDRESS 2 nC@ ;

\ N@ and N! move 16-bit numbers between the stack and the file.

: N@ ( a -- n )   ADDRESS 2 nC@
   DUP 32768 AND IF  -65536 OR  THEN ;
: N! ( n a -- )   ADDRESS 2 nC! TOUCH ;

\ L@ and L! move 32-bit numbers between the stack and the file.

: L@ ( a -- n )   ADDRESS 4 nC@ ;
: L! ( n a -- )   ADDRESS 4 nC! TOUCH ;

[DEFINED] SFALIGN [IF]
\ FL@ and FL! move 32-bit floats between the stack and the file.

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

\ D@ and D! move 64-bit numbers between the stack and the file.

: D@ ( a -- d )   ADDRESS 8 nC@ ;
: D! ( d a -- )   ADDRESS 8 nC! TOUCH ;

\ B@ and B! move strings between FILE-PAD and the file.

: B@ ( n a -- )   ADDRESS S@ ;
: B! ( n a -- )   ADDRESS S! TOUCH ;

\ 1@ and 1! move 8-bit numbers between the stack and the file.

: 1@ ( a -- n )   ADDRESS C@ ;
: 1! ( n a -- )   ADDRESS C! TOUCH ;

\ PUT puts the rest of the input stream in a specified BYTES field.

: PUT ( n a -- )   1 TEXT B! ;

\ ASK awaits input and then PUTs the input in a specified BYTES field.

: ASK ( n a -- )   FILE-PAD #TB 2DUP BLANK ACCEPT DROP B! ;

\ Field parameters

: (FIELD) ( o n -- o+n ) \ Usage: (FIELD) <name>
   CREATE  OVER , +  DOES> ( a -- a' )   @  FLD# @ + ;

\ #FIELD holds offset of next (FIELD) field.

0 VALUE #FIELD

#FIELD CELL (FIELD) FIELD-OFFSET    \ holds offset within the record.
       CELL (FIELD) FIELD-SIZE      \ holds the size of the field.
       CELL (FIELD) FIELD-COPIES    \ holds the numbers of times this field is repeated.
       CELL (FIELD) BOUND-FILE      \ holds address of FCB that this record is bound to, or 0.
TO #FIELD

\ Fields within records

\ The following words define named fields within records.   All return
\ an address in  WORKING , which the operators in the previous code
\ convert to addresses in the file.

\ FILLER skips a specified number of bytes in the description.

[defined] -warning [IF] -warning [THEN]
: FILLER ( o n _ -- o+n )   BL WORD DROP + ;
[defined] +warning [IF] +warning [THEN]

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

\ COPIES makes an array of the previous field.  Use INDEX to access the
\ individual elements.

: COPIES ( o n -- o' )
   FIELD-COPIES @ ABORT" Invalid COPIES"  DUP
   FIELD-COPIES !  1- FIELD-SIZE @ * + ;

\ ENTIRE returns parameters for a pseudo-BYTES field occupying the
\ entire record.

: ENTIRE ( -- n a )   B/R @ WORKING ;

\ LINK is a field defined as the 1st 32-bits of a record.  It is used by
\ "chains" and Ordered indexes.  If you aren't using these features, you
\ may re-name this field.

0 LONG LINK DROP

\ N? , L? , D? , 1? and B? fetch and type data from NUMERIC ,
\ LONG , DOUBLE , 1BYTE , and BYTES fields, respectively.

: N? ( a -- )   N@ . ;

: NU? ( a -- )   NU@ . ;

: L? ( a -- )   L@ . ;

[DEFINED] SFALIGN [IF]
: FL? ( a -- )   FL@ F. ;
[THEN]

: D? ( a -- )   D@ . ;

: 1? ( a -- )   1@ . ;

: B? ( n a -- )   2DUP B@ S.  SPACE ;
[THEN]
