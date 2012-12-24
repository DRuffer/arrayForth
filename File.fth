\ File.fth - Simple database support using files

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

\ This utility is required by the GLOSSARY and any application that needs
\ this organized method of managing files. If most tasks use it, it should
\ be loaded in the system electives.  Then the applications should begin
\ with EMPTY instead of FILE-TASK MARKER FILE-TASK .

\ Compatibility words
\
\ The following definitions are required by the FILE system, but are not
\ considered part of it.  They may all, eventually, be moved to the
\ supporting Forth system.

[defined] 2+! 0= [IF] : 2+! ( d a -- )   DUP >R 2@ D+ R> 2! ; [THEN]

: prepend ( from len to -- )   dup count  dup 1+ allocate throw  dup >r place
    dup >r place  r> r@ count rot append  r> free throw ;

: ,string ( str len -- a )   HERE DUP >R OVER 1+ ALLOT place R> ;
: $c, ( str len -- )   OVER + SWAP ?DO  I C@ C,  LOOP ;

: null? DUP 0= IF NIP DUP THEN ;

: left-parse-string ( str len char -- rstr rlen lstr llen ) \ IEEE 1275 parser from left
    OVER IF
        >R 2DUP R> ROT ROT OVER + SWAP 2DUP = IF  2DROP  ELSE  DO
            DUP I C@ = IF
                DROP 2DUP + I 1+ SWAP OVER - null?      ( rstr rlen | 0 0 )
                2SWAP DROP I OVER - null?           ( lstr llen | 0 0 )
                UNLOOP EXIT  THEN
        LOOP THEN  DROP 0 0 2SWAP
    ELSE  DROP 0 0 2SWAP  THEN ;

\ ***   The following are used to fetch data that is in known Endian format.  E.g.
\ *** in file system structures or network packets.  These words work on un-aligned
\ *** entities.

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

\ @LINK  and  !LINK  do relocation translation, if needed.

: @LINK ( a -- a )   [defined] @REL [IF]
        @REL  [ELSE]  @  [THEN] ;
: !LINK ( a1 a2 -- )   [defined] !REL [IF]
        !REL  [ELSE]  !  [THEN] ;
: ,LINK ( a -- )   [defined] ,REL [IF]
        ,REL  [ELSE]  ,  [THEN] ;

\ LINKS  searches the linked list until it finds the last entry
\    in the list (the one with a 0 link).

: LINKS ( a -- a' )   BEGIN  DUP @LINK ?DUP WHILE  NIP  REPEAT ;

\ >LINK  adds the top of the dictionary to the given linked list.

: >LINK ( a -- )   ALIGN HERE  OVER @LINK ,LINK  SWAP !LINK ;

\ <LINK  adds the top of the dictionary to the end of the given
\    linked list.

: <LINK ( a -- )   LINKS  >LINK ;

\ UNLINK  breaks the link of the given entry, resetting it to the link
\ pointed to by the element on top of the stack.

: UNLINK ( a a' -- a )   @LINK OVER !LINK ;

\ CALLS  runs down a linked list, executing the high level code
\    that follows each entry in the list.

: CALLS ( a -- )
   BEGIN
      @LINK ?DUP WHILE
      DUP >R  1 CELLS + @LINK EXECUTE  R>
   REPEAT ;

: pfDatabaseHelp ( -- )
    s" This plug in is an implementation of the polyFORTH Data Base" TYPE CR
    s" Support package converted to run on ANS Forth systems.  It is" TYPE CR
    s" documented separately in a 1275 Binding for a Database Package" TYPE CR
    s" but that binding was never actually published.  It is the last" TYPE CR
    s" time that I worked on the documentation with Elizabeth Rather" TYPE CR
    s" with the hope to make this tool public, but the effort died" TYPE CR
    s" when Apple switched to Intel.  So, we are free to try again." TYPE CR
        CR
    s" While this could be called an ANS Forth Application, you are" TYPE CR
    s" not left with an ANS Forth System after it loads, due to many" TYPE CR
    s" naming conflicts.  So, I have placed it into a \begin{bf}files-wordlist\end{bf}" TYPE CR
    s" that is made current by the word \begin{bf}files\end{bf}.  Thus," TYPE CR
    s" all of the naming conflicts, and most of the application usages" TYPE CR
    s" are buried away from normal user interaction.  Only those words" TYPE CR
    s" which are considered to be application interface words should" TYPE CR
    s" be exposed to the user.  The developer, however, will need to" TYPE CR
    s" execute \begin{bf}files\end{bf} before the words in this plug" TYPE CR
    s" in can be used." TYPE CR ;

: SortHelp ( -- )
    s" While many application databases are kept entirely in application" TYPE CR
    s" specific locations, some must be sorted before they can be used." TYPE CR
    s" This sorting uses a temporary disk file called TEMP.DBF in the data" TYPE CR
    s" folder which is a peer to the folder that your application is loaded" TYPE CR
    s" from.  You will generated an error if this folder does not exist." TYPE CR
    s" Plug ins typically create this folder for you, but you may need to" TYPE CR
    s" create it yourself if your environment does not make this possible." TYPE CR ;

wordlist constant DBfiles-wordlist

: files   forth-wordlist  DBfiles-wordlist
      2 set-order  definitions ;  immediate
files

[defined] spin    0= [IF] : spin ; IMMEDIATE [THEN]
[defined] -spin   0= [IF] : -spin ; IMMEDIATE [THEN]

[defined] GET 0= [IF] : GET DROP ; [THEN]
[defined] GRAB 0= [IF] : GRAB DROP ; [THEN]
[defined] RELEASE 0= [IF] : RELEASE DROP ; [THEN]

256 VALUE #TB       \ #TB is the size of the text input buffer.

CREATE FILE-PAD   #TB ALLOT     \ Make a PAD for our exclusive use

\ TEXT takes a delimiter and parses the input stream, moving the string
\ up to the delimiter or the end of the string to FILE-PAD .

: >TEXT ( a n -- )   FILE-PAD #TB BLANK  FILE-PAD SWAP MOVE ;
: TEXT ( c -- )   WORD COUNT  >TEXT ;

\ REVERSE  when true >MOVE< swaps bytes in each 16 bit word moved. This
\ is necessary on byte-reversed processors such as the 80386, so that
\ text data may be in natural order on disk for compatibility with other
\ processors.  Byte fields are in internal form at FILE-PAD , but
\ byte-swapped in WORKING .  REVERSE ON establishes the default.

VARIABLE REVERSE   ( Default ) FALSE REVERSE !

\ KEEP-CLOSED  when false, file handles are never explicitely closed
\ unless a new file name is bound to a file.  When true, file handles
\ are closed after each major operation.

VARIABLE KEEP-CLOSED   ( Default ) FALSE KEEP-CLOSED !
[defined] ForTimbre [IF]  TRUE KEEP-CLOSED !  [THEN]

\ >MOVE< is a version of MOVE that may swap bytes.

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

\ -TEXT takes the address of two strings and a length and returns +1 if
\ the 1st string has a higher ASCII value, -1 if it has a lower value,
\ and 0 (false) if they are equal.  The string length must be even.  If
\ the length is odd, it will be rounded down to the next even value.

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

\ Modified Julian Date

\ Dates in the range 01 Jan 1900 through 05 Jun 2079 may be input and
\ output converted, although dates in the first two months of 1900 are
\ not consistent with the rest of the calendar because 1900 was not a
\ leap year.

\ D/Y is the number of days per year for a four-year period.

365 4 * 1+ CONSTANT D/Y

\ DAYS is the lookup table of total days in the year at the start of
\ each month.  @MTH  returns the value from days for the given month.

CREATE DAYS   -1 ,  0 ,  31 ,  59 ,  90 ,  120 ,  151 ,
   181 ,  212 ,  243 ,  273 ,  304 ,  334 ,  367 ,

: @MTH ( u1 -- u2 )   CELLS DAYS + @ ;

\ D/M/Y converts day, month, year into MJD.

: D-M-Y ( d m y -- u )   >R  @MTH
   58 OVER < IF  R@ 3 AND 0= - THEN + 1-
   R> 1900 -  D/Y UM*  4 UM/MOD SWAP 0<> - + ;

VARIABLE YEAR   \ YEAR  holds the current year.  A.D.  sets it.

: A.D. ( n -- )   YEAR ! ;   TIME&DATE A.D. 2DROP 2DROP DROP

\ MTH  defines words that convert date to internal form in month
\ of the same name using year in  YEAR .

: MTH ( n _ -- )   CREATE , DOES> ( d -- n )   @ YEAR @ D-M-Y ;

1 MTH JAN       2 MTH FEB       3 MTH MAR       4 MTH APR
5 MTH MAY       6 MTH JUN       7 MTH JUL       8 MTH AUG
9 MTH SEP      10 MTH OCT      11 MTH NOV      12 MTH DEC

\ M/D/Y takes a double number mm/dd/yyyy and converts it to MJD.

: M/D/Y ( ud -- u )   10000 UM/MOD  100 /MOD  ROT D-M-Y ;

\ Y-DD and DM split the serial number back into its components.

: Y-DD ( u1 -- y u2 u3 )   4 UM* D/Y  UM/MOD 1900 +  SWAP 4 /MOD 1+
   DUP ROT 0= IF  DUP 60 > +  SWAP DUP 59 > +  THEN ;

: DM ( u1 u2 -- d m )   1 BEGIN  1+  2DUP @MTH > 0= UNTIL  1-
   SWAP DROP SWAP  OVER @MTH - SWAP ;

CREATE MTHS   s" JanFebMarAprMayJunJulAugSepOctNovDec" $c,

\ DD-MMM-YYYY  converts internal form to text formatted as dd mmm yyyy.

: DD-MMM-YYYY ( n -- a n )   BASE @ >R  DECIMAL  Y-DD
   ROT 0 <#  # # # #  2DROP  BL HOLD  DM 1- 3 * MTHS +
   3 + 3 0 DO  1- DUP C@ HOLD  LOOP  DROP
   BL HOLD  0 # #  #>  R> BASE ! ;

\ MM/DD/YYYY converts internal form to text formatted as mm/dd/yyyy.

: MM/DD/YYYY ( u1 -- c-addr u2 )   BASE @ >R  DECIMAL  Y-DD
   ROT 0 <#  # # # #  2DROP  [char] / HOLD  DM SWAP
   0 # #  2DROP   [char] / HOLD  0 # #  #>  R> BASE ! ;

VARIABLE DATE-FORMAT   ' MM/DD/YYYY DATE-FORMAT !

\ .DATE displays the system date (u) as mm/dd/yyyy.

: .DATE ( u -- )   DATE-FORMAT @ EXECUTE TYPE SPACE ;

\ @DATE gets the current system date.

: @DATE ( -- n )   TIME&DATE D-M-Y NIP NIP NIP ;

: DATE ( -- )   @DATE .DATE ;

: :00 ( ud1 -- ud2)   DECIMAL  #  6 BASE !  # [char] : HOLD ;

: (TIME) ( secs -- c-addr u)   BASE @ >R  0 <#  :00 :00
   DECIMAL # #  #>  R> BASE ! ;

: .TIME ( secs -- )   (TIME) TYPE SPACE ;

\ @TIME gets the current system time.

: @TIME ( -- secs )   TIME&DATE 2DROP DROP 60 * + 60 * + ;

: TIME ( -- )   @TIME .TIME ;

VARIABLE 'TITLE     \ holds the address of the counted string used for listings.

\ TITLE" defines the string to be used for listings.  When its children
\ are executed, they leave the string address in  'TITLE .

: TITLE" ( -- ) \ Usage: TITLE" text"
   CREATE  HERE 'TITLE !  ,"
   DOES> ( -- )   'TITLE ! ;

VARIABLE 'COMPANY   \ holds the address of the counted string used for report titles.

\ COMPANY" sets the company name used for report titles.

: COMPANY" ( -- ) \ Usage: COMPANY" text"
   HERE 'COMPANY !  ," ;

COMPANY" IntellaSys, Inc."

VARIABLE 'APP       \ holds the address of the application title.

\ APP" sets the application name used for screen titles.

: APP" ( -- ) \ Usage: APP" text"
   HERE 'APP !  ," ;

APP"  "

[defined] #USER 0= [IF]  0 VALUE #USER  [THEN]
[defined] +USER 0= [IF]
    : +USER ( o n _ -- o+n )   CREATE DUP ALLOT + ;
[THEN] [THEN]

INCLUDE File/Support.fth   \ DataBase Support System
INCLUDE File/Reports.fth   \ Report Generator
INCLUDE File/Struct.fth    \ Structured files
INCLUDE File/Memory.fth    \ Memory based data
INCLUDE File/Index.fth     \ Ordered Index
INCLUDE File/Sort.fth      \ Field sorting
INCLUDE File/csvParser.fth \ Comma Separated Value (CSV) parser

FORTH DEFINITIONS
