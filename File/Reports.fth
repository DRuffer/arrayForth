\ Reports.fth - Database Report Generator

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

[defined] Reports.version 0= [IF]

forth definitions 2 constant Reports.version files

\ User Variables

#USER CELL +USER P#   \ holds the current page number of the report.
      CELL +USER RPT  \ holds the address of the current report title block.
      CELL +USER #COL \ holds the address of the current column width.
TO #USER

\ REGISTER returns the address of the beginning of the sub-total
\ registers, normally in WORKING .  2 CELLS plus 2 CELLS for each
\ total; subtotal, total & grand total, must be allotted.

: REGISTER ( -- a )   WORKING 16 + ;

\ Subtotalling

\ REG returns the address of the next CELL register, treating the
\ registers, as many as specified by  TOTALS , in a circle.

: REG ( -- a )
   REGISTER 2@ MOD  2 CELLS + DUP
   REGISTER CELL+ !  REGISTER + ;

\ TOTALS initializes the subtotaling registers, given the number of
\ 2 CELL columnar totals to be maintained.  It must be used at the
\ beginning of a report which uses this feature.

: TOTALS ( n -- )   2* CELLS REGISTER  OVER 3 * 2 CELLS + ERASE  REGISTER ! ;

\ SUM is given a 2 CELL value and a sub-total register number, adds the
\ amount to the register.

: SUM ( d n -- )   2* CELLS REGISTER + 2+! ;

\ FOOT add a value to the next register, and return a copy of the value.

: FOOT ( d -- d )   2DUP REG 2+! ;

\ SUB returns the next subtotal, leaving that register cleared and
\ adding the sum to the associated grand total.

: SUB ( -- d )
   REG  DUP >R  2@  2DUP REGISTER @ R@ + 2+!
   0 0 R> 2! ;

\ TOTAL copies the totals to the subtotals and adds them to the grand
\ totals, leaving the totals negated for SUB cycle.

: TOTAL ( -- )
   REGISTER DUP @ DUP 0 DO  >R
      2 CELLS + DUP DUP R@ + 2@  DUP 2OVER ROT ROT 2!
      DUP 2OVER ROT ROT R@ 2* + 2+!  DNEGATE ROT R@ + 2!  R>
   2 CELLS +LOOP  2DROP ;

\ GRAND copies the grand totals to the subtotals.

: GRAND ( -- )   REGISTER DUP @ >R  2 CELLS + DUP R@ 2* +  SWAP R> MOVE ;

\ Report Formatting

\ 0COL resets the column pointer to the 1st column.

: 0COL ( -- )   RPT @ @ #COL ! ;

\ +L performs a CR and increments the line counter.

: +L ( -- )   -spin  CR  0COL ;

\ COL returns the address of the next column width.

: COL ( -- n )   #COL @  DUP CELL+  #COL ! ;

\ COLS returns the next column width.

: COLS ( -- n )
   COL @  DUP 0< 0= IF
         EXIT
      THEN  DROP +L  COL @ ;

\ HEADING is given the address of a title-heading table.  It outputs the
\ title and heading, and saves the address of the table to control
\ future columnar output.

: HEADING ( a -- )
   DUP RPT !  CELL+  DUP >R  CELL+ COUNT TYPE
      R@ 3 CELLS - @ ?DUP IF EXECUTE THEN  +L
      R> @  COUNT TYPE +L ;

\ TITLE  outputs the current title/heading pair.

: TITLE ( -- )   RPT @ HEADING ;

\ NOTE: This version of the report generator compiles the header
\ information in-line so the source does not need to be resident when
\ running.  It also assumes that the source for the title source line is
\ entirely accessable in the input stream.  In other words, in a text file,
\ the entire title must be described on one line.

\ [R+ constructs a title-heading table which uses backslashes to delimit
\ columns.  It is immediately followed by the name of a word that will be
\ executed on the first header line of each page.  Formatting of table:
\
\        xp of header definition
\        xp of TITLE
\        addr of column sizes <- address returned
\        addr of column headings
\        counted string of header
\        counted string of columns
\        column size array
\        -1

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

\ Page Formatting

\ SKIP-COL skips one report column.

: SKIP-COL ( -- )   COLS 1+ SPACES ;

\ SKIPS skips a specified number of report columns.

: SKIP-COLS ( n -- )   0 DO  SKIP-COL  LOOP ;

\ RIGHT types a string right-justified in the next report column.
\ Used like TYPE .  Default for numeric fields.

: RIGHT ( a n -- )   COLS OVER -  SPACES TYPE  SPACE ;

\ LEFT types a string left-justified in the next report column.
\ Used like TYPE .  Default for text fields.

: LEFT ( a n -- )   COLS OVER - >R  TYPE  R> 1+ SPACES ;

\ CENTER types a string centered in the next report column.
\ Used like TYPE .

: CENTER ( a n -- )
   COLS OVER - DUP 2/  DUP >R - SPACES
   TYPE  R> 1+ SPACES ;

VARIABLE 'PAGE   ' PAGE 'PAGE !

\ +PAGE is the new-page function for the report generator.

: +PAGE ( -- )
   'PAGE @ EXECUTE  1 P# +!  +L  ." Page "  P# ?
   SPACE  DATE  SPACE  'APP @ COUNT TYPE
   +L  RPT @  CELL - @ ?DUP IF EXECUTE THEN ;

\ LAYOUT specifies that the title-heading table whose address is given
\ is the one for this report, and initializes the first page of the
\ report.

: LAYOUT ( a -- )   RPT !  0 P# !  +PAGE ;

\ Report Generator Output

: (D.) ( d -- a n )
   SWAP OVER DUP 0< IF DNEGATE THEN
   <#  #S ROT SIGN  #> ;

\ .N outputs a 16-bit number in the next report column.

: .N ( n -- )   DUP 0< (D.) RIGHT ;

\ .L outputs a 32-bit number in the next report column.

: .L ( n -- )   DUP 0< (D.) RIGHT ;

[DEFINED] SFALIGN [IF]
\ .FL outputs a floating point number in the next report column.

: .FL ( F: r -- )   PAD 4 REPRESENT IF  <#
        >R 1- DUP ABS 0 #S 2DROP SIGN  [CHAR] e HOLD        \ exponent
        3 0 DO  PAD 3 + I - C@ HOLD  LOOP  [CHAR] . HOLD
        PAD C@ HOLD  R> IF  [CHAR] - HOLD  THEN  0 0 #>
    ELSE  S" ?.?e?"  THEN  RIGHT ;
[THEN]

\ .D outputs a 64-bit number in the next report column.

: .D ( d -- )   (D.) RIGHT ;

\ ?N fetches the contents of a specified NUMERIC field and outputs it in
\ the next report column.

: ?N ( a -- )   N@ .N ;

\ ?L fetches the contents of a specified LONG field and outputs it in
\ the next report column.

: ?L ( a -- )   L@ .L ;

[DEFINED] SFALIGN [IF]
\ ?FL fetches the contents of a specified FLOAT field and outputs it in
\ the next report column.

: ?FL ( a -- )   FL@ .FL ;
[THEN]

\ ?D fetches the contents of a specified DOUBLE field and outputs it in
\ the next report column.

: ?D ( a -- )   D@ .D ;

\ ?1 fetches the contents of a specified 1BYTE field and outputs it in
\ the next report column.

: ?1 ( a -- )   1@ .N ;

\ ?S outputs a specified BYTES field from FILE-PAD .

: ?S ( n a -- )   (S.) LEFT ;

\ ?B fetches the contents of a specified BYTES field and outputs it in
\ the next report column.

: ?B ( n a -- )
   2DUP B@  OVER FILE-PAD SWAP 4 MIN nC@ IF
      ?S  ELSE  2DROP SKIP-COL
   THEN ;

\ .M/D/Y outputs a Julian date in the next report column.

: .M/D/Y ( n -- )
   ?DUP IF
      MM/DD/YYYY RIGHT  ELSE  SKIP-COL
   THEN ;

\ .D-M-Y outputs a Julian date in the next report column.

: .D-M-Y ( n -- )
   ?DUP IF
      DD-MMM-YYYY RIGHT  ELSE  SKIP-COL
   THEN ;

\ .WHEN prints the given time in the next report column.

: .WHEN ( n -- )
   ?DUP IF
      BASE @ >R  0  <#
      DECIMAL #  6 BASE ! #  [char] : HOLD
      DECIMAL #  6 BASE ! #  [char] : HOLD
      DECIMAL # #  #>  R> BASE !
      RIGHT  ELSE  SKIP-COL
   THEN ;
[THEN]
