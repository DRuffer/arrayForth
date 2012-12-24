\ csvParser.fth - Compile time Comma Separated Value (CSV) parser

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

[defined] csvParser.version 0= [IF]

forth definitions 2 constant csvParser.version files

[defined] Verbose 0= [IF] 0 value Verbose [THEN]

0 VALUE csvSource           \ Holds handle of file being parsed
0 VALUE csv#Field           \ Holds index of csv field we are working on
0 VALUE csv#Record          \ Holds csv record number we are working on
0 VALUE csvContinued        \ Holds true if the current line is continued

CREATE csvLine   HERE #TB 2 + DUP ALLOT ERASE   \ Holds a source line

VARIABLE doLine             \ Action to perform on each line

: Show-csv-Line ( -- )   csvLine CR COUNT TYPE ;
' Show-csv-Line doLine !

: Read-csv-Line ( handle -- flag )
    csvContinued 0= IF  0 csvLine C!
    THEN  csvLine COUNT TUCK + #TB 1+ ROT - BLANK
    csvLine COUNT TUCK + #TB 1- ROT - ROT
    READ-LINE ABORT" Can't read file "
    SWAP csvLine C@ + csvLine C! ;

: Parse-csv-File ( str len -- )
    R/O OPEN-FILE ABORT" Can't open file " TO csvSource
    BEGIN  csvSource Read-csv-Line
    WHILE  Verbose $10 AND
        IF  Show-csv-Line
        THEN  doLine @ EXECUTE
    REPEAT  csvSource CLOSE-FILE ABORT" Can't close file " ;

: List-csv-File ( str len -- )
    ['] Show-csv-Line doLine !
    Parse-csv-File CR ;

\ See http://www.creativyst.com/Doc/Articles/CSV/CSV01.htm
: csvField ( str len -- true | str2 len2 str1 len1 false )
    OVER C@ [char] " = DUP
    IF  TO csvContinued
        2DUP 1 /STRING 0 -ROT BOUNDS
        DO  I C@ [char] " =
            IF  I 1+ C@ [char] " <>             \ I do not remove the double quotes
                I 1- C@ [char] " <> AND
                IF  FALSE TO csvContinued
                    DROP I  LEAVE
                THEN
            THEN
        LOOP  csvContinued DUP >R
        IF  DROP 1+ csvLine place               \ I assume EOL is 1 character
        ELSE  ROT 1+ SWAP OVER -                ( len str1 len1 )
            ROT OVER - 2 - >R
            2DUP + 1+ R>                        ( str1 len1 str2 len2 )
            OVER C@ [char] , =
            IF  1 /STRING
            THEN  2SWAP
        THEN  R>
    ELSE  >R [char] , left-parse-string R>
    THEN ;
[THEN]
