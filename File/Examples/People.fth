\ People.fth - People example for the Database Package

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

INCLUDE ../../File.fth  files  TRUE REVERSE !  LITTLE-ENDIAN

\ 2.2   CREATING A SIMPLE FILE

\ Step 1
\ Define the fields:
0  20 BYTES NAME   20 BYTES STREET
   14 BYTES CITY    2 BYTES STATE
    6 BYTES ZIP    14 BYTES PHONE   DROP

\ Step 2
\ Determine how many records and blocks the file will need

\ Step 3
\ Define the file:
FILE People.dbf   FILE= People.dbf

74 500 400 BLOCK-DATA PEOPLE

\ Step 4
\ Initialize the file.

\ PEOPLE INITIALIZE

\ Step 5
\ Enter data.

\ Here is the definition of a word that will allow data entry for a single record (person):
: enter   PEOPLE  SLOT READ
    CR ." Name? "     NAME ASK
    CR ." Address? "  STREET ASK
    CR ." City? "     CITY ASK
    CR ." State? "    STATE ASK
    CR ." Zip? "      ZIP ASK
    CR ." Phone? "    PHONE ASK ;

\ Step 6
\ Display the data.

\ We define the following word to display the current record:
: person   CR NAME B?  CR STREET B?  CR
   CITY B?  STATE B?  ZIP B?  PHONE B? ;

\ To display the contents of all records that have been entered, we define:
: everyone   PEOPLE RECORDS DO CR
   I READ  person  LOOP ;

0 [IF] \ Here is a sample of the output of everyone:
Andrews, Carl
1432 Morriston Ave.
Parkerville PA 17214 (717) 555-9853

Boehning, Greg
POB 41256
Santa Cruz CA 95061 (408) 666-7891

Chapel, Doug
75 Fleetwood Dr.
Rockville MD 20852 (301) 777-1259

Cook, Dottie
154 Sweet Rd.
Grand Prairie TX 75050 (214) 642-0011
[THEN]

\ Step 7
\ Add report generator

: .person   NAME ?B  STREET ?B  CITY ?B
   STATE ?B   ZIP ?B ;

[R                     People\Name          \Address            \City         \St.\Zip ]
   CONSTANT PEOPLE-TITLE

: all   PEOPLE-TITLE LAYOUT  +L
      PEOPLE RECORDS DO  I READ  .person
   +L  LOOP ;
