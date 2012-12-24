\ Personnel.fth - Personnel example for the Database Package

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

INCLUDE ../../File.fth  files  FALSE REVERSE !  BIG-ENDIAN

: Help ( -- )
    CR ." HELP          Display these PERSONNEL instructions."
    CR ." enter name    Enter a new person into the file with"
    CR ."                   access key of 'name'."
    CR
    CR ." remove name   Delete 'name' from the data base."
    CR
    CR ." fix name      Enter new information replacing all"
    CR ."                   current data for 'name'."
    CR
    CR ." see name      Display a person whose key is 'name'."
    CR
    CR ." s             Display current person."
    CR
    CR ." all           Display all records in the file."
    CR ;

FILE Personnel.dbf   FILE= Personnel.dbf

( Bytes  records   origin           name   )
    16      300        0 BLOCK-DATA (PERSONNEL)
   128      300  +ORIGIN BLOCK-DATA  PERSONNEL

\ The record layout for both the PERSONNEL and (PERSONNEL)
\    files.  The LINK is predefined, and subsequent
\    fields are offset from the previous 4 fields.
\    For example, the NICK name is 14 bytes long
\    starting in the 5th byte.
\ ZIP  is a 32-bit number, as is PHONE.
\ AREA  code is single precision.

  4 ( LINK)
 12 BYTES NICK     ( Nickname, used as the key.)
 32 BYTES NAME     ( Full name, first name first.)
 32 BYTES STREET   ( Street addr. or PO Box, etc.)
 32 BYTES CITY
    LONG ZIP       ( Note:  can only handle US zips)
    NUMERIC AREA
    LONG PHONE

\ The offset for the field types is carried on the
\    stack so that it may be either displayed or 
\    dropped at the end of the load.  We use it in
\    this case to display the record size.

CR .( Main file: ) .  .( Bytes )

( Data storage)

\ PERSON  parses the input stream following it for the
\    NICK field.  It leaves us pointing at the NICK 
\    field in the (PERSONNEL) file.

: PERSON ( - n a)   1 TEXT  NICK S!  (PERSONNEL) NICK ;

\ DIGITS  Prompts the terminal for input and converts 
\    it to binary on the stack.

: DIGITS ( - n)   QUERY  32 WORD COUNT  0 0 2SWAP
    BEGIN  >NUMBER  DUP
    WHILE  1 /STRING
    REPEAT  2DROP DROP ;

\ !LABEL  Prompts for each field in order.

: !LABEL   CR ." Name: "   NAME ASK
    CR   ." Street: "   STREET ASK
    CR   ." City, State: "   CITY ASK
    CR   ." Zip: "   DIGITS ZIP L!
    CR   ." Area: "   DIGITS AREA N!
         ." Phone: "   DIGITS PHONE L! ;

( Record management)

\ enter  creates a new entry for the person whose
\    nickname follows in the input stream, prompting 
\    for entry of additional data. If there is already
\    an entry for that nickname, an error message is
\    issued.  In either case, the record remains the
\    current one for future editing. 

: enter   PERSON -BINARY IF  SAVE  PERSONNEL SLOT DUP  
      READ  NICK S@ NICK B!  RESTORE  DUP LINK !
      +ORDERED  PERSONNEL READ !LABEL 
      ELSE  ORDERED RELEASE  1 ABORT" Already known "
   THEN ;

\ fix  accepts new data for the pre-existing entry
\    whose nickname follows in the input stream.

: fix   PERSON BINARY  PERSONNEL READ !LABEL ;

\ remove  deletes the person whose nickname follows
\   from the data base.

: remove   PERSON  BINARY -ORDERED  PERSONNEL SCRATCH ;

( Data Display)

\ .PHONE  displays the AREA and PHONE numbers as one 
\    would expect to see them.

: .PHONE   AREA N@ 0  <# 41 HOLD # # #  40 HOLD #>
   TYPE SPACE  PHONE L@ 0 <# # # # #  45 HOLD ( -)
   # # # #> TYPE ;

\ .ZIP  forces the zip code to be displayed in
\    nnnnn format.

: .ZIP   ZIP L@ 0 <# # # # # # #>  TYPE ;

\ n .PERSON  displays the data from the nth record in
\    the PERSONNEL data file.

: .PERSON ( n)   PERSONNEL READ  CR  NAME B?  5 SPACES
   ." (" SPACE   NICK B?  ." )"  CR   STREET B? 
   CR  CITY B? CR   .ZIP  10 SPACES  .PHONE  SPACE ; 

\ see  Parses the input stream and displays the proper
\    record.  s does the same thing using R#  
\    (the current record).

: see   PERSON BINARY  .PERSON ;

: s   R# @ .PERSON ;

\ all  uses the RECORDS word which returns the 
\    initial value and number of records+1 in the  
\    data file.  The loop counter is used to access
\    each record in the ordered index (PERSONNEL),
\    where the LINK field points to the data in the 
\    PERSONNEL file.

: all   (PERSONNEL) RECORDS DO  I (PERSONNEL) READ  
   LINK L@ .PERSON  CR  LOOP  SPACE ;

0 [IF] \ Here is a sample of the output of all:
Andrews, Carl      ( Carl )
1432 Morriston Ave. 
Parkerville, PA 
17214          (717) 555-9853 

Cook, Dottie      ( Dot )
154 Sweet Rd. 
Grand Prairie, TX 
75050          (214) 642-0011 

Chapel, Doug      ( Doug )
75 Fleetwood Dr. 
Rockville, MD 
20852          (301) 777-1259 

Boehning, Greg      ( Greg )
POB 41256 
Santa Cruz, CA 
95061          (408) 666-7891 
[THEN]
