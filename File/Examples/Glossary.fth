\ Glossary.fth - Sample FILE application for documenting Forth words

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

\ Documentor help

: HELP ( S: -- ) ( G: This is the  GLOSSARY  help screen )
CR PAGE ." HELP                 To display this  GLOSSARY  application"
     CR ."    This utility uses a disk file named Glossary.dbf"
     CR ." SUMMARY              Displays all words in the file."
     CR ." /VOCABULARY _____    Displays all words in vocabulary _____ ."
     CR ." VOCAB _____          Specifies vocabulary for entries & searches"
     CR ." n SOURCE 2 nC!       Specifies source block  n  for entries."
     CR ." ENTER _____          Enters  ____  making it current."
     CR ."    Thereafter use  U  to enter descriptive text."
     CR ." FIND _____           Locates and shows _____ making it current."
     CR ." F                    Re-displays the current word."
     CR ." n AT                 Changes the current word's source to  n ."
     CR ." STACKS               Enter 'input' and 'output' stack arguments."
     CR ." l T  ( or  t)        Types line  l (0-n)  making it current."
     CR ." P  ( or  p ) _____   Puts  _____  on the current line."
     CR ." U  ( or  u ) _____   Inserts  _____  under the current line."
     CR ." The date used for today's entries will be "  DATE
; HELP

\ The file structure is as follows:

\ An ordered index file, (GLOSSARY) , contains the keys by which the
\ main sequential file, GLOSSARY , is  accessed.  The key is made up of
\ the name of the word and the vocabulary in which it is defined, and is
\ stored in the field named NAME+VOC .  The link field of records in
\ (GLOSSARY) contain the record number of the start of a chain of
\ records in GLOSSARY .

\ The first or head record of the chain contains the name and header
\ information for the word.  The header consists of the vocabulary name
\ in which the word is defined ( NAME ) , the input and output stack
\ effects as ASCII strings ( BEFORE and AFTER ) , the block in which the
\ word is defined ( SOURCE ) and the date on which the word was entered
\ or last modified ( ENTERED ) .

\ The link field of the header record contains either the record number
\ of the next record in the chain or the -1 which marks the end of a
\ chain.

\ Each record in the chain contains one line of text which may be used
\ to describe the operation of the word.  Any number of records may be
\ chained to the head, which allows for unlimited text if it is
\ necessary, but also does not impose huge amounts of unused space if
\ the text is brief.

\ This is a good example of how Forth achieves the flexibility of
\ "variable length records" without the overhead imposed by standard
\ implementations.

\ Be sure to enter VOCAB and SOURCE information before proceeding with
\ the entry of words.  These fields are assumed to contain valid
\ information at entry time.

\ Glossary.dbf is the file for the GLOSSARY utility.  It is
\ provided with the system and both of the GLOSSARY files are defined
\ within this file.

FILE Glossary.dbf   FILE= Glossary.dbf

( Bytes  records  origin             name )
   28      432    0       BLOCK-DATA (GLOSSARY)
   64     2300   +ORIGIN  BLOCK-DATA GLOSSARY

4 ( LINK )  24 BYTES NAME+VOC  DROP   ( Index )

4 ( LINK )  12 BYTES NAME
            12 BYTES VOC
            16 BYTES BEFORE
            16 BYTES AFTER
             NUMERIC SOURCE
             NUMERIC ENTERED  DROP

4 ( LINK )  60 BYTES PHRASE  DROP

: (FIND) ( -- n a ) \ Usage: (FIND) <name>
   BL TEXT NAME S!  (GLOSSARY) NAME+VOC ;

: (ENTER) ( -- ) \ Usage: (ENTER) <name>
   (FIND) -BINARY IF
      SAVE  GLOSSARY SLOT  DUP HEAD !  LINK !  RESTORE
      NAME+VOC S@  +ORDERED  NAME+VOC S!
      GLOSSARY FIRST  NAME+VOC B!
      SOURCE 2 nC@  SOURCE N!
   ELSE  ORDERED RELEASE  LINK L@  GLOSSARY READ
   THEN  @DATE ENTERED N! ;

: VOCAB ( -- )   BL TEXT  VOC S! ;

: (SHOW) ( -- )
   FIRST  NAME ?B  VOC ?B  BEFORE ?B  AFTER ?B
   SOURCE ?N  ENTERED NU@  .M/D/Y  BEGIN
      +L  -NEXT 0= WHILE
         10 SPACES  PHRASE B?
   REPEAT ;

: GLOSS ( n -- t )
   (GLOSSARY) READ  LINK L@ DUP 0> IF
      HEAD !  GLOSSARY FIRST TRUE
   ELSE  DROP 0  THEN ;

[R                         FORTH GLOSSARY\ NAME        \VOCABULARY  \INPUT           \OUTPUT          \BLK\      ENTERED]
   CONSTANT TITLE

VARIABLE #L

\ Glossary Maintenance

\ F re-displays the current word.

: F ( -- )    TITLE HEADING  FIRST (SHOW)  1 #L ! ;

\ STACKS prompts user for 'input' and 'output' stack arguments.

: STACKS ( -- )
   FIRST  CR  ." Before: "  BEFORE ASK
   CR ." After: "  AFTER ASK  F ;

\ ENTER enters a name making it current.  It prompts the user for stack
\ effects, which may be entered as any string of up to 16 characters.

: ENTER ( -- ) \ Usage: ENTER <name>
   (ENTER)  STACKS  CR  64 SPACES  0 #L ! ;

: DELETE ( -- )
   (FIND)  BINARY   ORDERED GRAB  -ORDERED
   GLOSSARY  BEGIN
      READ  LINK 0 SNATCH  DUP 0<
   UNTIL  DROP ;

\ AT changes the current word's source to n.

: AT ( n -- )   FIRST  SOURCE N! ;

\ T types line n making it current.  When using T , note that lines
\ count from 0.  Always use T before X or P .

: T ( n -- )
   1+ DUP -LOCATE ABORT" Not there"  #L !  CR
   2 SPACES  PHRASE B? ;

\ P puts text on the current line.

: P ( -- ) \ Usage: P text
   1 TEXT  PHRASE B! ;

\ U inserts text under the current line.  After ENTERing a new word, use
\ U to enter its first descriptive line.  You cannot delete the last
\ remaining descriptive line, but you can change it.  All others may be
\ deleted.

: U ( -- ) \ Usage: U text
   #L @  CHAIN P  1 #L +! ;

: X ( -- )   #L @ 1- UNCHAIN ;

\ FIND locates and shows name making it current.

: FIND ( -- ) \ Usage: FIND <name>
   (FIND) BINARY HEAD !  GLOSSARY F ;

: MOVED ( s d -- )
   SWAP  (GLOSSARY) RECORDS DO
      I GLOSS DROP  SOURCE N@ OVER = IF
         OVER SOURCE N!
   THEN  LOOP  2DROP ;

\ SUMMARY displays all words in the file.

: SUMMARY ( -- )
   TITLE LAYOUT  (GLOSSARY) RECORDS DO
      I GLOSS IF
         +L (SHOW)
   THEN  LOOP  SPACE ;

\ /VOCABULARY displays all words in vocabulary.

: /VOCABULARY ( -- ) \ Usage: \VOCABULARY <name>
   VOCAB  TITLE LAYOUT  (GLOSSARY) RECORDS DO
      I GLOSS IF
         VOC  SWAP OVER ADDRESS -TEXT 0= IF
            +L (SHOW)
   THEN  THEN  LOOP  SPACE ;

