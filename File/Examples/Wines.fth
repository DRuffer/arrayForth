\ Wines.fth - Wine Inventory example for the Database Package

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

( Totals and Subtotals)

FILE Wines.dbf   FILE= Wines.dbf

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

: enter ( Cablis Rose Champagne -- )   WINES  SLOT READ
    Champagne N!  Rose N!  Chablis N!  Location PUT ;

( Example:  25 42 78 enter Palo Alto <RETURN> )

0 [IF] \ Example output
Page 1  05/17/2005    
Wine Inventory by Store 
Location         Chablis   Rose  Champagne 
 
Northern California 
Palo Alto             25     42         78  
San Jose              16     32         50  
Mill Valley           31     29         36  
San Francisco         70     59         82  
                     142    162        246  
 
Southern California  
Chatsworth            35     48         29  
Woodland Hills        32     40         60  
                      67     88         89  
Grand Total:         209    250        335  
[THEN]
