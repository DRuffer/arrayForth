\ Accounts.fth - Accounts example for the Database Package

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

( Accounts example)

FILE Accounts.dbf   FILE= Accounts.dbf

76 500 0 BLOCK-DATA ACCOUNTS

0  10 BYTES NAME   NUMERIC ACCT#   DOUBLE BALANCE

: (.$) ( d - a n)   SWAP OVER DABS
    <#  # #  46 HOLD  #S  SIGN  #> ;
: .ACCOUNT   ACCT# ?N  NAME ?B
    BALANCE D@ (.$) RIGHT ;

[R Account Balances\   Account#\Name          \Balance]
   CONSTANT ACCOUNTS-TITLE

: balances   ACCOUNTS-TITLE LAYOUT
    ACCOUNTS RECORDS DO  I READ  .ACCOUNT  LOOP ;

: enter ( n d)   ACCOUNTS  SLOT READ  BALANCE D!
    ACCT# N!  NAME PUT ;

( Example:  456 100.00 enter John Doe <RETURN> )

0 [IF] \ Example output
Page 1  05/12/2005    
Account Balances 
   Account# Name           Balance 
        456 John Doe        100.00  
        489 Mary Smith     2970.00  
        620 Ed Poore          2.59
[THEN]
