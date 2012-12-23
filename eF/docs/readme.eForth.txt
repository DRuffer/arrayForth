eForth
------
eForth was developed as a tool to port Forth to a new processor,
some of them with very limited resources. It uses a compact version
of threaded code called 'bit-threaded code' (BTC) to implement
a virtual Forth machine.

The newest processor is the Green Arrays, Inc. GA144, with 144
processor 'nodes', each of which is an 18-bit cpu with 64 words
of RAM, 64 words of ROM, 10-deep data and 9-deep return stacks,
two address registers, and up to four addressable neighbors.

This version of eForth is 16-bit cell addressed, with a single
character occupying a 16-bit cell.

The eForth virtual machine occupies five nodes:
  -- the BTC engine node, called 'bitsy', and two of its neighbor
     nodes, called 'buds'.
  -- the ALU interface and stack controller node, called 'stack',
     and one neighbor node.

The eForth iobus currently occupies fifteen nodes to access some
of the GA144's i/o pins, including the required serial terminal
controller.

eForth requires a serial terminal; its asynchronous serial
interface controller occupies two nodes.

eForth requires external memory; four nodes are used to access
external sram:
  -- user interface node
  -- data read/write node
  -- control node
  -- address node

eForth has about 30 primitive words in code, implemented on the
GA144 as arrayForth subroutines in the bitsy, stack, and bud nodes.
The remainder of eForth is made of high level calls to those
primitives.

eForth is a SUBSET of the ANS Programming Language -- Forth.
Words defined in eForth adhere to the usage described by ANS
Programming Language -- Forth.

The file   lib.e4   defines many words not in the eForth kernel.
Please add only words as needed for your application.

eForth dictionary structure
---------------------------
In this version of eForth a CELL and a CHAR are both 16-bit
values, one ASCII character per CHAR.

The dictionary is composed of inline definition headers and
execution tokens.

The word   '   (tic) returns an execution token, NOT an address.
An execution token may be changed to an address with:
  >ADR ( xt -- a )

A definition header is composed as follows:

\ name structure \link\attribute+count\name_string\[xt]
  \ 'link' pointer to previous 'name string' -- cell
  \ 'attribute+count' string length and attribute 'ican nnnn'
  \   n -- string length, 31 chars max
  \   i -- immediate
  \   c -- compile-only
  \   a -- assembler code
  \ 'name_string' variable length ascii 'ccc' -- chars
  \   compiler does not set bits in the 'name string'
  \ 0 < la na < ... < la na < last name compiled
  \ '[xt]' execution token -- cell -- code or separated heads

The wordlist structure when:
ORDER is FORTH-WORDLIST ++ FORTH-WORDLIST
and WORDLISTS are:

 FORTH-WORDLIST
 |             ASM-WORDLIST
 |             |             EMPTY-WORDLIST
 |             |             |
 v             v             v
 context @    (search first)
 | current @  (new definitions)    current cell+ @
 | |                               |
 | |   +-----------+ +-----------+ |
 | |   |           | |           | |
 v v   v           | v           | v
+---+---+     +---+---+     +---+---+
|   | 0 |     |   |   |     | 0 |   |
+---+---+     +---+---+     +---+---+
  |             |
  +---+         +---+
      v             v
+---+-----+   +---+-----+
|   |     |   | 0 |     |
+---+-----+   +---+-----+
  |
  +---+
      v
+---+-----+
| 0 |     |
+---+-----+

eForth external memory map
--------------------------
jwr pages
0x0000                          0x7fff 0xffff 0x10000
  |---h>-pad>----<s--<r\\u>-\tib>-|------|------pages>

0x0000 -- start of memory
HERE   -- Start of user dictionary space
PAD    -- Transient buffer, floats 80 characters above HERE
0x7f50 -- Data stack (builds down)
0x7f60 -- Return stack (builds down)
0x7f60 -- User task area (builds up)
0x7f70 -- Terminal input buffer, 128+2 characters
0x7fff -- End of eForth program space
0xffff -- End of page 0 memory

It is possible to edit  code.e4  to allocate more return stack
space by increasing the following equate:
  D#   128 EQU #RP \ size in cells  return stack

Utilities
---------
NUF? -- As part of utilities that may display a large amount of
        text, the word NUF? is used to throttle, pause or stop
        output:

  throttle -- press a number key, 0=fastest to 9=slowest.
  pause    -- press any alpha key.
  stop     -- while display is paused press return.

Words that use NUF? are WORDS, DUMP, DM, and SEE.

DUMP ( a u -- ) displays eight cells per line and ASCII text.

+ address
|    + page
|    |  + cell value
|    |  |                                        + ascii equivalent
|    |  |                                        |
   0:0  1324    0 1313    0    0  954  982 D040   $         T   @
   8:0  D044    0    1    2    3    0   24   45   D           $ E
  10:0    58   49   54 C00E    E   27   45   58   X I T     ' E X
  18:0    45   43   55   54   45 C029   15   23   E C U T E )   #
  20:0    52   50   40 C004   1F   63   52   50   R P @     c R P
  28:0    21 C01B   25   62   3E   52 C018   2B   !   % b > R   +
  30:0    62   52   3E C00A   30   25   43   48   b R >   0 % C H
  38:0    41   52   2B 8000   35   25   43   45   A R +   5 % C E
  40:0    4C   4C   2B 8000   3D   25   43   48   L L +   = % C H

DM ( a u -- ) displays an address, one cell value, the ASCII
  equivalent of the cell value, and if the cell value references
  a named execution token, display the token name.

SEE ( "name" -- ) a vertical decompiler.
  "name" is converted to an address, the offset to HERE is
  used for the count, then both are passed to DM.
  SEE will run continuously.
  Use the NUF? keys to slow, pause or stop.
  Usage example: see hex <return>

ok SEE HEX
+ address
|    + cell value
|    |    + ascii equivalent
|    |    |  + token name
|    |    |  |
 1EF    3
 1F0   48  H
 1F1   45  E
 1F2   58  X
 1F3 BBBB
 1F4 F038  8 _LIT
 1F5   10
 1F6   CB    BASE
 1F7 8008    AU!
 1F8 C00E    EXIT

: WORDS ( [" ccc"] -- )( 15.6.1.2465 ) list dictionary.
  Optionally  WORDS  may be followed by a search character string.
  It will match all words in all wordlists that START with the character
  string.

ok WORDS
wid=467
WARM IOS UNAQ HAND[] .VERSION COLD 'WARM FILE _FILE HAND SEE >NAME
DM NAMED? DUMP P4 >CHAR WORDS WIDWORDS VOCS ORDER .ORDER .WID .ID
.S _.S ?CSP !CSP UNUSED AT-XY PAGE NEXT REPEAT AGAIN UNTIL AFT WHILE
ELSE AHEAD IF MARK RESOLVE FOR THEN BEGIN MARKER _MARKER DEFINITIONS
PREVIOUS ALSO ONLY +ORDER -ORDER _-ORDER -ROT SET-ORDER GET-ORDER
ORDER@ WORDLIST HAT USER CONSTANT VARIABLE CREATE DOES> _DOES> ;
: :NONAME >BODY >ADR POSTPONE RECURSE REVEAL COMPILE-ONLY IMMEDIATE
LEX! HEAD, ?UNIQUE SET-CURRENT GET-CURRENT LAST ] _] ABORT" ." "S,
S" SLITERAL S, STRING, _S, IF\ .\ \ .( ( PARSE [DEFINED] [COMPILE]
['] ' [CTRL] CTRL [CHAR] CHAR LITERAL COMPILE, , C, AU, ALLOT QUIT
?THROW .OK EVALUATE PARSE-WORD SOURCE [ _[ ?STACK SFIND CURRENT CONTEXT
FORTH-WORDLIST WID? NAME> CODE? ID>AN ADR> _PARSE _DELIMIT SAME?
ACCEPT ? . U. D. .R U.R D.R S.R ?CR CR _ABORT" _." _S" _" NUF? NUF
MS MSA TYPE _TYPE SPACES EMITS SPACE EMIT C# L# _EMIT KEY ?KEY SIGN
#> #S # HOLD DIGIT <# PAD HERE NUMBER? >NUMBER DIGIT? ABORT THROW
CATCH DEPTH RELEASE GET BUILD ACTIVATE STOP SLEEP AWAKE 'S PASS WAKE
_WAKE PAUSE U2 U1 TF TOS TID FOLLOWER STATUS _USR UP UPPER -TRAILING
FILL ERASE MOVE 2@ 2! /STRING BOUNDS COUNT +! RIVE U2/ RSHIFT */
*/MOD / MOD /MOD FM/MOD SM/REM U/MOD UM/MOD LSHIFT * M* UM* WITHIN
MAX MIN < U< = 0= DABS DNEGATE D+ S>D ABS - NEGATE ?DUP 2DUP 2DROP
ROT NIP DECIMAL HEX #TIB BL SUP DP STATE CSP #IN >IN HLD DPL BASE
XCX XM@ XM! 2/ 2* NOOP ALIGNED ALIGN CHAR/ CELL/ 1- 1+ + INVERT @EXECUTE
_NEXT I R@ _ELSE _LIT _CON _VAR IO! TX IO@ RX N! N@ AND XOR OR 0<
UM+ SWAP OVER DUP DROP SP! SP@ _IF AU! C! ! AU@ C@ @ CELLS CHARS
CELL- CHAR- CELL+ CHAR+ R> >R RP! RP@ EXECUTE EXIT

ok WORDS S \ bbb


For program control, the eForth kernel has the following words:

  IF ELSE THEN AHEAD
  BEGIN UNTIL WHILE REPEAT AGAIN
  FOR AFT NEXT

Misc words non-standard words
-----------------------------
Words preceded by an "_" are internal to eForth.

RIVE ( hl -- h l )
  return a cell value shifted 8-bits and the low 8-bit value

?KEY ( -- 0 | ~c ~c )
  checks for input, used by KEY, more efficient than KEY?

C# ( -- a )
  current output column number and max column number for ?CR

EMITS ( n c -- )
 output count n of char c to the current device

?CR ( -- )
  output a CR if the current line is longer than specified by C#

S.R ( ca u n -- )
  display a string right aligned in a field n characters wide.
  If the number of characters required to display the string is
  greater than n, all characters are displayed with no leading
  spaces in a field as wide as necessary.

SAME? ( ca ca u -- f )
  compare two strings for length u, return true if same

WID? ( ca u wid -- xt lex -1 | ca u 0 )
  does the string ca u exist in the wordlist wid

SFIND ( ca u -- xt lex -1 | ca u 0 )
  does the string ca u exist in the search order

.\ ( "comment" -- )
  display the remainder of a comment line, like .(

IF\ ( f "comment" -- )
  conditionally comment the remainder of one line

-ORDER ( wid -- )
+ORDER ( wid -- )
  remove/add wid from/to search order

WORDS ( [" ccc"] -- )( 15.6.1.2465 )
  List definition names in the first wordlist of the search
  order. Optionally list only words STARTING with "ccc".

>CHAR ( u -- c )
  printable characters only, others converted to "." (dots)

NAMED? ( a -- na | 0 )
  does the address have an associated token name

Multitasker
-----------
The eForth multitasker is a cooperative round robin style, not
preemptive.

UP       ( -- a )         current task pointer
PAUSE    ( -- )           allow another task to execute
WAKE     ( -- xt )        execution token of a running task
PASS     ( -- xt )        execution token of a sleeping task
'S       ( tid a -- a )   index another task's variables
AWAKE    ( tid -- )       awake another task, set STATUS to WAKE
SLEEP    ( tid -- )       sleep another task, set STATUS to PASS
STOP     ( -- )           sleep current task, set STATUS to PASS
ACTIVATE ( tid -- )       initialize task
BUILD    ( tid -- )       link new task, do only once
GET      ( semaphore -- ) lock access to a resource
RELEASE  ( semaphore -- ) unlock access to a resource
USER     ( n "name" -- )  name user variables, n is the index
HAT      ( u s r "name" -- ( -- tid )
           define task area in ADDRESS UNITS
           u -- extra space for user variables
                (in addition to U1 and U2)
           s -- date stack depth
           r -- return stack depth

These are the default task variables:

D# 0 \ --<s--<r\\status\follower\tid\tos\tf\u1\u2
D# 0 CELLS + DUP USER STATUS   \ PASS or WAKE
D# 1 CELLS + DUP USER FOLLOWER \ address of next task's STATUS
D# 1 CELLS + DUP USER TID      \ current tid
D# 1 CELLS + DUP USER TOS      \ top of stack
D# 1 CELLS + DUP USER TF       \ throw frame
D# 1 CELLS + DUP USER U1       \ free
D# 1 CELLS + DUP USER U2       \ free
D# 2 CELLS + ABS EQU =USER     \ size of user area in cells

Basic Task Template
-------------------
0  16 CELLS  DUP HAT TASK-1   \ create a task and allocate space
                              \ no extra variables
                              \ 16 cells for each stack
TASK-1 BUILD                  \ link task

: TASK-1-GO ( -- )            \ task that increments a variable
  0  TASK-1 U1 'S  !          \ init task user variable
  TASK-1 ACTIVATE             \ start task
  BEGIN                       \ task loop
    PAUSE                     \ pause to allow other tasks
    1 U1 +!                   \ increment variable U1
  AGAIN ;

: .U1 ( -- )
  TASK-1 U1 'S @ . ;          \ display the current value

NOTE: When initializing the user variable U1, the main eForth
task is the current task. The phrase   TASK-1 U1 'S   returns
the address of an instance of the user variable U1 belonging to
TASK-1.

To stop the task    -- TASK-1 SLEEP
To restart the task -- TASK-1 AWAKE

\ ==============================================================
\ Appendix

4. Standard Forth Documentation Requirements
--------------------------------------------

4.1 System documentation
------------------------

4.1.1 Implementation-defined options
------------------------------------
- aligned address requirements (3.1.3.3 Addresses);
  No requirement jwr

- behavior of 6.1.1320 EMIT for non-graphic characters;
  All characters transmitted

- character editing of 6.1.0695 ACCEPT and 6.2.1390 EXPECT;
  ACCEPT: BS deletes; CR terminates
  EXPECT: not supported

- character set
  (3.1.2 Character types, 6.1.1320 EMIT, 6.1.1750 KEY);
  7-bit ASCII

- character-aligned address requirements (3.1.3.3 Addresses);
  No requirement

- character-set-extensions matching characteristics
  (3.4.2 Finding definition names);
  Case-insensitive

- conditions under which control characters
  match a space delimiter (3.4.1.1 Delimiters);
  All characters are distinct, only BL ( H# 020 ) matches space

- format of the control-flow stack (3.2.3.2 Control-flow stack);
  Addresses on data stack

- conversion of digits larger than thirty-five
  (3.2.1.2 Digit conversion);
  Case-sensitive, any character accepted

- display after input terminates in
  6.1.0695 ACCEPT and 6.2.1390 EXPECT;
  ACCEPT: text is displayed upon receipt of a character.
          When loading a file display of characters is
          suppressed.
  EXPECT: not supported

- exception abort sequence (as in 6.1.0680 ABORT");
  Implemented as -2 THROW, display the string

- input line terminator (3.2.4.1 User input device);
  Return key

- maximum size of a counted string, in characters
  (3.1.3.4 Counted strings, 6.1.2450 WORD);
  The string count is 16-bits,
  but the input buffer is limited to 128 characters

- maximum size of a parsed string (3.4.1 Parsing);
  128, limited by the input buffer

- maximum size of a definition name, in characters
  (3.3.1.2 Definition names);
  The count is 16-bits,
  but the input buffer is limited to 128 characters

- maximum string length for
  6.1.1345 ENVIRONMENT?, in characters;
  ENVIRONMENT? not supported

- method of selecting 3.2.4.1 User input device;
  Vectored thru an array bbb ???

- method of selecting 3.2.4.2 User output device;
  Vectored thru an array bbb ???

- methods of dictionary compilation (3.3 The Forth dictionary);
  Bit Threaded Code (btc) commonly called bitsy

- number of bits in one address unit (3.1.3.3 Addresses);
  16

- number representation and arithmetic
  (3.2.1.1 Internal number representation);
  2's complement

- ranges for n, +n, u, d, +d, and ud
  (3.1.3 Single-cell types, 3.1.4 Cell-pair types);
  Single: 16-bits
  Double: 32-bits

- read-only data-space regions (3.3.3 Data space);
  None

- size of buffer at 6.1.2450 WORD
  (3.3.3.6 Other transient regions);
  Input buffer is 128+2 characters. WORD not implemented

- size of one cell in address units (3.1.3 Single-cell types);
  1

- size of one character in address units
  (3.1.2 Character types);
  1

- size of the keyboard terminal input buffer
  (3.3.3.5 Input buffers);
  128+2

- size of the pictured numeric output string buffer
  (3.3.3.6 Other transient regions);
  80

- size of the scratch area whose address is returned by
  6.2.2000 PAD (3.3.3.6 Other transient regions);
  Size is variable, UNUSED - 80

- system case-sensitivity characteristics
  (3.4.2 Finding definition names);
  Case-insensitive

- system prompt (3.4 The Forth text interpreter, 6.1.2050 QUIT);
  "ok" CR LF

- type of division rounding
  (3.2.2.1 Integer division, 6.1.0100 */, 6.1.0110 */MOD,
    6.1.0230 /, 6.1.0240 /MOD, 6.1.1890 MOD);
  Floored, FM/MOD is default. SM/REM is provided

- values of 6.1.2250 STATE when true;
  -1 ( H# 0ffff )
  the execution token of _] bbb jwr

- values returned after arithmetic overflow
  (3.2.2.2 Other integer operations);
  -1 ( H# 0ffff )

- whether the current definition can be found after
  6.1.1250 DOES> (6.1.0450 :);
  No

4.1.2 Ambiguous conditions
--------------------------
- a name is neither a valid definition name
  nor a valid number during text interpretation
  (3.4 The Forth text interpreter);
  -13 THROW

- a definition name exceeded the maximum length allowed
  (3.3.1.2 Definition names);
  All input limited to 128+2 by input buffer
  All string counts are full 16-bit cell bbb jwr

- addressing a region not listed in 3.3.3 Data Space;
  Not possible jwr

- argument type incompatible with specified input parameter,
  e.g., passing a flag to a word expecting an n
  (3.1 Data types);
  Ignore and continue

- attempting to obtain the execution token,
  (e.g., with 6.1.0070 ', 6.1.1550 FIND, etc.)
  of a definition with undefined interpretation semantics;
  Return token

- dividing by zero
  (6.1.0100 */, 6.1.0110 */MOD, 6.1.0230 /, 6.1.0240 /MOD,
   6.1.1561 FM/MOD, 6.1.1890 MOD, 6.1.2214 SM/REM,
   6.1.2370 UM/MOD, 8.6.1.1820 M*/);
  Return maximum 16-bit values ( H# ffff H# ffff )

- insufficient data-stack space or
  return-stack space (stack overflow);
  Return stack builds down toward Data stack which builds down
  Stack collision, unknown result

- insufficient space for loop-control parameters;
  Ignore and continue

- insufficient space in the dictionary;
  No test made.
  Most likely crash system by overwriting return stack

- interpreting a word with undefined interpretation semantics;
  COMPILE-ONLY will ABORT" compile?"

- modifying the contents of the input buffer or a string literal
  (3.3.3.4 Text-literal regions, 3.3.3.5 Input buffers);
  Ignore and continue

- overflow of a pictured numeric output string;
  Overrun dictionary, most likely destroying last defined word

- parsed string overflow;
  No test made

- producing a result out of range, e.g.,
  multiplication (using *) results in a value
  too big to be represented by a single-cell integer
  (6.1.0090 *, 6.1.0100 */, 6.1.0110 */MOD, 6.1.0570 >NUMBER,
   6.1.1561 FM/MOD, 6.1.2214 SM/REM, 6.1.2370 UM/MOD,
   6.2.0970 CONVERT, 8.6.1.1820 M*/);
  2's complement "wrapping"

- reading from an empty data stack or
  return stack (stack underflow);
  Error message  "depth?"

- unexpected end of input buffer,
  resulting in an attempt to use a zero-length string as a name;
  Ignore and continue

- >IN greater than size of input buffer (3.4.1 Parsing);
  2's complement "wrapping",
  most likely  -13 THROW  interpretation error

- 6.1.2120 RECURSE appears after 6.1.1250 DOES>;
  Yes

- argument input source different than
  current input source for 6.2.2148 RESTORE-INPUT;
  RESTORE-INPUT not supported

- data space containing definitions is de-allocated
  (3.3.3.2 Contiguous regions);
  No test made; definitions may be over-written

- data space read/write with incorrect alignment
  (3.3.3.1 Address alignment);
  Allowed

- data-space pointer not properly aligned
  (6.1.0150 ,, 6.1.0860 C,);
  Allowed

- less than u+2 stack items (6.2.2030 PICK, 6.2.2150 ROLL);
  Ignore and continue

- loop-control parameters not available
  (6.1.0140 +LOOP, 6.1.1680 I, 6.1.1730 J,
   6.1.1760 LEAVE, 6.1.1800 LOOP, 6.1.2380 UNLOOP);
  Ignore and continue

- most recent definition does not have a name
  (6.1.1710 IMMEDIATE);
  IMMEDIATE bit set on previous definition

- name not defined by 6.2.2405 VALUE used by 6.2.2295 TO;
  Allowed

- name not found
  (6.1.0070 ', 6.1.2033 POSTPONE,
   6.1.2510 ['], 6.2.2530 [COMPILE]);
  -13 THROW

- parameters are not of the same type
  (6.1.1240 DO, 6.2.0620 ?DO, 6.2.2440 WITHIN);
  Allowed

- 6.1.2033 POSTPONE or
  6.2.2530 [COMPILE] applied to 6.2.2295 TO;
  TO not supported

- string longer than a counted string returned by 6.1.2450 WORD;
  WORD not supported.
  Use PARSE-WORD ( "ccc" -- ca u )

- u greater than or equal to the number of bits in a cell
  ( 6.1.1805 LSHIFT, 6.1.2162 RSHIFT);
  Allowed

- word not defined via 6.1.1000 CREATE
  (6.1.0550 >BODY, 6.1.1250 DOES>);
  Allowed

- words improperly used outside 6.1.0490 <# and 6.1.0040 #>
  (6.1.0030 #, 6.1.0050 #S, 6.1.1670 HOLD, 6.1.2210 SIGN);
  Allowed

\ ==============================================================

