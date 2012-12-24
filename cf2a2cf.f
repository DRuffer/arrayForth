\ cf2a2cf.f
\ display cf to ascii block

\ 07/17/06 jr mods: space before ")", var# h# d#, ALLCAPS, personality change
\ 08/02/06 jr: new names, identify 32/27 bit lits, code/shadows, ignore y->g
\ 08/08/06 DaR make this run on gforth in OSX (9 hours)
\ 08/09/06 DaR add ASCII to cf routines (13 hours)
\ 08/10/06 DaR af2cf tested with exceptions (5.5 hours)
\ 08/11/06 DaR massive cleanup, docs and errors (9 hours)
\ 08/12/06 DaR replace personalities, add tests and files (8.25 hours)
\ 05/09/10 DaR update to latest tags and use : rather than |
\ 05/11/10 DaR add restore of compressed images
\ 05/18/10 DaR add icon output

decimal

\ ******************************************************************************
: FindDefinition ( _ -- flag )   BL WORD FIND NIP ;

CR .( \ Using ) -1 \ Try to figure out which Forth is loading this code

FindDefinition ENVIRONMENT? [IF]
	S" gforth" ENVIRONMENT? [IF]

		.( GForth version ) TYPE  SPACE
		: hide-redefinition-warnings   WARNINGS OFF ;
		FALSE SWAP CONSTANT ForGForth

	[ELSE]
		FindDefinition VERSION [IF]
			S" SwiftForth" VERSION OVER COMPARE 0= [IF]

				VERSION ZCOUNT TYPE  SPACE
				: hide-redefinition-warnings   WARNING OFF ;
				FALSE SWAP CONSTANT ForSwiftForth

			[THEN]
		[THEN]
	[THEN]
[THEN] [IF]

	.( unknown Forth!  Be careful! )
	: hide-redefinition-warnings ;

[THEN]  hide-redefinition-warnings cr \ it is useful to turn this off sometimes

\ ******************************************************************************
\ testing support

: <?> ( n -- )   .s drop ."  <?>" cr ;	\ a fence post to isolate issues

false value testing   \ only true in development testing

\ ******************************************************************************
[DEFINED] ForSwiftForth [IF] \ Add missing SwiftForth definitions

true constant ForDOS

create <eol>   2 c, 13 c, 10 c, \ We are assuming DOS line terminators

[THEN]

\ ******************************************************************************
[DEFINED] ForGForth [IF] \ Add missing GForth definitions

s" PWD" getenv drop c@ char \ = [IF]	\ using DOS path separators
	true constant ForDOS
[THEN]

[DEFINED] ForDOS [IF]
	create <eol>   2 c, 13 c, 10 c,	\ We are assuming DOS line terminators
[ELSE]
	create <eol>   1 c, 10 c,		\ We are assuming UNIX line terminators
[THEN]

: not ( flag -- flag' )   0= ;

: @execute ( addr -- )   @ execute ;

[THEN]

\ ******************************************************************************
\ string handling words

: cappend ( char to -- )   DUP >R COUNT + C! R@ C@ 1+ R> C! ;
: append ( from len to -- )   2dup >r >r  count + swap move  r> r@ c@ + r> c! ;
: place ( from len to -- )   0 over c! swap 255 min swap append ;

: $c, ( str len -- )   here  over 1+ allot  place ;		\ compile string

: +$c, ( str len dest -- )	\ concatenate strings
    dup count + here <> abort" can't concatenate strings"
    over allot  2dup >r >r  count + swap move  r> r@ c@ + r> c! ;

: ,"      ( _"..." -- )   [char] " word count      $c, ;
: +" ( addr _"..." -- )   [char] " word count rot +$c, ; 

\ ******************************************************************************
\ The following are used to fetch data that is in known Endian format.  E.g.
\ in file system structures or network packets.  These words work on un-aligned
\ entities.

: 1c!-le ( x a n -- )   BEGIN ?DUP WHILE
		1- ROT DUP 8 RSHIFT SWAP 2OVER DROP C! ROT 1+ ROT
	REPEAT 2DROP ;

: 1c!-be ( x a n -- )   BEGIN ?DUP WHILE
        1- ROT DUP 8 RSHIFT SWAP 2OVER +    C! ROT    ROT
    REPEAT 2DROP ;

: 1c@-le ( a n -- x )   0 SWAP BEGIN ?DUP WHILE
 		1- ROT 2DUP +      C@ >R ROT 8 LSHIFT R> + ROT
 	REPEAT NIP ;

: 1c@-be ( a n -- x )   0 SWAP BEGIN ?DUP WHILE
        1- ROT DUP 1+ SWAP C@ >R ROT 8 LSHIFT R> + ROT
    REPEAT NIP ;

: sign-extend ( x n -- x' )  32 swap - dup >r
	lshift  r> 0 do  2/  loop ;

: @-le ( a -- x )   4 1c@-le ;
: !-le ( x a -- )   4 1c!-le ;

: w@-be ( a -- x )   2 1c@-be ;
: w!-be ( x a -- )   2 1c!-be ;

\ ******************************************************************************
\ stack administration * originally from gforth's grey.fs * with GNU v2 license
\ this implementation does not check overflow

\ creates a stack called word with n cells
\ the first cell is the stackpointer

: stack ( n -- )   \ use: n stack word
	create here cell+ , here 0 ,
	swap cells allot
	here swap ! ;

: push ( n stack -- )   cell over +!
	dup 2@ < abort" stack full"
	@ ! ;

: clear? ( stack -- f )   dup @ [ 1 cells ] literal - = ;

: top ( stack -- n )   \ returns top of stack
	dup clear? abort" no items on stack"
	@ @ ;

: top? ( stack -- n|0 )   \ returns top or 0
	dup clear? if  drop 0
	else  top  then ;

: pop ( stack -- )   \ discards top stack item
	dup clear? abort" no items on stack"
	[ -1 cells ] literal swap +! ;

: pull ( stack -- n )   dup top swap pop ;

: pull? ( stack -- n|0 )   \ pull top if available
	dup clear? if  drop 0
	else  pull  then ;

: clear ( stack -- )   dup cell+ swap ! ;

\ ******************************************************************************
\ colorforth file

variable cfname

: color4th.cf    ( -- )   c" color4th.cf"    cfname ! ;   color4th.cf

: blocks ( blks -- bytes) 1024 * ;

create [ns] 0 , \ set from 2nd cell in cfblock 18
create [nblk] 1440 , \ number of uncompressed blocks
variable cfptr	\ address of next colorForth 32-bit token
0 value cfend	\ address of last colorForth token in block
0 value cfc     \ size of compressed file

    0			value cfblock0  \ block# of 1st byte in file
    0			value cfhandle  \ colorForth file handle
[nblk] @ blocks	value cfsize    \ allocated cf bytes
    0			value cfbase    \ allocated cf memory pointer

: cfblock ( blk -- addr) cfblock0 - blocks cfbase + ;
: >cf ( a -- a')   cfblock0 blocks - cfbase + ;

include Restore.f

: cffree ( -- ) cfbase ?dup if FREE   ABORT" FREE FAILED" 0 to cfbase then ;

: cfload ( -- ) cfhandle if exit then
    cffree
    cfname @ count
    	   R/O OPEN-FILE  ABORT" OPEN-FILE FAILED"  to cfhandle
    cfhandle FILE-SIZE    ABORT" FILE-SIZE FAILED"  d>s to cfc
    cfsize ALLOCATE       ABORT" ALLOCATE FAILED"   to cfbase
    cfbase cfsize erase
    cfbase cfc cfhandle READ-FILE swap cfc xor or  ABORT" READ-FILE FAILED"
	cfhandle CLOSE-FILE   ABORT" CLOSE-FILE FAILED" 0 to cfhandle
	18 cfblock 1 cells + @ [ns] ! restore ;

: cfbuffer ( -- )
     cffree
     cfsize ALLOCATE       ABORT" ALLOCATE FAILED"   to cfbase
     cfbase cfsize erase ;

: xfsave ( base size adr len -- )
        W/O CREATE-FILE  ABORT" OPEN-FILE FAILED"    to cfhandle
    cfhandle WRITE-FILE  ABORT" WRITE-FILE FAILED"
    cfhandle CLOSE-FILE  ABORT" CLOSE-FILE FAILED" 0 to cfhandle ;

: cfsave ( -- )   cfbase cfsize cfname @ count xfsave ;

\ ******************************************************************************
\ redirect output between display and af file

variable afname

: color4th.af    ( -- )   c" color4th.af"    afname ! ;   color4th.af

0 value afhandle	\ ASCII file handle

: af-OPEN ( -- )   afname @ count W/O CREATE-FILE ABORT" OPEN-FILE FAILED"
	to afhandle ;

: af-CLOSE ( -- )   afhandle CLOSE-FILE ABORT" CLOSE-FILE FAILED"
	0 to afhandle ;

: af-TYPE ( str len -- )   afhandle WRITE-FILE ABORT" WRITE-FILE FAILED" ;

: af-CR ( -- )   <eol> count af-TYPE ;

variable 'af-TYPE	' TYPE 'af-TYPE !
variable 'af-CR		' CR 'af-CR !

: <TYPE> ( str len -- )   'af-TYPE @EXECUTE ;
: <CR> ( -- )   'af-CR @EXECUTE ;

: <file> ( -- )   af-OPEN
	['] af-TYPE 'af-TYPE !
	['] af-CR 'af-CR ! ;

: </file> ( -- )   af-CLOSE
	['] TYPE 'af-TYPE !
	['] CR 'af-CR ! ;

\ **********************************************************************
\ unpack cf word to ascii text

\ 4-bit 0.xxx 0-7
\ 5-bit 10.xxx 8-15
\ 7-bit 11xx.xxx 16-47
: unpack ( n -- n' chr) dup  dup 0<
    if     1 lshift dup 0<
      if   6 lshift swap 25 rshift 63 and  16 -    \ 11xxxxx.. 16-47
      else 4 lshift swap 27 rshift  7 and  8 xor   \ 10xxx..    8-15
      then
    else   4 lshift swap 28 rshift  7 and          \ 0xxx..     0-7
    then ;

: preshift ( n -- n')   32 0 do  [ hex ]
		dup F0000000 and if
			unloop exit
		then  2*
	loop ;

create cf-ii ( -- adr)   here ,"  rtoeanismcylgfwdvpbhxuq" 0 over 1+ c!
                              +" 0123456789j-k.z/;'!+@*,?"

: ch ( n -- n' chr) fffffff0 and unpack dup cf-ii count
	rot < abort" invalid character" + c@ ;
decimal

variable phere
: pad, ( chr -- ) phere @ c! 1 phere +! ;
: paddecode ( n -- ) begin ch dup while pad, repeat 2drop ;
: padcount ( n -- adr len) pad 1+ phere !  paddecode pad 1+ phere @ over -
    dup pad c! ;

: .name ( n -- )   cells [ hex ] 2700 >cf +
	@-le preshift padcount type space ;
: .names ( n -- )   0 do  i .name  loop ;
decimal

: 1cap ( addr -- ) dup c@ [char] a [char] z 1+ within
     if dup c@ 32 - swap c! else drop then ;
: caps ( addr len -- ) 0 ?do dup 1cap 1+ loop drop ;

\ ******************************************************************************
\ convert cf block to ascii file or display

variable col		\ count of characters on current line
68 value linemax	\ limit for starting to display a new word
true value cf+		\ true if limiting lines to linemax width

: >cr< ( -- )   col @ if  <cr>  0 col !  then ;   \ do not cr if at col 0

: >type< ( str len -- )   dup >r  <type>  r> col +! ;   \ remember column

: >space< ( -- )   cf+ if  col @ linemax >	\ wrap space if past linemax
	else  0  then  if  >cr<  s"    "  else  s"  "  then  >type< ;

: >spaces< ( n -- )   0 max 0 ?do  >space<  loop ;

: >r< ( n # -- )   >r 0   \ display decimal without sign, right justified
	<# #s #>  r> over - >spaces<  >type< ;

: >d< ( n -- )   s>d   \ display decimal with sign
	tuck dabs <# #s rot sign #> >type< ;

: >h< ( n -- )   base @ >r  hex  0   \ display hex without sign
	<# #s #> >type<  r> base ! ;

: gnn ( -- n )   cfptr @ @-le  4 cfptr +! ;
: n32 ( x -- n )   drop gnn ;
: n27 ( n -- n' )   2/ 2/ 2/ 2/ 2/ ;

0 value curcolor \ color of current token

: transition ( new -- x )   \ check against multiple transitions
   ( new    <-- )  curcolor
    over 14 = not  over 14 =     and if    s"  }" >type< >cr<  then   \  b -> ~b
    over 13 = not  over 13 =     and if         s"  }" >type<  then   \  g -> ~g
    over  9 = not  over  9 =     and if         s"  )" >type<  then   \  w -> ~w
    over  1 = not  over  1 =     and if         s"  ]" >type<  then   \  y -> ~y
    over  7 = not  over  7 =     and if         s"  >" >type<  then   \  c -> ~c
    over  7 =      over  7 = not and if  >space< s" <" >type<  then   \ ~c ->  c
    over  1 =      over  1 = not and if  >space< s" [" >type<  then   \ ~y ->  y
    over  9 =      over  9 = not and if  >space< s" (" >type<  then   \ ~w ->  w
    over 13 =      over 13 = not and if  >space< s" {" >type<  then   \ ~g ->  g
    over 14 =      over 14 = not and if  >space< s" {" >type<  then   \ ~b ->  b
    swap to curcolor ;

: newc ( new -- ) dup curcolor xor if transition then drop ;

hex
: .number ( n -- ) dup 1f and case
       02 of  s" D# " >type< n32 >d<  endof   \  y: execute 32-bit dec number
       12 of  s" H# " >type< n32 >h<  endof   \ dy: execute 32-bit hex number
       05 of  s" D# " >type< n32 >d<  endof   \  g: compile 32-bit dec number
       15 of  s" H# " >type< n32 >h<  endof   \ dg: compile 32-bit hex number
       06 of  s" d# " >type< n27 >d<  endof   \  g: compile 27-bit dec number
       16 of  s" h# " >type< n27 >h<  endof   \ dg: compile 27-bit hex number
       08 of  s" d# " >type< n27 >d<  endof   \  y: execute 27-bit dec number
       18 of  s" h# " >type< n27 >h<  endof   \ dy: execute 27-bit hex number
    endcase ;
decimal

0 value capext
: .word ( n -- ) 0 to capext padcount >type< ;
: .capword ( n -- ) 0 to capext padcount over 1cap >type< ;
: .allcaps ( n -- ) -1 to capext padcount 2dup caps >type< ;
: .extension ( n -- ) padcount capext if 2dup caps then >type< ;
: .colondef ( -- ) s" : " >type< .word ;
: .variable ( -- ) s" :# " >type< .word gnn  >space< dup >d<
	pad count s" ns" compare if  drop  else  [ns] !  then ;

hex
: .token ( n -- ) dup 0f and case
        0 of                  .extension	endof   \ --- extension word
        1 of   1 newc >space< .word			endof   \ yel execute word
        2 of   1 newc >space< .number		endof   \ yel execute 32-bit
        3 of   3 newc >cr<    .colondef		endof   \ red define word
        4 of   4 newc >space< .word			endof   \ gre compile word
        5 of   4 newc >space< .number		endof   \ gre compile 32-bit
        6 of   4 newc >space< .number		endof   \ gre compile 27-bit
        7 of   7 newc >space< .word			endof   \ cya compile a macro
        8 of   1 newc >space< .number		endof   \ yel execute 27-bit
        9 of   9 newc >space< .word			endof   \ whi comment word
       0a of   9 newc >space< .capword		endof   \ whi Capitalized Word
       0b of   9 newc >space< .allcaps		endof   \ whi ALL CAPS WORD
       0c of  0c newc >cr<    .variable		endof   \ mag variable + number
	   0d of  0d newc >space< >h<			endof   \ gre compiler feedback
	   0e of  0e newc >space< .word			endof   \ blu display word
      dup of >space< s" { " >type< >h< s"  }" >type< endof \ $f commented number
    endcase ;
decimal

: ablock ( blk# -- )
    cfblock dup cfptr !  1020 + to cfend
    0 to curcolor
    begin
      cfptr @ cfend <
      cfptr @ @ 0= not  and
    while
      gnn .token
    repeat  6 newc ;   \ dummy color to mark end of block

: .block ( blk# -- )   \ display block ready to be translated back
	<cr>  0 col !  dup 5 >r<  dup 1 and
	if  s"  shadow{ "  else  s"  code{ "
	then  >type<  ablock
	s"  }block" >type<
	>cr< ;

: cf2a ( inc blk n -- ) \ display n blocks starting at blk, incrementing by inc
	s" \ " >type<  afname @ count >type<  <cr>
	0 ?do  2dup .block +  loop  2drop ;

: run ( i*x -<name>- j*x )   \ redirect -<name>-'s output to af file
	cfload ' ( word-to-run )  <file>
	catch </file> throw ;

: cf2af ( -- )   cfload  1 18 1440 cf2a ;

\ ******************************************************************************
\ Support for translating ASCII to colorForth

variable nbits		\ number of bits left in token
0 value wordlen		\ size of current ASCII word name
true value 1stcap	\ 1st letter that can be capitalized

4 stack savecolor	\ remember colors around extensions and comments

: /bits ( -- )		\ reset token specific flags
	28 nbits !  true to 1stcap
	curcolor case
		 0 of  savecolor top? ?dup
		 	 if  to curcolor  then  endof
		 3 of  4 to curcolor  endof
		10 of  9 to curcolor  endof
		11 of  9 to curcolor  endof
		12 of  1 to curcolor  endof
	endcase ;

: .cfptr ( -- )   base @ >r  decimal  >cr<			\ display block location
	cfptr @ cfbase -  1024 /mod 3 >r<  4 / 4 >r<  >space<
	r> base ! ;

: .afptr ( -- )   source dup >r >type<				\ display text location
	>cr< 27 >spaces<  >in @ dup r@ - if  dup wordlen + r@ - 1-
		if  1-  then  then  r> drop  wordlen - >spaces<
	wordlen 0 do  s" ^" >type<  loop ;

: 8h.r ( n -- )   base @ swap  hex 0
	<#  8 0 do  #  loop  #>  >type<
	base ! ;

: addn ( n -- )   testing if  dup cfptr @ @-le -	\ add a fully formed token
		if  .cfptr  dup 8h.r s" <>" >type< cfptr @ @-le 8h.r >space< .afptr
	then  then  cfptr @ !-le  4 cfptr +!  /bits ;

: addc ( n -- )   nbits @ 4 + lshift  curcolor dup >r +		\ add text token
	addn  r> 0= if  savecolor pop  then ;

: getword ( -- str len )	\ get next ASCII word or abort if none left
	begin  bl word count  dup to wordlen  dup 0=
	while  2drop refill 0= abort" end of input before block finished"
	repeat ;

: getn ( -- n )   0 0 getword over c@ [char] - = dup >r   \ get an ASCII number
	if  1 /string  then  >number rot or
		if  .cfptr  s" Invalid number     " >type<  .afptr
		then  drop r>
	if  negate  then ;

: largen ( -- color )   curcolor 1 = if  2 exit  then  5 ;
: smalln ( -- color )   curcolor 1 = if  8 exit  then  6 ;

hex
: l/s ( hex -- n )   \ format a numeric token, adding prefix token if large
	>r getn dup 80000000 = over abs 2* F8000000 and or
	if  largen r> + addn exit  then
	5 lshift smalln r> + + ;
decimal

: setcolor ( n _ -- )   create , does> @ to curcolor ;   \ define format words

hex
create ii-cf   here 60 dup allot  erase   \ ASCII to cf translation table

: /ii-cf ( -- )   cf-ii count 1
	do  i 2dup + c@  dup [char] a [char] z 1+ within
		if  swap 40 +  swap 20 -  2dup ii-cf + c!		\ lower case alpha
			swap 40 +  swap 20 -       ii-cf + c!		\ upper case alpha
		else				20 -	   ii-cf + c!		\ non-alpha chars
		then
	loop  drop ;

/ii-cf   \ fill programatically to reduce maintenance

: chc ( ascii -- cf )   \ convert ASCII to cf character
	dup 80 20 within if  .cfptr s" invalid character  " >type<  .afptr  0=
	else  -20 + ii-cf + c@  then ;
decimal

: huf ( cf -- cf huf len )   \ convert cf char to huffman, #-bits
	dup 63 and
	dup 16 < if
		dup 8 < if
			4 ( 0xxx 0-7 )
		else  24 xor 5 ( 10xxx 8-15 )
		then
	else  80 + 7 ( 11xxxxx 16-47 )
	then ;

\ Chuck's 'trick': since he is shifting the bits left, which fills with zeros,
\ he allows the packed encoded characters to be larger than 28 bits if the
\ trailing bits are zeros.

: shortpack ( n cf huf len -- cf huf' n' len' )   >r rot r@ nbits @ >
	if  over dup  r@ nbits @ -  dup >r rshift  r@ lshift xor
		if  r> drop  addc  0  curcolor savecolor push  0 to curcolor
		else  swap r> r> over - >r rshift swap  then
	then  r> ;

: firstcap ( -- )   \ process 1st capital character
	curcolor 9 =  curcolor 0= or not
	if  .cfptr  s" Cap not in comment " >type<  .afptr  then
	curcolor if  10 to curcolor  false to 1stcap  then ;

: allcaps ( -- )   \ process folllowing capital characters
	curcolor 10 =  curcolor 11 = or not
	if  .cfptr  s" Not 1st/all caps   " >type<  .afptr  then
	curcolor if  11 to curcolor  then ;

: notcap ( -- )   \ process lower case characters
	curcolor 11 = if  .cfptr  s" Must be all caps   " >type<  .afptr  then
	false to 1stcap ;

: pack ( n cf huf len -- n' )   shortpack   \ pack huffman characters
	dup >r ( len )  lshift + >r ( n )  dup 128 and
	if  1stcap if  firstcap  else  allcaps  then
	else  dup 64 and if  notcap  then
	then  drop r> ( n )  r> ( len ) negate nbits +! ;

: addword ( str len -- )   \ add ASCII word to image
	0 -rot  over + swap ?do
		i c@ chc huf pack
	loop  addc ;

: endblock ( -- )   \ finish up the block by filling it with null tokens
	cfend cfptr @ - 4 /mod dup >r 0< or abort" bad pointers"
	.cfptr s" tokens processed ok" >type<  0 to curcolor
	r> 1+ 0 do  0 addn  loop ;

\ ******************************************************************************
\ Translate ASCII to colorForth

GET-CURRENT ( * )   WORDLIST DUP CONSTANT AF-VOC   SET-CURRENT

: D# ( _ -- )               0 l/s addn ;
: H# ( _ -- )   base @ hex 16 l/s addn base ! ;

: :# ( _name _value -- )   curcolor savecolor push \ compile a variable
	cfptr @  12 to curcolor  getword addword  cfptr @ swap - 4 -
	if  .cfptr  s" Var name too long  " >type<  .afptr  then
	getn addn  savecolor pull to curcolor ;

: }blocks ( -- ) ;  \ terminate blocks
: }block ( -- )   \ terminate block
	endblock  -1 to curcolor ;

: }   savecolor pull to curcolor ; \ restore color
: { ( _value -- )   \ compile an error, comment or display token
	curcolor savecolor push  base @  hex
	0 0 getword 2dup >r >r  over c@ [char] - =
	dup >r if  1 /string  then  >number rot or
	if  2drop r> drop  14 to curcolor  r> r> addword
	else  drop r> if  negate  then  addn  r> r> 2drop
	then  base ! ;

3 setcolor :		\ define
1 setcolor [		\ execute
4 setcolor ]		\ compile
7 setcolor <		\ compile macro
4 setcolor >		\ compile

: )   savecolor pull to curcolor ;				\ restore color
: (   curcolor savecolor push  9 to curcolor ;	\ comment

( * ) SET-CURRENT

: +token ( str len -- )   \ process ASCII word
	2dup AF-VOC SEARCH-WORDLIST
	if  execute  2drop  exit  then
	addword ;

: cblock ( blk# -- )   \ process ASCII into a block
    cfblock dup cfptr !  1020 + to cfend
    4 to curcolor  savecolor clear
    /bits  cf+ >r  false to cf+
	begin  curcolor 0< not  while
		cfend cfptr @ > not
		if  .cfptr  s" Block too long     " >type<  .afptr
			begin  getword s" }block" compare 0=
			until  -1 to curcolor
		else  getword +token  then
	repeat  r> to cf+ ;

: code{ ( n -- )   dup 1 and   \ start code block
	if  >cr<  8 >spaces< s" Odd code block     " >type<
		5 to wordlen  .afptr  then
	cblock ;

: shadow{ ( n -- )   dup 1 and 0=   \ start shadow block
	if  >cr<  8 >spaces< s" Even shadow block  " >type<
		7 to wordlen  .afptr  then
	cblock ;

: af2cf ( -- )   cfload  afname @ count included  cfsave ;

\ ******************************************************************************
\ Testing routines

: cf2a2cf ( -- )   \ Test the complete round trip of cf -> af -> cf
	true to testing  <file>  cf2af  </file>  af2cf  cr cr ;

: af2c2af ( -- )   \ Test the complete round trip of af -> cf -> af
	false to testing  cfbuffer  afname @ dup >r count included
	cfend 4 + to cfsize  here afname !  r@ count $c,
	s" n" afname @ +$c,  <file>  cf2af  </file>
	cr ." Manually compare " r@ count type
	."  to " afname @ count type
	r> afname ! ;

: errors.txt ( -- )   c" errors.txt" afname ! ;
: errors.af  ( -- )   c" errors.af"  afname ! ;
: errors.cf  ( -- )   c" errors.cf"  cfname ! ;

: errors ( -- )   \ Generate all af2cf errors
	true to testing
	afname @ >r  errors.af
	afname @     errors.txt
	cfname @ >r  errors.cf  cfload
	<file>  count included  <cr>  </file>
	cfsave  r> cfname !  r> afname ! ;

\ ******************************************************************************
: usage ( -- )   \ display usage instructions
	[DEFINED] ForSwiftForth [IF]
		\ cd f:\ar24c18\rev8\okad
		cr ." directory: " cd
	[THEN]

	cr ."  Default file names are defined as dictionary words:"
	cr ."  cf file: " cfname @ count type
	cr ."  af file: " afname @ count type
	cr
	cr ."  At this point, you can manipulate the cf blocks with:"
	cr ."  af2cf                                        \ translates af to cf"
	cr ."  cf2af                                        \ display all blocks"
	cr ."  errors                                       \ makes errors.txt
	cr ."  cf2a2cf or af2c2af                           \ full round testing"
	cr ."  <block#> .block                              \ display 1 block"
	cr ."  <increment> <start-block#> <#blocks> cf2a    \ display blocks"
	cr
	cr ."  <block#> code{ ... }block                    \ enter code manually"
	cr ."  <block#> shadow{ ... }block                  \ enter shadow text"
	cr
	cr ."  You can redirect output to the af file with:"
	cr ."  <stack-parameters> run <display-word>        \ writes to af file"
	cr
	cr ."  cfload reads the cf file into memory.  MUST BE DONE AT LEAST ONCE!"
	cr ."  By default the cf file starts with block 0."
	cr ."  Use  n to cfblock0  if the cf file doesn't start at block 0."
	cr ."  cf2af assumes that block 18 is the first source block. " ;

.( -> Type USAGE for instructions <- )

cr cr
