( $Id: ciasdis.frt,v 1.26 2009/03/26 19:40:39 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

CR \ Start compile time output on a new line

: uses ( flag -- )  0= IF POSTPONE \ THEN ;
: ?? ( "name" -- flag )  BL WORD FIND SWAP DROP 0= 0= ;
?? STR= 0= uses : MATCH? >R COUNT ROT COUNT ROT = R> AND ;
?? STR= 0= uses : (STR=) BEGIN DUP 0 > WHILE MATCH? 1- REPEAT 0= ;
?? STR= 0= uses : STR= ROT 2DUP = >R MIN (STR=) >R 2DROP R> R> AND ;
?? ENVIRONMENT? 0= uses : ENVIRONMENT? 2DROP 0 ;
: env-flag?  DUP IF DROP THEN ;              \ flag true?
: env-str?  DUP IF >R 2DROP R> THEN ;        \ string present?
: env-str=  ROT IF STR= ELSE 2DROP 0 THEN ;  \ string matches?

\ bigforth compat.f -e bye
S" BIGFORTH" ENVIRONMENT? env-str?
    uses CREATE ForBigForth
\ lina -s compat.f
S" NAME" ENVIRONMENT? S" ciforth" env-str=
    uses CREATE ForCiForth
\ ficl compat.f
\ Grrr.  FICL has no way to quit automatically after running a script.
S" ficl-version" ENVIRONMENT? env-str?
    uses CREATE ForFicl
\ gforth compat.f -e bye
S" gforth" ENVIRONMENT? env-str?
    uses CREATE ForGForth
\ ???
S" IFORTH" ENVIRONMENT? env-flag?
     uses CREATE ForIForth
\ kforth compat.f -e bye
?? NONDEFERRED
    uses CREATE ForKForth
\ pfe -q -y compat.f
S" FORTH-NAME" ENVIRONMENT? S" pfe" env-str=
    uses CREATE ForPfe
\ pforth compat.f
?? ::::loadp4th.fth
    uses CREATE ForPForth
\ spf4 compat.f BYE
S" FORTH-SYS" ENVIRONMENT? S" SP-FORTH" env-str=
    uses CREATE ForSpf4
        \ REQUIRE CASE-INS lib/ext/caseins.f
\ ??? vfx include compat.f bye ???
?? VFXFORTH
    uses CREATE ForVfx
\ win32forth include compat.f bye
S" WIN32FORTH" ENVIRONMENT? env-str?
    uses CREATE ForWin32Forth
\ sf include compat.f bye
?? VERSION DUP uses S" SwiftForth" version over compare 0= AND
    uses CREATE ForSwiftForth
: NONAME$ ( -- a n )   s" NONAME" ;

[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
    : GET-FILE ( a1 n1 -- a2 n2 )
        r/o open-file throw
        dup >r file-size throw
        abort" file too large"
        dup allocate throw
        swap 2dup r@ read-file throw
        over <> abort" could not read whole file"
        r> close-file throw ;
    : PUT-FILE ( a1 n1 a2 n2 -- )
        r/w create-file throw
        dup >r write-file throw
        r> close-file throw ;
[THEN]
[DEFINED] ForCiForth [IF]
    REQUIRE OLD:    REQUIRE $=    REQUIRE class    REQUIRE W/O
    : NEW-PRESENT   OLD: PRESENT DUP IF DUP >NFA @ $@ NONAME$ $= 0= AND THEN ;
    ' NEW-PRESENT ' PRESENT 3 CELLS MOVE
    : FAR@ L@ ;    HIDE L@
[THEN]

\ --------------------------------------------------------------
( $Id: tools.frt,v 1.2 2005/01/04 23:23:48 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

: <?> ( n -- )   .S DROP S" <?>" TYPE CR ;    \ a fence post to isolate issues

[DEFINED] ForGForth [IF]
: -warning ( - x)   warnings @  0 warnings ! ;
: +warning ( x)   warnings ! ; 
[THEN]

[DEFINED] ForSwiftForth [IF]
: -warning ( -- x)   warning @  0 warning ! ;
: +warning ( x)   warning ! ;
[THEN]

[DEFINED] ForCiForth [IF]
: -warning 0 ; : +warning drop ;
[THEN]
CREATE 'TYPE   ' TYPE DUP , ,

-warning
: TYPE ( a n -- )   'TYPE @ EXECUTE ;
+warning

: RESTORED ( a -- )   DUP CELL+ @  SWAP ! ;

: 2DROP'  2DROP ;      \ Need a high level word here.
: (SHUTUP) ( xt -- )   ['] 2DROP' 'TYPE !  EXECUTE  'TYPE RESTORED ;
: SHUTUP ( -name- )   ' POSTPONE LITERAL  POSTPONE (SHUTUP) ; IMMEDIATE

[DEFINED] ForSwiftForth [IF]  true constant ForDOS  [THEN]
[DEFINED] ForGForth [IF] \ Add missing GForth definitions
s" PWD" getenv drop c@ char \ = [IF]    \ using DOS path separators
     true constant ForDOS [THEN] [THEN]

[DEFINED] ForDOS [IF]
     CREATE <EOL>   2 C, 13 C, 10 C, \ We are assuming DOS line terminators
[ELSE]
     CREATE <EOL>   1 C, 10 C,       \ We are assuming UNIX line terminators
[THEN]

CREATE CHAR-BUF   0 C,

-warning
: CR ( -- )         'TYPE 2@ = IF  CR  ELSE  <EOL> COUNT TYPE  THEN ;
: EMIT ( c -- )     'TYPE 2@ = IF  EMIT  ELSE
         CHAR-BUF C!  CHAR-BUF 1 TYPE  THEN ;
: SPACE ( -- )      'TYPE 2@ = IF  SPACE  ELSE  BL EMIT  THEN ;
: SPACES ( n -- )   'TYPE 2@ = IF  SPACES  ELSE  0 ?DO  SPACE  LOOP  THEN ;
+warning

[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
     : NOT ( flag -- flag' )   0= ;

    -warning
    : cappend ( char to -- )   DUP >R COUNT + C! R@ C@ 1+ R> C! ;
    : append ( from len to -- )   2DUP >R >R  COUNT + SWAP MOVE
        R> R@ C@ + R> C! ;
    : place ( from len to -- )   0 OVER C! SWAP 255 MIN SWAP APPEND ;
    
    : @+ ( a -- a' n )   DUP >R CELL+ R> @ ;
    : $= ( a1 n1 a2 n2 -- f )   COMPARE 0= ;
    : $@ ( a -- a' n )   COUNT ;
    : $, ( a n -- a' )   HERE >R  DUP C,  0 ?DO  $@ C,  LOOP  DROP R> ;
    : $! ( a n a' -- )   PLACE ;
    : $+! ( a n a' -- )   APPEND ;
    : $C+ ( c a -- )   CAPPEND ;
    +warning
    VARIABLE BASE'
    : <HEX   BASE @ BASE' ! HEX ;       ( 0/1  SWITCH TO HEX)
    : HEX>   BASE' @ BASE !     ;       ( 1/0  AND BACK)
    
    [DEFINED] ForCiForth [IF]
    : 4? ( n -- )   1+ 4 MOD 0= IF  [CHAR] , HOLD THEN ;
        : (DH.)   <HEX  <#  1- 0 ?DO  #  I 4?  LOOP  #  #>  HEX> ;
    [ELSE]
        : (DH.)   <HEX  <#  1- 0 ?DO  #  LOOP  #  #>  HEX> ;
    [THEN]
    : (H.R) ( n digits -- a len )   >R S>D R> (DH.) ;
    : H.R ( n digits -- )   (H.R) TYPE ;
    
    : (H.) ( n -- a len )   DUP ABS 0  <HEX  <# #S ROT SIGN #>  HEX> ;
    : H.   (H.) TYPE SPACE ;
    -warning
    : C.   S>D <# # #S #> TYPE SPACE ; ( print byte )
    : (.)   S>D TUCK DABS <# #S ROT SIGN #> ;
    : .   (.) TYPE SPACE ;
    : .R   >R (.) R> OVER - 0 MAX SPACES TYPE ;
    +warning

     : CORA swap over compare ;
     : ETYPE TYPE ;
     : TOGGLE ( a b -- ) over @ xor swap ! ;
     variable exit-code
     0 value _  ' _ to _

    : BUILD-BAG ( n -- )   HERE CELL+ , CELLS ALLOT ;
    : BAG ( n -- )   CREATE HERE CELL+ , CELLS ALLOT DOES> ;
    : !BAG ( bag -- )   DUP CELL+ SWAP ! ;
    : BAG? ( bag -- )   @+ = 0= ;
    : BAG+! ( x bag -- )   DUP >R @ ! 0 CELL+ R> +! ;
    : BAG@- ( bag -- x )   0 CELL+ NEGATE OVER +! @ @ ;
    : BAG-REMOVE ( a bag -- )
        >R  DUP CELL+ SWAP  OVER R@ @ SWAP - MOVE  -1 CELLS R> +! ;
    : BAG-HOLE ( a bag -- )
        >R  DUP CELL+   OVER R@ @ SWAP - MOVE   0 CELL+ R> +! ;
    : BAG-INSERT ( x a bag -- )   OVER SWAP BAG-HOLE ! ;
    : |BAG| ( bag -- n )   @+ SWAP - 0 CELL+ / ;
    : DO-BAG  POSTPONE @+ POSTPONE SWAP POSTPONE ?DO ; IMMEDIATE
    : LOOP-BAG 0 CELL+ POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE
    : .BAG ( bag -- )   DO-BAG  I ?  LOOP-BAG ;
    : BAG-WHERE ( x bag -- a )   DO-BAG  DUP I @ = IF
            DROP I UNLOOP EXIT  THEN
        LOOP-BAG  DROP 0 ;
    : IN-BAG? ( x bag -- )   BAG-WHERE 0= 0= ;
    : BAG- ( x bag -- )   DUP >R   BAG-WHERE   R> BAG-REMOVE ;
    : SET+ ( x bag -- )   2DUP IN-BAG? IF 2DROP ELSE BAG+! THEN ;
    : SET- ( x bag -- )   2DUP IN-BAG? IF BAG- ELSE 2DROP THEN ;
    : BIN-SEARCH ( n IMIN, n IMAX, xt COMP -- n IRES )   >R
        BEGIN       \ Loop variant IMAX - IMIN
            2DUP  <> WHILE
                2DUP + 2/  ( -- ihalf )
                DUP R@ EXECUTE IF
                    1+  SWAP ROT DROP \ Replace IMIN
                ELSE
                    SWAP DROP \ Replace IMAX
                THEN
        REPEAT
        R> 2DROP ;
    : EXCHANGE ( a1 a2 n -- )   0 ?DO  OVER I +  OVER I +  OVER C@  OVER C@
            >R SWAP C!  R> SWAP C!  LOOP  2DROP ;
    ( QSORT ) \ AvdH A2apr22
    
    \ Compare item N1 and N2. Return 'N1' IS lower and not equal.
    VARIABLE *<
    \ Exchange item N1 and N2.
    VARIABLE *<-->
    : PARTITION   2DUP + 2/   >R  ( R: median)
        2DUP BEGIN      ( lo_1 hi_2 lo_2 hi_1)
            SWAP BEGIN  DUP R@ *< @ EXECUTE WHILE  1+  REPEAT
            SWAP BEGIN  R@ OVER *< @ EXECUTE WHILE  1-  REPEAT
            2DUP > 0= IF
                \ Do we have a new position for our pivot?
                OVER R@ = IF R> DROP DUP >R ELSE
                    DUP  R@ = IF R> DROP OVER >R THEN THEN
                2DUP *<--> @ EXECUTE
                >R 1+ R> 1-
            THEN
        2DUP > UNTIL    ( lo_1 hi_2 lo_2 hi_1)
        R> DROP                            ( R: )
        SWAP ROT ;      ( lo_1 hi_1 lo_2 hi_2)
    : (QSORT)             ( lo hi -- )
        PARTITION         ( lo_1 hi_1 lo_2 hi_2)
        2DUP < IF  RECURSE  ELSE  2DROP  THEN
        2DUP < IF  RECURSE  ELSE  2DROP  THEN ;
    
    : QSORT ( xt1 xt2 -- )   *<--> !  *< !  (QSORT) ;
[ELSE]
    REQUIRE H.    REQUIRE RESTORED
    : .^   .S R@ @ >NFA @ $@ TYPE ;
[THEN]

: \D POSTPONE \ ;
\ : \D ;
\ : QSORT-SAFE 2>R 2DUP < IF 2R> QSORT ELSE 2DROP 2R> 2DROP THEN ;
CREATE NAME-BUF   256 ALLOT

: INVENT-NAME   s" L" NAME-BUF $!   0 8 (DH.) NAME-BUF $+! NAME-BUF $@ ;
: INVENTED-NAME? ( a1 a2 n -- flag )  9 <> IF  2DROP 0
    ELSE  SWAP INVENT-NAME CORA 0=  THEN ;

HEX

: TEST-INVENT-NAME
    assert( 42 INVENT-NAME S" L00000042" COMPARE 0= )
    assert( 42 S" L00000043" INVENTED-NAME? 0= )
    assert( 42 S" L00000042" INVENTED-NAME? -1 = )
; TEST-INVENT-NAME

DECIMAL
[DEFINED] >= 0= [IF] : >= < 0= ; [THEN]
[DEFINED] <= 0= [IF] : <= > 0= ; [THEN]
: ?ABORT ROT IF ETYPE 2 EXIT-CODE ! BYE ELSE 2DROP THEN ;

[DEFINED] ForCiForth [IF]
    REQUIRE $=    REQUIRE ."$"
[THEN]
include File.fth  Files FALSE REVERSE !  BIG-ENDIAN  ONLY FORTH DEFINITIONS
( $Id: asgen.frt,v 4.31 2005/03/07 11:54:58 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
    : (?ERROR) ( f n -- )   swap if throw else drop then ;
    CREATE '?ERROR   ' (?ERROR) DUP , ,   : ?ERROR '?ERROR @ EXECUTE ;
    
    -warning
    : ?CSP ( -- ) ; \ check stack
    : !CSP ( -- ) ; \ set stack check
    +warning
    : ?EXEC ( -- ) ;
    : asm-create ( -- )   create ;
[THEN]
[DEFINED] ForCiForth [IF]
    : asm-create ( -- )   (WORD) (CREATE) ;
    REQUIRE ALIAS
    REQUIRE @+ ( Fetch from ADDRES. Leave incremented ADDRESS and DATA )
    REQUIRE BAG
    REQUIRE POSTFIX
[THEN]

\ Vectors that are hot-patched in aswrap.frt
CREATE 'AS-ALLOT   ' ALLOT DUP , ,   : AS-ALLOT 'AS-ALLOT @ EXECUTE ;
CREATE 'AS-HERE    ' HERE DUP , ,    : AS-HERE 'AS-HERE @ EXECUTE ;
CREATE 'AS-C,      ' C, DUP , ,      : AS-C, 'AS-C, @ EXECUTE ;
CREATE '_AP_       ' HERE DUP , ,    : _AP_ '_AP_ @ EXECUTE ;

: (-ADORN-ADDRESS) DROP CR ;   ( Action between two disassembled instr.    )

CREATE 'ADORN-ADDRESS   ' (-ADORN-ADDRESS) DUP , ,
: ADORN-ADDRESS ( a -- )   'ADORN-ADDRESS @ EXECUTE ;
( ############### PART I ASSEMBLER #################################### )
: !+ ( x a -- a' )   >R R@ ! R> CELL+ ;
: @- ( a -- x a' )   0 CELL+ - >R R@ @ R>  ;
-warning
: CTRL CHAR 31 AND ;
: [CTRL] CTRL POSTPONE LITERAL ; IMMEDIATE
+warning
CREATE TABLE1   1 , 1 ,
: ROTLEFT ( x n -- x' )   TABLE1 + @ UM* OR ;

[DEFINED] ForCiForth [IF]
    'TABLE1 HIDDEN
[THEN]
: %>BODY ; ( From DEA to the DATA field of a created word, now the same )
: %BODY> ; ( Reverse of above)

: DEA-FIELD ( u size -- u' )   CREATE  OVER , +
    DOES> ( dea -- a )   @ SWAP %>BODY + ;

0
  1 CELLS DEA-FIELD >LFA    ( link to previous word for compatibility   )
  1 CELLS DEA-FIELD >NFA    ( variable length name field is at the end  )
  1 CELLS DEA-FIELD >DATA

( Work on TALLY-BI etc.        Effects  for posits fixups and commaers. )
(                                            |||    |||       |||       )
  1 CELLS DEA-FIELD >BI                    ( OR!    AND!      --        )
  1 CELLS DEA-FIELD >BY                    ( OR!    OR!       AND!      )
  1 CELLS DEA-FIELD >BA                    ( OR!U   OR!U      OR!U      )
  1 CELLS DEA-FIELD >CNT    ( `HERE' advances with count )
  0 CELLS DEA-FIELD >DIS    ( disassembler only for COMMA , 0 -> default)
  1 CELLS DEA-FIELD >PRF    ( prefix flag, only for PI ,    0 -> default)
  1 CELLS DEA-FIELD >DFA    ( type of word, replacing the DOES> check   )
CONSTANT |DEA| ( the name is tacked onto the end when it is created     )

: %ID. >NFA @ $@ TYPE SPACE ;

-warning
VOCABULARY ASSEMBLER IMMEDIATE
+warning

: Assem   ONLY FORTH ALSO POSTPONE ASSEMBLER ALSO DEFINITIONS ; IMMEDIATE

Assem HEX

[DEFINED] ForCiForth [IF]
    'ONLY >WID CURRENT !    \ Making ONLY the CONTEXT is dangerous! This will do.
    "'" 'ONLY >WID (FIND)   ALIAS %         ( "'" ) 2DROP
    CONTEXT @ CURRENT !     \ Restore current.
[THEN]

: %>DOES ( dea -- x ) >DFA ;
: IGNORE? >NFA @ CHAR+ C@ [CHAR] ~ = ;
: VOCEND? ( dea -- f )   >LFA @ 0= ;
: (>NEXT%) ( dea -- dea )   >LFA @ ;
: >NEXT% ( dea -- dea' )   BEGIN  (>NEXT%) DUP >NFA @ CHAR+ C@
    [CHAR] - - UNTIL ;
0 VALUE STARTVOC ( -- dea )
VARIABLE 'IS-A   1 'IS-A !
: IS-A ( -- )   CREATE  'IS-A @ ,  DOES> ( dea -- f ) @ SWAP %>DOES @ = ;
: MEMBER ( n -- )   STARTVOC >DFA ! ;
: REMEMBER ( -- )   ?CSP  'IS-A @ POSTPONE LITERAL  POSTPONE MEMBER
    1 'IS-A +!  !CSP ; IMMEDIATE
\ : ?ERROR DROP DROP ;
: CREATE--   SAVE-INPUT  BL WORD $@ $, >R  RESTORE-INPUT THROW
    R> DUP $@ S" --" COMPARE 0= IF  -warning CREATE +warning
    ELSE  CREATE  THEN  HERE DUP >R  |DEA| DUP ALLOT  ERASE
    R@ >NFA !  STARTVOC R@ >LFA !  R> TO STARTVOC ;
: CONTAINED-IN OVER AND = ;
: lsbyte, DUP AS-C, 0008 RSHIFT ;
: lsbyte@ 1- SWAP 8 LSHIFT OVER C@ OR SWAP ;
: lsbytes  >R R@ + BEGIN R> DUP WHILE 1- >R  lsbyte@ REPEAT 2DROP ;
: MC@ 0 ROT ROT lsbytes ;
: MC<0 + 1- C@ 80 AND 80 = ;
: MC@-S 2DUP MC<0 ROT ROT lsbytes ;
VARIABLE TALLY-BI
VARIABLE TALLY-BY
VARIABLE TALLY-BA
VARIABLE BA-DEFAULT    0 BA-DEFAULT !
VARIABLE OLDCOMMA
VARIABLE ISS
VARIABLE ISL
VARIABLE BA-XT
: RESET-BAD ( -- )   BA-XT @ DUP IF EXECUTE ELSE
        DROP  BA-DEFAULT @ TALLY-BA ! THEN ;
: !TALLY ( -- )   0 TALLY-BI !   0 TALLY-BY !   RESET-BAD   0 OLDCOMMA ! ;
    0 BA-XT !   !TALLY
: AT-REST? TALLY-BI @ 0=   TALLY-BY @ 0=  AND ;
: BADPAIRS? DUP 1 LSHIFT AND AAAAAAAAAAAAAAAA AND ;
: BAD? TALLY-BA @ BADPAIRS? ;
: CONSISTENT? TALLY-BA @ OR BADPAIRS? 0= ;
DECIMAL
: CHECK26 AT-REST? 0= ABORT" PREVIOUS INSTRUCTION INCOMPLETE" ;
: CHECK32 BAD? ABORT" PREVIOUS OPCODE PLUS FIXUPS INCONSISTENT" ;
: CHECK31 2DUP SWAP CONTAINED-IN 0=
    ABORT" DESIGN ERROR, INCOMPATIBLE MASK" ;
: CHECK31A 2DUP OVER >R RSHIFT 1 OR OVER LSHIFT R> <>
    ABORT" DESIGN ERROR, INCOMPATIBLE MASK" ;
: CHECK32B  2DUP OR INVERT 0= ( all ones) >R
    INVERT AND 0= ( all zero's ) R> OR ( okay)
    0= ABORT" PREVIOUS OPCODE PLUS FIXUPS INCONSISTENT" ;
: CHECK33 2DUP SWAP INVERT CONTAINED-IN 0=
    ABORT" DESIGN ERROR, INCOMPATIBLE MASK" ;
: CHECK28 2DUP AND ABORT" UNEXPECTED FIXUP/COMMAER" ;
: CHECK29 2DUP OR -1 - ABORT" DUPLICATE FIXUP/UNEXPECTED COMMAER" ;
: CHECK30 DUP OLDCOMMA @ < ABORT" COMMAERS IN WRONG ORDER"
    DUP OLDCOMMA ! ;
HEX
: OR! >R R@ @  CHECK28 OR R> ! ;
: OR!U >R R@ @  OR R> ! ;
: AND! >R INVERT R@ @ CHECK29 AND R> ! ;
: assemble, ( x -- )   ISL @ 0 DO  lsbyte,  LOOP  DROP ;
: !POSTIT ( -- )   AS-HERE ISS !  0 OLDCOMMA ! ;
: TALLY:, ( a -- )   DUP >BI @ TALLY-BI !  DUP >BY @ TALLY-BY !
    DUP >BA @ TALLY-BA OR!U  DUP >CNT @ ISL !  >DIS @ BA-XT ! ;
: POSTIT ( a -- )   CHECK26   !TALLY   !POSTIT
    DUP >DATA @ >R   TALLY:,   R> assemble, ;
: BUILD-IP ( ba by bi opc cnt -- )   STARTVOC >CNT !  STARTVOC >DATA !
    STARTVOC >BI !  STARTVOC >BY !  STARTVOC >BA !
    0 ( prefix) STARTVOC >PRF ! ;
IS-A IS-1PI : 1PI  CHECK33 CREATE-- REMEMBER  1 BUILD-IP DOES>  POSTIT ;
IS-A IS-2PI : 2PI  CHECK33 CREATE-- REMEMBER  2 BUILD-IP DOES>  POSTIT ;
IS-A IS-3PI : 3PI  CHECK33 CREATE-- REMEMBER  3 BUILD-IP DOES>  POSTIT ;
IS-A IS-4PI : 4PI  CHECK33 CREATE-- REMEMBER  4 BUILD-IP DOES>  POSTIT ;
: IS-PI  >R 0
    R@ IS-1PI OR  R@ IS-2PI OR  R@ IS-3PI OR   R@ IS-4PI OR R> DROP ;
: TALLY:| ( a -- )   DUP >BI @ TALLY-BI AND!  DUP >BY @ TALLY-BY OR!
    >BA @ TALLY-BA OR!U ;
: FIXUP>   DUP >DATA @ ISS @ OR!   TALLY:|   CHECK32 ;
IS-A IS-xFI : xFI  CHECK31 CREATE-- REMEMBER STARTVOC >DATA !
    STARTVOC >BI !  STARTVOC >BY !  STARTVOC >BA ! DOES>  FIXUP> ;
: TRIM-SIGNED >R   2DUP R@ SWAP RSHIFT CHECK32B   LSHIFT R> AND ;
: FIXUP-DATA ( a -- )   DUP >DATA @  SWAP >R  LSHIFT ISS @ OR!
    R> TALLY:|  CHECK32 ;
: FIXUP-SIGNED ( a -- )   DUP >DATA @  SWAP >R
    R@ >BI @ TRIM-SIGNED ISS @ OR!
    R> TALLY:| CHECK32 ;
IS-A IS-DFI : DFI  CHECK31A CREATE-- REMEMBER STARTVOC >DATA !
    STARTVOC >BI !  STARTVOC >BY !  STARTVOC >BA !  DOES>  FIXUP-DATA ;
IS-A IS-DFIs : DFIs  CHECK31A CREATE-- REMEMBER STARTVOC >DATA !
    STARTVOC >BI !  STARTVOC >BY !  STARTVOC >BA !  DOES>  FIXUP-SIGNED ;
: REVERSE-BYTES
    1 CELLS 0 DO DUP  FF AND SWAP 8 RSHIFT   LOOP
    8 CELLS 0 DO SWAP I LSHIFT OR   8 +LOOP ;
: CORRECT-R 0 CELL+ ISL @ - ROTLEFT ;
: TALLY:|R ( a -- )   DUP >BI @ CORRECT-R TALLY-BI AND!
    DUP >BY @ TALLY-BY OR!  >BA @ TALLY-BA OR!U ;
: FIXUP<   CORRECT-R ISS @ OR! ;
IS-A IS-FIR : FIR  CHECK31 CREATE-- REMEMBER REVERSE-BYTES STARTVOC >DATA !
    REVERSE-BYTES STARTVOC >BI !  STARTVOC >BY !  STARTVOC >BA !
    DOES>  DUP >DATA @  FIXUP< TALLY:|R  CHECK32 ;
IS-A IS-DFIR : DFIR  CHECK31 CREATE-- REMEMBER STARTVOC >DATA !
    REVERSE-BYTES STARTVOC >BI !  STARTVOC >BY !  STARTVOC >BA !
    DOES>  DUP >DATA @  SWAP >R  LSHIFT REVERSE-BYTES FIXUP<
        R> TALLY:|R  CHECK32 ;
: (AND!BYTE) >R 0FF AND INVERT R@ C@ CHECK29 AND R> C! ;
: AND!BYTE BEGIN 2DUP (AND!BYTE) SWAP 8 RSHIFT DUP WHILE SWAP 1+ REPEAT 2DROP ;
: (OR!BYTE) >R R@ C@  CHECK28 OR R> C! ;
: OR!BYTE BEGIN 1- 2DUP (OR!BYTE) SWAP 8 RSHIFT DUP WHILE SWAP REPEAT 2DROP ;
: TALLY:|R'  DUP >BI @ TALLY-BI AND!BYTE  DUP >BY @ TALLY-BY OR!
    >BA @ TALLY-BA OR!U ;
: FIXUP<'   DUP >DATA @ ISS @ ISL @ + OR!BYTE  TALLY:|R'  CHECK32 ;
: TALLY:,, ( a -- )   DUP >BY @ CHECK30 TALLY-BY AND!  >BA @ TALLY-BA OR!U ;
: COMMA ( a -- )   DUP >DATA @ >R  TALLY:,,  CHECK32  R> EXECUTE ;
IS-A IS-COMMA : COMMAER CREATE-- REMEMBER STARTVOC >DATA !  0 STARTVOC >BI !
    STARTVOC >BY !  STARTVOC >BA !  STARTVOC >CNT !  STARTVOC >DIS !
    DOES>  COMMA ;
CREATE PRO-TALLY 3 CELLS ALLOT
: T! PRO-TALLY !+ !+ !+ DROP ;
: T!R   REVERSE-BYTES T! ; 
: T@ PRO-TALLY 3 CELLS +  @- @- @- DROP ;
: 1FAMILY,    0 DO   DUP >R T@ R> 1PI   OVER + LOOP DROP DROP ;
: 2FAMILY,    0 DO   DUP >R T@ R> 2PI   OVER + LOOP DROP DROP ;
: 3FAMILY,    0 DO   DUP >R T@ R> 3PI   OVER + LOOP DROP DROP ;
: 4FAMILY,    0 DO   DUP >R T@ R> 4PI   OVER + LOOP DROP DROP ;
: xFAMILY|    0 DO   DUP >R T@ R> xFI   OVER + LOOP DROP DROP ;
: FAMILY|R    0 DO   DUP >R T@ REVERSE-BYTES R> FIR   OVER + LOOP DROP DROP ;
: xFAMILY|F   0 DO   DUP >R T@ R> DFI   OVER + LOOP DROP DROP ;
( ############### PART II DISASSEMBLER ################################ )
12 BAG DISS
: !DISS DISS !BAG ;
: .DISS-AUX DISS @+ SWAP DO
        I @ DUP IS-COMMA OVER IS-DFI OR OVER IS-DFIs OR IF I DISS - . THEN
        [DEFINED] ForSwiftForth [IF]  .'  [THEN]
        [DEFINED] ForGForth [IF]  ID.  [THEN]
    0 CELL+ +LOOP  CR ;
VARIABLE DISS-VECTOR    ' .DISS-AUX DISS-VECTOR !
: +DISS DISS BAG+! ;
: DISS? DISS BAG? ;
: DISS- 0 CELL+ NEGATE DISS +! ;
: TRY-PI
    DUP IS-PI IF
    AT-REST? IF
        DUP TALLY:,
        DUP +DISS
    THEN
    THEN ;
: TRY-xFI
    DUP IS-xFI IF
    DUP >BI @ TALLY-BI @ CONTAINED-IN IF
        DUP TALLY:|
        DUP +DISS
    THEN
    THEN ;
: TRY-DFI
    DUP IS-DFI OVER IS-DFIs OR IF
    DUP >BI @ TALLY-BI @ CONTAINED-IN IF
        DUP TALLY:|
        DUP +DISS
    THEN
    THEN ;
: TRY-FIR
    DUP IS-FIR IF
    DUP >BI @ CORRECT-R TALLY-BI @ CONTAINED-IN IF
        DUP TALLY:|R
        DUP +DISS
    THEN
    THEN ;
: TRY-COMMA
    DUP IS-COMMA IF
    DUP >BY @ TALLY-BY @ CONTAINED-IN IF
        DUP TALLY:,,
        DUP +DISS
    THEN
    THEN ;
: REBUILD
    !TALLY
    DISS? IF
        DISS @+ SWAP !DISS DO  ( Get bounds before clearing)
            I @ TRY-PI TRY-xFI TRY-DFI TRY-FIR TRY-COMMA DROP
        0 CELL+ +LOOP
    THEN ;
: BACKTRACK
(   S" BACKTRACKING" TYPE                                               )
    DROP DISS @ @- DISS !
(   DROP DISS @ 0 CELL+ - @                                             )
(   S" Failed at :" TYPE DUP ID. CR                                     )
    >NEXT%
(   DISS-                                                               )
    REBUILD ;
: RESULT? AT-REST? DISS? AND   BAD? 0= AND ;
: .RESULT
    RESULT? IF
        DISS-VECTOR @ EXECUTE
        DISS-
        REBUILD
    THEN ;
\     % RESULT +DISS Spurious? Remove after next total test.
: SHOW-STEP
    TRY-PI TRY-DFI TRY-xFI TRY-FIR TRY-COMMA
    .RESULT
    >NEXT%
(       DUP ID.                                                         )
    BAD? IF BACKTRACK THEN
    BEGIN DUP VOCEND? DISS? AND WHILE BACKTRACK REPEAT ;
: SHOW-ALL ( -- )
    !DISS   !TALLY
    STARTVOC BEGIN
        SHOW-STEP
    DUP VOCEND? UNTIL DROP ;
: SHOW-OPCODES ( -- )
    !DISS   !TALLY
    STARTVOC BEGIN
        DUP IS-PI IF DUP %ID. THEN >NEXT%
    DUP VOCEND? UNTIL DROP ;
: SHOW:
    !DISS   !TALLY
    ' DUP BEGIN
        SHOW-STEP
    OVER DISS CELL+ @ - OVER VOCEND? OR UNTIL DROP DROP ;
VARIABLE AS-POINTER       HERE AS-POINTER !
: INSTRUCTION  ISS @   ISL @   MC@ ;
VARIABLE LATEST-INSTRUCTION
: DIS-PI ( dea -- dea )
    DUP IS-PI IF
    AT-REST? IF
    DUP >BI OVER >CNT @  MC@ INVERT
    >R AS-POINTER @ OVER >CNT @  MC@ R>   AND
    OVER >DATA @ = IF
        DUP TALLY:,
        DUP +DISS
        DUP LATEST-INSTRUCTION !
        AS-POINTER @ ISS !
        DUP >CNT @ AS-POINTER +!
    THEN
    THEN
    THEN ;
: DIS-xFI ( dea -- dea )
    DUP IS-xFI IF
    DUP >BI @ TALLY-BI @ CONTAINED-IN IF
    DUP >BI @ INSTRUCTION AND   OVER >DATA @ = IF
    DUP >BA @  CONSISTENT? IF
        DUP TALLY:|
        DUP +DISS
    THEN
    THEN
    THEN
    THEN ;
: DIS-DFI ( dea -- dea )
    DUP IS-DFI OVER IS-DFIs OR IF
    DUP >BI @ TALLY-BI @ CONTAINED-IN IF
    DUP >BA @  CONSISTENT? IF
        DUP TALLY:|
        DUP +DISS
    THEN
    THEN
    THEN ;
: DIS-DFIR ( dea -- dea )
    DUP IS-DFIR IF
    DUP >BI @ CORRECT-R   TALLY-BI @ CONTAINED-IN IF
    DUP >BA @  CONSISTENT? IF
        DUP TALLY:|R
        DUP +DISS
    THEN
    THEN
    THEN ;
: DIS-FIR ( dea -- dea )
    DUP IS-FIR IF
    DUP >BI @ CORRECT-R   TALLY-BI @ CONTAINED-IN IF
    DUP >BI @ CORRECT-R   INSTRUCTION AND   OVER >DATA @ CORRECT-R = IF
    DUP >BA @  CONSISTENT? IF
        DUP TALLY:|R
        DUP +DISS
    THEN
    THEN
    THEN
    THEN ;
: DIS-COMMA ( dea -- dea )
    DUP IS-COMMA IF
    DUP >BY @ TALLY-BY @ CONTAINED-IN IF
    DUP >BA @  CONSISTENT? IF
        DUP TALLY:,,
        DUP +DISS
    THEN
    THEN
    THEN ;
: .DFI
    INSTRUCTION   OVER >BI @ AND   OVER >DATA @ RSHIFT   U.
    %ID. ;
: .DFIR
    INSTRUCTION   OVER >BI @ CORRECT-R AND   OVER >DATA @ RSHIFT
    REVERSE-BYTES CORRECT-R U.
    %ID. ;
: .COMMA-STANDARD
    AS-POINTER @ OVER >CNT @ MC@ U.
    DUP >CNT @ AS-POINTER +!
    %ID. ;
: .COMMA-SIGNED
    AS-POINTER @ OVER >CNT @ MC@ .
    DUP >CNT @ AS-POINTER +!
    %ID. ;
: .COMMA   DUP >DIS @ IF   DUP >DIS @ EXECUTE   ELSE
        .COMMA-STANDARD   THEN ;
: %~ID. DUP IGNORE? IF DROP ELSE %ID. THEN  ;
: .DISS   DISS @+ SWAP DO
        I @
        DUP IS-COMMA IF
            .COMMA
        ELSE DUP IS-DFI IF
            .DFI
        ELSE DUP IS-DFIs IF
            .DFI            \ For the moment.
        ELSE DUP IS-DFIR IF
            .DFIR
        ELSE
            %~ID.
        THEN THEN THEN THEN
    0 CELL+ +LOOP ;
VARIABLE I-ALIGNMENT    1 I-ALIGNMENT !   ( Instruction alignment )

: SHOW-MEMORY ( a -- a' )   BEGIN  COUNT C. S"  C, " TYPE
        DUP I-ALIGNMENT @ MOD WHILE  REPEAT ;
: ((DISASSEMBLE)) ( a dea -- a' )
    SWAP
    DUP AS-POINTER !  >R
    3 SPACES
    ( startdea -- ) BEGIN
        DIS-PI DIS-xFI DIS-DFI DIS-DFIR DIS-FIR DIS-COMMA
        >NEXT%
(       DUP ID. S" : " TYPE  DISS-VECTOR @ EXECUTE                         )
    DUP VOCEND? RESULT? OR UNTIL DROP
    RESULT? IF
        .DISS     \ Advances pointer past commaers
        LATEST-INSTRUCTION @ >PRF @ BA-XT !
        R> DROP AS-POINTER @
    ELSE
        R> SHOW-MEMORY
    THEN ;
: (DISASSEMBLE) ( a -- a' )   !DISS !TALLY STARTVOC ((DISASSEMBLE)) ;
: DIS ( x -- )   PAD !  PAD (DISASSEMBLE) ;
: FORCED-DISASSEMBLY ( dea -- )
    !DISS  !TALLY  AS-POINTER @ SWAP ((DISASSEMBLE)) DROP ;
: DISASSEMBLE-RANGE ( first last -- )
    SWAP  BEGIN DUP ADORN-ADDRESS  (DISASSEMBLE) 2DUP > 0= UNTIL  2DROP ;
: END-CODE
    ?CSP ?EXEC CHECK26 CHECK32 PREVIOUS ; IMMEDIATE

( FIXME : we must get rid of this one )
: ;C POSTPONE END-CODE S" WARNING: get rid of C;" TYPE CR ; IMMEDIATE

\ The following two definitions must *NOT* be in the assembler wordlist.
PREVIOUS DEFINITIONS DECIMAL

ALSO ASSEMBLER
: CODE
    ?EXEC ASM-CREATE POSTPONE ASSEMBLER !TALLY !CSP ; IMMEDIATE
: ;CODE
    ?CSP   POSTPONE (;CODE)   POSTPONE [   POSTPONE ASSEMBLER ; IMMEDIATE
: DDD ( a -- a' )   (DISASSEMBLE) ;
: D-R ( first last -- )   DISASSEMBLE-RANGE ;
PREVIOUS 
( $Id: aswrap.frt,v 1.21 2009/03/26 19:40:39 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

: HOT-PATCH ( xt -- ) ' >BODY ! ;
VARIABLE CODE-LENGTH 2000000 CODE-LENGTH !
100 BAG SECTION-REGISTRY
0 VALUE CURRENT-SECTION

: SECTION-FIELD ( u size -- u' ) CREATE OVER , +
    DOES> ( -- a ) @ CURRENT-SECTION + ;

0
    2 CELLS SECTION-FIELD SECTION-RESERVED
    1 CELLS SECTION-FIELD CP \ The local dictionary pointer ("code pointer")
    1 CELLS SECTION-FIELD 'CODE-SPACE \ Start of the code space
    1 CELLS SECTION-FIELD 'TARGET-START \ Return corresponding target address.
    1 CELLS SECTION-FIELD 'FILE-OFFSET \ Return corresponding files address.
CONSTANT |SECTION|

: CODE-SPACE ( -- a ) 'CODE-SPACE @ ;
: -ORG- ( a -- ) 'TARGET-START ! ;
: TARGET-START ( -- a ) 'TARGET-START @ ;
: FILE-OFFSET ( -- a ) 'FILE-OFFSET @ ;
: ((SECTION)) ( file target code -name- )
    SAVE-INPUT CREATE HERE DUP >R DUP SECTION-REGISTRY BAG+!
    |SECTION| DUP ALLOT ERASE RESTORE-INPUT THROW BL WORD $@ $, R@ >NFA !
    CURRENT-SECTION R@ >LFA ! R> TO CURRENT-SECTION DUP CP !
    'CODE-SPACE ! 'TARGET-START ! 'FILE-OFFSET !
    DOES> TO CURRENT-SECTION ;
: (SECTION) ( file target -- ) CODE-LENGTH @ ALLOCATE THROW ((SECTION)) ;
CREATE 'SECTION ' (SECTION) DUP , , : SECTION 'SECTION @ EXECUTE ;
: DEFAULT-SECTION ( -- )
    0 \ File start address
    0 \ Target start address
    s" SECTION the-default-section" EVALUATE ;
DEFAULT-SECTION
:NONAME ( -- a ) CP @ ; HOT-PATCH 'AS-HERE
: TARGET-END ( -- a ) TARGET-START CP @ CODE-SPACE - + ;
: PLAUSIBLE-LABEL? ( a -- f ) TARGET-START TARGET-END WITHIN ;
: HOST-END ( -- a ) CP @ ;
: ORG ( a -- ) -ORG- CODE-SPACE CP ! ;
: HOST>TARGET ( a -- a' ) CODE-SPACE - TARGET-START + ;
: TARGET>HOST ( a -- a' ) TARGET-START - CODE-SPACE + ;
: FILE>TARGET ( a -- a' ) FILE-OFFSET - TARGET-START + ;
: TARGET>FILE ( a -- a' ) TARGET-START - FILE-OFFSET + ;

\ Abbreviation.
[DEFINED] ForSwiftForth NOT [IF]
    ' TARGET>HOST ALIAS th
[THEN]

: SUB-SECTION ( a -name- ) DUP TARGET>FILE
    OVER ROT TARGET>HOST CP @ >R ((SECTION)) R> CP ! ;
:NONAME ( -- a ) CP @ HOST>TARGET ; HOT-PATCH '_AP_
[DEFINED] ForCiForth
[DEFINED] ForGForth OR [IF]
    : SWAP-AS ( -- ) CP @ DP @ CP ! DP ! ;
[THEN]
[DEFINED] ForSwiftForth [IF]
    : SWAP-AS ( -- ) CP @ H @ CP ! H ! ;
[THEN]
:NONAME ( n -- ) CP +! ; HOT-PATCH 'AS-ALLOT
:NONAME ( c -- ) CP @ 1 AS-ALLOT C! ; HOT-PATCH 'AS-C,
: RESB ( u -- ) AS-HERE OVER AS-ALLOT SWAP ERASE ;
: RES-TIL ( a -- ) _AP_ - AS-ALLOT ;
: AS-ALIGN ( n -- ) _AP_ BEGIN 2DUP SWAP MOD WHILE 1+ REPEAT RES-TIL DROP ; 
( $Id: asi386.frt,v 4.23 2005/05/09 01:00:40 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)

ALSO ASSEMBLER DEFINITIONS HEX

 : (W,) lsbyte, lsbyte, DROP ;
 : (L,) lsbyte, lsbyte, lsbyte, lsbyte, DROP ;

 ( ############## 80386 ASSEMBLER PROPER ############################### )
 ( The decreasing BY means that a decompiler hits them in the right      )
 ( order to reassemble.                                                  )
 ( Fields: a disassembly XT,      LENGTH to comma, the BA BY information )
 ( and the XT that puts data in the dictionary.                          )
 ( Where there is a placeholder ``_'' the execution token is filled in   )
 ( later. )
 (   CNT                                                                 )
 ( XT   BA   BY     XT-AS          NAME                                  )
   0 2  0000 00100 ' (W,) COMMAER OW,    ( obligatory word     )
   0 4  8000 00080 ' (L,) COMMAER (RL,)  ( cell relative to IP )
   0 2  4000 00080 ' (W,) COMMAER (RW,)  ( cell relative to IP )
   0 1  0000 00040 ' AS-C, COMMAER (RB,) ( byte relative to IP )
   0 2  0000 00020 ' (W,) COMMAER SG,    (  Segment: WORD      )
   0 1  0000 00010 ' AS-C, COMMAER P,    ( port number ; byte     )
   0 1  0000 00008 ' AS-C, COMMAER IS,   ( Single -obl-  byte )
   0 4 20002 00004 ' (L,) COMMAER IL,    ( immediate data : cell)
   0 2 10002 00004 ' (W,) COMMAER IW,    ( immediate data : cell)
   0 1  0001 00004 ' AS-C, COMMAER IB,   ( immediate byte data)
   0 4  8008 00002 ' (L,) COMMAER L,     ( immediate data : address/offset )
   0 2  4008 00002 ' (W,) COMMAER W,     ( immediate data : address/offset )
   0 1  0004 00002 ' AS-C, COMMAER B,    ( immediate byte : address/offset )
   _ 1  0000 00001 _    COMMAER SIB,, ( An instruction with in an instruction )

 ( Meaning of the bits in TALLY-BA :                                     )
 ( Inconsistent:  0001 OPERAND IS BYTE     0002 OPERAND IS CELL  W/L     )
 (                0004 OFFSET  IS BYTE     0008 OFFSET  IS CELL  W/L     )
 ( By setting 0020 an opcode can force a memory reference, e.g. CALLFARO )
 (               0010 Register op         0020 Memory op                 )
 (               0040 ZO|                 0080 [BP]% {16} [BP] [BP  {32} )
 (  sib:         0100 no ..             0200 [AX +8*| DI]                )
 (  logical      0400 no ..             0800 Y| Y'| Z| Z'|               )
 (  segment      1000 no ..             2000 ES| ..                      )
 (  AS:          4000 16 bit Addr       8000 32 bit Address              )
 (  OS:         10000 16 bit Op        20000 32 bit Operand              )
 (  Use debug   40000 no ..            80000 CR0 ..DB0                   )
 (  FP:        100000 FP-specific     200000 Not FP                      )

 ( Names *ending* in percent BP|% -- not BP'| the prime registers -- are )
 ( only valid for 16 bits mode, or with an address overwite. Use W, L,   )
 ( appropriately.                                                        )

8200 0 38 T!R
  08 00 8 FAMILY|R AX] CX] DX] BX] 0] BP] SI] DI]
 8200 0 0C0 T!R
  40 00 4 FAMILY|R  +1* +2* +4* +8*
 8200 0 7 REVERSE-BYTES 1 + T!
  01 00 8 FAMILY|R [AX [CX [DX [BX [SP -- [SI [DI
 8280 00 1 REVERSE-BYTES 7 + 05 FIR [BP   ( Fits in the hole, but disallow ZO| )
 8248 02 1 REVERSE-BYTES 7 + 05 FIR [MEM  ( Fits in the hole, but requires ZO| )

4120 0 07 T!R
   01 00 8
     FAMILY|R [BX+SI]% [BX+DI]% [BP+SI]% [BP+DI]% [SI]% [DI]% -- [BX]%
 40A0 0000 07 06 FIR [BP]%  ( Fits in the hole, safe inconsistency check)
 8120 0 07 T!R
  01 00 4 FAMILY|R [AX] [CX] [DX] [BX]
 8120 01 07 04 FIR ~SIB|   ( Fits in the hole, but requires ~SIB, )
 81A0 00 07 05 FIR [BP]   ( Fits in the hole, but disallow ZO| )
 8120 0 07 T!R  01 06 2 FAMILY|R [SI] [DI]

 200111 0 07 T!R  01 00 8 FAMILY|R AL| CL| DL| BL| AH| CH| DH| BH|
 200112 0 07 T!R
  01 00 8 FAMILY|R AX| CX| DX| BX| SP| BP| SI| DI|
 0160 00 0C0 00 FIR      ZO|
 0124 02 0C0 40 FIR      BO| 0128 02 0C0 80 FIR      XO|
 200110 00 0C0 0C0 FIR      R|
 204048 02 0C7 06 FIR      MEM|% ( Overrules ZO| [BP]% )
 208108 02 0C7 05 FIR      MEM| ( Overrules ZO| [BP] )

 241101 0000 38 T!R
  08 00 8 FAMILY|R AL'| CL'| DL'| BL'| AH'| CH'| DH'| BH'|
 241102 0000 38 T!R  08 00 8 FAMILY|R AX'| CX'| DX'| BX'| SP'| BP'| SI'| DI'|
 242100 0000  38 T!R   08 00 6 FAMILY|R ES| CS| SS| DS| FS| GS|
 280002 0000 138 REVERSE-BYTES T!   ( 3)
  08 00 5 FAMILY|R CR0| -- CR2| CR3| CR4|                 ( 3)
  0008 0100 8 FAMILY|R DR0| DR1| DR2| DR3| DR4| DR5| DR6| DR7| ( 3)

 200000 0000 0200 T!R  0200 00 2 FAMILY|R F| T|
 240401 0000 0100 0000 FIR B|
 240402 0000 0100 0100 FIR X|

0600 0 01FF 0000 1PI ~SIB,
 041000 0000 FF03 T!
  0008 0000 8 2FAMILY, ADD, OR, ADC, SBB, AND, SUB, XOR, CMP,
 041000 0000 FF01 T!
  0002 0084 2 2FAMILY, TEST, XCHG,
 041000 0000 FF03 0088 2PI MOV,
 1022 0 FF00 008D 2PI LEA,
 1022 0 FF00 T!   0001 00C4 2 2FAMILY, LES, LDS,
 1022 0 FF00 0062 2PI BOUND,  ( 3)
 1002 0 FF00 0063 2PI ARPL,   ( 3)
 1002 04 FF00 0069 2PI IMULI, ( 3)
 1002 08 FF00 006B 2PI IMULSI, ( 3)
 1002 0 FF0000 T! 0100 00020F 2 3FAMILY, LAR, LSL, ( 3)
 1002 0 FF0000 T! 0800 00A30F 4 3FAMILY, BT, BTS, BTR, BTC, ( 3)
 1002 0 FF0000 T! 0800 00A50F 2 3FAMILY, SHLD|C, SHRD|C,    ( 3)
 1002 0 FF0000 T! 0100 00BC0F 2 3FAMILY, BSF, BSR,          ( 3)
 1002 08 FF0000 T! 0800 00A40F 2 3FAMILY, SHLDI, SHRDI,    ( 3)
 1022 0 FF0000 T! 0100 00B20F 4 3FAMILY, LSS, -- LFS, LGS, ( 3)
 1501 0 FF0000 T! 0800 00B60F 2 3FAMILY, MOVZX|B, MOVSX|B,  ( 3)
 1502 0 FF0000 T! 0800 00B70F 2 3FAMILY, MOVZX|W, MOVSX|W,  ( 3)
 1002 0 FF0000 00AF0F 3PI IMUL,                     ( 3)
 0 04 C701 00C6 2PI MOVI,
 0012 0 0007 T!   0008 40 4 1FAMILY, INC|X, DEC|X, PUSH|X, POP|X,
 0012 0 0007 90 1PI XCHG|AX,
 0011 04 0007 B0 1PI MOVI|B,
 0012 04 0007 B8 1PI MOVI|X,
 0 04 C701 T!
  0800 0080 8 2FAMILY, ADDI, ORI, ADCI, SBBI, ANDI, SUBI, XORI, CMPI,
 0002 08 C700 T!
  0800 0083 8 2FAMILY, ADDSI, ORSI, ADCSI, SBBSI, ANDSI, SUBSI, XORSI, CMPSI,
 0000 0 C701 T!
  0800 10F6 6 2FAMILY, NOT, NEG, MUL|AD, IMUL|AD, DIV|AD, IDIV|AD,
  0800 00FE 2 2FAMILY, INC, DEC,
 0 04 C701 00F6 2PI TESTI,
 0002 0 C700 008F 2PI POP,
 0002 0 C700 30FF 2PI PUSH,
 0002 0 C700 T!  1000 10FF 2 2FAMILY, CALLO, JMPO,
 0022 0 C700 T!  1000 18FF 2 2FAMILY, CALLFARO, JMPFARO,
 0002 08 C70000 T!  080000 20BA0F 4 3FAMILY, BTI, BTSI, BTRI, BTCI, ( 3)
 0002 0 C70000 T! ( It says X but in fact W : descriptor mostly - ) ( 3)
   080000 00000F 6 3FAMILY, SLDT, STR, LLDT, LTR, VERR, VERW,  ( 3)
   100000 20010F 2 3FAMILY, SMSW, LMSW,       ( 3)
 0022 0 C70000 T! ( It says X but in fact memory of different sizes) ( 3)
   080000 00010F 4 3FAMILY, SGDT, SIDT, LGDT, LIDT, ( 3)
 0001 0 02000001 00 FIR B'|
 0002 0 02000001 01 FIR X'|
 0008 02 0201 T!    0002 A0 2 1FAMILY, MOV|TA, MOV|FA,
 0 04 0201 T!
  0008 04 8 1FAMILY, ADDI|A, ORI|A, ADCI|A, SBBI|A, ANDI|A, SUBI|A, XORI|A, CMPI|A,
 0000 04 0201 00A8 1PI TESTI|A,
 0000 0 0201 T!  0002 A4 6 1FAMILY, MOVS, CMPS, -- STOS, LODS, SCAS,
 0 10 0201 T!   0002 E4 2 1FAMILY, IN|P, OUT|P,
 0 00 0201 T!   0002 EC 2 1FAMILY, IN|D, OUT|D,
 0 00 0201 T!   0002 6C 2 1FAMILY, INS, OUTS,     ( 3)
 0800     0000 01000001 T!R     01 00 2 FAMILY|R Y| N|
 0800     0000 0400000E T!R     02 00 8 FAMILY|R O| C| Z| CZ| S| P| L| LE|
 0800 40 050F 0070 1PI J,

 2102 0 FF02 008C 2PI MOV|SG,

 0000 0 02000200 0000 FIR 1|   0000 0 02000200 0200 FIR V|          ( 3)
 0100 0 2C703 T! ( 20000 is a lockin for 1| V|)                   ( 3)
  0800 00D0 8 2FAMILY, ROL, ROR, RCL, RCR, SHL, SHR, -- SAR,  ( 3)
 0000 8 C701 T!  0800 00C0 8 2FAMILY, ROLI, RORI, RCLI, RCRI, SHLI, SHRI, -- SARI,  ( 3)
 80012 0000 3F0300 C0200F 3PI  MOV|CD,  ( 3)

 0800 80 50F00 800F 2PI J|X,                                           ( 3)
 0800 0 0100 T!R  0100 0000 2 FAMILY|R Y'| N'|                          ( 3)
 0800 0 0E00 T!R  0200 0000 8 FAMILY|R O'| C'| Z'| CZ'| S'| P'| L'| LE'| ( 3)
 0901 0 C70F00 00900F 3PI SET,  ( 3)
 2000 0000 0 T!  0008 06 4 1FAMILY, PUSH|ES, PUSH|CS, PUSH|SS, PUSH|DS,
 2000 0000 0 T!  0008 07 4 1FAMILY, POP|ES, -- POP|SS, POP|DS,

 0001 04 0000 T!    0001 D4 2 1FAMILY, AAM, AAD, 0001 04 0000 0CD 1PI INT,
 0008 22 0000 09A 1PI CALLFAR,
 0008 22 0000 0EA 1PI JMPFAR,
 0 0100 0000 T!   0008 C2 2 1FAMILY, RET+, RETFAR+,
 0004 80 0000 T!   0001 E8 2 1FAMILY, CALL, JMP,
 0 40 0000 0EB 1PI JMPS,
 0 40 0000 T!   0001 E0 4 1FAMILY, LOOPNZ, LOOPZ, LOOP, JCXZ,
 0000 0 0000 T!
    0008   0026 4 1FAMILY, ES:, CS:, SS:, DS:,
    0008   0027 4 1FAMILY, DAA, DAS, AAA, AAS,
    0001   0098 8 1FAMILY, CBW, CWD, -- WAIT, PUSHF, POPF, SAHF, LAHF,
    0008   00C3 2 1FAMILY, RET,  RETFAR,
    0001   00CC 4 1FAMILY, INT3, -- INTO, IRET,
    0001   00F0 6 1FAMILY, LOCK, -- REPNZ, REPZ, HLT, CMC,
    0001   00F8 6 1FAMILY, CLC, STC, CLI, STI, CLD, STD,
    0001   0060 2 1FAMILY, PUSH|ALL, POP|ALL, ( 3)
    0001   0064 4 1FAMILY, FS:, GS:, OS:, AS:, ( 3)
  0100 A00F 3 2FAMILY, PUSH|FS, POP|FS, CPUID,
  0100 A80F 2 2FAMILY, PUSH|GS, POP|GS, ( RSM,)
   0002 04 0000   0068 1PI PUSHI|X,  ( 3)
   0001 04 0000   006A 1PI PUSHI|B,  ( 3)
   0001 0104 0000   00C8 1PI ENTER, ( 3)
       0000 0 00   00C9 1PI LEAVE, ( 3)
       0000 0 00   00D7 1PI XLAT,  ( 3)
       0000 0 00 060F 2PI CLTS,  ( 3)
: (SIB),,   TALLY-BY @   0 TALLY-BY !
    CHECK32 TALLY-BA @ 0900 INVERT AND TALLY-BA !   ['] NOOP BA-XT !
    ~SIB,   TALLY-BY !   ;
' (SIB),,  ' SIB,, >BODY >DATA !
: DIS-SIB DROP
    LATEST-INSTRUCTION @        \ We don't want sib visible.
    [ ' ~SIB, >BODY ] LITERAL FORCED-DISASSEMBLY
    LATEST-INSTRUCTION ! ;
' DIS-SIB    ' SIB,, >BODY >DIS !
-warning
: [AX   ~SIB| SIB,, [AX ;
: [SP   ~SIB| SIB,, [SP ;
: [CX   ~SIB| SIB,, [CX ;
: [BP   ~SIB| SIB,, [BP ;
: [DX   ~SIB| SIB,, [DX ;
: [SI   ~SIB| SIB,, [SI ;
: [BX   ~SIB| SIB,, [BX ;
: [DI   ~SIB| SIB,, [DI ;
: [MEM  ~SIB| SIB,, [MEM ;
+warning
:NONAME   TALLY-BA  C000 TOGGLE ;  ' AS:, >BODY >PRF !
:NONAME   TALLY-BA 30000 TOGGLE ;  ' OS:, >BODY >PRF !

( ############## 80386 ASSEMBLER PROPER END ########################### )
: RB, _AP_ 1 + - (RB,) ;    ' .COMMA-SIGNED   ' (RB,) >BODY >DIS !
: RW, _AP_ 2 + - (RW,) ;    ' .COMMA-SIGNED   ' (RW,) >BODY >DIS !
: RL, _AP_ 4 + - (RL,) ;    ' .COMMA-SIGNED   ' (RL,) >BODY >DIS !
: BITS-32   28000 BA-DEFAULT ! ;
: BITS-16   14000 BA-DEFAULT ! ;

BITS-32
PREVIOUS DEFINITIONS DECIMAL
( ############## 8086 ASSEMBLER POST ################################## )

\ Tools
( $Id: access.frt,v 1.6 2005/01/04 19:20:35 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

ALSO ASSEMBLER
: B@ TARGET>HOST C@ ;
: W@ TARGET>HOST 2 MC@ ;
: L@ TARGET>HOST 4 MC@ ;
PREVIOUS
( $Id: labelas.frt,v 1.17 2009/03/26 19:40:39 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

[DEFINED] ForCiForth [IF]
    REQUIRE BAG             \ Simple bag facility
    REQUIRE DO-BAG          \ More advanced bag facility
    REQUIRE POSTFIX
[THEN]

: FIX-DEA DROP ['] _ ;
[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
     : BACKSPACE-IN   -2 >IN +! 0 ;
[THEN]
[DEFINED] ForCiForth [IF]
    : BACKSPACE-IN   IN[] IF -2 IN +! THEN DROP ;
[THEN]
[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
    : FIX-NMB   -1 >IN +!  BL WORD DROP  BACKSPACE-IN ;
[THEN]
[DEFINED] ForCiForth [IF]
    : FIX-NMB   -1 IN +!  (WORD) 2DROP  BACKSPACE-IN   0 DPL ! ;
[THEN]
: ERROR10 ( f n -- )   DROP IF  FIX-NMB  THEN ;
: ERROR12 ( f n -- )   DROP IF  FIX-DEA  THEN ;

[DEFINED] ForCiForth [IF]
    REQUIRE OLD:
[THEN]

: ?ERROR-FIXING ( f n -- )
    DUP 10 = IF  ERROR10  ELSE
    DUP 12 = IF  ERROR12  ELSE
        (?ERROR)
    THEN THEN ;
: RESET-SECTION ( file target -- )   2DROP  BL WORD COUNT EVALUATE  CODE-SPACE CP ! ;
: FIRSTPASS ( -- )   CR S" FIRSTPASS " TYPE CR
    ['] ?ERROR-FIXING '?ERROR !
    'SECTION RESTORED ;

: SECONDPASS ( -- )   CR S" SECONDPASS " TYPE CR
    ['] RESET-SECTION 'SECTION !
    '?ERROR RESTORED ;
CREATE 'LABELS  HERE ,
: IS-A-LABEL? ( a n -- f )   GET-CURRENT SEARCH-WORDLIST DUP IF
        SWAP >BODY  BEGIN  >LFA ['] @ CATCH IF  DROP 0= EXIT
            THEN  ?DUP WHILE  DUP 'LABELS = IF  DROP EXIT
                THEN  REPEAT  0=  THEN ;
: KNOWN-LABEL? ( a n -- a n f )   2DUP IS-A-LABEL? >R
    R@ IF  2DUP GET-CURRENT SEARCH-WORDLIST IF
            EXECUTE _AP_ <> IF  S" ERROR: phase error defining label "
                TYPE  2DUP TYPE  CR
    THEN THEN THEN  R> ;

[DEFINED] ForCiForth [IF]
    'ONLY >WID CURRENT !  \ Making ONLY the CONTEXT is dangerous! This will do.
    : : (WORD)
        KNOWN-LABEL? IF 2DROP ELSE 2>R _AP_ 2R> POSTFIX CONSTANT THEN ;
    PREFIX IMMEDIATE DEFINITIONS
[THEN]

100 BAG DX-SET    : !DX-SET DX-SET !BAG ;
: GET-DX-SET ( -- )   DEPTH >R
    BEGIN  BL WORD DUP C@  WHILE  FIND IF  EXECUTE  ELSE
            COUNT 0 0 2SWAP >NUMBER 2DROP DROP
        THEN  REPEAT  DROP
    DEPTH R> ?DO  DX-SET BAG+!  LOOP ;
: C,-DX-SET ( -- )   BEGIN  DX-SET BAG@- AS-C,  DX-SET BAG? 0=  UNTIL ;
: db ( -- )   !DX-SET  GET-DX-SET  C,-DX-SET ;
ALSO ASSEMBLER
: W,-DX-SET ( -- )   BEGIN  DX-SET BAG@- (W,)  DX-SET BAG? 0=  UNTIL ;
: dw ( -- )   !DX-SET  GET-DX-SET  W,-DX-SET ;
: L,-DX-SET ( -- )   BEGIN  DX-SET BAG@- (L,)  DX-SET BAG? 0=  UNTIL ;
: dl ( -- )   !DX-SET  GET-DX-SET  L,-DX-SET ;
: ($,) ( a n -- )   AS-HERE SWAP DUP AS-ALLOT MOVE ;
: $,-DX-SET ( -- )
    BEGIN  DX-SET BAG@- DUP 256 0 WITHIN IF
        DX-SET BAG@- ($,)  ELSE  AS-C,  THEN
    DX-SET BAG? 0=  UNTIL ;
: d$ ( -- )   !DX-SET  GET-DX-SET  $,-DX-SET ;
: "   [CHAR] " PARSE ;
PREVIOUS
( $Id: labeldis.frt,v 1.86 2010/06/03 23:27:28 albert Exp $ )
( Copyright{2004}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

[DEFINED] ForCiForth [IF]
    REQUIRE ALIAS    REQUIRE @+    REQUIRE QSORT    REQUIRE EXCHANGE
    REQUIRE BIN-SEARCH    REQUIRE POSTFIX    REQUIRE H. \ In behalf of (DH.)
    REQUIRE 2>R REQUIRE BAG \ Simple bag facility
[THEN]

1000 CONSTANT MAX-LABEL

\ -------------------- INTRODUCTION --------------------------------

100 BAG THE-REGISTER
: REALLOC ( u -- a )   HERE >R  DUP ALLOT  R@ SWAP MOVE   R> ;
: REALLOC-POINTER ( a n -- )   >R  DUP @ R> REALLOC  SWAP ! ;
0 VALUE CURRENT-LABELSTRUCT \ current label pointer

: LABEL-FIELD ( u size -- u' )   CREATE  OVER , +
    DOES> ( -- a )   @ CURRENT-LABELSTRUCT + ;

0
    2 CELLS LABEL-FIELD LABEL-RESERVED
    1 CELLS LABEL-FIELD 'CURRENT-LABEL
    1 CELLS LABEL-FIELD 'DECOMP           \ (Re)generate source for INDEX.
    1 CELLS LABEL-FIELD '.PAY             \ Print payload
    1 CELLS LABEL-FIELD 'MAX-LAB
    0 LABEL-FIELD LABELS            \ Return ADDRESS
CONSTANT |LABELSTRUCT|

: CURRENT-LABEL ( -- a )   'CURRENT-LABEL @ ;
: DECOMP ( -- )   'DECOMP @ EXECUTE ;
: .PAY ( -- )   '.PAY @ EXECUTE ;

: DOUBLE-SIZE ( -- )   'MAX-LAB DUP @ 2* SWAP ! ;
: MAX-LAB ( -- n )   'MAX-LAB @ ;
: LAB-UPB ( -- n )   LABELS |BAG| 2/ ;
: >RELOCATABLE ( -- )   LABELS DUP @  OVER - SWAP ! ;
: RELOCATABLE> ( -- )   LABELS DUP +! ;
: ?REALLOC? ( -- )
    MAX-LAB LAB-UPB = IF  DOUBLE-SIZE
        >RELOCATABLE  CURRENT-LABEL MAX-LAB 2* 6 + CELLS REALLOC-POINTER
        CURRENT-LABEL EXECUTE   RELOCATABLE>  THEN ;

: LAB+! ( a -- )   LABELS BAG+!  ?REALLOC? ;

: LABELSTRUCT ( n print gen -- )   SAVE-INPUT  CREATE  HERE DUP >R
    DUP THE-REGISTER BAG+!  |LABELSTRUCT| DUP ALLOT  ERASE
    RESTORE-INPUT THROW  BL WORD $@ $, R@ >NFA !
    CURRENT-LABELSTRUCT R@ >LFA !  R> TO CURRENT-LABELSTRUCT
    DUP 'CURRENT-LABEL !  'DECOMP !  '.PAY !  DUP 'MAX-LAB ! 2* BUILD-BAG
    DOES>  TO CURRENT-LABELSTRUCT ;

: LABELS[] ( n -- a )   1- 2* CELLS LABELS CELL+ + ;
: REMOVE-LABEL ( -- )   LABELS[] LABELS  2DUP BAG-REMOVE BAG-REMOVE ;
: DO-LAB   POSTPONE LABELS POSTPONE DO-BAG ; IMMEDIATE
: LOOP-LAB   2 CELLS POSTPONE LITERAL POSTPONE +LOOP ; IMMEDIATE
: .PAY. ( a -- )   CELL+ ? ;
: .PAY$ ( a -- )   CELL+ @ $@ TYPE  3 SPACES ;
: .PAY-DEA ( a -- )   CELL+ @ %ID. ;
: LABEL-NAME ( n -- a n )   LABELS[] CELL+ @ >BODY >NFA @ $@ ;
: .LABELS ( -- )   DO-LAB  I @ .  I .PAY  CR  LOOP-LAB ;
: LAB-BOUNDS ( -- l u )   1 LAB-UPB ;
: LAB< ( i1 i2 -- f )   LABELS[] @  SWAP LABELS[] @  SWAP < ;
: LAB<-> ( i1 i2 -- )   LABELS[] SWAP LABELS[]  2 CELLS EXCHANGE ;
: SORT-LABELS ( -- )   LAB-BOUNDS  ['] LAB<  ['] LAB<->  QSORT ;
VARIABLE CONT
: L< ( n -- f )   LABELS[] @  CONT @ < ;
: WHERE-LABEL ( a -- )   CONT !  LAB-BOUNDS 1+  ['] L<  BIN-SEARCH ;

VARIABLE LABEL-CACHE    \ Index of next label.

: FIND-LABEL ( n -- )   WHERE-LABEL  DUP LAB-UPB 1+ <> AND  DUP LABEL-CACHE ! ;
: >LABEL ( a -- )   DUP >R  FIND-LABEL DUP IF  LABELS[] DUP @  R@ <> IF
    DROP 0  THEN THEN
    R> DROP ;

VARIABLE MAX-DEV-P   -8 MAX-DEV-P !    \ Max deviation acceptable with previous
VARIABLE MAX-DEV-N    8 MAX-DEV-N !    \ Max deviation acceptable with next

: (~LABEL)   DUP MAX-DEV-P @ + FIND-LABEL
    DUP 0= IF 2DROP 0 ELSE
        OVER MAX-DEV-N @ + OVER LABELS[] @  < IF 2DROP 0 ELSE
    SWAP DROP THEN THEN ;
: IMPROVE-LABEL
    BEGIN  DUP LAB-UPB <> IF  2DUP 1+ LABELS[] @ < 0=  ELSE  0  THEN
    WHILE  1+  REPEAT ;
: ~LABEL   DUP (~LABEL) DUP 0= IF  2DROP 0 0
    ELSE  IMPROVE-LABEL  LABELS[] SWAP OVER @ -  THEN ;
: ROLL-LABEL   DUP   LABELS[]  DUP LABELS BAG-HOLE   LABELS BAG-HOLE
    LAB-BOUNDS SWAP DROP   LAB<->   -2 CELLS LABELS  +! ;
: NEXT-LABEL   LABEL-CACHE @   DUP IF
        1+ DUP LAB-BOUNDS + = IF DROP 0 THEN
    DUP LABEL-CACHE ! THEN ;
: .EQU    LABELS[] DUP @ 8 H.R S"  LABEL " TYPE  CELL+ @ %ID. CR ;
MAX-LABEL ' .PAY-DEA ' .EQU LABELSTRUCT EQU-LABELS  LABELS !BAG
: LABELED ( x a n -- )   S" CREATE " PAD $!  PAD COUNT + OVER
    2SWAP PAD $+!  PAD $@ EVALUATE  HERE >R  'LABELS @ , ( >LFA )
    R@ 'LABELS !  HERE 2 CELLS + , ( >NFA )  ROT DUP , ( >DATA )
    -ROT $, DROP  EQU-LABELS  LAB+!  R> LAB+!
    DOES> ( -- x )   >DATA @ ;
: LABEL   BL WORD COUNT LABELED ;
: EQU LABEL ;
: =EQU-LABEL   HOST>TARGET  EQU-LABELS >LABEL ;
: .EQU-ALL   HOST>TARGET  EQU-LABELS   0 ( no labels printed) SWAP
    LAB-UPB 1+ OVER WHERE-LABEL ?DO
        DUP I LABELS[] @ <> IF LEAVE THEN
        SWAP 1+ SWAP
        [CHAR] : EMIT I LABELS[] .PAY
    LOOP DROP ;
: ADORN-WITH-LABEL   .EQU-ALL    0= IF 12 SPACES THEN ;

HEX FFFF0000 CONSTANT LARGE-NUMBER-MASK

: .0?   DUP 0A0 100 WITHIN SWAP 0A 10 WITHIN OR IF [CHAR] 0 EMIT THEN ;
: SMART.   DUP ABS 100 < IF DUP .0? . ELSE
    LARGE-NUMBER-MASK OVER AND IF 8 ELSE 4 THEN H.R SPACE THEN ;
: .~LABEL   SWAP .PAY   ?DUP IF
    DUP 0< IF NEGATE . S" - " TYPE ELSE . S" + " TYPE THEN THEN ;

DECIMAL
VARIABLE SMALL-LABEL-LIMIT   100 SMALL-LABEL-LIMIT !

: .LABEL/.   EQU-LABELS
    DUP ABS SMALL-LABEL-LIMIT @ < IF SMART.
    ELSE DUP ~LABEL OVER IF .~LABEL DROP
    ELSE 2DROP SMART.
    THEN THEN ;

\D 0 SMALL-LABEL-LIMIT !
\D 12 LABEL AAP
\D 5 LABEL NOOT
\D 2 LABEL MIES
\D 123 LABEL POPI

\D .LABELS CR
\D SORT-LABELS
\D .LABELS CR

\D 200 FIND-LABEL . CR
\D 12 FIND-LABEL  LABELS[] .PAY CR
\D 12 1- FIND-LABEL  LABELS[] .PAY CR
\D 12 >LABEL .PAY CR
\D 12 1- >LABEL H. CR
\D 12 ADORN-WITH-LABEL  4 <?> CR  \ Should give zero, not found!
\D 12 0 HOST>TARGET - ADORN-WITH-LABEL  CR

: @LABEL    LABELS[] CELL+ @ ;

\D 5 DUP INVENT-NAME LABELED
\D SORT-LABELS .LABELS 5 <?> CR
\D .LABELS 6 <?> CR
\D S" EXPECT 5: " TYPE 5 FIND-LABEL @LABEL >DATA @ . 7 <?> CR
: LABEL=   @LABEL >DATA @   SWAP @LABEL >DATA @   = ;

\D S" EXPECT -1:" TYPE 5 FIND-LABEL DUP 1+ LABEL= . CR
: REMOVE-TRIVIAL   DUP @LABEL DUP >DATA @ SWAP >NFA @ $@ INVENTED-NAME? IF
        DUP REMOVE-LABEL ELSE 1+ THEN ;

\D 5 FIND-LABEL  DUP REMOVE-TRIVIAL .  REMOVE-TRIVIAL .
\D .LABELS 8 <?> CR
: CLEAN-LABELS   EQU-LABELS
    2 BEGIN DUP LAB-UPB < WHILE DUP DUP 1- LABEL= >R DUP DUP 1+ LABEL= R> OR IF
            REMOVE-TRIVIAL ELSE 1+ THEN REPEAT DROP ;

\D 5 DUP INVENT-NAME LABELED  SORT-LABELS
\D .LABELS 9 <?> CR
\D CLEAN-LABELS
\D .LABELS 10 <?> CR
: .COMMENT:   LABELS[] DUP @ 8 H.R S"  COMMENT: " TYPE  CELL+ @ $@ TYPE CR ;
MAX-LABEL ' .PAY$ ' .COMMENT: LABELSTRUCT COMMENT:-LABELS  LABELS !BAG
: COMMENT:   COMMENT:-LABELS   LAB+!  [CTRL] J PARSE $, LAB+! ;

\D 12 COMMENT: AAP
\D 115 COMMENT: NOOTJE
\D 2 COMMENT: MIES
\D 123 COMMENT: POPI
VARIABLE COMMENT:-TO-BE
: INIT-COMMENT:   0 COMMENT:-TO-BE ! ;
  INIT-COMMENT:
: PRINT-OLD-COMMENT:   COMMENT:-TO-BE @ DUP IF
    S" \ " TYPE   $@ TYPE _ THEN DROP
    INIT-COMMENT: ;
: REMEMBER-COMMENT:   COMMENT:-LABELS   HOST>TARGET >LABEL
    DUP IF CELL+ @ COMMENT:-TO-BE ! _ THEN DROP  ;

\D 12 REMEMBER-COMMENT: PRINT-OLD-COMMENT: CR  \ Should give nothing, not found!
\D 12 0 HOST>TARGET - REMEMBER-COMMENT: PRINT-OLD-COMMENT: CR

\D .LABELS CR
\D SORT-LABELS
\D .LABELS CR

\D 200 FIND-LABEL . CR
\D 12 FIND-LABEL  LABELS[] .PAY CR
\D 12 1- FIND-LABEL  LABELS[] .PAY CR
\D 12 >LABEL .PAY CR
\D 12 1- >LABEL H. CR
: .MDIRECTIVE   LABELS[] DUP @ DUP >R 8 H.R S"  :COMMENT " TYPE
    CELL+ @ $@  BEGIN  2DUP 10 SCAN ?DUP
    WHILE  1 /STRING 2SWAP 2OVER NIP - 1- 2 /STRING TYPE CR
        R@ 8 H.R S"  :COMMENT " TYPE
    REPEAT  R> 2DROP 2 /STRING TYPE CR ;
MAX-LABEL ' .PAY$ ' .MDIRECTIVE LABELSTRUCT MCOMMENT-LABELS  LABELS !BAG
: NEW-DIRECTIVE ( a n x -- )   LAB+! $, LAB+! ;
: OLD-DIRECTIVE ( a n -- )   >LABEL CELL+ DUP >R
    @ $@ PAD $!  [CTRL] J PAD $C+  PAD $+!  PAD $@ $, R> ! ;
: DIRECTIVE ( a n x -- )   MCOMMENT-LABELS >R
    R@ >LABEL IF   R@ OLD-DIRECTIVE   ELSE
    R@ NEW-DIRECTIVE   R@ WHERE-LABEL ROLL-LABEL THEN R> DROP ;
: COMMENT ( a n x -- )   >R  S" \ " $,  DUP >R  OVER ALLOT  $+!
    R> $@  R> DIRECTIVE ;
: :COMMENT ( x -line- )   [CTRL] J PARSE $, $@ ROT COMMENT ;

\D S" AAP" 12 COMMENT
\D S" NOOT" 5 COMMENT
\D S" MIES" 2 COMMENT
\D S" POPI
\ \D JOPI"
\D 123 COMMENT
: PRINT-DIRECTIVE MCOMMENT-LABELS  HOST>TARGET  >LABEL DUP IF
    CR   .PAY _ THEN DROP ;

\D 12 PRINT-DIRECTIVE CR  \ Should give nothing, not found!
\D 12 0 HOST>TARGET - PRINT-DIRECTIVE CR

\D .LABELS CR
\D SORT-LABELS
\D .LABELS CR

\D 200 FIND-LABEL . CR
\D 12 FIND-LABEL  LABELS[] .PAY CR
\D 12 1- FIND-LABEL  LABELS[] .PAY CR
\D 12 >LABEL .PAY CR
\D 12 1- >LABEL H. CR
[DEFINED] ForCiForth [IF]
    REQUIRE NEW-IF
[THEN]

CREATE TABLE2 256 ALLOT      TABLE2 256 ERASE
: /TABLE2 [CHAR] ~ 1 + BL 1 + DO 3 TABLE2 I + C! LOOP ;
/TABLE2
2 BL TABLE2 + C!
1 CTRL I TABLE2 + C!
1 CTRL J TABLE2 + C!
1 CTRL M TABLE2 + C!
1 CTRL L TABLE2 + C!
: IS-CTRL   TABLE2 + C@ 1 = ;

\D S" EXPECT 0 -1 :" TYPE CHAR A IS-CTRL .   CTRL J IS-CTRL . CR 11 <?>
: IS-PRINT   TABLE2 + C@ 1 > ;

\D S" EXPECT 0 -1 -1 :" TYPE CTRL A IS-PRINT .
\D CHAR A IS-PRINT . BL IS-PRINT . CR 12 <?>
CREATE ACCU 100 ALLOT           ACCU 100 ERASE

\ \D S" Expect " TYPE  """ AA""""AA """ TYPE CHAR : EMIT " AA""AA " S" $" TYPE CR 13 <?>
: .ACCU   ACCU $@
    OVER C@ BL = OVER 1 = AND IF  2DROP  S"  BL" TYPE  ELSE
        DUP 1 > IF  SPACE [CHAR] " EMIT SPACE TYPE [CHAR] " EMIT  ELSE
            IF  SPACE [CHAR] " EMIT SPACE C@ EMIT [CHAR] " EMIT  ELSE  DROP
    THEN THEN THEN  0 0 ACCU $! ;

\D S" EXPECT " TYPE  S" XY :" TYPE  S" XY" ACCU $! .ACCU CR 14 <?>
\D S" EXPECT BL :" TYPE   S"  " ACCU $!   .ACCU CR 15 <?>
\D S" EXPECT CHAR Y :" TYPE   S" Y" ACCU $!   .ACCU CR 16 <?>
: .B-CLEAN   DUP .0? 0 <# BL HOLD #S #> TYPE ;
: .C   .ACCU SPACE DUP IS-CTRL IF  S" CTRL " TYPE  [CHAR] @ + EMIT
    ELSE  .B-CLEAN  THEN  ;

\D S" EXPECT CTRL J :" TYPE  CTRL J .C CR 17 <?>
\D S" EXPECT 0: " TYPE  0 .C CR 18 <?>
\D S" EXPECT 9A: " TYPE  HEX 9A .C CR 19 <?> DECIMAL
\D S" EXPECT 0FA: " TYPE  HEX FA .C CR 20 <?> DECIMAL

\ FIXME: to be renamd in WHERE-FLUSH
VARIABLE NEXT-CUT       \ Host address where to separate db etc. in chunks.
VARIABLE CUT-SIZE    16 CUT-SIZE !   \ Chunks for data-disassembly.

: ACCU-$C+   DUP C@ ACCU $C+   ACCU @ 64 = IF 1+ ELSE 2 + THEN NEXT-CUT ! ;
: .TARGET-ADDRESS S" ( " TYPE DUP HOST>TARGET 8 H.R S"  )   " TYPE ;
: CR-ADORNED
    PRINT-OLD-COMMENT:
    DUP PRINT-DIRECTIVE
    CR 'ADORN-ADDRESS 2@ - IF  .TARGET-ADDRESS  THEN
    ADORN-WITH-LABEL ;
: NEXT-CUT?   NEXT-CUT @ =  DUP IF CUT-SIZE @ NEXT-CUT +! THEN ;
: CR+GENERIC   2>R DUP =EQU-LABEL >R DUP NEXT-CUT?   R> OR IF
    DUP CR-ADORNED  2R@ TYPE THEN REMEMBER-COMMENT: 2R> 2DROP ;

: CR+$         2>R DUP =EQU-LABEL >R DUP NEXT-CUT?   R> OR IF .ACCU
    DUP CR-ADORNED  2R@ TYPE THEN REMEMBER-COMMENT: 2R> 2DROP ;
: CR+dn   S"   " CR+GENERIC ;
: CR+db   S"   db " CR+GENERIC ;
: CR+dw   S"   dw " CR+GENERIC ;
: CR+dl   S"   dl " CR+GENERIC ;
: CR+d$   S"   d$ " CR+$ ;
0 VALUE CURRENT-RANGE \ current range pointer

: RANGE-FIELD ( u size -- u' )   CREATE  OVER , +
    DOES> ( -- a )   @ CURRENT-RANGE + ;

0
    2 CELLS RANGE-FIELD RANGE-RESERVED
    1 CELLS RANGE-FIELD 'RANGE-START      \ Start of range
    1 CELLS RANGE-FIELD 'RANGE-END        \ End of range
    1 CELLS RANGE-FIELD 'RANGE-STRIDE     \ For the moment = 1 FIXME!
    1 CELLS RANGE-FIELD 'RANGE-XT
    1 CELLS RANGE-FIELD 'RANGE-SECTION
CONSTANT |RANGE|

: RANGE-START ( -- a )   'RANGE-START @ ;
: RANGE-END! ( a -- )   'RANGE-END ! ;
: RANGE-END ( -- a )   'RANGE-END @ ;
: RANGE-STRIDE ( -- n )   'RANGE-STRIDE @ ;
: RANGE-XT ( -- xt )   'RANGE-XT @ ;
: RANGE-DECODE ( -- )   'RANGE-XT @ >R RANGE-START RANGE-END R> EXECUTE ;
: RANGE-SECTION ( a -- )   TO CURRENT-RANGE
    'RANGE-SECTION @ TO CURRENT-SECTION ;
: .PAY-RANGE   CELL+ @ DUP RANGE-SECTION
    RANGE-START 8 H.R SPACE  RANGE-END 8 H.R  S"  BY " TYPE
    RANGE-XT >BODY %ID.  %ID. ;

20 BAG RANGE-TYPES  \ Contains dea of dumper, creator, alternating.

: ARE-COUPLED   >BODY SWAP >BODY  RANGE-TYPES BAG+! RANGE-TYPES BAG+! ;
: CREATOR-XT   RANGE-XT >BODY RANGE-TYPES BAG-WHERE CELL+ @ ;
: MAKE-CURRENT ( n -- )   LABELS[] CELL+ @ RANGE-SECTION ;
: DECOMP-RANGE   MAKE-CURRENT RANGE-START 8 H.R SPACE RANGE-END 8 H.R SPACE
    CURRENT-RANGE >NFA @ $@  CREATOR-XT >NFA @ $@
    2OVER S" NONAME" COMPARE 0= IF
        1- TYPE ." - "  2DROP
    ELSE  TYPE SPACE  TYPE
    THEN  CR ;
MAX-LABEL ' .PAY-RANGE ' DECOMP-RANGE LABELSTRUCT RANGE-LABELS  LABELS !BAG
: RANGE ( ad1 ad2 dea1 a n -- )   S" CREATE " PAD $!  PAD COUNT + OVER
    2SWAP PAD $+!  PAD $@ EVALUATE  HERE DUP >R  |RANGE| DUP ALLOT  ERASE
    $, R@ >NFA !  CURRENT-RANGE R@ >LFA !  R> TO CURRENT-RANGE
    'RANGE-XT !  1 'RANGE-STRIDE !  'RANGE-END !  'RANGE-START !
    RANGE-LABELS  RANGE-START LAB+!  CURRENT-RANGE LAB+!
    CURRENT-SECTION 'RANGE-SECTION !
    DOES>  RANGE-SECTION ;

: ANON-RANGE ( ad1 ad2 dea1 -- )   -warning >R  NONAME$ RANGE  R> +warning ;

0 VALUE DISASSEMBLERS
0 VALUE RANGE-RANGES

0
    2 CELLS DEA-FIELD DEA-RESERVED
    1 CELLS DEA-FIELD >DIS:
CONSTANT |DISASSEMBLER|

: DIS: ( xt -- )   SAVE-INPUT  CREATE  HERE DUP >R  |DISASSEMBLER| DUP
    ALLOT  ERASE  RESTORE-INPUT THROW  BL WORD $@ $, R@ >NFA !
    DISASSEMBLERS R@ >LFA !  R@ >DIS: !  R> TO DISASSEMBLERS
    DOES> ( a1 a2 -- )   >R TARGET>HOST SWAP TARGET>HOST
        DUP NEXT-CUT !  R> >DIS: @ EXECUTE ;
: RANGE: ( xt -- )   SAVE-INPUT  CREATE  HERE DUP >R  |DISASSEMBLER| DUP
    ALLOT  ERASE  RESTORE-INPUT THROW  BL WORD $@ $, R@ >NFA !
    RANGE-RANGES R@ >LFA !  R@ >DIS: !  R> TO RANGE-RANGES
    DOES> ( a1 a2 -- )   >R BL WORD COUNT  R> >DIS: @ EXECUTE ;
: (D-R-T) ( a2 a1 -- )   SWAP DISASSEMBLE-RANGE ;
' (D-R-T) DIS: D-R-T ( a1 a2 -- )
: -dc ( a n -- )   2>R ['] D-R-T 2R> RANGE ;
' -dc RANGE: -dc: ( -name- )
: -dc- ( a n -- )   -warning >R  NONAME$ -dc  R> +warning ;
' D-R-T  ' -dc:  ARE-COUPLED
: (DUMP-N) ( a2 a1 -- )   DUP CR+dn - .LABEL/. S"  RESB" TYPE CR   ;
' (DUMP-N) DIS: DUMP-N ( a1 a2 -- )
: -dn ( a n -- )   2>R ['] DUMP-N 2R> RANGE ;
' -dn RANGE: -dn: ( -name- )
: -dn- ( a n -- )   -warning >R  NONAME$ -dn  R> +warning ;
' DUMP-N  ' -dn:  ARE-COUPLED
: (DUMP-B) ( a2 a1 -- )   DO  I DUP CR+db C@ .B-CLEAN  LOOP     PRINT-OLD-COMMENT: CR ;
' (DUMP-B) DIS: DUMP-B ( a1 a2 -- )
: -db ( a n -- )   2>R ['] DUMP-B 2R> RANGE ;
' -db RANGE: -db: ( -name- )
: -db- ( a n -- )   -warning >R  NONAME$ -db  R> +warning ;
' DUMP-B  ' -db:  ARE-COUPLED \ Register the decompiler.
: W. 4 H.R SPACE ;
: (DUMP-W) ( a2 a1 -- )   DO  I DUP CR+dw @ W.  2 +LOOP
    PRINT-OLD-COMMENT: CR ;
' (DUMP-W) DIS: DUMP-W ( a1 a2 -- )
: -dw ( a n -- )   2>R ['] DUMP-W 2R> RANGE ;
' -dw RANGE: -dw: ( -name- )
: -dw- ( a n -- )   -warning >R  NONAME$ -dw  R> +warning ;
' DUMP-W  ' -dw:  ARE-COUPLED
: (DUMP-L) ( a2 a1 -- )   DO  I DUP CR+dl @ .LABEL/.  4 +LOOP
    PRINT-OLD-COMMENT: CR ;
' (DUMP-L) DIS: DUMP-L ( a1 a2 -- )
: -dl ( a n -- )   2>R ['] DUMP-L 2R> RANGE ;
' -dl RANGE: -dl: ( -name- )
: -dl- ( a n -- )   -warning >R  NONAME$ -dl  R> +warning ;
' DUMP-L  ' -dl:  ARE-COUPLED
: (DUMP-$) ( a2 a1 -- )
    DO  I CR+d$
        I C@ IS-PRINT   IF I ACCU-$C+ ELSE I C@ .C THEN
    LOOP  .ACCU   PRINT-OLD-COMMENT: CR ;

\D 'ADORN-ADDRESS @  'ADORN-ADDRESS RESTORED  HERE
\D S" AAP" $, DROP CTRL J C, CTRL M C, CHAR A C, CHAR A C, BL C, CHAR P C,
\D 0 C, 1 C, BL C, 2 C, 3 C,
\D HERE
\D S" EXPECT  ``3 0 0 0 'AAP' XX ^J ^M 'AA P' 0 1 BL 2 3 '':" TYPE
\D SWAP (DUMP-$) CR  'ADORN-ADDRESS !  22 <?>
' (DUMP-$) DIS: DUMP-$ ( a1 a2 -- )
: -d$ ( a n -- )   2>R ['] DUMP-$ 2R> RANGE ;
' -d$ RANGE: -d$: ( -name- )
: -d$- ( a n -- )   -warning >R  NONAME$ -d$  R> +warning ;
' DUMP-$  ' -d$:  ARE-COUPLED
' 2DROP DIS: IGNORE ( a1 a2 -- )
: -ignore ( a n -- )   2>R ['] IGNORE 2R> RANGE ;
' -ignore RANGE: -ignore: ( -name- )
: -ignore- ( a n -- )   -warning >R  NONAME$ -ignore  R> +warning ;
: .HOW-FIT ( a1 a2 -- )   2DUP = IF  2DROP  ELSE  > IF
            S" \ WARNING: This range overlaps with the previous one."  ELSE
            S" \ WARNING: There is hole between this range and the previous one"
    THEN  CR TYPE CR  THEN ;
: HOW-FIT ( a -- a' )   RANGE-START .HOW-FIT  RANGE-END ;
: HOW-FIT-END ( a -- )   TARGET-END .HOW-FIT ;
: DISASSEMBLE-ALL ( -- )   TARGET-START  RANGE-LABELS DO-LAB
        I CELL+ @ RANGE-SECTION  HOW-FIT RANGE-DECODE
    LOOP-LAB  HOW-FIT-END   HOST-END CR-ADORNED ;

\ ------------------- Generic again -------------------

: (ADORN-ADDRESS)   DUP CR-ADORNED   REMEMBER-COMMENT: ;
' (ADORN-ADDRESS) 'ADORN-ADDRESS !
: INIT-ALL   THE-REGISTER DO-BAG  I @ TO CURRENT-LABELSTRUCT  LABELS !BAG
    LOOP-BAG  INIT-COMMENT: ;
: SORT-ALL   THE-REGISTER DO-BAG  I @ TO CURRENT-LABELSTRUCT  SORT-LABELS
    LOOP-BAG ;
: DECOMP-ONE  LAB-UPB 1+ 1 ?DO  I DECOMP  LOOP ;
: DECOMP-ALL   THE-REGISTER DO-BAG  I @ TO CURRENT-LABELSTRUCT  DECOMP-ONE
    LOOP-BAG ;
: MAKE-CUL  S" HEX" TYPE CR  TARGET-START 8 H.R
    S"  -ORG-" TYPE CR DECOMP-ALL ;
: SHOW-REGISTER   THE-REGISTER DO-BAG  I @ %ID.  LOOP-BAG ;
: DISASSEMBLE-TARGET ( -- )   TARGET-START . S"  ORG" TYPE CR
    DISASSEMBLE-ALL ;
-warning
: DISASSEMBLE-TARGET ( -- )   S" [ASM HEX BITS-32" TYPE CR  DISASSEMBLE-TARGET CR ;
+warning
VARIABLE DEFAULT-DISASSEMBLY
' -d$- DEFAULT-DISASSEMBLY !
: -ddef- DEFAULT-DISASSEMBLY @ EXECUTE ;

ALSO ASSEMBLER

: (D-R-T-16) ( a2 a1 -- )   BITS-16 CR S" BITS-16" TYPE  SWAP DISASSEMBLE-RANGE
    BITS-32 CR S" BITS-32" TYPE ;
' (D-R-T-16) DIS: D-R-T-16 ( a1 a2 -- )
: -dc16 ( a n -- )   2>R ['] D-R-T-16   2R> RANGE ;
' -dc16 RANGE: -dc16: ( -name- )
: -dc16- ( a n -- )   -warning >R  NONAME$ -dc16  R> +warning ;

' D-R-T-16  ' -dc16:  ARE-COUPLED

DEFINITIONS

: AS-@+ ( xt -- )   >CNT @ >R  AS-POINTER @ R@ MC@  R> AS-POINTER +! ;

\D HEX
\D S" EXPECT 34 12 " TYPE 1234 PAD ! PAD AS-POINTER !
\D ' IB, >BODY DUP AS-@+ . AS-@+ . CR 23 <?>
\D DECIMAL
: AS-S-@+ ( xt -- )   >CNT @ >R  AS-POINTER @ R@ MC@-S  R> AS-POINTER +! ;

\D HEX
\D S" EXPECT -1 12 " TYPE 12FF PAD ! PAD AS-POINTER !
\D ' IB, >BODY DUP AS-S-@+ . AS-S-@+ . CR
\D DECIMAL
VARIABLE LATEST-OFFSET
: .COMMA-LABEL   DUP AS-@+ .LABEL/. %ID. ;
: ID.-NO() ( xt -- )   >NFA @ $@  2 - SWAP 1 + SWAP TYPE SPACE ;
: NEXT-INSTRUCTION  >CNT @ AS-POINTER @ + ;
: GET-OFFSET   AS-POINTER @ SWAP >CNT @ MC@-S DUP LATEST-OFFSET ! ;
: GOAL-RB   DUP GET-OFFSET SWAP NEXT-INSTRUCTION + ;
: .BRANCH/.  EQU-LABELS   ~LABEL OVER IF .~LABEL ID.-NO() ELSE
    2DROP DUP GET-OFFSET . %ID. THEN  ;
: .COMMA-REL
    DUP  DUP GOAL-RB HOST>TARGET  .BRANCH/.
    >CNT @ AS-POINTER +! ;

\D 5 .LABEL/. CR
\D 5 .LABEL/. CR
\D ' (RB,) >BODY ID.-NO() CR

' .COMMA-LABEL  ' OW,   >BODY >DIS !  ( obligatory word     )
' .COMMA-REL    ' (RL,) >BODY >DIS !  ( cell relative to IP )
' .COMMA-REL    ' (RW,) >BODY >DIS !  ( cell relative to IP )
' .COMMA-REL    ' (RB,) >BODY >DIS !  ( byte relative to IP )
' .COMMA-LABEL  ' SG,   >BODY >DIS !  (  Segment: WORD      )
' .COMMA-LABEL  ' P,    >BODY >DIS !  ( port number ; byte     )
' .COMMA-LABEL  ' IS,   >BODY >DIS !  ( Single -obl-  byte )
' .COMMA-LABEL  ' IL,   >BODY >DIS !  ( immediate data : cell)
' .COMMA-LABEL  ' IW,   >BODY >DIS !  ( immediate data : cell)
' .COMMA-LABEL  ' IB,   >BODY >DIS !  ( immediate byte data)
' .COMMA-LABEL  ' L,    >BODY >DIS !  ( immediate data : address/offset )
' .COMMA-LABEL  ' W,    >BODY >DIS !  ( immediate data : address/offset )
' .COMMA-LABEL  ' B,    >BODY >DIS !  ( immediate byte : address/offset )
0 BAG UNCONDITIONAL-TRANSFERS
    ' CALL, , ' CALLFAR, , ' CALLFARO, , ' CALLO, , ' INT, , ' INT3, , ' INTO, ,
    ' IRET, , ' JMP, , ' JMPFAR, , ' JMPFARO, , ' JMPO, , ' JMPS, , ' RET+, ,
    ' RET, , ' RETFAR+, , ' RETFAR, ,
HERE UNCONDITIONAL-TRANSFERS ! 100 CELLS ALLOT         \ Allow to put more here
0 BAG JUMPS
    ' CALL, , ' J, , ' JCXZ, , ' JMP, , ' JMPS, , ' J|X, ,
    ' LOOP, , ' LOOPNZ, , ' LOOPZ, ,
HERE JUMPS ! 100 CELLS ALLOT         \ Allow to put more here

PREVIOUS DEFINITIONS
( $Id: crawl.frt,v 1.36 2009/03/26 09:07:17 albert Exp $ )
( Copyright{2000}: Albert van der Horst, HCC FIG Holland by GNU Public License)
( Uses Richard Stallmans convention. Uppercased word are parameters.    )

[DEFINED] ForCiForth [IF]
    REQUIRE H.
    REQUIRE BAG
[THEN]

: INSERT-EQU 2>R DUP EQU-LABELS WHERE-LABEL SWAP 2R> LABELED
    ROLL-LABEL ;
: INSERT-EQU-INVENT DUP INVENT-NAME INSERT-EQU ;
: ?INSERT-EQU?    EQU-LABELS DUP >LABEL IF DROP ELSE INSERT-EQU-INVENT THEN ;

EQU-LABELS LABELS !BAG

HEX

: TEST-?INSERT-EQU?
    assert( EQU-LABELS LABELS |BAG| 0= )
    42 ?INSERT-EQU?
    assert( EQU-LABELS LABELS |BAG| 2 = )
    42 ?INSERT-EQU?
    assert( EQU-LABELS LABELS |BAG| 2 = )
; TEST-?INSERT-EQU?

DECIMAL

HEX

RANGE-LABELS LABELS !BAG
4FE 510 -dc-
520 530 -dc: oops
530 570 -dc-
560 590 -db: bytes

: TEST-RANGES
    assert( LABELS |BAG| 8 = )
; TEST-RANGES

DECIMAL

: COMPATIBLE?   DUP MAKE-CURRENT RANGE-XT   SWAP 1- MAKE-CURRENT RANGE-XT  = ;

: TEST-COMPATIBLE?
    assert( 2 COMPATIBLE? -1 = )
    assert( 3 COMPATIBLE? -1 = )
    assert( 4 COMPATIBLE? 0= )
; TEST-COMPATIBLE?
: RANGE-NAME LABELS[] CELL+ @ >NFA @ $@ ;

: TEST-RANGE-NAME
    assert( 1 RANGE-NAME S" NONAME" COMPARE 0= )
    assert( 2 RANGE-NAME S" oops" COMPARE 0= )
; TEST-RANGE-NAME
: NEW-RANGE-START OVER MAKE-CURRENT RANGE-START  OVER MAKE-CURRENT RANGE-START
    MIN ;

HEX

: TEST-NEW-RANGE-START
    assert( 2 3 NEW-RANGE-START NIP NIP 520 = )
; TEST-NEW-RANGE-START

DECIMAL
: NEW-RANGE-END OVER MAKE-CURRENT RANGE-END  OVER MAKE-CURRENT RANGE-END
    MAX ;

HEX

: TEST-NEW-RANGE-END
    assert( 3 4 NEW-RANGE-END NIP NIP 590 = )
; TEST-NEW-RANGE-END

DECIMAL
: REPLACE  OVER >R REMOVE-LABEL REMOVE-LABEL R> ROLL-LABEL ;

: TEST-REPLACE
   assert( LABELS |BAG| 2 3 REPLACE LABELS |BAG| - 4 = )
; TEST-REPLACE
: SAME-ALIGN    DUP MAKE-CURRENT  RANGE-START SWAP
    1- MAKE-CURRENT   RANGE-START - RANGE-STRIDE MOD 0= ;

HEX

INIT-ALL RANGE-LABELS
12 34 -dc-
34 65 -db: AAP
38 80 -dl-
82 90 -dl-
88 94 -dl-

DECIMAL

: TEST-SAME-ALIGN
    assert( 2 SAME-ALIGN -1 = )
    assert( 3 SAME-ALIGN -1 = ) \ Must become 0
    assert( 4 SAME-ALIGN -1 = )
; TEST-SAME-ALIGN
: END+START DUP MAKE-CURRENT RANGE-START SWAP 1- MAKE-CURRENT RANGE-END SWAP ;

HEX

: TEST-END+START
    assert( 2 END+START 34 = SWAP 34 = AND )
    assert( 3 END+START 38 = SWAP 65 = AND )
; TEST-END+START

DECIMAL
: OVERLAP? END+START > ;

: TEST-OVERLAP?
    assert( 2 OVERLAP? 0= )
    assert( 3 OVERLAP? -1 = )
    assert( 4 OVERLAP? 0= )
; TEST-OVERLAP?
: OVERLAP-OR-BORDER? END+START >= ;

: TEST-OVERLAP-OR-BORDER?
    assert( 2 OVERLAP-OR-BORDER? -1 = )
    assert( 3 OVERLAP-OR-BORDER? -1 = )
    assert( 4 OVERLAP-OR-BORDER? 0= )
; TEST-OVERLAP-OR-BORDER?
: GAP? END+START < ;

: TEST-GAP?
    assert( 2 GAP? 0 = )
    assert( 3 GAP? 0= )
    assert( 4 GAP? -1 = )
; TEST-GAP?
: IS-NAMED ( n -- )   RANGE-NAME NONAME$ $= 0= ;

: TEST-IS-NAMED
    assert( 2 IS-NAMED -1 = )
    assert( 3 IS-NAMED 0= )
; TEST-IS-NAMED
: COLLAPSE ( i -- )   DUP MAKE-CURRENT  RANGE-END OVER 1- MAKE-CURRENT
    RANGE-END MAX RANGE-END!  REMOVE-LABEL ;

HEX

: TEST-COLLAPSE
    assert( LAB-UPB 5 = ) 5 COLLAPSE 4 MAKE-CURRENT
    assert( RANGE-START 82 = )
    assert( RANGE-END 94 = )
    assert( LAB-UPB 4 = )
; TEST-COLLAPSE

DECIMAL
: TRIM-RANGE ( i -- )   DUP MAKE-CURRENT  RANGE-START SWAP 1- MAKE-CURRENT
    RANGE-END! ;

HEX

90 1000 -dl-

: TEST-TRIM-RANGE
    5 TRIM-RANGE 4 MAKE-CURRENT
    assert( RANGE-START 82 = ) assert( RANGE-END 90 = )
; TEST-TRIM-RANGE

DECIMAL
: COMBINE ( n -- )
    DUP OVERLAP-OR-BORDER? OVER IS-NAMED 0= AND IF DUP COLLAPSE THEN
    DUP OVERLAP? OVER IS-NAMED AND IF DUP TRIM-RANGE THEN  DROP ;

HEX

INIT-ALL
10  30 -dl-
20  40 -dl-
30  50 -dl: aap0
60  80 -dl-
90 100 -dl: noot

: TEST-COMBINE
    assert( LAB-UPB 5 = ) 5 COMBINE assert( LAB-UPB 5 = )
    assert( LAB-UPB 5 = ) 4 COMBINE assert( LAB-UPB 5 = )
    assert( LAB-UPB 5 = ) 3 COMBINE assert( LAB-UPB 5 = )
    2 MAKE-CURRENT assert( RANGE-START 20 = ) assert( RANGE-END 30 = )
    assert( LAB-UPB 5 = ) 2 COMBINE assert( LAB-UPB 4 = )
    1 MAKE-CURRENT assert( RANGE-START 10 = ) assert( RANGE-END 30 = )
; TEST-COMBINE

DECIMAL
: KILL-OVERLAP ( i -- )   DUP SAME-ALIGN  OVER COMPATIBLE? AND IF
        DUP COMBINE  THEN  DROP ;

HEX

INIT-ALL
10  30 -dl-
20  40 -dl-
30  50 -dl: aap1
60  80 -dl-
90 100 -dl: noot1

: TEST-KILL-OVERLAP-1
    assert( LAB-UPB 5 = ) 5 KILL-OVERLAP assert( LAB-UPB 5 = )
    assert( LAB-UPB 5 = ) 4 KILL-OVERLAP assert( LAB-UPB 5 = )
    assert( LAB-UPB 5 = ) 3 KILL-OVERLAP assert( LAB-UPB 5 = )
    2 MAKE-CURRENT assert( RANGE-START 20 = ) assert( RANGE-END 30 = )
    assert( LAB-UPB 5 = ) 2 KILL-OVERLAP assert( LAB-UPB 4 = )
    1 MAKE-CURRENT assert( RANGE-START 10 = ) assert( RANGE-END 30 = )
; TEST-KILL-OVERLAP-1

INIT-ALL
10  30 -dl-
20  28 -db-
30  70 -dl: aap2
60  80 -dl-
7F 10F -dl-

\ The following is actually wrong because the aligning is not tested yet.
: TEST-KILL-OVERLAP-2
    assert( LAB-UPB 5 = ) 5 KILL-OVERLAP assert( LAB-UPB 4 = )
    4 MAKE-CURRENT assert( RANGE-START 60 = ) assert( RANGE-END 10F = )
    assert( LAB-UPB 4 = ) 4 KILL-OVERLAP assert( LAB-UPB 3 = )
    3 MAKE-CURRENT assert( RANGE-START 30 = ) assert( RANGE-END 10F = )
    assert( LAB-UPB 3 = ) 3 KILL-OVERLAP assert( LAB-UPB 3 = )
    2 MAKE-CURRENT assert( RANGE-START 20 = ) assert( RANGE-END 28 = )
    assert( LAB-UPB 3 = ) 2 KILL-OVERLAP assert( LAB-UPB 3 = )
    1 MAKE-CURRENT assert( RANGE-START 10 = ) assert( RANGE-END 30 = )
; TEST-KILL-OVERLAP-2

DECIMAL
: FILL-GAP ( i -- )   DUP GAP? IF   DUP END+START -ddef-
        DUP 1+ LAB-UPB MAX KILL-OVERLAP
        DUP KILL-OVERLAP  THEN  DROP ;

HEX

: TEST-FILL-GAP
    assert( LAB-UPB 3 = ) 3 FILL-GAP assert( LAB-UPB 4 = )
    4 MAKE-CURRENT assert( RANGE-START 28 = ) assert( RANGE-END 30 = )
    assert( LAB-UPB 4 = ) 2 FILL-GAP assert( LAB-UPB 4 = )
    2 MAKE-CURRENT assert( RANGE-START 20 = ) assert( RANGE-END 28 = )
; TEST-FILL-GAP

DECIMAL
: CLEANUP-RANGES ( -- )   RANGE-LABELS
    2 LAB-UPB 2DUP <= IF  DO  I KILL-OVERLAP  -1 +LOOP  THEN ;
: PLUG-FIRST ( -- )   1 MAKE-CURRENT
    TARGET-START RANGE-START 2DUP <> IF
        -ddef- _ _  THEN  2DROP ;
: PLUG-LAST ( -- )   LAB-UPB MAKE-CURRENT
    RANGE-END TARGET-END 2DUP <> IF
        -ddef- _ _  THEN  2DROP ;
: PLUG-SPECIAL ( -- )   LAB-UPB IF  PLUG-LAST PLUG-FIRST  ELSE
    TARGET-START TARGET-END -ddef-  THEN ;
: PLUG-HOLES ( -- )   CURRENT-SECTION  RANGE-LABELS LAB-UPB 1+ 2
    2DUP > IF  DO  I FILL-GAP  LOOP  ELSE  2DROP  THEN
    SORT-LABELS  PLUG-SPECIAL  SORT-LABELS
    TO CURRENT-SECTION ;

ALSO ASSEMBLER

1000 BAG STARTERS
VARIABLE (R-XT)
: REQUIRED-XT (R-XT) @ ;
: NORMAL-DISASSEMBLY ['] D-R-T (R-XT) ! BITS-32 ;
  NORMAL-DISASSEMBLY
: IN-CURRENT-CODE? ( -- f )   RANGE-START RANGE-END WITHIN
    RANGE-XT REQUIRED-XT =  AND ;
: IN-CODE-N? ( i -- f ) MAKE-CURRENT IN-CURRENT-CODE? ;
: IN-CODE?  DUP 0 = IF 2DROP 0 ELSE   \ Not present.
        2DUP IN-CODE-N? IF 2DROP -1 ELSE
            DUP 1 = IF 2DROP 0 ELSE   \ Previous not present.
                1- IN-CODE-N? THEN THEN THEN ;
: KNOWN-CODE?   RANGE-LABELS DUP WHERE-LABEL LAB-UPB MIN IN-CODE? ;
: IN-CODE-SPACE?   TARGET-START TARGET-END WITHIN ;
: STARTER?   DUP KNOWN-CODE? 0=  SWAP IN-CODE-SPACE? AND ;
: JUMP-TARGET   AS-POINTER @   LATEST-OFFSET @  + HOST>TARGET ;
: ANALYSE-INSTRUCTION   LATEST-INSTRUCTION @ JUMPS IN-BAG? IF
    JUMP-TARGET DUP ?INSERT-EQU?
    STARTER? IF JUMP-TARGET STARTERS SET+ THEN THEN ;
: COLLAPSE[I1] RANGE-LABELS
    DUP LAB-UPB < IF DUP 1+ KILL-OVERLAP THEN
    DUP 1 > IF DUP KILL-OVERLAP THEN
    DROP ;

HEX
LABELS !BAG
4FE 520 -dc-
520 530 -dc: oops1
52A 570 -dc-
560 590 -db: bytes1
DECIMAL

: TEST-COLLAPSE[I1]
    assert( LABELS |BAG| 2 COLLAPSE[I1] LABELS |BAG| - 2 = )
; TEST-COLLAPSE[I1]
: INSERT-RANGE   OVER RANGE-LABELS WHERE-LABEL >R
    REQUIRED-XT ANON-RANGE   R@ ROLL-LABEL   R> COLLAPSE[I1] ;
: CRAWL-ONE  DUP >R TARGET>HOST BEGIN
        (DISASSEMBLE) ANALYSE-INSTRUCTION DUP HOST-END >=
        LATEST-INSTRUCTION @ UNCONDITIONAL-TRANSFERS IN-BAG? OR
    UNTIL R> SWAP HOST>TARGET INSERT-RANGE ;
: ?CRAWL-ONE? DUP STARTER? IF CRAWL-ONE _ THEN DROP ;
: (CRAWL)   BEGIN STARTERS BAG? WHILE STARTERS BAG@- ?CRAWL-ONE? REPEAT ;
: CRAWL   DUP ?INSERT-EQU?   RANGE-LABELS SORT-LABELS
    STARTERS DUP !BAG BAG+!   SHUTUP (CRAWL) ;
: NEW-LABEL? ( a -- )   DUP PLAUSIBLE-LABEL? IF  ?INSERT-EQU? _  THEN  DROP ;
: ADD-L-LABELS ( l h -- )   SWAP DO  I L@ NEW-LABEL?  0 CELL+ +LOOP ;
: ALL-L-LABELS ( -- )   CURRENT-SECTION  RANGE-LABELS DO-LAB
        I CELL+ @ RANGE-SECTION  RANGE-XT ['] DUMP-L = IF
            RANGE-START RANGE-END ADD-L-LABELS  THEN
    LOOP-LAB  TO CURRENT-SECTION ;
: CRAWL16  ['] D-R-T-16 (R-XT) ! BITS-16 CRAWL NORMAL-DISASSEMBLY ;

PREVIOUS
\ Copyright (c) 2012 Dennis Ruffer

\ Copyright (c) 2010 Dennis Ruffer

CREATE cfca 0 , \ address of compressed allocation
CREATE ebx 0 ,
CREATE ecx 0 ,

: 2*d ( n -- n )   DUP 32 ecx @ - RSHIFT  ebx @ ecx @ LSHIFT  + ebx ! ;
: 2*c ( n -- n' )   ecx @ LSHIFT ;

CREATE [na] 26 , \ bits remaining in source word
CREATE [nb] -6 , \ bits remaining in ebx
CREATE [h] 67510272 , \ destination address
CREATE [an] 0 ,
CREATE [aa] 67977026 ,
CREATE [nz] 4 ,

: NEW ( 32-bits in current word )   [aa] @ @ [an] !
    1 CELLS [aa] +!  32 [na] ! ;
: ?NEW ( fetch new word if necessary )   [na] @ 0= IF  NEW  THEN ;
: SHIFT ( n -- n ) ( into ebx, decrement nb )
    DUP NEGATE DUP [nb] +!  [na] +!  ecx !
    [an] @ 2*d 2*c [an] ! ;
: BITS ( n -- ) ( shift bits into ebx. overflow into next word )
    ?NEW DUP NEGATE [na] @ +  DUP 0< IF
        DUP >R + SHIFT NEW R> NEGATE SHIFT
    ELSE  DROP SHIFT  THEN ;

: h, ( n -- ) ( store at destination )   [h] @ !  1 CELLS [h] +! ;
: TBITS ( n n -- ) ( fill ebx with tag )   [nb] @ 8 + ecx !  2*c OR h, ;

: TZ ( n n -- n ? )   OVER [nz] !  DUP NEGATE >R + ebx @
    R> 0 DO  DUP 1 AND IF
            2DROP  UNLOOP  [nz] @ 0 EXIT
        THEN  2/
    LOOP  ebx ! DUP [nz] @ INVERT + INVERT [nb] +!  1 ;

: ?FULL ( n -- n ) ( is there room in ebx? )
    [nb] @ DUP AND DUP 0< IF
        TZ IF  EXIT  THEN
        DUP >R  4 - [nb] +!  TBITS
        0 DUP R> DUP INVERT 29 + [nb] !
    ELSE  DROP  THEN ;

: CHR ( -- n 1 | 0 ) \ examine high bits; shift 4, 5 or 7 bits
    0 ebx ! ( ?NEW )  4 BITS ebx @ 8 AND IF
        ebx @ 4 AND IF
            3 BITS 7 1 EXIT
        THEN  1 BITS 5 1 EXIT
    THEN  4 ebx @ 15 AND IF  1 EXIT
    THEN  DROP 0 ;
: CHRS ( n -- n ) \ shift characters until 0
    CHR IF  ?FULL ecx !  2*c ebx @ OR RECURSE  THEN ;
: WRD ( n -- ) \ shift characters, then tag
    28 [nb] !  DUP CHRS TBITS ;

: t, ( -- )   -4 [nb] !  ebx @ TBITS ;
: SHORT ( n -- ) ( 28-bit value+tag )   28 BITS t, ;
: 32BITS ( -- ) ( for values )   16 BITS  16 BITS  ebx @ h, ;
: LITRAL ( n -- ) \  1-bit base base, tag. value in next word
    0 ebx !  1 BITS t,  32BITS ;
: VAR ( n -- ) ( word, value )   WRD 32BITS ;

: TAG ( -- n 1 | 0 ) \ vector
    ebx @ 15 AND DUP
    DUP  0 = IF  2DROP          0 EXIT  THEN
    DUP  1 = IF   DROP  WRD     1 EXIT  THEN
    DUP  2 = IF   DROP  LITRAL  1 EXIT  THEN
    DUP  3 = IF   DROP  WRD     1 EXIT  THEN
    DUP  4 = IF   DROP  WRD     1 EXIT  THEN
    DUP  5 = IF   DROP  LITRAL  1 EXIT  THEN
    DUP  6 = IF   DROP  SHORT   1 EXIT  THEN
    DUP  7 = IF   DROP  WRD     1 EXIT  THEN
    DUP  8 = IF   DROP  SHORT   1 EXIT  THEN
    DUP  9 = IF   DROP  WRD     1 EXIT  THEN
    DUP 10 = IF   DROP  WRD     1 EXIT  THEN
    DUP 11 = IF   DROP  WRD     1 EXIT  THEN
    DUP 12 = IF   DROP  VAR     1 EXIT  THEN
    DUP 13 = IF   DROP  SHORT   1 EXIT  THEN
    DUP 14 = IF   DROP  WRD     1 EXIT  THEN
    DUP 15 = IF   DROP  SHORT   1 EXIT  THEN ;

: WRDS ( ?new -- ) \ examine tags
    4 BITS TAG IF  RECURSE  THEN ;

: BLOCKS ( blks -- bytes)   1024 * ;
: CFBLOCK ( blk -- addr)   BLOCKS CODE-SPACE + ;
: ERASEBLKS ( b n -- )   >R CFBLOCK R> BLOCKS ERASE ;

: BLOCK-RANGE ( a n n -- ) \ process each block
    OVER CFBLOCK [h] !  DUP >R ERASEBLKS  [aa] !  0 [na] !
    R> 0 DO  WRDS
        [h] @ CODE-SPACE - 1024 + -1024 AND
        CODE-SPACE + [h] !
    LOOP ;

: ns ( -- n )   18 CFBLOCK 1 CELLS + ;    \ compressed if negative
: cfc ( -- n )   CP @ CODE-SPACE - ;      \ size of compressed file
: nblk ( -- n )   18 CFBLOCK 3 CELLS + ;  \ size of uncompressed file

: RESTORE ( -- ) \ restore compressed blocks
    ns @ 0< IF  nblk @ BLOCKS CODE-LENGTH @ > ABORT" Too big!"
        36 CFBLOCK  HERE DUP cfca !  cfc 36 BLOCKS - DUP ALLOT
        MOVE  cfca @ 36 nblk @ OVER - BLOCK-RANGE
        nblk @ BLOCKS CODE-SPACE + CP !
    THEN ;

: SIGN-EXTEND ( x n -- x' )  32 SWAP - DUP >R
    LSHIFT  R> 0 DO  2/  LOOP ;

: @-LE ( a -- x )   4 1C@-LE ;
: !-LE ( x a -- )   4 1C!-LE ;

: W@-BE ( a -- x )   2 1C@-BE ;
: W!-BE ( x a -- )   2 1C!-BE ;

: ?? ( "name" -- flag )  BL WORD FIND SWAP DROP 0= 0= ;
: uses ( flag -- )  0= IF POSTPONE \ THEN ;

?? CTRL 0= uses : CTRL CHAR 31 AND ;
?? [CTRL] 0= uses : [CTRL] CTRL POSTPONE LITERAL ; IMMEDIATE

?? ForGForth uses ALSO
ASSEMBLER

: .ICON-ROW ( x -- )   BASE @ >R  2 BASE !  S>D
    <#  16 0 DO  #  LOOP  #>  TYPE  R> BASE ! ;

16 CONSTANT ICON-COLUMNS
24 CONSTANT ICON-ROWS

: |ICON| ( -- n )   ICON-COLUMNS 8 / ICON-ROWS * ;

: IH. ( n -- a n )   0 1 (DH.) TYPE SPACE ;

: DUMP-ICONS ( a2 a1 -- )   DUP S" icons{ " CR+GENERIC  2DUP - |ICON| /
    0 DO  I  ICON-ROWS 0 DO  CR  4 0 DO  DUP I +
                J 0= IF  16 / IH.  ELSE
                    J 1 = IF  16 MOD IH.  ELSE
                        DROP 2 SPACES  THEN  THEN
                OVER |ICON| I * + J 2* + W@-BE .ICON-ROW  SPACE
        LOOP  LOOP  CR  DROP  4 |ICON| * +  4 +LOOP
    2DUP - IF  (DUMP-B)  ELSE  2DROP
    THEN  S" }icons" TYPE CR ;

: DIS-ICONS ( a1 a2 -- )   TARGET>HOST SWAP TARGET>HOST
    DUP NEXT-CUT ! DUMP-ICONS ;
: -icons ( a n -- )   2>R ['] DIS-ICONS 2R> RANGE ;
' -icons RANGE: -icons: ( -name- )
: -icons- ( a n -- )   -warning >R  NONAME$ -icons  R> +warning ;
' DIS-ICONS  ' -icons:  ARE-COUPLED
: UNPACK ( n -- n' chr )   DUP  DUP 0<
    IF       1 LSHIFT DUP 0<
        IF   6 LSHIFT SWAP 25 RSHIFT 63 AND  16 -    \ 11xxxxx.. 16-47
        ELSE 4 LSHIFT SWAP 27 RSHIFT  7 AND  8 XOR   \ 10xxx..    8-15
        THEN
    ELSE     4 LSHIFT SWAP 28 RSHIFT  7 AND          \ 0xxx..     0-7
    THEN ;

: PRESHIFT ( n -- n' )   32 0 DO  [ HEX ]
        DUP F0000000 AND IF
            UNLOOP EXIT
        THEN  2*
    LOOP ;

: s, ( a n -- )   DUP C,  0 ?DO  COUNT C,  LOOP  DROP ;

S"  rtoeanismcylgfwdvpbhxuq0123456789j-k.z/;'!+@*,?"
CREATE cf-ii ( -- adr)   s,  0 cf-ii 1+ C!

: CH ( n -- n' chr )   0FFFFFFF0 AND UNPACK DUP cf-ii COUNT
    ROT < ABORT" invalid character" + C@ ;
DECIMAL

VARIABLE PHERE
: PAD| ( -- )   0 PAD C! ;
: PAD+ ( a n -- )   PAD APPEND ;
: PAD+BL ( -- )   S"  " PAD+ ;
: PADTYPE ( -- )   PAD COUNT TYPE  PAD| ;
: PAD, ( chr -- )   PHERE @ C!  1 PHERE +! ;
: PADDECODE ( n -- )   BEGIN  CH DUP WHILE PAD,  REPEAT  2DROP ;
: PADCOUNT ( n -- adr len )   PAD COUNT + DUP >R PHERE !
    PADDECODE  R> PHERE @ OVER -  DUP PAD C@ + PAD C! ;

: @NAME ( a -- a' n )   @-LE PRESHIFT PAD| PADCOUNT
    ?DUP 0= IF  DROP S" _"  THEN  PAD| ;
Files  FILE Words.dbf  FILE= Words.dbf

( Bytes  records   origin             name )
      8      256        0 BLOCK-DATA (WORD-NAMES)
     18      256  +ORIGIN BLOCK-DATA  WORD-NAMES
     10      256  +ORIGIN BLOCK-DATA  ADJUSTMENTS

: /Words ( -- )   Words.dbf  FILE-HANDLE @ 0= IF
        S" Words.dbf" >FILE  THEN
    (WORD-NAMES) INITIALIZE  WORD-NAMES INITIALIZE ;
: /Adjust ( -- )   ADJUSTMENTS INITIALIZE ;

4 ( LINK)
    LONG ADDR     ( Address, used as the key)
 NUMERIC ADJUST   ( Difference from old)
 8 BYTES NAME     ( Name, word name)
CONSTANT |Words|

: .ADDR ( -- )   BASE @ HEX  ADDR L@ 8 H.R SPACE  BASE ! ;

: .Word ( n -- )   WORD-NAMES READ  .ADDR  ADJUST N?
    NAME OVER >R B@  FILE-PAD R> xTYPE  SPACE ;
: .Words ( -- )   CR  (WORD-NAMES) RECORDS ?DO
        I (WORD-NAMES) READ  LINK L@ .Word
        I 4 MOD 0= IF  CR  THEN
    LOOP  CR ;
: .Adjusts ( -- )   ADJUSTMENTS RECORDS ?DO
        CR  I READ  .ADDR  ADJUST N?
    LOOP ;

: FIND-ADDRESS ( a1 -- flag )   FILE-PAD #TB BLANK
    ADDR 4 nC!  4 ADDR -BINARY ;
: add-word ( a1 a2 n -- )   ROT (WORD-NAMES) FIND-ADDRESS IF
        SAVE  WORD-NAMES SLOT DUP READ  ADDR 4 nC@ ADDR L!
        RESTORE  DUP LINK 4 nC!  +ORDERED  WORD-NAMES READ
        FILE-PAD DUP #TB BLANK  SWAP MOVE  NAME B!  0 ADJUST N!
    ELSE  ORDERED RELEASE  2DROP \ TYPE 1 ABORT" at duplicate address "
    THEN ;
: add-adjust ( a n -- )   SWAP ADJUSTMENTS FIND-ADDRESS IF
        ADJUST 2 nC!  +ORDERED  ELSE  ORDERED RELEASE
        1 ABORT" Duplicate adjustment "
    THEN ;

: adjust-address ( a -- n )   ADJUSTMENTS FIND-ADDRESS IF
    -1 R# +!  THEN  ORDERED RELEASE  ADJUST N@ ;
Assem

: MAKE-NAME-LABELS ( code names -- )
    2DUP - 0 DO  OVER TARGET>HOST I + @ ?DUP IF
            OVER TARGET>HOST I + @NAME
            Files add-word Assem
    THEN  0 CELL+ +LOOP  2DROP ;

: ADJUST-DELTA ( delta anew aold -- new-delta )
    ROT >R 2DUP - DUP R@ - ?DUP IF
        \ CR  PAD COUNT TYPE  ."  moved " DUP . ." bytes "
        R> 2DROP ROT DROP SWAP OVER
        Files add-adjust Assem
    ELSE  DROP 2DROP R>  THEN ;
: FIND-ADJUSTMENTS ( -- n )   TARGET-START  Files
    /Adjust 0 add-adjust  0 (WORD-NAMES) RECORDS ?DO
        I (WORD-NAMES) READ  LINK L@ WORD-NAMES READ
        ADDR L@  NAME OVER >R B@  FILE-PAD R> -TRAILING  Assem
        S" X_" PAD PLACE  PAD APPEND  PAD FIND IF
            EXECUTE ADJUST-DELTA
        ELSE  2DROP  Files -1 ADJUST N! Assem  THEN
    LOOP ;

: ADJUST-RANGES   LAB-UPB 1+ 1 ?DO  I MAKE-CURRENT
        'RANGE-START @ Files adjust-address Assem 'RANGE-START +!
        'RANGE-END @ Files adjust-address Assem 'RANGE-END +!
    LOOP ;
: ADJUST-DIRECTIVES   LAB-UPB 1+ 1 ?DO  I LABELS[]
        DUP @ Files adjust-address Assem SWAP +!
    LOOP ;
: ADJUST-COMMENTS   LAB-UPB 1+ 1 ?DO  I LABELS[]
        DUP @ Files adjust-address Assem SWAP +!
    LOOP ;
: ADJUST-EQUS   LAB-UPB 1+ 1 ?DO
        I LABELS[] DUP @ DUP Files adjust-address Assem ?DUP IF
            ROT 2DUP +!  >R SWAP R> CELL+ @ >NFA @
            2DUP $@ INVENTED-NAME? IF
                >R + INVENT-NAME R> PLACE
            ELSE  2DROP DROP  THEN
        ELSE  2DROP  THEN
    LOOP ;

: ADD-LABELS ( -- )   Files  WORD-NAMES RECORDS ?DO
        I READ  ADJUST N@ -1 = IF
            ADDR L@  NAME OVER >R B@  FILE-PAD R> -TRAILING
            Assem  S" LABEL X_" PAD PLACE  PAD APPEND
            PAD COUNT EVALUATE
        THEN
    LOOP ;

: DUMP-NAMES ( a2 a1 -- )   DO  I DUP S" names " CR+$
        @NAME TYPE SPACE  0 CELL+ +LOOP  CR ;
: DIS-NAMES ( a1 a2 -- )   TARGET>HOST SWAP TARGET>HOST
    DUP NEXT-CUT !  DUMP-NAMES ;
: -names ( a n -- )   2>R ['] DIS-NAMES  2R> RANGE ;
' -names RANGE: -names: ( -name- )
: -names- ( a n -- )   -warning >R  NONAME$ -names  R> +warning ;
' DIS-NAMES  ' -names:  ARE-COUPLED

65 CONSTANT TRIM#
VARIABLE #OUT  0 #OUT !
VARIABLE #CRS  1 #CRS !
VARIABLE capext   0 capext !
VARIABLE curcolor   0 curcolor ! \ color of current token

: CRS ( -- )   PADTYPE  #CRS @ ?DUP IF
        ABS 0 DO  CR  LOOP
        #CRS @ 0< IF
            S"      " PAD+
        THEN
    ELSE  PAD+BL  THEN ;
: |CR ( -- )   #CRS @ IF  #OUT @ IF  CR  THEN  THEN  1 #CRS ! ;
: PAD?TYPE ( -- )   PAD C@ #OUT @ + TRIM# > IF  PADTYPE |CR  THEN ;

: TRANSITION ( new -- x )   \ check against multiple transitions
    ( new    <-- )  curcolor @
    OVER 14 <>  OVER 14 =  AND IF  S"  }" PAD+ CRS THEN   \  b -> ~b
    OVER 13 <>  OVER 13 =  AND IF  S"  ^" PAD+     THEN   \  g -> ~g
    OVER  9 <>  OVER  9 =  AND IF  S"  )" PAD+     THEN   \  w -> ~w
    OVER  1 <>  OVER  1 =  AND IF  S"  ]" PAD+     THEN   \  y -> ~y
    OVER  7 <>  OVER  7 =  AND IF  S"  >" PAD+     THEN   \  c -> ~c
    PAD?TYPE
    OVER  7 =   OVER  7 <> AND IF  S"  <" PAD+     THEN   \ ~c ->  c
    OVER  1 =   OVER  1 <> AND IF  S"  [" PAD+     THEN   \ ~y ->  y
    OVER  9 =   OVER  9 <> AND IF  S"  (" PAD+     THEN   \ ~w ->  w
    OVER 13 =   OVER 13 <> AND IF  S"  ^" PAD+     THEN   \ ~g ->  g
    OVER 14 =   OVER 14 <> AND IF  S"  {" PAD+     THEN   \ ~b ->  b
    SWAP curcolor ! ;

: NEWC ( new -- )  ( DUP curcolor @ XOR IF ) TRANSITION ( THEN ) DROP ;
: gnn ( a -- a' n )   DUP >R  CELL+  R> @-LE ;
: n32 ( a x -- a' n )   DROP gnn ;
: n27 ( n -- n' )   2/ 2/ 2/ 2/ 2/ ;

HEX
: .NUMBER ( n -- )   DUP 1F AND
    DUP 02 = IF DROP PAD?TYPE S"  D# " PAD+ n32 (.)  PAD+ EXIT THEN \  y: execute 32-bit dec
    DUP 12 = IF DROP PAD?TYPE S"  H# " PAD+ n32 (H.) PAD+ EXIT THEN \ dy: execute 32-bit hex
    DUP 05 = IF DROP PAD?TYPE S"  D# " PAD+ n32 (.)  PAD+ EXIT THEN \  g: compile 32-bit dec
    DUP 15 = IF DROP PAD?TYPE S"  H# " PAD+ n32 (H.) PAD+ EXIT THEN \ dg: compile 32-bit hex
    DUP 06 = IF DROP PAD?TYPE S"  d# " PAD+ n27 (.)  PAD+ EXIT THEN \  g: compile 27-bit dec
    DUP 16 = IF DROP PAD?TYPE S"  h# " PAD+ n27 (H.) PAD+ EXIT THEN \ dg: compile 27-bit hex
    DUP 08 = IF DROP PAD?TYPE S"  d# " PAD+ n27 (.)  PAD+ EXIT THEN \  y: execute 27-bit dec
    DUP 18 = IF DROP PAD?TYPE S"  h# " PAD+ n27 (H.) PAD+ EXIT THEN \ dy: execute 27-bit hex
    DROP ;
DECIMAL

: 1CAP ( addr -- )   DUP C@ [CHAR] a [CHAR] z 1+ WITHIN
    IF  DUP C@ 32 - SWAP C!  ELSE  DROP  THEN ;
: CAPS ( addr len -- )   0 ?DO  DUP 1CAP 1+  LOOP  DROP ;

FALSE CONSTANT SHOW-UNKNOWN

: .WORD ( n -- )   0 capext !  PAD+BL  PADCOUNT 2DROP ;
: .BLUE ( n -- )   0 capext !  PAD+BL  PADCOUNT
    2DUP S" cr" COMPARE 0= IF  1  ELSE
    2DUP S" br" COMPARE 0= IF  2  ELSE
    2DUP S" -cr" COMPARE 0= IF  1  ELSE
    2DUP S" indent" COMPARE 0= IF  -1  ELSE
    2DUP S" ," COMPARE 0= IF  1  ELSE
    2DUP S" *" COMPARE 0= IF  1  ELSE
    OVER C@ [CHAR] . = IF  -1  ELSE
        SHOW-UNKNOWN IF  S"  ^" TYPE 2DUP DUMP  THEN  1
    THEN THEN THEN THEN THEN THEN THEN
    #CRS !  2DROP ;

: .CAPWORD ( n -- )   0 capext !  PADCOUNT DROP 1CAP ;
: .ALLCAPS ( n -- )   -1 capext !  PADCOUNT CAPS ;
: .EXTENSION ( n -- )   PADCOUNT capext @ IF  CAPS  ELSE  2DROP  THEN ;
: .COLONDEF ( -- )   PADTYPE  |CR S" :" PAD+ .WORD ;
: .VARIABLE ( -- )   S"  :#" PAD+ .WORD gnn  PAD+BL  (.) PAD+ ;
: .COMMENT# ( n -- )   S"  { " PAD+  (H.) PAD+  S"  }" PAD+ ;

HEX
: .GREY ( n -- )   PAD+BL  DUP 1F AND
    DUP 1D = IF  DROP  5 RSHIFT 2 (H.R) PAD+  ELSE
    DUP 0D = IF  2DROP  S" c01/1" PAD+  ELSE
        DROP 1 <?> 2 (H.R) PAD+
    THEN THEN ;

: .TOKEN ( n -- )   DUP 0F AND
    DUP  0 = IF DROP         .EXTENSION EXIT THEN \ --- extension word
    DUP  1 = IF DROP  1 NEWC .WORD      EXIT THEN \ yel execute word
    DUP  2 = IF DROP  1 NEWC .NUMBER    EXIT THEN \ yel execute 32-bit
    DUP  3 = IF DROP  3 NEWC .COLONDEF  EXIT THEN \ red define word
    DUP  4 = IF DROP  4 NEWC .WORD      EXIT THEN \ gre compile word
    DUP  5 = IF DROP  4 NEWC .NUMBER    EXIT THEN \ gre compile 32-bit
    DUP  6 = IF DROP  4 NEWC .NUMBER    EXIT THEN \ gre compile 27-bit
    DUP  7 = IF DROP  7 NEWC .WORD      EXIT THEN \ cya compile a macro
    DUP  8 = IF DROP  1 NEWC .NUMBER    EXIT THEN \ yel execute 27-bit
    DUP  9 = IF DROP  9 NEWC .WORD      EXIT THEN \ whi comment word
    DUP 0A = IF DROP  9 NEWC .CAPWORD   EXIT THEN \ whi Capitalized Word
    DUP 0B = IF DROP  9 NEWC .ALLCAPS   EXIT THEN \ whi ALL CAPS WORD
    DUP 0C = IF DROP 0C NEWC .VARIABLE  EXIT THEN \ mag variable + number
    DUP 0D = IF DROP 0D NEWC .GREY      EXIT THEN \ gre compiler feedback
    DUP 0E = IF DROP 0E NEWC .BLUE      EXIT THEN \ blu display word
    DROP         .COMMENT# ;          \ $F commented number
DECIMAL
: ABLOCK ( a -- )   DUP 1020 + SWAP  0 curcolor !
    BEGIN  2DUP >  OVER @ 0= 0=  AND WHILE  gnn .TOKEN
    REPEAT  6 NEWC  2DROP ;   \ dummy color to mark end of block

: DUMP-BLOCKS ( a2 a1 -- )   \ display blocks ready to be translated back
    CUT-SIZE @ >R  1024 CUT-SIZE !  BASE @ >R  DECIMAL
    DO  I CODE-SPACE - 1024 /  S" D# " PAD $!  DUP S>D <# #S #> PAD $+!
        1 AND IF  S"  shadow{ "  ELSE  S"  code{ "  THEN  PAD $+!
        I PAD $@ CR+$  CR  PAD|  I ABLOCK  PADTYPE  S"  }block" TYPE  CR
    1024 +LOOP  R> BASE !  R> CUT-SIZE !  CR ;

?? ForGForth uses : GET-TYPE   'TYPE @ ;
?? ForGForth uses : SET-TYPE   'TYPE ! ;

?? ForGForth 0= uses : GET-TYPE   'TYPE >DFA @ ;
?? ForGForth 0= uses : SET-TYPE   'TYPE >DFA ! ;

: TRIM-EMIT ( c -- )   GET-TYPE  'TYPE RESTORED  SWAP
    DUP BL = IF \ #OUT @ TRIM# > IF
            \    CR  SPACE  1 #OUT !
        (    THEN ) BL EMIT  1 #OUT +!  ELSE
        DUP 10 = IF  10 EMIT  0 #OUT !  ELSE
            DUP EMIT  1 #OUT +!
    THEN THEN  DROP  SET-TYPE ;

: TRIM-TYPE ( a n -- )   0 ?DO  COUNT TRIM-EMIT  LOOP  DROP ;

?? ForGForth uses : GET-TRIM   ['] TRIM-TYPE ;
?? ForGForth 0= uses : GET-TRIM   'TRIM-TYPE >DFA @ ;

: DUMP-TRIM-BLOCKS   GET-TRIM SET-TYPE  DUMP-BLOCKS  'TYPE RESTORED ;

: DIS-BLOCKS ( a1 a2 -- )   TARGET>HOST SWAP TARGET>HOST
    DUP NEXT-CUT !  DUMP-TRIM-BLOCKS ;
: -blocks ( a n -- )   2>R ['] DIS-BLOCKS  2R> RANGE ;
' -blocks RANGE: -blocks: ( -name- )
: -blocks- ( a n -- )   -warning >R  NONAME$ -blocks  R> +warning ;
' DIS-BLOCKS  ' -blocks:  ARE-COUPLED

PREVIOUS
\ Copyright (c) 2010 Dennis Ruffer

FALSE CONSTANT STACK-DEBUG

: STACK ( n -- )   \ use: n stack word
    CREATE HERE CELL+ , HERE 0 ,
    SWAP CELLS ALLOT
    HERE SWAP ! ;

: STACK-CLEAR? ( stack -- f )   DUP @ [ 1 CELLS ] LITERAL - = ;

: STACK-DUMP ( stack -- )   DUP STACK-CLEAR? 0= IF
        ." > " DUP @ SWAP CELL+
        BEGIN  2DUP - WHILE  CELL+ DUP ?
        REPEAT  2DROP ." < "
    ELSE  DROP  THEN ;

: STACK-PUSH ( n stack -- )   CELL OVER +!
    DUP 2@ < ABORT" stack full"
    DUP -ROT @ !  STACK-DEBUG IF
        CR ." PUSH " DUP STACK-DUMP
    THEN  DROP ;

: STACK-TOP ( stack -- n )   \ returns top of stack
    DUP STACK-CLEAR? ABORT" no items on stack"
    DUP @ @  SWAP STACK-DEBUG IF
        CR ." TOP  " DUP STACK-DUMP
    THEN  DROP ;

: STACK-TOP? ( stack -- n|0 )   \ returns top or 0
    DUP STACK-CLEAR? IF  DROP 0
    ELSE  STACK-TOP  THEN ;

: STACK-POP ( stack -- )   \ discards top stack item
    DUP STACK-CLEAR? ABORT" no items on stack"
    -1 CELLS OVER +!  STACK-DEBUG IF
        CR ." POP  " DUP STACK-DUMP
    THEN  DROP ;

: STACK-PULL ( stack -- n )   DUP STACK-TOP SWAP STACK-POP ;

: STACK-PULL? ( stack -- n|0 )   \ pull top if available
    DUP STACK-CLEAR? IF  DROP 0
    ELSE  STACK-PULL  THEN ;

: STACK-CLEAR ( stack -- )   DUP CELL+ SWAP ! ;

?? ForGForth uses ALSO ASSEMBLER

VARIABLE #ICONS      \ Number of icons in the array
VARIABLE #ICON-COL   \ Index into array of icons at HERE
VARIABLE #ICON-ROW   \ Scan row within the icons we are assembling

ICON-COLUMNS 8 / CONSTANT |ICON-ROW|
ICON-ROWS 1+ |ICON-ROW| * CONSTANT |ICON-BUFFER|

: >ICON ( i -- a )   |ICON-BUFFER| * HERE 32 + + ;
: >ICON-ROW ( i r -- a )   1+ |ICON-ROW| * SWAP >ICON + ;

: SAVE-ROW ( str len -- )   2 BASE !
    0 0 2SWAP >NUMBER 2DROP DROP
    #ICON-COL @  #ICON-ROW @
    >ICON-ROW W!-BE ;

: SAVE-ICON ( str len -- )   HEX
    0 0 2SWAP >NUMBER 2DROP DROP
    #ICON-COL @ >ICON >R  #ICON-ROW @ IF
        R@ W@-BE  4 LSHIFT +  R> W!-BE  ELSE
        R@ |ICON-BUFFER| ERASE  R> W!-BE  THEN ;

: SAVE-ICONS ( -- )   HEX  #ICONS @ 0 ?DO
        I >ICON DUP W@-BE >R  |ICON-ROW| +
        |ICON-BUFFER| |ICON-ROW| - DUP R> *
        AS-HERE + SWAP MOVE
    LOOP  0 #ICONS ! ;

: NEXT-ICON-ROW ( -- )
    REFILL 0= ABORT" End of input before icons finished"
    #ICON-COL @ IF  #ICON-COL @ #ICONS !  0 #ICON-COL !  1 #ICON-ROW +!
    ELSE  SAVE-ICONS  0 #ICON-COL !  0 #ICON-ROW !
    THEN ;

: icons{ ( -- )   BASE @ >R
    0 #ICONS !  0 #ICON-COL !  0 #ICON-ROW !
    BEGIN
        BEGIN
            BEGIN  #ICON-ROW @ 2 <
            WHILE  BL WORD COUNT  DUP 0=
                WHILE  2DROP  NEXT-ICON-ROW
                REPEAT
                2DUP S" }icons" COMPARE
                IF  SAVE-ICON
                ELSE  2DROP  SAVE-ICONS  R> BASE !  EXIT
                THEN
            THEN
            BL WORD COUNT  DUP 0=
        WHILE  2DROP  NEXT-ICON-ROW
        REPEAT
        2DUP S" }icons" COMPARE
        IF  SAVE-ROW  1 #ICON-COL +!
        ELSE  2DROP  SAVE-ICONS  R> BASE !  EXIT
        THEN
    AGAIN ;
VARIABLE CFEND                    \ address of last colorForth token in block
VARIABLE NBITS                    \ number of bits left in token
VARIABLE WORDLEN  0 WORDLEN !    \ size of current ASCII word name
VARIABLE 1STCAP  TRUE 1STCAP !    \ 1st letter that can be capitalized

20 STACK SAVECOLOR    \ remember colors around extensions and comments

: /BITS ( -- )        \ reset token specific flags
    28 NBITS !  TRUE 1STCAP !
    curcolor CASE
         0 OF  SAVECOLOR STACK-TOP? ?DUP
            IF  curcolor !  THEN  ENDOF
         3 OF  4 curcolor !  ENDOF
        10 OF  9 curcolor !  ENDOF
        11 OF  9 curcolor !  ENDOF
        12 OF  1 curcolor !  ENDOF
    ENDCASE ;

: .CFPTR ( -- )   BASE @ >R  DECIMAL  CR            \ display block location
    CP @ CODE-SPACE -  1024 /MOD 3 .R  4 / 4 .R  SPACE
    R> BASE ! ;

: .AFPTR ( -- )   SOURCE DUP >R TYPE                \ display text location
    CR 27 SPACES  >IN @ DUP R@ - IF  DUP WORDLEN @ + R@ - 1-
        IF  1-  THEN  THEN  R> DROP  WORDLEN @ - SPACES
    WORDLEN @ 0 DO  S" ^" TYPE  LOOP ;
FALSE CONSTANT TESTING

: ADDN ( n -- )   TESTING IF  DUP CP @ @-LE -    \ add a fully formed token
        IF  .CFPTR  DUP 8 H.R S" <>" TYPE CP @ @-LE 8 H.R SPACE .AFPTR
    THEN  THEN  CP @ !-LE  4 CP +!  /BITS ;

: ADDC ( n -- )   NBITS @ 4 + LSHIFT  curcolor @ ( DUP >R ) +    \ add text token
    ADDN ( ??? R> 0= IF  SAVECOLOR STACK-POP  THEN ) ;

FALSE CONSTANT GETWORD-DEBUG

: GETWORD ( -- str len )    \ get next ASCII word or abort if none left
    BEGIN  BL WORD COUNT  GETWORD-DEBUG IF
            2DUP TYPE SPACE  STACK-DEBUG IF
                SAVECOLOR STACK-DUMP
            THEN
        THEN  DUP WORDLEN !  DUP 0=
    WHILE  2DROP REFILL 0= ABORT" end of input before block finished"
    REPEAT ;

: GETN ( hex -- n )   BASE @ SWAP IF  HEX  ELSE  DECIMAL  THEN
    0 0 GETWORD OVER C@ [CHAR] - = DUP >R   \ get an ASCII number
    IF  1 /STRING  THEN  >NUMBER ROT OR
        IF  .CFPTR  S" Invalid number     " TYPE  .AFPTR
        THEN  DROP R>
    IF  NEGATE  THEN  SWAP BASE ! ;

: LARGEN ( -- color )   curcolor @ 1 = IF  2 EXIT  THEN  5 ;
: SMALLN ( -- color )   curcolor @ 1 = IF  8 EXIT  THEN  6 ;

HEX

: L/S ( hex -- n )   \ format a numeric token, adding prefix token if large
    DUP >R GETN DUP 80000000 = OVER ABS 2* F8000000 AND OR
    IF  LARGEN R> + ADDN EXIT  THEN
    5 LSHIFT SMALLN R> + + ;

DECIMAL

: SETCOLOR ( n _ -- )   CREATE , DOES> @ curcolor ! ;   \ define format words

HEX

CREATE II-CF   HERE 60 DUP ALLOT  ERASE   \ ASCII to cf translation table

: /II-CF ( -- )   cf-ii COUNT 1
    DO  I 2DUP + C@  DUP [CHAR] a [CHAR] z 1+ WITHIN
        IF  SWAP 40 +  SWAP 20 -  2DUP II-CF + C!        \ lower case alpha
            SWAP 40 +  SWAP 20 -       II-CF + C!        \ upper case alpha
        ELSE                20 -       II-CF + C!        \ non-alpha chars
        THEN
    LOOP  DROP ;

/II-CF   \ fill programatically to reduce maintenance

: CHC ( ascii -- cf )   \ convert ASCII to cf character
    DUP 80 20 WITHIN IF  .CFPTR S" invalid character  " TYPE  .AFPTR  0=
    ELSE  -20 + II-CF + C@  THEN ;

DECIMAL

: HUF ( cf -- cf huf len )   \ convert cf char to huffman, #-bits
    DUP 63 AND
    DUP 16 < IF
        DUP 8 < IF
            4 ( 0xxx 0-7 )
        ELSE  24 XOR 5 ( 10xxx 8-15 )
        THEN
    ELSE  80 + 7 ( 11xxxxx 16-47 )
    THEN ;
: SHORTPACK ( n cf huf len -- cf huf' n' len' )   >R ROT R@ NBITS @ >
    IF  OVER DUP  R@ NBITS @ -  DUP >R RSHIFT  R@ LSHIFT XOR ( ??? ) drop 0
        IF  R> DROP  ADDC  0  curcolor @ SAVECOLOR STACK-PUSH  0 curcolor !
        ELSE  SWAP R> R> OVER - >R RSHIFT SWAP  THEN
    THEN  R> ;

: FIRSTCAP ( -- )   \ process 1st capital character
    curcolor @ 9 =  curcolor @ 0= OR NOT
    IF  .CFPTR  S" Cap not in comment " TYPE  .AFPTR  THEN
    curcolor @ IF  10 curcolor !  FALSE 1STCAP !  THEN ;

: ALLCAPS ( -- )   \ process folllowing capital characters
    curcolor @ 10 =  curcolor @ 11 = OR NOT
    IF  .CFPTR  S" Not 1st/all caps   " TYPE  .AFPTR  THEN
    curcolor @ IF  11 curcolor !  THEN ;

: NOTCAP ( -- )   \ process lower case characters
    curcolor @ 11 = IF  .CFPTR  S" Must be all caps   " TYPE  .AFPTR  THEN
    FALSE 1STCAP ! ;

: PACK ( n cf huf len -- n' )   SHORTPACK   \ pack huffman characters
    DUP >R ( len )  LSHIFT + >R ( n )  DUP 128 AND
    IF  1STCAP @ IF  FIRSTCAP  ELSE  ALLCAPS  THEN
    ELSE  DUP 64 AND IF  NOTCAP  THEN
    THEN  DROP R> ( n )  R> ( len ) NEGATE NBITS +! ;

: ADDWORD ( str len -- )   \ add ASCII word to image
    0 -ROT  OVER + SWAP ?DO
        I C@ CHC HUF PACK
    LOOP  ADDC ;

: names ( -- )   0 curcolor !  SAVECOLOR STACK-CLEAR  /BITS
    BEGIN  BL WORD COUNT  DUP WORDLEN !  ?DUP WHILE
            S" _" 2OVER COMPARE 0= IF
                2DROP 0 ADDN  ELSE  ADDWORD  THEN
    REPEAT  DROP ;
FALSE CONSTANT SHOW-#TOKENS

: ENDBLOCK ( -- )   \ finish up the block by filling it with null tokens
    CFEND @ CP @ - 4 /MOD DUP >R 0< OR ABORT" bad pointers"
    SHOW-#TOKENS IF  .CFPTR S" tokens processed ok" TYPE
    THEN  0 curcolor !  R> 1+ 0 DO  0 ADDN  LOOP ;

GET-CURRENT ( * )   WORDLIST DUP CONSTANT AF-VOC   SET-CURRENT

: D# ( _ -- )   BASE @ DECIMAL 0 L/S ADDN BASE ! ;
: H# ( _ -- )   BASE @ HEX    16 L/S ADDN BASE ! ;

: :# ( _name _value -- )   curcolor @ SAVECOLOR STACK-PUSH \ compile a variable
    CP @  12 curcolor !  GETWORD ADDWORD  CP @ SWAP - 4 -
    IF  .CFPTR  S" Var name too long  " TYPE  .AFPTR  THEN
    0 GETN ADDN  SAVECOLOR STACK-PULL curcolor ! ;

: }blocks ( -- ) ;  \ terminate blocks
: }block ( -- )   \ terminate block
    ENDBLOCK  -1 curcolor ! ;

: }   SAVECOLOR STACK-PULL curcolor ! ; \ restore color
: { ( _value -- )   \ compile an error or comment
    curcolor @ SAVECOLOR STACK-PUSH  BASE @  HEX
    0 0 GETWORD 2DUP >R >R  OVER C@ [CHAR] - =
    DUP >R IF  1 /STRING  THEN  >NUMBER ROT OR
    IF  2DROP R> DROP  14 curcolor !  R> R> ADDWORD
    ELSE  DROP R> IF  NEGATE  THEN  ADDN  R> R> 2DROP
    THEN  BASE ! ;

: ^ ( _value -- )   \ compile a display token
    curcolor @ 13 = IF  SAVECOLOR STACK-PULL curcolor ! \ restore color
    ELSE  curcolor @ SAVECOLOR STACK-PUSH  BASE @  HEX
        GETWORD  2DUP S" c01/1" COMPARE 0= IF  2DROP 13  ELSE
            0 0 2SWAP >NUMBER NIP OR ABORT" Invalid display number"
        THEN  13 CURCOLOR !  5 LSHIFT 29 + ADDN  BASE !
    THEN ;

3 SETCOLOR :        \ define
1 SETCOLOR [        \ execute
4 SETCOLOR ]        \ compile
7 SETCOLOR <        \ compile macro
4 SETCOLOR >        \ compile

: )   SAVECOLOR STACK-PULL curcolor ! ;                \ restore color
: (   curcolor @ SAVECOLOR STACK-PUSH  9 curcolor ! ;    \ comment

( * ) SET-CURRENT
: +TOKEN ( str len -- )   \ process ASCII word
    2DUP AF-VOC SEARCH-WORDLIST
    IF  EXECUTE  2DROP  EXIT  THEN
    ADDWORD ;

: CBLOCK ( blk# -- )   \ process ASCII into a block
    CFBLOCK DUP CP !  1020 + CFEND !
    4 curcolor !  SAVECOLOR STACK-CLEAR  /BITS
    BEGIN  curcolor @ 0< NOT  WHILE
        CFEND CP @ > NOT
        IF  .CFPTR  S" Block too long     " TYPE  .AFPTR
            BEGIN  GETWORD S" }block" COMPARE 0=
            UNTIL  -1 curcolor !
        ELSE  GETWORD +TOKEN  THEN
    REPEAT ;

: code{ ( n -- )   DUP 1 AND   \ start code block
    IF  CR  8 SPACES S" Odd code block     " TYPE
        5 WORDLEN !  .AFPTR  THEN
    CBLOCK ;

: shadow{ ( n -- )   DUP 1 AND 0=   \ start shadow block
    IF  CR  8 SPACES S" Even shadow block  " TYPE
        7 WORDLEN !  .AFPTR  THEN
    CBLOCK ;

: D# ( -n- n )   BASE @ DECIMAL  0 0 BL WORD COUNT
    >NUMBER ABORT" Invalid number" 2DROP  SWAP BASE ! ;

PREVIOUS
[DEFINED] ForCiForth [IF]
    REQUIRE #-PREFIX    \ In behalf of user.
    REQUIRE ARGC    \ In behalf of building an executable.
[THEN]

: WRITE-ONE-SECTION ( handle -- handle )
    >R  FILE-OFFSET 0 R@ REPOSITION-FILE THROW
    CODE-SPACE CP @ OVER - R@ WRITE-FILE THROW  R> ;
: WRITE-SECTIONS ( handle -- handle )   SECTION-REGISTRY DO-BAG
        I @ TO CURRENT-SECTION  WRITE-ONE-SECTION
    LOOP-BAG ;
: OPEN-IT ( a n -- handle )   R/W CREATE-FILE THROW ;
: CLOSE-IT ( handle -- )   CLOSE-FILE THROW ;

[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
    : SOURCE-AS ( -- a n )   BL WORD COUNT ;
    : TARGET-AS ( -- a n )   BL WORD COUNT ;
[THEN]
[DEFINED] ForCiForth [IF]
    : SOURCE-AS ( -- a n )   1 ARG[] ;
    : TARGET-AS ( -- a n )   2 ARG[] ;
[THEN]

: WRITE-IT ( a n -- )   OPEN-IT  WRITE-SECTIONS  CLOSE-IT ;
VARIABLE file.cf    \ colorForth source
VARIABLE file.cfo   \ colorForth source reproduced
VARIABLE file.cul   \ consultant file
VARIABLE file.dsm   \ disassembled output

: make-filename ( aext next a n ahere -- )
    >R  DUP 5 + ALLOT  R@ PLACE  R> APPEND ;
: make-filenames ( a n -- )   HERE DUP >R file.cf !
    DUP 4 + ALLOT  R@ PLACE  R@ COUNT  S" .cf" R> APPEND
    S" .cfo" 2OVER  HERE DUP file.cfo ! make-filename
    S" .cul" 2OVER  HERE DUP file.cul ! make-filename
    S" .dsm" 2SWAP  HERE DUP file.dsm ! make-filename ;
: PARSE-ASM ( -- )
    BEGIN BEGIN  BL WORD DUP C@
            WHILE  FIND IF  EXECUTE
                ELSE  COUNT 2DUP S" ASM]" $= IF  2DROP EXIT
                    THEN  OVER C@ [CHAR] : = IF  1 /STRING
                        KNOWN-LABEL? IF  2DROP
                        ELSE  _AP_ -ROT LABELED
                        THEN
                    ELSE  OVER C@ [CHAR] - = DUP >R IF  1 /STRING
                        THEN  0 0 2SWAP >NUMBER 2DROP DROP  R> IF  NEGATE
                        THEN
                    THEN
                THEN
            REPEAT
        DROP REFILL 0= UNTIL ;
: [ASM ( -- )   BASE @ >R  GET-ORDER
    POSTPONE ONLY POSTPONE FORTH POSTPONE ALSO POSTPONE ASSEMBLER
    SAVE-INPUT  FIRSTPASS  2 0 DO  DEPTH >R  PARSE-ASM  CR  DEPTH R> -
        IF  .S  TRUE ABORT" Stack depth error"  THEN
        I 0= IF  SECONDPASS  RESTORE-INPUT THROW  THEN
    LOOP  SET-ORDER  R> BASE ! ;
: cias ( -- )   SOURCE-AS INCLUDED  TARGET-AS WRITE-IT ;
: FETCHED ( a n -- )   GET-FILE DUP CODE-LENGTH @ > ABORT" Too big!"
    CODE-SPACE SWAP  2DUP + CP !  MOVE ;
: FETCH ( -- )   BL WORD COUNT FETCHED ;

[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
    : TARGET-DIS ( -- a n )   BL WORD COUNT ;
[THEN]
[DEFINED] ForCiForth [IF]
    : TARGET-DIS ( -- a n )   2 ARG[] ;
    REQUIRE DUMP
[THEN]

: CONSULTED ( a n -- )   INIT-ALL  HEX  INCLUDED ( file)  SORT-ALL
    PLUG-HOLES  ALL-L-LABELS  DISASSEMBLE-TARGET  DECIMAL ;
: CONSULT ( -- )   BL WORD COUNT CONSULTED ;

[DEFINED] ForSwiftForth
[DEFINED] ForGForth OR [IF]
    : cidis ( -- )   BL WORD COUNT FETCHED TARGET-DIS CONSULTED ;
    : cfdis ( -- )   BL WORD COUNT make-filenames
        file.cf @ COUNT FETCHED  file.cul @ COUNT CONSULTED ;
    : cfas ( -- )   BL WORD COUNT make-filenames
        TESTING IF  file.cf @ COUNT FETCHED  THEN
        file.dsm @ COUNT INCLUDED  file.cfo @ COUNT WRITE-IT ;
[THEN]
[DEFINED] ForCiForth [IF]
    : cidis ( -- )   1 ARG[] FETCHED TARGET-DIS CONSULTED ;
[THEN]

: RESTORE-ALL ( -- )   '?ERROR RESTORED  'SECTION RESTORED
    'TYPE RESTORED ;
RESTORE-ALL

[DEFINED] ForCiForth [IF]
    : INTERACTIVE    'OK DUP >DFA @ SWAP >PHA = IF 0 LIST OK THEN
        ASSEMBLER   0 ORG   QUIT ;
    : HANDLE-ARG   ARGC 1 = IF INTERACTIVE THEN
        ARGC ( 2) 3 4 WITHIN 0= 13 ?ERROR ;
    : CONTAINS-D?    2DUP &D $I >R  &d $I R>  OR ;
    "forth.lab" BLOCK-FILE $!
    'TASK >DFA @   '.SIGNON >DFA !
    : MAIN   RESTORE-ALL  DEFAULT-SEGMENT HANDLE-ARG
        0 ARG[] CONTAINS-D? IF cidis ELSE cias THEN ;
[THEN]
