\ eForth Terminal for SwiftForth
\ ==============================================================
\ todo
\ eForth
\ save -- marker tasks
\ buffers
\
\ SwiftForth
\ windows sfk key? ekey? do not work correctly
\ : k1 ( -- ) begin 0 0 begin 1 0 d+ key? until d. key dup emit bl = until ;
\ key? sf linux works, gforth works
\ key akey ekey ??? ttyekey ttyemit ???
\ BUG Linux e-accept !!! ubuntu 11.04 ok puppy 513 bad
\     LF on BS + scroll ??? SIMPLE-ACCEPT ok but headerless
\     : m ( -- ) pad dup 99 accept space type ;
\     edit (accept)  edit e-accept
\ BUG %cpu (com-key?) (com-key)
\ ==============================================================
\ locate <name>
\ edit [<name>]
\ wh where <name>
\ ==============================================================

[defined] m.e4term [if] m.e4term [then]
marker M.E4TERM
decimal

[defined] WIN32-CONSTANTS
[defined] WIN64-CONSTANTS or constant win? immediate
1 constant gui? immediate

[defined] of [if]
: =if ( -- sys ) ( n -- n | n -- ) [compile] of ; immediate
[else]
: =if ( -- sys ) ( n -- n | n -- )
  ['] over compile,  ['] = compile,
  [compile] if  ['] drop compile, ; immediate
[then]

\ ==============================================================
256 constant tpadsize
create tpad  tpadsize chars allot

0 constant nmi immediate ( NewMicrosInc )

128 2 +  nmi [if] drop 80 [then]
  constant tibsize ( Input Buffer Size )

win? [if] \ Windows
: acceptt ( -- ca n ) pad  dup tpadsize accept ;

[else] \ Linux BUG e-accept BUG -- scroll lf on bs bbb ???
\ defined headerless grrr

8 constant <bs>

: .BS ( lim addr n char -- lim addr n' )
   DROP  DUP IF  1-  8 EMIT BL EMIT 8 EMIT  THEN ;

: .ESC ( lim addr n char -- lim addr 0 )
   DROP  BEGIN  DUP WHILE 0 .BS REPEAT ;

: .RETURN ( lim addr n char -- n addr -1 )
   DROP -ROT NIP -1 ;

: .ACCEPTED ( lim addr n char -- lim addr n' )
   FOURTH THIRD = IF  DROP  EXIT  THEN
   BL MAX  DUP EMIT  THIRD THIRD + C!  1+ ;

: ASTROKE ( lim addr n char -- lim addr n )
   DUP 127 > IF  DROP EXIT  THEN
   dup 127 = if  .bs exit  then ( delete ) \ bbb !!!
   DUP <BS> = IF  .BS EXIT  THEN
   DUP 13 = IF  .RETURN EXIT  THEN
   DUP 27 = IF  .ESC EXIT  THEN
   .ACCEPTED ;

: SIMPLE-ACCEPT ( addr n -- n ) ( SwiftForth headerless )
   SWAP 0 BEGIN ( lim addr n )
      DUP 0< NOT WHILE
      AKEY ASTROKE
   REPEAT 2DROP ;

1 [if] \ puppy 513 bad
: acceptt ( -- ca n ) pad  dup tpadsize SIMPLE-ACCEPT ;
[else] \ ubuntu 11.04 ok
: acceptt ( -- ca n ) pad  dup tpadsize accept ;
[then]

[then]

\ ==============================================================
: scounter ( -- n ) counter ; \ bbb

win? [if] \ Windows
: counter ( -- ms )
  DCOUNTER  100 0 0 sp@
  QueryPerformanceFrequency drop nip m*/ drop ;

[else] \ Linux
: counter ( -- ms ) GET-TIME ( us s ) 100 * swap 10000 / + ;

[then]

: timer ( ms -- )
  counter swap - 0
  <# # #  [char] . hold  #s #> type ."  seconds" ;

variable sda ( Short Delay Adjust )
: ~100us ( n -- ) ( short delay for chardelay )
  begin ?dup
  while  sda @
    begin  1-  over expired drop  ?dup 0=
    until  1-
  repeat ;

: calibrate ( -- ) ( time 100ms adjust for ~100us )
  scounter 100 +  0 ( ms 0 )
  begin  1+  over expired  0= 0=
  until ( ms n ) 1000 / sda !  drop ;

1 [if] calibrate ( test )
: z1 ( n -- ) >r counter r>     ms timer ;
: z2 ( n -- ) >r counter r> ~100us timer ;
: m ( -- )  1000 z1 ; ( ~1000 ms )
: z ( -- ) 10000 z2 ; ( ~1000 ms )
[then]

\ ==============================================================
: ?path ( f -- ) throw ;

win? [if] \ Windows
: slash ( -- ca n ) s" \" ;

\ GetCurrentDirectory ( n ca -- n )
: path@ ( -- ca n ) pad tpadsize over GetCurrentDirectory ;

\ SetCurrentDirectory ( za -- f )
: path! ( ca n -- )
  pad ZPLACE pad SetCurrentDirectory 0= ?path ;

: dpath ( -- ) ( default demo path )
  s" ..\demo" ;

: ls ( -- ) dir ;

[else] \ Linux
: slash ( -- ca n ) s" /" ;

\ getcwd ( ca n -- za )
: path@ ( -- ca n ) pad tpadsize getcwd ZCOUNT ;

\ chdir ( za -- f )
: path! ( ca n -- ) pad ZPLACE pad chdir ?path ;

: dpath ( -- ) ( default demo path )
  s" ../demo" ;

( -? turns off 'previously defined' warning )
-? : ls ( -- ) s" ls --color=auto -F" >shell ;

[then]

: /dpath ( -- ) dpath path! ;

/dpath ( init default demo path )

\ "-" not supported, spaces ok
: !cd ( -- ) acceptt ['] path! catch if 2drop ."  path?" then ;

\ ==============================================================
\ ideas
create l#  0 , 24 , ( line, max-1 )
create c#  0 , 81 , ( column, max-1 )
: temit ( c -- ) emit  1 c# +! ;
: tcr ( -- ) cr  0 c# !  1 l# +! ;
: ?tcr ( -- ) c# 2@ u< if tcr then ;

: ?lf ( -- ) c# cell+ @ get-xy drop u< if cr then ;

\ ==============================================================
\ serial communication

create chardelay  3 , 111 , ( after sending character )
chardelay cell+ constant linedelay ( after sending line )

: siokey? ( -- f ) (com-key?) ;
: siokey ( -- c ) (com-key) ;
: sioemit ( c -- ) (com-emit) chardelay @ ~100us ;

: siotype ( ca n -- ) bounds ?do i c@ sioemit loop ;
: siocr ( -- ) [ctrl] M sioemit ;
: sioecho ( -- ) begin siokey? while siokey emit repeat ;
: siosend ( ca n -- ) sioecho  siotype siocr ;
: siosendkeys ( ca n -- ) ( send timed )
  200 chardelay dup @ >r +!  siosend  r> chardelay ! ;

\ ==============================================================
create line#  0 , 28 , ( current line number, mod lines for cr )
: /line ( -- ) 0 line# ! ;
: +line ( -- ) 1 line# +! ;

win? [if] \ Windows
OFN-DIALOGS +ORDER ( access 'OFN-DIALOG' )
OFN-DIALOG subclass choose-file-dialog
: custom ( -- title filter flags )
  Z" Choose file"  all-files  default-open-flags ;
end-class
OFN-DIALOGS -ORDER

: choose-file ( -- )
  [objects choose-file-dialog makes cfd objects]
  cfd choose  /line
  if  cfd filename ZCOUNT  tpad place  exit
  then 0 tpad ! ;

[else] \ Linux
: choose-file ( -- )
  ."  file: "  /line
  path@ tpad place  slash tpad append  acceptt ( ca n ) dup
  if tpad append exit then and tpad ! ;

[then]

: ?edit ( -- ) \ bbb ??? win
  line# @ ?dup 0= if 1 ." Edit" choose-file then
  tpad count ?dup 0= if 2drop exit then
  ( line ca n ) edit-file /line ;

create 'loader  ' 2drop ,
: ?load ( -- ) \ bbb ??? catch ???
  /line tpad count dup
  if 'loader @execute exit
  then 2drop siocr ;

\ ==============================================================
\ file access

ctrl Q constant pacer    ( xon )
ctrl [ constant ESC_KEY  ( esc )
ctrl [ constant HELP_KEY ( esc )

: .err ( -- ) ."  Error " 1 throw ;
: f.err ( -- ) ." -File" .err ;

256 constant lpadsize
create lpad  lpadsize chars allot

variable fid ( file id )

: ?open-file ( ca n -- )
  r/o open-file if ."  Open" f.err then fid ! ;

: ?close-file ( -- )
  fid @ ?dup
  if close-file if cr ." Close" f.err then  0 fid ! then ;

: read-line? ( -- n f )
  lpad tibsize fid @ read-line
\ drop -1 \ bbb testing force read-line error
  if cr ." Read-Line" .err then ;

: ?line ( n -- n )
  tibsize over u< if cr line# ? ." Line Too Long" .err then ;

: ?user ( -- ) key? if key esc_key = if 2 throw then then ;

: doter ( -- ) [char] . emit  ?lf ;
: texter ( ca n -- ca n ) cr 2dup type ;

create verbosity  1 ,
: .verbosity ( -- )
  verbosity @ ?dup 0= if ." none" exit then
  1- if ." text" exit then ." dots" ;
: ~verbosity ( -- )
  verbosity @ 1+ 3 mod verbosity !  .verbosity ;
create 'verbosity  0 ( none ) ,  ' doter ,  ' texter ,
: progress ( -- ) 'verbosity verbosity @ cells + @execute ;

\ ==============================================================
\ Pacer upload

: ?pace ( -- )
  begin
    begin  ?user  siokey?
    until  siokey  dup pacer xor ( eForth ready )
  while dup 0= ( eForth sends 0 on error )
    if 3 throw then  emit ( eForth output echo )
  repeat drop ;

: _pupload ( -- )
  counter >r
  ?open-file  s"  FILE" siosendkeys
  begin  ?pace  read-line? ( n f )
  while  ?line  lpad swap  progress  siosend  +line
  repeat drop
  r> cr timer
;

: .hand ( -- ) s"  [ HAND " siosendkeys /line ;

: pxxx ( n -- )
  500 ms sioecho ( wait for response ) \ bbb ???
  1 =if exit then ( file open, close, read error )
  2 =if ." User Abort" .hand exit then ( user abort )
  3 =if ." Line " line# ? exit then ( compile error )
  drop ." unknown throw" ;

: pupload ( ca n -- ) ( pacer upload )
  ['] _pupload catch ?dup
  if pxxx 2drop
  else .hand
  then ?close-file ;

: cpupload ( -- ) ['] pupload 'loader !  choose-file ?load ;

\ ==============================================================
\ Timed upload

: tprogress ( -- )
  +line
  begin siokey?
  while siokey ?dup 0= ( eForth sends 0 on error )
    if 3 throw then emit
  repeat ;

: _tupload ( ca n -- ) ( timed upload )
  ?open-file
  begin  ?user  read-line?
  while  ?line  lpad swap siosend  linedelay @ ms  tprogress
  repeat drop  sioecho ;

: txxx ( n -- )
  500 ms sioecho ( wait for response ) \ bbb ???
  1 =if exit then ( file open, close, read error )
  2 =if ." User Abort" /line exit then ( user abort )
  3 =if ." Line " line# ? exit then ( compile error )
  drop ." unknown throw" ;

: tupload ( ca n -- )
  ['] _tupload catch ?dup
  if txxx 2drop
  else /line
  then ?close-file ;

: ctupload ( -- ) ['] tupload 'loader !  choose-file ?load ;

\ ==============================================================
create 'upper  ' upper ,  0 ,
: .upper ( -- ) 'upper @ if ." upper" exit then ." lower" ;
: ~upper ( -- ) 'upper 2@ swap 'upper 2! .upper ;

SERIALPORT +ORDER ( access 'COM-SPEED' and 'COM-NAME' )
: .comport ( -- )
  base @ decimal
  COM-SPEED ? ." comport " COM-NAME ZCOUNT type
  base ! ;
SERIALPORT -ORDER

: .stat ( -- )
  .comport space space
  ." CharDelay=" CharDelay ?
  ." LineDelay=" LineDelay ?
  space .upper  space .verbosity ;

: sload ( ca n -- )
  path@ tpad place  slash tpad append
  ( ca n ) tpad append  tpad count pupload ;

: appload ( -- ) \ bbb ??? halt on error ???
  counter >r
  s" lib-demo.e4" sload
  s" hanoi.e4"    sload
  s" random.e4"   sload
  s" sieve.e4"    sload
  s" tetris.e4"   sload
  s" led-task.e4" sload
  r> cr timer ;

: apps ( -- ) s" appload" evaluate ;

: crs ( -- ) cr 3 spaces ;
: crx ( n -- n ) 1 xor dup if crs then ;
: help ( -- ) 0
  crx ." *--------------------"
  crx ." --------------------+"
  crs ." | Terminal           "
      ."    Escape and...    |"
  crx ." +--------------------"
  crx ." --------------------+"
  crx ." |  H  Help          |"
  crx ." |  S  Status        |"
  crx ." |  P  Pacer load    |" \ bbb
\ crx ." |  T  Timer load    |" \ bbb
  crx ." |  R  Re-load       |"
  crx ." |  E  Edit [file]   |"
  crx ." |  L  List files    |"
  crx ." |  D  Display path  |"
  crx ." |  C  Change path   |"
  crx ." |  U  Uppercase     |"
  crx ." |  V  Verbosity     |"
  crx ." |  X  eXit          |"
  crx ." |  A  App load      |"
  crx ." +--------------------"
  crx ." --------------------+"
  crs ." |  Host              "
      ."    T  Terminal      |"
  crx ." +--------------------"
  crx ." --------------------*" drop
;

: crm ( -- ) cr ." ok " ;
: mkey ( 0 -- f )
  key upper ( allow both upper and lower case selection )
  [char] A =if apps crm exit then
  [char] C =if ." Change path: " !cd crm exit then
  [char] D =if pwd crm exit then
  [char] E =if ?edit crm exit then
  [char] H =if help crm exit then
  [char] L =if ls crm exit then
  [char] P =if cpupload exit then
  [char] R =if ?load exit then
  [char] S =if .stat crm exit then
\ [char] T =if ctupload exit then
  [char] U =if ~upper crm exit then
  [char] V =if ~verbosity crm exit then
  [char] X =if 0= exit then ( exit terminal to SwiftForth )
  [char] Q =if key upper [char] Q = if BYE then [ctrl] M then
  sioemit ;

: sio_emit ( 0 c -- f )
  help_key =if mkey exit then
  127      =if [ctrl] h then ( convert del to bs )
  'upper @execute ( nmi 68hc11 only uppercase )
  sioemit ;

\ ==============================================================
win? [if] \ Windows
\ extending ansi terminal escape sequences

: DIGIT? ( c base -- n f ) ( try to convert a char to a number )
  >R [CHAR] 0 - 9 OVER <
  IF -7 + DUP 10 < OR THEN DUP R> U< ;

: MORE-DIGITS ( n -- n' f ) ( collect digits and form a number )
  BEGIN SIOKEY  DUP 10 DIGIT? ( valid decimal digit ? )
  WHILE NIP  SWAP 10 * +  REPEAT DROP ;

: ESC[M ( u*i -- ) ( display attributes ) ( console-gui.f )
   0 =IF  0 THEN ( ALLATTOFF )
  30 =IF  0 THEN ( Black )
  31 =IF  2 THEN ( Red )
  32 =IF  7 THEN ( Green )
  33 =IF  6 THEN ( Yellow )
  34 =IF  3 THEN ( Blue )
  35 =IF  5 THEN ( Magenta )
  36 =IF  4 THEN ( Cyan )
  37 =IF 31 THEN ( White )
   8 =IF 31 THEN ( CONCEALED )
  DUP #COLORS 0 WITHIN IF DROP 31 THEN ATTRIBUTE ;

: ANSI? ( u*i c -- u*j f )
  [CHAR] H =IF SWAP AT-XY        -1 EXIT THEN ( Home )
  [CHAR] ; =IF 0                  0 EXIT THEN ( more )
  [CHAR] J =IF 2 = IF PAGE THEN  -1 EXIT THEN ( clear )
  [CHAR] m =IF ESC[M             -1 EXIT THEN ( attributes )
  THROW ; ( unknown char )

: _ANSI ( -- ) 0 BEGIN MORE-DIGITS ANSI? UNTIL ;

: HOST_EMIT ( f c -- f ) ( listen for commands )
  ESC_KEY OVER = ( start ansi escape ? )
  IF SWAP IF DUP EMIT THEN  EXIT
  THEN OVER ( previous escape ? )
  IF [CHAR] [ OVER = ( really ansi escape ? )
    IF 2DROP  ['] _ANSI CATCH DROP  0 EXIT
    THEN >R  EMIT  0  R>
  THEN DUP EMIT  BL = IF ?LF THEN ;

[else] \ Linux
: host_emit ( c -- ) emit ;

[then]

\ ==============================================================
\ start terminal

n,8,1 ( default )

: talk/ ( -- ) -com normal ;
: /talk ( -- ) talk/ +com ;

: T ( -- ) ( start terminal )
  calibrate ( chardelay delay ~100us )
  base @ decimal  .stat cr  /talk
  0 ( ansi terminal escape sequence flag )
  begin
    begin  1 ms
      begin siokey?
      while siokey host_emit
      repeat  key?
    until  0  key sio_emit
  until drop  talk/  cr ." SwiftForth "  base ! ;

\ ==============================================================


