\ Restore.f - Pulled out of OkadWork.cf blocks

0 value cfca \ address of compressed allocation
0 value ebx
0 value ecx

: 2*d ( n -- n )   dup 32 ecx - rshift  ebx ecx lshift  + to ebx ;
: 2*c ( n -- n' )   ecx lshift ;

create [na] 26 , \ bits remaining in source word
create [nb] -6 , \ bits remaining in ebx
create [h] 67510272 , \ destination address
create [an] 0 ,
create [aa] 67977026 ,
create [nz] 4 ,

: new ( 32-bits in current word )   [aa] @ @ [an] !  1 cells [aa] +!  32 [na] ! ;
: ?new ( fetch new word if necessary )   [na] @ 0= if  new  then ;
: shift ( n -- n ) ( into ebx, decrement nb )
	dup negate dup [nb] +!  [na] +!  to ecx
	[an] @ 2*d 2*c [an] ! ;
: bits ( n -- ) ( shift bits into ebx. overflow into next word )
	?new dup negate [na] @ +  dup 0< if
		dup >r + shift new r> negate shift
	else  drop shift  then ;

: h, ( n -- ) ( store at destination )   [h] @ !  1 cells [h] +! ;
: tbits ( n n -- ) ( fill ebx with tag )   [nb] @ 8 + to ecx  2*c or h, ;

: tz ( n n -- n ? )   over [nz] !  dup negate >r + ebx
	r> 0 do  dup 1 and if
			2drop  unloop  [nz] @ 0 exit
		then  2/
	loop  to ebx dup [nz] @ invert + invert [nb] +!  1 ;

: ?full ( n -- n ) ( is there room in ebx? )
	[nb] @ dup and dup 0< if
		tz if  exit  then
		dup >r  4 - [nb] +!  tbits
		0 dup r> dup invert 29 + [nb] !
	else  drop  then ;

: chr ( -- n 1 | 0 ) \ examine high bits; shift 4, 5 or 7 bits
	0 to ebx ( ?new )  4 bits ebx 8 and if
		ebx 4 and if
			3 bits 7 1 exit
		then  1 bits 5 1 exit
	then  4 ebx 15 and if  1 exit
	then  drop 0 ;
: chrs ( n -- n ) \ shift characters until 0
	chr if  ?full to ecx  2*c ebx or recurse  then ;
: wrd ( n -- ) \ shift characters, then tag
	28 [nb] !  dup chrs tbits ;

: t, ( -- )   -4 [nb] !  ebx tbits ;
: short ( n -- ) ( 28-bit value+tag )   28 bits t, ;
: 32bits ( -- ) ( for values )   16 bits  16 bits  ebx h, ;
: litral ( n -- ) \  1-bit base base, tag. value in next word
	0 to ebx  1 bits t,  32bits ;
: var ( n -- ) ( word, value )   wrd 32bits ;

: tag ( -- n 1 | 0 ) \ vector
	ebx 15 and dup case
		0 of drop 0 exit endof  1 of wrd endof  2 of litral endof
		3 of wrd endof  4 of wrd endof  5 of litral endof
		6 of short endof  7 of wrd endof  8 of short endof
		9 of wrd endof  10 of wrd endof  11 of wrd endof
		12 of var endof  13 of short endof
		14 of wrd endof  15 of short endof
	endcase  1 ;

: wrds ( ?new -- ) \ examine tags
	4 bits tag if  recurse  then ;

: eraseBlks ( b n -- )   >r cfblock r> blocks erase ;

: range ( a n n -- ) \ process each block
	over cfblock [h] !  dup >r eraseBlks  [aa] !  0 [na] !
	r> 0 do  wrds
		[h] @ cfbase - 1024 + -1024 and cfbase + [h] !
	loop ;

: restore ( -- ) \ restore compressed blocks
	[ns] @ 0< if
		36 cfblock  cfc 36 blocks - dup allocate throw  to cfca
		cfca swap move  cfca 36 [nblk] @ over - range
		cfca free throw
	then ;
