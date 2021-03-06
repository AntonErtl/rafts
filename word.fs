\ word.fs	function words
\
\ Copyright (C) 1995-97 Martin Anton Ertl, Christian Pirker
\
\ This file is part of RAFTS.
\
\	RAFTS is free software; you can redistribute it and/or
\	modify it under the terms of the GNU General Public License
\	as published by the Free Software Foundation; either version 2
\	of the License, or (at your option) any later version.
\
\	This program is distributed in the hope that it will be useful,
\	but WITHOUT ANY WARRANTY; without even the implied warranty of
\	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\	GNU General Public License for more details.
\
\	You should have received a copy of the GNU General Public License
\	along with this program; if not, write to the Free Software
\	Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

: word-regs-read ( xt-addr -- )
    dup ih-regs-out @ swap dup ih-regs-in @ swap ih-regs-flag @
    word-regs-adjust ;

: word-regs-write ( xt-addr -- )
    >r
    word-regs-get
    ?trace $0020 [IF]
	." word regs: " dup . over . >r over . r> cr
    [THEN]
    r> tuck ih-regs-flag !
    tuck ih-regs-in !
    ih-regs-out ! ;

: :compile-gforth ( xt b -- xt xt xt )
    docon: over =
    over dovar: = or
    over douser: = or if
	drop
	['] compile,-constant-gforth
	['] compile,-nonative-xt swap rot
    else
	dofield: = if
	    ['] compile,-field-gforth
	    ['] compile,-nonative-xt swap rot
	else
	    ['] compile,-interpreter
	    ['] compile,-interpreter-xt swap rot
	endif
    endif ;

: :compile ( xt b -- xt xt xt )
    alias-mask and if
	dup forthstart < over >does-code or if
	    ['] compile,-interpreter
	    ['] compile,-interpreter-xt swap rot
	else
	    dup >code-address case
		dodata: of
		dup ih-compile-xt @
		over ih-compiler @
		rot ih-interpreter @ endof
		docode: of
		2cells + @
		dup forthstart < over >does-code or if
		    ['] compile,-interpreter
		    ['] compile,-interpreter-xt swap rot
		else
		    dup >code-address case
			docon: over =
			over dovar: = or
			over douser: = or true of
			drop
			['] compile,-constant-gforth
			['] compile,-nonative-xt swap rot endof
			drop
			dofield: of
			['] compile,-field-gforth
			['] compile,-nonative-xt swap rot endof
			dodefer: over =
			over docol: = or true of
			drop
			['] compile,-interpreter
			['] compile,-interpreter-xt swap rot endof
			drop
			>r ['] compile,-native
			['] compile,-native-xt swap rot r>
		    endcase
		endif
		endof
		dup >r :compile-gforth r>
	    endcase
	endif
    else
	@ alias-mask recurse
    endif ;

: :word ( regs-out regs-in "name" -- )
    header docode: cfa,
    last @ cell+ count $1f and
    find-name ?dup 0= if
	['] noop
    endif
    dup ((name>)) swap cell+ c@
    dup >r :compile
    \ interpreter xt, compiler xt
    a, a,
    >r
    \ # in-register, # out-register
    a, a,
    \ word-flag
    a,
    \ compile count xt
    r> a,
    ih-size 7cells ?do
	0 a,
    cell +loop
    true a,
    reveal
    r> dup
    restrict-mask and if
	compile-only
    endif
    immediate-mask and if
	immediate
    endif ;

>target
also
word-good 0 0 vtarget :word docode:
word-good 0 0 vtarget :word dodata:
word-good 0 0 vtarget :word dodoes:
previous
>source

: word-call ( pfa -- )
    jal,
    nop, ;

: word-native ( cfa -- )
    ?trace $0020 [IF]
	dup rafts->head hex. ." word-native:" dup name. cr
    [THEN]
    2cells + @ word-call ;

: word-interpreter ( cfa -- )
    ?trace $0020 [IF]
	dup rafts->head hex. ." word-interpreter:" dup name. cr
    [THEN]
    tos-#register 0> if
	$0000 #tos tos-#register + 1- #tos tos-store
	swap
    endif
    #cfa swap li,
    ?word-mode-direct [IF]
	#ip here 4cells + tuck li,
	#cfa jr,
    [ELSE]
	@t0 0 #cfa lw,
	#ip here 5cells + tuck li,
	@t0 jr,
    [THEN]
    tos-#register 0> if
	#tos tos-#register + 1- swap #sp sw,
    else
	nop,
    endif
    dup here <> if
	." word-interpreter here difference: " dup hex. here hex. cr
    endif
    dp ! \ make sure we put the xt where it is expected (by the #ip .. li,)
    here cell+ a, \ put an xt here for reentering native code
    ?word-mode-indirect [IF]
	here cell+ a,
    [THEN]
    tos-#register 0> if
	$0000 #tos tos-#register + #tos tos-load drop
	nop,
    endif ;

: check-ra ( -- )
    return>
    dup il-reg @ @ra <> if
	@ra over il-reg ! inst-ils-insert
    else
	drop
    endif ;

: compile-word-init-does ( -- )
    (word-init)
    ?trace $0020 [IF]
	0 basic-block !
    [THEN]
    basic-init
    \ minimise the save of return address !!!
    \ dup
    \ dup ih-#xt @
    \ swap ih-#ixt @ or if
	0 @ra I_REG terminal >return
    \ endif
;

: compile-word-init ( cfa -- )
    here basic-code-sav !
    basic-code-ptr @ dup dp !
    swap 2cells + tuck ! cell flush-icache
    ?trace $0020 [IF]
	0 basic-block !
    [THEN]
    basic-init
    \ minimise the save of return address !!!
    drop
    \ dup ih-#xt @
    \ swap ih-#ixt @ or if
	0 @ra I_REG terminal >return
    \ endif
;

: word-init ( -- )
    docode: cfa,
    (word-init)
    word-regs-init
    native-xt-init
    interpreter-xt-init
    docol: cfa,
    lastih lastcfa ! ;

: compile-word-exit-does ( -- )
    \ minimise the save of return address !!!
    \ dup
    \ dup ih-#xt @
    \ swap ih-#ixt @ or if
	check-ra
    \ endif
    basic-exit
    (word-exit) ;

: compile-word-exit ( cfa -- )
    \ minimise the save of return address !!!
    drop
    \ dup ih-#xt @
    \ swap ih-#ixt @ or if
	check-ra
    \ endif
    basic-exit
    (word-exit)
    basic-code-ptr @ here
    dup basic-code-ptr !
    basic-code-sav @ dp !
    over -
    ?trace $0800 [IF]
	." code: ( " version @ hex. ." ) " lastnfa .name cr
	2dup disasm-dump
    [THEN]
    flush-icache ;

: word-exit ( -- )
    [ also Forth ' ;s previous ] literal gforth-compile, ;

: (:header:) ( "name" -- xt )
    header docode: cfa,
    last @ cell+ count $1f and
    also Forth sfind previous 0= if
	['] noop
    endif ;

: :: ( xt "name" -- )
    (:header:)
    \ interpreter xt, compiler xt
    a, a,
    \ # in-register, # out-register
    a, a,
    word-good a,
    ['] compile,-nonative-xt a,
    ih-size 7cells ?do
        0 a,
    cell +loop
    true a,
    reveal ;

: pass2 ( cfa -- )
    dup ih-status @ 0= if
	?trace $0020 [IF]
	    ." compiling: ( " version @ hex. ." ) " dup name. cr
	[THEN]
	dup ih-xt-addr @
	over ih-#xt @ 0 ?do
	    dup @
	    ?trace $0020 [IF]
		dup look drop (lastnfa) !
	    [THEN]
	    recurse
	    cell+
	loop
	drop
	?trace $0820 [IF]
	    dup look drop (lastnfa) !
	[THEN]
	dup
	dup compile-word-init
	dup ih-cfsize + execute
	dup compile-word-exit
	true over ih-status !
	['] compile,-native over ih-compiler !
	j,-docode: over !
	dup 2cells flush-icache
    endif
    drop ;

>target

: :noname ( -- )
    noname-state on
    lastih-init
    word-init ]
does>
    body> dup pass2
    execute ;

: : ( "name" -- )
    noname-state off
    header
    lastih-init
    lastnfa-init
    word-init ]
does>
    body> dup pass2
    execute ;

: ; ( -- )
    ?trace $0020 [IF]
	hex.s cr
    [THEN]
    postpone [
    word-exit
    lastih word-regs-write
    \ here lastih - ih-cfsize - lastih ih-thread-size !
    ?trace $0020 [IF]
	.native-xt cr
    [THEN]
    lastih
    dup native-xt-write
    interpreter-xt-write
    ?trace $0020 [IF]
	\ Dump vom Info Header
	lastih dup $10 - $20 dump
	dup 2cells disasm-dump 2cells +
	ih-size disasm-dump
	.cs regs-print cr
    [THEN]
    cs-depth abort" unstructured"
    noname-state @ if
	lastxt
    else
	reveal
    endif
    noname-state off
; immediate

>source
: (header) ( regs-out regs-in xt xt "name" -- )
    header
    lastih-init
    dodata: cfa,
    \ interpreter xt-addr, compiler xt-addr,
    a, a,
    \ regs-in, regs-out
    a, a,
    word-good a,
    ['] compile,-nonative-xt a,
    ih-size 7cells ?do
	0 a,
    cell +loop
    true a,
    reveal ;

: (create-head)
    ih-cfsize + ;

: (create) ( xt "name" -- )
    1 0 rot ['] (create-head) (header) ;

: (constant-head)
    ih-cfsize + @ ;

: (constant) ( xt "name" -- )
    1 0 rot ['] (constant-head) (header) ;

: (2constant-head)
    ih-cfsize + 2@ ;

: (2constant) ( xt "name" -- )
    2 0 rot ['] (2constant-head) (header) ;

: (field-head)
    ih-cfsize + @ + ;

: (field) ( xt "name" -- )
    -1 1 rot ['] (field-head) (header) ;

>target

: create ( "name" -- )
    ['] compile,-variable (create) ;

: variable ( "name" -- )
    vtarget create vsource
    0 , ;

: 2variable ( "name" -- )
    vtarget create vsource
    0 , 0 , ;

vtarget ' variable vsource alias user
vtarget ' 2variable vsource alias 2user

: constant ( n "name" -- )
    ['] compile,-constant (constant)
    , ;

: 2constant ( n1 n2 "name" -- )
    ['] compile,-2constant (2constant)
    2, ;

\ : field ( offset1 allign1 size align "name" -- offset2 align2 )
\     ['] compile,-field (field)
\     >r rot r@ nalign dup ,
\     + swap r> nalign ;

: field ( align1 offset1 align size "name" --  align2 offset2 )
    ['] compile,-field (field)
    swap rot over nalign dup , ( align1 size align offset )
    rot + >r nalign r> ;

: end-struct ( offset allign "name" -- )
    vtarget 2constant vsource ;

1 chars 0 vtarget end-struct struct vsource
>source

?test $0020 [IF]
cr ." Test for word.fs" cr

docol: hex.
docon: hex.
dovar: hex.
douser: hex.
dodefer: hex.
dofield: hex.
dodoes: hex.
docode: hex. cr
dodata: hex. cr

finish
[THEN]
