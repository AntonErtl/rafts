\ word.fs	function words
\
\ Copyright (C) 1995-96 Martin Anton Ertl, Christian Pirker
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
    ?trace $0800 [IF]
	." word regs: " dup . over . >r over . r> cr
    [THEN]
    r> tuck ih-regs-flag !
    tuck ih-regs-in !
    ih-regs-out ! ;

: :compile ( xt b -- xt xt )
    alias-mask and if
	dup forthstart < over >does-code 0<> or if
	    ['] compile,-interpreter swap
	else
	    dup >code-address case
		docon: of
		['] compile,-constant-gforth swap endof
		dovar: of
		['] compile,-variable-gforth swap endof
		douser: of
		['] compile,-user-gforth swap endof
		dofield: of
		['] compile,-field-gforth swap endof
		dodefer: of
		['] compile,-defer-gforth swap endof
		docol: of
		['] compile,-interpreter swap endof
		docode: of
		2cells + @
		dup forthstart < over >does-code 0<> or if
		    ['] compile,-interpreter swap
		else
		    dup >code-address case
			docon: of
			['] compile,-constant-gforth swap endof
			dovar: of
			['] compile,-variable-gforth swap endof
			douser: of
			['] compile,-user-gforth swap endof
			dofield: of
			['] compile,-field-gforth swap endof
			dodefer: of
			['] compile,-defer-gforth swap endof
			docol: of
			['] compile,-interpreter swap endof
			>r ['] compile,-native swap r>
		    endcase
		endif
		endof

		dodata: of
		['] compile,-interpreter swap endof
		>r ['] compile,-interpreter swap r>
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
    \ # in-register, # out-register
    a, a,
    \ word-flag
    a,
    ih-size 5cells ?do
	0 a,
    cell +loop
    reveal
    r> dup
    restrict-mask and if
	compile-only
    endif
    immediate-mask and if
	immediate
    endif ;

: word-call ( pfa -- )
    jal,
    nop, ;

: word-native ( cfa -- )
    ?trace $0002 [IF]
	dup >name hex. ." word-native:" dup name. cr
    [THEN]
    2cells + @ word-call ;

: word-interpreter ( cfa -- )
    ?trace $0002 [IF]
	dup >name hex. ." word-interpreter:" dup name. cr
    [THEN]
    #tos $0000 #sp sw,
    #sos $0004 #sp sw,
    #cfa swap li,
    ?word-mode-direct [IF]
	#ip here 4cells + li,
	#cfa jr,
    [ELSE]
	#ip here 5cells + li,
	@t0 0 rot lw,
	@t0 jr,
    [THEN]
    nop,
    here cell+ a,
    ?word-mode-indirect [IF]
	here cell+ a,
    [THEN]
    #tos $0000 #sp lw,
    #sos $0004 #sp lw,
    nop,
;

: check-ra ( -- )
    return>
    dup il-reg @ @ra <> if
	@ra over il-reg ! inst-btrees-insert
    else
	drop
    endif ;

: compile,-word-init-check ( -- )
    (word-init)
    basic-init
    0 @ra I_REG terminal >return ;

: compile,-word-init ( -- )
    here basic-code-sav !
    basic-code-ptr @ dup dp !
    swap 2cells + !
    basic-init
    0 @ra I_REG terminal >return ;

: word-init ( -- )
    here
    docode: cfa,
    (word-init)
    word-regs-init
    docol: cfa,
    [ also Forth ' lit previous ] literal gforth-compile, ,
    ['] compile,-word-init gforth-compile, ;

: compile,-word-exit-check ( -- )
    check-ra
    basic-exit
    (word-exit) ;

: compile,-word-exit ( -- )
    check-ra
    basic-exit
    (word-exit)
    basic-code-ptr @ here
    dup basic-code-ptr !
    basic-code-sav @ dp !
    over -
    ?trace $0800 [IF]
	2dup disasm-dump
    [THEN]
    flush-icache ;

: word-exit ( -- )
    ['] compile,-word-exit gforth-compile,
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
    ih-size 4cells ?do
        0 a,
    cell +loop
    reveal ;

>target
also
word-good 0 0 vtarget :word docode:
word-good 0 0 vtarget :word dodata:
word-good 0 0 vtarget :word dodoes:
previous

>source
variable (lastih)
: lastih ( -- addr )
    (lastih) @ ;

: set-lastih ( -- )
    noname-state @ if
	lastxt
    else
	last @
	dup ((name>))
    endif ;
>target

: :noname ( -- )
    noname-state on
    here (lastih) !
    word-init
    lastih lastcfa ! ] ;

: : ( "name" -- )
    noname-state off
    header
    here (lastih) !
    word-init ] ;

: ; ( -- )
    ?trace $0800 [IF]
	hex.s cr
    [THEN]
    postpone [
    word-exit
    \ generate the code
    lastih 2cells + @ execute
    lastih word-regs-write
    \ here over - flush-icache
    ?trace $0800 [IF]
	\ Dump vom Info Header
	lastih dup $10 - $20 dump
	dup 2cells disasm-dump 2cells +
	ih-size disasm-dump
	.cs regs-print cr
    [THEN]
    cs-depth 0<> abort" unstructured"
    noname-state @ if
	lastxt
    else
	reveal
    endif ; immediate

>source
: (header) ( regs-out regs-in xt xt "name" -- )
    header
    here (lastih) !
    dodata: cfa,
    \ interpreter xt-addr, compiler xt-addr,
    a, a,
    \ regs-in, regs-out
    a, a,
    ih-size 4cells ?do
	0 a,
    cell +loop
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

: (defer-head)
    \ rdrop
    ih-cfsize + @ execute ;

: (defer) ( xt "name" -- )
    0 0 rot ['] (defer-head) (header) ;

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

: defer ( "name" -- )
    ['] compile,-defer (defer)
    ['] noop , ;

: field ( offset1 allign1 size align "name" -- offset2 align2 )
    ['] compile,-field (field)
    >r rot r@ nalign dup ,
    + swap r> nalign ;

: end-struct ( offset allign "name" -- )
    vtarget 2constant vsource ;

0 1 chars vtarget end-struct struct vsource
>source

?test $0008 [IF]
cr ." Test for word.fs" cr

docol: hex.
docon: hex.
dovar: hex.
douser: hex.
dodefer: hex.
dofield: hex.
dodoes: hex.
docode: hex. cr

finish
[THEN]
