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

: check-ra ( -- )
    return>
    dup il-reg @ @ra <> if
	@ra over il-reg ! inst-btrees-insert
    else
	drop
    endif ;

: word-init-check ( -- )
    (word-init)
    basic-init
    0 @ra I_REG terminal >return ;

: word-exit-check ( -- )
    check-ra
    basic-exit
    (word-exit) ;

: word-init ( -- )
    header docode: cfa,
    word-init-check ;

: word-exit ( -- )
    word-exit-check
    reveal ;

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
    #cfa swap li,
    ?word-mode-direct [IF]
	#ip here 4cells + li,
	#cfa jr,
    [THEN]
    ?word-mode-indirect [IF]
	#ip here 5cells + li,
	@t0 0 rot lw,
	@t0 jr,
    [THEN]
    nop,
    here cell+ a,
    ?word-mode-indirect [IF]
	here cell+ a,
    [THEN] ;

' word-init alias :code
' word-exit alias ;code

: comp, ( xt cfa -- )
    3cells - ! ;

: (::) ( "name" -- )
    header docode: cfa,
    last @ cell+ count $1f and
    also Forth sfind previous 0= if
	['] noop
    endif
    a,
    info-head-size 1cells ?do
        0 a,
    cell +loop
    reveal ;

: :: ( "name" -- )
    (::)
    :noname
    postpone drop ;

: ;; ( -- )
    postpone ; lastcfa @ comp, ; immediate compile-only

: :compile ( xt -- xt xt )
    alias-mask and if
	dup forthstart < if
	    ['] compile,-interpreter swap
	else
	    dup >code-address case
		docon: of
		['] compile,-constant-gforth swap endof
		dovar: of
		['] compile,-variable-gforth swap endof
		douser: of
		['] compile,-user-gforth swap endof
		docol: of
		['] compile,-interpreter swap endof
		docode: of
		dup 2cells + @ dup forthstart < if
		    nip ['] compile,-interpreter swap
		else
		    over 2cells + info-head-size + over = if
			nip ['] compile-native swap
		    else
			nip alias-mask recurse
		    endif
		endif
		endof

		dodata: of
		['] compile,-interpreter swap endof
		>r ['] compile,-interpreter swap r>
	    endcase
	endif
    else
	dup forthstart < if
	    ['] compile,-interpreter swap
	else
	    @ alias-mask recurse
	endif
    endif ;

: :word ( "name" -- )
    header docode: cfa,
    last @ cell+ count $1f and
    find-name ?dup 0= if
	['] noop
    endif
    dup ((name>)) swap cell+ c@
    dup >r :compile
    a, a,
    info-head-size 2cells ?do
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

>target
also vtarget :word docode: previous
also vtarget :word dodata: previous
also vtarget :word dodoes: previous

: :noname ( -- )
    true noname-state !
    docode: cfa,
    word-init-check
    ] ;

: : ( "name" -- )
    false noname-state !
    word-init
    ] ;

: ; ( -- )
    ?trace $0008 [IF]
	hex.s cr
    [THEN]
    postpone [
    noname-state @ if
	word-exit-check
    else
	word-exit
    endif
    ?trace $0800 [IF]
	noname-state @ 0<> if
	    lastcfa @
	else
	    last @
	endif
	\ Hexdump vom generierten Maschinencode
	here 2dup over - hex.s dump
	noname-state @ 0= if
	    swap name>int swap
	endif
	\ disassemblierter Dump vom generierten Maschienencode
	hex.s swap
	dup 2cells disasm-dump 2cells + swap
	over - disasm-dump
	.cs cr
	regs-print
    [THEN]
    cs-depth 0<> abort" unstructured"
    noname-state @ 0<> if
	lastcfa @
    else
	last @
    endif
    here over - flush-icache
    noname-state @ 0<> if
	lastcfa @
    endif ; immediate compile-only

>source
: (header) ( xt xt "name" -- )
    header dodata: cfa,
    a, a,
    info-head-size 2cells ?do
	0 a,
    cell +loop
    reveal ;

: (create-head)
     [ 2cells info-head-size + ] literal + ;

: (create) ( xt "name" -- )
    ['] (create-head) (header) ;

: (constant-head)
    [ 2cells info-head-size + ] literal + @ ;

: (constant) ( xt "name" -- )
    ['] (constant-head) (header) ;

: (2constant-head)
    [ 2cells info-head-size + ] literal + 2@ ;

: (2constant) ( xt "name" -- )
    ['] (2constant-head) (header) ;

: (defer-head)
    [ 2cells info-head-size + ] literal + @ execute ;

: (defer) ( xt "name" -- )
    ['] (defer-head) (header) ;

: (field-head)
    [ 2cells info-head-size + ] literal + @ + ;

: (field) ( xt "name" -- )
    ['] (field-head) (header) ;

>target

: create ( "name" -- )
    ['] compile,-variable (create) ;

: variable ( "name" -- )
    vtarget create vsource
    0 , ;

: 2variable ( "name" -- )
    vtarget create vsource
    0 , 0 , ;

: user ( "name" -- )
    ['] compile,-user (create)
    0 , ;

: 2user ( "name" -- )
    ['] compile,-user (create)
    0 , 0 , ;

\ vtarget ' variable vsource alias user
\ vtarget ' 2variable vsource alias 2user

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

' docode: >name &164 dump

docol: hex.
docon: hex.
dovar: hex.
douser: hex.
dodefer: hex.
dofield: hex.
dodoes: hex.
docode: hex. cr

here
word-init foo
\ $1234 word-native
\ $5678 word-interpreter
word-exit
here 2dup over - dump
swap cell+ dup c@ $1f and + char+ aligned swap over - disasm-dump

finish
[THEN]
