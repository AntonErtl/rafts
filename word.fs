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

docode constant docode:

: word-init-noname ( -- )
    docode cfa,
    (word-init)
    0 , 0 ,
    basic-init
    0 @ra I_REG terminal
    >return ;

: check-ra ( -- )
    return>
    dup il-reg @ @ra <> if
	@ra over il-reg ! inst-btrees-insert
    else
	drop
    endif ;

: word-exit-noname ( -- )
    check-ra
    basic-exit
    (word-exit) ;

: word-init ( -- )
    header
    word-init-noname ;

: word-exit ( -- )
    word-exit-noname
    reveal ;

: word-call ( pfa -- )
    @jal
    @nop ;

: word-native ( cfa -- )
    ?trace $0002 [IF]
	dup >name hex. ." word-native:" dup name. cr
    [THEN]
    2 cells + word-call ;

: word-interpreter ( cfa -- )
    ?trace $0002 [IF]
	dup >name hex. ." word-interpreter:" dup name. cr
    [THEN]
    #cfa swap @li
    ?word-mode-direct [IF]
	#ip here 4 cells + @li drop
    [THEN]
    ?word-mode-indirect [IF]
	#ip here 5 cells + @li drop
	@t0 0 rot @lw
    [THEN]
    @jr
    @nop
    here cell+ a,
    ?word-mode-indirect [IF]
	here cell+ a,
    [THEN] ;

' word-init alias :code
' word-exit alias ;code

variable dostruc
variable noname-state
false noname-state !

>target-compile
: [ ( -- )
    ['Forth] interpreter IS parser
    state off previous previous ; immediate compile-only

>target
: ] ( -- )
    ['Forth] compiler IS parser
    state on target-compile> ;

: :noname ( -- )
    vtarget ] vsource
    true noname-state !
    word-init-noname ;

: : ( "name" -- )
    vtarget ] vsource
    false noname-state !
    word-init ;

>target-compile
: ; ( -- )
    ?trace $0008 [IF]
	hex.s cr
    [THEN]
    noname-state @ if
	word-exit-noname
    else
	word-exit
    endif
    vtarget-compile postpone [ vsource
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
	dup 2 cells + tuck disasm-dump
	\ dup 2 cells + tuck disasm-dump
	\ swap 4 cells - tuck disasm-dump
	swap disasm-dump
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

?test $0008 [IF]
cr ." Test for word.fs" cr

' docode >name &164 dump

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
$1234 word-native
$5678 word-interpreter
word-exit
here 2dup over - dump
swap cell+ dup c@ $1f and + char+ aligned swap disasm-dump

finish
[THEN]
