." START: " here hex . decimal cr

\ compiler.fs	compiler main load file
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

vocabulary voc-source
\ basically the host-residing words, but may be called from target words
vocabulary voc-target
\ all words compiled by the target, and a few more
vocabulary voc-target-compile
\ compilation semantics of "primitives" and other words visible in "compile state"

voc-source also definitions

include options.fs
include stdlib/stdlib.fs

: vForth ( -- )
    Forth ; restrict immediate

: vsource ( -- )
    voc-source ; restrict immediate
: >source ( -- )
    also voc-source definitions previous ;
: source> ( -- )
    voc-source also ;

: vtarget ( -- )
    voc-target ; restrict immediate
: >target ( -- )
    also voc-target definitions previous ;
: target> ( -- )
    voc-target also ;

: vtarget-compile ( -- )
    voc-target-compile ; restrict immediate
: >target-compile ( -- )
    also voc-target-compile definitions previous ;
: target-compile> ( -- )
    also voc-target-compile also ;

include asm.fs
include disasm.fs
include basic.fs

: interpreter-default ( c-addr u xt -- )
    nip nip
    ?trace $0001 [IF]
	." interpreter:" dup name.
	dup hex. dup >code-address hex. cr
    [THEN]
    execute ;

: interpreter ( c-addr u -- )
    ?trace $0002 [IF]
	." INTER:" 2dup type cr
    [THEN]
    2dup sfind
    ?trace $0001 [IF]
	order cr
	hex.s cr
    [THEN]
    1 and if
	interpreter-default
    else
	snumber? 0= if
	    interpreter-notfound
	endif
    endif ;

: compile,-constant ( xt -- )
    ?trace $0001 [IF]
	." constant:" hex.s
    [THEN]
    execute vtarget-compile postpone literal vsource ;

: compile,-variable ( xt -- )
    ?trace $0001 [IF]
	." variable:" hex.s cr
    [THEN]
    execute vtarget-compile postpone literal vsource ;

: compile,-user ( xt -- )
    ?trace $0001 [IF]
	." user:" hex.s cr
    [THEN]
    execute vtarget-compile postpone literal vsource ;

: compile,-interpreter ( xt -- )
    ?trace $0001 [IF]
	." FUNC-INTERPRETER:" hex.s cr
    [THEN]
    basic-exit
    word-interpreter
    basic-init ;

: compile,-forth ( xt -- )
    ?trace $0001 [IF]
	." FUNC-INTERPRETER (FORTH):" hex.s cr
    [THEN]
    basic-exit
    word-interpreter
    basic-init ;

: compile,-defer ( xt -- )
    ?trace $0001 [IF]
	." FUNC-DEFER:" hex.s cr
    [THEN]
    basic-exit
    word-interpreter
    basic-init ;

: compile,-struct ( xt -- )
    ?trace $0001 [IF]
	." FUNC-STRUC:" hex.s cr
    [THEN]
    2 cells + @ vtarget-compile postpone literal vsource dostruc @
    execute ;

: compile,-native ( xt -- )
    ?trace $0001 [IF]
	." FUNC-NATIVE:" hex.s cr
    [THEN]
    basic-exit
    word-native
    basic-init ;

: compile,-native-does ( xt ca -- )
    ?trace $0001 [IF]
	." FUNC-NATIVE (DOES>):" hex.s cr
    [THEN]
    swap 2 cells + vtarget-compile postpone literal vsource basic-exit
    word-native
    basic-init ;

: compile,-interpreter-does ( xt ca -- )
    drop
    ?trace $0001 [IF]
	." FUNC-INTERPRETER (DOES>):" hex.s cr
    [THEN]
    basic-exit
    word-interpreter
    basic-init ;

: compile,-does ( xt ca -- )
    over >does-code 0= if \ !! defaults to native-code does> handler, interpreter would be better
	compile,-native-does
    else
	compile,-interpreter-does
    endif ;

: compile, ( xt -- )
    dup forthstart u> if
	dup >code-address case
	    docon: of
	    compile,-constant endof
	    dovar: of
	    compile,-variable endof
	    douser: of
	    compile,-user endof
	    dofield: of
	    compile,-struct endof
	    dodefer: of
	    compile,-defer endof
	    docol: of
	    compile,-interpreter endof
	    docode: of
	    compile,-native endof
	    dup >r compile,-does r>
	endcase
    else
	compile,-forth
    endif ;

: compiler-rest&imm ( c-addr u xt -- )
    nip nip
    ?trace $0001 [IF]
	." compiler (restrict&immediate):" dup name.
	dup hex. dup >code-address hex. cr
    [THEN]
    execute ;

: compiler-rest ( c-addr u xt -- )
    nip nip
    ?trace $0001 [IF]
	." compiler (restrict):" dup name.
	dup hex. dup >code-address hex. cr
    [THEN]
    compile, ;

: compiler-imm ( c-addr u xt -- )
    nip nip
    ?trace $0001 [IF]
	." compiler (immediate):" dup name.
	dup hex. dup >code-address hex. cr
    [THEN]
    execute ;

: compiler-default ( c-addr u xt -- )
    nip nip
    ?trace $0001 [IF]
	." compiler:" dup name.
	dup hex. dup >code-address hex. cr
    [THEN]
    compile, ;

: compiler-number ( n -- )
    ?trace $0001 [IF]
	." number:" dup hex. cr
    [THEN]
    vtarget-compile postpone literal vsource ;

: compiler ( c-addr u -- )
    ?trace $0002 [IF]
	." COMP:" 2dup type cr
    [THEN]
    2dup sfind
    ?trace $0001 [IF]
	order cr hex.s cr
    [THEN]
    case
	2 of
	compiler-rest&imm endof
	-2 of
	compiler-rest endof
	1 of
	compiler-imm endof
	-1 of
	compiler-default endof
	0 of
	snumber? 0<> if
	    compiler-number
	else
	    compiler-notfound
	endif
	endof
    endcase ;

: interpret ( -- )
    begin
	?stack
	name dup 0<>
    while
	state @ 0= if
	    interpreter
	else
	    compiler
	endif
    repeat
    2drop ;

: printok ( -- )
    ."  o"
    ?trace $0001 [IF]
	hex.s
    [THEN]
    ." k" ;

: printcompile ( -- )
    ."  com"
    ?trace $0001 [IF]
	hex.s
    [THEN]
    ." piled" ;

include primitives.fs
include control.fs
include primitives-ext.fs

: quit ( -- )
    loadfile off blk off
    begin
	cr refill
    while
	interpret
	state @ 0= if
	    printok
	else
	    printcompile
	endif
    repeat ;

: cold ( -- )
    \ 2, because compiler.fs is 1 in testmod !!!
    argc @ 2 > if
	begin
	    argc @ 2 >
	while
	    2 arg
	    -1 argc +!
	    1 cells argv +!
	    included
	repeat
    endif
    quit ;

>target
: bye ( -- )
    ?trace $8000 [IF]
	finish
	regs-print
	text-print
	order cr
	\ ['] Root list
	\ ['] Forth list
	['] voc-source list
	['] voc-target list
	['] voc-target-compile list
    [THEN]
    finish
    ?trace $4000 [IF]
	depth 0 ?do
	    . loop
	cr
    [THEN]
    bye ;

?test $0001 [IF]
cr ." Test for compiler.fs" cr

finish
[THEN]

\ ." START AGAIN: " here hex . decimal cr

target>
\ order .s cr
cold
