base @ hex
." START: " here . cr
decimal

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

: replace-word ( xt cfa -- )
    \ replace word at cfa with xt. !! This is quite general-purpose
    \ and should migrate elsewhere.
    dodefer: over code-address!
    >body ! ;

include options.fs
include stdlib/stdlib.fs

: vForth ( -- )
    Forth ; immediate

: vsource ( -- )
    voc-source ; immediate
: >source ( -- )
    also voc-source definitions previous ;
: source> ( -- )
    voc-source also ;

: vtarget ( -- )
    voc-target ; immediate
: >target ( -- )
    also voc-target definitions previous ;
: target> ( -- )
    voc-target also ;

: vtarget-compile ( -- )
    voc-target-compile ; immediate
: >target-compile ( -- )
    also voc-target-compile definitions previous ;
: target-compile> ( -- )
    also voc-target-compile also ;

: comp'Forth ( "name" -- )
    postpone vForth comp' postpone vsource drop ; immediate
: 'Forth ( "name" -- )
    postpone vForth ' postpone vsource ; immediate
: ['Forth] ( "name" -- )
    postpone vForth postpone ['] postpone vsource ; immediate compile-only

include asm.fs
include disasm.fs
include basic.fs

: compile,-constant ( xt -- )
    ?trace $0001 [IF]
	." constant: " dup name. hex.s cr
    [THEN]
    execute vtarget-compile postpone literal vsource ;

: compile,-variable ( xt -- )
    ?trace $0001 [IF]
	." variable: " dup name. hex.s cr
    [THEN]
    execute vtarget-compile postpone literal vsource ;

: compile,-user ( xt -- )
    ?trace $0001 [IF]
	." user: " dup name. hex.s cr
    [THEN]
    execute vtarget-compile postpone literal vsource ;

: compile,-struct ( xt -- )
    ?trace $0001 [IF]
	." func-struc: " dup name. hex.s cr
    [THEN]
    2cell + @ vtarget-compile postpone literal vsource dostruc @
    execute ;

: compile,-interpreter ( xt -- )
    ?trace $0001 [IF]
	." func-interpreter: " dup name. hex.s cr
    [THEN]
    basic-exit
    word-interpreter
    basic-init ;

: compile,-forth ( xt -- )
    ?trace $0001 [IF]
	." func-interpreter (forth): " dup name. hex.s cr
    [THEN]
    basic-exit
    word-interpreter
    basic-init ;

: compile,-defer ( xt -- )
    ?trace $0001 [IF]
	." func-defer: " dup name. hex.s cr
    [THEN]
    basic-exit
    word-interpreter
    basic-init ;

: compile,-native ( xt -- )
    ?trace $0001 [IF]
	." func-native: " dup name. hex.s cr
    [THEN]
    basic-exit
    word-native
    basic-init ;

: compile,-native-does ( xt ca -- )
    ?trace $0001 [IF]
	." func-native (does>): " hex.s cr
    [THEN]
    swap 2cell + vtarget-compile postpone literal vsource basic-exit
    word-native
    basic-init ;

: compile,-interpreter-does ( xt ca -- )
    drop
    ?trace $0001 [IF]
	." func-interpreter (does>): " hex.s cr
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

include primitives.fs
include control.fs

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
base !
\ order .s cr

' compile, 'Forth compile, vtarget replace-word
vtarget-compile comp' literal vtarget drop comp'Forth literal replace-word
