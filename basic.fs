\ basic.fs	basic words
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

\ variables for compile-time data stack
$20 constant ds-size
ds-size 2/ constant ds-tosstart
variable ds-tos
\ compile-time data stack pointer
ds-size array ds-data
ds-size 2/ array ds-init

\ functions for handling the compile-time data stack
: #data@ ( n -- x )
    ds-tos @ + ds-data @ ;

: #data! ( x n -- )
    ds-tos @ + ds-data ! ;

: >data ( x -- ) ( D: -- x )
    -1 ds-tos +!
    0 #data! ;

: data> ( -- x ) ( D: x -- )
    0 #data@
    1 ds-tos +! ;

: .ds ( -- )
    ." <D:" ds-tos @ 0 .r ." > "
    ds-tos @ dup 0 ?do
	dup i - #data@ hex.
    loop
    drop ;

\ variables for compile-time return stack
$20 constant rs-size
rs-size 2/ constant rs-torstart
variable rs-tor
\ compile-time return stack pointer
rs-size array rs-data
rs-size 2/ array rs-init

\ functions for handling the compile-time return stack
: #return@ ( n -- x )
    rs-tor @ + rs-data @ ;

: #return! ( x n -- )
    rs-tor @ + rs-data ! ;

: >return ( x -- ) ( R: -- x )
    -1 rs-tor +!
    0 #return! ;

: return> ( -- x ) ( R: x -- )
    0 #return@
    1 rs-tor +! ;

: .rs ( -- )
    ." <R:" rs-tor @ 0 .r ." > "
    rs-tor @ dup 0 ?do
	dup i - #return@ hex.
    loop
    drop ;

\ variables for local controll stack
$20 constant cs-size
variable cs-tos
cs-size array cs-data

cs-size 1- cs-tos !

\ functions for handling the local control stack
: #control@ ( n -- x )
    cs-tos @ + cs-data @ ;

: #control! ( x n -- )
    cs-tos @ + cs-data ! ;

: >control ( x -- ) ( C: -- x )
    0 #control!
    -1 cs-tos +! ;

: control@ ( -- x ) ( C: -- )
    0 #control@ ;

: control> ( -- x ) ( C: x -- )
    1 cs-tos +!
    0 #control@ ;

: cs-depth ( -- n )
    cs-size cs-tos @ - 1- ;

: .cs ( -- )
    ." <C:" cs-depth 0 .r ." > "
    cs-depth dup 0 ?do
	dup i - #control@ hex.
    loop
    drop ;

variable basic-head-ptr
$2000 constant basic-code

variable inst-!-list
\ contains the last !
variable inst-@-list
\ contains the last ! and all @ since the last !
variable inst-s!-list
\ contains all stores to the data stack (and all loads)

include inst-selection.fs
include inst-scheduling.fs

: init-stack ( register addr n -- )
    0 ?do ( register addr )
	i cells 2 pick id@ ( register addr node )
	dup inst inst-s!-list @ slist-insert drop
	over !
	cell+
    loop
    2drop ;

: data-init ( -- )
    #sp 0 ds-init ds-size 2/ init-stack
    0 ds-init ds-size 2/ dup ds-data swap cells move ;

: return-init ( -- )
    #rp 0 rs-init rs-size 2/ init-stack
    0 rs-init rs-size 2/ dup rs-data swap cells move ;

: control-init ( -- )
    0 cs-data cs-size cells NULL fill ;
control-init

\ initial a basic block
: basic-init ( -- )
    ?trace $0020 [IF]
	." basic-init " here hex. cr
    [THEN]
    regs-init
    here basic-head-ptr !
    basic-code allot
    ?trace $0020 [IF]
	." BASIC-INIT{ " here hex. cr
    [THEN]
    inst-init
    NIL inst inst-!-list !
    NIL inst inst-@-list !
    \ initialize the data stack
    ds-tosstart ds-tos !
    NIL inst inst-s!-list !
    data-init
    \ initialize the temp return stack
    rs-torstart rs-tor !
    return-init
    ?trace $0020 [IF]
	." BASIC-INIT " hex.s cr
    [THEN] ;

: (basic-stackupdate) ( val register -- )
    >r regs-unused I_LITS terminal
    0 r@ I_REG terminal
    I_PLUS op inst-s!-list @ over il-depends !
    r> dup regs-inc over il-reg ! inst-btrees-insert-end ;

: basic-stackupdate ( register n -- )
    ?trace $0100 [IF]
	." stack update:" 2dup . . cr
    [THEN]
    dup 0<> if
	cells swap (basic-stackupdate)
    else
	2drop
    endif ;

: basic-stackdump ( -- )
    ds-tos @ ds-tosstart - >r
    ?trace $0100 [IF]
	ds-size 2/ 0 ?do
	    i ds-init @ dup hex.
	    inst-print-node
	loop
	cr
	ds-size 0 ?do
	    i ds-data @ dup hex.
	    ?dup 0<> if
		inst-print-node
	    else
		cr
	    endif
	loop
	cr
	." TOS:" ds-tos @ . cr
    [THEN]
    ds-size ds-tos @ ?do				\ dump the data stack
	?trace $0100 [IF]
	    ." STACKDUMP (data):" i . hex.s cr
	[THEN]
	data> i ds-tosstart - dup 0< if			\ new stackelements
	    ?trace $0100 [IF]
		." STACKDUMP (new):" hex.s cr
	    [THEN]
	    cells #sp id!
	    dup inst inst-s!-list @ slist-insert drop
	    inst-btrees-insert
	else
	    2dup ds-init @ <> if				\ old stackelements (changed)
		?trace $0100 [IF]
		    ." STACKDUMP (old):" hex.s cr
		[THEN]
		tuck cells #sp id!
		dup inst inst-s!-list @ slist-insert drop
		swap ds-init @ inst NULL inst tuck slist-insert drop
		over il-depends !
		inst-btrees-insert
	    else
		?trace $0100 [IF]
		    ." STACKDUMP (nothing):" hex.s cr
		[THEN]
		2drop
	    endif
	endif
    loop
    \ update the data stackpointer
    #sp r> basic-stackupdate
    \ dump the return stack
    rs-size rs-tor @ ?do
	?trace $0100 [IF]
	    ." STACKDUMP (return):" i . hex.s cr
	[THEN]
	i rs-data @ i rs-torstart - rs-init @ over <> if
	    i rs-torstart - cells #rp id!
	    dup inst inst-s!-list @ slist-insert drop
	    i rs-torstart - dup 0>= if
		rs-init @ inst NULL inst tuck slist-insert drop
		over il-depends !
	    else
		drop
	    endif
	    inst-btrees-insert
	else
	    drop
	endif
    loop
    \ update the data stackpointer
    #rp rs-tor @ rs-torstart - basic-stackupdate ;

: basic-print ( -- )
    ." BTREE PRINT" hex.s cr
    inst-btrees-print
    ." NODE PRINT" hex.s cr
    inst-nodes-print
    ." PNODE PRINT" hex.s cr
    inst-pnodes-print
    ." LISTS PRINT" hex.s cr
    inst-lists-print ;

\ exit a basic block and generate the code of the basic block
make-ml
0 over ml-reg !
' asm-nop over  ml-asm !
constant nop-ml \ nop instruction, usable only after scheduling

: basic-exit ( -- )
    ?trace $0020 [IF]
	." BASIC-EXIT " hex.s cr
    [THEN]
    basic-stackdump

    ?trace $0200 [IF]
	basic-print
	." }BASIC-EXIT " here hex. cr
	." INST SELECTION" hex.s cr
    [THEN]
    inst-selection

    translate-all-dependences

    ?trace $0200 [IF]
	basic-print
	." INST SCHEDULING" hex.s cr
    [THEN]
    inst-scheduling

    ?trace $0200 [IF]
	basic-print
	." REGISTER ALLOCATION" hex.s cr
    [THEN]
    register-allocation

    ?trace $0200 [IF]
	basic-print
	regs-print
	." ASSEMBLE" hex.s cr
    [THEN]
    basic-head-ptr @ dp !
    assemble ;

>target-compile
: cs-pick ( u -- ) ( C: dest/origu ... dest/orig1 dest/orig0 -- dest/origu ... dest/orig1 dest/orig0 dest/origu )
    #control@ ;

: cs-roll ( u -- ) ( C: dest/origu dest/origu-1 ... dest/orig0 -- dest/origu-1 ... dest/orig0 dest/origu )
    dup 1+ #control@ swap
    cs-tos @ cs-data cell+ dup cell+ rot cells move
    control> drop >control ;
>source

include word.fs

?test $0004 [IF]
cr ." Test for basic.fs" cr

finish
[THEN]
