\ basic.fs	basic words
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

\ variables for compile-time data stacks
$20 cells constant st-size
st-size 2/ constant st-size-half

\ functions for handling the compile-time data stacks
: #stack@ ( n st -- x )
    dup >r 2@
    rot + tuck <= if
	dup cell+ r@ cell+ !
    endif
    r> 2cells + + @ ;

: #stack! ( x n st -- )
    dup @ + + 2cells + ! ;

: >stack ( x st -- ) ( ST: -- x )
    -1cells over +!
    0 swap #stack! ;

: stack> ( st -- x ) ( ST: x -- )
    0 over #stack@
    over 0 0 rot #stack!
    1cells rot +! ;

?trace $0fff [IF]
: .stack ( -- )
    dup @ 2 rshift st-size-half - 0 .r ." > "
    dup st-size 2 cells + dump
    dup 2@ st-size-half - swap st-size-half - swap ?do
	i over #stack@ hex.
    cell +loop
    drop ;
[THEN]

\ variables for compile-time data stacks
create ds-tos 0 , 0 ,
here st-size allot constant ds-data
create ds-init st-size-half allot

\ functions for handling the compile-time data stacks
: >data ( x -- ) ( D: -- x )
    ds-tos >stack ;

: data> ( -- x ) ( D: x -- )
    ds-tos stack> ;

?trace $0fff [IF]
: .ds ( -- )
    ." <D:" ds-tos .stack ;
[THEN]

\ variables for compile-time return stack
create rs-tos 0 , 0 ,
here st-size allot constant rs-data
create rs-init st-size-half allot

\ functions for handling the compile-time return stack
: >return ( x -- ) ( R: -- x )
    rs-tos >stack ;

: return> ( -- x ) ( R: x -- )
    rs-tos stack> ;

?trace $0fff [IF]
: .rs ( -- )
    ." <R:" rs-tos .stack ;
[THEN]

\ variables for local controll stack
: cs-dsize ( n -- n )
    3 * ;
: cs-dget ( addr -- n n n )
    dup @
    swap cell+ dup @
    swap cell+ @ ;
: cs-dput ( n n n addr -- )
    tuck 2cells + !
    tuck cell+ !
    ! ;
$20 constant cs-size
variable cs-tos
cs-size cs-dsize create cs-data cells allot
\ cs-size cs-dsize array cs-data

cs-size 1- cs-tos !

\ functions for handling the local control stack
: #control@ ( n -- x x x )
    cs-tos @ + cs-dsize cells cs-data +
    cs-dget ;

: #control! ( x x x n -- )
    cs-tos @ + cs-dsize cells cs-data +
    cs-dput ;

: >control ( x x x -- ) ( C: -- x x x )
    0 #control!
    -1 cs-tos +! ;

: control@ ( -- x x x ) ( C: x x x -- x x x )
    0 #control@ ;

: control> ( -- x x x ) ( C: x x x -- )
    1 cs-tos +!
    0 #control@
    0 0 0 0 #control! ;

: cs-depth ( -- n )
    cs-size cs-tos @ - 1- ;

?trace $0fff [IF]
: .cs ( -- )
    ." <C:" cs-depth 0 .r ." > "
    cs-depth dup 0 ?do
	dup i - #control@ hex. hex. hex.
    loop
    drop ;
[THEN]

       variable basic-block

       variable basic-code-ptr
       variable basic-code-sav
$20000 constant basic-code
here basic-code allot basic-code-ptr !
       variable basic-data-ptr
       variable basic-data-sav
$10000 constant basic-data
here basic-data allot basic-data-ptr !

?trace $0004 [IF]
    ." BASIC-CODE:" basic-code-ptr hex.
    ." BASIC-CODE(end):" basic-code-ptr basic-code + hex. cr
    ." BASIC-DATA:" basic-data-ptr hex.
    ." BASIC-DATA(end):" basic-data-ptr basic-data + hex. cr
[THEN]

variable inst-!-list
\ contains the last !
variable inst-@-list
\ contains the last ! and all @ since the last !
variable inst-ds!-list
\ contains all stores to the data stack (and all loads)
variable inst-rs!-list
\ contains all stores to the return stack (and all loads)

variable ds-stackupdate
variable rs-stackupdate

include inst-selection.fs
include register.fs
include inst-scheduling.fs

: (il-reset) ( il -- )
    NIL over il-depends !
    il-nt-insts cell+ max-NT 1- cells erase ;

: il-reset ( il -- )
    dup il-left @ ?dup if
	recurse
    endif
    dup il-right @ ?dup if
	recurse
    endif
    (il-reset) ;

: data-init-stack ( addr -- )
    \ assign the top of stack elements to a register
    #tos tos-#register + #tos ?do
	i register-terminal over !
	cell+
    loop
    st-size-half tos-#register cells ?do
	i #sp id@ over !
	cell+
    cell +loop
    drop ;

: data-init ( -- )
    NIL inst-ds!-list !
    NIL ds-stackupdate !
    ds-init data-init-stack
    ds-init st-size-half dup ds-data + swap move
    ds-data st-size-half erase
    st-size-half
    tos-#register cells over + swap ds-tos 2! ;
data-init

: data-reset-stack ( addr -- )
    \ assign the top of stack elements to a register
    tos-#register 0 ?do
	dup @ dup il-reset regs-unallocated swap il-reg !
	cell+
    loop
    ds-tos cell+ @ st-size-half - 2 rshift tos-#register ?do
	dup @ dup il-reset regs-unallocated swap il-reg !
	cell+
    loop
    drop ;

: data-reset ( -- )
    NIL inst-ds!-list !
    NIL ds-stackupdate !
    ds-init data-reset-stack
    ds-init st-size-half ds-data + ds-tos cell+ @ st-size-half - move
    st-size-half
    tos-#register cells over + swap ds-tos 2! ;

: return-init-stack ( addr -- )
    st-size-half 0 ?do
	i #rp id@ over !
	cell+
    cell +loop
    drop ;

: return-init ( -- )
    NIL inst-rs!-list !
    NIL rs-stackupdate !
    rs-init return-init-stack
    rs-init st-size-half dup rs-data + swap move
    rs-data st-size-half erase
    st-size-half dup rs-tos 2! ;
return-init

: return-reset-stack ( addr -- )
    rs-tos cell+ @ st-size-half - 2 rshift 0 ?do
	dup @ dup il-reset regs-unallocated swap il-reg !
	cell+
    loop
    drop ;

: return-reset ( -- )
    NIL inst-rs!-list !
    NIL rs-stackupdate !
    rs-init return-reset-stack
    rs-init st-size-half rs-data + rs-tos cell+ @ st-size-half - move
    st-size-half dup rs-tos 2! ;

: control-init ( -- )
    cs-data cs-size cells erase ;
control-init

\ initial a basic block
: basic-init ( -- )
    ?trace $0004 [IF]
	." BASIC-INIT{ " basic-block ? here hex. cr
	1 basic-block +!
    [THEN]
    \ regs-reset
    here basic-data-sav !
    basic-data-ptr @ dp !
    ?trace $0004 [IF]
	." BASIC-INIT{ " here hex. cr
    [THEN]
    inst-init
    \ initialize the data stack
    data-reset
    \ initialize the temp return stack
    return-reset ;

: (basic-stackupdate) ( val register -- il )
    >r regs-unallocated I_LITS terminal
    0 r@ I_REG terminal
    I_PLUS op r> over il-reg !
    dup inst-ils-insert ;

: basic-stackupdate ( register n -- il )
    ?trace $0008 [IF]
	." stack update:" 2dup . . cr
    [THEN]
    dup if
	cells swap (basic-stackupdate)
    else
	2drop NIL
    endif ;

: basic-datastackdump-regs ( il n -- )
    \ dump the data stackregisters
    #tos tos-#register + #tos ?do
	data>
	dup i #tos - cells ds-init + @ = if
	    drop
	else
	    dup il-op @ I_MOVE = if
		i #tos - cells ds-init + @ il-reg @ regs-unallocated <> if
		    register-move
		endif
	    endif
	    dup il-reg @ regs-unallocated <> if
		register-move
	    endif
	    i over il-reg !
	    dup dup il-depends @ i #tos - cells ds-init + @ inst slist-insert swap il-depends !
	    inst-ils-insert
	endif
    loop ;

: basic-datastackdump-new ( il n -- )
    \ new stackelements
    ?trace $0008 [IF]
	." STACKDUMP (new):" hex.s cr
    [THEN]
    cells #sp id!
    inst-ils-insert ;

: basic-datastackdump-old ( il n -- )
    \ old stackelements (changed)
    ?trace $0008 [IF]
	." STACKDUMP (old):" hex.s cr
    [THEN]
    tuck cells #sp id!
    over tos-#register >= if
	dup il-depends @ rot cells ds-init + @
	inst slist-insert over il-depends !
    else
	nip
    endif
    inst-ils-insert ;

: basic-datastackdump ( -- )
    ds-tos @ st-size-half - 2 rshift >r

    \ dump the data stackregisters
    basic-datastackdump-regs

    \ dump the data stack
    ds-tos 2@ st-size-half - cell / swap st-size-half - cell / swap ?do
	?trace $0008 [IF]
	    ." STACKDUMP (data):" i st-size-half + . hex.s cr
	[THEN]
	data>
	i dup 0< if
	    \ new stackelements
	    basic-datastackdump-new
	else
	    2dup cells ds-init + @ <> if
		\ old stackelements (changed)
		basic-datastackdump-old
	    else
		tos-#register over > if
		    \ top of stack elements (changed)
		    basic-datastackdump-old
		else
		    ?trace $0008 [IF]
			." STACKDUMP (nothing):" hex.s cr
		    [THEN]
		    2drop
		endif
	    endif
	endif
    loop

    \ update the data stackpointer
    #sp r> basic-stackupdate ds-stackupdate ! ;

: basic-returnstackdump ( -- )
    rs-tos @ st-size-half - 2 rshift >r

    \ dump the return stack
    rs-tos 2@ st-size-half - cell / swap st-size-half - cell / swap ?do
	?trace $0008 [IF]
	    ." STACKDUMP (return):" i st-size-half 2 rshift + . hex.s cr
	[THEN]
	return> i cells rs-init + @ over <> if
	    i cells #rp id!
	    i dup 0>= if
		over il-depends @ swap cells rs-init + @ inst slist-insert over il-depends !
	    else
		drop
	    endif
	    inst-ils-insert
	else
	    drop
	endif
    loop

    \ update the return stackpointer
    #rp r> basic-stackupdate rs-stackupdate ! ;

: basic-stackdump ( -- )
    \ dump the data stack
    basic-datastackdump

    \ dump the return stack
    basic-returnstackdump ;

\ exit a basic block and generate the code of the basic block
: basic-exit ( -- )
    ?trace $0004 [IF]
	." BASIC-EXIT} " hex.s .ds cr
    [THEN]
    basic-stackdump

    ?trace $0004 [IF]
	inst-ils-print
	." BASIC-EXIT} " here hex. cr
	." INSTRUCTION SELECTION" hex.s cr
    [THEN]
    inst-ils-end @ if
	inst-selection
    
	?trace $0004 [IF]
	    inst-mls-print
	    ." INSTRUCTION SCHEDULING & REGISTER ALLOCATION" hex.s cr
	[THEN]
	inst-mls-end @ if
	    inst-scheduling

	    ?trace $0004 [IF]
		inst-lists-print
		regs-print
		." CODE EMISSION" hex.s cr
	    [THEN]
	    basic-data-sav @ dp !
	    inst-lists-end @ inst-size 1- <> if
		code-emission
	    endif
	else
	    basic-data-sav @ dp !
	endif
    else
	basic-data-sav @ dp !
    endif ;

: compile-cs-pick ( u -- ) ( C: dest/origu ... dest/orig1 dest/orig0 -- dest/origu ... dest/orig1 dest/orig0 dest/origu )
    #control@ ;

: compile-cs-roll ( u -- ) ( C: dest/origu dest/origu-1 ... dest/orig0 -- dest/origu-1 ... dest/orig0 dest/origu )
    dup 1+ #control@ 2>r >r
    cs-tos @ 1+ dup cs-dsize cells cs-data + swap 1+ cs-dsize cells cs-data + rot cs-dsize cells move
    control> 2drop drop
    r> 2r> >control ;

>target
: cs-pick ( u -- )
    dup compile-cs-pick
    [ also Forth ' lit previous ] literal gforth-compile, ,
    ['] compile-cs-pick gforth-compile, ;

: cs-roll ( u -- )
    dup compile-cs-roll
    [ also Forth ' lit previous ] literal gforth-compile, ,
    ['] compile-cs-roll gforth-compile, ;
>source

include word.fs

?test $0004 [IF]
cr ." Test for basic.fs" cr

finish
[THEN]
