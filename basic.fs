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
$40 constant ds-size
ds-size 2/ constant ds-tos-start
variable ds-tos
variable ds-tos-bottom
\ compile-time data stack pointer
ds-size array ds-data
ds-size 2/ array ds-init

\ functions for handling the compile-time data stack
: #data@ ( n -- x )
    ds-tos-bottom @
    ds-tos @ rot + tuck <= if
	dup 1+ ds-tos-bottom !
    endif
    ds-data @ ;

: #data! ( x n -- )
    ds-tos @ + ds-data ! ;

: >data ( x -- ) ( D: -- x )
    -1 ds-tos +!
    0 #data! ;

: data> ( -- x ) ( D: x -- )
    0 #data@
    0 0 #data!
    1 ds-tos +! ;

: .ds ( -- )
    ." <D:" ds-tos @ 0 .r ." > "
    ds-tos @ dup 0 ?do
	dup i - #data@ hex.
    loop
    drop ;

\ variables for compile-time return stack
$20 constant rs-size
rs-size 2/ constant rs-tos-start
variable rs-tos
variable rs-tos-bottom
\ compile-time return stack pointer
rs-size array rs-data
rs-size 2/ array rs-init

\ functions for handling the compile-time return stack
: #return@ ( n -- x )
    rs-tos-bottom @
    rs-tos @ rot + tuck <= if
	dup 1+ rs-tos-bottom !
    endif
    rs-data @

    \ rs-tos @ + rs-data @
;

: #return! ( x n -- )
    rs-tos @ + rs-data ! ;

: >return ( x -- ) ( R: -- x )
    -1 rs-tos +!
    0 #return! ;

: return> ( -- x ) ( R: x -- )
    0 #return@
    0 0 #return!
    1 rs-tos +! ;

: .rs ( -- )
    ." <R:" rs-tos @ 0 .r ." > "
    rs-tos @ dup 0 ?do
	dup i - #return@ hex.
    loop
    drop ;

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
cs-size cs-dsize array cs-data

cs-size 1- cs-tos !

\ functions for handling the local control stack
: #control@ ( n -- x x x )
    cs-tos @ + cs-dsize cs-data
    cs-dget ;

: #control! ( x x x n -- )
    cs-tos @ + cs-dsize cs-data
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

: .cs ( -- )
    ." <C:" cs-depth 0 .r ." > "
    cs-depth dup 0 ?do
	dup i - #control@ hex. hex. hex.
    loop
    drop ;

       variable basic-block

       variable basic-code-ptr
       variable basic-code-sav
$20000 constant basic-code
here basic-code allot basic-code-ptr !
       variable basic-head-ptr
       variable basic-head-sav
$10000 constant basic-head
here basic-head allot basic-head-ptr !

?trace $0020 [IF]
    ." BASIC-CODE:" basic-code-ptr hex.
    ." BASIC-CODE(end):" basic-code-ptr basic-code + hex. cr
    ." BASIC-HEAD:" basic-head-ptr hex.
    ." BASIC-HEAD(end):" basic-head-ptr basic-head + hex. cr
[THEN]

variable inst-!-list
\ contains the last !
variable inst-@-list
\ contains the last ! and all @ since the last !
variable inst-ds!-list
\ contains all stores to the data stack (and all loads)
variable inst-rs!-list
\ contains all stores to the return stack (and all loads)

include inst-selection.fs
include inst-scheduling.fs
include register.fs

: data-init-stack ( register addr -- )
    \ assign the top of stack elements to a register
    #tos tos-#register + #tos ?do
	i register-terminal over !
	cell+
    loop
    ds-tos-bottom @ ds-tos-start - tos-#register ?do ( register addr )
	i cells 2 pick id@ ( register addr node )
	\ dup inst inst-ds!-list @ slist-insert drop
	over !
	cell+
    loop
    2drop ;

: data-init ( -- )
    ds-tos-start ds-tos !
    NIL inst inst-ds!-list !
    #sp 0 ds-init data-init-stack
    0 ds-init ds-size 2/ dup ds-data swap cells move ;
ds-size ds-tos-bottom !
data-init
ds-tos-start tos-#register + ds-tos-bottom !

: return-init-stack ( register addr -- )
    rs-tos-bottom @ rs-tos-start - 0 ?do ( register addr )
	i cells 2 pick id@ ( register addr node )
	\ dup inst inst-rs!-list @ slist-insert drop
	over !
	cell+
    loop
    2drop ;

: return-init ( -- )
    rs-tos-start rs-tos !
    NIL inst inst-rs!-list !
    #rp 0 rs-init return-init-stack
    0 rs-init rs-size 2/ dup rs-data swap cells move ;
rs-size rs-tos-bottom !
return-init
rs-tos-start rs-tos-bottom !

: control-init ( -- )
    0 cs-data cs-size cells NULL fill ;
control-init

\ initial a basic block
: basic-init ( -- )
    ?trace $0020 [IF]
	." BASIC-INIT{ " basic-block ? here hex. cr
	1 basic-block +!
    [THEN]
    \ regs-reset
    here basic-head-sav !
    basic-head-ptr @ dp !
    ?trace $0020 [IF]
	." BASIC-INIT{ " here hex. cr
    [THEN]
    inst-init
    \ initialize the data stack
    data-init
    \ initialize the temp return stack
    return-init ;

: (basic-stackupdate) ( val register -- )
    >r regs-unused I_LITS terminal
    0 r@ I_REG terminal
    r@ #sp = if
	I_PLUS op inst-ds!-list @ over il-depends !
    else
	I_PLUS op inst-rs!-list @ over il-depends !
    endif
    r> over il-reg ! inst-btrees-insert-end ;

: basic-stackupdate ( register n -- )
    ?trace $0100 [IF]
	." stack update:" 2dup . . cr
    [THEN]
    dup if
	cells swap (basic-stackupdate)
    else
	2drop
    endif ;

: basic-datastackdump-print ( -- )
    ds-size 0 ?do
	i ds-size 2/ >= if
	    i ds-size 2/ - ds-init @
	    i ds-data @ tuck = if
		drop
	    else
		?dup if
		    il-print
		endif
	    endif
	else
	    i ds-data @ ?dup if
		il-print
	    endif
	endif
    loop
    cr
    ." TOS-bottom:" ds-tos-bottom @ . cr
    ." TOS:" ds-tos @ . cr ;

: basic-datastackdump-new ( il-addr n -- )
    \ new stackelements
    ?trace $0100 [IF]
	." STACKDUMP (new):" hex.s cr
    [THEN]
    cells #sp id!
    dup inst inst-ds!-list @ slist-insert drop
    inst-btrees-insert ;

: basic-datastackdump-old ( il-addr n -- )
    \ old stackelements (changed)
    ?trace $0100 [IF]
	." STACKDUMP (old):" hex.s cr
    [THEN]
    tuck cells #sp id!
    dup inst inst-ds!-list @ slist-insert drop
    swap ds-init @ inst NULL inst tuck slist-insert drop
    over il-depends !
    inst-btrees-insert ;

: basic-datastackdump ( -- )
    ds-tos @ ds-tos-start - >r
    ?trace $0100 [IF]
	basic-datastackdump-print
    [THEN]

    \ dump the data stackregisters
    #tos tos-#register + #tos ?do
	data>
	dup i #tos - ds-init @ = if
	    drop
	else
	    dup il-op @ I_MOVE =
	    over il-reg @ regs-unused = or if
		register-move
	    endif
	    i over il-reg !
	    dup
	    i #tos - ds-init @ inst NIL inst tuck slist-insert drop
	    swap il-depends !
	    inst-btrees-insert
	endif
    loop

    \ dump the data stack
    ds-tos-bottom @ ds-tos-start - ds-tos @ ds-tos-start - ?do
	?trace $0100 [IF]
	    ." STACKDUMP (data):" i ds-tos-start + . hex.s cr
	[THEN]
	data>
	i dup 0< if
	    \ new stackelements
	    basic-datastackdump-new
	else
	    2dup ds-init @ <> if
		\ old stackelements (changed)
		basic-datastackdump-old
	    else
		tos-#register over > if
		    \ top of stack elements (changed)
		    basic-datastackdump-old
		else
		    ?trace $0100 [IF]
			." STACKDUMP (nothing):" hex.s cr
		    [THEN]
		    2drop
		endif
	    endif
	endif
    loop

    \ update the data stackpointer
    ds-tos-bottom @ ds-tos-start - tos-#register ?do
	i ds-init @ inst inst-ds!-list @ slist-insert drop
    loop
    #sp r> basic-stackupdate ;

: basic-returnstackdump-print ( -- )
    rs-size 0 ?do
	i rs-size 2/ >= if
	    i rs-size 2/ - rs-init @
	    i rs-data @ tuck = if
		drop
	    else
		?dup if
		    il-print
		endif
	    endif
	else
	    i rs-data @ ?dup if
		il-print
	    endif
	endif
    loop
    cr
    ." TOR-bottom:" rs-tos-bottom @ . cr
    ." TOR:" rs-tos @ . cr ;

: basic-returnstackdump ( -- )
    rs-tos @ rs-tos-start - >r
    ?trace $0100 [IF]
	basic-returnstackdump-print
    [THEN]

    \ dump the return stack
    rs-tos-bottom @ rs-tos-start - rs-tos @ rs-tos-start - ?do
	?trace $0100 [IF]
	    ." STACKDUMP (return):" i rs-tos-start + . hex.s cr
	[THEN]
	return> i rs-init @ over <> if
	    i cells #rp id!
	    dup inst inst-rs!-list @ slist-insert drop
	    i dup 0>= if
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

    \ update the return stackpointer
    rs-tos-bottom @ rs-tos-start - 0 ?do
	i rs-init @ inst inst-rs!-list @ slist-insert drop
    loop
    #rp r> basic-stackupdate ;

: basic-stackdump ( -- )
    \ dump the data stack
    basic-datastackdump

    \ dump the return stack
    basic-returnstackdump ;

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
	." BASIC-EXIT} " hex.s cr
    [THEN]
    basic-stackdump

    ?trace $0200 [IF]
	basic-print
	." BASIC-EXIT} " here hex. cr
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
    basic-head-sav @ dp !
    assemble ;

: compile-cs-pick ( u -- ) ( C: dest/origu ... dest/orig1 dest/orig0 -- dest/origu ... dest/orig1 dest/orig0 dest/origu )
    #control@ ;

: compile-cs-roll ( u -- ) ( C: dest/origu dest/origu-1 ... dest/orig0 -- dest/origu-1 ... dest/orig0 dest/origu )
    dup 1+ #control@ 2>r >r
    cs-tos @ 1+ dup cs-dsize cs-data swap 1+ cs-dsize cs-data rot cs-dsize cells move
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
