\ inst-selection.fs	instruction selection words
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

slist-struct
    1 cells: field inst-node
end-struct inst-struct

\ allocate and initial a inst
: inst ( il -- inst-addr )
    inst-struct struct-allot	\ allocate
    slist			\ initial values
    tuck inst-node ! ;

\ variables for local instruction arrays
$40 constant inst-size
inst-size array inst-ils
variable inst-ils-end
\ contains all intermediate code end nodes, i.e., ! nodes, stack pointer updates, and nodes that are on the stack at the basic block end
inst-size array inst-mls
variable inst-mls-end
\ contains all possible instruction code nodes, in arbitrary order
inst-size array inst-lists
variable inst-lists-end
\ contains all instruction nodes, in the right order

\ functions for handling the instruction array
: inst-init ( -- )
    NIL inst-!-list !
    NIL inst-@-list !
    0 inst-ils inst-size cells erase
    0 inst-ils-end !
    0 inst-mls inst-size cells erase
    0 inst-mls-end !
    0 inst-lists inst-size cells erase
    inst-size 1- inst-lists-end ! ;

include node.fs

\ allocate and initialize a node
: make-il ( val reg op -- il )
    il-struct struct-allot	\ allocate
    btree
    tuck il-op !		\ initial values
    tuck il-reg !
    tuck il-val !
    0 over il-slabel !
    NIL over il-depends !
    dup il-nt-insts cell+ max-NT 1- cells erase \ !! or just reset the mls in the nts
;

: inst-sequence ( xt addr n -- )
    rot >r
    cells over + swap
    begin ( to-addr from-addr )
	2dup >
    while
	r@ -rot
	tuck 2>r
	@ swap execute
	2r>
	cell+
    repeat
    rdrop 2drop ;

: inst-sequence-code-emission ( xt addr n -- )
    rot >r
    cells over + swap
    begin ( to-addr from-addr )
	2dup >
    while
	r@ -rot
	tuck 2>r
	dup last-load !
	@ swap execute
	2r>
	cell+
    repeat
    rdrop 2drop ;

: inst-ils-insert ( ml -- )
    inst-ils-end @ inst-ils !
    1 inst-ils-end +! ;

: inst-mls-insert ( ml -- )
    inst-mls-end @ inst-mls !
    1 inst-mls-end +! ;

: inst-mls-delete ( addr -- )
    inst-mls-end @ 1- dup if
	inst-mls dup @ NIL rot !
	-1 inst-mls-end +!
	over @ if
	    swap !
	else
	    2drop
	endif
    else
	NIL swap inst-mls !
	0 inst-mls-end !
	drop
    endif ;

: inst-lists-insert ( ml -- )
    inst-lists-end @ inst-lists !
    -1 inst-lists-end +! ;

: make-ml ( -- ml )
    ml-struct struct-allot	\ allocate
    btree
    dup ml-delay off            \ initial values
;

: il>ml ( il nt asm-xt -- ml )
    \ create ml
    -rot
    over il-nt-insts swap th	( asm-xt node il-nt-instp )
    make-ml
    dup rot !			( asm-xt node ml )
    over il-val @ over ml-val !
    over il-reg @ over ml-reg !
    1 over ml-count !
    over il-depends @ over ml-depends !
    nip				( asm-xt ml )
    tuck ml-asm !		( ml )
    1 over ml-latency !
    0 over ml-pathlength !
    1 over ml-let !             \ erst beim scheduler initialisieren
;

: ml-data-pathlength ( ml -- n )
    dup if
	dup ml-pathlength @ swap ml-latency @ +
    endif ;

defer ml-translate
: ml-join ( ml-left ml-right ml -- ml )
    2dup ml-right ! swap ml-data-pathlength >r
    2dup ml-left ! swap ml-data-pathlength
    r> max swap                        ( n ml )
    dup ml-depends @ ?dup if ( n ml il-list )
	>r tuck swap
	NIL r>
	( ml ml n ml-list il-list )
	['] ml-translate swap slist-forall ( ml ml n ml-list )
	rot ml-depends ! swap
    endif                             ( n ml )
    tuck ml-pathlength ! ;

include machine/grammar.fs
include regs.fs

?trace $0fff [IF]
0 constant il-print-flag
1 constant ml-print-flag
variable print-flag
il-print-flag print-flag !

: (print-.-) ( flag -- )
    if
	." -"
    else
	." _"
    endif ;

: (print-.name) ( flag -- )
    print-flag @ il-print-flag = if
	." il"
    else
	." ml" print-flag @ 0 0 d.r
    endif
    dup (print-.-)
    lastnfa
    name>string 0 ?do
	2dup c@
	swap if
	    emit
	else
	    dup '- = if
		drop '_ emit
	    else
		dup 'a 'z 1+ within
		over 'A 'Z 1+ within or
		over '0 '9 1+ within or
		over '_ = or if
		    emit
		else
		    0 0 d.r
		endif
	    endif
	endif
	char+
    loop
    drop
    (print-.-) ;

: (print-name) ( flag -- )
    dup (print-.name) basic-block @ 0 0 d.r (print-.-) ;

: print-name ( -- )
    true (print-name) ;

: print-cname ( -- )
    false (print-name) ;

: print-number ( n -- )
    [ 2 cells ] literal swap hexn. ;

: ?print-number ( addr -- )
    @ print-number ;

: print-number-alone ( n -- )
    [ 2 cells ] literal swap (hexn.) ;

: ?print-number-alone ( addr -- )
    @ print-number-alone ;

: print-bool ( n -- )
    if
	." true"
    else
	." false"
    endif ;

: ?print-bool ( addr -- )
    @ print-bool ;

: print-register ( n -- )
    dup regs-unallocated = if
	drop
	." unallocated"
    else
	2 swap hexn.
    endif ;

: ?print-register ( addr -- )
    @ print-register ;

: print-node-name ( addr -- )
    print-cname hexnum. ;

: print-edge ( addr addr -- )
    dup if
	print-cname hexnum.
	." -> "
	print-cname hexnum. ." ;" cr
    else
	2drop
    endif ;

: print-edge-dotted ( addr addr -- )
    dup if
	print-cname hexnum.
	." -> "
	print-cname hexnum. ." [ style = dotted ];" cr
    else
	2drop
    endif ;

: il-print-depends-func ( il inst -- il )
    over swap inst-node @ print-edge-dotted cr ;

: il-print-depends ( il inst -- il )
    ?dup if
	['] il-print-depends-func swap slist-forall
    endif ;

?trace $0fff [IF]
: il-print ( il -- )
    dup il-struct drop dump
    dup dup il-left @ print-edge
    dup dup il-right @ print-edge

    dup print-node-name
    ." [ "
    ." label = " '" emit
    dup burm-OP-LABEL @ burm-opname
    ." (" dup il-slabel ?print-number ." )\l"
    ." [" dup print-cname print-number-alone ." ]\l"
    ." val: " dup il-val ?print-number ." \l"
    ." reg: " dup il-reg ?print-register ." \l"
    '" emit
    ."  ];" cr
    dup il-depends @ il-print-depends
    drop ;

: il-print-all ( il -- )
    ['] il-print swap btree-postorder cr ;
[THEN]

: ml-print-rule ( cfa -- )
    >name cell+
    count $1f and cell- swap cell+ swap snumber? if
	burm-string
    else
	true burm-assert" invalid rule"
    endif ;

defer ml-print-all

: ml-print-depends-func ( ml inst -- ml )
    inst-node @
    dup ml-count @ 1 = if
	dup ml-print-all
    endif
    over swap print-edge-dotted cr ;

: ml-print-depends ( ml inst-addr -- ml )
    ?dup if
	['] ml-print-depends-func swap slist-forall
    endif ;

: ml-print ( ml -- )
    dup ml-struct drop dump
    dup dup ml-left @ print-edge
    dup dup ml-right @ print-edge

    dup print-node-name
    ." [ "
    ." label = " '" emit
    dup ml-asm @ ml-print-rule ." \l"
    ." [" dup print-cname print-number-alone ." ]\l"
    ." count: " dup ml-count ?print-number ." \l"
    ." val: " dup ml-val ?print-number ." \l"
    ." reg: " dup ml-reg ?print-register ." \l"
    ." delay: " dup ml-delay ?print-bool ." \l"
    ." latency: " dup ml-latency ?print-number ." \l"
    ." pathlength: " dup ml-pathlength ?print-number ." \l"
    ." let: " dup ml-let ?print-number ." \l"
    '" emit
    ."  ];" cr
    dup ml-depends @ ml-print-depends
    drop ;

: print-edge-asm-ml ( addr -- )
    dup if
	print-cname dup hexnum.
	." -> "
	1 print-flag !
	print-cname
	3 print-flag !
	hexnum. ." ;" cr
    else
	2drop
    endif ;

: ml-asm-print ( ml ml -- )
    dup ml-struct drop dump
    swap ?dup if
	over swap print-edge
    endif
    dup print-edge-asm-ml

    dup print-node-name
    ." [ "
    ." label = " '" emit
    dup ml-asm @ ml-print-rule ." \l"
    ." [" dup print-cname print-number-alone ." ]\l"
    ." count: " dup ml-count ?print-number ." \l"
    ." val: " dup ml-val ?print-number ." \l"
    ." reg: " dup ml-reg ?print-register ." \l"
    ." delay: " dup ml-delay ?print-bool ." \l"
    ." latency: " dup ml-latency ?print-number ." \l"
    ." pathlength: " dup ml-pathlength ?print-number ." \l"
    ." let: " dup ml-let ?print-number ." \l"
    '" emit
    ."  ];" cr ;

:noname ( il -- )
    ['] ml-print swap btree-postorder cr ;
is  ml-print-all

: inst-ils-print ( -- )
    ." BTREE PRINT" hex.s cr
    il-print-flag print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " '" emit print-name stamp '" emit ." ;" cr
    ['] il-print-all 0 inst-ils inst-ils-end @ inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: inst-mls-print ( -- )
    ." NODE PRINT" hex.s cr
    ml-print-flag print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " '" emit print-name stamp '" emit ." ;" cr
    ['] ml-print-all 0 inst-mls inst-mls-end @ inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: inst-lists-print ( -- )
    ." LISTS PRINT" hex.s cr
    ml-print-flag 2 + print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " '" emit print-name stamp '" emit ." ;" cr
    NIL ['] ml-asm-print 0 inst-lists
    inst-lists-end @ 1+ tuck cells + inst-size rot - inst-sequence drop
    ." /* cluster_" print-cname ." */ }" cr ;
[THEN]

: op ( il il op -- il )
    dup burm-arity c@ 2 <> burm-assert" invalid arity(2)"
    >r over il-slabel @ over il-slabel @ swap r@ burm-state
    0 regs-unallocated r> make-il
    tuck il-slabel !
    tuck il-left !
    tuck il-right ! ;

: uop ( il op -- il )
    dup burm-arity c@ 1 <> burm-assert" invalid arity(1)"
    >r dup il-slabel @ NIL r@ burm-state
    0 regs-unallocated r> make-il
    tuck il-slabel !
    tuck il-left ! ;

: terminal ( val reg op -- il )
    \ ." A:" hex.s cr
    \ 0 burm-arity $40 dump
    dup burm-arity
    \ dup $20 dump
    \ ." B:" hex.s cr
    c@
    \ ." C:" hex.s cr
    0<>
    burm-assert" invalid arity(0)"
    dup NIL NIL rot burm-state >r make-il
    r> over il-slabel ! ;

: register-move ( il1 -- il2 )
    I_MOVE uop ;

: register-terminal ( reg -- il )
    NULL swap I_REG terminal
    register-move ;

: lit ( n -- il )
    dup if
	dup $8000 <
	over $0000 > and if
	    regs-unallocated I_LITS terminal
	else
	    regs-unallocated I_LIT terminal
	endif
    else
	0 I_ZERO terminal
    endif ;

: addr ( offset register -- il )
    swap regs-unallocated I_LITS terminal
    NULL rot I_REG terminal
    I_PLUS op ;

: id@ ( offset register -- il )
    addr I_FETCH uop ;

: id! ( il offset register -- il )
    addr swap I_STORE op ;

: inst-cover ( indent goal il -- )
    dup hex.
    tuck burm-STATE-LABEL @ swap burm-rule dup 0= if
	nip nip
	." no cover" cr
    else
	>r over 0 ?do
	    ." ."
	loop
	." ( " r@ . ." ) " r@ burm-string cr
	r> rot >r				\ (R: indent)
	dup burm-nts @ >r			\ (R: indent nts-addr)
	burm-kids r>
	begin					\ (R: indent)
	    dup @ dup
	while
	    rot r@ 1+ -rot
	    \ [ 1 -3 wword-regs-adjust ]
	    recurse
	    cell+
	repeat
	2drop rdrop
    endif ;

burm-max-rule 1+ array burm-reduce-rules

: burm-reduce ( goal node -- ... )
    \ if an ml is produced, the ... is an ml
    \ otherwise it can be anything (a constant, a register number ...)
    over dup 1 < swap burm-NT > or burm-assert" invalid goal"
    2dup il-nt-insts swap th @ dup if ( goal node ml )
	\ node already reduced
	1 over ml-count +!
	nip nip
    else
	drop
	2dup il-slabel @ swap burm-rule
	burm-reduce-rules @ execute
    endif ;

: gen-reduce-rule ( n -- )
    >r
    :noname
	postpone tuck postpone 2>r
	r@ [burm-kids] @ compile,
	0 r@ burm-nts @
	begin
	    dup @
	while
	    swap 1+ swap
	    cell+
	repeat
	drop
	\ skip for terminals
	dup 0= if
	    drop
	else
	    dup 2 = if
		\ save second kiddy on return stack
		postpone swap postpone >r
	    endif
	    r@ burm-nts @ @ postpone literal postpone swap postpone burm-reduce
	    2 = if
		\ reduce second kiddy
		r@ burm-nts @ cell+ @ postpone literal postpone r> postpone burm-reduce
	    endif
	endif
	postpone 2r> postpone swap r@ [burm-reduce] @ compile,
	postpone ;
    r> burm-reduce-rules ! ;

: gen-all-reduce-rules ( -- )
    burm-max-rule 1+ 1 ?do
	i gen-reduce-rule
    loop ;

:noname ( goal node -- ... )
    \ node has no cover
    nip
    burm-assert" no cover" cr ;
0 burm-reduce-rules !
gen-all-reduce-rules

: mls-pr ( -- )
    ." SCHED MLS: "
    inst-mls-end @ 0 ?do
	i inst-mls @ ?dup if
	    hex.
	endif
    loop
    cr ;

: lists-pr ( -- )
    ." SCHED LIST: "
    inst-size 0 ?do
	i inst-lists @ ?dup if
	    hex.
	endif
    loop
    cr ;

: inst-check ( -- )
    \ ." CHECK BB:" basic-block @ . cr
    \ mls-pr
    \ lists-pr
    0 inst-mls
    begin
	\ mls-pr
	\ 0 inst-mls $20 dump
	dup @
    while
	dup @ ml-count @ if
	    dup inst-mls-delete
	else
	    cell+
	endif
    repeat
    drop ;

: inst-selection-func ( il -- )
    ?trace $0100 [IF]
	." inst-selection:" hex.s cr
    [THEN]
    ?trace $0100 [IF]
	\ dup 0 burm-reg-NT rot inst-cover
    [THEN]
    \ reduce the tree
    burm-reg-NT swap burm-reduce
    -1 over ml-count +!
    inst-mls-insert ;

: inst-selection ( -- )
    ['] inst-selection-func 0 inst-ils inst-ils-end @ inst-sequence
    ds-stackupdate @ ?dup if
	inst-ds!-list @ swap il-nt-insts burm-reg-NT th @
	tuck ml-depends !
	100 swap ml-pathlength !
    endif
    rs-stackupdate @ ?dup if
	inst-rs!-list @ swap il-nt-insts burm-reg-NT th @
	tuck ml-depends !
	100 swap ml-pathlength !
    endif
    inst-check ;

:noname ( pathlength1 ml-list1 inst -- pathlength2 ml-list2 )
    burm-reg-NT swap inst-node @ burm-reduce    ( pathlength1 ml-list1 ml )
    rot over ml-pathlength @ 1+ max -rot        ( pathlength2 ml-list1 ml )
    inst tuck slist-next !                      ( pathlength2 ml-list2 )
;
is ml-translate

: code-emission-func ( ml -- )
    last-load @ here
    = if
	NIL asm-nop
    endif
    dup dup ml-asm @ execute			\ assemble the instruction
    ml-delay @ if				\ fill delay slot ?
	NIL asm-nop
    endif ;

: code-emission ( -- )
    ['] code-emission-func
    0 inst-lists inst-lists-end @ 1+ tuck cells +
    inst-size rot - inst-sequence-code-emission ;

?test $0010 [IF]
cr ." Test for inst-selection.fs" cr

finish
[THEN]
