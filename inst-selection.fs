\ inst-selection.fs	instruction selection words
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

slist-struct
    1 cells: field inst-node
end-struct inst-struct

\ allocate and initial a inst
: inst ( il-addr -- inst-addr )
    inst-struct struct-allot	\ allocate
    slist			\ initial values
    tuck inst-node ! ;

\ variables for local instruction arrays
$80 constant inst-size
inst-size array inst-btrees
\ contains all intermediate code end nodes, i.e., ! nodes, stack pointer updates, and nodes that are on the stack at the basic block end
inst-size array inst-lists
\ contains all instruction nodes, in the right order
inst-size array inst-nodes
\ contains all instruction nodes, in arbitrary order
inst-size array inst-pnodes
\ contains leader instruction nodes for instruction scheduling

\ functions for handling the instruction array
: inst-init ( -- )
    NIL inst inst-!-list !
    NIL inst inst-@-list !
    0 inst-btrees inst-size cells 0 fill
    0 inst-lists inst-size cells 0 fill
    0 inst-nodes inst-size cells 0 fill
    0 inst-pnodes inst-size cells 0 fill ;

include node.fs

: asm ( il-addr -- )
    drop ;

\ reset node values
: il-reset ( il-addr -- )
    0 over il-slabel !
    -1 over il-reg !
    NIL over il-depends !
    dup il-nt-insts cell+ max-NT 1- cells erase \ !! or just reset the mls in the nts
    drop ;

\ allocate and initialize a node
: make-il ( val reg op -- il-addr )
    il-struct struct-allot	\ allocate
    btree
    dup il-reset
    tuck il-op !		\ initial values
    tuck il-reg !
    tuck il-val ! ;

: ?inst-delay ( ml-addr -- flag )
    ml-delay @ ;

: inst-done ( ml-addr -- )
    ml-done on ;

: inst-notdone ( ml-addr -- )
    ml-done off ;

: ?inst-done ( ml-addr -- flag )
    ml-done @ ;

: ?inst-notdone ( ml-addr -- flag )
    ?inst-done 0= ;

: inst-sequence ( xt addr -- )
    swap over inst-size 1- cells + >r
    begin ( addr xt )
	over @ ?dup if ( addr xt addr@ )
	    over execute
	endif
	swap cell+ swap over r@ >
    until
    rdrop 2drop ;

: inst-sequence-reverse ( xt addr -- )
    swap >r
    dup inst-size 1- cells +
    begin ( begin ptr )
	2dup u<=
    while
	dup @ ?dup if ( begin ptr element )
	    r@ execute
	endif
	cell-
    repeat
    2drop rdrop ;

: inst-insert ( ml-addr addr -- )
    begin
	dup @
    while
	cell+
    repeat
    ! ;

: inst-insert-end ( ml-addr addr -- )
    inst-size 1- cells +
    begin
	dup @
    while
	cell-
    repeat
    ! ;

: inst-btrees-insert ( ml-addr -- )
    0 inst-btrees inst-insert ;
: inst-btrees-insert-end ( ml-addr -- )
    0 inst-btrees inst-insert-end ;

: inst-lists-insert ( ml-addr -- )
    0 inst-lists inst-insert ;
: inst-lists-insert-end ( ml-addr -- )
    0 inst-lists inst-insert-end ;

: inst-nodes-insert ( ml-addr -- )
    0 inst-nodes inst-insert ;
: inst-nodes-insert-end ( ml-addr -- )
    0 inst-nodes inst-insert-end ;

: inst-pnodes-insert ( ml-addr -- )
    0 inst-pnodes inst-insert ;
: inst-pnodes-insert-end ( ml-addr -- )
    0 inst-pnodes inst-insert-end ;

: make-ml ( -- ml )
    ml-struct struct-allot
    dup ml-delay off ;

: cons-ml ( ml ml-list1 -- ml-list2 )
    swap inst
    tuck slist-next ! ;

: inst-ok ( node nt asm-xt -- ml )
    \ create ml
    -rot
    over il-nt-insts swap th	( left-ml right-ml asm-xt node il-nt-instp )
    make-ml
    dup rot !			( left-ml right-ml asm-xt node ml )
    over il-val @ over ml-val !
    over il-reg @ over ml-reg !
    over il-depends @ over ml-node-dependences !
    NIL over ml-depends !
    nip				( left-ml right-ml asm-xt ml )
    tuck ml-asm !		( ml )
    0 over ml-count !
    dup ml-done off
    1 over ml-cost !
    dup
    inst-nodes-insert-end
    \ inst-nodes-insert
;

include machine/grammar.fs
include regs.fs

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
	dup c@
	over if
	    emit
	else
	    dup [char] - = if
		drop [char] _ emit
	    else
		dup [char] a [char] z 1+ within
		over [char] A [char] Z 1+ within or
		over [char] 0 [char] 9 1+ within or if
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
    [ cell 2* ] literal swap hexn. ;

: ?print-number ( addr -- )
    @ print-number ;

: print-bool ( n -- )
    if
	." true"
    else
	." false"
    endif ;

: ?print-bool ( addr -- )
    @ print-bool ;

: print-register ( n -- )
    dup regs-unused = if
	drop
	." unallocated"
    else
	2 swap hexn.
    endif ;

: ?print-register ( addr -- )
    @ print-register ;

: print-node-name ( addr -- )
    print-cname . ;

: print-edge ( addr addr -- )
    dup if
	print-cname .
	." -> "
	print-cname . ." ;" cr
    else
	2drop
    endif ;

: print-edge-dashed ( addr addr -- )
    dup if
	print-cname .
	." -> "
	print-cname . ." [ style = dashed ];" cr
    else
	2drop
    endif ;

: il-print-depends-func ( il-addr -- )
    over swap inst-node @ print-edge-dashed cr ;

: il-print-depends ( inst-addr -- )
    dup if
	slist-next @ ['] il-print-depends-func maplist
    else
	drop
    endif ;

: il-print ( il -- )
    dup $20 - $80 dump
    \ il-print-flag print-flag !
    dup dup il-left @ print-edge
    dup dup il-right @ print-edge

    dup print-node-name
    ." [ "
    ." label = " [char] " emit
    dup burm-OP-LABEL @ burm-opname ." (" dup il-slabel ?print-number ." )\l"
    ." val: " dup il-val ?print-number ." \l"
    ." reg: " dup il-reg ?print-register ." \l"
    [char] " emit
    ."  ];" cr
    dup il-depends @ il-print-depends
    drop ;

: il-print-all ( il-addr -- )
    ['] il-print swap btree-postorder cr ;

: ml-print-depends ( inst-addr -- )
    ['] il-print-depends-func maplist ;

: ml-print ( ml -- )
    \ ml-print-flag print-flag !
    dup dup ml-left @ print-edge
    dup dup ml-right @ print-edge

    dup print-node-name
    ." [ "
    ." label = " [char] " emit
    dup ml-asm @ name. ." \l"
    ." count:  " dup ml-count ?print-number ." \l"
    ." val:    " dup ml-val ?print-number ." \l"
    ." done:   " dup ml-done ?print-bool ." \l"
    ." reg:    " dup ml-reg ?print-register ." \l"
    ." delay:  " dup ml-delay ?print-bool ." \l"
    ." cost:   " dup ml-cost ?print-number ." \l"
    [char] " emit
    ."  ];" cr
    dup ml-depends @ ml-print-depends
    drop ;

: inst-btrees-print ( -- )
    il-print-flag print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " [char] " emit print-name [char] " emit ." ;" cr
    ['] il-print-all 0 inst-btrees inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: inst-nodes-print ( -- )
    ml-print-flag
    ~~
    print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " [char] " emit print-name [char] " emit ." ;" cr
    ['] ml-print 0 inst-nodes inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: inst-pnodes-print ( -- )
    ml-print-flag 1+
    ~~
    print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " [char] " emit print-name [char] " emit ." ;" cr
    ['] ml-print 0 inst-pnodes inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: inst-lists-print ( -- )
    ml-print-flag 2 +
    ~~
    print-flag !
    ." subgraph cluster_" print-cname ."  {" cr
    ." label = " [char] " emit print-name [char] " emit ." ;" cr
    ['] ml-print 0 inst-lists inst-sequence
    ." /* cluster_" print-cname ." */ }" cr ;

: op ( il-addr il-addr op -- il-addr )
    dup burm-arity c@ 2 <> burm-assert" invalid arity(2)"
    >r over il-slabel @ over il-slabel @ swap r@ burm-state
    0 regs-unused r> make-il
    tuck il-slabel !
    tuck il-left !
    tuck il-right ! ;

: uop ( il-addr op -- il-addr )
    dup burm-arity c@ 1 <> burm-assert" invalid arity(1)"
    >r dup il-slabel @ NIL r@ burm-state
    0 regs-unused r> make-il
    tuck il-slabel !
    tuck il-left ! ;

: terminal ( val reg op -- il-addr )
    dup burm-arity c@ burm-assert" invalid arity(0)"
    dup NIL NIL rot burm-state >r make-il
    r> over il-slabel ! ;

: register-move ( il-addr register -- il-addr )
    I_MOVE uop ;

: register-terminal ( reg -- il-addr )
    NULL swap I_REG terminal
    register-move ;

: lit ( n -- il-addr )
    dup $8000 <
    over $0000 >= and if
	regs-unused I_LITS terminal
    else
	regs-unused I_LIT terminal
    endif ;

: fff
    dup 0= if
	regs-unused I_LITS terminal \ !! warum 0 ?
    else
	dup $8000 >= if
	    regs-unused I_LIT terminal
	else
	    dup $0000 < if
		regs-unused I_LIT terminal
	    else
		regs-unused I_LITS terminal
	    endif
	endif
    endif ;

: addr ( offset register -- il-addr )
    swap regs-unused I_LITS terminal
    NULL rot I_REG terminal
    I_PLUS op ;

: id@ ( offset register -- il-addr )
    addr I_FETCH uop ;

: id! ( il-addr offset register -- il-addr )
    addr swap I_STORE op ;

: translate-dependence ( ml-list1 slist-node -- ml-list2 )
    inst-node @
    \ MAX-NT 1
    \ ?do ( ml-list il )
    \ dup
    il-nt-insts ( i ) burm-reg-NT th @ ?dup if ( ml-list il ml )
	\ rot cons-ml swap
	swap cons-ml
    endif
    \ loop
    \ drop
;

: translate-dependences ( il-list -- ml-list )
\ translate dependences at the IL level into dependences at the ML level
\ !! looks only at the top IL for an ML
    dup if
	NIL swap slist-next @ ['] translate-dependence maplist
    endif ;

: translate-ml-dependences ( ml -- )
    dup ml-node-dependences @ translate-dependences
    swap ml-depends ! ;

: translate-all-dependences ( -- )
    ['] translate-ml-dependences 0 inst-nodes inst-sequence ;

: inst-cover ( indent goal il-addr -- )
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
	    rot r@ 1+ rot rot
	    [ 1 -3 wword-regs-adjust ]
	    recurse
	    cell+
	repeat
	2drop rdrop
    endif
;

burm-max-rule 1+ array burm-reduce-rules

: burm-reduce ( goal node -- ... )
    \ if an ml is produced, the ... is an ml
    \ otherwise it can be anything (a constant, a register number ...)
    over dup 1 < swap burm-NT > or burm-assert" invalid goal"
    2dup il-nt-insts swap th @ dup if ( goal node ml )
	\ node already reduced
	\ 1 over ml-count +!
	nip nip
	exit
    endif
    drop
    2dup il-slabel @ swap burm-rule
    burm-reduce-rules @ execute ;

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

: inst-selection-func ( il-addr -- )
    ?trace $0020 [IF]
	." inst-selection:" hex.s cr
    [THEN]
    ?trace $0020 [IF]
	dup 0 burm-reg-NT rot inst-cover
    [THEN]
    \ reduce the tree
    burm-reg-NT swap burm-reduce drop ;

: inst-selection ( -- )
    ['] inst-selection-func 0 inst-btrees inst-sequence ;

: assemble-func ( ml -- )
    dup dup ml-asm @ execute			\ assemble the instruction
    ?inst-delay if				\ fill delay slot ?
	NIL asm-nop
    endif ;

: assemble ( -- )
    ['] assemble-func 0 inst-lists inst-sequence ;

?test $0020 [IF]
cr ." Test for inst-selection.fs" cr

finish
[THEN]
