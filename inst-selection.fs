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
  swap over inst-size 1- cells + >r begin ( addr xt )
    over @ ?dup 0<> if ( addr xt addr@ )
      over execute endif
    swap cell+ swap over r@ > until
  rdrop 2drop ;

: inst-sequence-reverse ( xt addr -- )
  swap >r
  dup inst-size 1- cells + begin ( begin ptr )
     2dup u<= while
     dup @ ?dup if ( begin ptr element )
	r@ execute
     endif
     cell-
  repeat
  2drop rdrop ;

: inst-insert ( ml-addr addr -- )
  begin
      dup @ 0<> while
      cell+
  repeat
  ! ;

: inst-insert-end ( ml-addr addr -- )
  inst-size 1- cells +
  begin
    dup @ 0<> while
    cell- repeat
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

: translate-dependence ( ml-list1 slist-node -- ml-list2 )
    inst-node @
    MAX-NT 1 ?do ( ml-list il )
	dup il-nt-insts i th @ ?dup if ( ml-list il ml )
	    rot cons-ml swap
	endif
    loop
    drop ;

: translate-dependences ( il-list -- ml-list )
\ translate dependences at the IL level into dependences at the ML level
\ !! looks only at the top IL for an ML
    dup il-depends-init <> if
	NIL ['] translate-dependence rot slist-forall
    else
	drop NIL
    endif ;

: translate-ml-dependences ( ml -- )
    dup ml-node-dependences @ translate-dependences
    swap ml-depends ! ;

: translate-all-dependences ( -- )
    ['] translate-ml-dependences 0 inst-nodes inst-sequence ;

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
    1 over ml-count !
    dup ml-done off
    1 over ml-cost !
    dup inst-nodes-insert-end ;

include machine/grammar.fs
include regs.fs

: inst-print-node ( il -- )
    ." { "
    dup hex.
    dup il-left hex?
    dup il-right hex?
    \ dup il-lval hex?
    \ dup il-rval hex?
    \ dup il-copy hex?
    \ dup il-asm hex?

    dup burm-OP-LABEL @ burm-opname
    dup il-slabel hex?
    dup il-val hex?
    ." reg:" dup il-reg ?
    \ ." done:" dup il-done ?
    \ ." count:" dup il-count ?
    \ ." cost:" dup il-cost ?
    \ ." delay:" dup il-delay ?
    ." depends:" dup il-depends @ inst-print-depends
    ." mls:" MAX-NT 1 ?DO dup il-nt-insts i th hex? LOOP
    drop
    ." }" cr ;

: inst-print-nodes ( il-addr -- )
    ['] inst-print-node swap btree-postorder cr ;

: inst-btrees-print ( -- )
    ['] inst-print-nodes 0 inst-btrees inst-sequence ;

: inst-lists-print ( -- )
    ['] print-ml 0 inst-lists inst-sequence ;

: inst-nodes-print ( -- )
    ['] print-ml 0 inst-nodes inst-sequence ;

: inst-pnodes-print ( -- )
    ['] print-ml 0 inst-pnodes inst-sequence ;

: op ( il-addr il-addr op -- il-addr )
    assert( dup burm-arity c@ 2 = ) \ !!
    >r over il-slabel @ over il-slabel @ swap r@ burm-state
    0 regs-unused r> make-il
    tuck il-slabel !
    tuck il-left !
    tuck il-right ! ;

: uop ( il-addr op -- il-addr )
    assert( dup burm-arity c@ 1 = ) \ !!
    >r dup il-slabel @ NIL r@ burm-state
    0 regs-unused r> make-il
    tuck il-slabel !
    tuck il-left ! ;

: terminal ( val reg op -- il-addr )
    dup NIL NIL rot burm-state >r
    assert( dup burm-arity c@ 0= ) \ !! assert funktioniert nicht
    make-il
    r> over il-slabel ! ;

: lit ( n -- il-addr )
    dup 0= if
	0 I_LITS terminal \ !! warum 0 ?
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

: inst-cover ( indent goal il-addr -- flag )
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
	    dup @ dup 0<>
	while
	    rot r@ 1+ rot rot
	    [ 1 -3 wword-regs-adjust ]
	    recurse drop
	    cell+
	repeat
	2drop r>
    endif ;

: burm-reduce ( goal node -- ... )
    \ if an ml is produced, the ... is an ml
    \ otherwise it can be anything (a constant, a register number ...)
    ( ." printReduce1" hex.s cr )
    assert( over 1 >= )
    assert( over MAX-NT < )
    \ dup inst-print-node
    2dup il-nt-insts swap th @
    dup if ( goal node ml )
	1 over ml-count +!
	nip nip
	EXIT
    endif
    drop
    over >r
    tuck il-slabel @ swap burm-rule ( node rule )
    dup 0= if
	nip nip r> drop
	drop
	\ burm-assert" no cover" cr
    else ( node rule )
	2dup 2>r
	dup burm-nts @ >r		( node rule; R: node rule nts-addr )
	depth >r burm-kids depth r> - 2 + \ !! fix this!
	r>
	swap case
	    0 of
	    endof
	    1 of
	    swap >r endof
	    2 of
	    rot >r swap >r endof endcase
	>r
	begin
	    r> dup @ dup 0<>
	while
	    r> rot cell+ >r
	    recurse
	repeat
	2drop
	2r>
	[burm-reduce] @ r> swap execute
    endif
    ( ." printReduce2" hex.s cr ) ;

: inst-selection-func ( il-addr -- )
    ?trace $0020 [IF]
	." inst-selection:" hex.s cr
    [THEN]
    ?trace $0020 [IF]
	dup 0 burm-stmt-NT rot inst-cover drop
    [THEN]
    \ reduce the tree
    burm-stmt-NT swap burm-reduce drop ;

: inst-selection ( -- )
    ['] inst-selection-func 0 inst-btrees inst-sequence ;

: assemble-func ( ml -- )
    \ dup print-ml
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
