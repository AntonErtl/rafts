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

slist_struct
  1 cells: field inst_node
end-struct inst_struct

\ allocate and initial a inst
: inst ( node_addr -- inst_addr )
  inst_struct struct-allot	\ allocate
  slist				\ initial values
  tuck inst_node ! ;

\ variables for local instruction arrays
$80 constant inst_size
inst_size array inst_btrees
\ contains all intermediate code end nodes, i.e., ! nodes, stack pointer updates, and nodes that are on the stack at the basic block end
inst_size array inst_lists
\ contains all instruction nodes, in the right order
inst_size array inst_nodes
\ contains all instruction nodes, in arbitrary order
inst_size array inst_pnodes
\ contains leader instruction nodes for instruction scheduling

: inst_btrees@ ( n -- n ) inst_btrees @ ;
: inst_btrees! ( n n -- ) inst_btrees ! ;
: inst_lists@ ( n -- n ) inst_lists @ ;
: inst_lists! ( n n -- ) inst_lists ! ;
: inst_nodes@ ( n -- n ) inst_nodes @ ;
: inst_nodes! ( n n -- ) inst_nodes ! ;
: inst_pnodes@ ( n -- n ) inst_pnodes @ ;
: inst_pnodes! ( n n -- ) inst_pnodes ! ;

\ functions for handling the instruction array
: inst_init ( -- )
  0 inst_btrees inst_size cells 0 fill
  0 inst_lists inst_size cells 0 fill
  0 inst_nodes inst_size cells 0 fill
  0 inst_pnodes inst_size cells 0 fill ;

include node.fs

: ?inst_delay ( ml-addr -- flag )
  ml-delay @ ;

: inst_done ( ml-addr -- )
  ml-done on ;

: inst_notdone ( ml-addr -- )
  ml-done off ;

: ?inst_done ( ml-addr -- flag )
  ml-done @ ;

: ?inst_notdone ( ml-addr -- flag )
  ?inst_done 0= ;

: inst_sequence ( xt addr -- )
  swap over inst_size 1- cells + >r begin ( addr xt )
    over @ ?dup 0<> if ( addr xt addr@ )
      over execute endif
    swap cell+ swap over r@ > until
  rdrop 2drop ;

: inst_sequence_reverse ( xt addr -- )
  swap >r
  dup inst_size 1- cells + begin ( begin ptr )
     2dup u<= while
     dup @ ?dup if ( begin ptr element )
	r@ execute
     endif
     cell-
  repeat
  2drop rdrop ;

: inst_insert ( ml-addr addr -- )
  begin
    dup @ 0<> while
    cell+ repeat
  ! ;

: inst_insert_end ( ml-addr addr -- )
  inst_size 1- cells +
  begin
    dup @ 0<> while
    cell- repeat
  ! ;

: inst_btrees_insert ( ml-addr -- )
  0 inst_btrees inst_insert ;
: inst_btrees_insert_end ( ml-addr -- )
  0 inst_btrees inst_insert_end ;

: inst_lists_insert ( ml-addr -- )
  0 inst_lists inst_insert ;
: inst_lists_insert_end ( ml-addr -- )
  0 inst_lists inst_insert_end ;

: inst_nodes_insert ( ml-addr -- )
  0 inst_nodes inst_insert ;
: inst_nodes_insert_end ( ml-addr -- )
  0 inst_nodes inst_insert_end ;

: inst_pnodes_insert ( ml-addr -- )
  0 inst_pnodes inst_insert ;
: inst_pnodes_insert_end ( ml-addr -- )
  0 inst_pnodes inst_insert_end ;

: make-ml ( -- ml )
  ml-struct struct-allot
  dup ml-delay off ;

: cons-ml ( ml ml-list1 -- ml-list2 )
    swap inst
    tuck slist_next ! ;

: translate-dependence ( ml-list1 slist_node -- ml-list2 )
    inst_node @
    MAX-NT 1 ?DO ( ml-list node )
	dup node-nt-insts i th @ ?dup if ( ml-list node ml )
	    rot cons-ml swap
        endif
    LOOP
    drop ;

: translate-dependences ( node-list -- ml-list )
\ translate dependences at the IL level into dependences at the ML level
\ !! looks only at the top IL for an ML
    dup node_depends_init <> if
	NIL ['] translate-dependence rot slist_forall
    else
	drop NIL
    endif ;

: translate-ml-dependences ( ml -- )
    dup ml-node-dependences @ translate-dependences
    swap ml-depends ! ;

: translate-all-dependences ( -- )
  ['] translate-ml-dependences 0 inst_nodes inst_sequence ;

: inst_ok ( node nt asm-xt -- ml )
  \ create ml
  -rot
  over node-nt-insts swap th	( left-ml right-ml asm_xt node node-nt-instp )
  make-ml
  dup rot !			( left-ml right-ml asm_xt node ml )
  over node_val @ over ml-val !
  over node_reg @ over ml-reg !
  over node_depends @ over ml-node-dependences !
  NIL over ml-depends !
  nip				( left-ml right-ml asm_xt ml )
  tuck ml-asm !		( ml )
  1 over ml-count !
  dup ml-done off
  1 over ml-cost !
  dup inst_nodes_insert_end ;

: (asm_lval@) ( ml-addr -- ml-addr )
  btree_left @ ;
: (asm_rval@) ( ml-addr -- ml-addr )
  btree_right @ ;
: asm_val@ ( ml-addr -- n )
  ml-val @ ;
: asm_lval@ ( ml-addr -- val )
  (asm_lval@) ml-val @ ;
: asm_reg@ ( ml-addr -- register )
  ml-reg @ ;
: asm_lreg@ ( ml-addr -- register )
  (asm_lval@) ml-reg @ ;
: asm_rreg@ ( ml-addr -- register )
  (asm_rval@) ml-reg @ ;

: asm_nop ( ml-addr -- )
  drop @nop ;

: asm_lit ( ml-addr -- )
  dup ml-reg @ swap ml-val @ @li drop ;

: prep-load ( ml-addr -- rt offset rs )
  >r 
  r@ ml-reg @
  r@ ml-val @
  r> btree_left @ ml-reg @ ;

: asm_fetchc ( ml-addr -- )
  prep-load @lbu drop ;

: asm_fetchi ( ml-addr -- )
  prep-load @lw drop ;

: prep-store ( ml-addr -- rt offset rs )
  >r 
  r@ btree_left @ ml-reg @
  r@ ml-val @
  r> btree_right @ ml-reg @ ;

: asm_storec ( ml-addr -- )
  prep-store @sb ;

: asm_storei ( ml-addr -- )
  prep-store @sw ;

: asm_add ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @addu drop ;

: asm_addi ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @addiu drop ;

: asm_sub ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @subu drop ;

\ !! two instructions
: asm_mul ( ml-addr -- )
  dup asm_lreg@ over asm_rreg@ @multu asm_reg@ @mflo drop ;

: asm_div ( ml-addr -- )
  dup asm_lreg@ over asm_rreg@ @div asm_reg@ @mflo drop ;

: asm_mod ( ml-addr -- )
  dup asm_lreg@ over asm_rreg@ @div asm_reg@ @mfhi drop ;

: asm_neg ( ml-addr -- )
  dup asm_reg@ swap asm_lreg@ @neg drop ;

: asm_abs ( ml-addr -- )
  dup asm_reg@ swap asm_lreg@ @abs drop ;

: asm_and ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @and drop ;

: asm_andi ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @andi drop ;

: asm_or ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @or drop ;

: asm_ori ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @ori drop ;

: asm_xor ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @xor drop ;

: asm_xori ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @xori drop ;

: asm_not ( ml-addr -- )
  dup asm_reg@ swap asm_lreg@ @not drop ;

: asm_lsh ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @sllv drop ;

: asm_lshi ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @sll drop ;

: asm_rshu ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @srlv drop ;

: asm_rsh ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @srav drop ;

: asm_rshui ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @srl drop ;

: asm_rshi ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @sra drop ;

: asm_0branch ( ml-addr -- )
  dup asm_lreg@ @zero rot asm_val@
  here - cell- @beq ;

: asm_branch ( ml-addr -- )
  @zero @zero rot asm_val@
  here - cell- @beq ;

: asm_beq ( ml-addr -- )
  dup asm_lreg@ over asm_rreg@ rot asm_val@
  here - cell- @beq ;

: asm_seq ( ml-addr -- )
  >r
  r@ asm_reg@ r@ asm_lreg@ r> asm_rreg@ @xor drop
  dup 1 @sltiu drop ;

: asm_slt ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @slt drop ;

: asm_slti ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @slti drop ;

: asm_sltu ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @sltu drop ;

: asm_sltui ( ml-addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap ml-val @ @sltiu drop ;

include grammar.fs

: inst_print_node ( node -- )
  ." { "
  dup hex.
  dup node_left @ hex.
  dup node_right @ hex.
\  dup node_lval @ hex.
\  dup node_rval @ hex.
\  dup node_copy @ hex.
\  dup node_asm @ hex.

  dup burm_OP_LABEL@ burm_opname
  dup node_slabel hex?
  dup node_val hex?
  ." reg:" dup node_reg ?
\  ." done:" dup node_done ?
\  ." count:" dup node_count ?
\  ." cost:" dup node_cost ?
\  ." delay:" dup node_delay ?
  ." depends:" dup node_depends @ inst_print_depends
  ." mls:" MAX-NT 1 ?DO dup node-nt-insts i th @ hex. LOOP
  drop

  ." }" cr ;

: inst_print_nodes ( node_addr -- )
  ['] inst_print_node swap btree_postorder cr ;

: inst_btrees_print ( -- )
  ['] inst_print_nodes 0 inst_btrees inst_sequence ;

: inst_lists_print ( -- )
  ['] print-ml 0 inst_lists inst_sequence ;

: inst_nodes_print ( -- )
  ['] print-ml 0 inst_nodes inst_sequence ;

: inst_pnodes_print ( -- )
  ['] print-ml 0 inst_pnodes inst_sequence ;

: op ( node_addr node_addr op -- node_addr )
  assert( dup burm_arity@ 2 = ) \ !!
  >r over node_slabel @ over node_slabel @ swap r@ burm_state
  0 regs_unused r> node
  tuck node_slabel !
  tuck node_left !
  tuck node_right ! ;

: uop ( node_addr op -- node_addr )
  assert( dup burm_arity@ 1 = ) \ !!
  >r dup node_slabel @ NIL r@ burm_state
  0 regs_unused r> node
  tuck node_slabel !
  tuck node_left ! ;

: terminal ( val reg op -- node_addr )
 dup NIL NIL rot burm_state >r
 assert( dup burm_arity@ 0= ) \ !! assert funktioniert nicht
 node
 r> over node_slabel ! ;

: lit ( n -- node_addr )
  dup 0= if
    0 I_LITS terminal \ !! warum 0 ?
  else dup $8000 >= if
    regs_unused I_LIT terminal
  else dup $0000 < if
    regs_unused I_LIT terminal
  else
    regs_unused I_LITS terminal endif endif endif ;

>target_compile
: literal ( x -- D: addr )
  lit >data ; immediate restrict
>source

: addr ( offset register -- node_addr )
  swap regs_unused I_LITS terminal
  NULL rot I_REG terminal
  I_PLUS op ;

: id@ ( offset register -- node_addr )
  addr I_FETCH uop ;

: id! ( node_addr offset register -- node_addr )
  addr swap I_STORE op ;

: inst_cover ( indent goal node_addr -- flag )
  dup hex.
  tuck burm_STATE_LABEL@ swap burm_rule dup 0= if
    nip nip
    ." no cover" cr
  else
    >r over 0 ?do
      ." ." loop
    ." ( " r@ . ." ) " r@ burm_string cr
    r> rot >r					\ (R: indent)
    dup burm_nts@ >r				\ (R: indent nts_addr)
    burm_kids r>
    begin					\ (R: indent)
      dup @ dup 0<> while
      rot r@ 1+ rot rot recurse drop
      cell+ repeat
    2drop r> endif ;

: burm_reduce ( goal node -- ... )
  \ if an ml is produced, the ... is an ml
  \ otherwise it can be anything (a constant, a register number ...)
  ( ." printReduce1" hex.s cr )
  assert( over 1 >= )
  assert( over MAX-NT < )
\  dup inst_print_node
  2dup node-nt-insts swap th @
  dup if ( goal node ml )
    1 over ml-count +!
    nip nip
    EXIT
  endif
  drop
  over >r
  tuck node_slabel @ swap burm_rule ( node rule )
  dup 0= if
    nip nip r> drop
    burm_assert" no cover" cr 
  else ( node rule )
    2dup 2>r
    dup burm_nts@ >r		( node rule; R: node rule nts-addr )
    depth >r burm_kids depth r> - 2 + \ !! fix this!
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
      r> dup @ dup 0<> while
      r> rot cell+ >r
      recurse
      repeat
    2drop
    2r>
    [burm_reduce] @ r> swap execute
  endif
  ( ." printReduce2" hex.s cr )
  ;


: inst_selection_func ( node_addr -- )
?trace $0020 [IF]
  ." inst_selection:" hex.s cr
[THEN]
?trace $0020 [IF]
  dup 0 burm_stmt_NT rot inst_cover drop
[THEN]
  burm_stmt_NT swap burm_reduce drop ;		\ reduce the tree

: inst_selection ( -- )
  ['] inst_selection_func 0 inst_btrees inst_sequence ;

\ register allocation
: alloc-reg ( ml -- )
\ allocate a register for the result of the ml, if necessary
  dup ml-reg @ regs_unused = if
    1 regs_get swap  ml-reg !
  else
    drop
  endif ;

: reg-freeable? ( reg -- f )
\ returns true if register is freeable
   assert( dup 0>= over regs_useable < and )
   1 swap lshift freeable-set and 0<> ;

: free-reg ( ml -- )
\ return the register to the free ones (except 0, which stands for "no
\ result register")
   ml-reg @ dup reg-freeable? if
	assert( dup 0> over regs_useable < and )
        regs_unused swap regs_data !
   else
        drop
   endif ;

: register_allocation_func ( ml -- )
  dup free-reg
  dup btree_left @ ?dup 0<> if
    alloc-reg endif
  btree_right @ ?dup 0<> if
    alloc-reg endif ;

: register_allocation ( -- )
  ['] register_allocation_func 0 inst_lists inst_sequence_reverse ;

: assemble_func ( ml -- )
\  dup print-ml
  dup dup ml-asm @ execute			\ assemble the instruction
  ?inst_delay if				\ fill delay slot ?
    NIL asm_nop endif ;

: assemble ( -- )
  ['] assemble_func 0 inst_lists inst_sequence ;

?test $0020 [IF]
cr ." Test for inst-selection.fs" cr

finish
[THEN]
