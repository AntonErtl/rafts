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

: ?inst_delay ( node_addr -- flag )
  node_delay @ ;

: inst_done ( node_addr -- )
  true swap node_done ! ;

: inst_notdone ( node_addr -- )
  false swap node_done ! ;

: ?inst_done ( node_addr -- flag )
  node_done @ ;

: ?inst_notdone ( node_addr -- flag )
  node_done @ invert ;

: inst_sequence ( xt addr -- )
  swap over inst_size 1- cells + >r begin
    over @ ?dup 0<> if
      over execute endif
    swap cell+ swap over r@ > until
  rdrop 2drop ;

: inst_insert ( node_addr addr -- )
  begin
    dup @ 0<> while
    cell+ repeat
  ! ;

: inst_insert_end ( node_addr addr -- )
  inst_size 1- cells +
  begin
    dup @ 0<> while
    cell- repeat
  ! ;

: inst_btrees_insert ( node_addr -- )
  0 inst_btrees inst_insert ;
: inst_btrees_insert_end ( node_addr -- )
  0 inst_btrees inst_insert_end ;

: inst_lists_insert ( node_addr -- )
  0 inst_lists inst_insert ;
: inst_lists_insert_end ( node_addr -- )
  0 inst_lists inst_insert_end ;

: inst_nodes_insert ( node_addr -- )
  0 inst_nodes inst_insert ;
: inst_nodes_insert_end ( node_addr -- )
  0 inst_nodes inst_insert_end ;

: inst_pnodes_insert ( node_addr -- )
  0 inst_pnodes inst_insert ;
: inst_pnodes_insert_end ( node_addr -- )
  0 inst_pnodes inst_insert_end ;

: inst_ok ( left_node right_node asm_xt node_addr -- node_addr )
  tuck node_asm !
  tuck node_rval !
  tuck node_lval !
  dup inst_nodes_insert_end ;

: (asm_lval@) ( node_addr -- node_addr )
  node_lval @ ;
: (asm_rval@) ( node_addr -- node_addr )
  node_rval @ ;
: asm_val@ ( node_addr -- register )
  node_val @ ;
: asm_lval@ ( node_addr -- val )
  (asm_lval@) node_val @ ;
: asm_rval@ ( node_addr -- val )
  (asm_rval@) node_val @ ;
: asm_reg@ ( node_addr -- register )
  node_reg @ ;
: asm_lreg@ ( node_addr -- register )
  (asm_lval@) node_reg @ ;
: asm_rreg@ ( node_addr -- register )
  (asm_rval@) node_reg @ ;

: asm_nop ( node_addr -- )
  drop @nop ;

: asm_lit ( node_addr -- )
  dup asm_reg@ swap asm_lval@ @li drop ;

: asm_addr ( node_addr -- )
  drop ;

: get_addr ( node_addr -- imm register )
  dup node_right @ node_val @ swap node_left @ node_reg @ ;

: asm_fetchc ( node_addr -- )
  dup asm_reg@ swap (asm_lval@) get_addr @lbu drop ;

: asm_fetchregc ( node_addr -- )
  dup asm_reg@ swap asm_lreg@ 0 swap @lbu drop ;

: asm_fetchi ( node_addr -- )
  dup asm_reg@ swap (asm_lval@) get_addr @lw drop ;

: asm_fetchregi ( node_addr -- )
  dup asm_reg@ swap asm_lreg@ 0 swap @lw drop ;

: asm_storec ( node_addr -- )
  dup asm_lreg@ swap (asm_rval@) get_addr @sb ;

: asm_storeregc ( node_addr -- )
  dup asm_lreg@ swap asm_rreg@ 0 swap @sb ;

: asm_storei ( node_addr -- )
  dup asm_lreg@ swap (asm_rval@) get_addr @sw ;

: asm_storeregi ( node_addr -- )
  dup asm_lreg@ swap asm_rreg@ 0 swap @sw ;

: asm_add ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @add drop ;

: asm_addi ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @addi drop ;

: asm_sub ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @sub drop ;

: asm_mul ( node_addr -- )
  dup asm_lreg@ over asm_rreg@ @mult asm_reg@ @mflo drop ;

: asm_mull ( node_addr -- )
  asm_reg@ @mflo drop ;

: asm_mulh ( node_addr -- )
  asm_reg@ @mfhi drop ;

: asm_div ( node_addr -- )
  dup asm_lreg@ swap asm_rreg@ @div ;

: asm_divl ( node_addr -- )
  asm_reg@ @mflo drop ;

: asm_divh ( node_addr -- )
  asm_reg@ @mfhi drop ;

: asm_neg ( node_addr -- )
  dup asm_reg@ swap asm_lreg@ @neg drop ;

: asm_abs ( node_addr -- )
  dup asm_reg@ swap asm_lreg@ @abs drop ;

: asm_and ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @and drop ;

: asm_andi ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @andi drop ;

: asm_or ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @or drop ;

: asm_ori ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @ori drop ;

: asm_xor ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @xor drop ;

: asm_xori ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @xori drop ;

: asm_not ( node_addr -- )
  dup asm_reg@ swap asm_lreg@ @not drop ;

: asm_lsh ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @sllv drop ;

: asm_lshi ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @sll drop ;

: asm_rshu ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @srlv drop ;

: asm_rsh ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @srav drop ;

: asm_rshui ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @srl drop ;

: asm_rshi ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @sra drop ;

: asm_eq ( node_addr -- )
  dup asm_lreg@ swap dup asm_rreg@ swap asm_val@
  here - cell- @beq ;

: asm_slt ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @slt drop ;

: asm_slti ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @slti drop ;

: asm_sltu ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rreg@ @sltu drop ;

: asm_sltui ( node_addr -- )
  dup asm_reg@ swap dup asm_lreg@ swap asm_rval@ @sltiu drop ;

include grammar.fs

: inst_print_depends_func ( inst_addr -- )
  inst_node hex? ;

: inst_print_depends ( inst_addr -- )
  dup node_depends_init <> if
    ['] inst_print_depends_func swap slist_forall else
    drop ." no " endif ;

: inst_print_node ( node_addr -- )
  ." { "
  dup hex.
  dup node_left@ hex.
  dup node_right@ hex.
  dup node_lval @ hex.
  dup node_rval @ hex.
\  dup node_copy @ hex.
  dup node_asm @ hex.

  dup burm_OP_LABEL@ burm_opname
  dup node_slabel hex?
  dup node_val hex?
  ." reg:" dup node_reg ?
  ." done:" dup node_done ?
  ." count:" dup node_count ?
  ." cost:" dup node_cost ?
  ." delay:" dup node_delay ?
  ." depends:" dup node_depends @ inst_print_depends
  drop

  ." }" cr ;

: inst_print_nodes ( node_addr -- )
  ['] inst_print_node swap btree_postorder cr ;

: inst_btrees_print ( -- )
  ['] inst_print_nodes 0 inst_btrees inst_sequence ;

: inst_lists_print ( -- )
  ['] inst_print_node 0 inst_lists inst_sequence ;

: inst_nodes_print ( -- )
  ['] inst_print_node 0 inst_nodes inst_sequence ;

: inst_pnodes_print ( -- )
  ['] inst_print_node 0 inst_pnodes inst_sequence ;

NULL regs_unused NOP node ' asm_nop over node_asm ! constant inst_nop

: op ( node_addr node_addr op -- node_addr )
\  assert( dup burm_arity@ 2 = ) \ !!
  >r over node_slabel @ over node_slabel @ swap r@ burm_state
  0 regs_unused r> node
  tuck node_slabel !
  tuck node_left !
  tuck node_right ! ;

: uop ( node_addr op -- node_addr )
\  assert( dup burm_arity@ 1 = ) \ !!
  >r dup node_slabel @ NIL r@ burm_state
  0 regs_unused r> node
  tuck node_slabel !
  tuck node_left ! ;

: terminal ( val reg op -- node_addr )
 dup NIL NIL rot burm_state >r
\ assert( dup burm_arity@ 0= ) \ !! assert funktioniert nicht
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
    regs_unused I_LITS terminal endif endif endif
  dup inst_done ;

>target_compile
: literal ( x -- D: addr )
  lit >data ; immediate restrict
>source

: addr ( offset register -- node_addr )
  swap regs_unused I_LITS terminal dup inst_done
  NULL rot I_REG terminal dup inst_done
  I_PLUS op
  dup inst_done ;

: id@ ( offset register -- node_addr )
  addr I_FETCH uop ;

: id! ( node_addr offset register -- node_addr )
  addr swap I_STORE op ;

: inst_cover ( indent goal node_addr -- flag )
  dup hex.
  tuck burm_STATE_LABEL@ swap burm_rule dup 0= if
    nip nip
    ." no cover" cr else
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

: inst_cost ( cost goal node_addr -- )
  \ ." INST_COST:" hex.s cr
  tuck burm_STATE_LABEL@ swap burm_rule dup 0= if
    drop 2drop else
    dup 0 burm_cost@ >r rot r> + >r
    over node_cost r@ swap ! 
    dup burm_nts@ >r				\ (R: cost nts_addr)
    over swap burm_kids r>
    begin					\ (R: cost)
      dup @ dup 0<> while
      rot r@ rot rot recurse
      cell+ repeat
    rdrop drop 2drop endif ;

: inst_selection_func ( node_addr -- )
?trace $0020 [IF]
  ." inst_selection:" hex.s cr
[THEN]
\  dup burm_label drop				\ label the tree
?trace $0020 [IF]
  dup 0 burm_stmt_NT rot inst_cover drop
[THEN]
  dup 0 burm_stmt_NT rot inst_cost		\ calculate the costs
  burm_stmt_NT swap burm_reduce ;		\ reduce the tree

: inst_selection ( -- )
  ['] inst_selection_func 0 inst_btrees inst_sequence ;

: register_free ( node_addr -- )
  node_reg @ dup regs_unused <> if
    regs_dec else
    drop endif ;

: register_allocation_func ( node_addr -- )
  dup node_lval @ ?dup 0<> if			\ free possible left register
    register_free endif
  dup node_rval @ ?dup 0<> if			\ free possible right register
    register_free endif
  dup node_reg @ regs_unused <> if		\ check if already set
    drop else
    dup node_count @ regs_get			\ take a new register
    swap node_reg ! endif ;

: register_allocation ( -- )
  ['] register_allocation_func 0 inst_lists inst_sequence ;

: assemble_func ( node_addr -- )
  dup dup node_asm @ execute			\ assemble the instruction
  ?inst_delay if				\ fill delay slot ?
    NIL asm_nop endif ;

: assemble ( -- )
  ['] assemble_func 0 inst_lists inst_sequence ;

?test $0020 [IF]
cr ." Test for inst-selection.fs" cr

finish
[THEN]
