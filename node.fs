\ node.fs	node structur and words
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

btree_struct
  1 cells: field node_op	\ operation (intermediate language)
  1 cells: field node_slabel	\ state label

  1 cells: field node_val	\ literal value (alias: node_offset)
  1 cells: field node_count	\ use count (# of parents)
  1 cells: field node_done	\ scheduling done flag
  1 cells: field node_reg	\ used register
  1 cells: field node_depends	\ list of dependencies
  1 cells: field node_cost	\ path length to end of bb
  1 cells: field node_lval	\ left child instruction
  1 cells: field node_rval	\ right child instruction
  1 cells: field node_asm	\ xt of the assembler word for the instruction
\  1 cells: field node_copy	\ can point to a copy of the node, used for dealing with chain rules in the code selection grammar
  1 cells: field node_delay	\ true: create a delay slot nop for the instruction
end-struct node_struct

' btree_left alias node_left
' btree_right alias node_right
' node_val alias node_offset	\ for stack elements: offset (in bytes) from stack pointer at start of basic block

\ ' inst >body 96 over + disasm_dump
NIL inst constant node_depends_init

: asm ( node_addr -- )
  drop ;

\ reset node values
: node_reset ( node_addr -- )
\  0 over node_slabel !
  0 over node_count !
  false over node_done !
  -1 over node_reg !
  node_depends_init over node_depends !
  0 over node_cost !
  NIL over node_lval !
  NIL over node_rval !
  ['] asm over node_asm !
\  dup over node_copy !
  false over node_delay !
  drop ;

\ allocate and initial a node
: node ( val reg op -- node_addr )
  node_struct struct-allot	\ allocate
  btree
  dup node_reset
  tuck node_op !		\ initial values
  tuck node_reg !
  tuck node_val ! ;

: node_dup ( node_addr -- node_addr )
  0 0 0 node tuck node_struct drop move ;
\  dup node_copy @ over = if
\    0 0 0 node 2dup node_struct drop move
\    dup rot node_copy ! endif ;

: (count+) ( node_addr -- )
  node_count 1 swap +! ;

: (count-) ( node_addr -- )
  node_count -1 swap +! ;

: count+ ( node_addr -- node_addr )
  ['] (count+) over btree_postorder ;

: count- ( node_addr -- node_addr )
  ['] (count-) over btree_postorder ;

?test $0002 [IF]
cr ." Test for node.fs" cr

finish
[THEN]
