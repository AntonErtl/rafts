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

7 constant MAX-NT \ !! machine dependent

btree_struct
  1 cells: field node_op	\ operation (intermediate language)
  1 cells: field node_slabel	\ state label
  1 cells: field node_val	\ literal value (alias: node_offset)
  1 cells: field node_reg	\ used register
  1 cells: field node_depends	\ list of dependencies
  -1 cells: field node_unpad	\ next field starts at offset 1
  MAX-NT cells: field node-nt-insts \ for each nt, the ml for this node
end-struct node_struct

' btree_left alias node_left
' btree_right alias node_right
' node_val alias node_offset	\ for stack elements: offset (in bytes) from stack pointer at start of basic block

NIL inst constant node_depends_init

btree_struct \ !! machine dep: is it really a *binary* tree?
  1 cells: field ml-asm		\ xt of the assembler word for the instruction
  1 cells: field ml-count	\ use count (# of parents)
  1 cells: field ml-val		\ literal value
  1 cells: field ml-done	\ scheduling done flag
  1 cells: field ml-reg		\ used register
  1 cells: field ml-node-dependences \ dependences of ml's node
  1 cells: field ml-depends	\ list of dependencies
  1 cells: field ml-delay	\ true: create a delay slot nop for the instruction
  1 cells: field ml-cost	\ path length to end of bb
end-struct ml-struct

: flag. ( f -- )
 dup 0= if
  ." true " drop EXIT endif
 dup true = if
  ." false" drop EXIT endif
 hex. assert( FALSE ) ;

: inst_print_depends_func ( inst_addr -- )
  inst_node hex? ;

: inst_print_depends ( inst_addr -- )
  dup node_depends_init <> if
    ['] inst_print_depends_func swap slist_forall
  else
    drop ." no " endif ;

: ml-print-depends ( inst_addr -- )
  ['] inst_print_depends_func maplist ;

: print-ml ( ml -- )
  dup hex.
  dup btree_left @ hex.
  dup btree_right @ hex.
  dup ml-asm @ name.
  ." count=" dup ml-count @ .
  ." val=" dup ml-val @ .
  ." done=" dup ml-done @ flag.
  ." reg=" dup ml-reg @ .
  ." delay=" dup ml-delay @ flag.
  ." cost=" dup ml-cost @ .
  ." depends=" dup ml-depends @ ml-print-depends
  drop cr ;

: asm ( node_addr -- )
  drop ;

\ reset node values
: node_reset ( node_addr -- )
\  0 over node_slabel !
  -1 over node_reg !
  node_depends_init over node_depends !
  drop ;

\ allocate and initialize a node
: node ( val reg op -- node_addr )
  node_struct struct-allot	\ allocate
  btree
  dup node_reset
  dup node-nt-insts cell+ max-nt 1- cells erase \ !! or just reset the mls in the nts
  tuck node_op !		\ initial values
  tuck node_reg !
  tuck node_val ! ;

?test $0002 [IF]
cr ." Test for node.fs" cr

finish
[THEN]
