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
  1 cells: field node_op	\ operation
  1 cells: field node_slabel	\ state label

  1 cells: field node_val
  1 cells: field node_count	\ use count
  1 cells: field node_done	\ scheduling done flag
  1 cells: field node_reg	\ used register
  1 cells: field node_depends	\ list of dependencies
  1 cells: field node_cost
  1 cells: field node_lval
  1 cells: field node_rval
  1 cells: field node_asm
  1 cells: field node_copy
  1 cells: field node_delay
end-struct node_struct

['] btree_left alias node_left
['] btree_right alias node_right
['] node_val alias node_offset

NIL inst constant node_depends_init

: asm ( node_addr -- )
  drop ;

\ reset node values
: node_reset ( node_addr -- )
  0 over node_slabel !
  0 over node_count !
  false over node_done !
  -1 over node_reg !
  node_depends_init over node_depends !
  0 over node_cost !
  NIL over node_lval !
  NIL over node_rval !
  ['] asm over node_asm !
  dup over node_copy !
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
  dup node_copy @ over = if
    0 0 0 node 2dup node_struct drop move
    dup rot node_copy ! endif ;

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
