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
  1 cells: field node_type	\ kind of node
  1 cells: field node_inst	\ instruction values
  1 cells: field node_val
  1 cells: field node_offset
  1 cells: field node_done	\ scheduling done flag
  1 cells: field node_link	\ link count
  1 cells: field node_reg	\ used register
  1 cells: field node_depends	\ list of dependencies
end-struct node_struct

['] node_val alias node_pointer

NIL inst constant node_depends_init

\ reset node values
: node_reset ( node-addr -- node-addr )
  btree
  true over node_done !
  0 over node_link !
  0 over node_reg !
  node_depends_init over node_depends ! ;

\ allocate and initial a node
: node ( offset val inst type -- node-addr )
  node_struct struct-allot	\ allocate
  tuck node_type !		\ initial values
  tuck node_inst !
  tuck node_val !
  tuck node_offset !
  node_reset ;

?test $0002 [IF]
cr ." Test for node.fs" cr

finish
[THEN]
