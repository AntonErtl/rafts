\ $Id: node.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: node.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

struct
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

slist_init constant node_depends_init

\ reset node values
: node_reset ( addr -- addr )
  true over node_done !
  0 over node_link !
  0 over node_reg !
  node_depends_init over node_depends ! ;

\ allocate and initial a node
: node ( offset val inst type -- addr )
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
