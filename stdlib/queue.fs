\ $Id: queue.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: queue.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

\ data allocation and definitions
['] dlist_data alias queue_data
['] dlist_prev alias queue_prev
['] dlist_next alias queue_next

\ init function
: queue_init ( -- addr addr )
  dlist_init ;

\ put an element
: queue_put ( x addr addr -- addr )
  drop dlist_insert_prev ;

\ get an element
: queue_get ( addr addr -- x )
  nip dlist_next @ dup dlist_data @ swap
  dlist_delete ;

\ exit function
: queue_exit ( addr -- )
  dlist_exit ;

\ depth function
: queue_depth ( addr -- u )
  dlist_size ;

\ empty function
: queue_empty ( addr -- flag )
  queue_depth 0= ;

\ print function
: queue_print ( addr -- )
  dlist_print ;

?test $0800 [IF]
cr ." Test for queue.fs" cr

variable queue_head
variable queue_tail

." queue_init: " queue_init queue_head ! queue_tail ! .s cr

." queue_put: " 123 queue_tail @ queue_head @ queue_put
  dup . queue_data @ . .s cr
." queue_put: " 456 queue_tail @ queue_head @ queue_put
  dup . queue_data @ . .s cr
." queue_put: " 789 queue_tail @ queue_head @ queue_put
  dup . queue_data @ . .s cr
." queue_print: " queue_head @ queue_print .s cr

." queue_get: " queue_tail @ queue_head @ dup queue_next @ queue_data @ .
  queue_get . .s cr
." queue_get: " queue_tail @ queue_head @ dup queue_next @ queue_data @ .
  queue_get . .s cr
." queue_depth: " queue_head @ queue_depth . .s cr
." queue_empty: " queue_head @ queue_empty . false . .s cr
." queue_print: " queue_head @ queue_print .s cr
." queue_get: " queue_tail @ queue_head @ dup queue_next @ queue_data @ .
  queue_get . .s cr
." queue_depth: " queue_head @ queue_depth . .s cr
." queue_empty: " queue_head @ queue_empty . true . .s cr
." queue_print: " queue_head @ queue_print .s cr

." queue_exit: " queue_head @ queue_exit .s cr

finish
[THEN]
