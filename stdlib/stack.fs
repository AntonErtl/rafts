\ $Id: stack.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: stack.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

\ data allocation and definitions
['] slist_data alias stack_data
['] slist_next alias stack_next

\ init function
: stack_init ( -- addr )
  slist_init ;

\ push an element
: stack_push ( x addr -- )
  slist_insert drop ;

\ pop an element
: stack_pop ( addr -- x )
  dup slist_next @ slist_data @ swap
  slist_delete ;

\ exit function
: stack_exit ( addr -- )
  slist_exit ;

\ depth function
: stack_depth ( addr -- u )
  slist_size ;

\ empty function
: stack_empty ( addr -- flag )
  stack_depth 0= ;

\ print function
: stack_print ( addr -- )
  slist_print ;

?test $0800 [IF]
cr ." Test for stack.fs" cr

variable stack_head

." stack_init: " stack_init stack_head ! .s cr

." stack_push: " 123 stack_head @ stack_push
  stack_head @ stack_next @ dup . stack_data @ . .s cr
." stack_push: " 456 stack_head @ stack_push
  stack_head @ stack_next @ dup . stack_data @ . .s cr
." stack_push: " 789 stack_head @ stack_push
  stack_head @ stack_next @ dup . stack_data @ . .s cr
." stack_print: " stack_head @ stack_print .s cr

." stack_pop: " stack_head @ dup stack_next @ stack_data @ .
  stack_pop . .s cr
." stack_pop: " stack_head @ dup stack_next @ stack_data @ .
  stack_pop . .s cr
." stack_depth: " stack_head @ stack_depth . .s cr
." stack_empty: " stack_head @ stack_empty . false . .s cr
." stack_print: " stack_head @ stack_print .s cr
." stack_pop: " stack_head @ dup stack_next @ stack_data @ .
  stack_pop . .s cr
." stack_depth: " stack_head @ stack_depth . .s cr
." stack_empty: " stack_head @ stack_empty . true . .s cr
." stack_print: " stack_head @ stack_print .s cr

." stack_exit: " stack_head @ stack_exit .s cr

finish
[THEN]
