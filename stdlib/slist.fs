\ $Id: slist.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: slist.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

\ data allocation and definitions
struct
  1 cells: field slist_data
  1 cells: field slist_next
end-struct slist_struct

: slist ( -- addr )
  slist_struct struct-allot ;

\ init function
: slist_init ( -- addr )
  slist						\ allocate head
  dup NIL swap slist_data !
  dup NIL swap slist_next ! ;

\ insert an element
: slist_insert ( x addr -- addr )
  slist_next dup @ swap
  slist						\ allocate (new)
  dup rot !					\ set next
  tuck slist_next !				\ set next (new)
  tuck slist_data ! ;				\ set data (new)

\ deletes an element
: slist_delete ( addr -- )
  slist_next dup @ slist_next @ swap ! ;	\ set next

\ exit function
: slist_exit ( addr -- )
  begin
    dup slist_next @ NIL <> while
    dup slist_delete repeat
  drop ;

\ executes a function for all elements
: slist_forall ( xt addr -- )
  slist_next @
  begin
    dup NIL <> while
    2>r 2r@ swap execute 2r>
    slist_next @ repeat
  2drop ;

\ executes a function for all elements until it is true
: slist_find ( xt addr -- addr )
  slist_next @
  begin
    dup NIL <> while
    2>r 2r@ swap execute 2r> rot if
      nip exit endif
    slist_next @ repeat
  2drop NIL ;

\ size function
: slist_size_func ( u addr -- u )
  drop 1+ ;

: slist_size ( addr -- u )
  0 swap ['] slist_size_func swap slist_forall ;

\ print function
: slist_print_func ( addr -- )
  slist_data @ . ;

: slist_print ( addr -- )
  ['] slist_print_func swap slist_forall ;

?test $0800 [IF]
cr ." Test for slist.fs" cr

variable slist_head

." slist_init: " slist_init slist_head ! .s cr

." slist_insert: " 123 slist_head @ slist_insert
  dup . slist_data @ . .s cr
." slist_insert: " 456 slist_head @ slist_insert
  dup . slist_data @ . .s cr
." slist_insert: " 789 slist_head @ slist_next @ slist_insert
  dup . slist_data @ . .s cr
." slist_size: " slist_head @ slist_size . .s cr
." slist_print: " slist_head @ slist_print .s cr

: slist_foo1 ( addr -- )
  -100 swap slist_data +! ;
." slist_forall: " ' slist_foo1 slist_head @ slist_forall .s cr
." slist_print: " slist_head @ slist_print .s cr

: slist_foo2 ( addr -- flag )
  slist_data @ 456 = ;
: slist_foo02 ( -- )
  ." slist_find: " ['] slist_foo2 slist_head @ slist_find dup NIL <> if
    slist_print_func else
    ." not found " drop endif
    .s cr ;
slist_foo02

: slist_foo3 ( addr -- flag )
  slist_data @ 356 = ;
: slist_foo03 ( -- )
  ." slist_find: " ['] slist_foo3 slist_head @ slist_find dup NIL <> if
    slist_print_func else
    ." not found " drop endif
    .s cr ;
slist_foo03

." slist_delete: " slist_head @ slist_next @ dup slist_next @ slist_data @ .
  slist_delete .s cr
." slist_size: " slist_head @ slist_size . .s cr
." slist_print: " slist_head @ slist_print .s cr

." slist_exit: " slist_head @ slist_exit .s cr

finish
[THEN]
