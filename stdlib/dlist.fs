\ $Id: dlist.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: dlist.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

\ data allocation and definitions
struct
  1 cells: field dlist_data
  1 cells: field dlist_prev
  1 cells: field dlist_next
end-struct dlist_struct

: dlist ( -- addr )
  dlist_struct struct-allot ;

\ init function
: dlist_init ( -- addr addr )
  dlist						\ allocate tail
  dup NIL swap dlist_data !
  dlist						\ allocate head
  dup NIL swap dlist_data !
  2dup 2dup dup dlist_prev NIL swap ! dlist_next !
  swap dup dlist_next NIL swap ! dlist_prev ! ;

\ insert an element
: dlist_insert_next ( x addr -- addr )
  dlist_next dup @ dup NIL = abort" dlist: insert not possible"
  dup dlist_prev dup @
  dlist						\ allocate (new)
  swap over dlist_prev !			\ set prev (new)
  dup rot !					\ set prev (next)
  swap over dlist_next !			\ set next (new)
  dup rot !					\ set next (prev)
  tuck dlist_data ! ;				\ set data (new)

: dlist_insert_prev ( x addr -- addr )
  dlist_prev @ dup NIL = abort" dlist: insert not possible"
  dlist_insert_next ;

\ deletes an element
: dlist_delete ( addr -- )
  dup NIL = abort" dlist: delete not possible"
  dup dlist_next @ NIL = abort" dlist: delete not possible"
  dup dlist_prev @ NIL = abort" dlist: delete not possible"
  dup dlist_next @ swap dlist_prev @
  2dup dlist_next !				\ set next (prev)
  swap dlist_prev ! ;				\ set prev (next)

: dlist_delete_prev ( addr -- )
  dlist_prev @ dlist_delete ;

: dlist_delete_next ( addr -- )
  dlist_next @ dlist_delete ;

\ exit function
: dlist_exit ( addr -- )
  begin
    dup dlist_next @ dlist_next @ NIL <> while
    dup dlist_delete_next repeat
  drop ;

\ executes a function for all elements
: dlist_forall ( xt addr -- )
  dlist_next @
  begin
    dup dlist_next @ NIL <> while
    2>r 2r@ swap execute 2r>
    dlist_next @ repeat
  2drop ;

\ executes a function for all elements until it is true
: dlist_find_next ( xt addr -- addr )
  dlist_next @
  begin
    dup dlist_next @ NIL <> while
    2>r 2r@ swap execute 2r> rot if
      nip exit endif
    dlist_next @ repeat
  2drop NIL ;

' dlist_find_next alias dlist_find ( xt addr -- addr )

: dlist_find_prev ( xt addr -- )
  dlist_next @
  begin
    dup dlist_next @ NIL <> while
    2>r 2r@ swap execute 2r> rot if
      nip exit endif
    dlist_next @ repeat
  2drop NIL ;

\ size function
: dlist_size_func ( u addr -- u )
  drop 1+ ;

: dlist_size ( addr -- u )
  0 swap ['] dlist_size_func swap dlist_forall ;

\ print function
: dlist_print_func ( addr -- )
  dlist_data @ . ;

: dlist_print ( addr -- )
  ['] dlist_print_func swap dlist_forall ;

?test $0800 [IF]
cr ." Test for dlist.fs" cr

variable dlist_head
variable dlist_tail

: dlist_foo1 ( addr -- )
  -100 swap dlist_data +! ;

." dlist_init: " dlist_init dlist_head ! dlist_tail ! .s cr

." dlist_forall: " ' dlist_foo1 dlist_head @ dlist_forall .s cr

." dlist_insert_next: " 123 dlist_head @ dlist_insert_next
  dup . dlist_data @ . .s cr
." dlist_insert_next: " 456 dlist_head @ dlist_insert_next
  dup . dlist_data @ . .s cr
." dlist_insert_prev: " 789 dlist_tail @ dlist_insert_prev
  dup . dlist_data @ . .s cr
." dlist_size: " dlist_head @ dlist_size . .s cr
." dlist_print: " dlist_head @ dlist_print .s cr

." dlist_forall: " ' dlist_foo1 dlist_head @ dlist_forall .s cr

." dlist_print: " dlist_head @ dlist_print .s cr

: dlist_foo2 ( addr -- flag )
  dlist_data @ 456 = ;
: dlist_foo02 ( -- )
  ." dlist_find: " ['] dlist_foo2 dlist_head @ dlist_find dup NIL <> if
    dlist_print_func else
    ." not found " drop endif
    .s cr ;
dlist_foo02

: dlist_foo3 ( addr -- flag )
  dlist_data @ 356 = ;
: dlist_foo03 ( -- )
  ." dlist_find_prev: " ['] dlist_foo3 dlist_head @ dlist_find_prev .s dup NIL <> if
    dlist_print_func else
    ." not found " drop endif
    .s cr ;
dlist_foo03

." dlist_delete: " dlist_head @ dlist_next @ dlist_next @ dup dlist_data @ .
  dlist_delete .s cr
." dlist_print: " dlist_head @ dlist_print .s cr
." dlist_delete_prev: " dlist_head @ dlist_next @ dup dlist_data @ . dlist_next @
  dlist_delete_prev .s cr
." dlist_size: " dlist_head @ dlist_size . .s cr
." dlist_print: " dlist_head @ dlist_print .s cr

." dlist_exit: " dlist_head @ dlist_exit .s cr

finish
[THEN]
