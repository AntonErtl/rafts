\ dlist.fs	double linked list words
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

\ data allocation and definitions
struct
  1 cells: field dlist_prev
  1 cells: field dlist_next
end-struct dlist_struct

\ init function
: dlist ( addr -- addr )
  NIL over dlist_prev !
  NIL over dlist_next ! ;

: dlist_init ( addr addr -- addr addr )
  2dup 2dup dup dlist_prev NIL swap ! dlist_next !
  swap dup dlist_next NIL swap ! dlist_prev ! ;

\ insert an element
: dlist_insert_next ( new-addr old-addr -- new-addr )
  swap >r
  dlist_next dup @ dup NIL = abort" dlist: insert not possible"
  dup dlist_prev dup @
  \ hex.s cr
  \ dlist						\ allocate (new)
  r@ dlist_prev !			\ set prev (new)
  r@ swap !					\ set prev (next)
  r@ dlist_next !			\ set next (new)
  r@ swap !					\ set next (prev)
  r> ;

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
  hex. ;

: dlist_print ( addr -- )
  ['] dlist_print_func swap dlist_forall ;

?test $0800 [IF]
cr ." Test for dlist.fs" cr

dlist_struct
  1 cells: field ddata_value
end-struct ddata_struct

: ddata ( -- addr )
  ddata_struct struct-allot dlist
  NIL over ddata_value ! ;

: ddata_init ( x -- addr )
  ddata
  tuck ddata_value ! ;

: ddata_print_func ( addr -- )
  ." ( " dup hex. ." ) " ddata_value ? ;

: ddata_print ( addr -- )
  ['] ddata_print_func swap dlist_forall ;

variable ddata_head
variable ddata_tail

: ddata_foo1 ( addr -- )
  -100 swap ddata_value +! ;

." dlist_init: " ddata ddata dlist_init ddata_head ! ddata_tail ! .s cr

." dlist_forall: " ' ddata_foo1 ddata_head @ dlist_forall .s cr

." dlist_insert_next: " 123 ddata_init ddata_head @ dlist_insert_next
  ddata_print_func .s cr
." dlist_insert_next: " 456 ddata_init ddata_head @ dlist_insert_next
  ddata_print_func .s cr
." dlist_insert_prev: " 789 ddata_init ddata_tail @ dlist_insert_prev
  ddata_print_func .s cr
." dlist_size: " ddata_head @ dlist_size . .s cr
." ddata_print: " ddata_head @ ddata_print .s cr

." dlist_forall: " ' ddata_foo1 ddata_head @ dlist_forall .s cr

." ddata_print: " ddata_head @ ddata_print .s cr

: ddata_foo2 ( addr -- flag )
  ddata_value @ 456 = ;
: ddata_foo02 ( -- )
  ." dlist_find: " ['] ddata_foo2 ddata_head @ dlist_find dup NIL <> if
    ddata_print_func else
    ." not found " drop endif
    .s cr ;
ddata_foo02

: ddata_foo3 ( addr -- flag )
  ddata_value @ 356 = ;
: ddata_foo03 ( -- )
  ." dlist_find_prev: " ['] ddata_foo3 ddata_head @ dlist_find_prev dup NIL <> if
    ddata_print_func else
    ." not found " drop endif
    .s cr ;
ddata_foo03

." dlist_delete: " ddata_head @ dlist_next @ dlist_next @ dup ddata_print_func
  dlist_delete .s cr
." ddata_print: " ddata_head @ ddata_print .s cr
." dlist_delete_prev: " ddata_head @ dlist_next @ dup ddata_print_func dlist_next @
  dlist_delete_prev .s cr
." dlist_size: " ddata_head @ dlist_size . .s cr
." ddata_print: " ddata_head @ ddata_print .s cr

." dlist_exit: " ddata_head @ dlist_exit .s cr
." ddata_print: " ddata_head @ ddata_print .s cr

finish
[THEN]
