\ slist.fs	single linked list words
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
  1 cells: field slist_next
end-struct slist_struct

\ init function
: slist ( slistp -- slistp )
  \ initialize slist node
  NIL over slist_next ! ;

\ insert an element
: slist_insert ( new old -- new )
  \ inserts new node right after old node
  slist_next dup @ rot ( old old-next new )
  tuck slist_next !				\ set next (new)
  tuck swap ! ;					\ set next (old)

\ deletes an element
: slist_delete ( slist-node -- )
  \ delete the node after slist-node
  slist_next dup @ slist_next @ swap ! ;	\ set next

\ exit function
: slist_exit ( addr -- )
  begin
    dup slist_next @ NIL <> while
    dup slist_delete repeat
  drop ;

: maplist ( addr xt -- )
  \ xt must be of the form ( x1 ... xi slist -- y1 ... yi )
  swap
  begin
    dup NIL <> while
    2>r 2r@ swap execute 2r>
    slist_next @ repeat
  2drop ;

\ executes a function for all elements
: slist_forall ( xt addr -- )
  \ xt must be of the form ( x1 ... xi slist -- y1 ... yi )
  slist_next @ swap maplist ;

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
  0 ['] slist_size_func rot slist_forall ;

\ print function
: slist_print_func ( addr -- )
  hex. ;

: slist_print ( addr -- )
  ['] slist_print_func swap slist_forall ;

?test $0800 [IF]
cr ." Test for slist.fs" cr

slist_struct
  1 cells: field sdata_value
end-struct sdata_struct

: sdata ( -- addr )
  sdata_struct struct-allot slist
  NIL over sdata_value ! ;

: sdata_init ( x -- addr )
  sdata
  tuck sdata_value ! ;

: sdata_print_func ( addr -- )
  ." ( " dup hex. ." ) " sdata_value ? ;

: sdata_print ( addr -- )
  ['] sdata_print_func swap slist_forall ;

variable sdata_head

." sdata: " sdata sdata_head ! .s cr

." slist_insert: " 123 sdata_init sdata_head @ slist_insert
  sdata_print_func .s cr
." slist_insert: " 456 sdata_init sdata_head @ slist_insert
  sdata_print_func .s cr
." slist_insert: " 789 sdata_init sdata_head @ slist_next @ slist_insert
  sdata_print_func .s cr
." slist_size: " sdata_head @ slist_size . .s cr
." sdata_print: " sdata_head @ sdata_print .s cr

: sdata_foo1 ( addr -- )
  -100 swap sdata_value +! ;
." slist_forall: " ' sdata_foo1 sdata_head @ slist_forall .s cr
." sdata_print: " sdata_head @ sdata_print .s cr

: sdata_foo2 ( addr -- flag )
  sdata_value @ 456 = ;
: sdata_foo02 ( -- )
  ." slist_find: " ['] sdata_foo2 sdata_head @ slist_find dup NIL <> if
    sdata_print_func else
    ." not found " drop endif
    .s cr ;
sdata_foo02

: sdata_foo3 ( addr -- flag )
  sdata_value @ 356 = ;
: sdata_foo03 ( -- )
  ." slist_find: " ['] sdata_foo3 sdata_head @ slist_find dup NIL <> if
    sdata_print_func else
    ." not found " drop endif
    .s cr ;
sdata_foo03

." slist_delete: " sdata_head @ slist_next @ dup slist_next @ sdata_print_func
  slist_delete .s cr
." slist_size: " sdata_head @ slist_size . .s cr
." sdata_print: " sdata_head @ sdata_print .s cr

." slist_exit: " sdata_head @ slist_exit .s cr
." sdata_print: " sdata_head @ sdata_print .s cr

finish
[THEN]
