\ btree.fs	binary tree words
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
  1 cells: field btree_left
  1 cells: field btree_right
end-struct btree_struct

: btree ( addr -- addr )
  NIL over btree_left !
  NIL over btree_right ! ;

\ executes a function for all elements
: btree_preorder ( xt addr -- )
  dup NIL <> if
    2>r 2r@ swap execute 2r>
    2>r 2r@ btree_left @ recurse 2r>
    2>r 2r@ btree_right @ recurse 2r>
    endif
  2drop ;

: btree_inorder ( xt addr -- )
  dup NIL <> if
    2>r 2r@ btree_left @ recurse 2r>
    2>r 2r@ swap execute 2r>
    2>r 2r@ btree_right @ recurse 2r>
    endif
  2drop ;

: btree_postorder ( xt addr -- )
  dup NIL <> if
    2>r 2r@ btree_left @ recurse 2r>
    2>r 2r@ btree_right @ recurse 2r>
    2>r 2r@ swap execute 2r>
    endif
  2drop ;

\ print function
: btree_print_func ( addr -- )
  hex. ;

: btree_print_preorder ( addr -- )
  ['] btree_print_func swap btree_preorder ;

: btree_print_inorder ( addr -- )
  ['] btree_print_func swap btree_inorder ;

: btree_print_postorder ( addr -- )
  ['] btree_print_func swap btree_postorder ;

?test $1000 [IF]
cr ." Test for btree.fs" cr

btree_struct
  1 cells: field bdata_value
end-struct bdata_struct

: bdata ( -- addr )
  bdata_struct struct-allot btree
  NIL over bdata_value ! ;

: bdata_init ( x -- addr )
  bdata
  tuck bdata_value ! ;

: bdata_print_func ( addr -- )
  ." ( " dup hex. ." ) " bdata_value @ emit space ;

: bdata_print ( addr -- )
  bdata_value @ emit space ;

: bdata_print_preorder ( addr -- )
  ['] bdata_print swap btree_preorder ;

: bdata_print_inorder ( addr -- )
  ['] bdata_print swap btree_inorder ;

: bdata_print_postorder ( addr -- )
  ['] bdata_print swap btree_postorder ;

variable bdata_root

." bdata_init: " char + bdata_init bdata_root ! .s cr
." bdata_init: " char * bdata_init bdata_root @ btree_left ! .s cr
." bdata_init: " char 3 bdata_init bdata_root @ btree_right ! .s cr
." bdata_init: " char 1 bdata_init bdata_root @ btree_left @ btree_left ! .s cr
." bdata_init: " char 2 bdata_init bdata_root @ btree_left @ btree_right ! .s cr

." bdata_print_preorder: " bdata_root @ bdata_print_preorder .s cr
." bdata_print_inorder: " bdata_root @ bdata_print_inorder .s cr
." bdata_print_postorder: " bdata_root @ bdata_print_postorder .s cr

: bdata_foo1_func ( u addr -- u )
  drop 1+ ;
: bdata_foo1 ( addr -- u )
  0 ['] bdata_foo1_func rot btree_postorder ;
." btree_postorder: " bdata_root @ bdata_foo1 . cr

: bdata_foo2_func ( addr u addr -- addr u )
  2>r dup NIL = 2r> rot if
    2dup bdata_value @ = if
      rot drop swap else
      drop endif else
    drop endif ;
: bdata_foo2 ( addr -- addr )
  NIL [char] r rot ['] bdata_foo2_func swap btree_postorder drop ;
: bdata_foo02 ( -- )
  ." btree_postorder: " bdata_root @ bdata_foo2 dup NIL <> if
    bdata_print_func else
    ." not found " drop endif
  .s cr ;
bdata_foo02

: bdata_foo3 ( addr -- addr )
  NIL [char] 1 rot ['] bdata_foo2_func swap btree_preorder drop ;
: bdata_foo03 ( -- )
  ." btree_preorder: " bdata_root @ bdata_foo3 dup NIL <> if
    bdata_print_func else
    ." not found " drop endif
  .s cr ;
bdata_foo03

finish
[THEN]
