\ $Id: btree.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: btree.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

\ data allocation and definitions
struct
  1 cells: field btree_data
  1 cells: field btree_left
  1 cells: field btree_right
end-struct btree_struct

: btree ( x -- addr )
  btree_struct struct-allot
  tuck btree_data !
  dup btree_left NIL swap !
  dup btree_right NIL swap ! ;

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
  btree_data @ emit space ;

: btree_print_preorder ( addr -- )
  ['] btree_print_func swap btree_preorder ;

: btree_print_inorder ( addr -- )
  ['] btree_print_func swap btree_inorder ;

: btree_print_postorder ( addr -- )
  ['] btree_print_func swap btree_postorder ;

?test $1000 [IF]
cr ." Test for btree.fs" cr

variable btree_root

." btree: " char + btree btree_root ! .s cr
." btree: " char * btree btree_root @ btree_left ! .s cr
." btree: " char 3 btree btree_root @ btree_right ! .s cr
." btree: " char 1 btree btree_root @ btree_left @ btree_left ! .s cr
." btree: " char 2 btree btree_root @ btree_left @ btree_right ! .s cr

." btree_print_preorder: " btree_root @ btree_print_preorder .s cr
." btree_print_inorder: " btree_root @ btree_print_inorder .s cr
." btree_print_postorder: " btree_root @ btree_print_postorder .s cr

: btree_foo1_func ( u addr -- u )
  drop 1+ ;
: btree_foo1 ( addr -- u )
  0 ['] btree_foo1_func rot btree_postorder ;
." btree_postorder: " btree_root @ btree_foo1 . cr

: btree_foo2_func ( addr u addr -- addr u )
  2>r dup NIL = 2r> rot if
    2dup btree_data @ = if
      rot drop swap else
      drop endif else
    drop endif ;
: btree_foo2 ( addr -- addr )
  NIL [char] r rot ['] btree_foo2_func swap btree_postorder drop ;
: btree_foo02 ( -- )
  ." btree_postorder: " btree_root @ btree_foo2 dup NIL <> if
    btree_print_func else
    ." not found " drop endif
  .s cr ;
btree_foo02

: btree_foo3 ( addr -- u )
  NIL [char] 1 rot ['] btree_foo2_func swap btree_preorder drop ;
: btree_foo03 ( -- )
  ." btree_preorder: " btree_root @ btree_foo3 dup NIL <> if
    btree_print_func else
    ." not found " drop endif
  .s cr ;
btree_foo03

finish
[THEN]
