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
    1 cells: field btree-left
    1 cells: field btree-right
end-struct btree-struct

: btree ( addr -- addr )
    NIL over btree-left !
    NIL over btree-right ! ;

: btree-preorder ( xt addr -- )
\ executes a function for all elements
    dup NIL <> if
	2>r 2r@ swap [ 0 -1 wword-regs-adjust ] execute 2r>
	2>r 2r@ btree-left @ [ 0 -2 wword-regs-adjust ] recurse 2r>
	2>r 2r@ btree-right @ [ 0 -2 wword-regs-adjust ] recurse 2r>
    endif
    2drop ;

: btree-inorder ( xt addr -- )
    dup NIL <> if
	2>r 2r@ btree-left @ [ 0 -2 wword-regs-adjust ] recurse 2r>
	2>r 2r@ swap [ 0 -1 wword-regs-adjust ] execute 2r>
	2>r 2r@ btree-right @ [ 0 -2 wword-regs-adjust ] recurse 2r>
    endif
    2drop ;

: btree-postorder ( xt addr -- )
    dup NIL <> if
	2>r 2r@ btree-left @ [ 0 -2 wword-regs-adjust ] recurse 2r>
	2>r 2r@ btree-right @ [ 0 -2 wword-regs-adjust ] recurse 2r>
	2>r 2r@ swap [ 0 -1 wword-regs-adjust ] execute 2r>
    endif
    2drop ;

: btree-print-func ( addr -- )
\ print function
    hex. ;

: btree-print-preorder ( addr -- )
    ['] btree-print-func swap btree-preorder ;

: btree-print-inorder ( addr -- )
    ['] btree-print-func swap btree-inorder ;

: btree-print-postorder ( addr -- )
    ['] btree-print-func swap btree-postorder ;

?test $1000 [IF]
cr ." Test for btree.fs" cr

btree-struct
    1 cells: field bdata-value
end-struct bdata-struct

: bdata ( -- addr )
    bdata-struct struct-allot btree
    NIL over bdata-value ! ;

: bdata-init ( x -- addr )
    bdata
    tuck bdata-value ! ;

: bdata-print-func ( addr -- )
    ." ( " dup hex. ." ) " bdata-value @ emit space ;

: bdata-print ( addr -- )
    bdata-value @ emit space ;

: bdata-print-preorder ( addr -- )
    ['] bdata-print swap btree-preorder ;

: bdata-print-inorder ( addr -- )
    ['] bdata-print swap btree-inorder ;

: bdata-print-postorder ( addr -- )
    ['] bdata-print swap btree-postorder ;

variable bdata-root

." bdata-init: " char + bdata-init bdata-root ! .s cr
." bdata-init: " char * bdata-init bdata-root @ btree-left ! .s cr
." bdata-init: " char 3 bdata-init bdata-root @ btree-right ! .s cr
." bdata-init: " char 1 bdata-init bdata-root @ btree-left @ btree-left ! .s cr
." bdata-init: " char 2 bdata-init bdata-root @ btree-left @ btree-right ! .s cr

." bdata-print-preorder: " bdata-root @ bdata-print-preorder .s cr
." bdata-print-inorder: " bdata-root @ bdata-print-inorder .s cr
." bdata-print-postorder: " bdata-root @ bdata-print-postorder .s cr

: bdata-foo1-func ( u addr -- u )
    drop 1+ ;
: bdata-foo1 ( addr -- u )
    0 ['] bdata-foo1-func rot btree-postorder ;
." btree-postorder: " bdata-root @ bdata-foo1 . cr

: bdata-foo2-func ( addr u addr -- addr u )
    2>r dup NIL = 2r> rot if
	2dup bdata-value @ = if
	    rot drop swap
	else
	    drop
	endif
    else
	drop
    endif ;
: bdata-foo2 ( addr -- addr )
    NIL [char] r rot ['] bdata-foo2-func swap btree-postorder drop ;
: bdata-foo02 ( -- )
    ." btree-postorder: " bdata-root @ bdata-foo2 dup NIL <> if
	bdata-print-func
    else
	." not found " drop
    endif
    .s cr ;
bdata-foo02

: bdata-foo3 ( addr -- addr )
    NIL [char] 1 rot ['] bdata-foo2-func swap btree-preorder drop ;
: bdata-foo03 ( -- )
    ." btree-preorder: " bdata-root @ bdata-foo3 dup NIL <> if
	bdata-print-func
    else
	." not found " drop
    endif
    .s cr ;
bdata-foo03

finish
[THEN]
