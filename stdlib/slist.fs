\ slist.fs	single linked list words
\
\ Copyright (C) 1995-97 Martin Anton Ertl, Christian Pirker
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
    cell% field slist-next
end-struct slist-struct

: slist ( slistp -- slistp )
\ init function
    NIL over slist-next ! ;

: slist-insert ( old new -- new )
\ insert an element
    tuck slist-next !  ;

: slist-forall ( xt addr -- )
\ executes a function for all elements
    begin
	dup NIL <>
    while
	2>r 2r@ swap [ 0 -1 wword-regs-adjust ] execute 2r>
	slist-next @
    repeat
    2drop ;

: slist-find ( xt addr -- addr )
\ executes a function for all elements until it is true
    begin
	dup NIL <>
    while
	2>r 2r@ swap [ 0 -1 wword-regs-adjust ] execute 2r> rot if
	    nip exit
	endif
	slist-next @
    repeat
    2drop NIL ;

: slist-size-func ( u addr -- u )
\ size function
    drop 1+ ;

: slist-size ( addr -- u )
    0 ['] slist-size-func rot slist-forall ;

: slist-print-func ( addr -- )
\ print function
    hex. ;

: slist-print ( addr -- )
    ['] slist-print-func swap slist-forall ;

?test $2000 [IF]
cr ." Test for slist.fs" cr

slist-struct
    cell% field sdata-value
end-struct sdata-struct

: sdata ( -- addr )
    sdata-struct ~~ %allot slist
    NIL over sdata-value ! ;

: sdata-init ( x -- addr )
    sdata
    tuck sdata-value ! ;

: sdata-print-func ( addr -- )
    ." ( " dup hex. ." ) " sdata-value ? ;

: sdata-print ( addr -- )
    ['] sdata-print-func swap slist-forall ;

variable sdata-head

." sdata: " NIL sdata-head ! .s cr

." slist-insert: " sdata-head @ 123 sdata-init slist-insert dup sdata-head !
sdata-print-func .s cr
." slist-insert: " sdata-head @ 456 sdata-init slist-insert dup sdata-head !
sdata-print-func .s cr
." slist-insert: " sdata-head @ 789 sdata-init slist-insert dup sdata-head !
sdata-print-func .s cr
." slist-size: " sdata-head @ slist-size . .s cr
." sdata-print: " sdata-head @ sdata-print .s cr

: sdata-foo1 ( addr -- )
    -100 swap sdata-value +! ;
." slist-forall: " ' sdata-foo1 sdata-head @ slist-forall .s cr
." sdata-print: " sdata-head @ sdata-print .s cr

: sdata-foo2 ( addr -- flag )
    sdata-value @ 456 = ;
: sdata-foo02 ( -- )
    ." slist-find: " ['] sdata-foo2 sdata-head @ slist-find dup NIL <> if
	sdata-print-func
    else
	." not found " drop
    endif
    .s cr ;
sdata-foo02

: sdata-foo3 ( addr -- flag )
    sdata-value @ 356 = ;
: sdata-foo03 ( -- )
    ." slist-find: " ['] sdata-foo3 sdata-head @ slist-find dup NIL <> if
	sdata-print-func
    else
	." not found " drop
    endif
    .s cr ;
sdata-foo03

finish
[THEN]
