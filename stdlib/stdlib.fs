\ stdlib.fs	misc words
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

0 constant NIL
0 constant NULL
1 negate 1 rshift constant MAXC

: th ( addr1 n -- addr2 )
    cells + ;

: ? ( addr -- )
\ prints the cell at addr
    @ . ;

: c? ( c-addr -- )
\ prints the char at addr
    c@ . ;

cell constant 1cells
2 cells constant 2cells
3 cells constant 3cells
4 cells constant 4cells
5 cells constant 5cells
cell negate constant -1cells
-2 cells constant -2cells
-3 cells constant -3cells
-4 cells constant -4cells
-5 cells constant -5cells

: char- ( addr -- addr )
\ decrements addr by one char
    [ 1 chars ] literal - ;

: cell- ( addr -- addr )
\ decrements addr by one cell
    1cells - ;

: hexn. ( n x -- )
    base @ >r hex
    s>d <# bl hold
    rot 0 ?do
	#
    loop
    [char] $ hold #>
    type
    r> base ! ;

: hex. ( x -- )
\ prints x in hex with 2*cell digits (e.g. $12345678)
    [ cell 2* ] literal swap hexn. ;

: hex? ( addr -- )
\ prints the cell at addr in hex with 2*cell digits
    @ hex. ;

: hex.s ( -- )
    ." <" depth 0 .r ." > "
    depth 0 max maxdepth-.s @ min dup 0 ?do
	dup i - pick hex.
    loop
    drop ;

' hex.s IS printdebugdata
: hex.rs ( -- )
    ." <R: " rp@ hex. ." > "
    rp@ cell+ dup maxdepth-.s @ cells + ?do
	i @ hex.
    -1 cells +loop ;

: name. ( cfa -- )
    look if
	.name
    else
	drop ." not defined"
    endif ;

: code. ( cfa n -- )
    over name.
    dump ;

: finish ( -- )
    ." stack: " hex.s cr ;

include stdlib/array.fs
include stdlib/matrix.fs
include stdlib/slist.fs
include stdlib/btree.fs

?test $0100 [IF]
cr ." Test for stdlib.fs" cr

finish
[THEN]
