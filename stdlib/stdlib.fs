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
1 negate 1 rshift constant MAXC

0 constant NULL

: ? ( addr -- )
  @ . ;

: c? ( c-addr -- )
  c@ . ;

: ndup ( xn ... x1 n -- )
  dup 0 ?do
    dup pick swap loop
  drop ;

: ndrop ( xn ... x1 n -- )
  0 ?do
    drop loop ;

: 4dup ( d1 d2 -- d1 d2 d1 d2 )
  2over 2over ;

: char- ( addr -- addr )
  1 chars - ;

: cell- ( addr -- addr )
  1 cells - ;

: hexn. ( n x -- )
  base @ rot rot hex
  <# s>d bl hold rot 0 ?do
    # loop
  [char] x hold [char] 0 hold #>
  type base ! ;

: hex. ( x -- )
  8 swap hexn. ;

: hex? ( addr -- )
  @ hex. ;

: hex.s ( -- )
  ." <" depth 0 .r ." > "
  depth 0 max maxdepth-.s @ min dup 0 ?do
    dup i - pick hex. loop
  drop ;

: hex.rs ( -- )
  ." <R: " rp@ hex. ." > "
  rp@ cell+ dup maxdepth-.s @ cells + swap ?do
    i @ hex. 1 cells +loop ;

: name. ( cfa -- )
  look if
    .name else
    ." not defined" endif ;

: code. ( cfa n -- )
  over name.
  dump ;

: list ( wid -- )
  ." Vocabulary: " dup name. dup hex. cr
  >body begin
    @ dup 0<> while
    dup .name
    dup cell+ c@
    over name> ." ( " hex. ." )"
    dup $20 and if
      ." [imm]" endif
    $40 and if
      ." [rest]" endif
    space repeat
  drop cr ;

: finish ( -- )
  ." stack: " hex.s cr ;

\ include struct.fs
include stdlib/marray.fs
include stdlib/array.fs
include stdlib/slist.fs
\ include stdlib/stack.fs
\ include stdlib/dlist.fs
\ include stdlib/queue.fs

include stdlib/btree.fs

\ include stdlib/string.fs

?test $0100 [IF]
cr ." Test for stdlib.fs" cr

finish
bye
[THEN]
