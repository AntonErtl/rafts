\ $Id: stdlib.fs,v 1.1 1995/10/06 18:12:54 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: stdlib.fs,v $
\ Revision 1.1  1995/10/06 18:12:54  anton
\ Initial revision
\

0 constant NIL
1 negate 1 rshift constant MAXC

0 constant NULL

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

: ? ( addr -- )
  @ . ;

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
  >name .name ;

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
  ." stack: " .s cr ;

include struct.fs
include stdlib/marray.fs
include stdlib/array.fs
include stdlib/slist.fs
\ include stdlib/dlist.fs
\ include stdlib/stack.fs
\ include stdlib/queue.fs

include stdlib/btree.fs

\ include stdlib/string.fs

?test $0100 [IF]
cr ." Test for stdlib.fs" cr

finish
[THEN]
