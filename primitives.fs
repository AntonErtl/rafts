\ primitives.fs	primitive words
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

: ['Forth] ( "name" -- )
  postpone vForth postpone ['] postpone vsource ; immediate restrict

>target_compile

\ stack primitives
: drop ( D: x -- )
  data>
  count- vForth drop vsource ; immediate restrict

: dup ( D: x -- x x )
  data>
  vForth dup vsource count+
  >data >data ; immediate restrict

: over ( D: x1 x2 -- x1 x2 x1 )
  data> data>
  vForth dup vsource count+
  >data vForth swap vsource >data >data ; immediate restrict

: rot ( D: x1 x2 x3 -- x2 x3 x1 )
  data> data> data>
  vForth rot rot vsource
  >data >data >data ; immediate restrict

: swap ( D: x1 x2 -- x2 x1 )
  data> data>
  vForth swap vsource
  >data >data ; immediate restrict

: pick ( D: xu ... x1 x0 u -- xu ... x1 x0 xu )
  data> dup node_op @ dup I_LITS = swap I_LIT = or if
    node_val @ #data@ count+ >data else
    >data ['Forth] pick compile, endif ; immediate restrict

: roll ( D: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
  data> dup node_op @ dup I_LITS = swap I_LIT = or if
    node_val @ dup #data@
    ds_tos@ rot ds_data dup cell+ rot cells move
    data> drop >data else
    >data ['Forth] roll compile, endif ; immediate restrict

: 2drop ( D: x1 x2 -- )
  vtarget_compile postpone drop postpone drop vsource ; immediate restrict

: 2dup ( D: x1 x2 -- x1 x2 x1 x2 )
  vtarget_compile postpone over postpone over vsource ; immediate restrict

: 2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
  vtarget_compile 3 postpone literal postpone pick 3 postpone literal postpone pick vsource ; immediate restrict

: 2rot ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
  vtarget_compile 5 postpone literal postpone roll 5 postpone literal postpone roll vsource ; immediate restrict

: 2swap ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
  vtarget_compile 3 postpone literal postpone roll 3 postpone literal postpone roll vsource ; immediate restrict

: ?dup ( D: x -- 0 | x x )
  data> dup node_op @ dup I_LITS = swap I_LIT = or if
    dup node_val @ 0<> if
      vForth dup vsource count+ >data >data else
      >data endif else
    >data ['Forth] ?dup compile, endif ; immediate restrict

: nip ( D: x1 x2 -- x2 )
  vtarget_compile postpone swap postpone drop vsource ; immediate restrict

: tuck ( D: x1 x2 -- x2 x1 x2 )
  vtarget_compile postpone swap postpone over vsource ; immediate restrict

\ primitives
>source
: binop-primitive ( op -- )
    create , immediate restrict
does> ( D: addr addr -- addr )
    data> data> rot @ op >data ; 

: unop-primitive ( op -- )
    create , immediate restrict
does> ( D: addr -- addr )
    data> swap @ uop >data ; 
>target_compile

I_PLUS	binop-primitive +
I_MINUS	binop-primitive -
I_TIMES binop-primitive *
I_SLASH binop-primitive /
I_MOD	binop-primitive mod
I_LSHIFT binop-primitive lshift
I_LSHIFT binop-primitive lshifta
I_RSHIFT binop-primitive rshift
I_SRSHIFT binop-primitive rshifta
I_AND	binop-primitive and
I_OR	binop-primitive or
I_XOR	binop-primitive xor

I_NEGATE unop-primitive negate
I_INVERT unop-primitive invert

\ I_ binop-primitive 

: 1+ ( D: addr -- addr )
  vtarget_compile 1 postpone literal postpone + vsource ; immediate restrict

: 1- ( D: addr -- addr )
  vtarget_compile -1 postpone literal postpone + vsource ; immediate restrict

: 2* ( D: addr -- addr )
  vtarget_compile 1 postpone literal postpone lshifta vsource ; immediate restrict

: 2/ ( D: addr -- addr )
  vtarget_compile 1 postpone literal postpone rshifta vsource ; immediate restrict

: = ( D: addr addr -- addr )
  data> data> I_XOR op
  vtarget_compile 1 postpone literal vsource data> swap I_ULESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: <> ( D: addr addr -- addr )
  data> data> I_XOR op
  vtarget_compile 0 postpone literal vsource data> I_ULESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: < ( D: addr addr -- addr )
  data> data> I_LESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: <= ( D: addr addr -- addr )
  data> data> swap I_LESS op
  vtarget_compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: > ( D: addr addr -- addr )
  data> data> swap I_LESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: >= ( D: addr addr -- addr )
  data> data> I_LESS op
  vtarget_compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: 0= ( D: addr -- addr )
  data> vtarget_compile 1 postpone literal vsource data> swap I_ULESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0<> ( D: addr -- addr )
  data> vtarget_compile 0 postpone literal vsource data> I_ULESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0< ( D: addr -- addr )
  data> vtarget_compile 0 postpone literal vsource data> swap I_LESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0<= ( D: addr -- addr )
  data> vtarget_compile 0 postpone literal vsource data> I_LESS op
  vtarget_compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: 0> ( D: addr -- addr )
  data> vtarget_compile 0 postpone literal vsource data> I_LESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0>= ( D: addr -- addr )
  data> vtarget_compile 0 postpone literal vsource data> swap I_LESS op
  vtarget_compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: u< ( D: addr addr -- addr )
  data> data> I_ULESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: u<= ( D: addr addr -- addr )
  data> data> swap I_ULESS op
  vtarget_compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: u> ( D: addr addr -- addr )
  data> data> swap I_ULESS op
  vtarget_compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: u>= ( D: addr addr -- addr )
  data> data> I_ULESS op
  vtarget_compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: cells ( D: addr -- addr )
  vtarget_compile 2 postpone literal postpone lshift vsource ; immediate restrict

: cell+ ( D: addr -- addr )
  vtarget_compile 4 postpone literal postpone + vsource ; immediate restrict

: cell- ( D: addr -- addr )
  vtarget_compile 4 postpone literal postpone - vsource ; immediate restrict

: chars ( D: addr -- addr )
  ; immediate restrict

: char+ ( D: addr -- addr )
  vtarget_compile postpone 1+ vsource ; immediate restrict

: char- ( D: addr -- addr )
  vtarget_compile postpone 1- vsource ; immediate restrict

: @ ( D: addr -- addr )
  data> I_FETCH uop
  true over node_delay !
  inst_!_list @ over node_depends !
  dup inst inst_@_list @ slist_insert drop
  >data ; immediate restrict

: ! ( D: addr addr -- )
  data> data> I_STORE op
  inst_@_list @ over node_depends !
  NIL inst inst_!_list !
  dup inst inst_!_list @ slist_insert drop
  NIL inst inst_@_list !
  dup inst inst_@_list @ slist_insert drop
  inst_btrees_insert ; immediate restrict

: 2@ ( D: addr -- addr addr )
  vtarget_compile postpone dup postpone cell+ postpone @ postpone swap postpone @ vsource ; immediate restrict

: 2! ( D: addr addr addr -- )
  vtarget_compile postpone swap postpone over postpone ! postpone cell+ postpone ! vsource ; immediate restrict

: c@ ( D: addr -- addr )
  data> I_CFETCH uop
  true over node_delay !
  inst_!_list @ over node_depends !
  dup inst inst_@_list @ slist_insert drop
  >data ; immediate restrict

: c! ( D: addr addr -- )
  data> data> I_CSTORE op
  inst_@_list @ over node_depends !
  NIL inst inst_!_list !
  dup inst inst_!_list @ slist_insert drop
  NIL inst inst_@_list !
  inst_btrees_insert ; immediate restrict

: >r ( R: -- addr )
  data> >return ; immediate restrict

: r@ ( R: -- addr )
  0 #return@ count+ >data ; immediate restrict

: r> ( R: -- addr )
  return> >data ; immediate restrict

: 2>r ( R: addr addr -- )
  vtarget_compile postpone swap postpone >r postpone >r vsource ; immediate restrict

: 2r@ ( R: -- addr addr )
  vtarget_compile postpone r> postpone r@ postpone over postpone >r postpone swap vsource ; immediate restrict

: 2r> ( R: -- addr addr )
  vtarget_compile postpone r> postpone r> postpone swap vsource ; immediate restrict

: rdrop ( R: addr -- )
  return> drop ; immediate restrict

: ['] ( "name" -- )
  ' vtarget_compile postpone literal vsource ; immediate restrict

: [char] ( 'char' -- n )
  char vtarget_compile postpone literal vsource ; immediate restrict

>source
\ Variablen fuer Stringbehandlung
$2000 constant text_size	\ 16 kByte Textbuffer
create text_data
  here cell+ ,
  text_size allot

: text_print ( -- )
  text_data dup @ over - dump ;

: ," ( "string"<"> -- n )
  text_data @ dup
  [char] " parse
  rot 2dup + char+ dup aligned swap over swap ?do
    bl i c! loop
  2dup text_data !
  2>r place
  2r> tuck - flush-icache ;

: (.")
  count type ;
: (s")
  count ;
: (abort")
  "error ! if
    -2 throw endif ;
>target_compile

: ." ( "string"<"> -- )
  ," vtarget_compile postpone literal vsource
  ['] (.") compile, ; immediate restrict

: s" ( "string"<"> -- )
  ," vtarget_compile postpone literal vsource
  ['] (s") compile, ; immediate restrict

: abort" ( "string"<"> -- )
  ," vtarget_compile postpone literal vsource
  ['] (abort") compile, ; immediate restrict

: ( ( -- )
  postpone ( ; immediate restrict

: \ ( -- )
  postpone \ ; immediate restrict

>source

: (dostruc) ( addr u - addr )
  vtarget_compile postpone + vsource ; immediate restrict
comp' (dostruc) drop dostruc !

?test $0004 [IF]
cr ." Test for primitives.fs" cr

finish
[THEN]
