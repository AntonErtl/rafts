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

>target-compile

\ stack primitives
: drop ( D: x -- )
    data>
    vForth drop vsource ; immediate restrict

: dup ( D: x -- x x )
    data>
    vForth dup vsource
    >data >data ; immediate restrict

: over ( D: x1 x2 -- x1 x2 x1 )
    data> data>
    vForth dup vsource
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
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ #data@ >data
    else
	>data ['Forth] pick compile,
    endif ; immediate restrict

: roll ( D: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ dup #data@
	ds-tos @ rot ds-data dup cell+ rot cells move
	data> drop >data
    else
	>data ['Forth] roll compile,
    endif ; immediate restrict

: 2drop ( D: x1 x2 -- )
    vtarget-compile postpone drop postpone drop vsource ; immediate restrict

: 2dup ( D: x1 x2 -- x1 x2 x1 x2 )
    vtarget-compile postpone over postpone over vsource ; immediate restrict

: 2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
    vtarget-compile 3 postpone literal postpone pick 3 postpone literal postpone pick vsource ; immediate restrict

: 2rot ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
    vtarget-compile 5 postpone literal postpone roll 5 postpone literal postpone roll vsource ; immediate restrict

: 2swap ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
    vtarget-compile 3 postpone literal postpone roll 3 postpone literal postpone roll vsource ; immediate restrict

: ?dup ( D: x -- 0 | x x )
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	dup il-val @ 0<> if
	    vForth dup vsource >data >data
	else
	    >data
	endif
    else
	>data ['Forth] ?dup compile,
    endif ; immediate restrict

: nip ( D: x1 x2 -- x2 )
    vtarget-compile postpone swap postpone drop vsource ; immediate restrict

: tuck ( D: x1 x2 -- x2 x1 x2 )
    vtarget-compile postpone swap postpone over vsource ; immediate restrict

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
>target-compile

\ I_ binop-primitive
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

\ I_ unop-primitive
I_NEGATE unop-primitive negate
I_INVERT unop-primitive invert

: 1+ ( D: addr -- addr )
    vtarget-compile 1 postpone literal postpone + vsource ; immediate restrict

: 1- ( D: addr -- addr )
    vtarget-compile -1 postpone literal postpone + vsource ; immediate restrict

: 2* ( D: addr -- addr )
    vtarget-compile 1 postpone literal postpone lshifta vsource ; immediate restrict

: 2/ ( D: addr -- addr )
    vtarget-compile 1 postpone literal postpone rshifta vsource ; immediate restrict

: = ( D: addr addr -- addr )
    data> data> I_XOR op
    vtarget-compile 1 postpone literal vsource data> swap I_ULESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: <> ( D: addr addr -- addr )
    data> data> I_XOR op
    vtarget-compile 0 postpone literal vsource data> I_ULESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: < ( D: addr addr -- addr )
    data> data> I_LESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: <= ( D: addr addr -- addr )
    data> data> swap I_LESS op
    vtarget-compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: > ( D: addr addr -- addr )
    data> data> swap I_LESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: >= ( D: addr addr -- addr )
    data> data> I_LESS op
    vtarget-compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: 0= ( D: addr -- addr )
    data> vtarget-compile 1 postpone literal vsource data> swap I_ULESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0<> ( D: addr -- addr )
    data> vtarget-compile 0 postpone literal vsource data> I_ULESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0< ( D: addr -- addr )
    data> vtarget-compile 0 postpone literal vsource data> swap I_LESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0<= ( D: addr -- addr )
    data> vtarget-compile 0 postpone literal vsource data> I_LESS op
    vtarget-compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: 0> ( D: addr -- addr )
    data> vtarget-compile 0 postpone literal vsource data> I_LESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: 0>= ( D: addr -- addr )
    data> vtarget-compile 0 postpone literal vsource data> swap I_LESS op
    vtarget-compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: u< ( D: addr addr -- addr )
    data> data> I_ULESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: u<= ( D: addr addr -- addr )
    data> data> swap I_ULESS op
    vtarget-compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: u> ( D: addr addr -- addr )
    data> data> swap I_ULESS op
    vtarget-compile 0 postpone literal vsource data> I_MINUS op >data ; immediate restrict

: u>= ( D: addr addr -- addr )
    data> data> I_ULESS op
    vtarget-compile -1 postpone literal vsource data> I_PLUS op >data ; immediate restrict

: cells ( D: addr -- addr )
    vtarget-compile 2 postpone literal postpone lshift vsource ; immediate restrict

: cell+ ( D: addr -- addr )
    vtarget-compile 4 postpone literal postpone + vsource ; immediate restrict

: cell- ( D: addr -- addr )
    vtarget-compile 4 postpone literal postpone - vsource ; immediate restrict

: chars ( D: addr -- addr ) ; immediate restrict

: char+ ( D: addr -- addr )
    vtarget-compile postpone 1+ vsource ; immediate restrict

: char- ( D: addr -- addr )
    vtarget-compile postpone 1- vsource ; immediate restrict

: @ ( D: addr -- addr )
    data> I_FETCH uop
    inst-!-list @ over il-depends !
    dup inst inst-@-list @ slist-insert drop
    >data ; immediate restrict

: ! ( D: addr addr -- )
    data> data> I_STORE op
    inst-@-list @ over il-depends !
    NIL inst inst-!-list !
    dup inst inst-!-list @ slist-insert drop
    NIL inst inst-@-list !
    dup inst inst-@-list @ slist-insert drop
    inst-btrees-insert ; immediate restrict

: 2@ ( D: addr -- addr addr )
    vtarget-compile postpone dup postpone cell+ postpone @ postpone swap postpone @ vsource ; immediate restrict

: 2! ( D: addr addr addr -- )
    vtarget-compile postpone swap postpone over postpone ! postpone cell+ postpone ! vsource ; immediate restrict

: c@ ( D: addr -- addr )
    data> I_CFETCH uop
    inst-!-list @ over il-depends !
    dup inst inst-@-list @ slist-insert drop
    >data ; immediate restrict

: c! ( D: addr addr -- )
    data> data> I_CSTORE op
    inst-@-list @ over il-depends !
    NIL inst inst-!-list !
    dup inst inst-!-list @ slist-insert drop
    NIL inst inst-@-list !
    inst-btrees-insert ; immediate restrict

: >r ( R: -- addr )
    data> >return ; immediate restrict

: r@ ( R: -- addr )
    0 #return@ >data ; immediate restrict

: r> ( R: -- addr )
    return> >data ; immediate restrict

: 2>r ( R: addr addr -- )
    vtarget-compile postpone swap postpone >r postpone >r vsource ; immediate restrict

: 2r@ ( R: -- addr addr )
    vtarget-compile postpone r> postpone r@ postpone over postpone >r postpone swap vsource ; immediate restrict

: 2r> ( R: -- addr addr )
    vtarget-compile postpone r> postpone r> postpone swap vsource ; immediate restrict

: rdrop ( R: addr -- )
    return> drop ; immediate restrict

: ['] ( "name" -- )
    ' vtarget-compile postpone literal vsource ; immediate restrict

: [char] ( 'char' -- n )
    char vtarget-compile postpone literal vsource ; immediate restrict

>source
\ Variablen fuer Stringbehandlung
$2000 constant text-size	\ 16 kByte Textbuffer
create text-data
    here cell+ ,
    text-size allot

: text-print ( -- )
    text-data dup @ over - dump ;

: ," ( "string"<"> -- addr )
    text-data @ dup
    [char] " parse
    rot 2dup + char+ dup aligned swap over swap ?do
	bl i c!
    loop
    2dup text-data !
    2>r place
    2r> tuck - flush-icache ;

: (.")
    count type ;
: (s")
    count ;
: (abort")
    "error ! if
        -2 throw
    endif ;
>target-compile

: ." ( "string"<"> -- )
    ," vtarget-compile postpone literal vsource
    ['] (.") compile, ; immediate restrict

: s" ( "string"<"> -- )
    ," vtarget-compile postpone literal vsource
    ['] (s") compile, ; immediate restrict

: abort" ( "string"<"> -- )
    ," vtarget-compile postpone literal vsource
    ['] (abort") compile, ; immediate restrict

: ( ( -- )
    postpone ( ; immediate restrict

: \ ( -- )
    postpone \ ; immediate restrict

>source

: (dostruc) ( addr u - addr )
    vtarget-compile postpone + vsource ; immediate restrict
comp' (dostruc) drop dostruc !

: nothing ( -- ) ;		\ a unknown bugfix
: sourcepos, ( -- )
    sourceline# vtarget-compile postpone literal vsource
    loadfilename# @ vtarget-compile postpone literal vsource ;
: print-sourcepos ( n n -- )
    2* cells included-files nothing 2@ drop + 2@ type
    ." :" 0 .r ;
: (~~) ( -- )
    cr print-sourcepos ." :"
    printdebugdata cr ;
>target-compile

: ~~ ( -- )
    sourcepos,
    ['] (~~) compile, ; immediate restrict

>source
: assertn ( -- )
    assert-level @ > if
	postpone (
    endif ;
: (endassert) ( -- )
    rot if
	2drop
    else
	cr print-sourcepos ." : failed assertion"
	true abort" assertion failed"
    endif ;
>target-compile

: assert0( ( -- )
    0 assertn ; immediate restrict
: assert1( ( -- )
    1 assertn ; immediate restrict
: assert2( ( -- )
    2 assertn ; immediate restrict
: assert3( ( -- )
    3 assertn ; immediate restrict
: assert( ( -- )
    vtarget-compile postpone assert1( vsource ; immediate restrict
: ) ( -- )
    sourcepos,
    ['] (endassert) compile, ; immediate restrict

>source

?test $0004 [IF]
cr ." Test for primitives.fs" cr

finish
[THEN]
