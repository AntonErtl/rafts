\ primitives.fs	primitive words
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

>target

>source
: compile,-literal ( x -- D: addr )
    lit >data ; compile-only
>target

: literal ( x -- )
    1 0 word-good word-regs-adjust
    [ also Forth ' lit previous ] literal gforth-compile, ,
    [comp'] compile,-literal drop gforth-compile, ; immediate compile-only

\ stack primitives
>source
also vtarget also Forth
word-good 1 -1 :word pick
word-good 1 -1 :word roll
\ word-bad 1 -1 :word dup?
previous previous

: compile-drop					( addr -- ) ( D: x1 -- )
    drop
    data> drop ;
: compile-dup					( addr -- ) ( D: x1 -- x1 x1 )
    drop
    data> dup >data >data ;
: compile-swap					( addr -- ) ( D: x1 x2 -- x2 x1 )
    drop
    data> data> swap >data >data ;
: compile-over					( addr -- ) ( D: x1 x2 -- x1 x2 x1 )
    drop
    data> data> dup >data swap >data >data ;
: compile-rot					( addr -- ) ( D: x1 x2 x3 -- x2 x3 x1 )
    drop
    data> data> data> rot rot >data >data >data ;
: compile--rot					( addr -- ) ( D: x1 x2 x3 -- x2 x3 x1 )
    drop
    0 compile-rot 0 compile-rot ;
: compile-nip					( addr -- ) ( D: x1 x2 -- x2 ) 
    drop
    0 compile-swap 0 compile-drop ;
: compile-tuck					( addr -- ) ( D: x1 x2 -- x2 x1 x2 )
    drop
    0 compile-swap 0 compile-over ;
: compile-pick					( addr -- ) ( D: xu ... x1 x0 u -- xu ... x1 x0 xu )
    drop
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ cells ds-tos #stack@ >data
    else
	>data vsource ['] pick compile,-now
    endif ;
: compile-roll					( addr -- ) ( D: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
    drop
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ dup cells ds-tos #stack@
	ds-tos @ rot cells ds-data + dup cell+ rot move
	data> drop >data
    else
	>data vsource ['] roll compile,-now
    endif ;

false [IF]
: compile-?dups					 ( addr -- ) ( D: x -- 0 | x x )
    drop
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	dup il-val @ if
	    dup
	    >data >data
	else
	    >data
	endif
    else
	>data ['] ?dup compile,
    endif ;
[THEN]

>target

0 -1 ' compile-drop	:: drop			( x -- )
2 -1 ' compile-dup	:: dup			( x -- x x )
2 -2 ' compile-swap	:: swap			( x1 x2 -- x2 x1 )
3 -2 ' compile-over	:: over			( x1 x2 -- x1 x2 x1 )
3 -3 ' compile-rot	:: rot			( x1 x2 x3 -- x2 x3 x1 )
3 -3 ' compile--rot	:: -rot			( x1 x2 x3 -- x2 x3 x1 )
3 -2 ' compile-tuck	:: tuck			( x1 x2 -- x2 x1 x2 )
1 -2 ' compile-nip	:: nip			( x1 x2 -- x1 )
1 -1 ' compile-pick	:: pick			( xu ... x1 x0 u -- xu ... x1 x0 xu )
1 -1 ' compile-roll	:: roll			( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
false [IF]
0 -1 ' compile-?dup	:: ?dup			( x1 x2 -- x2 x1 )
[THEN]

\ primitives
>source
: binop-prim ( op -- )
    create a,
does> ( addr -- ) ( D: addr addr -- addr )
    nip
    data> data> rot @ op >data ;

: unop-prim ( op -- )
    create a,
does> ( addr -- ) ( D: addr -- addr )
    nip
    data> swap @ uop >data ;

I_PLUS		binop-prim compile-+		( addr -- ) ( D: x1 x2 -- x3 )
I_MINUS		binop-prim compile--		( addr -- ) ( D: x1 x2 -- x3 )
I_TIMES		binop-prim compile-*		( addr -- ) ( D: x1 x2 -- x3 )
I_SLASH		binop-prim compile-/		( addr -- ) ( D: x1 x2 -- x3 )
I_MOD		binop-prim compile-mod		( addr -- ) ( D: x1 x2 -- x3 )
I_LSHIFT	binop-prim compile-lshift	( addr -- ) ( D: x1 x2 -- x3 )
I_LSHIFT	binop-prim compile-lshifta	( addr -- ) ( D: x1 x2 -- x3 )
I_RSHIFT	binop-prim compile-rshift	( addr -- ) ( D: x1 x2 -- x3 )
I_SRSHIFT	binop-prim compile-rshifta	( addr -- ) ( D: x1 x2 -- x3 )
I_AND		binop-prim compile-and		( addr -- ) ( D: x1 x2 -- x3 )
I_OR		binop-prim compile-or		( addr -- ) ( D: x1 x2 -- x3 )
I_XOR		binop-prim compile-xor		( addr -- ) ( D: x1 x2 -- x3 )

I_NEGATE	unop-prim  compile-negate	( addr -- ) ( D: x1 -- x2 )
I_INVERT	unop-prim  compile-invert	( addr -- ) ( D: x1 -- x2 )
>target

\ I_ binop-primitive
1 -2 ' compile-+	:: +			( n1|u1 n2|u2 -- n3|u3 )
1 -2 ' compile--	:: -			( n1|u1 n2|u2 -- n3|u3 )
1 -2 ' compile-*	:: *			( n1|u1 n2|u2 -- n3|u3 )
1 -2 ' compile-/	:: /			( n1 n2 -- n3 )
1 -2 ' compile-mod	:: mod			( n1 n2 -- n3 )
1 -2 ' compile-lshift	:: lshift		( x1 u -- x2 )
1 -2 ' compile-lshifta	:: lshifta		( x1 u -- x2 )
1 -2 ' compile-rshift	:: rshift		( x1 u -- x2 )
1 -2 ' compile-rshifta	:: rshifta		( x1 u -- x2 )
1 -2 ' compile-and	:: and			( x1 x2 -- x3 )
1 -2 ' compile-or	:: or			( x1 x2 -- x3 )
1 -2 ' compile-xor	:: xor			( x1 x2 -- x3 )

\ I_ unop-primitive
1 -1 ' compile-negate	:: negate		( n1 -- n2 )
1 -1 ' compile-invert	:: invert		( x1 -- x2 )

>source
: compile-=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_EQUALS op >data ;
: compile-<>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_EQUALS op I_INVERT uop >data ;
: compile-<					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_LESS op >data ;
: compile-<=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_LESS op I_INVERT uop >data ;
: compile->					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_LESS op >data ;
: compile->=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_LESS op I_INVERT uop >data ;
: compile-0=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit I_EQUALS op >data ;
: compile-0<>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit I_EQUALS op I_INVERT uop >data ;
: compile-0<					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit swap I_LESS op >data ;
: compile-0<=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit I_LESS op I_INVERT uop >data ;
: compile-0>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit I_LESS op >data ;
: compile-0>=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit swap I_LESS op I_INVERT uop >data ;
: compile-u<					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_ULESS op >data ;
: compile-u<=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_ULESS op I_INVERT uop >data ;
: compile-u>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_ULESS op >data ;
: compile-u>=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_ULESS op I_INVERT uop >data ;
>target

1 -2 ' compile-=	:: =			( x1 x2 -- flag )
1 -2 ' compile-<>	:: <>			( x1 x2 -- flag )
1 -2 ' compile-<	:: <			( n1 n2 -- flag )
1 -2 ' compile-<=	:: <=			( n1 n2 -- flag )
1 -2 ' compile->	:: >			( n1 n2 -- flag )
1 -2 ' compile->=	:: >=			( n1 n2 -- flag )
1 -1 ' compile-0=	:: 0=			( x -- flag )
1 -1 ' compile-0<>	:: 0<>			( x -- flag )
1 -1 ' compile-0<	:: 0<			( n -- flag )
1 -1 ' compile-0<=	:: 0<=			( n -- flag )
1 -1 ' compile-0>	:: 0>			( n -- flag )
1 -1 ' compile-0>=	:: 0>=			( n -- flag )
1 -2 ' compile-u<	:: u<			( u1 u2 -- flag )
1 -2 ' compile-u<=	:: u<=			( u1 u2 -- flag )
1 -2 ' compile-u>	:: u>			( u1 u2 -- flag )
1 -2 ' compile-u>=	:: u>=			( u1 u2 -- flag )

>source
: compile-1+					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile-+ ;
: compile-1-					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile-- ;
: compile-2*					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile-lshift ;
: compile-2/					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile-rshift ;
: compile-cell					( addr -- ) ( D: -- x1 )
    drop
    4 compile,-literal ;
: compile-cells					( addr -- ) ( D: x1 -- x2 )
    drop
    2 compile,-literal 0 compile-lshift ;
: compile-cell+					( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile-cell 0 compile-+ ;
: compile-cell-					( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile-cell 0 compile-- ;
: compile-chars					( addr -- ) ( D: x1 -- x2 )
    drop ;
: compile-char+					( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile-1+ ;
: compile-char-					( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile-1- ;
>target

1 -1 ' compile-1+	:: 1+			( n1 -- n2 )
1 -1 ' compile-1-	:: 1-			( n1 -- n2 )
1 -1 ' compile-2*	:: 2*			( n1 -- n2 )
1 -1 ' compile-2/	:: 2/			( n1 -- n2 )
1  0 ' compile-cell	:: cell			( -- n1 )
1 -1 ' compile-cells	:: cells		( n1 -- n2 )
1 -1 ' compile-cell+	:: cell+		( addr1 -- addr2 )
1 -1 ' compile-cell-	:: cell-		( addr1 -- addr2 )
1 -1 ' compile-chars	:: chars		( n1 -- n2 )
1 -1 ' compile-char+	:: char+		( addr1 -- addr2 )
1 -1 ' compile-char-	:: char-		( addr1 -- addr2 )

>source
: compile-@					( addr -- ) ( D: x1 -- x2 )
    drop
    data> I_FETCH uop
    inst-!-list @ over il-depends !
    inst-@-list @ over inst slist-insert inst-@-list !
    >data ;
: compile-!					( addr -- ) ( D: x1 x2 -- )
    drop
    data> data> I_STORE op
    inst-@-list @ over il-depends !
    dup inst inst-!-list !
    dup inst inst-@-list !
    inst-ils-insert ;
: compile-2@					( addr -- ) ( D: x1 -- x2 x3 )
    drop
    0 compile-dup 0 compile-cell+ 0 compile-@ 0 compile-swap 0 compile-@ ;
: compile-2!					( addr -- ) ( D: x1 x2 x3 -- )
    drop
    0 compile-tuck 0 compile-! 0 compile-cell+ 0 compile-! ;
: compile-+!					( addr -- ) ( D: x1 x2 -- )
    drop
    0 compile-tuck 0 compile-@ 0 compile-+
    0 compile-swap 0 compile-! ;
: compile--!					( addr -- ) ( D: x1 x2 -- )
    drop
    0 compile-tuck 0 compile-@ 0 compile--
    0 compile-swap 0 compile-! ;
: compile-c@					( addr -- ) ( D: x1 -- x2 )
    drop
    data> I_CFETCH uop
    inst-!-list @ over il-depends !
    inst-@-list @ over inst slist-insert inst-@-list !
    >data ;
: compile-c!					( addr -- ) ( D: x1 x2 -- )
    drop
    data> data> I_CSTORE op
    inst-@-list @ over il-depends !
    dup inst inst-!-list !
    dup inst inst-@-list !
    inst-ils-insert ;
>target

1 -1 ' compile-@	:: @			( addr -- x )
0 -2 ' compile-!	:: !			( x addr -- )
2 -1 ' compile-2@	:: 2@			( addr -- x1 x2 )
0 -3 ' compile-2!	:: 2!			( x1 x2 addr -- )
0 -2 ' compile-+!	:: +!			( x addr -- )
0 -2 ' compile--!	:: -!			( x addr -- )
1 -1 ' compile-c@	:: c@			( addr -- x )
0 -2 ' compile-c!	:: c!			( x addr -- )

>source
: compile->r					( addr -- ) ( D: x -- ) ( R: -- x )
    drop
    data> >return ;
: compile-r@					( addr -- ) ( D: -- x ) ( R: x -- x )
    drop
    0 rs-tos #stack@ >data ;
: compile-r>					( addr -- ) ( D: -- x ) ( R: x -- )
    drop
    return> >data ;
: compile-rdrop					( addr -- ) ( D: -- ) ( R: x -- )
    drop
    return> drop ;
: compile-2>r					( addr -- ) ( D: x1 x1 -- ) ( R: -- x1 x2 )
    drop
    0 compile-swap 0 compile->r 0 compile->r ;
: compile-2r@					( addr -- ) ( D: -- x1 x2 ) ( R: x1 x2 -- x1 x2  )
    drop
    0 compile-r> 0 compile-r@ 0 compile-over 0 compile->r 0 compile-swap ;
: compile-2r>					( addr -- ) ( D: -- x1 x2 ) ( R: x1 x2 -- )
    drop
    0 compile-r> 0 compile-r> 0 compile-swap ;
>target

0 -1 ' compile->r	:: >r			( x -- ) ( R: -- x )
1  0 ' compile-r@	:: r@			( -- x ) ( R: x -- x )
1  0 ' compile-r>	:: r>			( -- x ) ( R: x -- )
0  0 ' compile-rdrop	:: rdrop		( -- ) ( R: x -- )
0 -2 ' compile-2>r	:: 2>r			( x1 x2 -- ) ( R: -- x1 x2 )
2  0 ' compile-2r@	:: 2r@			( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
2  0 ' compile-2r>	:: 2r>			( -- x1 x2 ) ( R: x1 x2 -- )

: ['] ( "name" -- )
    ' postpone literal ; immediate compile-only

: [comp'] ( "name" -- )
    comp' swap postpone literal postpone literal ; immediate compile-only

: [char] ( 'char' -- n )
    char postpone literal ; immediate compile-only

>source
: compile-2drop					( addr -- ) ( D: x1 x2 -- )
    drop
    0 compile-drop 0 compile-drop ;
: compile-2dup					( addr -- ) ( D: x1 x2 -- x1 x2 x1 x2 )
    drop
    0 compile-over 0 compile-over ;
: compile-2swap					( addr -- ) ( D: x1 x2 x3 x4 -- x3 x4 x1 x2 )
    drop
    0 compile->r 0 compile-rot 0 compile-rot 0 compile-r> 0 compile-rot 0 compile-rot ;
: compile-2over					( addr -- ) ( D: x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
    drop
    0 compile-2>r 0 compile-2dup 0 compile-2r> 0 compile-2swap ;
: compile-2rot					( addr -- ) ( D: x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
    drop
    0 compile-2>r 0 compile-2swap 0 compile-2r> 0 compile-2swap ;
: compile-2tuck					( addr -- ) ( D: x1 x2 x3 x4 -- x3 x4 x1 x2 x3 x4 )
    drop
    0 compile-2dup 0 compile-2rot 0 compile-2rot ;
: compile-2nip					( addr -- ) ( D: x1 x2 x3 x4 -- x3 x4 )
    drop
    0 compile-2swap 0 compile-2drop ;
>target

0 -2 ' compile-2drop	:: 2drop		( x1 x2  -- )
4 -2 ' compile-2dup	:: 2dup			( x1 x2  -- x1 x2 x1 x2 )
4 -4 ' compile-2swap	:: 2swap		( x1 x2  x3 x4 -- x3 x4 x1 x2 )
6 -4 ' compile-2over	:: 2over		( x1 x2  x3 x4 -- x1 x2 x3 x4 x1 x2)
6 -6 ' compile-2rot	:: 2rot			( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
6 -4 ' compile-2tuck	:: 2tuck		( x1 x2 x3 x4 -- x3 x4 x1 x2 x3 x4 )
2 -4 ' compile-2nip	:: 2nip			( x1 x2 x3 x4 -- x3 x4 )

>source
: compile-on					( addr -- ) ( D: x1 -- )
    drop
    true compile,-literal 0 compile-swap 0 compile-! ;
: compile-off					( addr -- ) ( D: x1 -- )
    drop
    false compile,-literal 0 compile-swap 0 compile-! ;
>target

0 -1 ' compile-on	:: on			( addr -- )
0 -1 ' compile-off	:: off			( addr -- )

>source
\ Variablen fuer Stringbehandlung
$2000 constant text-size	\ 16 kByte Textbuffer
create text-data
    here cell+ a,
    text-size allot

: text-print ( -- )
    text-data dup @ over - dump ;

: ," ( "string"<"> -- addr )
    text-data @ dup
    '" parse
    rot 2dup + char+ dup aligned swap over swap ?do
	bl i c!
    loop
    2dup text-data !
    2>r place
    2r> tuck - flush-icache ;
>target

also vtarget also Forth
word-good 0 0 :word type
previous previous

>source
: gforth-s" ( "string"<"> -- )
    '" parse ;

: gforth-." ( "string"<"> -- )
    gforth-s" type ;

: compile,-s" ( addr -- )
    count
    swap compile,-literal compile,-literal ;
: compile,-." ( addr -- )
    count
    swap compile,-literal compile,-literal
    vtarget ['] type vsource compile,-interpreter ;
: (compile,-abort") ( addr -- )
    "error !
    if
	-2 throw
    endif ;
>target

also vtarget
word-good 0 0 :word (compile,-abort")
previous

>source
: compile,-abort" ( addr -- )
    compile,-literal
    vtarget ['] (compile,-abort") vsource compile,-now ;
>target

: s" ( "string"<"> -- )
    state @ if
	," [ also Forth ' lit previous ] literal gforth-compile, ,
	['] compile,-s" gforth-compile,
    else
	gforth-s"
    endif ; immediate

: ." ( "string"<"> -- )
    state @ if
	," [ also Forth ' lit previous ] literal gforth-compile, ,
	['] compile,-." gforth-compile,
    else
	gforth-."
    endif ; immediate

: abort" ( "string"<"> -- )
    ," [ also Forth ' lit previous ] literal gforth-compile, ,
    ['] compile,-abort" gforth-compile, ; immediate compile-only

>source
: rafts-sourcepos, ( -- )
    sourceline# postpone literal
    loadfilename# @ postpone literal ;
: rafts-print-sourcepos ( n n -- )
    2* cells included-files 2@ drop + 2@ type
    ." :" 0 .r ;
: rafts-(~~) ( n n -- )
    cr rafts-print-sourcepos ." :"
    printdebugdata cr ;
>target

also vtarget
word-good 0 0 :word rafts-(~~)
previous

: ~~ ( -- )
    rafts-sourcepos,
    vtarget postpone rafts-(~~) vsource ;  immediate

>source
: assertn ( -- )
    assert-level @ > if
	postpone (
    endif ;
: rafts-(endassert) ( -- )
    rot if
	2drop
    else
	cr rafts-print-sourcepos ." : failed assertion"
	true abort" assertion failed"
    endif ;
>target

also vtarget
word-good 0 0 :word rafts-(endassert)
previous

: assert0( ( -- )
    0 assertn ; immediate
: assert1( ( -- )
    1 assertn ; immediate
: assert2( ( -- )
    2 assertn ; immediate
: assert3( ( -- )
    3 assertn ; immediate
: assert( ( -- )
    vtarget postpone assert1( vsource ; immediate
: ) ( -- )
    rafts-sourcepos,
    vtarget postpone rafts-(endassert) vsource ; immediate

>source

?test $0004 [IF]
cr ." Test for primitives.fs" cr

finish
[THEN]
