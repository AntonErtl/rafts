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

>target

\ stack primitives
>source
also vtarget
:word pick
:word roll
\ :word dup?
previous

: compile,-drop					( addr -- ) ( D: x1 -- )
    drop
    data> drop ;
: compile,-dup					( addr -- ) ( D: x1 -- x1 x1 )
    drop
    data> dup >data >data ;
: compile,-over					( addr -- ) ( D: x1 x2 -- x1 x2 x1 )
    drop
    data> data> dup >data swap >data >data ;
: compile,-rot					( addr -- ) ( D: x1 x2 x3 -- x2 x3 x1 )
    drop
    data> data> data> rot rot >data >data >data ;
: compile,--rot					( addr -- ) ( D: x1 x2 x3 -- x2 x3 x1 )
    drop
    data> data> data> rot >data >data >data ;
: compile,-swap					( addr -- ) ( D: x1 x2 -- x2 x1 )
    drop
    data> data> swap >data >data ;
: compile,-nip					( addr -- ) ( D: x1 x2 -- x2 ) 
    drop
    0 compile,-swap 0 compile,-drop ;
: compile,-tuck					( addr -- ) ( D: x1 x2 -- x2 x1 x2 )
    drop
    0 compile,-swap 0 compile,-over ;
: compile,-pick					( addr -- ) ( D: xu ... x1 x0 u -- xu ... x1 x0 xu )
    drop
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ #data@ >data
    else
	>data vsource ['] pick imm-compile,
    endif ;
: compile,-roll					( addr -- ) ( D: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
    drop
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ dup #data@
	ds-tos @ rot ds-data dup cell+ rot cells move
	data> drop >data
    else
	>data vsource ['] roll imm-compile,
    endif ;

false [IF]
: compile,-?dups				 ( addr -- ) ( D: x -- 0 | x x )
    drop
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	dup il-val @ 0<> if
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

' compile,-drop		:: drop			( x -- )
' compile,-dup		:: dup			( x -- x x )
' compile,-over		:: over			( x1 x2 -- x1 x2 x1 )
' compile,-rot		:: rot			( x1 x2 x3 -- x2 x3 x1 )
' compile,--rot		:: -rot			( x1 x2 x3 -- x2 x3 x1 )
' compile,-swap		:: swap			( x1 x2 -- x2 x1 )
' compile,-tuck		:: tuck			( x1 x2 -- x2 x1 x2 )
' compile,-nip		:: nip			( x1 x2 -- x1 )
' compile,-pick		:: pick			( xu ... x1 x0 u -- xu ... x1 x0 xu )
' compile,-roll		:: roll			( xu xu-1 ... x0 u -- xu-1 ... x0 xu )
false [IF]
' compile,-?dup		:: ?dup			( x1 x2 -- x2 x1 )
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

I_PLUS		binop-prim compile,-+		( addr -- ) ( D: x1 x2 -- x3 )
I_MINUS		binop-prim compile,--		( addr -- ) ( D: x1 x2 -- x3 )
I_TIMES		binop-prim compile,-*		( addr -- ) ( D: x1 x2 -- x3 )
I_SLASH		binop-prim compile,-/		( addr -- ) ( D: x1 x2 -- x3 )
I_MOD		binop-prim compile,-mod		( addr -- ) ( D: x1 x2 -- x3 )
I_LSHIFT	binop-prim compile,-lshift	( addr -- ) ( D: x1 x2 -- x3 )
I_LSHIFT	binop-prim compile,-lshifta	( addr -- ) ( D: x1 x2 -- x3 )
I_RSHIFT	binop-prim compile,-rshift	( addr -- ) ( D: x1 x2 -- x3 )
I_SRSHIFT	binop-prim compile,-rshifta	( addr -- ) ( D: x1 x2 -- x3 )
I_AND		binop-prim compile,-and		( addr -- ) ( D: x1 x2 -- x3 )
I_OR		binop-prim compile,-or		( addr -- ) ( D: x1 x2 -- x3 )
I_XOR		binop-prim compile,-xor		( addr -- ) ( D: x1 x2 -- x3 )

I_NEGATE	unop-prim  compile,-negate	( addr -- ) ( D: x1 -- x2 )
I_INVERT	unop-prim  compile,-invert	( addr -- ) ( D: x1 -- x2 )
>target

\ I_ binop-primitive
' compile,-+		:: +			( n1|u1 n2|u2 -- n3|u3 )
' compile,--		:: -			( n1|u1 n2|u2 -- n3|u3 )
' compile,-*		:: *			( n1|u1 n2|u2 -- n3|u3 )
' compile,-/		:: /			( n1 n2 -- n3 )
' compile,-mod		:: mod			( n1 n2 -- n3 )
' compile,-lshift	:: lshift		( x1 u -- x2 )
' compile,-lshifta	:: lshifta		( x1 u -- x2 )
' compile,-rshift	:: rshift		( x1 u -- x2 )
' compile,-rshifta	:: rshifta		( x1 u -- x2 )
' compile,-and		:: and			( x1 x2 -- x3 )
' compile,-or		:: or			( x1 x2 -- x3 )
' compile,-xor		:: xor			( x1 x2 -- x3 )

\ I_ unop-primitive
' compile,-negate	:: negate		( n1 -- n2 )
' compile,-invert	:: invert		( x1 -- x2 )

>source
: compile,-=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_XOR op 1 lit swap I_ULESS op 0 lit I_MINUS op >data ;
: compile,-<>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_XOR op 0 lit I_ULESS op 0 lit I_MINUS op >data ;
: compile,-<					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_LESS op 0 lit I_MINUS op >data ;
: compile,-<=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_LESS op -1 lit I_PLUS op >data ;
: compile,->					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_LESS op 0 lit I_MINUS op >data ;
: compile,->=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_LESS op -1 lit I_PLUS op >data ;
: compile,-0=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 1 lit swap I_ULESS op 0 lit I_MINUS op >data ;
: compile,-0<>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit I_ULESS op 0 lit I_MINUS op >data ;
: compile,-0<					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit swap I_LESS op 0 lit I_MINUS op >data ;
: compile,-0<=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit I_LESS op -1 lit I_PLUS op >data ;
: compile,-0>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit I_LESS op 0 lit I_MINUS op >data ;
: compile,-0>=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> 0 lit swap I_LESS op -1 lit I_PLUS op >data ;
: compile,-u<					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_ULESS op 0 lit I_MINUS op >data ;
: compile,-u<=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_ULESS op -1 lit I_PLUS op >data ;
: compile,-u>					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> swap I_ULESS op 0 lit I_MINUS op >data ;
: compile,-u>=					( addr -- ) ( D: x1 x2 -- x3 )
    drop
    data> data> I_ULESS op -1 lit I_PLUS op >data ;
>target

' compile,-=		:: =			( x1 x2 -- flag )
' compile,-<>		:: <>			( x1 x2 -- flag )
' compile,-<		:: <			( n1 n2 -- flag )
' compile,-<=		:: <=			( n1 n2 -- flag )
' compile,->		:: >			( n1 n2 -- flag )
' compile,->=		:: >=			( n1 n2 -- flag )
' compile,-0=		:: 0=			( x -- flag )
' compile,-0<>		:: 0<>			( x -- flag )
' compile,-0<		:: 0<			( n -- flag )
' compile,-0<=		:: 0<=			( n -- flag )
' compile,-0>		:: 0>			( n -- flag )
' compile,-0>=		:: 0>=			( n -- flag )
' compile,-u<		:: u<			( u1 u2 -- flag )
' compile,-u<=		:: u<=			( u1 u2 -- flag )
' compile,-u>		:: u>			( u1 u2 -- flag )
' compile,-u>=		:: u>=			( u1 u2 -- flag )

>source
: compile,-1+					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile,-+ ;
: compile,-1-					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile,-- ;
: compile,-2*					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile,-lshift ;
: compile,-2/					( addr -- ) ( D: x1 -- x2 )
    drop
    1 compile,-literal 0 compile,-rshift ;
: compile,-cell					( addr -- ) ( D: -- x1 )
    drop
    4 compile,-literal ;
: compile,-cells				( addr -- ) ( D: x1 -- x2 )
    drop
    2 compile,-literal 0 compile,-lshift ;
: compile,-cell+				( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile,-cell 0 compile,-+ ;
: compile,-cell-				( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile,-cell 0 compile,-- ;
: compile,-chars				( addr -- ) ( D: x1 -- x2 )
    drop ;
: compile,-char+				( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile,-1+ ;
: compile,-char-				( addr -- ) ( D: x1 -- x2 )
    drop
    0 compile,-1- ;
>target

' compile,-1+		:: 1+			( n1 -- n2 )
' compile,-1-		:: 1-			( n1 -- n2 )
' compile,-2*		:: 2*			( n1 -- n2 )
' compile,-2/		:: 2/			( n1 -- n2 )
' compile,-cell		:: cell			( -- n1 )
' compile,-cells	:: cells		( n1 -- n2 )
' compile,-cell+	:: cell+		( addr1 -- addr2 )
' compile,-cell-	:: cell-		( addr1 -- addr2 )
' compile,-chars	:: chars		( n1 -- n2 )
' compile,-char+	:: char+		( addr1 -- addr2 )
' compile,-char-	:: char-		( addr1 -- addr2 )

>source
: compile,-@					( addr -- ) ( D: x1 -- x2 )
    drop
    data> I_FETCH uop
    inst-!-list @ over il-depends !
    dup inst inst-@-list @ slist-insert drop
    >data ;
: compile,-!					( addr -- ) ( D: x1 x2 -- )
    drop
    data> data> I_STORE op
    inst-@-list @ over il-depends !
    NIL inst inst-!-list !
    dup inst inst-!-list @ slist-insert drop
    NIL inst inst-@-list !
    dup inst inst-@-list @ slist-insert drop
    inst-btrees-insert ;
: compile,-2@					( addr -- ) ( D: x1 -- x2 x3 )
    drop
    0 compile,-dup 0 compile,-cell+ 0 compile,-@ 0 compile,-swap 0 compile,-@ ;
: compile,-2!					( addr -- ) ( D: x1 x2 x3 -- )
    drop
    0 compile,-tuck 0 compile,-! 0 compile,-cell+ 0 compile,-! ;
: compile,-+!					( addr -- ) ( D: x1 x2 -- )
    drop
    0 compile,-tuck 0 compile,-@ 0 compile,-+
    0 compile,-swap 0 compile,-! ;
: compile,--!					( addr -- ) ( D: x1 x2 -- )
    drop
    0 compile,-tuck 0 compile,-@ 0 compile,--
    0 compile,-swap 0 compile,-! ;
: compile,-c@					( addr -- ) ( D: x1 -- x2 )
    drop
    data> I_CFETCH uop
    inst-!-list @ over il-depends !
    dup inst inst-@-list @ slist-insert drop
    >data ;
: compile,-c!					( addr -- ) ( D: x1 x2 -- )
    drop
    data> data> I_CSTORE op
    inst-@-list @ over il-depends !
    NIL inst inst-!-list !
    dup inst inst-!-list @ slist-insert drop
    NIL inst inst-@-list !
    inst-btrees-insert ;
>target
    
' compile,-@		:: @			( addr -- x )
' compile,-!		:: !			( x addr -- )
' compile,-2@		:: 2@			( addr -- x1 x2 )
' compile,-2!		:: 2!			( x1 x2 addr -- )
' compile,-+!		:: +!			( x addr -- )
' compile,--!		:: -!			( x addr -- )
' compile,-c@		:: c@			( addr -- x )
' compile,-c!		:: c!			( x addr -- )

>source
: compile,->r					( addr -- ) ( D: x -- ) ( R: -- x )
    drop
    data> >return ;
: compile,-r@					( addr -- ) ( D: -- x ) ( R: x -- x )
    drop
    0 #return@ >data ;
: compile,-r>					( addr -- ) ( D: -- x ) ( R: x -- )
    drop
    return> >data ;
: compile,-rdrop				( addr -- ) ( D: -- ) ( R: x -- )
    drop
    return> drop ;
: compile,-2>r					( addr -- ) ( D: x1 x1 -- ) ( R: -- x1 x2 )
    drop
    0 compile,-swap 0 compile,->r 0 compile,->r ;
: compile,-2r@					( addr -- ) ( D: -- x1 x2 ) ( R: x1 x2 -- x1 x2  )
    drop
    0 compile,-r> 0 compile,-r@ 0 compile,-over 0 compile,->r 0 compile,-swap ;
: compile,-2r>					( addr -- ) ( D: -- x1 x2 ) ( R: x1 x2 -- )
    drop
    0 compile,-r> 0 compile,-r> 0 compile,-swap ;
>target

' compile,->r		:: >r			( x -- ) ( R: -- x )
' compile,-r@		:: r@			( -- x ) ( R: x -- x )
' compile,-r>		:: r>			( -- x ) ( R: x -- )
' compile,-rdrop	:: rdrop		( -- ) ( R: x -- )
' compile,-2>r		:: 2>r			( x1 x2 -- ) ( R: -- x1 x2 )
' compile,-2r@		:: 2r@			( -- x1 x2 ) ( R: x1 x2 -- x1 x2 )
' compile,-2r>		:: 2r>			( -- x1 x2 ) ( R: x1 x2 -- )

: ['] ( "name" -- )
    ' postpone literal ; immediate compile-only

: [comp'] ( "name" -- )
    comp' swap postpone literal postpone literal ; immediate compile-only

: [char] ( 'char' -- n )
    char postpone literal ; immediate compile-only

>source
: compile,-2drop				( addr -- ) ( D: x1 x2 -- )
    drop
    0 compile,-drop 0 compile,-drop ;
: compile,-2dup					( addr -- ) ( D: x1 x2 -- x1 x2 x1 x2 )
    drop
    0 compile,-over 0 compile,-over ;
: compile,-2swap				( addr -- ) ( D: x1 x2 x3 x4 -- x3 x4 x1 x2 )
    drop
    0 compile,->r 0 compile,-rot 0 compile,-rot 0 compile,-r> 0 compile,-rot 0 compile,-rot ;
: compile,-2over				( addr -- ) ( D: x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
    drop
    0 compile,-2>r 0 compile,-2dup 0 compile,-2r> 0 compile,-2swap ;
>target

' compile,-2drop	:: 2drop		( x1 x2  -- )
' compile,-2dup		:: 2dup			( x1 x2  -- x1 x2 x1 x2 )
' compile,-2swap	:: 2swap		( x1 x2  x3 x4 -- x3 x4 x1 x2 )
' compile,-2over	:: 2over		( x1 x2  x3 x4 -- x1 x2 x3 x4 x1 x2)

>source
: compile,-on					( addr -- ) ( D: x1 -- )
    drop
  true compile,-literal 0 compile,-swap 0 compile,-! ;
: compile,-off					( addr -- ) ( D: x1 -- )
    drop
  false compile,-literal 0 compile,-swap 0 compile,-! ;
>target

' compile,-on		:: on			( addr -- )
' compile,-off		:: off			( addr -- )

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
    [char] " parse
    rot 2dup + char+ dup aligned swap over swap ?do
	bl i c!
    loop
    2dup text-data !
    2>r place
    2r> tuck - flush-icache ;
>target

also vtarget :word type previous

>source
: gforth-s" ( "string"<"> -- )
    [char] " parse ;

: gforth-." ( "string"<"> -- )
    gforth-s" type ;

: compile,-s" ( addr -- )
    count
    swap compile,-literal compile,-literal ;
: compile,-." ( addr -- )
    count
    swap compile,-literal compile,-literal
    ['] type compile,-interpreter ;
: (compile,-abort") ( addr -- )
    "error !
    if
	-2 throw
    endif ;
>target

also vtarget :word (compile,-abort") previous

>source
: compile,-abort" ( addr -- )
    compile,-literal
    vtarget ['] (compile,-abort") vsource imm-compile, ;
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
: nothing ( -- ) ;		\ a unknown bugfix
: sourcepos, ( -- )
    sourceline# postpone literal
    loadfilename# @ postpone literal ;
: print-sourcepos ( n n -- )
    2* cells included-files nothing 2@ drop + 2@ type
    ." :" 0 .r ;
: (~~) ( n n -- )
    cr print-sourcepos ." :"
    printdebugdata cr ;
>target

also vtarget :word (~~) previous

: ~~ ( -- )
    sourcepos,
    vtarget postpone (~~) vsource ;  immediate

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
>target

also vtarget :word (endassert) previous

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
    sourcepos,
    vtarget postpone (endassert) vsource ; immediate

>source

?test $0004 [IF]
cr ." Test for primitives.fs" cr

finish
[THEN]
