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
:: drop ( D: x -- )
    data>
    drop ;;

:: dup ( D: x -- x x )
    data>
    dup
    >data >data ;;

:: over ( D: x1 x2 -- x1 x2 x1 )
    data> data>
    dup
    >data
    swap
    >data >data ;;

:: rot ( D: x1 x2 x3 -- x2 x3 x1 )
    data> data> data>
    rot rot
    >data >data >data ;;

:: -rot ( D: x1 x2 x3 -- x2 x3 x1 )
    data> data> data>
    rot
    >data >data >data ;;

:: swap ( D: x1 x2 -- x2 x1 )
    data> data>
    swap
    >data >data ;;
    
>source
also
vtarget :word pick
vtarget :word roll
\ vtarget :word dup?
previous
>target

:: pick ( D: xu ... x1 x0 u -- xu ... x1 x0 xu )
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ #data@ >data
    else
	>data vsource ['] pick compile,
    endif ;;

:: roll ( D: xu xu-1 ... x0 u -- xu-1 ... x0 xu )
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	il-val @ dup #data@
	ds-tos @ rot ds-data dup cell+ rot cells move
	data> drop >data
    else
	>data vsource ['] roll compile,
    endif ;;

:: 2drop ( D: x1 x2 -- )
    vtarget postpone drop postpone drop vsource ;;

:: 2dup ( D: x1 x2 -- x1 x2 x1 x2 )
    vtarget postpone over postpone over vsource ;;

:: 2over ( x1 x2 x3 x4 -- x1 x2 x3 x4 x1 x2 )
    3 postpone literal vtarget postpone pick vsource 3 postpone literal vtarget postpone pick vsource ;;

:: 2rot ( x1 x2 x3 x4 x5 x6 -- x3 x4 x5 x6 x1 x2 )
    5 postpone literal vtarget postpone roll vsource 5 postpone literal vtarget postpone roll vsource ;;

:: 2swap ( x1 x2 x3 x4 -- x3 x4 x1 x2 )
    3 postpone literal vtarget postpone roll vsource 3 postpone literal vtarget postpone roll vsource ;;

false [IF]
:: ?dup ( D: x -- 0 | x x )
    ~~
    data> dup il-op @ dup I_LITS = swap I_LIT = or if
	dup il-val @ 0<> if
	    dup
	    >data >data
	else
	    >data
	endif
    else
	>data ['] ?dup compile,
    endif ;;
[THEN]

:: nip ( D: x1 x2 -- x2 )
    vtarget postpone swap postpone drop vsource ;;

:: tuck ( D: x1 x2 -- x2 x1 x2 )
    vtarget postpone swap postpone over vsource ;;

\ primitives
>source
: binop-primitive ( op -- )
    create a,
does> ( D: addr addr -- addr )
    data> data> rot @ op >data ;

: unop-primitive ( op -- )
    create a,
does> ( D: addr -- addr )
    data> swap @ uop >data ;

I_PLUS		binop-primitive (+)
I_MINUS		binop-primitive (-)
I_TIMES		binop-primitive (*)
I_SLASH		binop-primitive (/)
I_MOD		binop-primitive (mod)
I_LSHIFT	binop-primitive (lshift)
I_LSHIFT	binop-primitive (lshifta)
I_RSHIFT	binop-primitive (rshift)
I_SRSHIFT	binop-primitive (rshifta)
I_AND		binop-primitive (and)
I_OR		binop-primitive (or)
I_XOR		binop-primitive (xor)

I_NEGATE	unop-primitive (negate)
I_INVERT	unop-primitive (invert)
>target

\ I_ binop-primitive
:: + (+) ;;
:: - (-) ;;
:: * (*) ;;
:: / (/) ;;
:: mod (mod) ;;
:: lshift (lshift) ;;
:: lshifta (lshifta) ;;
:: rshift (rshift) ;;
:: rshifta (rshifta) ;;
:: and (and) ;;
:: or (or) ;;
:: xor (xor) ;;

\ I_ unop-primitive
:: negate vtarget (negate) vsource ;;
:: invert vtarget (invert) vsource ;;

:: 1+ ( D: addr -- addr )
    1 postpone literal vtarget postpone + vsource ;;

:: 1- ( D: addr -- addr )
    -1 postpone literal vtarget postpone + vsource ;;

:: 2* ( D: addr -- addr )
    1 postpone literal vtarget postpone lshifta vsource ;;

:: 2/ ( D: addr -- addr )
    1 postpone literal vtarget postpone rshifta vsource ;;

:: = ( D: addr addr -- addr )
    data> data> I_XOR op
    1 postpone literal data> swap I_ULESS op
    0 postpone literal data> I_MINUS op >data ;;

:: <> ( D: addr addr -- addr )
    data> data> I_XOR op
    0 postpone literal data> I_ULESS op
    0 postpone literal data> I_MINUS op >data ;;

:: < ( D: addr addr -- addr )
    data> data> I_LESS op
    0 postpone literal data> I_MINUS op >data ;;

:: <= ( D: addr addr -- addr )
    data> data> swap I_LESS op
    -1 postpone literal data> I_PLUS op >data ;;

:: > ( D: addr addr -- addr )
    data> data> swap I_LESS op
    0 postpone literal data> I_MINUS op >data ;;

:: >= ( D: addr addr -- addr )
    data> data> I_LESS op
    -1 postpone literal data> I_PLUS op >data ;;

:: 0= ( D: addr -- addr )
    data> 1 postpone literal data> swap I_ULESS op
    0 postpone literal data> I_MINUS op >data ;;

:: 0<> ( D: addr -- addr )
    data> 0 postpone literal data> I_ULESS op
    0 postpone literal data> I_MINUS op >data ;;

:: 0< ( D: addr -- addr )
    data> 0 postpone literal data> swap I_LESS op
    0 postpone literal data> I_MINUS op >data ;;

:: 0<= ( D: addr -- addr )
    data> 0 postpone literal data> I_LESS op
    -1 postpone literal data> I_PLUS op >data ;;

:: 0> ( D: addr -- addr )
    data> 0 postpone literal data> I_LESS op
    0 postpone literal data> I_MINUS op >data ;;

:: 0>= ( D: addr -- addr )
    data> 0 postpone literal data> swap I_LESS op
    -1 postpone literal data> I_PLUS op >data ;;

:: u< ( D: addr addr -- addr )
    data> data> I_ULESS op
    0 postpone literal data> I_MINUS op >data ;;

:: u<= ( D: addr addr -- addr )
    data> data> swap I_ULESS op
    -1 postpone literal data> I_PLUS op >data ;;

:: u> ( D: addr addr -- addr )
    data> data> swap I_ULESS op
    0 postpone literal data> I_MINUS op >data ;;

:: u>= ( D: addr addr -- addr )
    data> data> I_ULESS op
    -1 postpone literal data> I_PLUS op >data ;;

:: cell ( D: addr -- addr )
    4 postpone literal ;;

:: cells ( D: addr -- addr )
    2 postpone literal vtarget postpone lshift vsource ;;

:: cell+ ( D: addr -- addr )
    4 postpone literal vtarget postpone + vsource ;;

:: cell- ( D: addr -- addr )
    4 postpone literal vtarget postpone - vsource ;;

:: chars ( D: addr -- addr ) ;;

:: char+ ( D: addr -- addr )
    vtarget postpone 1+ vsource ;;

:: char- ( D: addr -- addr )
    vtarget postpone 1- vsource ;;

:: @ ( D: addr -- addr )
    data> I_FETCH uop
    inst-!-list @ over il-depends !
    dup inst inst-@-list @ slist-insert drop
    >data ;;

:: ! ( D: addr addr -- )
    data> data> I_STORE op
    inst-@-list @ over il-depends !
    NIL inst inst-!-list !
    dup inst inst-!-list @ slist-insert drop
    NIL inst inst-@-list !
    dup inst inst-@-list @ slist-insert drop
    inst-btrees-insert ;;

:: 2@ ( D: addr -- addr addr )
    vtarget postpone dup postpone cell+ postpone @ postpone swap postpone @ vsource ;;

:: 2! ( D: addr addr addr -- )
    vtarget postpone swap postpone over postpone ! postpone cell+ postpone ! vsource ;;

:: c@ ( D: addr -- addr )
    data> I_CFETCH uop
    inst-!-list @ over il-depends !
    dup inst inst-@-list @ slist-insert drop
    >data ;;

:: c! ( D: addr addr -- )
    data> data> I_CSTORE op
    inst-@-list @ over il-depends !
    NIL inst inst-!-list !
    dup inst inst-!-list @ slist-insert drop
    NIL inst inst-@-list !
    inst-btrees-insert ;;

:: +! ( D: addr addr -- )
    vtarget postpone tuck postpone @ postpone +
    postpone swap postpone ! vsource ;;

:: -! ( D: addr addr -- )
    vtarget postpone tuck postpone @ postpone -
    postpone swap postpone ! vsource ;;

:: >r ( R: -- addr )
    data> >return ;;

:: r@ ( R: -- addr )
    0 #return@ >data ;;

:: r> ( R: -- addr )
    return> >data ;;

:: 2>r ( R: addr addr -- )
    vtarget postpone swap postpone >r postpone >r vsource ;;

:: 2r@ ( R: -- addr addr )
    vtarget postpone r> postpone r@ postpone over postpone >r postpone swap vsource ;;

:: 2r> ( R: -- addr addr )
    vtarget postpone r> postpone r> postpone swap vsource ;;

:: rdrop ( R: addr -- )
    return> drop ;;

: ['] ( "name" -- )
    ' postpone literal ; immediate compile-only

: [comp'] ( "name" -- )
    comp' swap postpone literal postpone literal ; immediate compile-only

: [char] ( 'char' -- n )
    char postpone literal ; immediate compile-only

\ :: is ( xt "name" -- )
\     ' >body postpone literal vtarget postpone ! vsource ;;

:: on ( addr -- )
  true postpone literal vtarget postpone swap postpone ! vsource ;;

:: off ( addr -- )
  false postpone literal vtarget postpone swap postpone ! vsource ;;
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

: (.")
    count type ;
: (s")
    count ;
: (abort")
    "error ! if
        -2 throw
    endif ;
>target

also vtarget :word (.") previous
also vtarget :word (s") previous
also vtarget :word (abort") previous

:: ." ( "string"<"> -- )
    ," postpone literal
    vtarget postpone (.") vsource ;; \ immediate

:: s" ( "string"<"> -- )
    ," postpone literal
    vtarget postpone (s") vsource ;; \ immediate

:: abort" ( "string"<"> -- )
    ," postpone literal
    vtarget postpone (abort") vsource ;; \ immediate compile-only
>source

: (dostruc) ( addr u - addr )
    vtarget postpone + vsource ;
comp' (dostruc) drop dostruc !

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
