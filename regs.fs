\ regs.fs	usable registers (for MIPS R3000)
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

$20 constant regs-useable
regs-useable array regs-data

@s0 constant #ip
@s1 constant #sp
@s2 constant #rp
\ @s3 constant #fp
\ @s4 constant #lp
@s4 constant #cfa
\ @s6 constant #up
\ @s7 constant #tos
\ @s8 constant #ftos

-1 constant regs-unused

\ initialize free registers
: regs-init ( -- )
    regs-useable 0 ?do
	regs-unused i regs-data !
    loop
    0 @zero regs-data !	\ reserved registers
    0 @at regs-data !
    \ 0 @v0 regs-data !
    \ 0 @v1 regs-data !
    \ 0 @a0 regs-data !
    \ 0 @a1 regs-data !
    \ 0 @a2 regs-data !
    \ 0 @a3 regs-data !
    0 @k0 regs-data !
    0 @k1 regs-data !
    0 @gp regs-data !
    0 @sp regs-data !
    0 @s0 regs-data !	\ saved registers
    0 @s1 regs-data !
    0 @s2 regs-data !
    0 @s3 regs-data !
    0 @s4 regs-data !
    0 @s5 regs-data !
    0 @s6 regs-data !
    0 @s7 regs-data !
    0 @s8 regs-data !
    0 @ra regs-data ! ;
regs-init

: free-set ( -- w )
    \ produces a bitset of (currently) free registers
    \ relies on #regs<bits/cell
    0
    regs-useable 0 ?do
	i regs-data @ regs-unused = if
	    1 i lshift or
	endif
    loop ;

free-set constant freeable-set

\ set register with use count
: regs-set ( n register -- )
    regs-data ! ;

\ get first free register
: regs-get ( n -- register )
    0				\ inital NO register is free
    regs-useable 1 ?do
	i regs-data @ -1 = if	\ check free register
	    drop i leave
	endif
    loop
    dup 0= abort" no more registers"
    tuck regs-set ;

\ decrement use count
: regs-inc ( register -- )
    1 swap regs-data +! ;

: regs-dec ( register -- )
    -1 swap regs-data +! ;

\ print out all registers, that are not free
: regs-print ( -- )
    ." regs:"
    regs-useable 0 ?do
	i @zero = 			\ reserved registers
	i @at = or
	\ i @v0 = or
	\ i @v1 = or
	\ i @a0 = or
	\ i @a1 = or
	\ i @a2 = or
	\ i @a3 = or
	i @k0 = or
	i @k1 = or
	i @gp = or
	i @sp = or
	\ i @s0 = or			\ saved registers
	\ i @s1 = or
	\ i @s2 = or
	i @s3 = or
	\ i @s4 = or
	i @s5 = or
	i @s6 = or
	i @s7 = or
	i @s8 = or
	i @ra = or 0= if
	    i regs-data @ dup -1 <> if
		2 i hexn. .
	    else
		drop
	    endif
	endif
    loop
    cr ;

?test $0040 [IF]
cr ." Test for regs.fs" cr

1 regs-get ." regs-get:" . cr
2 regs-get ." regs-get:" . cr
regs-print
0 1 regs-set ." regs-set" cr
0 2 regs-set ." regs-set" cr
0 3 regs-set ." regs-set" cr
1 4 regs-set ." regs-set" cr
0 5 regs-set ." regs-set" cr
1 regs-get ." regs-get:" . cr
regs-print

4 regs-dec ." regs-dec" cr
1 regs-get ." regs-get:" . cr
4 regs-dec ." regs-dec" cr
1 regs-get ." regs-get:" . cr
regs-print

regs-init
finish
[THEN]
