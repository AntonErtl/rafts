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

$20 constant regs_useable
regs_useable array regs_data

@s0 constant #ip
@s1 constant #sp
@s2 constant #rp
\ @s3 constant #fp
\ @s4 constant #lp
@s4 constant #cfa
\ @s6 constant #up
\ @s7 constant #tos
\ @s8 constant #ftos

-1 constant regs_unused

\ initial free registers
: regs_init ( -- )
  regs_useable 0 ?do
    regs_unused i regs_data ! loop
  0 @zero regs_data !	\ reserved registers
  0 @at regs_data !
  \ 0 @v0 regs_data !
  \ 0 @v1 regs_data !
  \ 0 @a0 regs_data !
  \ 0 @a1 regs_data !
  \ 0 @a2 regs_data !
  \ 0 @a3 regs_data !
  0 @k0 regs_data !
  0 @k1 regs_data !
  0 @gp regs_data !
  0 @sp regs_data !
  0 @s0 regs_data !	\ saved registers
  0 @s1 regs_data !
  0 @s2 regs_data !
  0 @s3 regs_data !
  0 @s4 regs_data !
  0 @s5 regs_data !
  0 @s6 regs_data !
  0 @s7 regs_data !
  0 @s8 regs_data !
  0 @ra regs_data ! ;
regs_init

\ set register with use count
: regs_set ( n register -- )
  regs_data ! ;

\ get first free register
: regs_get ( n -- register )
  0				\ inital NO register is free
  regs_useable 1 ?do
    i regs_data @ -1 = if	\ check free register
      drop i leave endif loop
  dup 0= abort" no more registers"
  tuck regs_set ;

\ decrement use count
: regs_inc ( register -- )
  1 swap regs_data +! ;

: regs_dec ( register -- )
  -1 swap regs_data +! ;

\ print out all registers, that are not free
: regs_print ( -- )
  ." regs:"
  regs_useable 0 ?do
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
      i regs_data @ dup -1 <> if
        2 i hexn. . else
        drop endif endif loop
  cr ;

?test $0040 [IF]
cr ." Test for regs.fs" cr

1 regs_get ." regs_get:" . cr
2 regs_get ." regs_get:" . cr
regs_print
0 1 regs_set ." regs_set" cr
0 2 regs_set ." regs_set" cr
0 3 regs_set ." regs_set" cr
1 4 regs_set ." regs_set" cr
0 5 regs_set ." regs_set" cr
1 regs_get ." regs_get:" . cr
regs_print

4 regs_dec ." regs_dec" cr
1 regs_get ." regs_get:" . cr
4 regs_dec ." regs_dec" cr
1 regs_get ." regs_get:" . cr
regs_print

regs_init
finish
[THEN]
