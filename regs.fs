\ regs.fs	usable registers (for MIPS R3000)
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

variable regs-free-set

: regs-reset ( -- )
\ initialize free registers
    regs-freeable-set regs-free-set ! ;
regs-reset

: regs-set ( register -- )
\ set register with used flag
    regs-free-set tuck @
    1 rot lshift invert and swap ! ;

: regs-free ( register -- )
\ set register with freed flag
    regs-free-set tuck @
    1 rot lshift or swap ! ;

: regs-check ( n -- n | true )
\ checkout first free register
    0
    begin
	over
    while
	over 1 and if
	    nip exit
	endif
	1+ swap 1 rshift swap
    repeat
    2drop true ;

: regs-get ( -- register )
\ get first free register
    regs-free-set @ regs-check
    dup true = abort" no more registers"
    dup regs-set ;

: regs-print ( -- )
\ print out all registers, that are not free
    ." regs: ( " regs-free-set @ dup hex. ." ) "
    invert regs-freeable-set and 0
    begin
	over
    while
	over 1 and if
	    dup hex.
	endif
	1+ swap 1 rshift swap
    repeat
    2drop
    cr ;

?test $0040 [IF]
cr ." Test for regs.fs" cr

regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-print
4 regs-set ." regs-set" cr
5 regs-set ." regs-set" cr
6 regs-set ." regs-set" cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-print

4 regs-free ." regs-set" cr
5 regs-free ." regs-set" cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-get ." regs-get:" . cr
regs-print

regs-reset
hex.s cr
finish
[THEN]
