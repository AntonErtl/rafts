\ register.fs	instruction selection words
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

\ local register allocation
: alloc-reg ( ml -- )
\ allocate a register for the result of the ml, if necessary
    dup ml-reg @ regs-unused = if
	regs-get swap ml-reg !
    else
	drop
    endif ;

: freeable-reg ( reg -- flag )
\ returns true if register is freeable
     1 swap lshift regs-freeable-set and 0<> ;

: free-reg ( ml -- )
\ return the register to the free ones
\ (except 0, which stands for "no result register")
    ml-reg @ dup freeable-reg if
	regs-free
    else
	drop
    endif ;

: register-allocation-func ( ml -- )
\ register allocation for one instruction tree
    dup free-reg
    dup ml-left @ ?dup 0<> if
	alloc-reg
    endif
    ml-right @ ?dup 0<> if
	alloc-reg
    endif ;

: register-allocation ( -- )
\ register allocation for the whole instructions list
    ['] register-allocation-func 0 inst-lists inst-sequence-reverse ;

?test $0020 [IF]
cr ." Test for register.fs" cr

finish
[THEN]