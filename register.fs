\ register.fs	instruction selection words
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

variable tos-register

\ local register allocation

: alloc-reg ( ml -- )
\ allocate a register for the result of the ml, if necessary
    dup ml-reg @ regs-unallocated = if
	regs-get swap ml-reg !
    else
	drop
    endif ;

: freeable-reg ( reg -- flag )
\ returns true if register is freeable
     1 swap lshift regs-freeable-set and ;

: free-tos ( reg tos-reg -- )
    drop dup #tos - cells ds-init + @
    \ fetch the ml address of the register nonterminal
    il-nt-insts burm-reg-NT cells + @ dup if
	dup >r
	ml-reg dup @ regs-unallocated = if
	    !
	    10 r> ml-count +!
	else
	    2drop
	    rdrop
	endif
    else
	2drop
    endif ;

: free-reg ( ml -- )
\ return the register to the free ones
\ (except 0, which stands for "no result register")
    ml-reg @
    dup freeable-reg if
	regs-free
    else
	tos-#register 0> if
	    #tos over <=
	    over #tos tos-#register + < and if
		tos-register @ 2dup = if
		    free-tos
		else
		    2dup or tos-register !
		    free-tos
		endif
	    else
		drop
	    endif
	    \ sos, tos Behandlung if
	    \   ist ml-reg des entsprechenden MOVE unallocated if
	    \     setze ml-reg des MOVE auf entsprechendes (sos, tos)reg
	    \   endif
	    \ endif
	else
	    drop
	endif
    endif ;

: register-allocation ( ml -- )
\ register allocation for one instruction tree
    dup free-reg
    dup ml-left @ ?dup if
	alloc-reg
    endif
    ml-right @ ?dup if
	alloc-reg
    endif ;

?test $0040 [IF]
cr ." Test for register.fs" cr

finish
[THEN]
