\ inst-scheduling.fs	instruction scheduling words
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

: inst-next ( -- ml )
    \ find the best possible in inst-pnodes
    0 inst-pnodes
    -1
    over inst-size 1- inst-pnodes swap
    begin
	2dup >=
    while
	dup @ if
	    tuck 2>r
	    2dup @ ml-cost @ tuck < if
		rot drop rot drop
	    else
		2drop
	    endif
	    2r>
	endif
	cell+
    repeat
    2drop drop
    dup @ dup if
	NIL rot !
    else					\ remove fetched node
	2drop NIL				\ no node found
    endif ;

: inst-depends-done-func ( flag inst-addr -- flag )
    inst-node @ ?inst-done and ;

: ?inst-depends-done ( ml -- flag )
    true swap
    ml-depends @ ['] inst-depends-done-func maplist ;

: ml-done? ( parent-ml ml -- parent-ml flag )
    ?dup if
	2dup <> if
	    ?inst-done
	else
	    ." parent is child" cr
	    drop true
	endif
    else
	true
    endif ;

: ?inst-kids-done ( ml -- flag )
    dup ?inst-depends-done if
	dup ml-left @ ml-done? if
	    ml-right @ ml-done?
	else
	    drop false
	endif
    else
	drop false
    endif ;

: inst-join ( -- )
    inst-size 1- inst-nodes
    0 inst-nodes
    begin
	2dup >=
    while
	dup @ ?dup if
	    dup ?inst-kids-done if		\ check done kids
		inst-pnodes-insert		\ add to pnodes
		NULL over !			\ del from nodes
	    else
		drop
	    endif
	endif
	cell+
    repeat
    2drop ;

: inst-scheduling ( -- )
    begin
	inst-join inst-next dup
    while
	dup inst-done inst-lists-insert
    repeat
    drop ;

?test $0040 [IF]
cr ." Test for inst-scheduling.fs" cr

finish
[THEN]
