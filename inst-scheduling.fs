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
    -1 over
    begin
	inst-size 1- inst-pnodes over >=
    while
	dup @ 0<> if
	    dup @ ?inst-done if		\ remove done nodes !
		NULL over !
	    else
		2dup @ ml-cost @ < if		\ search highest costs
		    nip nip dup @ ml-cost @ over
		endif
	    endif
	endif
	cell+
    repeat
    2drop
    dup @ 0<> if
	dup @ NIL rot !
    else					\ remove fetched node
	drop NIL				\ no node found
    endif ;

: inst-depends-done-func ( inst-addr -- )
    inst-node @ ?inst-done and ;

: ?inst-depends-done ( ml -- flag )
    true swap
    ml-depends @ ['] inst-depends-done-func maplist ;

: ml-done? ( parent-ml flag1 ml -- parent-ml flag1 flag )
    ?dup 0<> if
	>r over r> tuck <> if		\ !! does it ever take the second branch?
	    \ ~~ dup print-ml
	    ?inst-done
	else
	    ." parent is child" cr
	    drop true
	endif
    else
	true
    endif ;

: ?inst-kids-done ( ml -- flag )
    \ ~~ dup print-ml
    dup ?inst-depends-done ( ml flag )
    over ml-left  @ ml-done? and
    swap ml-right @ ml-done? and ;

: inst-join ( -- )
    0 inst-nodes
    begin
	inst-size 1- inst-nodes over >=
    while
	dup @ ?dup 0<> if
	    dup ?inst-kids-done if		\ check done kids
		inst-pnodes-insert		\ add to pnodes
		NULL over !			\ del from nodes
	    else
		drop
	    endif
	endif
	cell+
    repeat
    drop ;

: inst-scheduling ( -- )
    begin
	inst-join inst-next dup 0<>
    while
	dup inst-done inst-lists-insert
    repeat
    drop ;

?test $0040 [IF]
cr ." Test for inst-scheduling.fs" cr

finish
[THEN]
