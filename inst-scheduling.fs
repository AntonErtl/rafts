\ inst-scheduling.fs	instruction scheduling words
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

variable inst-cycle

: ml-dec                                        ( addr -- )
    ?dup if
	-1 over ml-count +!
	dup ml-count @ if
	    drop
	else
	    inst-cycle @ over ml-let +!
	    inst-mls-insert
	endif
    endif ;

: ml-depends-dec                                ( addr -- )
    ?dup if
	begin
	    dup inst-node @ ml-dec
	    slist-next @
	    dup 0=
	until
	drop
    endif ;

: inst-scheduling-heuristic                     ( addr-new addr-old -- addr )
    over @ ml-let @ inst-cycle @ < if
	dup if
	    over @ ml-pathlength @
	    over @ ml-pathlength @ < if
		swap
	    endif
	endif
    else
	swap
    endif
    drop ;

: inst-next                                     ( -- addr )
    NIL
    0 inst-mls
    begin
	dup @
    while
	dup rot inst-scheduling-heuristic
	swap
	cell+
    repeat
    drop ;

: inst-scheduling                               ( -- )
    0 tos-register !
    1 inst-cycle !
    ?trace $0004 [IF]
	." SCHEDULING BB:" basic-block @ . cr
	mls-pr
	lists-pr
    [THEN]
    begin
	?trace $0004 [IF]
	    mls-pr
	    lists-pr
	[THEN]
	inst-next                               ( addr addr )
	inst-mls-end @
    while
	?dup if
	    dup @                               ( addr ml )
	    swap inst-mls-delete                ( ml )
	    dup ml-reg @ regs-unallocated <> if
		dup register-allocation
		dup inst-lists-insert

		dup ml-depends @ ml-depends-dec ( ml )
		dup ml-left @ ml-dec            ( ml )
		dup ml-right @ ml-dec           ( ml )
	    endif
	    drop
	endif
	1 inst-cycle +!
    repeat
    drop ;

?test $0020 [IF]
cr ." Test for inst-scheduling.fs" cr

finish
[THEN]
