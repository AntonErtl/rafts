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

: inst_next ( -- node_addr )
  0 inst_pnodes
  -1 over
  begin
    inst_size 1- inst_pnodes over >= while
      dup @ 0<> if
        dup @ ?inst_done if		\ remove done nodes !
	  NULL over ! else
          2dup @ node_cost @ < if	\ search highest costs
            nip nip dup @ node_cost @ over endif
	  endif
	endif
    cell+ repeat
  2drop
  dup @ 0<> if
    dup @ NIL rot ! else		\ remove fetched node
    drop NIL endif ;			\ no node found

: inst_depends_done_func ( inst_addr -- )
  inst_node @ ?inst_done and ;

: ?inst_depends_done ( node_addr -- flag )
  true swap
  node_depends @ dup node_depends_init <> if
    ['] inst_depends_done_func swap slist_forall else
    drop endif ;

: ?inst_kids_done ( node_addr -- flag )
  dup ?inst_depends_done		\ check yourself
  over node_lval @ ?dup 0<> if		\ check left kid
    >r over r> tuck <> if
      ?inst_done else
      drop true endif else
    true endif
  and swap node_rval @ ?dup 0<> if	\ check right kid
    >r over r> tuck <> if
      ?inst_done else
      drop true endif else
    true endif
  and ;

: inst_join ( -- )
  0 inst_nodes
  begin
    inst_size 1- inst_nodes over >= while
    dup @ ?dup 0<> if
      dup ?inst_kids_done if		\ check done kids
        inst_pnodes_insert		\ add to pnodes
	NULL over ! else		\ del from nodes
	drop endif endif
    cell+ repeat
  drop ;

: inst_scheduling ( -- )
  begin
    inst_join inst_next dup 0<> while
    dup inst_done inst_lists_insert repeat
  drop ;

?test $0040 [IF]
cr ." Test for inst-scheduling.fs" cr

finish
[THEN]
