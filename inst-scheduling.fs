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

: (inst_dec) ( node-addr -- )
  node_reg @ regs_dec ;

: inst_dec ( node-addr -- )
  ['] (inst_dec) swap btree_postorder ;

: inst_check ( node-addr -- flag )
  dup dup node_type @ ['] n_id! = if
    swap btree_right @ dup node_type @ ['] n_id@ = if
      dup node_offset @ swap node_pointer @
      rot dup node_offset @ swap node_pointer @
      rot = rot rot = and else
      2drop false endif else
    2drop false endif ;

: inst_depends_func ( inst-addr -- flag )
  inst_node @
?trace $0040 [IF]
  ." INST_DEPENDS:" dup hex. cr
[THEN]
  ?inst_done ;

: inst_depends ( inst-addr -- inst-addr )
?trace $0040 [IF]
  ." inst_depends:" dup hex. ." ;" dup inst_print_depends cr
[THEN]
  dup 0<> if
    ['] inst_depends_func swap slist_find dup 0<> if
      inst_node @ endif endif ;

: inst_scheduling_out ( node-addr -- )
  dup node_type @ execute ;

: inst_scheduling_postorder ( xt node-addr -- )
?trace $0040 [IF]
  ." scheduling:" hex.rs hex.s cr
[THEN]
  dup NIL <> if
?trace $0040 [IF]
    ." SCHEDULING" hex.s cr
[THEN]
    dup node_reg @ 0<> if
?trace $0040 [IF]
      ." regs opt:" hex.s cr
[THEN]
      dup node_link @ 0<> if
?trace $0040 [IF]
    hex ." regs link opt:" hex.s cr decimal
[THEN]
        dup btree_left @ inst_dec
        dup btree_right @ inst_dec endif
      else
      dup inst_check if
?trace $0040 [IF]
        ." stack opt:" hex.s cr
[THEN]
        dup btree_right @ link-
        node_reg @ dup 0<> if
?trace $0040 [IF]
        ." stack regs opt:" hex.s cr
[THEN]
          regs_dec else
?trace $0040 [IF]
        ." stack no regs opt:" hex.s cr
[THEN]
	  drop endif
        else
        begin
          dup node_depends @ inst_depends
?trace $0040 [IF]
          ." recurse depends:" hex.s cr
[THEN]
	  dup 0<> while
          rot rot 2>r 2r@
	  drop swap
?trace $0040 [IF]
          ." depends{:" hex.rs hex.s cr
[THEN]
	  recurse 2r>
?trace $0040 [IF]
          ." }depends done:" hex.rs hex.s cr
[THEN]
	  repeat
	drop

        2>r 2r@ btree_left @
?trace $0040 [IF]
	." recurse{ (left):" hex.rs hex.s cr
[THEN]
	recurse 2r>
?trace $0040 [IF]
        ." }recurse done (left):" hex.rs hex.s cr
[THEN]
        2>r 2r@ btree_right @
?trace $0040 [IF]
	." recurse{ (right):" hex.rs hex.s cr
[THEN]
	recurse 2r>
?trace $0040 [IF]
        ." }recurse done (right):" hex.rs hex.s cr
[THEN]

        dup ?inst_done if
          2>r 2r@ nip btree_left @ dup 0<> if
            inst_reg@ else
	    drop endif
          2r@ nip btree_right @ dup 0<> if
            inst_reg@ else
	    drop endif
?trace $0040 [IF]
          ." used registers:" hex.s cr
[THEN]
          2r@ swap
?trace $0040 [IF]
          ." inst{:" hex.rs hex.s cr
[THEN]
          execute 2r>
?trace $0040 [IF]
          ." }inst done:" hex.rs hex.s cr
[THEN]
	  else
?trace $0040 [IF]
          ." inst already done:" hex.s cr
[THEN]
	  endif
        endif endif endif
  2drop ;

: inst_scheduling_func ( inst-addr -- )
  inst_node @
  ['] inst_scheduling_out swap inst_scheduling_postorder ;

: inst_scheduling ( -- )
  ['] inst_scheduling_func inst_head @ slist_forall ;

?test $0040 [IF]
cr ." Test for inst-scheduling.fs" cr

finish
[THEN]
