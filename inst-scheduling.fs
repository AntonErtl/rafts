\ $Id: inst-scheduling.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: inst-scheduling.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

: (inst_dec) ( btree-addr -- )
  btree_data @ node_reg @ regs_dec ;

: inst_dec ( btree-addr -- )
  ['] (inst_dec) swap btree_postorder ;

: inst_check ( btree-addr -- flag )
  dup btree_data @ dup node_type @ ['] n_id! = if
    swap btree_right @ btree_data @ dup node_type @ ['] n_id@ = if
      dup node_offset @ swap node_pointer @
      rot dup node_offset @ swap node_pointer @
      rot = rot rot = and else
      2drop false endif else
    2drop false endif ;

: inst_depends_func ( slist-addr -- flag )
  slist_data @
?trace $0040 [IF]
  ." INST_DEPENDS:" dup hex. cr
[THEN]
  btree_data @ ?inst_done ;

: inst_depends ( slist-addr -- slist-addr )
?trace $0040 [IF]
  ." inst_depends:" dup hex. ." ;" dup inst_print_depends cr
[THEN]
  dup 0<> if
    ['] inst_depends_func swap slist_find dup 0<> if
      slist_data @ endif endif ;

: inst_scheduling_out ( btree-addr -- )
  btree_data @
  dup node_type @ execute ;

: inst_scheduling_postorder ( xt btree-addr -- )
?trace $0040 [IF]
  ." scheduling:" hex.rs hex.s cr
[THEN]
  dup NIL <> if
?trace $0040 [IF]
    ." SCHEDULING" hex.s cr
[THEN]
    dup btree_data @ node_reg @ 0<> if
?trace $0040 [IF]
      ." regs opt:" hex.s cr
[THEN]
      dup btree_data @ node_link @ 0<> if
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
        btree_data @ node_reg @ dup 0<> if
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
          dup btree_data @ node_depends @ inst_depends
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

        dup btree_data @ ?inst_done if
          2>r 2r@ nip btree_left @ dup 0<> if
            btree_data @ inst_reg@ else
	    drop endif
          2r@ nip btree_right @ dup 0<> if
            btree_data @ inst_reg@ else
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

: inst_scheduling_func ( slist-addr -- )
  slist_data @
  ['] inst_scheduling_out swap inst_scheduling_postorder ;

: inst_scheduling ( -- )
  ['] inst_scheduling_func inst_head @ slist_forall ;

?test $0040 [IF]
cr ." Test for inst-scheduling.fs" cr

finish
[THEN]
