\ $Id: inst-selection.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: inst-selection.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

include node.fs

variable inst_head

: (link+) ( x -- )
  btree_data @ node_link 1 swap +! ;

: (link-) ( x -- )
  btree_data @ node_link -1 swap +! ;

: link+ ( x -- x )
  ['] (link+) over btree_postorder ;

: link- ( x -- x )
  ['] (link-) over btree_postorder ;

: inst_init ( -- )
  slist_init inst_head ! ;

: inst_insert ( x -- )
  inst_head @ slist_insert drop ;

: inst_insert_end ( x -- )
  inst_head @ dup
  begin
    nip dup slist_next @ dup 0= until
  drop slist_insert drop ;

: inst_reg@ ( addr -- | register )
  node_reg @ dup 0= if
    2drop endif ;

: inst_reg! ( addr -- register )
  dup node_reg @ 0<> if
    node_reg @ else
    regs_get over node_link @ over regs_set
    tuck swap node_reg ! endif ;

: inst_done ( addr -- addr )
  false over node_done ! ;

: ?inst_done ( addr -- flag )
  node_done @ ;

: n_literal ( addr -- )
  inst_done
  dup inst_reg!
  swap dup node_val @ swap node_inst @ execute drop
?trace $0020 [IF]
  ." n_literal end:" .s regs_print cr
[THEN]
  ;

?lit_mode_val [IF]
: literal_depends_out ( btree-addr val btree-addr -- btree-addr val )
  dup btree_data @
  dup node_type @ ['] n_literal = if
    >r over r> node_val @ = if
      rot drop swap else
      drop endif else
    2drop endif ;

: literal_depends_func ( btree-addr val slist-addr -- btree-addr val )
  slist_data @
  ['] literal_depends_out swap btree_postorder ;

: literal_depends ( val -- btree-addr )
  0 swap ['] literal_depends_func inst_head @ slist_forall
  swap dup 0= if
    swap depth ds_depth @ 2 + ?do
      ['] literal_depends_out i 1+ pick btree_postorder loop
    swap endif ;
[THEN]

: lit ( n -- addr )
?lit_mode_val [IF]
  literal_depends
  dup 0<> if
    nip link+ else
    drop
[THEN]
    @zero swap ['] @li ['] n_literal node btree
?lit_mode_val [IF]
    endif
[THEN]
    ; immediate

: n_id@ ( addr -- )
  inst_done
  dup inst_reg!
  swap dup node_offset @ swap dup node_pointer @ swap node_inst @ execute drop
?trace $0020 [IF]
  ." n_id@ end:" .s regs_print cr
[THEN]
  ;

: id@ ( offset val inst -- addr )
  ['] n_id@ node
  btree ; immediate

: n_id! ( register addr -- )
  inst_done
  over regs_dec
  dup node_offset @ swap dup node_pointer @ swap node_inst @ execute
?trace $0020 [IF]
  ." n_id! end:" .s regs_print cr
[THEN]
  ;

: id! ( addr offset val inst -- addr )
  ['] n_id! node
  btree tuck btree_right ! ; immediate

: n_op ( register register addr -- )
  inst_done
  >r r@ inst_reg!
  rot dup regs_dec rot dup regs_dec r> node_inst @ execute drop
?trace $0020 [IF]
  ." n_op end:" .s regs_print cr
[THEN]
  ;

: op ( addr addr offset val inst -- addr )
  ['] n_op node
  btree tuck btree_right !
  tuck btree_left ! ; immediate

: n_opn ( register register addr -- )
  inst_done
  >r
  dup regs_dec swap dup regs_dec swap r> node_inst @ execute
?trace $0020 [IF]
  ." n_opn end:" .s regs_print cr
[THEN]
  ;

: opn ( addr addr offset val inst -- addr )
  ['] n_opn node
  btree tuck btree_right !
  tuck btree_left ! ; immediate

: n_uop ( register addr -- )
  inst_done
  >r r@ inst_reg!
  swap dup regs_dec r> node_inst @ execute drop
?trace $0020 [IF]
  ." n_uop end:" .s regs_print cr
[THEN]
  ;

: uop ( addr offset val inst -- addr )
  ['] n_uop node
  btree tuck btree_right ! ; immediate

: n_uopn ( register addr -- )
  inst_done
  >r
  dup regs_dec r> node_inst @ execute
?trace $0020 [IF]
  ." n_uopn end:" .s regs_print cr
[THEN]
  ;

: uopn ( addr offset val inst -- addr )
  ['] n_uopn node
  btree tuck btree_right ! ; immediate

: inst_print_depends_func ( slist-addr -- )
  dup hex.
  slist_data hex? ;

: inst_print_depends ( slist-addr -- )
  dup hex.
  dup node_depends_init <> if
    ['] inst_print_depends_func swap slist_forall else
    drop ." no" endif ;

: inst_print_out ( btree-addr -- )
  ." { " dup hex.
  btree_data @
  dup hex.
  dup dup node_type @ case
    ['] n_literal of ." (n_literal) " dup node_offset hex? node_val hex? endof
    ['] n_id@ of ." (n_id@) " dup node_offset hex? node_pointer hex? endof
    ['] n_id! of ." (n_id!) " dup node_offset hex? node_pointer hex? endof
    ['] n_op of ." (n_op) " node_inst @ name. endof
    ['] n_opn of ." (n_opn) " node_inst @ name. endof
    ['] n_uop of ." (n_uop) " node_inst @ name. endof
    ['] n_uopn of ." (n_uopn) " node_inst @ name. endof
    >r dup node_offset hex? node_val hex? r> endcase
  ." link:" dup node_link ?
  ." reg:" dup node_reg ?
  ." depends:" node_depends @ inst_print_depends
  ." }" cr ;

: inst_print_func ( slist-addr -- )
  slist_data @ ['] inst_print_out swap btree_postorder cr ;

: inst_print ( -- )
  ['] inst_print_func inst_head @ slist_forall ;

?test $0020 [IF]
cr ." Test for inst-selection.fs" cr

finish
[THEN]
