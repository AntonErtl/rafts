\ inst-selection.fs	instruction selection words
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

slist_struct
  1 cells: field inst_node
end-struct inst_struct

\ allocate and initial a inst
: inst ( node-addr -- inst-addr )
  inst_struct struct-allot	\ allocate
  slist				\ initial values
  tuck inst_node ! ;

variable inst_head

include node.fs

: (link+) ( node-addr -- )
  node_link 1 swap +! ;

: (link-) ( node-addr -- )
  node_link -1 swap +! ;

: link+ ( node-addr -- node-addr )
  ['] (link+) over btree_postorder ;

: link- ( node-addr -- node-addr )
  ['] (link-) over btree_postorder ;

: inst_init ( -- )
  NIL inst inst_head ! ;

: inst_insert ( node-addr -- )
  inst inst_head @
  slist_insert drop ;

: inst_insert_end ( node-addr -- )
  inst inst_head @ dup
  begin
    nip dup slist_next @ dup 0= until
  drop slist_insert drop ;

: inst_reg@ ( node-addr -- | register )
  node_reg @ dup 0= if
    2drop endif ;

: inst_reg! ( node-addr -- register )
  dup node_reg @ 0<> if
    node_reg @ else
    regs_get over node_link @ over regs_set
    tuck swap node_reg ! endif ;

: inst_done ( node-addr -- node-addr )
  false over node_done ! ;

: ?inst_done ( node-addr -- flag )
  node_done @ ;

: n_literal ( node-addr -- )
  inst_done
  dup inst_reg!
  swap dup node_val @ swap node_inst @ execute drop
?trace $0020 [IF]
  ." n_literal end:" .s regs_print cr
[THEN]
  ;

?lit_mode_val [IF]
: literal_depends_out ( node-addr val node-addr -- node-addr val )
  dup dup node_type @ ['] n_literal = if
    >r over r> node_val @ = if
      rot drop swap else
      drop endif else
    2drop endif ;

: literal_depends_func ( node-addr val inst-addr -- node-addr val )
  inst_node @
  ['] literal_depends_out swap btree_postorder ;

: literal_depends ( val -- node-addr )
  0 swap ['] literal_depends_func inst_head @ slist_forall
  swap dup 0= if
    swap depth ds_depth @ 2 + ?do
      ['] literal_depends_out i 1+ pick btree_postorder loop
    swap endif ;
[THEN]

: lit ( n -- node-addr )
?lit_mode_val [IF]
  literal_depends
  dup 0<> if
    nip link+ else
    drop
[THEN]
    @zero swap ['] @li ['] n_literal node
?lit_mode_val [IF]
    endif
[THEN]
    ; immediate

: n_id@ ( node-addr -- )
  inst_done
  dup inst_reg!
  swap dup node_offset @ swap dup node_pointer @ swap node_inst @ execute drop
?trace $0020 [IF]
  ." n_id@ end:" .s regs_print cr
[THEN]
  ;

: id@ ( offset val inst -- node-addr )
  ['] n_id@ node ; immediate

: n_id! ( register node-addr -- )
  inst_done
  over regs_dec
  dup node_offset @ swap dup node_pointer @ swap node_inst @ execute
?trace $0020 [IF]
  ." n_id! end:" .s regs_print cr
[THEN]
  ;

: id! ( node-addr offset val inst -- node-addr )
  ['] n_id! node
  tuck btree_right ! ; immediate

: n_op ( register register node-addr -- )
  inst_done
  >r r@ inst_reg!
  rot dup regs_dec rot dup regs_dec r> node_inst @ execute drop
?trace $0020 [IF]
  ." n_op end:" .s regs_print cr
[THEN]
  ;

: op ( node-addr node-addr offset val inst -- node-addr )
  ['] n_op node
  tuck btree_right !
  tuck btree_left ! ; immediate

: n_opn ( register register node-addr -- )
  inst_done
  >r
  dup regs_dec swap dup regs_dec swap r> node_inst @ execute
?trace $0020 [IF]
  ." n_opn end:" .s regs_print cr
[THEN]
  ;

: opn ( node-addr node-addr offset val inst -- node-addr )
  ['] n_opn node
  tuck btree_right !
  tuck btree_left ! ; immediate

: n_uop ( register node-addr -- )
  inst_done
  >r r@ inst_reg!
  swap dup regs_dec r> node_inst @ execute drop
?trace $0020 [IF]
  ." n_uop end:" .s regs_print cr
[THEN]
  ;

: uop ( node-addr offset val inst -- node-addr )
  ['] n_uop node
  tuck btree_right ! ; immediate

: n_uopn ( register node-addr -- )
  inst_done
  >r
  dup regs_dec r> node_inst @ execute
?trace $0020 [IF]
  ." n_uopn end:" .s regs_print cr
[THEN]
  ;

: uopn ( node-addr offset val inst -- node-addr )
  ['] n_uopn node
  tuck btree_right ! ; immediate

: inst_print_depends_func ( inst-addr -- )
  dup hex.
  inst_node hex? ;

: inst_print_depends ( inst-addr -- )
  dup hex.
  dup node_depends_init <> if
    ['] inst_print_depends_func swap slist_forall else
    drop ." no" endif ;

: inst_print_out ( node-addr -- )
  ." { " dup hex.
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

: inst_print_func ( inst-addr -- )
  inst_node @
dup hex. ." INST_PRINT_FUNC." cr
  ['] inst_print_out swap btree_postorder cr ;

: inst_print ( -- )
  ['] inst_print_func inst_head @ slist_forall ;

?test $0020 [IF]
cr ." Test for inst-selection.fs" cr

finish
[THEN]
