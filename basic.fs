\ $Id: basic.fs,v 1.1 1995/10/06 18:12:53 anton Exp $
\
\ Copyright (c) 1994 Christian PIRKER (pirky@mips.complang.tuwien.ac.at)
\ All Rights Reserved.
\
\ $Log: basic.fs,v $
\ Revision 1.1  1995/10/06 18:12:53  anton
\ Initial revision
\

\ Variablen for Datenstackbehandlung
variable ds_depth
variable ds_tos

\ Variablen fuer Returnstackbehandlung
$20 constant rs_size
rs_size 2 / constant rs_torstart
variable rs_tor
rs_size array rs_data
rs_size array rs_init

\ Variablen fuer Kontrollstackbehandlung
$20 constant cs_size
variable cs_tos
cs_size 1- cs_tos !
cs_size array cs_data

variable basic_head_ptr
$2000 constant basic_code

variable node_!_list
variable node_@_list

include inst-selection.fs
include inst-scheduling.fs

\ initial the initial temp returnstack
: @lw_nop ( -- )
  @lw
  @nop ;

: return_init_stack ( -- )
  rs_torstart dup negate ?do
    i cells #rp ['] @lw_nop postpone id@
    i rs_torstart + rs_init ! loop ;
return_init_stack

\ initial the temp returnstack
: return_init ( -- )
  rs_size 0 ?do
    i rs_init @
    dup btree_data @ node_reset drop link-	\ reset the node values
    i rs_data ! loop ;

\ initial a basic block
: basic_init ( -- )
  depth ds_depth !			\ initial the datastack
?trace $0020 [IF]
  ." BASIC_INIT " here hex. cr
[THEN]
  here basic_head_ptr !
  basic_code allot
?trace $0020 [IF]
  ." BASIC_INIT{ " here hex. cr
[THEN]
  0 ds_tos !
  0 rs_tor !				\ initial the temp returnstack
  return_init
  slist_init node_!_list !
  slist_init node_@_list !
  inst_init ;

: n_basic ( node-addr -- )
  dup node_pointer @ dup rot dup node_offset @ swap
  node_inst @ execute drop
?trace $0020 [IF]
  ." n_basic end:" .s regs_print cr
[THEN]
  ;

: (basic_stackupdate) ( val register -- )
  ['] @addiu ['] n_basic node btree inst_insert_end ;

: basic_stackupdate ( n -- )
?trace $0100 [IF]
  ." ds_tos:" ds_tos ? .s cr
[THEN]
  ds_tos @ swap - cells
?trace $0100 [IF]
  ." ds_tos update:" dup . cr
[THEN]
  dup 0<> if
    #sp (basic_stackupdate) else
    drop endif
  rs_tor @ cells
?trace $0100 [IF]
  ." rs_tor update:" dup . cr
[THEN]
  dup 0<> if
    #rp (basic_stackupdate) else
    drop endif ;

: basic_stackdump ( btree-addr-n-1 ... btree-addr-0 -- )
  depth ds_depth @ -
  >r
  r@ ds_tos @ over - swap 0 ?do		\ dump the datastack
?trace $0100 [IF]
    ." STACKDUP:" i . .s cr
[THEN]
    i over + cells rot swap #sp ['] @sw postpone id! inst_insert loop
  drop
  rs_size rs_torstart rs_tor @ + ?do	\ dump the returnstack
    i rs_data @ i rs_init @ over <> if
      link+ i rs_torstart - cells #rp ['] @sw postpone id! inst_insert else
      drop endif
    loop
  r> basic_stackupdate ;

\ generate the dependencies between the datastackelements
: (basic_depends_out) ( btree-addr-found offset val btree-addr -- btree-addr-found offset val )
  dup btree_data @
  dup node_type @ ['] n_id@ = if	\ search stack-reads
    swap >r >r 2dup r@ node_val @ = swap r> node_offset @ = and r> swap if
      >r rot drop r> rot rot else	\ the same stackelement found
      drop endif else			\ the same stackelement not found
    2drop endif ;

: (basic_depends_func) ( btree-addr-found offset val slist-addr -- btree-addr-found )
  slist_data @
  ['] (basic_depends_out) swap btree_postorder ;

: (basic_depends) ( offset val -- btree-addr-found )
  0 rot rot
  ['] (basic_depends_func) inst_head @ slist_forall
  2drop ;

: basic_depends_out ( btree-addr -- )
  btree_data @
  dup node_type @ ['] n_id! = if	\ search stack-writes
    dup node_offset @ over node_val @ (basic_depends) dup 0<> if
      slist_init tuck slist_insert drop
      swap node_depends ! else		\ insert the dependency
      2drop endif else			\ no dependency found
    drop endif ;

: basic_depends_func ( slist-addr -- )
  slist_data @
  ['] basic_depends_out swap btree_postorder ;

: basic_depends ( -- )
  ['] basic_depends_func inst_head @ slist_forall ;

\ exit a basic block and generate the code of the basic block
: basic_exit ( btree-addr-n-1 ... btree-addr-0 -- )
  basic_stackdump
  basic_depends
?trace $0200 [IF]
  inst_print
[THEN]
?trace $0020 [IF]
  ." }BASIC_EXIT " here hex. cr
[THEN]
  basic_head_ptr @ dp !
  inst_scheduling
?trace $0400 [IF]
  inst_print
  regs_print
[THEN]
  ;

: data_stackel ( n -- btree-addr-n-1 ... btree-addr-0 )
  dup depth ds_depth @ - swap - 2 -
  dup 0< if
    2dup + >r swap >r negate dup ds_tos +!
    1+ 1 ?do				\ generate new btree elements for the stackelements
      ds_tos @ i - cells #sp ['] @lw_nop ['] n_id@ node btree loop
    r> r>
    0 ?do				\ put the new btree elements to the right position
      dup roll swap loop
    drop else
    2drop endif ;

?lit_mode_op [IF]
: ?data_stackel_literal ( n -- flag )
  true swap 1+ 1 ?do
    i pick btree_data @ node_type @ ['] n_literal = and loop ;
[THEN]

: data_stackel_literal ( n -- btree-addr-n-1 ... btree-addr-0 )
?lit_mode_op [IF]
  >r r@
[THEN]
  data_stackel
?lit_mode_op [IF]
  r@ ?data_stackel_literal r> swap if
    dup 1+ 1 ?do
      dup roll btree_data link- @ node_val @ swap loop
    drop true else
    drop false endif
[THEN]
?lit_mode_op_not [IF]
  false
[THEN]
  ;

: return_stackel_fetch ( n -- btree-addr )
  rs_torstart rs_tor @ + + rs_data @ ;

: return_stackel_get ( -- btree-addr )
  0 return_stackel_fetch
  1 rs_tor +! ;

: return_stackel_put ( btree-addr -- )
  -1 rs_tor +!
  rs_torstart rs_tor @ + rs_data ! ;

\ function for handling the control stack
: >control ( x -- ) ( C: -- x )
  cs_tos @ cs_data !
  -1 cs_tos +! ;

: control@ ( -- x ) ( C: -- )
  cs_tos @ cs_data @ ;

: control> ( -- x ) ( C: x -- )
  1 cs_tos +!
  cs_tos @ cs_data @ ;

: cs_depth ( -- n )
  cs_size cs_tos @ - 1- ;

>target_compile
: cs-pick ( u -- ) ( C: dest/origu ... dest/orig1 dest/orig0 -- dest/origu ... dest/orig1 dest/orig0 dest/origu )
  cs_tos @ swap + cs_data @ ; immediate restrict

: cs-roll ( u -- ) ( C: dest/origu dest/origu-1 ... dest/orig0 -- dest/origu-1 ... dest/orig0 dest/origu )
  dup 1+
  vtarget_compile
  postpone cs-pick
  vsource
  >r
  cells cs_tos @ cs_data cell+ dup cell+ rot move
  control> drop r> >control ; immediate restrict
>source

: .cs ( -- )
  ." <C:" cs_depth 0 .r ." > "
  cs_depth dup 0 ?do
    dup i -
    vtarget_compile
    postpone cs-pick
    vsource
    hex. loop
  drop ;

include func.fs

?test $0004 [IF]
cr ." Test for basic.fs" cr

here
func_init foo
basic_init
1 lit
2 lit
3 data_stackel
.s cr
return_stackel_get
\ 3 lit
.s cr
3 roll dup inst_print_out
3 roll dup inst_print_out
3 roll dup inst_print_out
3 roll dup inst_print_out
basic_exit
func_exit
here 2dup over - dump
swap cell+ dup c@ $1f and + char+ aligned swap disasm_dump
.cs cr
regs_print

finish
[THEN]
