\ basic.fs	basic words
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

\ variables for compile-time data stack
$20 constant ds_size
ds_size 2/ constant ds_tosstart
variable ds_tos
\ compile-time data stack pointer
ds_size array ds_data
ds_size 2/ array ds_init

: ds_tos@ ( -- n ) ds_tos @ ;
: ds_tos! ( n -- ) ds_tos ! ;
: ds_data@ ( n -- n ) ds_data @ ;
: ds_data! ( n n -- ) ds_data ! ;

\ functions for handling the compile-time data stack
: #data@ ( n -- x )
  ds_tos@ + ds_data@ ;

: #data! ( x n -- )
  ds_tos@ + ds_data! ;

: >data ( x -- ) ( D: -- x )
  -1 ds_tos +!
  0 #data! ;

: data> ( -- x ) ( D: x -- )
  0 #data@
  1 ds_tos +! ;

: .ds ( -- )
  ." <D:" ds_tos@ 0 .r ." > "
  ds_tos@ dup 0 ?do
    dup i - #data@
    hex. loop
  drop ;

\ variables for compile-time return stack
$20 constant rs_size
rs_size 2/ constant rs_torstart
variable rs_tor
\ compile-time return stack pointer
rs_size array rs_data
rs_size 2/ array rs_init

: rs_tor@ ( -- n ) rs_tor @ ;
: rs_tor! ( n -- ) rs_tor ! ;
: rs_data@ ( n -- n ) rs_data @ ;
: rs_data! ( n n -- ) rs_data ! ;

\ functions for handling the compile-time return stack
: #return@ ( n -- x )
  rs_tor@ + rs_data@ ;

: #return! ( x n -- )
  rs_tor@ + rs_data! ;

: >return ( x -- ) ( R: -- x )
  -1 rs_tor +!
  0 #return! ;

: return> ( -- x ) ( R: x -- )
  0 #return@
  1 rs_tor +! ;

: .rs ( -- )
  ." <R:" rs_tor@ 0 .r ." > "
  rs_tor@ dup 0 ?do
    dup i - #return@
    hex. loop
  drop ;

\ variables for local controll stack
$20 constant cs_size
variable cs_tos
cs_size array cs_data

: cs_tos@ ( -- n ) cs_tos @ ;
: cs_tos! ( n -- ) cs_tos ! ;
: cs_data@ ( n -- n ) cs_data @ ;
: cs_data! ( n n -- ) cs_data ! ;

cs_size 1- cs_tos!

\ functions for handling the local control stack
: #control@ ( n -- x )
  cs_tos@ + cs_data@ ;

: #control! ( x n -- )
  cs_tos@ + cs_data! ;

: >control ( x -- ) ( C: -- x )
  0 #control!
  -1 cs_tos +! ;

: control@ ( -- x ) ( C: -- )
  0 #control@ ;

: control> ( -- x ) ( C: x -- )
  1 cs_tos +!
  0 #control@ ;

: cs_depth ( -- n )
  cs_size cs_tos@ - 1- ;

: .cs ( -- )
  ." <C:" cs_depth 0 .r ." > "
  cs_depth dup 0 ?do
    dup i - #control@
    hex. loop
  drop ;

variable basic_head_ptr
$2000 constant basic_code

variable inst_!_list
\ contains the last !
variable inst_@_list
\ contains the last ! and all @ since the last !
variable inst_s!_list
\ contains all stores to the data stack (and all loads)

include inst-selection.fs
include inst-scheduling.fs

\ \ initialize the compile-time data stack
\ : data_init_stack ( -- )
\   0 ds_data ds_size cells NULL fill
\   ds_size 2/ 0 ?do
\     i cells #sp id@
\     burm_reg_NT over burm_reduce drop	\ make ml for tree
\     0 0 inst_nodes !			\ make sure inst_nodes does not overrun
\     i ds_init ! loop ;
\ data_init_stack
\ inst_init

\ : reset-stack-node ( node -- )
\     dup node_reset
\     regs_unused swap node_reg ! ;

: init-compile-stack ( reg addr n -- )
  0 ?do ( reg addr )
    i cells 2 pick id@ ( reg addr node )
    dup inst inst_s!_list @ slist_insert drop
    over !
    cell+
  loop
  2drop ;

: data_init ( -- )
  #sp 0 ds_init ds_size 2/ init-compile-stack
  0 ds_init ds_size 2/ dup ds_data swap cells move ;

\ \ initialize the compile-time return stack
\ : return_init_stack ( -- )
\   0 rs_data rs_size cells NULL fill
\   rs_torstart dup negate ?do
\     i cells #rp id@
\     i rs_torstart + rs_init ! loop ;
\ return_init_stack

: return_init ( -- )
  #rp 0 rs_init rs_size 2/ init-compile-stack
  0 rs_init rs_size 2/ dup rs_data swap cells move ;

: control_init ( -- )
  0 cs_data cs_size cells NULL fill ;
control_init

\ initial a basic block
: basic_init ( -- )
?trace $0020 [IF]
  ." basic_init " here hex. cr
[THEN]
  regs_init
  here basic_head_ptr !
  basic_code allot
?trace $0020 [IF]
  ." BASIC_INIT{ " here hex. cr
[THEN]
  inst_init
  NIL inst inst_!_list !
  NIL inst inst_@_list !
  ds_tosstart ds_tos!			\ initialize the data stack
  NIL inst inst_s!_list !
  data_init
  rs_torstart rs_tor!			\ initialize the temp return stack
  return_init
?trace $0020 [IF]
  ." BASIC_INIT " hex.s cr
[THEN]
  ;

: (basic_stackupdate) ( val register -- )
  >r regs_unused I_LITS terminal
  0 r@ I_REG terminal
  I_PLUS op inst_s!_list @ over node_depends !
  r> dup regs_inc over node_reg ! inst_btrees_insert_end ;

: basic_stackupdate ( register n -- )
?trace $0100 [IF]
  ." stack update:" 2dup . . cr
[THEN]
  dup 0<> if
    cells swap (basic_stackupdate) else
    2drop endif ;

: basic_stackdump ( -- )
  ds_tos@ ds_tosstart - >r
?trace $0100 [IF]
  ds_size 2/ 0 ?do
    i ds_init @ dup hex.
    inst_print_node loop cr
  ds_size 0 ?do
    i ds_data@ dup hex.
    ?dup 0<> if
      inst_print_node else
      cr endif loop cr
  ." TOS:" ds_tos@ . cr
[THEN]
  ds_size ds_tos@ ?do					\ dump the data stack
?trace $0100 [IF]
    ." STACKDUMP (data):" i . hex.s cr
[THEN]
    data> i ds_tosstart - dup 0< if			\ new stackelements
?trace $0100 [IF]
    ." STACKDUMP (new):" hex.s cr
[THEN]
      cells #sp id!
      dup inst inst_s!_list @ slist_insert drop
      inst_btrees_insert else
      2dup ds_init @ <> if				\ old stackelements (changed)
?trace $0100 [IF]
    ." STACKDUMP (old):" hex.s cr
[THEN]
        tuck cells #sp id!
        dup inst inst_s!_list @ slist_insert drop
        swap ds_init @ inst NULL inst tuck slist_insert drop
        over node_depends !
        inst_btrees_insert
      else
?trace $0100 [IF]
    ." STACKDUMP (nothing):" hex.s cr
[THEN]
	2drop endif endif loop
  #sp r> basic_stackupdate				\ update the data stackpointer
  rs_size rs_tor@ ?do					\ dump the return stack
?trace $0100 [IF]
    ." STACKDUMP (return):" i . hex.s cr
[THEN]
    i rs_data@ i rs_torstart - rs_init @ over <> if
      i rs_torstart - cells #rp id!
      dup inst inst_s!_list @ slist_insert drop
      i rs_torstart - dup 0>= if
        rs_init @ inst NULL inst tuck slist_insert drop
        over node_depends !
      else
	drop endif
      inst_btrees_insert
    else
      drop endif
    loop
  #rp rs_tor@ rs_torstart - basic_stackupdate ;		\ update the data stackpointer

: basic_print ( -- )
  ." BTREE PRINT" hex.s cr
  inst_btrees_print
  ." NODE PRINT" hex.s cr
  inst_nodes_print
  ." PNODE PRINT" hex.s cr
  inst_pnodes_print
  ." LISTS PRINT" hex.s cr
  inst_lists_print ;

\ exit a basic block and generate the code of the basic block
make-ml
0 over ml-reg !
' asm_nop over  ml-asm !
constant nop-ml \ nop instruction, usable only after scheduling

: basic_exit ( -- )
?trace $0020 [IF]
  ." BASIC_EXIT " hex.s cr
[THEN]
  basic_stackdump

?trace $0200 [IF]
  basic_print
  ." }BASIC_EXIT " here hex. cr
  ." INST SELECTION" hex.s cr
[THEN]
  inst_selection

  translate-all-dependences

?trace $0200 [IF]
  basic_print
  ." INST SCHEDULING" hex.s cr
[THEN]
  inst_scheduling

?trace $0200 [IF]
  basic_print
." REGISTER ALLOCATION" hex.s cr
[THEN]
  register_allocation

?trace $0200 [IF]
basic_print
regs_print
." ASSEMBLE" hex.s cr
[THEN]
  basic_head_ptr @ dp !
  assemble ;

>target_compile
: cs-pick ( u -- ) ( C: dest/origu ... dest/orig1 dest/orig0 -- dest/origu ... dest/orig1 dest/orig0 dest/origu )
  #control@ ;

: cs-roll ( u -- ) ( C: dest/origu dest/origu-1 ... dest/orig0 -- dest/origu-1 ... dest/orig0 dest/origu )
  dup 1+ #control@ swap
  cs_tos@ cs_data cell+ dup cell+ rot cells move
  control> drop >control ;
>source

include func.fs

?test $0004 [IF]
cr ." Test for basic.fs" cr

finish
[THEN]
